#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <errno.h>
#include <sched.h>
#include <sys/types.h>

#include "mumps.h"
#include "proto.h"

extern void panic(char*);


#ifdef USE_LIBATOMIC_OPS
#define inter_add(ptr,incr) AO_fetch_and_add_acquire_read(ptr,incr)
#else

AO_t inter_add(volatile AO_t *ptr, AO_t incr)
{
  AO_t oldval, newval;
  do
  { oldval = *ptr;
    newval = oldval + incr;
  } while(!__sync_bool_compare_and_swap(ptr, oldval, newval));
  return newval;
}

#endif


/* --- LATCH --- */

#define USE_EXPBACK             1

#define LOCK_TRIES_PER_SEC      (4 * 1000)
#define LOCK_TRIES              (50 * LOCK_TRIES_PER_SEC)
#define LOCK_SLEEP              1
#ifdef USE_EXPBACK
#define LOCK_SPINS              8
#else
#define LOCK_SPINS              1024
#endif

#ifndef USE_LIBATOMIC_OPS
static LATCH_T LATCH_FREE;
#endif


void LatchInit(LATCH_T *latch)
{
#ifdef USE_LIBATOMIC_OPS
  *latch = AO_TS_INITIALIZER;
#else
  LatchUnlock(latch);
  LATCH_FREE = *latch;
#endif
}


static
int LatchTryLock(LATCH_T *latch)
{
#ifdef USE_LIBATOMIC_OPS
  AO_TS_t ret;
  ret = AO_test_and_set_acquire_read(latch);
  return AO_TS_CLEAR == ret;
#else
  AO_t ret;

  ret = __sync_lock_test_and_set(latch, LATCH_FREE + 1);
  return LATCH_FREE == ret;
#endif
}


void LatchUnlock(LATCH_T *latch)
{
#ifdef USE_LIBATOMIC_OPS
  AO_CLEAR(latch);
#else
  __sync_lock_release(latch);
#endif
}

void MicroSleep(u_long useconds)
{
  struct timeval tout;

  tout.tv_sec = 0;
  tout.tv_usec = useconds;
  select(0, NULL, NULL, NULL, &tout);
}

int LatchLock(LATCH_T *latch)
{ int i, j;
  u_int slot;

  for (i = 0; i < LOCK_TRIES; i++)
  { for (j = 0; j < LOCK_SPINS; j++)
    { if (LatchTryLock(latch))
        return 0;
#ifdef USE_EXPBACK
      slot = random() & ((1 << j) - 1);
      MicroSleep(slot);
#endif
    }
#ifdef USE_EXPBACK
    if (0 == (i & 3))
      sched_yield();
#else
    if (i & 3)
      sched_yield();
    else
      MicroSleep(1000 * LOCK_SLEEP);
#endif
  }
  // fprintf(stderr, "lock_latch: timeout\n");
  errno = ETIMEDOUT;
  return -1;
}


/* --- SEM --- */

void SemInit(SEM_T *sem)
{
  bzero(sem, sizeof(SEM_T));
  LatchInit(&sem->g_latch);
}


void SemWait(SEM_T *sem)
{ int i, j, s, done;
  u_int slot;
  
  for (i = 0; i < LOCK_TRIES; i++)
  { for (j = 0; j < LOCK_SPINS; j++)
    { done = 0;
      s = LatchLock(&sem->g_latch);
      if (s < 0)
      { panic("SemWait(): failed [g_latch]");
      }
      if (sem->ntok)
      { sem->ntok--;
        done = 1;
      }
      LatchUnlock(&sem->g_latch);
      if (done)
        return;
#ifdef USE_EXPBACK
      slot = random() & ((1 << j) - 1);
      MicroSleep(slot);
#endif
    }
#ifdef USE_EXPBACK
    if (0 == (i & 3))
      sched_yield();
#else
    if (i & 3)
      sched_yield();
    else
      MicroSleep(1000 * LOCK_SLEEP);
#endif
  }
  // fprintf(stderr, "SemWait(): timeout\n");
  // fflush(stderr);
  panic("SemWait(): failed");
}


void SemSignal(SEM_T *sem, int numb)
{ int s;

  s = LatchLock(&sem->g_latch);
  if (s < 0)
  { panic("SemSignal(): failed [g_latch]");
  }
  sem->ntok += numb;
  LatchUnlock(&sem->g_latch);
}


/* --- RWLOCK --- */

int RWLockInit(RWLOCK_T *lok, int maxjob)
{
  bzero(lok, sizeof(RWLOCK_T));

  LatchInit(&lok->g_latch);

  LatchInit(&lok->wr_latch);
  LatchLock(&lok->wr_latch);

  SemInit(&lok->rd_sem);

  return 0;
}


void LockWriter(RWLOCK_T *lok)
{ int dowait, s;
  char msg[128];

  dowait = 0;
  s = LatchLock(&lok->g_latch);
  if (s < 0)
  { panic("LockWriter(): failed [g_latch]");
  }
  if (lok->readers || lok->writers)
  { dowait = 1;
  }
  lok->writers++;
  LatchUnlock(&lok->g_latch);

  if (dowait)
  { // fprintf(stderr, "LockWriter(): %d waiting...\n", getpid());
    // fflush(stderr);
    s = LatchLock(&lok->wr_latch);
    if (s < 0)
    { sprintf(msg,"LockWriter(): failed [wr_latch] %d,%d,%d",
                    lok->readers,lok->wait_to_read,lok->writers);
      panic(msg);
    }
  }
}


void UnlockWriter(RWLOCK_T *lok)
{ int s;

  s = LatchLock(&lok->g_latch);
  if (s < 0)
  { panic("UnlockWriter(): failed [g_latch]");
  }
  ASSERT(0 == lok->readers);
  lok->writers--;
  ASSERT(0 <= lok->writers);
  if (lok->wait_to_read)
  { lok->readers = lok->wait_to_read;
    lok->wait_to_read = 0;
    SemSignal(&lok->rd_sem, lok->readers);
  }
  else if (lok->writers)
    LatchUnlock(&lok->wr_latch);
  LatchUnlock(&lok->g_latch);
}


void UnlockWriterToReader(RWLOCK_T *lok)
{ int s;
  AO_t wait_to_read;

  s = LatchLock(&lok->g_latch);
  if (s < 0)
  { panic("UnlockWriterToReader(): failed [g_latch]");
  }
  ASSERT(0 == lok->readers);
  lok->writers--;
  wait_to_read = lok->wait_to_read;
  lok->readers = 1 + lok->wait_to_read;
  lok->wait_to_read = 0;
  SemSignal(&lok->rd_sem, wait_to_read);
  LatchUnlock(&lok->g_latch);
}

void LockReader(RWLOCK_T *lok)
{ int dowait;
  int i, s;

  dowait = 0;
  s = LatchLock(&lok->g_latch);
  if (s < 0)
  { panic("LockReader(): failed [g_latch]");
  }
  if (lok->writers)
  { dowait = 1;
    lok->wait_to_read++;
  }
  else
    lok->readers++;
  LatchUnlock(&lok->g_latch);

  if (dowait)
  { // fprintf(stderr, "LockReader(): %d waiting...\n", getpid());
    // fflush(stderr);
    SemWait(&lok->rd_sem);
  }
}


void UnlockReader(RWLOCK_T *lok)
{ int s;

  s = LatchLock(&lok->g_latch);
  if (s < 0)
  { panic("UnlockReader(): failed [g_latch]");
  }
  ASSERT(0 != lok->readers);
  lok->readers--;
  ASSERT(0 <= lok->readers);
  if ((0 == lok->readers) && (0 < lok->writers))
    LatchUnlock(&lok->wr_latch);
  LatchUnlock(&lok->g_latch);
}

