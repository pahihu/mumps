#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <assert.h>
#include <unistd.h>
#include <sched.h>

#include "d_rwlock.h"


uint32_t inter_add(volatile uint32_t *ptr, uint32_t incr)
{
  // return __sync_add_and_fetch(ptr, incr);
  uint32_t oldval, newval;
  do
  { oldval = *ptr;
    newval = oldval + incr;
  } while(!__sync_bool_compare_and_swap(ptr, oldval, newval));
  return newval;
}


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

static LATCH_T LATCH_FREE;


void LatchInit(LATCH_T *latch)
{
  LatchUnlock(latch);
  LATCH_FREE = *latch;
}


static
int LatchTryLock(LATCH_T *latch)
{
  uint32_t ret;

  ret = __sync_lock_test_and_set(latch, LATCH_FREE + 1);
  return LATCH_FREE == ret;
}


void LatchUnlock(LATCH_T *latch)
{
  __sync_lock_release(latch);
}


void LatchLock(LATCH_T *latch)
{ int i, j;
  uint32_t slot;

  for (i = 0; i < LOCK_TRIES; i++)
  { for (j = 0; j < LOCK_SPINS; j++)
    { if (LatchTryLock(latch))
        return;
#ifdef USE_EXPBACK
      slot = random() & ((1 << j) - 1);
      usleep(slot);
#endif
    }
#ifdef USE_EXPBACK
    if (0 == (i & 3))
      sched_yield();
#else
    if (i & 3)
      sched_yield();
    else
      usleep(1000 * LOCK_SLEEP);
#endif
  }
  fprintf(stderr, "lock_latch: timeout\n");
  assert(0);
}


/* --- SEM --- */

void SemInit(SEM_T *sem)
{
  bzero(sem, sizeof(SEM_T));
  LatchInit(&sem->g_latch);
}


void SemWait(SEM_T *sem)
{ int i, j, done;
  uint32_t slot;
  
  for (i = 0; i < LOCK_TRIES; i++)
  { for (j = 0; j < LOCK_SPINS; j++)
    { done = 0;
      LatchLock(&sem->g_latch);
      if (sem->ntok)
      { sem->ntok--;
        done = 1;
      }
      LatchUnlock(&sem->g_latch);
      if (done)
        return;
#ifdef USE_EXPBACK
      slot = random() & ((1 << j) - 1);
      usleep(slot);
#endif
    }
#ifdef USE_EXPBACK
    if (0 == (i & 3))
      sched_yield();
#else
    if (i & 3)
      sched_yield();
    else
      usleep(1000 * LOCK_SLEEP);
#endif
  }
  fprintf(stderr, "SemWait(): timeout\n");
  fflush(stderr);
  assert(0);
}


void SemSignal(SEM_T *sem, int numb)
{
  LatchLock(&sem->g_latch);
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
{ int dowait;

  dowait = 0;
  LatchLock(&lok->g_latch);
  if (lok->readers || lok->writers)
  { dowait = 1;
  }
  lok->writers++;
  LatchUnlock(&lok->g_latch);

  if (dowait)
  { // fprintf(stderr, "LockWriter(): %d waiting...\n", getpid());
    // fflush(stderr);
    LatchLock(&lok->wr_latch);
  }
}


void UnlockWriter(RWLOCK_T *lok)
{
  LatchLock(&lok->g_latch);
  assert(0 == lok->readers);
  lok->writers--;
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
{
  uint32_t wait_to_read;

  LatchLock(&lok->g_latch);
  assert(0 == lok->readers);
  lok->writers--;
  wait_to_read = lok->wait_to_read;
  lok->readers = 1 + lok->wait_to_read;
  lok->wait_to_read = 0;
  SemSignal(&lok->rd_sem, wait_to_read);
  LatchUnlock(&lok->g_latch);
}

void LockReader(RWLOCK_T *lok)
{ int dowait;

  dowait = 0;
  LatchLock(&lok->g_latch);
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
{
  LatchLock(&lok->g_latch);
  assert(0 != lok->readers);
  lok->readers--;
  if ((0 == lok->readers) && (0 < lok->writers))
    LatchUnlock(&lok->wr_latch);
  LatchUnlock(&lok->g_latch);
}

