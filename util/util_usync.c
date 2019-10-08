#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <errno.h>
#include <sched.h>
#include <sys/types.h>

#include "mumps.h"
#include "proto.h"
#include "usync_mv1.h"

extern void panic(char*);


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

#define LATCH_FREE 0


void MV1LatchInit(MV1LATCH_T *latch)
{ MV1LatchUnlock(latch);
}


static
int MV1LatchTryLock(MV1LATCH_T *latch)
{ AO_t ret;

  ret = __sync_lock_test_and_set(latch, LATCH_FREE + 1);
  return LATCH_FREE == ret;
}


void MV1LatchUnlock(MV1LATCH_T *latch)
{ __sync_lock_release(latch);
}

void MicroSleep(u_long useconds)
{
  struct timeval tout;

  tout.tv_sec = 0;
  tout.tv_usec = useconds;
  select(0, NULL, NULL, NULL, &tout);
}

int MV1LatchLock(MV1LATCH_T *latch)
{ int i, j;
  u_int slot;

  for (i = 0; i < LOCK_TRIES; i++)
  { for (j = 0; j < LOCK_SPINS; j++)
    { if (MV1LatchTryLock(latch))			// try lock
        return 0;					//   done
#ifdef USE_EXPBACK
      slot = random() & ((1 << j) - 1);			// random wait
      MicroSleep(slot);
#endif
    }
#ifdef USE_EXPBACK
    if (0 == (i & 3))					// release CPU
      sched_yield();					//   in slot 0
#else
    if (i & 3)						// release CPU
      sched_yield();					//   in slot 1, 2, 3
    else
      MicroSleep(1000 * LOCK_SLEEP);			// sleep in slot 0
#endif
  }
  // fprintf(stderr, "lock_latch: timeout\n");
  errno = ETIMEDOUT;
  return -1;
}


/* --- SEM --- */

void MV1SemInit(MV1SEM_T *sem)				// init SEM struct
{
  bzero(sem, sizeof(MV1SEM_T));
  MV1LatchInit(&sem->g_latch);
}


int MV1SemWait(MV1SEM_T *sem)
{ int i, j, s, done;
  u_int slot;
  
  for (i = 0; i < LOCK_TRIES; i++)
  { for (j = 0; j < LOCK_SPINS; j++)
    { done = 0;
      s = MV1LatchLock(&sem->g_latch);			// lock SEM struct
      if (s < 0)					// failed ?
        continue;					//   just skip
      if (sem->ntok)					// has tokens ?
      { sem->ntok--;					//   get READER token
        done = 1;
      }
      MV1LatchUnlock(&sem->g_latch);			// release SEM struct
      if (done)						// done ?
        return 0;					//   return
#ifdef USE_EXPBACK
      slot = random() & ((1 << j) - 1);			// random wait
      MicroSleep(slot);
#endif
    }
#ifdef USE_EXPBACK
    if (0 == (i & 3))					// release CPU
      sched_yield();					//   in slot 0
#else
    if (i & 3)						// release CPU
      sched_yield();					//   in slot 1, 2, 3
    else
      MicroSleep(1000 * LOCK_SLEEP);			// sleep in slot 0
#endif
  }
  errno = ETIMEDOUT;					// report error
  return -1;
}


int MV1SemSignal(MV1SEM_T *sem, int numb)
{ int s;

#if 1
  inter_add(&sem->ntok, numb);				// increment #tokens
  MEM_BARRIER;
#else
  s = MV1LatchLock(&sem->g_latch);			// lock SEM struct
  if (s < 0)						// failed ?
  { errno = EAGAIN;					//   report error
    return -1;						//   done
  }
  sem->ntok += numb;					// adjust READER tokens
  MV1LatchUnlock(&sem->g_latch);			// unlock SEM struct
#endif
  return 0;						// done
}


/* --- RWLOCK --- */

int MV1RWLockInit(MV1RWLOCK_T *lok, int maxjob)		// init RWLOCK struct
{
  bzero(lok, sizeof(MV1RWLOCK_T));

  MV1LatchInit(&lok->g_latch);

  MV1LatchInit(&lok->wr_latch);
  MV1LatchLock(&lok->wr_latch);

  MV1SemInit(&lok->rd_sem);

  return 0;
}


void MV1LockWriter(MV1RWLOCK_T *lok)
{ int dowait, s;
  char msg[128];

  dowait = 0;
  s = MV1LatchLock(&lok->g_latch);			// lock RWLOCK struct
  if (s < 0)						// failed ?
  { panic("LockWriter(): failed [g_latch]");		//   do panic
  }
  if (lok->readers || lok->writers)			// READERs or WRITERs
  { dowait = 1;						//   present ? wait
  }
  lok->writers++;					// adjust WRITERs
  MV1LatchUnlock(&lok->g_latch);			// release RWLOCK struct

  if (dowait)						// should wait ?
  { // fprintf(stderr, "LockWriter(): %d waiting...\n", getpid());
    // fflush(stderr);
    s = MV1LatchLock(&lok->wr_latch);			// lock wr_latch
    if (s < 0)						// failed ?
    { inter_add(&lok->writers, -1);			//   adjust WRITERs
      sprintf(msg,"LockWriter(): failed [wr_latch] %d,%d,%d",
                    lok->readers,lok->wait_to_read,lok->writers);
      panic(msg);					//   do panic
    }
  }
}


int MV1TryLockWriter(MV1RWLOCK_T *lok)
{ int dowait, s;

  s = MV1LatchLock(&lok->g_latch);			// lock RWLOCK struct
  if (s < 0)						// failed ?
  { panic("TryLockWriter(): failed [g_latch]");		//   do panic
  }
  dowait = 0;
  if (lok->readers || lok->writers)			// READERs or WRITERs
  { dowait = 1;						//   present ? wait
  }
  else
    lok->writers++;					// adjust WRITERs
  MV1LatchUnlock(&lok->g_latch);			// release RWLOCK struct

  return dowait ? 0 : 1;
}


void MV1UnlockWriter(MV1RWLOCK_T *lok)
{ int i, s;

  s = MV1LatchLock(&lok->g_latch);			// lock RWLOCK struct
  if (s < 0)						// failed ?
  { inter_add(&lok->writers, -1);			//   adjust writers
    panic("UnlockWriter(): failed [g_latch]");		//   do panic
  }
  ASSERT(0 == lok->readers);				// check READERs
  lok->writers--;					// adjust writers
  ASSERT(0 <= lok->writers);				// check WRITERs
  if (lok->wait_to_read)				// READERs waiting ?
  { lok->readers = lok->wait_to_read;			//   release them
    lok->wait_to_read = 0;				//   all
    for (i = 0; i < 5; i++)				// try 5 times
    { s = MV1SemSignal(&lok->rd_sem, lok->readers);	//   signal READERs
      if (0 == s)
        break;
    }
  }
  else if (lok->writers)				// WRITERs waiting ?
    MV1LatchUnlock(&lok->wr_latch);			//   release wr_latch
  MV1LatchUnlock(&lok->g_latch);			// release RWLOCK struct
}


void MV1UnlockWriterToReader(MV1RWLOCK_T *lok)
{ int s;
  AO_t wait_to_read;

  s = MV1LatchLock(&lok->g_latch);			// lock RWLOCK struct
  if (s < 0)						// failed ?
  { inter_add(&lok->writers, -1);			//   adjust writers
    panic("UnlockWriterToReader(): failed [g_latch]");	//   do panic
  }
  ASSERT(0 == lok->readers);				// check READERs
  lok->writers--;					// adjust writers
  wait_to_read = lok->wait_to_read;
  lok->readers = 1 + lok->wait_to_read;			// all waiting + myself
  lok->wait_to_read = 0;
  MV1SemSignal(&lok->rd_sem, wait_to_read);		// rel. waiting READERs
  MV1LatchUnlock(&lok->g_latch);			// release RWLOCK struct
}


void MV1LockReader(MV1RWLOCK_T *lok)
{ int dowait;
  int s;
  char msg[128];

  dowait = 0;
  s = MV1LatchLock(&lok->g_latch);			// lock RWLOCK struct
  if (s < 0)						// failed ?
  { panic("LockReader(): failed [g_latch]");		//   do panic
  }
  if (lok->writers)					// WRITERs present ?
  { dowait = 1;						//   should wait
    lok->wait_to_read++;				//   adjust waiting RDs
  }
  else
    lok->readers++;					// adjust READERs
  MV1LatchUnlock(&lok->g_latch);			// release RWLOCK struct

  if (dowait)						// should wait ?
  { // fprintf(stderr, "LockReader(): %d waiting...\n", getpid());
    // fflush(stderr);
    s = MV1SemWait(&lok->rd_sem);			// wait for rd_sem
    if (s < 0)						// failed ?
    { inter_add(&lok->wait_to_read, -1);		//   adjust waiting RDs
      sprintf(msg,"LockReader(): failed [rd_sem] %d,%d,%d",
                    lok->readers,lok->wait_to_read,lok->writers);
      panic(msg);					//   do panic
    }
  }
}


int MV1TryLockReader(MV1RWLOCK_T *lok)
{ int dowait;
  int s;

  s = MV1LatchLock(&lok->g_latch);			// lock RWLOCK struct
  if (s < 0)						// failed ?
  { panic("TryLockReader(): failed [g_latch]");		//   do panic
  }
  dowait = 0;
  if (lok->writers)					// WRITERs present ?
  { dowait = 1;						// flag wait
  }
  else
    lok->readers++;					// adjust READERs
  MV1LatchUnlock(&lok->g_latch);			// release RWLOCK struct

  return dowait ? 0 : 1;
}


void MV1UnlockReader(MV1RWLOCK_T *lok)
{ int s;

  s = MV1LatchLock(&lok->g_latch);			// lock RWLOCK struct
  if (s < 0)						// failed ?
  { inter_add(&lok->readers, -1);			//   adjust READERs
    panic("UnlockReader(): failed [g_latch]");		//   do panic
  }
  ASSERT(0 != lok->readers);				// check READERs
  lok->readers--;					// adjust READERs
  ASSERT(0 <= lok->readers);				// check READERs again
  if ((0 == lok->readers) && (0 < lok->writers))	// WRITERs waiting ?
    MV1LatchUnlock(&lok->wr_latch);			//   release wr_latch
  MV1LatchUnlock(&lok->g_latch);			// release RWLOCK struct
}


// --- Slim R/W lock -------------------------

#define CAS(x,y,z)	__sync_bool_compare_and_swap(x,y,z)

#define SRWLOCK_EXCLUSIVE	((AO_t) -1L)

void SRWInit(SRWLOCK_T *lok)
{ 
  *lok = SRWLOCK_INIT;
}

void SRWLockExclusive(SRWLOCK_T *lok)
{ u_int count;

  count = 0;
  while (!CAS(lok, 0L, SRWLOCK_EXCLUSIVE))
  { if (0 == (++count & 3))
      sched_yield();
  }
}

void SRWUnlockExclusive(SRWLOCK_T *lok)
{ u_int count;

  count = 0;
  while (!CAS(lok, SRWLOCK_EXCLUSIVE, 0L))
  { if (0 == (++count & 3))
      sched_yield();
  }
}

int SRWTryLockExclusive(SRWLOCK_T *lok)
{
  return CAS(lok, 0L, SRWLOCK_EXCLUSIVE);
}

void SRWDowngradeExclusive(SRWLOCK_T *lok)
{ u_int count;

  count = 0;
  while (!CAS(lok, SRWLOCK_EXCLUSIVE, 1L))
  { if (0 == (++count & 3))
      sched_yield();
  }
}

void SRWUnlockShared(SRWLOCK_T *lok)
{ AO_t oldval, newval;
  u_int count;

  count = 0;
  do
  { if (0 == (++count & 3))
      sched_yield();
    MEM_BARRIER;
    oldval = *lok;
    newval = oldval - 1;
  } while (!CAS(lok, oldval, newval));
}

void SRWLockShared(SRWLOCK_T *lok)
{ AO_t oldval, newval;
  u_int count;

  count = 0;
  do
  { do
    { if (0 == (++count & 3))
        sched_yield();
      MEM_BARRIER;
      oldval = *lok;
    } while (SRWLOCK_EXCLUSIVE == oldval);
    newval = oldval + 1;
  } while (!CAS(lok, oldval, newval));
}

int SRWTryLockShared(SRWLOCK_T *lok)
{ AO_t oldval, newval;
  u_int count;

  count = 0;
  do
  { if (0 == (++count & 3))
      sched_yield();
    MEM_BARRIER;
    oldval = *lok;
  } while (SRWLOCK_EXCLUSIVE == oldval);
  newval = oldval + 1;
  return CAS(lok, oldval, newval);
}

#undef CAS
