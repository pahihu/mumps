#ifndef _USYNC_MV1_H
#define _USYNC_MV1_H

#include <stdint.h>

// #define USE_LIBATOMIC_OPS 1

#ifdef USE_LIBATOMIC_OPS

#include <atomic_ops.h>
typedef AO_TS_t LATCH_T;

#else

typedef uint32_t AO_t;
typedef volatile uint32_t LATCH_T;

#endif


// --- interlocked increment -----------------
AO_t inter_add(volatile AO_t *ptr, AO_t incr);

// --- latch
void LatchInit(LATCH_T *latch);
int  LatchLock(LATCH_T *latch);
void LatchUnlock(LATCH_T *latch);


// --- semaphore -----------------------------

typedef struct _SEM_T
{ LATCH_T  g_latch;
  AO_t ntok;
} SEM_T;

void SemInit(SEM_T *sem);
void SemWait(SEM_T *sem);
void SemSignal(SEM_T *sem, int numb);


// --- read-write lock -----------------------

typedef struct _RWLOCK_T
{ LATCH_T g_latch;
  LATCH_T wr_latch;
  SEM_T   rd_sem;
  AO_t readers;
  AO_t writers;
  AO_t wait_to_read;
} RWLOCK_T;

int  RWLockInit(RWLOCK_T *lok, int maxjob);
void LockWriter(RWLOCK_T *lok);
void UnlockWriter(RWLOCK_T *lok);
void UnlockWriterToReader(RWLOCK_T *lok);
void LockReader(RWLOCK_T *lok);
void UnlockReader(RWLOCK_T *lok);


#endif
