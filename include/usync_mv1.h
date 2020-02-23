#ifndef _USYNC_MV1_H
#define _USYNC_MV1_H

#include <stdint.h>

// #define USE_LIBATOMIC_OPS 1

#ifdef USE_LIBATOMIC_OPS

#include <atomic_ops.h>
typedef AO_TS_t LATCH_T;

#else

typedef uint32_t AO_t;
typedef volatile uint32_t MV1LATCH_T;

#endif


// --- interlocked increment -----------------
AO_t inter_add(volatile AO_t *ptr, AO_t incr);

// --- latch
void MV1LatchInit(MV1LATCH_T *latch);
int  MV1LatchLock(MV1LATCH_T *latch);
void MV1LatchUnlock(MV1LATCH_T *latch);


// --- semaphore -----------------------------

typedef struct _MV1SEM_T
{ MV1LATCH_T  g_latch;
  AO_t ntok;
} MV1SEM_T;

void MV1SemInit(MV1SEM_T *sem);
void MV1SemWait(MV1SEM_T *sem);
void MV1SemSignal(MV1SEM_T *sem, int numb);


// --- read-write lock -----------------------

typedef struct _MV1RWLOCK_T
{ MV1LATCH_T g_latch;
  MV1LATCH_T wr_latch;
  MV1SEM_T   rd_sem;
  AO_t readers;
  AO_t writers;
  AO_t wait_to_read;
} MV1RWLOCK_T;

int  MV1RWLockInit(MV1RWLOCK_T *lok, int maxjob);
void MV1LockWriter(MV1RWLOCK_T *lok);
int  MV1TryLockWriter(MV1RWLOCK_T *lok);
void MV1UnlockWriter(MV1RWLOCK_T *lok);
void MV1UnlockWriterToReader(MV1RWLOCK_T *lok);
void MV1LockReader(MV1RWLOCK_T *lok);
int  MV1TryLockReader(MV1RWLOCK_T *lok);
void MV1UnlockReader(MV1RWLOCK_T *lok);


// --- slim read-write lock ------------------

#define SRWLOCK_INIT	((AO_t) 0L)

typedef volatile uint32_t SRWLOCK_T;

void SRWInit(SRWLOCK_T *lok);
void SRWLockExclusive(SRWLOCK_T *lok);
void SRWUnlockExclusive(SRWLOCK_T *lok);
int  SRWTryLockExclusive(SRWLOCK_T *lok);
void SRWDowngradeExclusive(SRWLOCK_T *lok);
void SRWLockShared(SRWLOCK_T *lok);
void SRWUnlockShared(SRWLOCK_T *lok);
int  SRWTryLockShared(SRWLOCK_T *lok);
void SRWUnlockExclusiveToShared(SRWLOCK_T *lok);


// -------------------------------------------
// --- COMMON INTERFACE ----------------------
// -------------------------------------------

// --- latch ---------------------------------
#define LATCH_T         MV1LATCH_T

#define LatchInit(x)    MV1LatchInit(x)
#define LatchLock(x)    MV1LatchLock(x)
#define LatchUnlock(x)  MV1LatchUnlock(x)


// --- read-write lock -----------------------

#ifdef MV1_SRWLOCK

#define RWLOCK_T        	SRWLOCK_T

#define RWLockInit(x,y)         SRWInit(x)
#define LockWriter(x)           SRWLockExclusive(x)
#define TryLockWriter(x)        SRWTryLockExclusive(x)
#define UnlockWriter(x)         SRWUnlockExclusive(x)
#define UnlockWriterToReader(x) SRWDowngradeExclusive(x)
#define LockReader(x)           SRWLockShared(x)
#define TryLockReader(x)        SRWTryLockShared(x)
#define UnlockReader(x)         SRWUnlockShared(x)

#else

#define RWLOCK_T        	MV1RWLOCK_T

#define RWLockInit(x,y)         MV1RWLockInit(x,y)
#define LockWriter(x)           MV1LockWriter(x)
#define TryLockWriter(x)        MV1TryLockWriter(x)
#define UnlockWriter(x)         MV1UnlockWriter(x)
#define UnlockWriterToReader(x) MV1UnlockWriterToReader(x)
#define LockReader(x)           MV1LockReader(x)
#define TryLockReader(x)        MV1TryLockReader(x)
#define UnlockReader(x)         MV1UnlockReader(x)

#endif

#endif	/* _USYNC_MV1_H */
