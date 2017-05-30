#ifndef _USYNC_CK_H
#define _USYNC_CK_H

#include <stdint.h>

#include <ck_pr.h>
#include <ck_spinlock.h>
#include <ck_rwlock.h>

typedef uint32_t AO_t;

// --- interlocked increment -------------------------------
#define inter_add(x,y)  ck_pr_add_32(x,y)


// --- latch -----------------------------------------------
#define LATCH_T         ck_spinlock_ticket_t

#define LatchInit(x)    ck_spinlock_ticket_init(x)
#define LatchLock(x)    ck_spinlock_ticket_lock_pb(x, 1)
#define LatchUnlock(x)  ck_spinlock_ticket_unlock(x)


// --- read-write lock -------------------------------------
#define RWLOCK_T        ck_rwlock_t

#define RWLockInit(x,y) ck_rwlock_init(x)
#define LockWriter(x)   ck_rwlock_write_lock(x)
#define UnlockWriter(x) ck_rwlock_write_unlock(x)
#define UnlockWriterToReader(x) ck_rwlock_write_downgrade(x)
#define LockReader(x)   ck_rwlock_read_lock(x)
#define UnlockReader(x) ck_rwlock_read_unlock(x)


#endif
