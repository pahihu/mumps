#ifndef _C_RWLOCK_H
#define _C_RWLOCK_H

#ifdef __cpp__
extern "C" {
#endif

#include <sys/types.h>
#ifdef USE_MACH_SEMA
#include <mach/mach.h>
#endif

typedef struct RWLOCK_T
{
  volatile uint32_t status;
  int      maxjob;
#ifdef USE_MACH_SEMA
  semaphore_t sema[2];
#else
  int semid;
#endif
} RwLock_t;

 int rwlock_init(RwLock_t *lok, int maxjob);

void   lock_reader(RwLock_t *lok);
void unlock_reader(RwLock_t *lok);
void   lock_writer(RwLock_t *lok);
void unlock_writer(RwLock_t *lok);

 int rwlock_finish(RwLock_t *lok);

#ifdef __cpp__
}
#endif

#endif
