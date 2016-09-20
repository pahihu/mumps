#include <errno.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <mach/mach_time.h>
#include <assert.h>

// #define USE_MACH_SEMA   1
#include "c_rwlock.h"

// #define USE_BITFIELDS   1
#ifdef USE_BITFIELDS
#include "bitfield.h"

BEGIN_BITFIELD_TYPE(Status, uint32_t)
  ADD_BITFIELD_MEMBER(F_READERS, 0, 10)
  ADD_BITFIELD_MEMBER(F_WAIT_TO_READ, 10, 10)
  ADD_BITFIELD_MEMBER(F_WRITERS, 20, 10)
END_BITFIELD_TYPE()

#define BITONE(x)       Status().x.one()
#define FLD(x,y)        x.y
#define FLDINC(x,y)     x.y++
#define FLDDEC(x,y)     x.y--
#define SET_FLD(z,x,y)  z.x = y
#define VALPTR(x)       &(x.wrapper.value)
#define VAL(x)          (x.wrapper.value)
#define FLDMAX(x)       Status().x.maximum()

#else

#define Status          uint32_t
#define F_BIT0            0
#define F_WRITERS        20
#define F_WAIT_TO_READ   10
#define F_READERS         0
#define FLD_WIDTH        10
#define BITONE(x)       ((uint32_t) (1 << (x)))
#define FLD_MAX         ((uint32_t) BITONE(FLD_WIDTH) - 1)
#define FLDINC(x,y)     x += BITONE(y)
#define FLDDEC(x,y)     x -= BITONE(y)
#define FLD(x,y)        (FLD_MAX & ((x) >> (y)))
#define SET_FLD(z,x,y)  z = (z & ~(FLD_MAX << x)) | ((y) << x)
#define VALPTR(x)       &x
#define VAL(x)          x
#define FLDMAX(x)       FLD_MAX

#endif


#ifdef USE_GCC_ATOMIC

#define ATOMIC_LOAD(ptr) \
  __atomic_load_n(ptr,__ATOMIC_RELAXED)

#define ATOMIC_FETCH_ADD(ptr,val,mo) \
  __atomic_fetch_add(ptr,val,mo)

#define ATOMIC_FETCH_SUB(ptr,val,mo) \
  __atomic_fetch_sub(ptr,val,mo)

#define ATOMIC_CAS(ptr,pold,newval,mo) \
  __atomic_compare_exchange_n(ptr,pold,newval,-1,mo,__ATOMIC_RELAXED)

#else

#define ATOMIC_LOAD(ptr) \
  __sync_fetch_and_add(ptr, 0)

#define ATOMIC_FETCH_ADD(ptr,val,mo) \
  __sync_fetch_and_add(ptr,val)

#define ATOMIC_FETCH_SUB(ptr,val,mo) \
  __sync_fetch_and_sub(ptr,val)

#define XATOMIC_CAS(ptr,oldval,newval,mo) \
  __sync_bool_compare_and_swap(ptr,oldval,newval)

uint32_t ATOMIC_CAS(volatile uint32_t *ptr, uint32_t *pold, uint32_t newval, int mo)
{
  uint32_t ret = XATOMIC_CAS(ptr,*pold,newval,0);
  if (ret) return ret;
  *pold = *ptr;
  return ret;
}

#endif

#define SEM_GLOBAL_RD   0
#define SEM_GLOBAL_WR   1
#define SEM_MAX         2

static
void myassert(uint32_t val, uint32_t expr, const char *file, int line)
{
  if (expr) return;
  fprintf(stderr, "%5d %20lld %08X xxx %s:%d\n",
                  getpid(), mach_absolute_time(),
                  val, file, line);
  fflush(stderr);
  *(volatile char *)0 = 0;
  assert(0);
}

#define ASSERT(x,y)     myassert(x, y, __FILE__, __LINE__)

static
int DoSem(RwLock_t *lok, int sem_num, int numb)
{
#ifdef USE_MACH_SEMA
  if (numb < 0)
    semaphore_wait(lok->sema[sem_num]);
  else
  { while (numb-- > 0)
      semaphore_signal(lok->sema[sem_num]);
  }
  return 0;
#else
  int rc;
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()

  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  if (numb < 0)
  { do
    { rc = semop(lok->semid, &buf, 1);
    } while (rc == -1 && errno == EINTR);
  }
  else
    rc = semop(lok->semid,  &buf, 1);               // doit
  assert(rc == 0);
  return rc;
#endif
}

int rwlock_init(RwLock_t *lok, int maxjob)
{
#ifdef USE_MACH_SEMA
  semaphore_create(mach_task_self(), &lok->sema[SEM_GLOBAL_RD], SYNC_POLICY_FIFO, 0);
  semaphore_create(mach_task_self(), &lok->sema[SEM_GLOBAL_WR], SYNC_POLICY_FIFO, 0);
#else
  int semid, i;
  key_t sem_key;
  u_short sem_val[SEM_MAX];
  union semun semvals;

  sem_key = ftok("/tmp/rwkey", 'W');
  if (sem_key == -1)
  { fprintf(stderr, "ftok(): %s\n", strerror(errno));
    return errno;
  }
  for (i = 0; i < SEM_MAX; i++)
  { sem_val[i] = 0;
  }
  semvals.array = sem_val;

  semid = semget(sem_key, SEM_MAX, (SEM_R|SEM_A|(SEM_R>>3)|(SEM_A>>3)|IPC_CREAT));
  if (semid < 0)
  { fprintf(stderr, "semget(): %s\n", strerror(errno));
    return errno;
  }
  i = semctl(semid, 0, SETALL, semvals);
  if (i < 0)
  { fprintf(stderr, "semctl(): %s\n", strerror(errno));
    i = semctl(semid, 0, (IPC_RMID), NULL);
    return errno;
  }
  lok->semid  = semid;
#endif
  lok->status = 0;
  lok->maxjob = maxjob;
  fprintf(stderr, "C-status=%u\n", lok->status);
  fflush(stderr);
  return 0;
}

int rwlock_finish(RwLock_t *lck)
{ 
#ifdef USE_MACH_SEMA
  semaphore_destroy(mach_task_self(), lck->sema[SEM_GLOBAL_RD]);
  semaphore_destroy(mach_task_self(), lck->sema[SEM_GLOBAL_WR]);
  return 0;
#else
  return semctl(lck->semid, 0, (IPC_RMID), NULL);
#endif
}

void lock_reader(RwLock_t *lok)
{
  Status old_status;
  Status new_status;
  uint32_t retry;

  volatile uint32_t *lck = &lok->status;

  old_status = ATOMIC_LOAD(lck);
  do
  { new_status = old_status;
    if (FLD(old_status,F_WRITERS) > 0)
      FLDINC(new_status,F_WAIT_TO_READ);
    else
      FLDINC(new_status,F_READERS);
  } while (!ATOMIC_CAS(lck, VALPTR(old_status), new_status, __ATOMIC_ACQUIRE));

  if (FLD(old_status,F_WRITERS) > 0)
  { DoSem(lok, SEM_GLOBAL_RD, -1);
  }
}


void unlock_reader(RwLock_t *lok)
{
  Status old_status;
  volatile uint32_t *lck = &lok->status;

  old_status = ATOMIC_FETCH_SUB(lck, BITONE(F_READERS), __ATOMIC_RELEASE);

  ASSERT(old_status, (VAL(old_status) & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_READERS) > 0);
  ASSERT(old_status, FLD(old_status,F_READERS) <= lok->maxjob);
  if ((FLD(old_status,F_READERS) == 1) && (FLD(old_status,F_WRITERS) > 0))
  { DoSem(lok, SEM_GLOBAL_WR, 1);
  }
}


void lock_writer(RwLock_t *lok)
{
  Status old_status;
  volatile uint32_t *lck = &lok->status;

  old_status = ATOMIC_FETCH_ADD(lck, BITONE(F_WRITERS), __ATOMIC_ACQUIRE);

  ASSERT(old_status, (VAL(old_status) & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_WRITERS) + 1 <= FLDMAX(F_WRITERS));
  if ((FLD(old_status,F_READERS) > 0) || (FLD(old_status,F_WRITERS) > 0))
  { DoSem(lok, SEM_GLOBAL_WR, -1);
  }
}

void unlock_writer(RwLock_t *lok)
{
  Status old_status;
  Status new_status;
  uint32_t wait_to_read = 0;
  volatile uint32_t *lck = &lok->status;

  old_status = ATOMIC_LOAD(lck);
  do
  { ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    new_status = old_status;
    FLDDEC(new_status,F_WRITERS);
    wait_to_read = FLD(old_status,F_WAIT_TO_READ);
    if (wait_to_read > 0)
    { SET_FLD(new_status,F_WAIT_TO_READ,0);
      SET_FLD(new_status,F_READERS,wait_to_read);
    }
  } while (!ATOMIC_CAS(lck, VALPTR(old_status), new_status, __ATOMIC_RELEASE));

  if (wait_to_read > 0)
  { DoSem(lok, SEM_GLOBAL_RD, wait_to_read);
  }
  else if (FLD(old_status,F_WRITERS) > 1)
  { DoSem(lok, SEM_GLOBAL_WR, 1);
  }
}
