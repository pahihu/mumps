// Non-recursive read-write lock
// Based on code from Jeff Preshing
// https://github.com/preshing/cpp11-on-multicore
//
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <sys/sem.h>
#include <sys/time.h>
#include <sys/types.h>

#include "mumps.h"
#include "database.h"
#include "proto.h"
#include "rwlock.h"

#ifdef MV1_PTHREAD
#include <pthread.h>
#endif

static u_int          semop_time;

static u_int lrsucc = 0;
static u_int ulrsucc = 0;
static u_int lwsucc = 0;
static u_int ulwsucc = 0;
static u_int ulw2rsucc = 0;

int rwlock_init(void)
{
#ifdef MV1_PTHREAD
  int s;
  pthread_rwlockattr_t attr;

  s = pthread_rwlockattr_init(&attr);
  if (s) return s;
  s = pthread_rwlockattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
  if (s) return s;
  s = pthread_rwlock_init(&systab->gblock, &attr);
  if (s) return s;
  s = pthread_rwlockattr_destroy(&attr);
  if (s) return s;
#endif
  return 0;
}

static
int Semop(int semid, struct sembuf *sops, size_t nsops)
{
  int s;
#ifdef MV1_PROFILE
  struct timeval st, et;

  gettimeofday(&st, NULL);
#endif
  s = semop(semid, sops, nsops);           // doit
#ifdef MV1_PROFILE
  gettimeofday(&et, NULL);
  semop_time = 1000000 * (et.tv_sec  - st.tv_sec) +
                         (et.tv_usec - st.tv_usec);
#endif
  return s;
}

static
short DoSem(int sem_num, int numb)
{
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()

  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  return semop(systab->sem_id, &buf, 1);        // doit
}

#ifdef MV1_SHSEM

#if defined(__APPLE__) && !defined(MV1_GCC_SYNC)

#include <libkern/OSAtomic.h>
#define ATOMIC_FETCH_ADD(ptr,fld) \
  (OSAtomicAdd32Barrier(BITONE(fld),(volatile int32_t*)ptr) - BITONE(fld))

#define ATOMIC_FETCH_SUB(ptr,fld) \
  (OSAtomicAdd32Barrier(-BITONE(fld),(volatile int32_t*)ptr) + BITONE(fld))

#define ATOMIC_CAS(ptr,oldval,newval) \
  OSAtomicCompareAndSwap32Barrier(oldval,newval,(volatile int32_t*)ptr)

#define ATOMIC_SYNC \
  OSMemoryBarrier()

#define ATOMIC_INC(var) \
  OSAtomicIncrement32(&var)

#define ATOMIC_FETCH(ptr) \
  OSAtomicAdd32Barrier(0, (volatile int32_t*)ptr)

#else

#define ATOMIC_FETCH_ADD(ptr,fld) \
  __sync_fetch_and_add(ptr,BITONE(fld))

#define ATOMIC_FETCH_SUB(ptr,fld) \
  __sync_fetch_and_sub(ptr,BITONE(fld))

#define ATOMIC_CAS(ptr,oldval,newval) \
  __sync_bool_compare_and_swap(ptr,oldval,newval)

#define ATOMIC_SYNC \
  __sync_synchronize()

#define ATOMIC_INC(var) \
  __sync_fetch_and_add(&var,1)

#define ATOMIC_FETCH(ptr) \
  __sync_fetch_and_add(ptr,0)

#endif


#define F_BIT0            0
#define F_WRITERS         0
#define F_WAIT_TO_READ    8
#define F_READERS        16
#define FLD_WIDTH       255
#define BITONE(y)       (1 << (y))
#define FLDINC(x,y)     x = (x) + BITONE(y)
#define FLDDEC(x,y)     x = (x) - BITONE(y)
#define FLD(x,y)        (FLD_WIDTH & ((x) >> (y)))
#define SET_FLD(z,x,y)  { z &= ~(FLD_WIDTH << (x)); z |= (FLD_WIDTH & (y)) << (x); }

extern const char *sem_file;
extern       int   sem_line;
       const char *rtn;

static int32_t opseq = 0;

static
void myassert(u_int val, u_int expr, const char *file, int line)
{
  if (expr) return;
  fprintf(stderr, "Assertion failed: opseq=%d val=%08X at %s() %s:%d, SemOp() at %s:%d\r\n",
                  opseq, val, rtn, file, line, sem_file, sem_line);
  fflush(stderr);
  fprintf(stderr, "  LRsucc = %u\r\n", lrsucc);
  fprintf(stderr, "  URsucc = %u\r\n", ulrsucc);
  fprintf(stderr, "  LWsucc = %u\r\n", lwsucc);
  fprintf(stderr, "  UWsucc = %u\r\n", ulwsucc);
  fprintf(stderr, "UW2Rsucc = %u\r\n", ulw2rsucc);
  fflush(stderr);
  assert(0);
}

#define ASSERT(x,y)     myassert(x, y, __FILE__, __LINE__)

void lock_reader(volatile u_int *lck)
{
#ifdef MV1_PTHREAD
  pthread_rwlock_rdlock(&systab->gblock);
#else
  u_int old_status;
  u_int new_status;
  u_int retry;

  rtn = "lock_reader";
  ATOMIC_INC(opseq);

  ASSERT(curr_lock, curr_lock == 0);

  do
  { old_status = ATOMIC_FETCH(lck);
    ASSERT(old_status, (old_status & 0xFF000000U) == 0);
    new_status = old_status;
    if (FLD(old_status,F_WRITERS) > 0)
      FLDINC(new_status,F_WAIT_TO_READ);
    else
      FLDINC(new_status,F_READERS);
  } while (!ATOMIC_CAS(lck, old_status, new_status));

  if (FLD(old_status,F_WRITERS) > 0)
  { DoSem(SEM_GLOBAL_RD, -1);
    retry = 0;
    do
    { old_status = ATOMIC_FETCH(lck);
    // } while ((0 == FLD(old_status,F_READERS)) && (++retry < (1000*1000)));
    } while (0);
    // fprintf(stderr, "  lock_reader: opseq=%d status=%08X retry=%u\r\n",
    //                 opseq, old_status, retry);
    // fflush(stderr);
    ASSERT(old_status, FLD(old_status,F_READERS) > 0);
    lrsucc++;
  }
#endif
}


void unlock_reader(volatile u_int *lck)
{
#ifdef MV1_PTHREAD
  pthread_rwlock_unlock(&systab->gblock);
#else
  u_int old_status;

  rtn = "unlock_reader";
  ATOMIC_INC(opseq);

  ASSERT(curr_lock, curr_lock == READ);

  old_status = ATOMIC_FETCH_SUB(lck, F_READERS);
  ASSERT(old_status, (old_status & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_READERS) > 0);
  if ((FLD(old_status,F_READERS) == 1) && (FLD(old_status,F_WRITERS) > 0))
  { // fprintf(stderr, "unlock_reader: opseq=%d status=%08X\r\n",
    //                  opseq, old_status);
    // fflush(stderr);
    DoSem(SEM_GLOBAL_WR, 1);
    old_status = ATOMIC_FETCH(lck);
    ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    ulrsucc++;
  }
#endif
}


void lock_writer(volatile u_int *lck)
{
#ifdef MV1_PTHREAD
  pthread_rwlock_wrlock(&systab->gblock);
#else
  u_int nreaders;
  u_int old_status;
  u_int retry;

  rtn = "lock_writer";
  ATOMIC_INC(opseq);

  ASSERT(curr_lock, curr_lock == 0);

  old_status = ATOMIC_FETCH_ADD(lck, F_WRITERS);
  ASSERT(old_status, (old_status & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_WRITERS) + 1 <= FLD_WIDTH);
  if ((FLD(old_status,F_READERS) > 0) || (FLD(old_status,F_WRITERS) > 0))
  { DoSem(SEM_GLOBAL_WR, -1);
    retry = 0;
    do
    { old_status = ATOMIC_FETCH(lck);
    // } while (FLD(old_status,F_READERS) && ++retry < (1000*1000));
    } while (0);
    // fprintf(stderr, "  lock_writer: opseq=%d status=%08X retry=%u\r\n",
    //                 opseq, old_status, retry);
    // fflush(stderr);
    ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    lwsucc++;
  }
#endif
}

void unlock_writer(volatile u_int *lck)
{
#ifdef MV1_PTHREAD
  pthread_rwlock_unlock(&systab->gblock);
#else
  u_int old_status;
  u_int new_status;
  u_int wait_to_read = 0;

  rtn = "unlock_writer";
  ATOMIC_INC(opseq);

  ASSERT(curr_lock, curr_lock == WRITE);

  do
  { old_status = ATOMIC_FETCH(lck);
    ASSERT(old_status, (old_status & 0xFF000000U) == 0);
    ASSERT(old_status, FLD(old_status,F_WRITERS) >  0);
    ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    new_status = old_status;
    FLDDEC(new_status,F_WRITERS);
    wait_to_read = FLD(old_status,F_WAIT_TO_READ);
    if (wait_to_read > 0)
    { SET_FLD(new_status,F_WAIT_TO_READ,0);
      SET_FLD(new_status,F_READERS,wait_to_read);
    }
  } while (!ATOMIC_CAS(lck, old_status, new_status));

  if (wait_to_read > 0)
  { DoSem(SEM_GLOBAL_RD, wait_to_read);
  }
  else if (FLD(old_status,F_WRITERS) > 1)
  { DoSem(SEM_GLOBAL_WR, 1);
    old_status = ATOMIC_FETCH(lck);
    ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    ulwsucc++;
  }
#endif
}

// based on unlock_writer from Preshing above
void unlock_writer_to_reader(volatile u_int *lck)
{
#ifdef MV1_PTHREAD
  return;
#else
  u_int old_status;
  u_int new_status;
  u_int wait_to_read = 0;

  rtn = "unlock_writer_to_reader";
  ATOMIC_INC(opseq);

  ASSERT(curr_lock, curr_lock == WRITE);

  do
  { old_status = ATOMIC_FETCH(lck);
    ASSERT(old_status, (old_status & 0xFF000000U) == 0);
    ASSERT(old_status, FLD(old_status,F_WRITERS) >  0);
    ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    new_status = old_status;
    FLDDEC(new_status,F_WRITERS);
    wait_to_read = FLD(old_status,F_WAIT_TO_READ);
    if (wait_to_read > 0)
    { SET_FLD(new_status,F_WAIT_TO_READ,0);
      SET_FLD(new_status,F_READERS,1 + wait_to_read);
    }
    else
    { SET_FLD(new_status,F_WAIT_TO_READ,0);
      SET_FLD(new_status,F_READERS,1);
    }
  } while (!ATOMIC_CAS(lck, old_status, new_status));

  if (wait_to_read > 0)
  { DoSem(SEM_GLOBAL_RD, wait_to_read);
  }
  ulw2rsucc++;
#endif
}
#endif


// Checking and profiling versions of semaphore operations
// Source: pahia@t-online.hu

static struct timeval sem_start[SEM_MAX];

short TrySemLock(int sem_num, int numb)
{
  short s;
  int dosemop;
  struct sembuf buf={0, 0, SEM_UNDO|IPC_NOWAIT};// for semop()

  s = 0;
#ifdef MV1_SHSEM
  dosemop = 0;
  if (SEM_GLOBAL == sem_num)
  { if (numb == WRITE)
      lock_writer(&systab->shsem[SEM_GLOBAL]);
    else if (numb == READ)
      lock_reader(&systab->shsem[SEM_GLOBAL]);
    else
    { char msg[64];
      sprintf(msg, "TrySemLock(): numb=%d", numb);
      panic(msg);
    }
  }
  else {
    u_int old_status = ATOMIC_FETCH_ADD(&systab->shsem[sem_num], F_BIT0);
    if (old_status > 0)
    { dosemop = 1;
    }
  }

  if (dosemop)
#endif
  { buf.sem_num = (u_short) sem_num;            // get the one we want
    buf.sem_op = (short) numb;                  // and the number of them
    s = Semop(systab->sem_id, &buf, 1);         // doit
  }
  return s;
}

short SemLock(int sem_num, int numb)
{
  short s;
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()
  int x;
  u_int semop_time_sav;

  x = 2*sem_num;
  if (-1 == numb)       // READ lock
    x += 1;

  semop_time = 0;
  s = TrySemLock(sem_num, numb);
  semop_time_sav = semop_time; 
  if (s != 0)
  { buf.sem_num = (u_short) sem_num;            // get the one we want
    buf.sem_op = (short) numb;                  // and the number of them
    s = Semop(systab->sem_id, &buf, 1);         // doit
  }

// #define MV1_PROFILE 1
#ifdef MV1_PROFILE
  if (s == 0)
  { semtab[x].semop_time += semop_time_sav;
    gettimeofday(&sem_start[sem_num], NULL);
  }
#endif
  return s;
}
 
short SemUnlock(int sem_num, int numb)
{
  struct timeval tv;
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()
  short s;
  u_int curr_held_time;
  int x, dosemop;

  x = 2*sem_num + (1 == abs(numb) ? 1 : 0);

  s = 0;
#ifdef MV1_SHSEM
  dosemop = 0;
  if (SEM_GLOBAL == sem_num)
  { if (numb == -WRITE)
      unlock_writer(&systab->shsem[SEM_GLOBAL]);
    else if (numb == WR_TO_R)
      unlock_writer_to_reader(&systab->shsem[SEM_GLOBAL]);
    else if (numb == -READ)
      unlock_reader(&systab->shsem[SEM_GLOBAL]);
    else
    { char msg[64];
      sprintf(msg, "SemUnLock(): numb=%d", numb);
      panic(msg);
    }
  }
  else {
    u_int old_status = ATOMIC_FETCH_SUB(&systab->shsem[sem_num], F_BIT0);
    if (old_status > 1)
    { dosemop = 1;
    }
  }

  if (dosemop)
#endif
  { buf.sem_num = (u_short) sem_num;            // get the one we want
    buf.sem_op = (short) numb;                  // and the number of them
    s = Semop(systab->sem_id, &buf, 1);
  }

#ifdef MV1_PROFILE
  gettimeofday(&tv, NULL);
  curr_held_time = 1000000 * (tv.tv_sec  - sem_start[sem_num].tv_sec) +
                             (tv.tv_usec - sem_start[sem_num].tv_usec);
  semtab[x].held_time += curr_held_time;
  sem_start[sem_num] = tv;
  semtab[x].semop_time += semop_time;
#endif

  semtab[x].held_count++;
  return s;
}

void UTIL_Barrier(void)
{
  do { asm volatile ("mfence":::"memory"); } while(0);
}

//	struct sembuf {
//		   u_short sem_num;        /* semaphore # */
//		   short   sem_op;         /* semaphore operation */
//		   short   sem_flg;        /* operation flags */
//	};

