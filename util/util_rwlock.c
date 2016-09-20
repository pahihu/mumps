// Non-recursive read-write lock
// Based on code from Jeff Preshing
// https://github.com/preshing/cpp11-on-multicore
//
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <math.h>
#include <sys/sem.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "mumps.h"
#include "database.h"
#include "proto.h"
#include "rwlock.h"


static u_int          semop_time;

static u_int lrsucc = 0, lros = 0;
static u_int ulrsucc = 0;
static u_int lwsucc = 0, lwos = 0;
static u_int ulwsucc = 0;
static u_int ulw2rsucc = 0;

extern const char *sem_file;
extern       int   sem_line;
extern     pid_t   mypid;
       const char *rtn;

#ifdef __APPLE__
#include <mach/mach_time.h>
#define monotonic_time  mach_absolute_time
#endif
#ifdef __linux__
#include <time.h>
u_int64 monotonic_time(void)
{ struct timespec time;
  clock_gettime(CLOCK_MONOTONIC, &time);
  return (u_int64)time.tv_sec * 1000000000 + time.tv_nsec;
}
#endif

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
  int rc;
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()

  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  if (numb < 0)
  { do
    { rc = semop(systab->sem_id, &buf, 1);
    } while (rc == -1 && rc == EINTR);
  }
  else
    rc = semop(systab->sem_id, &buf, 1);        // doit
  assert(rc == 0); 
  return rc;
}

#define MAX_NTRCBUF     1024

typedef struct TRCITEM
{ u_int64  stamp;
  uint32_t status;
  int      numb;
  const char *file;
  int      line;
} trcitem_t;

trcitem_t trcbuf[MAX_NTRCBUF];
int ntrcbuf = 0;

static
void mv1_log(uint32_t old_status, int numb)
{
  trcbuf[ntrcbuf].stamp  = monotonic_time();
  trcbuf[ntrcbuf].status = old_status;
  trcbuf[ntrcbuf].numb   = numb;
  trcbuf[ntrcbuf].file   = sem_file;
  trcbuf[ntrcbuf].line   = sem_line;
  ntrcbuf++; ntrcbuf &= (MAX_NTRCBUF - 1);
}

void mv1_log_init(void)
{ int i;

  for (i = 0; i < MAX_NTRCBUF; i++)
    trcbuf[i].stamp = 0;
  ntrcbuf = 0;
}

void mv1_log_flush(void)
{ int i;

  for (i = 0; i < MAX_NTRCBUF; i++)
  { trcitem_t *t = &trcbuf[i];
    if (0 == t->stamp) continue;
    fprintf(stderr,"%5d %16lld %08X %3d   %s:%d\r\n",
                  mypid, t->stamp,
                  t->status, t->numb, t->file, t->line);
  }
}

#ifdef MV1_SHSEM

#if defined(__APPLE__) && !defined(MV1_GCC_SYNC)

#include <libkern/OSAtomic.h>
#define _ATOMIC_FETCH_ADD(ptr,fld) \
  (OSAtomicAdd32Barrier(BITONE(fld),(volatile int32_t*)ptr) - BITONE(fld))

#define _ATOMIC_FETCH_SUB(ptr,fld) \
  (OSAtomicAdd32Barrier(-BITONE(fld),(volatile int32_t*)ptr) + BITONE(fld))

#define _ATOMIC_CAS(ptr,oldval,newval) \
  OSAtomicCompareAndSwap32Barrier(oldval,newval,(volatile int32_t*)ptr)

#define ATOMIC_SYNC \
  OSMemoryBarrier()

#define ATOMIC_INC(var) \
  OSAtomicIncrement32(&var)

#else

#define _ATOMIC_FETCH_ADD(ptr,fld) \
  __sync_fetch_and_add(ptr,BITONE(fld))

#define _ATOMIC_FETCH_SUB(ptr,fld) \
  __sync_fetch_and_sub(ptr,BITONE(fld))

#define _ATOMIC_CAS(ptr,oldval,newval) \
  __sync_bool_compare_and_swap(ptr,oldval,newval)

#define ATOMIC_SYNC \
  __sync_synchronize()

#define ATOMIC_INC(var) \
  __sync_fetch_and_add(&var,1)

#endif


#define Status          uint32_t
#define F_BIT0           0
#define F_WRITERS       20
#define F_WAIT_TO_READ  10
#define F_READERS        0
#define FLD_WIDTH       10
#define BITONE(x)       ((uint32_t) (1 << (x)))
#define FLD_MAX         ((uint32_t) BITONE(FLD_WIDTH) - 1)
#define FLDINC(x,y)     x += BITONE(y)
#define FLDDEC(x,y)     x -= BITONE(y)
#define FLD(x,y)        (FLD_MAX & ((x) >> (y)))
#define SET_FLD(z,x,y)  z = (z & ~(FLD_MAX << x)) | ((y) << x)
#define VALPTR(x)       &(x)
#define VAL(x)          (x)
#define FLDMAX(x)       FLD_MAX

static
void myassert(uint32_t val, uint32_t expr, const char *file, int line)
{
  if (expr) return;
  fprintf(stderr, "%5d %20lld %08X xxx %s:%d %s()\r\n",
                  mypid, monotonic_time(),
                  val, file, line, rtn);
  fprintf(stderr, "  LRsucc = %u (%u)\r\n", lrsucc, lros);
  fprintf(stderr, "  URsucc = %u\r\n", ulrsucc);
  fprintf(stderr, "  LWsucc = %u (%u)\r\n", lwsucc, lwos);
  fprintf(stderr, "  UWsucc = %u\r\n", ulwsucc);
  fprintf(stderr, "UW2Rsucc = %u\r\n", ulw2rsucc);
  mv1_log_flush();
  fflush(stderr);
  assert(0);
}

#define ASSERT(x,y)     myassert(x, y, __FILE__, __LINE__)
#define ASSERT0(x,y)

// #define MV1_GCC_ATOMIC  1
#ifdef MV1_GCC_ATOMIC
#define ATOMIC_LOAD(ptr) \
  __atomic_load_n(ptr,__ATOMIC_RELAXED)

#define ATOMIC_FETCH_ADD(ptr,val,mo) \
  __atomic_fetch_add(ptr,val,mo)

#define ATOMIC_FETCH_SUB(ptr,val,mo) \
  __atomic_fetch_sub(ptr,val,mo)

#define ATOMIC_CAS(ptr,pold,newval,mo) \
  __atomic_compare_exchange_n(ptr,pold,newval,-1,mo,__ATOMIC_RELAXED)

#else

uint32_t ATOMIC_FETCH_ADD(volatile uint32_t *ptr, uint32_t val)
{
  uint32_t ret;
  ATOMIC_SYNC;
  ret = _ATOMIC_FETCH_ADD(ptr,val);
  ATOMIC_SYNC;
  return ret;
}

uint32_t ATOMIC_FETCH_SUB(volatile uint32_t *ptr, uint32_t val)
{
  uint32_t ret;
  ATOMIC_SYNC;
  ret = _ATOMIC_FETCH_SUB(ptr,val);
  ATOMIC_SYNC;
  return ret;
}

uint32_t ATOMIC_CAS(volatile uint32_t *ptr, uint32_t *poldval, uint32_t newval)
{
  uint32_t pval, oldval;

  ATOMIC_SYNC;
  oldval = *poldval;
  pval   = *ptr;
  do
  { if (pval != oldval)
      goto fail;
  } while(!_ATOMIC_CAS(ptr,oldval,newval));
  ATOMIC_SYNC;
  return (uint32_t)-1;
fail:
  *poldval = *ptr;
  return 0;
}

uint32_t ATOMIC_LOAD(volatile uint32_t *lck)
{
  uint32_t t = 0;

  // ATOMIC_SYNC;
  // _ATOMIC_CAS(&t,0,*lck);
  return *lck;
}
#endif

void lock_reader(volatile uint32_t *lck)
{
  Status old_status, new_status;

  rtn = "  lock_reader";
  ASSERT(curr_lock, curr_lock == 0);

  old_status = ATOMIC_LOAD(lck);
  do
  { ASSERT(old_status, (VAL(old_status) & 0xFF000000U) == 0);
    new_status = old_status;
    if (FLD(old_status,F_WRITERS) > 0)
      FLDINC(new_status,F_WAIT_TO_READ);
    else
      FLDINC(new_status,F_READERS);
  } while (!ATOMIC_CAS(lck, VALPTR(old_status), new_status, __ATOMIC_ACQUIRE));
  mv1_log(old_status, READ);

  if (FLD(old_status,F_WRITERS) > 0)
  { DoSem(SEM_GLOBAL_RD, -1);
    lros++;
  }
  lrsucc++;
}


void unlock_reader(volatile uint32_t *lck)
{
  Status old_status;

  rtn = "unlock_reader";
  ASSERT(curr_lock, curr_lock == READ);

  old_status = ATOMIC_FETCH_SUB(lck, BITONE(F_READERS), __ATOMIC_RELEASE);
  mv1_log(old_status, -READ);

  ASSERT(old_status, (VAL(old_status) & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_READERS) > 0);
  if ((FLD(old_status,F_READERS) == 1) && (FLD(old_status,F_WRITERS) > 0))
  { DoSem(SEM_GLOBAL_WR, 1);
  }
  ulrsucc++;
}


void lock_writer(volatile uint32_t *lck)
{
  Status old_status;

  rtn = "  lock_writer";
  ASSERT(curr_lock, curr_lock == 0);

  old_status = ATOMIC_FETCH_ADD(lck, BITONE(F_WRITERS), __ATOMIC_ACQUIRE);
  mv1_log(old_status, WRITE);

  ASSERT(old_status, (VAL(old_status) & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_WRITERS) + 1 <= FLD_WIDTH);
  if ((FLD(old_status,F_READERS) > 0) || (FLD(old_status,F_WRITERS) > 0))
  { DoSem(SEM_GLOBAL_WR, -1);
    lwos++;
  }
  lwsucc++;
}

void unlock_writer(volatile uint32_t *lck)
{
  Status old_status, new_status;
  uint32_t wait_to_read = 0;

  rtn = "unlock_writer";
  ASSERT(curr_lock, curr_lock == WRITE);

  old_status = ATOMIC_LOAD(lck);
  do
  { ASSERT(old_status, (VAL(old_status) & 0xFF000000U) == 0);
    ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    new_status = old_status;
    FLDDEC(new_status,F_WRITERS);
    wait_to_read = FLD(old_status,F_WAIT_TO_READ);
    if (wait_to_read > 0)
    { SET_FLD(new_status,F_WAIT_TO_READ,0);
      SET_FLD(new_status,F_READERS,wait_to_read);
    }
  } while (!ATOMIC_CAS(lck, VALPTR(old_status), new_status, __ATOMIC_RELEASE));
  mv1_log(old_status, -WRITE);

  if (wait_to_read > 0)
  { DoSem(SEM_GLOBAL_RD, wait_to_read);
  }
  else if (FLD(old_status,F_WRITERS) > 1)
  { DoSem(SEM_GLOBAL_WR, 1);
  }
  ulwsucc++;
}

// based on unlock_writer from Preshing above
void unlock_writer_to_reader(volatile uint32_t *lck)
{
  Status old_status, new_status;
  uint32_t wait_to_read = 0;

  rtn = "unlock_writer_to_reader";
  ASSERT(curr_lock, curr_lock == WRITE);

  old_status = ATOMIC_LOAD(lck);
  do
  { ASSERT(old_status, (VAL(old_status) & 0xFF000000U) == 0);
    ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    new_status = old_status;
    FLDDEC(new_status,F_WRITERS);
    wait_to_read = FLD(old_status,F_WAIT_TO_READ);
    SET_FLD(new_status,F_WAIT_TO_READ,0);
    SET_FLD(new_status,F_READERS,1 + wait_to_read);
  } while (!ATOMIC_CAS(lck, VALPTR(old_status), new_status, __ATOMIC_RELEASE));
  mv1_log(old_status, WR_TO_R);

  if (wait_to_read > 0)
  { DoSem(SEM_GLOBAL_RD, wait_to_read);
  }
  ulw2rsucc++;
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
    if (ATOMIC_FETCH_ADD(&systab->shsem[sem_num], BITONE(F_BIT0), __ATOMIC_ACQUIRE) > 0)
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
    if (ATOMIC_FETCH_SUB(&systab->shsem[sem_num], BITONE(F_BIT0), __ATOMIC_RELEASE) > 1)
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
#ifdef MV1_SHSEM
  do { ATOMIC_SYNC; } while(0);
#else
  return;
#endif
}

//	struct sembuf {
//		   u_short sem_num;        /* semaphore # */
//		   short   sem_op;         /* semaphore operation */
//		   short   sem_flg;        /* operation flags */
//	};

