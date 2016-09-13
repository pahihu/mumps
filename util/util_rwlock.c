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
#include <libkern/OSAtomic.h>

#include "mumps.h"
#include "database.h"
#include "proto.h"
#include "rwlock.h"

static u_int          semop_time;

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

#define ATOMIC_ADD_FETCH(ptr,val) \
        OSAtomicAdd32Barrier(val,(volatile int32_t*)ptr)

#define ATOMIC_SUB_FETCH(ptr,val) \
        OSAtomicAdd32Barrier(-(val),(volatile int32_t*)ptr)

#define ATOMIC_CAS(ptr,oldval,newval) \
        OSAtomicCompareAndSwap32Barrier(oldval,newval,(volatile int32_t*)ptr)

#define ATOMIC_SYNC \
        OSMemoryBarrier()


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

static
void myassert(u_int val, u_int expr, const char *file, int line)
{
  if (expr) return;
  fprintf(stderr, "Assertion failed: val=%08X at %s:%d, SemOp() at %s:%d\r\n",
                  val, file, line, sem_file, sem_line);
  assert(0);
}

#define ASSERT(x,y)     myassert(x, y, __FILE__, __LINE__)

u_int ATOMIC_FETCH(volatile u_int *lck)
{
  u_int t = 0;

  while (!ATOMIC_CAS(&t, 0, systab->shsem[SEM_GLOBAL]));
  return t;
}

void lock_reader(volatile u_int *lck)
{
  u_int old_status;
  u_int new_status;

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
  { // fprintf(stderr, "lock_reader: waiting\r\n");
    // fflush(stderr);
    // ATOMIC_FETCH(old_status,lck);
    // ASSERT(old_status, FLD(old_status,F_WRITERS) > 0);
    // ASSERT(old_status, FLD(old_status,F_WAIT_TO_READ) > 0);
    DoSem(SEM_GLOBAL_RD, -1);
  }

#ifndef NDEBUG
  // ATOMIC_FETCH(old_status,lck);
  // ASSERT(old_status, FLD(old_status,F_READERS) > 0);
#endif
}


void unlock_reader(volatile u_int *lck)
{
  u_int old_status;

  ASSERT(curr_lock, curr_lock == READ);

#ifndef NDEBUG
  old_status = ATOMIC_FETCH(lck); 
  ASSERT(old_status, FLD(old_status,F_READERS) > 0);
#endif
  old_status = ATOMIC_SUB_FETCH(lck, BITONE(F_READERS));
  ASSERT(old_status, (old_status & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_READERS) < systab->maxjob);
  // fprintf(stderr, "unlock_reader: %08X\r\n", old_status);
  // fflush(stderr);
  if ((FLD(old_status,F_READERS) == 0) && (FLD(old_status,F_WRITERS) > 0))
  { // ATOMIC_FETCH(old_status,lck);
    // ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    // ASSERT(old_status, FLD(old_status,F_WRITERS) > 0);
    DoSem(SEM_GLOBAL_WR, 1);
    // ATOMIC_FETCH(old_status,lck);
    // ASSERT(old_status, FLD(old_status,F_READERS) == 0);
    // ASSERT(old_status, FLD(old_status,F_WRITERS) > 0);
  }
}


void lock_writer(volatile u_int *lck)
{
  int dowait;
  u_int nreaders;
  u_int old_status;

  ASSERT(curr_lock, curr_lock == 0);

  old_status = ATOMIC_ADD_FETCH(lck, BITONE(F_WRITERS));
  ASSERT(old_status, (old_status & 0xFF000000U) == 0);
  ASSERT(old_status, FLD(old_status,F_WRITERS) <= FLD_WIDTH);
  dowait = 0;
  if ((FLD(old_status,F_READERS) > 0) || (FLD(old_status,F_WRITERS) > 1))
  { // fprintf(stderr, "lock_writer: waiting\r\n");
    // fflush(stderr);
    dowait = 1;
    DoSem(SEM_GLOBAL_WR, -1);
  }
#ifndef NDEBUG
  // ATOMIC_FETCH(old_status,lck);
  // ASSERT(old_status, FLD(old_status,F_WRITERS) > 0);
#endif
  // fprintf(stderr, "lock_writer: %08X\r\n", *lck);
  // fflush(stderr);
  // ATOMIC_FETCH(old_status,lck);
  // ASSERT(old_status, (old_status & 0xFF000000U) == 0);
  // nreaders = FLD(old_status,F_READERS);
  // if (nreaders)
  // { fprintf(stderr, "lock_writer: old_status=%08X dowait=%d\r\n",
  //                    old_status, dowait);
  //   fflush(stderr);
  //   ASSERT(old_status, nreaders == 0);
  // }
}

void unlock_writer(volatile u_int *lck)
{
  u_int old_status;
  u_int new_status;
  u_int wait_to_read = 0;

  ASSERT(curr_lock, curr_lock == WRITE);

  do
  { old_status = ATOMIC_FETCH(lck);
#ifndef NDEBUG
    ASSERT(old_status, FLD(old_status,F_WRITERS) > 0);
#endif
    ASSERT(old_status, (old_status & 0xFF000000U) == 0);
    if (FLD(old_status,F_READERS) != 0)
    { ATOMIC_SUB_FETCH(lck,BITONE(F_WRITERS));
      ASSERT(old_status, FLD(old_status,F_READERS) == 0); // XXX
    }
    new_status = old_status;
    FLDDEC(new_status,F_WRITERS);
    wait_to_read = FLD(old_status,F_WAIT_TO_READ);
    if (wait_to_read > 0)
    { SET_FLD(new_status,F_WAIT_TO_READ,0);
      SET_FLD(new_status,F_READERS,wait_to_read);
    }
  } while (!ATOMIC_CAS(lck, old_status, new_status));

  if (wait_to_read > 0)
    DoSem(SEM_GLOBAL_RD, wait_to_read);
  else if (FLD(old_status,F_WRITERS) > 0)
    DoSem(SEM_GLOBAL_WR, 1);
}

// based on unlock_writer from Preshing above
void unlock_writer_to_reader(volatile u_int *lck)
{
  u_int old_status;
  u_int new_status;
  u_int wait_to_read = 0;

  ASSERT(curr_lock, curr_lock == WRITE);

  do
  { old_status = ATOMIC_FETCH(lck);
#ifndef NDEBUG
    ASSERT(old_status, FLD(old_status,F_WRITERS) > 0);
#endif
    ASSERT(old_status, (old_status & 0xFF000000U) == 0);
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
    DoSem(SEM_GLOBAL_RD, wait_to_read);
#ifndef NDEBUG
  // ATOMIC_FETCH(old_status,lck);
  // ASSERT(old_status, FLD(old_status,F_READERS) > 0);
#endif
  // else if (old_status.writers > 1)
  // SemOp(SEM_GLOBAL_WRITE, -WRITE);
}


// Checking and profiling versions of semaphore operations
// Source: pahia@t-online.hu

static struct timeval sem_start[SEM_MAX];

short TrySemLock(int sem_num, int numb)
{
  short s;
  int dosemop;
  struct sembuf buf={0, 0, SEM_UNDO|IPC_NOWAIT};// for semop()

  s = 0; dosemop = 0;
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
  else if (ATOMIC_ADD_FETCH(&systab->shsem[sem_num], 1) > 1)
    dosemop = 1;

  if (dosemop)
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

  s = 0; dosemop = 0;
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
  else if (ATOMIC_SUB_FETCH(&systab->shsem[sem_num], 1) > 0)
    dosemop = 1;

  if (dosemop)
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

//	struct sembuf {
//		   u_short sem_num;        /* semaphore # */
//		   short   sem_op;         /* semaphore operation */
//		   short   sem_flg;        /* operation flags */
//	};

