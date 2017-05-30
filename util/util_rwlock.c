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

#ifdef USE_LIBATOMIC_OPS
#define ATOMIC_SYNC     AO_nop_full()
#else
#define ATOMIC_SYNC     __sync_synchronize()
#endif

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

#endif


// Checking and profiling versions of semaphore operations
// Source: pahia@t-online.hu

static struct timeval sem_start[SEM_MAX];

short TrySemLock(int sem_num, int numb)
{
  short s;
  int dosemop;
  struct sembuf buf={0, 0, SEM_UNDO|IPC_NOWAIT};// for semop()
#ifdef MV1_PROFILE
  struct timeval st, et;

  gettimeofday(&st, NULL);
#endif

  s = 0;
#ifdef MV1_SHSEM
  dosemop = 0;
  if (SEM_GLOBAL == sem_num)
  { if (numb == WRITE)
      LockWriter(&systab->glorw);
    else if (numb == READ)
      LockReader(&systab->glorw);
    else
    { char msg[64];
      sprintf(msg, "TrySemLock(): numb=%d", numb);
      panic(msg);
    }
  }
  else {
    // s = LatchLock(&systab->shsem[sem_num]);
    LatchLock(&systab->shsem[sem_num]);
    s = 0;
    if (s < 0)
    { panic("TrySemLock: failed");
    }
  }

  if (dosemop)
#endif
  { buf.sem_num = (u_short) sem_num;            // get the one we want
    buf.sem_op = (short) numb;                  // and the number of them
    s = Semop(systab->sem_id, &buf, 1);         // doit
  }
#ifdef MV1_PROFILE
  gettimeofday(&et, NULL);
  semop_time = 1000000 * (et.tv_sec  - st.tv_sec) +
                         (et.tv_usec - st.tv_usec);
#endif
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
      UnlockWriter(&systab->glorw);
    else if (numb == WR_TO_R)
      UnlockWriterToReader(&systab->glorw);
    else if (numb == -READ)
      UnlockReader(&systab->glorw);
    else
    { char msg[64];
      sprintf(msg, "SemUnLock(): numb=%d", numb);
      panic(msg);
    }
  }
  else {
    LatchUnlock(&systab->shsem[sem_num]);
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

