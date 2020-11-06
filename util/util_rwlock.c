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

extern const char *sem_file;
extern       int   sem_line;
extern     pid_t   mypid;
       const char *rtn;

#ifdef USE_LIBATOMIC_OPS
#define ATOMIC_SYNC     AO_nop_full()
#else
#define ATOMIC_SYNC     __sync_synchronize()
#endif



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


// Checking and profiling versions of semaphore operations
// Source: pahia@t-online.hu

static struct timeval sem_start[SEM_MAX];

#define NUMTRY  (1024*1024)

#ifdef MV1_SHSEM
static
void SpinLockWriter(RWLOCK_T *lok)
{
  int i, j;

#if 0
  for (i = 0; i < NUMTRY; i++) {
    if (TryLockWriter(lok))
      return;
    SchedYield();
  }
#else
  for (i = 0; i < 16; i++)
  { for (j = 0; j < NUMTRY; j++)
      if (TryLockWriter(lok))
        return;
    if (i & 3)
      SchedYield();
  }
#endif
  LockWriter(lok);
}

static
void SpinLockReader(RWLOCK_T *lok)
{
  int i, j;

#if 0
  for (i = 0; i < NUMTRY; i++) {
    if (TryLockReader(lok))
      return;
    SchedYield();
  }
#else
  for (i = 0; i < 16; i++)
  { for (j = 0; j < NUMTRY; j++)
      if (TryLockReader(lok))
        return;
    if (i & 3)
      SchedYield();
  }
#endif
  LockReader(lok);
}
#endif


#ifdef MV1_SHSEM

short TrySemLock(int sem_num, int numb)
{
  short s;

  s = 0;
  if (SEM_GLOBAL == sem_num)
  { if (numb == WRITE)
    { SpinLockWriter(&systab->glorw[0]);
    }
    else if (numb == READ)
    { SpinLockReader(&systab->glorw[0]);
    }
    else
    { char msg[64];
      sprintf(msg, "TrySemLock(): numb=%d", numb);
      panic(msg);
    }
  }
  else {
    LatchLock(&systab->shsem[sem_num]);
    s = 0;
    if (s < 0)
    { panic("TrySemLock: failed");
    }
  }

  return s;
}

short SemLock(int sem_num, int numb)
{
  return TrySemLock(sem_num, numb);
}

short SemUnlock(int sem_num, int numb)
{
  short s;

  s = 0;
  if (SEM_GLOBAL == sem_num)
  { if (numb == -WRITE)
      UnlockWriter(&systab->glorw[0]);
    else if (numb == WR_TO_R)
      UnlockWriterToReader(&systab->glorw[0]);
    else if (numb == -READ)
      UnlockReader(&systab->glorw[0]);
    else
    { char msg[64];
      sprintf(msg, "SemUnLock(): numb=%d", numb);
      panic(msg);
    }
  }
  else {
    LatchUnlock(&systab->shsem[sem_num]);
  }

  return s;
}

#else

short TrySemLock(int sem_num, int numb)
{
  struct sembuf buf={0, 0, SEM_UNDO|IPC_NOWAIT};// for semop()

  if (SEM_GLOBAL == sem_num)                    // adjust sem_num
  { sem_num += volnum - 1;                      //   take volume into account
  }
  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them

  return semop(systab->sem_id, &buf, 1);        // doit
}

short SemLock(int sem_num, int numb)
{
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()

  if (SEM_GLOBAL == sem_num)                    // adjust sem_num
  { sem_num += volnum - 1;                      //   take volume into account
  }
  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them

  return semop(systab->sem_id, &buf, 1);        // doit
}

short SemUnlock(int sem_num, int numb)
{
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()

  if (SEM_GLOBAL == sem_num)                    // adjust sem_num
  { sem_num += volnum - 1;                      //   take volume into account
  }
  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them

  return semop(systab->sem_id, &buf, 1);
}

#endif

// vim:ts=8:sw=8:et
