// File: mumps/util/util_share.c
//
// module MUMPS util_share - shared memory

/*      Copyright (c) 1999 - 2014
 *      Raymond Douglas Newman.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of Raymond Douglas Newman nor the names of the
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <string.h>
#include <strings.h>                            // for bzero()
#include <sys/types.h>                          // for u_char def
#include <errno.h>                              // error stuf
#include <sched.h>                              // for sched_yield()
#include <sys/ipc.h>                            // shared memory
#include <sys/shm.h>                            // shared memory
#include <sys/sem.h>                            // semaphores
#include <sys/time.h>                           // for gettimeofday()
#include <unistd.h>                             // for usleep()
#include "mumps.h"                              // standard includes
#include "error.h"                              // standard includes
#include "proto.h"                              // standard includes
#include "database.h"                           // for MTIME()

extern int curr_lock;				// for tracking SEM_GLOBAL

//****************************************************************************
//**  Function: UTIL_Share - attach shared memory section        ***
//**  returns addr (or NULL on error)                            ***
int UTIL_Share(char *dbf)                     	// pointer to dbfile name
{ key_t shar_mem_key;                           // memory "key"
  int shar_mem_id;                              // memory id
  int sem_id;					// semaphore id
  int i;
  systab_struct *sad;				// systab address
  shar_mem_key = ftok(dbf, MUMPS_SYSTEM);       // get a unique key
  if (shar_mem_key == -1) return (errno);       // die on error
  shar_mem_id = shmget(shar_mem_key, 0, 0);     // attach to existing share
  if (shar_mem_id == -1) return (errno);        // die on error
  sad = shmat(shar_mem_id, SHMAT_SEED, 0);  	// map it
  if (sad == (void *)-1) 	                // die on error
  { i = errno;
    fprintf(stderr, "Unable to attach to systab correctly\n"); // give error
    return(i);                                  // and return with error
  }
  systab = (systab_struct *) sad->address;  	// get required address
  if ( sad != systab)				// if not in correct place
  { 
    fprintf(stderr, "attach = %lX need = %lX\n", (u_long)sad, (u_long)systab);
    i = shmdt( sad );				// unmap it
    fprintf(stderr, "shmdt return = %X\n", i);
    sad = shmat(shar_mem_id, (void *) systab, 0); // try again
    fprintf(stderr, "systab = %lX  sad = %lX\n", (u_long) systab, (u_long) sad);
    if ( systab != sad)
	return(EADDRNOTAVAIL);			// die on error
  }
  sem_id = semget(shar_mem_key, 0, 0);		// attach to semaphores
  if (sem_id < 0) return (errno);		// die on error
  return(0);                                    // return 0 for OK
}

static struct timeval sem_start[SEM_MAX];

short TrySemLock(int sem_num, int numb)
{
  short s;
  struct sembuf buf={0, 0, SEM_UNDO|IPC_NOWAIT};// for semop()

  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  s = semop(systab->sem_id, &buf, 1);           // doit
  return s;
}

short SemLock(int sem_num, int numb)
{
  short s;
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()
  int x;

  x = 2*sem_num;
  if (-1 == numb)       // READ lock
    x += 1;

  s = TrySemLock(sem_num, numb);
  if (s != 0)
  { buf.sem_num = (u_short) sem_num;            // get the one we want
    buf.sem_op = (short) numb;                  // and the number of them
    s = semop(systab->sem_id, &buf, 1);         // doit
  }

// #define MV1_PROFILE 1
#ifdef MV1_PROFILE
  if (s == 0)
    gettimeofday(&sem_start[sem_num], NULL);
#endif
  return s;
}
 
short SemUnlock(int sem_num, int numb)
{
  struct timeval tv;
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()
  short s;
  u_int curr_held_time;
  int x;

  x = 2*sem_num + (1 == abs(numb) ? 1 : 0);
  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  s = semop(systab->sem_id, &buf, 1);

#ifdef MV1_PROFILE
  gettimeofday(&tv, NULL);
  curr_held_time = 1000000 * (tv.tv_sec  - sem_start[sem_num].tv_sec) +
                             (tv.tv_usec - sem_start[sem_num].tv_usec);
  semtab[x].held_time += curr_held_time;
  sem_start[sem_num] = tv;
#endif

  semtab[x].held_count++;
  return s;
}

//	struct sembuf {
//		   u_short sem_num;        /* semaphore # */
//		   short   sem_op;         /* semaphore operation */
//		   short   sem_flg;        /* operation flags */
//	};

static int curr_sem[SEM_MAX];
       int curr_sem_init = 1;

short SemOpEx0(int sem_num, int numb,
              const char *file, int line)        // Add/Remove semaphore
{ short s;                                      // for returns
  int i;                                        // for try loop
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()
  char msg[128];

  // fprintf(stderr,"%s:%d %d %3d\r\n", file, line, sem_num, numb);
  // fflush(stderr);

  if (curr_sem_init)
  { bzero(curr_sem, sizeof(curr_sem));
    curr_sem_init = 0;
  }
  if (numb == 0)				// check for junk
  { return 0;					// just return
  }
  curr_sem[sem_num] += numb;
  if (abs(curr_sem[sem_num]) > systab->maxjob)
  { sprintf(msg, "SemOp(): overload sem_num=%d, numb=%d, curr_sem=%d",
                  sem_num, numb, curr_sem[sem_num]);
    panic(msg);
  }
  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  for (i = 0; i < 5; i++)                       // try this many times
  { s = semop(systab->sem_id, &buf, 1);         // doit
    if (s == 0)					// if that worked
    { if (sem_num == SEM_GLOBAL) curr_lock += numb; // adjust curr_lock
      return 0;					// exit success
    }
    if (numb < 1)                               // if it was an add
      if (partab.jobtab == NULL)		// from a daemon
	panic("SemOp() error in write daemon");	// yes - die
      if (partab.jobtab->trap)                  // and we got a <Ctrl><C>
        return -(ERRZ51+ERRMLAST);              // return an error
  }
  curr_sem[sem_num] -= numb;
  if (systab->start_user == -1)			// If shutting down
  { exit (0);					// just quit
  }
  if ((sem_num != SEM_LOCK) || (numb != 1)) panic("SemOp() failed");  // die... unless a lock release
  return 0;                                     // shouldn't get here except lock
}

short SemOpEx(int sem_num, int numb,
              const char *file, int line)        // Add/Remove semaphore
{ short s;                                      // for returns
  int i;                                        // for try loop
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()
  char msg[128];

  // fprintf(stderr,"%s:%d %d %3d\r\n", file, line, sem_num, numb);
  // fflush(stderr);

  if (curr_sem_init)
  { bzero(curr_sem, sizeof(curr_sem));
    curr_sem_init = 0;
  }
  if (numb == 0)				// check for junk
  { return 0;					// just return
  }
  if ((abs(curr_lock) >= systab->maxjob) && (numb < 0))
  { sprintf(msg,"SemOp(): have WRITE lock %s:%d", file, line);
    panic(msg);
  }
  curr_sem[sem_num] += numb;
  if (abs(curr_sem[sem_num]) > systab->maxjob)
  { sprintf(msg, "SemOp(): overload sem_num=%d, numb=%d, curr_sem=%d",
                  sem_num, numb, curr_sem[sem_num]);
    panic(msg);
  }
  // buf.sem_num = (u_short) sem_num;              // get the one we want
  // buf.sem_op = (short) numb;                    // and the number of them
  for (i = 0; i < 5; i++)                       // try this many times
  { s = (numb < 0) ? SemLock(sem_num, numb) : SemUnlock(sem_num, numb);
    // s = semop(systab->sem_id, &buf, 1);         // doit
    if (s == 0)					// if that worked
    { if (sem_num == SEM_GLOBAL) curr_lock += numb; // adjust curr_lock
      return 0;					// exit success
    }
    if (numb < 1)                               // if it was an add
      if (partab.jobtab == NULL)		// from a daemon
	panic("SemOp() error in write daemon");	// yes - die
      if (partab.jobtab->trap)                  // and we got a <Ctrl><C>
        return -(ERRZ51+ERRMLAST);              // return an error
  }
  curr_sem[sem_num] -= numb;
  if (systab->start_user == -1)			// If shutting down
  { exit (0);					// just quit
  }
  if ((sem_num != SEM_LOCK) || (numb != 1)) panic("SemOp() failed");  // die... unless a lock release
  return 0;                                     // shouldn't get here except lock
}
u_int semslot(int pass)
{
  u_int slot;
  db_stat *stats;

  slot  = (u_int) MTIME(0);
  if (partab.jobtab)
    slot += partab.jobtab->pid + partab.jobtab->commands +
            partab.jobtab->grefs;
  stats = &systab->vol[0]->stats;
  slot += stats->dbget + stats->dbset + stats->dbkil + 
          stats->dbdat + stats->dbord + stats->dbqry;
  return slot ^ (u_int) pass;
}

short SemOpEx1(int sem_num, int numb,
              const char *file, int line)        // Add/Remove semaphore
{ short s;                                      // for returns
  int i;                                        // for try loop
  struct sembuf buf={0, 0, SEM_UNDO|IPC_NOWAIT};// for semop()
  static int doinit = 1;
  char msg[128];
  u_int mask, slot;
  int pass;

  // fprintf(stderr,"%s:%d %d %3d\r\n", file, line, sem_num, numb);
  // fflush(stderr);

  if (doinit)
  { bzero(curr_sem, sizeof(curr_sem));
    doinit = 0;
  }
  if (numb == 0)				// check for junk
  { return 0;					// just return
  }
  curr_sem[sem_num] += numb;
  if (abs(curr_sem[sem_num]) > systab->maxjob)
  { sprintf(msg, "SemOp(): overload sem_num=%d, numb=%d, curr_sem=%d",
                  sem_num, numb, curr_sem[sem_num]);
    panic(msg);
  }
  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  pass = 0;
retry:
  for (i = 0; i < 10; i++)                      // try this many times
  { s = semop(systab->sem_id, &buf, 1);         // doit
    if ((s < 0) && (errno == EAGAIN))
    { mask = (1 << (i + 1)) - 1;
      slot = semslot(i) & mask;
      if (pass)
      { fprintf(stderr,"SemOp(): pass=%d loop=%d slot=%d\r\n", pass, i, slot);
        fflush(stderr);
      }
      usleep(1000 * slot);
      continue;
    }
    break;
  }
  if ((s != 0) && (pass < 16))
  { pass++;
    goto retry;
  }
  if (s == 0)					// if that worked
  { if (sem_num == SEM_GLOBAL) curr_lock += numb; // adjust curr_lock
      return 0;					// exit success
  }
  if (numb < 1)                               // if it was an add
    if (partab.jobtab == NULL)		// from a daemon
      panic("SemOp() error in write daemon");	// yes - die
    if (partab.jobtab->trap)                  // and we got a <Ctrl><C>
      return -(ERRZ51+ERRMLAST);              // return an error

  curr_sem[sem_num] -= numb;

  if (systab->start_user == -1)			// If shutting down
  { exit (0);					// just quit
  }
  if ((sem_num != SEM_LOCK) || (numb != 1)) panic("SemOp() failed");  // die... unless a lock release
  return 0;                                     // shouldn't get here except lock
}


short SemLock1(int sem_num, int numb)
{
  short s;
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()
  int i, x, slot, pass;
  int slot_time, lock_read;
  u_int backoff;
  char msg[256];

  slot_time = 15;       // READ lock held time
  lock_read = 0;
  x = 2*sem_num;
  if (-1 == numb)       // READ lock
  { x += 1;
    slot_time = 30;     // WRITE lock held time
    lock_read = 1;
  }

  pass = 0;
retry:
  for (i = 0; i < 16; i++)
  { s = TrySemLock(sem_num, numb);
    if (s == 0)
      break;
#if 0
    if ((lock_read == 0) && (i == 0))
    { s = semctl(systab->sem_id, sem_num, GETVAL);
      if (s != -1)
      { semtab[x].tryfailed_count++;
        usleep(slot_time * s);
        continue;
      }
    }
#endif
    switch (errno)
    { case EAGAIN:
        semtab[x].tryfailed_count++;
        slot = random() & ((1 << (i + 1)) - 1);
        backoff = slot_time * slot;
        semtab[x].backoff_time += backoff;
        s = usleep(backoff);
        if (s < 0)
        { fprintf(stderr, "usleep(): %s\r\n", strerror(errno));
        }
        break;
      default:
        sprintf(msg,"SemLock()@1: %d/%d sem_num=%d numb=%d returned: %s\r\n",
                pass, i,
                sem_num, numb, strerror(errno));
        panic(msg);
    }
  }

  pass++;
  if ((s != 0) && (pass < 16))
  { s = sched_yield();
    if (s < 0)
    { fprintf(stderr,"sched_yield(): %s\r\n", strerror(errno));
    }
    goto retry;
  }

  if (s != 0)
  { sprintf(msg,"SemLock()@2: %d/%d sem_num=%d numb=%d returned: %s\r\n",
            pass, i,
            sem_num, numb, strerror(errno));
    panic(msg);
  }

/*
  if (s != 0)
  { buf.sem_num = (u_short) sem_num;            // get the one we want
    buf.sem_op = (short) numb;                  // and the number of them
    s = semop(systab->sem_id, &buf, 1);         // doit
  }
*/

/*
  s = TrySemLock(sem_num, numb);
  if (s)
  { semtab[i].tryfailed_count++;
    buf.sem_num = (u_short) sem_num;            // get the one we want
    buf.sem_op = (short) numb;                  // and the number of them
    s = semop(systab->sem_id, &buf, 1);         // doit
  }
*/
  gettimeofday(&sem_start[sem_num], NULL);
  return s;
}


short SemOpEx2(int sem_num, int numb,
              const char *file, int line)        // Add/Remove semaphore
{ short s;                                      // for returns
  static int doinit = 1;
  char msg[128];

  // fprintf(stderr,"%s:%d %d %3d\r\n", file, line, sem_num, numb);
  // fflush(stderr);

  if (doinit)
  { bzero(curr_sem, sizeof(curr_sem));
    doinit = 0;
  }
  if (numb == 0)				// check for junk
  { return 0;					// just return
  }
  curr_sem[sem_num] += numb;
  if (abs(curr_sem[sem_num]) > systab->maxjob)
  { sprintf(msg, "SemOp(): overload sem_num=%d, numb=%d, curr_sem=%d",
                  sem_num, numb, curr_sem[sem_num]);
    panic(msg);
  }
  s = numb < 0 ? SemLock(sem_num, numb) : SemUnlock(sem_num, numb);
  if (s == 0)					// if that worked
  { if (sem_num == SEM_GLOBAL) curr_lock += numb; // adjust curr_lock
      return 0;					// exit success
  }
  if (numb < 1)                               // if it was an add
    if (partab.jobtab == NULL)		// from a daemon
      panic("SemOp() error in write daemon");	// yes - die
    if (partab.jobtab->trap)                  // and we got a <Ctrl><C>
      return -(ERRZ51+ERRMLAST);              // return an error

  curr_sem[sem_num] -= numb;

  if (systab->start_user == -1)			// If shutting down
  { exit (0);					// just quit
  }
  if ((sem_num != SEM_LOCK) || (numb != 1)) panic("SemOp() failed");  // die... unless a lock release
  return 0;                                     // shouldn't get here except lock
}
