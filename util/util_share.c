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
#include "rwlock.h"                             // for SemLock()/SemUnlock()

#ifdef __APPLE__
#include <mach/mach_time.h>
u_int64 monotonic_time(void)
{ return mach_absolute_time();
}
#endif
#ifdef __linux__
#include <time.h>
u_int64 monotonic_time(void)
{ struct timespec time;
  clock_gettime(CLOCK_MONOTONIC, &time);
  return (u_int64)time.tv_sec * 1000000000 + time.tv_nsec;
}
#endif

//****************************************************************************
//**  Function: UTIL_assert - my assert() funcion, if supplied     ***
//**  prints the caller of the function which called ASSERT()      ***
void UTIL_assert(int cond, const char *expr,
               const char *fn, const char *path, int line,
               const char *caller_path, int caller_line)
{
  if (!cond)
  { if (curr_lock)
    { SemOp( SEM_GLOBAL, -curr_lock);
    }
    if (caller_path)
    { fprintf(stderr, "Assertion failed: (%s), function %s at %s:%d, called from %s:%d\r\n", expr, fn, path, line, caller_path, caller_line);
    }
    else
    { fprintf(stderr, "Assertion failed: (%s), function %s at %s:%d\r\n", expr, fn, path, line);
    }
    fflush(stderr);
    *((volatile u_char*) NULL) = 0;
    exit(-1);
  }
}

//****************************************************************************
//**  Function: UTIL_Share - attach shared memory section        ***
//**  returns addr (or NULL on error)                            ***
int UTIL_Share(const char *dbf)                 // pointer to dbfile name
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
  { i = shmdt( sad );				// unmap it
    sad = shmat(shar_mem_id, (void *) systab, 0); // try again
    if ( systab != sad)
    { fprintf(stderr, "Unable to attach to systab at %lX\n", (u_long) systab);
      return(EADDRNOTAVAIL);			// die on error
    }
  }
  sem_id = semget(shar_mem_key, 0, 0);		// attach to semaphores
  if (sem_id < 0) return (errno);		// die on error
  return(0);                                    // return 0 for OK
}

static int curr_sem[SEM_MAX];
       int curr_sem_init = 1;

const  char *sem_file;
       int  sem_line;

pid_t mypid = 0;
extern void mv1_log_init();

extern void DB_Unlocked();

short SemOpEx(int sem_num, int numb,
              const char *file, int line)        // Add/Remove semaphore
{ short s;                                      // for returns
  int i;                                        // for try loop
  char msg[128];

  sem_file = file;
  sem_line = line;
  if (mypid == 0)
  { mypid = getpid();
    mv1_log_init();
  }
  // fprintf(stderr,"%5d %20lld %08X %d %3d %s:%d\r\n",
  //                 mypid, monotonic_time(),
  //                 systab->shsem[SEM_GLOBAL], sem_num, numb,
  //                 file, line); fflush(stderr);

  if (curr_sem_init)
  { bzero(curr_sem, sizeof(curr_sem));
    curr_sem_init = 0;
  }
  if (numb == 0)				// check for junk
  { return 0;					// just return
  }
  if ((SEM_GLOBAL == sem_num) &&                // global lock ?
      (abs(curr_lock) >= systab->maxjob) &&     //   AND already have WRITE lock
      (numb < 0))                               //   AND acquire
  { sprintf(msg,"SemOp(): have WRITE lock, sem_num=%d numb=%d curr_lock=%d @ %s:%d",
                 sem_num, numb, curr_lock, file, line);
    panic(msg);
  }
  curr_sem[sem_num] += numb;
  if (abs(curr_sem[sem_num]) > systab->maxjob)
  { sprintf(msg, "SemOp(): overload sem_num=%d numb=%d curr_sem=%d @ %s:%d",
                  sem_num, numb, curr_sem[sem_num], file, line);
    panic(msg);
  }
  for (i = 0; i < 5; i++)                       // try this many times
  { s = (numb < 0) ? SemLock(sem_num, numb) : SemUnlock(sem_num, numb);
    if ((SEM_GLOBAL == sem_num) && (0 < numb))
      DB_Unlocked();
    if (s == 0)					// if that worked
    { if (SEM_GLOBAL == sem_num)curr_lock += numb; // adjust curr_lock
      return 0;					// exit success
    }
    if (numb < 1)                               // if it was an add
    { if (partab.jobtab == NULL)		// from a daemon
	panic("SemOp() error in write daemon");	// yes - die
      if (partab.jobtab->trap)                  // and we got a <Ctrl><C>
        return -(ERRZ51+ERRMLAST);              // return an error
    }
  }
  curr_sem[sem_num] -= numb;
  if (systab->start_user == -1)			// If shutting down
  { exit (0);					// just quit
  }
  panic("SemOp() failed");                      // die...
  return 0;                                     // shouldn't get here
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
