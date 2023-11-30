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
#include <sys/types.h>                          // for u_char def
#include <errno.h>                              // error stuf
#include <sys/ipc.h>                            // shared memory
#include <sys/shm.h>                            // shared memory
#include <sys/sem.h>                            // semaphores
#include <sys/time.h>                           // gettimeofday()
#include "mumps.h"                              // standard includes
#include "error.h"                              // standard includes
#include "proto.h"                              // standard includes
#include "database.h"                           // standard includes

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
  { // fprintf(stderr, "attach = %lX need = %lX\n", (u_long)sad, (u_long)systab);
    i = shmdt( sad );				// unmap it
    // fprintf(stderr, "shmdt return = %X\n", i);
    sad = shmat(shar_mem_id, (void *) systab, 0); // try again
    // fprintf(stderr, "systab = %lX  sad = %lX\n", (u_long) systab, (u_long) sad);
    if ( systab != sad)
    { fprintf(stderr, "Unable to attach to systab at %lX\n", (u_long) systab);
      return(EADDRNOTAVAIL);			// die on error
    }
  }
  sem_id = semget(shar_mem_key, 0, 0);		// attach to semaphores
  if (sem_id < 0) return (errno);		// die on error
  return(0);                                    // return 0 for OK
}

//	struct sembuf {
//		   u_short sem_num;        /* semaphore # */
//		   short   sem_op;         /* semaphore operation */
//		   short   sem_flg;        /* operation flags */
//	};

short SemOp(int sem_num, int numb)              // Add/Remove semaphore
{ short s;                                      // for returns
  int i;                                        // for try loop
  struct sembuf buf={0, 0, SEM_UNDO};           // for semop()

  if (numb == 0)				// check for junk
  { return 0;					// just return
  }
  buf.sem_num = (u_short) sem_num;              // get the one we want
  buf.sem_op = (short) numb;                    // and the number of them
  if (numb > 0)                                 // release
    MEM_BARRIER;
  for (i = 0; i < 5; i++)                       // try this many times
  { s = semop(systab->sem_id, &buf, 1);         // doit
    if (s == 0)					// if that worked
    { if (sem_num == SEM_GLOBAL) curr_lock += numb; // adjust curr_lock
      if (numb < 1)                             // acquire
        MEM_BARRIER;
      return 0;					// exit success
    }
    if (numb < 1)                               // if it was an add
    { if (partab.jobtab == NULL)		// from a daemon
	Panic("SemOp() error in write daemon");	// yes - die
      if (partab.jobtab->trap)                  // and we got a <Ctrl><C>
        return -(ERRZ51+ERRMLAST);              // return an error
    }
  }
  if (systab->start_user == -1)			// If shutting down
  { exit(0);					// just quit
  }
  // NB. nonsense, why allow unlocking SEM_LOCK fail here ?
  //     if fails, you could not LOCK anything after this
  //     --- PROVEN WITH SIM4.ROU --- (32 processes on 16 CPUs)
  Panic("SemOp() failed");                      // die...
  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//      Forth COUNTER - TIMER
//
u_long fCounter(u_long *p_stamp)
{ struct timeval tv;
  u_long ret;

  gettimeofday(&tv, NULL);
  ret = 1000000 * tv.tv_sec + tv.tv_usec;
  if (p_stamp) *p_stamp = ret;
  return ret;
}

u_long fTimer(u_long p_start)
{ return fCounter(0) - p_start;
}


////////////////////////////////////////////////////////////////////////////////
//
//      Timer
//

#define ONE_SEC (1000000UL)

void TimerStart(TIMER_T *p_tim,
                int p_timeout_sec, const char *p_msg, int p_msgarg)
{ fCounter(&p_tim->tim_start);
  p_tim->tim_timeout = p_tim->tim_start + ONE_SEC * p_timeout_sec;
  p_tim->tim_wait = ONE_SEC;
  p_tim->tim_msg = p_msg;
  p_tim->tim_msgarg = p_msgarg;
  p_tim->tim_counter = 1;
}

int TimerCheck(TIMER_T *p_tim)
{ u_long elapsed;

  // if (0 == (++p_tim->tim_counter & 31))
  { elapsed = fTimer(p_tim->tim_start);
    if (elapsed > p_tim->tim_timeout)
      return 1;
    if (elapsed > p_tim->tim_wait)
    { systab->vol[volnum-1]->stats.timwt++;
      // fprintf(stderr, p_tim->tim_msg, p_tim->tim_msgarg);
      // fflush(stderr);
      p_tim->tim_wait += ONE_SEC;
    }
  }
  return 0;
}

void UTIL_assert(int cond, const char *expr, const char *path, int lno)
{ char msg[128];
  if (!cond)
  { if (curr_lock)
    { SemOp( SEM_GLOBAL, -curr_lock);
    }
    sprintf(msg,"%s:%d: assertion failed %s",path,lno,expr);
    Panic(msg);
  }
}

