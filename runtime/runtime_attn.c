// File: mumps/runtime/runtime_attn.c
//
// module runtime - look after attention conditions

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
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>				// for sleep
#include <sched.h>                              // for sched_yield()
#include <sysexits.h> 				// for exit() condition
#include <errno.h>                              // error stuf
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"				// standard errors
#include "opcodes.h"				// the op codes
#include "compile.h"				// for XECUTE
#include "seqio.h"				// for sig stuff

extern int failed_tty;				// tty reset flag

short attention()				// process attention
{ short s = 0;					// return value

  if (partab.jobtab->trap & SIG_CC)		// control c
  { partab.jobtab->trap = partab.jobtab->trap & ~SIG_CC; // clear it
    partab.jobtab->async_error = -(ERRZ51+ERRMLAST); // store the error
  }

//      if (partab.jobtab->trap & SIG_WS)	// window size change SIGWINCH
//      { partab.jobtab->trap = partab.jobtab->trap & ~SIG_WS; // clear it
//          THIS IS IGNORED IN SQ_Signal.c CURRENTLY
//      }

  if (partab.jobtab->trap & SIG_HUP)		// SIGHUP
  { partab.jobtab->trap = partab.jobtab->trap & ~SIG_HUP; // clear it
    partab.jobtab->async_error = -(ERRZ66+ERRMLAST); // store the error
  }
  if (partab.jobtab->trap & SIG_U1)		// user defined signal 1
  { partab.jobtab->trap = partab.jobtab->trap & ~SIG_U1; // clear it
    partab.jobtab->async_error = -(ERRZ67+ERRMLAST); // store the error
  }
  if (partab.jobtab->trap & SIG_U2)		// user defined signal 2
  { partab.jobtab->trap = partab.jobtab->trap & ~SIG_U2; // clear it
    partab.jobtab->async_error = -(ERRZ68+ERRMLAST); // store the error
  }

  if (partab.jobtab->trap & (SIG_QUIT | SIG_TERM | SIG_STOP)) // stop type
  { partab.jobtab->trap = 0; 			// clear it
    partab.jobtab->async_error = 0;		// clear the error
    partab.jobtab->attention = 0;		// clear attention
    return OPHALT;                          	// and just halt
  }

  partab.jobtab->trap = 0; 			// clear signals
  partab.jobtab->attention = 0;			// clear attention
  s = partab.jobtab->async_error;		// do we have an error
  partab.jobtab->async_error = 0;		// clear it

  if ((s == 0) && (partab.debug > 0))		// check the debug junk
  { if (partab.debug <= partab.jobtab->commands) // there yet?
      s = BREAK_NOW;				// time to break again
    else partab.jobtab->attention = 1;		// reset attention
  }
  return s;					// return whatever
}


//  DoInfo() - look after a control T
//
void DoInfo()
{ int i;					// a handy int
  int j;					// and another
  char ct[400];					// some space for control t
  char *p;					// a handy pointer
  mvar *var;					// and another

  bcopy("\033\067\033[99;1H",ct,9);		// start off
  i = 9;					// next char
  i += sprintf(&ct[i],"%d", (int)(partab.jobtab - systab->jobtab) + 1);
  i += sprintf(&ct[i]," (%d) ", partab.jobtab->pid);
  p = (char *) &partab.jobtab->dostk[partab.jobtab->cur_do].rounam;
          					// point at routine name
  for (j = 0; (j < MAX_NAME_BYTES) && (p[j]); ct[i++] = p[j++]); // copy it
  i += sprintf(&ct[i]," Cmds: %d ", partab.jobtab->commands);
  i += sprintf(&ct[i],"Grefs: %d ", partab.jobtab->grefs);
  var = &partab.jobtab->last_ref;		// point at $R
  if (var->name.var_cu[0] != '\0')		// something there?
    i += UTIL_String_Mvar(var, (u_char *) &ct[i], 32767);	// decode it
  if (i > 89) i = 89;				// fit on terminal
  bcopy("\033\133\113\033\070\0", &ct[i], 6);	// and the trailing bit
  fprintf(stderr, "%s", ct);			// output it
  return;					// all done
}

// The ForkIt subroutine, forks another MUMPS process
// Returns:	Success (parent) MUMPS job number of child
//		Success (child) -MUMPS job number of parent
//		Failure 0 (zot)
//
//	cft = 0 JOB
//	      1 FORK
//	     -1 Just do a rfork() for the daemons, no file table

extern int curr_sem_init;

int ForkIt(int cft)				// Copy File Table True/False
{ int i;					// a handy int
  int ret;					// ant another
  int mid = -1;					// for the MUMPS id
  char stderr_out[128];

  for (i = 0; i < systab->maxjob; i++)		// scan the slots
  { ret = systab->jobtab[i].pid;		// get pid
    if (ret)					// if one there
    { if (kill(ret, 0))				// check the job
      { if (errno == ESRCH)			// doesn't exist
        { CleanJob(i + 1);			// zot if not there
	  break;				// have at least one
        }
      }
    }
    else					// it's free or ours
    { break;					// quit
    }
  }

#ifdef __FreeBSD__
  ret = RFPROC | RFNOWAIT | RFCFDG;             // default - no copy ft
  if (cft > -1)					// if it is a fork or JOB
    ret = RFPROC | RFNOWAIT | RFFDG;		// copy the file table
#endif

  if (cft > -1)					// not a daemon
  { i = SemOp(SEM_SYS, -systab->maxjob);	// lock systab
    if (i < 0) return 0;			// quit on error
    for (i = 0; i < systab->maxjob; i++)        // look for a free slot
      if (systab->jobtab[i].pid == 0)           // this one ?
      { mid = i;                  		// yes - save int job num
        break;                                  // and exit
      }
    if (mid == -1)				// if no slots
    { i = SemOp(SEM_SYS, systab->maxjob);	// unlock
      return 0;					// return fail
    }
  }

#if defined(__FreeBSD__) || defined(__NetBSD__)	// for BSD
  i = rfork(ret);				// create new process
#else						// OSX et al
  signal(SIGCHLD, SIG_IGN);                     // try this
  i = fork();					//
#endif

  if (!i)					// if the child
  { failed_tty = -1;				// don't attempt to restore
  }						// term settings on exit

  if (cft < 0)
  {
#if !defined(__FreeBSD__) && !defined(__NetBSD__)
    if (!i)
    { (void)freopen("/dev/null", "w", stdin);	// redirect stdin
      (void)freopen("/dev/null", "w", stdout);	// redirect stdout
      (void)freopen("/dev/null", "w", stderr);	// redirect stderr
    }
#endif
    return i;
  }
  if (i == 0)
    (void) setSignal ( SIGINT, IGNORE );	// disable control C

  if (i < 0)					// fail ?
  { i = SemOp(SEM_SYS, systab->maxjob);		// unlock
    return 0;					// return fail
  }
  if (i > 0)					// the parent ?
  { bcopy(partab.jobtab, &systab->jobtab[mid], sizeof(jobtab)); // copy job info
    systab->jobtab[mid].pid = i;		// save the pid
    i = SemOp(SEM_SYS, systab->maxjob);		// unlock
    return (mid + 1);				// return child job number
  }
  ret = -((partab.jobtab - systab->jobtab) + 1); // save minus parent job#
  partab.jobtab = &systab->jobtab[mid];         // and save our jobtab address

  curr_sem_init = 1;

  for (i = 0; i < 10000; i++)			// wait for the above to happen
  { if (getpid() == partab.jobtab->pid)		// done yet ?
      break;					// yes - exit
    SchedYield();                    		// give up slice
  }
  if (i > 9999)					// if that didn't work
  { for (i = 0; ; i++)				// try the long way
    { if (getpid() == partab.jobtab->pid)	// done yet ?
        break;					// yes - exit
      if (i > 120)				// two minutes is nuff
	panic("ForkIt: Child job never got setup");
      sleep(1);					// wait for a second
    }
  }
  if (cft)					// fork type?
  { i = SemOp(SEM_ROU, -systab->maxjob);	// grab the routine semaphore
    if (i < 0) panic("Can't get SEM_ROU in ForkIt()"); // die on fail
    for (i = partab.jobtab->cur_do; i > 0; i--)	// scan all do frames
    { if (partab.jobtab->dostk[i].flags & DO_FLAG_ATT)
        ((rbd *) partab.jobtab->dostk[i].routine)->attached++; // count attached
    }
    i = SemOp(SEM_ROU, systab->maxjob);		// release the routine buffers
    return ret;					// return -parent job#
  }
  for (i = 1; i < MAX_SEQ_IO; SQ_Close(i++));	// close all open files

  (void)freopen("/dev/null", "r", stdin);	// redirect stdin
  (void)freopen("/dev/null", "w", stdout);	// redirect stdout
  sprintf(stderr_out, "%d.stderr", partab.jobtab->pid);
  // (void)freopen("/dev/null", "w", stderr);	// redirect stderr
  (void)freopen(stderr_out, "w", stderr);	// redirect stderr

  return ret;					// return -parent job#

}

// SchedYield()
//

void SchedYield()				// do a sched_yield()
{ 
  (void)sched_yield();				// do it
  return;					// and exit
}
