// File: mumps/seqio/SQ_Signal.c
//
// This module can be viewed as the facility for catching all signals that may
// be delivered to a process ( except SIGSTOP and SIGKILL ), in order to handle
// them internally.  It provides the following function:
//
//	setSignals	Sets up the system defined set of signals.
//

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


#include	<errno.h>
#include	<signal.h>
#include	<stdio.h>
#include	"seqio.h"

void signalHandler ( int sig );

// ************************************************************************* //
//									     //
// Signal functions							     //
//									     //
// ************************************************************************* //

//read(2),  write(2),
//     sendto(2),  recvfrom(2),  sendmsg(2) and recvmsg(2) on a communications
//     channel or a low speed device and during a ioctl(2) or wait(2).  However,
//     calls that have already committed are not restarted, but instead return a
//     partial success (for example, a short read count).

int setSignal (int sig, int flag)
{ struct sigaction	action;
  sigset_t		handlermask;
  int			ret;

  sigemptyset ( &handlermask );
  sigfillset ( &handlermask );
  action.sa_mask = handlermask;
  action.sa_flags = 0;
  if ( flag == CATCH ) action.sa_handler = signalHandler;
  else action.sa_handler = SIG_IGN;
  ret = sigaction ( sig, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  return ( ret );
}

// ************************************************************************* //
// This function tells the system that the set of all signals that can be
// caught should be delivered to the designated signal handler.  Upon success,
// 0 is returned.  Otherwise, a negative integer value is returned to indicate
// the error that has occured.
//
// Note, if a signal arrives during one of the primitive operations ( ie open,
//	 read etc ), the operation will exit with -1, with "errno" set to
//	 EINTR.

int setSignals (void)
{ struct sigaction	action;			// man 2 sigaction
  sigset_t		handlermask;		// man 2 sigaction
  int			ret;			// Return value

  sigemptyset ( &handlermask );
  sigfillset ( &handlermask );
  action.sa_mask = handlermask;
  action.sa_flags = 0;
  action.sa_handler = signalHandler;

  action.sa_handler = SIG_IGN;
  ret = sigaction ( SIGHUP, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;

  ret = sigaction ( SIGINT, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  action.sa_flags = SA_RESTART;
  ret = sigaction ( SIGQUIT, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_flags = 0;

  action.sa_handler = SIG_DFL;				// let unix do this one
  ret = sigaction ( SIGILL, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;

  ret = sigaction ( SIGTRAP, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  ret = sigaction ( SIGABRT, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

#if defined(__FreeBSD__) || defined(__NetBSD__)
  action.sa_handler = SIG_DFL;				// let unix do this one
  ret = sigaction ( SIGEMT, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;
#endif

  action.sa_handler = SIG_DFL;				// let unix do this one
  ret = sigaction ( SIGFPE, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;

  action.sa_handler = SIG_DFL;				// let unix do this one
  ret = sigaction ( SIGBUS, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;

  action.sa_handler = SIG_DFL;				// SIGSEGV is desirable
  ret = sigaction ( SIGSEGV, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;

#if defined(__FreeBSD__) || defined(__NetBSD__)
  action.sa_handler = SIG_DFL;				// let unix do this one
  ret = sigaction ( SIGSYS, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;
#endif

  ret = sigaction ( SIGPIPE, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  ret = sigaction ( SIGALRM, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  ret = sigaction ( SIGTERM, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  ret = sigaction ( SIGURG, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  ret = sigaction ( SIGTSTP, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  action.sa_handler = SIG_IGN;
  ret = sigaction ( SIGCONT, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;

// Setting this to SIG_IGN should stop zombies
//  action.sa_handler = SIG_IGN;
//  ret = sigaction ( SIGCHLD, &action, NULL );
//  if ( ret == -1 ) return ( getError ( SYS, errno ) );
//  action.sa_handler = signalHandler;

  ret = sigaction ( SIGTTIN, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  ret = sigaction ( SIGTTOU, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  ret = sigaction ( SIGIO, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  ret = sigaction ( SIGXCPU, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  ret = sigaction ( SIGXFSZ, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  ret = sigaction ( SIGVTALRM, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  ret = sigaction ( SIGPROF, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

  action.sa_handler = SIG_IGN;				// Ignore for now
  ret = sigaction ( SIGWINCH, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_handler = signalHandler;

#if defined(__FreeBSD__) || defined(__NetBSD__)
  action.sa_flags = SA_RESTART;
  ret = sigaction ( SIGINFO, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  action.sa_flags = 0;
#endif

  ret = sigaction ( SIGUSR1, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  ret = sigaction ( SIGUSR2, &action, NULL );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  return ( 0 );
}

// ************************************************************************* //
//									     //
// Local functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function handles all caught signals.
//
// Note, refer to the function "setSignalBitMask" in the file
//	 "/mumps/seqio/SQ_Util.c".

void signalHandler (int sig)				// Caught signal
{ setSignalBitMask ( sig );
}
