// File: mumps/seqio/SQ_Util.c
//
// This module implements the following miscellaneous sequential input/output
// ( ie IO ) operations:
//
//	printBytestr	Prints the integer value of each element in a character
//			array
//	printSQChan	Prints out each field in the structure SQ_Chan, and
//			selected fields in the structure jobtab
//	getError	Returns a negative integer value ( which corresponds to
//			a particular error message )
//	setSignalBitMask
//			Sets the bits of those signals that have been caught
//	seqioSelect	Waits until an object is ready for reading or writing
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


#include	<stdio.h>				// always include
#include 	<stdlib.h>                         	// these two
#include	<errno.h>
#include 	<sys/types.h>                      	// for u_char def
#include	<sys/time.h>
#include	<signal.h>				//
#include	<string.h>
#include	<unistd.h>
#include	"mumps.h"
#include	"error.h"
#include	"proto.h"
#include	"seqio.h"

// ************************************************************************* //
// This function works as follows:
//
//	"type"	Description	Return
//
//	SYS	System error	-( "errnum" + ERRMLAST + ERRZLAST );
//	INT	Internal error	-( "errnum" + ERRMLAST );

int getError (int type, int errnum)
{ int	err;

  switch ( type )
  { case SYS:
      err = errnum + ERRMLAST + ERRZLAST;
      break;
    case INT:
      err = errnum + ERRMLAST;
      break;
    default:
      err = ERRZ20 + ERRMLAST;
      break;
  }
  return ( -err );
}

// ************************************************************************* //
// This function notifies the process that a signal has been caught by setting
// "partab.jobtab->attention" to 1.  The process can then determine which
// signal(s) have been caught by the bits in the mask "partab.jobtab->trap";
// hence, if a bit has been set, then the signal corresponding to that bit has
// been caught.

void setSignalBitMask (int sig)
{ int	mask;
  if ( sig == SIGQUIT )
    DoInfo();
//  else if ( sig == SIGCHLD )
//    wait(&mask);
  else
  { partab.jobtab->attention = 1;
    mask = 1 << sig;
    partab.jobtab->trap |= mask;
  }
}

// ************************************************************************* //
// This function will return 0 when the object referenced by the descriptor
// "sid" is ready for reading or writing ( as determined by "type" ).
// Otherwise, a negative integer is returned to indicate the error that has
// occured.

int seqioSelect (int sid, int type, int tout)
{ int           	nfds;
  fd_set        	fds;
  int           	ret;
  struct timeval	timeout;

  nfds = sid + 1;
  FD_ZERO ( &fds );
  FD_SET ( sid, &fds );
  if ( tout == 0 )
  { timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    if ( type == FDRD ) ret = select ( nfds, &fds, NULL, NULL, &timeout );
    else ret = select ( nfds, NULL, &fds, NULL, &timeout );
  }

  // Time out handled by alarm(), where no alarm() would be set if "tout"
  // was -1

  else
  { if ( type == FDRD ) ret = select ( nfds, &fds, NULL, NULL, NULL );
    else ret = select ( nfds, NULL, &fds, NULL, NULL );
  }

  // An error has occured ( possibly timed out by alarm() )

  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  else if ( ret == 0 )
  { ret = raise ( SIGALRM );			// Force select to time out
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
    return ( -1 );
  }
  else return ( 0 );				// Success
}
