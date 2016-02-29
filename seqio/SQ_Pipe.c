// File: mumps/seqio/SQ_Pipe.c
//
// This module implements the following sequential input/output ( ie IO )
// operations for named pipes ( or FIFO ):
//
//	SQ_Pipe_Open		Opens FIFO for reading or writing
//	SQ_Pipe_Write		Writes to FIFO
//	SQ_Pipe_Read		Reads from FIFO
//	SQ_Pipe_Close		Closes ( and/or removes ) FIFO
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
#include	<sys/stat.h>
#include	<sys/types.h>
#include	<sys/uio.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<unistd.h>
#include	"error.h"
#include	"seqio.h"

int createPipe ( char *pipe );

// ************************************************************************* //
//									     //
// Pipe functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function opens the FIFO "pipe" for the specified operation "op".  A
// FIFO can be opened for one of two operations:
//
//	Operation	Description
//
//	NEWPIPE		Create and open FIFO for reading ( does not block on
//			open )
//	PIPE		Open pipe for writing
//
// If successful, this function returns a non-negative integer, termed a
// descriptor.  Otherwise, a negative integer value is returned to indicate the
// error that has occured.

int SQ_Pipe_Open (char *pipe, int op)
{ int	ret;
  int	flag;
  int	pid;

  switch ( op )
  { case NEWPIPE:
      ret = createPipe ( pipe );
      if ( ret < 0 ) return ( ret );
      flag = O_RDONLY|O_NONBLOCK;
      break;
    case PIPE:
      flag = O_WRONLY;
      break;
    default:
      return ( getError ( INT, ERRZ21 ) );
  }
  pid = open ( pipe, flag, 0 );
  if ( pid == -1 ) return ( getError ( SYS, errno ) );
  return ( pid );
}

// ************************************************************************* //
// This function closes the FIFO "pipe"; hence:
//
//	- deletes the descriptor "pid" from the per-process object reference
//	  table; and
//	- removes the FIFO "pipe" itself.
//
// Upon successful completion, a value of 0 is returned.  Otherwise, a negative
// integer value is returned to indicate the error that has occured.

int SQ_Pipe_Close (int pid, char *pipe)
{ int	ret;
  int	oid;

  ret = close ( pid );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );

// Determine if there are any other readers on the pipe
//
// By closing the reader, and opening a writer on the pipe, if there does not
// exist another reader on the pipe, an ENXIO error will occur; that is, the
// named file is a character special or block special file, and the device
// associated with this special file does not exist.

  oid = open ( pipe, O_WRONLY|O_NONBLOCK, 0 );
  if ( oid == -1 )
  { ret = unlink ( pipe );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
  };
  return ( 0 );
}

// ************************************************************************* //
// This function writes "nbytes" bytes from the buffer "writebuf" to the FIFO
// associated with the descriptor "pid".  Upon successful completion, the number
// of bytes actually written is returned.  Otherwise, a negative integer value
// is returned to indicate the error that has occured.
//
// Note, if one tries to write to a pipe with no reader, a SIGPIPE signal is
// generated.  This signal is caught, where write will return -1 with errno set
// to EPIPE ( ie broken pipe ).

int SQ_Pipe_Write (int pid, u_char *writebuf, int nbytes)
{ int	ret;

  ret = write ( pid, writebuf, nbytes );
  if ( ret == -1 )
  { if ( errno == EPIPE ) return ( getError ( INT, ERRZ46 ) );
    else return ( getError ( SYS, errno ) );
  }
  else return ( ret );
}

// ************************************************************************* //
// This function reads "nbytes" bytes into the buffer "readbuf" from the FIFO
// associated with the descriptor "pid".  Upon successful completion, the number
// of bytes actually read is returned.  Otherwise, a negative integer value is
// returned to indicate the error that has occured.

int SQ_Pipe_Read (int pid, u_char *readbuf, int tout)
{ int	ret;
  int	bytesread;

  // Wait for input

start:

  ret = seqioSelect ( pid, FDRD, tout );
  if ( ret < 0 ) return ( ret );

  // Read byte

  bytesread = read ( pid, readbuf, 1 );

  if ((bytesread == -1) && (errno == EAGAIN))	// Resource temporarily unavail
  { sleep(1);
    goto start;
  }

  // An error has occured

  if ( bytesread == -1 )
  { if (errno == EAGAIN)			// Resource temporarily unavail
    { sleep(1);					// wait a bit
      errno = 0;				// clear this
      goto start;				// and try again
    }
    return ( getError ( SYS, errno ) );		// EOF received
  }
  else if ( bytesread == 0 )			// Force read to time out
  { if ( tout == 0 )
    { ret = raise ( SIGALRM );
      if ( ret == -1 )
      { return ( getError ( SYS, errno ) );
      }
      return ( -1 );
    }

  // Wait, then check for any data on the pipe

    sleep ( 1 );
    return ( 0 );
  }

  // Return bytes read ( ie 1 )

  else return ( 1 );
}

// ************************************************************************* //
//									     //
// Local functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function creates a FIFO special file with the name "pipe".  Upon
// successful completion, a value of 0 is returned.  Otherwise, a negative
// integer value is returned to indicate the error that has occured.

int createPipe (char *pipe)
{ int	ret;

  ret = mkfifo ( pipe, MODE );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  else return ( ret );
}
