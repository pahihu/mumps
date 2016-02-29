// File: mumps/seqio/SQ_File.c
//
// This module implements the following sequential input/output ( ie IO )
// operations for files:
//
//	SQ_File_Open		Opens a file for read, write or append mode
//	SQ_File_Write		Writes to file
//	SQ_File_Read		Reads from file
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
#include	<stdio.h>
#include	<unistd.h>
#include	"error.h"
#include	"seqio.h"

// ************************************************************************* //
//									     //
// File functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function opens a sequential file "file" for the specified operation
// "op" ( ie writing, reading or appending ).  If successful, it returns a
// non-negative integer, termed a file descriptor.  Otherwise, a negative
// integer is returned to indicate the error that has occured.

int SQ_File_Open (char *file, int op)
{ int	flag;
  int	fid;

  switch ( op )
  { case WRITE:
      flag = O_WRONLY|O_TRUNC|O_CREAT;
      break;
    case READ:
      flag = O_RDONLY;
      break;
    case APPEND:
      flag = O_WRONLY|O_APPEND|O_CREAT;
      break;
    default:
      return ( getError ( INT, ERRZ21 ) );
  }

  // I am assuming that MODE will always be ignored, except when the file does
  // not exist and "op" is either WRITE or APPEND.

  fid = open ( file, flag, MODE );
  if ( fid == -1 ) return ( getError ( SYS, errno ) );
  return ( fid );
}

// ************************************************************************* //
// This function writes "nbytes" bytes from the buffer "writebuf" to the file
// associated with the descriptor "fid".  Upon successful completion, the number
// of bytes actually written is returned.  Otherwise, a negative integer is
// returned to indicate the error that has occured.

int SQ_File_Write (int fid, u_char *writebuf, int nbytes)
{ int	ret;

  ret = write ( fid, writebuf, nbytes );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  return ( ret );
}

// ************************************************************************* //
// This function reads "nbytes" bytes into the buffer "readbuf" from the file
// associated with the descriptor "fid".  If successful, the number of bytes
// actually read is returned.  Otherwise, a negative integer is returned to
// indicate the error that has occured.

int SQ_File_Read (int fid, u_char *readbuf)
{ int	ret;

  ret = read ( fid, readbuf, 1 );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  else return ( ret );
}
