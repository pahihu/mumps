// File: mumps/seqio/SQ_Device.c
//
// This module implements the following sequential input/output ( ie IO )
// operations for devices:
//
//	SQ_Device_Open		Opens a file for read, write or read/write mode
//	SQ_Device_Write		Writes to a device
//	SQ_Device_Read		Determines the type of device to read from
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
#include	<sys/types.h>
#include	<sys/uio.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<termios.h>
#include	<unistd.h>
#include	"error.h"
#include	"seqio.h"

int SQ_Device_Read_TTY ( int fid, u_char *buf, int tout );

// ************************************************************************* //
//									     //
// Device functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function opens a device "device" for the specified operation "op" ( ie
// writing, reading or reading and/or writing ).  If successful, it returns a
// non-negative integer, termed a descriptor.  Otherwise, it returns a negative
// integer to indicate the error that has occured.

int SQ_Device_Open (char *device, int op)
{	int	flag;
	int	did;

  switch ( op )
  {
    case WRITE:
      flag = O_WRONLY;
      break;
    case READ:
      flag = O_RDONLY;
      break;
    case IO:
      flag = O_RDWR;
      break;
    default:
      return ( getError ( INT, ERRZ21 ) );
  }

// If device is busy, keep trying until a timeout ( ie alarm signal ) has been
// received

  while ( 1 )
  { did = open ( device, flag, 0 );
    if ( did == -1 )
    { if ( errno != EBUSY )
      { return ( getError ( SYS, errno ) );
      }
//      else if (partab.jobtab->trap |= 16384)		// MASK[SIGALRM]
      else if (partab.jobtab->trap | 16384)             // MASK[SIGALRM]
      { return (-1);
      }
    }
    else
    { return ( did );
    }
  }							// end while (1)
  return ( getError ( INT, ERRZ20 ) );
}

// ************************************************************************* //
// This function writes "nbytes" bytes from the buffer "writebuf" to the device
// associated with the descriptor "did".  Upon successful completion, the number
// of bytes actually written is returned.  Otherwise, it returns a negative
// integer to indicate the error that has occured.

int SQ_Device_Write (int did, u_char *writebuf, int nbytes)
{ int	ret;
  ret = write ( did, writebuf, nbytes );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  return ( ret );
}

// ************************************************************************* //
// This function determines the type of device to read from.  If it can not
// determine the type of device, a negative integer value is returned to
// indicate the error that has occured.
//
// Note, support is only implemented for terminal type devices.

int SQ_Device_Read (int did, u_char *readbuf, int tout)
{ int	ret;
  ret = isatty ( did );
  if ( ret == 1 ) return ( SQ_Device_Read_TTY ( did, readbuf, tout ) );
  else return ( getError ( INT, ERRZ24 ) );
}

// ************************************************************************* //
//									     //
// Local functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function reads at most one character from the device associated with
// the descriptor "did" into the buffer "readbuf".  A pending read is not
// satisfied until one byte or a signal has been received.  Upon successful
// completion, the number of bytes actually read is returned.  Otherwise, a
// negative integer is returned to indicate the error that has occured.

int SQ_Device_Read_TTY (int did, u_char *readbuf, int tout)
{ struct termios		settings;
  int		ret;
  int		rret;

  if ( tout == 0 )
  { ret = tcgetattr ( did, &settings );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
    settings.c_cc[VMIN] = 0;
    ret = tcsetattr ( did, TCSANOW, &settings );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
  }
  rret = read ( did, readbuf, 1 );
  if ( tout == 0 )
  { ret = tcgetattr ( did, &settings );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
    settings.c_cc[VMIN] = 1;
    ret = tcsetattr ( did, TCSANOW, &settings );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
    if (rret == 0)				// zero timeout and no chars
    { partab.jobtab->trap |= 16384;		// MASK[SIGALRM]
      return (-1);
    }
  } 
  if ( rret == -1 )
  { if ( errno == EAGAIN )
    { ret = raise ( SIGALRM );
      if ( ret == -1 ) return ( getError ( SYS, errno ) );
    }
    return ( getError ( SYS, errno ) );
  }
  else
  { return ( rret );
  }
}
