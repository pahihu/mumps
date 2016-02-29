// File: mumps/seqio/SQ_Tcpip.c
//
// This module implements the following sequential input/output ( ie IO )
// operations for TCP/IP sockets:
//
//	SQ_Tcpip_Open		Determines the type of socket to open
//	SQ_Tcpip_Accept		Accepts a connection on a server socket
//	SQ_Tcpip_Write		Writes to a socket
//	SQ_Tcpip_Read		Reads from a socket
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
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>
#include	"error.h"
#include	"seqio.h"

int SQ_Tcpip_Open_Server ( char *bind );
int SQ_Tcpip_Open_Client ( char *conn );

int SQ_Socket_Create ( int nonblock );
int SQ_Socket_Bind ( int sid, u_short port );
int SQ_Socket_Listen ( int sid );
int SQ_Socket_Accept ( int sid, int tout );
int SQ_Socket_Connect ( int sid, char *addr, u_short port );
int SQ_Socket_Write ( int sid, u_char *writebuf, int nbytes );
int SQ_Socket_Read ( int sid, u_char *readbuf, int tout );

// ************************************************************************* //
//									     //
// Tcpip functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function determines the type of socket to open.  If it can not determine
// the type of socket, a negative integer value is returned to indicate the
// error that has occured.

int SQ_Tcpip_Open (char *bind, int op)
{ switch ( op )
  { case SERVER:
      return ( SQ_Tcpip_Open_Server ( bind ) );
    case TCPIP:
      return ( SQ_Tcpip_Open_Client ( bind ) );
    default:
      return ( getError ( INT, ERRZ21 ) );
  }
}

// ************************************************************************* //
// Refer to function SQ_Socket_Accept in the file SQ_Socket.c.

int SQ_Tcpip_Accept (int sid, int tout)
{ return ( SQ_Socket_Accept ( sid, tout ) );
}

// ************************************************************************* //
// Refer to function SQ_Socket_Write in the file SQ_Socket.c.

int SQ_Tcpip_Write (int sid, u_char *writebuf, int nbytes)
{ return ( SQ_Socket_Write ( sid, writebuf, nbytes ) );
}

// ************************************************************************* //
// Refer to function SQ_Socket_Read in the file SQ_Socket.c.

int SQ_Tcpip_Read (int sid, u_char *readbuf, int tout)
{ return ( SQ_Socket_Read ( sid, readbuf, tout ) );
}

// ************************************************************************* //
//									     //
// Local functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function opens a server socket end-point and binds it to the port
// "bind".  If successful, it returns a non-negative integer, termed a
// descriptor.  Otherwise, it returns a negative integer to indicate the error
// that has occured.

int SQ_Tcpip_Open_Server (char *bind)
{ int	sid;
  int	ret;
  u_short	port;

  sid = SQ_Socket_Create (1);
  if ( sid < 0 ) return ( sid );
  port = atoi ( bind );
  ret = SQ_Socket_Bind ( sid, port );
  if ( ret < 0 )
  { ( void ) close ( sid );
    return ( ret );
  }
  ret = SQ_Socket_Listen ( sid );
  if ( ret < 0 )
  { ( void ) close ( sid );
    return ( ret );
  }
  return ( sid );
}

// ************************************************************************* //
// This function opens a client socket end-point and connects it to the
// port "conn".  If successful, it returns a non-negative integer, termed a
// descriptor.  Otherwise, it returns a negative integer to indicate the error
// that has occured.

int SQ_Tcpip_Open_Client (char *conn)
{ int	sid;
  char	*portptr;
  char	*addrptr;
  u_short	port;
  int	ret;
  char	xxxx[100];

  strcpy( xxxx, conn);

  sid = SQ_Socket_Create (0);
  if ( sid < 0 ) return ( sid );
  portptr = strpbrk ( xxxx, " " );
  if ( portptr == NULL )
  { ( void ) close ( sid );
    return ( getError ( INT, ERRZ28 ) );
  }
  *portptr = '\0';
  addrptr = xxxx;
  portptr++;
  port = atoi ( portptr );
  ret = SQ_Socket_Connect ( sid, addrptr, port );
  if ( ret < 0 )
  { ( void ) close ( sid );
    return ( ret );
  }
  return ( sid );
}
