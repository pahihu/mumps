// File: mumps/seqio/SQ_Seqio.c
//
// This module can be viewed as the "main" program for all matters relating to
// sequential Input/Output ( IO ).  It provides the following functions:
//
//	SQ_Init			Initialises channel 0, signal handlers...
//	SQ_Open			Obtain ownership of an object
//	SQ_Use			Make an owned object current for IO
//	SQ_Close		Relinquish ownership of an object
//	SQ_Write		Output a byte(s) to the current IO object
//	SQ_WriteStar		Output one byte to the current IO object
//	SQ_WriteFormat		Output a format byte(s) to the current IO object
//	SQ_Read			Read a byte(s) from the current IO object
//	SQ_ReadStar		Reads one byte from the current IO object
//	SQ_Flush		Flushes input buffered for the current IO object
//	SQ_Device		Gathers information about the current IO object
//	SQ_Force		Forces data to an object
//
// Note, an object is one of a file, device, named pipe or tcpip socket.
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
#include	<sys/file.h>
#include	<sys/stat.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>
#include	<ctype.h>
#include	<netdb.h>
#include	<signal.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include <strings.h>
#include	<termios.h>
#include	<unistd.h>
#include	"error.h"
#include	"mumps.h"
#include	"proto.h"
#include	"seqio.h"

// Set bit flags

#define		UNSET		-1		// Unset bit
#define		LEAVE		0		// Leave bit
#define		SET		1		// Set bit

// Object types

#define		UNKNOWN		0		// Unknown object
#define		DIR		1		// Directory
#define		CHR		2		// Character special
#define 	BLK		3		// Block special
#define 	REG		4		// Regular
#define 	FIFO		5		// Named pipe ( ie FIFO )
#define 	LNK		6		// Symbolic link
#define 	SOCK		7		// Socket
#define 	WHT		8		// Whiteout

// Miscellaneous buffer sizes

#define		MASKSIZE	32		// Maximum bits in set bit mask
#define		WRITESIZE	132		// Maximum write buffer size
#define		BUFSIZE		32767		// Maximum buffer size
#define		READSIZE	1024		// Maximum read buffer size
#define		OPSIZE		30		// Maximum operation size
#define		TTYSIZE		128		// Maximim tty name size
#define		ERRSIZE		100		// Maximum error buffer size

// SQ_Chan.options bit mask

#define		INTERM		0		// Input terminator(s) bit
#define		OUTERM		1		// Output terminator bit
#define		ESC		2		// Escape sequence bit
#define		TTYECHO		3		// Echo bit
#define		DEL8		4		// Delete key 8 bit
#define		DEL127		5		// Delete key 127 bit
//#define	?		6		// Unused
//#define	?		7		// Unused

// Miscellaneous

#define		STDCHAN		0		// STDIN, STDOUT and STDERR
#define		MIN_SEQ_IO	0		// Minimum sequential IO channel
#define		UNLIMITED	-1		// Unlimited

static int	MASK[MASKSIZE];			// Set bit mask
static int	CRLF;				// CRLF

// ************************************************************************* //
// The following required for linux					     //
// ************************************************************************* //

#ifdef linux
#include        <sys/ttydefaults.h>
#endif //linux

#ifndef S_ISWHT
#define S_ISWHT(m)      (((m) & 0170000) == 0160000)		// whiteout
#endif

int toThePower ( int exp );
int setOptionsBitMask ( int options, int bit, int flag );
int checkBytestr ( char *bytestr );
int checkNbytes ( int nbytes );
int checkCstring ( cstring *cstr );
int getOperation ( cstring *op );
int getObjectType ( char *object );
int getObjectMode ( int fd );
int getModeCategory ( int mode );
int checkCtrlChars ( cstring *cstr );
int getBitMask ( cstring *cstr );
int isINTERM ( char *readbuf, int nbytes, int options, int interm );
int isChan ( int chan );
int isType ( int type );
int isChanFree ( int chan );
void getErrorMsg ( int errnum, char *errmsg );

int initObject ( int chan, int type );

int objectWrite ( int chan, char *buf, int len );

int readFILE ( int chan, u_char *buf, int maxbyt );
int readTCP ( int chan, u_char *buf, int maxbyt, int tout );
int readPIPE ( int chan, u_char *buf, int maxbyt, int tout );
int readTERM ( int chan, u_char *buf, int maxbyt, int tout );

void initFORK ( forktab *f );
int initSERVER ( int chan, int size );
int openSERVER ( int chan, char *oper );
int acceptSERVER ( int chan, int tout );
int closeSERVER ( int chan );
int closeSERVERClient ( int chan );

int signalCaught ( SQ_Chan *c );

// ************************************************************************* //
//									     //
// IO functions								     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function initialises STDCHAN and other IO related stuff.  If
// successful, this function will return 0.  Otherwise, it returns a
// negative integer value to indicate the error that has occured.

short SQ_Init (void)
{ int			index;			// Useful variable
  int			mask;			// Useful variable
  int			ret;			// Return value
  int			typ;			// object type
  
  // Integer bit mask

  for ( index = 0; index < MASKSIZE; index++ )
    MASK[index] = toThePower ( index );

  // CRLF

  mask = 1 << 13;
  CRLF = mask;
  mask = 1 << 10;
  CRLF |= mask;
  
  // Define the set of signals that are to be delivered, ignored etc
 
  ret = setSignals ();
  if ( ret < 0 ) return ( ret );

  // Set the parameters associated with STDCHAN, depending on what it is
  // associated with

  ret = getObjectMode ( STDCHAN );
  if ( ret < 0 ) return ( ret );
  typ = getModeCategory ( ret );
  if ( typ < 0 ) return ( typ );

  // Initialise object

  ret = initObject ( STDCHAN, typ );
  if ( ret < 0 ) return ( ret );

  if (typ == SQ_TCP)					// from inetd?
  { int len;
    struct sockaddr_in sin;
//    struct hostent	*host;				// Peer host name
    servertab		*s;				// Forked process table

    s = &(partab.jobtab->seqio[STDCHAN].s);
    ret = openSERVER ( STDCHAN, "S" );			// set it up
    if ( ret < 0 ) return ( ret );
    s->cid = STDCHAN;					// already accepted

    len = sizeof ( struct sockaddr_in );
    ret = getpeername ( s->cid, (struct sockaddr *) &sin, (socklen_t *)&len );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
    len = sizeof ( sin.sin_addr );
    (void) snprintf ( (char *)s->name, MAX_SEQ_NAME, "%s %d",
    		inet_ntoa ( sin.sin_addr ), ntohs ( sin.sin_port ) );

//    host = gethostbyaddrgethostbyaddr ( inet_ntoa ( sin.sin_addr ), len, AF_INET );
//    if ( host == NULL )
//      (void) snprintf ( s->name, MAX_SEQ_NAME, "%s %d",
//			inet_ntoa ( sin.sin_addr ), ntohs ( sin.sin_port ) );
//    else
//      (void) snprintf ( s->name, MAX_SEQ_NAME, "%s %d",
//			host->h_name, ntohs ( sin.sin_port ) );
    return (0);						// done
  }

  // Determine associated terminal if tty

  if ( isatty ( STDCHAN ) )
    (void) snprintf ( (char *)partab.jobtab->seqio[STDCHAN].name,
		      MAX_SEQ_NAME,
		      "%s",
		      ttyname ( STDCHAN )
		    );
  else
    (void) snprintf ( (char *)partab.jobtab->seqio[STDCHAN].name,
		      MAX_SEQ_NAME,
		      "Not a tty"
		    );

  // Indicate success

  return ( 0 );
}

// ************************************************************************* //
// This function opens an object "object" on channel "chan".  The object can be
// opened for any ( relevant ) one of the following operations ( ie "op" ):
//
//	read		Read only
//	write		Write only
//	append		Append
//	io		Read and write
//	tcpip		Client socket
//	server		Server socket
//	pipe		Write to named pipe
//	newpipe		Read from named pipe
//
// A timeout value "tout" ( where -1 means unlimited ) is also provided.  If
// successful, open returns a non-negative integer, termed a descriptor. 
// Otherwise, it returns a negative integer to indicate the error that has
// occured.
//
// Note, "tout" does not apply for files; and
//	  a "tout" of zero is ignored.

short SQ_Open (int chan, cstring *object, cstring *op, int tout)
{ SQ_Chan	*c;				// Channel to open
  int		oper;				// Operation identifier
  int		ford;				// File or device identifier
  int		obj;				// Object identifier
  int		oid;				// Object descriptor
  int		ret;				// Return value

// Check for Opening $Principal. In MV1, this is a no-op. (See close code as
// well.)
  if (chan == STDCHAN)
  {  if ( tout > -1 )				// if there is a timeout
     {  partab.jobtab->test = 1;		// say that open succeeded
     }
     return 0;
  }
// TODO: make sure that object and op are at least one character long
// If not, this means that somebody tried to use the new syntax O 1::2 on
// it.

// Check parameters

  if ( isChan ( chan ) == 0 ) 			// Channel out of range
  { return ( getError ( INT, ERRZ25 ) );
  }						// Channel not free
  if ( isChanFree ( chan ) == 0 )
  { return ( getError ( INT, ERRZ26 ) );
  }
  c = &(partab.jobtab->seqio[chan]);
  ret = checkCstring ( object );		// Invalid cstrings
  if ( ret < 0 )
  { return ( ret );
  }
  ret = checkCstring ( op );
  if ( ret < 0 )
  { return ( ret );
  }

  if (object->len == 0) 			// Is device descriptor empty?
  { return ( getError ( INT, ERRZ28)); 		// Invalid execpt for O $P::TO
  }
						// Invalid timeout
  if ( tout < UNLIMITED )
  { return ( getError ( INT, ERRZ22 ) );
  }

  // Assume operation won't time out
  
  if ( tout > -1 )
  { partab.jobtab->test = 1;			// assume won't time out
  }

  // Determine object type and operation

  oper = getOperation ( op );			// Determine operation

  if ( oper < 0 )
  { return ( oper );
  }						// Determine object
  else if (( oper == READ ) || ( oper == WRITE ))
  { ford = getObjectType ( (char *)object->buf );
    if ( ford < 0 )
    { return ( ford );
    }
    else if (( ford == UNKNOWN ) && ( oper == WRITE )) obj = SQ_FILE;
    else if ( ford == CHR ) obj = SQ_TERM;
    else if ( ford == REG ) obj = SQ_FILE;
    else
    { return ( getError ( INT, ERRZ29 ) );
    }
  }
  else if ( oper == APPEND ) obj = SQ_FILE;
  else if ( oper == IO ) obj = SQ_TERM;
  else if ( oper == TCPIP ) obj = SQ_TCP;
  else if ( oper == SERVER ) obj = SQ_TCP;
  else if ( oper == PIPE ) obj = SQ_PIPE;
  else if ( oper == NEWPIPE ) obj = SQ_PIPE;
  else
  { return ( getError ( INT, ERRZ29 ) );
  }

  // Start timer

  if (( tout > 0 ) && ( obj != SQ_FILE )) alarm ( tout );

  // Open object

  switch ( obj )
  { case SQ_FILE:
      oid = SQ_File_Open ( (char *)object->buf, oper );
      break;
    case SQ_TERM:
      oid = SQ_Device_Open ( (char *)object->buf, oper );
      break;
    case SQ_PIPE:
      oid = SQ_Pipe_Open ( (char *)object->buf, oper );
      break;
    case SQ_TCP:
      oid = SQ_Tcpip_Open ( (char *)object->buf, oper );
      break;
    default:
      return ( getError ( INT, ERRZ30 ) );
  }
  
  // Open completed ( with or without error ) - prevent alarm signal from being
  // sent, if it hasn't already done so

  alarm ( 0 );

  if ( oid < 0 )
  { if ( partab.jobtab->trap & MASK[SIGALRM] )		// Timeout received
    { partab.jobtab->trap &= ~ ( MASK[SIGALRM] );
      partab.jobtab->test = 0;
      return ( 0 );
    }
    else if ( partab.jobtab->trap & MASK[SIGINT] )	// Caught SIGINT
    { return ( 0 );
    }
    else						// Open error
    { return ( oid );
    }
  }

  // Set default channel attributes

  ret = initObject ( chan, obj );
  if ( ret < 0 )
  { return ( ret );
  }
  c->mode = (char)oper;
  c->fid = oid;
  (void) snprintf ( (char *)c->name, MAX_SEQ_NAME, "%s", object->buf );

  // Set SQ_Chan appropriately ( if server socket )

  if ( oper == SERVER )
  { ret = openSERVER ( chan, (char *)op->buf );
    if ( ret < 0 )
    { return ( ret );
    }
  }

  // Return object's descriptor

  return ( oid );
}

// ************************************************************************* //
// This function modifies the characteristics of a channel; that is, it:
//
//	- sets the channel as the current IO channel;
//	- sets the channel's input terminator bit mask;
//	- sets the channel's output terminator;
//      - ignores or catches the SIGINT signal;
//	- sets the channel's escape sequence set bit flag;
//	- sets the channel's echo set bit flag;
//      - sets the channel's delete key bit mask; and
//      - disconnects a client from a server socket ( if client exists ).
//
// Upon success, this function will return 0.  Otherwise, a negative integer
// value is returned to indicate the error that has occured.

short SQ_Use (int chan, cstring *interm, cstring *outerm, int par)
{ SQ_Chan	*c;				// Pointer to channel
  int		ret;				// Return value
  int		flag;				// UNSET, LEAVE, SET

  // Check parameters

  if ( isChan ( chan ) == 0 )
  { return ( getError ( INT, ERRZ25 ));
  }
  if ( isChanFree ( chan ) == 1 )
  { if ( chan == STDCHAN ) return ( 0 );
    else return ( getError ( INT, ERRZ27 ));
  }
  if ( interm != NULL )
  { ret = checkCtrlChars ( interm );
    if ( ret < 0 ) return ( ret );
  }
  if ( outerm != NULL )
  { ret = checkCstring ( outerm );
    if ( ret < 0 ) return ( ret );
    if ( outerm->len > MAX_SEQ_OUT ) return ( getError ( INT, ERRZ33 ) );
  }
  if ( par < 0 )
  { return ( getError ( INT, ERRZ45 ) );
  }

  partab.jobtab->io = (char)chan;	// Set "chan" as the current IO channel

  c = &(partab.jobtab->seqio[chan]);	// Acquire a pointer to the channel

  if ( interm != NULL )			// Set channel's input term(s) bit mask
  { if ( interm->len != 0 )
    { c->in_term = getBitMask ( interm );
      flag = SET;
    }
    else
    { c->in_term = 0;
      flag = UNSET;
    }
  }
  else flag = LEAVE;

  // Set bit INTERM in channel's options 

  c->options = setOptionsBitMask ( c->options, INTERM, flag );

  if ( outerm != NULL )		// Set channel's output terminator
  { if ( outerm->len != 0 )
    { c->out_len = outerm->len;
      bcopy ( outerm->buf, c->out_term, outerm->len );
      flag = SET;
    }
    else
    { c->out_len = 0;
      c->out_term[0] = '\0';
      flag = UNSET;
    }
  }
  else flag = LEAVE;

  // Set bit OUTERM in channel's options

  c->options = setOptionsBitMask ( c->options, OUTERM, flag );

  // Set bit CONTROLC in channel's options

  if ((!chan) &&				// Chan zero only
      (par & (SQ_CONTROLC | SQ_NOCONTROLC | SQ_CONTROLT | SQ_NOCONTROLT)))
  { struct termios settings;                    // man 4 termios
    ret = tcgetattr ( STDCHAN, &settings );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
    if (par & SQ_CONTROLC)
    { settings.c_cc[VINTR] = '\003';		// ^C
    }
    if (par & SQ_NOCONTROLC)
    { settings.c_cc[VINTR] = _POSIX_VDISABLE;	// No ^C
    }
    if (par & SQ_CONTROLT)
    { settings.c_cc[VQUIT] = '\024';		// ^T
    }
    if (par & SQ_NOCONTROLT)
    { settings.c_cc[VQUIT] = _POSIX_VDISABLE;	// No ^T
    }
    ret = tcsetattr ( STDCHAN, TCSANOW, &settings ); // Set parameters
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
  }

  // else LEAVE

  // Set bit ESC in channel's options

  if ( par & SQ_USE_ESCAPE )
  { c->options = setOptionsBitMask ( c->options, ESC, SET );
  }
  else if ( par & SQ_USE_NOESCAPE )
  { c->options = setOptionsBitMask ( c->options, ESC, UNSET );
  }
  else
  { c->options = setOptionsBitMask ( c->options, ESC, LEAVE );
  }

  // Set bit TTYECHO in channel's options

  if ( par & SQ_USE_ECHO )
  { c->options = setOptionsBitMask ( c->options, TTYECHO, SET );
  }
  else if ( par & SQ_USE_NOECHO )
  { c->options = setOptionsBitMask ( c->options, TTYECHO, UNSET );
  }
  else
  { c->options = setOptionsBitMask ( c->options, TTYECHO, LEAVE );
  }

  // Set bit DEL8 in channel's options

  if ( par & SQ_USE_DEL8 )
  { c->options = setOptionsBitMask ( c->options, DEL8, SET );
  }
  else if ( par & SQ_USE_DELBOTH )
  { c->options = setOptionsBitMask ( c->options, DEL8, SET );
  } 
  else if ( par & SQ_USE_DELNONE )
  { c->options = setOptionsBitMask ( c->options, DEL8, UNSET );
  }
  else
  { c->options = setOptionsBitMask ( c->options, DEL8, LEAVE );
  }

  // Set bit DEL127 in channel's options

  if ( par & SQ_USE_DEL127 )
  { c->options = setOptionsBitMask ( c->options, DEL127, SET );
  }
  else if ( par & SQ_USE_DELBOTH )
  { c->options = setOptionsBitMask ( c->options, DEL127, SET );
  }
  else if ( par & SQ_USE_DELNONE )
  { c->options = setOptionsBitMask ( c->options, DEL127, UNSET );
  }
  else
  { c->options = setOptionsBitMask ( c->options, DEL127, LEAVE );
  }

  // Disconnect client from socket
  
  if ( par & SQ_USE_DISCON )
  { ret = closeSERVERClient ( chan );
    if ( ret < 0 ) return ( ret );
  }
  return ( 0 );
}

// ************************************************************************* //
// This function closes the channel "chan".  It will always return 0; that
// is, it will never return an error.

short SQ_Close (int chan)
{ SQ_Chan	*c;					// Channel to close
  int		ret;					// Return value
  struct stat	sb;				 	// File properties

  // Check parameters

  if ( isChan ( chan ) == 0 )				// Channel out of range
  { return ( 0 );
  }
  if ( isChanFree ( chan ) == 1 )			// Channel free
  { return ( 0 );
  }
  if ( chan == STDCHAN )				// Never close STDCHAN 
  { return ( 0 );
  }
  // If channel is current, the set current channel to STDCHAN

  if ( chan == (int)(partab.jobtab->io) )
  { partab.jobtab->io = (char)STDCHAN;
  }
  // Close channel

  c = &(partab.jobtab->seqio[chan]);
  switch ( (int)(c->type) )
  { case SQ_FILE:

      // If the file is opened for writing or appending and it is empty,
      // then  delete it

      if (( (int)(c->mode) == WRITE ) ||
	  ( (int)(c->mode) == APPEND ))
      { ret = fstat ( c->fid, &sb );
	if ( ( ret == 0 ) && ( sb.st_size == 0 ) )
	{ (void) close ( c->fid );
	  (void) unlink ( (char *)c->name );
	}
	else
	{ (void) close ( c->fid );
	}
      }
      else
      { (void) close ( c->fid );
      }
      c->type = (char)SQ_FREE;
      break;

    case SQ_TCP:
      (void) closeSERVER ( chan );
      break;

    case SQ_PIPE:
      if ( (int)(c->mode) == NEWPIPE )
      { (void) SQ_Pipe_Close ( c->fid, (char *)c->name );
      }
      else
      { (void) close ( c->fid );
      }
      c->type = (char)SQ_FREE;
      break;

    case SQ_TERM:
      (void) close ( c->fid );
      c->type = (char)SQ_FREE;
      break;
  }
  return ( 0 );					// Return success
}

// ************************************************************************* //
// This function writes "writebuf->len" bytes from the buffer "writebuf->buf" to
// the current IO channel.  It also increments $X to the number of bytes
// written.  Upon successful completion, the number of bytes actually written
// is returned.  Otherwise, a negative integer value is returned to indicate
// the error that has occured.

short SQ_Write (cstring *writebuf)
{ int	chan;				// Current IO channel
  int	ret;				// Return value

  ret = checkCstring ( writebuf );
  if ( ret < 0 ) return ( ret );
  chan = (int)(partab.jobtab->io);
  if ( isChan ( chan ) == 0 ) return ( getError ( INT, ERRZ25 ) );
  if ( isChanFree ( chan ) == 1 ) return ( getError ( INT, ERRZ27 ) );
  if ( writebuf->len == 0 ) return ( 0 );
  ret = objectWrite ( chan, (char *)writebuf->buf, writebuf->len );
  if ( ret < 0 ) return ( ret );
  partab.jobtab->seqio[chan].dx = partab.jobtab->seqio[chan].dx + ret;
  return ( ret );
}

// ************************************************************************* //
// This function writes a character "c" to the current IO channel.  Upon
// successful completion, the number of bytes actually written is returned.
// Otherwise, a negative integer value is returned to indicate the error that
// has occured.

short SQ_WriteStar (u_char c)
{ int	chan;				// Current IO channel

  chan = (int)(partab.jobtab->io);
  if ( isChan ( chan ) == 0 ) return ( getError ( INT, ERRZ25 ) );
  if ( isChanFree ( chan ) == 1 ) return ( getError ( INT, ERRZ27 ) );
  return ( objectWrite ( chan, (char *)&c, 1 ) );
}

// ************************************************************************* //
// This function works as follows:
//
//	"count" = SQ_FF ( -2 )
//
// Writes two characters with an integer value of 13 and 12 to the current IO
// channel respectively.  It also clears $X and $Y.
//
//	"count" = SQ_LF ( -1 )
//
// Writes the output terminator ( if set ) to the current IO channel.  It also
// clears $X, but adds 1 to $Y.
//
//	"count" >= 0
//
// Writes spaces to the current IO channel until $X is equal to "count".
//
// Upon successful completion the number of bytes actually written is returned.
// Otherwise, a negative integer value is returned to indicate the error that
// has occured.

short SQ_WriteFormat (int count)
{ int	chan;				// Current IO channel
 SQ_Chan *IOptr;			// Useful pointer
 char	writebuf[WRITESIZE];		// Buffer to write from
 int	ret;				// Return value
 int	byteswritten;			// Bytes written
 int	numspaces;			// Number of spaces to write
 int	bytestowrite;			// Bytes to write
 int	index;				// Useful integer

  if ( count < -2 ) return ( getError ( INT, ERRZ41 ) );
  chan = (int)(partab.jobtab->io);
  if ( isChan ( chan ) == 0 ) return ( getError ( INT, ERRZ25 ) );
  if ( isChanFree ( chan ) == 1 ) return ( getError ( INT, ERRZ27 ) );
  IOptr = &(partab.jobtab->seqio[chan]);

  switch ( count )
  { case SQ_FF:
      writebuf[0] = (char)13;
      writebuf[1] = (char)12;
      ret = objectWrite ( chan, writebuf, 2 );
      if ( ret < 0 ) return ( ret );
      IOptr->dx = 0;
      IOptr->dy = 0;
      return ( ret );

    case SQ_LF:
      ret = 0;
      if ( IOptr->options & MASK[OUTERM] ) {
	ret = objectWrite ( chan, (char *)IOptr->out_term, IOptr->out_len );
	if ( ret < 0 ) return ( ret );
      }
      IOptr->dx = 0;
      IOptr->dy++;
      return ( ret );

    default:
      byteswritten = 0;
      numspaces = count - IOptr->dx;
      while ( numspaces > 0 ) {
	if ( numspaces <= WRITESIZE ) bytestowrite = numspaces;
	else bytestowrite = WRITESIZE;
	for ( index = 0; index < bytestowrite; index++ ) writebuf[index] = ' ';
	ret = objectWrite ( chan, writebuf, bytestowrite );
	if ( ret < 0 ) return ( ret );
	else if ( ret == 0 )
	{
	  if ( partab.jobtab->trap & MASK[SIGINT] ) return ( byteswritten );
	}
	IOptr->dx = IOptr->dx + ret;
	byteswritten = byteswritten + ret;
	numspaces = numspaces - bytestowrite;
      }
      return ( byteswritten );

  }
}

// ************************************************************************* //
// This function reads a maximum of "maxbyt" bytes from the current IO
// channel into the buffer "buf".  Upon successful completion, the number of
// bytes read into the buffer is returned.  Otherwise, a negative integer
// value is returned to indicate the error which has occured.
//
// Note, that a timeout value ( ie "tout" ) or "maxbyt" value of -1 means
//	 unlimited.

short SQ_Read (u_char *buf, int tout, int maxbyt)
{ int		chan;					// Channel to read from
  int		type;					// Channel type
  int		ret;					// Return value

  // Check parameters

  if ( buf == NULL ) return ( getError ( INT, ERRZ28 ) );
  chan = (int)(partab.jobtab->io);
  if ( isChan ( chan ) == 0 ) return ( getError ( INT, ERRZ25 ) );
  if ( isChanFree ( chan ) == 1 ) return ( getError ( INT, ERRZ27 ) );
  if ( tout < -1 ) return ( getError ( INT, ERRZ22 ) );
  if ( maxbyt < -1 ) return ( getError ( INT, ERRZ36 ) );

  // Initialise variables

  partab.jobtab->seqio[chan].dkey_len = 0;
  partab.jobtab->seqio[chan].dkey[0] = '\0';
  if ( maxbyt == -1 ) maxbyt = MAX_STR_LEN;
  type = (int)(partab.jobtab->seqio[chan].type);
  if ( ( tout > 0 ) && ( type != SQ_FILE ) )
  { alarm ( tout );
  }
  if ( ( tout > -1 ) && ( type !=SQ_FILE ) )
  { partab.jobtab->test = 1;				// assume won't time out
  }

  // Read from object

  switch ( type )
  { case SQ_FILE:
      ret = readFILE ( chan, buf, maxbyt );
      break;
    case SQ_TCP:
      ret = readTCP ( chan, buf, maxbyt, tout );
      break;
    case SQ_PIPE:
      ret = readPIPE ( chan, buf, maxbyt, tout );
      break;
    case SQ_TERM:
      ret = readTERM ( chan, buf, maxbyt, tout );
      break;
    default:
      return ( getError ( INT, ERRZ30 ) );
  }

  // Void the current alarm ( even if one is not pending )

  alarm ( 0 );

  // Return bytes read or error

  if ( ret >= 0 )					// Read successfull
  { if ((ret == 0) && (tout == 0))			// no char and 0 t/o
    {  partab.jobtab->test = 0;				// say failed
    }

    buf[ret] = '\0';					// NULL terminate
    return ( ret );
  }
  else if ( partab.jobtab->trap & MASK[SIGINT] )	// SIGINT caught
  { buf[0] = '\0';
    return ( 0 );
  }
  else							// Error
    return ( ret );
}

// ************************************************************************* //
// This function reads one character from $IO and returns its ASCII value in
// "result".  If an escape sequence or input terminator has been received,
// then "result" is equal to 0.  If a timeout expires or the signal SIGINT
// has been caught, then "result" is equal to -1.  This function will return
// 1 if a character was acquired.  It will return 0 if an escape sequence,
// input terminator, timeout or SIGINT has been received.  Otherwise, a
// negative integer value to indicate the error that has occured.
//
// NOTE, $X and $Y are unchanged, and any key pressed does not echo on a
//	 terminal device

short SQ_ReadStar (int *result, int timeout)
{ int	chan;					// $IO
  char	origopt;				// Original options
  u_char buf[2];				// Read buffer
  int	ret;					// Return value

  // Check parameters

  if ( result == NULL ) return ( getError ( INT, ERRZ28 ) );
  if ( timeout < -1 ) return ( getError ( INT, ERRZ36 ) );
  chan = (int)(partab.jobtab->io);
  if ( isChan ( chan ) == 0 ) return ( getError ( INT, ERRZ25 ) );
  if ( isChanFree ( chan ) == 1 ) return ( getError ( INT, ERRZ27 ) );

  // Save channel's original options

  origopt = partab.jobtab->seqio[chan].options;

  // ReadStar options

#ifndef  __APPLE__
  partab.jobtab->seqio[chan].options &= ~ ( MASK[INTERM] );
#endif                                                          // OSX doesn't like this !!!!!
  partab.jobtab->seqio[chan].options &= ~ ( MASK[TTYECHO] );
  partab.jobtab->seqio[chan].options &= ~ ( MASK[DEL8] );
  partab.jobtab->seqio[chan].options &= ~ ( MASK[DEL127] );

  // Get character

  ret = SQ_Read ( buf, timeout, 1 );

  // Restore options

  partab.jobtab->seqio[chan].options = origopt;

  // Return character's ASCII value or error

  if ( ret < 0 ) return ( ret );			// Read error
  else if ( ret == 0 )
  { if ( partab.jobtab->seqio[chan].dkey_len > 0 )	// Escape sequence
    { *result = 0;
      return ( 0 );
    }
    else						// Timeout or SIGINT
    { *result = -1;
      return ( 0 );
    }
  }
  else							// Received character
  { *result = (int)(buf[0]);
    return ( 1 );
  }
}

// ************************************************************************* //
// This function flushes the input queue on a character special device.  If
// successful, 0 is returned.  Otherwise, a negative integer is returned to
// indicate the error that has occurred.

short SQ_Flush (void)
{ int		chan;					// $IO 
  SQ_Chan	*c;					// Pointer to $IO
  int		what;					// Queue
  int		oid;					// Device
  int		ret;					// Return value

  chan = (int)(partab.jobtab->io);			// Determine $IO
  if ( isChan ( chan ) == 0 )				// $IO out of range
    return ( getError ( INT, ERRZ25 ) );
  c = &(partab.jobtab->seqio[chan]);			// Channel to flush
  what = TCIFLUSH;					// Flush input queue
  if ( (int)(c->type) == SQ_TERM )
  { if ( chan == STDCHAN )				// Flush STDIN
      oid = 0;
    else						// Flush other device
      oid = c->fid;
    if ( isatty ( oid ) )
    { ret = tcflush ( oid, what );			// Do flush
      if ( ret == -1 )
	return ( getError ( SYS, errno ) );
    }
    else						// Not a character
      return ( getError ( INT, ERRZ24 ) );		//  special device
  }
  return ( 0 );
}

// ************************************************************************* //
// This function returns in the buffer "buf" the following three pieces of
// information about the current IO channel:
//
//	Piece	Description
//
//	1	1 or 0
//	2	error_code or object type ( ie SQ_FILE, _TCP, _PIPE, _TERM )
//	3	error_text or description of channel ( eg. file/device name or
//		ip address port ).
//
// Upon successful completion, this function returns the length of the buffer.
// It returns a negative integer to indicate the internal or system error that
// has occured.
//
// Note, each piece is comma deliminated.

short SQ_Device (u_char *buf)
{ int		chan;					// Current IO channel
  char		errmsg[MAX_STR_LEN];			// Error message
  SQ_Chan	*c;					// $IO pointer
  char		*name;					// Channel's attributes

  // Check parameters

  if ( buf == NULL )
    return ( getError ( INT, ERRZ28 ) );

  chan = (int)(partab.jobtab->io);	// Determine the current IO channel

  if ( isChan ( chan ) == 0 )		// Check if channel is out of range
  { getErrorMsg ( ERRZ25, errmsg );
    sprintf ( (char *)buf, "1,%d,%s", ERRZ25, errmsg );
    return ( strlen ( (char *)buf ) );
  }

  c = &(partab.jobtab->seqio[chan]);	// Acquire a point to $IO

  if ( isChanFree ( chan ) == 1 )	// Channel free
  { sprintf ( (char *)buf, "1,0," );
    return ( strlen ( (char *)buf ) );
  }

  // Get channel's attributes
  if ( (int)(c->type) == SQ_TCP )			// Socket specific
  { switch ( (int)(c->mode) )
    { case TCPIP:
	name = (char *)c->name;
	break;
      case SERVER:
	if ( c->s.cid != -1 )
	  name = (char *)c->s.name;
	else
	  name = (char *)c->name;
	break;
      case NOFORK:
	if ( c->s.cid != -1 )
	  name = (char *)c->s.name;
	else
	  name = (char *)c->name;
	break;
      case FORKED:
	name = (char *)c->name;
	break;
      default:
	name = NULL;
	break;
    }
  }
  else							// All other objects
    name = (char *)c->name;

  // Check for NULL pointer ( name )

  if ( name == NULL )
  { getErrorMsg ( ERRMLAST+ERRZ28, errmsg );
    (void) sprintf ( (char *)buf, "1,%d,%s", ERRMLAST+ERRZ28, errmsg );
    return ( strlen ( (char *)buf ) );
  }

  // Return channel's attributes

  (void) sprintf ( (char *)buf, "0,%d,%s", (int)(c->type), name );
  return ( strlen ( (char *)buf ) );

}

// ************************************************************************* //
// This function forces the message "msg" to the character special device
// "device".  Upon successful completion, this functions returns 0.
// Otherwise, it returns a negative integer value to indicate the error that
// has occurred.

short SQ_Force(cstring *device, cstring *msg)
{
  FILE	*fd;					// Useful variable
  int	ret;					// Useful variable

// Check parameters

  ret = checkCstring ( device );
  if ( ret < 0 ) return ( ret );			// Invalid cstring
  ret = checkCstring ( msg );
  if ( ret < 0 ) return ( ret );			// Invalid cstring

  if ( msg->len == 0 ) return ( 0 );	// Quit without error if "msg" is empty

#ifdef __APPLE__

  return ( 0 );

#else

  fd = fopen ( (char *)device->buf, "w" );
  if ( fd == NULL )
  { return ( 0 );
  }

  alarm(2);						// timeout in two secs

  ret = fprintf ( fd, "%s", msg->buf );

  ret = fclose( fd );
  
  alarm(0);
  
  return (0);

#endif

}


// ************************************************************************* //
//									     //
// Common functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function returns the value of 2 to the power of the exponent "exp".

int toThePower (int exp)
{ int	index;						// Useful variable
  int	power;						// Useful variable

  power = 1;
  for ( index = 1; index <= exp; index++ ) power = power * 2;
  return ( power );
}

// ************************************************************************* //
// This function checks the character array "bytestr" for the following:
//
//	- "bytestr" is NULL;
//
// If any of the forementioned statements are true, this function returns a
// negative integer value to indicate the error.  Otherwise, a 0 is returned.
//

int checkBytestr (char *bytestr)
{ if ( bytestr == NULL )
  { return ( getError ( INT, ERRZ28 ) );
  }
  return ( 0 );
}

// ************************************************************************* //
// This function checks the integer "nbytes" for the following:
//
//	- "nbytes" < 0; and
//	- "nbytes" > BUFSIZE.
//
// If any of the forementioned statements are true, this function returns a
// negative integer value to indicate the error.  Otherwise, a 0 is returned.

int checkNbytes (int nbytes)
{ if ( ( nbytes < 0 ) || ( nbytes > BUFSIZE ) )
  { return ( getError ( INT, ERRZ33 ) );
  }
  return ( 0 );
}

// ************************************************************************* //
// This functions checks the cstring "cstr" for the following:
//
//	- "cstr" is NULL;
//	- "cstr->buf" is invalid ( refer to checkBytestr ); and
//	- "cstr->len" is invalid ( refer to checkNbytes ).
//
// If any of the forementioned statements are true, this function returns a
// negative integer value to indicate the error.  Otherwise, a 0 is returned.

int checkCstring (cstring *cstr)
{ int	ret;

  if ( cstr == NULL ) return ( getError ( INT, ERRZ28 ) );
  ret = checkBytestr ( (char *)cstr->buf );
  if ( ret < 0 ) return ( ret );
  ret = checkNbytes ( cstr->len );
  if ( ret < 0 ) return ( ret );
  return ( 0 );
}


// ************************************************************************* //
// This function determines the type of an object "object".  If the object can
// be successfully identified, a postive integer is returned ( which indicates
// the objects type ).  If the object can not be identified, 0 is returned.
// Otherwise, a negative integer value is returned to indicate the error that
// has occured.
//
// Note, if stat fails because the object does not exist, then it is possible
// that the object is a file still to be created; hence, the object could be a
// file, but this function is unable to confirm this as the file is not yet
// created.  Thus, a 0 is returned as opposed to a negative integer value.

int getObjectType (char *object)
{ struct stat	sb;			// Object status
  int		ret;			// Useful variable

  ret = stat ( object, &sb );
  if ( ret == -1 )
  { if ( errno == ENOENT ) return ( UNKNOWN );	// File to be created ??
    else return ( getError ( SYS, errno ) );
  }
  if ( S_ISDIR ( sb.st_mode ) == 1 ) return ( DIR );
  else if ( S_ISCHR ( sb.st_mode ) == 1 ) return ( CHR );
  else if ( S_ISBLK ( sb.st_mode ) == 1 ) return ( BLK );
  else if ( S_ISREG ( sb.st_mode ) == 1 ) return ( REG );
  else if ( S_ISFIFO ( sb.st_mode ) == 1 ) return ( FIFO );
  else if ( S_ISLNK ( sb.st_mode ) == 1 ) return ( LNK );
  else if ( S_ISSOCK ( sb.st_mode ) == 1 ) return ( SOCK );
  else if ( S_ISWHT ( sb.st_mode ) == 1 ) return ( WHT );
  else return ( getError ( INT, ERRZ30 ) );
}

// ************************************************************************* //
//

int getObjectMode (int fd)
{ struct stat sb;
  int ret;

  ret = fstat ( fd, &sb );
  if ( ret == -1 ) return ( getError ( SYS, errno ) );
  if ( S_ISDIR ( sb.st_mode ) == 1 ) return ( DIR );
  else if ( S_ISCHR ( sb.st_mode ) == 1 ) return ( CHR );
  else if ( S_ISBLK ( sb.st_mode ) == 1 ) return ( BLK );
  else if ( S_ISREG ( sb.st_mode ) == 1 ) return ( REG );
  else if ( S_ISFIFO ( sb.st_mode ) == 1 ) return ( FIFO );
  else if ( S_ISLNK ( sb.st_mode ) == 1 ) return ( LNK );
  else if ( S_ISSOCK ( sb.st_mode ) == 1 ) return ( SOCK );
  else if ( S_ISWHT ( sb.st_mode ) == 1 ) return ( WHT );
  else return ( getError ( INT, ERRZ30 ) );
}

// ************************************************************************* //
//

int getModeCategory (int mode)
{ if ( mode == REG ) return ( SQ_FILE );
  if ( mode == SOCK ) return ( SQ_TCP );
  if ( mode == FIFO ) return ( SQ_PIPE );
  if ( mode == CHR ) return ( SQ_TERM );
  return ( getError ( INT, ERRZ30 ) );
}

// ************************************************************************* //
// This function checks if "chan" is a valid channel.  If "chan" is not valid,
// then 0 is returned.  Otherwise, 1 is returned.

int isChan (int chan)
{ if (( chan < MIN_SEQ_IO ) ||
      ( chan > MAX_SEQ_IO ))
    return ( 0 );
  return ( 1 );
}

// ************************************************************************* //
// This function checks if "type" is a valid type for an object.  If "type" is
// not valid, then 0 is returned.  Otherwise, 1 is returned.

int isType (int type)
{ if ( ( type != SQ_FREE ) &&
       ( type != SQ_FILE ) &&
       ( type != SQ_TCP ) &&
       ( type != SQ_PIPE ) &&
       ( type != SQ_TERM ) ) {
    return ( 0 );
  }
  return ( 1 );
}

// ************************************************************************* //
// This function checks if channel "chan" is free.  If "chan" is not free, then
// 0 is returned.  Otherwise, 1 is returned.

int isChanFree (int chan)
{ if ( (int)(partab.jobtab->seqio[chan].type) != SQ_FREE )
  { return ( 0 );
  }
  return ( 1 );
}

// ************************************************************************* //
// This function checks the cstring "cstr" for the following:
//
//	- "cstr" is invalid ( refer to checkCstring ); and
//	- one or more characters in the character array "cstr->buf" have an
//	  integer value not between 0 and 31 inclusive.
//
// If any of the forementioned statements are true, this function returns a
// negative integer value to indicate the error.  Otherwise, a 0 is returned.

int checkCtrlChars (cstring *cstr)
{ int	index;
  int	ret;

  ret = checkCstring ( cstr );
  if ( ret < 0 ) return ( ret );
  for ( index = 0; index < cstr->len; index++ ) {
    if ( (int)(cstr->buf[index]) > 31 )
      return ( getError ( INT, ERRZ34 ) );
  }
  return ( 0 );
}

// ************************************************************************* //
// This function returns an integer, which represents a bit mask corresponding
// to the array of control characters "cstr->buf".

int getBitMask (cstring *cstr)
{ int	index;				// Index
  int	chmask;				// Characters bit mask
  int	mask;				// Array's bit mask

  mask = 0;
  for ( index = 0; index < cstr->len; index++ ) {
    chmask = 1 << (int)cstr->buf[index];
    mask = mask | chmask;
  }
  return ( mask );
}

// ************************************************************************* //
// This function sets the bit "bit" in the integer "options" according to the
// flag "flag".

int setOptionsBitMask (int options, int bit, int flag)
{ int	mask;

  mask = 1 << bit;
  if ( flag == SET ) return ( options | mask );
  if ( flag == UNSET ) return ( options & ( ~ mask ) );
  return ( options );
}

// ************************************************************************* //
// This function returns an integer which uniquely identifies the operation
// "op->buf"; hence, "write", "read", "append", "io", "tcpip", "server", "pipe"
// or "newpipe".  Otherwise, it returns a negative integer to indicate the error
// that has occured.

int getOperation (cstring *op)
{ char	ch;				// Operation identifier
  char	str[OPSIZE];			// Useful buffer
  char	*ptr;				// Pointer to '=' in operation

  if ( op->len > OPSIZE )
  { return ( getError ( INT, ERRZ33 ) );
  }
  ptr = strcpy ( str, (char *)op->buf );
  ptr = strchr ( ptr, '=' );
  if ( ptr != NULL ) *ptr = '\0';
  if ( strlen ( str ) == 1 )
  { ch = tolower ( op->buf[0] );
    if ( ch == 'w' ) return ( WRITE );
    else if ( ch == 'r' ) return ( READ );
    else if ( ch == 'a' ) return ( APPEND );
    else if ( ch == 'i' ) return ( IO );
    else if ( ch == 't' ) return ( TCPIP );
    else if ( ch == 's' ) return ( SERVER );
    else if ( ch == 'p' ) return ( PIPE );
    else if ( ch == 'n' ) return ( NEWPIPE );
  }
  else if ( strlen ( str ) > 1 )
  { if ( strcasecmp ( str, "write" ) == 0 ) return ( WRITE );    
    else if ( strcasecmp ( str, "read" ) == 0 ) return ( READ );
    else if ( strcasecmp ( str, "append" ) == 0 ) return ( APPEND );
    else if ( strcasecmp ( str, "io" ) == 0 ) return ( IO );
    else if ( strcasecmp ( str, "tcpip" ) == 0 ) return ( TCPIP );
    else if ( strcasecmp ( str, "server" ) == 0 ) return ( SERVER );
    else if ( strcasecmp ( str, "pipe" ) == 0 ) return ( PIPE );
    else if ( strcasecmp ( str, "newpipe" ) == 0 ) return ( NEWPIPE );
  }
  return ( getError ( INT, ERRZ21 ) );
}

// ************************************************************************* //
// This function searches for an input terminator in the buffer "readbuf".  If
// the input terminator bit has been set in the integer "options", and a set
// input terminator has been received ( ie "interm ), this function will return
// the position of the input terminator in the buffer.  If no input terminator
// can be found, the the number of bytes in the buffer searched ( ie "nbytes )
// is returned.
//
// Note, a set input terminator is a control character ( ie a character with an
// integer value between 0 and 31 inclusive ) whose integer value corresponds to
// a set bit in the integer "interm".  For example, the character NULL has an
// integer value of 0.  If bit 0 in the integer "interm" is equal to 1, then
// this bit has been set.

int isINTERM (char *readbuf, int nbytes, int options, int interm)
{ int	index;
  int	value;

  if ( options & MASK[INTERM] )
  { for ( index = 0; index < nbytes; index++ )
    { value = (int)(readbuf[index]);
      if ( ( value >= 0 ) && ( value <= 31 ) )
      { if ( interm & MASK[value] )
	{ return ( index );
	}
      }
    }
  }
  return ( nbytes );
}

// ************************************************************************* //
// This function accepts an error number argument "errnum" and returns a pointer
// to the corresponding message string "errmsg".

void getErrorMsg (int errnum, char  *errmsg)
{ (void) UTIL_strerror ( -errnum, (u_char *)errmsg );
}

// ************************************************************************* //
//									     //
// INITIALISE SPECIFIC							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function initialises the channel "chan" to the defaults setting for
// an object of type "type".  If successful, it returns 0.  Otherwise, it
// returns a negative integer value to indicate the error that has occured.

int initObject (int chan, int type)
{ SQ_Chan	*c;				// Channel to initialise
  int		par;				// Channel's options
  cstring	interm;				// Input terminator(s)
  cstring	outerm;				// Output terminator
  struct termios settings;			// man 4 termios
  int		ret;				// Return value
  char		io;				// Current IO channel

  c = &(partab.jobtab->seqio[chan]);
  par = 0;
  switch ( type )
  { case SQ_FREE:
      c->type = (char)(SQ_FREE);
      break;
    case SQ_FILE:
      c->type = (char)(SQ_FILE);
      ( void ) snprintf ( (char *)outerm.buf, MAX_STR_LEN, "%c", (char)10 );
      ( void ) snprintf ( (char *)interm.buf, MAX_STR_LEN, "%c", (char)10 );
      break;
    case SQ_TCP:
      c->type = (char)(SQ_TCP);
      ( void ) snprintf ( (char *)outerm.buf, MAX_STR_LEN, "%c%c", (char)13, (char)10 );
      ( void ) snprintf ( (char *)interm.buf, MAX_STR_LEN, "%c%c", (char)13, (char)10 );
      break;
    case SQ_PIPE:
      c->type = (char)(SQ_PIPE);
      ( void ) snprintf ( (char *)outerm.buf, MAX_STR_LEN, "%c", (char)10 );
      ( void ) snprintf ( (char *)interm.buf, MAX_STR_LEN, "%c", (char)10 );
      break;
    case SQ_TERM:
      c->type = (char)(SQ_TERM);

      if ( chan == STDCHAN )		// Setup tty settings ( if STDCHAN )
      {
        ret = tcgetattr ( STDCHAN, &settings );
        if ( ret == -1 ) return ( getError ( SYS, errno ) );
        settings.c_lflag &= ~ICANON;		// Non-canonical mode
        settings.c_lflag &= ~ECHO;		// Do not echo
 	settings.c_oflag &= ~ONLCR;		// Do not map NL to CR-NL out
 	settings.c_iflag &= ~ICRNL;		// Do not map CR to NL in
        settings.c_cc[VMIN] = 1;		// Guarantees one byte is read
        settings.c_cc[VTIME] = 0;		//  in at a time
        settings.c_cc[VSUSP] = _POSIX_VDISABLE;	// Disables SIGTSTP signal
#if defined(__FreeBSD__) || defined(__NetBSD__)
        settings.c_cc[VDSUSP] = _POSIX_VDISABLE; // Funnee <Ctrl><Y> thing
	settings.c_cc[VSTATUS] = _POSIX_VDISABLE; // Disables status
#endif
	settings.c_cc[VINTR] = '\003';		// ^C
	settings.c_cc[VQUIT] = '\024';		// ^T (use for status)
        ret = tcsetattr ( STDCHAN, TCSANOW, &settings );
  						// Set parameters
        if ( ret == -1 ) return ( getError ( SYS, errno ) );
      }

      par |= SQ_USE_ECHO;
      par |= SQ_USE_ESCAPE;
      par |= SQ_USE_DEL127;
      par |= SQ_CONTROLC;
      ( void ) snprintf ( (char *)outerm.buf, MAX_STR_LEN, "%c%c", (char)13, (char)10 );
      ( void ) snprintf ( (char *)interm.buf, MAX_STR_LEN, "%c", (char)13 );
      break;
    default:
      return ( getError ( INT, ERRZ20 ) );
  }
  c->options = 0;
  c->mode = (char)UNKNOWN;
  c->fid = -1;
  (void) initSERVER ( chan, 0 );
  c->dx = 0;
  c->dy = 0;
  c->name[0] = '\0';
  c->dkey_len = 0;
  c->dkey[0] = '\0';
  outerm.len = strlen ( (char *)outerm.buf );
  interm.len = strlen ( (char *)interm.buf );
  io = partab.jobtab->io;
  (void) SQ_Use ( chan, &interm, &outerm, par );
  partab.jobtab->io = io;
  return ( 0 );
}

// ************************************************************************* //
//									     //
// WRITE SPECIFIC							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function writes "nbytes" bytes from the buffer "writebuf" to the object
// associated with the channel "chan".   Upon successful completion, the number
// of bytes actually written is returned.  Otherwise, a negative integer value
// is returned to indicate the error that has occured.

int objectWrite (int chan, char *writebuf, int nbytes)
{ SQ_Chan	*c;				// Channel to write to
  int		oid;				// Channel descriptor
  int		byteswritten;			// Bytes written
  int		bytestowrite;			// Bytes left to write
  int		ret;				// Return value

  c = &(partab.jobtab->seqio[chan]);	// Acquire a pointer to current channel

  if ( chan == STDCHAN )		// Get appropriate channel descriptor
  { oid = 1;
  }
  else if ( (int)(c->type) == SQ_TCP )
  { if ( (int)(c->mode) == TCPIP )
      oid = c->fid;
    else if ( (int)(c->mode) == FORKED )
      oid = c->fid;
    else if ( (int)(c->mode) == NOFORK )
    { if ( c->s.cid == -1 )
	return ( getError ( INT, ERRZ47 ) );
      else
	oid = c->s.cid;
    }
    else if ( (int)(c->mode) == SERVER )
    { if ( c->s.cid == -1 )
	return ( getError ( INT, ERRZ47 ) );
      else
	oid = c->s.cid;
    }
    else
      return ( getError ( INT, ERRZ20 ) );
  }
  else
    oid = c->fid;

  byteswritten = 0;			// Initialise bytes written

  while ( byteswritten < nbytes )	// Write bytes
  { bytestowrite = nbytes - byteswritten;
    switch ( (int)(c->type) )
    { case SQ_FILE:
	ret = SQ_File_Write ( oid, (u_char *)&(writebuf[byteswritten]), bytestowrite );
	break;
      case SQ_TERM:
	ret = SQ_Device_Write ( oid, (u_char *)&(writebuf[byteswritten]), bytestowrite );
	break;
      case SQ_PIPE:
	ret = SQ_Pipe_Write ( oid, (u_char *)&(writebuf[byteswritten]), bytestowrite );
	break;
      case SQ_TCP:
	ret = SQ_Tcpip_Write ( oid, (u_char *)&(writebuf[byteswritten]), bytestowrite );
	break;
      default:
	return ( getError ( INT, ERRZ30 ) );
    }

    // Increment byteswritten or return error

    if ( ret < 0 )
    { if ( partab.jobtab->trap & MASK[SIGPIPE] )
      { partab.jobtab->trap &= ~ ( MASK[SIGPIPE] );
        return ( getError ( INT, ERRZ47 ) );
      }
      else
        return ( ret );
    }
    else
      byteswritten = byteswritten + ret;
  }
  return ( byteswritten );
}

// ************************************************************************* //
//									     //
// READ SPECIFIC							     //
//									     //
//	"chan"		Guaranteed to be valid				     //
//	"buf"		Guaranteed not to be NULL, and large enough to store //
//			"maxbyt" bytes					     //
//	"maxbyt"	Guaranteed to be a positive integer value less than  //
//			MAX_STR_LEN					     //
//	"tout"		If 0, forces the read to retrieve the next character //
//			from the object.  If no character is immediately     //
//			available, a timeout will occur ( this does not	     //
//			apply to files ).  All other "tout" values are	     //
//			effectively ignored; that is, they are handled at a  //
//			higher level.					     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function reads at most "maxbyt" bytes from the file referenced by
// the channel "chan" into the buffer "buf".  Upon success, it returns the
// number of bytes read. Otherwise, it returns a negative integer value to
// indicate the error that has occured.

int readFILE (int chan, u_char *buf, int maxbyt)
{ SQ_Chan 	*c;				// Current channel
  int		ret;				// Return value
  int		bytesread;			// Bytes read
  int		crflag;				// CR received

  c = &(partab.jobtab->seqio[chan]);	// Acquire a pointer to current channel
  bytesread = 0;			// Initialise bytes read
  crflag = 0;				// Initialise CR flag
  for (;;)				// Read in bytes
  { if ( bytesread >= maxbyt )		// Check if # byts reqd are rec
    { c->dkey_len = 0;
      c->dkey[0] = '\0';
      return ( bytesread );
    }
    ret = SQ_File_Read ( c->fid, &(buf[bytesread]) ); // Read in one byte
    if ( ret < 0 )			// An error has occured
    { c->dkey_len = 0;
      c->dkey[0] = '\0';
      return ( ret );
    }
    else if ( ret == 0 )		// EOF reached
    { c->dkey_len = 1;
      c->dkey[0] = (char)255;
      c->dkey[1] = '\0';
      return ( bytesread );
    } 

    // Support for escape sequences with sockets is still to be implemented

    // Check if an input terminator has been received

    if ( c->options & MASK[INTERM] )
    { if ((int)(buf[bytesread]) <= 31 )
      { if ( c->in_term == CRLF )
        { if ( (int)(buf[bytesread]) == 13 ) crflag = 1;
          else if (((int)(buf[bytesread]) == 10 ) &&
                   ( crflag == 1 ))
          { c->dkey_len = 2;
            c->dkey[0] = (char)13;
            c->dkey[1] = (char)10;
            c->dkey[2] = '\0';
            return ( bytesread - 1 );
          }
        }
        else if ( c->in_term & MASK[(int)(buf[bytesread])] )
        { c->dkey_len = 1;
          c->dkey[0] = buf[bytesread];
          c->dkey[1] = '\0';
          return ( bytesread );
        }
      }
    }

    // Support to echo the last byte read is still to be implemented

    bytesread += 1;		// Increment total number of bytes read

  }

}

// ************************************************************************* //
// This function reads at most "maxbyt" bytes from the socket referenced by
// the channel "chan" into the buffer "buf".  Upon success, it returns the
// number of bytes read. Otherwise, it returns a negative integer value to
// indicate the error that has occured.

int readTCP (int chan, u_char *buf, int maxbyt, int tout)
{ SQ_Chan	*c;				// Useful variable
  int		oid;				// Object descriptor
  int		bytesread;			// Bytes read
  int		ret;				// Return value
  int		crflag;				// CR received

  // Aquire a pointer to the appropriate channel structure

  c = &(partab.jobtab->seqio[chan]);

  // Get peer's socket descriptor

  // SERVER	Forking server socket
  // NOFORK	Non-forking server socket
  // FORKED	Forked server socket client
  // TCPIP	Client socket

  if ( c->mode == (char)SERVER )
  { ret = 0;
    while ( ret == 0 ) ret = acceptSERVER ( chan, tout );
    oid = ret;
  }
  else if ( c->mode == (char)NOFORK )
  { ret = acceptSERVER ( chan, tout );
    oid = ret;
  }
  else oid = c->fid;

  if ( oid < 0 )				// Check for timeout
  { c->dkey_len = 0;
    c->dkey[0] = '\0';
    if ( partab.jobtab->trap & MASK[SIGALRM] )
    { c->dkey_len = 0;
      c->dkey[0] = '\0';
      partab.jobtab->trap &= ~ ( MASK[SIGALRM] );
      partab.jobtab->test = 0;
      return ( 0 );
    }
    else
    { return ( oid );
    }
  }

  bytesread = 0;				// Initialise bytes read
  crflag = 0;					// Initialise crflag
  for (;;)					// Read in bytes
  { if ( bytesread >= maxbyt )			// Check for bytes read
    { c->dkey_len = 0;
      c->dkey[0] = '\0';
      return ( bytesread );
    }
    ret = SQ_Tcpip_Read ( oid, &(buf[bytesread]), tout );	// Read 1 byte

    if ( partab.jobtab->attention )		// Check if signal received
    { ret = signalCaught ( c );
      if ( ret == SIGALRM ) return ( bytesread );
      else return ( -1 );
    }
    else if ( ret < 0 )				// Check if an error has occured
    { return ( ret );
    }
    else if ( ret == 0 )			// EOF received
    { c->dkey_len = 1;
      c->dkey[0] = (char)255;
      c->dkey[1] = '\0';
      (void) closeSERVERClient ( chan );
      return ( bytesread );
    }

    // Support for escape sequences with sockets is still to be implemented

    // Check if an input terminator has been received

    if ( c->options & MASK[INTERM] )
    { if ( (int)(buf[bytesread]) <= 31 )
      { if ( c->in_term == CRLF )
	{ if ( (int)(buf[bytesread]) == 13 ) crflag = 1;
	  else if (( (int)(buf[bytesread]) == 10 ) &&
		   ( crflag == 1 ))
	  { c->dkey_len = 2;
	    c->dkey[0] = (char)13;
	    c->dkey[1] = (char)10;
	    c->dkey[2] = '\0';
	    return ( bytesread - 1 );
	  }
	}
        else if ( c->in_term & MASK[(int)(buf[bytesread])] )
	{ c->dkey_len = 1;
	  c->dkey[0] = buf[bytesread];
	  c->dkey[1] = '\0';
          return ( bytesread );
        }
      }
    }

    // Support to echo the last byte read is still to be implemented

    bytesread += 1;			// Increment total number of bytes read
  }
}

// ************************************************************************* //
// This function reads at most "maxbyt" bytes from the pipe referenced by
// the channel "chan" into the buffer "buf".  Upon success, it returns the
// number of bytes read. Otherwise, it returns a negative integer value to
// indicate the error that has occured.

int readPIPE (int chan, u_char *buf, int maxbyt, int tout)
{ SQ_Chan	*c;					// Current channel
  int		oid;					// Object descriptor
  int		bytesread;				// Bytes read
  int		crflag;					// CR received
  int		ret, tmp;				// Return value

  // Acquire a pointer to the current channel

  c = &(partab.jobtab->seqio[chan]);
  if ( chan == STDCHAN ) oid = 0;			// STDIN
  else oid = c->fid;
  bytesread = 0;					// Initialise bytes read
  crflag = 0;						// Initialise CR flag
  for (;;)						// Read in bytes
  { if ( bytesread >= maxbyt )				// Chk # bytes recd
    { c->dkey_len = 0;
      c->dkey[0] = '\0';
      return ( bytesread );
    }
    ret = SQ_Pipe_Read ( oid, &(buf[bytesread]), tout );	// Read 1 byte
    if ( partab.jobtab->attention )			// Check for signal
    { tmp = signalCaught ( c );
      if ( tmp == SIGALRM ) return ( bytesread );
      else return ( -1 );
    }
    else if ( ret < 0 )
    { return ( ret );
    }

    // Support for escape sequences with pipes is still to be implemented

    // Check if an input terminator has been received

    else if (( c->options & MASK[INTERM] ) && (ret))
    { if ((int)(buf[bytesread]) <= 31 )
      { if ( c->in_term == CRLF )
        { if ( (int)(buf[bytesread]) == 13 ) crflag = 1;
          else if (((int)(buf[bytesread]) == 10 ) &&
                   ( crflag == 1 ))
          { c->dkey_len = 2;
            c->dkey[0] = (char)13;
            c->dkey[1] = (char)10;
            c->dkey[2] = '\0';
            return ( bytesread - 1 );
          }
        }
        else if ( c->in_term & MASK[(int)(buf[bytesread])] )
        { c->dkey_len = 1;
          c->dkey[0] = buf[bytesread];
          c->dkey[1] = '\0';
          return ( bytesread );
        }
      }
    }

    // Support to echo the last byte read is still to be implemented

    bytesread += ret;		// Increment total number of bytes read
  }
}

// ************************************************************************* //

int signalCaught (SQ_Chan *c)
{ c->dkey_len = 0;
  c->dkey[0] = '\0';
  if ( partab.jobtab->trap & MASK[SIGALRM] )
  { c->dkey_len = 0;
    c->dkey[0] = '\0';
    partab.jobtab->trap &= ~ ( MASK[SIGALRM] );
    partab.jobtab->test = 0;
    return ( SIGALRM );
  }
  else
  { return ( -1 );
  }  
}

// ************************************************************************* //
// This function reads at most "maxbyt" bytes from the device referenced by
// the channel "chan" into the buffer "buf".  Upon success, it returns the
// number of bytes read. Otherwise, it returns a negative integer value to
// indicate the error that has occured.

int readTERM (int chan, u_char *buf, int maxbyt, int tout)
{ SQ_Chan	*c;					// Current channel
  int		oid;					// Object descriptor
  int		bytesread;				// Bytes read
  int		ret;					// Return value
  int		crflag;					// CR received
  char		value;					// Useful variable
  cstring	writebuf;				// Bytes to echo

  // Aquire a pointer to the appropriate channel structure

  c = &(partab.jobtab->seqio[chan]);

  // Initialise local variables

  if ( chan == STDCHAN ) oid = 0;			// STDIN
  else oid = c->fid;
  bytesread = 0;
  crflag = 0;

  // Read in bytes

  for (;;)
  { if ( bytesread >= maxbyt )				// Chk bytes received
    { c->dkey_len = 0;
      c->dkey[0] = '\0';
      return ( bytesread );
    }

    // Read in one byte

    ret = SQ_Device_Read ( oid, &(buf[bytesread]), tout );
    if ( ret < 0 )
    { c->dkey_len = 0;
      c->dkey[0] = '\0';    
      if ( partab.jobtab->trap & MASK[SIGALRM] )
      { c->dkey_len = 0;
	c->dkey[0] = '\0';
	partab.jobtab->trap &= ~ ( MASK[SIGALRM] );
	partab.jobtab->test = 0;
	return ( bytesread );
      }
      else
	return ( ret );
    }

    // EOF received

    else if ( ret == 0 )
    { c->dkey_len = 1;
      c->dkey[0] = (char)255;
      c->dkey[1] = '\0';
      return ( bytesread );
    }

    // Check for backspace or delete key

    if (((int)(buf[bytesread]) == 8 ) || ((int)(buf[bytesread]) == 127))
    { if ((((int)(buf[bytesread]) == 8 ) && ( c->options & MASK[DEL8])) ||
	  (((int)(buf[bytesread]) == 127 ) && ( c->options & MASK[DEL127])))
      { if ( bytesread > 0 )
	{ ret = SQ_WriteStar ( (char)8 );
	  if ( ret < 0 ) return ( ret );
	  ret = SQ_WriteStar ( (char)32 );
	  if ( ret < 0 ) return ( ret );
	  ret = SQ_WriteStar ( (char)8 );
	  if ( ret < 0 ) return ( ret );
	  bytesread -= 1;
	  c->dx -= 1;
	}
	continue;
      }
    }

    // Check to see if an escape sequence is about to be received

    if (((int)(buf[bytesread]) == 27 ) &&
        ( c->options & MASK[ESC] ))
    { c->dkey_len = 1;
      c->dkey[0] = (char)27;
      for (;;)
      { if ( c->dkey_len > MAX_DKEY_LEN ) return ( getError ( INT, ERRZ39 ) );
	ret = SQ_Device_Read ( oid, &(c->dkey[c->dkey_len]), tout );
	if ( ret < 0 )
	{ c->dkey_len = 0;
	  c->dkey[0] = '\0';
	  if ( partab.jobtab->trap & MASK[SIGALRM] )	// Operation timed out
	  { partab.jobtab->trap &= ~ ( MASK[SIGALRM] );
	    partab.jobtab->test = 0; 
	    return ( bytesread );
	  }
	  else						// Read error
	    return ( ret );
	}
	else if ( ret == 0 )				// EOF received
	{ c->dkey_len = 1;
	  c->dkey[0] = (char)255;
	  c->dkey[1] = '\0';
	  return ( bytesread );
	}
	value = c->dkey[c->dkey_len];
	if ( value != 'O' )
	{ if ((((int)value >= (int)'A') &&
	        ((int)value <= (int)'Z')) ||
	      (((int)value >= (int)'a') &&
	       ((int)value <= (int)'z')) ||
	     ( value == '~' ))
	  { c->dkey_len += 1;
	    c->dkey[c->dkey_len] = '\0';
	    return ( bytesread );
	  }
	}
	c->dkey_len += 1;
      }
    }

    // Check if an input terminator has been received

    if ( c->options & MASK[INTERM] ) {
      if ( (int)(buf[bytesread]) <= 31 )
      { 
        if ( c->in_term == CRLF )
        { if ( (int)(buf[bytesread]) == 13 ) crflag = 1;
          else if (
                  ( (int)(buf[bytesread]) == 10 ) &&
                  ( crflag == 1 )
                  )
          { c->dkey_len = 2;
            c->dkey[0] = (char)13;
            c->dkey[1] = (char)10;
            c->dkey[2] = '\0';
            return ( bytesread - 1 );
          }
        }
        else if ( c->in_term & MASK[(int)(buf[bytesread])] )
        { c->dkey_len = 1;
          c->dkey[0] = buf[bytesread];
          c->dkey[1] = '\0';
          return ( bytesread );
        }
      }
    } 

    // Echo last byte read

    if ( c->options & MASK[TTYECHO] )
    { writebuf.len = 1;
      sprintf ( (char *)writebuf.buf, "%c", buf[bytesread] );
      ret = SQ_Write ( &writebuf );
      if ( ret < 0 ) return ( ret );
    }
    bytesread += 1;			// Increment number of bytes read
  }
}

// ************************************************************************* //
//									     //
// SERVER SOCKET SPECIFIC						     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function initialises the forktab structure.  It does not generate
// any errors.

void initFORK (forktab *f)
{ f->job_no = -1;
  f->pid = -1;
}

// ************************************************************************* //
// This function initialises the servertab structure.  "chan" is a server
// socket, where "size" represents the maximum number of processes this
// socket can spawn to handle incoming client connections.  It returns 0 on
// success.  Otherwise, it returns a negative integer value to indicate the
// error that has occurred.

int initSERVER (int chan, int size)
{ servertab	*s;
  forktab	*f = NULL;		//***TEMP FIX
  int		index;

  if ( size > systab->maxjob ) return ( getError ( INT, ERRZ42 ) );
  s = &(partab.jobtab->seqio[chan].s);
  if (size)				//***TEMP FIX
  { f = malloc ( sizeof ( forktab ) * size );   ///to forktab from fork
    if ( f == NULL ) return ( getError ( SYS, errno ) );
  }					//***TEMP FIX
  for ( index = 0; index < size; index++ ) initFORK ( &(f[index]) );
  s->slots = size;
  s->taken = 0;
  s->cid = -1;
  s->name[0] = '\0';
  s->forked = f;
  return ( 0 );
}

// ************************************************************************* //
// This function opens a server socket on channel "chan".  "oper" is used
// to determine the type of server socket to open:
//
//	SERVER		Forking server
//	NOFORK		Non-forking server
//
// If successfull, this function returns 0.  Otherwise, it returns a
// negative integer value to indicate the error that has occurred.

int openSERVER (int chan, char *oper)
{ SQ_Chan	*c;
  char	*ptr;
  int	ret;
  int	size;

  // Acquire a pointer to the SQ_Chan structure

  c = &(partab.jobtab->seqio[chan]);

  // Extract size component from oper ( SERVER[=size] )

  ptr = strchr ( oper, (int)'=' );
  if ( ptr == NULL )
  { c->mode = (char)NOFORK;
    if ( ( ret = initSERVER ( chan, 0 ) ) < 0 ) return ( ret );
  }
  else
  { ptr += 1;
    if ( *ptr == '\0' ) 
    { c->mode = (char)NOFORK;
      if ( ( ret = initSERVER ( chan, 0 ) ) < 0 ) return ( ret );
    }
    else
    { size = atoi ( ptr );
      if ( size == -1 ) return ( getError ( SYS, errno ) );
      else
      { if ( ( ret = initSERVER ( chan, size ) ) < 0 ) return ( ret );
      }
    }
  }
  return ( 0 );
}

// ************************************************************************* //
// This function:
//
//	- Accepts a pending connection ( if no clients are currently
//	  connected ) on the channel "chan";
//	- Forks a new process to handle the connection if any slots are
//	  available; and
//	- Maintains a table of forked child processes ( removing "dead"
//	  processes as required ).
//
// If successfull, this function will return the descriptor which references
// the current connected client.  Otherwise, it will return a negative integer
// value to indicate the error that has occurred.

int acceptSERVER (int chan, int tout)
{ servertab	*s;				// Forked process table
  SQ_Chan	*c;				// Server socket
  int		index;				// Useful variable
  int		len;				// Useful variable
  int		ret;				// Return value
  struct sockaddr_in	sin;			// Peer socket
//  struct hostent	*host;			// Peer host name
  forktab	*slot;				// Forked process
  int		jobno;				// Mumps job number

  // Acquire a pointer to the SQ_CHAN structure

  c = &(partab.jobtab->seqio[chan]);

  // Acquire a pointer to the SERVERTAB structure

  s = &(c->s);

  // Removing any dead child processes is only required if:
  //
  // s->slots > 0 AND
  // s->taken > 0

  if ( ( s->slots > 0 ) && ( s->taken > 0 ) )
  { for ( index = 0; index < s->slots; index++ )
    { if ( s->forked[index].pid != -1 )
      { if ( systab->jobtab[(s->forked[index].job_no - 1)].pid !=
	     s->forked[index].pid )
	{						// Child dead
	  initFORK ( &(s->forked[index]) );
	  s->taken = s->taken - 1;
	}
      }
    }
  }

  // An accept is only required if:
  //
  // s->cid == -1

  if ( s->cid == -1 )
  { s->cid = SQ_Tcpip_Accept ( c->fid, tout );
    if ( s->cid < 0 )
    { s->cid = -1;
      return ( s->cid );
    }

    len = sizeof ( struct sockaddr_in );
    ret = getpeername ( s->cid, (struct sockaddr *) &sin, (socklen_t *)&len );
    if ( ret == -1 ) return ( getError ( SYS, errno ) );
    len = sizeof ( sin.sin_addr );
    (void) snprintf ( (char *)s->name, MAX_SEQ_NAME, "%s %d",
    		inet_ntoa ( sin.sin_addr ), ntohs ( sin.sin_port ) );

//    host = gethostbyaddr ( inet_ntoa ( sin.sin_addr ), len, AF_INET );
//    if ( host == NULL )
//      (void) snprintf ( s->name, MAX_SEQ_NAME, "%s %d",
//			inet_ntoa ( sin.sin_addr ), ntohs ( sin.sin_port ) );
//    else
//      (void) snprintf ( s->name, MAX_SEQ_NAME, "%s %d",
//			host->h_name, ntohs ( sin.sin_port ) );
  }

  // A rfork is only required if:
  //
  // s->slots > 0 AND
  // s->taken < s->slots

  if ( ( s->slots > 0 ) && ( s->taken < s->slots ) )
  { slot = NULL;			// Find first available slot
    for ( index = 0; index < s->slots; index++ )
    { if ( s->forked[index].pid == -1 )
      { slot = &(s->forked[index]);
	index = s->slots;
      }
    }
    if ( slot == NULL ) return ( getError ( INT, ERRZ20 ) );

    // Spawn server client process

    jobno = ForkIt ( 1 );			// copy the file table
    if ( jobno > 0 )                            // Parent process; child jobno 
    {
      slot->job_no = jobno;
      slot->pid = systab->jobtab[(jobno - 1)].pid;
      s->taken = s->taken + 1;
      close ( s->cid );
      s->cid = -1;
      s->name[0] = '\0';
      return ( 0 );
    }
    else if ( jobno < 0 )			// Child process; parent jobno
    {
      c = &(partab.jobtab->seqio[chan]);
      s = &(c->s);
      s->slots = 0;
      s->taken = 0;
      free ( s->forked );
      close ( c->fid );
      c->mode = (char)FORKED;
      c->fid = s->cid;
      (void) snprintf ( (char *)c->name, MAX_SEQ_NAME, "%s", s->name );
      s->cid = -1;
      s->name[0] = '\0';
      return ( c->fid );
    }
    else					// Rfork failed
      return ( getError ( SYS, errno ) );

    // An unknown error has occured

    return ( getError ( INT, ERRZ20 ) );

  }

  // Accepted by parent

  return ( s->cid );
}

// ************************************************************************* //
// This function closes a connected client.  This function will never
// exit with an error ( ie will always return 0 ).

int closeSERVERClient (int chan)
{ SQ_Chan       *c;                             // Socket channel
    
  // Acquire a pointer to the channel

  c = &(partab.jobtab->seqio[chan]);
  
  // Determine socket to close and close it ( if required )

  if ( (int)(c->type) == SQ_TCP )
  { 
    if ( (int)(c->mode) == NOFORK )
    {
      if ( c->s.cid > -1 ) 
      {
        (void) close ( c->s.cid );
        free ( c->s.forked ); 
        (void) initSERVER ( chan, 0 );
      }
    }
    else if ( (int)(c->mode) == SERVER )
    {
      if ( c->s.cid > -1 )
      { 
        (void) close ( c->s.cid ); 
        c->s.cid = -1;
        c->s.name[0] = '\0';
      }
    }   
  }
  return ( 0 );
}

// ************************************************************************* //
// This function closes the socket on channel "chan".  This function is
// called whenever a channel of type SQ_TCP is closed.  It will never exit
// with an error ( ie will always return 0 ).

int closeSERVER (int chan)
{ SQ_Chan       *c;                                     // Socket
  servertab     *s;                                     // Forked process table

  c = &(partab.jobtab->seqio[chan]);
  s = &(c->s);
  switch ( (int)c->mode )				// Close socket client
  { case TCPIP:
      (void) close ( c->fid );
      c->type = (u_char)SQ_FREE;
      break;

    // Does not close all forked child processes ( just the server socket and
    // the client ( if one exists ) accepted by the server parent )
    
    case SERVER:
      if ( s->cid != -1 ) (void) close ( s->cid );
      free ( s->forked );				// Free fork structure
      (void) close ( c->fid );
      c->type = (u_char)SQ_FREE;
      break;
    
    // Closes the connected client ( if one exists ) and the server socket
    
    case NOFORK:
      if ( s->cid != -1 ) (void) close ( s->cid );
      (void) close ( c->fid );
      c->type = (u_char)SQ_FREE;
      break;
    
    // Closes the connected client accepted by a forked child process
    
    case FORKED:
      (void) close ( c->fid );
      c->type = (u_char)SQ_FREE;
      break;
    
   // Unknown class of socket
    
    default:
      break;
  } 
  
  return ( 0 );
}   
