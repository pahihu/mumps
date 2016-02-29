// File: mumps/include/seqio.h
//
// This module declares all the global constants and functions used only by
// sequential Input/Output ( IO ) operations.
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


#ifndef _MUMPS_SEQIO_H_				// only do this once
#define _MUMPS_SEQIO_H_

#include	<sys/types.h>
#include	"mumps.h"

// ************************************************************************* //
//									     //
// Global constants							     //
//									     //
// ************************************************************************* //

// IO operations

#define		WRITE		1		// Write only
#define		READ		2		// Read only
#define		APPEND		3		// Append
#define		IO		4		// Write/Read
#define		TCPIP		5		// Client socket
#define		SERVER		6		// Forking server socket
#define		NOFORK		7		// Non-forking server socket
#define		FORKED		8		// Forked server socket client
#define		PIPE		9		// Named pipe ( writing )
#define		NEWPIPE		10		// Named pipe ( reading )

// Signal operations

#define		CATCH		0		// Catch signal
#define		IGNORE		1		// Ignore signal

// Types of errors

#define		SYS		0		// System error
#define		INT		1		// Internal error

// File/Pipe permissions

#define		MODE		S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH

// Select operations

#define		FDRD		0		// Ready to read on object
#define		FDWR		1		// Ready to write on object

// ************************************************************************* //
//									     //
// Global function declarations						     //
//									     //
// ************************************************************************* //

void setSignalBitMask ( int sig );
int setSignal ( int sig, int flag );
int setSignals ( void );
void printBytestr ( char *bytestr, int nbytes );
void printSQChan ( jobtab *jobptr, SQ_Chan *chanptr );
int seqioSelect ( int sid, int type, int tout );
int getError ( int type, int errnum );
int SQ_File_Open ( char *file, int op );
int SQ_File_Write ( int fid, u_char *writebuf, int nbytes );
int SQ_File_Read ( int fid, u_char *readbuf );
int SQ_Device_Open ( char *device, int op );
int SQ_Device_Write ( int did, u_char *writebuf, int nbytes );
int SQ_Device_Read ( int did, u_char *readbuf, int tout );
int SQ_Pipe_Open ( char *pipe, int op );
int SQ_Pipe_Close ( int pid, char *pipe );
int SQ_Pipe_Read ( int pid, u_char *readbuf, int tout );
int SQ_Pipe_Write ( int pid, u_char *writebuf, int nbytes );
int SQ_Tcpip_Open ( char *bind, int op );
int SQ_Tcpip_Write ( int sid, u_char *writebuf, int nbytes );
int SQ_Tcpip_Accept ( int sid, int tout );
int SQ_Tcpip_Read ( int sid, u_char *readbuf, int tout );

#endif						// _MUMPS_SEQIO_H_
