// File: mumps/include/mv1conn.h
//
// MV1 connection library

/*      Copyright (c) 1999 - 2016
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

// sizeof() for structures added 23 Sep 2013.  NOTE: will change with changes
// to constant defs and the sizeof() info below will not be updated - rdn

#ifndef _MUMPS_MV1CONN_H_                       // only do this once
#define _MUMPS_MV1CONN_H_

#include <sys/types.h>
#include <termios.h>
#include "mumps.h"

typedef struct _MV1CONN
{ char *file;                                   // database file name
  int dbfd;                                     // database file descriptor
  short start_type;		                // how we started
  struct termios tty_settings;			// man 4 termios
  int ret;					// return value
  int env_num;				        // startup environment number
  int ssp;					// string stack ptr
  int asp;					// address stack ptr
} MV1CONN;

int MV1_Initialize(MV1CONN *hnd, char *file, char *env);
int MV1_Xecute(MV1CONN *hnd, char *cmd);
int MV1_Rundown(MV1CONN *hnd);

typedef struct _MV1VAR
{
  chr_x  volset;
  chr_x  env;
  u_char nsubs;                 // max. 63
  u_char spos[64];              // subscript positions
  u_char slen[64];              // subscript lengths
  mvar   var_m;                 // contains keys, keylen, varname
} MV1VAR;

int MV1VAR_Init(MV1VAR *var);
int MV1VAR_ClearVar(MV1VAR *var);
int MV1VAR_ExtractVar(MV1VAR *var, char *glb, char *env, char *volset);
int MV1VAR_InsertVar(MV1VAR *var, char *glb, char *env, char *volset);

int MV1VAR_ClearSubs(MV1VAR *var);
int MV1VAR_Count(MV1VAR *var, int *cnt);

int MV1VAR_ExtractDouble(MV1VAR *var, int pos, double *val);
int MV1VAR_ExtractLong(MV1VAR *var, int pos, long *val);
int MV1VAR_ExtractString(MV1VAR *var, int pos, unsigned char *val, int *len);

int MV1VAR_InsertDouble(MV1VAR *var, int pos, double val);
int MV1VAR_InsertLong(MV1VAR *var, int pos, long val);
int MV1VAR_InsertString(MV1VAR *var, int pos, unsigned char *val, int len);
int MV1VAR_InsertNull(MV1VAR *var, int pos);

int MV1VAR_AppendDouble(MV1VAR *var, double val);
int MV1VAR_AppendLong(MV1VAR *var, long val);
int MV1VAR_AppendString(MV1VAR *var, unsigned char *val, int len);
int MV1VAR_AppendNull(MV1VAR *var);

int MV1_GetDouble(MV1CONN *conn, MV1VAR *var, double *val);
int MV1_GetLong(MV1CONN *conn, MV1VAR *var, long *val);
int MV1_GetString(MV1CONN *conn, MV1VAR *var, unsigned char *val, int *len);

int MV1_SetDouble(MV1CONN *conn, MV1VAR *var, double val);
int MV1_SetLong(MV1CONN *conn, MV1VAR *var, long val);
int MV1_SetString(MV1CONN *conn, MV1VAR *var, unsigned char *val, int len);
int MV1_SetNull(MV1CONN *conn, MV1VAR *var);
 
int MV1_Kill(MV1CONN *conn, MV1VAR *var);

int MV1_Data(MV1CONN *conn, MV1VAR *var, int *dval);
int MV1_Order(MV1CONN *conn, MV1VAR *var, int dir, unsigned char *sibling, int *len);
int MV1_Query(MV1CONN *conn, MV1VAR *var, int dir, MV1VAR *next);

int MV1_Lock(MV1CONN *conn, MV1VAR *var, int incr);
int MV1_Unlock(MV1CONN *conn, MV1VAR *var, int decr);

#endif                                          // _MUMPS_MV1CONN_H_
