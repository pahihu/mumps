// File: mumps/mv1api//mv1api.h
//
// MV1 connection API

/*      Copyright (c) 2016
 *      Andras Pahi.  All rights reserved.
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

#ifndef _MUMPS_MV1API_H_                       // only do this once
#define _MUMPS_MV1API_H_

#include <sys/types.h>
#include <termios.h>
#include "mumps.h"

typedef struct _MV1DB
{ char *file;                                   // database file name
  int dbfd;                                     // database file descriptor
  short start_type;		                // how we started
  struct termios tty_settings;			// man 4 termios
  int ret;					// return value
  int env_num;				        // startup environment number
  int ssp;					// string stack ptr
  int asp;					// address stack ptr
} MV1DB;

int mv1_initialize(MV1DB *db, char *file, char *env);
int mv1_xecute(MV1DB *db, char *cmd);
int mv1_rundown(MV1DB *db);

typedef struct _MV1VAR
{
  chr_x  volset;
  chr_x  env;
  u_char nsubs;                 // max. 63
  u_char spos[64];              // subscript positions
  u_char slen[64];              // subscript lengths
  mvar   var_m;                 // contains keys, keylen, varname
} MV1VAR;

/* global variable reference functions */
int mv1_var_init(MV1VAR *var);
int mv1_var_clear(MV1VAR *var);
int mv1_var_extract(MV1VAR *var, char *glb, char *env, char *volset);
int mv1_var_insert(MV1VAR *var, char *glb, char *env, char *volset);

int mv1_subs_clear(MV1VAR *var);
int mv1_subs_count(MV1VAR *var, int *cnt);

int mv1_subs_extract(MV1VAR *var, int pos, u_char *val, int *len);
int mv1_subs_insert(MV1VAR *var, int pos, u_char *val, int len);
int mv1_subs_insert_null(MV1VAR *var, int pos);

int mv1_subs_append(MV1VAR *var, u_char *val, int len);
int mv1_subs_append_null(MV1VAR *var);

/* global functions */
int mv1_global_get(MV1DB *db, MV1VAR *var, u_char *val, int *len);

int mv1_global_set(MV1DB *db, MV1VAR *var, u_char *val, int len);
int mv1_global_set_null(MV1DB *db, MV1VAR *var);
 
int mv1_global_kill(MV1DB *db, MV1VAR *var);

int mv1_global_data(MV1DB *db, MV1VAR *var, int *dval);
int mv1_global_order(MV1DB *db, MV1VAR *var, int dir, u_char *subs, int *len);
int mv1_global_query(MV1DB *db, MV1VAR *var, int dir, MV1VAR *next);

int mv1_global_lock(MV1DB *db, MV1VAR *var, int incr);
int mv1_global_unlock(MV1DB *db, MV1VAR *var, int decr);

#endif                                          // _MUMPS_MV1API_H_
