// File: mumps/mv1api/mv1api_glb.c
//
// MV1 Connection API library

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "mumps.h"
#include "proto.h"
#include "mv1api.h"

extern mvar db_var;

static
short Resolve_UCI_VOLSET(MV1DB *hnd, MV1VAR* var)
{
  int i;

  if (var->resolved_uci_volset)
    return 0;

  // TODO: if empty, then defaults from MV1DB
  for (i = 0; i < MAX_VOL; i++)
    if (systab->vol[i] != NULL)
      if (X_EQ(systab->vol[i]->vollab->volnam.var_xu, var->volset))
        break;
  if (i == MAX_VOL) return -ERRM26;
  var->var_m.volset = i + 1;
   
  if (0 == var->var_m.volset)
    var->var_m.volset = partab.jobtab->vol;

  for (i = 0; i < UCIS; i++)
    if (X_EQ(systab->vol[var->var_m.volset-1]->vollab->uci[i].name.var_xu,
             var->env))
      break;
  if (i == UCIS) return -ERRM26;
  var->var_m.uci = i + 1;

  var->resolved_uci_volset = 1;

  return 0;
}

int mv1_global_get(MV1DB *hnd, MV1VAR *var, u_char *val, int *len)
{
  short s;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  s = DB_Get(&var->var_m, val);
  if (s < 0)
    return s;

  *len = s;
  return 0;
}

int mv1_global_set(MV1DB *hnd, MV1VAR *var, u_char *val, int len)
{
  short s;
  cstring cstr;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  if (0 == len)
    len = strlen((char*) val);

  bcopy(val, &cstr.buf[0], len);
  cstr.len = len;
  s = DB_Set(&var->var_m, &cstr);
  if (s < 0)
    return s;

  return 0;
}

int mv1_global_set_null(MV1DB *hnd, MV1VAR *var)
{
  short s;
  cstring cstr;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  cstr.len = 0;
  s = DB_Set(&var->var_m, &cstr);
  if (s < 0)
    return s;

  return 0;
}

int mv1_global_kill(MV1DB *hnd, MV1VAR *var)
{
  short s;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  return DB_Kill(&var->var_m);
}

int mv1_global_data(MV1DB *hnd, MV1VAR *var, int *dval)
{
  short s;
  cstring cstr;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  s = DB_Data(&var->var_m, &cstr.buf[0]);
  if (s < 0)
    return s;
  cstr.buf[s] = '\0';
  *dval = atol((char*) &cstr.buf[0]);
  return 0;
}

int mv1_global_order(MV1DB *hnd, MV1VAR *var, int dir,
                u_char *sibling, int *len)
{
  short s;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  s = DB_Order(&var->var_m, sibling, dir);
  if (s < 0)
    return s;

  *len = s;
  return 0;
}

int mv1_global_query(MV1DB *hnd, MV1VAR *var, int dir, MV1VAR *next)
{
  short s;
  cstring cstr;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  // TODO: inefficient, it converts to string
  s = DB_Query(&var->var_m, &cstr.buf[0], dir);
  if (s < 0)
    return s;
  bcopy(&db_var, next, sizeof(mvar));
  // TODO: setup spos/slen/nsubs
  return -1;
}

int mv1_global_next(MV1DB *hnd, MV1VAR *var, u_char *data, int *dlen)
{
  short s;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  // TODO: uses mcopy() inside...
  s = DB_QueryD(&var->var_m, data);
  if (s < 0)
    return s;

  *dlen = s;
  // TODO: setup spos/slen/nsubs
  return -1;
}

int mv1_global_lock(MV1DB *hnd, MV1VAR *var, int incr, int timeout)
{
  short s;
  cstring cstr;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  s = UTIL_mvartolock(&var->var_m, &cstr.buf[0]);
  if (s < 0)
    return s;
  cstr.len = s;
  return incr ? LCK_Add(1, &cstr, timeout) : LCK_Old(1, &cstr, timeout);
}

int mv1_global_unlock(MV1DB *hnd, MV1VAR *var)
{
  short s;
  cstring cstr;

  s = Resolve_UCI_VOLSET(hnd, var);
  if (s < 0)
    return 0;

  s = UTIL_mvartolock(&var->var_m, &cstr.buf[0]);
  if (s < 0)
    return s;
  return LCK_Sub(1, &cstr);
}

