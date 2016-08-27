// File: mumps/connect/mv1var.c
//
// MV1 connection library

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
#include <errno.h>
#include <strings.h>
#include <sys/types.h>
#include "mumps.h"
#include "proto.h"
#include "mv1conn.h"

#if 0
typedef struct _MV1VAR
{
  chr_x  volset[MAX_NAME_BYTES];
  chr_x  env[MAX_NAME_BYTES];
  u_char nsubs;                 // max. 63
  u_char subsidx[64];  
  mvar   mvar;                  // contains keys, keylen, varname
} MV1VAR;
#endif

int MV1VAR_Init(MV1VAR *var)
{
  bzero(var, sizeof(MV1VAR));
  return 0;
}

int MV1VAR_ClearVar(MV1VAR *var)
{
  X_Clear(var->volset);
  X_Clear(var->env);
  X_Clear(var->var_m.name.var_xu);
  return 0;
}

int MV1VAR_ExtractVar(MV1VAR *var, char *glb, char *env, char *volset)
{
  return -1;
}

int MV1VAR_InsertVar(MV1VAR *var, char *glb, char *env, char *volset)
{
  return -1;
}

int MV1VAR_ClearSubs(MV1VAR *var)
{
  var->nsubs = 0;
  var->var_m.slen = 0;
  return 0;
}

int MV1VAR_Count(MV1VAR *var, int *cnt)
{
  *cnt = var->nsubs;
  return 0;
}

int MV1VAR_ExtractDouble(MV1VAR *var, int pos, double *val)
{
  return -1;
}

int MV1VAR_ExtractLong(MV1VAR *var, int pos, long *val)
{
  return -1;
}

int MV1VAR_ExtractString(MV1VAR *var, int pos, unsigned char *val, int *len)
{
  return -1;
}

static
int InsertCString(MV1VAR *var, int pos, cstring *cstr)
{
  u_char *dst;
  short s;

  if ((pos < 0) || (pos > 1 + var->nsubs) || (pos > 63))
    return EINVAL;

  if (0 != pos)
    var->spos[pos] = var->spos[pos - 1] + var->slen[pos - 1];
  else
    var->spos[pos] = 0;

  dst = &var->var_m.key[var->spos[pos]];
  s = UTIL_Key_Build(cstr, dst);
  if (s < 0)
    return s;

  var->slen[pos] = s;
  var->var_m.slen = var->spos[pos] + s;
  var->nsubs = 1 + pos;

  return 0;
}

int MV1VAR_InsertDouble(MV1VAR *var, int pos, double val)
{
  cstring cstr;

  sprintf((char*) &cstr.buf[0], "%f", val);
  cstr.len = strlen((char*) &cstr.buf[0]);
  return InsertCString(var, pos, &cstr);
}

int MV1VAR_InsertLong(MV1VAR *var, int pos, long val)
{
  cstring cstr;

  sprintf((char*) &cstr.buf[0], "%ld", val);
  cstr.len = strlen((char*) &cstr.buf[0]);
  return InsertCString(var, pos, &cstr);
}

int MV1VAR_InsertString(MV1VAR *var, int pos, unsigned char *val, int len)
{
   cstring cstr;

   if (0 == len)
     len = strlen((char*) val);
   bcopy(val, &cstr.buf[0], len);
   cstr.len = len;
   return InsertCString(var, pos, &cstr);
}

int MV1VAR_InsertNull(MV1VAR *var, int pos)
{
  cstring cstr;

  cstr.len = 0;
  return InsertCString(var, pos, &cstr);
}

int MV1VAR_AppendDouble(MV1VAR *var, double val)
{
  return MV1VAR_InsertDouble(var, 1 + var->nsubs, val);
}

int MV1VAR_AppendLong(MV1VAR *var, long val)
{
  return MV1VAR_InsertLong(var, 1 + var->nsubs, val);
}

int MV1VAR_AppendString(MV1VAR *var, unsigned char *val, int len)
{
  return MV1VAR_InsertString(var, 1 + var->nsubs, val, len);
}

int MV1VAR_AppendNull(MV1VAR *var)
{
  return MV1VAR_InsertNull(var, 1 + var->nsubs);
}

