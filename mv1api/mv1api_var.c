// File: mumps/mv1api/mv1api_var.c
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
#include <errno.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include "mumps.h"
#include "proto.h"
#include "mv1api.h"

#if 0
typedef struct _MV1VAR
{
  chr_x  volset[MAX_NAME_BYTES];
  chr_x  env[MAX_NAME_BYTES];
  u_char resolved_uci_volset;
  u_char nsubs;                 // max. 63
  u_char subsidx[64];  
  mvar   mvar;                  // contains keys, keylen, varname
} MV1VAR;
#endif

int mv1_var_init(MV1VAR *var)
{
  bzero(var, sizeof(MV1VAR));                           // clear MV1VAR
  return 0;
} 

int mv1_var_clear(MV1VAR *var)
{
  X_Clear(var->volset);                                 // clear volset
  X_Clear(var->env);                                    //   env
  X_Clear(var->var_m.name.var_xu);                      //   glb name
  return 0;
}

int mv1_var_extract(MV1VAR *var, char *glb, char *env, char *volset)
{
  int i;

  for (i = 0; (*glb++ = var->var_m.name.var_cu[i]); i++);
  for (i = 0; (*env++ = var->env.buf[i]); i++);
  for (i = 0; (*volset++ = var->volset.buf[i]); i++);
  return 0;
}

int mv1_var_insert(MV1VAR *var, char *glb, char *env, char *volset)
{
  int i, c;

  if (glb)                                              // overwrite glb
  { for (i = 0; i < MAX_NAME_BYTES; i++)                //   if given
    { var->var_m.name.var_cu[i] = c = *glb++;
      if (0 == c) break;
    }
    if (c)                                              // if too long,
      return -(ERRMLAST+ERRZ12);                        //   complain
  }

  if (volset || env)                                    // clear resolved
    var->resolved_uci_volset = 0;                       //   if volset or env
                                                        //   given

  if (volset)                                           // overwrite volset
  { for (i = 0; i < MAX_NAME_BYTES; i++)                //   if given
    { var->volset.buf[i] = c = *volset++;
      if (0 == c) break;
    }
    if (c)                                              // if too long,
      return -(ERRMLAST+ERRZ12);                        //   complain
  }

  if (env)                                              // overwrite env
  { for (i = 0; i < MAX_NAME_BYTES; i++)                //   if given
    { var->env.buf[i] = c = *env++;
      if (0 == c) break;
    }
    if (c)                                              // if too long,
      return -(ERRMLAST+ERRZ12);                        //   complain
  }

  return 0;
}

int mv1_subs_clear(MV1VAR *var)
{
  var->nsubs = 0;                                       // clear subscripts
  var->var_m.slen = 0;
  return 0;
}

int mv1_subs_count(MV1VAR *var, int *cnt)
{
  *cnt = var->nsubs;                                    // return #subscripts
  return 0;
}

int mv1_subs_extract(MV1VAR *var, int pos, unsigned char *val, int *len)
{
  short s;

  if ((pos < 0) || (pos > var->nsubs - 1))
    return EINVAL;

  *len = 0;
  s = UTIL_Key_Extract(&var->var_m.key[var->spos[pos]], val, len);
  if (s < 0)
    return s;

  return 0;
}

static
int insert_cstring(MV1VAR *var, int pos, cstring *cstr)
{
  u_char *dst;
  short s;

  if ((pos < 0) || (pos > var->nsubs) || (pos >= 63))   // max. 63 subscripts
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

int mv1_subs_insert(MV1VAR *var, int pos, unsigned char *val, int len)
{
   cstring cstr;

   if (0 == len)
     len = strlen((char*) val);
   bcopy(val, &cstr.buf[0], len);
   cstr.len = len;
   return insert_cstring(var, pos, &cstr);
}

int mv1_subs_insert_null(MV1VAR *var, int pos)
{
  cstring cstr;

  cstr.len = 0;
  return insert_cstring(var, pos, &cstr);
}

int mv1_subs_append(MV1VAR *var, unsigned char *val, int len)
{
  return mv1_subs_insert(var, var->nsubs, val, len);
}

int mv1_subs_append_null(MV1VAR *var)
{
  return mv1_subs_insert_null(var, var->nsubs);
}

