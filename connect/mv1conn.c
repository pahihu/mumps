// File: mumps/connect/mv1conn.c
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
#include <string.h>
#include <sys/types.h>
#include "mumps.h"
#include "proto.h"
#include "mv1conn.h"

int MV1_GetDouble(MV1CONN *conn, MV1VAR *var, double *val)
{
  int res, len;
  cstring cstr;

  len = sizeof(cstr.buf);

  res = MV1_GetString(conn, var, &cstr.buf[0], &len);
  if (res)
    return res;

  *val = atof((char*) &cstr.buf[0]);
  return 0;
}

int MV1_GetLong(MV1CONN *conn, MV1VAR *var, long *val)
{
  int res, len;
  cstring cstr;

  len = sizeof(cstr.buf);

  res = MV1_GetString(conn, var, &cstr.buf[0], &len);
  if (res)
    return res;

  *val = atol((char*) &cstr.buf[0]);
  return 0;
}

int MV1_GetString(MV1CONN *conn, MV1VAR *var, unsigned char *val, int *len)
{
  short s;

  s = DB_Get(&var->var_m, val);
  if (s < 0)
    return s;

  *len = s;
  return 0;
}

int MV1_SetDouble(MV1CONN *conn, MV1VAR *var, double val)
{
  short s;
  cstring cstr;

  sprintf((char*) &cstr.buf[0], "%f", val);
  cstr.len = strlen((char *) &cstr.buf[0]);

  s = DB_Set(&var->var_m, &cstr);
  if (s < 0)
    return s;

  return 0;
}

int MV1_SetLong(MV1CONN *conn, MV1VAR *var, long val)
{
  short s;
  cstring cstr;

  sprintf((char*) &cstr.buf[0], "%ld", val);
  cstr.len = strlen((char*) &cstr.buf[0]);

  s = DB_Set(&var->var_m, &cstr);
  if (s < 0)
    return s;

  return 0;
}

int MV1_SetString(MV1CONN *conn, MV1VAR *var, unsigned char *val, int len)
{
  short s;
  cstring cstr;

  if (0 == len)
    len = strlen((char*) val);

  bcopy(val, &cstr.buf[0], len);
  cstr.len = len;
  s = DB_Set(&var->var_m, &cstr);
  if (s < 0)
    return s;

  return 0;
}

int MV1_SetNull(MV1CONN *conn, MV1VAR *var)
{
  short s;
  cstring cstr;

  cstr.len = 0;
  s = DB_Set(&var->var_m, &cstr);
  if (s < 0)
    return s;

  return 0;
}

int MV1_Kill(MV1CONN *conn, MV1VAR *var)
{
  return -1;
}

int MV1_Data(MV1CONN *conn, MV1VAR *var, int *dval)
{
  return -1;
}

int MV1_Order(MV1CONN *conn, MV1VAR *var, int dir,
                unsigned char *sibling, int *len)
{
  return -1;
}

int MV1_Query(MV1CONN *conn, MV1VAR *var, int dir, MV1VAR *next)
{
  return -1;
}

int MV1_Lock(MV1CONN *conn, MV1VAR *var, int incr)
{
  return -1;
}

int MV1_Unlock(MV1CONN *conn, MV1VAR *var, int decr)
{
  return -1;
}

