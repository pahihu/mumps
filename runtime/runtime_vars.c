// File: mumps/runtime/runtime_vars.c
//
// module runtime - RunTime Variables

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


#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>                              // error stuf
#include <time.h>                               // for $H
#include <sys/time.h>                           // for $ZH
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"                              // standard errors

// All variables use the following structure
//
// short Vname(u_char *ret_buffer)
//
// The argument is of type *u_char and is the destination
// for the value returned by the function (max size is 32767).
// The function returns a count of characters in the return string
// or a negative error number (to be defined).
// The function name is Vvarname where thevariable call is $varname.
//

//***********************************************************************
// $ECODE
//
short Vecode(u_char *ret_buffer)                // $ECODE
{ mvar *var;					// for ST_Get
  short s;
  var = (mvar *) ret_buffer;			// use here for the mvar
  X_set("$ECODE\0\0", &var->name.var_cu[0], 8);	// get the name
  var->volset = 0;				// clear volset
  var->uci = UCI_IS_LOCALVAR;			// local var
  var->slen = 0;				// no subscripts
  s =  ST_Get(var, ret_buffer);			// get it
  if (s == -ERRM6)
  { s = 0;					// ignore undef
    ret_buffer[0] = '\0';			// null terminate
  }
  return s;
}

//***********************************************************************
// $ETRAP
//
short Vetrap(u_char *ret_buffer)                // $ETRAP
{ mvar *var;					// for ST_Get
  short s;
  var = (mvar *) ret_buffer;			// use here for the mvar
  X_set("$ETRAP\0\0", &var->name.var_cu[0], 8);	// get the name
  var->volset = 0;				// clear volset
  var->uci = UCI_IS_LOCALVAR;			// local var
  var->slen = 0;				// no subscripts
  s = ST_Get(var, ret_buffer);			// exit with result
  if (s == -ERRM6) s = 0;			// ignore undef
  return s;
}

//***********************************************************************
// $HOROLOG
//
short Vhorolog(u_char *ret_buffer)              // $HOROLOG
{ time_t sec = time(NULL);                      // get secs from 1 Jan 1970 UTC
  struct tm *buf;                               // struct for localtime()
  int day;                                      // number of days
  buf = localtime(&sec);                        // get GMT-localtime
#if !defined __CYGWIN__ && !defined __sun__
  sec = sec + buf->tm_gmtoff;                   // adjust to local
#endif
  day = sec/SECDAY+YRADJ;                       // get number of days
  sec = sec%SECDAY;                             // and number of seconds
  return sprintf( (char *)ret_buffer, "%d,%d", day, (int) sec); // return count and $H
}

//***********************************************************************
// $KEY
//
short Vkey(u_char *ret_buffer)                  // $KEY
{ SQ_Chan *ioptr;				// ptr to current $IO
  ioptr = &partab.jobtab->seqio[(int) partab.jobtab->io]; // point at it
  return mcopy( &ioptr->dkey[0],		// copy from here
                ret_buffer,                     // to here
		ioptr->dkey_len);		// this many bytes
}

//***********************************************************************
// $REFERENCE
//
short Vreference(u_char *ret_buffer)            // $REFERENCE
{ mvar *var;					// variable pointer
  var = &partab.jobtab->last_ref;		// point at $R
  ret_buffer[0] = '\0';				// null JIC
  if (var->name.var_cu[0] == '\0') return 0;	// return null string if null
  return UTIL_String_Mvar(var, ret_buffer, 32767); // do it elsewhere
}

//***********************************************************************
// $SYSTEM
//
short Vsystem(u_char *ret_buffer)               // $SYSTEM
{ int i;                                        // to copy value
  i = itocstring( ret_buffer, MUMPS_SYSTEM); 	// copy assigned #
  ret_buffer[i++] = ',';                        // and a comma
  i = i + mumps_version(&ret_buffer[i]);        // do it elsewhere
  return i;                                     // return the count
}

//***********************************************************************
// $X
//
short Vx(u_char *ret_buffer)                    // $X
{ SQ_Chan *ioptr;				// ptr to current $IO
  ioptr = &partab.jobtab->seqio[(int) partab.jobtab->io]; // point at it
  return itocstring( ret_buffer,
  		  ioptr->dx);			// return len with data in buf
}

//***********************************************************************
// $Y
//
short Vy(u_char *ret_buffer)                    // $Y
{ SQ_Chan *ioptr;				// ptr to current $IO
  ioptr = &partab.jobtab->seqio[(int) partab.jobtab->io]; // point at it
  return itocstring( ret_buffer,
  		  ioptr->dy);			// return len with data in buf
}

//***********************************************************************
// $ZHOROLOG
//
short Vzhorolog(u_char *ret_buffer)             // $ZHOROLOG
{ struct timeval tv;                            // struct for gettimeofday()
  time_t sec;                                   // seconds
  struct tm *buf;                               // struct for localtime()
  int day;                                      // number of days

  gettimeofday(&tv, NULL);
  sec = tv.tv_sec;
  buf = localtime(&sec);                        // get GMT-localtime
#if !defined __CYGWIN__ && !defined __sun__
  sec += buf->tm_gmtoff;                        // adjust to local
#endif
  day = sec/SECDAY+YRADJ;                       // get number of days
  sec = sec%SECDAY;                             // and number of seconds
  return sprintf( (char *)ret_buffer, "%d,%f", day, // return count and $ZH
                  (double) sec + tv.tv_usec / 1000000.0);
}

//***********************************************************************
// Set special variables - those that may be set are:
//	$EC[ODE]
//	$ET[RAP]
//	$K[EY]
//	$X
//	$Y

short Vset(mvar *var, cstring *cptr)		// set a special variable
{ int i;
  if (var->slen != 0) return -ERRM8;		// no subscripts permitted
  if ((strncasecmp( (char *)&var->name.var_cu[1], "ec", 2) == 0) ||
      (strncasecmp( (char *)&var->name.var_cu[1], "ecode", 5) == 0)) // $EC[ODE]
  { if ((cptr->len > 1) && (cptr->buf[0] == ',')    // If it starts with a comma
        && (cptr->buf[cptr->len - 1] == ','))        // and ends with a comma
    {  cptr->len--;
       bcopy(&cptr->buf[1], &cptr->buf[0], cptr->len--);  // Ignore the commas
       cptr->buf[cptr->len] = '\0';                     // and nul terminate
    }
    if ((cptr->len == 0) ||			// set to null ok
	(cptr->buf[0] == 'U'))			// or Uanything
    { X_set("$ECODE\0\0", &var->name.var_cu[0], 8); // ensure name correct
      partab.jobtab->error_frame = 0;		// and where the error happened
      partab.jobtab->etrap_at = 0;		// not required
      if (cptr->len == 0)			// if we are clearing it
        return ST_Kill(var);			// kill it
      return USRERR;				// do it elsewhere
    }
    return -ERRM101;				// can't do that
  }
  if ((strncasecmp( (char *)&var->name.var_cu[1], "et", 2) == 0) ||
      (strncasecmp( (char *)&var->name.var_cu[1], "etrap", 5) == 0)) // $ET[RAP]
  { X_set("$ETRAP\0\0", &var->name.var_cu[0], 8); // ensure name correct
    if (cptr->len == 0) return ST_Kill(var);	// kill it
    return ST_Set(var, cptr);			// do it in symbol
  }
  if ((strncasecmp( (char *)&var->name.var_cu[1], "k", 1) == 0) ||
      (strncasecmp( (char *)&var->name.var_cu[1], "key", 3) == 0)) // $K[EY]
  { if (cptr->len > MAX_DKEY_LEN) return -ERRM75; // too big
    bcopy(cptr->buf,				// copy this
	  partab.jobtab->seqio[partab.jobtab->io].dkey, // to here
	  cptr->len+1);				// this many (incl null)
    partab.jobtab->seqio[partab.jobtab->io].dkey_len = cptr->len;
    return 0;
  }
  if (strncasecmp( (char *)&var->name.var_cu[1], "x", 1) == 0)	// $X
  { i = cstringtoi(cptr);			// get val
    if (i < 0) i = 0;
    partab.jobtab->seqio[partab.jobtab->io].dx = (u_short) i;
    return 0;					// and return
  }
  if (strncasecmp( (char *)&var->name.var_cu[1], "y", 1) == 0)	// $Y
  { i = cstringtoi(cptr);			// get val
    if (i < 0) i = 0;
    partab.jobtab->seqio[partab.jobtab->io].dy = (u_short) i;
    return 0;					// and return
  }
  return -ERRM8;				// else junk
}
