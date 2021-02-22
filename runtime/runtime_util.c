// File: mumps/runtime/runtime_util.c

// module runtime - RunTime Utilities

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
#include <sys/utsname.h>                        // defines struct utsname
#include <limits.h>
#include <math.h>
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard includes
#include "error.h"                              // standard includes

int cstringtoi(cstring *str)                    // convert cstring to int
{ int ret = 0;                                  // return value
  int i;                                        // for loops
  int minus = FALSE;                            // sign check

  for (i=0; (i < (int)str->len)&&
            ((str->buf[i] == '-') ||
             (str->buf[i] == '+')); i++)        // check leading characters
    if (str->buf[i] == '-') minus = !minus;     // count minus signs
  for (i = i; i < (int)str->len; i++)           // for each character
  { if ((str->buf[i] > '9')||(str->buf[i] < '0')) break; // check for digit
    if (ret > 214748363) return INT_MAX;	// check for possible overflow
    ret = (ret * 10) + ((int)str->buf[i] - 48); // Cvt to int
  }                                             // end convert loop
  if ((systab->historic & HISTORIC_EOK)
     && (i < (str->len - 1)) && (str->buf[i] == 'E'))
  { int exp = 0;				// an exponent
    int expsgn = 1;				// and the sign
    int j = 10;					// for E calc

    i++;					// point past the 'E'
    if (str->buf[i] == '-')			// if a minus
    { expsgn = -1;				// flag it
      i++;					// and increment i
    }
    else if (str->buf[i] == '+')		// if a plus
    { i++;					// just increment i
    }
    for (i = i; i < str->len; i++)		// scan remainder
    { if ((str->buf[i] < '0') || (str->buf[i] > '9'))
      { break;					// quit when done
      }
      exp = (exp * 10) + (str->buf[i] - '0');	// add to exponent
    }
    if (exp)					// if there was an exponent
    { while (exp > 1)				// for each
      { j = j * 10;				// multiply
        exp--;					// and count it
      }
      if (expsgn > 0)				// if positive
      { ret = ret * j;				// hope it fits
      }
      else					// if negative
      { ret = ret / j;				// do this
      }
    }
  }
  if (minus) ret = -ret;                        // change sign if reqd
  return ret;                                   // return the value
}                                               // end cstringtoi()

double cstringtod(cstring *str)                 // convert cstring to double
{ double ret = 0.0;                             // return value
  int i;                                        // for loops
  int minus = FALSE;                            // sign check
  double frac = 0.0;                            // fraction
  double scale = 1.0;                           // no. of fraction digits

  for (i=0; (i < (int)str->len)&&
            ((str->buf[i] == '-') ||
             (str->buf[i] == '+')); i++)        // check leading characters
    if (str->buf[i] == '-') minus = !minus;     // count minus signs
  for (i = i; i < (int)str->len; i++)           // for each character
  { if ((str->buf[i] > '9')||(str->buf[i] < '0')) break; // check for digit
    ret = (ret * 10.0) + ((int)str->buf[i] - '0'); // Cvt to int
  }                                             // end convert loop
  if ((i < str->len - 1) &&                     // check for dot, and a digit
      (str->buf[i] == '.') &&
      (isdigit(str->buf[i+1])))
  { i++;
    for (i = i; i < (int)str->len; i++)         // for each character
    { if (!isdigit(str->buf[i])) break;         // check for digit
      frac = (frac * 10.0) + ((int)str->buf[i] - '0'); // Cvt to int
      scale *= 10.0;                            // adjust scale
    }
    ret += frac / scale;
  }
  if ((systab->historic & HISTORIC_EOK)
     && (i < (str->len - 1)) && (str->buf[i] == 'E'))
  { int exp = 0;				// an exponent
    int expsgn = 1;				// and the sign
    int j = 10;					// for E calc

    i++;					// point past the 'E'
    if (str->buf[i] == '-')			// if a minus
    { expsgn = -1;				// flag it
      i++;					// and increment i
    }
    else if (str->buf[i] == '+')		// if a plus
    { i++;					// just increment i
    }
    for (i = i; i < str->len; i++)		// scan remainder
    { if ((str->buf[i] < '0') || (str->buf[i] > '9'))
      { break;					// quit when done
      }
      exp = (exp * 10) + (str->buf[i] - '0');	// add to exponent
    }
    if (exp)					// if there was an exponent
    { while (exp > 1)				// for each
      { j = j * 10;				// multiply
        exp--;					// and count it
      }
      if (expsgn > 0)				// if positive
      { ret = ret * j;				// hope it fits
      }
      else					// if negative
      { ret = ret / j;				// do this
      }
    }
  }
  if (minus) ret = -ret;                        // change sign if reqd
  return ret;                                   // return the value
}                                               // end cstringtod()

int cstringtob(cstring *str)                    // convert cstring to boolean
{ int ret = 0;                                  // return value
  int i;                                        // for loops
  int dp = 0;					// decimal place flag
  for (i=0; (i < (int)str->len)&&
            ((str->buf[i] == '-') ||
            (str->buf[i] == '+')); i++);        // check leading characters
  for (i = i; i < (int)str->len; i++)           // for each character
  { if (str->buf[i] == '0') continue;		// ignore zeroes
    if (str->buf[i] == '.')			// check for a dot
    { if (dp) break;				// quit if not the first
      dp = 1;					// flag it
      continue;					// go for next
    }
    if ((str->buf[i] >= '1') &&
        (str->buf[i] <= '9'))			// check for digit
      ret = 1;					// got a true value
    break;
  }                                             // end convert loop
  return ret;                                   // return the value
}                                               // end cstringtob()

short mv1_itocstring(u_char *buf, int n)	// convert int to string
{ int i = 0;					// array index
  int p = 0;					// string index
  int a[12];					// array for digits
  a[0] = 0;					// ensure first is zero
  if (n < 0)					// if negative
  { buf[p++] = '-';				// store the sign
    n = -n;					// negate the number
  }
  while (n)					// while there is a value
  { a[i++] = n % 10;				// get low decimal digit
    n = n/10;					// reduce number
  }
  while (i) buf[p++] = a[--i] + 48;		// copy digits backwards
  if (!p) buf[p++] = '0';			// ensure we have something
  buf[p] = '\0';				// null terminate
  return (short) p;				// and exit
}

short mv1_uitocstring(u_char *buf, u_int n)	// convert u_int to string
{ int i = 0;					// array index
  int p = 0;					// string index
  int a[12];					// array for digits
  a[0] = 0;					// ensure first is zero
  while (n)					// while there is a value
  { a[i++] = n % 10;				// get low decimal digit
    n = n/10;					// reduce number
  }
  while (i) buf[p++] = a[--i] + 48;		// copy digits backwards
  if (!p) buf[p++] = '0';			// ensure we have something
  buf[p] = '\0';				// null terminate
  return (short) p;				// and exit
}


// Set data into $ECODE
// Returns 0 if no previous error at this level
short Set_Error(int err, cstring *user, cstring *space)
{ short t;					// for function calls
  int j;					// a handy int
  int flag;					// to remember
  mvar *var;					// a handy mvar
  cstring *tmp;					// spare cstring ptr
  char temp[3*MAX_NAME_BYTES];			// and some space

  var = &partab.src_var;			// a spare mvar
  var->slen = 0;				// no subs
  // note - the uci and volset were setup by the caller

  X_set("$ECODE\0\0", &var->name.var_cu[0], 8); // get the name
  t = ST_Get(var, space->buf);			// get it
  if (t < 0) t = 0;				// ignore undefined
  flag = t;					// remember if some there
  if (t < MAX_ECODE)				// if not too big
  { if ((t == 0) || (space->buf[t-1] != ','))
      space->buf[t++] = ',';			// for new $EC
    j = -err;					// copy the error (-ve)
    if (err == USRERR)				// was it a SET $EC
    { bcopy(user->buf, &space->buf[t], user->len); // copy the error
      t += user->len;				// add the length
    }
    else					// not user error
    { if (j > ERRMLAST)				// implementation error?
      { space->buf[t++] = 'Z';			// yes, Z type
        j -= ERRMLAST;				// subtract it
      }
      else
        space->buf[t++] = 'M';			// MDC error
      t += itocstring(&space->buf[t], j); 	// convert the number
    }						// end 'not user error'
    space->buf[t++] = ',';			// trailing comma
    space->buf[t] = '\0';			// null terminate
    space->len = t;
    t = ST_Set(var, space);			// set it
    tmp = (cstring *) temp;			// temp space
    tmp->len = itocstring(tmp->buf, partab.jobtab->cur_do);
    var->slen = UTIL_Key_Build(tmp, var->key);
    if (flag)					// if not first one
    { t = ST_Get(var, space->buf);		// get it
      if (t < 0) t = 0;				// ignore undefined
      flag = t;					// remember for the return
      if ((t == 0) || (space->buf[t-1] != ','))
        space->buf[t++] = ',';			// for new $EC
      j = -err;					// copy the error (-ve)
      if (err == USRERR)			// was it a SET $EC
      { bcopy(user->buf, &space->buf[t], user->len); // copy the error
        t += user->len;				// add the length
      }
      else					// not user error
      { if (j > ERRMLAST)			// implementation error?
        { space->buf[t++] = 'Z';		// yes, Z type
          j -= ERRMLAST;			// subtract it
        }
        else
        space->buf[t++] = 'M';			// MDC error
        t += itocstring(&space->buf[t], j); 	// convert the number
      }
      space->buf[t++] = ',';			// trailing comma
      space->buf[t] = '\0';			// null terminate
      space->len = t;
    }
    t = ST_Set(var, space);			// set it
  }						// end "TOO BIG" test
  return (short) flag;				// done
}

int mumps_version(u_char *ret_buffer)           // return version string
{ int i;                                        // to copy value
  int j = 0;                                    // for returned strings
  struct utsname uts;                           // struct for uname
  i = uname(&uts);                              // get system info
  if (i == -1) return (-1);                      // exit on error
  bcopy("MUMPS V", ret_buffer, 7);		// copy in MUMPS V
  i = 7;					// point past it
  if (VERSION_TEST)				// if an internal version
  { i += sprintf((char *)&ret_buffer[i], "%d.%02d T%d for ",
		 VERSION_MAJOR, VERSION_MINOR, VERSION_TEST);
  }
  else						// else normal release
  { i += sprintf((char *)&ret_buffer[i], "%d.%02d for ",
		 VERSION_MAJOR, VERSION_MINOR);
  }
  j = 0;                                        // clear src ptr
  while ((ret_buffer[i++] = uts.sysname[j++])); // copy name
  ret_buffer[i-1] = ' ';                        // and a space over the null
  j = 0;                                        // clear src ptr
  while ((ret_buffer[i++] = uts.machine[j++])); // copy hardware
  ret_buffer[i-1] = ' ';                        // and a space over the null
  i += sprintf((char *)&ret_buffer[i], "Built %s at %s.", __DATE__ , __TIME__ );
  return i-1;                                   // and return count
}
