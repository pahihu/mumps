// File: mumps/compile/localvar.c
//
// module compile - parse a local variable

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
#include <ctype.h>
#include <errno.h>                              // error stuf
#include <limits.h>                     	// for LONG_MAX etc
#include <math.h>
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"                              // and the error defs
#include "opcodes.h"                            // and the opcodes
#include "compile.h"				// compile stuff

// function localvar entered with source_ptr pointing at the source
// variable to evaluate and comp_ptr pointing at where to put the code.
//
// Return       Means
// -ERR         Nothing compiled, error returned
// off		Offset from starting point of comp_ptr for OPVAR
//
// Following the OPVAR is a byte indicating type of
// variable as per the following:
//	TYPMAXSUB       63                      // max subscripts
//	TYPVARNAM       0                       // name only
//	                                        //   (MAX_NAME_BYTES bytes)
//	TYPVARLOCMAX    TYPVARNAM+TYPMAXSUB     // local is 1->63 subs
//	TYPVARIDX       64                      // 1 byte index (+ #subs)
//	TYPVARGBL       128                     // first global
//	TYPVARGBLMAX    TYPVARGBL+TYPMAXSUB     // global 128->191 subs
//	TYPVARNAKED	252			// global naked reference
//	TYPVARGBLUCI	253			// global with uci
//	TYPVARGBLUCIENV	254			// global with uci and env
//	TYPVARIND	255			// indirect
//
//  For TYPVARNAM: 	OPVAR TYPVARNAM (var_u) name
//	TYPVARLOC: 	subscripts OPVAR TYPVARNAM+#subs (var_u) name
//	TYPVARIDX:	subscripts OPVAR TYPVARIDX+#subs (u_char) idx
//	TYPVARGBL:	subscripts OPVAR TYPVARGBL+#subs (var_u) name
//	TYPVARNAKED:	subscripts OPVAR TYPVARNAKED #subs
//	TYPVARGBLUCI:	subscripts uci OPVAR TYPVARGBLUCI #subs (var_u) name
//	TYPVARGBLUCIENV: subs uci env OPVAR TYPVARGBLUCIENV #subs (var_u) name
//	TYPVARIND:	(str on astk[]) [subs] OPVAR TYPEVARIND #subs

u_char *localvar_op;

short localvar()                                // evaluate local variable
{ char c;                                       // current character
  u_short idx = 0;				// index
  int i;                                        // a usefull int
  int count = 0;				// count subscripts
  var_u var;                                    // to hold variable names
  u_char *ptr;					// to save comp_ptr
  short type = TYPVARNAM;			// the type code
  short ret;					// the return
  int isbp;                                     // flag for $BP

  // fprintf(stderr, "localvar(): [%s]\r\n", source_ptr);
  localvar_op = 0;
  ptr = comp_ptr;				// save comp_ptr
  c = *source_ptr++;                            // get a character
  if (c == '^')					// if it's global
  { type = TYPVARGBL;				// say it's global
    c = *source_ptr++;				// point past it
    if (c == '[')				// uci/env specified
    { type = TYPVARGBLUCI;			// we have a uci specified
      atom();					// eval the argument
      c = *source_ptr++; 			// get next
      if (c == ',')				// if it's a comma
      { type = TYPVARGBLUCIENV;			// say we have vol as well
	atom();					// eval the argument
	c = *source_ptr++; 			// get next
      }
      if (c != ']')
      { // fprintf(stderr,"pos1\r\n");
        return (-(ERRZ12+ERRMLAST));             // that's junk
      }
      c = *source_ptr++; 			// get next
    }
    else if (c == '(')				// naked reference ?
    { type = TYPVARNAKED;			// set type
      source_ptr--;				// back up source
      goto subs;
    }
  }
  else if (c == '@')				// indirect ?
  { type = TYPVARIND;				// yes @...@ ... on astk[]
    if (*source_ptr == '(') goto subs;		// go do subscripts
    // fprintf(stderr,"pos2\r\n");
    return (-(ERRZ12+ERRMLAST));		// else it's junk
  }
  isbp = (c == '$') && (0 == strncasecmp((char *)source_ptr,"BP",2));
  if ((isalpha((int)c) == 0) && (c != '%') && (c != '$'))// check for a variable
  { // fprintf(stderr,"pos3\r\n");
    return (-(ERRZ12+ERRMLAST));                // return the error
  }
  if ((c == '$') && (type == TYPVARNAM) && !isbp)  // check $...
  { if (isalpha(*source_ptr) == 0)		// next must be alpha
    { // fprintf(stderr,"pos4\r\n");
      return (-(ERRZ12+ERRMLAST));              // return the error
    }
    i = toupper(*source_ptr);			// get the next character
    if (strchr("DEHIJKPQRSTXY", i) == NULL)	// if letter is invalid
    { return -ERRM8;				// complain
    }
  }
  //var.var_xu = 0;                             // clear the variable name
  X_Clear(var.var_xu);
  var.var_cu[0] = c;                            // save first char
  for (i = 1; i<MAX_NAME_BYTES; i++)            // scan for rest of name
  { c = *source_ptr++;                          // get next char
    if (isalnum((int)c) == 0)                   // if not alpha numeric
    { --source_ptr;                             // point back at it
      break;                                    // and exit
    }
    var.var_cu[i] = c;                          // save in the variable
  }
  while (isalnum(*source_ptr) !=0) source_ptr++; // skip extended name
subs:
  if (*source_ptr == '(')                       // see if it's subscripted
  { source_ptr++;				// skip the bracket
    while (TRUE)				// loop
    { eval();					// get a subscript
      count++;					// count it
      // fprintf(stderr,"subs: [%s]\r\n",source_ptr);
      c = *source_ptr++;			// get next character
      if (c == ')') break;			// quit when done
      if (c != ',') 
      { // fprintf(stderr,"pos5: '%d'\r\n",(int)c);
        return (-(ERRZ12+ERRMLAST));             // return the error
      }
    }
  }
  if (count > TYPMAXSUB)			// too many
  { return -(ERRZ15+ERRMLAST);			// error
  }
  localvar_op = comp_ptr;
  ret = comp_ptr - ptr;				// remember here
  *comp_ptr++ = OPVAR;				// opcode
  if ((type < TYPVARGBL) &&			// candidate for index?
      (partab.varlst != NULL) &&		// and in a routine compile
      (var.var_cu[0] != '$'))			// and it's not $...
  { for (i = 0; ; i++)				// scan list
    { if (i == MAX_VAR_TBL) break;		// too many
      //if (partab.varlst[i].var_qu == var.var_qu)
      if (X_EQ(partab.varlst[i].var_xu, var.var_xu))
        break;					// found it
      // if (partab.varlst[i].var_qu == 0)
      if (X_Empty(partab.varlst[i].var_xu))
      { 
        //partab.varlst[i].var_qu = var.var_qu;	// set it
        partab.varlst[i].var_xu = var.var_xu;	// set it
	break;
      }
    }
    if (i == MAX_VAR_TBL)                       // too many?
      return -(ERRZ53+ERRMLAST);                // error exit
    type |= TYPVARIDX;			        // change the type
    idx = i;					// save index
  }
  if (type < TYPVARNAKED)			// normal local or global var
  { type = type + count;			// add the count
    *comp_ptr++ = (u_char) type;		// store it
    if (type & TYPVARIDX)			// index type?
    { bcopy(&idx, comp_ptr, sizeof(u_short));   // save the index
      comp_ptr += sizeof(u_short);
      // *comp_ptr++ = idx;			// save the index
    }
  }
  else						// funnee type
  { *comp_ptr++ = (u_char) type;		// store the type
    *comp_ptr++ = count;			// then the subscripts
  }
  if (((type < TYPVARIDX) ||			// if simple local (not idx)
       (type >= TYPVARGBL)) &&			// a 'normal' global
      (type != TYPVARNAKED) &&			// and not naked
      (type != TYPVARIND))			// or indirect
  {
    comp_ptr += X_put(&var.var_xu, comp_ptr);   // XXX
    // for (i = 0; i<MAX_NAME_BYTES; i++)          // scan the name
    //   *comp_ptr++ = var.var_cu[i];              // copy into compiled code
  }
  return ret;					// say what we did
}                                               // end variable parse

