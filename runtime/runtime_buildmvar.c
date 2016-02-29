// File: mumps/runtime/runtime_buildmvar.c
//
// module runtime - build an mvar

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
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"				// standard errors
#include "opcodes.h"				// the op codes
#include "compile.h"				// rbd structure

// This module is the runtime code to build an mvar.
// It is passed the addres of the mvar and reads from *mumpspc++.
// see comments in mumps/compile/localvar.c for more info.
// If nul_ok is true, a null subscript as the last is OK.
// Returns new asp or -err
//

short getvol(cstring *vol)			// get vol number for vol
{ int i;					// a handy int
  short s;					// for cstring length
  s = vol->len;					// get len
  if (s < MAX_NAME_BYTES) s++;			// include term null if poss
  for (i = 0; i < MAX_VOL; i++)			// scan the volumes
  { if (systab->vol[i]->vollab == NULL) continue; // continue if none in slot
    if (bcmp(&systab->vol[i]->vollab->volnam.var_cu[0],
	     vol->buf, s) != 0) continue;	// if not the same continue
    return (short) i + 1;			// return vol number
  }
  return (-ERRM26);				// complain - no such
}

short getuci(cstring *uci, int vol)		// get uci number
{ int i;					// for loops
  short s;					// for cstring length
  s = uci->len;					// get len
  if (s < MAX_NAME_BYTES) s++;			// include term null if poss
  if (vol == 0) vol = partab.jobtab->vol;	// get current vol
  vol--;					// make internal reference
  for (i = 0; i < UCIS; i++)			// scan the ucis
  { if (bcmp(&systab->vol[vol]->vollab->uci[i].name.var_cu[0],
	     uci->buf, s) == 0) return (short) i + 1;
  }
  return (-ERRM26);				// complain - no such
}


short buildmvar(mvar *var, int nul_ok, int asp) // build an mvar
{ u_char type;					// variable type
  int subs;					// subscript count
  int i;					// a handy int
  cstring *ptr;					// and a handy pointer
  short s;					// for returns
  //chr_q *vt;					// var table pointer
  chr_x *vt;					// var table pointer
  rbd *p;					// a handy pointer
  mvar *ind;					// ind mvar ptr

  type = *mumpspc++;				// get the type
  if (type < TYPVARNAKED)			// subs in type
  { subs = (type & TYPMAXSUB);			// the low bits
    type = (type & ~TYPMAXSUB);			// and the type
  }
  else
    subs = *mumpspc++;				// get in line
  var->volset = 0;				// default vol set
  var->uci = (type < TYPVARGBL) ?
    UCI_IS_LOCALVAR : 0;			// assume local var or uci 0
  var->slen = 0;				// and no subscripts
  if (type == TYPVARNAKED)			// if it's a naked
  //{ if ( partab.jobtab->last_ref.name.var_qu == 0)
  { if ( X_Empty(partab.jobtab->last_ref.name.var_xu) )
      return (-ERRM1); 				// say "Naked indicator undef"
    i = UTIL_Key_Last( &partab.jobtab->last_ref); // start of last key
    if (i < 0) return (-ERRM1); 		// say "Naked indicator undef"
    bcopy( &(partab.jobtab->last_ref), var, 
            sizeof(var_u) + 5 + i);   		// copy naked naked
    var->slen = (u_char) i;			// stuf in the count
  }
  else if (type == TYPVARIND)			// it's an indirect
  { ind = (mvar *) astk[asp-subs-1];		// point at mvar so far
    bcopy(ind, var, ind->slen + sizeof(var_u) + 5); // copy it in
  }
  else if ((type & TYPVARIDX) &&		// if it's the index type
	   (type < TYPVARGBL))			// and it's local
  { i = *mumpspc++;				// get the index
    if (i < 255)				// can't do the last one
    { var->volset = i + 1;			// save the index (+ 1)
      //var->name.var_qu = 0;			// clear the name
      X_Clear(var->name.var_xu);		// clear the name
    }
    else
    { p = (rbd *) (partab.jobtab->dostk[partab.jobtab->cur_do].routine);
      //vt = (chr_q *) (((u_char *) p) + p->var_tbl); // point at var table
      //var->name.var_qu = vt[i];		// get the var name
      vt = (chr_x *) (((u_char *) p) + p->var_tbl); // point at var table
      var->name.var_xu = vt[i];			// get the var name
    }
  }
  else
  { bcopy( mumpspc, &var->name, sizeof(var_u));	// get the variable name
    mumpspc += sizeof(var_u);
  }

  for (i = 0; i < subs; i++)			// for each subscript
  { ptr = (cstring *) astk[asp-subs+i];		// point at the string
    if ((ptr->len == 0)	&&			// if it's a null
        ((!nul_ok) || (i != (subs-1))))		// not ok or not last subs
      return (-(ERRZ16+ERRMLAST));		// complain
    s = UTIL_Key_Build(ptr,
		       &var->key[var->slen]); 	// get one subscript
    if ((s + var->slen) > 255)			// check how big
      return (-(ERRZ2+ERRMLAST));		// complain on error
    var->slen = s + var->slen; 			// add it in
  }

  if (type == TYPVARGBLUCIENV)			// need vol?
  { ptr = (cstring *) astk[asp-subs-1];		// point at the string
    s = getvol(ptr);				// get volume
    if (s < 0) return s;			// die on error
    var->volset = (u_char) s;			// save the value
  }
  if ((type == TYPVARGBLUCI) ||
      (type == TYPVARGBLUCIENV))		// need uci?
  { ptr = (cstring *)
      astk[asp-subs-1-(type == TYPVARGBLUCIENV)]; // point at the string
    s = getuci(ptr, var->volset);		// get uci
    if (s < 0) return s;			// die on error
    var->uci = (u_char) s;			// save the value
  }
  if (type == TYPVARIND) asp--;			// fixup asp for return
  return asp - subs
    - (type == TYPVARGBLUCI)
    - ((type == TYPVARGBLUCIENV) * 2);		// all done
}
