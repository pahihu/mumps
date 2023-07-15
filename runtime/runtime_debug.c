// File: mumps/runtime/runtime_debug.c
//
// module runtime - debug

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
#include "compile.h"				// for XECUTE

mvar dvar;					// an mvar for debugging
u_char src[1024];				// some space for entered
u_char cmp[1024];				// ditto compiled
u_char *debug;					// a pointer to compiled code

void Debug_off()				// turn off debugging
{ 
  X_set("$BP\0\0\0\0\0", &dvar.name.var_cu[0], 8);
  dvar.volset = 0;				// clear volume
  dvar.uci = UCI_IS_LOCALVAR;			// local variable
  dvar.slen = 0;				// no subscripts
  (void)ST_Kill(&dvar);				// dong it
  partab.debug = 0;				// turn off flag
  return;					// done
}

// Turn on (or modify) debug stuff
// We are passed one cstring containing:
//	Debug_ref		Remove breakpoint
//	Debug_ref:		Add simple breakpoint
//	Debug_ref:code		Add breakpoint with code to Xecute
//	:code			Code to execute at QUIT n breakpoint
//0

short Debug_on(cstring *param)			// turn on/modify debug
{ int i = 0;					// a handy int
  int j = 0;					// and another
  short s;
  int off = 1;					// line offset
  cstring *ptr, *cptr;				// string pointers
  u_char temp[128];                             // temp buffer

  debug = NULL;					// clear auto debug
  X_set("$BP\0\0\0\0\0", &dvar.name.var_cu[0], 8);
  dvar.volset = 0;				// clear volume
  dvar.uci = UCI_IS_LOCALVAR;			// local variable
  dvar.slen = 0;				// assume no key - aka :code
  dvar.nsubs = 255;                             // clear nsubs
  // dvar.key[0] = 128;				// setup for string key

  cptr = (cstring *) temp;                      // point to temp buffer
  cptr->len = 0;                                // zero length

  if (param->buf[0] != ':')			// If not a : first
  { if (param->buf[i] == '+')			// offset?
    { i++;					// point past it
      off = 0;					// clear offset
      while (isdigit(param->buf[i]))		// for the digits
        off = (off * 10) + (param->buf[i++] - '0'); // convert
    }
    if (param->buf[i++] != '^')
      return -(ERRZ9+ERRMLAST);			// we don't like it
    for (j = 0; j < MAX_NAME_BYTES; j++)	// copy the routine name
    { if ((isalnum(param->buf[j + i]) == 0) &&
	  ((param->buf[j + i] != '%') || (j != 0)))
        break;					// done
      // dvar.key[j + 1] = param->buf[j + i];	// copy it
      cptr->buf[j] = param->buf[j + i];         // copy it
    }
    // dvar.key[j + 1] = '\0';			// null terminate it
    // dvar.slen = j + 2;			// save the length
    cptr->len = j;                              // save length
    cptr->buf[j] = '\0';                        // null terminate
    s = UTIL_Key_BuildEx(&dvar, cptr, &dvar.key[0]); // build subscript
    // fprintf(stderr,"--- DON subs1=%d\r\n",s);
    if (s < 0) return s;                        // complain
    dvar.slen = s;                              // set dvar len
    j = j + i;					// point to next char
    while (isalnum(param->buf[j])) j++;		// skip long names
    if ((param->buf[j] != ':') &&		// not :
        (param->buf[j] != '\0'))		// and not eol
	  return -(ERRZ9+ERRMLAST);		// we don't like it
    // dvar.key[dvar.slen++] = 64;		// new key
    // s = itocstring(&dvar.key[dvar.slen], off);// copy offset
    // dvar.key[dvar.slen - 1] |= s;		// fixup type byte
    // dvar.slen += s;				// and count
    // dvar.slen++;				// count the null
    cptr->len = 0;
    // fprintf(stderr,"--- DON off=%d\r\n",off);
    s = itocstring(&cptr->buf[0], off);         // setup for subscript
    // fprintf(stderr,"--- DON itocs=%d\r\n",s);
    if (s < 0) return s;                        // complain
    cptr->len = s;
    // fprintf(stderr,"--- DON cstr=%s\r\n",&cptr->buf[0]);
    s = UTIL_Key_BuildEx(&dvar, cptr, &dvar.key[dvar.slen]); // build subscript
    // fprintf(stderr,"--- DON subs2=%d\r\n",s);
    if (s < 0) return s;                        // complain
    dvar.slen += s;				// and count
  }						// end of +off^rou code

  if (param->buf[j++] == '\0')			// end of string?
  {
    // fprintf(stderr,"--- DON kill\r\n");
    return ST_Kill(&dvar);			// dong and exit
  }
  partab.debug = -1;				// turn on debug
  if (param->buf[j] == '\0')			// end of string?
  { ptr = (cstring *) src;			// make a cstring
    ptr->len = 0;				// length
    ptr->buf[0] = '\0';				// null terminated
    // fprintf(stderr,"--- DON emptystr\r\n");
    return ST_Set(&dvar, ptr);			// set and return
  }
  if ((param->len - j) > 255)
    return -(ERRZ9+ERRMLAST);			// too bloody long
  source_ptr = &param->buf[j];			// point at the source
  ptr = (cstring *) cmp;			// where it goes
  comp_ptr = ptr->buf;				// for parse
  parse();					// compile it
  *comp_ptr++ = ENDLIN;				// eol
  *comp_ptr++ = ENDLIN;				// eor
  ptr->len = (comp_ptr - ptr->buf);		// save the length
  // fprintf(stderr,"--- DON compiled=%d\r\n",dvar.slen);
  return ST_Set(&dvar, ptr);			// set and return
}

// dot = -1 for the return from a QUIT n
//	  0 to check to see if we need to break
//	  1 from a BREAK sp sp
//

short Debug(long savasp, long savssp, int dot)	// drop into debug
{ int i;					// a handy int
  int io;					// save current $IO
  short s = 0;					// for calls
  do_frame *curframe;				// a do frame pointer
  cstring *ptr, *cptr;				// a string pointers
  mvar *var;					// and an mvar ptr
  u_char temp[128];                             // temp buffer

  if (!partab.debug) partab.debug = -1;		// ensure it's on


  X_set("$BP\0\0\0\0\0", &dvar.name.var_cu[0], 8);
  dvar.volset = 0;				// clear volume
  dvar.uci = UCI_IS_LOCALVAR;			// local variable
  dvar.slen = 0;				// no key
  dvar.nsubs = 255;                             // clear nsubs

  curframe = &partab.jobtab->dostk[partab.jobtab->cur_do]; // point at it

  cptr = (cstring *) temp;                      // point to buffer
  cptr->len = 0;

  if (dot == 0)					// a check type, setup mvar
  { if ((curframe->type != TYPE_DO) &&
        (curframe->type != TYPE_EXTRINSIC))
      return 0;					// ensure we have a routine
    // dvar.key[0] = 128;			// setup for string key
    for (i = 0; i < MAX_NAME_BYTES; i++)
      // if (!(dvar.key[i + 1] = curframe->rounam.var_cu[i]))
      if (!(cptr->buf[i] = curframe->rounam.var_cu[i]))
        break;
    // dvar.slen = i + 1;			// the length so far
    cptr->len = i;                              // set length
    cptr->buf[i] = '\0';                        // null terminate
    s = UTIL_Key_BuildEx(&dvar, cptr, &dvar.key[0]); // build subscript
    // fprintf(stderr,"--- DBG subs1=%d\r\n",s);
    if (s < 0) return s;                        // complain
    dvar.slen = s;                              // save length
    // dvar.key[dvar.slen++] = '\0';		// null terminate it
    // dvar.key[dvar.slen++] = 64;		// next key
    // s = itocstring(&dvar.key[dvar.slen],
    //			   curframe->line_num);	// setup second key
    // dvar.key[dvar.slen - 1] |= s;		// fix type
    // dvar.slen += s;				// and length
    // dvar.slen++;				// count the null
    cptr->len = 0;                              // clear temp
    // fprintf(stderr,"--- DBG line_num=%d\r\n",curframe->line_num);
    s = itocstring(&cptr->buf[0],
			   curframe->line_num);	// setup second key
    // fprintf(stderr,"--- DBG itocs=%d\r\n",s);
    if (s < 0) return s;                        // complain
    cptr->len = s;
    // fprintf(stderr,"--- DBG cstr=%s\r\n",&cptr->buf[0]);
    s = UTIL_Key_BuildEx(&dvar, cptr, &dvar.key[dvar.slen]); // build subscript
    // fprintf(stderr,"--- DBG subs2=%d\r\n",s);
    if (s < 0) return s;                        // complain
    dvar.slen += s;                             // update length
    // fprintf(stderr,"--- DBG slens=%d\r\n",s);
    s = ST_Get(&dvar, &cmp[sizeof(short)]);	// get whatever
    // fprintf(stderr,"--- DBG itocs=%d\r\n",s);
    if (s < 0) return 0;			// just return if nothing
    (*(short *) cmp) = s;			// save the length
  }

  if (dot == -1)				// from a QUIT n
  { s = ST_Get(&dvar, &cmp[sizeof(short)]);	// get whatever
    if (s < 0) s = 0;				// ignore errors
    (*(short *) cmp) = s;			// save the length
  }
  if (partab.jobtab->cur_do >= MAX_DO_FRAMES)
    return (-(ERRMLAST+ERRZ8));			// too many (perhaps ??????)
  partab.jobtab->dostk[partab.jobtab->cur_do].pc = mumpspc; // save current
  if (s > 0)					// code to execute
  { partab.jobtab->cur_do++;			// increment do frame
    mumpspc = &cmp[sizeof(short)];		// where it is
    src[0] = '\0';				// a spare null
    partab.jobtab->dostk[partab.jobtab->cur_do].routine = src;
    partab.jobtab->dostk[partab.jobtab->cur_do].pc = mumpspc;
    partab.jobtab->dostk[partab.jobtab->cur_do].symbol = NULL;
    partab.jobtab->dostk[partab.jobtab->cur_do].newtab = NULL;
    partab.jobtab->dostk[partab.jobtab->cur_do].endlin =
      &cmp[(*(short *) cmp) - 3 + sizeof(short)];
    //partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_qu = 0;
    X_Clear(partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_xu);
    partab.jobtab->dostk[partab.jobtab->cur_do].vol = partab.jobtab->vol;
    partab.jobtab->dostk[partab.jobtab->cur_do].uci = partab.jobtab->uci;
    partab.jobtab->dostk[partab.jobtab->cur_do].line_num = 0;
    partab.jobtab->dostk[partab.jobtab->cur_do].type = TYPE_RUN;
    partab.jobtab->dostk[partab.jobtab->cur_do].level = 0;
    partab.jobtab->dostk[partab.jobtab->cur_do].flags = 0;
    partab.jobtab->dostk[partab.jobtab->cur_do].isp = isp;
    partab.jobtab->dostk[partab.jobtab->cur_do].asp = savasp;
    partab.jobtab->dostk[partab.jobtab->cur_do].ssp = savssp;
    partab.jobtab->dostk[partab.jobtab->cur_do].savasp = savasp;
    partab.jobtab->dostk[partab.jobtab->cur_do].savssp = savssp;
    s = run(savasp, savssp);			// do it
    if (s == OPHALT) return s;			// just halt if reqd
    --partab.jobtab->cur_do;			// restore do frame
    mumpspc = partab.jobtab->dostk[partab.jobtab->cur_do].pc; // restore pc
    if (s & BREAK_QN)
    { s = s & ~BREAK_QN;			// clear the bit
      if (s > 0)
      { partab.debug = s + partab.jobtab->commands; // when to stop
        partab.jobtab->attention = 1;		// say to check this thing
        s = 0;					// don't confuse the return
      }
    }
    return s;					// return whatever
  }
  io = partab.jobtab->io;			// save current $IO
  debug = NULL;					// clear code ptr
  partab.jobtab->io = 0;			// ensure 0
  partab.jobtab->seqio[0].options |= 8;		// ensure echo on
  ptr = (cstring *) src;			// some space
  while (TRUE)					// see what they want
  { if (partab.jobtab->seqio[0].dx)		// need a cr/lf
      s = SQ_WriteFormat(SQ_LF);		// doit
    //if (curframe->rounam.var_qu == 0)
    if (X_Empty(curframe->rounam.var_xu))
    { bcopy("debug", ptr->buf, 5);
      ptr->len = 5;
    }
    else
    { ptr->len = 0;				// clear ptr
      ptr->buf[ptr->len++] = '+';		// lead off
      ptr->len = itocstring(&ptr->buf[ptr->len],
			   curframe->line_num) + ptr->len; // setup line number

      ptr->buf[ptr->len++] = '^';		// lead off routine
      for (i = 0; i < MAX_NAME_BYTES; i++)
        if (!(ptr->buf[i + ptr->len] = curframe->rounam.var_cu[i]))
          break;				// copy rou name
      ptr->len += i;				// save length
    }
    ptr->buf[ptr->len++] = '>';			// and that bit
    ptr->buf[ptr->len++] = ' ';			// and that bit
    ptr->buf[ptr->len] = '\0';			// null terminate
    s = SQ_Write(ptr);				// write it
    s = SQ_Read(ptr->buf, -1, 256);		// read something
    if (s < 1) continue;			// ignore nulls and errors
    s = SQ_WriteFormat(SQ_LF);			// return
    source_ptr = ptr->buf;			// point at source
    comp_ptr = cmp;				// and where it goes
    parse();					// compile it
    *comp_ptr++ = ENDLIN;			// JIC
    *comp_ptr++ = ENDLIN;			// JIC

    partab.jobtab->cur_do++;			// increment do frame
    mumpspc = cmp;				// where it is
    src[0] = '\0';				// a spare null
    partab.jobtab->dostk[partab.jobtab->cur_do].routine = src;
    partab.jobtab->dostk[partab.jobtab->cur_do].pc = mumpspc;
    partab.jobtab->dostk[partab.jobtab->cur_do].symbol = NULL;
    partab.jobtab->dostk[partab.jobtab->cur_do].newtab = NULL;
    partab.jobtab->dostk[partab.jobtab->cur_do].endlin = comp_ptr - 3;
    //partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_qu = 0;
    X_Clear(partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_xu);
    partab.jobtab->dostk[partab.jobtab->cur_do].vol = partab.jobtab->vol;
    partab.jobtab->dostk[partab.jobtab->cur_do].uci = partab.jobtab->uci;
    partab.jobtab->dostk[partab.jobtab->cur_do].line_num = 0;
    partab.jobtab->dostk[partab.jobtab->cur_do].type = TYPE_RUN;
    partab.jobtab->dostk[partab.jobtab->cur_do].level = 0;
    partab.jobtab->dostk[partab.jobtab->cur_do].flags = 0;
    partab.jobtab->dostk[partab.jobtab->cur_do].savasp = savasp;
    partab.jobtab->dostk[partab.jobtab->cur_do].savssp = savssp;
    partab.jobtab->dostk[partab.jobtab->cur_do].asp = savasp;
    partab.jobtab->dostk[partab.jobtab->cur_do].ssp = savssp;
    partab.jobtab->dostk[partab.jobtab->cur_do].isp = isp;
    s = run(savasp, savssp);			// do it
    if (s == OPHALT) return s;			// just halt if reqd
    --partab.jobtab->cur_do;			// restore do frame
    if (!partab.debug) break;			// go away if debug now off
    if (s & BREAK_QN)
    { s = s - BREAK_QN;				// clear the bit
      if (s > 0)
      { partab.debug = s + partab.jobtab->commands; // when to stop
        partab.jobtab->attention = 1;		// say to check this thing
	s = 0;
        break;					// exit
      }
    }
    if (s == CMQUIT) break;			// exit on QUIT
    if (s == OPHALT)
    { partab.jobtab->io = io;			// restore io
      mumpspc = partab.jobtab->dostk[partab.jobtab->cur_do].pc; // restore pc
      return s;
    }
    var = (mvar *) &sstk[savssp];		// space to setup a var
    X_set("$ECODE\0\0", &var->name.var_cu[0], 8);
    var->volset = 0;
    var->uci = UCI_IS_LOCALVAR;
    var->slen = 0;				// setup for $EC
    ptr = (cstring *) (&sstk[savssp] + sizeof(mvar)); // for result
    bcopy("$ECODE=", ptr->buf, 7);
    s = ST_Get(var, &ptr->buf[7]);
    if (s < 1) continue;			// ignore if nothing there
    ptr->len = s + 7;
    if (partab.jobtab->seqio[0].dx)		// need a cr/lf
      s = SQ_WriteFormat(SQ_LF);		// doit
    s = SQ_Write(ptr);				// write the prompt
    s = SQ_WriteFormat(SQ_LF);			// new line
  }
  partab.jobtab->io = io;			// restore io
  mumpspc = partab.jobtab->dostk[partab.jobtab->cur_do].pc; // restore pc
  return 0;
}
