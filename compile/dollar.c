// File: mumps/compile/dollar.c
//
// module compile - evaluate functions, vars etc

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
#include <limits.h>                     	// for LONG_MAX etc
#include <math.h>
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"                              // and the error defs
#include "opcodes.h"				// and the opcodes
#include "compile.h"				// compiler stuf

void dodollar()					// parse var, funct etc
{ int len;					// length of name
  short s, ss;
  int i = 0;					// a handy int
  int sel;					// and another
  int args = 0;					// function args
  u_char *ptr;					// a handy pointer
  u_char *p;                                    // a handy pointer
  u_char *selj[256];				// a heap of them for $S()
  char name[3*MAX_NAME_BYTES];			// where to put the name
  char c;					// current character
  u_char save[1024];                            // a usefull save area
  int savecount;                                // number of bytes saved
  short errm4 = -ERRM4;                         // usefull error number
  int allowundf = 0;                            // enable UNDF function arg
  int needvar = 0;                              // $D() 2nd, $O(), $Q() 3rd arg
  c = toupper(*source_ptr++);			// get the character in upper
  if (c == '$')					// extrinsic
  { ptr = comp_ptr;				// save compile pointer
    *comp_ptr++ = CMDOTAG;			// assume a do tag
    i = routine(0);				// parse the rouref
    if ((i > -1) || (i == -4)) SYNTX		// indirect etc not on here
    if (i < -4)					// check for error
    { comperror(i);				// complain
      return;					// and exit
    }
    args = 129;					// number of args (128=$$)
    if (i == -2) *ptr = CMDORT;			// just a routine
    if (i == -3) *ptr = CMDOROU;		// both
    if (*source_ptr == '(')			// any args?
    { args--;					// back to 128
      savecount = comp_ptr - ptr;		// bytes that got compiled
      bcopy( ptr, save, savecount);		// save that lot
      comp_ptr = ptr;				// back where we started
      source_ptr++;				// skip the (
      while (TRUE)				// while we have args
      { args++;					// count an argument
	if (args > 254) SYNTX			// too many
	if (*source_ptr == ')')			// trailing bracket ?
	{ source_ptr++;				// skip the )
	  break;				// and exit
	}
	if ((*source_ptr == ',') ||
	    (*source_ptr == ')'))		// if empty argument
	{ *comp_ptr++ = VARUNDF;		// flag it
	}
        else if ((*source_ptr == '.') &&	// by reference?
	         (isdigit(source_ptr[1]) == 0))	// and not .numeric
	{ source_ptr++;				// skip the dot
	  if (*source_ptr == '@')		// if indirection
	  { source_ptr++;			// skip the @
            atom();				// eval the string
	    *comp_ptr++ = INDMVAR;
	  }
	  else
	  { p = comp_ptr;			// save current posn
	    s = localvar();			// get a variable
            if (s < 0)                    	// if we got an error
            { comperror(s);               	// compile it
              return;                     	// and exit
            }
	    p = p + s;				// point here
	    *p = OPMVAR;			// get the mvar onto stack
	  }
	  *comp_ptr++ = NEWBREF;		// flag 'by reference'
	}
	else					// by value
	{ eval();				// leave the value on the stack
	}
	if (*source_ptr == ')')			// trailing bracket ?
	  continue;				// do it above
	if (*source_ptr == ',')			// a comma ?
	{ source_ptr++;				// skip the ,
	  continue;				// go for more
	}
	SYNTX					// all else is an error
      }						// end of while
      bcopy( save, comp_ptr, savecount); 	// copy the code back
      comp_ptr = comp_ptr + savecount;		// and add to the pointer
    }						// end of argument decode
    *comp_ptr++ = (u_char) args;		// store number of args
    return;					// and exit
  }
  if (c == '&')					// xcall
  { c = toupper(*source_ptr++);			// get next
    if (c == '%')				// if it's a percent
    { name[i++] = c;				// save it
      c = toupper(*source_ptr++);		// get next
    }
    while (isalpha((int)c) != 0)		// while we have alphas
    { name[i++] = c;				// save it
      c = toupper(*source_ptr++);		// get next
    }
    name[i] = '\0';				// null terminate
    if (c == '(')				// if it has args
      while (TRUE)				// loop
      { eval();					// get next argument
        args++;					// count an argument
        c = *source_ptr++;			// get term char
        if (c == ')') break;			// all done if closing )
        if (c != ',') EXPRE			// if it's not a comma
      }						// end of args loop
    else source_ptr--;				// else backup the source ptr
    if (args > 2)				// all xcalls take 2 args
    { comperror(-(ERRZ18+ERRMLAST));		// junk
      return;
    }
    for (i = args; i < 2; i++)			// force two arguments
    { *comp_ptr++ = OPSTR;			// say string follows
      *comp_ptr++ = 0;				// these were
      *comp_ptr++ = 0;				// *((short *)comp_ptr)++ = 0
      *comp_ptr++ = '\0';			// null terminated
    }
    if (strcmp(name, "%DIRECTORY") == 0)	// $&%DIRECTORY()
      *comp_ptr++ = XCDIR;			// save the opcode
    else if (strcmp(name, "%HOST") == 0)	// $&%HOST()
      *comp_ptr++ = XCHOST;			// save the opcode
    else if (strcmp(name, "%FILE") == 0)	// $&%FILE()
      *comp_ptr++ = XCFILE;			// save the opcode
    else if (strcmp(name, "%ERRMSG") == 0)	// $&%ERRMSG()
      *comp_ptr++ = XCERR;			// save the opcode
    else if (strcmp(name, "%OPCOM") == 0)	// $&%OPCOM()
      *comp_ptr++ = XCOPC;			// save the opcode
    else if (strcmp(name, "%SIGNAL") == 0)	// $&%SIGNAL()
      *comp_ptr++ = XCSIG;			// save the opcode
    else if (strcmp(name, "%SPAWN") == 0)	// $&%SPAWN()
      *comp_ptr++ = XCSPA;			// save the opcode
    else if (strcmp(name, "%VERSION") == 0)	// $&%VERSION()
      *comp_ptr++ = XCVER;			// save the opcode
    else if (strcmp(name, "%ZWRITE") == 0)	// $&%ZWRITE()
      *comp_ptr++ = XCZWR;			// save the opcode
    else if (strcmp(name, "E") == 0)		// $&E()
      *comp_ptr++ = XCE;			// save the opcode
    else if (strcmp(name, "PASCHK") == 0)	// $&PASCHK()
      *comp_ptr++ = XCPAS;			// save the opcode
    else if (strcmp(name, "V") == 0)		// $&V()
      *comp_ptr++ = XCV;			// save the opcode
    else if (strcmp(name, "X") == 0)		// $&X()
      *comp_ptr++ = XCX;			// save the opcode
    else if (strcmp(name, "XRSM") == 0)		// $&XRSM()
      *comp_ptr++ = XCXRSM;			// save the opcode
    else if (strcmp(name, "%SETENV") == 0)	// $&%SETENV()
      *comp_ptr++ = XCSETENV;			// save the opcode
    else if (strcmp(name, "%GETENV") == 0)	// $&%GETENV()
      *comp_ptr++ = XCGETENV;			// save the opcode
    else if (strcmp(name, "%ROUCHK") == 0)	// $&%ROUCHK()
      *comp_ptr++ = XCROUCHK;			// save the opcode
    else if (strcmp(name, "%FORK") == 0)	// $&%FORK()
      *comp_ptr++ = XCFORK;			// save the opcode
    else if (strcmp(name, "%IC") == 0)		// $&%IC()
      *comp_ptr++ = XCIC;			// save the opcode
    else if (strcmp(name, "%WAIT") == 0)	// $&%WAIT()
      *comp_ptr++ = XCWAIT;			// save the opcode
    else if (strcmp(name, "DEBUG") == 0)	// $&DEBUG()
      *comp_ptr++ = XCDEBUG;			// save the opcode
    else if (strcmp(name, "%COMPRESS") == 0)	// $&%COMPRESS()
      *comp_ptr++ = XCCOMP;			// save the opcode
    else
      comperror(-(ERRZ18+ERRMLAST));		// junk
    return;					// end of xcalls
  }
  name[0] = c;					// save first char
  for (len = 0; isalpha(source_ptr[len]) != 0; len++) // scan string
    name[len+1] = source_ptr[len];		// copy alphas
  source_ptr = source_ptr + len;		// move source along
  len++;					// add in first character
  name[len] = '\0';				// null terminate name
  if (*source_ptr == '(') goto function;	// check for a function
  switch (name[0])				// dispatch on initial
  { case 'D':					// $D[EVICE]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "device", 6) != 0) UNVAR
      *comp_ptr++ = VARD;			// add the opcode
      return;					// and exit
    case 'E':					// $EC[ODE], $ES[TACK],
    						// $ET[RAP]
      if (len < 2) UNVAR			// must be 2 for this one
      switch (toupper((int)name[1]))		// switch on second char
      { case 'C':				// $EC[ODE]
	  if (len > 2)				// check for extended name
	    if (strncasecmp(name, "ecode", 5) != 0) UNVAR
        *comp_ptr++ = VAREC;			// add the opcode
        return;					// and exit
	case 'S':				// $ES[TACK]  
	  if (len > 2)				// check for extended name
	    if (strncasecmp(name, "estack", 6) != 0) UNVAR
        *comp_ptr++ = VARES;			// add the opcode
        return;					// and exit
	case 'T':				// $ET[RAP]
	  if (len > 2)				// check for extended name
	    if (strncasecmp(name, "etrap", 5) != 0) UNVAR
        *comp_ptr++ = VARET;			// add the opcode
        return;					// and exit
	default:				// junk
	  UNVAR
      }						// end of $E... switch
    case 'H':					// $H[OROLOG]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "horolog", 7) != 0) UNVAR
      *comp_ptr++ = VARH;			// add the opcode
      return;					// and exit
    case 'I':					// $I[O]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "io", 2) != 0) UNVAR
      *comp_ptr++ = VARI;			// add the opcode
      return;					// and exit
    case 'J':					// $J[OB]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "job", 3) != 0) UNVAR
      *comp_ptr++ = VARJ;			// add the opcode
      return;					// and exit
    case 'K':					// $K[EY]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "key", 3) != 0) UNVAR
      *comp_ptr++ = VARK;			// add the opcode
      return;					// and exit
    case 'P':					// $P[RINCIPAL]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "principal", 9) != 0) UNVAR
      *comp_ptr++ = VARP;			// add the opcode
      return;					// and exit
    case 'Q':					// $Q[UIT]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "quit", 2) != 0) UNVAR
      *comp_ptr++ = VARQ;			// add the opcode
      return;					// and exit
    case 'R':					// $R[EFERENCE]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "reference", 9) != 0) UNVAR
      *comp_ptr++ = VARR;			// add the opcode
      return;					// and exit
    case 'S':					// $ST[ACK], $S[TORAGE],
						// $SY[STEM]
      if ((len == 1) ||
	  (strncasecmp(name, "storage", 7) == 0)) // $S[TORAGE]
      { *comp_ptr++ = VARS;			// add the opcode
        return;					// and exit
      }
      switch (toupper((int)name[1]))		// switch on second char
      { case 'T':				// $ST[ACK]
	  if (len > 2)				// check for extended name
	    if (strncasecmp(name, "stack", 5) != 0) UNVAR
        *comp_ptr++ = VARST;			// add the opcode
        return;					// and exit
	case 'Y':				// $SY[STEM]
	  if (len > 2)				// check for extended name
	    if (strncasecmp(name, "system", 6) != 0) UNVAR
        *comp_ptr++ = VARSY;			// add the opcode
        return;					// and exit
	default:				// junk
	  UNVAR
      }						// end of $S... switch
    case 'T':					// $T[EST]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "test", 4) != 0) UNVAR
      *comp_ptr++ = VART;			// add the opcode
      return;					// and exit
    case 'X':					// $X
      if (len > 1) UNVAR			// check for extended name
      *comp_ptr++ = VARX;			// add the opcode
      return;					// and exit
    case 'Y':					// $Y
      if (len > 1) UNVAR			// check for extended name
      *comp_ptr++ = VARY;			// add the opcode
      return;					// and exit
    default:					// an error
      UNVAR
  }						// end of vars switch

function:					// function code starts here
  source_ptr++;					// incr past the bracket
  ptr = comp_ptr;				// remember where this goes
  sel = ((name[0] == 'S') && (toupper((int)name[1]) != 'T')); // is $SELECT
  allowundf = (name[0] == 'L') &&               // allow UNDF arg for $LIST
                (toupper(name[1]) != 'E');      //   functions
  if ((name[0] == 'D') ||			// $DATA
      (name[0] == 'G') ||			// $GET
     ((name[0] == 'Z') &&                       // $ZINCR[EMENT]
      (toupper((int)name[1]) == 'I'))  ||       //
      (name[0] == 'N') ||			// $NAME / $NEXT
      (name[0] == 'O') ||			// $ORDER
     ((name[0] == 'Q') &&			// $QUERY
      (toupper((int)name[1]) != 'S') &&		// but not $QS
      (toupper((int)name[1]) != 'L')))		// and not $QL

  { if (*source_ptr == '@')			// indirection ?
    { atom();					// eval it
      ptr = comp_ptr - 1;			// remember where this goes
      if (*ptr == INDEVAL)			// if it's going to eval it
      { if ((name[0] == 'N') ||
            (name[0] == 'O') ||
	    (name[0] == 'Q'))			// $NAME, $ORDER or $QUERY
	  *ptr = INDMVARN;			// allow null subs
        else
	  *ptr = INDMVAR;			// make an mvar from it
      }
      else					// experimantal for $O(@.@())
      { ptr -= 2;				// back up over subs to type
        if (*ptr == OPVAR)
        { if ((name[0] == 'N') ||
	      (name[0] == 'O') ||
	      (name[0] == 'Q'))		        // $NAME, $ORDER or $QUERY
            *ptr = OPMVARN;			// allow null subs
	  else
	    *ptr = OPMVAR;			// change to OPMVAR
	}
      }
    }
    else
    { s = localvar();				// we need a var
      if (s < 0)
      { comperror(s);				// compile the error
        return;				        // and exit
      }
      ptr = &ptr[s];				// point at the OPVAR
      if ((name[0] == 'N') ||
          (name[0] == 'O') ||
	  (name[0] == 'Q'))			// $NAME, $ORDER or $QUERY
        *ptr = OPMVARN;			        // allow null subs
      else
        *ptr = OPMVAR;			        // change to a OPMVAR
    }
  }
  else if ((name[0] == 'T') && (toupper((int)name[1]) != 'R')) // $TEXT
  { i = routine(-2);				// parse to sstk
    if (i < -4)				        // check for error
    { comperror(i);				// complain
      return;					// and exit
    }
  }
  else
  { if (allowundf &&                            // if UNDF allowed
        ((*source_ptr == ')') || (*source_ptr == ',')))
      *comp_ptr++ = VARUNDF;                    // compile a single UNDEF
    else
      eval();					// for other functions
  }
  if (name[0] == 'D')                           // $DATA 2nd arg
    needvar = 1;
  if ((name[0] == 'O') ||                       // $ORDER, $QUERY 3rd arg
      (name[0] == 'Q'))
    needvar = 2;
  while (TRUE)
  { args++;					// count an argument
    if (args > 255) EXPRE			// too many args
    c = *source_ptr++;				// get term char
    if (c == ')') break;			// all done if closing )
    if (sel)					// if in a $SELECT()
    { if (c != ((args & 1) ? ':' : ',')) EXPRE	// must be colon or comma
      *comp_ptr++ = (args & 1) ? JMP0 : JMP;	// the opcode
      selj[args] = comp_ptr;			// remember for offset
      comp_ptr = comp_ptr + sizeof(short);	// leave space for it
    }						// end special $SELECT() stuf
    else if (c != ',') EXPRE			// else must be a comma
    if (allowundf &&                            // if UNDF allowed
        ((*source_ptr == ',') || (*source_ptr == ')')))
      *comp_ptr++ = VARUNDF;
    else
    { if (needvar && (needvar == args))         // get next arg as var
      { ptr = comp_ptr;
        s = localvar();
        if (s < 0)
        { comperror(s);				// compile the error
          return;			        // and exit
        }
        ptr = &ptr[s];				// point at the OPVAR
        *ptr = OPMVAR;			        // change to a OPMVAR
      }
      else
      { eval();					// get next arg
      }
    }
  }						// end of args loop
  switch (name[0])				// dispatch on initial
  { case 'A':					// $A[SCII]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "ascii", 5) != 0) EXPRE
      if (args == 1)
	{ *comp_ptr++ = FUNA1;			// one arg form
	  return;				// and exit
	}
      if (args == 2)
	{ *comp_ptr++ = FUNA2;			// two arg form
	  return;				// and exit
	}
      EXPRE
    case 'C':					// $C[HARACTER]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "char", 4) != 0) EXPRE
      if (args > 255) EXPRE			// check number of args
      *comp_ptr++ = FUNC;			// push the opcode
      *comp_ptr++ = (u_char) args;		// number of arguments
      return;                             	// and give up
    case 'D':					// $D[ATA]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "data", 4) != 0) EXPRE
      if (args == 1)                            // check number of args
        *comp_ptr++ = FUND;			// one arg form
      else if (args == 2)
        *comp_ptr++ = FUND2;			// two arg form
      else EXPRE			        //
      return;                             	// and give up
    case 'E':					// $E[XTRACT]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "extract", 7) != 0) EXPRE
      if (args == 1)
	{ *comp_ptr++ = FUNE1;			// one arg form
	  return;				// and exit
	}
      if (args == 2)
	{ *comp_ptr++ = FUNE2;			// two arg form
	  return;				// and exit
	}
      if (args == 3)
	{ *comp_ptr++ = FUNE3;			// two arg form
	  return;				// and exit
	}
      EXPRE
    case 'F':					// $F[IND] and $FN[UMBER]
      if ((len == 1) ||
	  (strncasecmp(name, "find", 4) == 0))	// $F[IND]
      { if (args == 2)
	{ *comp_ptr++ = FUNF2;			// two arg form
	  return;				// and exit
	}
        if (args == 3)
	{ *comp_ptr++ = FUNF3;			// three arg form
	  return;				// and exit
	}
	EXPRE
      }						// end $FIND
      if (((len == 2) && (toupper((int)name[1]) == 'N')) ||
	  (strncasecmp(name, "fnumber", 7) == 0)) // $FNUMBER
      { if (args == 2)
	{ *comp_ptr++ = FUNFN2;			// two arg form
	  return;				// and exit
	}
        if (args == 3)
	{ *comp_ptr++ = FUNFN3;			// two arg form
	  return;				// and exit
	}
	EXPRE
      }						// end $FIND
      EXPRE
    case 'G':					// $G[ET]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "get", 3) != 0) EXPRE
      if (args == 1)
        *comp_ptr++ = FUNG1;			// one arg form
      else if (args == 2)
	*comp_ptr++ = FUNG2;			// the 2 arg opcode
      else
        EXPRE					// all others junk
      return;					// done
    case 'J':					// $J[USTIFY]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "justify", 7) != 0) EXPRE
      if (args == 2)
	*comp_ptr++ = FUNJ2;			// two arg form
      else if (args == 3)
	*comp_ptr++ = FUNJ3;			// three arg form
      else EXPRE				// all else is junk
      return;					// and exit
    case 'L':					// $L[ENGTH]
      if (len > 1)
      { switch (toupper(name[1]))
        { case 'B':                             // $LISTBUILD, $LB
ListBuild:  if (len > 2) EXPRE
            if (args > 255) EXPRE		// check number of args
            *comp_ptr++ = FUNLB;                // unlimited args
            *comp_ptr++ = (u_char) args;	// number of arguments
            return;                             // and exit
          case 'D':                             // $LISTDATA, $LD
ListData:   if (len > 2) EXPRE
            if (1 == args)
              *comp_ptr++ = FUNLD1;             // one arg form
            else if (2 == args)
              *comp_ptr++ = FUNLD2;             // two args form
            else EXPRE                          // all else is junk
            return;                             // and exit
          case 'E':
            if (strncasecmp(name, "length", 6) != 0) EXPRE
            goto Length;
          case 'F':                             // $LISTFIND, $LF
ListFind:   if (len > 2) EXPRE
            if (2 == args)
              *comp_ptr++ = FUNLF2;             // two args form
            else if (3 == args)
              *comp_ptr++ = FUNLF3;             // three args form
            else EXPRE                          // all else is junk
            return;                             // and exit
          case 'G':                             // $LISTGET, $LG
ListGet:    if (len > 2) EXPRE
            if (1 == args)
              *comp_ptr++ = FUNLG1;             // one arg form
            else if (2 == args)
              *comp_ptr++ = FUNLG2;             // two args form
            else if (3 == args)
              *comp_ptr++ = FUNLG3;             // three args form
            else EXPRE                          // all else is junk
            return;                             // and exit
          case 'I':                             // $LIST, $LI
            if (len > 2)
            {      if (!strncasecmp(name, "listbuild",   9)) goto ListBuild;
              else if (!strncasecmp(name, "listdata",    8)) goto ListData;
              else if (!strncasecmp(name, "listfind",    8)) goto ListFind;
              else if (!strncasecmp(name, "listget",     7)) goto ListGet;
              else if (!strncasecmp(name, "listlength", 10)) goto ListLength;
              if (strncasecmp(name, "list", 4)) EXPRE
            }
            if (1 == args)
              *comp_ptr++ = FUNLI1;             // one arg form
            else if (2 == args)
              *comp_ptr++ = FUNLI2;             // two args form
            else if (3 == args)
              *comp_ptr++ = FUNLI3;             // three args form
            else EXPRE                          // all else is junk
            return;                             // and exit
          case 'L':                             // $LISTLENGTH, $LL
ListLength: if (len > 2) EXPRE
            if (1 == args)
              *comp_ptr++ = FUNLL;              // one arg form
            else EXPRE                          // all else is junk
            return;                             // and exit
          default:
            EXPRE
        }
      }
Length: if (args == 1)
	*comp_ptr++ = FUNL1;			// one arg form
      else if (args == 2)
	*comp_ptr++ = FUNL2;			// two arg form
      else EXPRE
      return;
    case 'N':					// $NA[ME] or $N[EXT]
      if (toupper((int)name[1]) != 'A')		// check second letter
      { if (len > 1)
	{ if (strncasecmp(name, "next", 4) != 0) EXPRE
	}
	if (!(systab->historic & HISTORIC_DNOK)) EXPRE
	if (args != 1) EXPRE
	*comp_ptr++ = OPSTR;
        ss = 1;                                 // string length
        bcopy(&ss, comp_ptr, sizeof(short));
        comp_ptr += sizeof(short);
    	//*comp_ptr++ = 1;
    	//*comp_ptr++ = 0;
	*comp_ptr++ = '2';			// $N kludge
        *comp_ptr++ = '\0';			// null terminated
	*comp_ptr++ = FUNO2;			// 2 arg form of $O()
	return;
      }
      if (len > 2)				// check for extended name
        if (strncasecmp(name, "name", 4) != 0) EXPRE
      if (args == 1)
	*comp_ptr++ = FUNNA1;			// one arg form
      else if (args == 2)
	*comp_ptr++ = FUNNA2;			// 2 arg opcode
      else EXPRE				// all else is junk
      return;					// and exit
    case 'O':					// $O[RDER]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "order", 5) != 0) EXPRE
      if (args == 1)
	*comp_ptr++ = FUNO1;			// 1 arg form
      else if (args == 2)
	*comp_ptr++ = FUNO2;			// 2 arg form
      else if (args == 3)
	*comp_ptr++ = FUNO3;			// 3 arg form
      else EXPRE
      return;
    case 'P':					// $P[IECE]
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "piece", 5) != 0)
          { comperror(-(ERRMLAST+ERRZ12));	// compile an error
            return;                             // and give up
          }
      if (args == 2)
	*comp_ptr++ = FUNP2;			// two arg form
      else if (args == 3)
	*comp_ptr++ = FUNP3;			// three arg form
      else if (args == 4)
	*comp_ptr++ = FUNP4;			// four arg form
      else EXPRE
      return;
    case 'Q':					// $Q[UERY], $QS[UBSCRIPT]
						// $QL[ENGTH]
      if ((len == 1) ||
	  (strncasecmp(name, "query", 5) == 0))	// $Q[UERY]
      { if (args == 1)
	  *comp_ptr++ = FUNQ1;			// 1 arg form
	else if (args == 2)
	  *comp_ptr++ = FUNQ2;			// 2 arg form
	else if (args == 3)
	  *comp_ptr++ = FUNQ3;			// 3 arg form
	else EXPRE
	return;					// and exit
      }						// end $Q[UERY]
      if (((len == 2) && (toupper((int)name[1]) == 'L')) ||
	  (strncasecmp(name, "qlength", 7) == 0)) // $QLENGTH
      { if (args == 1)
	{ *comp_ptr++ = FUNQL;
	  return;				// and exit
	}
	EXPRE
      }						// end $FIND
      if (((len == 2) && (toupper((int)name[1]) == 'S')) ||
	  (strncasecmp(name, "qsubscript", 10) == 0)) // $QSUBSCRIPT
      { if (args == 2)
	{ *comp_ptr++ = FUNQS;
	  return;				// and exit
	}
	EXPRE
      }						// end $FIND
      EXPRE
    case 'R':					// $R[ANDOM], $RE[VERSE]
      if ((len == 1) ||
	  (strncasecmp(name, "random", 6) == 0)) // $R[ANDOM]
      { if (args == 1)
	{ *comp_ptr++ = FUNR;			// one arg form
	  return;				// and exit
	}
	EXPRE
      }
      if (((len == 2) && (toupper((int)name[1]) == 'E')) ||
	  (strncasecmp(name, "reverse", 7) == 0)) // $REVERSE
      { if (args == 1)
	{ *comp_ptr++ = FUNRE;
	  return;				// and exit
	}
	EXPRE
      }
      EXPRE
    case 'S':					// $S[ELECT], $ST[ACK]
      if ((len == 1) ||
	  (strncasecmp(name, "select", 6) == 0)) // $S[ELECT]
      { if (args & 1)				// must be even number
        { comp_ptr = ptr;			// start of this
	  EXPRE				// and error it
	}
	*comp_ptr++ = JMP;			// for the last expr
        selj[args] = comp_ptr;		// remember for offset
        comp_ptr = comp_ptr + sizeof(short);	// leave space for it
        selj[args+1] = comp_ptr;		// for the last JMP0
        *comp_ptr++ = OPERROR;		// no tve is an error
        bcopy(&errm4, comp_ptr, sizeof(short));// and the error#
        comp_ptr += sizeof(short);
	for (i = 1; i <= args; i++)		// scan the addr array
	{ if (i & 1)
	    *((short *) (selj[i])) =
	      (short) (selj[i+1] - selj[i]);
	  else
	    *((short *) (selj[i])) =
	      (short) (comp_ptr - selj[i]) - sizeof(short);
	}
	return;				// end of $SELECT()
     }
     if (((len == 2) && (toupper((int)name[1]) == 'T')) ||
         (strncasecmp(name, "stack", 5) == 0)) // $ST[ACK]
     { if (args == 1)
       { *comp_ptr++ = FUNST1;
          return;				// and exit
       }
       if (args == 2)
       { *comp_ptr++ = FUNST2;
         return;				// and exit
       }
       EXPRE
     }
    case 'T':					// $T[EXT], $TR[ANSLATE]
      if ((len == 1) ||
          (strncasecmp(name, "text", 4) == 0))	// $T[EXT]
      { if (args == 1)
        { *comp_ptr++ = FUNT;			// one arg form
          return;				// and exit
        }
        EXPRE
      }
      if (((len == 2) && (toupper((int)name[1]) == 'R')) ||
          (strncasecmp(name, "translate", 9) == 0)) // $TR[ANSLATE]
      { if (args == 2)
        { *comp_ptr++ = FUNTR2;
          return;				// and exit
        }
        if (args == 3)
        { *comp_ptr++ = FUNTR3;
	  return;				// and exit
        }
        EXPRE
      }
    case 'V':					// $VIEW
      if (len > 1)				// check for extended name
        if (strncasecmp(name, "view", 4) != 0) EXPRE
      if (args == 2)
	{ *comp_ptr++ = FUNV2;			// two arg form
	  return;				// and exit
	}
      if (args == 3)
	{ *comp_ptr++ = FUNV3;			// three arg form
	  return;				// and exit
	}
      if (args == 4)
	{ *comp_ptr++ = FUNV4;			// four arg form
	  return;				// and exit
	}
      EXPRE
    case 'Z':					// $ZINCR[EMENT](var[,expr])
      if (len > 1)
      { if ((0 == strncasecmp(name, "zincr", 5)) ||
            (0 == strncasecmp(name, "zincrement", 10)))
        { if (args == 1)
            *comp_ptr++ = FUNZINC1;		// one arg form
          else if (args == 2)
	    *comp_ptr++ = FUNZINC2;		// the 2 arg opcode
          else
            EXPRE				// all others junk
          return;				// done
        }
        else if (0 == strncasecmp(name, "zbitstr", 7)) // $ZBITSTR(len[,ff)
        { if (1 == args)                        // one arg form
            *comp_ptr++ = FUNZBSTR1;
          else if (2 == args)                   // the 2 arg opcode
            *comp_ptr++ = FUNZBSTR2;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitlen", 7)) // $ZBITLEN(bstr)
        { if (1 == args)                        // one arg form
            *comp_ptr++ = FUNZBLEN;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitcount", 9)) // $ZBITCOUNT(bstr)
        { if (1 == args)                        // one arg form
            *comp_ptr++ = FUNZBCNT;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitget", 7))
        { if (2 == args)                        // two args form
            *comp_ptr++ = FUNZBGET;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitset", 7)) // $ZBITSET(bstr,pos,ff)
        { if (3 == args)                        // three args form
            *comp_ptr++ = FUNZBSET;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
                                                // $ZBITFIND(bstr,ff[,pos])
        else if (0 == strncasecmp(name, "zbitfind", 8))
        { if (2 == args)                        // two args form
            *comp_ptr++ = FUNZBFND2;
          else if (3 == args)                   // three args form
            *comp_ptr++ = FUNZBFND3;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitnot", 7))
        { if (1 == args)                        // one arg form
            *comp_ptr++ = FUNZBNOT;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitand", 7))
        { if (2 == args)                        // two args form
            *comp_ptr++ = FUNZBAND;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitor", 6))
        { if ( 2 == args)                       // two args form
            *comp_ptr++ = FUNZBOR;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        else if (0 == strncasecmp(name, "zbitxor", 7))
        { if (2 == args)                        // two args form
            *comp_ptr++ = FUNZBXOR;
          else
            EXPRE                               // all others junk
          return;                               // done
        }
        
      }
      EXPRE
    default: 
	EXPRE
  }						// end of switch
}
