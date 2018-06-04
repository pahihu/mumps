// File: mumps/compile/eval.c
//
// module compile - evaluate

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

u_char *source_ptr;                             // pointer to source code
u_char *comp_ptr;                               // pointer to compiled code
int     disp_errors;				// flag to display errors

void CompError(short err,const char *file,int lno)// compile error
{ short s;					// for functions
  cstring *line;				// line of code
  u_char *src;					// current src ptr
  int i;					// a handy int
  u_char tmp[128];				// some space

  // fprintf(stderr,"comperror() called from %s:%d:\r\n",file,lno);
  *comp_ptr++ = OPERROR;                        // say it's an error
  bcopy(&err, comp_ptr, sizeof(short));
  comp_ptr += sizeof(short);
  *comp_ptr++ = OPNOP;				// in case of IF etc
  *comp_ptr++ = OPNOP;				// in case of IF etc
  if (!partab.checkonly) goto scan;		// done
  if (!disp_errors) return;			// done if disp.errors disabled
  if (partab.checkonly == *partab.ln) return; 	// done this one once
  partab.checkonly = *partab.ln;		// record done
  line = *partab.lp;				// get the line address
  src = *partab.sp;				// and the current source
  s = SQ_Write(line);				// write the line
  if (s < 0) goto scan;				// exit on error
  s = SQ_WriteFormat(SQ_LF);			// return
  if (s < 0) goto scan;				// exit on error
  i = (src - line->buf) - 1;			// get the offset
  if (i > 0)
  { s = SQ_WriteFormat(i);			// tab
    if (s < 0) goto scan;			// exit on error
  }
  line = (cstring *) tmp;			// some space
  line->buf[0] = '^';				// point
  line->buf[1] = ' ';				// and a space
  s = UTIL_strerror(err, &line->buf[2]);	// get the error
  if (s < 0) goto scan;				// exit on error
  line->len = s + 2;				// the length
  bcopy(" - At line ", &line->buf[line->len], 11); // front bit
  s = itocstring(&line->buf[line->len + 11], *partab.ln); // format line number
  if (s < 0) goto scan;				// exit on error
  line->len = line->len + s + 11;		// the length
  s = SQ_Write(line);				// write the line
  if (s >= 0)					// if no error error
    s = SQ_WriteFormat(SQ_LF);			// return
scan:
  while (*source_ptr) source_ptr++;		// skip rest of line
  return;					// and done
}


// function atom entered with source_ptr pointing at the source
// atom to evaluate and comp_ptr pointing at where to put the code.
//
void atom()                                     // evaluate source
{ char c;                                       // current character
  int j;                                        // and another
  short s;                                      // for function returns
  u_char *p;                                    // a pointer
  u_char *sav_source_ptr;			// save source_ptr

  // fprintf(stderr,"atom: [%s]\r\n",source_ptr);
  c = *source_ptr++;                            // get a character
  if (c == '@')					// indirection?
  { atom();					// eval what follows
    if (*source_ptr != '@')			// another one?
    { *comp_ptr++ = INDEVAL;			// no, eval what follows
      return;					// and just exit
    }
    *comp_ptr++ = INDMVAR;			// make an mvar out of it
    s = localvar();				// parse the rest of it
    if (s < 0)					// if we got an error
    { comperror(s);				// compile it
      return;					// and exit
    }
    return;					// and exit
  }
  if ((isalpha((int)c) != 0) || (c == '%') ||   // check for local variable
      (c == '^') ||				// or a global var
      ((c == '$') &&                            // check for $BP
       (0 == strncasecmp((char *)source_ptr,"BP",2))))
  { --source_ptr;				// backup to first character
    sav_source_ptr = source_ptr;
    s = localvar();				// parse the variable
    if (s < 0)					// if we got an error
    { // fprintf(stderr,"localvar() failed: %s\r\n",sav_source_ptr);
      comperror(s);				// compile it
      return;					// and exit
    }
    return;					// and exit
  }                                             // end variable parse

  if (c == '$')                                 // check for a function
  { dodollar();					// eval it
    return;                                     // and exit
  }                                             // end function parse

  if ((isdigit((int)c) != 0) || (c == '.'))     // check for number or dot
  { source_ptr--;                               // back up the source ptr
    *comp_ptr++ = OPSTR;                        // say string following
    s = ncopy(&source_ptr, comp_ptr+2);         // copy as number
    *((short *)comp_ptr) = s;                   // store string count
    comp_ptr = comp_ptr + sizeof(short) + s + 1; // allow for null byte
    return;
  }                                             // end numeric parse

  if (c == '"')                                 // rabit ear
  { *comp_ptr++ = OPSTR;                        // say string following
    p = comp_ptr;                               // possible destination
    j = 2;                                      // point at p->buf[0]
    while (TRUE)                                // scan the string
    { if (*source_ptr == '\0')                  // check for end of string
      { comp_ptr--;				// remove the OPSTR
        comperror(-(ERRZ12+ERRMLAST));          // compile an error
        return;                                 // and exit
      }                                         // end of error bit
      if ((*source_ptr == '"') &&
          (source_ptr[1] != '"'))               // check end of literal
      { p[j] = '\0';                            // null terminate it
        source_ptr++;                           // point past it
        break;                                  // and exit
      }                                         // end 'end of str' code
      p[j++] = *source_ptr++;                   // copy the character
      if ((*(source_ptr-1) == '"') &&
          (*source_ptr == '"'))                 // check for rabit ear
        source_ptr++;                           // point past the second one
    }                                           // end of copy loop
    *((short *)p) = (short) j-2;                // store cstring count
    comp_ptr = comp_ptr + j + 1;                // point past str and null
    return;
  }                                             // end string literal

  if (c == '\'')                                // check for single quote
  { atom();                                     // get the following
    *comp_ptr++ = OPNOT;                        // do the NOT
    return;
  }                                             // end NOT parsing

  if (c == '+')                                 // check for plus
  { atom();                                     // get the following
    *comp_ptr++ = OPPLUS;                       // do the plus
    return;
  }                                             // end NOT parsing

  if (c == '-')                                 // check for unary minus
  { atom();                                     // get the following
    *comp_ptr++ = OPMINUS;                      // do the minus
    return;
  }                                             // end NOT parsing

  if (c == '(')                                 // open bracket
  { eval();                                     // eval content of ()
    if (*source_ptr++ != ')')                   // error if no trailing ) found
    { comperror(-(ERRZ12+ERRMLAST));            // compile an error
      return;                                   // and exit
    }                                           // end error
    return;
  }                                             // end open bracket parse

  comperror(-(ERRZ12+ERRMLAST));                // compile an error
  return;                                       // and exit
}

int operator()                                  // extract an operator
{ char c;                                       // the character
  int not = 0;                                  // not flag
  c = *source_ptr++;                            // get next char
  if (c == '\'')				// a NOT?
  { if (not) return 0;				// can't have two
    not = 1;					// set the not
    c = *source_ptr++;                          // get next char
  }
  switch(c)
  { case '+':                                   // add
      if (not) return 0;			// a not here is junk
      return OPADD;                             // save opcode
    case '-':                                   // subtract
      if (not) return 0;			// a not here is junk
      return OPSUB;                             // save opcode
    case '*':                                   // multiply (or power)
      if (not) return 0;			// a not here is junk
      if (*source_ptr == '*')                   // if there is another
      { source_ptr++;                           // advance the pointer
        return OPPOW;                           // it's a power
      }
      return OPMUL;                             // set as a multiply
    case '/':                                   // divide
      if (not) return 0;			// a not here is junk
      return OPDIV;                             // set the op code
    case '\\':                                  // back-slash
      if (not) return 0;			// a not here is junk
      return OPINT;                             // integer divide
    case '#':                                   // hash
      if (not) return 0;			// a not here is junk
      return OPMOD;                             // modulus
    case '_':                                   // underscore
      if (not) return 0;			// a not here is junk
      return OPCAT;                             // concatenate
    case '=':                                   // equal sign
      return not ? OPNEQL : OPEQL;              // equal or not
    case '<':                                   // less than
      return not ? OPNLES : OPLES;              // less than or not
    case '>':                                   // greater than
      return not ? OPNGTR : OPGTR;              // greater than or not
    case '&':                                   // and
      return not ? OPNAND : OPAND;              // and or nand
    case '!':                                   // exclam
      return not ? OPNIOR : OPIOR;              // or or nor
    case '[':                                   // left square bracket
      return not ? OPNCON : OPCON;              // contains or not
    case ']':                                   // right square bracket
      if (*source_ptr == ']')                   // if there is another
      { source_ptr++;                           // advance the pointer
        return not ? OPNSAF : OPSAF;            // sorts after or not
      }
      return not ? OPNFOL : OPFOL;              // follows or not
    case '?':                                   // question
      return not ? OPNPAT : OPPAT;              // matches or not
    case '.':                                   // chain
      return FUNZSE;
    default:                                    // stuffed up
      return 0;                                 // clear op
  }                                             // end of switch for operators
}


// function eval entered with source_ptr pointing at the source
// expression to evaluate and comp_ptr pointing at where to put the code.
//
void eval(int chain)                            // evaluate source
{ evalx(0);
}

// call chaining
//      0 - no chain
//      1 - x.F1(a1...).F2(b1...)
//      2 - x.F1(a1...).P2(b1...) last is an extrinsic routine call

void evalx(int chain)                           // evaluate source
{ int op;                                       // operator
  int q;					// in quotes indicator
  int patmat = 0;				// for pattern match funnee
  cstring *ptr;					// spare pointer
  u_char c;

  // fprintf(stderr, "\r\neval(): [%s]", source_ptr);
  atom();                                       // get first operand
  // fprintf(stderr,"next: '%c'\r\n",*source_ptr);
  if ((*source_ptr == ')') ||			// do it at a higher level
      (*source_ptr == ',') ||			// ditto
      (*source_ptr == ':') ||			// ditto
      (*source_ptr == '\0') ||			// end of string
      (*source_ptr == '^') ||			// start of routine ref
      (*source_ptr == '@') ||			// end of name indirection
      (*source_ptr == ' '))                     // end of command
    return;                                     // exit

  while (TRUE)                                  // until the end
  { // fprintf(stderr,"oper: %c\r\n",*source_ptr);
    op = operator();                            // get the operator
    if (op == 0)                                // an error??
    { comperror(-(ERRZ12+ERRMLAST));            // compile the error
      return;                                   // and exit
    }
    patmat = ((op == OPPAT) || (op == OPNPAT));	// bloody pattern match
    if (patmat && (*source_ptr == '@'))		// indirect pattern
    { source_ptr++;				// skip the @
      patmat = 0;				// clear funnee pattern match
    }
    if (patmat)					// normal (not @) pattern match
    { q = 0;					// not in quotes
      *comp_ptr++ = OPSTR;			// pretend it's a string
      ptr = (cstring *) comp_ptr;		// remember for ron
      comp_ptr = comp_ptr + sizeof(short);	// skip the count      
      while (TRUE)				// loop
      { c = *source_ptr++;			// get next char
	if (q)					// if in quotes
	{ *comp_ptr++ = c;			// copy char
	  if (c == '"') q = 0;			// check for a quote
	  continue;				// go for more
	}
	if (c == '"')				// if it's a quote
	{ q = 1;				// turn on
	  *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if ((isalnum(c) != 0) ||		// alpha numeric
	    (c == '.'))				// or a dot
	{ *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if (c == '(')				// open bracket
	{ patmat++;				// count it
	  *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if ((c == ')') && (patmat > 1))		// close bracket
	{ --patmat;				// count it
	  *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if ((patmat > 1) && (c == ','))		// comma inside ()
	{ *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	source_ptr--;				// backup
	break;					// and exit
      }						// end while
      ptr->len = comp_ptr - ptr->buf;		// get the length
      *comp_ptr++ = '\0';			// null terminate its
    }
    else if (op == FUNZSE)                      // chaining
      dodollarx(chain ? chain : 1);             //   already provided, or flag
    else atom();                                // else get next operand
    if (op != FUNZSE)
      *comp_ptr++ = (u_char) op;                // store the operator
    if ((*source_ptr == ')') ||                 // do it at a higher level
        (*source_ptr == ',') ||                 // ditto
        (*source_ptr == ':') ||                 // ditto
        (*source_ptr == '\0') ||                // end of string
        (*source_ptr == '^') ||			// start of routine ref
        (*source_ptr == ' '))                   // end of command
    { // in a DO chain all but the last is an extrinsic call
      // the last is just a DO
      if ((chain == 2) && (op == FUNZSE))       // DO chain, last was $ZSEND
      { ASSERT(comp_ptr[-1] > 127);
        comp_ptr[-1] -= 128;
      }
      return;                                   // exit
    }
  }
}
