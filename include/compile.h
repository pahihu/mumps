// File: mumps/include/compile.h
//
// module MUMPS header file - routine structures etc

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


#ifndef _MUMPS_COMPILE_H_                       // only do this once
#define _MUMPS_COMPILE_H_


#define UNVAR { comperror(-ERRM8); return; } 	// compile undef spec var
#define EXPRE { comperror(-(ERRZ12+ERRMLAST)); return; } // compile expr err
#define SYNTX { comperror(-(ERRZ13+ERRMLAST)); return; } // compile syntax err
#define ERROR(E) { partab.jobtab->async_error = E;\
		   partab.jobtab->attention = 1;\
		   break; }				// report an error

#define INDSNOK(S) (((S * 2) + (sizeof(int) *2) + isp) > MAX_ISTK)
	// The above is for testing indirection size - a guess
#define INDANOK(A) ((comp_ptr + (sizeof(int) * 2) + 1) >= &istk[MAX_ISTK])
	// The above is for testing the address of compiled indirection


#define RBD_OVERHEAD sizeof(rbd *) + sizeof(int) \
		+ sizeof(int) + sizeof(time_t) + sizeof(var_u) \
		+ sizeof(u_char) + sizeof(u_char) + sizeof(short)

#define RESERVE_TIME	17*60			// 17 minutes
#define SIZE_CLOSE	1024			// routine size match

#define FOR_TYP_0       0			// no args
#define FOR_TYP_1       1			// one arg
#define FOR_TYP_2       2			// two args
#define FOR_TYP_3       3			// three args

#define FOR_NESTED      16			// we are not an outside for

// Funnee op code stuff

#define BREAK_NOW 256				// break (not really an opcode)
#define JOBIT	512				// JOB (ditto)
#define BREAK_QN 16384				// return a QUIT n

// ** Variable types follow **

#define TYPMAXSUB	63			// max subscripts
#define TYPVARNAM	0			// name only (8 bytes)
#define TYPVARLOCMAX	TYPVARNAM+TYPMAXSUB	// local is 1->63 subs
#define TYPVARIDX	64			// 1 byte index (+ #subs)
#define TYPVARGBL	128			// first global
#define TYPVARGBLMAX	TYPVARGBL+TYPMAXSUB	// global 128->191 subs
#define TYPVARNAKED	252			// global naked reference
#define TYPVARGBLUCI	253			// global with uci
#define TYPVARGBLUCIENV	254			// global with uci and env
#define TYPVARIND	255			// indirect


extern u_char *source_ptr;                      // pointer to source code
extern u_char *comp_ptr;                        // pointer to compiled code

extern u_char istk[];				// indirect stack
extern long isp;				// indirect stack pointer

typedef struct __attribute__ ((__packed__)) FOR_STACK // saved FOR details
{ short type;					// type of for (see above)
  short svar;					// syment of simple var
						// ( if -1 use var)
  mvar *var;					// mvar on sstk of variable
  u_char *nxtarg;				// where to jump for next
  u_char *startpc;				// where the actual code starts
  u_char *quit;					// where to quit to
  u_char *increment;				// normalized incr string
  u_char *done;					// normalized end point
} for_stack;					// end of FOR stuff


typedef struct __attribute__ ((__packed__)) TAGS // define routine tags
{ var_u name;                                   // tag name
  u_short code;                                 // start of code this tag
} tags;                                         // end tags struct


typedef struct __attribute__ ((__packed__)) RBD // define routine buf desciptor
{ struct RBD *fwd_link;				// forward link this hash
  int chunk_size;                               // bytes in this chunk
  int attached;                                 // processes attached
  time_t last_access;                           // last used (sec since 1970)
  var_u rnam;                                   // routine name
  u_char uci;                                   // uci num for this rou
  u_char vol;					// vol num for this rou
  short rou_size;				// rou->len of routine node

// what follows is the routine from disk (up to 32767 bytes + a NULL)

  u_short comp_ver;                             // compiler version
  u_short comp_user;                            // compiled by user#
  int comp_date;                                // date compiled (MUMPS form)
  int comp_time;                                // time compiled (MUMPS form)
  u_short tag_tbl;                              // offset to tag table
  u_short num_tags;                             // number of tags in table
  u_short var_tbl;                              // offset to var table
  u_short num_vars;                             // number of vars in table
  u_short code;                                 // offset to compiled code
  u_short code_size;                            // bytes of code

} rbd;                 				// end rbd struct

// Compile only prototypes follow

void parse_close();				// CLOSE
void parse_do(int runtime);			// DO
void parse_goto(int runtime);			// GOTO
void parse_hang();				// HANG
void parse_if(int i);				// IF
void parse_job(int runtime);			// JOB
void parse_kill();				// KILL
void parse_lock();				// LOCK
void parse_merge();				// MERGE
void parse_new();				// NEW
void parse_open();				// OPEN
void parse_read();				// READ
void parse_set();				// SET
void parse_use();				// USE
void parse_write();				// WRITE
void parse_xecute();				// XECUTE

void parse();                                   // parse - main loop

short localvar();                               // evaluate local variable
void eval();                                    // eval a string
void atom();                                    // evaluate source
void comperror(short err);                      // compile error

// Debug prototypes

#if defined(__NetBSD__)
void Debug_GDB(short e);
#endif
void  Debug_off();				// turn off debugging
short Debug_on(cstring *param);			// turn on/modify debug
short Debug(int savasp, int savssp, int dot);	// drop into debug


#endif                                          // _MUMPS_COMPILE_H_
