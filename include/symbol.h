// File: mumps/include/symbol.h
//
// module MUMPS header file - includes for symbol module

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


#ifndef _MUMPS_SYMBOL_H_                        // only do this once
#define _MUMPS_SYMBOL_H_

#define DTBLKSIZE sizeof(ST_depend*)+(sizeof(short)*2)+sizeof(char)
#define DTMINSIZ 32				// leaves 21 for data
#define DPBLKSIZE \
  (sizeof(u_char)+sizeof(ST_depend *)+sizeof(short)+sizeof(char)+ \
   sizeof(u_char)+(MAX_SUBSCRIPTS+1)*sizeof(u_char))
#define NTBLKSIZE sizeof(ST_newtab *)+2*sizeof(short)+sizeof(short *)+sizeof(ST_locdata *)

struct ST_DATA;                                 // defined below
typedef struct __attribute__ ((__packed__)) NEW_STACK // define new stack
{ short type;                                   // type of new
  short ptr;                                    // ptr to variable
  struct ST_DATA *data;                         // data address
} new_stack;                                    // end of struct new_stack

//** SYMTAB definitions **
// #define ST_HASH         1023                    // hash size of symtab
// #define ST_FREE         ST_HASH                 // head of free list
// #define ST_MAX          ((ST_HASH + 1) * 3)     // max number of ST entries

#define ST_HASH         1024                    // hash size of symtab
#define ST_FREE         ST_HASH                 // head of free list
#define ST_MAX          ((ST_HASH) * 3)         // max number of ST entries

// structures for symbol table data

#define SIZ_KEY_DATA    (32768+256+3)           // for the following

typedef struct __attribute__ ((__packed__)) ST_DEPEND // symbol dependant block
{ struct ST_DEPEND *deplnk;                     // dependants link
#ifdef MV1_SUBSPOS
  u_char nsubs;                                 // no. of subscripts
  u_char subspos[MAX_SUBSCRIPTS+1];             // len. of upOneLevel key
#endif
  u_char keylen;                                // length of key (bytes)
  u_char bytes[SIZ_KEY_DATA];                   // key bytes then data bytes
} ST_depend;                                    // end ST_depend structure

typedef struct __attribute__ ((__packed__)) ST_DATA // symbol data block
{ ST_depend *deplnk;                            // dependants link
  short attach;                                 // variable attach count
  short dbc;                                    // data byte count
  u_char data[MAX_STR_LEN+1];                   // data bytes
} ST_data;                                      // end st_data structure
#define ST_DEPEND_NULL (ST_depend *) NULL       // define null pointer
#define ST_DATA_NULL (ST_data *) NULL           // define null pointer

typedef struct __attribute__ ((__packed__)) SYMTAB // define symtab structure
{ short fwd_link;                               // link to next entry
  short usage;                                  // usage count
  struct ST_DATA *data;                         // data block pointer
  var_u varnam;                                 // variable name union
} symtab_struct;       				// end symtab structure
//#define var_q varnam.var_qu                     // shorthand for quadword
#define var_x varnam.var_xu                     // shorthand for variable len
#define var_c varnam.var_cu                     // shorthand for char version
extern short st_hash[];                         // allocate hashing table
extern symtab_struct symtab[];                  // and symbol table

typedef struct __attribute__ ((__packed__)) ST_LOCDATA
{ short stindex;				// location in symtab
  ST_data *data;				// pointer to data
} ST_locdata;

typedef struct __attribute__ ((__packed__)) ST_NEWTAB
{ struct ST_NEWTAB *fwd_link;		// link to another newalltab
  short count_enn;			// existing non new count
  short *stindex;				// symtab indexes of enn vars
  short count_new;			// count of new'd vars
  ST_locdata *locdata;			// location of var and data
} ST_newtab;

typedef struct __attribute__ ((__packed__)) KEY_STRUCT // start struct KEY
{ u_char slen;                                  // length of key
  u_char key[256];                              // the actual key
} key_s;                                        // have 256 chars

//short ST_Locate(chr_q var);                   // locate a var name
short ST_Locate(chr_x *var);                    // locate a var name
short ST_LocateIdx(int idx);			// locate in symtab by index
//short ST_Create(chr_q var);                   // create and/or locate a var
short ST_Create(chr_x *var);                    // create and/or locate a var

void ST_RemDp(ST_data *dblk, ST_depend *prev, ST_depend *dp,
                             mvar *mvardr, int what);

void ST_Restore(ST_newtab *newtab);

short ST_Storage(void);                         // no. of free slots in symtab[]

#endif						// !_MUMPS_SYMBOL_H_
