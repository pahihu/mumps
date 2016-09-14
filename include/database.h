// file: mumps/database/database.h
//
// module database header file - standard includes

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


#ifndef _MUMPS_DATABASE_H_				// only do this once
#define _MUMPS_DATABASE_H_


// **** Defines ***************************************************************

#define READ		(-1)				// Locking defines makes
#define WRITE		(-systab->maxjob)		// easy reading code
#define WR_TO_R		(systab->maxjob-1)		// from write to read

#define NODE_UNDEFINED	-1				// junk record
#define PTR_UNDEFINED	0				// junk pointer

// Below is the maximum depth that the database code will search down to.
// (Note: With a minimum block size of 4kb a depth of 12 allows for 
//        potentially about 8.649e+012 data blocks available.)

#define MAXTREEDEPTH	12				// max level down
#define LAST_USED_LEVEL	3				// when last used works

#define MAXREKEY	(MAXTREEDEPTH * 3)		// max re-keys

#define GBD_EXPIRED	60				// seconds to expire

// DB_Block flags
#define BLOCK_DIRTY	1				// block needs tidying

// Write Daemon defines
#define DOING_NOTHING	0				// nothing
#define DOING_MAP	1				// cleaning the map
#define DOING_WRITE	2				// writing
#define DOING_GARB	3				// garbage collect
#define DOING_DISMOUNT	4				// dismounting

// MUMPS time
#define MTIME(x)        systab->Mtime                   // updated by daemon 0

// #include <libkern/OSAtomic.h>
// #define ATOMIC_INCREMENT(x)     OSAtomicIncrement32((volatile int32_t*)&(x))
#define ATOMIC_INCREMENT(x)        __sync_add_and_fetch(&(x), 1)

// **** Structures ***********************************************************

typedef struct __attribute__ ((__packed__)) DB_BLOCK	// database block layout
{ u_char type;						// block type
  u_char flags;						// flags
  u_short spare;					// future
  u_int right_ptr;					// right pointer
  u_short last_idx;					// last used index off
  u_short last_free;					// last free lw in block
  chr_x global;						// global name
  u_int64 blkver;
} DB_Block;						// end block header

#if MAX_NAME_BYTES == 8
#define BLK_HDR_SIZE	20                              // (1+sizeof(DB_Block))
                                                        //   / sizeof(short)
#define LOW_INDEX	10                              // BLK_HDR_SIZE
                                                        //   / sizeof(short)
#else
#define BLK_HDR_SIZE    52                              // 44
#define LOW_INDEX       26                              // 22
#endif

#define MV1_CACHE	1
//#define MV1_REFD	1
#undef MV1_BLKVER
#undef MV1_RSVD
#undef MV1_FORCE

typedef struct __attribute__ ((__packed__)) GBD		// global buf desciptor
{ u_int block;						// block number
  struct GBD *next;					// next entry in list
  struct DB_BLOCK *mem;					// memory address of blk
  struct GBD* dirty;				        // to write -> next
  time_t last_accessed;			                // last time used
#ifdef MV1_BLKVER
  u_int  blkver_low;                                    // blk version LOW
  u_int  blkver_high;                                   // blk version HIGH
#endif
#ifdef MV1_REFD
  u_int  referenced;
#endif
} gbd;							// end gbd struct

typedef struct __attribute__ ((__packed__)) JRNREC	// journal record
{ u_short size;						// size of record
  u_char action;					// what it is
  u_char uci;						// uci number
  time_t time;						// now
  var_u name;						// global name
  u_char slen;						// subs length
  u_char key[256];					// the key to 256 char
//short dbc;						// data byte count
//u_char data[32767];					// bytes to 32767
} jrnrec;						// end jrnrec struct

#define JRN_CREATE	0				// create file
#define JRN_START	1				// start/mount environ
#define JRN_STOP	2				// stop journaling
#define JRN_ESTOP	3				// stop/dism environ
#define JRN_SET		4				// Set global
#define JRN_KILL	5				// Kill global

// Note: The first 4 bytes (u_int) = (MUMPS_MAGIC - 1).
//	 The next 8 bytes (off_t) in the file point at the next free byte.
//	 (Initially 12 and always rounded to the next 4 byte boundary).
//	 Journal file is only accessed while a write lock is held.
//	 Protection on the file is changed to g:rw by init_start (TODO).

// **** External declarations ************************************************
// Defined in database/db_main.c

extern u_int64 txid;
extern int curr_lock;					// lock on globals
extern int gbd_expired;
extern mvar db_var;					// local copy of var
extern int volnum;					// current volume

extern gbd *blk[MAXTREEDEPTH];				// current tree
extern int level;					// level in above
							// 0 = global dir
extern u_int rekey_blk[MAXREKEY];			// to be re-keyed
extern int   rekey_lvl[MAXREKEY];			// from level

extern int Index;					// index # into above
extern cstring *chunk;					// chunk at index
extern cstring *record;					// record at index
							// points at dbc
extern u_char keybuf[260];				// for storing keys
extern u_short *idx;					// for indexes
extern int *iidx;					// int ver of index

extern int writing;					// set when writing
extern int wanna_writing;

extern int hash_start;					// start searching here

//**** Function Prototypes*****************************************************

// File: database/db_buffer.c
short Get_block(u_int blknum);				// Get block
short New_block();					// get new block
void Get_GBD();						// get a GBD
void Get_GBDs(int greqd);				// get n free GBDs
void Free_GBD(gbd *free);				// Free a GBD

// File: database/db_get.c
short Get_data(int dir);				// get db_var node

// File: database/db_kill.c
short Kill_data();					// remove tree

// File: database/db_locate.c
short Locate(u_char *key);				// find key
short LocateEx(u_char *key, int frominsert);		//   used in Insert()
short Locate_next(u_char *out);				// point at next key

// File: database/db_rekey.c
short Add_rekey(u_int block, int level);		// add to re-key table
short Re_key();						// re-key blocks
void Un_key();						// un-key blk[level]

// File: database/db_set.c
short Set_data(cstring *data);				// set a record

// File: database/db_util.c
void Allign_record();					// allign record (int)
void Copy_data(gbd *fptr, int fidx);			// copy records
void DoJournal(jrnrec *jj, cstring *data); 		// Write journal
void Free_block(int blknum);				// free blk in map
void Garbit(int blknum);				// que a blk for garb
short Insert(u_char *key, cstring *data);		// insert a node
void Queit();						// que a gbd for write
void Tidy_block();					// tidy current blk
void Used_block(int blknum);				// set blk in map
short Compress1();					// compress 1 block
void Ensure_GBDs();                                     // wait for GBDs

//*****************************************************************************


#endif							// !_MUMPS_DATABASE_H_
