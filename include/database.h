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

#ifndef VOLATILE
#define VOLATILE
#endif

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

// #include <libkern/OSAtomic.h>
// #define ATOMIC_INCREMENT(x)      OSAtomicIncrement32((volatile int32_t*)&(x))
// #define ATOMIC_INCREMENT(x)      __sync_add_and_fetch(&(x), 1)
#define ATOMIC_INCREMENT(x)         inter_add(&(x),1)

#define BLKNO(x)        ((x) & ((1 << 25) - 1))		// 25bit block number
#define VOLNO(x)        (15 & ((x) >> 25))		//  4bit volume number
#define VOLBLK(vol,blk) ((((vol) & 15) << 25) | (blk))

// **** Structures ***********************************************************

typedef struct __PACKED__ DB_BLOCK	             	// database block layout
{ u_char type;						// block type
  u_char flags;						// flags
  u_char bkprevno;                                      // backup revision
  u_char spare;					        // future
  u_int right_ptr;					// right pointer
  u_short last_idx;					// last used index off
  u_short last_free;					// last free lw in block
  chr_x global;						// global name
  u_int blkver_lo;
  u_int blkver_hi;
} DB_Block;						// end block header

/*
 * BLK_HDR_SIZE	- (1 + sizeof(DB_Block)) / sizeof(short)
 * LOW_INDEX	- BLK_HDR_SIZE / sizeof(short)
 *
 */
#if MAX_NAME_BYTES == 8
#define BLK_HDR_SIZE	20
#define LOW_INDEX	10
#else
#define BLK_HDR_SIZE    52
#define LOW_INDEX       26
#endif

/*
 * MV1_CCC        - use MV1 key build routines, key locate is sequential
 * MV1_CCC_DOCOMP - do MV1 prefix compression on keys
 *
 * If MV1_CCC is not defined, MV1R2DEV does NOT prefix compress keys,
 * and uses binary search to locate the key.
 *
 */
#undef MV1_CCC
#undef MV1_CCC_DOCOMP

#undef MV1_CACHE

/*
 * MV1_CACHE_DEBUG	- debug global buffer cache
 * MV1_CACHE_IO		- report replacement in global buffer cache
 * MV1_LOCATE_DEBUG	- check key uniqueness in global blocks
 *
 */
#undef MV1_CACHE_DEBUG
#undef MV1_CACHE_IO
#undef MV1_LOCATE_DEBUG

/*
 * MV1_REFD	    - use CLOCK algorithm for block replacement
 * MV1_REFD_GCLOCK  - use generalized CLOCK algorithm for block replacement
 *
 */
#define MV1_REFD	        1
#define MV1_REFD_GCLOCK         1

#ifdef MV1_REFD_GCLOCK

#define REFD_NEW_START          1
#define REFD_READ_START         1

#define REFD_NEW_INIT(x)        (x)->refd = REFD_NEW_START
#define REFD_READ_INIT(x)       (x)->refd = REFD_READ_START
#define REFD_TYPED_INIT(x)
#define REFD_VALUE(x)           (x)->refd
#define REFD_MARK(x)            (x)->refd++
#define REFD_UNMARK(x)          if ((x)->refd) (x)->refd--
#define REFD_CLEAR(x)           (x)->refd = 0
#define REFD_RETYPE(x)          REFD_UNMARK(x)
#define REFD_DEC(x,n)           \
        { if ((n) > (x)->refd)  \
          { REFD_CLEAR(x);      \
          }                     \
          else                  \
          { (x)->refd -= (n);   \
          }                     \
        }

#else

#define REFD_NEW_START          1
#define REFD_READ_START         1

#define REFD_NEW_INIT(x)        (x)->refd = REFD_NEW_START
#define REFD_READ_INIT(x)       (x)->refd = REFD_READ_START
#define REFD_TYPED_INIT(x)
#define REFD_VALUE(x)           (x)->refd
#define REFD_MARK(x)            (x)->refd = 1
#define REFD_UNMARK(x)          if ((x)->refd) (x)->refd--
#define REFD_CLEAR(x)           (x)->refd = 0
#define REFD_RETYPE(x)          REFD_UNMARK(x)
#define REFD_DEC(x,n)           \
        { if (n > (x)->refd)    \
          { REFD_CLEAR(x);      \
          }                     \
          else                  \
          { (x)->refd -= (n);   \
          }			\
        }

#endif

typedef struct __ALIGNED__ GBD		                // global buf desciptor
{ u_int block;						// block number
#ifdef MV1_REFD
  VOLATILE struct GBD *prev;				// prev entry in list
#endif
  VOLATILE struct GBD *next;				// next entry in list
  struct DB_BLOCK *mem;					// memory address of blk
  VOLATILE struct GBD* dirty;				// to write -> next
  VOLATILE time_t last_accessed;			// last time used
#ifdef MV1_REFD
  u_int  refd;                                          // block referenced
  int    hash;                                          // which chain?
#endif
#ifdef MV1_BLKSEM
  short  curr_lock;                                     // current block lock
#endif
  u_char vol;                                           // vol[] index
} gbd;							// end gbd struct

#define MIN_JRNREC_SIZE (sizeof(u_short) + 2 * sizeof(u_char) + sizeof(time_t))
#define MIN_JRNREC      20                              // min. JRN records

typedef struct __PACKED__ JRNREC			// journal record
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
#define JRN_SYNC        6                               // sync VOL
#define JRN_BACKUP	7				// backup VOL

// Note: The first 4 bytes (u_int) = (MUMPS_MAGIC - 1).
//	 The next 8 bytes (off_t) in the file point at the next free byte.
//	 (Initially 16+sizeof(time_t) and always rounded to the next 4 
//        byte boundary).
//	 Journal file is only accessed while a write lock is held.
//	 Protection on the file is changed to g:rw by init_start (TODO).

// **** External declarations ************************************************
// Defined in database/db_main.c

extern u_int64 txid;
extern int curr_locks[MAX_VOL+1];		        // GLOBAL locks
#define curr_lock       (curr_locks[volnum])
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

extern int gbd_local_state;				// local buffering
							// DISABLED/ENABLED/FILL

//**** Function Prototypes*****************************************************

// File: database/db_buffer.c
short GetBlock(u_int blknum,const char *file,int line);	// Get block, chk valid
short GetBlockRaw(u_int blknum,const char *file,int line);// Get block, raw
#define Get_block(u)    GetBlock(u,__FILE__,__LINE__)
#define Get_block_raw(u)    GetBlockRaw(u,__FILE__,__LINE__)
short New_block();					// get new block
void Get_GBD();				                // get a GBD
void Get_GBDs(int greqd);				// get n free GBDs
void Get_GBDsEx(int greqd, int haslock);		// get n free GBDs
void Free_GBD(int vol, gbd *free);			// Free a GBD
void Release_GBDs(int stopat);                          // release rsvd blk[]

#ifdef MV1_BLKSEM

short Block_TryReadLock(gbd *blk);
short Block_TryWriteLock(gbd *blk);
void  Block_Unlock(void);

#define BLOCK_UNLOCK(x)         Block_Unlock()
#define BLOCK_TRYREADLOCK(x)    Block_TryReadLock(x)
#define BLOCK_TRYWRITELOCK(x)   Block_TryWriteLock(x)

#else

#define BLOCK_UNLOCK(x)
#define BLOCK_TRYREADLOCK(x)    0
#define BLOCK_TRYWRITELOCK(x)   0

#endif
// File: database/db_get.c
#define TIPIDX_OFFS	10
#define TIPIDX(x)	(TIPIDX_OFFS + (x))
short Get_data(int dir);				// get db_var node
short Get_data_ex(int dir, int indexTip);		// get db_var node

// File: database/db_kill.c
short Kill_data();					// remove tree
short Kill_data_ex(int what);				// remove tree,selective

// File: database/db_locbuf.c
void LB_Init(void);					// init local buffers
void LB_CleanUp(void);					// release local buffers
void LB_Clear(void);					// clear local buffers
gbd *LB_GetBlock(u_int blknum);				// get local block
void LB_AddBlock(gbd *ptr);				// add GBD to local buf.

// File: database/db_locate.c
short Locate(u_char *key);				// find key
short LocateEx(u_char *key, int frominsert, int indexTip);//   used in Insert()
short Locate_next(u_char *out);				// point at next key
u_char* Build_KeyBuf(int pIndex, u_char *pKeyBuf, int doCopy);
							// pKeyBuf for pIndex
#define KEY_NOCOPY 0
#define KEY_COPY   1
u_short FindChunk(u_short from,u_char pfxlen);          // find less than pfxlen
u_short FindChunk0(u_short from);                       // find zero length
                                        // chunk backwards, not including from

// File: database/db_rekey.c
short Add_rekey(u_int block, int level);		// add to re-key table
short Re_key();						// re-key blocks
void Un_key();						// un-key blk[level]

// File: database/db_set.c
short Set_data(cstring *data, int has_wrlock);          // set a record

// File: database/db_util.c
void Allign_record();					// allign record (int)
void Copy_data(gbd *fptr, int fidx);			// copy records
void DoJournal(jrnrec *jj, cstring *data); 		// Write journal
void Free_block(int vol, int blknum);			// free blk in map
void Mark_map_dirty(int vol, int blknum);		// mark map dirty
void Garbit(int blknum);				// que a blk for garb
short Insert(u_char *key, cstring *data);		// insert a node
int Queit2(gbd *p_gbd);				        // que a gbd for write
void Queit(void);				// que blk[level] for write
void Tidy_block(void);					// tidy current blk
void Used_block(int vol, int blknum);			// set blk in map
short Compress1();					// compress 1 block
void Ensure_GBDs(int haslock);                          // wait for GBDs
short Check_BlockNo(int vol,u_int blkno,int checks,     // check blkno
           char *where,const char *file,int lno,int dopanic);
#define CBN_INRANGE     1
#define CBN_ALLOCATED   2
int  DirtyQ_Len();                                      // length of dirtyQ

void TX_Set(gbd *ptr);
void TX_Next(void);
#define TXSET(x)	TX_Set(x)
#define TX_NEXT		TX_Next()

void LocateAllP(gbd *ptr,int level,const char *path,int line);
void LocateCountP(gbd *ptr,u_char *key,const char *path,int line);

#ifdef MV1_LOCATE_DEBUG
#define LOCATE_ALL_PTR(ptr,lvl)	LocateAllP(ptr,lvl,__FILE__,__LINE__)
#define LOCATE_ALL		LOCATE_ALL_PTR(blk[level],level)
#define LOCATE_COUNT(key)	LocateCountP(blk[level],key,__FILE__,__LINE__)
#else
#define LOCATE_ALL
#define LOCATE_COUNT(key)
#endif

//*****************************************************************************

#endif							// !_MUMPS_DATABASE_H_
