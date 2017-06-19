// File: mumps/database/db_buffer.c
//
// module database - Buffer Management Database Functions

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


#include <stdio.h>					// always include
#include <stdlib.h>					// these two
#include <string.h>					// for bcopy
#include <strings.h>
#include <unistd.h>					// for file reading
#include <time.h>					// for gbd stuff
#include <ctype.h>					// for gbd stuff
#include <errno.h>                      		// error stuf
#include <fcntl.h>					// for open()
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include "mumps.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings
#include "rwlock.h"                                     // for barrier

//typedef struct GBD                              	// global buf desciptor
//{ int block;                                    	// block number
//  struct GBD *next;                             	// next entry in list
//  struct DB_BLOCK *mem;                         	// memory addr of block
//  struct GBD *dirty;                            	// to write -> next
//  time_t last_accessed;                         	// last time used
//} gbd;                                          	// end gbd struct
//
// This is a table of the meanings for the following arrangements of member
// contents:
//                     ---------------------------------------
//                     |  block  |  *dirty   |  last_access  |
//                     ---------------------------------------
// GBD is on freelist      0         NULL            0	(or should be)
//
// GBD is being read      SET        NULL            0
//  in from disk.
//
// GBD been garbaged,     SET        SET             0
//  but is on a dirty list.
//
// Note: if dirty pointer is less than (gbd *)3 then this dirty pointer has
// been set by a function to reserve this gbd.
//
// Note also that every GBD should always have its "mem" member set, not NULL.
//
// SEM_GBD is used for reading/writing GBDs and the hash table
//

#ifdef MV1_REFD

#define Unlink_GBD(x)   UnlinkGBD(x,__FILE__,__LINE__)

void UnlinkGBD(gbd *oldptr,                             // unlink a GBD
        const char *caller_path, int caller_line)
{ int hash;
  gbd *ptr;

  hash = oldptr->hash;                                  // the chain
  // fprintf(stderr,"Unlink_GBD(): hash=%d\r\n",hash); fflush(stderr);
  ASSERT2((0 <= hash) && (hash < GBD_HASH));
  ptr = systab->vol[volnum-1]->gbd_hash[hash];		// get the list
  if (ptr == oldptr)					// is this it
  { systab->vol[volnum-1]->gbd_hash[hash] = oldptr->next;// unlink it
    ptr = 0;                                            // removed
  }
  else							// inside the chain
  { oldptr->prev->next = oldptr->next;                  // nxt of prev is my nxt
    ptr = oldptr->prev;                                 // point to prev
  }
  if (oldptr->next)                                     // if not last
    oldptr->next->prev = ptr;                           //   point to head/prev

  oldptr->prev = NULL;                                  // clear chain ptrs
  oldptr->next = NULL;
  oldptr->hash = -1;                                    // out of hash chains
}


void Link_GBD(u_int blknum, gbd *newptr)                // lnk gbd in hash chain
{ int hash;
  gbd *ptr;

  hash = GBD_BUCKET(blknum);                            // calc chain no
  ptr = systab->vol[volnum-1]->gbd_hash[hash];          // get head
  newptr->hash = hash;                                  // store chain no
  newptr->prev = NULL;                                  // no prev
  newptr->next = ptr;                                   // next is old head
  if (ptr)                                              // not empty chain ?
    ptr->prev = newptr;                                 // set prev
  systab->vol[volnum-1]->gbd_hash[hash] = newptr;       // set as new head
}

#endif

//-----------------------------------------------------------------------------
// Function: Get_block
// Descript: Get specified block into blk[level] - get GBD first
//	     The gbd found is returned in blk[level]
//	     ->dirty is set to (gbd *) 1 if (writing)
//	     ->last_accessed is set to the current time
//	     The block pointers idx & iidx are setup, Index is set to LOW_INDEX
// Input(s): Block number to get
// Return:   0 -> Ok, negative MUMPS error
//

gbd *ro_gbd = 0;

#ifdef MV1_GBDRO
gbd* Get_GBDRO()
{
  gbd *ret = 0;
  void *qentry;
  bool result;

  result = ck_ring_dequeue_spmc(
                &systab->vol[volnum-1]->rogbdQ,
                &systab->vol[volnum-1]->rogbdQBuffer[0],
                &qentry);
  if (result)
  { ret = (gbd *) qentry;
  }
  return ret;
}

void Free_GBDROs(gbd **ptrs, int nptrs)
{
  int  i;
  bool result;

  SemOp(SEM_GBDRO, WRITE);
  for (i = 0; i < nptrs; i++)
  { result = ck_ring_enqueue_spmc(
                &systab->vol[volnum-1]->rogbdQ,
                &systab->vol[volnum-1]->rogbdQBuffer[0],
                ptrs[i]);
    if (!result)
    { SemOp(SEM_GBDRO, -WRITE);
      panic("cannot release ROGBD!!");
    }
  }
  SemOp(SEM_GBDRO, -WRITE);
}

void Free_GBDRO(gbd *ptr)
{
  bool result;

  SemOp(SEM_GBDRO, WRITE);
  result = ck_ring_enqueue_spmc(
                &systab->vol[volnum-1]->rogbdQ,
                &systab->vol[volnum-1]->rogbdQBuffer[0],
                ptr);
  if (!result)
  { SemOp(SEM_GBDRO, -WRITE);
    panic("cannot release ROGBD!!");
  }
  SemOp(SEM_GBDRO, -WRITE);
}
#endif

gbd *Get_RdGBD();				        // proto

gbd *locked_blk = 0;                                    // locked block

#ifdef MV1_BLKSEM

short Block_TryReadLock(gbd *blk)
{ int semidx;                                           // block sema index
  short oldlock;                                        // old value of blk lock

  ASSERT(0 == locked_blk);

  semidx = blk->block & (BLKSEM_MAX - 1);               // calc. sem to lock
  LatchLock(&systab->blksem[semidx]);                   // acquire block
  oldlock = blk->curr_lock;                             // get current lock
  if (oldlock < 0)                                      // signal error if
  { LatchUnlock(&systab->blksem[semidx]);               //   negative
    ASSERT(0 <= oldlock);
  }
  if (BLK_WRITE == oldlock)                             // exclusive lock ?
  { LatchUnlock(&systab->blksem[semidx]);               // release block
    return -1;                                          // failed
  }
  blk->curr_lock++;                                     // flag as reading
  LatchUnlock(&systab->blksem[semidx]);                 // release block
  
  locked_blk = blk;                                     // remember it

  return 0;
}

short Block_TryWriteLock(gbd *blk)
{ int semidx;                                           // block sema index
  short oldlock;                                        // old value of blk lock

  ASSERT(0 == locked_blk);

  semidx = blk->block & (BLKSEM_MAX - 1);               // calc. sem to lock
  LatchLock(&systab->blksem[semidx]);                   // acquire block
  oldlock = blk->curr_lock;                             // get current lock
  if (oldlock < 0)                                      // signal error if
  { LatchUnlock(&systab->blksem[semidx]);               //   negative
    ASSERT(0 <= oldlock);
  }
  if (oldlock)                                          // already locked ?
  { LatchUnlock(&systab->blksem[semidx]);               // release block
    return -1;                                          // failed
  }
  blk->curr_lock = BLK_WRITE;                           // flag as writing
  LatchUnlock(&systab->blksem[semidx]);                 // release block

  locked_blk = blk;                                     // remember it

  return 0;
}

void Block_Unlock(void)
{ int semidx;                                           // block sema index
  short oldlock;                                        // old value of blk lock

  ASSERT(0 != locked_blk);                              // should locked

  semidx = locked_blk->block  & (BLKSEM_MAX - 1);       // calc. sem to lock
  LatchLock(&systab->blksem[semidx]);                   // acquire block
  oldlock = locked_blk->curr_lock;                      // get current lock
  if (oldlock < 0)                                      // signal error
  { LatchUnlock(&systab->blksem[semidx]);               //   if negative
    ASSERT(0 <= oldlock);
  }
  if (BLK_WRITE == oldlock)                             // we had a write lock
    locked_blk->curr_lock = 0;                          //   release it
  else
    locked_blk->curr_lock--;                            // else decr. read count
  LatchUnlock(&systab->blksem[semidx]);                 // release block

  locked_blk = 0;                                       // forget it
}

#endif

void DB_Unlocked(void)
{
#ifdef MV1_GBDRO
  if (ro_gbd)
  { Free_GBDRO(ro_gbd);
    ro_gbd = 0;
  }
#endif
#ifdef MV1_BLKSEM
  if (locked_blk)
  { if (!writing)
    { BLOCK_UNLOCK(locked_blk);
    }
  }
#endif
}


void Release_GBDs(int stopat)
{ if (0 == stopat)
  { while (level >= 0)                                  // for each
    { if (blk[level])
      { if (blk[level]->dirty == (gbd *) 1)             // if we reserved it
        { blk[level]->dirty = NULL;                     // clear that
        }
      }
      level--;                                          // previous
    }
    // level = 0;
  }
  else
  { while (level)                                       // for each
    { if (blk[level])
      { if (blk[level]->dirty == (gbd *) 1)             // if we reserved it
        { blk[level]->dirty = NULL;                     // clear that
        }
      }
      level--;                                          // previous
    }
  }
}


short GetBlock(u_int blknum,char *file,int line)        // Get block
{ int i;						// a handy int
  short s = -1;						// for functions
  off_t file_off;					// for lseek()
  gbd *ptr;						// a handy pointer
  int  refd_inited = 0;                                 // reference initialized

  // fprintf(stderr,"GetBlock(%u) called from %s:%d\r\n", blknum, file, line);
  Check_BlockNo(blknum, "Get_block", file, line);       // check blknum validity

  if (!writing)                                         // a reader
  { if (locked_blk)                                     //  if locked a block
    { BLOCK_UNLOCK(locked_blk);                         //  unlock it
    }
  }

  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.logrd); // update stats
  ptr = systab->vol[volnum-1]->gbd_hash[GBD_BUCKET(blknum)]; // get head
  while (ptr != NULL)					// for entire list
  { if (ptr->block == blknum)				// found it?
    { blk[level] = ptr;					// save the ptr
      UTIL_Barrier();
      while (ptr->last_accessed == (time_t) 0)	        // if being read
      { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.rdwait);
        SchedYield();					// wait for it
        UTIL_Barrier();
      }
      goto exit;					// go common exit code
    }
    ptr = ptr->next;					// point at next
  }							// end memory search

  if (!writing)						// if read mode
  { 
#ifdef MV1_GBDRO
    if (!wanna_writing)                                 // not pre-write mode ?
    { SemOp( SEM_GBDRO, WRITE);
      ptr = Get_RdGBD();                                // try to get a free GBD
      if (ptr)                                          //   if succeeded, done
      { // fprintf(stderr,"Get_FreeGBD(): success\r\n");
        systab->vol[volnum-1]->stats.phyrd++;           // update stats
        blk[level]->block = blknum;			// set block number
#ifdef MV1_REFD
        REFD_READ_INIT(blk[level]);
        refd_inited = 1;
#endif
        blk[level]->last_accessed = (time_t) 0;		// clear last access
        UTIL_Barrier();
#ifdef MV1_REFD
        Link_GBD(blknum, blk[level]);
#else
        i = GBD_BUCKET(blknum);
        blk[level]->next = systab->vol[volnum-1]->gbd_hash[i];// link it in
        systab->vol[volnum-1]->gbd_hash[i] = blk[level];	//
#endif
        SemOp( SEM_GBDRO, -WRITE);
        goto unlocked;
      }
      SemOp( SEM_GBDRO, -WRITE);
      if (!ro_gbd)                                      // if we have no R/O GBD
      { // fprintf(stderr,"--- before Get_GBDRO() ---\r\n");
        ro_gbd = Get_GBDRO();                           // try to get one
        if (!ro_gbd)                                    // if failed, do a
        { // fprintf(stderr, "Get_GBDRO(): failed\r\n");
          goto writelock;                               //   write lock
        }
      }
      // fprintf(stderr,"--- use ro_gbd ---\r\n");
      ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.eventcnt); // update stats
      blk[level] = ro_gbd;                              // use the R/O GBD
      systab->vol[volnum-1]->stats.phyrd++;             // update stats
      blk[level]->block = blknum;			// set block number
#ifdef MV1_REFD
      REFD_READ_INIT(blk[level]);
      refd_inited = 1;
#endif
      blk[level]->last_accessed = (time_t) 0;		// clear last access
      UTIL_Barrier();
      goto unlocked;
    }
#endif
writelock:
    SemOp( SEM_GLOBAL, -curr_lock);			// release read lock
    s = SemOp( SEM_GLOBAL, WRITE);			// get write lock
    if (s < 0)						// on error
    { return s;						// return it
    }
    ptr = systab->vol[volnum-1]->gbd_hash[GBD_BUCKET(blknum)]; // get head
    while (ptr != NULL)					// for entire list
    { if (ptr->block == blknum)				// found it?
      { blk[level] = ptr;				// save the ptr
	SemOp( SEM_GLOBAL, WR_TO_R);			// drop to read lock
        UTIL_Barrier();
        while (ptr->last_accessed == (time_t) 0)	// if being read
        { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.rdwait);
          SchedYield();					// wait for it
          UTIL_Barrier();
        }
        goto exit;					// go common exit code
      }
      ptr = ptr->next;					// point at next
    }							// end memory search
  }							// now have a write lck
  systab->vol[volnum-1]->stats.phyrd++;                 // update stats
  Get_GBD();					        // get a GBD
  blk[level]->block = blknum;				// set block number
#ifdef MV1_REFD
  REFD_READ_INIT(blk[level]);
  refd_inited = 1;
#endif
  blk[level]->last_accessed = (time_t) 0;		// clear last access
  UTIL_Barrier();
#ifdef MV1_REFD
  Link_GBD(blknum, blk[level]);
#else
  i = GBD_BUCKET(blknum);
  blk[level]->next = systab->vol[volnum-1]->gbd_hash[i];// link it in
  systab->vol[volnum-1]->gbd_hash[i] = blk[level];	//
#endif
  if (!writing)						// if reading
  { SemOp( SEM_GLOBAL, WR_TO_R);			// drop to read lock
  }
unlocked:
  file_off = (off_t) blknum - 1;			// block#
  file_off = (file_off * (off_t) systab->vol[volnum-1]->vollab->block_size)
           + (off_t) systab->vol[volnum-1]->vollab->header_bytes;
  if ( ( volnum-1 ) > 0) 
  { if ( volnum > MAX_VOL ) return -(ERRZ72+ERRMLAST);	// Must be in range
    if (partab.vol_fds[volnum-1] == 0)			// if not open
    { if (systab->vol[volnum-1]->file_name[0] == 0)
	return -(ERRZ72+ERRMLAST); 			// need a filename
      i = open( systab->vol[volnum-1]->file_name, O_RDONLY); // Open the volume
      if (i < 0) return -(errno+ERRMLAST+ERRZLAST);	// Give up on error
      partab.vol_fds[volnum-1] = i;                   	// make sure fd right
    }
    else						// check still there
    { if (systab->vol[volnum-1]->file_name[0] == 0)
      { i = close( partab.vol_fds[volnum-1] );		// close the file
	partab.vol_fds[volnum-1] = 0;			// flag not there
	return -(ERRZ72+ERRMLAST);			// exit complaining
      }
    }
  }
  file_off = lseek( partab.vol_fds[volnum - 1],
		    file_off, SEEK_SET);		// Seek to blk
  if (file_off < 1)					// if that failed
  { panic("Get_block: lseek() failed!");		// die
  }
  i = read(partab.vol_fds[volnum-1], blk[level]->mem,
	   systab->vol[volnum-1]->vollab->block_size);
  if (i < 0)						// if read failed
  { panic("Get_block: read() failed!");			// die
  }
  REFD_TYPED_INIT(blk[level]);
exit:
  if ((writing) && (blk[level]->dirty < (gbd *) 5))	// if writing
  { blk[level]->dirty = (gbd *) 1;			// reserve it
  }
  if (!writing)                                         // if reading
  { while (BLOCK_TRYREADLOCK(blk[level]) < 0)           //   wait for read lock
    { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.brdwait); // count a wait
      SchedYield();                                     //   release quant
    }
  }
  blk[level]->last_accessed = MTIME(0);			// set access time
#ifdef MV1_REFD
  if (!refd_inited)
  { REFD_MARK(blk[level]);
  }
#endif
  UTIL_Barrier();
  Index = LOW_INDEX;					// first one
  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  return 0;						// return success
}

//-----------------------------------------------------------------------------
// Function: New_block
// Descript: Get new block into blk[level] - get GBD first
//	     The gbd found is returned in blk[level]
//	     ->dirty is set to (gbd *) -1
//	     ->last_accessed is set to the current time
//	     The entire block is zeroed
//	     The block pointers idx & iidx are setup, Index is set to LOW_INDEX
// Input(s): none
// Return:   0 -> Ok, negative MUMPS error
// Note:     curr_lock MUST be WRITE when calling this function
//
short New_block()					// get new block
{ int i;						// a handy int
  u_int blknum;						// the block#
  u_char *c;						// character ptr
  u_char *end;						// end of map
  int hash;

// NEED TO ADD A CHECK FOR A DIRTY MAP SCAN IN PROGRESS HERE

  Get_GBD();						// get a GBD

  Index = LOW_INDEX;					// first one
  c = (u_char *)systab->vol[volnum-1]->first_free;	// look at first_free  
  end = ((u_char *) systab->vol[volnum-1]->map)		// start
	+ (systab->vol[volnum-1]->vollab->max_block / 8); // plus bits
  while (c <= end)					// scan map
  { if (*c != 255)					// is there space
    { blknum = (c - ((u_char *) systab->vol[volnum-1]->map)) * 8; // base number
      for (i = 0; ((1 << i) & *c); i++);		// find first free bit
      blknum = blknum + i;				// add the little bit
      if (blknum <= systab->vol[volnum-1]->vollab->max_block)
      { *c |= (1 << i);					// mark block as used
        systab->vol[volnum-1]->map_dirty_flag++;	// mark map dirty
        blk[level]->block = blknum;			// save in structure
	blk[level]->dirty = (gbd *) 1;			// reserve it
	blk[level]->last_accessed = MTIME(0);		// accessed
#ifdef MV1_REFD
        REFD_NEW_INIT(blk[level]);                      // mark referenced
        Link_GBD(blknum, blk[level]);
#else
        hash = GBD_BUCKET(blknum);
        blk[level]->next                                // link it in
          = systab->vol[volnum-1]->gbd_hash[hash];
	systab->vol[volnum-1]->gbd_hash[hash] = blk[level];
#endif
	bzero(blk[level]->mem, systab->vol[volnum-1]->vollab->block_size);
        UTIL_Barrier();
	systab->vol[volnum-1]->first_free = c;		// save this
        ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.blkalloc); // update stats
	return 0;					// return success
      }
    }
    c++;						// point at next
  }							// end map scan
  Free_GBD(blk[level]);					// give it back
  return -(ERRMLAST + ERRZ11);				// error - no room
}

//-----------------------------------------------------------------------------
// Function: Get_GBDs
// Descript: Ensure there are n available GDBs
// Input(s): number of GBDs required
// Return:   none
// Note:     No lock is held when calling this function.
//	     When it completes, it returns with a write lock held.
//

#define GBD_SLEEP               (10)
#define GBD_TRIES_PER_SEC	(4 * (1000 / GBD_SLEEP))
#define GBD_TRIES		(60 * GBD_TRIES_PER_SEC)

void Get_GBDs(int greqd)				// get n free GBDs
{ return Get_GBDsEx(greqd, 0);
}

//-----------------------------------------------------------------------------
// Function: Get_GBDs
// Descript: Ensure there are n available GDBs
// Input(s): number of GBDs required
// Return:   none
// Note:     No lock is held when calling this function.
//	     When it completes, it returns with a write lock held.
//

void Get_GBDsEx(int greqd, int haslock)			// get n free GBDs
{ int i, j;						// a handy int
  int curr;						// current count
  gbd *ptr;						// and pointer
  time_t now;						// current time
  int pass = 0;						// pass number
  int num_gbd = systab->vol[volnum-1]->num_gbd;         // local var

start:
  if (!haslock)
    while (SemOp(SEM_GLOBAL, WRITE));                   // get write lock
  haslock = 0;                                          // clear for next turns
  curr = 0;						// clear current  
  ptr = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// head of free list
  while (ptr != NULL)					// while some there
  { curr++;						// count it
    if (curr >= greqd)					// if enough there
    { return;						// just exit
    }
    ptr = ptr->next;					// point at next
  }							// end while

  now = MTIME(0) + 1;				        // get current time

  // fprintf(stderr,"Get_GBDs(): search begin\r\n"); fflush(stderr);

  i = (systab->hash_start + 1) % num_gbd;	        // where to start
  for (j = 0; j < num_gbd; j++, i = (i + 1) % num_gbd)
  { ptr = &systab->vol[volnum-1]->gbd_head[i];
    if ((GBD_HASH == ptr->hash) ||                      // skip GBDs on free lst
        (NULL     != ptr->dirty))                       //   or dirty
      continue;                                         
    if (0 == ptr->block) 				// if no block
    { // fprintf(stderr,"Get_GBDs(): block == 0\r\n"); fflush(stderr);
      // fprintf(stderr,"Get_GBDs(): before Unlink_GBD\r\n"); fflush(stderr);
      Unlink_GBD(ptr);                                  // unlink ptr
      // fprintf(stderr,"Get_GBDs(): after Unlink_GBD\r\n"); fflush(stderr);
      ptr->prev = NULL;                                 // no dbllnk in free lst
      ptr->next = systab->vol[volnum-1]->gbd_hash [GBD_HASH]; // hook to free
      ptr->hash = GBD_HASH;
      systab->vol[volnum-1]->gbd_hash [GBD_HASH] = ptr; // and this
      ptr->dirty = NULL;				// ensure clear
      ptr->last_accessed = (time_t) 0;		        // ensure no time
      REFD_CLEAR(ptr);                                  // not refd
      curr++;						// count this
      if (curr >= greqd)				// if enough there
        return;					        // just exit
      continue;					        // next ptr
    }							// end - no block
    if ((now > ptr->last_accessed) &&                   // if not viewed
        (0   < ptr->last_accessed))			// and there is a time
    { curr++;					        // count that
      if (curr >= greqd)				// if enough there
          return;					// just exit
    }
  }

  SemOp(SEM_GLOBAL, -curr_lock);			// release our lock
  if (pass & 3)
    SchedYield();                                       // yield
  else
  { ATOMIC_INCREMENT(systab->vol[volnum - 1]->stats.gbswait);// incr. GBD wait
    MSleep(GBD_SLEEP);                                  // wait
  }
  pass++;						// increment a pass
  if (pass > GBD_TRIES)					// this is crazy!
  { panic("Get_GBDs: Can't get enough GBDs after 60 seconds");
  }
  // fprintf(stderr,"Get_GBDs(): goto start\r\n"); fflush(stderr);
  goto start;						// try again
}

//-----------------------------------------------------------------------------
// Function: Get_GBD
// Descript: Get a GBD into blk[level]
//	     ->block, next, dirty and last_accessed are cleared
//	     The block pointers idx & iidx are setup
//	     The block is NOT zeroed
// Input(s): none
// Return:   none
// Note:     curr_lock MUST be WRITE when calling this function
//

void Get_GBD()						// get a GBD
{ int i, j;						// a handy int
  time_t now;						// current time
  gbd *ptr;						// loop gbd ptr
  gbd *oldptr = NULL;					// remember last unrefd
  int pass;
  int clean;                                            // flag clean blk
  int num_gbd = systab->vol[volnum-1]->num_gbd;         // local var
  int oldpos;
  u_int oldval;

  pass   = 0;
  oldval = (u_int) -1;
start:
  oldptr = NULL;
  if (systab->vol[volnum-1]->gbd_hash [GBD_HASH])	// any free?
  { blk[level]
      = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// get one
    systab->vol[volnum-1]->gbd_hash [GBD_HASH]
      = blk[level]->next;				// unlink it
    // fprintf(stderr,"Get_GBD(): from free list\r\n"); fflush(stderr);
    goto exit;						// common exit code
  }

  now = MTIME(0) + 1;				        // get current time

  i = (systab->hash_start + 1) % num_gbd;		// where to start
  for (j = 0; j < num_gbd; j++)                         // for each GBD
  { ptr = &systab->vol[volnum-1]->gbd_head[i];
    if ((0 == ptr->block) && 				// no block ?
        (NULL == ptr->dirty))                           //   and not dirty ?
    { oldptr = ptr;                                     // mark this
      oldpos = i;				        // remember this
      // fprintf(stderr,"Get_GBD(): from block == 0/unreferenced clean\r\n");
      // fflush(stderr);
      goto unlink_gbd;				        // common exit code
    }							// end found expired
    clean = (NULL == ptr->dirty) &&                     // not dirty
            (now > ptr->last_accessed) &&               //   and not viewed
            (0   < ptr->last_accessed);                 //   and not being read
    if (clean)
    { if (REFD_VALUE(ptr))
      { REFD_UNMARK(ptr);
        if (REFD_VALUE(ptr) < oldval)
        { oldptr = ptr;
          oldpos = i;
          oldval = REFD_VALUE(ptr);
        }
      }
      else
      { oldptr = ptr;
        oldpos = i;
        goto unlink_gbd;
      }
    }
    i = (i + 1) % num_gbd;			        // next GBD entry
  }							// end for every GBD
  if (NULL == oldptr)
  { if (writing)				        // SET or KILL
    { panic("Get_GBD: Failed to find an available GBD while writing"); // die
    }

    systab->vol[volnum - 1]->stats.gbwait++;            // incr. GBD wait
    SemOp(SEM_GLOBAL, -curr_lock);			// release current
    if (pass & 3)
      SchedYield();                                     // yield
    else
      MSleep(GBD_SLEEP);                                // wait
    pass++;
    if (pass > GBD_TRIES)				// this is crazy!
    { panic("Get_GBD: Can't get a GDB after 60 seconds");
    }
    while (SemOp(SEM_GLOBAL, WRITE));			// re-get lock
    goto start;						// and try again
  }

  // NB.
  // - oldval lehet nulla, ha eppen 1-rol csokkentettuk
  // - mindenkit csokkenteni kell (oldval-1)-el
  if (oldval && --oldval)
  { for (i = 0; i < num_gbd; i++)                       // for each GBD
    { ptr = &systab->vol[volnum-1]->gbd_head[i];
      if ((0 == ptr->block) && 				// no block ?
          (NULL == ptr->dirty))                         //   and not dirty ?
        continue;
      clean = (NULL == ptr->dirty) &&                   // not dirty
              (now > ptr->last_accessed) &&             //   and not viewed
              (0   < ptr->last_accessed);               //   and not being read
      if (clean)
      { if (REFD_VALUE(ptr))
        { REFD_DEC(ptr,oldval);
        }
      }
    }
  }							// end for every GBD
unlink_gbd:
  systab->hash_start = oldpos;

  // fprintf(stderr,"Get_GBD(): before Unlink_GBD\r\n"); fflush(stderr);
  Unlink_GBD(oldptr);                                   // unlink oldptr
  // fprintf(stderr,"Get_GBD(): after Unlink_GBD\r\n"); fflush(stderr);
  blk[level] = oldptr;					// store where reqd

exit:
  // fprintf(stderr,"Get_GBD(): exit\r\n"); fflush(stderr);
  blk[level]->block = 0;			        // no block attached
  blk[level]->next = NULL;				// clear link
  blk[level]->dirty = writing ? (gbd *) 1 : NULL;       // clear dirty XXX
  blk[level]->last_accessed = (time_t) 0;		// and time
  REFD_READ_INIT(blk[level]);                           // mark refd
  blk[level]->prev = NULL;                              // clear prev link
  blk[level]->hash = -1;                                // clear hash chain
  UTIL_Barrier();
  idx = (u_short *) blk[level]->mem;			// set this up
  iidx = (int *) blk[level]->mem;			// and this
  return;						// return
}

gbd *Get_RdGBD()				        // get a GBD
{ int i, j;						// a handy int
  time_t now;						// current time
  gbd *ptr;						// loop gbd ptr
  gbd *oldptr = NULL;					// remember last unrefd
  int pass;
  int clean;                                            // flag clean blk
  int num_gbd = systab->vol[volnum-1]->num_gbd;         // local var
  int oldpos;

  oldptr = NULL;
  if (systab->vol[volnum-1]->gbd_hash [GBD_HASH])	// any free?
  { blk[level]
      = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// get one
    systab->vol[volnum-1]->gbd_hash [GBD_HASH]
      = blk[level]->next;				// unlink it
    // fprintf(stderr,"Get_GBD(): from free list\r\n"); fflush(stderr);
    goto exit;						// common exit code
  }

  now = MTIME(0) + 1;				        // get current time

  i = (systab->hash_start + 1) % num_gbd;		// where to start
  for (j = 0; j < num_gbd; j++)                         // for each GBD
  { ptr = &systab->vol[volnum-1]->gbd_head[i];
    if ((0 == ptr->block) && 				// no block ?
        (NULL == ptr->dirty))                           //   and not dirty ?
    { oldptr = ptr;                                     // mark this
      oldpos = i;				        // remember this
      // fprintf(stderr,"Get_GBD(): from block == 0/unreferenced clean\r\n");
      // fflush(stderr);
      goto unlink_gbd;				        // common exit code
    }							// end found expired
    clean = (NULL == ptr->dirty) &&                     // not dirty
            (now > ptr->last_accessed) &&               //   and not viewed
            (0   < ptr->last_accessed);                 //   and not being read
    if (clean)
    { if (REFD_VALUE(ptr))
      { REFD_UNMARK(ptr);
        if ((0 == REFD_VALUE(ptr)) && (NULL == oldptr))
        { oldptr = ptr;
          oldpos = i;
        }
      }
      else
      { oldptr = ptr;
        oldpos = i;
        goto unlink_gbd;
      }
    }
    i = (i + 1) % num_gbd;			        // next GBD entry
  }							// end for every GBD
  if (NULL == oldptr)
    return 0;

unlink_gbd:
  systab->hash_start = oldpos;

  // fprintf(stderr,"Get_GBD(): before Unlink_GBD\r\n"); fflush(stderr);
  Unlink_GBD(oldptr);                                   // unlink oldptr
  // fprintf(stderr,"Get_GBD(): after Unlink_GBD\r\n"); fflush(stderr);
  blk[level] = oldptr;					// store where reqd

exit:
  // fprintf(stderr,"Get_GBD(): exit\r\n"); fflush(stderr);
  blk[level]->block = 0;				// no block attached
  blk[level]->next = NULL;				// clear link
  blk[level]->dirty = NULL;				// clear dirty
  blk[level]->last_accessed = (time_t) 0;		// and time
  REFD_READ_INIT(blk[level]);                           // mark refd
  blk[level]->prev = NULL;                              // clear prev link
  blk[level]->hash = -1;                                // clear hash chain
  UTIL_Barrier();
  idx = (u_short *) blk[level]->mem;			// set this up
  iidx = (int *) blk[level]->mem;			// and this
  return blk[level];						// return
}

//-----------------------------------------------------------------------------
// Function: Free_GBD
// Descript: Free specified GBD (if ->block non-zero, remove from hash table)
// Input(s): GDB pointer
// Return:   none
// Note:     curr_lock MUST be WRITE when calling this function
//
void Free_GBD(gbd *free)				// Free a GBD
{ gbd *ptr;						// a handy pointer

  if (free->block)					// if there is a blk#
  { 
#ifdef MV1_CACHE
    ptr = systab->vol[volnum-1]->gbd_hash[GBD_BUCKET(free->block)];
    if (ptr == free)					// if this one
    { systab->vol[volnum-1]->gbd_hash[GBD_BUCKET(free->block)]
        = free->next;					// unlink it
    }
    else						// look for it
    { while (ptr->next != free)				// til found
      { ptr = ptr->next;				// get next
      }
      ptr->next = free->next;				// unlink it
    }
#else
    Unlink_GBD(free);                                   // unlink GBD ptr
#endif
  }

#ifdef MV1_REFD
  free->prev = NULL;
  REFD_CLEAR(free);
  free->hash = GBD_HASH;
#endif
  free->next = systab->vol[volnum-1]->gbd_hash[GBD_HASH]; // get free list
  systab->vol[volnum-1]->gbd_hash[GBD_HASH] = free; 	// link it in
  free->block = 0;					// clear this
  free->dirty = NULL;					// and this
  free->last_accessed = (time_t) 0;			// and this
  UTIL_Barrier();
  return;						// and exit
}

