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
#include <assert.h>                                     // for assert()
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

short Get_block(u_int blknum)                           // Get block
{ int i;						// a handy int
  short s = -1;						// for functions
  off_t file_off;					// for lseek()
  gbd *ptr;						// a handy pointer

  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.logrd); // update stats
  ptr = systab->vol[volnum-1]->gbd_hash[blknum & (GBD_HASH - 1)]; // get head
  while (ptr != NULL)					// for entire list
  { if (ptr->block == blknum)				// found it?
    { blk[level] = ptr;					// save the ptr
      UTIL_Barrier();
      while (ptr->last_accessed == (time_t) 0)		// if being read
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
#ifdef MV1_GBDLATCH
    s = LatchLock(&systab->shsem[SEM_GLOBAL]);          // get mutex on GLOBAL
    if (s < 0)                                          // on error
    { return s;                                         // return it
    }
#else
    SemOp( SEM_GLOBAL, -curr_lock);			// release read lock
    s = SemOp( SEM_GLOBAL, WRITE);			// get write lock
    if (s < 0)						// on error
    { return s;						// return it
    }
#endif
    ptr = systab->vol[volnum-1]->gbd_hash[blknum & (GBD_HASH - 1)]; // get head
    while (ptr != NULL)					// for entire list
    { if (ptr->block == blknum)				// found it?
      { blk[level] = ptr;				// save the ptr
#ifdef MV1_GBDLATCH
        LatchUnlock(&systab->shsem[SEM_GLOBAL]);
#else
	SemOp( SEM_GLOBAL, WR_TO_R);			// drop to read lock
#endif
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
  blk[level]->referenced    = 0;
  blk[level]->prev = 0;
#endif
#ifdef MV1_BLKVER
  blk[level]->blkver_high = systab->vol[volnum-1]->stats.phyrd;
  blk[level]->blkver_low  = 0;
#endif
  blk[level]->last_accessed = (time_t) 0;		// clear last access
  UTIL_Barrier();
  i = blknum & (GBD_HASH - 1);				// get hash entry
#ifdef MV1_REFD
  if (systab->vol[volnum-1]->gbd_hash[i])               // if already has entry
    systab->vol[volnum-1]->gbd_hash[i]->prev = blk[level];// link it in: prev
  blk[level]->hash = i;                                 // store chain no
#endif
  blk[level]->next = systab->vol[volnum-1]->gbd_hash[i];// link it in
  systab->vol[volnum-1]->gbd_hash[i] = blk[level];	//
  if (!writing)						// if reading
  { 
#ifdef MV1_GBDLATCH
    LatchUnlock(&systab->shsem[SEM_GLOBAL]);
#else
    SemOp( SEM_GLOBAL, WR_TO_R);			// drop to read lock
#endif
  }
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

exit:
#ifdef MV1_REFD
  blk[level]->referenced = 1;
#endif
  if ((writing) && (blk[level]->dirty < (gbd *) 5))	// if writing
  { blk[level]->dirty = (gbd *) 1;			// reserve it
  }
  blk[level]->last_accessed = MTIME(0);			// set access time
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
        hash = blknum & (GBD_HASH - 1);
        blk[level]->next                                // link it in
          = systab->vol[volnum-1]->gbd_hash[hash];
#ifdef MV1_REFD
        if (systab->vol[volnum-1]->gbd_hash[hash])      // double linked
	  systab->vol[volnum-1]->gbd_hash[hash]->prev = blk[level];
#endif
	systab->vol[volnum-1]->gbd_hash[hash] = blk[level];
	bzero(blk[level]->mem, systab->vol[volnum-1]->vollab->block_size);
	blk[level]->dirty = (gbd *) 1;			// reserve it
#ifdef MV1_BLKVER
        blk[level]->blkver_high = systab->vol[volnum-1]->stats.phyrd;
        blk[level]->blkver_low = 0;
#endif
	blk[level]->last_accessed = MTIME(0);		// accessed
        UTIL_Barrier();
#ifdef MV1_REFD
        blk[level]->referenced = 1;                     // mark referenced
        blk[level]->hash = hash;                        // save hash chain
#endif
	systab->vol[volnum-1]->first_free = c;		// save this
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

#ifdef MV1_CACHE

void Get_GBDsEx(int greqd, int haslock)			// get n free GBDs
{ int i;						// a handy int
  int curr;						// current count
  gbd *ptr;						// and pointer
  gbd *last;						// and another
  time_t now;						// current time
  int pass = 0;						// pass number

start:
  if (!haslock)
    while (SemOp(SEM_GLOBAL, WRITE));                   // get write lock
  haslock = 0;                                          // clear for next turns
  ptr = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// head of free list
  curr = 0;						// clear current  
  while (ptr != NULL)					// while some there
  { curr++;						// count it
    if (curr >= greqd)					// if enough there
    { return;						// just exit
    }
    ptr = ptr->next;					// point at next
  }							// end while
  now = MTIME(0) + 1;					// get current time +

  i = (systab->hash_start + 1) & (GBD_HASH - 1);	// where to start
  while (TRUE)						// loop
  { ptr = systab->vol[volnum-1]->gbd_hash[i];		// get first entry
    last = NULL;					// clear last
    while (ptr != NULL)					// while we have some
    { if (ptr->block == 0)				// if no block
      { if (last == NULL)				// if first one
        { systab->vol[volnum-1]->gbd_hash[i] = ptr->next; // hook it here
        }
        else						// not first one
	{ last->next = ptr->next;			// then hook it here
	}
	ptr->next = systab->vol[volnum-1]->gbd_hash [GBD_HASH]; // hook to free
	systab->vol[volnum-1]->gbd_hash [GBD_HASH] = ptr; // and this
	ptr->dirty = NULL;				// ensure clear
	ptr->last_accessed = (time_t) 0;		// ensure no time
	curr++;						// count this
        if (curr >= greqd)				// if enough there
        { systab->hash_start = i;
          return;					// just exit
        }
	if (last == NULL)				// if first one
	{ ptr = systab->vol[volnum-1]->gbd_hash[i];	// get next in list
	}
	else
	{ ptr = last->next;				// to allow for loop
	}
	continue;					// next ptr
      }							// end - no block
      if ((ptr->dirty == NULL) &&			// if free
	  (ptr->last_accessed < now) &&			// and time expired
	  (ptr->last_accessed > 0))			// and there is a time
      { curr++;						// count that
        if (curr >= greqd)				// if enough there
        { return;					// just exit
        }
      }

      last = ptr;					// remember last
      ptr = ptr->next;					// point at next
    }							// end 1 hash list

    i = (i + 1) & (GBD_HASH - 1);			// next hash entry
    if (i == systab->hash_start)			// where we started
    { break;						// done
    }
  }							// end while (TRUE)
  systab->vol[volnum - 1]->stats.gbwait++;              // incr. GBD wait
  SemOp(SEM_GLOBAL, -curr_lock);			// release our lock
  if (pass & 3)
    SchedYield();                                       // yield
  else
    MSleep(GBD_SLEEP);                                  // wait
  pass++;						// increment a pass
  if (pass > GBD_TRIES)					// this is crazy!
  { panic("Get_GBDs: Can't get enough GDBs after 60 seconds");
  }
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

void Get_GBD()				                // get a GBD
{ int i;						// a handy int
  time_t now;						// current time
  time_t exp;						// expiry time
  time_t old;						// oldest
  int hash = -1;					// for the table
  gbd *ptr;						// loop gbd ptr
  gbd *oldptr = NULL;					// remember oldest
  gbd *last;						// points to ptr
  int pass;
  int clean;                                            // flag clean blk
  short s;

  pass = 0;
start:
  if (systab->vol[volnum-1]->gbd_hash [GBD_HASH])	// any free?
  { blk[level]
      = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// get one
    systab->vol[volnum-1]->gbd_hash [GBD_HASH]
      = blk[level]->next;				// unlink it
    goto exit;						// common exit code
  }

  now = MTIME(0) + 1;					// get current time
  old = now;					        // remember oldest
  exp = now - gbd_expired + 1;				// expired time

  i = (systab->hash_start + 1) & (GBD_HASH - 1);	// where to start
  while (TRUE)						// loop
  { ptr = systab->vol[volnum-1]->gbd_hash[i];		// get first entry
    last = NULL;					// clear last
    while (ptr != NULL)					// while we have some
    { clean = (ptr->dirty == NULL) &&                   // not dirty
              (ptr->last_accessed < now) &&             //   and not viewed
              (ptr->last_accessed > 0);                 //   and not being read
      if ((ptr->block == 0) ||				// if no block OR
	  ((ptr->last_accessed < exp) &&		// time expired
	   (clean)))			                // blk clean
      { if (last == NULL)				// first one?
	{ systab->vol[volnum-1]->gbd_hash[i] = ptr->next; // unlink from hash
	}
	else						// subsequent
	{ last->next = ptr->next;			// unlink
	}
	systab->hash_start = i;				// remember this
	blk[level] = ptr;				// store where reqd
	goto exit;					// common exit code
      }							// end found expired
      if ((clean) &&                                    // blk clean
	  (ptr->last_accessed < old))    		// and less than oldest
      { old = ptr->last_accessed;			// save time
	oldptr = ptr;					// save the ptr
	hash = i;					// and the hash
      }
      if ((ptr->dirty == NULL) &&			// if free
	  (ptr->last_accessed < old) &&			// and less than oldest
	  (ptr->last_accessed > 0))			// and there is a time
      { old = ptr->last_accessed;			// save time
	oldptr = ptr;					// save the ptr
	hash = i;					// and the hash
      }
      last = ptr;					// save last
      ptr = ptr->next;					// point at next
    }							// end 1 hash list
    i = (i + 1) & (GBD_HASH - 1);			// next hash entry
    if (i == systab->hash_start)			// where we started
    { break;						// done
    }
  }							// end while (TRUE)
  if (oldptr == NULL)					// did we get one
  { if (writing)					// SET or KILL
    { panic("Get_GBD: Failed to find an available GBD while writing"); // die
    }
    systab->vol[volnum - 1]->stats.gbwait++;            // incr. GBD wait
#ifdef MV1_GBDLATCH
    LatchUnlock(&systab->shsem[SEM_GLOBAL]);            // release latch
#endif
    SemOp(SEM_GLOBAL, -curr_lock);			// release current
    if (pass & 3)
      SchedYield();                                     // yield
    else
      MSleep(GBD_SLEEP);                                // wait
    pass++;
    if (pass > GBD_TRIES)				// this is crazy!
    { panic("Get_GBD: Can't get a GDB after 60 seconds");
    }
#ifdef MV1_GBDLATCH
    while (SemOp(SEM_GLOBAL, READ));                    // re-get READ lock
    s = LatchLock(&systab->shsem[SEM_GLOBAL]);          // re-get GLOBAL mutex
    if (s < 0)
    { panic("Get_GBD: failed to get GLOBAL mutex");
    }
#else
    while (SemOp(SEM_GLOBAL, WRITE));			// re-get WRITE lock
#endif
    goto start;						// and try again
  }

quite_old:
  ptr = systab->vol[volnum-1]->gbd_hash[hash];		// get the list
  if (ptr == oldptr)					// is this it
  { systab->vol[volnum-1]->gbd_hash[hash] = ptr->next;	// unlink it
  }
  else							// we gota look for it
  { while (ptr->next != oldptr)				// until we do
    { ptr = ptr->next;					// get the next
    }
    ptr->next = oldptr->next;				// unlink it
  }
  blk[level] = oldptr;					// store where reqd

exit:
  blk[level]->block = 0;				// no block attached
  blk[level]->next = NULL;				// clear link
  blk[level]->dirty = NULL;				// clear dirty
  blk[level]->last_accessed = (time_t) 0;		// and time
  UTIL_Barrier();
  idx = (u_short *) blk[level]->mem;			// set this up
  iidx = (int *) blk[level]->mem;			// and this
  return;						// return
}

#else

//-----------------------------------------------------------------------------
// Function: Get_GBDs
// Descript: Ensure there are n available GDBs
// Input(s): number of GBDs required
// Return:   none
// Note:     No lock is held when calling this function.
//	     When it completes, it returns with a write lock held.
//

// Reserved blocks
//
// They are not free (ie. has block data), but otherwise not dirty.
// When a writer want GBDs it is first satisfied from the free list
// then fromm the reserved blocks.

       int  nrsvd;                                      // no. of rsvd blocks
static gbd *rsvd_gbd[MAXTREEDEPTH * 2];                 // GBDs of rsvd blocks

void Unlink_GBD(gbd *oldptr)                            // unlink a GBD
{ int hash;
  gbd *ptr;

  hash = oldptr->hash;                                  // the chain
  // fprintf(stderr,"Unlink_GBD(): hash=%d\r\n",hash); fflush(stderr);
  assert((0 <= hash) && (hash < GBD_HASH));
  ptr = systab->vol[volnum-1]->gbd_hash[hash];		// get the list
  if (ptr == oldptr)					// is this it
  { systab->vol[volnum-1]->gbd_hash[hash] = oldptr->next;// unlink it
  }
  else							// inside the chain
  { oldptr->prev->next = oldptr->next;                  // nxt of prev is my nxt
    ptr = oldptr->prev;                                 // point to prev
  }
  if (oldptr->next)                                     // if not last
    oldptr->next->prev = ptr;                           //   point to head/prev
}

void Get_GBDsEx(int greqd, int haslock)			// get n free GBDs
{ int i, j;						// a handy int
  int curr;						// current count
  gbd *ptr;						// and pointer
  time_t now;						// current time
  int pass = 0;						// pass number
  int num_gbd = systab->vol[volnum-1]->num_gbd;         // local var

  nrsvd = 0;
start:
  if (!haslock)
    while (SemOp(SEM_GLOBAL, WRITE));                   // get write lock
  haslock = 0;                                          // clear for next turns
  ptr = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// head of free list
  curr = 0;						// clear current  
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
    if (ptr->hash == GBD_HASH)                          // on free list
      continue;                                         
    if (ptr->block == 0)				// if no block
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
      ptr->referenced = 0;                              // not refd
      curr++;						// count this
      if (curr >= greqd)				// if enough there
      { systab->hash_start = i;
        return;					        // just exit
      }
      continue;					        // next ptr
    }							// end - no block
    if ((ptr->dirty == NULL) &&			        // if free
        (ptr->last_accessed < now) &&                   // and not viewed
        (ptr->last_accessed > 0))			// and there is a time
    { if (ptr->referenced)                              // if refd, clear it
        ptr->referenced--;
      else
      { // fprintf(stderr,"Get_GBDs(): ptr->hash=%d\r\n",ptr->hash);
        rsvd_gbd[nrsvd++] = ptr;                        // save as reserved
        curr++;					        // count that
        if (curr >= greqd)				// if enough there
        { systab->hash_start = i;
          return;					// just exit
        }
      }
    }
  }
  systab->vol[volnum - 1]->stats.gbwait++;              // incr. GBD wait
  SemOp(SEM_GLOBAL, -curr_lock);			// release our lock
  if (pass & 3)
    SchedYield();                                       // yield
  else
    MSleep(GBD_SLEEP);                                  // wait
  pass++;						// increment a pass
  if (pass > GBD_TRIES)					// this is crazy!
  { panic("Get_GBDs: Can't get enough GDBs after 60 seconds");
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
  int oldpos;                                           // pos of oldptr
  short s;

  pass = 0;
start:
  if (systab->vol[volnum-1]->gbd_hash [GBD_HASH])	// any free?
  { blk[level]
      = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// get one
    systab->vol[volnum-1]->gbd_hash [GBD_HASH]
      = blk[level]->next;				// unlink it
    // fprintf(stderr,"Get_GBD(): from free list\r\n"); fflush(stderr);
    goto exit;						// common exit code
  }

  if (writing)                                          // writing ?
  { assert(nrsvd > 0);
    // fprintf(stderr,"Get_GBD(): from rsvd_gbd[]\r\n"); fflush(stderr);
    oldptr = rsvd_gbd[--nrsvd];                         // get from rsvd array
    goto unlink_gbd;                                    // needs unlink
  }

  now = MTIME(0) + 1;				        // get current time

  i = (systab->hash_start + 1) % num_gbd;		// where to start
  for (j = 0; j < num_gbd; j++)                         // for each GBD
  { ptr = &systab->vol[volnum-1]->gbd_head[i];
    clean = (ptr->dirty == NULL) &&                     // not dirty
            (ptr->last_accessed < now) &&               //   and not viewed
            (ptr->last_accessed > 0);                   //   and not being read
    if ((ptr->block == 0) ||				// if no block OR
        ((ptr->referenced == 0) &&                      //   and not refd
         (clean)))			                // blk clean
    { oldptr = ptr;                                     // mark this
      systab->hash_start = i;				// remember this
      // fprintf(stderr,"Get_GBD(): from block == 0/unreferenced clean\r\n");
      // fflush(stderr);
      goto unlink_gbd;				        // common exit code
    }							// end found expired
    if ((clean) && (ptr->referenced))                   // blk clean, but refd
    { ptr->referenced--;                                //   clear it
      if (!oldptr)
      { oldptr = ptr;                                   //   remember GBD
        oldpos = i;                                     //   and its position
      }
    }
    i = (i + 1) % num_gbd;			        // next GBD entry
  }							// end for every GBD
  if (oldptr == NULL)
  { if (writing)				        // SET or KILL
    { panic("Get_GBD: Failed to find an available GBD while writing"); // die
    }
    systab->vol[volnum - 1]->stats.gbwait++;            // incr. GBD wait
#ifdef MV1_GBDLATCH
    LatchUnlock(&systab->shsem[SEM_GLOBAL]);            // release latch
#endif
    SemOp(SEM_GLOBAL, -curr_lock);			// release current
    if (pass & 3)
      SchedYield();                                     // yield
    else
      MSleep(GBD_SLEEP);                                // wait
    pass++;
    if (pass > GBD_TRIES)				// this is crazy!
    { panic("Get_GBD: Can't get a GDB after 60 seconds");
    }
#ifdef MV1_GBDLATCH
    while (SemOp(SEM_GLOBAL, READ));                    // re-get READ lock
    s = LatchLock(&systab->shsem[SEM_GLOBAL]);          // re-get GLOBAL mutex
    if (s < 0)
    { panic("Get_GBD: failed to get GLOBAL mutex");
    }
#else
    while (SemOp(SEM_GLOBAL, WRITE));			// re-get lock
#endif
    goto start;						// and try again
  }
  systab->hash_start = oldpos;                          // remember this

unlink_gbd:
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
  blk[level]->referenced = 1;                           // mark refd
  blk[level]->prev = NULL;                              // clear prev link
  blk[level]->hash = -1;                                // clear hash chain
  UTIL_Barrier();
  idx = (u_short *) blk[level]->mem;			// set this up
  iidx = (int *) blk[level]->mem;			// and this
  return;						// return
}

#endif

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
    ptr = systab->vol[volnum-1]->gbd_hash[free->block & (GBD_HASH -1)];
    if (ptr == free)					// if this one
    { systab->vol[volnum-1]->gbd_hash[free->block & (GBD_HASH -1)]
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
  free->referenced = 0;
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

