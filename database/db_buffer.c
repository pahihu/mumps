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

#define Unlink_GBD(vol,x)   UnlinkGBD(vol,x,__FILE__,__LINE__)

void UnlinkGBD(int vol, gbd *oldptr,                    // unlink a GBD
        const char *caller_path, int caller_line)
{ int hash;
  gbd *ptr;

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  hash = oldptr->hash;                                  // the chain
  // fprintf(stderr,"Unlink_GBD(): hash=%d\r\n",hash); fflush(stderr);
  ASSERT2(0 <= hash);
  ASSERT2(hash < GBD_HASH);
  ptr = systab->vol[vol]->gbd_hash[hash];	        // get the list
  if (ptr == oldptr)					// is this it
  { systab->vol[vol]->gbd_hash[hash] = oldptr->next;    // unlink it
    ptr = 0;                                            // removed
  }
  else						        // inside the chain
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

void DB_Locked(void)
{ int i, num_gbd;
  int vol;

  vol = volnum - 1;                                     // calc. vol index

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

#if 0
  num_gbd = systab->vol[vol]->num_gbd;
  for (i = 0; i < num_gbd; i++)
  { gbd* ptr = &(systab->vol[vol]->gbd_head[i]);
    if (ptr->dirty && (ptr->dirty < ((gbd *) 5)))
    { ptr->dirty = 0;
    }
  }
#endif
}

#define MAX_RESERVED_GBDS   (2*MAXTREEDEPTH)
static int numReservedGBDs = 0;                         // no. of reserved GBDs
static gbd *reservedGBDs[MAX_RESERVED_GBDS];            // reserved GBDs

void DB_WillUnlock(void)
{ int i;                                                // a handy int

  if (writing)                                          // writing ?
    for (i = 0; i < numReservedGBDs; i++) {             // check each rsvd GBD
      if (reservedGBDs[i]->dirty < (gbd *)5)            // if reserved
        reservedGBDs[i]->dirty = 0;                     //   release it
    }

  numReservedGBDs = 0;                                  // clear reserved
  return;
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

#define BLK_WAIT        50

extern pid_t mypid;

static
short GetBlockEx(u_int blknum,const char *file,int line)      // Get block
{ int i;						// a handy int
  short s = -1;						// for functions
  off_t file_off;					// for lseek()
  gbd *ptr;						// a handy pointer
  int  refd_inited = 0;                                 // reference initialized
  time_t wait_start;
  char msg[128];

  // fprintf(stderr,"GetBlock(%u) called from %s:%d\r\n", blknum, file, line);
#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"--- S:GetBlock(%u)\r\n",blknum);fflush(stderr);
#endif

  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.logrd); // update stats
  if (LB_ENABLED == gbd_local_state)			// local buffer enabled?
  { ptr = LB_GetBlock(blknum);				//   search block
    if (NULL == ptr)					// not found?
      return -(ERRZ94 + ERRMLAST);			//   return error
    blk[level] = ptr;					// save local block
    goto exitP;
  }
  ptr = systab->vol[volnum-1]->gbd_hash[GBD_BUCKET(blknum)]; // get head
  while (ptr != NULL)					// for entire list
  { if (ptr->block == blknum)				// found it?
    { blk[level] = ptr;					// save the ptr
      MEM_BARRIER;
      wait_start = MTIME(0);                            // remember time
      while (ptr->last_accessed == (time_t) 0)	        // if being read
      { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.rdwait);
        if ((MTIME(0) - wait_start) > BLK_WAIT)         // wait over ?
        { sprintf(msg,"Get_block: can't get block in stage1 after %d seconds",
                        BLK_WAIT);
          panic(msg);
        }
        SchedYield();					// wait for it
        MEM_BARRIER;
      }
      ASSERT(blknum == ptr->block);
      goto exit;					// go common exit code
    }
    ptr = ptr->next;					// point at next
  }							// end memory search

  if (!writing)						// if read mode
  { SemOp( SEM_GLOBAL, -curr_lock);			// release read lock
    s = SemOp( SEM_GLOBAL, WRITE);			// get write lock
    if (s < 0)                                          // on error
    { return s;                                         // return it
    }
    ptr = systab->vol[volnum-1]->gbd_hash[GBD_BUCKET(blknum)]; // get head
    while (ptr != NULL)					// for entire list
    { if (ptr->block == blknum)				// found it?
      { blk[level] = ptr;				// save the ptr
	while (SemOp( SEM_GLOBAL, WR_TO_R))		// drop to read lock
          ;
        MEM_BARRIER;
        wait_start = MTIME(0);
        while (ptr->last_accessed == (time_t) 0)	// if being read
        { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.rdwait);
          if ((MTIME(0) - wait_start) > BLK_WAIT)       // wait over ?
          { sprintf(msg,"Get_block: can't get block in stage2 after %d seconds",
                          BLK_WAIT);
            panic(msg);
          }
          SchedYield();					// wait for it
          MEM_BARRIER;
        }
        ASSERT(blknum == ptr->block);
        goto exit;					// go common exit code
      }
      ptr = ptr->next;					// point at next
    }							// end memory search
  }							// now have a write lck
  systab->vol[volnum-1]->stats.phyrd++;                 // update stats
  Get_GBD();					        // get a GBD
  blk[level]->block = blknum;				// set block number
  REFD_READ_INIT(blk[level]);
  refd_inited = 1;
  blk[level]->last_accessed = (time_t) 0;		// clear last access
  Link_GBD(blknum, blk[level]);
  MEM_BARRIER;
  if (!writing)						// if reading
  { while (SemOp( SEM_GLOBAL, WR_TO_R));		// drop to read lock
  }
#ifdef MV1_CACHE_IO
  fprintf(stderr,"%d %20lld R %d\r\n",mypid,monotonic_time(),blk[level]->block);
  fflush(stderr);
#endif
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
  if (LB_FILL == gbd_local_state)			// local buffer fill?
  { LB_AddBlock(blk[level]);				//   add to local buffer
  }
  if ((writing) && (blk[level]->dirty < (gbd *) 5))	// if writing
  // if ((writing) && (blk[level]->dirty == NULL))	// if writing
  { blk[level]->dirty = (gbd *) 1;			// reserve it
  }
  blk[level]->last_accessed = MTIME(0);			// set access time
  if (!refd_inited)
  { REFD_MARK(blk[level]);
  }
exitP:
  MEM_BARRIER;
  Index = LOW_INDEX;					// first one
  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"--- E:GetBlock\r\n"); fflush(stderr);
#endif
  return 0;						// return success
}

short GetBlock(u_int blknum,const char *file,int line)        // Get block
{
  Check_BlockNo(volnum-1, blknum,                       // check blknum, die
                CBN_ALLOCATED | CBN_INRANGE,    
                "Get_block", file, line, 1);
  return GetBlockEx(blknum,file,line);
}

short GetBlockRaw(u_int blknum, const char *file, int line)   // Get block, raw
{ short s;
  s = Check_BlockNo(volnum-1, blknum, CBN_INRANGE,      // check blknum
                        "Get_block_raw", file, line, 0);
  if (s < 0)
    return s;
  return GetBlockEx(blknum,file,line);
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
	Mark_map_dirty(volnum-1, blknum);		// mark map dirty
        blk[level]->block = blknum;			// save in structure
	blk[level]->dirty = (gbd *) 1;			// reserve it
	blk[level]->last_accessed = MTIME(0);		// accessed
        REFD_NEW_INIT(blk[level]);                      // mark referenced
        Link_GBD(blknum, blk[level]);
	bzero(blk[level]->mem, systab->vol[volnum-1]->vollab->block_size);
        MEM_BARRIER;
	systab->vol[volnum-1]->first_free = c;		// save this
        systab->vol[volnum-1]->stats.blkalloc++;        // update stats
#ifdef MV1_CACHE_IO
        fprintf(stderr,"%d %20lld A %d\r\n",mypid,monotonic_time(),blk[level]->block);
        fflush(stderr);
#endif
	return 0;					// return success
      }
    }
    c++;						// point at next
  }							// end map scan
  Free_GBD(volnum-1, blk[level]);			// give it back
  return -(ERRMLAST + ERRZ11);				// error - no room
}

void WriteBlock(gbd *gbdptr)				// write GBD
{ off_t file_off;                               	// for lseek() et al
  int i;						// a handy int
  u_int blkno;                                          // block#
  int dbfd;

//  if (!writing)
//    systab->vol[volnum-1]->stats.eventcnt++;            // update stats

#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"WriteBlock: %d %p\r\n",gbdptr->block,gbdptr->dirty);fflush(stderr);
#endif
#ifdef MV1_CACHE_IO
  fprintf(stderr,"%d %20lld W %d\r\n",mypid,monotonic_time(),gbdptr->block);
  fflush(stderr);
#endif
  blkno = gbdptr->block;
  dbfd = partab.vol_fds[volnum - 1];
  Check_BlockNo(volnum-1, blkno,                        // check blkno validity
                CBN_INRANGE | CBN_ALLOCATED,
                "WriteBlock", 0, 0, 1);         
  file_off = (off_t) blkno - 1;		                // block#
  file_off = (file_off * (off_t)
	systab->vol[volnum-1]->vollab->block_size)
	+ (off_t) systab->vol[volnum-1]->vollab->header_bytes;
  file_off = lseek( dbfd, file_off, SEEK_SET);          // seek to block
  if (file_off < 1)
  { systab->vol[volnum-1]->stats.diskerrors++;	        // count an error
    panic("lseek failed in WriteBlock()!!");	        // die on error
  }
  i = write( dbfd, gbdptr->mem,
	 systab->vol[volnum-1]->vollab->block_size);    // write it
  if (i < 0)
  { systab->vol[volnum-1]->stats.diskerrors++;	        // count an error
    panic("write failed in WriteBlock()!!");
  }
  systab->vol[volnum-1]->stats.phywt++;                 // count a write
  gbdptr->dirty = NULL;
  MEM_BARRIER;
}							// end write code

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
    while (SemOp( SEM_GLOBAL, WRITE));                  // get write lock
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

  i = (systab->vol[volnum-1]->hash_start + 1) % num_gbd;// where to start
  for (j = 0; j < num_gbd; j++, i = (i + 1) % num_gbd)
  { ptr = &systab->vol[volnum-1]->gbd_head[i];
    if ((GBD_HASH == ptr->hash) ||                      // skip GBDs on free lst
        (ptr->dirty && (ptr->dirty < (gbd *)5)))	//   or reserved
      continue;
    if (0 == ptr->block)  				// if no block
    { // fprintf(stderr,"Get_GBDs(): block == 0\r\n"); fflush(stderr);
      // fprintf(stderr,"Get_GBDs(): before Unlink_GBD\r\n"); fflush(stderr);
      Unlink_GBD(volnum-1, ptr);                        // unlink ptr
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
    if ((ptr->dirty == NULL) &&				// if free
        (now > ptr->last_accessed) &&                   //   and not viewed
        (0   < ptr->last_accessed))			//   and there is a time
    { curr++;					        // count that
      if (curr >= greqd)				// if enough there
        return;					        // just exit
    }
  }

  if (0 == (pass & 3))                                  // will wait ?
    systab->vol[volnum - 1]->stats.gbswait++;           //   update stats
  SemOp( SEM_GLOBAL, -curr_lock);			// release our lock
  if (pass & 3)
    SchedYield();                                       // yield
  else
  { MSleep(GBD_SLEEP);                                  // wait
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

static
gbd* GetGBDEx(int rdonly)				// get a GBD
{ int i, j;						// a handy int
  time_t now;						// current time
  gbd *ptr;						// loop gbd ptr
  gbd *oldptr = NULL;					// remember last unrefd
  int pass;
  int avail;                                            // flag available blk
  int num_gbd = systab->vol[volnum-1]->num_gbd;         // local var
  int oldpos;

#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"ENTER GBD\r\n"); fflush(stderr);
#endif
  pass    = 0;
start:
  oldptr = NULL;
  oldpos = -1;
  if (systab->vol[volnum-1]->gbd_hash [GBD_HASH])	// any free?
  { oldptr
      = systab->vol[volnum-1]->gbd_hash [GBD_HASH];	// get one
    systab->vol[volnum-1]->gbd_hash [GBD_HASH]
      = oldptr->next;			                // unlink it
    // fprintf(stderr,"Get_GBD(): from free list\r\n"); fflush(stderr);
#ifdef MV1_CACHE_IO
    fprintf(stderr,"%d %20lld F\r\n",mypid,monotonic_time());
    fflush(stderr);
#endif
    goto exit;						// common exit code
  }
  else if (rdonly)
  { return 0;
  }

  now = MTIME(0) + 1;				        // get current time

  i = (systab->vol[volnum-1]->hash_start + 1) % num_gbd;// where to start
  for (j = 0; j < num_gbd; j++, i = (i + 1) % num_gbd)  // for each GBD
  { ptr = &systab->vol[volnum-1]->gbd_head[i];
    if (ptr->dirty && (ptr->dirty < (gbd *) 5))         // skip if reserved
      continue;
    if (0 == ptr->block)  				// no block ?
    { oldptr = ptr;                                     // mark this
      oldpos = i;				        // remember this
      // fprintf(stderr,"Get_GBD(): from block == 0/unreferenced clean\r\n");
      // fflush(stderr);
      goto unlink_gbd;				        // common exit code
    }							// end found expired
    if (REFD_VALUE(ptr))                                // referenced ?
    { REFD_UNMARK(ptr);                                 //   decrement ref
    }
    else
    { avail = (ptr->dirty == NULL) &&                   // not dirty
              (now > ptr->last_accessed) &&             //   and not viewed
              (0   < ptr->last_accessed);               //   and not being read
      if (avail)                                        // available?
      { oldptr = ptr;                                   // save ptr
        oldpos = i;
        goto unlink_gbd;                                // and use it
      }
    }
  }							// end for every GBD

  if (NULL == oldptr)
  { if (writing)				        // SET or KILL
    { panic("Get_GBD: Failed to find an available GBD while writing"); // die
    }

    systab->vol[volnum - 1]->stats.gbwait++;            // incr. GBD wait
    SemOp( SEM_GLOBAL, -curr_lock);			// release current
    if (pass & 3)
      SchedYield();                                     // yield
    else
      MSleep(GBD_SLEEP);                                // wait
    pass++;
    if (pass > GBD_TRIES)				// this is crazy!
    { panic("Get_GBD: Can't get a GDB after 60 seconds");
    }
    while (SemOp( SEM_GLOBAL, WRITE));			// re-get lock
    goto start;						// and try again
  }

unlink_gbd:
#ifdef MV1_CACHE_IO
  fprintf(stderr,"%d %20lld C %d(%d)\r\n",mypid,monotonic_time(),oldptr->block,oldptr->refd);
  fflush(stderr);
#endif
  systab->vol[volnum-1]->hash_start = oldpos;

  // fprintf(stderr,"Get_GBD(): before Unlink_GBD\r\n"); fflush(stderr);
  Unlink_GBD(volnum-1, oldptr);                         // unlink oldptr
  // fprintf(stderr,"Get_GBD(): after Unlink_GBD\r\n"); fflush(stderr);

exit:
  oldptr->block = 0;			                // no block attached
  // fprintf(stderr,"Get_GBD(): exit\r\n"); fflush(stderr);
  oldptr->next = NULL;				        // clear link
  oldptr->dirty = writing ? (gbd *) 1 : NULL;           // reserve when writing
  oldptr->last_accessed = (time_t) 0;		        // and time
  REFD_READ_INIT(oldptr);                               // mark refd
  oldptr->prev = NULL;                                  // clear prev link
  oldptr->hash = -1;                                    // clear hash chain
  MEM_BARRIER;
  idx = (u_short *) oldptr->mem;			// set this up
  iidx = (int *) oldptr->mem;			        // and this
  return oldptr; 
}

void Get_GBD(void)                                      // get a GBD
{ blk[level] = GetGBDEx(0);				// store where reqd
  if (writing)                                          // writing ?
  { if (numReservedGBDs == MAX_RESERVED_GBDS)           // reserved buffer full?
    { panic("reserved GBD buffer overflow");            // do panic
    }
    reservedGBDs[numReservedGBDs++] = blk[level];       // save GBD
  }
#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"EXIT GBD\r\n"); fflush(stderr);
#endif
}


//-----------------------------------------------------------------------------
// Function: Free_GBD
// Descript: Free specified GBD (if ->block non-zero, remove from hash table)
// Input(s): GDB pointer
// Return:   none
// Note:     curr_lock MUST be WRITE when calling this function
//
void Free_GBD(int vol, gbd *free)			// Free a GBD
{ gbd *ptr;						// a handy pointer

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted
  ASSERT(free->vol == vol);                             // same volume

  if (free->block)					// if there is a blk#
  { Unlink_GBD(vol, free);                              // unlink GBD ptr
  }

  free->prev = NULL;
  REFD_CLEAR(free);
  free->hash = GBD_HASH;
  free->next = systab->vol[vol]->gbd_hash[GBD_HASH];    // get free list
  systab->vol[vol]->gbd_hash[GBD_HASH] = free; 	        // link it in
  free->block = 0;					// clear this
  free->dirty = NULL;					// and this
  free->last_accessed = (time_t) 0;			// and this
  MEM_BARRIER;
  return;						// and exit
}

