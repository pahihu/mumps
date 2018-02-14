// File: mumps/database/db_util.c
//
// module database - Database Functions - Utilities

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
#include <errno.h>                                      // for errno
#include <fcntl.h>					// for expand
#include <ctype.h>					// for gbd stuff
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include <sys/stat.h>					// for fchmod
#include "mumps.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

//-----------------------------------------------------------------------------
// Function: Insert
// Descript: Insert the supplied key and data in blk[level]
// Input(s): Pointer the the key and data to insert
// Return:   String length -> Ok, negative MUMPS error -(ERRMLAST+ERRZ62)
//

extern int KeyLocated;                                  // flag key located

u_short FindChunk0(u_short from)
{
  cstring *tsunk;

  for (from-- ; from >= LOW_INDEX; from--)              // search backward
  { tsunk = (cstring *) &iidx[idx[from]];
    if (tsunk->buf[0] == 0)                             // if prefix is zero
      return from;                                      //   done
  }
  return LOW_INDEX;                                     // sentinel
}

u_short FindChunk(u_short from, u_char pfxlen)
{
  cstring *tsunk;

  for (from-- ; from >= LOW_INDEX; from--)              // search backward
  { tsunk = (cstring *) &iidx[idx[from]];
    if (tsunk->buf[0] < pfxlen)                         // if prefix is shorter
      return from;                                      //   done
  }
  return LOW_INDEX;                                     // sentinel
}

short Insert(u_char *key, cstring *data)                // insert a node
{ int i;						// a handy int
  int isdata;						// data/ptr flag
  int rs;						// required size
  short s;						// for funcs
  u_char ccc;						// common char count
  u_char ucc;						// uncommon char count
  u_int flags = 0;					// for $GLOBAL
  int locate_used;                                      // Locate() used

  isdata = ((blk[level]->mem->type > 64) &&		// data block and
	    (level));					// not the directory

  locate_used = 0;
  if (blk[level]->mem->last_idx > LOW_INDEX - 1)	// if some data
  { if (!KeyLocated)                                    // key not located yet
    { locate_used = 1;                                  // flag using Locate()
      s = Locate(key);			                // search for it
      if (s >= 0)					// if found
      { return -(ERRMLAST+ERRZ61);                      // database stuffed
      }
      else if (s != -ERRM7)				// for any other error
      { return s;                                       // exit
      }
    }
  }
  else							// empty block
  { Index = LOW_INDEX;					// start
    idx = (u_short *) blk[level]->mem;			// point at the block
    iidx = (int *) blk[level]->mem;			// point at the block
  }
  if (!level)						// insert in GD
  { chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at $G chunk
    record = (cstring *) &chunk->buf[chunk->buf[1] + 2];
    Allign_record();					// allign it
    flags = ((u_int *) record)[1];			// get default flags
    partab.jobtab->last_block_flags[volnum - 1] = flags;
  }

  if (1 /*locate_used == 0*/)                           // XXX
  { keybuf[0] = 0;					// clear keybuf
#ifdef MV1_CCC
    // for (i = LOW_INDEX; i < Index; i++)		// for all prev Indexes
#if 0
    for (i = FindChunk0(Index); i < Index; i++)		// for all prev Indexes
    { chunk = (cstring *) &iidx[idx[i]];		// point at the chunk
      bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	    chunk->buf[1]);				// update the key
      keybuf[0] = chunk->buf[0] + chunk->buf[1];	// and the size
    }							// we insert after this
#endif
    Build_KeyBuf(Index - 1, &keybuf[0]);
#endif
  }

  ccc = 0;						// start here
#ifdef MV1_CCC
  // Key segmentation
  //
  //   To conserve space, we compress 7 keys, then 
  //   store 1 w/o compression.
  //
  // if (((Index - LOW_INDEX) & 7) &&                      // not segment marker
  if ((key[0]) && (keybuf[0]))			        //   and any there
  { while (key[ccc + 1] == keybuf[ccc + 1])		// while the same
    { if ((ccc == key[0]) || (ccc == keybuf[0]))	// at end of either
      { break;						// done
      }
      ccc++;						// increment ptr
    }
  }
#endif
  ucc = key[0] - ccc;					// and this
  rs = sizeof(short) + 2				// chunksiz + ccc + ucc
       + ucc + data->len;				// + key + data
  if (isdata)						// if it's a data blk
  { rs += sizeof(short);				// add the dbc size
  }
  else if (!level)					// if GD
  { rs += 4;						// allow for flags
  }
  if (rs & 3)						// not even long word
  { rs += (4 - (rs & 3));				// round it up
  }
  rs += 4;						// allow for the Index

  if (rs > ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
  { if (!(blk[level]->mem->flags & BLOCK_DIRTY))	// if block is clean
    { return -(ERRMLAST+ERRZ62);                        // say no room
    }
    Tidy_block();					// tidy it
    if (rs > ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
    { return -(ERRMLAST+ERRZ62);                        // say no room
    }
  }							// it will now fit
  rs -= 4;						// rs now chunksize

  for (i = blk[level]->mem->last_idx; i>=Index; i--)	// the trailing ones
  { idx[i + 1] = idx[i];				// get copied down
  }
  idx[Index] = blk[level]->mem->last_free - (rs / 4) + 1; // where it goes
  chunk = (cstring *) &iidx[idx[Index]];		// as an address
  record = (cstring *) &chunk->buf[ucc + 2];		// where the data goes
  chunk->len = rs;					// store chunk size
  chunk->buf[0] = ccc;					// this bit
  chunk->buf[1] = ucc;					// then this
  bcopy(&key[ccc + 1], &chunk->buf[2], ucc);		// then the key bit
  if (isdata)						// for data blk
  { record->len = data->len;				// copy the length
    bcopy(data->buf, record->buf, data->len);		// then the data
  }
  else							// it's a pointer
  { Allign_record();					// allign it
    bcopy(data->buf, record, sizeof(int));		// the block number
    if (!level)						// if GD
    { ((u_int *) record)[1] = flags;			// set/clear the flags
    }
  }
  blk[level]->mem->last_free -= (rs / 4);		// redo last_free
  blk[level]->mem->last_idx++;				// add to the index
  blk[level]->mem->flags |= BLOCK_DIRTY;		// mark dirty

  return 0;						// done
}

//-----------------------------------------------------------------------------
// Function: Queit
// Descript: Que the gbd p_gbd - links already setup
// Input(s): the gbd to queue
// Return:   0 - on success, 1 - on failed (queue empty)
// Note:     Must hold a write lock before calling this function
//

void Queit()						// que a gbd for write
{ int i;						// a handy int
  gbd *ptr, *nxt;					// a handy ptr
#ifdef MV1_CKIT
  bool result;
#endif

  ASSERT(0 < volnum);                                   // valid volnum
  ASSERT(volnum <= MAX_VOL);
  ASSERT(NULL != systab->vol[volnum-1]->vollab);        // mounted

  // LastBlock = 0;                                     // zot Locate() cache
  ptr = blk[level];					// point at the block
  // fprintf(stderr,"Queit: %d",ptr->block);
  systab->vol[volnum-1]->stats.logwt++;			// incr logical
  while (ptr->dirty != ptr)				// check it
  { ptr = ptr->dirty;					// point at next
    // fprintf(stderr," %d",ptr->block);
    systab->vol[volnum-1]->stats.logwt++;		// incr logical
  }
  // fprintf(stderr,"\r\n");

  if (curr_lock != WRITE)
  { char msg[32];
    sprintf(msg, "Queit(): curr_lock = %d", curr_lock);
    panic(msg);
  }

#ifdef MV1_CKIT
  result = ck_ring_enqueue_spmc(
                &systab->vol[0]->dirtyQ,
                &systab->vol[0]->dirtyQBuffer[0],
                blk[level]);
  if (false == result)
  { panic("Queit(): dirtyQ overflow");
  }
#else
  // we have the WRITE lock, at least NUM_DIRTY/2 is free
  i = systab->vol[0]->dirtyQw;			        // where to put it
  if (systab->vol[0]->dirtyQ[i] != NULL)
  { panic("Queit(): dirtyQ overflow");
  }
  systab->vol[0]->dirtyQ[i] = blk[level];	        // stuff it in
  systab->vol[0]->dirtyQw = (i + 1) & (NUM_DIRTY - 1);  // reset ptr
#endif

  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Garbit
// Descript: Que the block passed in for garbage collection
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Garbit(int blknum)					// que a blk for garb
{ int i;						// a handy int
  int j;						// for loop
#ifdef MV1_CKIT
  void *qentry;                                         // queue entry
  bool result;                                          // ck result
#endif

  ASSERT(0 < volnum);                                   // valid vol[] index
  ASSERT(volnum <= MAX_VOL);
  ASSERT(NULL != systab->vol[volnum-1]->vollab);        // mounted

  blknum = VOLBLK(volnum-1,blknum);                     // reformat (vol,blkno)

  if (curr_lock != WRITE)
  { char msg[32];
    sprintf(msg, "Garbit(): curr_lock = %d", curr_lock);
    panic(msg);
  }

#ifdef MV1_CKIT
  qentry = (void*) blknum;
  result = ck_ring_enqueue_spmc(
                &systab->vol[0]->garbQ,
                &systab->vol[0]->garbQBuffer[0],
                qentry);
  if (result == false)
  { panic("Garbit(): garbQ overflow");
  }
#else
  // we have the WRITE lock
  i = systab->vol[0]->garbQw;			        // where to put it
  if (systab->vol[0]->garbQ[i] != 0)
  { panic("Garbit(): garbQ overflow");
  }
  systab->vol[0]->garbQ[i] = blknum;		        // stuff it in
  systab->vol[0]->garbQw = (i + 1) & (NUM_GARB - 1);    // reset ptr
#endif
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Free_block
// Descript: Remove the specified block from the map
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Free_block(int vol, int blknum)			// free blk in map
{ int i;						// a handy int
  int off;						// and another
  u_char *map;						// map pointer

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  map = ((u_char *) systab->vol[vol]->map);	        // point at it
  i = blknum >> 3;					// map byte
  off = blknum & 7;					// bit number
  off = 1 << off;					// convert to mask
  if ((map[i] & off) == 0)				// if it's already free
  { return;						// just exit
  }
  ATOMIC_INCREMENT(systab->vol[vol]->stats.blkdeall);   // update stats
  map[i] &= ~off;					// clear the bit
  if (systab->vol[vol]->first_free > (void *) &map[i])	// if earlier
  { systab->vol[vol]->first_free = &map[i];             // reset first free
  }
  systab->vol[vol]->map_dirty_flag++;		        // mark map dirty
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Used_block
// Descript: Add the specified block to the map
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//	     The caller must have ensured that, if there is a map
//		scan in progress, this block is less than "upto".
// This is only called from database/db_view.c
//

void Used_block(int vol, int blknum)			// set blk in map
{ int i;						// a handy int
  int off;						// and another
  u_char *map;						// map pointer

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             //  mounted

  map = ((u_char *) systab->vol[vol]->map);	        // point at it
  i = blknum >> 3;					// map byte
  off = blknum & 7;					// bit number
  off = 1 << off;					// convert to mask
  if ((map[i] & off))					// if it's already used
  { return;						// just exit
  }
  ATOMIC_INCREMENT(systab->vol[vol]->stats.blkalloc);   // update stats
  map[i] |= off;					// set the bit
  systab->vol[vol]->map_dirty_flag++;		        // mark map dirty
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Tidy_block
// Descript: Tidy the current block
// Input(s): none
// Return:   none
// Note:     Must hold a write lock before calling this function
//	     This function ommits records with dbc = NODE_UNDEFINED
//	     This function ommits pointers with record = PTR_UNDEFINED
//

void Tidy_block()					// tidy current blk
{ gbd *ptr;						// a handy pointer
  DB_Block *btmp;					// ditto

  ptr = blk[level];					// remember current
  Get_GBD();						// get another
  bzero(blk[level]->mem, systab->vol[volnum-1]->vollab->block_size); // zot
  blk[level]->mem->type = ptr->mem->type;		// copy type

  if (!level)						// if it's a GD
  { blk[level]->mem->type |= 64;			// ensure it's data
  }

  blk[level]->mem->right_ptr = ptr->mem->right_ptr;	// copy RL
  blk[level]->mem->global = ptr->mem->global;		// copy global name
  blk[level]->mem->last_idx = LOW_INDEX - 1;		// unused block
  blk[level]->mem->last_free
    = (systab->vol[volnum-1]->vollab->block_size >> 2) - 1; // set this up
  Copy_data(ptr, LOW_INDEX);				// copy entire block
  btmp = blk[level]->mem;				// save this
  blk[level]->mem = ptr->mem;				// copy in this
  ptr->mem = btmp;					// end swap 'mem'

  Free_GBD(volnum-1, blk[level]);			// release it
  blk[level] = ptr;					// restore the ptr
  idx = (u_short *) blk[level]->mem;			// set this up
  iidx = (int *) blk[level]->mem;			// and this
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Copy_data
// Descript: Copy data from "from" to blk[level]
// Input(s): from GBD and index (or flag)
// Return:   none
// Note:     Must hold a write lock before calling this function
//	     All external variables describing blk[level] must be setup
//	     This function ommits records with dbc = NODE_UNDEFINED
//	     This function ommits pointers with record = PTR_UNDEFINED
//

void Copy_data(gbd *fptr, int fidx)			// copy records
{ int i;						// a handy int
  u_short *sfidx;					// for Indexes
  int *fiidx;						// int ver of Index
  u_char fk[260];					// for keys
  int isdata;						// a flag
  cstring *c;						// reading from old
  u_char ccc;						// common char count
  u_char ucc;						// uncommon char count
  short cs;						// new chunk size

  isdata = ((blk[level]->mem->type > 64) && (level));	// block type
  sfidx = (u_short *) fptr->mem;			// point at it
  fiidx = (int *) fptr->mem;				// point at it

  keybuf[0] = 0;					// clear this
#ifdef MV1_CCC
  // for (i = LOW_INDEX; i <= blk[level]->mem->last_idx; i++)// scan to end to blk
#if 0
  for (i = FindChunk0(blk[level]->mem->last_idx + 1);   // scan to end to blk
                  i <= blk[level]->mem->last_idx; i++)
  { chunk = (cstring *) &iidx[idx[i]];			// point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  }							// end update keybuf[]
#endif
  Build_KeyBuf(blk[level]->mem->last_idx, &keybuf[0]);
#else
  chunk = (cstring *) &iidx[idx[blk[level]->mem->last_idx]];// point at to chunk
  bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
  keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
#endif

  for (i = LOW_INDEX; i <= fptr->mem->last_idx; i++)	// for each Index
  { c = (cstring *) &fiidx[sfidx[i]];			// point at chunk
    bcopy(&c->buf[2], &fk[c->buf[0] + 1], c->buf[1]);	// copy key
    fk[0] = c->buf[0] + c->buf[1];			// and the length
    if (i < fidx)					// copy this one
    { continue;						// no - just continue
    }
    c = (cstring *) &(c->buf[c->buf[1] + 2]);		// point at dbc/ptr
    if (isdata)						// if data
    { if (c->len == NODE_UNDEFINED)			// junk record?
      { continue;					// ignore it
      }
    }
    else						// if a pointer
    { if ((long) c & 3)					// if not alligned
      { c = (cstring *) &c->buf[2 - ((long) c & 3)];	// allign
      }
      if ((*(int *) c) == PTR_UNDEFINED)		// see if that's junk
      { continue;					// ignore it
      }
    }
    ccc = 0;						// start here
#ifdef MV1_CCC
    // if (((blk[level]->mem->last_idx + 1 - LOW_INDEX) & 7) && // not seg. marker
    // if (((i - LOW_INDEX) & 7) &&                     // not seg. marker
    if ((fk[0]) && (keybuf[0]))			        // and if any there
    { while (fk[ccc + 1] == keybuf[ccc + 1])		// while the same
      { if ((ccc == fk[0]) || (ccc == keybuf[0]))	// at end of either
        { break;					// done
        }
        ccc++;						// increment ptr
      }
    }
#endif
    ucc = fk[0] - ccc;					// get the ucc
    cs = 4 + ucc + (isdata ? (c->len + 2) : 4);		// chunk size = this
    if (!level)						// if GD
    { cs += 4;						// allow for flags
    }
    if (cs & 3)						// but
    { cs += (4 - (cs & 3));				// round up
    }

    if (cs >=
        ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
    { if (fidx == -1)
      { return;
      }
      panic("Copy_data: about to overflow block");
    }

    blk[level]->mem->last_free -= (cs / 4);		// reset free
    idx[++blk[level]->mem->last_idx]
      = blk[level]->mem->last_free + 1;			// point at next chunk
    chunk = (cstring *) &iidx[blk[level]->mem->last_free + 1];
    chunk->len = cs;					// set the size
    chunk->buf[0] = ccc;				// ccc
    chunk->buf[1] = ucc;				// ucc
    bcopy(&fk[ccc + 1], &chunk->buf[2], ucc);		// the key
    record = (cstring *) &chunk->buf[ucc + 2];		// point at dbc/ptr
    if (isdata)						// for a data block
    { record->len = c->len;				// copy dbc
      bcopy(c->buf, record->buf, c->len);		// copy the data
      if (fidx == -1)
      { c->len = NODE_UNDEFINED;
      }
    }
    else						// for a pointer
    { Allign_record();					// ensure alligned
      *(u_int *) record = *(u_int *) c;			// copy ptr
      if (fidx == -1)
      { *(int *) c = PTR_UNDEFINED;
      }
      if (!level)					// if GD
      { ((u_int *) record)[1] = ((u_int *) c)[1] & GL_FLAGS; // copy flags
			// NOTE: ABOVE ALL FLAGS CLEARED !!!!!!!!
      }
    }
    bcopy(fk, keybuf, fk[0] + 1);			// save full key    
  }							// end copy loop
  return;						// and exit
}


//-----------------------------------------------------------------------------
// Function: Allign_record
// Descript: Ensure that record is on a four byte boundary
// Input(s): none
// Return:   none
// Note:     Must only be called for pointer/directory blocks
//

void Allign_record()					// allign record (int)
{ if ((long) record & 3)				// if not alligned
  { record = (cstring *) &record->buf[2 - ((long) record & 3)]; // allign
  }
  return;						// exit
}

//-----------------------------------------------------------------------------
// Function: Compress1
// Descript: Compress one block union
// Input(s): mvar * to the key to find
//	     the level to operate at
// Return:   zero or error
//

short Compress1()
{ int i;
  int curlevel;
  short s;
  u_char gtmp[2*MAX_NAME_BYTES];			// to find glob

  writing = 1;						// flag writing
  Ensure_GBDs(0);                        		// ensure this many

  curlevel = level;
  s = Get_data(curlevel);				// get the data
  if ((s == -ERRM7) && (!db_var.slen))			// if top
  { s = 0;						// it does exist
  }
  if (s == -ERRM7)					// if gone missing
  { Release_GBDs(0); 
    return 0;						// just exit
  }
  if (s < 0)						// any other error
  { return s;						// return it
  }
  if (!blk[level]->mem->right_ptr)			// if no more blocks
  { if ((level == 2) && (!db_var.slen))			// and blk 1 on level 2
    { level = 0;					// look at the GD
      gtmp[1] = 128;					// start string key
      for (i=0; i<MAX_NAME_BYTES; i++)			// for each char
      { if (db_var.name.var_cu[i] == '\0')		// check for null
        { break;					// break if found
        }
        gtmp[i+2] = db_var.name.var_cu[i];		// copy char
      }
      i +=2;						// correct count
      gtmp[i] = '\0';					// null terminate
      gtmp[0] = (u_char) i;				// add the count
      s = Locate(gtmp);					// search for it
      if (s < 0)					// failed?
      { return s;					// return error
      }
      Allign_record();					// if not alligned
      *( (u_int *) record) = blk[2]->block;		// new top level blk
      if (blk[level]->dirty < (gbd *) 5)		// if it needs queing
      { blk[level]->dirty = blk[level];			// terminate list
	Queit();					// and queue it
      }
	// Now, we totally release the block at level 1 for this global
      blk[1]->mem->type = 65;				// pretend it's data
      blk[1]->last_accessed = MTIME(0);			// clear last access
#ifdef MV1_REFD
      REFD_MARK(blk[1]);
#endif
      Garbit(blk[1]->block);				// que for freeing

      bzero(&partab.jobtab->last_ref, sizeof(mvar));	// clear last ref
      return 0;						// and exit
    }
    Release_GBDs(0);
    return 0;						// just exit
  }
  blk[level + 1] = blk[level];				// save that
  s = Get_block(blk[level]->mem->right_ptr);
  if (s < 0)						// if error
  { Release_GBDs(0);
    return s;						// just exit
  }
  i = ((blk[level+1]->mem->last_free*2 + 1 - blk[level+1]->mem->last_idx)*2)
    + ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2);
  if (i < systab->ZMinSpace /*1024*/)           // if REALLY not enough space
  { level++;
    Release_GBDs(0);
    return s;						// just exit
  }
  Un_key();						// unkey RL block
  level++;						// point at left blk
  Tidy_block();						// ensure it's tidy
  Copy_data(blk[level - 1], -1);			// combine them
  if (blk[level]->dirty == (gbd *) 1)			// if not queued
  { blk[level]->dirty = (gbd *) 2;			// mark for queuing
  }
  level--;						// point at rl
  Tidy_block();						// ensure it's tidy
  if (blk[level]->mem->last_idx < LOW_INDEX)		// if it's empty
  { blk[level]->mem->type = 65;				// pretend it's data
    blk[level]->last_accessed = MTIME(0);		// clear last access
#ifdef MV1_REFD
    REFD_MARK(blk[level]);
#endif
    blk[level + 1]->mem->right_ptr = blk[level]->mem->right_ptr; // copy RL
    Garbit(blk[level]->block);				// que for freeing
    blk[level] = NULL;					// ignore

    if (blk[level + 1]->mem->right_ptr)			// if we have a RL
    { s = Get_block(blk[level + 1]->mem->right_ptr);	// get it
    }							// and hope it worked

  }
  else
  { if (blk[level]->dirty == (gbd *) 1)			// if not queued
    { blk[level]->dirty = (gbd *) 2;			// mark to que
      s = Add_rekey(blk[level]->block, level);		// que to re-key later
    }
  }

  if (blk[level] != NULL)				// if more to go
  { if (blk[level]->dirty == (gbd *) 2)			// if some left
    { chunk = (cstring *) &iidx[idx[LOW_INDEX]];	// point at the first
      bcopy(&chunk->buf[1], &partab.jobtab->last_ref.slen,
  			  chunk->buf[1]+1);		// save the real key
    }
  }
  else
  { bzero(&partab.jobtab->last_ref, sizeof(mvar));	// or clear it
  }

  level += 2;						// spare level
  blk[level] = NULL;					// clear it
  for (i = level - 1; i >= 0; i--)			// scan ptr blks
  { if (blk[i] != NULL)
    {
      if (blk[i]->dirty == (gbd *) 2)			// if changed
      { if (blk[level] == NULL)				// list empty
        { blk[i]->dirty = blk[i];			// point at self
        }
        else
        { blk[i]->dirty = blk[level];			// else point at prev
        }
        blk[level] = blk[i];				// remember this one
      }
      else if (blk[i]->dirty == (gbd *) 1)		// if reserved
      { blk[i]->dirty = NULL;				// clear it
      }
    } 
  }
  if (blk[level] != NULL)				// if something there
  { Queit();						// que that lot
  }

  return Re_key();                                      // re-key and return

}


//-----------------------------------------------------------------------------
// Function: FlushJournal
// Descript: Flush journal buffer contents
// Input(s): volume index, journal file descriptor or 0, flag an fsync()
// Return:   none
// Note:     Must be called with a write lock
//
short FlushJournal(int vol, int jfd, int dosync)
{ off_t jptr;                                           // offset
  int j;
  u_int currsize;                                       // curr. JNL buffer size
  int ggg = 0;

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  // ASSERT(NULL != systab->vol[vol]->vollab);

  if (0 == jfd)
  { if (vol)                                            // only VOL0 is open
    { if (vol + 1 > MAX_VOL) return -(ERRZ72+ERRMLAST); // must be in range
      if (partab.jnl_fds[vol] == 0)                     //   if not open
      { if (systab->vol[vol]->vollab &&                 // mounted
            (systab->vol[vol]->vollab->journal_requested) && // journal req.
            (systab->vol[vol]->vollab->journal_file[0]))
        { OpenJournal(vol, 0);
          if (systab->vol[vol]->vollab->journal_available)
            partab.jnl_fds[vol] = open(systab->vol[vol]->vollab->journal_file, 
                                                        O_RDWR);
          if (partab.jnl_fds[vol] <= 0) return -(errno+ERRMLAST+ERRZLAST);

        } 
        else
        { return -(ERRZ72+ERRMLAST);                    // give up on error
        }
      }
      else                                              // check still there
      { if ((NULL == systab->vol[vol]->vollab) ||       // volume gone
            (systab->vol[vol]->vollab->journal_file[0] == 0)) // or no jrn file
        { j = close( partab.jnl_fds[vol] );             // close the file
          partab.jnl_fds[vol] = 0;                      // flag not there
          return -(ERRZ72+ERRMLAST);                    // exit complaining
        }
      }
    }
    jfd = partab.jnl_fds[vol];
  }

  currsize = systab->vol[vol]->jrnbufsize;
  ASSERT(0 <= currsize);
  ASSERT(currsize <= systab->vol[vol]->jrnbufcap);
  if (currsize)
  { jptr = lseek(jfd, systab->vol[vol]->jrn_next,       // address to locn
                 SEEK_SET);
    if (jptr != systab->vol[vol]->jrn_next)	        // if failed
    { ggg = 1; goto fail;
    }
    j = write(jfd, systab->vol[vol]->jrnbuf, currsize);
    if (j != currsize)                                  // flush buffer
    { ggg = 2; goto fail;
    }
    systab->vol[vol]->jrn_next += currsize;	        // update next
    jptr = lseek(jfd, sizeof(u_int), SEEK_SET);
    if (jptr != sizeof(u_int))
    { ggg = 3; goto fail;
    }
    j = write(jfd, &systab->vol[vol]->jrn_next,
	          sizeof(off_t));			// write next
    if (j < 0)
    { ggg = 4; goto fail;
    }
  }
  systab->vol[vol]->jrnbufsize = 0;                     // clear buffer
  if (dosync)                                           // if requested
    fsync(jfd);                                         //   do a sync.
  return 0;

fail:
  // fprintf(stderr,"FlushJournal: ggg = %d\n",ggg);
  systab->vol[vol]->jrnbufsize = 0;                     // clear buffer
  systab->vol[vol]->vollab->journal_available = 0;      // turn it off
  close(partab.jnl_fds[vol]);			        // close the file
  return -1;
}


//-----------------------------------------------------------------------------
// Function: ClearJournal
// Descript: Create/clear the journal file
// Input(s): internal volume number
// Return:   none
// Note:     Must be called with a write lock
//
void ClearJournal(int jfd, int vol)			// clear journal
{ jrnrec jj;						// to write with
  int fd;						// file descriptor
  u_char tmp[sizeof(u_int) + sizeof(off_t)];            // was 12bytes

  if (jfd)                                              // sync prev. contents
  { FlushJournal(vol, jfd, 1);                        
  }

  fd = open(systab->vol[vol]->vollab->journal_file,
        O_TRUNC | O_RDWR | O_CREAT, 0770);		// open it
  if (fd > 0)						// if OK
  { (*(u_int *) tmp) = (MUMPS_MAGIC - 1);
    (*(off_t *) &tmp[sizeof(u_int)]) = sizeof(tmp) +    // was 20bytes
                                        MIN_JRNREC_SIZE;// next free byte
    (void)write(fd, tmp, sizeof(tmp));
    jj.action = JRN_CREATE;
    jj.time = MTIME(0);
    jj.uci = 0;
    jj.size = MIN_JRNREC_SIZE;
    (void)write(fd, &jj, MIN_JRNREC_SIZE);		// write the create rec
    (void)fchmod(fd, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP); // make grp wrt
    (void)close(fd);					// and close it
    systab->vol[vol]->jrn_next = (off_t) sizeof(tmp) +
                                        MIN_JRNREC_SIZE;// where it's upto
  }
  return;						// done
}

//-----------------------------------------------------------------------------
// Function: OpenJournal
// Descript: Open/Create journal file
// Input(s): internal volume number
// Return:   none, but sets vollab->journal_available = 1, when succeeds
// Note:     Must be called with a write lock
//
void OpenJournal(int vol, int printlog)
{ int i;                                                // handy int
  int jfd = -1;                                         // file descriptor
  struct stat sb;
  off_t jptr;
  jrnrec jj;

  systab->vol[vol]->vollab->journal_available = 0;      // assume fail
  i = stat(systab->vol[vol]->vollab->journal_file, &sb );// check for file
  if ((i < 0) && (errno != ENOENT))		        // if that's junk
  { if (printlog)
      fprintf(stderr, "Failed to access journal file %s\n",
  		  systab->vol[vol]->vollab->journal_file);
  }
  else					                // do something
  { if (i < 0)				                // if doesn't exist
    { ClearJournal(0, vol);			        // create it
    }						        // end create code
    jfd = open(systab->vol[vol]->vollab->journal_file, O_RDWR);
    if (jfd < 0)				        // on fail
    { if (printlog)
        fprintf(stderr, "Failed to open journal file %s\nerrno = %d\n",
                  systab->vol[vol]->vollab->journal_file, errno);
    }
    else					        // if open OK
    { u_char tmp[sizeof(u_int) + sizeof(off_t)];

#ifdef MV1_F_NOCACHE
      i = fcntl(jfd, F_NOCACHE, 1);
#endif
      lseek(jfd, 0, SEEK_SET);
      errno = 0;
      i = read(jfd, tmp, sizeof(u_int));	        // read the magic
      if ((i != sizeof(u_int)) || (*(u_int *) tmp != (MUMPS_MAGIC - 1)))
      { if (printlog)
          fprintf(stderr, "Failed to open journal file %s\nWRONG MAGIC\n",
                    systab->vol[vol]->vollab->journal_file);
        close(jfd);
      }
      else
      { i = read(jfd, &systab->vol[vol]->jrn_next, sizeof(off_t));
	if (i != sizeof(off_t))
	{ if (printlog)
            fprintf(stderr, "Failed to use journal file %s\nRead failed - %d\n",
		      systab->vol[vol]->vollab->journal_file, errno);
	  close(jfd);
	}
	else
	{ jptr = lseek(jfd, systab->vol[vol]->jrn_next, SEEK_SET);
	  if (jptr != systab->vol[vol]->jrn_next)
	  { if (printlog)
              fprintf(stderr, "Failed journal file %s\nlseek failed - %d\n",
		        systab->vol[vol]->vollab->journal_file, errno);
	    close(jfd);
	  }
	  else
	  { jj.action = JRN_START;
	    jj.time = MTIME(0);
	    jj.uci = 0;
	    jj.size = MIN_JRNREC_SIZE;
	    i = write(jfd, &jj,                         // write the create rec.
                                MIN_JRNREC_SIZE);
	    systab->vol[vol]->jrn_next += 
                                MIN_JRNREC_SIZE;        // adjust pointer
	    lseek(jfd, sizeof(u_int), SEEK_SET);
	    i = write(jfd, &systab->vol[vol]->jrn_next, sizeof(off_t));
	    i = close(jfd);			        // and close it
	    systab->vol[vol]->vollab->journal_available = 1;
            if (printlog)                               // say it worked
	      fprintf(stderr, "Journaling started to %s.\n",
		        systab->vol[vol]->vollab->journal_file); 
	  }
	}
      }
    }
  }
}

//-----------------------------------------------------------------------------
// Function: DoJournal
// Descript: Write a journal record
// Input(s): jrnrec structure
//	     data pointer (set only)
// Return:   none
// Note:     Must be called with a write lock
//	     the date/time and size are filled in here
//
void DoJournal(jrnrec *jj, cstring *data) 		// Write journal
{ int i;                                                // handy int
  int jj_alignment;                                     // align. to 4byte bound
  u_int currsize;                                       // curr. JNL buffer size
  short s;

  currsize = systab->vol[volnum - 1]->jrnbufsize;       // get current size
  jj->time = MTIME(0);					// store the time
  jj->size = sizeof(u_short) + 2 * sizeof(u_char) + sizeof(time_t) + sizeof(var_u) + sizeof(u_char) + jj->slen;
  if ((jj->action != JRN_SET) && (jj->action != JRN_KILL)) // not SET of KILL
  { jj->size = MIN_JRNREC_SIZE;				// size is min.
  }
  i = jj->size;
  if (jj->action == JRN_SET)                            // add data length
  { jj->size += (sizeof(short) + data->len);
  }
  jj_alignment = 0;                                     // calc. aligment
  if (jj->size & 3)
  { jj_alignment = (4 - (jj->size & 3));		// round it
  }
  if (currsize + jj->size + jj_alignment >              // does not fit
                            systab->vol[volnum - 1]->jrnbufcap)
  { s = FlushJournal(volnum - 1, 0,                     // flush buffer
                            systab->vol[volnum - 1]->syncjrn); 
    if (s < 0)
      return;
    currsize = systab->vol[volnum - 1]->jrnbufsize;     // init currsize again
  }
  bcopy(jj, systab->vol[volnum - 1]->jrnbuf + currsize, i); // copy buffer
  currsize += i;
  if (jj->action == JRN_SET)                            // copy data
  { i = sizeof(short) + data->len;
    bcopy(data, systab->vol[volnum - 1]->jrnbuf + currsize, i);
    currsize += i;
  }
  if (jj_alignment)
    currsize += jj_alignment;
  systab->vol[volnum - 1]->jrnbufsize = currsize;       // update systab
  systab->vol[volnum - 1]->lastdojrn  = MTIME(0);       // save time
  return;

fail:
  systab->vol[volnum - 1]->jrnbufsize = 0;              // clear buffer
  systab->vol[volnum - 1]->vollab->journal_available = 0; // turn it off
  close(partab.jnl_fds[volnum - 1]);			// close the file
  return;						// and exit
}

u_int SleepEx(u_int seconds, const char* file, int line)
{
  fprintf(stderr,"%s:%d: curr_lock=%d sleep=%d #dQ=%d\r\n",
           file, line, curr_lock, seconds,
#ifdef MV1_CKIT
           NUM_DIRTY - ck_ring_size(&systab->vol[volnum-1]->dirtyQ) - 1
#else
           abs(systab->vol[volnum - 1]->dirtyQw - systab->vol[volnum - 1]->dirtyQr)
#endif
  );
  fflush(stderr);
  return MSleep(1000 * seconds);
}

int MSleep(u_int mseconds)
{ int ret;

  while (mseconds > 500)                                // while more than 500ms
  { mseconds -= 500;                                    // reduce mseconds
    systab->Mtime = time(0);                            // keep track of M time
    ret = usleep(1000 * 500);                           // sleep a bit
  }
  if (mseconds)                                         // any remains ?
  { systab->Mtime = time(0);                            // keep track of M time
    ret = usleep(1000 * mseconds);                      // sleep a bit
  }
  return ret;
}

#define DQ_SLEEP        10

void Ensure_GBDs(int haslock)
{
  int j;
  int qpos, wpos, rpos, qlen, qfree;
  int MinSlots;
  int pass = 0;

start:
  Get_GBDsEx(MAXTREEDEPTH * 2, haslock);		// ensure this many
  haslock = 0;                                          // clear for next turns

  j = 0;                                                // clear counter
  MinSlots = 2 * MAXTREEDEPTH;
#ifdef MV1_CKIT
  qfree = NUM_DIRTY - ck_ring_size(&systab->vol[volnum-1]->dirtyQ) - 1;
#else
  wpos = systab->vol[volnum - 1]->dirtyQw;
  rpos = systab->vol[volnum - 1]->dirtyQr;
  if (rpos <= wpos) qlen = wpos - rpos;
  else
    qlen = NUM_DIRTY + wpos - rpos;
  qfree = NUM_DIRTY - qlen;
#endif
  if (qfree >= MinSlots)
    goto cont;

  SemOp( SEM_GLOBAL, -curr_lock);			// release current lock

  // Sleep(1);
  if (pass & 3)
    SchedYield();
  else
  { ATOMIC_INCREMENT(systab->vol[volnum -1]->stats.dqstall);// count dirtQ stall
    MSleep(DQ_SLEEP);
  }
  pass++;
  goto start;

cont:
  return;
}


short Check_BlockNo(int vol, u_int blkno, int checks,
                        char *where, char *file, int lno, int dopanic)
{
  char msg[128];
  u_char *bitmap = (u_char *) systab->vol[vol]->map;
  int failed = 0;

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  if (checks & CBN_INRANGE)                             // out of range ?       
    failed = failed || (blkno > systab->vol[vol]->vollab->max_block);
  if (checks & CBN_ALLOCATED)                           // unallocated ?
    failed = failed || (0 == (bitmap[blkno >> 3] & (1 << (blkno & 7))));

  if (!dopanic)                                         // just check ?
  { return failed ? -1 : 0;                             //   return condition
  }

  if (failed)                                           // panic when failed
  { if (file)
      sprintf((char *) msg, "%s(%s:%d): invalid block (%d:%u) in file %s!!",
                                where, file, lno,
                                vol, blkno, 
                                systab->vol[vol]->file_name);
    else
      sprintf((char *) msg, "%s(): invalid block (%d:%u) in file %s!!",
                                where, vol, blkno, 
                                systab->vol[vol]->file_name);
    panic((char *) msg);
  }
  return 0;
}

int DirtyQ_Len(void)
{ int qlen = 0;

#ifdef MV1_CKIT
  qlen = ck_ring_size(&systab->vol[0]->dirtyQ);
#else
  int qpos, wpos, rpos;

  wpos = systab->vol[0]->dirtyQw;
  rpos = systab->vol[0]->dirtyQr;
  if (rpos <= wpos)
    qlen = wpos - rpos;
  else
    qlen = NUM_DIRTY + wpos - rpos;
#endif

  return qlen;
}

u_int DB_GetDirty(int vol)
{ int i, num_gbd;
  u_int cnt;

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  num_gbd = systab->vol[vol]->num_gbd;
  cnt = 0;
  for (i = 0; i < num_gbd; i++)
     if (systab->vol[vol]->gbd_head[i].dirty)
       cnt++;

  return cnt;
}

