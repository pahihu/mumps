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


#include <stdio.h>                                      // always include
#include <stdlib.h>                                     // these two
#include <string.h>                                     // for bcopy
#include <strings.h>
#include <unistd.h>                                     // for file reading
#include <time.h>                                       // for gbd stuff
#include <errno.h>                                      // for errno
#include <fcntl.h>                                      // for expand
#include <ctype.h>                                      // for gbd stuff
#include <sys/types.h>                                  // for semaphores
#include <sys/ipc.h>                                    // for semaphores
#include <sys/sem.h>                                    // for semaphores
#include <sys/stat.h>                                   // for fchmod
#include "mumps.h"                                      // standard includes
#include "database.h"                                   // database protos
#include "proto.h"                                      // standard prototypes
#include "error.h"                                      // error strings

//-----------------------------------------------------------------------------
// Function: Insert
// Descript: Insert the supplied key and data in blk[level]
// Input(s): Pointer the the key and data to insert
// Return:   String length -> Ok, negative MUMPS error -(ERRMLAST+ERRZ62)
//

short Insert(u_char *key, cstring *data)		// insert a node
{ int i;                                                // a handy int
  int isdata;                                           // data/ptr flag
  int rs;                                               // required size
  short s;                                              // for funcs
  u_char ccc;                                           // common char count
  u_char ucc;                                           // uncommon char count
  u_int flags = 0;                                      // for $GLOBAL

  isdata = ((blk[level]->mem->type > 64) &&             // data block and
            (level));                                   // not the directory

  if (blk[level]->mem->last_idx > LOW_INDEX - 1)        // if some data
  { s = Locate(key);                                  	// search for it
    if (s >= 0)                                       	// if found
    { return -(ERRMLAST+ERRZ61);                      	// database stuffed
    }
    else if (s != -ERRM7)                             	// for any other error
    { return s;                                       	// exit
    }
  }
  else                                                  // empty block
  { Index = LOW_INDEX;                                  // start
    idx = (u_short *) blk[level]->mem;                  // point at the block
    iidx = (int *) blk[level]->mem;                     // point at the block
  }
  if (!level)                                           // insert in GD
  { chunk = (cstring *) &iidx[idx[LOW_INDEX]];          // point at $G chunk
    record = (cstring *) &chunk->buf[chunk->buf[1] + 2];
    Allign_record();                                    // allign it
    flags = ((u_int *) record)[1];                      // get default flags
    partab.jobtab->last_block_flags[volnum - 1] = flags;
  }

  keybuf[0] = 0;                                      	// clear keybuf
#ifdef MV1_CCC
  Build_KeyBuf(Index - 1, &keybuf[0], KEY_COPY);        // rebuild keybuf[]
#else
  Build_KeyBuf(Index - 1, &keybuf[0], KEY_COPY);
#endif

  ccc = 0;            // start here
#ifdef MV1_CCC_DOCOMP
  if ((key[0]) && (keybuf[0]))                          //   and any there
  { while (key[ccc + 1] == keybuf[ccc + 1])             // while the same
    { if ((ccc == key[0]) || (ccc == keybuf[0]))        // at end of either
      { break;                                          // done
      }
      ccc++;                                            // increment ptr
    }
  }
#endif
  ucc = key[0] - ccc;                                   // and this
  rs = sizeof(short) + 2                                // chunksiz + ccc + ucc
         + ucc + data->len;                             // + key + data
  if (isdata)                                           // if it's a data blk
  { rs += sizeof(short);                                // add the dbc size
  }
  else if (!level)                                      // if GD
  { rs += 4;                                            // allow for flags
  }
  rs = ((rs + 3) >> 2) << 2;				// round it up
  rs += 4;                                              // allow for the Index

  if (rs > ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
  { if (!(blk[level]->mem->flags & BLOCK_DIRTY))        // if block is clean
    { return -(ERRMLAST+ERRZ62);                        // say no room
    }
    Tidy_block();                                       // tidy it
    if (rs > ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
    { return -(ERRMLAST+ERRZ62);                        // say no room
    }
  }                                                     // it will now fit
  rs -= 4;                                              // rs now chunksize

  for (i = blk[level]->mem->last_idx; i>=Index; i--)    // the trailing ones
  { idx[i + 1] = idx[i];                                // get copied down
  }
  idx[Index] = blk[level]->mem->last_free - (rs / 4) + 1; // where it goes
  chunk = (cstring *) &iidx[idx[Index]];                // as an address
  record = (cstring *) &chunk->buf[ucc + 2];            // where the data goes
  chunk->len = rs;                                      // store chunk size
  chunk->buf[0] = ccc;                                  // this bit
  chunk->buf[1] = ucc;                                  // then this
  bcopy(&key[ccc + 1], &chunk->buf[2], ucc);            // then the key bit
  if (isdata)                                           // for data blk
  { record->len = data->len;                            // copy the length
    bcopy(data->buf, record->buf, data->len);           // then the data
  }
  else                                                  // it's a pointer
  { Allign_record();                                    // allign it
    bcopy(data->buf, record, sizeof(int));              // the block number
    if (!level)                                         // if GD
    { ((u_int *) record)[1] = flags;                    // set/clear the flags
    }
  }
  blk[level]->mem->last_free -= (rs / 4);               // redo last_free
  blk[level]->mem->last_idx++;                          // add to the index
  blk[level]->mem->flags |= BLOCK_DIRTY;                // mark dirty

#ifdef MV1_LOCATE_DEBUG
  LocateCountP(blk[level],key,__FILE__,__LINE__);
#endif

  return 0;                                             // done
}

//-----------------------------------------------------------------------------
// Function: Mark_changes
// Descript: Mark the specified block as changed
// Input(s): vol - volume index, block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Mark_changes(int vol, int blknum)                	// mark changes
{ u_int off;                                            // chunk offset
  u_int msk;						// bit mask

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted
  ASSERT(blknum < systab->vol[vol]->vollab->max_block);

  off = blknum / (8*sizeof(u_char));			// offset
  msk = 1 << (blknum & (8*sizeof(u_char) - 1));		// bit mask

  ASSERT(0 <= off);
  ASSERT(off < 
	(systab->vol[vol]->vollab->header_bytes - SIZEOF_LABEL_BLOCK) / 
 	 sizeof(u_char));

  if (0 == (systab->vol[vol]->chgmap[off] & msk))	// was off ?
  { systab->vol[vol]->blkchanged++;			//   count as changed
  }
  systab->vol[vol]->chgmap[off] |=msk;

  return;                                               // and exit
}

//-----------------------------------------------------------------------------
// Function: Queit
// Descript: Que the gbd p_gbd - links already setup
// Input(s): the gbd to queue
// Return:   0 - on success, 1 - on failed (queue empty)
// Note:     Must hold a write lock before calling this function
//

void Queit(void)  					// que a gbd for write
{ gbd *ptr;                                       	// a handy ptr
#ifdef MV1_CKIT
  bool result;
#else
  int i;                                                // a handy int
#endif

  ASSERT(0 < volnum);                                   // valid volnum
  ASSERT(volnum <= MAX_VOL);
  ASSERT(NULL != systab->vol[volnum-1]->vollab);        // mounted

  // LastBlock = 0;                                     // zot Locate() cache
  ptr = blk[level];                                     // point at the block
  // fprintf(stderr,"Queit: %d",ptr->block);
  systab->vol[volnum-1]->stats.logwt++;                 // incr logical
  if (systab->vol[volnum-1]->track_changes)		// track changes ?
  { Mark_changes(volnum-1, ptr->block);			//   mark blk as changed
  }
#ifdef MV1_LOCATE_DEBUG
  LocateAllP(ptr,level,__FILE__,__LINE__);
#endif
  while (ptr->dirty != ptr)                             // check it
  { ptr = ptr->dirty;                                   // point at next
    // fprintf(stderr," %d",ptr->block);
    systab->vol[volnum-1]->stats.logwt++;               // incr logical
    if (systab->vol[volnum-1]->track_changes)		// track changes ?
    { Mark_changes(volnum-1, ptr->block);		//   mark blk as changed
    }
#ifdef MV1_LOCATE_DEBUG
    LocateAllP(ptr,-1,__FILE__,__LINE__);
#endif
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
  i = systab->vol[0]->dirtyQw;                          // where to put it
  if (systab->vol[0]->dirtyQ[i] != NULL)
  { panic("Queit(): dirtyQ overflow");
  }
  systab->vol[0]->dirtyQ[i] = blk[level];               // stuff it in
  systab->vol[0]->dirtyQw = (i + 1) & (NUM_DIRTY - 1);  // reset ptr
#endif

  return;                                               // and exit
}

//-----------------------------------------------------------------------------
// Function: Garbit
// Descript: Que the block passed in for garbage collection
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Garbit(int blknum)                                 // que a blk for garb
{ 
#ifdef MV1_CKIT
  void *qentry;                                         // queue entry
  bool result;                                          // ck result
#else
  int i;                                                // a handy int
#endif

  ASSERT(0 < volnum);                                   // valid vol[] index
  ASSERT(volnum <= MAX_VOL);
  ASSERT(NULL != systab->vol[volnum-1]->vollab);        // mounted
  ASSERT(0 == Check_BlockMapped(volnum-1, blknum));     // mapped

  // MV1DBG(fprintf(stderr,"\r\nGarbit: %d\r\n",blknum));

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
  i = systab->vol[0]->garbQw;                           // where to put it
  if (systab->vol[0]->garbQ[i] != 0)
  { panic("Garbit(): garbQ overflow");
  }
  systab->vol[0]->garbQ[i] = blknum;                    // stuff it in
  systab->vol[0]->garbQw = (i + 1) & (NUM_GARB - 1);    // reset ptr
#endif
  return;                                               // and exit
}

//-----------------------------------------------------------------------------
// Function: Free_block
// Descript: Remove the specified block from the map
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Free_block(int vol, int blknum)                    // free blk in map
{ int i;                                                // a handy int
  int off;                                              // and another
  u_char *map;                                          // map pointer

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  MEM_BARRIER;
  map = ((u_char *) systab->vol[vol]->map);             // point at it
  i = blknum >> 3;                                      // map byte
  off = blknum & 7;                                     // bit number
  off = 1 << off;                                       // convert to mask
  if ((map[i] & off) == 0)                              // if it's already free
  { return;                                             // just exit
  }
  ATOMIC_INCREMENT(systab->vol[vol]->stats.blkdeall);   // update stats
  map[i] &= ~off;                                       // clear the bit
  if (systab->vol[vol]->first_free > (void *) &map[i])  // if earlier
  { systab->vol[vol]->first_free = &map[i];             // reset first free
  }
  MEM_BARRIER;
  Mark_map_dirty(vol, blknum);
  return;                                               // and exit
}

//-----------------------------------------------------------------------------
// Function: Used_block
// Descript: Add the specified block to the map
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//           The caller must have ensured that, if there is a map
//           scan in progress, this block is less than "upto".
//           This is only called from database/db_view.c
//

void Used_block(int vol, int blknum)                    // set blk in map
{ int i;                                                // a handy int
  int off;                                              // and another
  u_char *map;                                          // map pointer

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             //  mounted

  MEM_BARRIER;
  map = ((u_char *) systab->vol[vol]->map);             // point at it
  i = blknum >> 3;                                      // map byte
  off = blknum & 7;                                     // bit number
  off = 1 << off;                                       // convert to mask
  if ((map[i] & off))                                   // if it's already used
  { return;                                             // just exit
  }
  ATOMIC_INCREMENT(systab->vol[vol]->stats.blkalloc);   // update stats
  map[i] |= off;                                        // set the bit
  MEM_BARRIER;
  Mark_map_dirty(vol, blknum);
  return;                                               // and exit
}

//-----------------------------------------------------------------------------
// Function: Tidy_block
// Descript: Tidy the current block
// Input(s): none
// Return:   none
// Note:     Must hold a write lock before calling this function
//       This function ommits records with dbc = NODE_UNDEFINED
//       This function ommits pointers with record = PTR_UNDEFINED
//

void Tidy_block(void)					// tidy current blk
{ gbd *ptr;                                             // a handy pointer
  DB_Block *btmp;                                       // ditto

  ptr = blk[level];                                     // remember current
  Get_GBD();                                            // get another
  ASSERT(blk[level] != ptr);
  bzero(blk[level]->mem, systab->vol[volnum-1]->vollab->block_size); // zot
  blk[level]->mem->type = ptr->mem->type;               // copy type

  if (!level)                                           // if it's a GD
  { blk[level]->mem->type |= 64;                        // ensure it's data
  }

  blk[level]->mem->right_ptr = ptr->mem->right_ptr;     // copy RL
  blk[level]->mem->global = ptr->mem->global;           // copy global name
  blk[level]->mem->last_idx = LOW_INDEX - 1;            // unused block
  blk[level]->mem->last_free
        = (systab->vol[volnum-1]->vollab->block_size >> 2) - 1; // set this up
  Copy_data(ptr, LOW_INDEX);                            // copy entire block
  btmp = blk[level]->mem;                               // save this
  blk[level]->mem = ptr->mem;                           // copy in this
  ptr->mem = btmp;                                      // end swap 'mem'

  Free_GBD(volnum-1, blk[level]);                       // release it
  blk[level] = ptr;                                     // restore the ptr
  idx = (u_short *) blk[level]->mem;                    // set this up
  iidx = (int *) blk[level]->mem;                       // and this

#ifdef MV1_LOCATE_DEBUG
  LocateAllP(blk[level],level,__FILE__,__LINE__);
#endif

  return;                                               // and exit
}

//-----------------------------------------------------------------------------
// Function: Mark_map_dirty
// Descript: Mark the specified 4K chunk of map as dirty
// Input(s): vol - volume index, block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Mark_map_dirty(int vol, int blknum)                // mark map dirty
{ int off;                                              // chunk offset

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  // NB. map is written in 4K chunks, each chunk holds
  //     data for 4K*8 blocks, each dirty_map_blocks[]
  //     entry contains a bitmap for 32 chunks.
  off = blknum >> 15;

  ASSERT((off >> 5) < MAX_MAP_CHUNKS);                  // offset should fit
  systab->vol[vol]->map_chunks[off >> 5] |= (1U << (off & 31)); // mark chunk
  systab->vol[vol]->map_dirty_flag++;                   // mark map dirty
  return;                                               // and exit
}

//-----------------------------------------------------------------------------
// Function: Copy_data
// Descript: Copy data from "from" to blk[level]
// Input(s): from GBD and index (or flag)
// Return:   none
// Note:     Must hold a write lock before calling this function
//           All external variables describing blk[level] must be setup
//           This function ommits records with dbc = NODE_UNDEFINED
//           This function ommits pointers with record = PTR_UNDEFINED
//

void Copy_data(gbd *fptr, int fidx)			// copy records
{ int i;                                                // a handy int
  u_short *sfidx;                                       // for Indexes
  int *fiidx;                                           // int ver of Index
#ifdef MV1_CCC
  u_char fk[260];                                       // for keys
#else
  u_char *fk, *sav_fk;                               	// for keys
#endif
  int isdata;                                           // a flag
  cstring *c;                                           // reading from old
  u_char ccc;                                           // common char count
  u_char ucc;                                           // uncommon char count
  short cs;                                             // new chunk size

  isdata = ((blk[level]->mem->type > 64) && (level));   // block type
  sfidx = (u_short *) fptr->mem;                        // point at it
  fiidx = (int *) fptr->mem;                            // point at it

  keybuf[0] = 0;                                        // clear this
#ifdef MV1_CCC
  Build_KeyBuf(blk[level]->mem->last_idx, 		// update keybuf[]
	       &keybuf[0], KEY_COPY);
#else
  Build_KeyBuf(blk[level]->mem->last_idx,
	       &keybuf[0], KEY_COPY);
  sav_fk = 0;						// full key to save
#endif

  for (i = LOW_INDEX; i <= fptr->mem->last_idx; i++)    // for each Index
  { c = (cstring *) &fiidx[sfidx[i]];                   // point at chunk
#ifdef MV1_CCC
    bcopy(&c->buf[2], &fk[c->buf[0] + 1], c->buf[1]);   // copy key
    fk[0] = c->buf[0] + c->buf[1];                      // and the length
#else
    fk = &c->buf[1];					// point at the key
#endif
    if (i < fidx)                                       // copy this one
    { continue;                                         // no - just continue
    }
    c = (cstring *) &(c->buf[c->buf[1] + 2]);           // point at dbc/ptr
    if (isdata)                                         // if data
    { if (c->len == NODE_UNDEFINED)                     // junk record?
      { continue;                                       // ignore it
      }
    }
    else                                                // if a pointer
    { if ((long) c & 3)                                 // if not alligned
      { c = (cstring *) &c->buf[2 - ((long) c & 3)];    // allign
      }
      if ((*(int *) c) == PTR_UNDEFINED)                // see if that's junk
      { continue;                                       // ignore it
      }
    }
    ccc = 0;                                            // start here
#ifdef MV1_CCC_DOCOMP
    if ((fk[0]) && (keybuf[0]))                         // and if any there
    { while (fk[ccc + 1] == keybuf[ccc + 1])            // while the same
      { if ((ccc == fk[0]) || (ccc == keybuf[0]))       // at end of either
        { break;                                        // done
        }
        ccc++;                                          // increment ptr
      }
    }
#endif
    ucc = fk[0] - ccc;                                  // get the ucc
    cs = 4 + ucc + (isdata ? (c->len + 2) : 4);         // chunk size = this
    if (!level)                                         // if GD
    { cs += 4;                                          // allow for flags
    }
    cs = ((cs + 3) >> 2) << 2;				// round it up

    if (cs >=
        ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
    { if (fidx == -1)
      { goto Lreturn;
      }
      panic("Copy_data: about to overflow block");
    }

    blk[level]->mem->last_free -= (cs / 4);             // reset free
    idx[++blk[level]->mem->last_idx]
      = blk[level]->mem->last_free + 1;                 // point at next chunk
    chunk = (cstring *) &iidx[blk[level]->mem->last_free + 1];
    chunk->len = cs;                                    // set the size
    chunk->buf[0] = ccc;                                // ccc
    chunk->buf[1] = ucc;                                // ucc
    bcopy(&fk[ccc + 1], &chunk->buf[2], ucc);           // the key
    record = (cstring *) &chunk->buf[ucc + 2];          // point at dbc/ptr
    if (isdata)                                         // for a data block
    { record->len = c->len;                             // copy dbc
      bcopy(c->buf, record->buf, c->len);               // copy the data
      if (fidx == -1)
      { c->len = NODE_UNDEFINED;
      }
    }
    else                                                // for a pointer
    { Allign_record();                                  // ensure alligned
      *(u_int *) record = *(u_int *) c;                 // copy ptr
      // === ASSERT(0 == Check_BlockMapped(volnum - 1, *(u_int *) c)); ===
      if (fidx == -1)
      { *(int *) c = PTR_UNDEFINED;
      }
      if (!level)                                       // if GD
      { ((u_int *) record)[1] = ((u_int *) c)[1] & GL_FLAGS; // copy flags
      }
    }
#ifdef MV1_CCC
    bcopy(fk, keybuf, fk[0] + 1);                     	// save full key
#else
    sav_fk = fk;					// save full key ptr
#endif
  }                                                     // end copy loop
Lreturn:
#ifndef MV1_CCC
  if (sav_fk)
    bcopy(sav_fk, keybuf, sav_fk[0] + 1);         	// save full key
#endif

#ifdef MV1_LOCATE_DEBUG
  LocateAllP(blk[level],level,__PATH__,__LINE__);
#endif

  return;                                               // and exit
}


//-----------------------------------------------------------------------------
// Function: Allign_record
// Descript: Ensure that record is on a four byte boundary
// Input(s): none
// Return:   none
// Note:     Must only be called for pointer/directory blocks
//

void Allign_record()                                  	// allign record (int)
{ if ((long) record & 3)                                // if not alligned
  { record = (cstring *) &record->buf[2 - ((long) record & 3)]; // allign
  }
  return;                                               // exit
}

//-----------------------------------------------------------------------------
// Function: Compress1
// Descript: Compress one block union
// Input(s): mvar * to the key to find
//       the level to operate at
// Return:   zero or error
//

short Compress1()
{ int i;
  int curlevel;
  short s;
  u_char gtmp[2*MAX_NAME_BYTES];                        // to find glob

  writing = 1;                                          // flag writing
  Ensure_GBDs(0);                                       // ensure this many

  curlevel = level;
  s = Get_data(curlevel);                               // get the data
  if ((s == -ERRM7) && (!db_var.slen))                  // if top
  { s = 0;                                              // it does exist
  }
  if (s == -ERRM7)                                      // if gone missing
  { Release_GBDs(0); 
    return 0;                                           // just exit
  }
  if (s < 0)                                            // any other error
  { return s;                                           // return it
  }
  if (!blk[level]->mem->right_ptr)                      // if no more blocks
  { if ((level == 2) && (!db_var.slen))                 // and blk 1 on level 2
    { level = 0;                                        // look at the GD
      gtmp[1] = 128;                                    // start string key
      for (i=0; i<MAX_NAME_BYTES; i++)                  // for each char
      { if (db_var.name.var_cu[i] == '\0')              // check for null
        { break;                                        // break if found
        }
        gtmp[i+2] = db_var.name.var_cu[i];              // copy char
      }
      i +=2;                                            // correct count
      gtmp[i] = '\0';                                   // null terminate
      gtmp[0] = (u_char) i;                             // add the count
      s = Locate(gtmp);                                 // search for it
      if (s < 0)                                        // failed?
      { return s;                                       // return error
      }
      Allign_record();                                  // if not alligned
      *( (u_int *) record) = blk[2]->block;             // new top level blk
      ASSERT(0 == Check_BlockMapped(volnum - 1, blk[2]->block));
      if (blk[level]->dirty < (gbd *) 5)                // if it needs queing
      { blk[level]->dirty = blk[level];                 // terminate list
	TXSET(blk[level]);
        Queit();                                       	// and queue it
      }
      // Now, we totally release the block at level 1 for this global
      blk[1]->mem->type = 65;                           // pretend it's data
      blk[1]->last_accessed = MTIME(0);                 // clear last access
#ifdef MV1_REFD
      REFD_MARK(blk[1]);
#endif
      Garbit(blk[1]->block);                            // que for freeing

      bzero(&partab.jobtab->last_ref, sizeof(mvar));    // clear last ref
      return 0;                                         // and exit
    }
    Release_GBDs(0);
    return 0;                                           // just exit
  }
  blk[level + 1] = blk[level];                          // save that
  s = Get_block(blk[level]->mem->right_ptr);
  if (s < 0)                                            // if error
  { Release_GBDs(0);
    return s;                                           // just exit
  }
  i = ((blk[level+1]->mem->last_free*2 + 1 - blk[level+1]->mem->last_idx)*2)
    + ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2);
  if (i < systab->ZMinSpace /*1024*/)           // if REALLY not enough space
  { level++;
    Release_GBDs(0);
    return s;                                           // just exit
  }
  Un_key();                                             // unkey RL block
  level++;                                              // point at left blk
  Tidy_block();                                         // ensure it's tidy
  Copy_data(blk[level - 1], -1);                        // combine them
  if (blk[level]->dirty == (gbd *) 1)                   // if not queued
  { blk[level]->dirty = (gbd *) 2;                      // mark for queuing
  }
  level--;                                              // point at rl
  Tidy_block();                                         // ensure it's tidy
  if (blk[level]->mem->last_idx < LOW_INDEX)            // if it's empty
  { blk[level]->mem->type = 65;                         // pretend it's data
    blk[level]->last_accessed = MTIME(0);               // clear last access
#ifdef MV1_REFD
    REFD_MARK(blk[level]);
#endif
    ASSERT(X_EQ(blk[level + 1]->mem->global, blk[level]->mem->global));
    blk[level + 1]->mem->right_ptr = blk[level]->mem->right_ptr; // copy RL
    Garbit(blk[level]->block);                          // que for freeing
    blk[level] = NULL;                                  // ignore

    if (blk[level + 1]->mem->right_ptr)                 // if we have a RL
    { s = Get_block(blk[level + 1]->mem->right_ptr);    // get it
    }                                                   // and hope it worked

  }
  else
  { if (blk[level]->dirty == (gbd *) 1)                 // if not queued
    { blk[level]->dirty = (gbd *) 2;                    // mark to que
      s = Add_rekey(blk[level]->block, level);          // que to re-key later
    }
  }

  if (blk[level] != NULL)                               // if more to go
  { if (blk[level]->dirty == (gbd *) 2)                 // if some left
    { chunk = (cstring *) &iidx[idx[LOW_INDEX]];        // point at the first
      bcopy(&chunk->buf[1], &partab.jobtab->last_ref.slen,
          chunk->buf[1]+1);                             // save the real key
    }
  }
  else
  { bzero(&partab.jobtab->last_ref, sizeof(mvar));      // or clear it
  }

  level += 2;                                           // spare level
  blk[level] = NULL;                                    // clear it
  for (i = level - 1; i >= 0; i--)                      // scan ptr blks
  { if (blk[i] != NULL)
    {
      if (blk[i]->dirty == (gbd *) 2)                   // if changed
      { if (blk[level] == NULL)                         // list empty
        { blk[i]->dirty = blk[i];                       // point at self
	  TXSET(blk[i]);
        }
        else
        { blk[i]->dirty = blk[level];                   // else point at prev
	  TXSET(blk[i]);
        }
        blk[level] = blk[i];                            // remember this one
      }
      else if (blk[i]->dirty == (gbd *) 1)              // if reserved
      { blk[i]->dirty = NULL;                           // clear it
      }
    } 
  }
  if (blk[level] != NULL)                               // if something there
  { Queit();                                           	// que that lot
  }

  return Re_key();                                      // re-key and return

}


//------------------------------------------------------------------------------
// Function: attach_jrn
// Descript: Attach to the jrn file of the given volume.
//           The jrn file must be initialized.
// Input(s): vol - the volume index
// Return:   jrn file descriptor, or 0 > MUMPS error code
//

int attach_jrn(int vol, int *jnl_fds, u_char *jnl_seq)
{ int j;                                                // handy int

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(0 == systab->vol[vol]->local_name[0]);		// not remote VOL
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  if (jnl_fds[vol] && 					// already open ?
      (jnl_seq[vol] == systab->vol[vol]->jnl_seq))      //   AND same seq ?
    return jnl_fds[vol];                                //   return file desc.

  if (jnl_fds[vol])					// jrn file changed
  { close(jnl_fds[vol]);				//   close old fd
    jnl_fds[vol] = 0;					// clear jrn fds
    jnl_seq[vol] = 0;					//   AND jrn file seq
  }

  if ((systab->vol[vol]->vollab->journal_available) &&
      (systab->vol[vol]->vollab->journal_requested) &&
         (systab->vol[vol]->vollab->journal_file[0]))
  { struct stat   sb;				        // File attributes
    int jfd;					        // file descriptor
    u_char tmp[sizeof(u_int) + sizeof(off_t)];

    j = stat(systab->vol[vol]->vollab->journal_file, &sb ); // check for file
    if (j < 0)		                                // if that's junk
    { return -(errno+ERRMLAST+ERRZLAST);
    }
    jfd = open(systab->vol[vol]->vollab->journal_file, O_RDWR);
    if (jfd < 0)				        // on fail
    { return (-errno+ERRMLAST+ERRZLAST);
    }

#ifdef MV1_F_NOCACHE
    j = fcntl(jfd, F_NOCACHE, 1);
#endif
    lseek(jfd, 0, SEEK_SET);
    errno = 0;
    j = read(jfd, tmp, sizeof(u_int));	        	// read the magic
    if ((j != sizeof(u_int)) || (*(u_int *) tmp != (MUMPS_MAGIC - 1)))
    { return -(EINVAL+ERRMLAST+ERRZLAST);
    }
    jnl_fds[vol] = jfd;					// save jrn fd
    jnl_seq[vol] = systab->vol[vol]->jnl_seq;		//   AND jrn seq
  }
  return jnl_fds[vol];
}

//-----------------------------------------------------------------------------
// Function: AttachJournal
// Descript: Updates the journal file descriptor in partab
// Input(s): volume index
// Return:   none
// Note:     Must be called with a write lock
//
int AttachJournal(int vol)
{ int j;						// a handy int

  if (vol + 1 > MAX_VOL) return -(ERRZ72+ERRMLAST);     // must be in range
                                                        // chek still there
  if ((NULL == systab->vol[vol]->vollab) ||             // volume gone
      (systab->vol[vol]->vollab->journal_file[0] == 0)) // or no jrn file
  { j = close( partab.jnl_fds[vol] );                   // close the file
    partab.jnl_fds[vol] = 0;                            // flag not there
    partab.jnl_seq[vol] = 0;                            // clear jrn seq
    return -(ERRZ72+ERRMLAST);                          // exit complaining
  }
  if ((partab.jnl_fds[vol]) &&                          // open ?
      (partab.jnl_seq[vol] != systab->vol[vol]->jnl_seq))//  AND changed ?
  { j = close( partab.jnl_fds[vol] );                   // close the file
    partab.jnl_fds[vol] = 0;                            // flag not there
    partab.jnl_seq[vol] = 0;                            // clear jrn seq
  }
  if (partab.jnl_fds[vol] == 0)                         // if not open
  { if (systab->vol[vol]->vollab &&                     // mounted
       (systab->vol[vol]->vollab->journal_requested) && // journal req.
       (systab->vol[vol]->vollab->journal_file[0]))
    { j = attach_jrn(vol, &partab.jnl_fds[0], &partab.jnl_seq[0]);
      if (j < 0) return j;				// return error
    } 
    else
    { return -(ERRZ72+ERRMLAST);                        // give up on error
    }
  }
  return partab.jnl_fds[vol];
}

//-----------------------------------------------------------------------------
// Function: FlushJournal
// Descript: Flush journal buffer contents
// Input(s): volume index, journal file descriptor or 0, flag a sync
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

  if (0 == jfd)
  { jfd = AttachJournal(vol);                           // attach jrn file
    if (jfd < 0) return jfd;                            // failed ? return error
  }

  currsize = systab->vol[vol]->jrnbufsize;
  ASSERT(0 <= currsize);
  ASSERT(currsize <= systab->vol[vol]->jrnbufcap);
  if (currsize)
  { jptr = lseek(jfd, systab->vol[vol]->jrn_next,       // address to locn
                 SEEK_SET);
    if (jptr != systab->vol[vol]->jrn_next)             // if failed
    { ggg = 1; goto fail;
    }
    j = write(jfd, systab->vol[vol]->jrnbuf, currsize);
    if (j != currsize)                                  // flush buffer
    { ggg = 2; goto fail;
    }
    systab->vol[vol]->jrn_next += currsize;             // update next
    jptr = lseek(jfd, sizeof(u_int), SEEK_SET);
    if (jptr != sizeof(u_int))
    { ggg = 3; goto fail;
    }
    j = write(jfd, &systab->vol[vol]->jrn_next,
            sizeof(off_t));                             // write next
    if (j < 0)
    { ggg = 4; goto fail;
    }
  }
  systab->vol[vol]->jrnbufsize = 0;                     // clear buffer
  if (dosync)                                           // if requested
    SyncFD(jfd);                                        //   do a sync.
  return 0;

fail:
  // fprintf(stderr,"FlushJournal: ggg = %d\n",ggg);
  systab->vol[vol]->jrnbufsize = 0;                     // clear buffer
  systab->vol[vol]->vollab->journal_available = 0;      // turn it off
  close(partab.jnl_fds[vol]);                           // close the file
  partab.jnl_fds[vol] = 0;                              // clear fd
  partab.jnl_seq[vol] = 0;                              // clear seq
  return -1;
}


//-----------------------------------------------------------------------------
// Function: ClearJournal
// Descript: Create/clear the journal file
// Input(s): jrn file descriptor to flush, internal volume number
// Return:   none
// Note:     Must be called with a write lock
//
void ClearJournal(int jfd, int vol)                     // clear journal
{ jrnrec jj;                                            // to write with
  int fd;                                               // file descriptor
  u_char tmp[sizeof(u_int) + sizeof(off_t)];            // was 12bytes

  if (jfd)                                              // sync prev. contents
  { FlushJournal(vol, jfd, 1);                        
  }

  fd = open(systab->vol[vol]->vollab->journal_file,
        O_TRUNC | O_RDWR | O_CREAT, 0770);              // open it
  if (fd > 0)                                           // if OK
  { (*(u_int *) tmp) = (MUMPS_MAGIC - 1);
    (*(off_t *) &tmp[sizeof(u_int)]) = sizeof(tmp) +    // was 20bytes
                                        MIN_JRNREC_SIZE;// next free byte
    (void)write(fd, tmp, sizeof(tmp));
    jj.action = JRN_CREATE;
    jj.time = MTIME(0);
    jj.uci = 0;
    jj.size = MIN_JRNREC_SIZE;
    (void)write(fd, &jj, MIN_JRNREC_SIZE);              // write the create rec
    (void)fchmod(fd, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP); // make grp wrt
    (void)close(fd);                                    // and close it
    systab->vol[vol]->jrn_next = (off_t) sizeof(tmp) +
                                        MIN_JRNREC_SIZE;// where it's upto
  }
  return;                                               // done
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
  if ((i < 0) && (errno != ENOENT))                     // if that's junk
  { if (printlog)
      fprintf(stderr, "Failed to access journal file %s\n",
        systab->vol[vol]->vollab->journal_file);
  }
  else                                                  // do something
  { if (i < 0)                                          // if doesn't exist
    { ClearJournal(0, vol);                             // create it
    }                                                   // end create code
    jfd = open(systab->vol[vol]->vollab->journal_file, O_RDWR);
    if (jfd < 0)                                        // on fail
    { if (printlog)
        fprintf(stderr, "Failed to open journal file %s\nerrno = %d\n",
                  systab->vol[vol]->vollab->journal_file, errno);
    }
    else                                                // if open OK
    { u_char tmp[sizeof(u_int) + sizeof(off_t)];

#ifdef MV1_F_NOCACHE
      i = fcntl(jfd, F_NOCACHE, 1);
#endif
      lseek(jfd, 0, SEEK_SET);
      errno = 0;
      i = read(jfd, tmp, sizeof(u_int));                // read the magic
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
            i = write(jfd, &jj, MIN_JRNREC_SIZE);       // write the create rec.
            systab->vol[vol]->jrn_next += MIN_JRNREC_SIZE;// adjust pointer
            lseek(jfd, sizeof(u_int), SEEK_SET);
            i = write(jfd, &systab->vol[vol]->jrn_next, sizeof(off_t));
            i = close(jfd);                             // and close it
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
//           data pointer (set only)
// Return:   none
// Note:     Must be called with a write lock
//           the date/time and size are filled in here
//
void DoJournal(jrnrec *jj, cstring *data)              	// Write journal
{ int i;                                                // handy int
  int jj_alignment;                                     // align. to 4byte bound
  u_int currsize;                                       // curr. JNL buffer size
  short s;

  currsize = systab->vol[volnum - 1]->jrnbufsize;       // get current size
  jj->time = MTIME(0);                                  // store the time
  jj->size = sizeof(u_short) + 2 * sizeof(u_char) + sizeof(time_t) + sizeof(var_u) + sizeof(u_char) + jj->slen;
  if ((jj->action != JRN_SET) && (jj->action != JRN_KILL)) // not SET of KILL
  { jj->size = MIN_JRNREC_SIZE;                         // size is min.
  }
  i = jj->size;
  if (jj->action == JRN_SET)                            // add data length
  { jj->size += (sizeof(short) + data->len);
  }
  jj_alignment = 0;                                     // calc. aligment
  if (jj->size & 3)
  { jj_alignment = (4 - (jj->size & 3));                // round it
  }
  if (currsize + jj->size + jj_alignment >              // does not fit
                            systab->vol[volnum - 1]->jrnbufcap)
  { s = FlushJournal(volnum - 1, 0,                     // flush buffer
                            systab->vol[volnum - 1]->syncjrn); 
    if (s < 0)
      goto fail;
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
  close(partab.jnl_fds[volnum - 1]);                    // close the file
  partab.jnl_fds[volnum - 1] = 0;                       // clear fd
  partab.jnl_seq[volnum - 1] = 0;                       // clear seq
  return;                                               // and exit
}

u_int SleepEx(u_int seconds, const char* file, int line)
{
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
#if !defined(MV1_CKIT)
  int qpos, wpos, rpos, qlen;
#endif
  int qfree;
  int MinSlots;
  int pass = 0;

start:
  Get_GBDsEx(MAXTREEDEPTH * 2, haslock);                // ensure this many
  haslock = 0;                                          // clear for next turns

  j = 0;                                                // clear counter
  MinSlots = 2 * MAXTREEDEPTH;
#ifdef MV1_CKIT
  qfree = NUM_DIRTY - ck_ring_size(&systab->vol[0]->dirtyQ) - 1;
#else
  wpos = systab->vol[0]->dirtyQw;
  rpos = systab->vol[0]->dirtyQr;
  if (rpos <= wpos) qlen = wpos - rpos;
  else
    qlen = NUM_DIRTY + wpos - rpos;
  qfree = NUM_DIRTY - qlen;
#endif
  if (qfree >= MinSlots)
    goto cont;

  SemOp( SEM_GLOBAL, -curr_lock);                       // release current lock

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

short Check_BlockMapped(int vol, u_int blkno)
{ u_char *bitmap;

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  MEM_BARRIER;
  bitmap = (u_char *) systab->vol[vol]->map;

  return bitmap[blkno >> 3] & (1 << (blkno & 7)) ? 0 : -1;
}


short Check_BlockNo(int vol, u_int blkno, int checks,
                        char *where, const char *file, int lno, int dopanic)
{
  char msg[128];
  int inrange_failed = 0, map_failed = 0;

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  if (checks & CBN_INRANGE)                             // out-of-range ?
    inrange_failed = (blkno > systab->vol[vol]->vollab->max_block);
  if (checks & CBN_MAPPED)                              // unmapped ?
    map_failed = (Check_BlockMapped(vol, blkno) < 0);

  if (!dopanic)                                         // just check ?
  { return (inrange_failed || map_failed) ? -1 : 0;     //   return condition
  }

  if (inrange_failed || map_failed)                     // panic when failed
  { sprintf((char *) msg, "%s(%s:%d): %s block (%d:%u) in file %s!!",
                              where, file ? file : "unknown", lno,
                              inrange_failed ? "out-of-range" : "unmapped",
                              vol, blkno,
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
  vol_def *curr_vol;

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  curr_vol = systab->vol[vol];
  ASSERT(NULL != curr_vol->vollab);                     // mounted

  num_gbd = curr_vol->num_gbd;
  cnt = 0;
  for (i = 0; i < num_gbd; i++)
    if (curr_vol->gbd_head[i].dirty &&                  // dirty
        curr_vol->gbd_head[i].block)                    //   AND has block
      cnt++;

  return cnt;
}

static
short bkp_read(int fd, void *buf, size_t n)
{ ssize_t bytes;

  bytes = read(fd, buf, n);
  if (bytes < 0)
  { return -(errno + ERRMLAST + ERRZLAST);
  }
  else if (0 == bytes)
  { return -(ERRZ38 + ERRMLAST);
  }
  else if (bytes != n)
  { return -(EIO + ERRMLAST + ERRZLAST);
  }
  return 0;
}

static
short bkp_write(int fd, const void *buf, size_t n)
{ ssize_t bytes;

  bytes = write(fd, buf, n);
  if (bytes < 0)
  { return -(errno + ERRMLAST + ERRZLAST);
  }
  else if (bytes != n)
  { return -(EIO + ERRMLAST + ERRZLAST);
  }
  return 0;
}

static
short bkp_lseek(int fd, off_t offset, int whence)
{ off_t new_offset;

  new_offset = lseek(fd, offset, whence);
  if (new_offset < 0)
  { return -(errno + ERRMLAST + ERRZLAST);
  }
  return 0;
}

typedef struct __attribute__((__packed__))  __BKPBLK__
{ u_int block;
  DB_Block mem;
} bkpblk_t;

typedef struct __attribute__((__packed__))  __BKPHDR__
{ u_int magic;		// inverted MUMPS_MAGIC
  time_t time;		// backup UNIX time stamp
  int   type;		// backup type
  u_int nvols;		// no. of VOLs in backup
  var_u volnams[MAX_VOL]; // VOL names
} bkphdr_t;

static const char* bkptypes[] =			// backup types
{ "FULL",
  "CUMULATIVE",
  "SERIAL"
};

static
void DB_DumpHeader(u_char *hdr, int header_bytes)
{
  label_block *vollab;
  u_char tmp[MAX_NAME_BYTES + 1];
  u_char *map, *c, *end;
  u_int blkoff, blknum;
  int i;

  vollab = (label_block *) hdr;
  fprintf(stderr,"        magic = %08X\r\n", vollab->magic);
  fprintf(stderr,"    max_block = %u\r\n", vollab->max_block);
  fprintf(stderr," header_bytes = %d\r\n", vollab->header_bytes);
  fprintf(stderr,"   block_size = %d\r\n", vollab->block_size);
  UTIL_Cat_VarU(&tmp[0], &vollab->volnam);
  fprintf(stderr,"       volnam = [%s]\r\n", tmp);
  fprintf(stderr,"       db_ver = %d\r\n", vollab->db_ver);
  fprintf(stderr,"jrn_available = %d\r\n", vollab->journal_available);
  fprintf(stderr,"jrn_requested = %d\r\n", vollab->journal_requested);
  fprintf(stderr,"        clean = %d\r\n", vollab->clean);
  fprintf(stderr,"     jrn_file = [%s]\r\n", &vollab->journal_file[0]);
  fprintf(stderr,"         txid = %lld\r\n", vollab->txid);
  fprintf(stderr,"     bkprevno = %d\r\n", vollab->bkprevno);
  fprintf(stderr,"UCI table:\r\n");
  for (i = 0; i < UCIS; i++)
  { if (0 == vollab->uci[i].name.var_cu[0])
      break;
    UTIL_Cat_VarU(&tmp[0], &vollab->uci[i].name);
    fprintf(stderr,"\t%2d) %s %u\r\n", i+1, (char *) &tmp[0], vollab->uci[i].global);
  }
  fflush(stderr);

  fprintf(stderr,"allocated blocks:\r\n");
  blkoff = 0;
  map = hdr + SIZEOF_LABEL_BLOCK;
  c   = map;
  end = map + vollab->max_block / 8;
  for (; c <= end; c++, blkoff += 8)
  { if (0 == *c)
      continue;
    for (i = 0; i < 8; i++)
    { if (*c & (1 << i))
      { blknum = blkoff + i;
	if (blknum &&
	    (blknum <= vollab->max_block))
        { fprintf(stderr, " %u", blknum);
        } // for all bits
      } // end of SCAN map
    }
  }
  fprintf(stderr,"\r\n");
  fflush(stderr);
}
//-----------------------------------------------------------------------------
// Function: DB_Backup
// Descript: Backup volumes
// Input(s): path - output file
// 	     volmask - volumes to backup
//	     typ - type of backup, 0 - FULL, 1 - CUMULATIVE, 2 - SERIAL
// Return:   None
//

short DB_Backup(const char *path, u_int volmask, int typ)
{ int fd;					// backup file
  short s;					// status
  int pass;					// current WRITE pass
  int done;					// done flag
  int dolock;					// lock VOL?
  int dowrite;					// do backup write flag
  int i, j;					// handy ints
  label_block *vollabs[MAX_VOL];		// VOL labels
  label_block *vollab;				// current VOL label
  label_block *labbufs[MAX_VOL];		// label blk buffer
  off_t headoffs[MAX_VOL];			// header offsets in backup file
  u_char *chgbufs[MAX_VOL];			// change map buffer
  bkpblk_t *blkbuf;				// backup block
  u_char *map, *c, *end;			// map block and ptrs
  u_int blknum;					// current blk number
  u_int blkoff;					// block offset
  u_int nwritten;				// no. of written blocks
  int vol;					// current VOL
  int vols[MAX_VOL];				// VOLs to backup
  int nvols;					// no. of VOLs
  int max_blkbuf;				// max. of block buffers
  bkphdr_t bheader;				// backup file header
  int jfd;					// jrn file descriptor
  off_t offs;					// file offset
  int old_volnum;				// old volnum

  if ((typ < BKP_FULL) || (typ > BKPLAST))
  { fprintf(stderr,"-E-BACKUP: unknown backup type (%d)\r\n", typ); 
    fflush(stderr);
    return -(ERRZ74 + ERRMLAST);
  }
  if (!path || !strlen(path))
  { fprintf(stderr,"-E-BACKUP: empty path\r\n");
    fflush(stderr);
    return -(ERRZ74 + ERRMLAST);
  }

  fprintf(stderr,"-I-BACKUP: start %s backup...\r\n", bkptypes[typ]);
  fflush(stderr);
  nvols = 0;
  for (j = 0; j < 16; j++)
  { if (volmask & (1 << j))			// VOL masked
    { if (NULL == systab->vol[j]->vollab)	// not mounted ?
      { return -(ERRZ90 + ERRMLAST);		//   return error
      }
      if (systab->vol[j]->local_name[0])	// remote VOL ?
      { return -(ERRZ91 + ERRMLAST);		//   return error
      }
      if (systab->vol[j]->bkprunning)		// backup is running ?
      { fprintf(stderr,"-E-BACKUP: backup is running on VOL%d\r\n", j+1);
        fflush(stderr);
        return -(ERRZ74 + ERRMLAST);
      }
      vols[nvols++] = j;
    }
  }
  fprintf(stderr, "-I-BACKUP: Backup volumes");
  for (j = 0; j < nvols; j++)
    fprintf(stderr, " %d", vols[j] + 1);
  fprintf(stderr, "\r\n");
  fflush(stderr);

  s  = 0;					// init vars
  fd = 0;
  max_blkbuf = 0;
  for (j = 0; j < nvols; j++)
  { labbufs[j] = NULL;
    chgbufs[j] = NULL;
  }

  old_volnum = volnum;				// save old VOL number

  for (j = 0; j < nvols; j++)
  { vol    = vols[j];
    volnum = vol + 1;
    while (SemOp( SEM_GLOBAL, WRITE));
    if (systab->vol[vol]->bkprunning)		// check BACKUP state
    { fprintf(stderr,"-E-BACKUP: backup is running on VOL%d\r\n", j+1);
      fflush(stderr);
      s = -(ERRZ74 + ERRMLAST);			//   already running
    }
    else 
      systab->vol[vol]->bkprunning = 1;		// set BACKUP state
    SemOp( SEM_GLOBAL, -WRITE);
    if (s) goto ErrOut;				// failed ?, bail out

    vollab = systab->vol[vol]->vollab;		// current VOL label
    vollabs[j] = vollab;			// save VOL label
    headoffs[j] = (off_t) -1;			// header offset unkown

    labbufs[j] = (label_block *) malloc(vollab->header_bytes);
    if (NULL == labbufs[j])
    { s = -(errno + ERRMLAST + ERRZLAST);
      goto ErrOut;
    }
    chgbufs[j] = (u_char *) malloc(vollab->header_bytes - SIZEOF_LABEL_BLOCK);
    if (NULL == chgbufs[j])
    { s = -(errno + ERRMLAST + ERRZLAST);
      goto ErrOut;
    }
    if (sizeof(u_int) + vollab->block_size > max_blkbuf)
      max_blkbuf = sizeof(u_int) + vollab->block_size;
  }

  blkbuf = (bkpblk_t *) malloc(max_blkbuf);	// allocate blkbuf
  if (NULL == blkbuf)				// failed?
  { s = -(errno + ERRMLAST + ERRZLAST);		//   return error
    goto ErrOut;
  }

  fprintf(stderr,"-I-BACKUP: opening file %s...\r\n", path); fflush(stderr);
  fd = open(path, O_TRUNC | O_WRONLY | O_CREAT, 0644); // open backup file
  if (fd < 0)					// failed?
  { s = -(errno + ERRMLAST + ERRZLAST);		//   return error
    goto ErrOut;
  }

  for (pass = 1; pass <= 5; pass++)
  { fprintf(stderr,"-I-BACKUP: PASS %d...\r\n", pass); fflush(stderr);
    done   = 1;					// assume done
    dolock = 1;					// assume do VOL lock

    for (j = 0; j < nvols; j++)
    { volnum = vols[j] + 1;
      while (SemOp( SEM_GLOBAL, READ));		// acquire READ locks
    }

    for (j = 0; j < nvols; j++)
    { vol    = vols[j];				// set current VOL
      vollab = vollabs[j];

      bcopy(vollab, labbufs[j], vollab->header_bytes);// copy VOL label
      if (1 == pass)				// always do 1 pass
      { done   = 0;
        dolock = 0;
      }
      else
      { bcopy(systab->vol[vol]->chgmap, chgbufs[j],// copy chg map
              vollab->header_bytes - SIZEOF_LABEL_BLOCK);
        done   = done &&
	         (0 == systab->vol[vol]->blkchanged);// done if nothing changed
        dolock = dolock &&
                 ((5 == pass) || 			// last pass
	          (systab->vol[vol]->blkchanged < 300));// OR not much changed
        fprintf(stderr, "-I-BACKUP: VOL%d has %d modified blocks\r\n",
                	vol + 1, systab->vol[vol]->blkchanged); fflush(stderr);
      }
    }

    for (j = 0; j < nvols; j++)
    { vol = vols[j];
      if (done || dolock)			// done OR need to lock ?
      { systab->vol[vol]->writelock = 1;	//   inhibit write
      }
      if (!done)				// not done ?
      { if (!dolock)				// VOL not locked ?
        { bzero(systab->vol[vol]->chgmap, 	// track changes
	        vollab->header_bytes - SIZEOF_LABEL_BLOCK);
          systab->vol[vol]->blkchanged = 0;
          systab->vol[vol]->track_changes = 1;
        }
      }
    }

    // dolock done
    // false  false	write headers, WRITE phase
    //			release global locks
    // false  true	#changed > 300, and 5 != pass, headers are OK
    //			writelock + release global locks
    // true   false	#changed < 300 OR 5 == pass, write headers, WRITE phase
    //			VOLs are writelocked, cannot be changed
    //			release global locks
    // true   true	5 == pass, headers are OK
    //			writelock, release global locks

    if (done || dolock)
    { fprintf(stderr, "-I-BACKUP: write lock volumes...\r\n");
      fflush(stderr);
    }

    for (j = 0; j < nvols; j++)
    { volnum = vols[j] + 1;
      SemOp( SEM_GLOBAL, -READ);		// release READ locks
    }

    // NB. The allocated blocks are marked in the map, 
    //     but the KILLs run in the background, which 
    //	   clear bits in the map.
    //     Wait for the queues to be emtpy, the map block
    //     won't change then.
    if (done || dolock)
      do_queueflush(1);                         // flush daemon queues

    if (done) break;

    if (1 == pass)				// on 1st pass
    { bzero(&bheader, sizeof(bkphdr_t));	// write dummy BACKUP header
      s = bkp_write(fd, &bheader, sizeof(bkphdr_t)); // write out
      if (s < 0) goto ErrOut;			
    }
    for (j = 0; j < nvols; j++)
    { vollab = vollabs[j];			// VOL label
      if ((off_t) -1 == headoffs[j])		// headoffs[] not set ?
      { headoffs[j] = lseek(fd, 0, SEEK_CUR);	// query current offset
        if ((off_t) -1 == headoffs[j]) 		// failed ?
        { s = -(errno + ERRMLAST + ERRZLAST);
          goto ErrOut;
        }
      }
      else
      { s = bkp_lseek(fd, headoffs[j], SEEK_SET);// seek to there
        if (s < 0) goto ErrOut;
      }
      s = bkp_write(fd, vollab, vollab->header_bytes);
      if (s < 0) goto ErrOut;
    }

						// save changed blocks
    s = bkp_lseek(fd, 0, SEEK_END);		// go to EOF
    if (s < 0) goto ErrOut;

    for (j = 0; j < nvols; j++)
    { vol    = vols[j];
      vollab = vollabs[j];
      fprintf(stderr,"-I-BACKUP: saving VOL%d...", vol+1);
      if (1 == pass)				// 1st pass ?
      { map = (u_char *) ((void*) labbufs[j] + SIZEOF_LABEL_BLOCK);
      }
      else					// on subsequent passes
      { map = chgbufs[j];			// use chgbuf
      }

      blkoff = 0;				// clear blkoff
      nwritten = 0;				// clear nwritten
      // NB. in the map bit 0 is always set, bit 1 is set
      //     because block 1 is allocated. Block 1 is stored
      //     physically just after the map.
      c = map; 					// range setup
      end = map + vollab->max_block / 8;
      for (; c <= end; c++, blkoff += 8)	// scan current map
      { if (0 == *c)				// empty ?
          continue;
        for (i = 0; i < 8; i++)			// check blocks
        { if (*c & (1 << i))			// changed? (allocated?)
          { blknum = blkoff + i;
	    if (blknum && 			// NB. block 0 does not exists!
		(blknum <= vollab->max_block))	// valid block ?
            { volnum = vol + 1;
              while (SemOp( SEM_GLOBAL, READ));	//   read block
              level = 0;
	      // NB. the block may be deleted
              s = GetBlockRaw(blknum, __FILE__, __LINE__);
              if (s < 0) goto ErrOut;
              bcopy(blk[level]->mem, 
		    &blkbuf->mem, 
		    vollab->block_size);
              SemOp( SEM_GLOBAL, -READ);

	      dowrite = (BKP_FULL == typ) ||	// FULL backup ?
		(vollab->bkprevno == blkbuf->mem.bkprevno); // OR match BRN ?
	      if (dowrite)
              { blkbuf->block = VOLBLK(j,blknum); // set blknum
                s = bkp_write(fd, blkbuf, sizeof(u_int) + vollab->block_size);
 	        if (s < 0) goto ErrOut;
	        nwritten++;
	      }
            } // block in range
	  } // bit set
        } // for all bits
      } // end of SCAN map
      fprintf(stderr," %u blocks\r\n", nwritten); fflush(stderr);
    } // end of VOLs
    
    if (dolock)					// writelocked VOLs ?
      break;					//   done

  } // end of WRITE pass

  // NB. backed up VOLs are writelock-ed here!

ErrOut:
  if (0 == s)					// no error ?
  { offs = lseek(fd, 0, SEEK_SET);		// write BACKUP header
    if ((off_t) -1 == offs)
    { s = -(errno + ERRMLAST + ERRZLAST);
    }
    if (0 == s)					// no error ?
    { bheader.magic = ~MUMPS_MAGIC;		// construct BACKUP header
      bheader.time  = time(0);			// current UNIX time
      bheader.type  = typ;			// backup type
      bheader.nvols = nvols;			// no. of VOLs in backup
      for (j = 0; j < nvols; j++)
      { bheader.volnams[j] = vollabs[j]->volnam;// VOL names
      }
      s = bkp_write(fd, &bheader, sizeof(bkphdr_t)); // write header
      if (0 == s)				// no error ?
      { s = close(fd);				//   close backup file
        if (s < 0)				//   failed ?
        { s = -(errno + ERRMLAST + ERRZLAST);	// make error code
        }
      }
    }
  }

  for (j = 0; j < nvols; j++)
  { vol = vols[j];
    volnum = vol + 1;
    while (SemOp( SEM_GLOBAL, WRITE));		// acquire WRITE locks
  }

  for (j = 0; j < nvols; j++)
  { vol    = vols[j];				// current VOL
    vollab = vollabs[j];			// current VOL label
    systab->vol[vol]->track_changes = 0;	// do NOT track changes
    systab->vol[vol]->bkprunning = 0;		// clear backup state
    systab->vol[vol]->writelock = 0;		// enable write
    if (0 == s)
    { if ((BKP_FULL == typ) || (BKP_SERIAL == typ))// FULL or SERIAL?
      { vollab->bkprevno++;			//   increment BRN
        systab->vol[vol]->map_dirty_flag |= VOLLAB_DIRTY;// mark LABEL blk dirty
      }
    }
  }

  if (0 == s)					// no error ?
  { for (j = 0; j < nvols; j++)
    { vol = vols[j];				// current VOL
      if (systab->vol[vol]->vollab->journal_available) // journal available ?
      { jfd = attach_jrn(vol, &partab.jnl_fds[0],// attach JRN file
			      &partab.jnl_seq[0]);
        if (jfd > 0)
        { jrnrec jj;                            // write BACKUP record
          jj.action = JRN_BACKUP;
          jj.time = MTIME(0);
          jj.uci = 0;
          volnum = vol + 1;
          DoJournal(&jj, 0);                    // do journal
          FlushJournal(vol, jfd, 0);            // flush journal
          SyncFD(jfd);                          // sync to disk
        }
      }
    }
  }

  for (j = 0; j < nvols; j++)
  { vol = vols[j];
    volnum = vol + 1;
    SemOp( SEM_GLOBAL, -WRITE);			// release WRITE locks
  }

  inter_add(&systab->delaywt, -1);              // release WRITEs
  MEM_BARRIER;

  for (j = 0; j < nvols; j++)
  { if (labbufs[j]) free(labbufs[j]);		// release mem
    if (chgbufs[j]) free(chgbufs[j]);
  }
  if (blkbuf) free(blkbuf);
  if (fd) 					// fd open ?
  { close(fd);					//   close it
    if (s) unlink(path);			//   when error delete it
  }

  if (s)
  { fprintf(stderr, "-E-BACKUP: failed with error code %d\r\n", s);
  }
  else
  { fprintf(stderr,"-I-BACKUP: completed\r\n");
  }
  fflush(stderr);

  volnum = old_volnum;				// restore old VOL number

  return s;
}


//-----------------------------------------------------------------------------
// Function: DB_Restore
// Descript: Restore a single volume from the backup file.
// Input(s): bkp_path - input backup file
// 	     bkp_vol - index of volume to restore in the backup file
//	     vol_path - output volume file
// Return:   None
//

short DB_Restore(const char *bkp_path, int bkp_vol, const char *vol_path)
{ int bkp_fd, vol_fd;				// backup/volume file descriptor
  short s;					// status
  int i;					// handy int
  label_block *volbuf;				// target VOL label
  u_char *blkbuf;				// target block buffer
  u_int copied;					// copy block from backup to tgt
  u_int nwritten;				// no. of blocks written
  bkphdr_t bheader;				// backup file header
  off_t offs;					// file offset
  u_char tmp[64];				// temporary string
  char volnam[MAX_NAME_BYTES + 1];		// target volume name
  union {
    label_block vollab;				// label block
    u_char tmp[SIZEOF_LABEL_BLOCK];		// storage for label block
  } u;
  u_int volblk;					// combined volume + block no.
  int vol;					// backup volume number
  u_int blknum;					// block number
  int block_sizes[MAX_VOL];			// volume block sizes
  int new_vol;					// new volume file flag
  time_t tmp_time;				// temp storage

  bkp_fd = 0;					// init vars
  vol_fd = 0;
  volbuf = NULL;
  blkbuf = NULL;
  tmp_time = 0;

  if (!bkp_path || !strlen(bkp_path))		// check backup file path
  { fprintf(stderr,"-E-RESTORE: empty backup file path\r\n");
    fflush(stderr);
    return -(ERRZ74 + ERRMLAST);
  }

  if ((bkp_vol < 1) || (bkp_vol > MAX_VOL))	// check volume index
  { fprintf(stderr,"-E-RESTORE: invalid volume index\r\n");
    fflush(stderr);
    return -(ERRZ74 + ERRMLAST);
  }
  bkp_vol--;					// adjust volume index


  if (!vol_path || !strlen(vol_path))
  { fprintf(stderr,"-E-RESTORE: empty volume file path\r\n");
    fflush(stderr);
    return -(ERRZ74 + ERRMLAST);
  }

  fprintf(stderr,"-I-RESTORE: start restore...\r\n");
  fflush(stderr);

  fprintf(stderr,"-I-RESTORE: opening backup file %s...\r\n", bkp_path);
  fflush(stderr);
  bkp_fd = open(bkp_path, O_RDONLY); 		// open backup file
  if (bkp_fd < 0)				// failed?
  { s = -(errno + ERRMLAST + ERRZLAST);		//   return error
    goto ErrOut;
  }

  fprintf(stderr,"-I-RESTORE: reading backup header...\r\n");
  fflush(stderr);
  s = bkp_read(bkp_fd, &bheader, sizeof(bkphdr_t));
  if (s < 0) goto ErrOut;

  if (bheader.magic != ~MUMPS_MAGIC)
  { fprintf(stderr, "-E-RESTORE: not a backup file...\r\n");
    fflush(stderr);
    s = -(ERRZ74 + ERRMLAST);
    goto ErrOut;
  }

  if ((bheader.type < BKP_FULL) || (bheader.type > BKPLAST))
  { fprintf(stderr,"-E-RESTORE: unknown backup type (%d)\r\n", bheader.type); 
    fflush(stderr);
    s = -(ERRZ74 + ERRMLAST);
    goto ErrOut;
  }

  strftime((char *) &tmp[0], sizeof(tmp), "%F %T", gmtime(&tmp_time));
  bheader.time = tmp_time;
  fprintf(stderr,"-I-RESTORE: backup type is %s\r\n",  bkptypes[bheader.type]);
  fprintf(stderr,"-I-RESTORE: saved on %s\r\n", (char *) &tmp[0]);

  fprintf(stderr, "-I-RESTORE: contains %d volume(s):\r\n", bheader.nvols);
  for (i = 0; i < bheader.nvols; i++)
  { UTIL_Cat_VarU(tmp, &bheader.volnams[i]);
    fprintf(stderr, "\t%2d) %s", i+1, (char *) &tmp[0]);
    if (i == bkp_vol)
    { fprintf(stderr, " <==");
      strcpy(volnam, (char *) &tmp[0]);
    }
    fprintf(stderr, "\r\n");
  }
  fflush(stderr);

  if (bkp_vol + 1 > bheader.nvols)
  { fprintf(stderr,"-E-RESTORE: invalid volume index\r\n");
    fflush(stderr);
    s = -(ERRZ74 + ERRMLAST);
    goto ErrOut;
  }

  for (i = 0; i < bheader.nvols; i++)
  { s = bkp_read(bkp_fd, &u.tmp, SIZEOF_LABEL_BLOCK);// read std volume LABEL
    if (s < 0) goto ErrOut;
    block_sizes[i] = u.vollab.block_size;	// save block size
    if (i == bkp_vol)				// volume to restore ?
    { volbuf = (label_block *) malloc(u.vollab.header_bytes);
      if (NULL == volbuf)			// allocate buffers
      { s = -(errno + ERRMLAST + ERRZLAST);
	goto ErrOut;
      }
      blkbuf = (u_char *) malloc(block_sizes[i]);
      if (NULL == blkbuf)
      { s = -(errno + ERRMLAST + ERRZLAST);
	goto ErrOut;
      }
      bcopy(&u.tmp, volbuf, SIZEOF_LABEL_BLOCK);// save volume header
      s = bkp_read(bkp_fd, 
		   SIZEOF_LABEL_BLOCK + (void *) volbuf,
		   u.vollab.header_bytes - SIZEOF_LABEL_BLOCK);
    }
    else
    { s = bkp_lseek(bkp_fd, 			// skip header bytes
		    u.vollab.header_bytes - SIZEOF_LABEL_BLOCK,
		    SEEK_CUR);

    }
    if (s < 0) goto ErrOut;			// return on error
  }

  fprintf(stderr,"-I-RESTORE: opening volume file %s...\r\n", vol_path);
  fflush(stderr);
  // NB. target volume may exists, if not create it
  new_vol = 0;					// assume volume exists
  vol_fd = open(vol_path, O_WRONLY);		// open volume file
  if (vol_fd < 0)				// failed?
  { if (BKP_FULL == bheader.type)		// FULL backup ?
    { fprintf(stderr,"-W-RESTORE: creating volume file %s...\r\n", vol_path);
      fflush(stderr);
      vol_fd = open(vol_path, O_WRONLY | O_CREAT, 0644);// create it
      if (vol_fd < 0)				// failed?
      { s = -(errno + ERRMLAST + ERRZLAST);	//   return error
        goto ErrOut;
      }
      new_vol = 1;				// new volume file
    }
    else
    { fprintf(stderr, "-E-RESTORE: volume file not found\r\n");
      fflush(stderr);
      s = -(errno + ERRMLAST + ERRZLAST);
      goto ErrOut;
    }
  }

  fprintf(stderr, "-I-RESTORE: writing volume header...\r\n");
  fflush(stderr);
  volbuf->journal_available = 0;		// clear journal settings
  volbuf->journal_requested = 0;
  bzero(&volbuf->journal_file[0], JNL_FILENAME_MAX + 1);
  volbuf->clean = 1;				// mark volume as clean
  s = bkp_write(vol_fd, volbuf, volbuf->header_bytes);// write VOL header
  if (s < 0) goto ErrOut;

  // DB_DumpHeader((u_char *) volbuf, volbuf->header_bytes);

  if (new_vol)					// expand if new volume file
  { fprintf(stderr, "-I-RESTORE: expanding volume...\r\n");
    fflush(stderr);
    bzero(blkbuf, volbuf->block_size);
    for (i = 0; i < volbuf->max_block; i++)
    { s = bkp_write(vol_fd, blkbuf, volbuf->block_size);
      if (s < 0) goto ErrOut;
    }
  }

  fprintf(stderr, "-I-RESTORE: restoring blocks...\r\n");
  fflush(stderr);

  nwritten = 0;					// no. of blocks written
  for (;;)
  { s = bkp_read(bkp_fd, &volblk, sizeof(u_int));// read block number
    if (s < 0)					// failed?
    { if (-(ERRZ38 + ERRMLAST) == s)		// EOF reached?
      { s = 0;					//   clear status flag
        break;					//   done
      }
      goto ErrOut;				// return error
    }
    vol = VOLNO(volblk);			// get volume no.
    blknum = BLKNO(volblk);			// get block no.
    if (vol + 1 > bheader.nvols)		// check volume index
    { fprintf(stderr,"-E-RESTORE: volume index out of range\r\n");
      fflush(stderr);
      s = -(ERRZ74 + ERRMLAST);
      goto ErrOut;
    }
    if ((0 == blknum) ||
        (blknum > volbuf->max_block))		// check block number
    { fprintf(stderr,"-E-RESTORE: block number %u is out of range\r\n", blknum);
      fflush(stderr);
      s = -(ERRZ74 + ERRMLAST);
      goto ErrOut;
    }
    copied = 0;					// assume no copy
    if (vol == bkp_vol)				// restore?
    { s = bkp_read(bkp_fd, blkbuf, volbuf->block_size);// read block
      if (s < 0) goto ErrOut;
      offs = (off_t) blknum - 1;		// NB. block 1 is stored
						//   at offset 0
      offs = (offs * (off_t) volbuf->block_size)// calc. target volume offset
	     + (off_t) volbuf->header_bytes;
      s = bkp_lseek(vol_fd,			// go to block in target
		    offs,
		    SEEK_SET);
      if (s < 0) goto ErrOut;
      // fprintf(stderr, " %u", blknum);
      s = bkp_write(vol_fd, blkbuf, volbuf->block_size);// write out
      copied = 1;
    }
    else
    { s = bkp_lseek(bkp_fd, block_sizes[vol], SEEK_CUR);// skip it
    }
    if (s < 0) goto ErrOut;			// return on error

    nwritten += copied;
    if (0 == (nwritten % 1000))
    { fprintf(stderr, "-I-RESTORE: #%u...\r\n", nwritten);
      fflush(stderr);
    }
  }

ErrOut:
  if (volbuf) free(volbuf);			// release memory
  if (blkbuf) free(blkbuf);

  if (bkp_fd)					// backup file open?
  { close(bkp_fd);				//   close it
  }
  if (vol_fd)					// volume file open?
  { close(vol_fd);				//   close it
    if (s)					// any error ?
    { unlink(vol_path);				//   remove it
    }
  }

  if (s)
  { fprintf(stderr, "-E-RESTORE: failed with error code %d\r\n", s);
  }
  else
  { fprintf(stderr, "-I-RESTORE: completed\r\n");
  }
  fflush(stderr);
  return s;
}


void TX_Set(gbd *ptr)
{ u_int64 txid;

  ASSERT((0 < volnum) && (volnum < MAX_VOL + 1));
  ASSERT(NULL != systab->vol[volnum - 1]->vollab);
  ASSERT(0 == systab->vol[volnum - 1]->local_name[0]);
  ASSERT(WRITE == curr_lock);

  MEM_BARRIER;
  txid = systab->vol[volnum - 1]->vollab->txid;
  ptr->mem->blkver_lo = (0x0FFFFFFFFUL & txid);
  ptr->mem->blkver_hi = (0x0FFFFFFFFUL & (txid >> 32));
}


void TX_Next(void)
{ 
  ASSERT((0 < volnum) && (volnum < MAX_VOL + 1));
  ASSERT(NULL != systab->vol[volnum - 1]->vollab);
  ASSERT(0 == systab->vol[volnum - 1]->local_name[0]);
  ASSERT(WRITE == curr_lock);

  systab->vol[volnum - 1]->vollab->txid++;
  systab->vol[volnum - 1]->map_dirty_flag |= VOLLAB_DIRTY;
  MEM_BARRIER;
}


int SyncFD(int fd)
{ int rc;

#ifdef __APPLE__
  rc = fcntl(fd, F_FULLFSYNC);
#else
  rc = fsync(fd);
#endif

  return rc;
}
