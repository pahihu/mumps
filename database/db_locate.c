// File: mumps/database/db_locate.c
//
// module database - Locate Database Functions

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
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include "mumps.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings
#include <assert.h>


#define NUM_CHUNK       (256 * 1024)

        int     KeyBufBuilt;
static  int     locate_init = 1;
static  u_short LastIndex;
static  u_short wPrevChunk[ 32*1024];                   // write cache
static  u_short aPrevChunk[NUM_CHUNK];                  //   read cache
static  u_short *PrevChunk;

static struct
{ u_int block;
  u_int blkver_low;
  u_int blkver_high;
  u_int idx_len;
  u_short *PrevChunk;
} blkcache[MAXTREEDEPTH];
static int nblkcache = 0;

        u_int   locqry = 0;
        u_int   lochit = 0;

//-----------------------------------------------------------------------------
// Function:    FindShorterChunk
// Descript:    find a previous chunk which has shorter
//              prefix than prefixLen.
// Input(s):    from - where to start, prefixLen - upper limit
// Return:      index of chunk
//

static
u_short FindShorterChunk(u_short from, u_short prefixLen)
{
  cstring *tsunk;

  for (from-- ; from >= LOW_INDEX; from--)              // backward search
  { 
    tsunk = (cstring*) &iidx[idx[from]];
    if (tsunk->buf[0] < prefixLen)                      // prefix is shorter?
      return from;                                      // done
  }
  return LOW_INDEX;                                     // sentinel
}

//-----------------------------------------------------------------------------
// Function:    GetPrevChunkIndex
// Descript:    return the index of the previous chunk
//              which is necessary to build keybuf[] for
//              Index x
// Input(s):    x - Index
// Returns:     index of previous chunk
//

static
u_short GetPrevChunkIndex(u_short x)
{
  u_char  prefixLen;
  cstring *tsunk;

  if (LOW_INDEX > x)
    panic("GetPrevChunkIndex: LOW_INDEX > x");
  if (x > blk[level]->mem->last_idx)
    panic("GetPrevChunkIndex: x > blk[level]->mem->last_idx");

  //if (PrevChunk[x])                                     // already calculated
  //  return PrevChunk[x];

  tsunk = (cstring*) &iidx[idx[x]];
  prefixLen = tsunk->buf[0];
  PrevChunk[x] = prefixLen ? FindShorterChunk(x, prefixLen) : x;
  if (LOW_INDEX > PrevChunk[x])
    panic("GetPrevChunkIndex: LOW_INDEX > PrevChunk[x]");
  if (PrevChunk[x] > blk[level]->mem->last_idx)
    panic("GetPrevChunkIndex: PrevChunk[x] > blk[level]->mem->last_idx");
  if (PrevChunk[x] > x)
    panic("GetPrevChunkIndex: PrevChunk[x] > x");
  return PrevChunk[x];
}

//-----------------------------------------------------------------------------
// Function:    Build_KeyBuf
// Descript:    build up keybuf[] for Index, uses GetPrevChunkIndex
//              to recursively build up keybuf[] updating external vars
//              keybuf, LastIndex
//

static
void Build_KeyBuf(void)
{
  u_short x;                                            // current Index
  u_short prev;                                         // previous chunk idx
  u_short chunkidx[256];                                // stores the chunk idxs
  int i, j, done;                                       // handy ints
  int goingup;
  cstring *tsunk;

  // fprintf(stderr,"--- Collect_Chunks(Index = %d, LastIndex = %d)\r\n", Index, LastIndex);
  goingup = LastIndex < Index;                          // going upwards
  j = 256;                                              // point to end of cidx
  x = Index;                                            // start at Index
  do
  { prev = PrevChunk[x];
    if (!prev) prev = GetPrevChunkIndex(x);
    if (LOW_INDEX > prev)
      panic("Build_KeyBuf: LOW_INDEX > prev");
    if (prev > blk[level]->mem->last_idx)
    { // fprintf(stderr,"wr_flag: %d\r\n", wr_flag);
      // fprintf(stderr,"cache mode: %d\r\n", cache_mode);
      fprintf(stderr,"blk: %d x: %d\r\n", level, x);
      fprintf(stderr,"last_idx: %d prev: %d\r\n", blk[level]->mem->last_idx, prev);
      panic("Build_KeyBuf: prev > blk[level]->mem->last_idx");
    }
    chunkidx[--j] = prev;                               // collect chunk idxs
    done = (prev == x) || (prev == LOW_INDEX);          // done ?
    if (goingup && (prev <= LastIndex))                 // reached last Idx
    { done = TRUE;                                      // then done
      j++;                                              // pop last entry
    }
    x = prev;                                           // recurse
  } while (!done);

  for ( ; j < 256; j++)                                 // for all collected idx
  { i = chunkidx[j];
    // fprintf(stderr,"i = %d\r\n", i);
    tsunk = (cstring *) &iidx[idx[i]];	                // point at the chunk
    bcopy(&tsunk->buf[2], &keybuf[tsunk->buf[0]+1],
	  tsunk->buf[1]);				// update the keybuf
  }
  LastIndex = Index;                                    // remember Index
}

//-----------------------------------------------------------------------------
// Function: Locate
// Descript: Locate passed in key in blk[level] updating extern vars
//	     Index, chunk, record and keybuf
// Input(s): Pointer to key to find (key[0] -> length)
// Return:   0 -> Ok, negative MUMPS error
// Note:     On fail (-ERRM7), Index etc points at the following record.
//	     External vars setup are:
//		(cstring *)	chunk	points at the chunk in the block
//		(u_short *)	idx	maps the block as an array
//		(int *)		iidx	maps the block as an array
//		(cstring *)	record	points at the data for the record
//					(not alligned for ptr/GD)
//		(u_char)	keybuf	the current full key
//

void UpdateLocateCache(void)
{
  return;
}

short LocateEx(u_char *key, int frominsert)		// find key
{ int i;						// a handy int
  int L, R;                                             // bin.search limits
  int wr_flag;
  int found;

  if (locate_init)
  { for (i = 0; i < MAXTREEDEPTH; i++)
    { blkcache[i].block     = (u_int) -1;
      blkcache[i].PrevChunk = 0;
    }
    nblkcache = 0;
    locate_init = 0;
  }

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  Index = LOW_INDEX;					// start at the start
  L = LOW_INDEX; R = blk[level]->mem->last_idx;         // setup limits

  locqry++;

#ifdef MV1_CCC
  found = 0;
  wr_flag = blk[level]->dirty != 0;
  if (0 == wr_flag)
  { for (i = nblkcache; i >= 0; i--)
    { if (blkcache[i].block != blk[level]->block)
        continue;
      if ((blkcache[i].blkver_low == blk[level]->blkver_low) &&
          (blkcache[i].blkver_high == blk[level]->blkver_high))
      { found = 1;
        lochit++;
        PrevChunk = blkcache[i].PrevChunk;
        break;
      }
    }
    if (0 == found)
    { blkcache[level].block = blk[level]->block;
      blkcache[level].blkver_low  = blk[level]->blkver_low;
      blkcache[level].blkver_high = blk[level]->blkver_high;
      blkcache[level].idx_len     = blk[level]->mem->last_idx + 1;
      if ((0 == level) ||
          (0 == blkcache[level-1].PrevChunk))
      { PrevChunk = aPrevChunk;
      }
      else
      { PrevChunk = blkcache[level-1].PrevChunk + blkcache[level-1].idx_len;
      }
      if (PrevChunk - aPrevChunk > NUM_CHUNK)
      { panic("LocateEx(): PrevChunk cache is full");
      }
      blkcache[level].PrevChunk = PrevChunk;
      nblkcache = level;
      bzero(PrevChunk + L, (R - L + 1) * sizeof(u_short));
    }
  }
  else
  { PrevChunk = wPrevChunk;
    bzero(PrevChunk + L, (R - L + 1) * sizeof(u_short));
  }
#else
  wr_flag = writing + wanna_writing;
#endif

  LastIndex = R + 1;                                    // setup LastIndex
  KeyBufBuilt = 0;
  if (frominsert)                                       // writing, chk last
  { Index = R;
    // Build_KeyBuf();                                  // build keybuf
    chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
    i = UTIL_Key_KeyCmp(&chunk->buf[2], &key[1], chunk->buf[1], key[0]); // cmp
    if (i == K2_GREATER)                                // key > last key
    { Index = R + 1;                                    // early exit
      // KeyBufBuilt = 1;
      return -ERRM7;
    }
  }

  // fprintf(stderr,"L=%d R=%d\n",L,R);
  while (L <= R)                                        // loop
  { // if (R - L + 1 < 4) // YYY
    // { Index = L;
    //   Build_KeyBuf();
    //   return LocateLin(key, R);
    // }
    KeyBufBuilt = 0;
    Index = (L + R) >> 1;                               // find middle
#ifdef MV1_CCC
    Build_KeyBuf();
#endif
    chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
#ifdef MV1_CCC
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
    	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
    i = UTIL_Key_KeyCmp(&keybuf[1], &key[1], keybuf[0], key[0]); // compare
#else
    i = UTIL_Key_KeyCmp(&chunk->buf[2], &key[1], chunk->buf[1], key[0]); // compare
#endif
#if 0
    fprintf(stderr," Index=%d\n",Index);
    fprintf(stderr,"keybuf=[%s] %d\n",&keybuf[1],keybuf[0]);
    fprintf(stderr,"   key=[%s] %d\n",&key[1],key[0]);
#endif
    if (i == K2_GREATER)                                // not reached, adj. L
    { L = Index + 1;
      // KeyBufBuilt = 1;
      continue;
    }
    else if (i == K2_LESSER)                            // passed, adj. R
    { R = Index - 1;
      continue;
    }
#ifndef MV1_CCC
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
    	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
#endif
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    return 0;						// key found, done
  }
  Index = L;                                            // not found
  if (Index <= blk[level]->mem->last_idx)               // update if Index valid
  { if (!frominsert)
    { chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
      bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
                  chunk->buf[1]);			// update the key
      keybuf[0] = chunk->buf[0] + chunk->buf[1];	// and the size
      record = (cstring *) &chunk->buf[chunk->buf[1]+2];// point at the dbc
    }
  }
  return -ERRM7;
}

short Locate(u_char *key)
{
  return LocateEx(key, 0);
}

//-----------------------------------------------------------------------------
// Function: Locate_next
// Descript: Locate next key in blk[level] updating extern vars
//	     Index, chunk, record and keybuf
// Input(s): none (extern vars must be setup)
// Return:   0 -> Ok, negative MUMPS error
// Note:     Must be be called with a read lock
//	     External vars setup as for Locate() above.
//

short Locate_next(u_char *out)				// point at next key
{ int i;						// a handy int
  short s;						// function returns

  Index++;						// point at next
  if (Index > blk[level]->mem->last_idx)		// passed end?
  { if (!blk[level]->mem->right_ptr)			// any more there?
    { return -ERRM7;					// no, just exit
    }
    i = blk[level]->mem->right_ptr;			// get right block#
    s = Get_block(i);					// attempt to get it
    if (s < 0)                                        	// if we got an error
    { return s;                                       	// return it
    }
  }							// end new block

  chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
  if (out)
  { 
    bcopy(&chunk->buf[2], &out[chunk->buf[0]+1],
	chunk->buf[1]);					// update the key
    out[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  }
  record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
  return 0;						// all done
}

