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

// PrevChunk[i] contains the index of the chunk, which 
// is necessary to build the current keybuf[], recursively.
#define CACHE_SIZE      (256*1024)
static u_short aPrevChunk[CACHE_SIZE];
static u_char  aBuf0[CACHE_SIZE];
static u_short *PrevChunk;
static u_char  *buf0;

static int wr_flag;

typedef struct
{ u_char  *buf0;
  u_short *PrevChunk;
  u_int   block;
  u_int   blkver_low;
  u_int   blkver_high;
  u_int   idx_len;
} TChunkInfo;
static TChunkInfo aChunkInfo[MAXTREEDEPTH];
static int     lastChunkInfo = -1;              // last ChunkInfo filled
static int     LastIndex;                       // last Index compared
       int     KeyBufBuilt;

static int cache_mode;

/*
  - so far:
  buf0[] fill and use, doesn't count
  threshold Lin/Bin little slower
  combined Lin/Bin little slower
  complex check of PrevChunk in FindShorterChunk() slower
*/

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
  // cstring *tsunk;

  for (from-- ; from >= LOW_INDEX; from--)              // backward search
  { 
    // tsunk = (cstring*) &iidx[idx[from]];
    // if (tsunk->buf[0] < prefixLen)                      // prefix is shorter?
    //   return from;                                      // done
    if (buf0[from] < prefixLen)
        return from;
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
  // cstring *tsunk;

  assert(LOW_INDEX <= x);
  assert(x <= blk[level]->mem->last_idx);

  //if (PrevChunk[x])                                     // already calculated
  //  return PrevChunk[x];

  // tsunk = (cstring*) &iidx[idx[x]];
  prefixLen = buf0[x]; //tsunk->buf[0];
  PrevChunk[x] = prefixLen ? FindShorterChunk(x, prefixLen) : x;
  assert(LOW_INDEX <= PrevChunk[x]);
  assert(PrevChunk[x] <= blk[level]->mem->last_idx);
  assert(PrevChunk[x] <= x);
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
    assert(LOW_INDEX <= prev);
    if (prev > blk[level]->mem->last_idx)
    { fprintf(stderr,"wr_flag: %d\r\n", wr_flag);
      fprintf(stderr,"cache mode: %d\r\n", cache_mode);
      fprintf(stderr,"blk: %d x: %d\r\n", level, x);
      fprintf(stderr,"last_idx: %d prev: %d\r\n", blk[level]->mem->last_idx, prev);
      exit(1);
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


static
void InitLocateCache(void)
{
  int i;

  for (i = 0; i < MAXTREEDEPTH; i++)
    aChunkInfo[i].block = 0;
}

void UpdateLocateCache(void)
{
  int i;
  cstring *tsunk;

  return;

  // fprintf(stderr,"blk(%d) cache update @ %d\r\n",blk[level]->block,level);
  aChunkInfo[level].block      = blk[level]->block;
  aChunkInfo[level].blkver_low = blk[level]->blkver_low;
  aChunkInfo[level].idx_len++;

  for (i = blk[level]->mem->last_idx; i >= Index; i--)
    buf0[i + 1] = buf0[i];
  // Index-th is the new entry
  tsunk = (cstring*) &iidx[idx[Index]];
  buf0[Index] = tsunk->buf[0];

}

short LocateEx(u_char *key, int frominsert)		// find key
{ int i;						// a handy int
  int L, R;                                             // bin.search limits
  int idx_len, chunkLevel;
  cstring *tsunk;

  // fprintf(stderr,"key=[%s]\r\n",&key[1]);
  if (-1 == lastChunkInfo)
    InitLocateCache();

  wr_flag = writing + wanna_writing;

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  Index = LOW_INDEX;					// start at the start
  L = LOW_INDEX; R = blk[level]->mem->last_idx;         // setup limits

  assert(0 <= level);
  assert(level < MAXTREEDEPTH);

  chunkLevel = level;
  if ((0 == wr_flag) && (LAST_USED_LEVEL == level))
  {
     // assert(-1 < lastChunkInfo);
     chunkLevel = lastChunkInfo;
     cache_mode = 0;
     // fprintf(stderr,"LAST_USED_LEVEL: %d\r\n",chunkLevel);
  }
  if ((0 == wr_flag) &&
      (chunkLevel <= lastChunkInfo) &&
      (aChunkInfo[chunkLevel].block       == blk[level]->block) &&
      (aChunkInfo[chunkLevel].blkver_low  == blk[level]->blkver_low) &&
      (aChunkInfo[chunkLevel].blkver_high == blk[level]->blkver_high))
      // && (aChunkInfo[chunkLevel].idx_len     == 1 + R)) // FIXME: why ???
  { buf0      = aChunkInfo[chunkLevel].buf0;
    PrevChunk = aChunkInfo[chunkLevel].PrevChunk;
    // fprintf(stderr,"blk(%d) cached @ %d\r\n",blk[level]->block,chunkLevel);
    cache_mode = 1;
    if (R != aChunkInfo[chunkLevel].idx_len - 1)
    { fprintf(stderr,"blk: %d changed w/o changing blkver (%d != %d)\r\n",
              blk[level]->block, R, aChunkInfo[chunkLevel].idx_len - 1);
      exit(1);
    }
  }
  else
  { if (wr_flag)
      lastChunkInfo = -1;

    if ((0 == chunkLevel) || (-1 == lastChunkInfo))
    { PrevChunk = &aPrevChunk[0];
      buf0      = &aBuf0[0];
      // fprintf(stderr,"blk(%d) put @ %d\r\n",blk[level]->block,level);
      cache_mode = 2;
    }
    else
    { assert(0 < chunkLevel);
      if (chunkLevel > 1 + lastChunkInfo)
      { // fprintf(stderr,"chunkLevel: %d lastChunkInfo: %d\r\n",chunkLevel,lastChunkInfo);
        chunkLevel = 1 + lastChunkInfo;
      }
      assert(chunkLevel <= 1 + lastChunkInfo);

      idx_len   = aChunkInfo[chunkLevel-1].idx_len;
      assert(0 != aChunkInfo[chunkLevel-1].PrevChunk);
      assert(0 != aChunkInfo[chunkLevel-1].buf0);

      PrevChunk = idx_len + aChunkInfo[chunkLevel-1].PrevChunk;
      buf0      = idx_len + aChunkInfo[chunkLevel-1].buf0;
      // fprintf(stderr,"blk(%d) put @ %d\r\n",blk[level]->block,level);
      cache_mode = 3;
    }
    // FIXME: PrevChunk/buf0 fit ?

    assert(chunkLevel < MAXTREEDEPTH);

    if (0 == wr_flag)
    { aChunkInfo[chunkLevel].block       = blk[level]->block;
      aChunkInfo[chunkLevel].blkver_low  = blk[level]->blkver_low;
      aChunkInfo[chunkLevel].blkver_high = blk[level]->blkver_high;
      aChunkInfo[chunkLevel].idx_len     = 1 + blk[level]->mem->last_idx;
      aChunkInfo[chunkLevel].buf0        = buf0;
      aChunkInfo[chunkLevel].PrevChunk   = PrevChunk;
      lastChunkInfo = chunkLevel;
    }

    if ((buf0 < aBuf0) || (buf0 > &aBuf0[CACHE_SIZE]))
    { fprintf(stderr,"buf0 out of range\r\n");
      exit(1);
    }
    if ((PrevChunk < aPrevChunk) || (PrevChunk > &aPrevChunk[CACHE_SIZE]))
    { fprintf(stderr,"PrevChunk out of range\r\n");
      exit(1);
    }

    // fprintf(stderr,"filling [%d,%d]\r\n",L,R);
    //memset(PrevChunk, 255, (1 + R) * sizeof(u_short));
    //memset(buf0, 255, (1 + R) * sizeof(u_char));
    bzero(PrevChunk + L, (R - L + 1) * sizeof(u_short));// clear PrevChunk
    for(i = L; i <= R; i++)                             // collect pfx lengths
    { tsunk = (cstring*) &iidx[idx[i]];
      buf0[i] = tsunk->buf[0];
    }
  }

#if 0
  if (writing ||                                        // zot PrevChunk, iff
      (level != LAST_USED_LEVEL) ||                     // LAST_USED_LEVEL set
      (blk[level]->block != LastBlock) ||               //   blknum mismatch
      (LastBlkVer_Low  != blk[level]->blkver_low) ||    //   blkver mismatch
      (LastBlkVer_High != blk[level]->blkver_high))
  { bzero(PrevChunk + L, (R - L + 1) * sizeof(u_short));// clear PrevChunk
    for(i = L; i <= R; i++)                             // collect pfx lengths
    { tsunk = (cstring*) &iidx[idx[i]];
      buf0[i] = tsunk->buf[0];
    }
  }
#endif

  LastIndex = R + 1;                                    // setup LastIndex

  KeyBufBuilt = 0;
  if (wr_flag)                                          // writing, chk last
  { Index = R;
    Build_KeyBuf();                                     // build keybuf
    chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    i = UTIL_Key_KeyCmp(&keybuf[1], &key[1], keybuf[0], key[0]); // compare
    if (i == K2_GREATER)                                // key > last key
    { Index = R + 1;                                    // early exit
      KeyBufBuilt = 1;
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
    Build_KeyBuf();
    chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
    	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
#if 0
    fprintf(stderr," Index=%d\n",Index);
    fprintf(stderr,"keybuf=[%s] %d\n",&keybuf[1],keybuf[0]);
    fprintf(stderr,"   key=[%s] %d\n",&key[1],key[0]);
#endif
    i = UTIL_Key_KeyCmp(&keybuf[1], &key[1], keybuf[0], key[0]); // compare
    if (i == K2_GREATER)                                // not reached, adj. L
    { L = Index + 1;
      KeyBufBuilt = 1;
      continue;
    }
    else if (i == K2_LESSER)                            // passed, adj. R
    { R = Index - 1;
      continue;
    }
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

short Locate_next()					// point at next key
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
  bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	chunk->buf[1]);					// update the key
  keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
  return 0;						// all done
}

