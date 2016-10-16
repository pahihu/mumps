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


        int     KeyLocated;                             // flag key located

       u_int   locqry = 0;                              // Locate() stats
       u_int   lochit = 0;

static int     KeyBufUpdated;                           // flag keybuf[] updated

//-----------------------------------------------------------------------------
// Function:    Build_KeyBuf
// Descript:    build up keybuf[] in pKeyBuf for pIndex
//

void Build_KeyBuf(int pIndex, u_char *pKeyBuf)
{ int i;                                                // handy int
  u_short  pos, from;
  u_char   pfx,nxtpfx;
  int      ncptr = 256;
  cstring *cptr[256];
  u_char   stride[256];
  u_char   last_ccc,last_ucc;

  chunk = (cstring *) &iidx[idx[pIndex]];
  pfx   = chunk->buf[0];
  pKeyBuf[0] = pfx + chunk->buf[1];                     // set keybuf size
  if (pfx == 0)
  { // fprintf(stderr,"keybuf ptr\r\n");
    bcopy(&chunk->buf[2], &pKeyBuf[chunk->buf[0]+1],    // update the keybuf
	  chunk->buf[1]);			
    return;
  }

  // last_ccc = chunk->buf[0];
  // last_ucc = chunk->buf[1];

  cptr[--ncptr] = chunk;                        // put chunk as last
  stride[ncptr] = chunk->buf[1];                // chunk should point there

  from = pIndex;
  do
  { pos = FindChunk(from, pfx);
    chunk = (cstring *) &iidx[idx[pos]];
    cptr[--ncptr] = chunk;
    stride[ncptr] = pfx - (nxtpfx = chunk->buf[0]);
    pfx = nxtpfx;
    from = pos;
  } while (pfx != 0);

  //    0 ucc0  cpy  ccc1 - 0     from    0  to  0
  // ccc1 ucc1  cpy  ccc2 - ccc1  from ccc1  to  ccc1
  // ccc2 ucc2
  //
  //    0    3  abc
  //    2    4  abxyzx
  //    3    2  abxab
  //    5

  // fprintf(stderr,"keybuf stride = %d\r\n",256 - ntidx);
  // keybuf[0] = 0;                                     // clear keybuf
#if 0
  fprintf(stderr,"keybuf stride = %d\r\n",Index-i);
  for (; i <= Index; i++)                               // for all collected idx
  { chunk = (cstring *) &iidx[idx[i]];	                // point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the keybuf
    keybuf[0] = chunk->buf[0] + chunk->buf[1];          // and the keybuf size
  }
#endif
  for (; ncptr < 256; ncptr++)                          // for all collected idx
  { chunk = cptr[ncptr];                                // point at the chunk
    // fprintf(stderr,"ccc=%d ucc=%d stride=%d\r\n",
    //                 chunk->buf[0],chunk->buf[1],stride[ncptr]);
    bcopy(&chunk->buf[2],
          &pKeyBuf[chunk->buf[0]+1],
	  stride[ncptr] /*chunk->buf[1]*/);		// update the keybuf
  }
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

short LocateEx(u_char *key, int frominsert)		// find key
{ int i;						// a handy int
  int L, R;                                             // bin.search limits
  int wr_flag;
  u_char *dbkey;                                        // ptr to key in blk

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  Index = LOW_INDEX;					// start at the start
  L = LOW_INDEX; R = blk[level]->mem->last_idx;         // setup limits

  locqry++;                                             // update stats
  wr_flag = writing + wanna_writing;                    // flag a write

  KeyLocated = 0;
  dbkey = &keybuf[0];
  // fprintf(stderr,"--- LocateEx()\r\n",Index);
  if ((frominsert) &&                                   // insert: check last
      (blk[level]->mem->right_ptr == 0))                //   if no right_ptr
  { Index = R;
#ifdef MV1_CCC
    Build_KeyBuf(Index, &keybuf[0]);                    // build keybuf
#else
    chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
    dbkey = &chunk->buf[1];
#endif
#if 0
    fprintf(stderr," KeyBufUpdated=%d\r\n",KeyBufUpdated);
    fprintf(stderr," Index=%d\r\n",Index);
    fprintf(stderr," dbkey=[%s] %d\r\n",&dbkey[1],dbkey[0]);
    fprintf(stderr,"   key=[%s] %d\r\n",&key[1],key[0]);
#endif
    i = UTIL_Key_KeyCmp(&dbkey[1], &key[1], dbkey[0], key[0]); // cmp
    if (i == K2_GREATER)                                // key > last key
    { Index = R + 1;                                    // early exit
      return -ERRM7;
    }
    else if (i != K2_LESSER)
    { goto K2_EQUAL;
    }
  }

  // fprintf(stderr,"L=%d R=%d\n",L,R);
  while (L <= R)                                        // loop
  { // if (R - L + 1 < 4) // YYY
    // { Index = L;
    //   Build_KeyBuf();
    //   return LocateLin(key, R);
    // }
    Index = (L + R) >> 1;                               // find middle
#ifdef MV1_CCC
    Build_KeyBuf(Index, &keybuf[0]);
#else
    chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
    dbkey = &chunk->buf[1];
#endif
    i = UTIL_Key_KeyCmp(&dbkey[1], &key[1], dbkey[0], key[0]); // compare
#if 0
    fprintf(stderr," KeyBufUpdated=%d\r\n",KeyBufUpdated);
    fprintf(stderr," Index=%d\r\n",Index);
    fprintf(stderr," dbkey=[%s] %d\r\n",&dbkey[1],dbkey[0]);
    fprintf(stderr,"   key=[%s] %d\r\n",&key[1],key[0]);
#endif
    if (i == K2_GREATER)                                // not reached, adj. L
    { L = Index + 1;
      continue;
    }
    else if (i == K2_LESSER)                            // passed, adj. R
    { R = Index - 1;
      continue;
    }
K2_EQUAL:
    if (KeyBufUpdated == 0)                             // if keybuf[] not upd'd
      bcopy(&dbkey[0], &keybuf[0], dbkey[0] + 1);       //   store last dbkey
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    return 0;						// key found, done
  }
  Index = L;                                            // not found
  if (Index <= blk[level]->mem->last_idx)               // update if Index valid
  { if (!frominsert)
    { 
#ifdef MV1_CCC
      if (KeyBufUpdated == 0)                           // if keybuf[] not upd'd
        bcopy(&dbkey[0], &keybuf[0], dbkey[0] + 1);     //   store last dbkey
#endif
      chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
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

