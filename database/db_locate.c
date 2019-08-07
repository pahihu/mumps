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

//-----------------------------------------------------------------------------
// Function:    Build_KeyBuf
// Descript:    build up keybuf[] in pKeyBuf for pIndex
// Inputs:      pIndex - index of entry, pKeyBuf[] - output buffer,
//		doCopy - copy key to pKeyBuf
// Return:	pointer to keybuf
// Notes:	MV1_CCC always copies the key, because it builds incrementally
//		If not in MV1_CCC mode, then we can avoid copying.
//

u_char* Build_KeyBuf(int pIndex, u_char *pKeyBuf, int doCopy)
{ int i;                                                // handy int

  pKeyBuf[0] = 0;
#ifdef MV1_CCC
  for (i = LOW_INDEX; i <= pIndex; i++)			// incrementally build
  { chunk = (cstring *) &iidx[idx[i]];			// point at chunk
    bcopy(&chunk->buf[2], &pKeyBuf[chunk->buf[0]+1],    // update the keybuf
	  chunk->buf[1]);				// always copy
    pKeyBuf[0] = chunk->buf[0] + chunk->buf[1];
  }
#else
  if (pIndex >= LOW_INDEX)				// valid index ?
  { chunk = (cstring *) &iidx[idx[pIndex]];		// point at chunk
    if (doCopy)						// need to copy?
    { bcopy(&chunk->buf[2], &pKeyBuf[1], chunk->buf[1]);// copy to keybuf
      pKeyBuf[0] = chunk->buf[1];
    }
    return &chunk->buf[1];				// return ptr chunk
  }
#endif
  return &pKeyBuf[0];					// return ptr to buf
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

#ifdef MV1_CCC

short Locate(u_char *key)				// find key
{ int i;						// a handy int

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  Index = LOW_INDEX;					// start at the start
  while (TRUE)						// loop
  { chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    i = UTIL_Key_KeyCmp(&keybuf[1], &key[1], keybuf[0], key[0]); // compare
    if (i == KEQUAL)					// same?
    { return 0;						// done
    }
    if (i == K2_LESSER)					// passed it?
    { return -ERRM7;					// no such
    }
    Index++;						// point at next
    if (Index > blk[level]->mem->last_idx)		// passed the end
    { return -ERRM7;					// no such
    }
  }							// end locate loop
}

short LocateEx(u_char *key, int frominsert, int indexTip)
{
  return Locate(key);
}

#else

short LocateEx(u_char *key, int frominsert, int indexTip)
{ int i;						// a handy int
  int L, R;                                             // bin.search limits
  u_char *dbkey;                                        // ptr to key in blk

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block

  Index = LOW_INDEX;					// start at the start
  L = LOW_INDEX; R = blk[level]->mem->last_idx;         // setup limits

  dbkey = &keybuf[0];

  if ((frominsert) &&                                   // insert: check last
      (blk[level]->mem->right_ptr == 0))                //   if no right_ptr
  { Index = R;
#ifdef MV1_CCC
    dbkey = Build_KeyBuf(Index, &keybuf[0], KEY_COPY);  // build keybuf
#else
    dbkey = Build_KeyBuf(Index, &keybuf[0], KEY_NOCOPY);// point at the chunk
#endif
    i = UTIL_Key_KeyCmp(&dbkey[1], &key[1], dbkey[0], key[0]); // cmp
    if (i == K2_GREATER)                                // key > last key
    { Index = R + 1;                                    // early exit
      if (dbkey != &keybuf[0])
        bcopy(&dbkey[0], &keybuf[0], dbkey[0] + 1);     // store last dbkey
      return -ERRM7;
    }
    else if (i != K2_LESSER)
    { goto K2_EQUAL;
    }
  }

  if (R - L < 25)					// small no of elts ?
  { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.eventcnt);
    Index = LOW_INDEX;					// start at the start
    while (TRUE)						// loop
    { chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
      dbkey = Build_KeyBuf(Index, &keybuf[0], KEY_NOCOPY);// point at the chunk
      record = (cstring *) &chunk->buf[chunk->buf[1]+2];// point at the dbc
      i = UTIL_Key_KeyCmp(&dbkey[1], &key[1], dbkey[0], key[0]); // compare
      if (i == KEQUAL)					// same?
      { bcopy(&dbkey[0], &keybuf[0], dbkey[0] + 1);     // store last dbkey
        return 0;					// done
      }
      if (i == K2_LESSER)				// passed it?
      { bcopy(&dbkey[0], &keybuf[0], dbkey[0] + 1);     // store last dbkey
        return -ERRM7;					// no such
      }
      Index++;						// point at next
      if (Index > blk[level]->mem->last_idx)		// passed the end
      { bcopy(&dbkey[0], &keybuf[0], dbkey[0] + 1);     // store last dbkey
        return -ERRM7;					// no such
      }
    }							// end locate loop
  }

  while (L <= R)                                        // loop
  { Index = (L + R) >> 1;                               // find middle
#ifdef MV1_CCC
    dbkey = Build_KeyBuf(Index, &keybuf[0], KEY_COPY);
#else
    dbkey = Build_KeyBuf(Index, &keybuf[0], KEY_NOCOPY);// point at the chunk
#endif
    i = UTIL_Key_KeyCmp(&dbkey[1], &key[1], dbkey[0], key[0]); // compare
    if (i == K2_GREATER)                                // not reached, adj. L
    { L = Index + 1;
      continue;
    }
    else if (i == K2_LESSER)                            // passed, adj. R
    { R = Index - 1;
      continue;
    }
K2_EQUAL:
    if (dbkey != &keybuf[0])
      bcopy(&dbkey[0], &keybuf[0], dbkey[0] + 1);       // store last dbkey
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    return 0;						// key found, done
  }
  Index = L;                                            // not found
  if (Index <= blk[level]->mem->last_idx)               // update if Index valid
    chunk = (cstring *) &iidx[idx[Index]];	        // point at the chunk
  else
    chunk = (cstring *) &iidx[idx[blk[level]->mem->last_idx]]; // point to chunk

  bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],	// update the key
              chunk->buf[1]);
  keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  record = (cstring *) &chunk->buf[chunk->buf[1]+2];  	// point at the dbc
  return -ERRM7;
}

short Locate(u_char *key)
{
  return LocateEx(key, 0, 0);
}

#endif

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
  { bcopy(&chunk->buf[2], &out[chunk->buf[0]+1],
	chunk->buf[1]);					// update the key
    out[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  }
  record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
  return 0;						// all done
}

void LocateCountP(gbd *ptr,u_char *key,
		  const char *path,int line)		// find key
{ int i, s;						// a handy int
  u_int cnt;
  u_char *keyptr;
  u_short *idx;
  int *iidx;
  cstring *chunk;

  cnt = 0;
  idx = (u_short *) ptr->mem;				// point at the block
  iidx = (int *) ptr->mem;				// point at the block
  for (i = LOW_INDEX; i <= ptr->mem->last_idx; i++)// loop
  { chunk = (cstring *) &iidx[idx[i]];			// point at the chunk
    keyptr = &(chunk->buf[1]);				// and the size
    s = UTIL_Key_KeyEqu(&keyptr[1], &key[1], keyptr[0], key[0]); // compare
    if (s == KEQUAL)					// same?
      cnt++;
  }							// end locate loop
Lcheck:
  if (cnt != 1)
  { char msg[512];
    sprintf(msg,"LocateCount: more than one key (%s,%d)\r\n",path,line);
    panic(msg);
  }
}

void LocateAllP(gbd *ptr,int level,const char *path,int line)// find key
{ int i;						// a handy int
  u_int cnt;
  short s;
  u_char *key;
  u_short *idx;
  int *iidx;
  cstring *chunk, *record;
  int isdata;
  u_int blknum;

  isdata = (ptr->mem->type > 64);
  idx = (u_short *) ptr->mem;				// point at the block
  iidx = (int *) ptr->mem;				// point at the block
  for (i = LOW_INDEX; i <= ptr->mem->last_idx; i++)
  { chunk = (cstring *) &iidx[idx[i]];			// point at the chunk
    if (!isdata ||					// ptr block
	(isdata && !level))				// or top-level
    { record = (cstring *) &chunk->buf[chunk->buf[1] + 2];// where the data goes
      if ((long) record & 3)                            // if not alligned
      { record = (cstring *) &record->buf[2 - ((long) record & 3)];// allign
      }
      if (((int*) record)[0] != PTR_UNDEFINED)		// if not undefined
      { blknum = ((u_int*) record)[0];			// get blkno
        s = Check_BlockNo(volnum-1, blknum,             // check blknum, die
                CBN_ALLOCATED | CBN_INRANGE,    
                "LocateAllP", path, line, 1);
      }
    }
    key = &(chunk->buf[1]);				// only uncomp. keys
    LocateCountP(ptr, key, path, line);
  }							// end locate loop
}
