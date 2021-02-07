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
#include <assert.h>					// for assert()
#include "mumps.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

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

static int KeyCmp(u_char *k1, u_char *k2, int klen1, int klen2, int *match)
{ int i, d;
  int klen;

  klen = klen1 < klen2 ? klen1 : klen2;
  d = 0;
  for (i = 0; i < klen; i++)
  { if ((d = k1[i] - k2[i]))
      break;
  }
  *match = i;
  return d ? d : (klen1 - klen2);
}

short Locate(u_char *key)			        // find key
{ int i, j;						// a handy int
  int ccc, ucc;                                         // ccc, ucc parts
  int match, newmatch;                                  // keybuf[] part matched
  short s;
  int pccc;                                             // prev. ccc
  u_char *pchunk;                                       // prev. chunk
  int upd;                                              // ucc updated?

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  Index = LOW_INDEX;					// start at the start
  match = 0; newmatch = 0;                              // keybuf[] part matched
  pchunk = NULL; pccc = 0;                              // prev. keybuf data
  while (TRUE)						// loop
  { chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
    ccc = chunk->buf[0];                                // get ccc, ucc
    ucc = chunk->buf[1];

    /* NB. We do not rebuild the differentially compressed key
     * entirely, and match only those part of it, which is not
     * already matched.
     */
    if (match && (ccc > pccc) && !upd)                  // update ccc key part
    { bcopy(pchunk, &keybuf[pccc+1], ccc - pccc);       //   only
    }
    upd = 0;

    if (!ccc)
    { i = KeyCmp(&chunk->buf[2], &key[1], ucc, key[0], &match);
    }
    else if (match)
    { if (ccc <= match)
      { i = KeyCmp(&chunk->buf[2], &key[1+ccc], ucc, key[0]-ccc, &newmatch);
        match = ccc + newmatch;
      }
      else // ccc > match
      { if (ccc > key[0])
        { i = KeyCmp(&keybuf[1+match], &key[1+match],   // compare until ccc
                ccc-match, key[0]-match, &newmatch);
        }
        else
        { i = KeyCmp(&keybuf[1+match], &key[1+match],   // compare until ccc
                ccc-match, ccc-match, &newmatch);
          if (0 == i)                                   // KEQUAL?
          { match = ccc;                                //   compare tail
            i = KeyCmp(&chunk->buf[2], &key[1+ccc],
                ucc, key[0]-ccc, &newmatch);
          }
        }
        match = match + newmatch;
      }
    }

    pccc = ccc; pchunk = &chunk->buf[2];                // save as previous

    if (i == 0 /*KEQUAL*/)                              // same?
    { s =  0;						// done
      goto Return;
    }

    if (i > 0 /*K2_LESSER*/)			        // passed it?
    { s = -ERRM7;					// no such
      goto Return;
    }
    Index++;						// point at next
    if (Index > blk[level]->mem->last_idx)		// passed the end
    { s = -ERRM7;					// no such
      goto Return;
    }
  }							// end locate loop
Return:
  if (!upd)                                             // ucc not updated?
  { bcopy(&chunk->buf[2], &keybuf[ccc+1], ucc);         //  update it!
  }
  keybuf[0] = ccc + ucc;		                // and the size
  record = (cstring *) &chunk->buf[ucc + 2];	        // point at the dbc
  return s;
}

short LocateOld(u_char *key)				// find key
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


//-----------------------------------------------------------------------------
// Function: Locate_next
// Descript: Locate next key in blk[level] updating extern vars
//	     Index, chunk, record and keybuf
// Input(s): none (extern vars must be setup)
// Return:   0 -> Ok, negative MUMPS error
// Note:     Must be be called with a lock
//	     External vars setup as for Locate() above.
//

short Locate_next()					// point at next key
{ int i;						// a handy int
  short s;						// function returns

  assert(curr_lock);
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

