// File: mumps/database/db_rekey.c
//
// module database - Database keying functions

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
// Function: Set_key
// Descript: Set the supplied block number to location pointed to by db_var
// Input(s): Block number to set, level to work on.
//	     Re_key (below) has refreshed blk[] and db_var
// Return:   0 -> Ok, negative MUMPS error
//

short Set_key(u_int ptr_blk, int this_level)		// set a block#
{ short s;						// for returns
  u_char tmp[MAX_NAME_BYTES];				// some space
  u_char gtmp[2*MAX_NAME_BYTES];			// to find glob
  int i;						// a handy int
  u_int *ui;						// an int ptr
  cstring *ptr;						// spare ptr
  int rs;						// reqd space
  int ts;						// trailing size
  int rls;						// RL space
  int trailings;					// ptr to orig trail
  gbd *cblk[3];						// current level blks
  u_int tgb;						// top blk in GD
  DB_Block *btmp;					// ditto

  ptr = (cstring *) tmp;				// point at this
  ptr->len = 4;						// always
  ui = (u_int *) ptr->buf;				// for pointers
  *ui = ptr_blk;					// copy this here
  
  level = this_level;					// set current level
  if (!this_level)					// top level split
  { gtmp[1] = 128;					// start string key
    for (i=0; i<MAX_NAME_BYTES; i++)			// for each char
    { if (db_var.name.var_cu[i] == '\0')		// check for null
      { break;						// break if found
      }
      gtmp[i+2] = db_var.name.var_cu[i];		// copy char
    }
    i +=2;						// correct count
    gtmp[i] = '\0';					// null terminate
    gtmp[0] = (u_char) i;				// add the count
    s=Get_block(systab->vol[db_var.volset-1]->vollab->uci[db_var.uci-1].global);
    if (s < 0)						// failed?
    { return s;						// return error
    }
    s = Locate(gtmp);					// search for it
    if (s < 0)						// failed?
    { return s;						// return error
    }
    Allign_record();					// if not alligned
    tgb = *(u_int *) record;				// get block#

    level = 1;						// at top level
    s = New_block();					// get a new block
    if (s < 0)						// if that failed
    { return s;						// return error
    }

    blk[level]->mem->type = db_var.uci;			// pointer block
    blk[level]->mem->last_idx = LOW_INDEX;		// first Index
    blk[level]->mem->last_free
      = (systab->vol[volnum-1]->vollab->block_size >> 2) - 3; // use 2 words
    bcopy(&db_var.name.var_cu[0], &blk[level]->mem->global, MAX_NAME_BYTES);
    idx[LOW_INDEX] = blk[level]->mem->last_free + 1;	// the data
    chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at it
    chunk->len = 8;					// used two words
    chunk->buf[0] = 0;					// ccc
    chunk->buf[1] = 0;					// ucc
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// setup record ptr

    Allign_record();
    *( (u_int *) record) = tgb;				// first entry
    s = Insert(&db_var.slen, ptr);			// insert this one
    if (s < 0)						// failed?
    { return s;						// return error
    }
    level = 0;						// point at GD
    s = Locate(gtmp);					// search for it
    if (s < 0)						// failed?
    { return s;						// return error
    }
    Allign_record();					// if not alligned
    *( (u_int *) record) = blk[1]->block;		// new top level blk
    level = 1;
    blk[level]->dirty = blk[level];			// hook to self
    TXSET(blk[level]);
    if (blk[0]->dirty == (gbd *) 1)			// if not queued
    { blk[0]->dirty = blk[level];			// hook it
      TXSET(blk[0]);
      level = 0;					// and clear level
    }
    Queit();						// que to write
    return 0;						// end of level == 0
  }

  s = Get_data(this_level);				// get the key blk
  if (s >= 0)						// if found
  { return -(ERRMLAST+ERRZ61);				// database problem
  }
  if (s != -ERRM7)					// any other error
  { return s;						// give up
  }
  if (blk[level]->block == ptr_blk)
  { return -(ERRMLAST+ERRZ61);				// database problem
  }
  Index++;						// point at insert
							// see Get_data()
  trailings = Index;					// remember for later
  if (trailings < LOW_INDEX + 1)			// if junk
  { return -(ERRMLAST+ERRZ61);				// database stuffed
  }

  s = Insert(&db_var.slen, ptr);			// attempt to insert
  if (s == 0)						// if that worked
  { 
    if (blk[level]->dirty == (gbd *) 1)
    { blk[level]->dirty = blk[level];			// hook to self
      TXSET(blk[level]);
      Queit();						// and que
    }
    level--;						// backup a level
    Release_GBDs(0);
    return 0;						// exit **0**
  }
  else if (s != -(ERRMLAST+ERRZ62))
  { return s;						// error!
  }

  ts = 0;						// none yet
  if (trailings <= blk[level]->mem->last_idx)		// if any
  { i = trailings;					// start here
    chunk = (cstring *) &iidx[idx[i]];			// point at first chunk
    ts = chunk->buf[0];					// get ccc
    while (i <= blk[level]->mem->last_idx)		// loop thru trailings
    { chunk = (cstring *) &iidx[idx[i]];		// point at chunk
      ts += (chunk->len + 2);				// add the chunk + idx
      i++;						// increment idx
    }
  }
  rs = 4 + db_var.slen + 4;				// reqd key + ptr space
  rs = ((rs + 3) >> 2) << 2;				// round it up
  rs += 4;						// allow for index

  cblk[0] = blk[level];					// remember this
  cblk[1] = NULL;					// clear this
  cblk[2] = NULL;					// and this
  rls = 0;						// nothing here yet
  if (blk[level]->mem->right_ptr)			// if there is one
  { s = Get_block(blk[level]->mem->right_ptr);		// get it
    cblk[2] = blk[level];				// remember address
    if (blk[level]->mem->flags & BLOCK_DIRTY)		// check state
    { Tidy_block();					// ensure tidy
    }
    rls = (blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2;
  }

  if ((ts < rls) && (ts))				// if trailings -> RL
  { Un_key();						// unlink RL key

    Get_GBD();						// get another gbd
    bzero(blk[level]->mem, systab->vol[(volnum)-1]->vollab->block_size); // zot
    blk[level]->mem->type = cblk[2]->mem->type;		// copy type
    blk[level]->mem->right_ptr = cblk[2]->mem->right_ptr; // copy RL
    blk[level]->mem->global = cblk[2]->mem->global;	// copy global name
    blk[level]->mem->last_idx = LOW_INDEX-1;		// unused block
    blk[level]->mem->last_free
      = (systab->vol[volnum-1]->vollab->block_size >> 2) - 1; // set this up
    keybuf[0] = 0;					// clear this
    if ((ts + rs) < rls)				// if new record fits
    { s = Insert(&db_var.slen, ptr);			// insert it
      if (s < 0)					// failed ?
      { panic("Set_key: Insert in new block (RL) failed");
      }
      bcopy(&chunk->buf[1], keybuf, chunk->buf[1] + 1);	// save key
    }
    Copy_data(cblk[0], trailings);			// copy trailings
    Copy_data(cblk[2], LOW_INDEX);			// and old RL XXX

    btmp = blk[level]->mem;				// save this
    blk[level]->mem = cblk[2]->mem;			// copy in this
    cblk[2]->mem = btmp;				// end swap 'mem'
    Free_GBD(volnum-1, blk[level]);			// give it back

    blk[level] = cblk[0];				// orig blk again
    idx = (u_short *) blk[level]->mem;			// point at it
    iidx = (int *) blk[level]->mem;			// point at it
    for (i = trailings; i <= blk[level]->mem->last_idx; i++)
    { chunk = (cstring *) &iidx[idx[i]];		// point at the chunk
      record = (cstring *) &chunk->buf[chunk->buf[1]+2]; // point at the dbc
      Allign_record();					// align
      *(int *) record = PTR_UNDEFINED;			// mark as junk
    }
    Tidy_block();					// tidy it
    if ((ts + rs) < rls)				// if new record done
    { goto fix_keys;					// exit **1**
    }

    s = Insert(&db_var.slen, ptr);			// attempt to insert
    if (s >= 0)						// if OK
    { goto fix_keys;					// exit **2**
    }
    else if (s != -(ERRMLAST+ERRZ62))
    { return s;						// error!
    }

    s = New_block();					// new blk for insert
    if (s < 0)						// if failed
    { panic("Set_key: Failed to get new block for insert");
    }
    
    blk[level]->mem->type = cblk[0]->mem->type;		// copy type
    blk[level]->mem->right_ptr = cblk[0]->mem->right_ptr; // copy RL
    blk[level]->mem->global = cblk[0]->mem->global;	// copy global name
    blk[level]->mem->last_idx = LOW_INDEX - 1;		// unused block
    blk[level]->mem->last_free
    = (systab->vol[volnum-1]->vollab->block_size >> 2) - 1; // set this up
    keybuf[0] = 0;					// clear this

    cblk[0]->mem->right_ptr = blk[level]->block;	// point at it
    s = Insert(&db_var.slen, ptr);			// insert it
    if (s < 0)						// failed ?
    { panic("Set_key: Insert in new block (insert) failed");
    }
    cblk[1] = blk[level];				// remember this one
    goto fix_keys;					// exit **3**
  }							// end trailings in RL

  if ((rs < rls) && (!ts))				// if will fit in RL
  { blk[level] = cblk[2];				// point at RL
    Un_key();						// delete current key
    s = Insert(&db_var.slen, ptr);			// insert it
    if (s >= 0)						// if OK
    { goto fix_keys;					// exit **6**
    }
    else if (s != -(ERRMLAST+ERRZ62))
    { return s;						// error!
    }
  }

  else if (cblk[2] != NULL)				// if RL allocated
  { if (cblk[2]->dirty == (gbd *) 1)			// if reserved
    { cblk[2]->dirty = NULL;				// clear it
    }
    cblk[2] = NULL;					// flag not used
  }

  s = New_block();					// new blk for trail
  if (s < 0)						// if failed
  { panic("Set_key: Failed to get new block for trailings");
  }
    
  blk[level]->mem->type = cblk[0]->mem->type;		// copy type
  blk[level]->mem->right_ptr = cblk[0]->mem->right_ptr; // copy RL
  blk[level]->mem->global = cblk[0]->mem->global;	// copy global name
  blk[level]->mem->last_idx = LOW_INDEX - 1;		// unused block
  blk[level]->mem->last_free
    = (systab->vol[volnum-1]->vollab->block_size >> 2) - 1; // set this up
  keybuf[0] = 0;					// clear this

  cblk[0]->mem->right_ptr = blk[level]->block;		// point at it
  cblk[1] = blk[level];					// save this one

  if (ts)						// if any trailings
  { Copy_data(cblk[0], trailings);			// copy trailings

    blk[level] = cblk[0];				// orig blk again
    idx = (u_short *) blk[level]->mem;			// point at it
    iidx = (int *) blk[level]->mem;			// point at it
    for (i = trailings; i <= blk[level]->mem->last_idx; i++)
    { chunk = (cstring *) &iidx[idx[i]];		// point at the chunk
      record = (cstring *) &chunk->buf[chunk->buf[1]+2]; // point at the dbc
      Allign_record();					// align
      *(int *) record = PTR_UNDEFINED;			// mark as junk
    }
    Tidy_block();					// tidy it

    s = Insert(&db_var.slen, ptr);			// attempt to insert
    if (s >= 0)						// if OK
    { goto fix_keys;					// exit **4**
    }
    else if (s != -(ERRMLAST+ERRZ62))
    { return s;						// error!
    }
    blk[level] = cblk[1];				// new blk again
    idx = (u_short *) blk[level]->mem;			// point at it
    iidx = (int *) blk[level]->mem;			// point at it
  }
  s = Insert(&db_var.slen, ptr);			// attempt to insert
  if (s >= 0)						// if OK
  { goto fix_keys;					// exit **5**
  }
  else if (s != -(ERRMLAST+ERRZ62))
  { return s;						// error!
  }
  panic("Set_key: Options 0->6 didn't work");		// die

fix_keys:

  blk[level] = NULL;					// clear this
  for (i = level - 1; i >= 0; i--)			// scan ptr blks
  { 
    if (blk[i]->dirty == (gbd *) 2)			// if changed
    { if (blk[level] == NULL)				// list empty
      { blk[i]->dirty = blk[i];				// point at self
	TXSET(blk[i]);
      }
      else
      { blk[i]->dirty = blk[level];			// else point at prev
	TXSET(blk[i]);
      }
      blk[level] = blk[i];				// remember this one
    }
    else if (blk[i]->dirty == (gbd *) 1)		// if reserved
    { blk[i]->dirty = NULL;				// clear it
    }
  }

  for (i = 0; i < 3; i++)				// scan cblk[]
  { if (cblk[i] == NULL)				// if empty
    { continue;						// ignore it
    }
    if (cblk[i]->dirty == (gbd *) 1)			// not queued
    { if (blk[level] == NULL)				// list empty
      { cblk[i]->dirty = cblk[i];			// point at self
	TXSET(cblk[i]);
      }
      else
      { cblk[i]->dirty = blk[level];			// else point at prev
	TXSET(cblk[i]);
      }
      blk[level] = cblk[i];				// remember this one
    }
  }
  if (blk[level] != NULL)				// if something there
  { Queit();						// que that lot
  }
  for (i = 1; i < 3; i++)				// scan cblk[] again
  { if (cblk[i] != NULL)				// if there
    { s = Add_rekey(cblk[i]->block, this_level);	// que a fix
    }
  }							// end fix key loop
  return 0;						// done
}

//-----------------------------------------------------------------------------
// Function: Add_rekey
// Descript: Set the supplied block number and level into rekey table
// Input(s): Block number to set, level it is currently at.
// Return:   0
//

short Add_rekey(u_int block, int level)			// add to re-key table
{ int i;						// a handy int
  for (i = 0; i < MAXREKEY; i++)			// scan table
  { if (!rekey_blk[i])					// if empty
    { rekey_blk[i] = block;				// save block
      rekey_lvl[i] = level;				// and level
      return 0;						// and exit
    }
  }
  panic("Add_rekey: rekey table overflowed - database is corrupt");
  return 0;						// won't happen
}

//-----------------------------------------------------------------------------
// Function: Re_key
// Descript: Re key all blocks in the re-key table
// Input(s): none
// Return:   0 or negative MUMPS error
//

short Re_key()						// re-key blocks
{ int i;						// a handy int
  short s;						// for functions
  int low_level;					// lowest found
  int low_index;					// where found

  while (TRUE)						// loop
  { low_level = -1;					// clear this
    low_index = -1;					// and this
    for (i = 0; i < MAXREKEY; i++)			// search table
    { if (rekey_blk[i])					// if something there
      { if (rekey_lvl[i] > low_level)			// higher than got
        { low_level = rekey_lvl[i];			// save level
	  low_index = i;				// and the index
        }
      }
    }
    if (low_index == -1)				// if none found
    { return 0;						// all done
    }
    ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.blkreorg); // update stats
    level = 0;						// clear level
    s = Get_block(rekey_blk[low_index]);		// get the block
    if (s < 0)
    { return -(ERRMLAST+ERRZ61);			// database stuffed
    }
    chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at first chunk
    bcopy(&chunk->buf[1], &db_var.slen, chunk->buf[1] + 1); // copy key

    if (blk[level]->dirty == (gbd *) 1)			// if reserved
    { blk[level]->dirty = NULL;				// clear it
    }							// this shouldn't be
    s = Set_key(rekey_blk[low_index], low_level - 1);	// doit
    if (s < 0)						// on fail
    { return s;
    }
    rekey_lvl[low_index] = 0;				// clear this
    rekey_blk[low_index] = 0;				// and this
    if (low_level == 1)					// if a top split
    { for (i = 0; i < MAXREKEY; i++)			// search table
      { if (rekey_lvl[i])				// if something there
        { rekey_lvl[i]++;				// increment it
	}
      }
    }
  }							// end while (TRUE)
  return 0;						// can't get here
}

//-----------------------------------------------------------------------------
// Function: Un_key
// Descript: Un key current blk[level]
// Input(s): none
// Return:   none
//

void Un_key()

{ u_int this_level;
  u_int save_level;
  u_int xxx_level;
  short s;						// for returns
  u_char cstr[8];					// and another
  u_int *xui;						// an int ptr
  cstring *xptr;					// spare ptr
  gbd *save;						// save a block
  u_char *uptr;						// a handy ptr
  u_char *lptr;						// a handy ptr
  u_int blkno;						// a block number
  int garbed;                                           // did a Garbit()?

  garbed = 0;
  this_level = level;					// save for ron

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at first chunk
  uptr = &(chunk->buf[1]);				// point at key

  for (level = level - 1; level; level--)		// for each above level
  { s = Locate(uptr);					// look for key
    if (s == -ERRM7)					// if not found
    { if (Index > blk[level]->mem->last_idx)		// if ran off end
      { save = blk[level];				// save this one
	s = Locate_next(keybuf);			// get rl
	if (s == 0)					// if one there
	{ s = Locate(uptr);				// look for key
	  if (s < 0)					// if not found
	  { if (blk[level]->dirty == (gbd *) 1)		// if reserved
	    blk[level]->dirty = NULL;			// free it
	    blk[level] = save;				// put this one back
	  }
	  else
	  { if (save->dirty == (gbd *) 1)		// if this reserved
	    save->dirty = NULL;				// clear it
	  }
	}
      }
    }
    if (s == 0)						// if found
    { Allign_record();					// align
      *(int *) record = PTR_UNDEFINED;			// mark as junk
      Tidy_block();					// and tidy the block

      if (level < (this_level - 1))			// if up > 1 level
      { if (blk[level + 1]->mem->last_idx > LOW_INDEX-1)// and if lower not mt
	{ idx = (u_short *) blk[level + 1]->mem;	// point at the block
	  iidx = (int *) blk[level + 1]->mem;		// point at the block
	  chunk = (cstring *) &iidx[idx[LOW_INDEX]];	// point at first chunk
	  lptr = &(chunk->buf[1]);			// point at key
	  xptr = (cstring *) cstr;			// point at spare
	  xptr->len = 4;				// one int
	  xui = (u_int *) xptr->buf;			// point the int here
	  *xui = blk[level + 1]->block;			// get the block#
	  s = Insert(lptr, xptr);			// insert that
	  if (s == -(ERRMLAST+ERRZ62))
	  { s = Add_rekey(blk[level + 1]->block, level + 1); // do it later
	  }
	  else if (s < 0)
	  { panic("Un_Key: Insert returned fatal value");
	  }
	}
	else						// lower level is empty

	{ save_level = level;				// remember where we at
	  blkno = 0;					// clear block#

	  while (TRUE)
	  { s = Locate(uptr);				// find key - must fail
            // 210109AP TESTING
	    if ((save_level == level) && (s != -ERRM7)) // if not - die
	    { panic("Un_key: key locate at 'level' didn't return -ERRM7");
	    }
	    if (Index > LOW_INDEX)			// if not first node
	    { chunk = (cstring *) &iidx[idx[Index - 1]]; // point at prev
	      record = (cstring *) &chunk->buf[chunk->buf[1]+2]; // point at it
	      Allign_record();				// align
	      blkno = *(u_int *) record;		// get the number
	      break;					// and exit loop
	    }
	    level--;					// up a level
	    if (!level)					// if not found
	    { panic("Un_key: failed to determine left edge");
	    }
	  }						// end while (TRUE)

	  while (level < save_level)
	  { xxx_level = level;				// remember this
	    level = MAXTREEDEPTH - 1;			// use this one
	    s = Get_block(blkno);
	    if (s < 0)
	    { panic("Un_key(): Get_block() failed in left block tree");
	    }
	    s = Locate(uptr);				// find key - must fail
	    if (s != -ERRM7)				// if not - die
	    { panic("Un_key: key locate in left edge didn't return -ERRM7");
	    }
	    chunk = (cstring *) &iidx[idx[Index - 1]]; // point at prev
	    record = (cstring *) &chunk->buf[chunk->buf[1]+2]; // point at it
	    Allign_record();				// align
	    blkno = *(u_int *) record;			// get the number
	    if (blk[level]->dirty == (gbd *) 1)
	    { blk[level]->dirty = NULL;			// free gbd
	    }
	    level = xxx_level;				// restore level
	    level++;					// and increment it
	  }
	  xxx_level = MAXTREEDEPTH - 1;			// use this one
	  level++;					// point at mt blk
	  blk[xxx_level] = blk[level];			// remember that there
	  s = Get_block(blkno);
	  if (s < 0)
	  { panic("Un_key(): Get_block() failed for left block");
	  }
	  blk[level]->mem->right_ptr = blk[xxx_level]->mem->right_ptr;
	  if (blk[level]->dirty == (gbd *) 1)		// if we changed it
	  { blk[level]->dirty = (gbd *) 2;		// mark for write
	  }
          blk[xxx_level]->mem->type = 65;               // 210109AP TESTING
          blk[xxx_level]->last_accessed = MTIME(0) + 86400;
          REFD_MARK(blk[xxx_level]);
	  Garbit(blk[xxx_level]->block);		// dump mt blk
          garbed = 1;
	  level = save_level;				// restore level
	}						// end empty block proc

      }
      if (blk[level]->dirty == (gbd *) 1)		// if we changed it
      { blk[level]->dirty = (gbd *) 2;			// mark for write
      }
    }
    else
    {  break;						// no more to find
    }
  }
  level = this_level;					// restore level

  if (garbed)                                           // did a Garbit()?
  { ClearLastBlk();                                     // zot last_blk[]
  }

  return;
}
