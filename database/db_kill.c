// File: mumps/database/db_kill.c
//
// module database - Database Functions, Kill

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
#include <sys/types.h>               // lepard seems to want this
#include "mumps.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

//-----------------------------------------------------------------------------
// Function: Kill_data
// Descript: Remove the sub-tree described by db_var
// Input(s): none
// Return:   0 -> Ok, negative MUMPS error
//

short Kill_data()				        // remove tree
{ return Kill_data_ex(KILL_ALL);
}

//-----------------------------------------------------------------------------
// Function: Kill_data_ex
// Descript: Remove the sub-tree described by db_var
// Input(s): what - KILL_VAL/KILL_SUBS/KILL_ALL
// Return:   0 -> Ok, negative MUMPS error
//

short Kill_data_ex(int what)				// remove tree
{ short s;						// for functs
  int i;						// a handy int
  int j;						// and another
  gbd *rblk[MAXTREEDEPTH];				// right side tree
  gbd *leftblk;						// save left side tree
  gbd *ptr;						// spare ptr
  int rlevel;						// level in rblk[]
  u_int blknum;						// for block numbers
  u_char tmp[2*MAX_NAME_BYTES];				// spare string
  int top;						// top in complex kill
  u_char *p;						// a handy ptr
  cstring *c;						// and another
  u_int *ui;						// and another
  int qpos, wpos, rpos, qlen, qfree;
  int save_level;                                       // save level 

  bzero(rekey_blk, MAXREKEY * sizeof(u_int));		// clear that table
  bzero(rekey_lvl, MAXREKEY * sizeof(int));		// and that table

  SemOp( SEM_GLOBAL, -curr_lock);			// release read lock
  systab->last_blk_used[partab.jobtab - systab->jobtab] = 0; // clear last

start:
  Get_GBDs(MAXTREEDEPTH * 2);				// ensure this many
  wpos = systab->vol[volnum - 1]->garbQw;
  rpos = systab->vol[volnum - 1]->garbQr;
  if (rpos <= wpos) qlen = wpos - rpos;
  else
    qlen = NUM_GARB + wpos - rpos;
  qfree = NUM_GARB - qlen;
  if (qfree >= NUM_GARB/2)
    goto cont;
#if 0
  j = 0;                                                // clear counter
  qpos = systab->vol[volnum - 1]->garbQw;
  for (i = 0; i < NUM_GARB; i++)
  {
    if (systab->vol[volnum - 1]->garbQ[qpos] == 0)
    { if (j++ >= NUM_GARB/2) goto cont;                 // ensure we have 1/2 table
    }
    else
      break;
    qpos = (qpos + 1) & (NUM_GARB - 1);
  }
#endif
  systab->vol[volnum - 1]->stats.gqstall++;             // count garbQ stall
  SemOp( SEM_GLOBAL, -curr_lock);			// release current lock
  Sleep(1);
  goto start;

cont:
  writing = 1;						// say we are killing
  level = 0;						// reset level
  s = Get_data(0);					// attempt to get it
  if ((s < 0) && (s != -ERRM7))				// error, not undef
  { return s;						// return it
  }
  if ((systab->vol[volnum - 1]->vollab->journal_available) &&
      (systab->vol[volnum - 1]->vollab->journal_requested) &&
      (partab.jobtab->last_block_flags & GL_JOURNAL))	// if journaling
  { jrnrec jj;						// jrn structure
    jj.action = JRN_KILL;				// doing kill
    jj.uci = db_var.uci;				// copy UCI
    //jj.name.var_qu = db_var.name.var_qu;		// global name
    jj.name.var_xu = db_var.name.var_xu;		// global name
    jj.slen = db_var.slen;				// subs length
    bcopy(db_var.key, jj.key, jj.slen);			// copy key
    DoJournal(&jj, NULL);				// and do it
  }

  if ((db_var.slen == 0) && (KILL_VAL & what))	        // full global kill?
  { 
FullGlobalKill:
    while (level)					// for each level
    { if (blk[level]->dirty == (gbd *) 1)		// if reserved
      { blk[level]->dirty = NULL;			// clear it
      }
      level--;						// up a level
    }
    tmp[1] = 128;					// start string key
    for (i=0; i<MAX_NAME_BYTES; i++)			// for each char
    { if (db_var.name.var_cu[i] == '\0')		// check for null
      { break;						// break if found
      }
      tmp[i+2] = db_var.name.var_cu[i];			// copy char
    }
    i +=2;						// correct count
    tmp[i] = '\0';					// null terminate
    tmp[0] = (u_char) i;				// add the count
    s = Locate(tmp);					// search for it
    if (s == -ERRM7)
    { if (blk[level]->dirty == (gbd *) 1)		// if reserved
      { blk[level]->dirty = NULL;			// clear it
      }
      return 0;						// nothing to do
    }
    Allign_record();					// align
    blknum = *(u_int *) record;				// remember the block
    if (KILL_ALL == what)
    { *(u_int *) record = PTR_UNDEFINED;		// mark as junk
      Tidy_block();					// and tidy it
    }
    else
      ((u_int *) record)[1] &= ~GL_TOP_DEFINED;         // clear top node

#ifdef XMV1_BLKVER
    blk[level]->blkver_low++;
#endif
    if (blk[level]->dirty == (gbd *) 1)			// if reserved
    { blk[level]->dirty = blk[level];			// set it
      Queit();						// and que for write
    }
    if (KILL_ALL == what)
      Garbit(blknum);					// garbage the block
    else
    { save_level = level;
      level = 0;
      s = Get_data(0);
      if ((s < 0) && (s != -ERRM7))
        return s;
      chunk = (cstring *) &iidx[idx[LOW_INDEX]];	// point at the chunk
      record = (cstring *) &chunk->buf[chunk->buf[1] + 2]; // point at record
      record->len = 0;			                // mark empty
      if (blk[level]->dirty == (gbd *) 1)
      { blk[level]->dirty = blk[level];
        Queit();
      }
      level = save_level;
    }
    bzero(&systab->last_blk_used[0], systab->maxjob * sizeof(int)); // zot all
    bzero(&systab->last_blk_written[0], systab->maxjob * sizeof(int));// zot all
    level--;						// backup a level

    return 0;						// and exit
  }							// end full kill

  systab->last_blk_used[partab.jobtab - systab->jobtab] = 0; // clear last
  while (level >= 0)					// what we just got
  { if (blk[level]->dirty == (gbd *) 1)			// if reserved
    { blk[level]->dirty = NULL;				// clear it
    }
    level--;						// up a level
  }

  db_var.key[db_var.slen++] = 255;			// modify key
  s = Get_data(0);					// attempt to get it
  if (s != -ERRM7)					// must be undefined
  { return -(ERRMLAST+ERRZ61);				// database stuffed
  }
  db_var.slen--;					// put count back

  rlevel = level;					// number in right side
  for (i = 0; i <= level; i++)				// for each level
  { rblk[i] = blk[i];					// copy gbd
  }
  level = 0;						// reset level
  systab->last_blk_used[partab.jobtab - systab->jobtab] = 0; // clear last
  s = Get_data(-1);					// get left side
  if ((s < 0) && (s != -ERRM7))				// error, not undef
  { return s;						// return it
  }				// WARNING: This leaves blocks reserved
  if (rlevel != level)					// check this
  { panic("Kill_data: left level not equal right level"); // die
  }
  for (level = 0; level < rlevel; level++)		// scan the levels
  { if (blk[level + 1] != rblk[level + 1])		// check following lvl
    { break;						// end loop
    }
  }
  if (level == rlevel)					// all in 1 data block
							// NEVER first one
  { i = Index;						// start here
    while (i <= blk[level]->mem->last_idx)		// while in block
    { chunk = (cstring *) &iidx[idx[i]];		// point at the chunk
      bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
            chunk->buf[1]);				// fix the key
      keybuf[0] = chunk->buf[0] + chunk->buf[1];	// and the size
      if ((keybuf[0] < db_var.slen) ||			// new key too small
	  (bcmp(&keybuf[1], &db_var.key, db_var.slen)))	// or different
      { break;						// quit loop
      }
      if (0 == (KILL_VAL & what))                       // don't kill value
      { if ((keybuf[0] == db_var.slen) &&               // if value, then
            (0 == bcmp(&keybuf[1], &db_var.key, db_var.slen)))
        { i++;                                          //   skip
          continue;
        }
      }
      else if (0 == (KILL_SUBS & what))                 // don't kill subs
      { if ((keybuf[0] > db_var.slen) &&                // if subs, then
            (0 == bcmp(&keybuf[1], &db_var.key, db_var.slen)))
        { i++;                                          // skip
          continue;
        }
      }
      record = (cstring *) &chunk->buf[chunk->buf[1] + 2]; // point at record
      record->len = NODE_UNDEFINED;			// mark not reqd
      i++;						// point at next
    }							// end removing recs

    Tidy_block();					// tidy the block
    if (!db_var.slen)                                   // KVALUE on global
    { s = Locate_next(0);                               // subscripted nodes ?
      if ((0 > s) && (s != -ERRM7))                     // got an error
        return s;                                       //   return
      if (-ERRM7 == s)                                  // no more nodes
      { what = KILL_ALL;                                //   do a full kill
        goto FullGlobalKill;                          
      }
    }
#ifdef XMV1_BLKVER
    blk[level]->blkver_low++;
#endif
    if (blk[level]->dirty == (gbd *) 1)			// if reserved
    { blk[level]->dirty = blk[level];			// set it
      Queit();						// and que for write
    }

    for (level = 0; level < rlevel; level++)		// scan the levels
    { if (blk[level]->dirty == (gbd *) 1)		// reserved?
      { blk[level]->dirty = NULL;			// yes, clear it
      }
    }
    return 0;					        // and exit
  }							// end all in 1

// We need to do a multi block kill - we now have:
// top common block at [level] in both trees
// left edge -> blk[]           rblk[] <- right edge
// the bottom level is [rlevel] in both trees
// Note: it is possible that no killable nodes live in the left edge,
//	 and we will never point at Index LOW_INDEX in the left edge
//       BUT, the RL may have to be changed.

  top = level;						// save for ron
  for (i = 0; i < top; i++)				// scan upper bit
  { if (blk[i]->dirty == (gbd *) 1)			// reserved?
    { blk[i]->dirty = NULL;				// yes, clear it
    }
  }
  for (level = top; level <= rlevel; level++)		// scan left edge
  { s = Locate(&db_var.slen);				// locate the record
    if ((s < 0) && (s != -ERRM7))			// error?
    { return s;						// give up
    }				// WARNING: This leaves blocks reserved
    for (i = Index; i <= blk[level]->mem->last_idx; i++) // scan block
    { chunk = (cstring *) &iidx[idx[i]];		// point at the chunk
      bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	    chunk->buf[1]);				// update the key
      keybuf[0] = chunk->buf[0] + chunk->buf[1];	// and the size
      if ((keybuf[0] < db_var.slen) ||			// new key too small
	  (bcmp(&keybuf[1], &db_var.key, db_var.slen)))	// or different
      { break;						// quit loop
      }
      if (0 == (KILL_VAL & what))                       // don't kill value
      { if ((keybuf[0] == db_var.slen) &&               // if value, then
            (0 == bcmp(&keybuf[1], &db_var.key, db_var.slen)))
          continue;                                     //   skip
      }
      else if (0 == (KILL_SUBS & what))                 // don't kill subs
      { if ((keybuf[0] > db_var.slen) &&                // if subs, then
            (0 == bcmp(&keybuf[1], &db_var.key, db_var.slen)))
          continue;                                     // skip
      }
      record = (cstring *) &chunk->buf[chunk->buf[1]+2]; // point at the dbc

      if (level != rlevel)				// if a pointer blk
      { Allign_record();				// allign the pointer
	j = *(int *) record;				// get blk#
	if (j != rblk[level + 1]->block)		// if not right edge
	{ Garbit(j);					// garbage it
	}
        *(int *) record = PTR_UNDEFINED;		// mark as junk
      }
      else						// its a data blk
      { record->len = NODE_UNDEFINED;			// mark as junk
      }
      blk[level]->mem->flags |= BLOCK_DIRTY;		// mark it so
    }							// end block scan
    if (blk[level]->mem->flags & BLOCK_DIRTY)		// if we changed it
    { Tidy_block();					// tidy it
    }
    if (level > top)					// not at top
    { blk[level]->mem->right_ptr = rblk[level]->block;	// hook to right edge
    }
  }							// end left edge scan

  for (level = rlevel; level > top; level--)		// scan right edge (up)
  { leftblk = blk[level];				// save left here
    blk[level] = rblk[level];				// get right one
    idx = (u_short *) blk[level]->mem;			// point at the block
    iidx = (int *) blk[level]->mem;			// point at the block
    Index = LOW_INDEX;					// start at the start
    while (Index <= blk[level]->mem->last_idx)		// scan the block
    { chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
      bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	    chunk->buf[1]);				// update the key
      keybuf[0] = chunk->buf[0] + chunk->buf[1];	// and the size
      if ((keybuf[0] < db_var.slen) ||			// new key too small
	  (bcmp(&keybuf[1], &db_var.key, db_var.slen)))	// or different
      { break;						// quit loop
      }
      if (0 == (KILL_VAL & what))                       // don't kill value
      { if ((keybuf[0] == db_var.slen) &&               // if value, then
            (0 == bcmp(&keybuf[1], &db_var.key, db_var.slen)))
        { Index++;                                      // skip
          continue;
        }
      }
      else if (0 == (KILL_SUBS & what))                 // don't kill subs
      { if ((keybuf[0] > db_var.slen) &&                // if subs, then
            (0 == bcmp(&keybuf[1], &db_var.key, db_var.slen)))
        { Index++;                                      // skip
          continue;
        }
      }
      record = (cstring *) &chunk->buf[chunk->buf[1]+2]; // point at the dbc
      if (level != rlevel)				// if a pointer blk
      { Allign_record();				// allign the pointer
	j = *(int *) record;				// get blk#
	if (rblk[level + 1] != NULL)			// if there is level up
	{ if (j != rblk[level + 1]->block)		// if not right edge
	  { Garbit(j);					// garbage it
	  }
        }
	else						// no level up
	{ Garbit(j);					// garbage it anyway
	}
        *(int *) record = PTR_UNDEFINED;		// mark as junk
      }
      else						// it's data
      { record->len = NODE_UNDEFINED;			// mark as junk
      }
      blk[level]->mem->flags |= BLOCK_DIRTY;		// mark it so
      Index++;						// next
    }							// end block scan
    if (blk[level]->mem->flags & BLOCK_DIRTY)		// if we changed it
    { Tidy_block();					// tidy it
    }

    if ((level < rlevel) &&				// if in a ptr blk
	(rblk[level + 1] != NULL))			// and is lower level
    { idx = (u_short *) rblk[level + 1]->mem;		// point at the block
      iidx = (int *) rblk[level + 1]->mem;		// point at the block
      chunk = (cstring *) &iidx[idx[LOW_INDEX]];	// point at first chunk
      p = &chunk->buf[1];				// point at the key
      s = Locate(p);					// see if it's there
      if (s == -ERRM7)					// if it isn't
      { c = (cstring *) tmp;				// point at this
	c->len = 4;					// the size
        ui = (u_int *) c->buf;				// point the int here
        *ui = rblk[level + 1]->block;			// get the block#
        s = Insert(p, c);				// insert the node
	if (s == -(ERRMLAST+ERRZ62))
	{ s = Add_rekey(rblk[level + 1]->block, level + 1); // do it later
	}
	else if (s < 0)
	{ return s;					// error!
	}
      }							//
    }							// end of insert ptr

    if (((((leftblk->mem->last_free*2 + 1 - leftblk->mem->last_idx)*2)
        +((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
	> (systab->vol[volnum-1]->vollab->block_size - BLK_HDR_SIZE))
                                                        // if will fit in 1
	|| (blk[level]->mem->last_idx < LOW_INDEX))	// or empty blk
    { ptr = blk[level];					// right edge
      blk[level] = leftblk;				// left edge
      idx = (u_short *) blk[level]->mem;		// point at the block
      iidx = (int *) blk[level]->mem;			// point at the block
      if (ptr->mem->last_idx > LOW_INDEX-1)		// if any data
      { Copy_data(ptr, LOW_INDEX);			// copy to left edge
      }
      blk[level]->mem->right_ptr = ptr->mem->right_ptr;	// copy right ptr
      ptr->mem->type = 65;				// say type = data!!
      ptr->last_accessed = MTIME(0);			// clear last access
      Garbit(ptr->block);				// dump the block
      rblk[level] = NULL;				// mark gone
    }							// end move to one
    blk[level] = leftblk;				// restore left edge
  }							// end right edge scan

// Now ensure that the right edge has a pointer in [top] - (level == top)
  if (rblk[top + 1] != NULL)				// and there is level+1
  { idx = (u_short *) rblk[top + 1]->mem;		// point at the block
    iidx = (int *) rblk[top + 1]->mem;			// point at the block
    chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at the chunk
    p = &chunk->buf[1];					// point at the key
    s = Locate(p);					// see if it's there
    if (s == -ERRM7)					// if it isn't
    { c = (cstring *) tmp;				// point at this
      c->len = 4;					// the size
      ui = (u_int *) c->buf;				// point the int here
      *ui = rblk[level + 1]->block;			// get the block#
      s = Insert(p, c);					// insert the node
      if (s == -(ERRMLAST+ERRZ62))
      { s = Add_rekey(rblk[level + 1]->block, level + 1); // do it later
      }
      else if (s < 0)
      { return s;					// error!
      }
    }							// end of insert ptr
  }							// end ptr level
  level = MAXTREEDEPTH - 1;				// a usefull level
  blk[level] = NULL;					// clear this
  for (i = top; i <= rlevel; i++)			// scan left list
  { 
#ifdef XMV1_BLKVER
    blk[i]->blkver_low++;
#endif
    if (blk[i]->dirty == (gbd *) 1)			// reserved?
    { if (blk[level] == NULL)				// if list not started
      { blk[i]->dirty = blk[i];				// point at self
      }							// end start of list
      else						// just add it in
      { blk[i]->dirty = blk[level];			// point at previous
      }
      blk[level] = blk[i];				// remember this one
    }
  }							// end scan
  for (i = top + 1; i <= rlevel; i++)			// scan right list
  { if (rblk[i] != NULL)				// if anything there
    { 
#ifdef XMV1_BLKVER
      rblk[i]->blkver_low++;
#endif
      if (rblk[i]->dirty == (gbd *) 1)			// reserved?
      { if (blk[level] == NULL)				// if list not started
        { rblk[i]->dirty = rblk[i];			// point at self
        }						// end start of list
        else						// just add it in
        { rblk[i]->dirty = blk[level];			// point at previous
        }
        blk[level] = rblk[i];				// remember this one
      }
    }							// end not NULL
  }							// end scan

  if (blk[level] != NULL)				// anything to que
  { Queit();						// yes - do so
  }							// end right edge stuff

  bzero(&systab->last_blk_used[0], systab->maxjob * sizeof(int)); // zot all
  bzero(&systab->last_blk_written[0], systab->maxjob * sizeof(int)); // zot all

  return Re_key();					// re-key and return
}
