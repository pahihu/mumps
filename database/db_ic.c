// File: mumps/database/db_ic.c
//
// module database - Database Functions, Integrity Check

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

int icerr;						// error count
int doing_full;						// type of ic

u_char wrt_buf[100];					// for output
cstring *outc;						// ditto

u_char *rlnk;						// for right links
u_char *dlnk;						// for down links
u_char *used;						// for the map
u_int volsiz;						// blocks in volume

void ic_full();						// full check
void ic_bits(u_int block, int flag, u_int points_at);	// check bits
u_int ic_block(u_int block, u_int points_at,
	      u_char *kin, chr_x *global);		// check block
void ic_map(int flag, int vol, int dbfd);		// check the map

//-----------------------------------------------------------------------------
// Function: DB_ic
// Descript: Do integrity check on vol according to flag
// Input(s): Volume number
//	     Check flag
// Return:   Number of errors found
//

int DB_ic(int volume, int block)                  	 // integrity checker
{ 
  if (volume > MAX_VOL)					// within limits?
  { return (-ERRM26);					// no - error
  }
  if (systab->vol[volume-1]->vollab == NULL)		// is it mounted?
  { return (-ERRM26);					// no - error
  }
  volnum = volume;					// save this
  // curr_lock = 0;					// ensure this is clear
  if (curr_lock)
    SemOp(SEM_GLOBAL, -curr_lock);
  writing = 0;						// clear this
  icerr = 0;						// clear errors
  doing_full = 0;					// and this
  outc = (cstring *) wrt_buf;				// for reporting
  used = ((u_char *) systab->vol[volnum-1]->map);	// point at map
  volsiz = systab->vol[volnum-1]->vollab->max_block;	// number of blocks
  gbd_expired = 0;					// clear this
  for (level = 0; level < MAXTREEDEPTH; blk[level++] = NULL);

  if (block == 0)					// full check?
  { level = 0;
    ic_full();						// do it
    gbd_expired = GBD_EXPIRED;
    return icerr;					// and return
  }
  else if (block > 0)
  { level = 1;
    //ic_block(block, 0, NULL, 0);			// check it
    ic_block(block, 0, NULL, NULL);			// check it
    gbd_expired = GBD_EXPIRED;
    return icerr;					// and return
  }
  // dbfd = partab.vol_fds[volnum - 1];			// set this up
  ic_map(block, volnum-1, partab.vol_fds[volnum-1]);	// map check
  return icerr;						// and return
}

//-----------------------------------------------------------------------------
// Function: ic_full
// Descript: Do full integrity check on volnum (updates icerr)
// Input(s): none
// Return:   none
//

void ic_full()						// full check
{ int i;						// a handy int
  int j;						// and another
  int uci;						// uci#
  u_int b1;						// a block
  u_char off;						// offset
  u_char msg[20];					// for messages

  doing_full = 1;					// set this
  i = (volsiz / 8) + 1;					// number of bytes  
  rlnk = mv1malloc(i);					// for right links
  if (rlnk == NULL)					// if failed
  { panic("ic_full: can't get memory for rlnk");	// die
  }
  dlnk = mv1malloc(i);					// for down links
  if (dlnk == NULL)					// if failed
  { panic("ic_full: can't get memory for dlnk");	// die
  }
  bzero(rlnk, i);					// clear this
  bzero(dlnk, i);					// and this
  rlnk[0] = 1;						// say blk 0 used
  dlnk[0] = 1;						// say blk 0 used

  for (uci = 0; uci < UCIS; uci++)			// scan uci table
  { b1 = systab->vol[volnum-1]->vollab->uci[uci].global; // get GD
    if (b1 == 0)					// if none
    { continue;						// ignore it
    }
    if ((used[b1/8] & (1 << (b1 & 7))) == 0)		// if marked free
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d free (global directory for UCI %d) - skipped",
        b1, uci + 1);					// error msg
      icerr++;						// count it
      (void)SQ_Write(outc);				// output it
      (void)SQ_WriteFormat(SQ_LF);			// and a !
      continue;						// ignore it
    }
    ic_bits(b1, 3, 0);					// set link bits
    level = 0;						// clear level
    //ic_block(b1, 0, NULL, 0);				// check the block
    ic_block(b1, 0, NULL, NULL);			// check the block
  }							// end main for loop
  
  for (i = 0; i < (volsiz / 8); i++)			// for each byte in map
  { for (j = 0; j < 8; j++)				// for each bit
    { off = (1 << j);					// setup offset
      b1 = ((u_int) i * 8) + j;				// and block#
      bcopy("both pointers\0", msg, 14);		// default msg
      if ((used[i] & off) != 0)				// if used
      { if (((rlnk[i] & off) == 0) ||			// if no RL
	    ((dlnk[i] & off) == 0))			// or no DL
	{ if ((rlnk[i] & off) != 0)			// if it has RL
	  { bcopy("down pointer\0", msg, 13);		// say down
	  }
	  else if ((dlnk[i] & off) != 0)		// if it has DL
	  { bcopy("right pointer\0", msg, 14);		// say right
	  }
	  outc->len = sprintf((char *)&outc->buf[0],
	    "%10d is used, missing %s",
	    b1, msg);					// error msg
	  icerr++;					// count it
	  (void)SQ_Write(outc);				// output it
	  (void)SQ_WriteFormat(SQ_LF);			// and a !
	}						// end error code
      }							// end used block
      else if (((rlnk[i] & off) != 0) ||		// there is a RL
	       ((dlnk[i] & off) != 0))			// or a DL and NOT used
      { outc->len = sprintf((char *)&outc->buf[0],
      			    "%10d is UNUSED but is pointed to",
			    b1);
	icerr++;					// count it
	(void)SQ_Write(outc);				// output it
	(void)SQ_WriteFormat(SQ_LF);			// and a !
      }
    }
  }

  mv1free(rlnk);					// free that
  mv1free(dlnk);					// and that
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: ic_bits
// Descript: check/set bits in rlnk and dlnk
// Input(s): Block number to check
//	     flag: 1 = chk RL, 2 = chk DL, 3 = check both
//	     block that points at this block (if any)
// Return:   none
//

void ic_bits(u_int block, int flag, u_int points_at)	// check bits

{ u_int i;						// a handy int
  u_char off;						// bit offset

  i = block >> 3;					// byte number
  off = 1 << (block & 7);				// bit offset

  if (flag & 1)
  { if (rlnk[i] & off)					// check rlnk
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - Duplicate right pointer",
        block, points_at);				// error msg
      icerr++;						// count it
      (void)SQ_Write(outc);				// output it
      (void)SQ_WriteFormat(SQ_LF);			// and a !
    }
    else						// set the bit
    { rlnk[i] |= off;					// set
    }
  }
  if (flag & 2)
  { if (dlnk[i] & off)					// check dlnk
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - Duplicate down pointer",
        block, points_at);				// error msg
      icerr++;						// count it
      (void)SQ_Write(outc);				// output it
      (void)SQ_WriteFormat(SQ_LF);			// and a !
    }
    else						// set the bit
    { dlnk[i] |= off;					// set
    }
  }

  if ((points_at) &&					// points_at supplied
      ((used[i] & off) == 0))				// marked free?
  { outc->len = sprintf((char *)&outc->buf[0],
      "%10d <- %10d - block is free",
      block, points_at);				// error msg
    icerr++;						// count it
    (void)SQ_Write(outc);					// output it
    (void)SQ_WriteFormat(SQ_LF);				// and a !
  }
  return;						// done
}

//-----------------------------------------------------------------------------
// Function: ic_block
// Descript: check supplied block
// Input(s): Block number to check
// Return:   none
//

u_int ic_block(u_int block, u_int points_at,
	      u_char *kin, chr_x *p_global)		// check block
{ int i;						// a handy int
  short s;						// for funct
  int left_edge;					// a flag
  u_char emsg[80];					// for errors
  int isdata;						// blk type
  int Lidx;						// Local index
  int Llevel;						// local level
  int Llast;						// local last_idx
  gbd *Lgbd;						// and gbd
  u_int b1;						// a block
  u_char k[260];					// local key
  u_short *isx;						// a map
  u_int *iix;						// a map
  u_char k1[260];					// for keys
  u_char k2[260];					// for keys
  cstring *c;						// for chunk
  cstring *r;						// for record
  u_char *eob;						// end of block
  u_int lb;						// last block
  u_int brl;						// block rl

  while (SemOp( SEM_GLOBAL, READ));			// get a read lock

  s = Get_block_raw(block);	                        // get it
  if (s < 0)						// if that failed
  { i = SemOp( SEM_GLOBAL, -curr_lock);			// release the lock
    s = UTIL_strerror(s, emsg);				// decode message
    outc->len = sprintf((char *)&outc->buf[0],
      "%10d <- %10d - error getting - %s",
      block, points_at, emsg);				// error msg
    icerr++;						// count it
    s = SQ_Write(outc);					// output it
    s = SQ_WriteFormat(SQ_LF);				// and a !
    return 0;						// and exit
  }

  if ((used[block/8] & (1 << (block & 7))) == 0)	// if marked free
  { s = SemOp( SEM_GLOBAL, -curr_lock);		        // release the lock
    outc->len = sprintf((char *)&outc->buf[0],
      "%10d <- %10d marked free, type = %d",
      block, points_at, blk[level]->mem->type);		// error msg
      icerr++;						// count it
      s = SQ_Write(outc);				// output it
      s = SQ_WriteFormat(SQ_LF);			// and a !
      return 0;						// give up
  }

  eob = (u_char *) blk[level]->mem
        + systab->vol[volnum-1]->vollab->block_size - 1;

  if (blk[level]->dirty == NULL)
  { blk[level]->dirty = (gbd *) 3;			// reserve it
  }

  isdata = ((blk[level]->mem->type > 64) && (level));	// blk type
  Llevel = level;					// save this
  Lgbd = blk[level];					// and this
  s = SemOp( SEM_GLOBAL, -curr_lock);			// release the lock

  //if (global)
  //{ if (global != blk[level]->mem->global)		// check global
  if (p_global)
  { if (_X_NE(p_global, &blk[level]->mem->global))		// check global
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - global is wrong",
        block, points_at);				// error msg
      icerr++;						// count it
      s = SQ_Write(outc);				// output it
      s = SQ_WriteFormat(SQ_LF);			// and a !
    }
  }

  chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at 1st chunk
  left_edge = (!chunk->buf[1]);				// check for first
  if (chunk->buf[0])					// non-zero ccc
  { outc->len = sprintf((char *)&outc->buf[0],
      "%10d <- %10d - non-zero ccc on first key",
      block, points_at);				// error msg
    icerr++;						// count it
    s = SQ_Write(outc);					// output it
    s = SQ_WriteFormat(SQ_LF);				// and a !
  }
  else if (kin != NULL)					// if key supplied
  { if (bcmp(kin, &chunk->buf[1], kin[0]+1))		// if not the same
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - downlink differs from first key",
        block, points_at);				// error msg
      icerr++;						// count it
      s = SQ_Write(outc);				// output it
      s = SQ_WriteFormat(SQ_LF);			// and a !
    }
  }

  if (!isdata)						// if a pointer
  { Llast = blk[level]->mem->last_idx;			// remember this
    lb = 0;						// clear this
    brl = 0;						// and this
    for (Lidx = LOW_INDEX; Lidx <= Llast; Lidx++)
    { level = Llevel;					// restore this
      if ((!level) && (Lidx ==LOW_INDEX))		// ignore entry
      { continue;					// for $GLOBAL in GD
      }
      blk[level] = Lgbd;				// and this
      idx = (u_short *) blk[level]->mem;		// point at the block
      iidx = (int *) blk[level]->mem;			// point at the block
      chunk = (cstring *) &iidx[idx[Lidx]];		// point at the chunk
      if (!level)					// a GD
      { k[0] = '\0';					// empty key
      }
      else						// pointer
      { bcopy(&chunk->buf[2], &k[chunk->buf[0]+1],
	      chunk->buf[1]);				// update the key
        k[0] = chunk->buf[0] + chunk->buf[1];		// and the size
      }
      record = (cstring *) &chunk->buf[chunk->buf[1]+2]; // point at the dbc
      Allign_record();					// ensure alligned
      b1 = *(u_int *) record;				// get blk#
      if ((b1 > volsiz) || (!b1))			// out of range
      { outc->len = sprintf((char *)&outc->buf[0],
          "%10d <- %10d - (%d) block %d outside vol - skipped",
          block, points_at, Lidx, b1);			// error msg
        icerr++;					// count it
        s = SQ_Write(outc);				// output it
        s = SQ_WriteFormat(SQ_LF);			// and a !
	continue;					// ignore
      }
      for (i = 0; i < level; i++)			// scan above
      { if ((blk[level]) && (blk[level]->block == b1))	// check for loop
	{ outc->len = sprintf((char *)&outc->buf[0],
            "%10d <- %10d - points at itself",
	    b1, block);					// error msg
	  icerr++;					// count it
	  s = SQ_Write(outc);				// output it
	  s = SQ_WriteFormat(SQ_LF);			// and a !
	  b1 = 0;					// flag error
	  break;					// quit
        }
      }							// end loop check
      if (!b1)						// check again
      { continue;
      }
      if (doing_full)
      { ic_bits(b1, 2 + (left_edge || !level), block);	// check bits
      }
      if ((lb) && (level))				// if we have a lb
      { if (brl != b1)					// if not the same
	{ outc->len = sprintf((char *)&outc->buf[0],
            "%10d <- %10d - right is %10d next down is %10d",
	    lb, block, brl, b1);			// error msg
	  icerr++;					// count it
	  s = SQ_Write(outc);				// output it
	  s = SQ_WriteFormat(SQ_LF);			// and a !
	}
      }
      lb = b1;						// save for next
      left_edge = 0;					// clear this
      level++;						// down a level
      if (level > 1)					// from a pointer
      //{ brl = ic_block(b1, block, k, blk[level-1]->mem->global); // check block
      { brl = ic_block(b1, block, k, &blk[level-1]->mem->global); // check block
      }
      else						// from GD
      //{ brl = ic_block(b1, block, k, 0); // check the block (DO BETTER LATER)
      { brl = ic_block(b1, block, k, NULL); // check the block (DO BETTER LATER)
      }
    }							// end block scan
  }							// end if (!isdata)
  level = Llevel;					// restore this
  blk[level] = Lgbd;					// and this
  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block

  if ((blk[level]->mem->right_ptr) && (doing_full))	// if we have a RL
  { ic_bits(blk[level]->mem->right_ptr, 1, block);	// say so
  }
  if (blk[level]->dirty == (gbd *) 3)			// if we reserved it
  { blk[level]->dirty = NULL;				// clear it
  }

  if (blk[level]->mem->last_idx < LOW_INDEX)
  { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - last_idx is too low",
        block, points_at);				// error msg
    icerr++;						// count it
    s = SQ_Write(outc);					// output it
    s = SQ_WriteFormat(SQ_LF);				// and a !
  }

  if (((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2) < 0)
  { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - last_idx too high or last_free too low",
        block, points_at);				// error msg
    icerr++;						// count it
    s = SQ_Write(outc);					// output it
    s = SQ_WriteFormat(SQ_LF);				// and a !
  }
  isdata = ((blk[level]->mem->type > 64) && (level));
  isx = (u_short *) blk[level]->mem;
  iix = (u_int *) blk[level]->mem;
  k1[0] = 0;

  for (i = LOW_INDEX; i <= blk[level]->mem->last_idx; i++)
  { c = (cstring *) &iix[isx[i]];
    if (&c->buf[c->len - 3] > eob)
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - chunk size is too big - overflows block",
        block, points_at);				// error msg
      icerr++;						// count it
      s = SQ_Write(outc);				// output it
      s = SQ_WriteFormat(SQ_LF);			// and a !
    }
    r = (cstring *) &c->buf[c->buf[1] + 2];
    if ((isdata) && (r->len != -1))
    { if (&r->buf[r->len - 1] > eob)
      { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - dbc is too big - overflows block",
        block, points_at);				// error msg
        icerr++;					// count it
        s = SQ_Write(outc);				// output it
        s = SQ_WriteFormat(SQ_LF);			// and a !
      }
    }
    if (c->buf[0] == 255)
    { continue;
    }
    if ((i == LOW_INDEX) && (c->buf[0]))
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - non-zero ccc in first record",
        block, points_at);				// error msg
      icerr++;						// count it
      s = SQ_Write(outc);				// output it
      s = SQ_WriteFormat(SQ_LF);			// and a !
    }
    if ((i > LOW_INDEX) && (!c->buf[1]))
    { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - zero ucc found",
        block, points_at);				// error msg
      icerr++;						// count it
      s = SQ_Write(outc);				// output it
      s = SQ_WriteFormat(SQ_LF);			// and a !
    }
    bcopy(&c->buf[2], &k2[c->buf[0] + 1], c->buf[1]);
    k2[0] = c->buf[0] + c->buf[1];
    if ((k2[0]) || (i > LOW_INDEX))
    { if (UTIL_Key_KeyCmp(&k1[1], &k2[1], k1[0], k2[0]) != K2_GREATER)
      { outc->len = sprintf((char *)&outc->buf[0],
        "%10d <- %10d - (%d) key does not follow previous",
        block, points_at, i);				// error msg
        icerr++;					// count it
        s = SQ_Write(outc);				// output it
        s = SQ_WriteFormat(SQ_LF);			// and a !
      }
    }
    bcopy(k2, k1, k2[0] + 1);
  }
  return blk[level]->mem->right_ptr;			// save for return
}

//-----------------------------------------------------------------------------
// Function: ic_map
// Descript: check map block
// Input(s): flag, -1 = Check only, -2 = Check and fix, -3 as -2 + track upto
//           dbfd - database file descriptor
// Return:   none
//

void ic_map(int flag, int vol, int dbfd)		// check the map
{ int i;						// a handy int
  u_int block;						// current block
  u_int base;						// current block base
  off_t file_off;                               	// for lseek() et al
  int lock;						// required lock
  int off;						// offset in byte
  u_char *c;						// map ptr
  u_char *e;						// end of map
  gbd *ptr;						// a handy pointer
  int status;						// block status
  u_char type_byte;					// for read

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  lock = (flag == -1) ? READ : WRITE;			// what we need
  c = (u_char *) systab->vol[vol]->map;		        // point at it
  e = &c[systab->vol[vol]->vollab->max_block >> 3];     // and the end
  off = 1;						// start at 1
  while (c <= e)					// scan the map
  { base = ((u_int) (c - (u_char *) systab->vol[vol]->map)) << 3;
							// base block number
    while (SemOp(SEM_GLOBAL, lock));			// grab a lock

    for (off = off; off < 8; off++)			// scan the byte
    { block = base + off;				// the block#
      status = -1;					// not yet known
      if (block > systab->vol[vol]->vollab->max_block)
      { continue;
      }
      ptr = systab->vol[vol]->gbd_hash[GBD_BUCKET(block)];
      while (ptr != NULL)				// scan for block
      { if (ptr->block == block)			// if found
	{ type_byte = ptr->mem->type;			// save this
	  if (ptr->mem->type)				// if used
	  { status = 1;					// say used
	  }
	  else
	  { status = 0;
	  }
	  break;					// and quit loop
	}
	ptr = ptr->next;				// point at next
      }							// end memory check
      if (status == -1)					// if not found
      { file_off = (off_t) block - 1;			// block#
        file_off = (file_off * (off_t)
    			systab->vol[vol]->vollab->block_size)
		 + (off_t) systab->vol[vol]->vollab->header_bytes;
        file_off = lseek( dbfd, file_off, SEEK_SET);	// Seek to block
        if (file_off < 1)
        { panic("ic_map: lseek failed!!");		// die on error
        }
        i = read( dbfd, &type_byte, 1);			// read one byte
	if (i < 0)
        { panic("ic_map: read failed!!");		// die on error
        }
	status = (type_byte != 0);			// check used
      }							// end disk read
      if ((*c & (1 << off)) && (status))		// used and OK
      { continue;					// go for next (in for)
      }
      if (((*c & (1 << off)) == 0) && (!status))	// free and OK
      { continue;					// go for next (in for)
      }
      icerr++;						// count error
      if (flag == -1)					// check only
      { continue;					// continue
      }
      if (status)					// used
      { *c |= (u_char) (1 << off);			// set the bit
      }
      else						// free
      { *c &= (u_char) (~(1 << off));			// clear it
      }
      Mark_map_dirty(vol, block);			// map needs writing
    }							// end byte scan
    SemOp(SEM_GLOBAL, -curr_lock);			// free lock
    c++;						// point at next
    off = 0;						// now start at 0
    if (flag == -3)					// daemon?
    { systab->vol[vol]->upto = ((u_int) (e - c)) << 3;
    }
  }							// end main while
  if (flag == -3)					// daemon?
  { systab->vol[vol]->upto = 0;			        // clear this
  }
  return;						// done
}
