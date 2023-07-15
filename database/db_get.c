// File: mumps/database/db_get.c
//
// module database - Get Database Functions

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

#ifdef MV1_CACHE_DEBUG
short logit(int where,short ret)
{ fprintf(stderr,"--- Get_data@%d: ret = %d\r\n",where,ret); fflush(stderr);
  return ret;
}
#else
#define logit(where,ret)        ret
#endif

//-----------------------------------------------------------------------------
// Function: Get_data
// Descript: Locate and return data described in db_var
// Input(s): Direction (flag), negative means backwards, 0 forward
//	     > 0 means stop at this level.
// Return:   String length -> Ok, negative MUMPS error
//	     extern variables defined in db_main.c are also setup
//		level 	-> pointer to current level in blk[]
//		blk[]	-> from 0 to level (how we got here)
//			   unless blk[0] == NULL (for lastused)
//	     This calls Locate() which sets up chunk, record, idx,
//		iidx, keybuf Index.
// NOTE: lastused block is NOT used if dir != 0 or writing

short Get_data(int dir)		                        // locate a record
{ int i, j;						// a handy int
  short s;						// for function returns
  u_char tmp[2*MAX_NAME_BYTES-1];			// spare string
  gbd *ptr;						// handy pointer

  if (!curr_lock)					// ensure locked
  { s = SemOp( SEM_GLOBAL, READ);			// take a read lock
    if (s < 0)						// if we got an error
    { return logit(1,s);				// return it
    }
  }

  if (systab->vol[db_var.volset-1]->vollab == NULL)	// vol still mounted?
  { return logit(2,(-ERRM26));				// no - error
  }
  if ((bcmp("$GLOBAL\0", &db_var.name.var_cu[0], 8) == 0) || // if ^$G
      (dir != 0) ||					// or level or backward
      /* NB. we CAN write without journaling */
      (// (systab->vol[volnum - 1]->vollab->journal_available) && // or journaling
       (writing + wanna_writing)))		        // and writing
  { systab->vol[volnum - 1]->last_blk_used[MV1_PID] = 0; // zot this
  }
  else
  { i = systab->vol[volnum - 1]->last_blk_used[MV1_PID];// get last used
    if ((i) &&                                          // any used ?
        ((((u_char *)systab->vol[volnum-1]->map)[i>>3]) &(1<<(i&7))))
							// still allocated ?
    { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.lasttry); // count a try
      if (LB_ENABLED == gbd_local_state)
      { ptr = LB_GetBlock(i);
        if (NULL == ptr)
          return -(ERRZ94 + ERRMLAST);
        goto Found;
      }
      ptr = systab->vol[volnum-1]->gbd_hash[GBD_BUCKET(i)]; // get listhead
      while (ptr != NULL)				// for each in list
      { if (ptr->block == i)				// found it
        { //if ((ptr->mem->global != db_var.name.var_qu) || // wrong global or
Found:    if ((X_NE(ptr->mem->global, 
                                db_var.name.var_xu)) || // wrong global or
	      (ptr->mem->type != (db_var.uci + 64)) ||	// wrong uci/type or
	      (ptr->last_accessed <= (time_t) 0))	// not available
          { break;					// exit the loop
	  }
	  if (LB_FILL == gbd_local_state)
          { LB_AddBlock(ptr);
          }
	  level = LAST_USED_LEVEL;			// use this level
	  blk[level] = ptr;				// point at it
	  s = Locate(&db_var.slen);	                // check for the key
	  if ((s >= 0) ||				// if found or
	      ((s = -ERRM7) &&				// not found and
	       (Index <= blk[level]->mem->last_idx) &&	// still in block
	       (Index > LOW_INDEX)))			// not at begining
	  { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.lastok);
                                                        // count success
            if (LB_DISABLED == gbd_local_state)
	      blk[level]->last_accessed = MTIME(0);	// accessed
            REFD_MARK(blk[level]);
            for (i = 0; i < level; blk[i++] = NULL);	// zot these
	    if (!s)					// if ok
	    { s = record->len;				// get the dbc
	    }
	    if ((writing) && (blk[level]->dirty == NULL)) // if writing
	    { Reserve_GBD(blk[level]);                  //  reserve it
	    }
	    if ((!db_var.slen) && (!s) &&
	        ((partab.jobtab->last_block_flags[volnum - 1] &
                        GL_TOP_DEFINED) == 0))
	    { s = -ERRM7;				// check for top node
	    }
	    return logit(3,s);				// and return
	  }
	  blk[level] = NULL;				// clear this
	  level = 0;					// and this
	  break;					// and exit loop
        }						// end found block
        ptr = ptr->next;				// get next
      }							// end while ptr
    }							// end last used stuff
    systab->vol[volnum - 1]->last_blk_used[MV1_PID] = 0;// zot it
  }

  i = systab->vol[db_var.volset-1]->vollab->uci[db_var.uci-1].global;
							// get directory blk#
  if (!i)						// if nosuch
  { return logit(4,(-ERRM26));				// then error
  }

  level = 0;						// where it goes
  s = Get_block(i);					// get the block
  if (s < 0)						// error?
  { return logit(5,s);					// give up
  }

  if (bcmp("$GLOBAL\0", &db_var.name.var_cu[0], 8) == 0) // if ^$G
  { s = Locate(&db_var.slen);				// look for it
    if (s >= 0)						// if found
    { Allign_record();
    }
    return logit(6,s);					// end ^$G() lookup
  }

  tmp[1] = 128;						// start string key
  for (i=0; i<MAX_NAME_BYTES; i++)			// for each char
  { if (db_var.name.var_cu[i] == '\0')			// check for null
    { break;						// break if found
    }
    tmp[i+2] = db_var.name.var_cu[i];			// copy char
  }
  i +=2;						// correct count
  tmp[i] = '\0';					// null terminate
  tmp[0] = (u_char) i;					// add the count

  s = Locate(tmp);					// search for it
  if (s < 0)						// failed?
  { return logit(7,s);					// return error
  }
  partab.jobtab->last_block_flags[volnum -1] = 0;       // clear JIC
  Allign_record();					// if not alligned
  i = *(int *) record;					// get block#
  if (!i)						// none there?
  { return logit(8,-ERRM7);				// say nosuch
  }
  partab.jobtab->last_block_flags[volnum - 1] = 
                        ((u_int *) record)[1];          // save flags

  if (partab.jobtab->last_block_flags[volnum - 1] > GL_FLAGS)// TEMP      ????
  { partab.jobtab->last_block_flags[volnum - 1] &= GL_FLAGS; // CLEAR UNUSED   ????
    ((u_int *) record)[1] = 
           partab.jobtab->last_block_flags[volnum - 1]; // RESET     ????
  }							//		  ????

  level++;						// where we want it
  s = Get_block(i);					// get the block
  if (s < 0)						// error?
  { return logit(9,s);					// give up
  }
  while (blk[level]->mem->type < 65)			// while we have ptrs
  { 
    //if (blk[level]->mem->global != db_var.name.var_qu)
    if (X_NE(blk[level]->mem->global, db_var.name.var_xu))
    { return logit(10,-(ERRMLAST+ERRZ61));		// database stuffed
    }
    s = Locate(&db_var.slen);				// locate the key
    if (s == -ERRM7)					// failed to find?
    { Index--;						// yes, backup the Index
    }
    else if (s < 0)					// else if error
    { return logit(11,s);				// return it
    }
    else if (dir < 0)					// if found and want -
    { Index--;						// backup the Index
      if (Index < LOW_INDEX)				// can't happen?
      { panic("Get_data: Problem with negative direction");
      }
    }

    chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    Allign_record();					// if not alligned
    if (level == dir)					// stop here?
    { return logit(12,s);				// yes - return result
    }
    i = *(int *) record;				// get block#
    level++;						// where it goes
    s = Get_block(i);					// get the block
    if (s < 0)						// error?
    { return logit(13,s);				// give up
    }
  }							// end while ptr

  //if (blk[level]->mem->global != db_var.name.var_qu)
  if (X_NE(blk[level]->mem->global, db_var.name.var_xu))
  { return logit(14,-(ERRMLAST+ERRZ61));		// database stuffed
  }
  s = Locate(&db_var.slen);				// locate key in data
  if (dir < 1)					        // if not a pointer
  { systab->vol[volnum - 1]->last_blk_used[MV1_PID] = i;// set last used
    systab->vol[volnum - 1]->last_idx_used[MV1_PID] = Index;
  }
  if ((!db_var.slen) && (!s) &&
      ((partab.jobtab->last_block_flags[volnum - 1] & GL_TOP_DEFINED) == 0))
  { if (!record->len)
    { s = -ERRM7;					// check for top node
    }
  }

  if (!s)						// if ok
  { s = record->len;					// get the dbc
  }
  return logit(15,s);					// return result
}

// vim:ts=8:sw=8:et
