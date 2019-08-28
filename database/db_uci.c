// File: mumps/database/db_uci.c
//
// module database - Database Functions, UCI manipulation

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
// Function: DB_UCISet
// Descript: Create UCI
// Input(s): Vol#
//	     UCI#
//	     UCI Name
// Return:   0 -> Ok, negative MUMPS error
//

short DB_UCISet(int volume, int uci, var_u name)	// set uci name
{ short s;						// for functions
 
  if ((volume > MAX_VOL) || (volume < 1))		// within limits?
  { return (-ERRM26);					// no - error
  }
  if ((uci > UCIS) || (uci < 1))
  { return (-ERRM26);					// too big
  }

  while (systab->vol[volume - 1]->writelock ||		// check for write lock
         systab->delaywt)                               //   or delay WRITEs
  { (void)Sleep(5);					// wait a bit
    if (partab.jobtab->attention)
    { return -(ERRMLAST+ERRZ51);			// for <Control><C>
    }
  }							// end writelock check
  volnum = volume;					// set this
  writing = 1;						// writing
  level = 0;						// clear this
  s = SemOp( SEM_GLOBAL, WRITE);			// get write lock
  if (s < 0)						// on error
  { return s;						// return it
  }
  if (systab->vol[volnum-1]->vollab == NULL)		// is it mounted?
  { SemOp( SEM_GLOBAL, -curr_lock);
    return (-ERRM26);					// no - error
  }
  if (!systab->vol[volnum-1]->vollab->uci[uci-1].global)// if no GD
  { s = New_block();					// get a new block
    if (s < 0)						// if failed
    { SemOp( SEM_GLOBAL, -curr_lock);
      return s;						// error
    }
    systab->vol[volnum-1]->vollab->uci[uci-1].global
      = blk[level]->block;				// save block #
    blk[level]->mem->type = uci + 64;			// block type
    blk[level]->mem->last_idx = LOW_INDEX;		// one index
    X_set("$GLOBAL\0", &blk[level]->mem->global, 8);	// the global
    blk[level]->mem->last_free
      = (systab->vol[volnum-1]->vollab->block_size >> 2) - 7; // use 6 words
    idx[LOW_INDEX] = blk[level]->mem->last_free + 1;	// the data
    chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at it
    chunk->len = 24;					// 5 words
    chunk->buf[0] = 0;					// zero ccc
    chunk->buf[1] = 9;					// ucc
    bcopy("\200$GLOBAL\0",&chunk->buf[2], 9);		// the key
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// setup record ptr
    Allign_record();					// allign it
    *(u_int *) record = blk[level]->block;		// point at self
    *(u_int *) &(record->buf[2]) = 0;			// zero flags
    blk[level]->dirty = blk[level];			// setup for write
    TXSET(blk[level]);
    Queit();						// que for write
  }							// end new block code
  //systab->vol[vol-1]->vollab->uci[uci-1].name.var_qu
  //  = name.var_qu;					// set the new name
  systab->vol[volnum-1]->vollab->uci[uci-1].name.var_xu
    = name.var_xu;					// set the new name
  SemOp( SEM_GLOBAL, -curr_lock);
  return 0;						// and exit
}

//-----------------------------------------------------------------------------
// Function: DB_UCIKill
// Descript: Remove UCI
// Input(s): Vol#
//	     UCI#
// Return:   0 -> Ok, negative MUMPS error
//


short DB_UCIKill(int volume, int uci)			// kill uci entry
{ short s;						// for functions
  u_int gb;						// block number
  int netjobs;						// #jobs + #netdaemons

  if ((volume > MAX_VOL) || (volume < 1))		// within limits?
  { return (-ERRM26);					// no - error
  }
  if ((uci > UCIS) || (uci < 1))
  { return (-ERRM26);					// too big
  }
  while (systab->vol[volume - 1]->writelock ||		// check for write lock
         systab->delaywt)                               //   or delay WRITEs
  { (void)Sleep(5);					// wait a bit
    if (partab.jobtab->attention)
    { return -(ERRMLAST+ERRZ51);			// for <Control><C>
    }
  }							// end writelock check
  volnum = volume;					// set this
  writing = 1;						// writing
  level = 0;						// clear this
  s = SemOp( SEM_GLOBAL, WRITE);			// get write lock
  if (s < 0)						// on error
  { return s;						// return it
  }
  if (systab->vol[volnum-1]->vollab == NULL)		// is it mounted?
  { SemOp( SEM_GLOBAL, -curr_lock);
    return (-ERRM26);					// no - error
  }
  if (systab->vol[volnum-1]->vollab->
      uci[uci-1].name.var_cu[0] == '\0')		// does uci exits?
  { SemOp( SEM_GLOBAL, -curr_lock);
    return 0;						// no - just return
  }
  gb = systab->vol[volnum-1]->vollab->uci[uci-1].global;// get global directory
  s = Get_block(gb);					// get the block
  if (s < 0)
  { SemOp( SEM_GLOBAL, -curr_lock);
    return s;						// error
  }
  if (blk[level]->dirty == (gbd *) 1)			// if reserved
  { blk[level]->dirty = NULL;				// clear it
  }
  if (blk[level]->mem->last_idx > LOW_INDEX)		// if any globals
  { SemOp( SEM_GLOBAL, -curr_lock);
    return -(ERRM29);					// no can do
  }
  systab->vol[volnum-1]->vollab->uci[uci-1].global = 0;	// clear this
  //systab->vol[vol-1]->vollab->uci[uci-1].name.var_qu = 0; // and this
  X_Clear(systab->vol[volnum-1]->vollab->uci[uci-1].name.var_xu); // and this
  systab->vol[volnum-1]->map_dirty_flag |= VOLLAB_DIRTY;// mark map dirty
  blk[level]->mem->last_idx = LOW_INDEX - 1;		// say no index
  Garbit(gb);						// garbage it
  netjobs = systab->maxjob + systab->vol[0]->num_of_net_daemons;
  bzero(&systab->vol[volnum - 1]->last_blk_used[0],     // zot all
                        netjobs * sizeof(u_int)); 
  bzero(&systab->vol[volnum - 1]->last_blk_written[0],  // zot all
                        netjobs * sizeof(u_int));
  SemOp( SEM_GLOBAL, -curr_lock);
  return 0;						// exit
}
