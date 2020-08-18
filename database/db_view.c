// File: mumps/database/db_view.c
//
// module database - Database Functions, View

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
// Function: DB_ViewGet
// Descript: return gbd address of specified block, null on err
// Input(s): Vol# and Block# to get
// Return:   Address of gbd or null on error
//

struct GBD *DB_ViewGet(int volume, int block)		// return gbd for blk
{ short s;						// for func

  ASSERT(0 < volume);                                   // valid volume
  ASSERT(volume <= MAX_VOL);
  ASSERT(NULL != systab->vol[volume-1]->vollab);        // mounted

#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"--- DB_ViewGet: %d\r\n",block);fflush(stderr);
#endif
  if ((block < 1) || (block > systab->vol[volume-1]->vollab->max_block))
  { return NULL;					// validate
  }
  level = 0;						// where it goes
  volnum = volume;					// need this
  writing = 0;						// clear this
  s = SemOp(SEM_GLOBAL, READ);				// write lock
  if (s < 0)						// check error
  { return NULL;					// quit if so
  }
  s = GetBlockRaw(block,__FILE__,__LINE__);             // get it
  if (s >= 0)
  { blk[level]->last_accessed = MTIME(0)+86400;		// push last access
  }
  if (curr_lock)
  { SemOp( SEM_GLOBAL, -curr_lock);			// unlock the globals
  }
  return (s < 0) ? NULL : blk[level];			// return whatever
}

//-----------------------------------------------------------------------------
// Function: DB_ViewPut
// Descript: Queue a block for write
// Input(s): Vol# and gbd ptr of block
// Return:   0 -> Ok, negative MUMPS error
//

short DB_ViewPut(int volume, struct GBD *ptr)		// que block for write
{ short s;						// for funcs

  ASSERT(0 < volume);                                   // valid volume
  ASSERT(volume <= MAX_VOL);
  ASSERT(NULL != systab->vol[volume-1]->vollab);        // mounted

#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"--- DB_ViewPut: %d\r\n",ptr->block);fflush(stderr);
#endif
  volnum = volume;					// for ron
  writing = 0;						// clear this
  s = SemOp(SEM_GLOBAL, WRITE);				// write lock
  if (s < 0)						// check error
  { return s;						// quit if so
  }
  ptr->last_accessed = MTIME(0);			// reset access
  if (ptr->mem->type)					// if used
  { Used_block(volnum-1, ptr->block);			// mark it so
  }
  else 							// trying to free it
  { Free_block(volnum-1, ptr->block);			// do so
    bzero(ptr->mem, systab->vol[volnum-1]->vollab->block_size); // clear it
  }
  level = 0;						// for Queit
  if (ptr->dirty == NULL)				// check dirty ptr
  { ptr->dirty = ptr;					// set if reqd
    blk[level] = ptr;					// ditto
    TXSET(blk[level]);
    Queit();						// do this
  }
  if (curr_lock)
  { SemOp(SEM_GLOBAL, -curr_lock);			// release lock
  }
  return 0;						// and exit
}


//-----------------------------------------------------------------------------
// Function: DB_ViewRel
// Descript: Release specified gbd
// Input(s): Vol# and gbd ptr of block
// Return:   0 -> Ok, negative MUMPS error
//

short DB_ViewRel(int volume, struct GBD *ptr)	      	// release block, gbd
{ short s;						// for functions

  ASSERT(0 < volume);                                   // valid volume
  ASSERT(volume <= MAX_VOL);
  ASSERT(NULL != systab->vol[volume-1]->vollab);        // mounted

#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"--- DB_ViewRel: %d\r\n",ptr->block);fflush(stderr);
#endif
  writing = 0;						// clear this
  ptr->last_accessed = MTIME(0);			// reset access
  if (ptr->dirty && (ptr->dirty < (gbd *) 5))		// not owned elsewhere
  { s = SemOp(SEM_GLOBAL, WRITE);			// write lock
    if (s < 0)						// check error
    { return s;						// quit if so
    }
#ifdef MV1_CACHE_DEBUG
    fprintf(stderr,"--- DB_ViewRel: !!! free !!!\r\n");fflush(stderr);
#endif
    Free_GBD(volume-1, ptr);				// free it
    SemOp(SEM_GLOBAL, -curr_lock);			// release lock
  }
  return 0;						// and exit
}
