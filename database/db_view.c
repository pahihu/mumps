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

struct GBD *DB_ViewGet(int vol, int block)		// return gbd for blk
{ short s;						// for func
  if ((block < 1) || (block > systab->vol[vol-1]->vollab->max_block))
  { return NULL;					// validate
  }
  level = 0;						// where it goes
  volnum = vol;						// need this
  writing = 0;						// clear this
  s = SemOp(SEM_GLOBAL, READ);				// write lock
  if (s < 0)						// check error
  { return NULL;					// quit if so
  }
  s = Get_block(block);					// get it
  if (s >= 0)
  { blk[level]->last_accessed = time(0)+86400;		// push last access
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
// Return:   none
//

void DB_ViewPut(int vol, struct GBD *ptr)		// que block for write
{ short s;						// for funcs

  volnum = vol;						// for ron
  writing = 0;						// clear this
  s = SemOp(SEM_GLOBAL, WRITE);				// write lock
  if (s < 0)						// check error
  { return;						// quit if so
  }
  ptr->last_accessed = time(0);				// reset access
  if (ptr->mem->type)					// if used
  { Used_block(ptr->block);				// mark it so
  }
  else 							// trying to free it
  { Free_block(ptr->block);				// do so
    bzero(ptr->mem, systab->vol[(volnum)-1]->vollab->block_size); // clear it
  }
  level = 0;						// for Queit
  if (ptr->dirty == NULL)				// check dirty ptr
  { ptr->dirty = ptr;					// set if reqd
    blk[level] = ptr;					// ditto
    Queit();						// do this
  }
  if (curr_lock)
  { SemOp(SEM_GLOBAL, -curr_lock);			// release lock
  }
  return;						// and exit
}


//-----------------------------------------------------------------------------
// Function: DB_ViewRel
// Descript: Release specified gbd
// Input(s): Vol# and gbd ptr of block
// Return:   none
//

void DB_ViewRel(int vol, struct GBD *ptr)	      	// release block, gbd
{ short s;						// for functions

  writing = 0;						// clear this
  ptr->last_accessed = time(0);				// reset access
  if (ptr->dirty != NULL)				// not owned elsewhere
  { s = SemOp(SEM_GLOBAL, WRITE);			// write lock
    if (s < 0)						// check error
    { return;						// quit if so
// PROBABLY SHOULD PERSIST HERE
    }
    Free_GBD(ptr);					// free it
    SemOp(SEM_GLOBAL, -curr_lock);			// release lock
  }
  return;						// and exit
}
