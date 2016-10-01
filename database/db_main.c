// File: mumps/database/db_main.c
//
// module database - Main Database Functions

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
#include <fcntl.h>					// for file stuff
#include <time.h>					// for gbd stuff
#include <ctype.h>					// for gbd stuff
#include <errno.h>					// errno
#include <sys/types.h>                                  // for semaphores
#include <sys/ipc.h>				        // for semaphores
#include <sys/sem.h>				        // for semaphores
#include "mumps.h"					// standard includes
#include "database.h"				        // database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings
#include <assert.h>


int curr_lock;						// lock on globals
mvar db_var;						// local copy of var
int volnum;                                             // current volume

gbd *blk[MAXTREEDEPTH];				        // current tree
int level;                                              // level in above
                                                        // 0 = global dir
u_int rekey_blk[MAXREKEY];			        // to be re-keyed
int   rekey_lvl[MAXREKEY];			        // from level

int Index;                                              // Index # into above
cstring *chunk;						// chunk at Index
cstring *record;					// record at Index
                                                        // points at dbc
u_char keybuf[260];					// for storing keys
u_short *idx;						// for Indexes
int *iidx;                                              // int ver of Index

int writing;						// set when writing
int wanna_writing;                                      // 
#ifdef MV1_RSVD
extern int nrsvd_gbds;
#endif

int hash_start = 0;					// start searching here

//-----------------------------------------------------------------------------
// Function: Copy2local
// Descript: Copy passed in mvar to db_var, adjusting volset and uci
//	     The local copy of the mvar, db_var, is then used by all
//	     other database code.  Only DB_QueryD uses the original.
//	     DB_Compress also updates the original so that it can be watched.
// Input(s): Pointer to mvar to copy from
// Return:   0 -> Ok, negative MUMPS error
//
// Note:     No locks are held at this stage.
//

void BAD_MVAR()
{
  int flag = 1;
  return;
}

short Copy2local(mvar *var, char *rtn)
{ int i;						// a handy int

  // fprintf(stderr,"=== %s\r\n", rtn);
  partab.jobtab->grefs++;				// count global ref
  for (i = 0; i < MAXTREEDEPTH; blk[i++] = NULL);	// clear blk[]
  if (curr_lock)
  { panic("Copy2local: curr_lock != 0");
  }
  curr_lock = 0;					// ensure this is clear
  writing = 0;						// assume reading
  wanna_writing = 0;
#ifdef MV1_RSVD
  nrsvd_gbds = 0;
#endif
  level = -1;						// no claimed gbds yet
  bcopy(var, &db_var, MVAR_SIZE+var->slen);	        // copy the data
  if (db_var.volset == 0)				// if volset is zero
  { db_var.volset = partab.jobtab->vol;			// get current volset
  }
  if (db_var.volset > MAX_VOL)				// within limits?
  { return (-ERRM26);					// no - error
  }
  if (systab->vol[db_var.volset-1] == NULL)		// is it mounted?
  { return (-ERRM26);					// no - error
  }
  if (db_var.uci == 0)					// uci specified?
  { if (db_var.name.var_cu[0] == '%')
    { db_var.uci = 1;					// MGR
    }
    else
    { db_var.uci = partab.jobtab->uci;			// or current
    }
  }
  if (db_var.uci > UCIS)
  { return (-ERRM26);					// too big
  }

  if ((var->volset == 0) && (var->uci == 0))		// no vol or uci
  { for (i = 0; i < systab->max_tt; i++)		// scan trantab
    if (bcmp(&db_var, &systab->tt[i], sizeof(var_u) + 2) == 0) // if a match
    { if (systab->tt[i].to_vol == 0)
      { return (i+1);					// flag routine proc
      }
      bcopy(&systab->tt[i].to_global, &db_var.name, sizeof(var_u) + 2);
      break;
    }							// end found one
  }							// end trantab lookup

  if (systab->vol[db_var.volset-1]->vollab->
      uci[db_var.uci-1].name.var_cu[0] == '\0')		// does uci exits?
  { return (-ERRM26);					// no - error
  }
  if ((db_var.name.var_cu[0] == '%') &&			// if a %global
      (db_var.uci != 1))				// and uci is not 1
  { return (-ERRM26);					// error
  }
  volnum = db_var.volset;				// save this for ron
  if (db_var.nsubs != 255)
  { int actsubs;
    UTIL_Key_Chars_In_Subs((char *)db_var.key, (int)db_var.slen,
                           255, &actsubs, NULL);
    if (db_var.nsubs != actsubs)
      BAD_MVAR();
    assert(db_var.nsubs == actsubs);
  }
  return 0;						// else return ok
}

//-----------------------------------------------------------------------------
// Function: DB_Get
// Descript: Locate and return data described in passed in mvar
// Input(s): Pointer to mvar to get
//	     Pointer to buffer for data
// Return:   String length -> Ok, negative MUMPS error
//

short DB_Get(mvar *var, u_char *buf)	                // get global data
{ return DB_GetEx(var, buf, 0);
}

short DB_GetEx(mvar *var, u_char *buf, int wrlock)	// get global data
{ short s;						// for returns

  s = Copy2local(var,"DB_Get");				// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  if (s > 0)						// ROU process
  { s++;						// point at trantab ent
    // This code needs to invoke XXX^ systab->tt[s].to_global.var_cu
    // as a routine where XXX is GET (this example), SET, KILL etc
    // with mvar *var converted to cstring as arg1 and buf as
    // argument 2 passed by reference.
    //
    // This code must then be copied to all 10 other calls to Copy2local
    //
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbget); // update stats

  if (wrlock)
  { Ensure_GBDs();
    writing = 1;
  }

  s = Get_data(0);					// attempt to get it
  if (s >= 0)						// if worked
  { if (bcmp("$GLOBAL\0", &db_var.name.var_cu[0], 8) == 0) // if ^$G
    { s = itocstring(buf, *(u_int *) record);		// block number
    }
    else
    { s = mcopy(record->buf, buf, record->len);		// copy the data
    }
  }
  if (curr_lock)		                        // if locked
  { if ((0 == wrlock) ||
        (wrlock && (s < 0) && (s != -(ERRM7))))
    SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  return s;						// return the count
}

//-----------------------------------------------------------------------------
// Function: DB_Set
// Descript: Set data passed to location described in mvar passed
// Input(s): Pointer to mvar to set
//	     Pointer to cstring containing data
// Return:   String length -> Ok, negative MUMPS error
//

short DB_Set(mvar *var, cstring *data)    	        // set global data
{ return DB_SetEx(var, data, 0);
}

short DB_SetEx(mvar *var, cstring *data, int wrlock)    // set global data
{ short s;						// for returns
  int i;						// a handy int
  int curr_lock_sav;

  curr_lock_sav = 0;                                    // $INCREMENT() locks
  if (wrlock)                                           //   in DB_Get
  { curr_lock_sav = curr_lock;                          //   save curr_lock
    curr_lock = 0;                                      //   to make Copy2local
  }                                                     //   happy
  s = Copy2local(var,"DB_Set");				// get local copy
  curr_lock = curr_lock_sav;                            //   restore curr_lock
  if (s < 0)
  { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
    return s;						// exit on error
  }
  i = 4 + db_var.slen + 2 + data->len;			// space reqd
  if (i & 3)						// if required
  { i += (4 - (i & 3));					// round up
  }
  i += 4;						// add Index
  if (i > (systab->vol[volnum-1]->vollab->block_size - BLK_HDR_SIZE)) // if too big
  { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
    return -ERRM75;					// return an error
  }
  writing = 1;						// say we are writing
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbset); // update stats
  while (systab->vol[volnum - 1]->writelock)		// check for write lock
  { i = Sleep(5);					// wait a bit
    if (partab.jobtab->attention)
    { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
      return -(ERRZLAST+ERRZ51);			// for <Control><C>
    }
  }							// end writelock check

  i = systab->vol[volnum-1]->vollab->max_block >> 3;	// last map byte
  while (i)						// check from the end
  { if ((((u_char *) systab->vol[volnum - 1]->map)[i--]) == 0)
    { break;						// OK if byte is free
    }
  }

  if (!i)
  { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
    return -(ERRZLAST+ERRZ11);				// complain if failed
  }

  s = Set_data(data);					// do the set

  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  return s;						// return the result
}

//-----------------------------------------------------------------------------
// Function: DB_Data
// Descript: Return $DATA() for the passed in mvar
// Input(s): Pointer to mvar to check
//	     Pointer to buffer for return result (0, 1, 10 or 11)
//           Pointer to buffer for return value of mvar if defined
// Return:   String length -> Ok, negative MUMPS error
//

short DB_Data(mvar *var, u_char *buf)	          	// get $DATA()
{ return DB_DataEx(var, buf, 0);
}

short DB_DataEx(mvar *var, u_char *buf, cstring *dat)   // get $DATA()
{ short s;						// for returns
  int i;						// a handy int

  s = Copy2local(var,"DB_Data");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbdat); // update stats
  s = Get_data(0);					// attempt to get it
  i = 1;						// assume data found
  if (s == -ERRM7)					// undefined global?
  { i = 0;						// yes - no data
    if (level == 0)					// check for global
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      buf[0] = '0';					// zero to return
      buf[1] = '\0';					// null terminated
      return 1;						// and exit
    }
  }
  else if (s < 0)					// if it failed
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return s;						// and exit
  }
  if ((i) && (dat))                                     // save value if dat
  { bcopy(record->buf, dat->buf, record->len);          //   present
    dat->len = record->len;
  }
  if ((!db_var.slen) && (!i))				// pointing at 1st
  { Index++;
  }
  if ((i) || (Index > blk[level]->mem->last_idx))	// found or passed end
  { s = Locate_next(keybuf);				// get next record
    if (s == -ERRM7)					// any more?
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return itocstring(buf, i);			// return result
    }
    else if (s < 0)					// error?
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return s;						// and exit
    }
  }							// got next record
  if (((db_var.slen < keybuf[0]) &&			// if smaller key and
       (bcmp(&keybuf[1], db_var.key, db_var.slen) == 0)) || // a descendant?
       (!db_var.slen))
  { i += 10;						// add 10 to result
  }
  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  return itocstring(buf, i);				// return result
}

//-----------------------------------------------------------------------------
// Function: DB_Kill
// Descript: Remove the sub-tree described by the passed in mvar
// Input(s): Pointer to mvar to remove
// Return:   0 -> Ok, negative MUMPS error
//

short DB_Kill(mvar *var)	                       	// remove sub-tree
{ return DB_KillEx(var, KILL_ALL);
}

//-----------------------------------------------------------------------------
// Function: DB_KillEx
// Descript: Remove the sub-tree/value described by the passed in mvar
// Input(s): Pointer to mvar to remove, what - what to remove
// Return:   0 -> Ok, negative MUMPS error
//

short DB_KillEx(mvar *var, int what)                   	// remove sub-tree
{ short s;						// for returns

  s = Copy2local(var,"DB_Kill");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  while (systab->vol[volnum - 1]->writelock)		// check for write lock
  { (void)Sleep(5);					// wait a bit
    if (partab.jobtab->attention)
    { return -(ERRZLAST+ERRZ51);			// for <Control><C>
    }
  }							// end writelock check
  wanna_writing = 1;
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbkil); // update stats
  s = Get_data(0);					// attempt to get it
  if (((s == -ERRM7) && (level == 0)) ||		// if nosuch
      ((s < 0) && (s != -ERRM7)))			// or an error
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    if (s == -ERRM7)					// if undefined
    { s = 0;						// that is OK
    }
    return s;						// nothing to do
  }

  if ((s == -ERRM7) && (db_var.slen))			// if undefined
  { if (Index <= blk[level]->mem->last_idx)		// and still in block

    { if ((db_var.slen > keybuf[0]) ||			// found smaller key
          (bcmp(&keybuf[1], db_var.key, db_var.slen)))	// not a descendant?
      { if (curr_lock)					// if locked
        { SemOp( SEM_GLOBAL, -curr_lock);		// release global lock
        }
        return 0;					// nothing to do
      }
    }							// end still in block
    else
    { s = Locate_next(keybuf);				// point at next block
      if (!s)						// found one
      { if ((db_var.slen > keybuf[0]) ||		// found smaller key
            (bcmp(&keybuf[1], db_var.key, db_var.slen))) // not a descendant?
        { s = -ERRM7;					// flag for later
        }
      }
      if (s < 0)					// no such or error
      { if (curr_lock)					// if locked
        { SemOp( SEM_GLOBAL, -curr_lock);		// release global lock
        }
	if (s == -ERRM7)
	{ s = 0;
	}
        return 0;					// nothing to do
      }
    }
  }
  s = Kill_data_ex(what);				// do the kill

  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  return s;						// return the result
}

//-----------------------------------------------------------------------------
// Function: DB_Order
// Descript: Return the next/prev subscript at the supplied level
// Input(s): Pointer to mvar to search from
//	     Pointer to buffer to hold result
//           Pointer to dat to hold value if present
//	     Direction, 1 = fwd, -1 = bck
// Return:   String length -> Ok, negative MUMPS error
//

short DB_Order(mvar *var, u_char *buf, int dir) 	// get next subscript
{ return DB_OrderEx(var, buf, dir, 0);
}

short DB_OrderEx(mvar *var, u_char *buf, int dir,       // get next subscript
                 cstring *dat)                          //   and value
{ short s;						// for returns
  int i;						// a handy int
  int last_key;						// start of last key

  s = Copy2local(var,"DB_Order");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbord); // update stats
  last_key = UTIL_Key_Last(&db_var);			// get start of last
  buf[0] = '\0';					// null terminate ret
  if (dir < 0)						// if it's backward
  { s = Get_data(-1);					// get the previous
    if ((s < 0) && (s != -ERRM7))			// check for errors
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return s;						// and return the error
    }
    if ((level == 0) && (s == -ERRM7) &&		// if no such global
	(bcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8)))	// and not ^$G()
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return 0;						// and return
    }
    Index--;                                          	// backup the Index
    if (Index < LOW_INDEX)                            	// can't happen?
    { panic("DB_Order: Problem with negative direction");
    }
    chunk = (cstring *) &iidx[idx[Index]];             	// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+4];	// point at the dbc
  }							// end backwards
  else							// it's forward
  { db_var.key[db_var.slen++] = 255;			// force next key
    s = Get_data(0);					// try to find that
    if (s != -ERRM7)					// MUST be undefined
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return (s < 0) ? s : -(ERRMLAST+ERRZ61);		// and return the error
    }
    if ((level == 0) &&					// if no such global
	(bcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8)))	// and not ^$G()
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return 0;						// and return
    }
    if (Index > blk[level]->mem->last_idx)		// no more avbl
    { s = Locate_next(0);				// get next (if there)
      if (s < 0)					// failed?
      { if (curr_lock)					// if locked
        { SemOp( SEM_GLOBAL, -curr_lock);		// release global lock
        }
        return (s == -ERRM7) ? 0 : s;			// done
      }
    }
  }							// end forwards
#ifdef MV1_CCC
  for (i = LOW_INDEX; i <= Index; i++)			// scan to current
  { chunk = (cstring *) &iidx[idx[i]];             	// point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  }
#else
  chunk = (cstring *) &iidx[idx[Index]];             	// point at the chunk
  bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
  keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
#endif

  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  if ((keybuf[0] < (last_key + 1)) ||
      (bcmp(&keybuf[1], db_var.key, last_key)))		// check for past it
  { return 0;						// done
  }
  i = 0;						// clear flag
  s = UTIL_Key_Extract(&keybuf[last_key+1], buf, &i);	// extract the key
  // fprintf(stderr, "DB_Order(): i=%d last_key=%d len=%d\r\n",
  //                  i, last_key, keybuf[0]);
  fflush(stderr);
  if (dat &&                                            // dat given
      (s >= 0) &&                                       //   extract was succ.
      (keybuf[0] == last_key + i))                      // extracted the last
  { record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    bcopy(&record->buf[0], &dat->buf[0], record->len);  //   copy it
    dat->len = record->len;
  }
  return s;						// return result
}

//-----------------------------------------------------------------------------
// Function: DB_Query
// Descript: Return the next/prev full key to the supplied one
// Input(s): Pointer to mvar to search from
//	     Pointer to buffer to hold result
//	     Direction, 1 = fwd, -1 = bck
//           Convert to string, 1 - yes
// Return:   String length -> Ok, negative MUMPS error
//

short DB_Query(mvar *var, u_char *buf, int dir, int docvt) // get next key
{ return DB_QueryEx(var, buf, dir, docvt, 0);
}

short DB_QueryEx(mvar *var, u_char *buf, int dir,       // get next key
                        int docvt, cstring *dat)
{ short s;						// for returns
  int i;						// a handy int

  s = Copy2local(var,"DB_Query");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbqry); // update stats
  if (dir < 0)						// if it's backward
  { if (!db_var.slen)                                   // if not subscripted
    { buf[0] = '\0';                                    // null terminate ret
      if (curr_lock)                                    // if locked
      { SemOp( SEM_GLOBAL, -curr_lock);                 // release global lock
      }
      return 0;                                         // and return
    }
    s = Get_data(-1);					// get the previous
    // fprintf(stderr, "DB_Query: Get_data(-1)=%d\r\n", s);
    // fflush(stderr);
    if ((s < 0) && (s != -ERRM7))			// check for errors
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return s;						// and return the error
    }
    if ((level == 0) && (s == -ERRM7))			// if no such global
    { buf[0] = '\0';					// null terminate ret
      if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return 0;						// and return
    }
    Index--;                                          	// backup the Index
#if 0
    if ((s == -ERRM7) && (Index < LOW_INDEX))           // glvn not subscripted
    { buf[0] = '\0';					// null terminate ret
      if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);		        // release global lock
      }
      return 0;					        // and return
    }
#endif
    if (Index < LOW_INDEX)                             	// can't happen?
    { panic("DB_Query: Problem with negative direction");
    }
    chunk = (cstring *) &iidx[idx[Index]];             	// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+4];	// point at the dbc
    if ((!chunk->buf[0]) && (!chunk->buf[1]) &&		// if first node
        ((partab.jobtab->last_block_flags & GL_TOP_DEFINED) == 0))
    { buf[0] = '\0';					// null terminate ret
      if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return 0;						// and return
    }
    // fprintf(stderr, "DB_Query: Index=%d\r\n", Index);
    // fflush(stderr);
  }							// end backwards
  else							// it's forward
  { s = Get_data(0);					// try to find that
    if ((s < 0) && (s != -ERRM7))			// check for errors
    { if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return s;						// and return the error
    }
    if ((level == 0) && (s == -ERRM7))			// if no such global
    { buf[0] = '\0';					// null terminate ret
      if (curr_lock)					// if locked
      { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      }
      return 0;						// and return
    }
    if ((s < 0) && (!db_var.slen))
    { Index++;
    }
    if ((Index > blk[level]->mem->last_idx) || (s >= 0)) // want next one
    { s = Locate_next(0);				// point at next
      if (s < 0)					// not found or error
      { if (curr_lock)					// if locked
        { SemOp( SEM_GLOBAL, -curr_lock);		// release global lock
        }
        buf[0] = '\0';					// null terminate ret
	if (s == -ERRM7)				// undefined?
	{ s = 0;					// yes - clear it
	}
        return s;					// done
      }
    }
  }

#ifdef MV1_CCC
  for (i = LOW_INDEX; i <= Index; i++)			// scan to current
  { chunk = (cstring *) &iidx[idx[i]];             	// point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  }
#else
  chunk = (cstring *) &iidx[idx[Index]];             	// point at the chunk
  bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
  keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
#endif
  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  if ((s == -ERRM7) &&                                  // not found
      (keybuf[0] == 0) &&                               // key has no subscript
       db_var.slen &&                                   // original subscript is
      (db_var.key[0] == 0 || db_var.key[0] == 255))     //   empty
  { buf[0] = '\0';
    return 0;
  }
  if (dat)                                              // dat given
  { record = (cstring *) &chunk->buf[chunk->buf[1]+2];  //   point to data
    bcopy(&record->buf[0], &dat->buf[0], record->len);  //     copy it
    dat->len = record->len;
  }
  db_var.uci = var->uci;				// copy
  db_var.volset = var->volset;				//   original & new
  //db_var.name.var_qu = var->name.var_qu;		//      data
  db_var.name.var_xu = var->name.var_xu;		//      data
  db_var.slen = keybuf[0];				//         to
  bcopy(&keybuf[1], &db_var.key[0], keybuf[0]);		//           db_var
  if (!docvt)                                           // if no conversion
    return db_var.slen;                                 //   return slen
  return UTIL_String_Mvar(&db_var, buf, 9999);		// convert and return
}

//-----------------------------------------------------------------------------
// Function: DB_QueryD
// Descript: Return the next full key to the supplied one
// Input(s): Pointer to mvar to search from
//	     Pointer to buffer to hold result
// Return:   Length of returned string or negative error number
//	     Updated MVAR if not error
//	     Data from updated mvar (if no error)
//

short DB_QueryD(mvar *var, u_char *buf) 		// get next key
{ short s;						// for returns
//  int i;						// a handy int

  s = Copy2local(var,"DB_QueryD");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  s = Get_data(0);					// try to find that
  if ((s < 0) && (s != -ERRM7))				// check for errors
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return s;						// and return the error
  }
  if ((level == 0) && (s == -ERRM7))			// if no such global
  { buf[0] = '\0';					// null terminate ret
    if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return -(ERRMLAST+ERRZ55);				// and return
  }

  if ((s < 0) && (db_var.slen))				// If we had a "real"
  { Index--;						// <UNDEF> last time
  }							// back up Index

#ifdef MV1_CCC
  s = Locate_next(keybuf);				// point at next
#else
  s = Locate_next(0);				        // point at next
#endif
  if (s < 0)						// not found or error
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    if (s == -ERRM7)					// if no more
    { s = -(ERRMLAST+ERRZ55);				// say 'at end'
    }
    return s;						// done
  }

//  for (i = LOW_INDEX; i <= Index; i++)		// scan to current
//  { chunk = (cstring *) &iidx[idx[i]];             	// point at the chunk
//    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
//	  chunk->buf[1]);				// update the key
//    keybuf[0] = chunk->buf[0] + chunk->buf[1];	// and the size
//  }
#ifdef MV1_CCC
  bcopy(&keybuf[1], var->key, (int) keybuf[0]);		// copy in the key
#else
  chunk = (cstring *) &iidx[idx[Index]];             	// point at the chunk
  bcopy(&chunk->buf[2], var->key, chunk->buf[1]);	// update the key
  keybuf[0] = chunk->buf[0] + chunk->buf[1];
#endif
  var->slen = keybuf[0];				// update the length
  s = mcopy(record->buf, buf, record->len);		// copy the data
  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  return s;						// return the count
}

//-----------------------------------------------------------------------------
// Function: DB_GetLen
// Descript: Locate and return length of data described in passed in mvar
//	     If buf is not NULL, return the data there.
//	     The global module is always unlocked on an error.
// Input(s): Pointer to mvar to get length of
//	     State to leave SEM_GLOBAL lock (1 -> leave locked, -1 -> unlock)
//	     A state of -1, JUST does an unlock and returns 0.
//	     Buffer for routine (if not NULL)
// Return:   String length -> Ok, negative MUMPS error
// Note:     There may be NO intervening calls to other DB modules
//	     when the GDB has been left locked.
//

short DB_GetLen( mvar *var, int lock, u_char *buf)	// length of node
{ short s;						// for returns
  int sav;						// save curr_lock

  if ((lock == -1) && (buf == NULL))			// just unlock?
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// unlock it
    }
    return 0;						// exit
  }
  sav = curr_lock;					// save this
  curr_lock = 0;
  s = Copy2local(var,"DB_GetLen");			// get local copy
  curr_lock = sav;					// restore current lock
  if (s < 0)						// check for error
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return s;						// and return
  }
  s = Get_data(0);					// attempt to get it

  if (s < 0)						// check for error
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return s;						// and return
  }
  if (buf != NULL)					// want data?
  { s = mcopy(record->buf, buf, record->len);		// copy the data
  }
  if ((lock != 1) && (curr_lock))			// preserve lock?
  { SemOp( SEM_GLOBAL, -curr_lock);			// no - release it
  }
  return s;						// and exit
}

//-----------------------------------------------------------------------------
// Function: DB_Free
// Descript: Return number of free blocks in volume set
// Input(s): Volume set number to examine
// Return:   Number of free blocks
//

int DB_Free(int vol)	                           	// total free blocks
{ short s;						// for funcs
  u_int i;						// loop cnt
  int count = 0;					// blk count

  s = SemOp( SEM_GLOBAL, READ);				// lock the globals
  if (s < 0)
  { return s;						// return any errors
  }
  for (i = 1;						// start at block 1
	i <= systab->vol[vol-1]->vollab->max_block;  	// while still in map
        i++)						// going up by one
  { count +=		 				// add up blocks
	(((((u_char *)systab->vol[vol-1]->map)[i>>3]) &(1<<(i&7))) == 0);
  }
  SemOp( SEM_GLOBAL, -curr_lock);			// unlock the globals
  return count;						// return the count
}							// end DB_Free

//-----------------------------------------------------------------------------
// Function: DB_Expand
// Descript: Expand volume set
// Input(s): Internal volume set number to dismount
//	     New size in blocks (checks have been done)
// Return:   0 or error
//

short DB_Expand(int vol, u_int vsiz)			// expand it
{ off_t fptr;						// for lseek
  off_t fres;						// ditto
  u_int vexp;						// expand by
  int i;						// a handy int
  u_char *p;						// for malloc
  int dbfd;						// for open

  p = dlmalloc(systab->vol[vol]->vollab->block_size);	// get some space
  if (p == NULL)
  { return -(ERRMLAST+ERRZLAST+errno);			// die
  }
  bzero(p, systab->vol[vol]->vollab->block_size);	// clear it
  dbfd = open(systab->vol[0]->file_name, O_RDWR);	// open database r/wr
  if (dbfd < 0)						// if failed
  { dlfree(p);						// free memory
    return -(ERRMLAST+ERRZLAST+errno);			// and die
  }

  fptr = (off_t)systab->vol[vol]->vollab->max_block;	// start here
  fptr = (fptr * (off_t) systab->vol[vol]->vollab->block_size)
	 + (off_t) systab->vol[vol]->vollab->header_bytes;

  fres = lseek( dbfd, fptr, SEEK_SET);			// Seek to eof
  if (fres != fptr)					// if failed
  { dlfree(p);						// free memory
    return -(ERRMLAST+ERRZLAST+errno);			// and die
  }
  vexp = vsiz - systab->vol[vol]->vollab->max_block;	// expand by
  while (vexp)
  { i = write(dbfd, p, systab->vol[vol]->vollab->block_size);
    if (i < 0)						// if failed
    { dlfree(p);					// free memory
      return -(ERRMLAST+ERRZLAST+errno);		// and die
    }
    vexp--;						// count 1
  }
  dlfree(p);						// free memory
  i = close(dbfd);					// close db file
  systab->vol[vol]->vollab->max_block = vsiz;		// store new size
  systab->vol[vol]->map_dirty_flag = 1;			// say write this
  return 0;
}

//-----------------------------------------------------------------------------
// Function: DB_Dismount
// Descript: Dismount volume set
// Input(s): Volume set number to dismount
// Return:   0
//

int DB_Dismount(int vol)	                       	// dismount a volume
{ if (vol == 1)
  { DB_StopJournal(vol, JRN_ESTOP);
  }
  systab->vol[vol-1]->dismount_flag = 1;		// set the flag
  return 0;						// that's all for now
}

//-----------------------------------------------------------------------------
// Function: DB_StopJournal
// Descript: Stop journaling on a volume
// Input(s): Volume set number to stop
//	     Reason (currently JRN_STOP and JRN_ESTOP)
// Return:   none
//

void DB_StopJournal(int vol, u_char action)		// Stop journal
{ jrnrec jj;

  volnum = vol;						// set common var
  if (!systab->vol[vol - 1]->vollab->journal_available) // if no journal
  { return;						// just exit
  }
  while (SemOp( SEM_GLOBAL, WRITE))
  { Sleep(1);
  }
  jj.action = action;
  jj.uci = 0;
  //jj.name.var_qu = 0;
  X_Clear(jj.name.var_xu);
  jj.slen = 0;
  DoJournal(&jj, NULL);
  systab->vol[vol - 1]->vollab->journal_available = 0;
  return;
}

//-----------------------------------------------------------------------------
// Function: DB_GetFlags
// Descript: Get global flags
// Input(s): Pointer to mvar -> ^$GLOBAL("name")
// Return:   flags or negative MUMPS error
//

int DB_GetFlags(mvar *var)	                       	// Get flags
{ short s;						// for returns
  int i;						// a handy int

  s = Copy2local(var,"DB_GetFlags");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  s = Get_data(0);					// try to find that
  if ((s < 0) && (s != -ERRM7))				// check for errors
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return s;						// and return the error
  }
  i = ((int *) record)[1];				// get the value
  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  return i;						// return the flags
}

//-----------------------------------------------------------------------------
// Function: DB_SetFlags
// Descript: Set global flags
// Input(s): Pointer to mvar -> ^$GLOBAL("name")
//	     Positive flags to set or negative flags to clear
// Return:   new flags or negative MUMPS error
//

int DB_SetFlags(mvar *var, int flags)                  	// Set flags
{ int clearit = 0;
  int i;
  short s;

  if (flags < 0)
  { clearit = 1;					// setup to clear
    flags = -flags;					// get flags correct
  }
  s = Copy2local(var,"DB_SetFlags");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  writing = 1;						// say we are writing
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbset); // update stats
  while (systab->vol[volnum - 1]->writelock)		// check for write lock
  { i = Sleep(5);					// wait a bit
    if (partab.jobtab->attention)
    { return -(ERRZLAST+ERRZ51);			// for <Control><C>
    }
  }							// end writelock check
  Get_GBDs(1);						// ensure this many
  s = Get_data(0);                                      // try to find that
  if ((s < 0) && (s != -ERRM7))                         // check for errors
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return (int) s;					// return error
  }
  i = ((int *)record)[1];				// get current flags
  if (clearit)
  { i = i & ~flags;					// clear flags
  }
  else
  { i = i | flags;					// set flags
  }
  ((int *)record)[1] = i;				// set back to GD
#ifdef XMV1_BLKVER
  blk[level]->blkver_low++;
#endif
  if (blk[level]->dirty == (gbd *) 1)			// if reserved
  { blk[level]->dirty = blk[level];			// terminate list
    Queit();						// que for write
  }
  SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  return i;						// return current flags
}

//-----------------------------------------------------------------------------
// Function: DB_Compress
// Descript: Compress a global on-line
// Input(s): Where to start in global (mvar) Must ---> partab.jobtab->last_ref
//	     Level to process 0 -> 15 (data level or more means data level)
// Return:   actual level number processed or error number
//

short DB_Compress(mvar *var, int flags)			// Compress global
{ int i;
  short s;
  int retlevel;						// the ACTUAL level

  flags &= 15;						// clear high bits
  s = Copy2local(var,"DB_Compress");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }

  bzero(rekey_blk, MAXREKEY * sizeof(u_int));           // clear that table
  bzero(rekey_lvl, MAXREKEY * sizeof(int));             // and that table

  bcopy(&db_var, var, sizeof(mvar));			// copy the data back
  wanna_writing = 1;
  s = Get_data(flags);					// get to level 'flags'
  retlevel = level;					// save real level
  if (!level)
  { return -ERRM7;					// give up if nosuch
  }
  chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at the first
  bcopy(&chunk->buf[1], &var->slen, chunk->buf[1]+1);	// save the real key

  while (TRUE)
  { bcopy(var, &db_var, sizeof(mvar));			// get next key
    writing = 0;					// flag we are reading

    while (systab->vol[volnum - 1]->writelock)		// check for write lock
    { i = Sleep(5);					// wait a bit
      if (partab.jobtab->attention)
      { return -(ERRZLAST+ERRZ51);			// for <Control><C>
      }
    }							// end writelock check
    if (partab.jobtab->attention)
    { return -(ERRZLAST+ERRZ51);			// for <Control><C>
    }

    s = Get_data(retlevel);				// get the block
    if ((s == -ERRM7) && (!db_var.slen))		// if first node
    { s = 0;						// it exists
    }
    if (s == -ERRM7)					// if key changed
    { if (blk[level]->mem->right_ptr)			// if more
      { chunk = (cstring *) &iidx[idx[LOW_INDEX]];	// point at the first
	bcopy(&chunk->buf[1], &db_var.slen, chunk->buf[1]+1); // save real key
	SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
	continue;					// go again
      }
      SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      return retlevel;					// all done, exit
    }
    if (s < 0)
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      return s;						// exit on error
    }
    if (!blk[level]->mem->right_ptr)			// if no more
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      if ((retlevel == 2) && (!db_var.slen))		// if only block lvl 2
      { s = Compress1();				// do that
	SemOp( SEM_GLOBAL, -curr_lock);			// release write lock
	if (s < 0)
	{ return s;					// exit on error
	}
      }
      return retlevel;					// all done, exit
    }
    level++;
    s = Get_block(blk[level - 1]->mem->right_ptr);
    if (s < 0)						// if error
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      return s;						// exit on error
    }
    i = ((blk[level-1]->mem->last_free*2 + 1 - blk[level-1]->mem->last_idx)*2)
      + ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2);
    if (i < 1024)	// if REALLY not enough space (btw: make this a param)
    { chunk = (cstring *) &iidx[idx[LOW_INDEX]];	// point at first in RL
      bcopy(&chunk->buf[1], &var->slen, chunk->buf[1]+1); // save the real key
      SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
      continue;						// go again
    }
    level = retlevel;
    SemOp( SEM_GLOBAL, -curr_lock);			// release read lock
    s = Compress1();					// do that
    SemOp( SEM_GLOBAL, -curr_lock);			// release write lock
    if (s < 0)
    { return s;						// exit on error
    }
    if (!var->volset)					// if done
    { return retlevel;
    }
  }
}
