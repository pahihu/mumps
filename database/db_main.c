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
#include "dgp_database.h"				// DGP database


int curr_locks[MAX_VOL + 1];				// GLOBAL locks
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
int gbd_local_state;					// local buffering

//-----------------------------------------------------------------------------
// Function: ROU_Process
// Descript: Call entry in systab->tt[tti].to_global.var_cu routine
//           with var passed as string, and buf/buflen passed as
//           reference.
// Input(s): Pointer to entry, trantab index, var, buf/buflen.
//              buf/buflen - buflen is -1, when output only
// Return:   0 -> Ok, negative MUMPS error
//
short ROU_Process(char *entry,
                  short tti,
                  mvar *var,
                  u_char *buf,
                  short buflen)
{ short s;
  char tmp[1024];

  s = UTIL_String_Mvar(var, (u_char *) &tmp[0], 9999);
  if (s < 0)
    return s;
  return -(ERRMLAST+ERRZ52);                            // not implemented yet
}

//-----------------------------------------------------------------------------
// Function: Copy2local
// Descript: Copy passed in mvar to db_var, adjusting volset and uci
//	     The local copy of the mvar, db_var, is then used by all
//	     other database code.  Only DB_QueryD uses the original.
//	     DB_Compress also updates the original so that it can be watched.
// Input(s): Pointer to mvar to copy from
// Return:   0 -> Ok, negative MUMPS error, 0< ROU proc
//
// Note:     No locks are held at this stage.
//

void BAD_MVAR()
{
  int flag = 1;
  return;
}

void UTIL_TTFillHash(void)                              // Fill up tthash[]
{ int i, j, x;                                          // handy ints
  var_u *var;

  while (SemOp( SEM_SYS, -systab->maxjob))              // lock it
    ;
  if (systab->tthash_empty)
  { for (i = 0; i < systab->max_tt; i++)		// scan trantab
    { if (!systab->tt[i].to_uci)                        // empty?
        continue;
      var = &systab->tt[i].from_global;                 // point to from_global
      j = FNV1aHash(sizeof(var_u) + 2,                  // calc. hash index
                          (u_char *) var) & (2 * MAX_TRANTAB - 1);
      for (x = 0; systab->tthash[j].tti && x < (2 * MAX_TRANTAB); x++)
      { if (bcmp(var, &systab->tthash[j].from_global,
                                sizeof(var_u) + 2) == 0)// if a match
        { systab->tthash[j].tti = i + 1;                // update entry 
          break;                                        // done
        }
        j = (j + 1) & (2 * MAX_TRANTAB - 1);            // next entry
      }
      if (0 == systab->tthash[j].tti)                   // slot empty ?
      { systab->tthash[j].tti = i + 1;                  // enter in trantab hash
        bcopy(var, &systab->tthash[j].from_global, sizeof(var_u) + 2);
      }
    }
    systab->tthash_empty = 0;                           // mark not empty
  }
  SemOp( SEM_SYS, systab->maxjob);                      // release lock
}


int UTIL_TTFind(var_u *src)                             // find src in tthash[]
{ int i, j, k;                                          // handy ints

  if (systab->tthash_empty)                             // hash empty?
    UTIL_TTFillHash();                                  //   fill it!

  j = FNV1aHash(sizeof(var_u) + 2,                      // calc. hash
                      (u_char *) src) & (2 * MAX_TRANTAB - 1);
  for (i = 0; systab->tthash[j].tti && i < (2 * MAX_TRANTAB); i++)
  { if (bcmp(src, &systab->tthash[j].from_global,       // if a match
                                sizeof(var_u) + 2) == 0)
    { k = systab->tthash[j].tti - 1;                    // check trantab[] index
      if (systab->tt[k].to_vol == 0)
      { return (k+1);					// flag routine proc
      }
      bcopy(&systab->tt[k].to_global, src, sizeof(var_u) + 2);
      return 0;
    }
    j = (j + 1) & (2 * MAX_TRANTAB - 1);                // next hash entry
  }
  return -1;
}


// NB. ha rtn eleje ":", akkor nem kell meghivni, mint rutint
short Copy2local(mvar *var, char *rtn)
{ int i;						// a handy int
  char msg[128];

#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"=== %s\r\n", rtn);
#endif
  partab.jobtab->grefs++;				// count global ref
  for (i = 0; i < MAXTREEDEPTH; blk[i++] = NULL);	// clear blk[]
  if (curr_lock)
  { sprintf(msg,"Copy2local: curr_lock != 0 [%s]", rtn);
    mv1_panic(msg);
  }
  curr_lock = 0;					// ensure this is clear
  writing = 0;						// assume reading
  wanna_writing = 0;
  level = -1;						// no claimed gbds yet
  bcopy(var, &db_var, MVAR_SIZE+var->slen);	        // copy the data
  if (db_var.volset == 0)				// if volset is zero
  { db_var.volset = partab.jobtab->vol;			// get current volset
  }
  ASSERT(0 < db_var.volset);
  if (db_var.volset > MAX_VOL)				// within limits?
  { return (-ERRM26);					// no - error
  }
  if (systab->vol[db_var.volset-1]->vollab == NULL)	// is it mounted?
  { return (-ERRM26);					// no - error
  }
  if (db_var.uci == 0)					// uci specified?
  { if (db_var.name.var_cu[0] == '%')
    { db_var.uci    = 1;					// MGR
      db_var.volset = 1;
    }
    else
    { db_var.uci = partab.jobtab->uci;			// or current
    }
  }
  if (db_var.uci > UCIS)
  { return (-ERRM26);					// too big
  }

  if ((var->volset == 0) &&                             // no vol or uci
      (var->uci == 0) &&		
      (systab->max_tt))                                 //   and has translation
  { i = UTIL_TTFind(&db_var.name);                      // trantab lookup
    if (i > 0)                                          // routine proc?
      return i;                                         //   done
  }							// end trantab lookup

  if (systab->vol[db_var.volset-1]->vollab->
      uci[db_var.uci-1].name.var_cu[0] == '\0')		// does uci exits?
  { return (-ERRM26);					// no - error
  }
  if ((db_var.name.var_cu[0] == '%') &&			// if a %global
      (db_var.uci != 1) && (db_var.volset != 1))	// and uci,vol is not 1
  { return (-ERRM26);					// error
  }
  volnum = db_var.volset;				// save this for ron
  ASSERT(0 < volnum);
  ASSERT(volnum <= MAX_VOL);

  if (systab->vol[volnum - 1]->flags & VOL_PROTECT)	// protected ?
  { return -(ERRZ92 + ERRMLAST);			//   exit on error
  }

#ifdef MV1_CHKSUBS
  if (db_var.nsubs != 255)
  { int actsubs;
    UTIL_Key_Chars_In_Subs((char *)db_var.key, (int)db_var.slen,
                           255, &actsubs, NULL);
    if (db_var.nsubs != actsubs)
      BAD_MVAR();
    ASSERT(db_var.nsubs == actsubs);
  }
#endif
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
{ return DB_GetEx(var, buf, 0, 0);
}

short DB_GetEx(mvar *var, 				// get global data
	       u_char *buf, int wrlock, 
	       int old_stat)
{ short s;						// for returns
  int i;                                                // handy int

  // NB. $ZINCR requests wrlock, Copy2local() is done in Dzincrement2()
  s = wrlock ? old_stat : Copy2local(var,"GET");
  if (s < 0)
  { return s;						// exit on error
  }
  if (s > 0)						// ROU process
  { s--;						// point at trantab ent
    // This code needs to invoke XXX^ systab->tt[s].to_global.var_cu
    // as a routine where XXX is GET (this example), SET, KILL etc
    // with mvar *var converted to cstring as arg1 and buf as
    // argument 2 passed by reference.
    //
    // This code must then be copied to all 10 other calls to Copy2local
    //
    return ROU_Process("GET", s, var, buf, -1);
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbget); // update stats
  if (systab->vol[volnum - 1]->local_name[0])		// remote VOL ?
  { return DGP_Get(volnum - 1, &db_var, buf);
  }

  if (wrlock)
  { writing = 1;
    while (systab->vol[volnum - 1]->writelock ||	// check for write lock
           systab->delaywt)                             //   or delay WRITEs
    { i = Sleep(5);					// wait a bit
      if (partab.jobtab->attention)
      { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
        return -(ERRMLAST+ERRZ51);			// for <Control><C>
      }
    }							// end writelock check
    Ensure_GBDs(0);
  }

  s = Get_data(0);			                // attempt to get it
  if (s >= 0)						// if worked
  { if (bcmp("$GLOBAL\0", &db_var.name.var_cu[0], 8) == 0) // if ^$G
    { s = itocstring(buf, *(u_int *) record);		// block number
    }
    else
    {                                                   // copy the data
      s = buf ? mcopy(record->buf, buf, record->len) : record->len;
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
// Function: DB_SetLong
// Descript: Set long data (>32K) in mvar
// Input(s): Pointer to mvar to set
//           Length of data
//	     Pointer to buffer for data
// Return:   String length -> Ok, negative MUMPS error
//
// Data is stored as
//    ^G=N (number of chunks)
//    ^G(0)=length of data
//    ^G(1)=data chunk 1
//    ...
//    ^G(N)=data chunk N

int DB_SetLong(mvar *var, int len, u_char *data)
{ u_char var_nsubs, var_slen;                           // var #subs, subs len
  int chunk_size;                                       // chunk size
  cstring cptr;                                         // temporary storage
  int i, n, nchunks;                                    // handy ints
  short s;                                              // status

  var_nsubs = var->nsubs;                               // save nsubs, slen
  var_slen  = var->slen;
  chunk_size = (systab->vol[volnum-1]->vollab->block_size * 9) / 10;
                                                        // 90% of block size

  if (len < chunk_size)                                 // if data fits
  { bcopy(data, &cptr.buf[0], len);                     // copy to local
    cptr.len = len;                                     //   buffer
    return DB_Set(var, &cptr);                          // set as short
  }

  cptr.len = itocstring(&cptr.buf[0], 0);               // store length at 0
  s = UTIL_Key_BuildEx(var, &cptr, &var->key[var_slen]);// append "0" to key
  if (s < 0)                                            // check error
  { goto ErrOut;
  }
  var->slen = var_slen + s;                             // adjust key length
  cptr.len = itocstring(&cptr.buf[0], len);             // cvt len to string
  s = DB_Set(var, &cptr);                               // set in DB
  if (s < 0)                                            // check error
  { goto ErrOut;
  }

  i = len; nchunks = 0;
  while (0 < i)
  { var->nsubs = var_nsubs;                             // reset var
    var->slen  = var_slen;
    cptr.len = itocstring(&cptr.buf[0], ++nchunks);     // add chunk to key
    s = UTIL_Key_BuildEx(var, &cptr, &var->key[var_slen]);
    if (s < 0)                                          // check error
    { goto ErrOut;
    }
    var->slen = var_slen + s;                           // adjust key length
    n = chunk_size;                                     // cpy chunk_size bytes
    if (i < n)                                          //   OR i
    { n = i;                                            //   whatever is less
    }
    bcopy(data, &cptr.buf[0], n);                       // copy n bytes
    cptr.len = n;
    s = DB_Set(var, &cptr);                             // set chunk
    if (s < 0)                                          // check error
    { goto ErrOut;
    }
    i -= n; data += n;                                  // advance input ptr
  }
  var->nsubs = var_nsubs;                               // reset var
  var->slen  = var_slen;
  cptr.len = itocstring(&cptr.buf[0], nchunks);         // cvt #chunks to string
  s = DB_Set(var, &cptr);                               // store #chunks at var
  if (s < 0)                                            // check error
  { goto ErrOut;
  }
  return len;
ErrOut:
  var->nsubs = var_nsubs;                               // reset var
  var->slen  = var_slen;
  DB_Kill(var);                                         // clean-up data
  return s;
}

//-----------------------------------------------------------------------------
// Function: DB_GetLong
// Descript: Get long data (>32K) in mvar (see DB_SetLong)
// Input(s): Pointer to mvar to get
//	     Pointer to buffer for data
// Return:   String length -> Ok, negative MUMPS error

int DB_GetLong(mvar *var, u_char *buf)
{ u_char var_nsubs, var_slen;                           // var #subs,subs len
  int i, nchunks, len;                                  // handy ints
  cstring cptr;                                         // temporary storage
  short s;                                              // status

  var_nsubs = var->nsubs;                               // save nsubs, slen
  var_slen  = var->slen;

  s = Ddata(&cptr.buf[0], var);                         // any subscripts?
  if (s < 0)                                            // check error
  { goto ErrOut;
  }
  cptr.buf[s] = '\0'; cptr.len = s;                     // null terminate
  i = atoi((char *)&cptr.buf[0]);
  if (i < 10)                                           // no subscripts
  { // fprintf(stderr,"DB_GetLong: no subscripts\r\n");
    return DB_Get(var, buf);                            // get short
  }

  cptr.len = itocstring(&cptr.buf[0], 0);               // get length at 0
  s = UTIL_Key_BuildEx(var, &cptr, &var->key[var_slen]);// append "0" to key
  if (s < 0)                                            // check error
  { goto ErrOut;
  }
  var->slen = var_slen + s;                             // adjust key length
  s = DB_Get(var, &cptr.buf[0]);                        // get length
  if (s < 0)
  { goto ErrOut;
  }
  cptr.buf[s] = '\0'; cptr.len = s;                     // null terminate
  len = atoi((char *)&cptr.buf[0]);                     // cvt length
  // fprintf(stderr,"DB_GetLong: len = %d\r\n",len);
  if (NULL == buf)                                      // no buf?
  { var->nsubs = var_nsubs;                             // reset var
    var->slen  = var_slen;
    return len;                                         //   just return len
  }

  var->nsubs = var_nsubs;                               // reset var
  var->slen  = var_slen;
  s = DB_Get(var, &cptr.buf[0]);                        // get #chunks
  if (s < 0)                                            // check error
  { goto ErrOut;
  }
  cptr.buf[s] = '\0'; cptr.len = s;                     // null terminate
  nchunks = atoi((char *)&cptr.buf[0]);                 // cvt nchunks
  // fprintf(stderr,"DB_GetLong: nchunks = %d\r\n",nchunks);

  i = 1; len = 0;
  while (i <= nchunks)                                  // read all chunks
  { var->nsubs = var_nsubs;                             // reset var
    var->slen  = var_slen;
    cptr.len = itocstring(&cptr.buf[0], i);             // add chunk to key
    s = UTIL_Key_BuildEx(var, &cptr, &var->key[var_slen]);
    if (s < 0)                                          // check error
    { goto ErrOut;
    }
    var->slen = var_slen + s;                           // adjust key length
    s = DB_Get(var, buf);                               // get chunk data
    if (s < 0)                                          // check error
    { goto ErrOut;
    }
    buf += s; len += s;                                 // advance out buffer
    i++;
  }
  // fprintf(stderr,"DB_GetLong: returned %d\r\n",len);
  var->nsubs = var_nsubs;                               // reset var
  var->slen  = var_slen;
  return len;
ErrOut:
  var->nsubs = var_nsubs;                               // reset var
  var->slen  = var_slen;
  return s;                                             // return status
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

short DB_SetEx(mvar *var, cstring *data, int has_wrlock)// set global data
{ short s;						// for returns
  int i;						// a handy int

  if (!has_wrlock)                                      // no wrlock ?
  { s = Copy2local(var,"SET");			        //   get local copy
    if (s < 0)
    { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
      return s;						// exit on error
    }

    if (systab->vol[volnum - 1]->flags & VOL_RDONLY)	// read-only ?
    { return -(ERRZ93 + ERRMLAST);			//   exit on error
    }

    if (s > 0)
    { s--;                                              // point at trantab ent
      return ROU_Process("SET", s, var, &data->buf[0], data->len);
    }
    if (systab->vol[volnum - 1]->local_name[0])		// remote VOL ?
    { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbset); // update stats
      return DGP_Set(volnum - 1, &db_var, data);
    }
  }
  i = 4 + db_var.slen + 2 + data->len;			// space reqd
  i = ((i + 3) >> 2) << 2;				// round it up
  i += 4;						// add Index
  if (i > (systab->vol[volnum-1]->vollab->block_size - BLK_HDR_SIZE)) // if too big
  { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
    return -ERRM75;					// return an error
  }
  writing = 1;						// say we are writing
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbset); // update stats
  if (!has_wrlock)                                      // has no WRITE lock ?
  { while (systab->vol[volnum - 1]->writelock ||	// check for write lock
           systab->delaywt)                               //   or delay WRITEs
    { i = Sleep(5);					// wait a bit
      if (partab.jobtab->attention)
      { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
        return -(ERRMLAST+ERRZ51);			// for <Control><C>
      }
    }							
  }                                                     // end writelock check

  i = systab->vol[volnum-1]->vollab->max_block >> 3;	// last map byte
  while (i)						// check from the end
  { if ((((u_char *) systab->vol[volnum - 1]->map)[i--]) == 0)
    { break;						// OK if byte is free
    }
  }

  if (!i)
  { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);
    return -(ERRMLAST+ERRZ11);				// complain if failed
  }

  if (has_wrlock)					// has WRITE lock?
  { TX_NEXT;
  }
  s = Set_data(data, has_wrlock);		        // do the set

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

  s = Copy2local(var,"DATA");			        // get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  if (s > 0)
  { s--;                                                // point at trantab ent
    return ROU_Process("DATA", s, var, buf, -1);
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbdat); // update stats
  if (systab->vol[volnum-1]->local_name[0])		// remote VOL ?
  { return DGP_Data(volnum-1, &db_var, buf, dat);
  }
  // fprintf(stderr,"--- S:Get_data\r\n"); fflush(stderr);
  s = Get_data(0);					// attempt to get it
  // fprintf(stderr,"--- E:Get_data\r\n"); fflush(stderr);
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
  u_char buf[16];

  s = Copy2local(var,"KILL");			        // get local copy
  if (s < 0)
  { return s;						// exit on error
  }

  if (systab->vol[volnum - 1]->flags & VOL_RDONLY)	// read-only ?
  { return -(ERRZ93 + ERRMLAST);			//   exit on error
  }

  if (s > 0)
  { s--;                                                // point at trantab ent
    sprintf((char *) &buf[0], "%d", what);
    return ROU_Process("KILL", s, var, &buf[0], strlen((char *) buf));
  }
  if (systab->vol[volnum - 1]->local_name[0])		// remote VOL ?
  { ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbkil); // update stats
    return DGP_Kill(volnum - 1, &db_var, what);
  }
   
  while (systab->vol[volnum - 1]->writelock ||	        // check for write lock
         systab->delaywt)                               //   or delay WRITEs
  { (void)Sleep(5);					// wait a bit
    if (partab.jobtab->attention)
    { return -(ERRMLAST+ERRZ51);			// for <Control><C>
    }
  }							// end writelock check
  wanna_writing = 1;
#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"--- S:Get_data KILL\r\n");
#endif
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbkil); // update stats
  s = Get_data(0);					// attempt to get it
#ifdef MV1_CACHE_DEBUG
  fprintf(stderr,"--- E:Get_data = %d KILL\r\n", s);
#endif
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
  u_char *keyptr;					// ptr to key[]

  s = Copy2local(var,"ORDER");			        // get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  if (s > 0)
  { s--;                                                // point at trantab ent
    sprintf((char *) buf, "%d", dir);
    return ROU_Process("ORDER", s, var, buf, strlen((char *) buf));
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbord); // update stats
  if (systab->vol[volnum-1]->local_name[0])		// remote VOL ?
  { return DGP_Order(volnum-1, &db_var, buf, dir, dat);
  }
  last_key = UTIL_Key_Last(&db_var);			// get start of last
  buf[0] = '\0';					// null terminate ret
  if (dir < 0)						// if it's backward
  { s = Get_data(-1);			                // get the previous
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
    { mv1_panic("DB_Order: Problem with negative direction");
    }
    chunk = (cstring *) &iidx[idx[Index]];             	// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+4];	// point at the dbc
  }							// end backwards
  else							// it's forward
  { db_var.key[db_var.slen++] = 255;			// force next key
    s = Get_data(0);			                // try to find that
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
  keyptr = Build_KeyBuf(Index, &keybuf[0], KEY_COPY);	// rebuild key
#else
  keyptr = Build_KeyBuf(Index, &keybuf[0], dat ? KEY_COPY : KEY_NOCOPY);
                                                        // point at the chunk
#endif

  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  if ((keyptr[0] < (last_key + 1)) ||
      (bcmp(&keyptr[1], db_var.key, last_key)))		// check for past it
  { return 0;						// done
  }
  i = 0;						// clear flag
  s = UTIL_Key_Extract(&keyptr[last_key+1], buf, &i);	// extract the key
  // fprintf(stderr, "DB_Order(): i=%d last_key=%d len=%d\r\n",
  //                  i, last_key, keybuf[0]);
  // fflush(stderr);
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

short DB_Query(mvar *var, u_char *buf, int dir, int flags) // get next key
{ return DB_QueryEx(var, buf, dir, flags, 0);
}

short DB_QueryEx(mvar *var, u_char *buf, int dir, int flags, cstring *dat)
{ short s;						// for returns
  int i;						// a handy int
  u_char *keyptr;					// ptr to key[]

  s = Copy2local(var,"QUERY");			        // get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  if (s > 0)
  { s--;                                                // point at trantab ent
    sprintf((char *) buf, "%d", dir);
    return ROU_Process("QUERY", s, var, buf, strlen((char *) buf));
  }
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbqry); // update stats
  if (systab->vol[volnum-1]->local_name[0])		// remote VOL ?
  { if (!(flags & GLO_DOCVT))		                // dont' convert ?
      return -(ERRM7);					//   not supported
    if (var->volset == 0)                               // default vol
    { var->volset = partab.jobtab->vol;                 //   is current vol
      flags += GLO_NOVOL;                               // no VOL specified
    }
    if (var->uci == 0)                                  // default uci
    { if (var->name.var_cu[0] == '%')                   //   if percent var
      { var->volset = 1;                                //     MGR on VOL1
        var->uci = 1;
      }
      else
      { var->uci = partab.jobtab->uci;                  //   else current uci
      }
      flags += GLO_NOUCI;                               // no UCI specified
    }
    db_var.volset = var->volset;                        // update db_var
    db_var.uci    = var->uci;
    db_var.name.var_xu = var->name.var_xu;
    if (dir < 0) flags += GLO_PREV;
    return DGP_Query(volnum-1, var, buf, flags, dat);
  }
  if (dir < 0)				                // if it's backward
  { if (!db_var.slen)                                   // if not subscripted
    { buf[0] = '\0';                                    // null terminate ret
      if (curr_lock)                                    // if locked
      { SemOp( SEM_GLOBAL, -curr_lock);                 // release global lock
      }
      return 0;                                         // and return
    }
    s = Get_data(-1);			                // get the previous
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
    { mv1_panic("DB_Query: Problem with negative direction");
    }
    chunk = (cstring *) &iidx[idx[Index]];             	// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+4];	// point at the dbc
    if ((!chunk->buf[0]) && (!chunk->buf[1]) &&		// if first node
        ((partab.jobtab->last_block_flags[volnum - 1] &
                                        GL_TOP_DEFINED) == 0))
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
  { s = Get_data(0);			                // try to find that
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
  keyptr = Build_KeyBuf(Index, &keybuf[0], KEY_COPY);	// rebuild key
#else
  keyptr = Build_KeyBuf(Index, &keybuf[0], dat ? KEY_COPY : KEY_NOCOPY);
                                                        // point at the chunk
#endif
  if (curr_lock)					// if locked
  { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
  }
  if ((s == -ERRM7) &&                                  // not found
      (keyptr[0] == 0) &&                               // key has no subscript
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
  db_var.uci = flags & GLO_NOUCI ? 0 : var->uci;	// copy
  db_var.volset = flags & GLO_NOVOL ? 0 : var->volset;	//   original & new
  db_var.name.var_xu = var->name.var_xu;		//      data
  db_var.slen = keyptr[0];				//         to
  bcopy(&keyptr[1], &db_var.key[0], keyptr[0]);		//           db_var
  if (!(flags & GLO_DOCVT))                             // if no conversion
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
  cstring dat;                                          // to hold GDP result
  cstring nextvar;

  s = Copy2local(var,"QUERYD");			        // get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  if (s > 0)
  { s--;                                                // point at trantab ent
    sprintf((char *) buf, "%d", 1);
    s = ROU_Process("QUERY", s, var, buf, strlen((char *) buf));
    // XXX kezeljuk le a fentieket, update MVAR stb.
    return s;
  }
  if (systab->vol[volnum-1]->local_name[0])		// remote VOL ?
  { buf[0] = '\0';                                      // clear buf
    s = DB_QueryEx(var, &nextvar.buf[0], 1, GLO_DOCVT, &dat);// get next w/ data
    if (s < 0)                                          // check error
    { return s;
    }
    // NB. DB_QueryEx() returns the buf length!
    nextvar.buf[s] = '\0';                              // zero terminate
    if (0 == s)
      return -(ERRMLAST+ERRZ55);
    s = UTIL_MvarFromCStr(&nextvar, var);               // convert nextvar str
    if (s < 0)                                          //   to mvar
    { return s;                                         // return if error
    }
    // NB. DB_QueryD() returns the length of the data!
    return mcopy(&dat.buf[0], buf, dat.len);            // copy data, return len
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

#ifdef MV1_CCC
  bcopy(&keybuf[1], var->key, (int) keybuf[0]);		// copy in the key
#else
  // chunk = (cstring *) &iidx[idx[Index]];            	// point at the chunk
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

short Lock_GBD(void)                                    // read lock SEM_GLOBAL
{ return SemOp(SEM_GLOBAL, READ);
}

void Unlock_GBD(void)                                   // unlock SEM_GLOBAL
{ if (curr_lock)
    SemOp(SEM_GLOBAL, -curr_lock);
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
  int remvol;                                           // flag remote volume

  if ((lock == -1) && (buf == NULL))			// just unlock?
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// unlock it
    }
    return 0;						// exit
  }
  sav = curr_lock;					// save this
  curr_lock = 0;
  s = Copy2local(var,":DB_GetLen");			// get local copy
  curr_lock = sav;					// restore current lock
  if (s < 0)						// check for error
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return s;						// and return
  }

  remvol = 0;                                           // assume not remote
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbget); // update stats
  if (systab->vol[volnum - 1]->local_name[0])		// remote VOL ?
  { remvol = 1;                                         // set remote
    s = DGP_Get(volnum - 1, &db_var, buf);              // get data
  }
  else
  { s = Get_data(0);					// attempt to get it
  }
  // fprintf(stderr, "Get_data(): s=%d\r\n", s);

  if (s < 0)						// check for error
  { if (curr_lock)					// if locked
    { SemOp( SEM_GLOBAL, -curr_lock);			// release global lock
    }
    return s;						// and return
  }
  if (!remvol && (buf != NULL))				// want data?
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

  if (systab->vol[vol-1]->local_name[0])                // remote VOL?
    return 0;

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

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);

  if (vol >= MAX_VOL)				        // within limits?
  { return (-ERRM26);					// no - error
  }
  if (systab->vol[vol]->vollab == NULL)	                // is it mounted?
  { return (-ERRM26);					// no - error
  }

  p = mv1malloc(systab->vol[vol]->vollab->block_size);	// get some space
  if (p == NULL)
  { return -(ERRMLAST+ERRZLAST+errno);			// die
  }
  bzero(p, systab->vol[vol]->vollab->block_size);	// clear it
  dbfd = open(systab->vol[vol]->file_name, O_RDWR);	// open database r/wr
  if (dbfd < 0)						// if failed
  { mv1free(p);						// free memory
    return -(ERRMLAST+ERRZLAST+errno);			// and die
  }

  fptr = (off_t)systab->vol[vol]->vollab->max_block;	// start here
  fptr = (fptr * (off_t) systab->vol[vol]->vollab->block_size)
	 + (off_t) systab->vol[vol]->vollab->header_bytes;

  fres = lseek( dbfd, fptr, SEEK_SET);			// Seek to eof
  if (fres != fptr)					// if failed
  { mv1free(p);						// free memory
    return -(ERRMLAST+ERRZLAST+errno);			// and die
  }
  vexp = vsiz - systab->vol[vol]->vollab->max_block;	// expand by
  while (vexp)
  { i = write(dbfd, p, systab->vol[vol]->vollab->block_size);
    if (i < 0)						// if failed
    { mv1free(p);					// free memory
      return -(ERRMLAST+ERRZLAST+errno);		// and die
    }
    vexp--;						// count 1
  }
  mv1free(p);						// free memory
  i = close(dbfd);					// close db file
  systab->vol[vol]->vollab->max_block = vsiz;		// store new size
  systab->vol[vol]->map_dirty_flag |= VOLLAB_DIRTY;	// say write this
  return 0;
}

//-----------------------------------------------------------------------------
// Function: DB_Dismount
// Descript: Dismount volume set
// Input(s): Volume set number to dismount
// Return:   0
//

int DB_Dismount(int volume)	                       	// dismount a volume
{ // if (volume > 1)
  { int old_volnum = volnum;
    volnum = volume;                                    // set volnum
    while (SemOp( SEM_GLOBAL, WRITE))
      ;
    DB_StopJournal(volume, JRN_ESTOP);
    SemOp( SEM_GLOBAL, -curr_lock);
    volnum = old_volnum;
  }
  systab->vol[volume-1]->dismount_flag = 1;		// set the flag
  return 0;						// that's all for now
}

//-----------------------------------------------------------------------------
// Function: DB_StopJournal
// Descript: Stop journaling on a volume
// Input(s): Volume set number to stop
//	     Reason (currently JRN_STOP and JRN_ESTOP)
// Return:   none
//

void DB_StopJournal(int volume, u_char action)		// Stop journal
{ jrnrec jj;

  ASSERT(curr_lock == WRITE);

  volnum = volume;					// set common var
  if (!systab->vol[volnum-1]->vollab->journal_available)// if no journal
  { return;						// just exit
  }
  jj.action = action;
  jj.uci = 0;
  //jj.name.var_qu = 0;
  X_Clear(jj.name.var_xu);
  jj.slen = 0;
  DoJournal(&jj, NULL);
  FlushJournal(volnum-1, 0, 1);                         // flush jrn file
  systab->vol[volnum-1]->vollab->journal_available = 0;
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

  s = Copy2local(var,":DB_GetFlags");			// get local copy
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
  s = Copy2local(var,":DB_SetFlags");			// get local copy
  if (s < 0)
  { return s;						// exit on error
  }
  writing = 1;						// say we are writing
  ATOMIC_INCREMENT(systab->vol[volnum-1]->stats.dbset); // update stats
  while (systab->vol[volnum - 1]->writelock ||		// check for write lock
         systab->delaywt)                               //   or delay WRITEs
  { i = Sleep(5);					// wait a bit
    if (partab.jobtab->attention)
    { return -(ERRMLAST+ERRZ51);			// for <Control><C>
    }
  }							// end writelock check
  Get_GBDs(0);						// ensure this many
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
  if (blk[level]->dirty == (gbd *) 1)			// if reserved
  { blk[level]->dirty = blk[level];			// terminate list
    TXSET(blk[level]);
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
  s = Copy2local(var,":DB_Compress");			// get local copy
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
  { if (curr_lock) SemOp( SEM_GLOBAL, -curr_lock);	// release curr. lock
    return -ERRM7;					// give up if nosuch
  }
  chunk = (cstring *) &iidx[idx[LOW_INDEX]];		// point at the first
  bcopy(&chunk->buf[1], &var->slen, chunk->buf[1]+1);	// save the real key

  while (TRUE)
  { bcopy(var, &db_var, sizeof(mvar));			// get next key
    writing = 0;					// flag we are reading

    while (systab->vol[volnum - 1]->writelock ||	// check for write lock
           systab->delaywt)                             //   or delay WRITEs
    { i = Sleep(5);					// wait a bit
      if (partab.jobtab->attention)
      { return -(ERRMLAST+ERRZ51);			// for <Control><C>
      }
    }							// end writelock check
    if (partab.jobtab->attention)
    { return -(ERRMLAST+ERRZ51);			// for <Control><C>
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
    if (i < systab->ZMinSpace /*1024*/)	         // if REALLY not enough space
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
