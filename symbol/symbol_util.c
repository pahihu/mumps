// File: mumps/symbol/symbol_util.c
//
// module symbol - Symbol Table Utilities

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


#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include <string.h>				// for string ops
#include <strings.h>
#include "mumps.h"                              // standard includes
#include "symbol.h"				// our definitions
#include "error.h"                              // errors
#include "init.h"				// init prototypes
#include "proto.h"				// standard prototypes
#include "compile.h"				// for routine buffer stuff

short st_hash[ST_HASH+1];                       // allocate hashing table
symtab_struct symtab[ST_MAX+1];                 // and symbol table

//****************************************************************************
//**  Function: ST_Hash - Create a hash from a variable name     ***
//**  returns hash number                                        ***
short ST_Hash(var_u *var)                       // var name in a quad
{ int i;                                        // for the loop
  int ret = 0;                                  // return value
  //int p[8] = {3,5,7,11,13,17,19,23};          // primes
  int p[MAX_NAME_BYTES] = {                     // primes
      3,  5,  7, 11, 13, 17, 19, 23,
     29, 31, 37, 41, 43, 47, 53, 59,
     61, 67, 71, 73, 79, 83, 89, 97,
    101,103,107,109,113,127,131,137 };
  for (i = 0; var->var_cu[i] && (i < MAX_NAME_BYTES); i++) // for each character
  { ret = ((var->var_cu[i] * p[i]) + ret);
  }
  return (short)(ret % ST_HASH);                // return mod hash value
}						// end of ST_Hash
//****************************************************************************
//**  Function: ST_Init - initialize an empty symbol table       ***
//**  returns nothing                                            ***
void ST_Init()                                  // no arguments
{ int i;                                        // for loops
  for (i = 0; i < (ST_HASH); i++ )              // clear hash table
  { st_hash[i] = -1;                            // -1 means empty
  }
  st_hash[ST_FREE] = 0;                         // head of free list
  for (i = 0; i < ST_MAX; i++)                  // for each symbol entry
  { symtab[i].fwd_link = (i + 1);               // point to next entry
    symtab[i].usage = 0;                        // clear usage count
    symtab[i].data = ST_DATA_NULL;              // clear data pointer
    //symtab[i].var_x = 0;                      // clear variable name
    X_Clear(symtab[i].var_x);                   // clear variable name
  }                                             // end symtab clear loop
  symtab[ST_MAX].fwd_link = -1;                 // indicate end of list
  return;                                       // done
}                                               // end of ST_Init()
//****************************************************************************
//**  Function: ST_Locate - locate varname in symbol table       ***
//**  returns short pointer or -1 on fail                        ***
//short ST_Locate(chr_q var)                      // var name in a quad
short ST_Locate(chr_x *var)                     // var name in a quad
{ int hash;					// hash value
  short fwd;                                    // fwd link pointer

  hash = ST_Hash((var_u*)var);                  // get hash value
  fwd = st_hash[hash];                          // get pointer (if any)
  while (fwd != -1)                             // while there are links
  //{ if (symtab[fwd].var_q == var)		// if var names match
  { if (_X_EQ(&symtab[fwd].var_x, var))		// if var names match
    { return (fwd);  				// return if we found it
    }
    fwd = symtab[fwd].fwd_link;                 // get next pointer (if any)
  }                                             // end search loop
  return (-1);                                  // failed to find it
}                                               // end of ST_Locate()
//****************************************************************************
//**  Function: ST_LocateIdx - locate in symbol table by index   ***
//**  returns short pointer or -1 on fail                        ***
short ST_LocateIdx(int idx)                     // var index
{ short fwd;                                    // fwd link pointer
  //chr_q var;					// var name (if reqd)
  chr_x var;					// var name (if reqd)
  rbd *p;					// for looking at routines
  //chr_q *vt;					// for the var table
  chr_x *vt;					// for the var table

  fwd = partab.jobtab->dostk[partab.jobtab->cur_do].symbol[idx];
  if (fwd > -1) return fwd;			// got it
  p = (rbd *) (partab.jobtab->dostk[partab.jobtab->cur_do].routine);
  //vt = (chr_q *) (((u_char *) p) + p->var_tbl); // point at var table
  vt = (chr_x *) (((u_char *) p) + p->var_tbl); // point at var table
  var = vt[idx];                 		// get the var name
  fwd = ST_SymAtt(&var);			// attach and get  index
  partab.jobtab->dostk[partab.jobtab->cur_do].symbol[idx] = fwd; // save idx
  return fwd;					// return index
}                                               // end of ST_LocateIdx()
//****************************************************************************
//**  Function: ST_Free - free varname entry in symbol table     ***
//**  returns nothing - only called when var exists              ***
//void ST_Free(chr_q var)                         // var name in a quad
void ST_Free(chr_x *var)                        // var name in a quad
{ short hash;                                   // hash value
  short fwd;                                    // fwd link pointer
  short last;                                   // last entry encountered

  hash = ST_Hash((var_u*)var);                  // get hash value
  last = -(hash);                               // save last value
  fwd = st_hash[hash];                          // get pointer (if any)
  while (fwd != -1)                             // while there are links
  //{ if (symtab[fwd].var_q == var) break;      // quit if we found it
  { if (_X_EQ(&symtab[fwd].var_x, var)) break;  // quit if we found it
    last = fwd;                                 // save last address
    fwd = symtab[fwd].fwd_link;                 // get next pointer (if any)
  }                                             // end search loop
  if (last == -(hash))                          // if it was top
    st_hash[hash] = symtab[fwd].fwd_link;       // remove this way
  else                                          // if it's a symtab entry
    symtab[last].fwd_link = symtab[fwd].fwd_link; // do it this way
  symtab[fwd].data = ST_DATA_NULL;		// in case it hasn't been rmvd
  //symtab[fwd].var_q = 0;                      // clear var name
  X_Clear(symtab[fwd].var_x);                   // clear var name
  symtab[fwd].fwd_link = st_hash[ST_FREE];      // point at next free
  st_hash[ST_FREE] = fwd;                       // point free list at this
  return;                                       // all done
}                                               // end of ST_Free()
//****************************************************************************
//**  Function: ST_Create - create/locate varname in symtab      ***
//**  returns short pointer or -1 on error                       ***
//short ST_Create(chr_q var)                      // var name in a quad
short ST_Create(chr_x *var)                     // var name in a quad
{ int hash;                                     // hash value
  short fwd;                                    // fwd link pointer

  hash = ST_Hash((var_u*)var);                  // get hash value
  fwd = st_hash[hash];                          // get pointer (if any)  
  while (fwd != -1)                             // while there are links
  //{ if (symtab[fwd].var_q == var)		// return if we found it
  { if (_X_EQ(&symtab[fwd].var_x, var))		// return if we found it
    { return (fwd);
    }
    fwd = symtab[fwd].fwd_link;                 // get next pointer (if any)
  }                                             // end search loop
  fwd = st_hash[ST_FREE];                       // get next free
  if (fwd == -1) return -(ERRZ56+ERRMLAST);     // error if none free
  st_hash[ST_FREE] = symtab[fwd].fwd_link;      // unlink from free list
  symtab[fwd].fwd_link = st_hash[hash];         // link previous after this
  st_hash[hash] = fwd;                          // link this first
  symtab[fwd].usage = 0;                        // no NEWs or routine att*
  //symtab[fwd].var_q = var;                    // copy in variable name
  symtab[fwd].var_x = *var;                     // copy in variable name
  symtab[fwd].data = NULL;			// no data just yet
  return (fwd);                                 // return the pointer
}                                               // end of ST_Create()
//****************************************************************************
//**  Function: ST_Kill - KILL a variable                        ***
//**  returns nothing                                            ***
short ST_Kill(mvar *var)                        // var name in a quad
{ return ST_KillEx(var, KILL_ALL);
}

short ST_KillEx(mvar *var, int what)            // var name in a quad
{ short ptr;                                    // for the pointer
  ST_data *data;                                // and ptr to data block
  ST_depend *check;				// working dependent pointer
  ST_depend *checkprev = ST_DEPEND_NULL;	// previous dependent pointer

  if (var->volset)				// if by index
  { ptr = ST_LocateIdx(var->volset - 1);	// get it this way
  }
  else						// else locate by name
  //{ ptr = ST_Locate(var->name.var_qu);        // locate the variable
  { ptr = ST_Locate(&var->name.var_xu);         // locate the variable
  }
  if (ptr < 0) return 0;			// just return if no such
  data = symtab[ptr].data;                      // get pointer to the data
  if (data != ST_DATA_NULL)			// data block exists
  { if (var->slen == 0)				// killing a data block
    { check = data->deplnk;			// point at 1st dp block
      while (check != NULL)			// for all dp blocks
      { checkprev = check;			// save a copy
        check = check->deplnk;			// get next
        dlfree(checkprev);			// free this one
      }
      data->deplnk = NULL;			// clear pointer
      data->dbc = VAR_UNDEFINED;		// dong it
    }						// end if killing a data block
    else					// killing a dep block
    { check = data->deplnk;			// get first dep if any
      if (check != ST_DEPEND_NULL)		// if deps exist
      { while ((check != ST_DEPEND_NULL) &&	// check key less than supplied
        (UTIL_Key_KeyCmp(check->bytes, var->key, check->keylen, var->slen) < 0))
        { checkprev = check;			// save current to previous
          check = check->deplnk;		// go to next
        }					// end if we go past it, or end
        if ((check != ST_DEPEND_NULL) &&
           (bcmp(check->bytes, var->key, var->slen) == 0)) // valid remove
        { ST_RemDp(data, checkprev, check, var); // get rid of it
        }					// end if valid remove found
      }						// end if dep exists
    }						// end else killing a dep blk
    if ((data->deplnk == ST_DEPEND_NULL) && (data->attach < 2) &&
        (data->dbc == VAR_UNDEFINED))		// none attached
    { dlfree(data);				// free data block space
      symtab[ptr].data = ST_DATA_NULL;		// and clear the pointer
    }						// end freeing data block
  }						// end if data block exists
  if ((symtab[ptr].data == ST_DATA_NULL) &&     // if no data block
      (symtab[ptr].usage == 0))                 // and no attaches or NEWs
  //{ ST_Free(symtab[ptr].varnam.var_qu);       // free this entry in symtab
  { ST_Free(&symtab[ptr].varnam.var_xu);        // free this entry in symtab
  }
  return 0;					// all done
}                                               // end of ST_Kill()
//****************************************************************************
//**  Function: ST_RemDp - Free a dependent block, if appropriate
//**  returns nothing
void ST_RemDp(ST_data *dblk, ST_depend *prev, ST_depend *dp, mvar *mvardr)
{
  if (dp == ST_DEPEND_NULL) return;		// no dependents to check
  if ((dp->deplnk != ST_DEPEND_NULL) && (mvardr->slen == 0)) // kill DT more dep
  { ST_RemDp(dblk, dp, dp->deplnk, mvardr);	// try to get rid of next one
  }						// end if more to do
  else						// kill DP or run out of deps
  { if ((dp->deplnk != ST_DEPEND_NULL) &&	// next dep, has part match key
        (bcmp(dp->bytes, mvardr->key, mvardr->slen) == 0))
    { ST_RemDp(dblk, dp, dp->deplnk, mvardr);	// try to get rid of next one
    }						// end if keys part match
  }						// end if more to do
  if (mvardr->slen == 0)			// killing a data block
  { if (prev != ST_DEPEND_NULL)			// prev is defined
    { prev->deplnk = ST_DEPEND_NULL;		// unlink all deps regardless
      dlfree(dp);				// get rid of dep
    }						// end if prev defined
    else					// prev not defined
    { dblk->deplnk = ST_DEPEND_NULL;		// unlink one and only dep
      dlfree(dp);				// get rid of that dep
    }						// end if prev not defined
  }						// end if killing a data block
  else						// killing a dep tree
  { if (bcmp(dp->bytes, mvardr->key, mvardr->slen) == 0) // keys match to slen
    { if (prev != ST_DEPEND_NULL)		// if not removing first dep
      { prev->deplnk = dp->deplnk;		// bypass a dep killee
      }						// end if !removing first dep
      else					// removing first dep
      { dblk->deplnk = dp->deplnk;		// bypass a first dep killee
      }						// end else removing first dep
      dlfree(dp);				// get rid of this dep
    }						// end if keys match up to slen
  }						// end else killing a dep
}						// end function ST_RemDp
//****************************************************************************
//**  Function: ST_Get - Retrieve data				 ***
//**  returns short ptr to length of data			 ***
short ST_Get(mvar *var, u_char *buf)		// get data at var/subscript
{
  short s;					// for return value
  cstring *data;				// ptr for ST_GetAdd()

  s = ST_GetAdd(var, &data);			// get address of data
  if (s < 0) return s;				// if error, quit
  s = mcopy(&data->buf[0], buf, s);		// copy data (if any)
  return s;					// return the size (or error)
}						// end function ST_Get

//****************************************************************************
//** Function: FixData(ST_data *old, ST_data *new, int count)
//** When the data pointer changes, this fixes count pointers to it
//** Only called internally from this file
void FixData(ST_data *old, ST_data *new, int count)
{ int i;					// for loops
  int c;					// a counter
  ST_newtab *newtab;				// for NEW tables

  for (i = 0; i < ST_MAX; i++)			// scan symtab[]
    if (symtab[i].data == old)			// same as our old one
    { symtab[i].data = new;			// change to new one
      count--;					// decrement the count
      if (count == 0) return;			// quit when done
    }
  i = partab.jobtab->cur_do;			// get current do level
  while (i)					// for each one
  { newtab = (ST_newtab *) partab.jobtab->dostk[i--].newtab; // get newtab
    while (newtab != NULL)			// for each table
    { for (c = 0; c < newtab->count_new; c++)	// for each variable
	if (newtab->locdata[c].data == old)	// if this is one
	{ newtab->locdata[c].data = new;	// copy in new value
	  count--;				// count that
	  if (count == 0) return;		// quit when done
	}
      newtab = newtab->fwd_link;		// get next
    }
  }
}

//****************************************************************************
//** Function: ST_Set - Set a variable in memory, create or replace	***
//** returns 0 on success, negative otherwise				***
short ST_Set(mvar *var, cstring *data)		// set var to be data
{
  ST_depend *ptr1;				// a dependent pointer
  ST_depend *newPtrDp = ST_DEPEND_NULL;		// a dependent pointer
  ST_data *newPtrDt = ST_DATA_NULL;		// a data pointer
  ST_depend *prevPtr = ST_DEPEND_NULL;		// pointer to previous element
  int n;					// key length
  int i;					// loops
  int pad = 0;					// extra space padding
  short *ptr2short;				// needed for short into char
  int fwd;					// position in symtab

  if ((var->slen & 1) != 0) pad = 1;		// set up for any extra space
  if (var->volset)				// if volset defined
    fwd = ST_LocateIdx(var->volset - 1);	// locate var by volset
  else						// if no volset or volset zero
    //fwd = ST_Create(var->name.var_qu);	// attempt to create new ST ent
    fwd = ST_Create(&var->name.var_xu);		// attempt to create new ST ent
  if (symtab[fwd].data == ST_DATA_NULL)		// if not already exists
  { i = DTBLKSIZE + data->len;			// reqd memory
    if ((var->slen != 0) || (i < DTMINSIZ))	// not reqd or too small
      i = DTMINSIZ;				// make it min size
    newPtrDt = dlmalloc(i);			// allocate necessary space
    if (newPtrDt == NULL) return -(ERRZ56+ERRMLAST); // no memory avlb
    if (var->slen == 0)				// no subscript key
    { newPtrDt->deplnk = ST_DEPEND_NULL;	// no dependents
      newPtrDt->attach = 1;			// initialize attach count
      newPtrDt->dbc = data->len;		// initialize data bytes count
      bcopy(&data->buf[0], &newPtrDt->data, data->len+1); // copy data in
    }						// end if - slen is zero
    else					// subscript defined
    { newPtrDp = dlmalloc(DPBLKSIZE + data->len + var->slen + pad);//new dep blk
      if (newPtrDp == NULL) return -(ERRZ56+ERRMLAST); // no memory avlb
      newPtrDt->dbc = VAR_UNDEFINED;		// initialize data byte count
      newPtrDt->deplnk = newPtrDp;		// initialize link to dependent
      newPtrDt->attach = 1;			// initialize attach count
      newPtrDp->deplnk = ST_DEPEND_NULL;	// no more dependents
      newPtrDp->keylen = var->slen;		// copy sub keylength
#ifdef MV1_SUBSPOS
      newPtrDp->nsubs = 255;
      // UTIL_Key_Subs((char *)var->key, (int)var->slen,
      //      &newPtrDp->nsubs, (u_char *)&newPtrDp->subspos[0]);
#endif
      n = var->slen;				// get the key size
      bcopy(&var->key[0], &newPtrDp->bytes[0], n); // copy the key
      if (n & 1) n++;				// ensure n is even
      ptr2short = (short *) &newPtrDp->bytes[n]; // get an (short *) to here
      *ptr2short = data->len;			// save the data length
      n += 2;					// point past the DBC
      bcopy(&data->buf[0], &newPtrDp->bytes[n], data->len+1); // data & term 0
    }						// end else-has subs
    symtab[fwd].data = newPtrDt;		// link it in
  }						// end if-no data block
// **************************************************************************
// The following ELSE segment uses the logic that data is unlinked and a new
// block DT or DP takes it's place.
// POSSIBLE ALTERNATIVE *** Implement the replacement of existing data by
// utilising the same block and replacing its elements
// **************************************************************************
  else						// data block already exists
  { if (var->slen == 0)				// not dependent setting
    { newPtrDt = dlrealloc(symtab[fwd].data,
			 DTBLKSIZE + data->len); // allocate data block
      if (newPtrDt == NULL) return -(ERRZ56+ERRMLAST); // no memory avlb
      if ((newPtrDt != symtab[fwd].data) &&	// did it move?
	  (newPtrDt->attach > 1))		// and many attached
      { FixData(symtab[fwd].data, newPtrDt, newPtrDt->attach); // fix it
      }
      else
      { symtab[fwd].data = newPtrDt;		// or just do this one
      }
      newPtrDt->dbc = data->len;		// set data byte count
      bcopy(&data->buf[0], &newPtrDt->data, data->len+1); // copy the data in
    }						// end if-not dependent setting
    else					// setting dependent
    { newPtrDp = dlmalloc(DPBLKSIZE + data->len + var->slen + pad);//allo DP blk
      if (newPtrDp == NULL) return -(ERRZ56+ERRMLAST); // no memory avlb
      newPtrDp->deplnk = ST_DEPEND_NULL;	// init dependent pointer
      newPtrDp->keylen = var->slen;		// copy sub keylength
#ifdef MV1_SUBSPOS
      newPtrDp->nsubs = 255;
      // UTIL_Key_Subs((char *)var->key, (int)var->slen,
      //      &newPtrDp->nsubs, (u_char *)&newPtrDp->subspos[0]);
#endif
      n = var->slen;				// get the key size
      bcopy(&var->key[0], &newPtrDp->bytes[0], n); // copy the key
      if (n & 1) n++;				// ensure n is even
      ptr2short = (short *) &newPtrDp->bytes[n]; // get an (short *) to here
      *ptr2short = data->len;			// save the data length
      n += 2;					// point past the DBC
      bcopy(&data->buf[0], &newPtrDp->bytes[n], data->len+1);
						// copy data and trailing 0
      ptr1 = symtab[fwd].data->deplnk;		// go into dependents
      if (ptr1 != ST_DEPEND_NULL)		// dep's currently exist
      { while ((ptr1 != ST_DEPEND_NULL) &&	// compare keys
          (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) > 0))
        { prevPtr = ptr1;			// save previous
          ptr1 = ptr1->deplnk;			// get next
          if (ptr1 == ST_DEPEND_NULL) break;	// gone beyond last
        }  					// end while-compare keys
        if ((ptr1 != ST_DEPEND_NULL) &&		// replace data
           (UTIL_Key_KeyEqu(ptr1->bytes, var->key, // if var keys equal it means
                      ptr1->keylen, var->slen) == 0)) // replace data
        { if (prevPtr == ST_DEPEND_NULL)	// if no prev pointer
          { newPtrDp->deplnk = ptr1->deplnk;	// link to previous first dep
            symtab[fwd].data->deplnk = newPtrDp; // link in as first dep
            dlfree(ptr1);			// remove previous dep link
          }					// end if no prev ptr
          else					// if prev pointer is defined
          { newPtrDp->deplnk = ptr1->deplnk;	// set new dependent link
            prevPtr->deplnk = newPtrDp;		// alter previous to link in
            dlfree(ptr1);			// remove previous dep link
          }					// end else bypassing mid list
        }					// end if-replace data
        else if ((ptr1 != ST_DEPEND_NULL) && 	// if we have a dependent
          (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) < 0))
        { if (ptr1 != ST_DEPEND_NULL)		// create more data
          { if (prevPtr == ST_DEPEND_NULL)	// new first element
            { newPtrDp->deplnk = ptr1;		// link in
              symtab[fwd].data->deplnk = newPtrDp; // link in
            }					// end add new first element
            else				// insert new element, mid list
            { newPtrDp->deplnk = ptr1;		// link in
              prevPtr->deplnk = newPtrDp;	// link in
            }					// end insert mid list
          }					// end if-create more data
        }					// end dep's currently exist
        else if ((ptr1 == ST_DEPEND_NULL) && (prevPtr != ST_DEPEND_NULL))
        { newPtrDp->deplnk = ST_DEPEND_NULL;	// link in
          prevPtr->deplnk = newPtrDp;		// link in
        }					// end add to end of list
      }						// end if elements exist
      else					// no elements currently exist
      { symtab[fwd].data->deplnk = newPtrDp;	// add as first element ever
      }						// end else no elements existed
    }						// end else-slen not zero
  }						// end else-data block not null
  return data->len;				// return length of data
}						// end ST_Set

//****************************************************************************
//** Function: ST_Data - examine type of variable
//** returns pointer to length of type of data
short ST_Data(mvar *var, u_char *buf)		// put var type in buf
{ return ST_DataEx(var, buf, 0);
}

short ST_DataEx(mvar *var, u_char *buf, cstring *dat)// put var type in buf
// *buf is set to 0, 1, 10, 11 depending on the type of data *var is
// if dat is not zero and var has a value, value is loaded into dat
//returned is the length of *buf, not including the /0 terminator
//                0 = The variable has no data and no descendants (ie. undef).
//                1 = The variable has data but no descendants.
//               10 = The variable has no data but has descendants.
//               11 = The variable has data and has descendants.
{
  int ptr1;					// position in symtab
  int i;
  ST_depend *depPtr;				// active pointer
  ST_depend *prevPtr;				// pointer to previous element
  cstring *addr;                                // pointer to data

  if (var->volset)				// if by index
  { ptr1 = ST_LocateIdx(var->volset - 1);	// get it this way
  }
  else						// if no volset, use name
  //{ ptr1 = ST_Locate(var->name.var_qu);       // locate the variable by name
  { ptr1 = ST_Locate(&var->name.var_xu);        // locate the variable by name
  }
  if (ptr1 == -1)				// not found
  { bcopy("0\0", buf, 2);
    return 1;  					// end if-no data&no descendants
  }						// end if not found
  if (symtab[ptr1].data == ST_DATA_NULL)	// not found
  { bcopy("0\0", buf, 2);
    return 1;					// return length of buf
  }						// end if not found
  if (var->slen > 0)				// going into dependents
  { depPtr = symtab[ptr1].data->deplnk;		// get first dependent
    i = var->slen;				// get the length
    if (depPtr != ST_DEPEND_NULL)		// only if we should go on
    { if (depPtr->keylen < i) i = depPtr->keylen; // adjust length if needed
    }						// end if
    while ((depPtr != ST_DEPEND_NULL) &&	// while not yet found or past
           (memcmp(depPtr->bytes, var->key, i) < 0))
    { prevPtr = depPtr;				// save this pos
      depPtr = depPtr->deplnk;			// go to next
      if (depPtr == ST_DEPEND_NULL) break;	// have we run out
      i = var->slen;				// get the length again
      if (depPtr->keylen < i) i = depPtr->keylen; // adjust length if needed
    }						// end while
    if (depPtr == ST_DEPEND_NULL)		// if we ran out of dep's
    { bcopy("0\0", buf, 2);
      return 1;					// return same
    }						// end if
    i = var->slen;				// get the length again
    if (depPtr->keylen < i)
    { i = depPtr->keylen; 			// adjust length if needed
    }
    while ((depPtr != ST_DEPEND_NULL) &&	// while more deps and
           (memcmp(depPtr->bytes, var->key, i) == 0) && // matches ok for i
           (depPtr->keylen < var->slen)	||      // and var slen still longer
           ((depPtr != ST_DEPEND_NULL) &&	// while more deps and
           (memcmp(depPtr->bytes, var->key, i) < 0))) // an exact match
    { prevPtr = depPtr;				// save this pos
      depPtr = depPtr->deplnk;			// go to next
      if (depPtr == ST_DEPEND_NULL)
      { break;					// have we run out
      }
      i = var->slen;				// get the length again
      if (depPtr->keylen < i) i = depPtr->keylen; // adjust length if needed
    }						// end while
    if (depPtr == ST_DEPEND_NULL)		// if we ran out
    { bcopy("0\0", buf, 2);
      return 1;					// return same
    }						// end if
    if ((depPtr != ST_DEPEND_NULL) &&
        (memcmp(depPtr->bytes, var->key, i) == 0)) // if matches ok for i
    { if (depPtr->keylen == var->slen)		// exact match
      { prevPtr = depPtr;			// save this pos
        depPtr = depPtr->deplnk;		// go to next
        if (depPtr == ST_DEPEND_NULL)		// have we run out
        { bcopy("1\0", buf, 2);
          return 1;				// return same
        }					// end if
        i = var->slen;				// get the length
        if (depPtr->keylen < i)
        { i = depPtr->keylen; // adjust len if needed
	}
        if (memcmp(depPtr->bytes, var->key, i) == 0) // if match ok for i
        { bcopy("11\0", buf, 3);
          return 2;				// return same
        }					// end if
        else
        { bcopy("1\0", buf, 2);
          return 1;				// return same
        }
      }						// end if exact match
      else					// beyond exact match
      { bcopy("10\0", buf, 3);
        return 2;				// return same
      }						// end else beyond exact match
    }						// end if match to i
    else					// no longer matches for i
    { bcopy("0\0", buf, 2);
      return 1;					// return same
    }
  }  						// end if-going to dependents
  else						// operate on data block
  { if (dat && (symtab[ptr1].data->dbc != VAR_UNDEFINED))// dat given and has
    { addr = (cstring *) &symtab[ptr1].data->dbc;     // a value, copy it
      bcopy(&addr->buf[0], &dat->buf[0], addr->len);
      dat->len = addr->len;
    }
    if ((symtab[ptr1].data->dbc != VAR_UNDEFINED) &&	// dbc defined
        (symtab[ptr1].data->deplnk == ST_DEPEND_NULL))	// data&no descendants
    { bcopy("1\0", buf, 2);
      return 1;
    }  						// end if-data&no descendants
    if ((symtab[ptr1].data->dbc == VAR_UNDEFINED) &&	// dbc not defined
        (symtab[ptr1].data->deplnk != ST_DEPEND_NULL))	// no data&descendants
    { bcopy("10\0", buf, 3);
      return 2; }  				// end if-no data&descendants
    if ((symtab[ptr1].data->dbc != VAR_UNDEFINED) &&	// dbc not defined
        (symtab[ptr1].data->deplnk != ST_DEPEND_NULL))	// data&descendants
    { bcopy("11\0", buf, 3);
      return 2; }  				// end if-data&descendants
  }						// end else-ops on data block
  bcopy("0\0", buf, 2);
  return 1;					// return
}  						// end ST_Data

//****************************************************************************
//** Function: ST_Order - get next subscript in sequence, forward or reverse
//** returns pointer to length of next subscript
short ST_Order(mvar *var, u_char *buf, int dir)
{ return ST_OrderEx(var, buf, dir, 0);
}

short ST_OrderEx(mvar *var, u_char *buf, int dir, cstring *dat)
{
  int ptr1;                                     // position in symtab
  ST_depend *current;                           // active pointer
  ST_depend *prev = ST_DEPEND_NULL;             // pointer to previous element
#ifdef MV1_SUBSPOS
  u_char pieces = 0;                            // subscripts in key
#else
  int pieces = 0;
#endif
  int subs;
  int i = 0;                                    // generic counter
  char keysub[256];                             // current key subscript
  u_char upOneLev[256];
  u_char crud[256];
  int index = 0;                                // where up to in key extract
  int upto = 0;                                 // max number of subs
  short ret = 0;                                // current position in key
  cstring *addr;                                // points to data

  if (var->volset)                              // if by index
  { ptr1 = ST_LocateIdx(var->volset - 1);       // get it this way
  }
  else                                          // no volset, so use var name
  //{ ptr1 = ST_Locate(var->name.var_qu);       // locate the variable by name
  { ptr1 = ST_Locate(&var->name.var_xu);        // locate the variable by name
  }
  buf[0] = '\0';                                // JIC
  if (ptr1 < 0)
  { return 0;
  }                                             // not found
  if (var->slen <= 0)                           // if trying to $O a data block
  { return 0;                                   // cant $Order a data block
  }                                             // return null

#ifdef MV1_SUBSPOS
  if (var->nsubs == 255)
  { UTIL_Key_Subs((char *)var->key, (int)var->slen,
                                    &var->nsubs, &var->subspos[0]);
  }
  pieces = var->nsubs;
  upOneLev[0] = UTIL_Key_Chars_In_SubsEx(
                  (char *)var->key, (int)var->slen,
                  &var->nsubs, &var->subspos[0],
                  pieces-1, &subs, (char *)&upOneLev[1]);
#else
  UTIL_Key_Chars_In_Subs((char *)var->key, (int)var->slen, 255, &pieces, (char *)NULL);
                                                // Return number of subscripts in pieces
  upOneLev[0]=(u_char) UTIL_Key_Chars_In_Subs((char *)var->key, (int)var->slen,
                                              pieces-1, &subs, (char *)&upOneLev[1]);
#endif
        // Return characters in all but last subscript, number of subscripts in subs
        // and key string (less last subscript) at upOneLev[1] on for upOneLev[0] bytes

  if (symtab[ptr1].data == ST_DATA_NULL)        // no data
  { return 0;
  }
  current = symtab[ptr1].data->deplnk;          // go to first dependent
  while ((current != ST_DEPEND_NULL) &&         // compare keys
          ( UTIL_Key_KeyCmp(var->key, current->bytes, // while we have dependent
                      var->slen, current->keylen ) > 0 )) // & key match fails
  { prev = current;                             // set prev pointer
    current = current->deplnk;                  // go to next dependant pointer
  }                                             // end while-compare keys
  if (current == ST_DEPEND_NULL)                // nothing past our key
  { if (dir == 1)
    { return 0;
    }
  }                                             // output same, return length

  if (dir == -1)                                // reverse order
  { current = prev;                             // save current
    if (current == ST_DEPEND_NULL)              // if current pointing nowhere
    { return 0;                                 // return length of zero
    }                                           // end if current points->NULL
#ifdef MV1_SUBSPOS
    crud[0]=UTIL_Key_Chars_In_SubsEx(
            (char *)current->bytes, (int)current->keylen,
            &current->nsubs, &current->subspos[0],
            pieces-1, &subs, (char *)&crud[1]);
#else
    crud[0]=UTIL_Key_Chars_In_Subs((char *)current->bytes, (int)current->keylen,
            pieces-1, &subs, (char *)&crud[1]);
#endif
    if ((crud[0] != 0) && (upOneLev[0] != 0))
    { if (crud[0] != upOneLev[0]) return(0);
      if (bcmp(&crud[1], &upOneLev[1], upOneLev[0]) != 0) return(0);
            // Ensure higher level subscripts (if any) are equal
    }
        
  }                                             // end if reverse order
 
  else                                          // forward order
  { while ((current != ST_DEPEND_NULL) &&       // while we have dependents
             (bcmp(var->key, current->bytes, var->slen) == 0)) // key cmp fails
    { current = current->deplnk;                // go to next dependent
    }                                           // end while
    if (current == ST_DEPEND_NULL)              // if current no points nowhere
    { return 0;                                 // return length of zero
    }                                           // end if current is now NULL

    if (UTIL_Key_KeyEqu(var->key, current->bytes, // compare keys. If compare
                    var->slen, current->keylen) != 0) // fails to match exact
    { 
#ifdef MV1_SUBSPOS
      crud[0]=UTIL_Key_Chars_In_SubsEx(
              (char *)current->bytes, (int)current->keylen,
              &current->nsubs, &current->subspos[0],
              pieces-1, &subs, (char *)&crud[1]);
#else
      crud[0]=UTIL_Key_Chars_In_Subs((char *)current->bytes, (int)current->keylen,
                                       pieces-1, &subs, (char *)&crud[1]);
#endif

      if ((crud[0] != 0) && (upOneLev[0] != 0)) // if lengths aren't 0
      { if (bcmp(&crud[1], &upOneLev[1], upOneLev[0]) != 0) // & cmp fails
	    { return 0;                             // return a length of zero
	    }                                       // end if higher key levels !=
      }                                         // end if slen's non zero
    }                                           // end if keys dont equal
  }                                             // end else-forward order
  if (current == ST_DEPEND_NULL)                // nothing past our key
  { return 0;                                   // return length
  }                                             // end if nothing past our key
  ret = 0;
  for (i=1; i<=pieces; i++)                     // number of keys
  { upto = 0;                                   // clear flag
                                                // nxt key
    ret = UTIL_Key_Extract(&current->bytes[index], (u_char *)keysub, &upto);
    index = index + upto;                       // increment index
    if ((index >= current->keylen) &&
          (i < pieces))                         // hit end of key & !found
    { return 0;                                 // return null
    }                                           // end if hit end
  }                                             // end for-pieces to level reqd
                                                // now have ascii key in
                                                // desired position number
  // fprintf(stderr, "keylen=%d index=%d upto=%d\r\n",
  //                  current->keylen, index, upto);
  // fflush(stderr);
  if (dat && (current->keylen == index))        // dat given and has value
  { i = (int) current->keylen;			// get key length
    if (i&1) i++;				// ensure even
    addr = (cstring *) &(current->bytes[i]);	// data addr as cstring
    bcopy(&addr->buf[0], &dat->buf[0], addr->len);
    dat->len = addr->len;
  }
  return mcopy((u_char *)keysub, buf, ret);	// put the ascii value of
                                                // that key in *buf
                                                // return the length of it
}                                               // end function - ST_Order

//****************************************************************************
//** Function: ST_Query - return next whole key in sequence, forward or reverse
//** returns pointer to length of next key
short ST_Query(mvar *var, u_char *buf, int dir)
{ return ST_QueryEx(var, buf, dir, 0);
}

short ST_QueryEx(mvar *var, u_char *buf, int dir, cstring *dat)
{
  int ptr1;					// position in symtab
  ST_depend *current = ST_DEPEND_NULL;		// active pointer
  ST_depend *prev = ST_DEPEND_NULL;		// pointer to previous element
  short askeylen = 0;				// length of *askey
  mvar outputVar = *var;			// copy of supplied mvar
  cstring *addr;                                // data addr as cstring
  int i, isdbc = 0;                             // flag as DBC

  if (!var->slen && (dir == -1))                // not subscripted and backwards
  { buf[0] = '\0';                              // null terminate ret
    return 0;                                   // and return
  }

  if (var->volset)				// if by index
  { ptr1 = ST_LocateIdx(var->volset - 1);	// get it this way
  }
  else						// no volset, use var name
  //{ ptr1 = ST_Locate(var->name.var_qu);       // locate the variable by name
  { ptr1 = ST_Locate(&var->name.var_xu);        // locate the variable by name
  }
  buf[0] = '\0';				// JIC
  if (ptr1 < 0)
  { return 0;
  }						// not found
  if (symtab[ptr1].data == ST_DATA_NULL)	// no data block, err
  { return 0;
  }						// output same, return length
  current = symtab[ptr1].data->deplnk;          // first dependant pointed at
  if (current == ST_DEPEND_NULL)		// not found
  { return 0;
  }						// output same, return length
  while ((current != ST_DEPEND_NULL) &&		// while more exist
         (UTIL_Key_KeyCmp(var->key, current->bytes, // with keys that are larger
          var->slen, current->keylen) > 0))	// get next dep
  { prev = current;				// save prev pointer
    current = current->deplnk;			// go to next dependant pointer
  }						// end while-compare keys
  if (var->slen > 0)				// looking in dependents
  { if (dir == -1)				// reverse order
    { if (prev != ST_DEPEND_NULL)		// only if previous ptr defined
      { current = prev;				// go back one
      }						// end if prev ptr defined
    }						// end if reverse order
    else					// forward order
    { if ((current != ST_DEPEND_NULL) &&	// not going past non exist last
          (UTIL_Key_KeyEqu(var->key, current->bytes,  // and keys are
           var->slen, current->keylen) == 0))	// equal
      { current = current->deplnk;		// go to next
      }						// end if exact match
    }						// end else-forward
  }						// finished looking

  if (current == ST_DEPEND_NULL)		// if we have gone past the end
  { return 0;					// return length of zero
  }						// end if gone past end
  if ((dir == -1) && (var->slen == 0))		// reverse dir and data block
  { return 0;					// return length of zero
  }						// past front going backwards
  outputVar.slen = current->keylen;		// key and length of set
  bcopy(current->bytes, outputVar.key, (int) current->keylen); // setup mvar
  if ((current == symtab[ptr1].data->deplnk) && (prev != current) &&
      (dir == -1) && (var->slen > 0))		// previous is a data block
  { outputVar.slen = 0;				// flag is as such
    isdbc = 1;
  }						// end if back to a data block
  if (dat)                                      // dat given
  { if (isdbc)
    { addr = (cstring *) &symtab[ptr1].data->dbc;
    }
    else
    { i = (int) current->keylen;		// get key length
      if (i&1) i++;				// ensure even
      addr = (cstring *) &(current->bytes[i]);	// data addr as cstring
    }
    bcopy(&addr->buf[0], &dat->buf[0], addr->len);
    dat->len = addr->len;
  }
  askeylen = UTIL_String_Mvar(&outputVar, buf, 255); // convert mvar
  return askeylen;				// return length of key
}						// end ST_Query

//****************************************************************************
short ST_GetAdd(mvar *var, cstring **add)	// get local data address
{ int ptr1;					// position in symtab
  ST_depend *depPtr = ST_DEPEND_NULL;		// active pointer
  ST_depend *prevPtr = ST_DEPEND_NULL;		// pointer to previous element
  int i;					// generic counter

  if (var->volset)				// if by index
  { ptr1 = ST_LocateIdx(var->volset - 1);	// get it this way
  }
  else						// no volset, use var name
  //{ ptr1 = ST_Locate(var->name.var_qu);       // locate the variable by name
  { ptr1 = ST_Locate(&var->name.var_xu);        // locate the variable by name
  }
  if ((ptr1 >= ST_MAX) || (ptr1 < -1))
  { panic("ST_GetAdd: Junk pointer returned from ST_LocateIdx");
  }
  if (ptr1 >= 0)				// think we found it
  { if (symtab[ptr1].data == ST_DATA_NULL)
    { return -(ERRM6); 				// not found
    }
    if (var->slen > 0)				// go to dependents
    { depPtr = symtab[ptr1].data->deplnk;	// get first dependent
      while (depPtr != ST_DEPEND_NULL)		// while dep ok, compare keys
      { i = UTIL_Key_KeyCmp(var->key, depPtr->bytes, var->slen, depPtr->keylen);
	if (i == K2_GREATER)
	{ return -(ERRM6);			// error if we passed it
	}
	if (i == KEQUAL)
	{ break;				// found it
	}
        prevPtr = depPtr;			// compare keys
        depPtr = depPtr->deplnk;		// save previous, get next
      }						// end while - compare keys
      if (depPtr == ST_DEPEND_NULL)		// if no exist
      { return -(ERRM6);			// return same
      }						// end if no exist
      i = (int) depPtr->keylen;			// get key length
      if (i&1) i++;				// ensure even
      *add = (cstring *) &(depPtr->bytes[i]);	// send data addr as cstring
      return (*add)->len;			// and return the length
    }						// end if-had to go dependent
    else					// data block
    { *add = (cstring *) &symtab[ptr1].data->dbc; // setup the address
      i = symtab[ptr1].data->dbc;		// get dbc
      if (i == VAR_UNDEFINED)			// dbc not defined and no int
      { return -(ERRM6);			// return same
      }
      return i;					// and return the count
    }						// finished with data block
  }						// end if - symtab posi valid
  return -(ERRM6);				// return if failed
}						// end ST_GetAdd
//****************************************************************************
short ST_Dump()                                 // dump entire ST to $I
// 1 to ST_MAX-1 (ie. 3071).
{
  int i;					// generic counter
  int j;					// generic counter
  short s;					// for functions
  cstring *cdata;				// variable data gets dumped
  u_char dump[512];				// variable name gets dumped
  ST_depend *depPtr = ST_DEPEND_NULL;		// active dependent ptr
  for (i = 0; i<= ST_MAX-1; i++)		// for each entry in ST
  { if (symtab[i].data == ST_DATA_NULL) continue; // get out if nothing to dump
    if (symtab[i].varnam.var_cu[0] == '$') continue; // dont spit out $ vars
    //partab.src_var.name.var_qu = symtab[i].varnam.var_qu; // init var name
    partab.src_var.name.var_xu = symtab[i].varnam.var_xu; // init var name
    partab.src_var.uci = UCI_IS_LOCALVAR;	// init uci as LOCAL
    partab.src_var.slen = 0;			// init subscript length
    partab.src_var.volset = 0;			// init volume set
    cdata = (cstring *) &dump[0];		// make it a cstring
    if (symtab[i].data->dbc != VAR_UNDEFINED)	// valid dbc
    { cdata->len = UTIL_String_Mvar(&partab.src_var, // get var name
    	cdata->buf, 255); 			// dump data block
      cdata->buf[cdata->len++] = '=';		// tack on equal sign
      s = SQ_Write(cdata);			// dump var name =
      if (s < 0) return s;			// die on error
      s = SQ_Write((cstring *) &(symtab[i].data->dbc));	// dump data block
      if (s < 0) return s;			// die on error
      s = SQ_WriteFormat(SQ_LF);		// line feed
      if (s < 0) return s;			// die on error
    }						// end if valid dbc
    cdata = NULL;				// nullify the cstring
    depPtr = symtab[i].data->deplnk;		// get first dependent
    while (depPtr != ST_DEPEND_NULL)		// while dependents exist
    //{ partab.src_var.name.var_qu = symtab[i].varnam.var_qu; // init var name
    { partab.src_var.name.var_xu = symtab[i].varnam.var_xu; // init var name
      partab.src_var.uci = UCI_IS_LOCALVAR;	// init uci as LOCAL
      partab.src_var.slen = depPtr->keylen;	// init subscript length
      partab.src_var.volset = 0;		// init volume set      
      bcopy(depPtr->bytes, partab.src_var.key, depPtr->keylen); // init key
      cdata = (cstring *) &dump[0];		// get into a cstring
      cdata->len = UTIL_String_Mvar(&partab.src_var, // get var name
        cdata->buf, 255);			// dump dependent block
      cdata->buf[cdata->len++] = '=';		// tack on an equal sign
      s = SQ_Write(cdata);			// dump var name =
      if (s < 0) return s;			// die on error
      j = (int) depPtr->keylen;			// find key length
      if ((j&1)!=0) j++;			// up it to next even boudary
      s = SQ_Write((cstring *) &depPtr->bytes[j]); // write out the data
      if (s < 0) return s;			// return if error occured
      s = SQ_WriteFormat(SQ_LF);		// write a line feed
      if (s < 0) return s;			// die on error
      depPtr = depPtr->deplnk;			// get next if any
    }						// end while dependents exist
  }						// end for all symtab entries
  return 0;					// finished successfully
}						// end function ST_Dump

//****************************************************************************
short ST_QueryD(mvar *var, u_char *buf)		// get next key and data
// Return next key in supplied mvar and data at buf
{
  int ptr1;					// position in symtab
  cstring *cdata;				// temporary data access
  ST_depend *current = ST_DEPEND_NULL;		// active pointer
  ST_depend *prev = ST_DEPEND_NULL;		// pointer to previous element
  int i;					// generic counter
 
  if (var->volset)				// if by index
  { ptr1 = ST_LocateIdx(var->volset - 1);	// get it this way
  }
  else						// no volset, use name
  //{ ptr1 = ST_Locate(var->name.var_qu);       // locate the variable by name
  { ptr1 = ST_Locate(&var->name.var_xu);        // locate the variable by name
  }
  buf[0] = '\0';				// JIC
  if (ptr1 < 0)
  { return -(ERRM6);				// not found
  }
  if (symtab[ptr1].data == ST_DATA_NULL)
  { return -(ERRM6); 				// no data, err
  }
  current = symtab[ptr1].data->deplnk;          // first dependant pointed at
  if (current == ST_DEPEND_NULL)		// not found
  { return -(ERRZ55+ERRMLAST);			// no data below
  }						// end if not found
  while ((current != ST_DEPEND_NULL) &&		// more deps exist
         (UTIL_Key_KeyCmp(var->key, current->bytes,	// and key compare fails
          var->slen, current->keylen) > 0 ))	// compare keys
  { prev = current;				// set prev pointer
    current = current->deplnk;			// to next dependant pointer
  }						// end while-compare keys

  if ((current != ST_DEPEND_NULL) &&		// while more deps exist
      (UTIL_Key_KeyEqu(var->key, current->bytes, // and keys match exactly
       var->slen, current->keylen) == 0))	// equal
  { current = current->deplnk;			// go to next
  }						// end if keys equal

  if (current == ST_DEPEND_NULL)		// if no more deps
  { return -(ERRZ55+ERRMLAST);			// out of data, err
  }						// end if no more deps

  bcopy(current->bytes, var->key, current->keylen); // set up mvar
  var->slen = current->keylen;			// key and len
  i = (int) current->keylen;			// get key length
  if ((i&1)!=0) i++;				// up it to next even boundary
  cdata = (cstring *) &(current->bytes[i]);	// convert to cstring
  return mcopy(cdata->buf, buf, cdata->len);	// get data into buf
}						// end ST_QueryD

//****************************************************************************
short ST_DumpV(mvar *global)
// copy all variables in as subscripts to specified global.
{
  int i;					// generic counter
  int j;					// generic counter
  short s;					// for functions
  short gslen, gnsubs;				// global slen save value
  u_char gks[255];
  cstring *cdata;				// variable data gets dumped
  u_char dump[1024];				// variable name gets dumped
  ST_depend *depPtr = ST_DEPEND_NULL;		// active dependent ptr

  cdata = (cstring *) dump;			// make it a cstring
  partab.src_var.uci = UCI_IS_LOCALVAR;		// init uci as LOCAL
  partab.src_var.volset = 0;			// init volume set
  gslen  = global->slen;			// save original sub length
  gnsubs = global->nsubs;
  bcopy(global->key, gks, global->slen);	// save original key
  for (i = 0; i<= ST_MAX-1; i++)		// for each entry in ST
  { if (symtab[i].data == ST_DATA_NULL) continue; // get out if nothing to dump
    if (symtab[i].varnam.var_cu[0] == '$') continue; // no $ vars
    //if (symtab[i].varnam.var_qu == 0) continue;// ensure something there
    //partab.src_var.name.var_qu = symtab[i].varnam.var_qu; // init var name
    if (X_Empty(symtab[i].varnam.var_xu)) continue;// ensure something there
    partab.src_var.name.var_xu = symtab[i].varnam.var_xu; // init var name
    partab.src_var.slen = 0;			// init subscript length
    if (symtab[i].data->dbc != -1)		// if data exists
    { s = UTIL_String_Mvar(&partab.src_var, cdata->buf, 255);
      cdata->len = s;
      bcopy(gks, global->key, gslen);		// restore initial key
      global->slen  = gslen;			// restore initial length
      global->nsubs = gnsubs;
      s = UTIL_Key_BuildEx(global, cdata, &global->key[gslen]);
      if (s < 0) return s;                      // got an error
      global->slen = global->slen + s;          // set rest of global key&len
      s = DB_Set(global, (cstring *) &(symtab[i].data->dbc)); // try to set it
      if (s == -ERRM75)				// if string too long
      { j = symtab[i].data->dbc;		// save this
        symtab[i].data->dbc = 900;		// that's enuf
	s = DB_Set(global, (cstring *) &(symtab[i].data->dbc)); // try again
	symtab[i].data->dbc = j;		// restore this
      }
    }						// end if data exists
    depPtr = symtab[i].data->deplnk;		// get first dependent
    while (depPtr != ST_DEPEND_NULL)		// while dependents exist
    { partab.src_var.slen = depPtr->keylen;	// init subscript length
      bcopy(depPtr->bytes, partab.src_var.key, depPtr->keylen); // init key
      cdata = (cstring *) &dump[0];		// get it into a cstring
      s = UTIL_String_Mvar(&partab.src_var, cdata->buf, 255);
      cdata->len = s;
      j = (int) depPtr->keylen;			// find key length
      if ((j&1)!=0) j++;			// up it to next even boudary
      bcopy(gks, global->key, gslen);		// restore initial key
      global->slen  = gslen;			// restore initial length
      global->nsubs = gnsubs;
      s = UTIL_Key_BuildEx(global, cdata, &global->key[gslen]);
      if (s < 0) return s;                      // got an error
      global->slen = global->slen + s;          // set up global key
      s = DB_Set(global, (cstring *) &depPtr->bytes[j]); // try to set it
      if (s < 0) return s;

      depPtr = depPtr->deplnk;			// get next if any
    }						// end while dependents exist
  }						// end for all symtab entries
  return 0;					// finished successfully
}						// end function DumpV

//****************************************************************************
short ST_KillAll(int count, var_u *keep)
{ return ST_KillAllEx(count, keep, KILL_ALL);
}

short ST_KillAllEx(int count, var_u *keep, int what)
// kill all local variables except those whose names appear in var_u *keep
{ 
  int i;					// generic counter
  int j;					// generic counter

  partab.src_var.uci = UCI_IS_LOCALVAR; 	// init uci as LOCAL
  partab.src_var.slen = 0;			// init subscript length
  partab.src_var.volset = 0;			// init volume set
  for (i = 0; i <= ST_MAX-1; i++)		// for each entry in ST
  { if ((symtab[i].varnam.var_cu[0] == '$') ||
        (symtab[i].varnam.var_cu[0] == '\0'))
      continue;					// dont touch $ vars
    if (symtab[i].data == ST_DATA_NULL) continue; // ditto if it's undefined
    for (j = 0; j < count; j++)			// scan the keep list
      //if (symtab[i].varnam.var_qu == keep[j].var_qu) // if we want it
      if (X_EQ(symtab[i].varnam.var_xu, keep[j].var_xu)) // if we want it
	break;					// quit the loop
    if (j < count) continue;			// continue if we want it
    //partab.src_var.name.var_qu = symtab[i].varnam.var_qu; // init varnam
    partab.src_var.name.var_xu = symtab[i].varnam.var_xu; // init varnam
    ST_Kill(&partab.src_var);			// kill it and all under
  }						// end for all in symtab
  return 0;					// finished OK
}						// end ST_KillAll

//***************************************************************************
//short ST_SymAtt(chr_q var)
short ST_SymAtt(chr_x *var)
// Locate variable 'var' - create the symtab entry if it doesn't exist.
// Increment usage.
// If ST_data block does not exits, create it.
// Return the symtab entry number or negative error number
{ short pos;					// position in symtab
  pos = ST_Create(var);				// locate/create variable
  if (pos >= 0) symtab[pos].usage++;		// if ok, increment usage
  return pos;					// return whatever we found
}

//***************************************************************************
void ST_SymDet(int count, short *list)
// For each symtab entry in list (ignoring those that are -1),
// decrement usage.  Remove the entry if it is undefined.
{ int i;					// a handy int
  for (i = 0; i < count; i++)			// for all supplied vars
  { if (list[i] >= 0)				// if this got attached
    { symtab[list[i]].usage--; 			// decrement usage
      if (symtab[list[i]].usage > 0)
      { continue;				// still NEWed or whatever
      }
      if (symtab[list[i]].data != NULL)		// data?
      { if (symtab[list[i]].data->dbc != VAR_UNDEFINED)
	{ continue;
	}
	if (symtab[list[i]].data->deplnk != NULL)
	{ continue;
	}
	dlfree(symtab[list[i]].data);		// free the data block
	symtab[list[i]].data = NULL;		// and remember this
      }
      if (symtab[list[i]].data == NULL)		// no data?
      //{ ST_Free(symtab[list[i]].var_q);	// not in use - free it
      { ST_Free(&symtab[list[i]].var_x);	// not in use - free it
      }
    }
  }
  dlfree(list);					// dump the memory
  return;
}						// end function ST_SymDet

//***************************************************************************
short ST_SymGet(short syment, u_char *buf)
// get local data - symtab entry number provided
{ if (symtab[syment].data == ST_DATA_NULL)	// if no data (undefined)
  { return -ERRM6;				// complain
  }
  if (symtab[syment].data->dbc == VAR_UNDEFINED)
  { return -ERRM6;				// complain
  }
  return mcopy(symtab[syment].data->data, buf, 	// go to the data and
               symtab[syment].data->dbc);	// copy data (if any)
}						// end function ST_SymGet

//***************************************************************************
short ST_SymSet(short pos, cstring *data)
// set local data - symtab entry number provided
{ int i;					// a handy int
  ST_data *ptr;					// and a pointer

  i = DTBLKSIZE + data->len + 1;		// size required
  if (i < DTMINSIZ) i = DTMINSIZ;		// check for minimum
  if (symtab[pos].data == ST_DATA_NULL)		// if no data block
  { symtab[pos].data = dlmalloc(i);		// get some memory
    if (symtab[pos].data == ST_DATA_NULL) return -(ERRZ56+ERRMLAST); // no mem
    symtab[pos].data->deplnk = ST_DEPEND_NULL;	// init dep link
    symtab[pos].data->attach = 1;		// init attach count
  }						// end if no data block defined
  else if (symtab[pos].data->dbc < data->len)	// enough space?
  { ptr = dlrealloc(symtab[pos].data, i);	// attempt to increase it
    if (ptr == NULL)
    { return -(ERRZ56+ERRMLAST); // no mem
    }
    if ((ptr != symtab[pos].data) &&		// did it move?
	(ptr->attach > 1))			// and many attached
    { FixData(symtab[pos].data, ptr, ptr->attach); // fix it
    }
    else
    { symtab[pos].data = ptr;			// or just do this one
    }
  }
  symtab[pos].data->dbc = data->len;		// set the dbc
  bcopy(&data->buf[0], &symtab[pos].data->data, data->len + 1); // set it
  return 0;					// and exit
}						// end function ST_SymSet

//***************************************************************************
short ST_SymKill(short pos)
// kill a local var - symtab entry number provided
{ ST_depend *dptr;				// dependant ptr
  ST_depend *fptr;				// dependant ptr

  if (symtab[pos].data != ST_DATA_NULL)		// there is data
  { dptr = symtab[pos].data->deplnk;		// get dependant ptr
    symtab[pos].data->deplnk = ST_DEPEND_NULL;	// clear it
    while (dptr != ST_DEPEND_NULL)		// for each dependant
    { fptr = dptr;				// save this one
      dptr = fptr->deplnk;			// get next
      dlfree(fptr);				// free this
    }
    if (symtab[pos].data->attach < 2)		// if no more attached
    { dlfree(symtab[pos].data);			// free data block
      symtab[pos].data = ST_DATA_NULL;		// clear the pointer
    }
  }
  if (symtab[pos].usage < 1)			// any NEWs etc?
  //{ ST_Free(symtab[pos].var_q);		// no - dong it
  { ST_Free(&symtab[pos].var_x);		// no - dong it
  }
  return 0;					// and exit
}
