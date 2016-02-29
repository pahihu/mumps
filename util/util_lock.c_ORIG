// File: mumps/util/util_lock.c
//
// module database - Lock Utilities

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
 
// 10 July 2009 - minor fix to Dump_lt() routine - rdn


#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <string.h>                             // for memcpy/memcmp
#include <strings.h>
#include <sys/types.h>                          // for u_char def
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"                              // errors
#include "compile.h"                            // for rbd definition
#include <unistd.h>                             // for sleep
#include <time.h>                               // for time functions
#include <sys/ipc.h>                            // semaphore stuff
#include <sys/sem.h>                            // semaphore stuff

//****************************************************************
short UTIL_String_Lock( locktab *var,         	// address of lock entry
                        u_char *str)            // locn of dest string
{ int i;                                        // for loops
  int p = 0;                                    // string pointer
  int slen;                                     // subscript length
  u_char save;                                  // stuffing with struct
  uci_tab up;                                   // ptr to uci tab
  u_char *vp;                                   // ptr to vol name

  if (var->uci != UCI_IS_LOCALVAR)              // if it's a global var
  { str[p++] = '^';                             // lead off with the caret
    str[p++] = '[';                             // open bracket
    str[p++] = '"';                             // a leading quote
    up = systab->vol[var->vol - 1]->vollab->uci[var->uci - 1]; // uci pointer
    for (i=0; i<8; i++)                         // for each possible character
    { if (up.name.var_cu[i] == '\0') break;     // done if we hit a null
      str[p++] = up.name.var_cu[i];             // copy the character
    }
    str[p++] = '"';                             // a trailing quote
    str[p++] = ',';                             // comma
    str[p++] = '"';                             // start quote for vol
    vp = systab->vol[var->vol - 1]->vollab->volnam.var_cu; // point at name
    for (i=0; i<8; i++)                         // for each possible character
    { if (vp[i] == '\0') break;                 // done if we hit a null
      str[p++] = vp[i];                         // copy the character
    }
    str[p++] = '"';                             // a trailing quote
    str[p++] = ']';                             // closing bracket
  }                                             // end global specific stuf

  for (i = 0; i < 8; i++)                       // now the name
  { if (var->name.var_cu[i] == '\0') break;     // quit when done
    str[p++] = var->name.var_cu[i];             // copy a byte
  }

  slen = var->byte_count - sizeof(var_u) - (2 * sizeof(u_char)); // subs len

  if (slen != 0)                                // if there are subscripts
  { save = var->name.var_cu[7];                 // save that value
    var->name.var_cu[7] = (u_char) slen;        // put len there for call
    i = UTIL_String_Key(&var->name.var_cu[7], &str[p], 255); //do the subscripts
    var->name.var_cu[7] = save;                 // restore saved value
    if (i < 0) return i;                        // quit on error
    p = p + i;                                  // add to length
  }
  str[p] = '\0';                                // null terminate
  return (short) p;                             // return the length
}

//****************************************************************

short UTIL_mvartolock( mvar *var, u_char *buf)	// convert mvar to string
{ int i;                                        // a handy int
  chr_q *vt;                                    // var table pointer
  rbd *p;                                       // a handy pointer

  if (var->uci == UCI_IS_LOCALVAR)              // if local
  { if (var->volset)                            // if index type
    { p = (rbd *) (partab.jobtab->dostk[partab.jobtab->cur_do].routine);
      vt = (chr_q *) (((u_char *) p) + p->var_tbl); // point at var table
      *((chr_q *) &buf[2]) = vt[var->volset - 1]; // get the var name
    }                                           // end index type
    else                                        // non index type
    { *((chr_q *) &buf[2]) = var->name.var_qu;	// copy name
    }
    bcopy(&var->key[0], &buf[2 + sizeof(chr_q)], var->slen); // copy key
    i = var->slen + sizeof(chr_q) + 2;          // how big it is
    buf[1] = UCI_IS_LOCALVAR;                   // setup as local
    buf[0] = 0;                                 // no vol set
    buf[i] = '\0';                              // ensure null terminated
    return (short) i;                           // return the length
  }                                             // end of local var code

  if (var->name.var_cu[0] == '$')               // ssvn?
  { i = SS_Norm(var);                           // normalize it
    if (i < 0) return (short)i;                 // quit on error
  }
  buf[0] = var->volset;                         // copy volset
  buf[1] = var->uci;                            // copy uci
  *((chr_q *) &buf[2]) = var->name.var_qu;      // copy varname
  if (!buf[0])                                  // if no volset
  { buf[0] = partab.jobtab->lvol;               // default
  }
  if (!buf[1])                                  // if no uci
  { if (var->name.var_cu[0] == '%')             // if % var
    { buf[1] = 1;                               // MGR
    }
    else
    { buf[1] = partab.jobtab->luci;             // default
    }
  }
  if ((var->volset == 0) && (var->uci == 0))	// no vol or uci
  { for (i = 0; i < systab->max_tt; i++)        // scan trantab
    { if ((buf[0] == systab->tt[i].from_vol) &&
	  (buf[1] == systab->tt[i].from_uci) &&
	  (var->name.var_qu == systab->tt[i].from_global.var_qu))
      { buf[0] = systab->tt[i].to_vol;          // copy this
	buf[1] = systab->tt[i].to_uci;              // and this
	*((chr_q *) &buf[2]) = systab->tt[i].to_global.var_qu; // and this
	break;                                      // done
      }
    }                                           // end found one
  }                                             // end trantab lookup
  bcopy(&var->key[0], &buf[sizeof(chr_q) + 2], var->slen); // copy key
  i = var->slen + sizeof(chr_q) + 2;            // how big it is
  buf[i] = '\0';                                // ensure null terminated
  return (short) i;                             // return the length
}

//****************************************************************

short LCK_Combine(locktab *ptr)
{ locktab *next;                                // a handy pointer

  if (ptr == NULL) panic("Null pointer passed to LCK_Combine()");

  while (TRUE)
  { next = (locktab *) (((u_char *) (ptr)) + ptr->size); // where next
    if (((char *) next) >= 
        (((char *) systab->lockstart) + systab->locksize))
      break;                                    // quit when done
    if (next != ptr->fwd_link) break;           // quit if next not free
    if (next->job > -1) panic("Attempt to combine non-free in LCK_Combine()");
    ptr->size += ptr->fwd_link->size;           // increment the new free space
    ptr->fwd_link = next->fwd_link;             // new block now points correct
  }                                             // prev block/this block merge
  return 0;                                     // finished OK
}                                               // end function LCK_Combine()

//****************************************************************
// Note, ptr has been removed from the lockhead list when we get here
//
short LCK_Free(locktab *ptr)
{ 
  locktab *currptr;
  locktab *prevptr = NULL;                      // handy ptr

  if (ptr == NULL)
    panic("Null pointer passed to LCK_Free()");	// die

  currptr = systab->lockfree;                   // start here
  if (ptr < currptr)                            // freeing at start of mem sect
  { ptr->fwd_link = currptr;                    // this block becomes new
    systab->lockfree = ptr;                     // head of free list
    return LCK_Combine(systab->lockfree);       // see if we can combine the 2
  }

  while ((currptr != NULL) && (ptr > currptr)) // while more to look at
  { prevptr = currptr;                          // save current to prev
    currptr = currptr->fwd_link;                // move on
  }
  ptr->fwd_link = currptr;                      // point at next or NULL
  prevptr->fwd_link = ptr;                      // link us in here
  return LCK_Combine(prevptr);                  // see if we can combine the 2
}

//****************************************************************

locktab *LCK_Insert(int size)	// go thru share section for chunk of size
                                // returns ptr to free space
{ locktab *free_curr = NULL;                    // locktab traverse ptr
  locktab *free_prev = NULL;                    // prev locktab ptr
  locktab *ptr = NULL;                          // pointer to free spc
  locktab *p = NULL;                            // handy ptr
  locktab *prevptr = NULL;                      // another handy ptr
  int ret = systab->locksize + 1;               // size comparitor
  free_curr = systab->lockfree;                 // start here

  while (free_curr != NULL)                     // while more locktabs
  {
    if ((free_curr->size < ret) && (free_curr->size > size)) // if this space
    { ptr = free_curr;                          // more appropriate
      prevptr = free_prev;                      // save to previous
      ret = free_curr->size;                    // than last space, use
    }                                           // this one instead
    free_prev = free_curr;                      // save prev ptr
    free_curr = free_curr->fwd_link;            // check next one
  }                                             // end while
  if ((ptr == NULL) || (ret == (systab->locksize + 1))) // if neither has chgd
  { 	return NULL;                            // no space available
  }                                             // and try again
  else                                          // if both changed
  {	// **** the 64 on the next line will be # defined shortly
    if ((size + 64) < ptr->size)                // if way too big
    { p = (locktab *) ((u_char *) ptr + size);	// setup for new block
      p->fwd_link = ptr->fwd_link;              // point it at same
      p->size = ptr->size - size;               // init the size
      p->job = -1;                              // mark as free
      p->lock_count = 0;                        // initialise
      p->byte_count = 0;                        // these two also
      if (prevptr == NULL) systab->lockfree = p; // new head of freelist
      else prevptr->fwd_link = p;               // else link as usual

      ptr->size = size;                         // set this NOW
    }                                           // end if way too big
    else                                        // size just right
    { if (prevptr == NULL) systab->lockfree = ptr->fwd_link; // link here
      else prevptr->fwd_link = ptr->fwd_link;	// or here
    }                                           // end else size OK
    return ptr;                                 // tell them where
  }                                             // end else get some space
  return NULL;                                  // finished NO SPACE
}                                               // end LCK_Insert

//****************************************************************


short LCK_Order(cstring *ent, u_char *buf, int dir) // get next/prev entry
{ locktab *lptr;                                // locktab entry we are doing
  locktab *plptr;                               // previous locktab entry
  int i;                                        // a handy int
  short s;                                      // for functions
  short x;                                      // for SEM's

  x = SemOp(SEM_LOCK, -1);                      // read lock SEM_LOCK
  if (x < 0) return x;                          // return error

  lptr = systab->lockhead;                      // get the list head
  plptr = NULL;                                 // init previous ptr
  while (lptr != NULL)                          // while more locktabs
  { i = ent->len;                               // length of entry
    if (i > lptr->byte_count) i = lptr->byte_count; // but if locktab is less
    s = memcmp(ent->buf, &lptr->vol, i);        // compare them
    if ((s == 0) && (lptr->byte_count != ent->len)) // if first part the same
    { s = 1;                                    // assume passed in greater
      if (lptr->byte_count > ent->len) s = -1;	// no - other way
    }                                           // end if first part the same
    if ((dir > 0) && (s < 0)) break;            // found in fwd direction
    if ((dir < 0) && (s <= 0))                  // found in the back direction
    { lptr = plptr;                             // point at previous
      break;                                    // and quit
    }                                           // end if found in back dir
    plptr = lptr;                               // remember that one
    lptr = lptr->fwd_link;                      // get the next one
  }                                             // end while more locktabs
  if ((dir < 0) && (lptr == NULL)) lptr = plptr; // adjust for last on ,-1)
  buf[0] = '\0';                                // null terminate
  s = 0;                                        // return value
  if (lptr != NULL)                             // got something ?
    s = UTIL_String_Lock(lptr, buf);            // convert it
  x = SemOp(SEM_LOCK, 1);                       // release SEM_LOCK
  return s;                                     // and return
}

//****************************************************************

short LCK_Get(cstring *ent, u_char *buf)        // get job#,lock_count
{ locktab *lptr;                                // locktab entry we are doing
  int i;                                        // a handy int
  short s = 0;                                  // return value
  short x;                                      // for SEM's
  buf[0] = '\0';                                // JIC

  x = SemOp(SEM_LOCK, -1);                      // read lock SEM_LOCK
  if (x < 0) return x;                          // return the error

  lptr = systab->lockhead;                      // init current locktab pointer
  i = ent->len;                                 // get length of supplied entry
  while (lptr != NULL)                          // while more lock tabs
  { if ((i == lptr->byte_count)	&&              // if bytes counts match
        (bcmp(ent->buf, &lptr->vol, i) == 0))	// is there a match ?
    { s = itocstring(buf, lptr->job);           // cvt job to str
      buf[s++] = ',';                           // copy in comma
      s = s + itocstring(&buf[s], lptr->lock_count); // cvt count to str
      break;                                    // found it
    }                                           // end if exact match
    lptr = lptr->fwd_link;                      // get next
  }                                             // end while more lock tabs
  x = SemOp(SEM_LOCK, 1);                       // release SEM_LOCK
  return s;                                     // return the count
}                                               // end function LCK_Get()

//****************************************************************

short LCK_Kill(cstring *ent)                    // remove an entry
{
 locktab *lptr;                                 // locktab entry we are doing
  locktab *plptr;                               // previous locktab entry
  int i;                                        // a handy int

  lptr = systab->lockhead;                      // init current locktab pointer
  plptr = NULL;                                 // init prev locktab pointer
  i = ent->len;                                 // get length of supplied entry
  while (lptr != NULL)                          // while more lock tabs
  { if (i == lptr->byte_count)                  // if bytes counts match
    { if (bcmp(ent->buf, &lptr->vol, i) == 0)	// is there a match ?
      { if (plptr == NULL)                      // killing head of lock tabs
        { systab->lockhead = lptr->fwd_link;	// bypass it
	  lptr->job = -1;                           // flag it as free
          LCK_Free(lptr);                       // add to the free list
          break;                                // finished OK
        }                                       // end if killing head of locks
        if ((plptr != NULL) && (lptr != NULL))	// killing mid list
        { plptr->fwd_link = lptr->fwd_link;     // bypass it
          lptr->job = -1;                       // flag it as free
          LCK_Free(lptr);                       // add to the free list
          break;                                // finished OK
        }                                       // end killing mid list
      }                                         // end if match
    }                                           // end if byte counts match

    plptr = lptr;                               // make current, previous
    lptr = lptr->fwd_link;                      // check next locktab
  }                                             // end while more lock tabs

  return 0;                                     // finished OK
}                                               // end function LCK_Kill()

//****************************************************************

void LCK_Remove(int job)                        // remove all locks for a job
{
  locktab *lptr;                                // locktab entry we are doing
  locktab *plptr;                               // previous locktab entry
  short x;                                      // for SEM's

  if (!job) job = partab.jobtab - systab->jobtab + 1; // current job

  x = SemOp(SEM_LOCK, -systab->maxjob);         // write lock SEM_LOCK
  if (x < 0) return;                            // return on error

  lptr = systab->lockhead;                      // init current locktab pointer
  plptr = NULL;                                 // init prev locktab pointer
  while (lptr != NULL)                          // while more lock tabs
  { if (lptr->job == job)                       // if we own it
    { if (plptr == NULL)                        // remove top node
      { systab->lockhead = lptr->fwd_link;      // link in new head lock node
        lptr->job = -1;                         // flag it as free
        LCK_Free(lptr);                         // add to the free list
        lptr = systab->lockhead;                // point at next
        plptr = NULL;                           // prev ptr still NULL
      }                                         // end if removing top node
      if ((plptr != NULL) && (lptr != NULL))	// if both ptrs defined
      { plptr->fwd_link = lptr->fwd_link;       // bypass it
        lptr->job = -1;                         // flag it as free
        LCK_Free(lptr);                         // add to the free list
        lptr = plptr->fwd_link;                 // point at next
      }                                         // end if both pointers defined
    }                                           // end if job numbers match
    else                                        // byte counts don't match
    { plptr = lptr;                             // make current, previous
      lptr = lptr->fwd_link;                    // check next locktab
    }                                           // end else job numbers !=
  }                                             // end while more lock tabs
  x = SemOp(SEM_LOCK, systab->maxjob);          // unlock SEM_LOCK
  return;                                       // return
}                                               // end function LCK_Remove()

//****************************************************************

short LCK_Old(int count, cstring *list, int to)	// old style lock
{
  LCK_Remove(0);                                // remove all locks for job

  if (partab.jobtab->trap & (SIG_CC | SIG_QUIT | SIG_TERM | SIG_STOP)) // quit
    return -(ERRMLAST+ERRZ51);

  if (count < 1) return 0;                      // just return if none
  return LCK_Add(count, list, to);              // add all locks in list
}                                               // end function LCK_Old()

//****************************************************************

short LCK_Add(int count, cstring *list, int to) // lock plus
{
  cstring *current = list;                      // temp cstring
  cstring *tempc;                               // temp cstring
  int pos = 0;                                  // position indicator
  int size = 0;                                 // size of entry count
  int done = 0;                                 // done counter
  int removedone = 0;                           // flag
  int posr = 0;                                 // position in *list
  int toremove = 0;                             // number of vars to remove
  int tryagain = 1;                             // retry flag
  locktab *lptr;                                // locktab we are working on
  locktab *plptr;                               // previous locktab
  locktab *nlptr;                               // new locktab
  time_t strttime;                              // timeout var
  time_t currtime;                              // timeout var
  int i;                                        // handy int
  int reqd;                                     // space reqd for lock
  short x;                                      // for SEM's

  int failed()                                  // common code
  { done = count + 1;                           // begin again from scratch

    if (to == 0)                                // if no timeout
        tryagain = 0;                           // flag as if timeout expired

    if (to > 0)                                 // if timeout value specified
    { currtime = time((time_t *) NULL);         // get current time
      if (strttime + to < currtime) tryagain = 0; // flag if time expired
    }                                           // end if timeout specified

    if (tryagain == 1)
    { x = SemOp(SEM_LOCK, systab->maxjob);      // unlock SEM_LOCK
      sleep(1);
    }
        
    if (tryagain == 0)
        partab.jobtab->test = 0;                // flag failure to lock

    if (partab.jobtab->attention)
    { if (partab.jobtab->trap & (SIG_CC | SIG_QUIT | SIG_TERM | SIG_STOP))
      { if (tryagain == 0)
          x = SemOp(SEM_LOCK, systab->maxjob);	// unlock SEM_LOCK
        return -(ERRMLAST+ERRZ51);
      }
    }
    lptr = NULL;
    return 0;
  }

 strttime = time((time_t *) NULL);              // save op start time
 while (tryagain)                               // while we should give it a go
 {tryagain = 0;                                 // reset retry flag
  if (to > -1)
    partab.jobtab->test = 1;                    // flag successful locking
  done = 0;                                     // init
  size = 0;                                     // these
  pos = 0;                                      // now
  x = SemOp(SEM_LOCK, -systab->maxjob);         // write lock SEM_LOCK
  if (x < 0) return x;                          // return the error
       
  while ((done < count) && (tryagain == 0))     // while more to do
  { current = (cstring *) &((u_char *)list)[pos]; // extract this entry
    reqd = sizeof(short)*3 + sizeof(int) + sizeof(locktab *) + current->len;
    lptr = systab->lockhead;                    // start at first locktab
    plptr = NULL;                               // init previous pointer
    if (lptr == NULL)                           // add first lock
    { systab->lockfree->fwd_link = NULL;        // make sure JIC
      nlptr = LCK_Insert(reqd);                 // try and get some space
      if (nlptr == NULL)
      { tryagain = 1;                           // set retry flag
        toremove = done;                        // save this value
        removedone = 0;                         // init this counter
        posr = 0;                               // init position indicator
        size = 0;                               // reset size for safety
        while (removedone < toremove)           // while more removals to do
        { LCK_Kill((cstring *) &((u_char *)list)[posr]); // rem entry at posr
          tempc = (cstring *) &((u_char *)list)[posr]; // extract this entry
          size = sizeof(short) + tempc->len + sizeof(char); // find size
          if (size & 1) size++;                 // ensure even
          posr += size;                         // posr to start of next entry
          removedone++;                         // increment removal counter
        }                                       // end while when no more to do

        if (failed()) return -(ERRMLAST+ERRZ51);
      }
      else
      {
        nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // init job number
        nlptr->lock_count = 1;                  // init lock count
        nlptr->byte_count = current->len;       // init data length
        bcopy(current->buf, &nlptr->vol, current->len); // copy in data
        nlptr->fwd_link = systab->lockhead;     // link it in sorted order
        systab->lockhead = nlptr;               // link it in sorted order
      }
    }                                           // end if first lock
    while (lptr != NULL)                        // while more locktabs to see
    { i = current->len;                         // get the length of this one
      if (lptr->byte_count < i) i = lptr->byte_count; // find shortest length
      while ((lptr != NULL) &&                  // while more to look
             (memcmp(current->buf, &lptr->vol, i) > 0)) // until found or past
      { plptr = lptr;                           // save current to previous
        lptr = lptr->fwd_link;                  // get next locktab
        if (lptr == NULL) break;                // run out of locktabs
        i = current->len;                       // get length of this one
        if (lptr->byte_count < i) i = lptr->byte_count; // find shortest length
      }                                         // end while more/found/past
      if ((lptr != NULL) && (memcmp(current->buf, &lptr->vol, i) == 0))
      {                                         // exists as sub/exact/superset
        if (lptr->job == (partab.jobtab - systab->jobtab) + 1) // we MUST own
        { while ((lptr != NULL) &&              // more to look at
                 (memcmp(current->buf, &lptr->vol, i) == 0) && // still matches
                 (lptr->byte_count < current->len) // length is less
                )
          { plptr = lptr;
            lptr = lptr->fwd_link;              // have a look at next one
            if (lptr == NULL) break;            // run out of locktabs
            i = current->len;                   // get length
            if (lptr->byte_count < i) i = lptr->byte_count; // get shortest
          }                                     // end while conditions true
          if (lptr != NULL)                     // if lptr defined
          { if (memcmp(current->buf, &lptr->vol, i) == 0) // if data matches
            { if (lptr->byte_count == current->len) // if exact length match
              { lptr->lock_count++;             // increment the lock count
                lptr = NULL;                    // NULLIFY the ptr, finished
              }                                 // end if exact match
              else                              // gone past it
              { nlptr = LCK_Insert(reqd);       // get some space
                if (nlptr == NULL)
                { tryagain = 1;                 // set retry flag
                  toremove = done;              // save this value
                  removedone = 0;               // init this counter
                  posr = 0;                     // init position indicator
                  size = 0;                     // reset size for safety
                  while (removedone < toremove)	// while more removals to do
                  { LCK_Kill((cstring *) &((u_char *)list)[posr]); // rem entry
                    tempc = (cstring *) &((u_char *)list)[posr]; // extract
                    size = sizeof(short) + tempc->len + sizeof(char); // size
                    if (size & 1) size++;       // ensure even
                    posr += size;               // posr to start of next entry
                    removedone++;               // increment removal counter
                  }                             // end while when no more to do
                  if (failed()) return -(ERRMLAST+ERRZ51);
                }
                else
                {
                  nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // job#
                  nlptr->lock_count = 1;		// init lock count
                  nlptr->byte_count = current->len; // length of data
                  bcopy(current->buf, &nlptr->vol, current->len); // copy data
                  if (plptr != NULL)            // not inserting at list head
                    plptr->fwd_link = nlptr;	// link it in this way
                  else				// inserting at list head
                    systab->lockhead = nlptr;	// so link it in this way
                  nlptr->fwd_link = lptr;		// link it in
                  lptr = NULL;                  // NULLIFY ptr, finished entry
                }
              }                                 // end else gone past it
            }                                   // end if match to shorter len
            else                                // memcmp no longer == 0
            { nlptr = LCK_Insert(reqd);         // get some space
              if (nlptr == NULL)
              { tryagain = 1;                   // set retry flag
                toremove = done;                // save this value
                removedone = 0;                 // init this counter
                posr = 0;                       // init position indicator
                size = 0;                       // reset size for safety
                while (removedone < toremove)	// while more removals to do
                { LCK_Kill((cstring *) &((u_char *)list)[posr]); // rem entry at posr
                  tempc = (cstring *) &((u_char *)list)[posr]; // extract this entry
                  size = sizeof(short) + tempc->len + sizeof(char); // find size
                  if (size & 1) size++;         // ensure even
                  posr += size;                 // posr to start of next entry
                  removedone++;                 // increment removal counter
                }                               // end while when no more to do
                if (failed()) return -(ERRMLAST+ERRZ51);
              }
              else
              {
                nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // init job
                nlptr->lock_count = 1;          // init lock count
                nlptr->byte_count = current->len; // length of data
                bcopy(current->buf, &nlptr->vol, current->len); // copy data
                plptr->fwd_link = nlptr;		// link it in
                nlptr->fwd_link = lptr;         // link it in
                lptr = NULL;                    // NULLIFY ptr, finished entry
              }
            }                                   // end else memcmp no longer 0
          }                                     // end if lptr defined
          else
          {
            nlptr = LCK_Insert(reqd);           // get some space
            if (nlptr == NULL)
            { tryagain = 1;                     // set retry flag
              toremove = done;                  // save this value
              removedone = 0;                   // init this counter
              posr = 0;                         // init position indicator
              size = 0;                         // reset size for safety
              while (removedone < toremove)		// while more removals to do
              { LCK_Kill((cstring *) &((u_char *)list)[posr]); // rem entry at posr
                tempc = (cstring *) &((u_char *)list)[posr]; // extract this entry
                size = sizeof(short) + tempc->len + sizeof(char); // find size
                if (size & 1) size++;           // ensure even
                posr += size;                   // posr to start of next entry
                removedone++;                   // increment removal counter
              }                                 // end while when no more to do
              if (failed()) return -(ERRMLAST+ERRZ51);
            }
            else
            {
              nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // job no.
              nlptr->lock_count = 1;            // init lock count
              nlptr->byte_count = current->len; // length of data
              bcopy(current->buf, &nlptr->vol, current->len); // copy data
              plptr->fwd_link = nlptr;          // link it in
              nlptr->fwd_link = lptr;           // link it in
              lptr = NULL;                      // NULLIFY ptr, finished entry
            }
          }                                     // end else, lptr not defined
        }                                       // end if we own it
        else                                    // we dont own it
        {                                       // remove all locks thus far
          tryagain = 1;                         // set retry flag
          toremove = done;                      // save this value
          removedone = 0;                       // init this counter
          posr = 0;                             // init position indicator
          size = 0;                             // reset size for safety
          while (removedone < toremove)         // while more removals to do
          { LCK_Kill((cstring *) &((u_char *)list)[posr]); // rem entry at posr
            tempc = (cstring *) &((u_char *)list)[posr]; // extract this entry
            size = sizeof(short) + tempc->len + sizeof(char); // find size
            if (size & 1) size++;               // ensure even
            posr += size;                       // posr to start of next entry
            removedone++;                       // increment removal counter
          }                                     // end while when no more to do
          if (failed()) return -(ERRMLAST+ERRZ51);
        }                                       // end else we dont own it
      }                                         // end if exists in super/sub
      else                                      // doesn't exist
      { if ((lptr != NULL) && (memcmp(current->buf, &lptr->vol, i) < 0))
        { nlptr = LCK_Insert(reqd);             // try get some space
          if (nlptr == NULL)
          { tryagain = 1;                       // set retry flag
            toremove = done;                    // save this value
            removedone = 0;                     // init this counter
            posr = 0;                           // init position indicator
            size = 0;                           // reset size for safety
            while (removedone < toremove)		// while more removals to do
            { LCK_Kill((cstring *) &((u_char *)list)[posr]); // rem entry at posr
              tempc = (cstring *) &((u_char *)list)[posr]; // extract this entry
              size = sizeof(short) + tempc->len + sizeof(char); // find size
              if (size & 1) size++;             // ensure even
              posr += size;                     // posr to start of next entry
              removedone++;                     // increment removal counter
            }                                   // end while when no more to do
            if (failed()) return -(ERRMLAST+ERRZ51);
          }
          else
          {
            nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // init job no.
            nlptr->lock_count = 1;              // init lock count
            nlptr->byte_count = current->len;	// length of data
            bcopy(current->buf, &nlptr->vol, current->len); // copy the data
            nlptr->fwd_link = lptr;             // link it in
            lptr = NULL;                        // NULLIFY ptr, finished entry
            if (plptr == NULL)                  // insert as new top node
            { systab->lockhead = nlptr;         // link this way
            }                                   // end if insert as new top node
            else                                // insert mid list
            { plptr->fwd_link = nlptr;          // or link this way
            }                                   // end else insert mid list
          }
        }                                       // end if match result greater
        else                                    // add to end of list 
        { if (lptr == NULL)                     // run out of locktabs
          { nlptr = LCK_Insert(reqd);           // get some space
            if (nlptr == NULL)
            { tryagain = 1;                     // set retry flag
              toremove = done;                  // save this value
              removedone = 0;                   // init this counter
              posr = 0;                         // init position indicator
              size = 0;                         // reset size for safety
              while (removedone < toremove)		// while more removals to do
              { LCK_Kill((cstring *) &((u_char *)list)[posr]); // rem entry at posr
                tempc = (cstring *) &((u_char *)list)[posr]; // extract this entry
                size = sizeof(short) + tempc->len + sizeof(char); // find size
                if (size & 1) size++;           // ensure even
                posr += size;                   // posr to start of next entry
                removedone++;                   // increment removal counter
              }                                 // end while when no more to do
              if (failed()) return -(ERRMLAST+ERRZ51);
            }
            else
            {
              nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // init job
              nlptr->lock_count = 1;            // init lock count
              nlptr->byte_count = current->len;	// length of data
              bcopy(current->buf, &nlptr->vol, current->len); // copy data
              nlptr->fwd_link = lptr;           // link it in
              lptr = NULL;                      // NULLIFY ptr, finished entry
              if (plptr == NULL)                // insert as new top node
              { systab->lockhead = nlptr;		// link it in this way
              }                                 // end if new top node
              else                              // insert mid list
              { plptr->fwd_link = nlptr;		// or link it in this way
              }                                 // end else insert mid list
            }
          }                                     // end if run out of locktabs
        }                                       // end else add to end of list
      }                                         // end else not already in
    }                                           // end while lptr not null
    if (tryagain == 0)
    { size = sizeof(short) + current->len + sizeof(char); // calc len of entry
      if (size & 1) size += 1;                  // pad to even boundary
      pos = pos + size;                         // find next start pos
      done++;                                   // number done + 1
    }
  }                                             // end while more to do
 }                                              // end while try again
    
 x = SemOp(SEM_LOCK, systab->maxjob);           // unlock SEM_LOCK
      
 return 0;                                      // finished OK
}                                               // end function LCK_Add()
             
//****************************************************************

short LCK_Sub(int count, cstring *list)         // lock minus
{
  int r = -1;                                   // return value
  int i;                                        // a handy int
  int pos = 0;                                  // position in *list
  int size;                                     // size of entry in *list
  int done = 0;                                 // number of entries completed
  cstring *current;                             // temp cstring
  locktab *lptr;                                // locktab pointer
  locktab *plptr;                               // previous locktab pointer
  short x;                                      // for SEM's

  x = SemOp(SEM_LOCK, -systab->maxjob);         // write lock SEM_LOCK
  if (x < 0) return x;                          // return the error

  while (done < count)                          // while more to do
  { 
    current = (cstring *) &((u_char *)list)[pos]; // extract this entry
    lptr = systab->lockhead;                    // start at first locktab
    plptr = NULL;                               // init previous pointer
    while (lptr != NULL)                        // while more locktabs to see
    { i = current->len;                         // get the length of this one
      if (lptr->byte_count < i) i = lptr->byte_count; // find shortest length
      while ((lptr != NULL) &&                  // while more locktabs do
             (memcmp(current->buf, &lptr->vol, i) > 0)) // until found or past
      { plptr = lptr;                           // save current to previous
        lptr = lptr->fwd_link;                  // get next locktab
        if (lptr == NULL) break;                // if we run out, stop
        i = current->len;                       // get length of this one
        if (lptr->byte_count < i) i = lptr->byte_count; // find shortest length
      }                                         // end while not found/past
      if ((lptr != NULL) && (memcmp(current->buf, &lptr->vol, i) == 0))
      {                                         // in sub/exact/superset form
        if (lptr->job == ((partab.jobtab - systab->jobtab) + 1)) // we MUST own
        { while ((lptr != NULL) &&              // more to see
                 (memcmp(current->buf, &lptr->vol, i) == 0) && // 1st bit match
                 (lptr->byte_count < current->len) // length is smaller
                )                               // while those things
          { lptr = lptr->fwd_link;              // look at next one
            if (lptr == NULL) break;            // run out of locktabs, stop
            i = current->len;                   // save length
            if (lptr->byte_count < i) i = lptr->byte_count; // get shorter one
          }                                     // end while
          if (lptr != NULL)                     // if lptr still defined
          { if (memcmp(current->buf, &lptr->vol, i) == 0) // first bit matches
            { if (lptr->byte_count == current->len)	// exact match
              { lptr->lock_count--;             // decrement the lockcount
                if (lptr->lock_count <= 0)      // if becomes un-wanted
                { r = LCK_Kill(current);        // Kill it
                  lptr = lptr->fwd_link;        // NULLIFY ptr, finished entry
                }                               // end if kill-able
                else lptr = lptr->fwd_link;     // NULLIFY ptr, finished entry
              }                                 // end if exact match
              else lptr = lptr->fwd_link;       // NULLIFY ptr, finished entry
            }                                   // end if entries match length
            else lptr = lptr->fwd_link;         // NULLIFY ptr, finished entry
          }                                     // end if still point at lock
        }                                       // end if we own it
        else lptr = lptr->fwd_link;             // NULLIFY ptr, finished entry
      }                                         // end if we match to length
      else
      { if (lptr != NULL) lptr = lptr->fwd_link; // NULLIFY ptr, finished entry
      }
    }                                           // end while more locktabs
    size = sizeof(short) + current->len + sizeof(char); // calc length of entry
    if (size & 1) size += 1;                    // pad to even boundary
    pos = pos + size;                           // find next start pos
    done++;                                     // number done + 1
  }                                             // successful
  x = SemOp(SEM_LOCK, systab->maxjob);          // unlock SEM_LOCK
  return 0;                                     // finished OK
}                                               // end function LCK_Sub()

//****************************************************************
void Dump_lt()
{
  locktab *lptr;                                // locktab pointer
  short x;  
  u_char keystr[300];
  u_char workstr[300];

  x = SemOp(SEM_LOCK, -systab->maxjob);         // write lock SEM_LOCK
  if (x < 0) return;                            // return the error


  lptr = (locktab *) systab->lockstart;
  
  printf("Dump of Lockspace starting at %p\r\n", lptr);
  printf("Lock Head starts at %p\r\n", systab->lockhead);
  printf("Lock Free starts at %p\r\n", systab->lockfree);
  printf("Lptr         Fwd_link  Size    Job  Lcnt Bcnt Vol  Uci  Var  Key\r\n");

  while (lptr != NULL)
  { keystr[0] = '\0';
    if ( lptr->byte_count > 10 )
    { workstr[0] = lptr->byte_count-10;
      bcopy( lptr->key, &workstr[1], workstr[0] );
      x = UTIL_String_Key( workstr, keystr, 255 );
      if (x < 0)
      { sprintf( (char *) keystr, " ERROR: %d", x );
      }
    }
    printf("%10p %10p %5d   %4d %4d %4d %4d %4d  %s%s\r\n",
           lptr, lptr->fwd_link,
           lptr->size, lptr->job, lptr->lock_count, lptr->byte_count,
           lptr->vol, lptr->uci, lptr->name.var_cu, keystr);
    lptr = (locktab *) (((u_char *) lptr) + lptr->size);
    if ((u_char *) lptr >= ((u_char *) systab->lockstart) + systab->locksize)
    { break;
    }
  }
  x = SemOp(SEM_LOCK, systab->maxjob);		// unlock SEM_LOCK
  return;					// finished OK
}
