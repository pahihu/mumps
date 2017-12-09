// File: mumps/util/util_routine.c
//
// module MUMPS util_routine - routine functions

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
#include <errno.h>                              // error stuf
#include <string.h>				// for bcopy
#include <strings.h>
#include <time.h>				// to get time_t
#include <unistd.h>				// for sleep
#include <sys/types.h>                          // for u_char def
#include "mumps.h"                              // standard includes
#include "compile.h"				// rbd structures
#include "proto.h"				// the prototypes

// The following is called ONLY from init_start.c
//

void Routine_Init()				// setup rdb for this vol
{ rbd *rou;					// a routine pointer
  int i;					// an int

  for (i = 0; i < RBD_HASH; i++)		// the hash table
    systab->vol[0]->rbd_hash[i] = NULL;		// clear it out

  rou = (rbd *) systab->vol[0]->rbd_head;	// free space entry

  systab->vol[0]->rbd_hash[RBD_HASH] = rou;	// head of free list

  i = (char *) systab->vol[0]->rbd_end - 
      (char *) systab->vol[0]->rbd_head;	// memory available

  rou->fwd_link = NULL;				// no forward link
  rou->chunk_size = i;				// size of this bit of free
  rou->attached = 0;				// nothing attached
  rou->last_access = 0;				// not used
  //rou->rnam.var_qu = 0;			// no routine
  X_Clear(rou->rnam.var_xu);			// no routine
  rou->uci = 0;					// no uci
  rou->vol = 0;					// no vol
  rou->rou_size = 0;				// no routine here

  return;					// done
}

// The following are internal only (first called from $&DEBUG())
//

void Dump_rbd()					// dump rbds
{ int i;					// an int
  rbd *p;					// a pointer
  char tmp[2+MAX_NAME_BYTES];			// some space

  i = SemOp(SEM_ROU, -systab->maxjob);		// write lock the rbds
  if (i < 0) return;				// exit on error
  p = (rbd *) systab->vol[0]->rbd_head;		// get the start
  printf("Dump of all Routine Buffer Descriptors at %ld\r\n", (long) time(0));
  printf("Free at %#10lx\r\n", (u_long)systab->vol[0]->rbd_hash[RBD_HASH]);
  printf("Address        fwd_link       chunk_sz atts last_acces rou_name                         UCI VOL Rsize\r\n");
  tmp[MAX_NAME_BYTES] = '\0';			// null terminate temp
  while (TRUE)					// for all
  { for (i = 0; i < MAX_NAME_BYTES; i++) tmp[i] = ' ';	// space fill tmp[]
    for (i = 0; i < MAX_NAME_BYTES; i++)
    { if (p->rnam.var_cu[i] == 0) break;
      tmp[i] = p->rnam.var_cu[i];
    }
    printf("%#14lx %#14lx %8d %4d %10ld %31s %3d %3d %5d\r\n",
      (u_long)p, (u_long)p->fwd_link,
      p->chunk_size, p->attached, (long) p->last_access,
      tmp, p->uci, p->vol, p->rou_size);
    p = (rbd *) ((u_char *) p + p->chunk_size); // point at next
    if (p >= (rbd *) systab->vol[0]->rbd_end) break; // quit when done
  }
  i = SemOp(SEM_ROU, systab->maxjob);		// release lock
  return;					// and exit
}

//int Routine_Hash(chr_q routine)			// return hash code
int Routine_Hash(chr_x routine)			// return hash code
{ // int hash = 0;			        // for the return
  int i;					// a handy int
  //int p[8] = {3,5,7,11,13,17,19,23};          // primes
  //for (i = 0; i < 8; i++)                     // for each character
  //{ hash = (((routine & 0xFF) * p[i]) + hash);// add that char
  //  routine = (routine >> 8);                 // right shift one byte
  // int p[MAX_NAME_BYTES] = {                  // primes
  //     3,  5,  7, 11, 13, 17, 19, 23,
  //    29, 31, 37, 41, 43, 47, 53, 59,
  //    61, 67, 71, 73, 79, 83, 89, 97,
  //   101,103,107,109,113,127,131,137 };
  // for (i = 0; i < MAX_NAME_BYTES; i++)          // for each character
  // { hash = ((routine.buf[i] * p[i]) + hash);    // add that char
  // }                                             // end hash loop
  // return (hash % RBD_HASH);			// return the code

  uint32_t hash = 2166136261UL;                 // FNV-1a hash
  for (i = 0; routine.buf[i] && (i < MAX_NAME_BYTES); i++)
  { hash ^= (unsigned char) routine.buf[i];
    hash *= 16777619UL;
  }
  return (hash % RBD_HASH);                     // return the code
}

void Routine_Combine(rbd *pointer)		// combine with following block
{ rbd *ptr;					// a handy pointer
  rbd *p;					// and another

  ptr = (rbd *) ((u_char *) pointer + pointer->chunk_size); // point at next

  p = systab->vol[0]->rbd_hash[RBD_HASH];	// see where it points
  if (p == ptr)					// if that's us
    systab->vol[0]->rbd_hash[RBD_HASH] = ptr->fwd_link; // point at our fwd link
  else						// else find our entry
  { while (p->fwd_link != ptr) p = p->fwd_link; // find previous entry
    p->fwd_link = ptr->fwd_link;		// point it at our forward link
  }
  pointer->chunk_size += ptr->chunk_size;	// add the two sizes together
  return;					// and exit
}

void Routine_Free(rbd *pointer)			// call internally, rbd locked
{ rbd *ptr;					// a handy pointer
  rbd *p;					// and another
  int hash;					// hash pointer

  pointer->rou_size = 0;			// flag 'not used'

  //hash = Routine_Hash(pointer->rnam.var_qu);	// get the hash
  hash = Routine_Hash(pointer->rnam.var_xu);	// get the hash
  ptr = systab->vol[0]->rbd_hash[hash];		// see where it points
  if (ptr == pointer)				// if that's us
  { systab->vol[0]->rbd_hash[hash] =
      pointer->fwd_link;			// point at our forward link
  }
  else						// else find our entry
  { while (TRUE)
    { if (ptr == NULL)				// if end of list
      { ptr = systab->vol[0]->rbd_hash[RBD_HASH]; //point at first free
	if (pointer == ptr)
	{ systab->vol[0]->rbd_hash[RBD_HASH] = pointer->fwd_link;
	}
	else
        { while (ptr != NULL)			// scan free list
          { if (ptr->fwd_link == pointer)	// if in freelist
            { ptr->fwd_link = pointer->fwd_link; // take it out
	      break;				// and quit
            }
            ptr = ptr->fwd_link;		// point at next
	    // if (ptr == NULL) we should search else where!!!
          }
	}
        break;
      }
      if (ptr->fwd_link == pointer)		// if found
      { ptr->fwd_link = pointer->fwd_link;	// point it at our forward link
        break;					// and quit
      }
      ptr = ptr->fwd_link; 			// point to next
    }
  }
  pointer->fwd_link = systab->vol[0]->rbd_hash[RBD_HASH]; //point at first free
  systab->vol[0]->rbd_hash[RBD_HASH] = pointer;	// add to free list
  //pointer->rnam.var_qu = 0;			// zot rou name
  X_Clear(pointer->rnam.var_xu);		// zot rou name
  pointer->uci = 0;				// and uci
  pointer->vol = 0;				// and vol
  pointer->last_access = (time_t) 0;		// and access

  while (TRUE)					// until end of list
  { ptr = (rbd *) ((u_char *) pointer + pointer->chunk_size); // point at next
    if (ptr >= (rbd *) systab->vol[0]->rbd_end) break;	// quit when done
    if (ptr->rou_size == 0)			// if there is no routine
      Routine_Combine(pointer);			// combine it in
    else break;					// else done
  }

  while (TRUE)					// look for previous
  { ptr = (rbd *) systab->vol[0]->rbd_head;	// start of rbds
    if (ptr == pointer) return;			// same - all done
    while (TRUE)				// scan for previous
    { p = (rbd *) ((u_char *) ptr + ptr->chunk_size); // point at next
      if (p == pointer) break;			// found it (in ptr)
      ptr = p;					// remember this one
    }
    if (ptr->rou_size == 0)			// if unused
    { pointer = ptr;				// make this one active
      Routine_Combine(pointer);			// combine it in
    }
    else return;				// else all done
  }  
}

void Routine_Collect(time_t off)		// collect based on time
{ rbd *ptr;					// a pointer

  // off = time(0) - off;				// get compare time
  off = MTIME(0) - off;				// get compare time
  ptr = (rbd *) systab->vol[0]->rbd_head;	// head of rbds
  while (TRUE)					// scan whole list
  { if ((ptr->attached < 1) &&			// nothing attached
        (ptr->last_access < off) &&		// and it fits the time
	(ptr->rou_size > 0))			// and not already free
    { Routine_Free(ptr);			// free it
      ptr = (rbd *) systab->vol[0]->rbd_head;	// start from the begining
    }
    ptr = (rbd *) ((u_char *) ptr + ptr->chunk_size); // point at next
    if (ptr >= (rbd *) systab->vol[0]->rbd_end) break;	// quit when done
  }
  return;					// all done
}

rbd *Routine_Find(int size)			// find int bytes
{ rbd *ptr;					// a pointer
  rbd *p;					// and another
  ptr = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
  while (ptr != NULL)				// while we have some
  { if (ptr->chunk_size >= size) break;		// if big enough
    ptr = ptr->fwd_link;			// get next
  }
  if (ptr == NULL)				// found nothing
  { Routine_Collect(RESERVE_TIME);		// do a collect using reserve
    ptr = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
    while (ptr != NULL)				// while we have some
    { if (ptr->chunk_size >= size) break;	// if big enough
      ptr = ptr->fwd_link;			// get next
    }
    if (ptr == NULL)				// found nothing
    { Routine_Collect(0);			// do a collect using zero
      ptr = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
      while (ptr != NULL)			// while we have some
      { if (ptr->chunk_size >= size) break;	// if big enough
	ptr = ptr->fwd_link;			// get next
      }
      if (ptr == NULL) return NULL;		// found nothing - give up
    }
  }
  if ((size + SIZE_CLOSE) < ptr->chunk_size)	// if far too big
  { p = (rbd *) ((u_char *) ptr + size);	// setup for another
    p->fwd_link = ptr->fwd_link;		// and forward link
    p->chunk_size = ptr->chunk_size - size;	// its size
    p->attached = 0;				// clear this lot
    p->last_access = 0;				// no time reqd
    //p->rnam.var_qu = 0;			// no routine name
    X_Clear(p->rnam.var_xu);			// no routine name
    p->uci = 0;					// no uci
    p->vol = 0;					// no vol
    p->rou_size = 0;				// no routine (not in use)
    ptr->fwd_link = p;				// point at new chunk
    ptr->chunk_size = size;			// the new size
  }
  if (systab->vol[0]->rbd_hash[RBD_HASH] == ptr)
    systab->vol[0]->rbd_hash[RBD_HASH] = ptr->fwd_link; // new free bit
  else
  { p = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
    while (p->fwd_link != ptr) p = p->fwd_link;	// find our ptr
    p->fwd_link = ptr->fwd_link;		// change to new one
  }
  return ptr;					// return the pointer
}

// The following are called from the general mumps code
//

//rbd *Routine_Attach(chr_q routine)		// attach to routine
rbd *Routine_Attach(chr_x routine)		// attach to routine
{ int hash = 0;					// for rbd_hash[]
  int i;					// a handy int
  int size;					// size required
  short s;					// a usefull thing
  rbd *ptr;					// a pointer for this
  rbd *p;					// and another
  u_char tmp[3*MAX_NAME_BYTES];			// temp space
  cstring *cptr;				// for making strings
  mvar rouglob;					// mvar for $ROUTINE
  u_char uci;					// current uci
  u_char vol;					// current vol
  var_u *test;					// for testing name
  
  test = (var_u *) &routine;			// map as a var_u
  hash = Routine_Hash(routine);			// get the hash
  s = SemOp(SEM_ROU, -systab->maxjob);		// write lock the rbds
  if (s < 0) return NULL;			// say can't find on error
  p = systab->vol[0]->rbd_hash[hash];		// see where it points
  ptr = p;					// use it in ptr
  uci = partab.jobtab->ruci;			// get current uci
  vol = partab.jobtab->rvol;			// and vol

  if (test->var_cu[0] == '%')
  {  uci = 1;					// check for a % routine
     vol = 1;
  }
  while (ptr != NULL)				// while we have something
  //{ if ((ptr->rnam.var_qu == routine) &&
  { if (X_EQ(ptr->rnam.var_xu, routine) &&
	(ptr->uci == uci) &&
        (ptr->vol == vol))			// if this is the right one
    { ptr->attached++;				// count an attach
      s = SemOp(SEM_ROU, systab->maxjob);	// release the lock
      return ptr;				// and return the pointer
    }
    ptr = ptr->fwd_link;			// point at the next one
  }						// end while loop
  s = SemOp(SEM_ROU, systab->maxjob);		// release the lock

  X_set( "$ROUTINE", rouglob.name.var_cu, 8);	// global name
  rouglob.nsubs = 255;
  rouglob.volset = vol;				// volume set
  rouglob.uci = uci;				// uci
  cptr = (cstring *) tmp;			// get some temp space

  for (i = 0; i < MAX_NAME_BYTES; i++)		// loop thru the name
  { if (((var_u) routine).var_cu[i] == '\0') break;
    cptr->buf[i] = ((var_u) routine).var_cu[i]; // copy a byte
  }
  cptr->buf[i] = '\0';				// terminate
  cptr->len = (short) i;			// the count
  s = UTIL_Key_BuildEx(&rouglob, cptr, &rouglob.key[0]); // first subs
  if (s < 0) return NULL;                       // got an error
  rouglob.slen = s;				// save count so far
  cptr->buf[0] = '0';				// now the zero
  cptr->buf[1] = '\0';				// null terminate
  cptr->len = 1;				// and the length
  s = UTIL_Key_BuildEx(&rouglob, cptr, &rouglob.key[s]); // second subs
  if (s < 0) return NULL;
  rouglob.slen = rouglob.slen + s;		// save count so far
  s = DB_GetLen(&rouglob, 0, NULL);		// get a possible length
  if (s < 1) return NULL;			// no such
  s = SemOp(SEM_ROU, -systab->maxjob);		// write lock & try again
  if (s < 0) return NULL;			// no such

  p = systab->vol[0]->rbd_hash[hash];		// see where it points
  ptr = p;					// use it in ptr
  while (ptr != NULL)				// while we have something
  //{ if ((ptr->rnam.var_qu == routine) &&
  { if (X_EQ(ptr->rnam.var_xu, routine) &&
	(ptr->uci == uci) &&
	(ptr->vol == vol))			// if this is the right one
    { ptr->attached++;				// count an attach
      s = SemOp(SEM_ROU, systab->maxjob);	// release the lock
      return ptr;				// and return the pointer
    }
    p = ptr;					// save for ron
    ptr = ptr->fwd_link;			// point at the next one
  }						// end while loop

  s = DB_GetLen(&rouglob, 1, NULL);		// lock the GBD
  if (s < 1)					// if it's gone
  { s = DB_GetLen(&rouglob, -1, NULL);		// un-lock the GBD
    s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
    return NULL;				// say no such
  }
  size = s + RBD_OVERHEAD + 1;			// space required
  if (size & 7) size = (size & ~7) + 8;		// rount up to 8 byte boundary
  ptr = Routine_Find(size);			// find location
  if (ptr == NULL)				// no space mate!!
  { s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
    s = DB_GetLen(&rouglob, -1, NULL);		// un-lock the GBD
    return (rbd *)(-1);				// say no space
  }
  if (p == NULL)				// listhead for this hash
    systab->vol[0]->rbd_hash[hash] = ptr;	// save it here
  else p->fwd_link = ptr;			// or here
  ptr->fwd_link = NULL;				// ensure this is null
  ptr->attached = 1;				// count the attach
  // ptr->last_access = time(0);			// and the current time
  ptr->last_access = MTIME(0);			// and the current time
  //ptr->rnam.var_qu = routine;			// the routine name
  ptr->rnam.var_xu = routine;			// the routine name
  ptr->uci = uci;				// the uci
  ptr->vol = vol;				// current volume
  ptr->rou_size = s;				// save the size
  s = DB_GetLen(&rouglob, -1, (u_char *) &ptr->comp_ver); // get the routine

  if (s != ptr->rou_size) panic("routine load - size wrong"); // DOUBLECHECK

  ptr->tag_tbl += RBD_OVERHEAD;			// adjust for rbd junk
  ptr->var_tbl += RBD_OVERHEAD;			// adjust for rbd junk
  ptr->code += RBD_OVERHEAD;			// adjust for rbd junk
  if (ptr->comp_ver != COMP_VER) 		// check comp version
  { ptr->attached--;				// decrement the count
    Routine_Free(ptr);				// free the space
    s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
    return (rbd *)(-2);				// yet another *magic* number
  }
  s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
  return ptr;					// success
}

void Routine_Detach(rbd *pointer)		// Detach from routine
{ short s;					// for SemOp() call
  while (SemOp(SEM_ROU, -systab->maxjob) < 0);	// lock the rbds, check error
  if (pointer->attached > 0)			// if not lost
  { pointer->attached--;			// decrement the count
  }
  if ((pointer->uci == 0) &&			// if it's invalid
      (pointer->attached == 0))			// and nothing is attached
    Routine_Free(pointer);			// free the space
  s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
  return;					// done
}

//void Routine_Delete(chr_q routine, int uci)	// mark routine deleted
void Routine_Delete(chr_x routine, int uci, int vol) // mark routine deleted
// NOTE: Semaphore must be held BEFORE calling this routine
{ int hash = 0;					// for rbd_hash[]
  rbd *ptr;					// a pointer for this

  hash = Routine_Hash(routine);			// get the hash
  ptr = systab->vol[0]->rbd_hash[hash];		// see where it points
  while (ptr != NULL)				// while we have something
  //{ if ((ptr->rnam.var_qu == routine) &&
  { if (X_EQ(ptr->rnam.var_xu, routine) &&
	(ptr->uci == uci) &&
        (ptr->vol == vol)) 			// if this is the right one
    { ptr->uci = 0;				// mark as deleted
      if (ptr->attached == 0)			// if not in use
	Routine_Free(ptr);			// free the space
      break;					// and exit
    }
    ptr = ptr->fwd_link;			// point at the next one
  }						// end while loop
  return;					// done
}
