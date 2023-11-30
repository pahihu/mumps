// File: mumps/util/util_trantab.c
//
// module database - TRANTAB Utilities

/*      Copyright (c) 2023
 *      Andras Pahi.  All rights reserved.
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

#include <stdint.h>
#include <string.h>

#include <stdio.h>                              // always include 
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes

//------------------------------------------------------------------------------
// Function: gethash
// Descript: Calculate FNV1a hash of var, JUST the name (no UCI/VOL)
// Input(s): Pointer to mvar
// Return:   unsigned 32bit hash value
//

static
uint32_t gethash(var_u *var)
{ return FNV1aHash(sizeof(var_u), (u_char *) var);
}


//------------------------------------------------------------------------------
// Function: UTIL_TTFindIdx
// Descript: Find the given var in the TRANTAB
// Input(s): Pointer to TRANTAB
//           Pointer to src mvar
//           Flag to enable empty UCI matching
// Return:  <0 - var found in TRANTAB, updated dst var
//          >0 - routine spec
//           0 - var not found
//

short UTIL_TTFindIdx(trantab *tt, mvar *src)
{ int i, j, x;                                          // handy ints
  uint32_t h;                                           // hash value
  size_t matchlen;                                      // matching name length

  x = (h = gethash(&src->name)) & TRANTAB_HASH_MASK;    // calc. hash index
  for (i = 0; tt->hash[x].tti && i < MAX_TRANTAB_HASH; i++)
  { if (h == tt->hash[x].from_h)                        // same hash
    { // from_global          ^GLO matches ANY  ^[UCI,VOL]GLO
      // from_global ^[UCI,VOL]GLO matches ONLY ^[UCI,VOL]GLO
      matchlen = sizeof(var_u) + tt->hash[x].from_uci ? 2 : 0;
      if (0 == bcmp(src, &tt->hash[x].from_global, matchlen)) // if a match
      { j = tt->hash[x].tti - 1;                        // check trantab[] index
        if (0 == tt->tab[j].to_vol)
        { return (j+1);					// flag routine proc
        }
        return -(j+1);                                  // flag as found
      }
    }
    x = (x + 1) & TRANTAB_HASH_MASK;                    // next hash entry
  }
  return 0;                                             // not found
}


//------------------------------------------------------------------------------
// Function: UTIL_TTFind
// Descript: Find the given var in the TRANTAB, update dst var
// Input(s): Pointer to TRANTAB
//           Pointer to src mvar
//           Pointer to dst mvar
// Return:  <0 - var found in TRANTAB, updated dst var
//          >0 - routine spec
//           0 - var not found
//

short UTIL_TTFind(trantab *tt, mvar *src, mvar *dst)
{ int i;                                                // handy int
  short s;                                              // for functions

  s = UTIL_TTFindIdx(tt, src);
  if (s < 0)
  { i = -s; i--;
    bcopy(&tt->tab[i].to_global, dst, sizeof(var_u) + 2);
  }
  return s;
}


//------------------------------------------------------------------------------
// Function: addhash
// Descript: Add the ith entry in TRANTAB, to the hash
// Input(s): Pointer to TRANTAB
//           Index to add/update
// Return:
//

static
void addhash(trantab *tt, int i)                        // add entry to hash
{ int j, x;                                             // handy ints
  uint32_t h;                                           // hash value
  var_u *var;                                           // pointer to var_u
  size_t matchlen;                                      // matching name length

  if (0 == tt->tab[i].to_uci)                           // empty?
    return;                                             //   done

  var = &tt->tab[i].from_global;
  h = gethash(var);                                     // calc. hash value
  x = h & TRANTAB_HASH_MASK;                            // initial index
  for (j = 0; tt->hash[x].tti && j < MAX_TRANTAB_HASH; j++)
  { if (h == tt->hash[x].from_h)                        // same hash
    { matchlen = sizeof(var_u) + (tt->hash[x].from_uci ? 2 : 0);
      if (0 == bcmp(var, &tt->hash[x].from_global, matchlen)) // if a match
      { tt->hash[x].tti = i + 1;                        // update entry
        return;                                         // done
      }
    }
    x = (x + 1) & TRANTAB_HASH_MASK;                    // next hash entry
  }
  if (0 == tt->hash[x].tti)                             // empty slot?
  { tt->hash[x].tti = i + 1;                            // add entry
    tt->hash[x].from_h = h;                             // update hash
    bcopy(var, &tt->hash[x].from_global,                // and from_global
                                sizeof(var_u) + 2);
  }
}


//------------------------------------------------------------------------------
// Function: UTIL_TTAdd
// Descript: Add the given entry to TRANTAB, rehash TRANTAB
// Input(s): Pointer to TRANTAB
//           Index to add/update
//           Pointer to TRANTAB entry
// Return:
//

void UTIL_TTAdd(trantab *tt, int i, ttentry *elt)
{ while( SemOp( SEM_SYS, -systab->maxjob))
    ;
  bcopy(elt, &tt->tab[i], sizeof(ttentry));             // add entry to TRANTAB
  tt->tab[i].from_h = gethash(&elt->from_global);       // calc. hash value
  if (i + 1 > tt->ntab)                                 // after ntab?
    tt->ntab = i + 1;                                   //   update it
  addhash(tt, i);
  SemOp( SEM_SYS, systab->maxjob);
}


//------------------------------------------------------------------------------
// Function: rehash
// Descript: Rehash TRANTAB entries
// Input(s): Pointer to TRANTAB
// Return:
//

static
void  rehash(trantab *tt)                               // rehash entries
{ int i;                                                // handy int

  bzero(&tt->hash[0], sizeof(tt->hash));                // clear hash
  for (i = 0; i < tt->ntab; i++)
    addhash(tt, i);                                     // add entry to hash
}


//------------------------------------------------------------------------------
// Function: UTIL_TTDelete
// Descript: Delete the ith entry in TRANTAB, update hash
// Input(s): Pointer to TRANTAB
//           Index to delete
// Return:
//

void UTIL_TTDelete(trantab *tt, int i)
{ if (0 != tt->tab[i].to_uci)                           // not empty?
  { while (SemOp( SEM_SYS, -systab->maxjob))
      ;
    bzero(&tt->tab[i], sizeof(ttentry));                //   clear entry
    tt->ntab = 0;                                       // clear it
    for (i = MAX_TRANTAB; i; i--)
    { if (0 != tt->tab[i - 1].to_uci)                   // not empty?
      { tt->ntab = i;                                   //   update ntab
      }
    }
    rehash(tt);                                         // rehash TRANTAB
    SemOp( SEM_SYS, systab->maxjob);
  }
}

