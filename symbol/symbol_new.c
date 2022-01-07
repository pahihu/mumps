// File: mumps/symbol/symbol_new.c
//
// module symbol - Symbol Table New'ing and UnNew'ing Utilities

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
#include "mumps.h"                              // standard includes
#include "symbol.h"				// our definitions
#include "error.h"                              // errors
#include "init.h"				// init prototypes
#include "proto.h"				// standard prototypes
#include <unistd.h>


//****************************************************************************
//**  Function: ST_New(int count, var_u *list)  - new one or more vars
//**  Returns : 0 on success or -'ve error
short ST_New(int count, var_u *list)
{
  ST_newtab *newtab;				// our new table
  int i;					// generic counter
  short s;

  newtab = malloc(sizeof(ST_newtab) + (count * sizeof(ST_locdata)));
  						// try to get enough memory
  if (newtab == NULL) return -(ERRZ56+ERRMLAST); // no memory avlb
  newtab->fwd_link =				// setup for link in
    (ST_newtab *) partab.jobtab->dostk[partab.jobtab->cur_do].newtab;
  newtab->count_enn = 0;			// not applicable
  newtab->stindex = NULL;			// not needed
  newtab->count_new = count;			// how many we are to new
  newtab->locdata = (ST_locdata *)
		    (((u_char *) &(newtab->locdata)) + sizeof(ST_locdata *));
						// point at next free address
  for (i = (count - 1); i >= 0; i--)		// for all vars in list
  { //s = ST_SymAtt(list[i].var_qu);		// attach to variable
    s = ST_SymAtt(&list[i].var_xu);		// attach to variable
    if (s < 0) return (s);			// check for error
    newtab->locdata[i].stindex = s;		// save the index
    newtab->locdata[i].data = symtab[s].data;	// and the data address
    symtab[s].data = ST_DATA_NULL;		// remove data link
  }
  partab.jobtab->dostk[partab.jobtab->cur_do].newtab =
    (u_char *) newtab;				// link it to the do stack
  return 0;					// finished OK
}						// end function ST_New

//****************************************************************************
//**  Function: ST_NewAll(int count, var_u *list)  - new all vars except listed
//**  Returns : 0 on success, or -'ve error
short ST_NewAll(int count, var_u *list)
{
  int i;                                        // generic counter
  int j;                                        // generic counter
  int k;                                        // generic counter
  int new = 0;                                  // to be new'd flag
  int cntnew = 0;				// new count
  int cntnon = 0;				// non new count
  ST_newtab *newtab;				// pointer to the new table

  for (k = 0; k < count; k++)			// for all supplied vars
    //(void)ST_Create(list[k].var_qu);		// Create if not existent
    (void)ST_Create(&list[k].var_xu);		// Create if not existent

  for (i = 0; i <= ST_MAX-1; i++)               // for each entry in ST
  { if (symtab[i].varnam.var_cu[0] == '$') continue; // ignore $ vars
    if (symtab[i].varnam.var_cu[0] == '\0') continue; // ignore unused
    if (count > 0)                              // if there are vars to keep
    { for (j = 0; j < count; j++)               // for all keep vars
      { new = 1;                                // init delete flag
        //if (symtab[i].varnam.var_qu == list[j].var_qu)
        if (X_EQ(symtab[i].varnam.var_xu, list[j].var_xu))
        { new = 0;				// dont new it
          break;
        }
      }                                         // if var is another non new
      if (new == 1)                             // if new flag set
      { cntnew += 1;				// incr num new'd vars
      }						// setup done for var
      else					// dont new, add to enn
      { cntnon += 1;				// incr num non new'd vars
      }						// end else add to enn
    }						// end if vars to not new
    else					// no vars to keep
    { cntnew += 1;				// incr count of new'd vars
    }                                           // end else new everything
  }                                             // end for all in symtab

  newtab = malloc( sizeof(ST_newtab) +
                   (cntnew * sizeof(ST_locdata)) +
                   (cntnon * sizeof(short)) );	// try allocate some memory

  if (newtab == NULL) return -(ERRZ56+ERRMLAST); // no memory avlb
  newtab->fwd_link =				// setup for link in
    (ST_newtab *) partab.jobtab->dostk[partab.jobtab->cur_do].newtab;
  newtab->count_enn = count;			// existing non new count
  newtab->count_new = 0;  			// num vars new'd

  newtab->stindex = (short *)
                    (((u_char *) &(newtab->locdata)) + sizeof(ST_locdata *));

  newtab->locdata = (ST_locdata *)
                    (((u_char *) &(newtab->locdata))
                                 + sizeof(ST_locdata *)
                                 + (cntnon * sizeof(short)));

  for (i = 0; i <= ST_MAX-1; i++)               // for each entry in ST
  { if (symtab[i].varnam.var_cu[0] == '$')	// ignore $ vars
      continue;					// so go to next one
    if (symtab[i].varnam.var_cu[0] == '\0') continue; // ignore unused
    if (count > 0)                              // if there are vars to keep
    { for (j = 0; j < count; j++)               // for all keep vars
      { new = 1;                                // init delete flag
        //if (symtab[i].varnam.var_qu == list[j].var_qu)
        if (X_EQ(symtab[i].varnam.var_xu, list[j].var_xu))
        { new = 0;				// dont new it
          break;
        }
      }                                          // if var is another non new
        if (new == 1)                           // if new flag set
        { newtab->locdata[newtab->count_new].stindex = i; // create index entry
          newtab->locdata[newtab->count_new].data = // point at current data
            symtab[newtab->locdata[newtab->count_new].stindex].data;
          symtab[newtab->locdata[newtab->count_new].stindex].data =
            ST_DATA_NULL;			// wipe out current data link
          ++symtab[newtab->locdata[newtab->count_new].stindex].usage;
          newtab->count_new++;			// incr num new'd vars & usage
        }					// setup done for var
        else					// dont new, add to enn
        { newtab->stindex[j] = i;		// set pos to symtab index
        }					// end else add to enn
    }						// end if vars to not new
    else					// no vars to keep
    { newtab->locdata[newtab->count_new].stindex = i; // create index entry
      newtab->locdata[newtab->count_new].data = // point at current data
        symtab[newtab->locdata[newtab->count_new].stindex].data;
      symtab[newtab->locdata[newtab->count_new].stindex].data =
        ST_DATA_NULL;				// wipe out current data link
      ++symtab[newtab->locdata[newtab->count_new].stindex].usage;
      newtab->count_new++;			// incr count of new'd vars
    }                                           // end else new everything
  }                                             // end for all in symtab
  partab.jobtab->dostk[partab.jobtab->cur_do].newtab =
    (u_char *) newtab;				// link it off partab
  return 0;					// finished OK
}                                               // end ST_NewAll


//****************************************************************************
//**  Function: ST_Restore(ST_newtab *)  - restore vars in newtab and its links
//**  Returns :
void ST_Restore(ST_newtab *newtab)
{
  ST_newtab *ptr, *savnewtab;                           // ptr-> current newtab
  ST_depend *dd;					// depend data ptr
  ST_depend *ddf;					// depend data ptr
  int i;						// generic counter
  int t;						// generic counter
  int chk;						// symtab index
  int kill;						// kill flag

  savnewtab = newtab;                                   // save newtab

Loop:
  ptr = newtab;						// go to first newtab
  if (ptr == NULL) return;				// nothing to do
  if (ptr->stindex != NULL)				// check for newall
  { for (t = 0; t < ST_HASH; t++)			// for all hash entries
    { if (st_hash[t] != -1)				// only those defined
      { chk = st_hash[t];				// get symtab link
        while (chk != -1)				// while fwdlinks exist
        { kill = chk;					// init kill flag
	  if (symtab[chk].varnam.var_cu[0] == '$')
	    kill = -1;					// leave $...
	  else
          { for (i = 0; i < ptr->count_enn; i++)	// for all enn vars
            //{ if (symtab[chk].varnam.var_qu ==		// if an ENN var
            //      symtab[ptr->stindex[i]].varnam.var_qu)
            { if (X_EQ(symtab[chk].varnam.var_xu, 	// if an ENN var
                  symtab[ptr->stindex[i]].varnam.var_xu))
              { kill = -1; 				// DONT KILL
	        break;					// and exit for
	      }
            }						// all enn vars checked
	  }
          chk = symtab[chk].fwd_link;			// get next fwd link
          if (kill > -1)				// if ok to kill
            (void)ST_SymKill(kill);			// kill by index
        }						// end if end of fwd's
      }							// end if no hash link
    }							// end for all hash lnk
  }							// all enn vars done
  for (i = 0; i < ptr->count_new; i++)			// for all new'd vars
  { if (symtab[ptr->locdata[i].stindex].data != NULL)	// if we have data blk
    { symtab[ptr->locdata[i].stindex].data->attach--;	// decrement attach
      if (symtab[ptr->locdata[i].stindex].data->attach < 1) // all gone?
      { dd = symtab[ptr->locdata[i].stindex].data->deplnk; // get dependants
        while (dd != NULL)
        { ddf = dd;					// save a copy
          dd = dd->deplnk;				// get next
          free(ddf);					// free this one
        }
        free(symtab[ptr->locdata[i].stindex].data);	// free data
	symtab[ptr->locdata[i].stindex].data = NULL;	// and remember
      }
    }

    symtab[ptr->locdata[i].stindex].data = ptr->locdata[i].data; // old data
    --symtab[ptr->locdata[i].stindex].usage;		// decrement usage
    if (symtab[ptr->locdata[i].stindex].data != NULL)	// any data?
      if ((symtab[ptr->locdata[i].stindex].data->deplnk == NULL) &&
	  (symtab[ptr->locdata[i].stindex].data->attach < 2) &&
	  (symtab[ptr->locdata[i].stindex].data->dbc == VAR_UNDEFINED))
      { free(symtab[ptr->locdata[i].stindex].data);	// free data memory
	symtab[ptr->locdata[i].stindex].data = NULL;	// clear ptr
      }

    if ((symtab[ptr->locdata[i].stindex].usage < 1) &&	// can we dong it?
	(symtab[ptr->locdata[i].stindex].data == NULL)) // any data?
      (void)ST_SymKill(ptr->locdata[i].stindex);	// dong it
  }							// all new'd vars done

  if (ptr->fwd_link != NULL)				// if there are more
  { newtab = ptr->fwd_link;                             // save fwd link
    goto Loop;                                          // restore next newtab
  }							// end if there's more

  ptr = savnewtab;                                      // restore newtab
  while (ptr != NULL)                                   // while there are more
  { newtab = ptr->fwd_link;                             // save fwd link
    if (ptr ==                                          // clear doframe
        (ST_newtab *) partab.jobtab->dostk[partab.jobtab->cur_do].newtab)
      partab.jobtab->dostk[partab.jobtab->cur_do].newtab = NULL;
    free(ptr);                                          // free the space
    ptr = newtab;                                       // check next
  }
}							// end function Restore

//****************************************************************************
//**  Function: ST_ConData(mvar *, ST_data *)  - connect reference to data ptr
//**  Returns : 0 on success, or -'ve error
short ST_ConData(mvar *var, u_char *data)
{ short cnct;						// connector var loc

  cnct = ST_LocateIdx(var->volset - 1);			// find connecting var
  if (cnct < 0) return -(ERRM6);			// if no exist, quit
  symtab[cnct].data = (ST_data *) data;			// lnk cnct var to src
  ++symtab[cnct].data->attach;				// incr src attach cnt
  return 0;						// finished OK
}							// end ST_ConRef
