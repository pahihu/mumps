// File: mumps/runtime/runtime_func.c
//
// module runtime - RunTime Functions

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
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>                              // error stuf
#include <limits.h>                     	// for LONG_MAX etc
#include <math.h>
#include <stdarg.h>                             // for va macros
#include <sys/time.h>				// for gettimeofday()
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"                              // standard errors
#include "database.h"				// for gbd def
#include "dgp_database.h"			// for remote VOL

#ifdef linux
#include <values.h>				// for linux
#endif //linux

// All functions use the following structure
//
// short Dfunc(u_char *ret_buf, ...
//
// The first argument is of type u_char* and is the destination
// for the value returned by the function (max size is 32767).
// The subsequent arguments are all read only and are the passed in values.
// The function returns a count of characters in the return string
// or a negative error number (MUMPS error).
// The function name is DfuncnameN
//      where the call is $funcname() and N is the number of arguments.
//

//***********************************************************************
// $ASCII(expr[,int]) - return ascii value
//
short Dascii1(u_char *ret_buffer, cstring *expr)
{ return Dascii2(ret_buffer, expr, 1);
}

short Dascii2(u_char *ret_buffer, cstring *expr, int posn)
{ int asc = -1;                                 // default to -1
  int i;                                        // for loops
  if ((posn > 0)&&(posn <= (int)expr->len))     // if within range
    asc = (int)expr->buf[posn-1];               // get from string
  i = itocstring( ret_buffer, asc);          	// convert answer to string
  return i;                                     // return the count
}

//***********************************************************************
// $CHAR(int1[,int2[,...]]) - return a string - we implement 1 char at a time
//      called for each argument (valid range is 0 to 255)
short Dchar(u_char *ret_buffer, int i)
{ ret_buffer[0] = '\0';                         // assume nothing
  if ((i < 0) || (i > 255)) return 0;           // out of range
  ret_buffer[0] = (u_char) i;                   // store the char
  ret_buffer[1] = '\0';                         // extra for null term
  return 1;                                     // count of 1
}

//***********************************************************************
// $DATA(variable[,target])
//
short Ddata(u_char *ret_buffer, mvar *var)
{ return Ddata2(ret_buffer, var, 0);            // use Data2()
}

short Ddata2(u_char *ret_buffer, mvar *var, mvar *target)
{ cstring cstr, *dat = 0;
  short s, ret;

  if (target)
  { cstr.len = VAR_UNDEFINED;                   // setup dat ptr
    dat = &cstr;
  }
  if (var->uci == UCI_IS_LOCALVAR)              // for a local var
    ret = ST_DataEx(var, ret_buffer, dat);
  else if (var->name.var_cu[0] == '$') 	        // ssvn?
  { if (target)                                 // 3 arg form not allowed
      return -(ERRM29);
    return SS_Data(var, ret_buffer);	        // yes
  }
  else
  { bcopy( var, &(partab.jobtab->last_ref), MVAR_SIZE + var->slen);
    ret = LDB_DataEx(var, ret_buffer, dat);     // else it's global
  }
  if (ret < 0)
    return ret;
  if (target && (VAR_UNDEFINED != dat->len))    // target given and has data
  { if (target->uci == UCI_IS_LOCALVAR)
      s = ST_Set(target, dat);                  // set as local
    else
      s = DB_Set(target, dat);                  // set as global
    if (s < 0)                                  // check error
      return s;
  }
  return ret;
}

//***********************************************************************
// Dispatch routine:
//
#define CSBUF(x)        (&x.buf[0])
#define CSBUFP(x)       (&(x)->buf[0])

static int dolog = 1;

int mv1log(int depth,const char *fmt,...)
{ int i;
  va_list ap;

  if (!dolog) return 0;

  fprintf(stderr,"\n#%08llX ",0xFFFFFFFF & systab->vol[volnum-1]->vollab->txid);
  for (i = 0; i < depth; i++)
     fprintf(stderr,"  ");

  va_start(ap, fmt);
  i = vfprintf(stderr,fmt,ap);
  va_end(ap);

  fflush(stderr);
  return i;
}

short cstringcpy(cstring *dst, const char *str)
{ if ((0 == str) || (0 == *str))                // either NULL or empty
  { dst->len = 0;                               //   zero length
  }
  else
  { dst->len = strlen(str);                     // get length of C string
    bcopy(str, CSBUFP(dst), dst->len);          //   copy to cstring
  }
  dst->buf[dst->len] = '\0';                    // null terminate JIC
  return dst->len;                              // return length
}

short cstringcat(cstring *dst, const char *str)
{ int len;

  len = strlen(str);
  if (len)
  { bcopy(str, &dst->buf[dst->len], len);
    dst->len += len;
    dst->buf[dst->len] = '\0';
  }
  return dst->len;
}

short cstringset(cstring *dst, cstring *src)
{ dst->len = src->len;
  if (dst->len)
  { bcopy(CSBUFP(src), CSBUFP(dst), dst->len);
  }
  dst->buf[dst->len] = '\0';
  return dst->len;
}

#define MAX_PARENTS     64

int   nVisitedClasses;
char *VisitedClasses[2*MAX_PARENTS];

short CheckVisitedClasses(char *cls)
{ int i;

  for (i = nVisitedClasses; i; i--)
  { if (0 == strcmp(VisitedClasses[i-1], cls))
      return -(ERRMLAST+ERRZ78);                // complain on circular dep.
  }
  return 0;
}

short ResolveEntry(int depth,char *cls, cstring *entry, cstring *tgt)
{ char *classes[MAX_PARENTS]; 
  u_char tmp1[2 + 3*MAX_NAME_BYTES];
  u_char tmp2[2 + 3*MAX_NAME_BYTES];
  cstring *cptr, *parent;
  cstring parents, line;
  int i, found;
  int sav_nsubs, sav_slen;
  mvar var;
  short s;
  char *ptr;

  LOG(mv1log(depth,">>> R E S O L V E E N T R Y"));
  LOG(mv1log(depth,"cls=%s entry=%s", cls, CSBUFP(entry)));

  ////////////////////////////////////////////////
  // 0) check circular dependency
  //
  if (nVisitedClasses == 2*MAX_PARENTS)         // too deep parents tree
    return -(ERRMLAST+ERRZ79);
  s = CheckVisitedClasses(cls);
  if (s < 0) return s;
  VisitedClasses[nVisitedClasses++] = cls;

  cptr   = (cstring *)&tmp1; cptr->len = 0;     // setup pointers
  parent = (cstring *)&tmp2; parent->len = 0;

  ////////////////////////////////////////////////
  // 1) check %ZSEND cache for (cls,entry)
  //
  var.nsubs = 255;                              // get %ZSEND(cls,entry)
  var.volset = 0;
  var.uci = UCI_IS_LOCALVAR;
  X_set("%ZSEND", &var.name.var_cu[0], 7);
  cstringcpy(cptr, cls);
  s = UTIL_Key_BuildEx(&var, cptr, &var.key[0]);
  if (s < 0) return s;
  var.slen  = s;
  sav_nsubs = var.nsubs;                        // save position of %ZSEND(cls,
  sav_slen  = var.slen;
  s = UTIL_Key_BuildEx(&var, entry, &var.key[var.slen]);
  if (s < 0) return s;
  var.slen += s;
  s = ST_Get(&var, CSBUFP(tgt));                // get it
  if (s > 0)                                    // found it, set back to rou
  { tgt->len = s;
    nVisitedClasses--;
    LOG(mv1log(depth,"found in %%ZSEND, tgt=%s", CSBUFP(tgt)));
    return tgt->len;
  }
  if (s == -(ERRM6)) s = 0;                     // not found, OK
  if (s < 0) return s;                          // complain

  ////////////////////////////////////////////////
  // 2) not found in %ZSEND cache
  //                    check current cls
  //
  found = 0;                                    // flag not found
  cstringset(cptr, entry);                      // construct entryref
  cstringcat(cptr, "^");                        //   entry^cls
  cstringcat(cptr, cls);
  s = Dtext(CSBUF(line), cptr);                 // check $TEXT(entry^cls)
  if (s > 0)                                    // if found,
  { found = 1;
    cstringcpy(tgt, cls);                       //   set target to cls
    LOG(mv1log(depth,"found in current rtn(%s), tgt=%s", cls, CSBUFP(tgt)));
    goto Set_ZSEND_cls_entry;                   //   set %ZSEND(cls,entry)=tgt
  }

  ////////////////////////////////////////////////
  // 3) if class is %Object, then terminate with
  //                    %Unknown
  if (0 == strcmp("%Object", cls))
    goto Set_ZSEND_cls_entry_Unknown;

  ////////////////////////////////////////////////
  // 4) try to get (cls,"%Parents") from cache
  //
  var.nsubs = sav_nsubs;                        // get %ZSEND(cls,"%Parents")
  var.slen  = sav_slen;
  cstringcpy(cptr, "%Parents");
  s = UTIL_Key_BuildEx(&var, cptr, &var.key[var.slen]);
  if (s < 0) return s;
  var.slen += s;
  s = ST_Get(&var, CSBUF(parents));             // get it
  if (s > 0)                                    // found, continue with search
  { parents.len = s;
    LOG(mv1log(depth,"found %%Parents in %%ZSEND, parents=%s", CSBUF(parents)));
    goto SearchParents;
  }
  if (s == -(ERRM6)) s = 0;                     // not found, OK
  if (s < 0) return s;                          // complain on error

  ////////////////////////////////////////////////
  // 5) no (cls,"%Parents") entry in cache, get
  //                    the line from cls
  cstringcpy(cptr, "%Parents^");                // no parent, try to get it
  cstringcat(cptr, cls);
  s = Dtext(CSBUF(parents), cptr);              // from the routine
  if (s <  0) return s;                         // complain on error
  if (s == 0)                                   // not found, default
  {
NoParents:
    cstringcpy(&parents, "%Object");            //   parent is %Object
    LOG(mv1log(depth,"no %%Parents line in rtn, use %%Objects"));
    goto Set_ZSEND_cls_Parents;
  }

  ////////////////////////////////////////////////
  // 6) found %Parents line, write back to cache
  //
  LOG(mv1log(depth,"parse parents=%s", CSBUF(parents)));
  parents.len = s;
  ptr = strstr((char *) CSBUF(parents), ";;");  // find delimiter
  if (ptr == 0) goto NoParents;                 //   not found, no %Parents
  i = ptr + 2 - (char *) CSBUF(parents);        // skip to delimiter end
  while ((i < parents.len) &&                   // skip space
         (parents.buf[i] < 33)) i++;
  if (i == parents.len) goto NoParents;         // empty line, no %Parents
  memcpy(CSBUF(parents), &parents.buf[i], parents.len - i); // shift contents
  parents.len -= i;                             // adjust length
  parents.buf[parents.len] = '\0';              // null terminate
  LOG(mv1log(depth,"parsed parents=%s", CSBUF(parents)));

Set_ZSEND_cls_Parents:
  s = ST_Set(&var, &parents);                   // set back
  if (s < 0) return s;

  ////////////////////////////////////////////////
  // 7) search %Parents for the entry, recursive
  //
  // example line:
  //    %Parents ;;Parent1,Parent2,...,ParentN
  //
  // search in reverse order:
  //    ParentN,Parent(N-1),...,Parent2,Parent1
  //
SearchParents:                                  // check the parents
  ptr = strtok((char *) CSBUF(parents), ",");   // tokenize %Parents line
  i = 0;
  while ((i < MAX_PARENTS) && (ptr))
  { if (strlen(ptr) > MAX_NAME_BYTES)           // restrict to MAX_NAME_BYTES
      ptr[MAX_NAME_BYTES] = '\0';
    classes[i++] = ptr;                         // save in classes[]
    ptr = strtok(NULL, ",");
  }
  LOG(mv1log(depth,"parents has %d classes",i));
  if (i == MAX_PARENTS)                         // too many parents
    return -(ERRMLAST+ERRZ80);
  LOG(mv1log(depth,"--- B E G I N  R E C U R S I V E  R E S O L V E"));
  for (; i; i--)
  { s = ResolveEntry(depth+1,classes[i-1], entry, tgt);
    if (s < 0) return s;
    if (s > 0)
    { // target is the resolved class
      // cstringcpy(tgt, classes[i-1]);
      found = 1;
      LOG(mv1log(depth,"entry=%s resolved by class=%s",CSBUFP(entry),CSBUFP(tgt)));
      goto Set_ZSEND_cls_entry;
    }
  }
  
  ////////////////////////////////////////////////
  // 8) cannot resolve entry in cls
  //                            and its parents
  //
Set_ZSEND_cls_entry_Unknown:
  cstringcpy(tgt, "%Unknown");                  // set it to %Unknown
  LOG(mv1log(depth,"entry=%s cannot be resolved, use tgt=%s",CSBUFP(entry),CSBUFP(tgt)));

Set_ZSEND_cls_entry:                            // SET %ZSEND(cls,entry)=tgt
  LOG(mv1log(depth,"set %%ZSEND(%s,%s)=%s",cls,CSBUFP(entry),CSBUFP(tgt)));
  var.nsubs = sav_nsubs;
  var.slen  = sav_slen;
  s = UTIL_Key_BuildEx(&var, entry, &var.key[var.slen]);
  if (s < 0) return s;
  var.slen += s;
  s = ST_Set(&var, tgt);
  if (s < 0) return s;                          // complain on error
  nVisitedClasses--;
  return found ? s : 0;                         // return 0 if not found
}

short Ddispatch(cstring *oref, cstring *entry, chr_x *rou, chr_x *tag)
{ u_char tmp1[2 +   MAX_NAME_BYTES];            // cls buffer
  u_char tmp2[2 + 3*MAX_NAME_BYTES];
  cstring *cls, *tgt;
  int i, j;
  int sav_entlen;
  short s;

  LOG(mv1log(0,">>> D I S P A T C H"));
  LOG(mv1log(0,"oref=%s entry=%s", CSBUFP(oref), CSBUFP(entry)));

  nVisitedClasses = 0;

  if ((0 >= oref->len) || (0 >= entry->len))    // OREF/entry empty/UNDEF
    return -(ERRMLAST+ERRZ74);                  // complain

  sav_entlen = entry->len;                      // truncate to MAX_NAME_BYTES 
  if (sav_entlen > MAX_NAME_BYTES) entry->len = MAX_NAME_BYTES;
  X_set(CSBUFP(entry), tag, entry->len);        //   set tag

  cls  = (cstring *)&tmp1; cls->len  = 0;       // setup pointers
  tgt  = (cstring *)&tmp2; tgt->len  = 0;

  for (i = 0; i < oref->len; i++)               // parse OREF
  { if (oref->buf[i] != '@')                    // search for separator
      continue;
    i++;                                        // skip separator
    if (i >= oref->len)                         // reached end of OREF
      break;
    j = oref->len - i;                          // truncate class name
    if (j > MAX_NAME_BYTES - 1)                 //   to MAX_NAME_BYTES
      j = MAX_NAME_BYTES - 1;
    bcopy(&oref->buf[i], CSBUFP(cls), j);       //   copy to class name
    cls->len = j;                               // set length
    cls->buf[cls->len] = '\0';                  // null terminate JIC
  }

  if (0 == cls->len)                            // empty class name
    cstringcpy(cls, "%Object");                 //   use default

  LOG(mv1log(0,"cls=%s",cls->buf); fflush(stderr));

  s = ResolveEntry(1, (char *) CSBUFP(cls), entry, tgt);// resolve the entry
  if (s < 0)                                    // complain if error
  { entry->len = sav_entlen;                    // restore entry len
    return s;
  }

  X_set(CSBUFP(tgt), rou, tgt->len);

  LOG(mv1log(0,"rou=%s tag=%s",(char *) rou,(char *) tag));
  LOG(mv1log(0,"<<<"));

  return tgt->len;
}

//***********************************************************************
// $EXTRACT(expr[,start[,stop]])
//
short Dextract(u_char *ret_buffer, cstring *expr, int start, int stop)
{ int i;                                        // for loops
  if ((start < 1) && (stop > 0))
    start = 1;                     		// ensure sensible
  ret_buffer[0] = '\0';                       	// setup null string
  if (start < 1) return 0;			// that's a null
  if (stop > (int)expr->len)                    // if past end of string
    stop = (int)expr->len;                      // point at the end
  if ((stop < start)||(start > (int)expr->len))
    return 0;                                   // and return it
  i = stop-start+1;                             // bytes to copy
  bcopy( &expr->buf[start-1],                   // copy from here
         ret_buffer,                            // to here
         i);                                    // this many bytes
  ret_buffer[i] = '\0';                         // null terminate
  return i;                                     // return count
}

//***********************************************************************
// $FIND(expr1,expr2[,int])
//
short Dfind2(u_char *ret_buffer, cstring *expr1, cstring *expr2)
{ return Dfind3(ret_buffer, expr1, expr2, 1);
}

int Dfind3x(cstring *expr1, cstring *expr2, int start)
{ int i;                                        // for loops
  int j;                                        // for expr2
  int ret = 0;                                  // return value
  if (start < 1) start = 1;                     // ensure sensible
  if (expr2->len == 0)                          // if expr2 is null str
    return start;				// just return start
  for (i=start-1; i < (int)expr1->len; i++)     // scan expr1
  { for (j=0; j != (int)expr2->len; j++)        // scan expr2
    { if (expr1->buf[i+j] != expr2->buf[j]) break; // quit if not the same
      if ((j + 1) == (int)expr2->len)           // if at end of expr2
        ret = i + j +2;                         // remember where we are
    }                                           // end compare loop
    if (ret != 0) break;                        // quit if found
  }                                             // end of expr
  return ret;                                   // and return it
}

short Dfind3(u_char *ret_buffer, cstring *expr1, cstring *expr2, int start)
{ int ret = 0;                                  // return value
  ret = itocstring( ret_buffer,
                 Dfind3x(expr1, expr2, start));	// eval into buffer
  return (short) ret;				// and return length
}

//***********************************************************************
// $FNUMBER(numexp,code[,int])
//
//      +       Inserts a plus sign (+) on numbers greater than 0.
//      -       Supresses the negative sign on negative values of string
//              either by default (on positive numbers), or by design
//              (using the code string "-").
//      ,       Inserts delimiters every third position to the left of the
//              decimal point within a string.
//      T       Places a plus sign (+) or a minus (-) sign after the
//              string. (If sign suppression is active, using this code
//              string results in a trailing space.)
//      P       Places a negative number in parentheses, or enters spaces
//              (prefix and suffix) if the number is non-negative.
//
// NOTE: numexp MUST be a canonic number!!

short Dfnumber2(u_char *ret_buffer, cstring *numexp, cstring *code)
{
  cstring *tempc;
  cstring *dest;
  int i;
  int z;
  int ndlen;
  int nlen;
  int nc;
  int cd = 0;
  u_char *ptr1;
  char *a1 = NULL;				// flag
  char *a2 = NULL;				// flag
  char *b1 = NULL;				// flag
  char *b2 = NULL;				// flag
  char *c1 = NULL;				// flag
  char *c2 = NULL;				// flag
  char *d1 = NULL;				// flag
  char *dp = NULL;				// decimal point pos
  short s;
  int zero = 0;                                 // flag '0'

  ret_buffer[0] = '\0';
  a1 = strchr((const char *) &code->buf[0],'p');		// in code string ??
  a2 = strchr((const char *) &code->buf[0],'P');		// in code string ??
  b1 = strchr((const char *) &code->buf[0],'+');		// in code string ??
  b2 = strchr((const char *) &code->buf[0],'-');		// in code string ??
  c1 = strchr((const char *) &code->buf[0],'t');		// in code string ??
  c2 = strchr((const char *) &code->buf[0],'T');		// in code string ??
  d1 = strchr((const char *) &code->buf[0],',');		// in code string ??
  dp = strchr((const char *) &numexp->buf[0],'.');		// decimal pos in number *|Null
  if (((a1 != NULL) || (a2 != NULL)) &&		// check for invalid
      ((b1 != NULL) || (b2 != NULL) || (c1 != NULL) || (c2 != NULL)))
    return -(ERRM2);				// invalid code, error


  if (numexp->len > 1)
  { for (z = 0; z <= numexp->len; z++)
    { if (numexp->buf[z] != '0') break;
    }
  }
  else
  { z = 0;
    zero = '0' == numexp->buf[0];               // '0' ?
  }
  if ((z == 1) && (numexp->buf[1] == '.'))	// check for '0.bla'
  { z = 0;					// leave the leading zero
  }

  tempc = mv1malloc(sizeof(short) + numexp->len + (numexp->len / 3) + 3);
  dest = mv1malloc(sizeof(short) + numexp->len + (numexp->len / 3) + 3);
  dest->len = numexp->len - z;
  bcopy(&numexp->buf[z], dest->buf, numexp->len);
  if (d1 != NULL)				// add in commas
  { if (dp != NULL) // contains a decimal point
    { for (i = 0; i <= dest->len-1; i++)
      { if (dest->buf[i] == '.') break;
      }
      ndlen = i;					// save this pos
      if (numexp->buf[0] == '-')
      { ndlen -= 1;					// dont count "-"
      }
      nc = ndlen / 3;					// num commas reqd
      if (((ndlen % 3) == 0) && (i > 0)) nc -= 1;	// if *3 need one less,
      nlen = dest->len+nc-1;				// orig+num commas-idx
      tempc->len = nlen+1;
      ptr1 = &dest->buf[dest->len-1];		// copy all including
      while (*ptr1 != '.')				// the NULL term, up to
      { bcopy(ptr1,&tempc->buf[nlen], 1);		// the decimal point
        nlen -= 1;					// but not including
        ptr1 -= 1;					// the decimal point
      }
      bcopy(ptr1, &tempc->buf[nlen], 1); 		// now copy over
      nlen -= 1;					// the decimal
      ptr1 -= 1;					// point only
      while (nlen >= 0)					// copy the rest
      { cd += 1;					// of the string
        bcopy(ptr1, &tempc->buf[nlen], 1);		// to the destination
        nlen -= 1;					// and every
        ptr1 -= 1;					// third position
        if (((cd % 3) == 0) && (nlen > 0))		// copy over
        { tempc->buf[nlen] = ',';			// a comma
          nlen -= 1;
        }
      }
    }
    else // no decimal point
    { ndlen = numexp->len;				// save this start pos
      if (numexp->buf[0] == '-')
      { ndlen -= 1;					// dont count "-"
      }
      nc = ndlen / 3;					// num commas reqd
      if ((ndlen % 3) == 0) nc -= 1;			// if *3 need one less,
      nlen = numexp->len+nc-1;				// orig+num commas-idx
      tempc->len = nlen+1;
      ptr1 = &dest->buf[dest->len-1];
      while (nlen >= 0)					// copy the rest
      { cd += 1;					// of the string
        bcopy(ptr1, &tempc->buf[nlen], 1);		// to the destination
        nlen -= 1;					// and every
        ptr1 -= 1;					// third position
        if (((cd % 3) == 0) && (nlen > 0))		// copy over
        { tempc->buf[nlen] = ',';			// a comma
          nlen -= 1;
        }
      }
    }
    dest->len = tempc->len;
    bcopy(tempc->buf, dest->buf, tempc->len);
  }
  if ((a1 != NULL) || (a2 != NULL))
  { if (numexp->buf[0] != '-')
    { tempc->buf[0] = ' ';			// space pad for '('
      bcopy(dest->buf, &tempc->buf[1], dest->len); // copy original data
      tempc->buf[1+dest->len] = ' ';		// space pad for ')'
      tempc->len = dest->len + 2;
    }						// no further action reqd
    if (numexp->buf[0] == '-')
    { tempc->buf[0] = '(';			// prefix a '('
      bcopy(&dest->buf[1], &tempc->buf[1], dest->len-1); // copy original data
      tempc->buf[dest->len] = ')';		// suffix a ')'
      tempc->len = dest->len + 1;
    }						// no further action reqd
    dest->len = tempc->len;
    bcopy(tempc->buf, dest->buf, tempc->len);
  }
  if ((c1 != NULL) || (c2 != NULL))		// trailing signs
  { if (numexp->buf[0] != '-')
    { if (!zero && (b1 != NULL))                // force + sign at end, except for '0'
      { if (dest->buf[0] == '+')		// if + sign already at front
        { bcopy(&dest->buf[1], &tempc->buf[0], dest->len-1);
          tempc->buf[dest->len-1] = '+';
          tempc->len = dest->len;
        }
        else					// no + sign at front
        { bcopy(&dest->buf[0],&tempc->buf[0],dest->len);
          tempc->buf[dest->len] = '+';
          tempc->len = dest->len + 1;
        }
      }
      else
      { bcopy(&dest->buf[0], tempc->buf, dest->len);
        tempc->len = dest->len;
      }
    }
    else					// negative number
    { if (b2 != NULL)				// - sign supress, tack a space
      { bcopy(&dest->buf[1],&tempc->buf[0],dest->len-1);
        tempc->buf[dest->len-1] = ' ';
        tempc->len = dest->len;
      }
      else					// force - sign at end
      { bcopy(&dest->buf[1],&tempc->buf[0],dest->len-1);
        tempc->buf[dest->len-1] = '-';
        tempc->len = dest->len;
      }
    }
    dest->len = tempc->len;
    bcopy(tempc->buf, dest->buf, tempc->len);
  }
  else						// non trailing signs
  { if (numexp->buf[0] != '-')
    { if (!zero && (b1 != NULL))                // force + sign at front, except for '0'
      { if (dest->buf[0] != '+')
        { if (numexp->buf[0] != '-')
          { tempc->buf[0] = '+';
            bcopy(dest->buf, &tempc->buf[1], dest->len);
            tempc->len = dest->len + 1;
          }
          else
          { bcopy(dest->buf, &tempc->buf[0], dest->len);
            tempc->len = dest->len;
          }
        }
      }
      else
      { bcopy(&dest->buf[0], tempc->buf, dest->len);
        tempc->len = dest->len;
      }
    }
    else // negative number
    { if (b2 != NULL) // - sign supressed
      { bcopy(&dest->buf[1], tempc->buf, dest->len-1);
        tempc->len = dest->len - 1;
      }
      else
      { bcopy(&dest->buf[0], tempc->buf, dest->len);
        tempc->len = dest->len;
      }        
    }
    dest->len = tempc->len;
    bcopy(tempc->buf, dest->buf, tempc->len);
  }
  dest->len = tempc->len;
  bcopy(dest->buf, ret_buffer, dest->len);
  ret_buffer[dest->len] = '\0';
  s = dest->len;
  mv1free(dest);
  mv1free(tempc);
  return s;
}						// end function $FNUMBER


short Dfnumber3(u_char *ret_buffer, cstring *numexp, cstring *code, int rnd)
{ cstring *change;
  short s;

  s = Djustify3(ret_buffer, numexp, 0, rnd);
  if (s < 0)
  { return s;
  }
  change = mv1malloc(sizeof(short) + s + 1);
  change->len = s;
  bcopy(ret_buffer, change->buf, s + 1);
  s = Dfnumber2(ret_buffer, change, code);
  mv1free(change);
  return s;
}


//***********************************************************************
// $GET(variable[,expr])
//
short Dget1(u_char *ret_buffer, mvar *var)
{ u_char tmp[8];				// some space
  cstring *cptr;				// for the call
  cptr = (cstring *) tmp;			// point at the space
  cptr->len = 0;				// zero length
  cptr->buf[0] = '\0';				// null terminated
  return Dget2(ret_buffer, var, cptr); 		// do it below
}

short Dget2(u_char *ret_buffer, mvar *var, cstring *expr)
{ short s;					// for return values 
  if (var->uci == UCI_IS_LOCALVAR)		// for a local var
  { s = ST_Get(var, ret_buffer);		// attempt to get the data
    if (s >= 0) return s;			// if we got data, return it
    if (s == -(ERRM6)) s = 0;			// flag undefined local var
  }
  else if (var->name.var_cu[0] == '$') 		// ssvn?
  { s = SS_Get(var, ret_buffer);		// attempt to get the data
    if (s >= 0) return s;			// if we got data, return it
    if ((s == -(ERRM38)) || (s == -(ERRM7)))
      s = 0;					// flag undefined ssvn
  }
  else						// for a global var
  { bcopy( var, &(partab.jobtab->last_ref), MVAR_SIZE + var->slen);
    s = LDB_Get(var, ret_buffer);		// attempt to get the data
    if (s >= 0) return s;			// if we got data, return it
    if (s == -(ERRM7)) s = 0;			// flag undefined global var
  }
  if (s != 0) return s;				// if an error, return it
  bcopy( &expr->buf[0],                         // copy from here
         &ret_buffer[0],                      	// to here
         expr->len);                            // this many bytes
  ret_buffer[expr->len] = '\0';			// ensure null terminated
  return (expr->len);				// and return the length
}

//***********************************************************************
// Bit strings
//
static u_char BitMask[] = { 255, 128, 192, 224, 240, 248, 252, 254 };
static struct {
  u_char bit1count;                             // count of '1's
  u_char bit1pos;                               // 1 + position of 1st '1'
  u_char bit0pos;                               // 1 + position of 1st '0'
} Bits[256];

void Bits_Init(void)                            // init Bits[] array
{ int i, j;
  u_char byt;

  for (i = 0; i < 256; i++)                     // for all possible value
  { byt = i;
    Bits[i].bit1count = 0;
    Bits[i].bit1pos = 0;
    Bits[i].bit0pos = 0;
    for (j = 0; j < 8; j++)
    { if (128 & byt)
      { Bits[i].bit1count++;                    // count '1's
        if (0 == Bits[i].bit1pos)               // remember 1st '1' pos
          Bits[i].bit1pos = 1 + j;
      }
      else if (0 == Bits[i].bit0pos)            // remember 1st '0' pos
        Bits[i].bit0pos = 1 + j;
      byt <<= 1;
    }
  }
}

//***********************************************************************
// $JUSTIFY(expr,int1[,int2])
//
short Djustify2(u_char *ret_buffer, cstring *expr, int size)
{ int adj;                                      // adjust reqd
  int i;                                        // for loops
  adj = size - (int)expr->len;                  // get number of spaces
  if (adj < 0) adj = 0;                         // ensure positive
    for (i=0; i<adj; i++)                       // for each required space
      ret_buffer[i] = ' ';                      // copy in a space
  bcopy( &expr->buf[0],                         // copy from here
         &ret_buffer[adj],                      // to here
         expr->len);                            // this many bytes
  i = expr->len+adj;                            // get the new size
  ret_buffer[i] ='\0';                          // nul terminate it
  return i;                                     // and return it
}                                               // end no 2 arg $J()

short Djustify3(u_char *ret_buffer, cstring *expr, int size, int round)
// NOTE: We must have been passed a canonic number

{ int i;
  int j = 0;
  int spc;					// leading space count
  int zer = 0;					// leading zero reqd flag
  int len;
  int ru = -2;					// round up flag
  int dp = -2;					// decimal point
  int cop;
  int neg;

  if (round < 0)
  { return -(ERRM28);				// that's an error
  }
  if (size < 0)					// if negative size
  { size = 0;					// make it zero
  }
  len = expr->len;
  for (i = 0; i < expr->len; i++)
  { if (expr->buf[i] == '.')			// search for DP
    { dp = i;
      break;
    }
  }
  if ((!dp) ||
      ((dp == 1) && (expr->buf[0] == '-')))
  { zer = 1;					// need to add a zero
  }
  if (!round)
  { if (dp != -2)
    { len = dp;
      if (expr->buf[dp + 1] > '4')
      { ru = dp - 1;
      }
    }
  }
  else
  { if (dp != -2)
    { len = dp + round + 1;
      if ((len < expr->len) && (expr->buf[len] > '4'))
      { ru = len - 1;
      }
    }
    else
    { len += (round + 1);
    }
  }
  spc = size - len - zer;			// spaces required
  if (spc < 0)
  { spc = 0;
  }
  for (i = 0; i < spc; ret_buffer[i++] = ' ');	// copy in spaces
  if (expr->buf[0] == '-')
  { ret_buffer[i++] = '-';			// copy minus
    j = 1;					// where copy starts
  }
  for (cop = 0; cop < zer; cop++)
  { ret_buffer[i++] = '0';			// possible leading zero
  }
  cop = expr->len - j;
  if (len < expr->len)
  { cop = len - j;
  }
  bcopy(&expr->buf[j], &ret_buffer[i], cop);	// copy the rest
  i += cop;
  len += (zer + spc);
  if ((dp == -2) && (i < len))
  { ret_buffer[i++] = '.';
  }
  while (i < len)
  { ret_buffer[i++] = '0';			// possible trailing zeroes
  }

  ret_buffer[len] = '\0';			// null terminate
  if (ru != -2)
  { ru += (zer + spc);				// adjust round up
    while (TRUE)
    { neg = ret_buffer[ru] == '-';              // check leading '-'
      if (neg)
      { ret_buffer[ru] = '1';                   // rewrite as '1'
        if (ru - 1 >= 0)                        // propagate sign
          ret_buffer[ru - 1] = '-';
        break;                                  // stop
      }
      ret_buffer[ru]++;				// increment it
      if (ret_buffer[ru] <= '9')
      { break;					// stop when done
      }
      ret_buffer[ru] = '0';			// change back to zero
      ru--;					// decrement ru
      if (ret_buffer[ru] == '.')
      { ru--;					// skip the DP
      }
      if (ret_buffer[ru] == ' ')
      { ret_buffer[ru] = '0';
      }
      if (ru >= j)
      { continue;
      }
      bcopy(&ret_buffer[j], &ret_buffer[j + 1], len - j + 1);
      ret_buffer[j] = '1';
      len++;
      break;
    }
  }
  for (i = 0; i < len; i++)
  { if (ret_buffer[i] == '-')
    { for (j = i + 1; j < len; j++)
      { if ((ret_buffer[j] != '0') && (ret_buffer[j] != '.'))
	{ break;
	}
      }
      if (j == len)				// found nothing usefull
      { if ((i != 0) || (len == size))
        { ret_buffer[i] = ' ';			// remove the minus
	  break;
        }
        bcopy(&ret_buffer[1], &ret_buffer[0], len--); // or this way
        break;
      }
    }
    else if (ret_buffer[i] != ' ')
    { break;
    }
  }
  return (short) len;
}

//***********************************************************************
// $LENGTH(expr1[,expr2])
//
short Dlength1(u_char *ret_buffer, cstring *expr)
{ return itocstring( ret_buffer, expr->len); 	// just do it
}

int Dlength2x(cstring *expr, cstring *delim)
{ int i;                                        // temp
  int j;                                        // index for delim
  int pce = 1;                                  // for version 2
  if (delim->len == 0)                          // special case
    return 0;                                   // return zero
  for (i=0; i < (int)expr->len; i++)            // scan expr
  { for (j=0; j != (int)delim->len; j++)
    { if (expr->buf[i+j] != delim->buf[j]) break; // quit if not the same
      if ((j + 1) == (int)delim->len)           // if at end of delim
      { pce++;                                  // count a piece
        i = i + j;                              // move i on a bit
      }                                         // end 'piece matches'
    }                                           // end compare loop
  }                                             // end of expr
  return pce;                                   // and return count
}

short Dlength2(u_char *ret_buffer, cstring *expr, cstring *delim)
{ return itocstring( ret_buffer,
  		  Dlength2x(expr, delim));	// copy to buf and ret len
}

//***********************************************************************
// $NAME(variable[,int])
//
short Dname1(u_char *ret_buffer, mvar *var)
{ return Dname2(ret_buffer, var, 99999);        // use Dname2()
}

short Dname2(u_char *ret_buffer, mvar *var, int sub)
{ if (sub < 0) return -(ERRM39);		// Invalid $NAME argument
  return UTIL_String_Mvar(var, ret_buffer, sub); // do it elsewhere
}

//***********************************************************************
// $ORDER(subscripted variable[,int[,target]])
//
short Dorder1(u_char *ret_buffer, mvar *var)
{ return Dorder3(ret_buffer, var, 1, 0);         // use Dorder3()
}

short Dorder2(u_char *ret_buffer, mvar *var, int dir)
{ return Dorder3(ret_buffer, var, dir, 0);      // use Dorder3()
}

short Dorder3(u_char *ret_buffer, mvar *var, int dir, mvar *target)
{ int i = -1;					// dir patch flag
  short s, ret;
  int realdir;
  cstring cstr, *dat = 0;

  if ((dir != 1) && (dir != -1)			// validate direction
    && ((dir != 2) && (systab->historic & HISTORIC_DNOK))) // for $NEXT
    return -(ERRMLAST+ERRZ12);			// complain on error
  if (0 == var->slen)                           // no subscripts
    return -(ERRMLAST+ERRZ74);
  if (target)                                   // setup pointers
  { cstr.len = VAR_UNDEFINED;
    dat = &cstr;
  }
  realdir = dir;
  if (dir == 2)
  { realdir = 1;
  }
  if (dir == -1)				// is it backwards?
    if ((var->key[var->slen-1] == '\0') &&
        (var->key[var->slen-2] == '\0'))	// is it a nul?
    { i = var->slen-2;				// posn of first 0
      var->key[i] = '\377';			// change to 255
    }
  if (var->uci == UCI_IS_LOCALVAR)
  { ret = ST_OrderEx(var, ret_buffer, realdir, dat);// for local var
  }
  else if (var->name.var_cu[0] == '$') 		// ssvn?
  { if (target)                                 // 3 arg form for ssvn
      return -(ERRM29);                         //   not allowed
    ret = SS_Order(var, ret_buffer, realdir);	// yes
  }
  else
  { bcopy( var, &(partab.jobtab->last_ref), MVAR_SIZE + var->slen);
    if (i != -1) partab.jobtab->last_ref.key[i] = '\0'; // unfix from above
    ret = LDB_OrderEx(var, ret_buffer, realdir, dat);// else it's global
  }
  if (target && (VAR_UNDEFINED != dat->len))    // target given and has data
  { if (target->uci == UCI_IS_LOCALVAR)
      s = ST_Set(target, dat);                  // set target as local
    else
      s = DB_Set(target, dat);                  // set target as global
    if (s < 0)                                  // check error
      return s;
  }
  if ((dir == 2) && (ret == 0))			// last for $NEXT
  { bcopy("-1\0", ret_buffer, 3);		// change to -1
    ret = 2;
  }
  return ret;
}

//***********************************************************************
// $PIECE(expr1,expr2[,int1[,int2]])
//
short Dpiece2(u_char *ret_buffer, cstring *expr, cstring *delim)
{ return Dpiece4(ret_buffer, expr, delim, 1, 1); // use Dpiece4()
}

short Dpiece3(u_char *ret_buffer, cstring *expr, cstring *delim, int i1)
{ return Dpiece4(ret_buffer, expr, delim, i1, i1); // use Dpiece4()
}

short Dpiece4(u_char *ret_buffer, cstring *expr, cstring *delim, int i1, int i2)
{ int beg = 0;                                  // start copy from
  int end;                                      // copy to
  int pce = 1;                                  // current piece
  int f;                                        // found flag
  int i, j;                                     // for delim scan
  u_char d;					// delim char
  ret_buffer[0] = '\0';                         // just in case
  if (delim->len == 0) return 0;                // null delimiter -> nul str
  if (i1 < 0) i1 = 0;                           // minus makes no sense
  if (i2 < 0) i2 = 0;                           // minus makes no sense
  if ((i1 == 0) && (i2 == 0)) return 0;         // piece 0 is null str
  if (i1 > i2) return 0;                        // that's also null
  if (1 == delim->len)
  { d = delim->buf[0];				// save delim
    for (end = 0; end < expr->len; end++)       // scan expr
    { if (expr->buf[end] == d)      		// if matches
      { if (pce == i2)                          // if this is last piece
        { end--;                                // point at last reqd char
          break;                                // and quit for loop
        }                                       // end last piece processing
        pce++;                                  // increment current piece
        if (pce == i1) beg = end + 1;  		// if this is the first pce
      }                                         // end found code
    }                                           // end of expr scan
  }
  else
  { for (end = 0; end < expr->len; end++)       // scan expr
    { if (expr->buf[end] == delim->buf[0])      // if first char matches
      { f = 1;                                  // set found flag
        for (j = 1; j < delim->len; j++)        // scan rest of delimiter
        { i = delim->len - j;
          if (expr->buf[end+i] != delim->buf[i])// if we have a mismatch
          { f = 0;                              // clear found flag
            break;                              // and quit
          }
        }                                       // end delim scan
        if (f == 0)				// no match, continue
          continue;
        if (pce == i2)                          // if this is last piece
        { end--;                                // point at last reqd char
          break;                                // and quit for loop
        }                                       // end last piece processing
        pce++;                                  // increment current piece
        end = end + delim->len - 1;		// point at last char of delim
        if (pce == i1) beg = end + 1;  		// if this is the first pce
      }                                         // end of got match
    }                                           // end of expr scan
  }

  if (pce < i1) return 0;			// didn't find anything
  if (end == expr->len) end--;			// don't point past end
  j = end - beg + 1;				// number of bytes we want
  bcopy( &expr->buf[beg],                       // copy from here
         ret_buffer,                            // to here
         j);                                    // this many bytes
  ret_buffer[j] = '\0';                         // null terminate it
  return j;                                     // return count
}

//***********************************************************************
// $QUERY(variable[,int[,target]])
//
short Dquery1(u_char *ret_buffer, mvar *var)
{ return Dquery3(ret_buffer, var, 1, 0);        // use Dquery3()
}

short Dquery2(u_char *ret_buffer, mvar *var, int dir)
{ return Dquery3(ret_buffer, var, dir, 0);      // use Dquery3()
}

short Dquery3(u_char *ret_buffer, mvar *var, int dir, mvar *target)
{ int i = -1;					// dir patch flag
  short s, ret;
  cstring cstr, *dat = 0;                       // cstring to store value

  if ((dir != 1) && (dir != -1))		// validate direction
    return -(ERRMLAST+ERRZ12);			// complain on error

  if (dir == -1)				// is it backwards?
    if ( var->slen &&
        (var->key[var->slen-1] == '\0') &&
        (var->key[var->slen-2] == '\0'))	// is it a nul?
    { i = var->slen-2;				// posn of first 0
      var->key[i] = '\377';			// change to 255
    }

  if (target)                                   // setup pointers
  { cstr.len = VAR_UNDEFINED;
    dat = &cstr;
  }

  if (var->uci == UCI_IS_LOCALVAR)
    ret = ST_QueryEx(var, ret_buffer, dir, dat);// for local var
  else if (var->name.var_cu[0] == '$') 		// ssvn?
    return (-ERRM38);				// no such
  else
  { bcopy( var, &(partab.jobtab->last_ref), MVAR_SIZE + var->slen);
    if (i != -1) partab.jobtab->last_ref.key[i] = '\0'; // unfix from above
    ret = LDB_QueryEx(var, ret_buffer, dir, GLO_DOCVT, dat);// else it's global
  }
  if (target && (VAR_UNDEFINED != dat->len))    // target given and has data
  { if (target->uci == UCI_IS_LOCALVAR)
      s = ST_Set(target, dat);                  // set as local
    else
      s = DB_Set(target, dat);                  // set as global
    if (s < 0)                                  // check error
      return s;
  }
  return ret;
}

//***********************************************************************
// $RANDOM(int)
//
short Drandom(u_char *ret_buffer, int seed)
{ if (seed < 1) return (-ERRM3);                // an error
  seed = random() % seed;                       // get a random number
  return itocstring( ret_buffer, seed);      	// convert answer to string
}

//***********************************************************************
// $REVERSE(expr)
//
short Dreverse(u_char *ret_buffer, cstring *expr)
{ int i;                                        // for the loop
  int j = 0;                                    // destination
  for (i = (int)expr->len-1; i >= 0; i--)       // for each character
    ret_buffer[j++] = expr->buf[i];             // copy it
  ret_buffer[j] = '\0';                         // terminate it
  return expr->len;                             // and return count
}

//***********************************************************************
// $STACK(int[,code])
//
//Retn:   $ST(-1) returns the largest value for which $ST(value) returns a
//        non-empty string.
//        $ST(0) returns an implementation specific value indicating how this
//        process was started. RUN or JOB.
//        $ST(n) where n is 1 to $ST(-1) returns how this level of process stack
//        was created (one of DO, XECUTE, $$ or an error code like ",M6,").
//        While int is zero or greater, the following codes may be used:
//        ECODE   the list of ecodes added at this level
//        MCODE   the source line of code identified by "PLACE" below
//        PLACE   the location of a command at this stack level as follows:
//                a) if int is not equal to $STACK and $ST(int,"ECODE") is
//                   empty, the last command executed.
//                b) if int is equal to $STACK and $ST(int,"ECODE") is
//                   empty, the currently executing command.
//                c) if $ST(int,"ECODE") is not empty, the last command to
//                   start execution while $ST(int,"ECODE") was empty.
//

short Dstack1(u_char *ret_buffer, int level)
{ return Dstack1x(ret_buffer, level, MV1_PID);
}

short Dstack1x(u_char *ret_buffer, int level, int job)
{ int i;					// a usefull int
  ret_buffer[0] = '\0';				// null terminate
  if (level < -1) return 0;			// junk
  i = systab->jobtab[job].cur_do;		// default
  if (systab->jobtab[job].error_frame > systab->jobtab[job].cur_do)
    i = systab->jobtab[job].error_frame;	// ensure we have the error bit
  if (level > i) return 0;			// nothing there
  if (level == -1)
    return itocstring(ret_buffer, i);		// return the number
  if (level == 0)
  { if (systab->jobtab[job].dostk[0].type == TYPE_JOB)
      return mcopy((u_char *) "JOB", ret_buffer, 3);	// for a JOB command
    return mcopy((u_char *) "RUN", ret_buffer, 3);		// normal run
  }
  if (level == systab->jobtab[job].error_frame) level = STM1_FRAME; // err frame
  i = systab->jobtab[job].dostk[level].type & 127; // get the type
  if (i == TYPE_RUN) return mcopy((u_char *) "BREAK", ret_buffer, 5);
  if (i == TYPE_DO) return mcopy((u_char *) "DO", ret_buffer, 2);
  if (i == TYPE_EXTRINSIC) return mcopy((u_char *) "$$", ret_buffer, 2);
  if (i == TYPE_XECUTE) return mcopy((u_char *) "XECUTE", ret_buffer, 6);
  ret_buffer[0] = '\0';
  return 0;					// else nothing
}

short Dstack2(u_char *ret_buffer, int level, cstring *code)
{ return Dstack2x(ret_buffer, level, code, MV1_PID);
}

short Dstack2x(u_char *ret_buffer, int level, cstring *code, int job)
{ int arg2 = 0;					// arg 2 1 = ECODE
						//       2 = MCODE
						//	 3 = PLACE
  var_u *rounam;				// routine name
  int line;					// line number
  int i;					// a handy int
  u_char *p;					// a handy pointer
  mvar *var;					// for ^$R()
  u_char temp[3*MAX_NAME_BYTES];		// ditto
  cstring *cptr;				// ditto
  short s;					// ditto

  ret_buffer[0] = '\0';				// null terminate
  if (level < 0) return 0;			// junk
  i = systab->jobtab[job].cur_do;		// default
  if (systab->jobtab[job].error_frame > systab->jobtab[job].cur_do)
    i = systab->jobtab[job].error_frame;	// ensure we have the error bit
  if (level > i) return 0;			// nothing there
  if (strncasecmp((const char *) code->buf, "ecode\0", 6) == 0) arg2 = 1;
  else if (strncasecmp((const char *) code->buf, "mcode\0", 6) == 0) arg2 = 2;
  else if (strncasecmp((const char *) code->buf, "place\0", 6) == 0) arg2 = 3;
  else return (-(ERRZ50+ERRMLAST));		// junk
  if (arg2 == 1)				// "ECODE"
  { ret_buffer[0] = '\0';			// assume nothing
    if (job != MV1_PID) return (0); // can't find
    var = (mvar *) ret_buffer;			// use same space for mvar
    X_set("$ECODE\0\0", &var->name.var_cu[0], 8);// copy in $ECODE
    var->nsubs = 255;
    var->volset = 0;
    var->uci = UCI_IS_LOCALVAR;
    cptr = (cstring *) temp;			// some spare space
    cptr->len = itocstring(cptr->buf, level);	// setup for subscript
    s = UTIL_Key_BuildEx(var, cptr, &var->key[0]);
    if (s < 0) return s;                        // got an error
    var->slen = s;
    s = ST_Get(var, ret_buffer);		// get and return
    if (s == -ERRM6) s = 0;			// allow for not there
    return s;
  }
  if ((level == systab->jobtab[job].error_frame) &&
      (level)) level = STM1_FRAME; 		// err frame adjust
  if ((((systab->jobtab[job].dostk[level].type & 127) == TYPE_XECUTE) ||
       ((systab->jobtab[job].dostk[level].type & 127) == TYPE_RUN) ||
       ((systab->jobtab[job].dostk[level].type & 127) == TYPE_JOB)) &&
       //(systab->jobtab[job].dostk[level].rounam.var_qu == 0))
       (X_Empty(systab->jobtab[job].dostk[level].rounam.var_xu)))
  { if (arg2 == 2)				// "MCODE"
    { ret_buffer[0] = '\0';			// JIC
      if (systab->jobtab[job].cur_do < level) return 0; // no can do
      if (job != MV1_PID) return (0); // can't find
      p = (u_char *)systab->jobtab[job].dostk[level].routine;
      if (p == NULL) return 0;			// nothing there
      for (i = 0; ((ret_buffer[i] = p[i])); i++); // copy it
      return i;					// return the count
    }
    return mcopy((u_char *) "XECUTE", ret_buffer, 6);	// "PLACE"
  }
  rounam = &(systab->jobtab[job].dostk[level].rounam); // point at routine name
  line = systab->jobtab[job].dostk[level].line_num; // get line number
  if (arg2 == 2)				// "MCODE"
  { var = (mvar *) ret_buffer;			// use same space for mvar
    var->nsubs = 255;
    X_set("$ROUTINE", &var->name.var_cu[0], 8); // copy in $ROUTINE
    var->volset = systab->jobtab[job].rvol;	// vol number
    var->uci = systab->jobtab[job].ruci;	// uci number
    if (rounam->var_cu[0] == '%')               // check for a percent rou
    { var->uci    = 1; 
      var->volset = 1;
    }	
    cptr = (cstring *) temp;			// some spare space
    for (i = 0; i < MAX_NAME_BYTES; i++)	// copy name
    { if (rounam->var_cu[i] == 0) break;	// quit when done
      cptr->buf[i] = rounam->var_cu[i];		// copy
    }
    cptr->buf[i] = '\0';			// null terminate
    cptr->len = i;				// save the length
    s = UTIL_Key_BuildEx(var, cptr, &var->key[0]); // make a key from it
    if (s < 0) return s;			// die on error
    var->slen = (u_char) s;			// save the length
    cptr->len = itocstring(cptr->buf, line);	// make a string from int
    s = UTIL_Key_BuildEx(var, cptr, &var->key[var->slen]);// make a key from it
    if (s < 0) return s;			// die on error
    var->slen = (u_char) s + var->slen;		// save the length
    s = Dget1(ret_buffer, var);			// get data
    if (s < 0) s = 0;				// ignore errors
    ret_buffer[s] = '\0';			// null terminate
    return s;					// and return
  }
  i = 0;					// the start
  ret_buffer[i++] = '+';			// add plus
  i = i + itocstring(&ret_buffer[i], line);	// add the line number
  ret_buffer[i++] = '^';			// the name indicator
  for (arg2 = 0; arg2 < MAX_NAME_BYTES; arg2++)	// copy name
    if ((ret_buffer[i++] = rounam->var_cu[arg2]) == 0) break;
  if (ret_buffer[i-1] == '\0') i--;		// back up over null
  ret_buffer[i] = '\0';				// null terminate
  return i;  					// return length
}

//***********************************************************************
// $TEXT(entryref)
//
// the entire string "entryref" is passed in one variable, eval it here
//
short Dtext(u_char *ret_buffer, cstring *str)	// $TEXT()
{ int i = 0;					// a handy int
  int j = 0;					// and another
  u_char slen;					// saved length
  u_char nsubs;                                 // saved nsubs
  short s;					// for functions
  int off = 1;					// line offset
  u_char rou[4+MAX_NAME_BYTES];			// routine name
  u_char tag[4+MAX_NAME_BYTES];			// the tag
  cstring *cr;					// the rou
  cstring *ct;					// and the tag

  ret_buffer[0] = '\0';				// JIC
  ct = (cstring *) &tag[0];			// use it this way
  cr = (cstring *) &rou[0];			// ditto  
  ct->len = 0;					// assume no tag
  cr->len = 0;					// no routine for now

  if (bcmp("+0\0", str->buf, 3) == 0)		// $T(+0) ?
  { for (i = 0; i < MAX_NAME_BYTES; i++)		// copy rou name
    { if (!partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[i])
        break;					// quit when done
      ret_buffer[i] = 
        partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[i]; // copy
    }
    ret_buffer[i] = '\0';			// null terminate
    return (short) i;				// and exit
  }
  if ((str->buf[i] != '+') && (str->buf[i] != '^')) // is there a tag
  { while (j < MAX_NAME_BYTES)
    { if ((i == 0) && (str->buf[i] == '%'))	// leading %
      { ct->buf[j++] = str->buf[i++];		// copy it
        continue;				// and go for more
      }
      if (isalnum(str->buf[i]) == 0) break;	// done
      ct->buf[j++] = str->buf[i++];		// copy it
    }
    ct->buf[j] = '\0';				// null terminate tag
    ct->len = j;				// save the length
    off = 0;					// change offset to zero
    while ((str->buf[i] != '+') &&
	   (str->buf[i] != '^') &&
	   (str->buf[i] != '\0')) i++;		// skip to + ^ or null
  }						// end tag processing
  if (str->buf[i] == '+')			// if we have a plus
  { off = 0;					// clear offset
    i++;					// skip the +
    while (isdigit(str->buf[i]) != 0)		// for all digits
      off = (off * 10) + (str->buf[i++] - '0');	// extract the offset
  }						// end offset stuf
  if ((str->buf[i] != '^') && (str->buf[i] != '\0'))
    return -(ERRMLAST + ERRZ12);		// complain
  j = 0;					// clear rou ptr
  if (str->buf[i] == '^')			// routine name
  { i++;					// skip the ^
    while (j < MAX_NAME_BYTES)
    { if ((j == 0) && (str->buf[i] == '%'))	// leading %
      { cr->buf[j++] = str->buf[i++];		// copy it
        continue;				// and go for more
      }
      if (isalnum(str->buf[i]) == 0) break;	// done
      cr->buf[j++] = str->buf[i++];		// copy it
    }
    cr->buf[j] = '\0';				// null terminate rou
    cr->len = j;				// save the length
  }
  else						// we need the current routine
  { for (j = 0; j < MAX_NAME_BYTES; j++)
      if ((cr->buf[j] = 
            partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[j])
		== '\0') break;			// copy till done
    cr->buf[j] = '\0';				// null terminate rou
    cr->len = j;				// save the length
  }
  if (cr->len == 0) return 0;			// no routine supplied -> null
  if ((ct->len == 0) && (!off))			// just the name reqd?
    return mcopy(cr->buf, ret_buffer, cr->len);	// return the name
  X_set("$ROUTINE", &partab.src_var.name.var_cu[0], 8); // setup for DB_Get
  partab.src_var.nsubs  = 255;
  partab.src_var.volset = partab.jobtab->rvol;	// vol
  partab.src_var.uci = partab.jobtab->ruci;	// uci
  if (cr->buf[0] == '%')			// manager routine?
  { partab.src_var.uci = 1;			// point there
    partab.src_var.volset = 1;
  }
  partab.src_var.slen = 0;			// init key size
  s = UTIL_Key_BuildEx(&partab.src_var, cr, &partab.src_var.key[0]);// first key
  if (s < 0) return s;				// die on error
  slen  = s;					// save key size
  nsubs = partab.src_var.nsubs;                 // save nsubs
  if (ct->len == 0)				// no tag?
  { ct->len = itocstring(ct->buf, off);		// cstring off
    s = UTIL_Key_BuildEx(&partab.src_var, ct,
		       &partab.src_var.key[slen]); // next key
    if (s < 0) return s;			// die on error
    partab.src_var.slen = s + slen;		// save key size
    s = DB_Get(&partab.src_var, ret_buffer);	// get it
    if (s < 0)
    { ret_buffer[0] = '\0';			// nothing
      s = 0;					// zero length
    }
    return s;					// and return it
  }
  for (j = 1; ; j++)				// need to read all lines
  { cr->len = itocstring(cr->buf, j);		// cstring j
    partab.src_var.slen  = slen;
    partab.src_var.nsubs = nsubs; 
    s = UTIL_Key_BuildEx(&partab.src_var, cr,
		       &partab.src_var.key[slen]); // next key
    if (s < 0) return s;			// die on error
    partab.src_var.slen = s + slen;		// save key size
    s = DB_Get(&partab.src_var, ret_buffer);	// get it
    if (s < 0)
    { ret_buffer[0] = '\0';			// nothing
      return 0;					// zero length
    }
    for (i = 0; i < ct->len; i++)		// check the tag
      if (ret_buffer[i] != ct->buf[i]) break;	// quit if different
    if (i < ct->len) continue;			// go for next if no match
    if ((ret_buffer[i] != ' ') &&		// must be space
        (ret_buffer[i] != '(') &&		//	or (
	(ret_buffer[i] != '\0')) continue;	//	or null
    if (off == 0) return s;			// no offset - all done
    j = j + off;				// add the offset
    cr->len = itocstring(cr->buf, j);		// cstring j
    partab.src_var.slen  = slen;
    partab.src_var.nsubs = nsubs;
    s = UTIL_Key_BuildEx(&partab.src_var, cr,
		       &partab.src_var.key[slen]); // next key
    if (s < 0) return s;			// die on error
    partab.src_var.slen = s + slen;		// save key size
    s = DB_Get(&partab.src_var, ret_buffer);	// get it
    if (s < 0)
    { ret_buffer[0] = '\0';			// nothing
      s = 0;					// zero length
    }
    return s;					// done
  }
}

//***********************************************************************
// $TRANSLATE(expr1,expr2[,expr3])
//
short Dtranslate2(u_char *ret_buffer, cstring *expr1, cstring *expr2)
{ short s = 0;                                  // arg 3
  return Dtranslate3(ret_buffer, expr1, expr2, (cstring *) &s);
}

short Dtranslate3(u_char *ret_buffer, cstring *expr1,
                 cstring *expr2, cstring *expr3)
{ int i1;                                       // for expr1
  int i2;                                       // for expr2
  int p = 0;                                    // ptr to ret_buffer
  int found;                                    // did we find that one
  u_char c2, c3;				// for expr2 and expr3 char

  if (1 == expr2->len)				// special cases
  { c2 = expr2->buf[0];				// save char
    if (0 == expr3->len)			// no expr3 given, remove chars
    { for (i1=0; i1 != expr1->len; i1++)        // scan expr1
      { if (expr1->buf[i1] != c2)     		// if we have no match
          ret_buffer[p++] = expr1->buf[i1]; 	// copy character
      }
    } else
    { c3 = expr3->buf[0];			// expr3 is at least 1 char
      for (i1=0; i1 != expr1->len; i1++)        // scan expr1
      { if (expr1->buf[i1] == c2)     		// if we have a match
          ret_buffer[p++] = c3;     		// copy in replacement char
	else
          ret_buffer[p++] = expr1->buf[i1]; 	// copy character
      }
    }
  }
  else
  { for (i1=0; i1 != expr1->len; i1++)            // scan expr1
    { found = FALSE;                              // assume no match
      for (i2=0; i2 != expr2->len; i2++)          // scan expr2 for char
      { if (expr1->buf[i1] == expr2->buf[i2])     // if we have a match
        { found = TRUE;                           // say so
          if (i2 < expr3->len)                    // if there is a match in expr3
            ret_buffer[p++] = expr3->buf[i2];     // copy in replacement char
          break;                                  // and quit this loop
        }
      }
      if (!found) ret_buffer[p++] = expr1->buf[i1]; // copy character
    }
  }
  ret_buffer[p] = '\0';                         // terminate it
  return p;                                     // and return count
}

//***********************************************************************
// $VIEW(channel#,location[,size[,value]])
//
short Dview(u_char *ret_buffer, int chan, int loc,
            int size, cstring *value)
{ int i;					// a handy int
  u_char *vb;					// view buffer address

  if (chan > -1) return -(ERRMLAST+ERRZ63);	// must be negative for now
  chan = (-chan) - 1;				// negate it and 0 base
  if (partab.jobtab->view[chan] == NULL)	// got a block
    return -(ERRMLAST+ERRZ63);			// no - die
  vb = (u_char *) partab.jobtab->view[chan]->mem; // get block mem address
  if ((loc < 0) || 
      (size < 1) ||
      ((loc + size) > systab->vol[chan]->vollab->block_size))
    return -(ERRMLAST+ERRZ63);			// out of range - die
  vb = vb + loc;				// offset to locn
  if (value == NULL)				// a read?
  { if (size == 1)
      return itocstring(ret_buffer, *vb);	// one byte
    if (size == 2)
      return itocstring(ret_buffer, *((u_short *) vb)); // two bytes
    if (size == 4)
      return itocstring(ret_buffer, *((u_int *) vb)); // four bytes
    return mcopy(vb, ret_buffer, size);		// return the string
  }
  ret_buffer[0] = '\0';				// null terminate
  if ((size == 1) || (size == 2) || (size == 4)) // int type?
  { i = cstringtoi(value);			// make int of it
    if (size == 1) *vb = (u_char) i;
    else if (size == 2) *((u_short *) vb) = (u_short) i;
    else *((u_int *) vb) = i;			// set some int type
  }
  else
  { if (size != value->len) return -(ERRMLAST+ERRZ63); // junk
    bcopy(value->buf, vb, size);		// copy whatever
  }
  return 0;					// return OK
}

//***********************************************************************
// set $PIECE
//
short DSetpiece(u_char *tmp, cstring *cptr, mvar *var,
		cstring *dptr, int i1, int i2)	// Set $PIECE()
{ cstring *vptr;				// where the variable goes
  short s;					// for the functions
  int beg = 0;                                  // start copy from
  int end;                                      // copy to
  int pce = 1;                                  // current piece
  int f;                                        // found flag
  int j;                                        // for delim scan
  int np;					// number of pieces

  if (i1 < 1) i1 = 1;				// ensure i1 positive
  if (i1 > i2) return 0;			// ignore that, it's junk
  vptr = (cstring *) tmp;			// where it goes
  s = Dget1(vptr->buf, var);			// get current value
  if (s < 0) return s;				// die on error
  vptr->len = s;				// save the size
  if (dptr->len == 0)				// null delimiter ?
  { s = mcopy(cptr->buf, &vptr->buf[vptr->len], cptr->len); // copy at end
    if (s < 0) return s;			// die on error
    vptr->len = vptr->len + cptr->len;		// the new length
    if (var->uci == UCI_IS_LOCALVAR)
      return ST_Set(var, vptr);			// set it back and return
    return DB_Set(var, vptr);		        // set it back and return
  }
  np = Dlength2x(vptr, dptr);			// get number of pieces
  if (np < i1)					// current < = start
  { f = i1 - np;				// delimiters required
    for (j = 0; j < f; j++)			// for each required delimiter
    { s = mcopy(dptr->buf, &vptr->buf[vptr->len], dptr->len); // copy 1 delim
      if (s < 0) return s;			// check for overflow
      if ((vptr->len+s)>MAX_STR_LEN) return -ERRM75;
      vptr->len += s;				// add to length
    }
    s = mcopy(cptr->buf, &vptr->buf[vptr->len], cptr->len); // copy in source
    vptr->len += s;				// add to length
    if (var->uci == UCI_IS_LOCALVAR)
      return ST_Set(var, vptr);			// set it back and return
    return DB_Set(var, vptr);		        // set it back and return
  }
  for (end = 0; end < vptr->len; end++)         // scan expr
  { if (vptr->buf[end] == dptr->buf[0])         // if first char matches
    { f = 1;                                    // set found flag
      for (j = 1; j < dptr->len; j++)           // scan rest of delimiter
      { if (vptr->buf[end+j] != dptr->buf[j])   // if we have a mismatch
        { f = 0;                                // clear found flag
          break;                                // and quit
        }
      }                                         // end delim scan
      if (f == 1)                               // just quit the if on fail
      { if (pce == i2)                          // if this is last piece
        { end--;                                // point at last reqd char
          break;                                // and quit for loop
        }                                       // end last piece processing
        pce++;                                  // increment current piece
        end = end + dptr->len - 1;              // point at last char of delim
        if (pce == i1) beg = end + 1;		// if this is the first pce
      }                                         // end found code
    }                                           // end of got match
  }                                             // end of expr scan
  if (np == i1)					// replace last piece
  { s = mcopy(cptr->buf, &vptr->buf[beg], cptr->len); // copy it
    vptr->len = beg + cptr->len;		// fixup length
    if (var->uci == UCI_IS_LOCALVAR)
      return ST_Set(var, vptr);			// set it back and return
    return DB_Set(var, vptr);		        // set it back and return
  }
  if (end >= vptr->len) end = vptr->len - 1;	// don't point past end
  i1 = beg;					// start of cut
  i2 = end;					// end of cut
  if ((i2 - i1 + 1) != cptr->len)		// not an exact fit?
  { s = mcopy(&vptr->buf[i2 + 1],		// move tail from here
	      &vptr->buf[i1 + cptr->len],	// to here
	      vptr->len - i2 + 2);		// this many bytes
    if (s < 0) return s;			// check overflow
  }
  if (cptr->len)
    bcopy(cptr->buf, &vptr->buf[i1], cptr->len); // can't use mcopy() here
  vptr->len = vptr->len - (i2 - i1 + 1) + cptr->len;
  if (var->uci == UCI_IS_LOCALVAR)
    return ST_Set(var, vptr);			// set it back and return
  return DB_Set(var, vptr);			// set it back and return
}

//***********************************************************************
// set $EXTRACT
//
short DSetextract(u_char *tmp, cstring *cptr, mvar *var,
		  int i1, int i2)		// Set $EXTRACT()
{ cstring *vptr;				// where the variable goes
  short s;					// for the functions
  int i;					// a handy int

  if (i1 < 1) i1 = 1;				// ensure i1 positive
  if (i1 > i2) return 0;			// ignore that, it's junk
  if (i2 > MAX_STR_LEN) return -ERRM75;		// complain if too long
  vptr = (cstring *) tmp;			// where it goes
  s = Dget1(vptr->buf, var);			// get current value
  if (s < 0) return s;				// die on error
  vptr->len = s;				// save the size
  for (i = s; i < i1; vptr->buf[i++] = ' ');	// ensure enough spaces
  if (s <= i2)					// if no trailing left
  { s = mcopy(cptr->buf, &vptr->buf[i1 - 1], cptr->len); // copy it in
    if (s < 0) return s;			// check for overflow
    vptr->len = i1 - 1 + cptr->len;		// the new length
    if (var->uci == UCI_IS_LOCALVAR)
      return ST_Set(var, vptr);			// set it back and return
    return DB_Set(var, vptr);		        // set it back and return
  }
  if ((i2 - i1 + 1) != cptr->len)		// not an exact fit?
  { s = mcopy(&vptr->buf[i2],			// move tail from here
	      &vptr->buf[i1 - 1 + cptr->len],	// to here
	      vptr->len - i2 + 2);		// this many bytes
    if (s < 0) return s;			// check overflow
  }
  bcopy(cptr->buf, &vptr->buf[i1 - 1], cptr->len); // can't use mcopy() here
  vptr->len = vptr->len - (i2 - i1 + 1) + cptr->len;
  if (var->uci == UCI_IS_LOCALVAR)
    return ST_Set(var, vptr);			// set it back and return
  return DB_Set(var, vptr);			// set it back and return
}

//***********************************************************************
// $ZBITSTR(len[,ff])
//
short Dzbitstr2(u_char *ret, int len, int ff)
{ if (len < 0)                                  // invalid arg
    return -(ERRMLAST+ERRZ74);
  ret[0] = len & 7;                             // trailing bit count
  len = (len + 7) >> 3;                         // round up to byte boundary
  if (1 + len > MAX_STR_LEN)                    // bitstr does not fit
    return -ERRM75;                             //   error
  memset(&ret[1], ff ? 255 : 0, len);           // set/clear contents
  ret[len] &= BitMask[ret[0]];          	// mask trailing byte
  return (short)(1 + len);                      // return length
}

short Dzbitstr(u_char *ret, int len)
{ return Dzbitstr2(ret, len, 0);                // no flag, assume 0
}

//***********************************************************************
// $ZBITLEN(bstr)
//
int Dzbitlen(cstring *bstr)
{ int len;

  if ((0 == bstr->len) || (7 < bstr->buf[0]))   // empty string, or invalid
    return -(ERRMLAST+ERRZ75);                  //   trailing bit count
  len = bstr->len - 1;
  if (bstr->buf[0])
    len--;
  return (len << 3) + bstr->buf[0];
}

//***********************************************************************
// $ZBITCOUNT(bstr)
//
int Dzbitcount(cstring *bstr)
{ int i, len, count;
  u_char byt;

  len = Dzbitlen(bstr);                         // check bit string
  if (0 > len)
    return len;

  count = 0;                                    // init counter
  for (i = 1; i < bstr->len - 1; i++)
  { byt = bstr->buf[i];
    count += Bits[byt].bit1count;               // count 1s
  }
  byt = BitMask[bstr->buf[0]] & bstr->buf[bstr->len - 1]; // handle
  count += Bits[byt].bit1count;                 //  trailing bits

  return count;
}

//***********************************************************************
// $ZBITGET(bstr,pos)
//
short Dzbitget(cstring *bstr, int pos)
{ int len;
  u_char byt, mask;

  len = Dzbitlen(bstr);                         // check bit string
  if (0 > len)
    return len;
  if (1 > pos)
    return -(ERRMLAST+ERRZ74);                  // invalid arg
  if (pos-- > len)                              // pos is larger than length
    return 0;                                   //   assume 0
  byt  = bstr->buf[1 + (pos >> 3)];             // byte storing the bit
  mask = 1 << (7 - (pos & 7));                  // bit position mask
  return byt & mask ? 1 : 0;
}

//***********************************************************************
// $ZBITSET(bstr,pos,ff)
//
short Dzbitset(u_char *ret, cstring *bstr, int pos, int ff)
{ int len;
  u_char byt, mask;
  int retlen;

  len = Dzbitlen(bstr);                         // check bit string
  if (0 > len)
    return len;
  if (1 > pos)
    return -(ERRMLAST+ERRZ74);                  // invalid arg
  bcopy(&bstr->buf[0], &ret[0], bstr->len);     // copy old to new
  retlen = bstr->len;
  if (pos > len)                                // enlarge bitstr
  { retlen = 1 + ((pos + 7) >> 3);              // calc. new len
    if (retlen > bstr->len)                     // needs a larger one
    { if (retlen > MAX_STR_LEN)                 // error, does not fit
        return -ERRM75;
      memset(ret + bstr->len, 0, retlen - bstr->len); // clear tail
    }
    ret[0] = pos & 7;                           // set trailing bit cnt
  }
  byt  = ret[1 + (--pos >> 3)];           	// byte storing bit
  mask = 1 << (7 - (pos & 7));                  // bit mask
  byt &= ~mask;                                 // clear bit
  if (ff) byt |= mask;                          // if 1, then set
  ret[1 + (pos >> 3)] = byt;                    // write back result
  return (short) retlen;
}

//***********************************************************************
// $BZITFIND(bstr,ff[,pos])
//
int Dzbitfind3(cstring *bstr, int ff, int pos)
{ int i;
  int len;
  u_char byt, bit1pos, bit0pos;

  len = Dzbitlen(bstr);                         // check bit string
  if (0 > len)
    return len;
  if (pos > len)                                // at end ?
    return 0;
  if (pos-- < 1)                                // check pos
    return -(ERRMLAST+ERRZ74); 
  if (ff)                                       // search for 1
  { if (pos & 7)                                // leading bits
    { byt = bstr->buf[1 + (pos >> 3)] << (pos & 7); // shift byte left
      bit1pos = Bits[byt].bit1pos;              // get leading '1's position
      if (bit1pos)                              //   if has a '1'
        return bit1pos + pos;                   // return pos after the '1'
      pos += 8 - (pos & 7);                     // bump pos to nxt byte boundary
    }
    for (i = 1 + (pos >> 3); i < bstr->len - 1; i++) // check bytes
    { byt = bstr->buf[i];
      if (0 == byt)                             // all '0', so skip
      { pos += 8;
        continue;
      }
      return Bits[byt].bit1pos + pos;           // return leading '1's position
    }
    byt = bstr->buf[bstr->len - 1];             // trailing byte
    bit1pos = Bits[byt].bit1pos;                // get leading '1's position
    if (bit1pos)                                // if has a '1' return
      return bit1pos + pos;                     //   pos after the '1'
  }
  else                                          // search for 0
  { if (pos & 7)                                // handle leading bits
    { byt = bstr->buf[1 + (pos >> 3)] << (pos & 7); // shift byte left
      bit0pos = Bits[byt].bit0pos;              // get leading pos of '0'
      if (bit0pos)                              // if it has a '0'
        return bit0pos + pos;                   //   return next pos after '0'
      pos += 8 - (pos & 7);                     // bump pos to byte boundary
    }
    for (i = 1 + (pos >> 3); i < bstr->len - 1; i++) // for each byte
    { byt = bstr->buf[i];
      if (255 == byt)                           // all '1's, skip
      { pos += 8;
        continue;
      }
      return Bits[byt].bit0pos + pos;           // return next pos after the '0'
    }
    byt = bstr->buf[bstr->len - 1];             // check trailing byte
    bit0pos = Bits[byt].bit0pos;                // get leading pos of '0'
    if (bit0pos)                                // if it has a '0'
      return bit0pos + pos;                     //   return next pos after '0'
  }
  return 0;                                     // bit not found, return 0
}

int Dzbitfind2(cstring *bstr, int ff)
{ return Dzbitfind3(bstr, ff, 1);
}

//***********************************************************************
// $ZBITNOT(bstr)
//
short Dzbitnot(u_char *ret, cstring *bstr)
{ int i;

  bcopy(&bstr->buf[0], &ret[0], bstr->len);     // init result with arg

  for (i = 1; i < bstr->len; i++)               // negate bits
    ret[i] = ~ret[i];
  ret[bstr->len - 1] &= BitMask[ret[0]];        // mask trailing byte

  return bstr->len;
}

//***********************************************************************
// $ZBITAND(bstr1,bstr2)
//
short Dzbitand(u_char *ret, cstring *bstr1, cstring *bstr2)
{ int i, bitlen, bitlen2;
  short len;

  len = bstr1->len;                             // assume bitstring 1 is shorter
  bitlen = Dzbitlen(bstr1);                     // check bit string 1
  if (0 > bitlen)
    return bitlen;
  bitlen2 = Dzbitlen(bstr2);                    // check bit string 2
  if (0 > bitlen2)
    return bitlen2;

  if (bitlen2 < bitlen)                         // result length is smaller of
  { bitlen = bitlen2;                           //   bit string 1 & bit string 2
    len = bstr2->len;
  }

  for (i = 1; i < len; i++)
    ret[i] = bstr1->buf[i] & bstr2->buf[i];     // result is bitwise AND of args

  ret[0]        = bitlen & 7;                   // set result trailing count
  ret[len - 1] &= BitMask[ret[0]];              // mask trailing byte

  return len;
}

//***********************************************************************
// $ZBITOR(bstr1,bstr2)
//
short Dzbitor(u_char *ret, cstring *bstr1, cstring *bstr2)
{ int i, bitlen, bitlen2;
  short len;

  len    = bstr1->len;
  bitlen = Dzbitlen(bstr1);                     // check bit string 1
  if (0 > bitlen)
    return bitlen;
  bitlen2 = Dzbitlen(bstr2);                    // check bit string 2
  if (0 > bitlen2)
    return bitlen2;

  if (bitlen2 > bitlen)                         // bit string 1 is shorter
  { for (i = 1; i < len; i++)
    { ret[i] = bstr1->buf[i] | bstr2->buf[i];
    }
    bcopy(&bstr2->buf[len], &ret[len], bstr2->len - len); // add trailing bits
    len = bstr2->len;                           //  from bit string2
    bitlen = bitlen2;                           // result length is the length
                                                //   of bit string 2
  }
  else 
  { len = bstr2->len;                           // bit string2 is shorter or eq.
    for (i = 1; i < len; i++)
      ret[i] = bstr1->buf[i] | bstr2->buf[i];
    bcopy(&bstr1->buf[len], &ret[len], bstr1->len - len); // add trailing bits
    len = bstr1->len;                           //  from bit string 1
  }

  ret[0]        = bitlen & 7;                   // set length
  ret[len - 1] &= BitMask[ret[0]];              // mask trailing byte

  return len;
}

//***********************************************************************
// $ZBITXOR(bstr1,bstr2)
//
short Dzbitxor(u_char *ret, cstring *bstr1, cstring *bstr2)
{ int i, bitlen, bitlen2;
  short len, len2;

  len    = bstr1->len;                          // assume bit string 1 is longer
  bitlen = Dzbitlen(bstr1);                     // check bit string
  if (0 > bitlen)
    return bitlen;
  len2   = bstr2->len;
  bitlen2 = Dzbitlen(bstr2);                    // check bit string
  if (0 > bitlen2)
    return bitlen2;

  if (len2 == len)				// same length
  { for (i = 1; i < len; i++)
      ret[i] = bstr1->buf[i] ^ bstr2->buf[i];   // result is bitwise XOR of args
  }
  else if (len2 < len)				// bstr2 is shorter
  { for (i = 1; i < len2; i++)
      ret[i] = bstr1->buf[i] ^ bstr2->buf[i];
    for (i = len2; i < len; i++)
      ret[i] = bstr1->buf[i] ^ 0;
  }
  else						// bstr1 is shorter
  { for (i = 1; i < len; i++)
      ret[i] = bstr1->buf[i] ^ bstr2->buf[i];
    for (i = len; i < len2; i++)
      ret[i] = 0 ^ bstr2->buf[i];
  }

  if (bitlen2 > bitlen)                         // result length is longer
  { len = len2;                           	//   bit string 1 & bit string 2
    bitlen = bitlen2;
  }

  ret[0]        = bitlen & 7;                   // set result length
  ret[len - 1] &= BitMask[ret[0]];              // mask trailing byte

  return len;
}

//***********************************************************************
// $ZINCR[EMENT](variable[,expr])
//

short Copy2local(mvar *var, char *rtn);		// in db_main.c

static
short Dzincrement2P(cstring *ret, mvar *var, cstring *expr)
{ short s;					// for return values 
  u_char num[128],temp[128];
  u_char *p;
  int fwd = -1;                                 // symtab position

  if (var->uci == UCI_IS_LOCALVAR)		// for a local var
  { s = ST_GetEx(var, ret->buf, &fwd);	        // attempt to get the data
    if (s >= 0) goto gotit;			// if we got data, return it
    if (s == -(ERRM6)) s = 0;			// flag undefined local var
  }
  else if (var->name.var_cu[0] == '$') 		// ssvn?
    return (-ERRM38);				// no such
  else						// for a global var
  { bcopy( var, &(partab.jobtab->last_ref), MVAR_SIZE + var->slen);
    s = Copy2local(var, "GET");			// setup DB global here
    if (s < 0)
    { return s;
    }

    if (systab->vol[volnum - 1]->flags & VOL_RDONLY)// read-only ?
    { return -(ERRZ93 + ERRMLAST);		//   exit on error
    }

    if (systab->vol[volnum-1]->local_name[0])  	// remote VOL ?
    { if (s > 0)
        return -(ERRM38);
      return DGP_ZIncrement(volnum-1, ret, &db_var, expr);
    }
    s = DB_GetEx(var, ret->buf, 1, s);	        // attempt to get the data
    // fprintf(stderr, "Dincrement: curr_lock=%d s=%d\r\n", curr_lock, s);
    if (s >= 0) goto gotit;			// if we got data, return it
    if (s == -(ERRM7)) s = 0;			// flag undefined global var
  }
  if (s != 0) goto errout;			// if an error, return it

gotit:
  ret->len = s;
  ret->buf[ret->len] = '\0';
  // fprintf(stderr, "Dincrement: len=%d buf=%s\r\n", ret->len, ret->buf);
  p = expr->buf;                                // make a number from expr
  s = ncopy(&p, temp);
  if (s < 0) goto errout;                       // got an error, bail out
  p = ret->buf;                                 // make a number from ret
  s = ncopy(&p, num);
  if (s < 0) goto errout;                       // got an error, bail out
  s = runtime_add((char *) num, (char *) temp); // add them together
  if (s < 0) goto errout;                       // check for error
  bcopy(num, ret->buf, s);                      // copy back the result
  ret->buf[s] = '\0';                           // terminate with zero
  ret->len = s;                                 // set length
  // fprintf(stderr, "Dincrement: len=%d buf=%s\r\n", ret->len, ret->buf);
  if (var->uci == UCI_IS_LOCALVAR)              // set as local
  { // fprintf(stderr, "Dincrement: set local\r\n");
    s = (0 <= fwd) ? ST_SetEx(fwd, var, ret) : ST_Set(var, ret);
  }
  else
  { // fprintf(stderr, "Dincrement: set global\r\n");
    s = DB_SetEx(var, ret, 1);                  // set as global
    // fprintf(stderr, "Dincrement: curr_lock=%d s=%d\r\n", curr_lock, s);
  }
  if (s < 0) goto errout;
  s = ret->len;		                        // and return the length

errout:
  // if (s < 0) fprintf(stderr,"errout: %d\r\n", s);
  if (curr_lock)
    SemOp( SEM_GLOBAL, -curr_lock);
  return s;
}

short Dzincrement1(cstring *ret, mvar *var)
{ u_char tmp[8];				// some space
  cstring *cptr;				// for the call
  cptr = (cstring *) tmp;			// point at the space
  cptr->len = 1;				// zero length
  cptr->buf[0] = '1';				// default one
  cptr->buf[1] = '\0';				// null terminated
  return Dzincrement2P(ret, var, cptr); 	// do it below
}

short Dzincrement2(cstring *ret, mvar *var, cstring *expr)
{ int is_seq;					// SEQUENCE flag
  int docopy;					// do a copy ?
  cstring cstr;					// temporary cstring
  int lenseq;					// length of SEQ reuested
  short s;					// status flag
  u_long now;					// current time stamp
  simple_mvar ref;

  is_seq = 0 == strncasecmp((char *) &expr->buf[0], "seq\0", 4);
  if ((var->uci != UCI_IS_LOCALVAR) && 		// not local
      (var->name.var_cu[0] != '$') &&		//   AND not ssvn
      is_seq)					//   AND SEQUENCE?
  { docopy = 1;					// assume no copy
    UTIL_Mvar2Simple(var, &ref);		// save var
    if ((0 == partab.lenseq) ||			// empty local SEQ?
	(ref.slen != partab.seqref.slen) ||	// OR different subscript len
        bcmp(&ref, &partab.seqref, SIMPLE_MVAR_SIZE + ref.slen)) // OR diff. SEQ
    { now = UTIL_GetMicroSec();			// current time stamp
      lenseq = 2;				// get 2 IDs
      if (partab.lastseq)			// got SEQ before?
      { lenseq = 10000 / (now - partab.lastseq);
        if (lenseq < 2) lenseq = 2;
      }
      cstr.len = sprintf((char *) &cstr.buf[0], "%d", lenseq);
      s = Dzincrement2P(ret, var, &cstr);	// increment counter
      if (s < 0) return s;			// failed?, return error
      partab.lenseq = lenseq;			// set SEQ length
						// calc. current value
      partab.curseq  = atoll((char *) &ret->buf[0]) - lenseq;
      partab.lastseq = now;			// save time stamp
      partab.seqref  = ref;			// save GVN ref
      docopy = 0;
    }
    if (docopy)
      bcopy( var, &(partab.jobtab->last_ref), MVAR_SIZE + var->slen);
    partab.lenseq--;				//   decr. count
    return sprintf((char *) &ret->buf[0],	// return current SEQ value
		   "%lld", 
		   partab.curseq++);
  }
  return Dzincrement2P(ret, var, expr);
}

//***********************************************************************
// $LIST(lst)
//
short Dlist3(u_char *ret, cstring *lst, int from, int to)
{ short s, i, len;
  u_char *eltpos, *frompos, *topos;

  if (0 == from)                                // if from is zero, eff. is one
    from = 1;
  if ((-1 > from) || (-1 > to))                 // from/to is less than -1
    return -(ERRMLAST+ERRZ74);                  //   flag error
  if ((0 == lst->len) ||                        // empty list or
      ((-1 != to) && (from > to)))              //   empty range
  { ret[0] = '\0';                              //     return empty list
    return 0;
  }

  i = 0; s = 0; frompos = 0; topos = 0;
  while (i < lst->len)
  { s++;
    eltpos = &lst->buf[i];                       // remember elt pos
    len = lst->buf[i++];
    if (127 < len)
    { len -= 128;
      len <<= 8;
      if (i < lst->len)
      { len += lst->buf[i++];
        if ((0 != len) && (len < 128))
          return -(ERRMLAST+ERRZ76);
      }
      else
        return -(ERRMLAST+ERRZ76);
    }
    if (from == s)                              // got to pos, flag as found
      frompos = eltpos;
    i += len;
    if (to == s)
    { topos = &lst->buf[i];
      goto done;
    }
  }
  if (i != lst->len)                            // check proper list
    return -(ERRMLAST+ERRZ76);

done:
  if (-1 == from)                               // EOL specified, set
  { from = s;                                   //   from/frompos
    frompos = eltpos;
  }
  if (-1 == to)                                 // EOL specified, set
  { to = s;                                     //   to/topos
    topos = &lst->buf[i];
  }
  if (!topos)                                   // topos is past EOL
  { to = s;                                     //   set to/topos
    topos = &lst->buf[i];
  }

  if (!frompos || (from > to))                  // from is past EOL, or empty
  { ret[0] = '\0';                              //   range, return empty list
    return 0;
  }

  bcopy(frompos, ret, topos - frompos);         // copy sublist
  return topos - frompos;
}

short Dlist2(u_char *ret, cstring *lst, int pos)
{ short s, i, len;
  short eltlen;
  u_char *eltpos;
  int found, undf;

  if (0 == pos)
    return -(ERRMLAST+ERRZ77);
  if (-1 > pos)
    return -(ERRMLAST+ERRZ74);

  i = 0; s = 0; found = 0;
  while (i < lst->len)
  { s++; undf = 0;
    len = lst->buf[i++];
    if (127 < len)
    { len -= 128;
      len <<= 8;
      if (i < lst->len)
      { len += lst->buf[i++];
        if ((0 != len) && (len < 128))
          return -(ERRMLAST+ERRZ76);
        undf = (0 == len);
      }
      else
        return -(ERRMLAST+ERRZ76);
    }
    eltlen = undf ? VAR_UNDEFINED : len;
    eltpos = &lst->buf[i];
    if (pos == s)                               // got to pos, flag as found
    { found = 1;
      goto found;
    }
    i += len;
  }
  if (i != lst->len)                            // check proper list
    return -(ERRMLAST+ERRZ76);

  if (s && (-1 == pos))                         // have elts and requested last
  { found = 1;                                  // mark as found
    goto found;
  }

  return -(ERRMLAST+ERRZ77);                    // not found, flag undefined err

found:
  if (VAR_UNDEFINED == eltlen)                  // undefined?
    return -(ERRMLAST+ERRZ77);                  //   return error

  bcopy(eltpos, ret, eltlen);                   // copy elt
  return eltlen;
}

short Dlist(u_char *ret, cstring *lst)
{ return Dlist2(ret, lst, 1);
}

//***********************************************************************
// $LISTBUILD(...)
//
short Dlistbuild(u_char *ret, cstring *arg)
{ short len;

  // fprintf(stderr, "Dlistbuild(): len=%d\r\n", arg->len);
  if (VAR_UNDEFINED == arg->len)
  { *ret++ = 128;
    *ret++ = 0;
    return 2;
  }
  else if (128 > arg->len)
  { *ret++ = arg->len;
    len = 1;
  }
  else
  { *ret++ = 128 + (arg->len / 256);
    *ret++ = arg->len & 255;
    len = 2;
  }
  bcopy(&arg->buf[0], ret, arg->len);
  return len + arg->len;
}

//***********************************************************************
// $LISTDATA(lst[,pos])
//
short Dlistdata2(cstring *lst, int pos)
{ short s, i, len;
  short eltlen;
  int undf;

  if (0 == pos)                                 // pos is zero, return zero
    return 0;

  if (-1 > pos)                                 // give a range errror
    return -(ERRMLAST+ERRZ74);

  i = 0; s = 0;
  while (i < lst->len)
  { s++; undf = 0;
    len = lst->buf[i++];
    if (127 < len)
    { len -= 128;
      len <<= 8;
      if (i < lst->len)
      { len += lst->buf[i++];
        if ((0 != len) && (len < 128))
          return -(ERRMLAST+ERRZ76);
        undf = (0 == len);
      }
      else
        return -(ERRMLAST+ERRZ76);
    }
    eltlen = undf ? VAR_UNDEFINED : len;
    if (pos == s)
      goto found;
    i += len;
  }
  if (i != lst->len)                            // check proper list
    return -(ERRMLAST+ERRZ76);

  if (s && (-1 == pos))                         // have elts and req. last
    return VAR_UNDEFINED == eltlen ? 0 : 1;     //   undefined ? then zero
                                                //   else one
  return 0;                                     // elt not found, then zero

found:
  if (VAR_UNDEFINED == eltlen)                  // elt found
    return 0;                                   //   undefined? then zero
  return 1;                                     // else one
}

short Dlistdata(cstring *lst)
{ return Dlistdata2(lst, 1);
}

//***********************************************************************
// $LISTFIND(lst,val[,after])
//
short Dlistfind3(cstring *lst, cstring *val, int after)
{ short s, i, len;
  short eltlen;
  u_char *eltpos;
  int undf;

  if (-1 == after)
    return 0;
  if (-1 > after)
    return -(ERRMLAST+ERRZ74);

  i = 0; s = 0;
  while (i < lst->len)
  { s++; undf = 0;
    len = lst->buf[i++];
    if (127 < len)
    { len -= 128;
      len <<= 8;
      if (i < lst->len)
      { len += lst->buf[i++];
        if ((0 != len) && (len < 128))
          return -(ERRMLAST+ERRZ76);
        undf = (0 == len);
      }
      else
        return -(ERRMLAST+ERRZ76);
    }
    eltlen = undf ? VAR_UNDEFINED : len;
    eltpos = &lst->buf[i];
    if (s > after)
    { if ((eltlen == val->len) &&
          (0 == bcmp(eltpos, &val->buf[0], eltlen)))
        return s;
    }
    i += len;
  }
  if (i != lst->len)
    return -(ERRMLAST+ERRZ76);

  return 0;
}

short Dlistfind2(cstring *lst, cstring *val)
{ return Dlistfind3(lst, val, 0);
}

//***********************************************************************
// $LISTGET(lst[,pos[,def])
//
short Dlistget3(u_char *ret, cstring *lst, int pos, cstring *def)
{ short s, i, len;
  short eltlen;
  u_char *eltpos;
  int found, undf;

  if ((-1 != pos) && (1 > pos))
    return -(ERRMLAST+ERRZ74);

  i = 0; s = 0; found = 0;
  while (i < lst->len)
  { s++; undf = 0;
    len = lst->buf[i++];
    if (127 < len)
    { len -= 128;
      len <<= 8;
      if (i < lst->len)
      { len += lst->buf[i++];
        if ((0 != len) && (len < 128))
          return -(ERRMLAST+ERRZ76);
        undf = (0 == len);
      }
      else
        return -(ERRMLAST+ERRZ76);
    }
    eltlen = undf ? VAR_UNDEFINED : len;
    eltpos = &lst->buf[i];
    if (pos == s)                               // got to pos, flag as found
    { found = 1;
      goto found;
    }
    i += len;
  }
  if (i != lst->len)                            // check proper list
    return -(ERRMLAST+ERRZ76);
  if (s && (-1 == pos))                         // have elts and requested last
    found = 1;                                  // mark as found

found:
  if (!found || (VAR_UNDEFINED == eltlen))      // not found or UNDF
  { bcopy(&def->buf[0], ret, def->len);         //   copy default
    eltlen = def->len;
  }
  else
    bcopy(eltpos, ret, eltlen);                 // found and not UNDF, copy elt
  return eltlen;
}

short Dlistget2(u_char *ret, cstring *lst, int pos)
{ cstring *tmp;
  char buf[16];

  tmp = (cstring *)&buf[0];
  tmp->len = 0;
  tmp->buf[0] = '\0';
  return Dlistget3(ret, lst, pos, tmp);
}

short Dlistget(u_char *ret, cstring *lst)
{ return Dlistget2(ret, lst, 1);
}

//***********************************************************************
// $LISTLENGTH(lst)
//
short Dlistlength(cstring *lst)
{ short s, i, len;

  i = 0; s = 0;
  while (i < lst->len)
  { len = lst->buf[i++];
    if (127 < len)
    { len -= 128;
      len <<= 8;
      if (i < lst->len)
      { len += lst->buf[i++];
        if ((0 != len) && (len < 128))
          return -(ERRMLAST+ERRZ76);
      }
      else
        return -(ERRMLAST+ERRZ76);
    }
    i += len; s++;
  }
  return (i == lst->len) ? s : -(ERRMLAST+ERRZ76);
}

//***********************************************************************
// set $LIST
//
short DSetlist(u_char *tmp, cstring *cptr, mvar *var,
		  int from, int to)		// Set $LIST()
{ short s, i, len, eltcnt;
  u_char *eltpos, *frompos, *topos;
  cstring *vptr;
  short var_undf = VAR_UNDEFINED;               // undefined var
  int disp = 0;                                 // trailing bytes disp.
  int trail = 0;                                // trail length

  if (0 == from)                                // if from is zero, eff. is one
    from = 1;
  if ((-1 > from) || (-1 > to))                 // from/to is less than -1
    return -(ERRMLAST+ERRZ74);                  //   flag error

  vptr = (cstring *) tmp;			// where it goes
  s = Dget1(vptr->buf, var);			// get current value
  if (s < 0) return s;				// die on error
  vptr->len = s;				// save the size

  i = 0; eltcnt = 0; frompos = 0; topos = 0;
  while (i < vptr->len)
  { eltcnt++;
    eltpos = &vptr->buf[i];                     // remember elt pos
    len = vptr->buf[i++];
    if (127 < len)
    { len -= 128;
      len <<= 8;
      if (i < vptr->len)
      { len += vptr->buf[i++];
        if ((0 != len) && (len < 128))
          return -(ERRMLAST+ERRZ76);
      }
      else
        return -(ERRMLAST+ERRZ76);
    }
    if (from == eltcnt)                         // got to pos, flag as found
      frompos = eltpos;
    i += len;
    if (to == eltcnt)
    { topos = &vptr->buf[i];
      goto done;
    }
  }
  if (i != vptr->len)                           // check proper list
    return -(ERRMLAST+ERRZ76);

done:
  if (-1 == from)				// change only last elt.
  { from  = eltcnt;
    frompos = eltpos;
    topos   = &vptr->buf[vptr->len];
  }
  if (!frompos)					// append by default
  { frompos = &vptr->buf[vptr->len];
  }
  if ((-1 == to) && !topos)			// 2 argument form
  { topos = frompos;				//   change single elt.
  }
  if (!topos)					// append by default
  { topos = &vptr->buf[vptr->len];
  }
  trail = 0;                                    // assume empty trail
  len = Dlistlength(cptr);                      // assume cptr is a list
  if (topos < &vptr->buf[vptr->len])            // we have trailing bytes
  { if ((len < 0) ||				// value is not a list
	(-1 == to))                		//   OR 2 argument form
    { disp = cptr->len;                         // calc. displacement
      if (VAR_UNDEFINED == cptr->len) disp += 2;
      else if (128 > cptr->len)       disp += 1;
      else                            disp += 2;
    }
    else
      disp += cptr->len;                        // else use the list length
    if (eltcnt < from - 1)                      // check if padding necessary
      disp += 2 * (from - eltcnt - 1);
    if ((frompos - &vptr->buf[0]) + disp +      // check string length
        (&vptr->buf[vptr->len] - topos) > MAX_STR_LEN)
      return -ERRM75;
    trail = &vptr->buf[vptr->len] - topos;      // calc. trail length
    bcopy(topos, frompos + disp, trail);        // shift trailing bytes up
  }
  if (eltcnt < from - 1)                        // padding is necessary ?
  { for ( ; eltcnt < from - 1; eltcnt++)        //   append UNDFs
    { s = Dlistbuild(&vptr->buf[vptr->len], (cstring *)&var_undf);
      if ((s < 0) || (vptr->len + s > MAX_STR_LEN)) // check string length
        return -ERRM75;                         // if doesn't fit, error
      vptr->len += s;                           // update result length
    }
  }
  else
    vptr->len = frompos - &vptr->buf[0];        // adjust result length
  if ((len < 0) ||				// value is not a list
      (-1 == to))                  		//   OR 2 argument form
  { s = Dlistbuild(&vptr->buf[vptr->len], cptr);// form a list from
    if ((s < 0) || (s + vptr->len > MAX_STR_LEN)) // check string length
      return -ERRM75;                           // if doesn't fit, error
    vptr->len += s;                             // update result length
  }
  else                                          // value is a list, append as is
  { bcopy(&cptr->buf[0], &vptr->buf[vptr->len], cptr->len);
    vptr->len += cptr->len;                     // update result length
  }
  if (trail)                                    // trail present ?
  { if (vptr->len + trail > MAX_STR_LEN)        // check string length
      return -ERRM75;                           // if doesn't fit, error
    vptr->len += trail;                         // adjust result length
  }

  if (var->uci == UCI_IS_LOCALVAR)              // write back result
    return ST_Set(var, vptr);			// set it back and return
  return DB_Set(var, vptr);			// set it back and return
}
