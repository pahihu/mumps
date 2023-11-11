// File: mumps/runtime/runtime_ssvn.c
//
// module runtime - RunTime Variables

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
#include <unistd.h>				// usually this too
#include <sys/types.h>                          // for u_char def
#include <sys/ipc.h>                            // shared memory
#include <sys/shm.h>                            // shared memory
#include <sys/sem.h>                            // semaphores
#include <signal.h>				// for kill()
#include <pwd.h>				// for user name
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>                              // error stuf
#include <time.h>                               // for $H
#include <sys/time.h>				// for priority
#include <sys/resource.h>			// ditto
#include <fcntl.h>                              // for open()
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"				// standard errors
#include "database.h"
#include "dgp.h"

// SSVNs use the same structures as ST_ and DB_ functions
// Note valid SSVNs are:	$GLOBAL
//				$JOB
//				$LOCK
//				$ROUTINE
//				$SYSTEM

//***********************************************************************
//

int priv()					// return TRUE if job has priv
{ return ((partab.jobtab->priv) ||
   (partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[0] == '%'));
			       			// is it priveleged ?
}

short SS_Norm(mvar *var)			// "normalize" ssvn
{ int i;					// for loops

  for (i=0; i<MAX_NAME_BYTES; i++)		// scan the supplied name
    var->name.var_cu[i] = toupper(var->name.var_cu[i]);	// copy to upper case
  switch (var->name.var_cu[1])			// check initial of name
  { case 'G':					// $GLOBAL
      if ((var->name.var_cu[2] == '\0')	||	// short form of name
          (bcmp("GLOBAL\0", &var->name.var_cu[1], 7) == 0)) // if OK
      { X_set("$GLOBAL\0", &var->name.var_cu[0], 8); // copy in full name
	if (var->uci == 0)
	  var->uci = partab.jobtab->uci;	// ensure uci is set
	if (var->volset == 0)
	  var->volset = partab.jobtab->vol;	// ensure volset is set
        return 0;				// and return saying OK
      }
      return -(ERRM60);				// Undefined ssvn

    case 'J':					// $JOB
      if ((var->name.var_cu[2] == '\0')	||	// short form of name
	  (bcmp("JOB\0\0\0\0", &var->name.var_cu[1], 7) == 0)) // if OK
      { X_set("$JOB\0\0\0\0", &var->name.var_cu[0], 8);	// copy in full name
	if (var->uci == 0)
	  var->uci = 1;				// ensure uci is set
	if (var->volset == 0)
	  var->volset = 1;			// ensure volset is set
        if (var->uci != 1) return -(ERRM59);	// Environment reference not OK
        if (var->volset != 1) return -(ERRM59);	// Environment reference not OK
        return 0;				// and return saying OK
      }
      return -(ERRM60);				// Undefined ssvn

    case 'L':					// $LOCK
      if ((var->name.var_cu[2] == '\0')	||	// short form of name
          (bcmp("LOCK\0\0\0", &var->name.var_cu[1], 7) == 0)) // if OK
      { X_set("$LOCK\0\0\0", &var->name.var_cu[0], 8); // copy in full name
	if (var->uci == 0)
	  var->uci = 1;				// ensure uci is set
	if (var->volset == 0)
	  var->volset = partab.jobtab->lvol;	// ensure volset is set
        if (var->uci != 1) return -(ERRM59);	// Environment reference out
        return 0;				// and return saying OK
      }
      return -(ERRM60);				// Undefined ssvn

    case 'R':					// $ROUTINE
      if ((var->name.var_cu[2] == '\0')	||	// short form of name
          (bcmp("ROUTINE", &var->name.var_cu[1], 7) == 0)) // if OK
      { X_set("$ROUTINE", &var->name.var_cu[0], 8); // copy in full name
	if (var->volset == 0)			// check volset
	  var->volset = partab.jobtab->rvol;	// ensure non-zero
	if (var->uci == 0)			// check uci
	{
	  if (var->key[1] == '%')               // manager
	  { var->uci = 1;
            var->volset = 1;
          }
	  else
	    var->uci = partab.jobtab->ruci;	// or here
	}
        return 0;				// and return saying OK
      }
      return -(ERRM60);				// Undefined ssvn

    case 'S':					// $SYSTEM
      if ((var->name.var_cu[2] == '\0')	||	// short form of name
          (bcmp("SYSTEM\0", &var->name.var_cu[1], 7) == 0)) // if OK
      { X_set("$SYSTEM\0", &var->name.var_cu[0], 8); // copy in full name
	if (var->uci == 0)
	  var->uci = 1;				// ensure uci is set
	if (var->volset == 0)
	  var->volset = 1;			// ensure volset is set
        if (var->uci != 1) return -(ERRM59);	// Environment reference out
        if (var->volset != 1) return -(ERRM59);	// Environment reference out
        return 0;				// and return saying OK
      }
      return -(ERRM60);				// Undefined ssvn
    default:					// error
      return -(ERRM60);				// Undefined ssvn
  }
}

short SS_Get(mvar *var, u_char *buf)            // get ssvn data
{ int i = 0;					// usefull int
  int j;					// and another
  short s;					// for functions
  int cnt;					// count of bytes used
  var_u *rounam;				// to extract rou name
  u_char tmp[1024];				// temp string space
  int ptmp = 0;					// pointer into this
  int nsubs = 0;				// count subscripts
  struct passwd *pp;				// for getpwuid()
  cstring *subs[4];				// where to put them
  mvar *vp;					// variable ptr
  while (i < var->slen)				// for all subs
  { cnt = 0;					// flag no rabit ears quotes
    if (nsubs > 3) return (-ERRM38);		// junk
    subs[nsubs] = (cstring *) &tmp[ptmp];	// point at the buffer
    s = UTIL_Key_Extract( &var->key[i],		// key from here
			  subs[nsubs]->buf,	// where to put it
			  &cnt);		// the count
    if (s < 0) return s;			// die on error
    subs[nsubs++]->len = s;			// save the size (incr count)
    ptmp = ptmp + s + sizeof(short) + 1;	// move up temp area
    i = i + cnt;				// count used bytes
  }

  i = SS_Norm(var);				// normalize the name
  if (i < 0) return i;				// return on error
  switch (var->name.var_cu[1])			// check initial of name
  { case 'G':					// $GLOBAL
      if (nsubs == 2)				// two sub case
      { if (strncasecmp( (char *) subs[1]->buf, "character\0", 10) == 0)
        { return mcopy((u_char *)"M", buf, 1);		// just an M
        }
        if (strncasecmp( (char *) subs[1]->buf, "journal\0", 8) == 0)
        { var->slen = strlen((char *)var->key) + 1;	// first subscript only
	  i = DB_GetFlags(var);			// get flags
	  if (i < 0)				// if error
	  { return (short) i;			// return it
	  }
	  return itocstring(buf, i & GL_JOURNAL); // return 1/0
        }					// end journal
      }						// end 2 subs
      if (nsubs > 1) return (-ERRM38);		// junk
      return DB_Get(var, buf);			// let the database module doit
    case 'J':					// $JOB
      buf[0] = '\0';				// JIC
      if (nsubs == 0)
	return itocstring(buf, systab->maxjob);	// max permitted jobs
      if (nsubs < 2) return (-ERRM38);		// junk
      i = cstringtoi(subs[0]) - 1;		// make an int of job#
      if ((i < 0) || (i >= systab->maxjob))	// in range?
	return (-ERRM23);			// no - complain
      if (systab->jobtab[i].pid == 0)		// process id?
	return (-ERRM23);			// complain if no such
      if (kill(systab->jobtab[i].pid, 0))	// check the job
      { if (errno == ESRCH)			// doesn't exist
        { CleanJob(i + 1);			// zot if not there
	  return (-ERRM23);			// no - complain
        }
      }
      if (nsubs == 2)				// two sub case
      { if (strncasecmp( (char *) subs[1]->buf, "$io\0", 4) == 0)
	  return itocstring(buf, systab->jobtab[i].io); // ^$J(n,"$io")
        if (strncasecmp( (char *) subs[1]->buf, "$reference\0", 11) == 0)
        { vp = &systab->jobtab[i].last_ref;	// addr of $R
	  if (vp->name.var_cu[0] == '\0') return 0; // return null string
	  bcopy(vp, tmp, vp->slen + MVAR_SIZE); // copy to tmp
	  vp = (mvar *) tmp;			// point at tmp
	  if (vp->uci == 0) vp->uci = systab->jobtab[i].uci;
	  if (vp->volset == 0) vp->volset = systab->jobtab[i].vol;
	  return UTIL_String_Mvar(vp, buf, 32767); // ^$J(n,"$reference")
        }
        if (strncasecmp( (char *) subs[1]->buf, "$stack\0", 7) == 0)
	  return itocstring(buf, systab->jobtab[i].cur_do);
        if (strncasecmp( (char *) subs[1]->buf, "character\0", 10) == 0)
	  return mcopy((u_char *)"M", buf, 1);	// just an M
        if (strncasecmp( (char *) subs[1]->buf, "commands\0", 9) == 0)
	  return uitocstring(buf, systab->jobtab[i].commands);
        if (strncasecmp( (char *) subs[1]->buf, "global\0", 7) == 0)
	  return itocstring(buf, systab->jobtab[i].uci);
        if (strncasecmp( (char *) subs[1]->buf, "global_vol\0", 11) == 0)
	  return itocstring(buf, systab->jobtab[i].vol);
        if (strncasecmp( (char *) subs[1]->buf, "grefs\0", 6) == 0)
	  return uitocstring(buf, systab->jobtab[i].grefs);
        if (strncasecmp( (char *) subs[1]->buf, "lock\0", 5) == 0)
	  return itocstring(buf, systab->jobtab[i].luci);
        if (strncasecmp( (char *) subs[1]->buf, "lock_vol\0", 9) == 0)
	  return itocstring(buf, systab->jobtab[i].lvol);
        if (strncasecmp( (char *) subs[1]->buf, "owner\0", 6) == 0)
	{ pp = getpwuid((uid_t) systab->jobtab[i].user);	// get pw
	  if (pp == NULL) 					// on fail
	    return itocstring(buf, systab->jobtab[i].user);	// return numb
	  strcpy((char *)buf, pp->pw_name);			// copy it
	  return strlen((char *)buf);				// return len
	}
        if (strncasecmp( (char *) subs[1]->buf, "owner_id\0", 9) == 0)
	  return itocstring(buf, systab->jobtab[i].user);
        if (strncasecmp( (char *) subs[1]->buf, "precision\0", 10) == 0)
	  return itocstring(buf, systab->jobtab[i].precision);
        if (strncasecmp( (char *) subs[1]->buf, "routine\0", 8) == 0)
	  return itocstring(buf, systab->jobtab[i].ruci);
        if (strncasecmp( (char *) subs[1]->buf, "routine_name\0", 13) == 0)
	{ j = systab->jobtab[i].cur_do;		// get current do
	  rounam = &systab->jobtab[i].dostk[j].rounam; // point at routine name
	  for (s = 0; s < MAX_NAME_BYTES; s++)	// for each character
	    if ((buf[s] = rounam->var_cu[s]) == 0) break; // copy it
	  buf[s] = '\0';			// null terminate
	  return s;  				// return length
	}
        if (strncasecmp( (char *) subs[1]->buf, "routine_vol\0", 12) == 0)
	  return itocstring(buf, systab->jobtab[i].rvol);
        if (strncasecmp( (char *) subs[1]->buf, "pid\0", 4) == 0)
	  return itocstring(buf, systab->jobtab[i].pid);
        if (strncasecmp( (char *) subs[1]->buf, "priority\0", 9) == 0)
	{ errno = 0;
	  j = getpriority(PRIO_PROCESS, systab->jobtab[i].pid);
	  if (errno != 0) return -(ERRMLAST+ERRZLAST+errno);
	  return itocstring(buf, j);
	}
        if (strncasecmp( (char *) subs[1]->buf, "priv\0", 5) == 0)
	  return itocstring(buf, systab->jobtab[i].priv);
        if (strncasecmp( (char *) subs[1]->buf, "process_start\0", 14) == 0)
	  return mcopy(systab->jobtab[i].start_dh,	// the data
		       buf,				// the destination
		       systab->jobtab[i].start_len);	// and length
      }						// end of two sub case
      else if (nsubs == 3)			// three sub case
      { if (strncasecmp( (char *) subs[1]->buf, "$io\0", 4) == 0)
	{ j = cstringtoi(subs[2]);		// get chan as int
	  if ((j < 0) || (j >= MAX_SEQ_IO))
	    return (-ERRM38);			// out of range
	  if (systab->jobtab[i].seqio[j].type == SQ_FREE)
	    return 0;				// not in use
	  cnt = strlen((char *)systab->jobtab[i].seqio[j].name); // get length
	  return mcopy(systab->jobtab[i].seqio[j].name,
		       buf, cnt);		// copy and exit
	}					// end $io,n)
        if (strncasecmp( (char *) subs[1]->buf, "$stack\0", 7) == 0)
	  return Dstack1x(buf,
			  cstringtoi(subs[2]),
			  i);			// do it elsewhere
      }						// end 3 sub case
      else if (nsubs == 4)			// four sub case
      { if (strncasecmp( (char *) subs[1]->buf, "$stack\0", 7) == 0)
	  return Dstack2x(buf,
			  cstringtoi(subs[2]),
			  subs[3],
			  i);			// do it elsewhere
      }						// end 4 sub case
      return (-ERRM38);				// junk

    case 'L':					// $LOCK
      if (nsubs != 1) return (-ERRM38);		// junk
      if (subs[0]->len > 511) return -(ERRMLAST+ERRZ12); // junk
      vp = (mvar *) &tmp[512];			// some temp space
      s = UTIL_MvarFromCStr(subs[0], vp);	// convert to mvar
      if (s < 0) return s;			// quit on error
      s = UTIL_mvartolock(vp, subs[0]->buf);	// cvt to locktab style
      if (s < 0) return s;			// quit on error
      subs[0]->len = s;				// save the length
      return LCK_Get(subs[0], buf);		// doit and exit

    case 'R':					// $ROUTINE
      if (nsubs > 3) return (-ERRM38);		// junk
      if (nsubs == 2)
        if (strncasecmp( (char *) subs[1]->buf, "character\0", 10) == 0)
	  return mcopy((u_char *)"M", buf, 1);	// just an M
      return DB_Get(var, buf);			// let the database module doit

    case 'S':					// $SYSTEM
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "tsize\0", 6) == 0))
      { return itocstring(buf, sizeof(time_t)); // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "zminspace\0", 10) == 0))
      { return itocstring(buf, systab->ZMinSpace); // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dqlen\0", 6) == 0))
      { return itocstring(buf, DirtyQ_Len()); 	// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "eok\0", 4) == 0))
      { return itocstring(buf,
      	       (systab->historic & HISTORIC_EOK)); // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "offok\0", 6) == 0))
      { return itocstring(buf,
      	       (systab->historic & HISTORIC_OFFOK)); // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "precision\0", 10) == 0))
      { return itocstring(buf, systab->precision);   // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "$nextok\0", 8) == 0))
      { return itocstring(buf,
      	       (systab->historic & HISTORIC_DNOK));  // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "compmsg\0", 8) == 0))
      { strcpy((char *) buf, (char *) &partab.compmsg->buf[0]);
        return partab.compmsg->len;                     // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "resttime\0", 9) == 0))
      { return itocstring(buf, systab->ZRestTime);   // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "maxresttime\0", 12) == 0))
      { return itocstring(buf, systab->ZMaxRestTime);   // return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_url\0", 8) == 0))
      { strcpy((char *) buf, (char *) systab->dgpURL);
        return strlen((char *) buf);		// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_port\0", 9) == 0))
      { return itocstring(buf, systab->dgpPORT);// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_send_timeout\0", 17) == 0))
      { return itocstring(buf, systab->dgpSNDTO);// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_recv_timeout\0", 17) == 0))
      { return itocstring(buf, systab->dgpRCVTO);// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_id\0", 7) == 0))
      { return itocstring(buf, systab->dgpID);	// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_lock_timeout\0", 17) == 0))
      { return itocstring(buf, systab->dgpLOCKTO);// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_routine_age\0", 16) == 0))
      { return itocstring(buf, systab->dgpROUAGE);// return the value
      }
      if (strncasecmp( (char *) subs[0]->buf, "trantab\0", 8) == 0)
      { i = cstringtoi(subs[1]) - 1;		// make an int of entry#
	if ((!(i < MAX_TRANTAB)) || (i < 0))	// validate it
	  return (-ERRM38);			// junk
	if (nsubs != 2) return (-ERRM38);	// must be 2 subs
	if (!systab->tt[i].to_uci)		// if nothing there
	{ buf[0] = '\0';			// null terminate
	  return 0;				// and return nothing
	}
	s = UTIL_String_Mvar((mvar *) &systab->tt[i].to_global, buf, 0);
	buf[s++] = '=';
	s += UTIL_String_Mvar((mvar *) &systab->tt[i].from_global, &buf[s], 0);
	return s;
      }						// end trantab stuf
      if (strncasecmp( (char *) subs[0]->buf, "replica\0", 7) == 0)
      { if (nsubs < 3) return (-ERRM38); 	// must be 2 subs
        i = cstringtoi(subs[1]) - 1;		// make an int of entry#
	if ((!(i < MAX_REPLICAS)) || (i < 0))	// validate it
	  return (-ERRM38);			// junk
	if (!systab->replicas[i].connection[0])  // if nothing there
	{ buf[0] = '\0';			// null terminate
	  return 0;				// and return nothing
	}
        if (strncasecmp( (char *) subs[2]->buf, "connection\0", 11) == 0)
        { strcpy((char *)buf, &systab->replicas[i].connection[0]);
          return strlen((char *) buf);
        }
        if (strncasecmp( (char *) subs[2]->buf, "type\0", 5) == 0)
        { strcpy((char *)buf, systab->replicas[i].typ ? "mandatory":"optional");
          return strlen((char *) buf);
        }
	return -(ERRM38);
      }						// end replica stuff
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "backup_type\0", 12) == 0))
      { static char *bkptypes[] = {"full","cumulative","serial"};
        strcpy((char *) buf, bkptypes[systab->bkptyp]);
        return strlen((char *) buf);		// return length
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "backup_volumes\0", 15) == 0))
      { return itocstring(buf,
      	       (systab->bkpvolmask)); 		// return the value
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "backup_file\0", 12) == 0))
      { strcpy((char *) buf, systab->bkpfile);
        return strlen((char *) buf);		// return the length
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "restore_file\0", 13) == 0))
      { strcpy((char *) buf, systab->rstfile);
        return strlen((char *) buf);		// return the length
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "local_buffer_timeout\0", 21) == 0))
      { return itocstring(buf,
      	       (systab->locbufTO)); 		// return the value
      }
      if (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0)
      { i = cstringtoi(subs[1]) - 1;		// make an int of vol#
	if ((!(i < MAX_VOL)) || (i < 0))	// validate it
	  return (-ERRM38);			// junk
	if (nsubs < 3) return (-ERRM38);	// must be 3 subs
	if (strncasecmp( (char *) subs[2]->buf, "file\0", 5) == 0)
	{ strcpy((char *)buf, systab->vol[i]->file_name); // copy it
	  return strlen((char *)buf);		// return the length
	}
	if (strncasecmp( (char *) subs[2]->buf, "journal_buffer_size\0", 20) == 0)
        { j = systab->vol[i]->jkb;
          if (0 == systab->vol[i]->syncjrn)
            j = -j;
	  return itocstring(buf, j);
        }
        if (strncasecmp( (char *) subs[2]->buf, "blocks_changed\0", 15) == 0)
          return itocstring(buf, systab->vol[i]->blkchanged);
        if (strncasecmp( (char *) subs[2]->buf, "track_changes\0", 14) == 0)
          return itocstring(buf, systab->vol[i]->track_changes);
        if (strncasecmp( (char *) subs[2]->buf, "backup_running\0", 15) == 0)
          return itocstring(buf, systab->vol[i]->bkprunning);
	if (strncasecmp( (char *) subs[2]->buf, "writelock\0", 10) == 0)
	  return itocstring(buf, systab->vol[i]->writelock);
	if (strncasecmp( (char *) subs[2]->buf, "protected\0", 10) == 0)
	  return itocstring(buf, systab->vol[i]->flags & VOL_PROTECT ? 1:0);
	if (strncasecmp( (char *) subs[2]->buf, "readonly\0", 9) == 0)
	  return itocstring(buf, systab->vol[i]->flags & VOL_RDONLY ? 1:0);
	if (strncasecmp( (char *) subs[2]->buf, "blkalloc\0", 9) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.blkalloc);
	if (strncasecmp( (char *) subs[2]->buf, "blkdeall\0", 9) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.blkdeall);
	if (strncasecmp( (char *) subs[2]->buf, "blkreorg\0", 9) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.blkreorg);
	if (strncasecmp( (char *) subs[2]->buf, "dbdat\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.dbdat);
	if (strncasecmp( (char *) subs[2]->buf, "dbget\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.dbget);
	if (strncasecmp( (char *) subs[2]->buf, "dbkil\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.dbkil);
	if (strncasecmp( (char *) subs[2]->buf, "dbord\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.dbord);
	if (strncasecmp( (char *) subs[2]->buf, "dbqry\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.dbqry);
	if (strncasecmp( (char *) subs[2]->buf, "dbset\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.dbset);
	if (strncasecmp( (char *) subs[2]->buf, "lastok\0", 7) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.lastok);
	if (strncasecmp( (char *) subs[2]->buf, "lasttry\0", 8) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.lasttry);
	if (strncasecmp( (char *) subs[2]->buf, "lastwtok\0", 9) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.lastwtok);
	if (strncasecmp( (char *) subs[2]->buf, "lastwttry\0", 10) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.lastwttry);
	if (strncasecmp( (char *) subs[2]->buf, "eventcnt\0", 9) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.eventcnt);
	if (strncasecmp( (char *) subs[2]->buf, "logrd\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.logrd);
	if (strncasecmp( (char *) subs[2]->buf, "logwt\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.logwt);
	if (strncasecmp( (char *) subs[2]->buf, "phyrd\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.phyrd);
	if (strncasecmp( (char *) subs[2]->buf, "phywt\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.phywt);
	if (strncasecmp( (char *) subs[2]->buf, "diskerrors\0", 11) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.diskerrors);
	if (strncasecmp( (char *) subs[2]->buf, "dqstall\0", 8) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.dqstall);
	if (strncasecmp( (char *) subs[2]->buf, "gqstall\0", 8) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.gqstall);
	if (strncasecmp( (char *) subs[2]->buf, "gbwait\0", 7) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.gbwait);
	if (strncasecmp( (char *) subs[2]->buf, "gbswait\0", 8) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.gbswait);
	if (strncasecmp( (char *) subs[2]->buf, "rdwait\0", 7) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.rdwait);
	if (strncasecmp( (char *) subs[2]->buf, "brdwait\0", 8) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.brdwait);
	if (strncasecmp( (char *) subs[2]->buf, "bwrwait\0", 8) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.bwrwait);
	if (strncasecmp( (char *) subs[2]->buf, "lckwait\0", 8) == 0)
	  return uitocstring(buf, systab->vol[i]->stats.lckwait);
	if (strncasecmp( (char *) subs[2]->buf, "local_name\0", 11) == 0)
	{ strcpy((char *) buf, (char *) systab->vol[i]->local_name);
	  return strlen((char *) buf);
	}

	if (systab->vol[i]->vollab == NULL)	// check volume
	  return -(ERRZ90 + ERRMLAST);		//   mounted

	if (strncasecmp( (char *) subs[2]->buf, "block\0", 6) == 0)
	  return uitocstring(buf, systab->vol[i]->vollab->block_size);
	if (strncasecmp( (char *) subs[2]->buf, "free\0", 5) == 0)
	  return uitocstring(buf, DB_Free(i + 1)); // return free blocks
	if (strncasecmp( (char *) subs[2]->buf, "header\0", 7) == 0)
	  return itocstring(buf, systab->vol[i]->vollab->header_bytes);
	if (strncasecmp( (char *) subs[2]->buf, "journal_available\0", 18) == 0)
	  return itocstring(buf, systab->vol[i]->vollab->journal_available);
	if (strncasecmp( (char *) subs[2]->buf, "journal_requested\0", 18) == 0)
	  return itocstring(buf, systab->vol[i]->vollab->journal_requested);
	if (strncasecmp( (char *) subs[2]->buf, "journal_size\0", 13) == 0)
	  return itocstring(buf, systab->vol[i]->jrn_next);
	if (strncasecmp( (char *) subs[2]->buf, "journal_file\0", 13) == 0)
	{ (void) strcpy((char *)buf, systab->vol[i]->vollab->journal_file);
	  return strlen((char *)buf);
	}
	if (strncasecmp((char *)subs[2]->buf,"backup_revision_number\0",23)==0)
	  return itocstring(buf, systab->vol[i]->vollab->bkprevno);
        if (strncasecmp((char *)subs[2]->buf, "txid\0", 5)==0)
        { return sprintf((char *)buf, "%llu", systab->vol[i]->vollab->txid);
        }
	if (strncasecmp( (char *) subs[2]->buf,"global_buffer_size\0", 19) == 0)
	  return itocstring(buf, systab->vol[i]->gmb);
	if (strncasecmp( (char *) subs[2]->buf,"global_buffer_sync\0", 19) == 0)
	  return itocstring(buf, systab->vol[i]->gbsync);
	if (strncasecmp( (char *) subs[2]->buf, "name\0", 5) == 0)
	{ for (j = 0; j < MAX_NAME_BYTES; j++)
	    if ((buf[j] = systab->vol[i]->vollab->volnam.var_cu[j]) == 0)
	      break;
	  buf[j] = '\0';
	  return j;
	}
	if (strncasecmp( (char *) subs[2]->buf, "size\0", 5) == 0)
	  return itocstring(buf, systab->vol[i]->vollab->max_block);
	if (strncasecmp( (char *) subs[2]->buf, "uci\0", 4) == 0)
	{ if (nsubs != 4) return (-ERRM38);	// must be 4 subs
	  j = cstringtoi(subs[3]) - 1;		// make an int of uci#
	  if ((!(j < UCIS)) || (j < 0))		// validate it
	    return (-ERRM38);			// junk
	  for (s = 0; s < MAX_NAME_BYTES; s++)
	    if ((buf[s] = systab->vol[i]->vollab->uci[j].name.var_cu[s]) == 0)
	      break;
	  buf[s] = '\0';
	  return s;
	}
	if (strncasecmp( (char *) subs[2]->buf, "dirty\0", 6) == 0)
	  return uitocstring(buf, DB_GetDirty(i));
      }						// end of "VOL"
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "zotdata\0", 8) == 0))
      { return itocstring(buf, systab->ZotData);// return the value
      }
      return (-ERRM38);				// junk
  }						// end of switch
  return (-ERRM38);				// can't get here?
}

short SS_Set(mvar *var, cstring *data)          // set ssvn data
{ int i = 0;					// usefull int
  int j;					// and another
  short s;					// for functions
  int cnt;					// count of bytes used
  var_u n;					// for names
  u_char tmp[1024];				// temp string space
  int ptmp = 0;					// pointer into this
  int nsubs = 0;				// count subscripts
  cstring *subs[4];				// where to put them
  trantab tt;					// for translations

  while (i < var->slen)				// for all subs
  { cnt = 0;					// flag no rabit ears quotes
    if (nsubs > 3) return (-ERRM38);		// junk
    subs[nsubs] = (cstring *) &tmp[ptmp];	// point at the buffer
    s = UTIL_Key_Extract( &var->key[i],		// key from here
			  subs[nsubs]->buf,	// where to put it
			  &cnt);		// the count
    if (s < 0) return s;			// die on error
    subs[nsubs++]->len = s;			// save the size (incr count)
    ptmp = ptmp + s + sizeof(short) + 1;	// move up temp area
    i = i + cnt;				// count used bytes
  }
  i = SS_Norm(var);				// normalize the name
  if (i < 0) return i;				// return on error
  switch (var->name.var_cu[1])			// check initial of name
  { case 'G':					// $GLOBAL
      if (nsubs == 2)				// two sub case
      { if (strncasecmp( (char *) subs[1]->buf, "journal\0", 8) == 0)
        { var->slen = strlen((char *)var->key) + 1;	// first subscript only
	  i = cstringtob(data);			// get value as 1/0
	  if (!i)
	  { i = -1;				// setup for a clear
	  }
	  i = DB_SetFlags(var, i);		// set flags
	  if (i < 0)				// if error
	  { return (short) i;			// return it
	  }
	  return 0;				// done
        }					// end journal
      }						// end 2 subs
      return -(ERRM29);				// SET or KILL on ssvn not on

    case 'J':					// $JOB
      if (nsubs != 2) return (-ERRM38);		// junk
      i = cstringtoi(subs[0]) - 1;		// make an int of job#
      if ((i < 0) || (i >= systab->maxjob))	// in range?
	return (-ERRM23);			// no - complain
      if (systab->jobtab[i].pid == 0)		// process id?
	return (-ERRM23);			// complain if no such

      j = cstringtoi(data);			// convert to int
      if ((j < 1) || (j > 64)) j = 65;		// limit size
      if ((MV1_PID == i) && 			// same job?
	  (priv()))				// and has privs
      { if (strncasecmp( (char *) subs[1]->buf, "global\0", 7) == 0)
	{ systab->jobtab[i].uci = j;		// set it
	  //systab->jobtab[i].last_ref.name.var_qu = 0; // clear $R
	  X_Clear(systab->jobtab[i].last_ref.name.var_xu); // clear $R
	  return 0;				// and quit
	}
	if (strncasecmp( (char *) subs[1]->buf, "global_vol\0", 11) == 0)
	{ systab->jobtab[i].vol = j;		// set it
	  //systab->jobtab[i].last_ref.name.var_qu = 0; // clear $R
	  X_Clear(systab->jobtab[i].last_ref.name.var_xu); // clear $R
	  return 0;				// and quit
	}
        if (strncasecmp( (char *) subs[1]->buf, "lock\0", 5) == 0)
	{ systab->jobtab[i].luci = j;		// set it
	  return 0;				// and quit
	}
        if (strncasecmp( (char *) subs[1]->buf, "lock_vol\0", 9) == 0)
	{ systab->jobtab[i].lvol = j;		// set it
	  return 0;				// and quit
	}
        if (strncasecmp( (char *) subs[1]->buf, "routine\0", 8) == 0)
	{ systab->jobtab[i].ruci = j;		// set it
	  return 0;				// and quit
	}
        if (strncasecmp( (char *) subs[1]->buf, "routine_vol\0", 12) == 0)
	{ systab->jobtab[i].rvol = j;		// set it
	  return 0;				// and quit
	}
      }
      j = cstringtoi(data);			// convert to int (again)
      if (priv())				// is it priveleged ?
      { if (strncasecmp( (char *) subs[1]->buf, "owner_id\0", 9) == 0)
	{ systab->jobtab[i].user = j;		// SHOULD HAVE SOME CHECKS HERE
	  return 0;				// and quit
	}
        if (strncasecmp( (char *) subs[1]->buf, "precision\0", 10) == 0)
	{ if ((j < 0) || (j > 31)) return -ERRM28;
	  systab->jobtab[i].precision = j;
	  return 0;
	}
        if (strncasecmp( (char *) subs[1]->buf, "priority\0", 9) == 0)
	{ errno = 0;
	  if (setpriority(PRIO_PROCESS, systab->jobtab[i].pid, j) == -1)
	    return -(ERRMLAST+ERRZLAST+errno);
	  return 0;
	}
        if (strncasecmp( (char *) subs[1]->buf, "priv\0", 5) == 0)
	{ systab->jobtab[i].priv = (j || 0);	// set to 0 or 1
	  if (!j)				// if clearing PRIV
	  { j = setuid(partab.jobtab->user);	// attempt to change user
	  }
	  return 0;
	}
      }
      return -(ERRM29);				// SET or KILL on ssvn not on

    case 'L':					// $LOCK
      return -(ERRM29);				// SET on ssvn not on

    case 'R':					// $ROUTINE
      if (nsubs > 2) return (-ERRM38);		// junk
      return -(ERRM29);				// SET on ssvn not on
      // We may eventually allow SET ^$R(rou,0) with tests.

    case 'S':					// $SYSTEM
      if (!priv()) return (-ERRM38);		// need privs
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "$nextok\0", 8) == 0))
      { if (cstringtob(data))
        { systab->historic |= HISTORIC_DNOK;
        }
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "eok\0", 4) == 0))
      { if (cstringtob(data))
        { systab->historic |= HISTORIC_EOK;
        }
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "offok\0", 6) == 0))
      { if (cstringtob(data))
        { systab->historic |= HISTORIC_OFFOK;
        }
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "precision\0", 10) == 0))
      { j = cstringtoi(data);
	if ((j < 0) || (j > 31)) return -ERRM28;
        systab->precision = j;
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "maxresttime\0", 12) == 0))
      { j = cstringtoi(data);
        if (j < MINRESTTIME) return -ERRM28;
        systab->ZMaxRestTime = j;
        systab->ZRestTime = systab->ZMaxRestTime;
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_lock_timeout\0", 17) == 0))
      { j = cstringtoi(data);
	if ((j < 0) || (j > DGP_MAX_LOCKTO)) return -ERRM28;
        systab->dgpLOCKTO = j;
	return 0;				// and exit
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_routine_age\0", 16) == 0))
      { j = cstringtoi(data);
	if ((j < 0) || (j > DGP_MAX_ROUAGE)) return -ERRM28;
        systab->dgpROUAGE = j;
	return 0;				// and exit
      }
      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "dgp_recv_timeout\0", 17) == 0))
      { j = cstringtoi(data);
	if (j < -1) return -ERRM28;
        systab->dgpRCVTO = j;
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "backup_type\0", 12) == 0))
      { j = -1;
        if (strncasecmp((char *) data->buf, "full\0", 5) == 0)
          j = BKP_FULL;
        else if (strncasecmp((char *) data->buf, "cumulative\0", 11) == 0)
          j = BKP_CUMULATIVE;
        else if (strncasecmp((char *) data->buf, "serial\0", 7) == 0)
          j = BKP_SERIAL;
	if (j < 0) return -(ERRZ64 + ERRMLAST);
        systab->bkptyp = j;
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "backup_volumes\0", 15) == 0))
      { j = cstringtoi(data);
	if ((j < 1) || (j > 65535)) return -ERRM28;
        systab->bkpvolmask = j;
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "restore_file\0", 13) == 0))
      { if ((data->len < 1) || (data->len + 1 > VOL_FILENAME_MAX))
          return -ERRM28;
	strcpy(systab->rstfile, (char *) data->buf);
	return 0;
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "backup_file\0", 12) == 0))
      { if ((data->len < 1) || (data->len + 1 > VOL_FILENAME_MAX))
          return -ERRM28;
	strcpy(systab->bkpfile, (char *) data->buf);
	if (strlen(systab->rstfile))		// have RESTORE_FILE ?
	{ s = DB_Restore(systab->bkpfile, 	//   do restore
			 systab->bkpvolmask, 
			 systab->rstfile);
	  systab->rstfile[0] = '\0';		// reset RESTORE_FILE
	}
	else
	{ s = DB_Backup(systab->bkpfile, 	//   do backup
			systab->bkpvolmask, 
			systab->bkptyp);
	}
	systab->bkpfile[0] = '\0';		// reset BACKUP_FILE
        return s;
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "zminspace\0", 10) == 0))
      { j = cstringtoi(data);
	if ((j < 128) ||
            (j > (0.9 * systab->vol[0]->vollab->block_size))) // XXX volume?
          return -(ERRMLAST+ERRZ64);
        systab->ZMinSpace = j;
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "zotdata\0", 8) == 0))
      { j = cstringtoi(data);
        systab->ZotData = 0 != j;
	return 0;				// and exit
      }

      if ((nsubs == 1) &&
	  (strncasecmp( (char *) subs[0]->buf, "local_buffer_timeout\0", 21) == 0))
      { j = cstringtoi(data);
	if ((j < 0) || (j > 30000)) return -ERRM28;
        systab->locbufTO = j;
	return 0;				// and exit
      }

      if (strncasecmp( (char *) subs[0]->buf, "trantab\0", 8) == 0)
      { if (nsubs != 2) return (-ERRM38);	// must be 2 subs
	cnt = cstringtoi(subs[1]) - 1;		// make an int of entry#
	if ((!(cnt < MAX_TRANTAB)) || (cnt < 0)) // validate it
	  return (-ERRM38);			// junk
	if (data->len == 0)			// if null
	{ while (SemOp( SEM_SYS, -systab->maxjob))
            ;
          bzero(&systab->tt[cnt], sizeof(trantab)); // clear it
	  systab->max_tt = 0;			// clear this for now
	  for (i = MAX_TRANTAB; i; i--)		// look for last used
	  { if (systab->tt[i - 1].to_uci)	// if found
	    { systab->max_tt = i;		// save here
	      break;				// exit
	    }
	  }
          bzero(&systab->tthash[0],             // clear trantab hash
                          sizeof(systab->tthash));
          systab->tthash_empty = 1;             // mark empty
          SemOp( SEM_SYS, systab->maxjob);
	  return 0;				// and exit
	}
	subs[2] = (cstring *) tmp;		// some space
	subs[3] = (cstring *) &tmp[512];	// some more
	for (i = 0; ;i++)			// scan input
	{ if (data->buf[i] == '=')		// found =
	  { subs[3]->buf[i] = '\0';		// null terminate
	    subs[3]->len = i++;			// save length (incr i)
	    break;				// and exit loop
	  }
	  subs[3]->buf[i] = data->buf[i];	// copy
	}					// destination created
	j = 0;					// clear index
	while ((subs[2]->buf[j++] = data->buf[i++])); // and other one
	s = UTIL_MvarFromCStr(subs[2], &partab.src_var); // encode
	if (s < 0)
	{ return s;				// complain on error
	}
	bcopy(&partab.src_var, &tt.from_global, sizeof(var_u) + 2);
	s = UTIL_MvarFromCStr(subs[3], &partab.src_var); // encode
	if (s < 0)
	{ return s;				// complain on error
	}
	bcopy(&partab.src_var, &tt.to_global, sizeof(var_u) + 2);
        while (SemOp( SEM_SYS, -systab->maxjob))
          ;
	bcopy(&tt, &systab->tt[cnt], sizeof(trantab));
	if ((cnt + 1) > systab->max_tt)		// check flag
	{ systab->max_tt = cnt + 1;		// ensure current is there
	}
        bzero(&systab->tthash[0],               // clear trantab hash
                          sizeof(systab->tthash));
        systab->tthash_empty = 1;               // mark empty
        SemOp( SEM_SYS, systab->maxjob);
	return 0;
      }						// end trantab stuf

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "replica\0", 8) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "connection\0", 11) == 0))
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_REPLICAS)) return (-ERRM60); // out of range
        if (data->len + 1 > VOL_FILENAME_MAX)	// long value ?
          return -(ERRM38);
        if (0 == strstr((char *) &data->buf[0], "://")) // not URL ?
          return -(ERRM38);
        if (i && !systab->replicas[i-1].connection[0])	// out of order?
          return -(ERRM38);
	while (SemOp( SEM_SYS, -systab->maxjob))	// set file_name
                ;
        strcpy((char *) &systab->replicas[i].connection[0],
	       (char *) data->buf);
        SemOp( SEM_SYS, systab->maxjob);
#ifdef MV1_DGP
        if (partab.dgp_repl[i] != -1)		// if connected, disconnect
        { s = DGP_ReplDisconnect(i);
        }
	s = DGP_ReplConnect(i);			// init connection
	if (s < 0) return s;			// failed ? return
	s = DGP_ReplDisconnect(i);		// disconnect
	if (s < 0) return s;			// failed ? return
	fprintf(stderr, "DGP: Replica connected at %s\r\n",
			&systab->replicas[i].connection[0]);
#endif
	return 0;				// return OK
      }

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "replica\0", 8) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "type\0", 11) == 0))
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_REPLICAS)) return (-ERRM60); // out of range
        if (i && !systab->replicas[i-1].connection[0])	// out of order?
          return -(ERRM38);
        if (systab->replicas[i].connection[0])	// cannot change after
          return -(ERRM38);			// connection set
        if (strncasecmp( (char *) &data->buf[0], "mandatory\0", 10) == 0)
        { systab->replicas[i].typ = DGP_REPL_REQ;
        }
        if (strncasecmp( (char *) &data->buf[0], "optional\0", 9) == 0)
        { systab->replicas[i].typ = DGP_REPL_OPT;
        }
	return 0;				// return OK
      }

      if ((nsubs == 4) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "uci\0", 4) == 0)) // ^$S(vol,n,uci,n)
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	j = cstringtoi(subs[3]) - 1;		// and uci#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	if ((j < 0) || (j >= UCIS)) return (-ERRM60); // out of range
	if ((data->len < 1) || (data->len > MAX_NAME_BYTES))
	  return -(ERRMLAST+ERRZ12);		// syntx
        if (systab->vol[i]->local_name[0])	// remote VOL ?
          return -(ERRZ91 + ERRMLAST);
        if (NULL == systab->vol[i]->vollab)	// not mounted ?
	  return -(ERRZ90 + ERRMLAST);
	//n.var_qu = 0;				// clear name
	X_Clear(n.var_xu);			// clear name
	for (s = 0; s < data->len; s++)
	{ if (isalpha(data->buf[s]) == 0)
	    return -(ERRMLAST+ERRZ12);		// syntx
	  n.var_cu[s] = data->buf[s];		// copy to name
	}
	return DB_UCISet(i + 1, j + 1, n);	// doit and return
      }

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "writelock\0", 10) == 0))
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	if (NULL == systab->vol[i]->vollab)	// not mounted ?
	  return -(ERRZ90 + ERRMLAST);
        if (systab->vol[i]->bkprunning)		// backup running ?
          return -(ERRM38);
	while (SemOp( SEM_SYS, -systab->maxjob))// lock SYSTEM
                ;
	systab->vol[i]->writelock = 
	  (cstringtob(data))
	    ? -(MV1_PID + 1)			// set it
	    : 0;				// clear it
        if (systab->vol[i]->writelock)          // block writers
        { inter_add(&systab->delaywt, 1);
          MEM_BARRIER;
        }
	SemOp( SEM_SYS, systab->maxjob);	// unlock SYSTEM
	return 0;				// return OK
      }

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "protected\0", 10) == 0))
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	if (NULL == systab->vol[i]->vollab)	// not mounted ?
	  return -(ERRZ90 + ERRMLAST);
        if (systab->vol[i]->bkprunning)		// backup running ?
          return -(ERRM38);
	while (SemOp( SEM_SYS, -systab->maxjob))// lock SYSTEM
                ;
        if (cstringtob(data))
    	{ systab->vol[i]->flags |= VOL_PROTECT;
	}
	else
	{ systab->vol[i]->flags &= ~VOL_PROTECT;
	}
	SemOp( SEM_SYS, systab->maxjob);	// unlock SYSTEM
	return 0;				// return OK
      }

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "readonly\0", 9) == 0))
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	if (NULL == systab->vol[i]->vollab)	// not mounted ?
	  return -(ERRZ90 + ERRMLAST);
        if (systab->vol[i]->bkprunning)		// backup running ?
          return -(ERRM38);
	while (SemOp( SEM_SYS, -systab->maxjob))// lock SYSTEM
                ;
        if (cstringtob(data))
    	{ systab->vol[i]->flags |= VOL_RDONLY;
	}
	else
	{ systab->vol[i]->flags &= ~VOL_RDONLY;
	}
	SemOp( SEM_SYS, systab->maxjob);	// unlock SYSTEM
	return 0;				// return OK
      }

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "backup_running\0", 15) == 0))
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	if (NULL == systab->vol[i]->vollab)	// not mounted ?
	  return -(ERRZ90 + ERRMLAST);
        while (SemOp( SEM_GLOBAL, -systab->maxjob))
                ;
        systab->vol[i]->bkprunning = cstringtob(data);
        SemOp( SEM_GLOBAL, systab->maxjob);
	return 0;				// return OK
      }

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "track_changes\0", 14) == 0))
      { int old_volnum = volnum;
        i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
        if (NULL == systab->vol[i]->vollab)	// not mounted ?
	  return -(ERRZ90 + ERRMLAST);

	j = cstringtob(data);
	volnum = i + 1;
        while (SemOp( SEM_GLOBAL, -systab->maxjob))
                ;
	if (j)
	{ bzero(systab->vol[i]->chgmap, 
		systab->vol[i]->vollab->header_bytes - SIZEOF_LABEL_BLOCK);
          systab->vol[i]->blkchanged = 0;
	}
	systab->vol[i]->track_changes = j;
        SemOp( SEM_GLOBAL, systab->maxjob);
	volnum = old_volnum;
	return 0;				// return OK
      }

      if ((nsubs == 3) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0)) // ^$S(vol,j,journ...)
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range

	if ((strncasecmp( (char *) subs[2]->buf, "local_name\0", 11) == 0) &&
	    (data->len > 0) && (data->len < MAX_NAME_BYTES))
	{ if (i && (systab->vol[i-1]->vollab == NULL))	// check prev. slot
          { return -(ERRZ90 + ERRMLAST);		//   mounted
          }
          if (systab->vol[i]->vollab != NULL)		// already mounted ?
          { return -(ERRM38);				//   cannot change name
          }
          for (j = 0; j < data->len; j++)
	  { if (isalpha(data->buf[j]) == 0)
	    { return (-ERRM38);
	    }
	  }
          strcpy((char *) &systab->vol[i]->local_name[0], 
		 (char *) data->buf);
	  return 0;
	}
	if ((strncasecmp( (char *) subs[2]->buf, 
                "file\0", 5) == 0))
        { if (i && (systab->vol[i-1]->vollab == NULL))// check prev. slot
          { return -(ERRZ90 + ERRMLAST);		//   mounted
          }
	  s = 0;					// assume success
          if (strstr((char *) &data->buf[0], "://")) 	// an URL ?
          { if (0 == systab->vol[i]->local_name[0])	// local name not set?
	      return -(ERRM38);
            if (data->len + 1 > VOL_FILENAME_MAX)	// long value ?
              return -(ERRM38);
#ifdef MV1_DGP
            s = DGP_GetConnectionURL((char *) &data->buf[0], NULL);
            if (s < 0) return s;			// has conn. part?
	    if (s < 6) return -(ERRM38);
            s = DGP_GetRemoteVOL((char *) &data->buf[0], NULL);
            if (s < 0) return s;			// has remote VOL name?
	    if (!s) return -(ERRM38);
#endif
	    while (SemOp( SEM_SYS, -systab->maxjob))	// set file_name
                ;
            strcpy((char *) &systab->vol[i]->file_name[0],
		   (char *) data->buf);
            SemOp( SEM_SYS, systab->maxjob);
#ifdef MV1_DGP
	    // NB. connect() - disconnect() pair will download remote VOL
	    //     label from server
	    s = DGP_Connect(i);				// init connection
	    if (s < 0) return s;			// failed ? return
	    // DGP client restart: remove any pending remote LOCKs
	    LCK_Remove(DGP_SYSJOB);
	    s = DGP_Disconnect(i);			// disconnect
	    if (s < 0) return s;			// failed ? return
	    fprintf(stderr, "DGP: Mounted %s as %s\r\n",// say OK
			&systab->vol[i]->file_name[0], 
			&systab->vol[i]->local_name[0]);
#endif
	  } else
          { 
            while (SemOp( SEM_SYS, -systab->maxjob))
                ;
            s = DB_Mount((char *) &data->buf[0],	// mount volume
                        i + 1,
                        systab->vol[i]->gmb,
                        systab->vol[i]->jkb);
            SemOp( SEM_SYS, systab->maxjob);
	  }
          return s;
        }

        if (systab->vol[i]->local_name[0])		// remote VOL ?
        { return -(ERRZ91 + ERRMLAST);			// don't do anything
        }						// below

	if ((strncasecmp( (char *) subs[2]->buf, "journal_size\0", 13) == 0) &&
	    (cstringtoi(data) == 0))		// clear journal
	{ int old_volnum = volnum;              // set volnum
          volnum = i + 1;
          while (SemOp( SEM_GLOBAL, -systab->maxjob)) // lock GLOBAL
                ;
	  ClearJournal(partab.jnl_fds[i], i);	// do it
	  SemOp( SEM_GLOBAL, systab->maxjob);	// unlock global
          volnum = old_volnum;
	  return 0;				// done
	}
	if (strncasecmp( (char *) subs[2]->buf, "journal_requested\0", 18) == 0)
	{ systab->vol[i]->vollab->journal_requested = cstringtob(data);
	  if (!systab->vol[i]->vollab->journal_requested)
	  { int old_volnum = volnum;
            volnum = i + 1;                     // set volnum
            while (SemOp( SEM_GLOBAL, WRITE))
              ;
            DB_StopJournal(i + 1, 2);		// JRN_STOP
            SemOp( SEM_GLOBAL, -curr_lock);
            volnum = old_volnum;
	  }
	  systab->vol[i]->map_dirty_flag |= VOLLAB_DIRTY;// tell to write it
	  return 0;
	}
	if ((strncasecmp( (char *) subs[2]->buf, "journal_file\0", 13) == 0))
	{ int old_volnum = volnum;
          if (data->len > JNL_FILENAME_MAX)
	  { return -ERRM56;			// too long
	  }
          volnum = i + 1;                       // set volnum
          while (SemOp( SEM_GLOBAL, WRITE))     // lock GLOBAL
            ;
          DB_StopJournal(i + 1, 2);             // write JRN_STOP
          if (partab.jnl_fds[i])                // close old jrn file
          { close( partab.jnl_fds[i]);
            partab.jnl_fds[i] = 0;
          }
	  (void) strcpy(systab->vol[i]->vollab->journal_file, 
                          (char *)data->buf);   // write new name
	  systab->vol[i]->map_dirty_flag |= VOLLAB_DIRTY;// tell to write it
	  systab->vol[i]->jnl_seq++;		// increment jrn sequence
          if (0 == systab->vol[i]->jnl_seq)
            systab->vol[i]->jnl_seq = 1;
          s = 0;
          OpenJournal(i, 0);                    // open new jrn file
          if (systab->vol[i]->vollab->journal_available)
          { partab.jnl_fds[i] =                 // open if available
                    open(systab->vol[i]->vollab->journal_file, O_RDWR);
            if (partab.jnl_fds[i] < 0)          // failed ?
            { partab.jnl_fds[i] = 0;		//   clear fd
              s = -(errno+ERRMLAST+ERRZLAST);   //   complain
            }
            else
              partab.jnl_seq[i] = systab->vol[i]->jnl_seq;
          }
          else                                  // open jrn file failed
              s = -(ERRZ72+ERRMLAST);           //   complain
          SemOp( SEM_GLOBAL, -curr_lock);
          volnum = old_volnum;
	  return s;
	}
	if ((strncasecmp( (char *) subs[2]->buf, "name\0", 5) == 0) &&
	    (systab->maxjob == 1) &&
	    (data->len > 0) && (data->len < MAX_NAME_BYTES))
	{ for (j = 0; j < data->len; j++)
	  { if (isalpha(data->buf[j]) == 0)
	    { return (-ERRM38);
	    }
	  }
	  //systab->vol[i]->vollab->volnam.var_qu = 0; // zot name
	  X_Clear(systab->vol[i]->vollab->volnam.var_xu); // zot name
	  bcopy(data->buf, systab->vol[i]->vollab->volnam.var_cu, data->len);
//	  (void) strcpy(systab->vol[i]->vollab->volnam.var_cu, data->buf);
	  systab->vol[i]->map_dirty_flag |= VOLLAB_DIRTY; // tell to write it
	  return 0;
	}
	if ((strncasecmp( (char *) subs[2]->buf, "size\0", 5) == 0) &&
	    (systab->maxjob == 1))
	{ u_int vsiz;				// for the size

	  vsiz = (u_int) atol((char *)data->buf);// get the new
	  if (vsiz <= systab->vol[i]->vollab->max_block)
	  { return (-ERRM38);
	  }
	  vsiz |= 7;				// fix size
	  if (vsiz > MAX_DATABASE_BLKS)
	  { return (-ERRM38);
	  }
	  if (vsiz > (((systab->vol[i]->vollab->header_bytes - 
		    SIZEOF_LABEL_BLOCK) * 8) | 7))
	  { return (-ERRM38);
	  }
	  return DB_Expand(i, vsiz);		// do it
	}
	if (strncasecmp( (char *) subs[2]->buf, 
                "backup_revision_number\0", 23) == 0)
        { int old_volnum = volnum;
	  j = cstringtoi(data);
          if ((j < 0) || (j > 255))
          { return -(ERRM38);
          }
	  volnum = i + 1;
	  while (SemOp( SEM_GLOBAL, -systab->maxjob))
                ;
          systab->vol[i]->vollab->bkprevno = j;
          systab->vol[i]->map_dirty_flag |= VOLLAB_DIRTY;
	  SemOp( SEM_GLOBAL, systab->maxjob);
	  volnum = old_volnum;
          return 0;
        }
	if ((strncasecmp( (char *) subs[2]->buf, 
                "global_buffer_sync\0", 19) == 0) &&
            (systab->maxjob != 1))
        { j = cstringtoi(data);
          if ((j < 0) || 				// check prev. slot
	      (i && (systab->vol[i-1]->vollab == NULL)))//   mounted
          { return -(ERRZ90 + ERRMLAST);
          }
          systab->vol[i]->gbsync = j;
          return 0;
        }
	if ((strncasecmp( (char *) subs[2]->buf, 
                "global_buffer_size\0", 19) == 0) &&
            (systab->maxjob != 1))
        { if (i && (systab->vol[i-1]->vollab == NULL))	// check prev. slot
          { return -(ERRZ90 + ERRMLAST);		//   mounted
          }
	  systab->vol[i]->gmb = cstringtoi(data);
          if (systab->vol[i]->gmb < 0)
          { systab->vol[i]->gmb = 0;
            return -(ERRM38);
          }
          return 0;
        }
	if ((strncasecmp( (char *) subs[2]->buf, 
                "journal_buffer_size\0", 20) == 0) &&
            (systab->maxjob != 1))
        { if (i && (systab->vol[i-1]->vollab == NULL))	// check prev. slot
          { return -(ERRZ90 + ERRMLAST);		//   mounted
          }
	  systab->vol[i]->jkb = cstringtoi(data);
          return 0;
        }
      }
      return (-ERRM38);				// do vol mount next vers
  }
  return (-ERRM38);				// can't get here?
}

short SS_Data(mvar *var, u_char *buf)           // get $DATA()
{ int i = 0;					// usefull int
  short s;					// for functions
  int cnt;					// count of bytes used
  u_char tmp[1024];				// temp string space
  int ptmp = 0;					// pointer into this
  int nsubs = 0;				// count subscripts
  mvar *vp;					// variable ptr
  cstring *subs[4];				// where to put them
  while (i < var->slen)				// for all subs
  { cnt = 0;					// flag no rabit ears quotes
    if (nsubs > 3) return (-ERRM38);		// junk
    subs[nsubs] = (cstring *) &tmp[ptmp];	// point at the buffer
    s = UTIL_Key_Extract( &var->key[i],		// key from here
			  subs[nsubs]->buf,	// where to put it
			  &cnt);		// the count
    if (s < 0) return s;			// die on error
    subs[nsubs++]->len = s;			// save the size (incr count)
    ptmp = ptmp + s + sizeof(short) + 1;	// move up temp area
    i = i + cnt;				// count used bytes
  }
  i = SS_Norm(var);				// normalize the name
  if (i < 0) return i;				// return on error
  switch (var->name.var_cu[1])			// check initial of name
  { case 'G':					// $GLOBAL
      if (nsubs > 1) return (-ERRM38);		// junk
      return DB_Data(var, buf);			// let the database module doit
    case 'J':					// $JOB
      if (nsubs != 1) return (-ERRM38);		// junk
      i = cstringtoi(subs[0]);			// make an int of job#
      if ((i < 1) || (i > systab->maxjob))	// in range?
	return (-ERRM23);			// no - complain
      buf[0] = '1';				// assume true
      buf[1] = '\0';				// null terminate
      if (systab->jobtab[i - 1].pid == 0)	// process id?
	buf[0] = '0';				// false if no such
      return 1;					// return the count
    case 'L':					// $LOCK
      if (nsubs != 1) return (-ERRM38);		// junk
      if (subs[0]->len > 511) return -(ERRMLAST+ERRZ12); // junk
      vp = (mvar *) &tmp[512];			// some temp space
      s = UTIL_MvarFromCStr(subs[0], vp);	// convert to mvar
      if (s < 0) return s;			// quit on error
      s = UTIL_mvartolock(vp, subs[0]->buf);	// cvt to locktab style
      if (s < 0) return s;			// quit on error
      subs[0]->len = s;				// save the length
      s = LCK_Get(subs[0], buf);		// try to get it
      if (s < 0) return s;			// quit on error
      buf[0] = s ? '1' : '0';			// set the answer
      buf[1] = '\0';				// null terminate
      return 1;					// and return

    case 'R':					// $ROUTINE
      if (nsubs > 3) return (-ERRM38);		// junk
      return DB_Data(var, buf);			// let the database module doit
    case 'S':					// $SYSTEM
      return (-ERRM38);				// junk
  }
  return (-ERRM38);				// can't get here?
}

short SS_Kill(mvar *var)                        // remove sub-tree
{ int i = 0;					// usefull int
  int j;					// and another
  short s;					// for functions
  int cnt;					// count of bytes used
  var_u rou;					// for routine name
  u_char tmp[1024];				// temp string space
  int ptmp = 0;					// pointer into this
  int nsubs = 0;				// count subscripts
  mvar *vp;					// variable ptr
  cstring *subs[4];				// where to put them
  struct shmid_ds sbuf;                         // for shmctl (shutdown)
  while (i < var->slen)				// for all subs
  { cnt = 0;					// flag no rabit ears quotes
    if (nsubs > 3) return (-ERRM38);		// junk
    subs[nsubs] = (cstring *) &tmp[ptmp];	// point at the buffer
    s = UTIL_Key_Extract( &var->key[i],		// key from here
			  subs[nsubs]->buf,	// where to put it
			  &cnt);		// the count
    if (s < 0) return s;			// die on error
    subs[nsubs++]->len = s;			// save the size (incr count)
    ptmp = ptmp + s + sizeof(short) + 1;	// move up temp area
    i = i + cnt;				// count used bytes
  }
  i = SS_Norm(var);				// normalize the name
  if (i < 0) return i;				// return on error
  switch (var->name.var_cu[1])			// check initial of name
  { case 'G':					// $GLOBAL
      if (nsubs > 1) return (-ERRM38);		// junk
      return -(ERRM29);				// SET or KILL on ssvn not on
    case 'J':					// $JOB
      if (nsubs > 1) return (-ERRM38);		// junk
      if (nsubs == 1)				// if there is a job
      { j = cstringtoi(subs[0]) - 1;		// make an int of it
	if ((j < 0) || (j >= systab->maxjob))	// in range?
	  return (-ERRM23);			// no - complain
	i = systab->jobtab[j].pid;		// get process id
	if (i == 0) return (-ERRM23);		// complain if no such

	if ((!priv()) && (systab->jobtab[j].user != partab.jobtab->user))
	  return -(ERRM29);			// SET or KILL on ssvn not on

	if (!kill( i, SIGTERM)) return 0;	// tell in to go home
	systab->jobtab[j].trap = 1 << SIGTERM;	// say go away
	systab->jobtab[j].attention = 1;	// look at it
	return 0;				// say it worked
      }
      if (!priv()) return -(ERRM29);		// SET or KILL on ssvn not on
      systab->start_user = -1;			// Say 'shutting down'
      i = shmctl(systab->vol[0]->shm_id, (IPC_RMID), &sbuf); // remove the share
      LCK_Remove(DGP_SYSJOB);			// remove remote LOCKs
      for (i = 0; i < systab->maxjob; i++)	// for each job
      { cnt = systab->jobtab[i].pid;		// get pid
	if ((cnt != partab.jobtab->pid) && (cnt))
	  if (!kill( cnt, SIGTERM))		// kill this one
	  { systab->jobtab[i].trap = 1 << SIGTERM; // say go away
	    systab->jobtab[i].attention = 1;	// look at it
	  }
      }
      DB_Dismount(1);				// dismount main vol
      exit (0);					// and exit

    case 'L':					// $LOCK
      if (nsubs != 1) return (-ERRM38);		// junk
      if (!priv()) return -(ERRM29);		// SET or KILL on ssvn not on
      if (subs[0]->len > 511) return -(ERRMLAST+ERRZ12); // junk
      vp = (mvar *) &tmp[512];			// some temp space
      s = UTIL_MvarFromCStr(subs[0], vp);	// convert to mvar
      if (s < 0) return s;			// quit on error
      s = UTIL_mvartolock(vp, subs[0]->buf);	// cvt to locktab style
      if (s < 0) return s;			// quit on error
      subs[0]->len = s;				// save the length
      while (SemOp(SEM_LOCK, -systab->maxjob))	// until success, get semaphore
      { sleep(1);
      }
      s = LCK_Kill(subs[0]);			// doit
      i = SemOp(SEM_LOCK, systab->maxjob);	// drop semaphore
      return s;					// doit and exit

    case 'R':					// $ROUTINE
      if (nsubs > 1) return (-ERRM38);		// junk
      if (var->slen == '\0')			// if unsubscripted
      { u_char tmp[16];				// for below
        if (!priv()) return -(ERRM29);		// SET or KILL on ssvn not on
        i = DB_Data(var, tmp);			// see if it's defined
        if (i < 0) return i;			// quit on error
        if (i > 1) return -(ERRM33);		// KILL ^$ROUTINE routine exists
	return DB_Kill(var);			// give it to the database
      }						// end KILLing ^$ROUTINE
      if ((!priv()) &&				// check privs
	  ((partab.jobtab->ruci != var->uci) ||
	   (partab.jobtab->rvol != var->volset)))
        return -(ERRM29);			// SET or KILL on ssvn not on
      //rou.var_qu = 0;				// clear routine name
      X_Clear(rou.var_xu);			// clear routine name
      for (i = 0; i < MAX_NAME_BYTES; i++)
        if ((rou.var_cu[i] = subs[0]->buf[i]) == '\0')
          break;
      s = SemOp(SEM_ROU, -systab->maxjob);	// lock it
      if (s < 0) return s;			// quit if no go
      s = DB_Kill(var);				// give it to the database
      if (s >= 0)				// if OK
        //Routine_Delete(rou.var_qu, var->uci);	// mark as deleted
        Routine_Delete(rou.var_xu, var->uci,var->volset); // mark as deleted
      i = SemOp(SEM_ROU, systab->maxjob);	// release the lock
      return s;					// exit

    case 'S':					// $SYSTEM
      if ((nsubs == 4) &&
          (priv()) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "uci\0", 4) == 0)) // ^$S(vol,n,uci,n)
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	j = cstringtoi(subs[3]) - 1;		// and uci#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	if ((j < 0) || (j >= UCIS)) return (-ERRM60); // out of range
	return DB_UCIKill(i + 1, j + 1);	// doit and return
      }
      return (-ERRM38);				// Do vol dismount later
  }
  return (-ERRM38);				// can't get here?
}

short SS_Order(mvar *var, u_char *buf, int dir) // get next subscript
{ int i = 0;					// usefull int
  int j;					// and another
  short s;					// for functions
  int cnt;					// count of bytes used
  u_char tmp[1024];				// temp string space
  int ptmp = 0;					// pointer into this
  int nsubs = 0;				// count subscripts
  mvar *vp;					// variable ptr
  cstring *subs[4];				// where to put them
  while (i < var->slen)				// for all subs
  { cnt = 0;					// flag no rabit ears quotes
    if (nsubs > 3) return (-ERRM38);		// junk
    subs[nsubs] = (cstring *) &tmp[ptmp];	// point at the buffer
    s = UTIL_Key_Extract( &var->key[i],		// key from here
			  subs[nsubs]->buf,	// where to put it
			  &cnt);		// the count
    if (s < 0) return s;			// die on error
    subs[nsubs++]->len = s;			// save the size (incr count)
    ptmp = ptmp + s + sizeof(short) + 1;	// move up temp area
    i = i + cnt;				// count used bytes
  }
  i = SS_Norm(var);				// normalize the name
  if (i < 0) return i;				// return on error
  switch (var->name.var_cu[1])			// check initial of name
  { case 'G':					// $GLOBAL
      if (nsubs != 1) return (-ERRM38);		// junk
      return DB_Order(var, buf, dir);		// let the database module doit
    case 'J':					// $JOB
      if (nsubs != 1) return (-ERRM38);		// junk
      i = cstringtoi(subs[0]);			// make an int of job#
      buf[0] = '\0';				// null terminate
      if (dir < 0)				// backwards
      { if (i == 0) i = systab->maxjob + 1;	// setup the seed
	for (i = i-2; i > -1; i--)		// scan backwards
	{
	  if (systab->jobtab[i].pid != 0)	// found one
	  {
            if ((kill(systab->jobtab[i].pid, 0)) && // check the job
	        (errno == ESRCH))		// doesn't exist
	    {
	      CleanJob(i + 1);		        // zot if not there
	    }
	    else
	      break;				// else OK
	  }
	}
	i++;					// convert back to job#
      }
      else					// forward
      { for (i = i+0; i < systab->maxjob; i++)	// scan the list
	{
	  if (systab->jobtab[i].pid != 0)	// found one
	  {
            if ((kill(systab->jobtab[i].pid, 0)) && // check the job
	        (errno == ESRCH))		// doesn't exist
	    {
              CleanJob(i + 1);			// zot if not there
	    }
	    else
	      break;				// else OK
	  }
	}
	i++;					// convert back to job#
	if (i > systab->maxjob) i = 0;		//
      }
      if (i) return itocstring(buf, i);		// return job number
      return 0;					// or nothing

    case 'L':					// $LOCK
      if (nsubs != 1) return (-ERRM38);		// junk
      if (subs[0]->len > 511) return -(ERRMLAST+ERRZ12); // junk
      vp = (mvar *) &tmp[512];			// some temp space
      s = UTIL_MvarFromCStr(subs[0], vp);	// convert to mvar
      if (s < 0) return s;			// quit on error
      s = UTIL_mvartolock(vp, subs[0]->buf);	// cvt to locktab style
      if (s < 0) return s;			// quit on error
      subs[0]->len = s;				// save the length
      return LCK_Order(subs[0], buf, dir);	// doit and exit

    case 'R':					// $ROUTINE
      if (nsubs > 3) return (-ERRM38);		// junk
      return DB_Order(var, buf, dir);		// let the database module doit
    case 'S':					// $SYSTEM
      if ((nsubs == 2) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0))
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	if ((i < -1) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	buf[0] = '\0';				// JIC
	if (dir > 0)				// forward?
	{ for (j = i + 1; j < MAX_VOL; j++)
	    if (systab->vol[j]->vollab != NULL) break;
	  if (j == MAX_VOL) return 0;		// ran out
	  return itocstring(buf, j + 1);	// return vol#
	}
	if (i == -1) i = MAX_VOL;		// fix the seed
	for (j = i-1; j >= 0; j--)
	    if (systab->vol[j]->vollab != NULL) break;
	  if (j < 0) return 0;			// ran out
	  return itocstring(buf, j + 1);	// return vol#
      }
      if ((nsubs == 4) &&
          (strncasecmp( (char *) subs[0]->buf, "vol\0", 4) == 0) &&
	  (strncasecmp( (char *) subs[2]->buf, "uci\0", 4) == 0)) // ^$S(vol,n,uci,n)
      { i = cstringtoi(subs[1]) - 1;		// get vol#
	j = cstringtoi(subs[3]) - 1;		// and uci#
	if ((i < 0) || (i >= MAX_VOL)) return (-ERRM60); // out of range
	if ((j < -1) || (j >= UCIS)) return (-ERRM60); // out of range
	if (systab->vol[i]->vollab == NULL)
          return -(ERRZ90 + ERRMLAST); 		// not mounted
	buf[0] = '\0';				// JIC
	if (dir > 0)				// forward?
	{ for (j = j + 1; j < UCIS; j++)
	    //if (systab->vol[i]->vollab->uci[j].name.var_qu != 0)
	    if (!X_Empty(systab->vol[i]->vollab->uci[j].name.var_xu))
	      break;
	  if (j == UCIS) return 0;		// ran out
	  return itocstring(buf, j + 1);	// return uci#
	}
	if (j == -1) j = UCIS;			// fix the seed
	for (j = j-1; j >= 0; j--)
	    //if (systab->vol[i]->vollab->uci[j].name.var_qu != 0)
	    if (!X_Empty(systab->vol[i]->vollab->uci[j].name.var_xu))
	      break;
	  if (j < 0) return 0;			// ran out
	  return itocstring(buf, j + 1);	// return uci#
      }
      return (-ERRM38);				// junk
  }
  return (-ERRM38);				// can't get here?
}
