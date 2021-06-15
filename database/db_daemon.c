// File: mumps/database/db_daemon.c
//
// module database - Database Daemon Functions

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
#include <errno.h>					// for errors
#include <fcntl.h>					// for file stuff
#include <signal.h>					// for kill()
#include <stdarg.h>                                     // for va_list
#include <sys/shm.h>
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include <sys/time.h>                                   // for gettimeofday()
#include <assert.h>					// for assert()
#include "mumps.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

int dbfd;						// global db file desc
int myslot;						// my slot in WD table

void do_daemon();					// do something
void do_dismount();					// dismount volnum
void do_write();					// write GBDs
void do_garb();						// garbage collect
int do_zot(u_int gb);					// zot block and lower
void do_free(u_int gb);					// free from map et al
void ic_map(int flag, int dbfd);			// check the map
void daemon_check();					// ensure all running

#define CHKPT   systab->vol[0]->wd_tab[myslot].chkpt = __LINE__

char *last_status(void)
{ static char buf[128];
  int doing, chkpt;

  buf[0] = '\0';
  doing = systab->vol[0]->wd_tab[myslot].doing;
  chkpt = systab->vol[0]->wd_tab[myslot].chkpt;
  if (0 == chkpt)
    return buf;

  if (DOING_WRITE == doing)
  { sprintf(buf, "doing=WRITE gbddata=%#lx chkpt=%d",
                  (u_long)(systab->vol[0]->wd_tab[myslot].currmsg.gbddata),
                  chkpt);
  }
  else if (DOING_GARB == doing)
  { sprintf(buf, "doing=GARB intdata=%d chkpt=%d",
                  systab->vol[0]->wd_tab[myslot].currmsg.intdata,
                  chkpt);
  }
  else
  { sprintf(buf, "doing=%d chkpt=%d", doing, chkpt);
  }
  return buf;
}

int do_log(const char *fmt,...)
{ int i;
  va_list ap;
  char tstamp[64];
  struct timeval tv_result;
  struct tm *tm_result;
  int tstamp_filled;

  tstamp_filled = 0;
  if (0 == gettimeofday(&tv_result, 0))
  { tm_result = gmtime(&tv_result.tv_sec);
    if (tm_result != NULL)
    { if (0 != strftime(tstamp, 64, "%Y-%m-%dT%H:%M:%S", tm_result))
      { sprintf(tstamp + 19, ".%03d", (int) (tv_result.tv_usec / 1000));
        tstamp_filled = 1;
      }
    }
  }
  if (0 == tstamp_filled)
    strcpy(tstamp,"YYYY-MM-DDTHH:MM:SS.MMM");

  fprintf(stderr,"%s [%5d] ", tstamp, getpid());
  va_start(ap, fmt);
  i = vfprintf(stderr,fmt,ap);
  va_end(ap);

  fflush(stderr);
  return i;
}

//------------------------------------------------------------------------------
// Function: MSLEEP
// Descript: sleep given seconds, if daemon 0 update Mtime
// Input(s): seconds to wait
// Return:   return value of sleep() system call
//

static
u_int MSLEEP(u_int seconds)
{
  CHKPT;
  if (!myslot)
    systab->Mtime = time(0);

  return sleep(seconds);
}

//-----------------------------------------------------------------------------
// Function: DB_Daemon
// Descript: Start daemon for passed in slot and vol#
// Input(s): slot# and Vol#
// Return:   0 -> Ok, any non-zero = error
//

int DB_Daemon(int slot, int vol)			// start a daemon
{ int i;						// a handy int 
  int k;						// and another
  int fit;						// for fork ret
  char logfile[100];					// daemon log file name
  FILE *a;						// file pointer
  char *msg;

  volnum = vol;						// save vol# here

  fit = ForkIt(-1);					// start a daemon
  if (fit > 0)						// check for ok (parent)
  { systab->vol[volnum-1]->wd_tab[slot].pid = fit;	// put in childs pid
    return (0);						// return (am parent)
  }							// end parent code
  if (fit < 0)
  { return (errno);					// die on error
  }
  curr_lock = 0;					// clear lock flag

  // -- Create log file name --
  k = strlen(systab->vol[0]->file_name);		// get len of filename
  for (i=(k-1); (systab->vol[0]->file_name[i] != '/') && (i > -1); i--);
  							// find last '/'
  strncpy( logfile, systab->vol[0]->file_name, (i+1) );	// copy to log filename
  logfile[(i+1)] = (char) '\0';				// terminate JIC

  sprintf(&logfile[strlen(logfile)],"daemon_%d.log",slot); // add slot to name
  myslot = slot;					// remember my slot
  msg = last_status();                                  // save last status
  CHKPT;

  // --- Reopen stdin, stdout, and stderr ( logfile ) ---
  a = freopen("/dev/null","r",stdin);			// stdin to bitbucket
  a = freopen("/dev/null","w",stdout);			// stdout to bitbucket
  a = freopen(logfile,"a",stderr);			// stderr to logfile
  CHKPT;
  if (!a) return (errno);			        // check for error
  CHKPT;
  dbfd = open(systab->vol[0]->file_name, O_RDWR);	// open database r/wr
  if (dbfd < 0)
  { do_log("Cannot open database file %s\n",
                  systab->vol[0]->file_name);
    return(errno);					// check for error
  }
  // i = fcntl(dbfd, F_NOCACHE, 1);
  if (msg[0])                                           // check restart
    do_log("Daemon %d restarted (status: %s)\n", myslot, msg); // log restart
  else
    do_log("Daemon %d started successfully\n", myslot); // log success

  if (!myslot)
    systab->Mtime = time(0);

  if ((systab->vol[0]->upto) && (!myslot))		// if map needs check
  { CHKPT;
    ic_map(-3, dbfd);					// doit
  }

  i = MSLEEP(1);					// wait a bit

  CHKPT;
  while (TRUE)						// forever
  { i = MSLEEP(1);					// rest
    do_daemon();					// do something
  }
  return 0;						// never gets here
}

//-----------------------------------------------------------------------------
// Function: do_daemon
// Descript: do daemon type things
// Input(s): none
// Return:   none
//

void do_daemon()					// do something
{ int i;						// handy int
  int j;						// and another
  off_t file_off;					// for lseek()
  int dirtyQr, garbQr;                                  // queue read positions

start:
  if (!myslot)                                          // update M time
    systab->Mtime = time(0);

  CHKPT;
  daemon_check();					// ensure all running
  if (systab->vol[volnum-1]->wd_tab[myslot].doing == DOING_NOTHING)
  { if ((!myslot) && (systab->vol[volnum-1]->map_dirty_flag)) // first daemon
    { CHKPT;
      file_off = lseek( dbfd, 0, SEEK_SET);		// move to start of file
      if (file_off<0)
      { systab->vol[volnum-1]->stats.diskerrors++;	// count an error
        panic("do_daemon: lseek() to start of file failed");
      }
      i = write( dbfd, systab->vol[volnum-1]->vollab,
		 systab->vol[volnum-1]->vollab->header_bytes);// map/label
      if (i < 0)
      { systab->vol[volnum-1]->stats.diskerrors++;	// count an error
        panic("do_daemon: write() map block failed");
      }
      systab->vol[volnum-1]->map_dirty_flag = 0;	// unset dirty flag
      systab->vol[volnum-1]->stats.phywt++;		// count a write
    }							// end map write
    if ((!myslot) && (systab->vol[volnum-1]->writelock < 0)) // check wrtlck
    { CHKPT;
      while (TRUE)					// loop
      { i = (systab->vol[volnum-1]->dirtyQ[systab->vol[volnum-1]->dirtyQr]
	     != NULL);					// check dirty que
	i = ((i) ||
	     (systab->vol[volnum-1]->garbQ[systab->vol[volnum-1]->garbQr]
	      != 0));					// and garbQ
	for (j = 1; j < systab->vol[volnum-1]->num_of_daemons; j++) // each one
	{ i = ((i) || (systab->vol[volnum-1]->wd_tab[myslot].doing != 0));
	}
	if (!i)						// if all clear
	{ break;					// leave loop
	}
	daemon_check();					// ensure all running
	i = MSLEEP(1);					// wait a bit
      }							// end while (TRUE)
      i = MSLEEP(1);					// just a bit more
      systab->vol[volnum-1]->writelock = abs(systab->vol[volnum-1]->writelock);
      // Set the writelock to a positive value when all quiet
    }							// end wrtlock
    CHKPT;
    while (SemOp(SEM_WD, WRITE));			// lock WD
    CHKPT;
    dirtyQr = systab->vol[volnum-1]->dirtyQr;
    garbQr  = systab->vol[volnum-1]->garbQr;
    if (systab->vol[volnum-1]->dirtyQ[dirtyQr] != NULL)	// any writes?
    { CHKPT;
      CheckGBD(systab->vol[volnum-1]->dirtyQ[dirtyQr]);
      systab->vol[volnum-1]->wd_tab[myslot].currmsg.gbddata
        = systab->vol[volnum-1]->dirtyQ[dirtyQr];       // get
      systab->vol[volnum-1]->wd_tab[myslot].doing = DOING_WRITE;
      systab->vol[volnum-1]->dirtyQ[dirtyQr] = NULL;
      systab->vol[volnum-1]->dirtyQr =                  // increment ptr
        (dirtyQr + 1) & (NUM_DIRTY - 1);                //   with wrap
    }
    else if (systab->vol[volnum-1]->garbQ[garbQr]) 	// any garbage?
    { CHKPT;
      systab->vol[volnum-1]->wd_tab[myslot].currmsg.intdata
        = systab->vol[volnum-1]->garbQ[garbQr];         // get
      systab->vol[volnum-1]->wd_tab[myslot].doing = DOING_GARB;
      systab->vol[volnum-1]->garbQ[garbQr] = 0;
      systab->vol[volnum-1]->garbQr =                   // increment ptr
        (garbQr + 1) & (NUM_GARB - 1);	                //   with wrap
    }
    CHKPT;
    SemOp( SEM_WD, -WRITE);				// release WD lock
    CHKPT;
  }							// end looking for work

  if (systab->vol[volnum-1]->wd_tab[myslot].doing == DOING_NOTHING)
  { CHKPT;
    if (systab->vol[volnum-1]->dismount_flag)		// dismounting?
    { CHKPT;
      if (myslot)					// first?
      { systab->vol[volnum-1]->wd_tab[myslot].pid = 0;	// say gone
	do_log("Daemon %d shutting down\n", myslot);    // log success
        exit(0);					// and exit
      }
      do_dismount();					// dismount it
      exit(0);						// and exit
    }							// end dismount code
    else
    { CHKPT;
      return;						// nothing to do
    }
  }
  if (systab->vol[volnum-1]->wd_tab[myslot].doing == DOING_WRITE)
  { do_write();						// do it 
    goto start;						// try again
  }
  if (systab->vol[volnum-1]->wd_tab[myslot].doing == DOING_GARB)
  { do_garb();						// or this 
    goto start;						// try again
  }
  return;						// can't get here
}

//-----------------------------------------------------------------------------
// Function: do_dismount
// Descript: Dismount current volnum
// Input(s): none
// Return:   none
//

void do_dismount()					// dismount volnum
{ int i;						// handy int
  int j;						// and another
  int pid;						// for jobs
  struct shmid_ds sbuf;					// for shmctl
  off_t off;                                            // for lseek

  CHKPT;
  i = shmctl(systab->vol[0]->shm_id, (IPC_RMID), &sbuf); //remove share
  for (i = 0; i < systab->maxjob; i++)			// for each job
  { pid = systab->jobtab[i].pid;			// get pid
    if (pid)						// if pid != 0
    if (kill( pid, SIGTERM))				// kill this one
    { systab->jobtab[i].trap = 1 << SIGTERM;		// say go away
      systab->jobtab[i].attention = 1;			// look at it
    }
  }

  for (i=0; i<systab->vol[volnum-1]->num_gbd; i++)	// look for unwritten
  { if ((systab->vol[volnum-1]->gbd_head[i].block) && 	// if there is a blk
        (systab->vol[volnum-1]->gbd_head[i].last_accessed != (time_t) 0) &&
	(systab->vol[volnum-1]->gbd_head[i].dirty))
    { systab->vol[volnum-1]->gbd_head[i].dirty
        = &systab->vol[volnum-1]->gbd_head[i]; 		// point at self

      systab->vol[volnum-1]->wd_tab[0].currmsg.gbddata  // add to our struct
	= &systab->vol[volnum-1]->gbd_head[i];
      do_write();					// write it
    }							// end gbd has blk
  }							// end blk search

  i = 1;
  while (i)						// while theres pids
  { i = 0;						// reset pid counter
    while (SemOp( SEM_WD, WRITE ));			// lock daemon table
    for (j=1; j<systab->vol[volnum-1]->num_of_daemons; j++) // search
    { if (systab->vol[volnum-1]->wd_tab[j].pid)
      { if (kill(systab->vol[volnum-1]->wd_tab[j].pid, 0))
        { if (errno == ESRCH)				// if no such
          { systab->vol[volnum-1]->wd_tab[j].pid = 0;	// clear it
          }
          else
          { i = 1;					// remember still there
	  }
        }
	else
	{ i = 1;
	}
      }
    }
    SemOp( SEM_WD, -WRITE );				// unlock daemon table
    if (i)						// if pids still around
    { MSLEEP(1);					// wait a second...
    }
  }							// end wait for daemons
  do_log("Writing out clean flag as clean\n");            // operation
  systab->vol[volnum-1]->vollab->clean = 1;		// set database as clean
  off =lseek( dbfd, 0, SEEK_SET);			// seek to start of file
  if (off < 0)
  { do_log("do_dismount: lseek() to start of file failed\n");
  }
  i = write( dbfd, 					// write to database
	 systab->vol[volnum-1]->vollab, 		// the map/label block
	 systab->vol[volnum-1]->vollab->		// with the clean
	 header_bytes );				// flag set.
  if (i != systab->vol[volnum-1]->vollab->header_bytes )
  { do_log("do_dismount: write() map block failed\n");
  } 
  close(dbfd);                                          // close database file
  i = semctl(systab->sem_id, 0, (IPC_RMID), NULL);	// remove the semaphores
  if (i)
  { do_log("do_dismount: semctl() failed to remove semaphores\n");
  }
  return;						// done
}

//-----------------------------------------------------------------------------
// Function: do_write
// Descript: Write out dirty GBDs
// Input(s): none
// Return:   none
//

void do_write()						// write GBDs
{ off_t file_off;                               	// for lseek() et al
  int i;						// a handy int
  gbd *gbdptr;						// for the gbd
  gbd *lastptr = NULL;					// for the gbd

  CHKPT;
  gbdptr = systab->vol[volnum-1]->			// get the gbdptr
  		wd_tab[myslot].currmsg.gbddata;		// from daemon table

  CheckGBD(gbdptr);
  if (!gbdptr)
  { panic("Daemon: write msg gbd is NULL");		// check for null
  }

  CHKPT;
  if (curr_lock == 0)					// if we need a lock
  { while (SemOp( SEM_GLOBAL, READ));			// take a read lock
  }
  CHKPT;
  while (TRUE)						// until we break
  { if (gbdptr->last_accessed == (time_t) 0)		// if garbaged
    { CHKPT;
      gbdptr->block = 0;				// just zot the block
    }
    else						// do a write
    { CHKPT;
      file_off = (off_t) gbdptr->block - 1;		// block#
      file_off = (file_off * (off_t)
    			systab->vol[volnum-1]->vollab->block_size)
		 + (off_t) systab->vol[volnum-1]->vollab->header_bytes;
      file_off = lseek( dbfd, file_off, SEEK_SET);	// Seek to block
      if (file_off < 1)
      { systab->vol[volnum-1]->stats.diskerrors++;	// count an error
        panic("lseek failed in Write_Chain()!!");	// die on error
      }
      i = write( dbfd, gbdptr->mem,
		 systab->vol[volnum-1]->vollab->block_size); // write it
      if (i < 0)
      { systab->vol[volnum-1]->stats.diskerrors++;	// count an error
        panic("write failed in Write_Chain()!!");
      }
      systab->vol[volnum-1]->stats.phywt++;		// count a write

    }							// end write code

    if (!gbdptr->dirty)
    { CHKPT;
      systab->vol[volnum-1]->wd_tab[myslot].		// update the daemon
		currmsg.gbddata = NULL;			// table JIC I vanish
      systab->vol[volnum-1]->wd_tab[myslot].
      		doing = DOING_NOTHING;			// and here
      break;						// break from while
    }
    CHKPT;
    lastptr = gbdptr;					// remember this ptr
    gbdptr = gbdptr->dirty;  				// get next in list
    CheckGBD(gbdptr);

    CHKPT;
    if (lastptr != gbdptr)  				// if not at end
    { CHKPT;
      systab->vol[volnum-1]->wd_tab[myslot].		// update the daemon
		currmsg.gbddata = gbdptr;		// table JIC I vanish
    }
    else
    { CHKPT;
      systab->vol[volnum-1]->wd_tab[myslot].		// update the daemon
		currmsg.gbddata = NULL;			// table JIC I vanish
      systab->vol[volnum-1]->wd_tab[myslot].
      		doing = DOING_NOTHING;			// and here
    }
    CHKPT;
    lastptr->dirty = NULL;				// clear old dirtyptr
    if (lastptr == gbdptr)  				// if reached end
    { CHKPT;
      break;  						// break from while
    }
  }							// end dirty write
  CHKPT;
  SemOp( SEM_GLOBAL, -curr_lock);			// release lock
  return;						// done

}

//-----------------------------------------------------------------------------
// Function: do_garb
// Descript: Garbage collect some block(s)
//	     At this stage:	there is NO recovery.
//				there is no integrity check.
// Input(s): none
// Return:   none
//

void do_garb()						// garbage collect
{ u_int gb;						// block being garbed

  CHKPT;
  if (systab->vol[volnum-1]->wd_tab[myslot].currmsg.intdata == 0) // done?
  { systab->vol[volnum-1]->wd_tab[myslot].doing = DOING_NOTHING; // yes
    return;						// and exit
  }

  gb = systab->vol[volnum-1]->wd_tab[myslot].currmsg.intdata; // get block
  systab->vol[volnum-1]->wd_tab[myslot].currmsg.intdata = 0;  // clear slot

  gb = do_zot(gb);					// do it

  systab->vol[volnum-1]->wd_tab[myslot].doing = DOING_NOTHING; // flag done
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: do_zot
// Descript: Zot block(s)
//	     At this stage:	there is NO recovery.
//				there is no integrity check.
// Input(s): block number to zot
// Return:   negative error number or type byte of block zotted.
//

int do_zot(u_int gb)					// zot block
{ u_int i;						// a handy int
  int ret;						// for returns
  int Idx;						// the index
  DB_Block *bptr;					// block pointer
  off_t file_off;                               	// for lseek() et al
  off_t file_ret;                               	// for lseek() et al
  int typ;						// block type
  int zot_data = 0;					// bottom level flag
  gbd *ptr;						// a handy pointer

  CHKPT;
  bptr = malloc(systab->vol[volnum-1]->vollab->block_size); // get some memory
  if (bptr == NULL)					// if failed
  { do_log("do_zot: malloc for block %d failed\n", gb);
    return -1;						// return fail
  }

  CHKPT;
  file_off = (off_t) gb - 1;				// the block#
  file_off = (file_off * (off_t) systab->vol[volnum-1]->vollab->block_size)
		+ (off_t) systab->vol[volnum-1]->vollab->header_bytes;

  CHKPT;
  while(SemOp(SEM_GLOBAL, WRITE));			// take a global lock
  CHKPT;
  ptr = systab->vol[volnum-1]->gbd_hash[gb & (GBD_HASH - 1)]; // get head
  while (ptr != NULL)					// for entire list
  { if (ptr->block == gb)				// found it?
    { bcopy(ptr->mem, bptr, systab->vol[volnum-1]->vollab->block_size);
      // NB. Setting last_accessed to zero could cause
      //     indefinite waiting in Get_buffer(), when a READER
      //     wants to read a block which is not cached, and
      //     the global is modified by another job without a
      //     LOCK.
      //     WRITE lock does NOT help here either.
      ptr->last_accessed = (time_t) 0;			// mark as zotted
      break;						// exit
    }
    ptr = ptr->next;					// point at next
  }							// end memory search
  CHKPT;
  SemOp(SEM_GLOBAL, -curr_lock);			// release the lock

  if (ptr == NULL)					// if not found
  { file_ret = lseek( dbfd, file_off, SEEK_SET);	// Seek to block
    if (file_ret < 1)
    { do_log("do_zot: seek to block %d failed\n", gb);
      free(bptr);					// free memory
      return -1;					// return error
    }
    ret = read( dbfd, bptr,
	       systab->vol[volnum-1]->vollab->block_size); // read it
    if (ret < 0)					// if it failed
    { do_log("do_zot: read of block %d failed\n", gb);
      free(bptr);					// free memory
      return -1;					// return error
    }
  }							// end read from disk

  typ = bptr->type;					// save type
  if (typ > 64)						// data type?
  { goto zotit;						// yes, just zot
  }

  CHKPT;
  for (Idx = LOW_INDEX; Idx <= bptr->last_idx; Idx++)	// for each entry
  { idx = (u_short *) bptr;				// point at the block
    iidx = (int *) bptr;				// point at the block
    chunk = (cstring *) &iidx[idx[Idx]];		// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    Allign_record();					// ensure alligned
    CHKPT;
    i = *(int *) record;				// get block#
    if (zot_data)					// if we are zotting
    { CHKPT;
      file_ret = (off_t) i - 1;				// block#
      file_ret = (file_ret * (off_t) systab->vol[volnum-1]->vollab->block_size)
		 + (off_t) systab->vol[volnum-1]->vollab->header_bytes;
      file_ret = lseek( dbfd, file_ret, SEEK_SET);	// Seek to block
      if (file_ret < 1)	        			// check for fail
      { do_log("do_zot: seek to block %d failed\n", i);
      }
      else						// looks ok
      { ret = write( dbfd, systab->vol[volnum-1]->zero_block,
	     systab->vol[volnum-1]->vollab->block_size); // write zeroes
        if (ret < 0)					// fail ?
        { do_log("do_zot: zero of block %d failed\n", i);
        }
        systab->vol[volnum-1]->stats.phywt++;		// count a write
        systab->vol[volnum-1]->stats.logwt++;		// and a logical
        do_free(i);					// free the block
      }
    }							// end zotting
    else						// give to lower level
    { ret = do_zot(i);					// re-call
      if (ret > 64)					// data block?
      { zot_data = TRUE;				// do the rest here
      }
    }
  }							// end of indexes

zotit:
  CHKPT;
  file_ret = lseek( dbfd, file_off, SEEK_SET);		// Seek to block
  if (file_ret < 1)
  { do_log("do_zot: zeroing seek to block %d failed\n", gb);
    free(bptr);						// free memory
    return -1;						// return error
  }
  ret = write( dbfd, systab->vol[volnum-1]->zero_block,
	     systab->vol[volnum-1]->vollab->block_size); // write zeroes
  if (ret < 0)						// if it failed
  { do_log("do_zot: zero of block %d failed\n", gb);
    typ = -1;						// flag fail
  }
  systab->vol[volnum-1]->stats.phywt++;			// count a write
  systab->vol[volnum-1]->stats.logwt++;			// and a logical
  free(bptr);						// free memory
  do_free(gb);						// and the block
  return typ;						// return the type
}

//-----------------------------------------------------------------------------
// Function: do_free
// Descript: Free a block block in the map and GBDs (if reqd)
// Input(s): block number to free
// Return:   none
//

void do_free(u_int gb)					// free from map et al
{ gbd *ptr;						// GBD ptr

  CHKPT;
  while (TRUE)						// a few times
  { daemon_check();					// ensure all running
    if (!SemOp( SEM_GLOBAL, WRITE))			// gain write lock
    { break;						// it worked
    }
    MSLEEP(1);						// wait a bit
  }
  
  Free_block(gb);					// free the block

  CHKPT;
  ptr = systab->vol[volnum-1]->gbd_hash[gb & (GBD_HASH - 1)]; // get listhead
  while (ptr != NULL)					// for each in list
  { if (ptr->block == gb)				// found it
    { if (ptr->dirty < (gbd *) 3)			// not in use
      { Free_GBD(ptr);					// free it
      }
      else						// in use or not locked
      { ptr->last_accessed = (time_t) 0;		// mark as zotted
      }
      break;						// and exit the loop
    }
    ptr = ptr->next;					// get next
  }							// end gbd stuff
  CHKPT;
  SemOp( SEM_GLOBAL, -curr_lock);			// release lock
  return;						// exit
}

//-----------------------------------------------------------------------------
// Function: daemon_check
// Descript: Ensure all daemons are currently running
// Input(s): none
// Return:   none
//

void daemon_check()					// ensure all running
{ int i;						// a handy int
  int fit;

  CHKPT;
  while (SemOp(SEM_WD, WRITE));				// lock WD
  for (i = 0; i < systab->vol[volnum - 1]->num_of_daemons; i++)
  { if (i != myslot)					// don't check self
    { fit = kill(systab->vol[volnum-1]->wd_tab[i].pid, 0);
      if ((fit < 0) && (errno == ESRCH))               	// if gone
      { fit = DB_Daemon(i, volnum); 			// restart the daemon
        // SHOULD LOG THIS SUCCESS OR FAIL
        if (fit != 0)
        { do_log("daemon_check: failed to start daemon %d - %s\n",
		i, strerror(fit));
        }
      }
    }
  }							// end daemon check
  SemOp(SEM_WD, -WRITE);				// release lock
  return;
}
