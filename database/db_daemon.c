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
#include <sys/shm.h>
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
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
static time_t last_daemon_check;                        // last daemon_check()

//------------------------------------------------------------------------------
// Function: MSLEEP
// Descript: sleep given milliseconds, if daemon 0 update Mtime
// Input(s): seconds to wait
// Return:   return value of sleep() system call
//

static
u_int MSLEEP(u_int mseconds)
{
  if (!myslot)
    systab->Mtime = time(0);

  //return sleep(seconds);
  return usleep(1000 * mseconds);
}

//-----------------------------------------------------------------------------
// Function: DB_Daemon
// Descript: Start daemon for passed in slot and vol#
// Input(s): slot# and Vol#
// Return:   0 -> Ok, any non-zero = error
//

extern int curr_sem_init;

int DB_Daemon(int slot, int vol)			// start a daemon
{ int i;						// a handy int 
  int k;						// and another
  int fit;						// for fork ret
  char logfile[100];					// daemon log file name
  FILE *a;						// file pointer
  time_t t;						// for ctime()

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
  bzero(semtab, sizeof(semtab));
  curr_sem_init = 1;
  last_daemon_check = (time_t) 0;

  // -- Create log file name --
  k = strlen(systab->vol[0]->file_name);		// get len of filename
  for (i=(k-1); (systab->vol[0]->file_name[i] != '/') && (i > -1); i--);
  							// find last '/'
  strncpy( logfile, systab->vol[0]->file_name, (i+1) );	// copy to log filename
  logfile[(i+1)] = (char) '\0';				// terminate JIC

  sprintf(&logfile[strlen(logfile)],"daemon_%d.log",slot); // add slot to name
  myslot = slot;					// remember my slot

  // --- Reopen stdin, stdout, and stderr ( logfile ) ---
  a = freopen("/dev/null","r",stdin);			// stdin to bitbucket
  a = freopen("/dev/null","w",stdout);			// stdout to bitbucket
  a = freopen(logfile,"a",stderr);			// stderr to logfile
  if (!a) return (errno);			        // check for error
  dbfd = open(systab->vol[0]->file_name, O_RDWR);	// open database r/wr
  if (dbfd < 0)
  { fprintf(stderr, "Cannot open database file %s\n",
                  systab->vol[0]->file_name);
    fflush( stderr );                                   // flush to the file
    return(errno);					// check for error
  }
  // i = fcntl(dbfd, F_NOCACHE, 1);
  t = time(0);						// for ctime()
  fprintf(stderr,"Daemon %d started successfully at %s\n",
	         myslot, ctime(&t));			// log success
  fflush( stderr );                                     // flush to the file

  if (!myslot)
    systab->Mtime = time(0);

  if ((systab->vol[0]->upto) && (!myslot))		// if map needs check
  { ic_map(-3, dbfd);					// doit
  }

  i = MSLEEP(1000);					// wait a bit

  while (TRUE)						// forever
  // { i = MSLEEP(1000);					// rest
  { i = MSLEEP(250);					// rest
  // { i = MSLEEP(125);					// rest
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
  time_t t;						// for ctime()

start:

  daemon_check();					// ensure all running
  if (systab->vol[volnum-1]->wd_tab[myslot].doing == DOING_NOTHING)
  { if ((!myslot) && (systab->vol[volnum-1]->map_dirty_flag)) // first daemon
    { file_off = lseek( dbfd, 0, SEEK_SET);		// move to start of file
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
    { while (TRUE)					// loop
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
	i = MSLEEP(1000);				// wait a bit
      }							// end while (TRUE)
      i = MSLEEP(1000);					// just a bit more
      systab->vol[volnum-1]->writelock = abs(systab->vol[volnum-1]->writelock);
      // Set the writelock to a positive value when all quiet
    }							// end wrtlock
    while (SemOp(SEM_WD, WRITE));			// lock WD
    if (systab->vol[volnum-1]->dirtyQ
	[systab->vol[volnum-1]->dirtyQr] != NULL)	// any writes?
    { systab->vol[volnum-1]->wd_tab[myslot].currmsg.gbddata
        = systab->vol[volnum-1]->dirtyQ[systab->vol[volnum-1]->dirtyQr]; // get
      systab->vol[volnum-1]->wd_tab[myslot].doing = DOING_WRITE;
      systab->vol[volnum-1]->dirtyQ[systab->vol[volnum-1]->dirtyQr] = NULL;
      systab->vol[volnum-1]->dirtyQr++;			// increment ptr
      systab->vol[volnum-1]->dirtyQr &= (NUM_DIRTY - 1); // do wrap
    }
    else if (systab->vol[volnum-1]->garbQ
	       [systab->vol[volnum-1]->garbQr]) 	// any garbage?
    { systab->vol[volnum-1]->wd_tab[myslot].currmsg.intdata
        = systab->vol[volnum-1]->garbQ[systab->vol[volnum-1]->garbQr]; // get
      systab->vol[volnum-1]->wd_tab[myslot].doing = DOING_GARB;
      systab->vol[volnum-1]->garbQ[systab->vol[volnum-1]->garbQr] = 0;
      systab->vol[volnum-1]->garbQr++;			// increment ptr
      systab->vol[volnum-1]->garbQr &= (NUM_GARB - 1);	// do wrap
    }
    SemOp( SEM_WD, -WRITE);				// release WD lock
  }							// end looking for work

  if (systab->vol[volnum-1]->wd_tab[myslot].doing == DOING_NOTHING)
  { if (systab->vol[volnum-1]->dismount_flag)		// dismounting?
    { if (myslot)					// first?
      { systab->vol[volnum-1]->wd_tab[myslot].pid = 0;	// say gone
	t = time(0);					// for ctime()
	fprintf(stderr,"Daemon %d shutting down at %s\n",
	         myslot, ctime(&t));			// log success
        fflush( stderr );
        SemStats();                                     // print sem stats
        exit (0);					// and exit
      }
      do_dismount();					// dismount it
      exit (0);						// and exit
    }							// end dismount code
    else
    { return;						// nothing to do
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
    SemOp( SEM_WD, WRITE );				// lock daemon table
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
    { MSLEEP(1000);					// wait a second...
    }
  }							// end wait for daemons
  SemStats();                                           // print sem stats
  fprintf(stderr,"Writing out clean flag as clean\n");  // operation
  fflush( stderr );
  systab->vol[volnum-1]->vollab->clean = 1;		// set database as clean
  off =lseek( dbfd, 0, SEEK_SET);			// seek to start of file
  if (off < 0)
  { fprintf(stderr, "do_dismount: lseek() to start of file failed");
    fflush( stderr );
  }
  i = write( dbfd, 					// write to database
	 systab->vol[volnum-1]->vollab, 		// the map/label block
	 systab->vol[volnum-1]->vollab->		// with the clean
	 header_bytes );				// flag set.
  if (i != systab->vol[volnum-1]->vollab->header_bytes )
  { fprintf(stderr, "do_dismount: write() map block failed");
    fflush( stderr );
  } 
  i = semctl(systab->sem_id, 0, (IPC_RMID), NULL);	// remove the semaphores
  if (i)
  { fprintf(stderr, "do_dismount: semctl() failed to remove semaphores");
    fflush( stderr );
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

  gbdptr = systab->vol[volnum-1]->			// get the gbdptr
  		wd_tab[myslot].currmsg.gbddata;		// from daemon table

  if (!gbdptr)
  { panic("Daemon: write msg gbd is NULL");		// check for null
  }

  if (curr_lock == 0)					// if we need a lock
  { SemOp( SEM_GLOBAL, READ);				// take a read lock
  }
  while (TRUE)						// until we break
  { if (gbdptr->last_accessed == (time_t) 0)		// if garbaged
    { gbdptr->block = 0;				// just zot the block
    }
    else						// do a write
    { file_off = (off_t) gbdptr->block - 1;		// block#
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
    { systab->vol[volnum-1]->wd_tab[myslot].		// update the daemon
		currmsg.gbddata = NULL;			// table JIC I vanish
      systab->vol[volnum-1]->wd_tab[myslot].
      		doing = DOING_NOTHING;			// and here
      break;						// break from while
    }
    lastptr = gbdptr;					// remember this ptr
    gbdptr = gbdptr->dirty;  				// get next in list

    if (lastptr != gbdptr)  				// if not at end
    { systab->vol[volnum-1]->wd_tab[myslot].		// update the daemon
		currmsg.gbddata = gbdptr;		// table JIC I vanish
    }
    else
    { systab->vol[volnum-1]->wd_tab[myslot].		// update the daemon
		currmsg.gbddata = NULL;			// table JIC I vanish
      systab->vol[volnum-1]->wd_tab[myslot].
      		doing = DOING_NOTHING;			// and here
    }
    lastptr->dirty = NULL;				// clear old dirtyptr
    if (lastptr == gbdptr)  				// if reached end
      break;  						// break from while
  }							// end dirty write
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

  bptr = malloc(systab->vol[volnum-1]->vollab->block_size); // get some memory
  if (bptr == NULL)					// if failed
  { fprintf(stderr, "do_zot: malloc for block %d failed\n", gb);
    fflush( stderr );                                   // flush to the file
    return -1;						// return fail
  }

  file_off = (off_t) gb - 1;				// the block#
  file_off = (file_off * (off_t) systab->vol[volnum-1]->vollab->block_size)
		+ (off_t) systab->vol[volnum-1]->vollab->header_bytes;

  while(SemOp(SEM_GLOBAL, READ));			// take a global lock
  ptr = systab->vol[volnum-1]->gbd_hash[gb & (GBD_HASH - 1)]; // get head
  while (ptr != NULL)					// for entire list
  { if (ptr->block == gb)				// found it?
    { bcopy(ptr->mem, bptr, systab->vol[volnum-1]->vollab->block_size);
      ptr->last_accessed = (time_t) 0;			// mark as zotted
      break;						// exit
    }
    ptr = ptr->next;					// point at next
  }							// end memory search
  SemOp(SEM_GLOBAL, -curr_lock);			// release the lock

  if (ptr == NULL)					// if not found
  { file_ret = lseek( dbfd, file_off, SEEK_SET);	// Seek to block
    if (file_ret < 1)
    { fprintf(stderr, "do_zot: seek to block %d failed\n", gb);
      fflush( stderr );                                 // flush to the file
      free(bptr);					// free memory
      return -1;					// return error
    }
    ret = read( dbfd, bptr,
	       systab->vol[volnum-1]->vollab->block_size); // read it
    if (ret < 0)					// if it failed
    { fprintf(stderr, "do_zot: read of block %d failed\n", gb);
      fflush( stderr );                                 // flush to the file
      free(bptr);					// free memory
      return -1;					// return error
    }
  }							// end read from disk

  typ = bptr->type;					// save type
  if (typ > 64)						// data type?
  { goto zotit;						// yes, just zot
  }

  for (Idx = LOW_INDEX; Idx <= bptr->last_idx; Idx++)	// for each entry
  { idx = (u_short *) bptr;				// point at the block
    iidx = (int *) bptr;				// point at the block
    chunk = (cstring *) &iidx[idx[Idx]];		// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    Allign_record();					// ensure alligned
    i = *(int *) record;				// get block#
    if (zot_data)					// if we are zotting
    { file_ret = (off_t) i - 1;				// block#
      file_ret = (file_ret * (off_t) systab->vol[volnum-1]->vollab->block_size)
		 + (off_t) systab->vol[volnum-1]->vollab->header_bytes;
      file_ret = lseek( dbfd, file_ret, SEEK_SET);	// Seek to block
      if (file_ret < 1)	        			// check for fail
      { fprintf(stderr, "do_zot: seek to block %d failed\n", i);
        fflush( stderr );                               // flush to the file
      }
      else						// looks ok
      { ret = write( dbfd, systab->vol[volnum-1]->zero_block,
	     systab->vol[volnum-1]->vollab->block_size); // write zeroes
        if (ret < 0)					// fail ?
        { fprintf(stderr, "do_zot: zero of block %d failed\n", i);
          fflush( stderr );                             // flush to the file
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
  file_ret = lseek( dbfd, file_off, SEEK_SET);		// Seek to block
  if (file_ret < 1)
  { fprintf(stderr, "do_zot: zeroing seek to block %d failed\n", gb);
    fflush( stderr );                                   // flush to the file
    free(bptr);						// free memory
    return -1;						// return error
  }
  ret = write( dbfd, systab->vol[volnum-1]->zero_block,
	     systab->vol[volnum-1]->vollab->block_size); // write zeroes
  if (ret < 0)						// if it failed
  { fprintf(stderr, "do_zot: zero of block %d failed\n", gb);
    fflush( stderr );                                   // flush to the file
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

  while (TRUE)						// a few times
  { daemon_check();					// ensure all running
    if (!SemOp( SEM_GLOBAL, WRITE))			// gain write lock
    // ;
    { break;						// it worked
    }
    MSLEEP(1000);					// wait a bit
  }
  
  Free_block(gb);					// free the block

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

  if (last_daemon_check == systab->Mtime)
    return;

  while (SemOp(SEM_WD, WRITE));				// lock WD
  for (i = 0; i < systab->vol[volnum - 1]->num_of_daemons; i++)
  { if (i != myslot)					// don't check self
    { if (kill(systab->vol[volnum-1]->wd_tab[i].pid, 0) < 0) // if gone
      { fit = DB_Daemon(i, volnum); 			// restart the daemon
        // SHOULD LOG THIS SUCCESS OR FAIL
        if (fit < 0)
        { fprintf(stderr, "daemon_check: failed to start Daemon %d\n", i);
          fflush( stderr );                             // flush to the file
        }
      }
    }
  }							// end daemon check
  SemOp(SEM_WD, -WRITE);				// release lock

  last_daemon_check = systab->Mtime;
  return;
}
