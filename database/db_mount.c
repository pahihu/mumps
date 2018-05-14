// File: mumps/init/init_start.c
//
// module database - mounting a dataset

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


#include <stdio.h>                      // always include
#include <stdlib.h>                     // these two
#include <sys/types.h>                  // for u_char def
#include <string.h>                     // string controls always handy
#include <strings.h>
#include <ctype.h>                      // this is handy too
#include <sys/param.h>                  // for realpath() function
#include <errno.h>                      // error stuf
#include <fcntl.h>                      // file stuff
#include <time.h>                       // for time(0)
#include <unistd.h>                     // database access
#include <sys/ipc.h>                    // shared memory
#include <sys/shm.h>                    // shared memory
#include <sys/sem.h>                    // semaphores
#include <sys/stat.h>                   // stat
#include "mumps.h"                      // standard includes
#include "proto.h"                      // standard includes
#include "database.h"                   // database includes
#include "error.h"                      // standard includes

#if defined(linux) || defined(__CYGWIN__)
union semun {
        int val;                        /* value for SETVAL */
        struct semid_ds *buf;           /* buffer for IPC_STAT, IPC_SET */
        unsigned short int *array;      /* array for GETALL, SETALL */
        struct seminfo *__buf;          /* buffer for IPC_INFO */
};
#endif                                  //linux

#if defined(__CYGWIN__)
#define IPC_R    000400  /* read permission */
#define IPC_W    000200  /* write/alter permission */
#define	SHM_R	(IPC_R)
#define	SHM_W	(IPC_W)
#endif


extern int rwlock_init();

int DB_Daemon( int slot, int vol); 		// proto DB_Daemon
#ifdef MV1_GBDRO
void Free_GBDRO(gbd *ptr);                      // proto for R/O GBD release
#endif

//****************************************************************************
// Init an environment
//
short DB_Mount( char *file,                     // database
                int volume,                     // where to mount
                int gmb,                        // mb of global buf
                int jkb)                        // kb of jrn buf
{ int dbfd;                                     // database file descriptor
  int hbuf[SIZEOF_LABEL_BLOCK/sizeof(int)];     // header buffer
  int i;                                        // usefull int
  int n_gbd;                                    // number of gbd
  size_t volset_size;                           // size of volset struct (bytes)
  size_t pagesize;                              // system pagesize (bytes)
  char fullpathvol[MAXPATHLEN];			// full pathname of vol file
  gbd *gptr;					// a gbd pointer
  u_char *ptr;					// and a byte one
  label_block *labelblock;			// label block pointer
  int syncjrn;                                  // sync jrn buf
  int minjkb;                                   // min JRN buf size
  int vol;                                      // vol[] indx
  int jobs;                                     // no. of jobs
  size_t addoff;                                // where to add this volume
  label_block *vollab;				// volset label block

  if ((volume < 2) || (volume > MAX_VOL))       // validate volume
    return -(ERRZ74+ERRMLAST);
  if (systab->vol[volume-1]->file_name[0] != 0) // check for already open
    return -(ERRZ74+ERRMLAST);

  vol  = volume - 1;                            // vol[] index
  jobs = systab->maxjob;                        

  if (systab->maxjob == 1)                      // if single user mode
  { return -(ERRZ74+ERRMLAST);                  //   complain
  }
  // ERRZ60 insuff global buffer space
  pagesize = getpagesize();                     // get sys pagesize (bytes)
  if (gmb == 0) gmb = jobs/2;                   // default global buffers
  if (gmb < 1) gmb = 1;                         // but at least 1mb

  dbfd = open(file, O_RDWR);                    // open the database read/write
  if (dbfd < 1)                                 // if that failed
  { fprintf( stderr,
             "Open of database %s failed\n - %s\n", // complain
             file,                              // what we tried
             strerror(errno));                  // what was returned
    return -(errno+ERRMLAST+ERRZLAST);          // exit with error
  }                                             // end file create test
#ifdef MV1_F_NOCACHE
  i = fcntl(dbfd, F_NOCACHE, 1);
#endif
  i = read(dbfd, hbuf, SIZEOF_LABEL_BLOCK);     // read label block
  if (i < SIZEOF_LABEL_BLOCK)                   // in case of error
  { fprintf( stderr, "Read of lable block failed\n - %s\n", // complain
             strerror(errno));
    close(dbfd);
    return -(errno+ERRMLAST+ERRZLAST);          // exit with error
  }

  labelblock = (label_block *) hbuf;		// point label block at it
  if (labelblock->db_ver != DB_VER)		// if we need to upgrade
  { fprintf(stderr,
	 "Database is version %04x, image requires version %04x - start failed!!\n",
	 labelblock->db_ver, DB_VER);
      close(dbfd);
      return -(ERRZ73+ERRMLAST);
  }						// end upgrade proceedure
  if (labelblock->magic != MUMPS_MAGIC)
  { fprintf( stderr, "Invalid MUMPS database (wrong magic) - start failed!!\n");
    close(dbfd);
    return -(ERRZ73+ERRMLAST);
  }

  n_gbd  = (gmb * MBYTE) / hbuf[3];		// number of gbd
  while (n_gbd < MIN_GBD)			// if not enough
  { gmb++;					// increase it
    n_gbd = (gmb * MBYTE) / hbuf[3];		// number of gbd
  }
  n_gbd -= NUM_GBDRO;                           // remove the R/O GBDs

  syncjrn = 1;                                  // buffer flush w/ fsync()
  if (jkb < 0)                                  // if negative
  { syncjrn = 0;                                //   then disable fsync()
    jkb   = -jkb;
  }
  minjkb = ((MIN(hbuf[3], MAX_STR_LEN) + sizeof(jrnrec)) * MIN_JRNREC) / 1024;
  if (minjkb > jkb)
  { jkb = minjkb;
  }
  jkb *= 1024;
  jkb = (((jkb - 1) / pagesize) + 1) * pagesize;
  jkb /= 1024;

  volset_size = hbuf[2]				// size of head and map block
	      + ((n_gbd + NUM_GBDRO) * sizeof(struct GBD))	// the gbd
              + (gmb * MBYTE)		  	// mb of global buffers
              + hbuf[3]			       	// size of block (zero block)
              + jkb * KBYTE                     // size of JRN buf
              + (sizeof(u_int) * (systab->maxjob)) // size of last_blk_used[]
              + (sizeof(u_int) * (systab->maxjob));// size of last_blk_written[]
  volset_size = (((volset_size - 1) / pagesize) + 1) * pagesize; // round up

  if (volset_size > systab->addsize)            // check avail additional mem.
  { close(dbfd);
    return -(ERRZ60+ERRMLAST);
  }
  addoff = systab->addoff;                      // where to put them

  // XXX - SEM
#if 0
  sem_id = semget(shar_mem_key, SEM_MAX,	// Use SMH_ not SEM_ as
                  (SHM_R|SHM_W|(SHM_R>>3)	// linux needs that
                  |(SHM_W>>3)|IPC_CREAT)); 	// create semaphores
  if (sem_id < 0)
  { fprintf( stderr, "Unable to create semaphore set - %s\n",
            strerror(errno));                   // give an error
    return(errno);                              // and return with error
  }
  i = semctl(sem_id, 0, SETALL, semvals);	// clear the values
  if (i < 0)					// check for error
  { fprintf( stderr, "Unable to clear semaphores - %s\n",
            strerror(errno));                   // give an error
    i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
    return(errno);                              // and return with error
  }
#endif

  bzero((void *)systab + addoff, volset_size);		// zot the lot

						// volume set memory
  // systab->vol[vol]->vollab =
  vollab = (label_block *) ((void *)systab + addoff);
                                                // and point to label blk
  systab->vol[vol]->map = (void*)((void *)vollab + SIZEOF_LABEL_BLOCK);
						// and point to map
  systab->vol[vol]->first_free = systab->vol[vol]->map;	// init first free

  systab->vol[vol]->gbd_head = (gbd *) ((void *)vollab + hbuf[2]); // gbds
  systab->vol[vol]->num_gbd = n_gbd;		// number of gbds

  systab->vol[vol]->global_buf =
    (void *) &systab->vol[vol]->gbd_head[n_gbd + NUM_GBDRO];//glob buffs
  systab->vol[vol]->zero_block =
    (void *) &(((u_char *)systab->vol[vol]->global_buf)[gmb*MBYTE]);
						// pointer to zero blk

  systab->vol[vol]->jrnbuf =                    // JRN buf
    (void *)((void *)systab->vol[vol]->zero_block + hbuf[3]);
  systab->vol[vol]->jrnbufcap  = jkb * KBYTE;   // the size
  systab->vol[vol]->jrnbufsize = 0;             // buffer empty
  systab->vol[vol]->syncjrn    = syncjrn;       // sync flag

  systab->vol[vol]->last_blk_used =               // setup last_blk_used[]
    (void *) ((void*)systab->vol[vol]->jrnbuf + jkb * KBYTE);
  systab->vol[vol]->last_blk_written =            // setup last_blk_written[]
    (void *) ((void*)systab->vol[vol]->last_blk_used + 
                                        sizeof(u_int) * (systab->maxjob));

  systab->vol[vol]->shm_id = systab->vol[0]->shm_id;	// set up share id
  systab->vol[vol]->map_dirty_flag = 0;		// clear dirty map flag
  systab->vol[vol]->hash_start = 0;             // start searching here
  systab->vol[vol]->gmb        = gmb;           // global bufffer cache in MB
  systab->vol[vol]->jkb        = jkb;           // jrn buffer cache in KB
  systab->vol[vol]->gbsync     = DEFAULT_GBSYNC;// global buffer sync in sec

  // bzero(semtab, sizeof(semtab));

  if ( (realpath( file, fullpathvol) ) )	// get full path
    { if (strlen(fullpathvol) < VOL_FILENAME_MAX) // if can fit in our struct
        { strcpy( systab->vol[vol]->file_name, 	// copy this full path into
		  fullpathvol );		// the vol_def structure
        }					// end if path will fit
      else						// otherwise
        { i = VOL_FILENAME_MAX - strlen(fullpathvol);	// copy as much as
          strcpy( systab->vol[vol]->file_name,		// is possible, thats
		  &fullpathvol[i] );			// the best we can do
        }						// end length testing
    }							// end realpath worked
  else						// otherwise there was an error
    { close(dbfd);
      return -(errno+ERRMLAST+ERRZLAST);	// exit with error
    }

  i = lseek(dbfd, 0, SEEK_SET);			// re-point at start of file
  i = read(dbfd, vollab, hbuf[2]); 		// read label & map block
  if (i < hbuf[2])                              // in case of error
  { fprintf( stderr, "Read of label/map block failed\n - %s\n", //complain
            strerror(errno));                   // what was returned
    close(dbfd);
    return -(errno+ERRMLAST+ERRZLAST);          // exit with error
  }

  if (vollab->clean == 0)			// if not a clean dismount
  { fprintf(stderr, "WARNING: Volume was not dismounted properly!\n");
    systab->vol[vol]->upto = 1;			// mark for cleaning
  }
  else
  { vollab->clean = 0;				// mark as mounted, was 1
    systab->vol[vol]->map_dirty_flag |= VOLLAB_DIRTY; // and map needs writing
  }
  systab->vol[vol]->num_of_daemons = 0;	        // initalise this
  systab->vol[vol]->volset_size = volset_size;  // save volset_size

  systab->addoff  += volset_size;                // adjust additional memory
  systab->addsize -= volset_size;

  curr_lock = 0;                                // clear lock on globals

  gptr = systab->vol[vol]->gbd_head; 		// get start of gbds
  ptr = (u_char *) systab->vol[vol]->global_buf;	// get start of Globuff
  for (i = 0; i < systab->vol[vol]->num_gbd; i++)	// for each GBD
  { gptr[i].mem = (struct DB_BLOCK *) ptr;	// point at block
    ptr += vollab->block_size;			// point at next
    if (i < (systab->vol[vol]->num_gbd - 1))	// all but the last
    { gptr[i].next = &gptr[i+1];		// link to next
    }
    else					// the last
    { gptr[i].next = NULL;			// end of list
    }
    systab->vol[vol]->gbd_hash[ GBD_HASH ] = gptr; // head of free list
#ifdef MV1_REFD
    gptr[i].prev = NULL;                        // no prev in free list
    gptr[i].hash = GBD_HASH;                    // store hash
#endif
#ifdef MV1_BLKSEM
    gptr[i].curr_lock = 0;                      // block lock flag
#endif
    gptr[i].vol = vol;                          // vol[] index
  }						// end setup gbds
#ifdef MV1_GBDRO
  for (j = 0; j < NUM_GBDRO - 1; j++, i++)      // setup R/O GBDs at the tail
  { gptr[i].mem = (struct DB_BLOCK *) ptr;
    ptr += vollab->block_size;			// point at next
    Free_GBDRO(&gptr[i]);
  }
#endif

  i = close(dbfd);                              // close the database

  while (SemOp( SEM_WD, WRITE));		// lock WD

  systab->vol[vol]->vollab = vollab;		// enter vollab in vol[] table
  MEM_BARRIER;

  if ((systab->vol[vol]->vollab->journal_requested) &&
           (systab->vol[vol]->vollab->journal_file[0]))
  { 
    while (SemOp( SEM_GLOBAL, WRITE));		// lock GLOBAL
    OpenJournal( vol, 1);
    SemOp( SEM_GLOBAL, -WRITE);			// unlock global
  }						// end journal stuff
  
  SemOp( SEM_WD, -WRITE);			// release WD lock

  return (0);                                   // indicate success
}
