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
                int volnum,                     // where to mount
                int gmb,                        // mb of global buf
                int jrnkb)                      // kb of jrn buf
{ int dbfd;                                     // database file descriptor
  int hbuf[SIZEOF_LABEL_BLOCK/sizeof(int)];     // header buffer
  int i;                                        // usefull int
  int n_gbd;                                    // number of gbd
  u_short sem_val[SEM_MAX];			// to init them
  union semun semvals;				// for a ptr to sem_val
  size_t volset_size;                           // size of volset struct (bytes)
  size_t pagesize;                              // system pagesize (bytes)
  char fullpathvol[MAXPATHLEN];			// full pathname of vol file
  gbd *gptr;					// a gbd pointer
  u_char *ptr;					// and a byte one
  label_block *labelblock;			// label block pointer
  int syncjrn;                                  // sync jrn buf
  int minjrnkb;                                 // min JRN buf size
  int vol;                                      // vol[] indx
  int jobs;                                     // no. of jobs
  size_t addoff;                                // where to add this volume

  if ((volnum < 2) || (volnum > MAX_VOL))       // validate volnum
    return -(ERRZ74+ERRMLAST);
  if (systab->vol[volnum-1]->file_name[0] != 0) // check for already open
    return -(ERRZ74+ERRMLAST);

  vol  = volnum - 1;                            // vol[] index
  jobs = systab->maxjob;                        

  if (systab->maxjob != 1)                      // if not in single user mode
  { return -(ERRZ60+ERRMLAST);                  //   complain
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
    return -(errno+ERRMLAST+ERRZLAST);          // exit with error
  }

  labelblock = (label_block *) hbuf;		// point label block at it
  if (labelblock->db_ver != DB_VER)		// if we need to upgrade
  { fprintf(stderr,
	 "Database is version %04x, image requires version %04x - start failed!!\n",
	 labelblock->db_ver, DB_VER);
      return -(ERRZ73+ERRMLAST);
  }						// end upgrade proceedure
  if (labelblock->magic != MUMPS_MAGIC)
  { fprintf( stderr, "Invalid MUMPS database (wrong magic) - start failed!!\n");
    return -(ERRZ73+ERRMLAST);
  }

  n_gbd  = (gmb * MBYTE) / hbuf[3];		// number of gbd
  while (n_gbd < MIN_GBD)			// if not enough
  { gmb++;					// increase it
    n_gbd = (gmb * MBYTE) / hbuf[3];		// number of gbd
  }
  n_gbd -= NUM_GBDRO;                           // remove the R/O GBDs

  syncjrn = 1;                                  // buffer flush w/ fsync()
  if (jrnkb < 0)                                // if negative
  { syncjrn = 0;                                //   then disable fsync()
    jrnkb   = -jrnkb;
  }
  minjrnkb = ((MIN(hbuf[3], MAX_STR_LEN) + sizeof(jrnrec)) * MIN_JRNREC) / 1024;
  if (minjrnkb > jrnkb)
  { jrnkb = minjrnkb;
  }
  jrnkb *= 1024;
  jrnkb = (((jrnkb - 1) / pagesize) + 1) * pagesize;
  jrnkb /= 1024;

  for (i = 0; i < SEM_MAX; i++)                 // setup for sem init
#ifdef MV1_SHSEM
    sem_val[i] = 0;
#else
    sem_val[i] = jobs;
#endif
  semvals.array = sem_val;

  volset_size = sizeof(vol_def)                 // vol_def size
              + hbuf[2]				// size of head and map block
	      + ((n_gbd + NUM_GBDRO) * sizeof(struct GBD))	// the gbd
              + (gmb * MBYTE)		  	// mb of global buffers
              + hbuf[3]			       	// size of block (zero block)
              + jrnkb * KBYTE;                  // size of JRN buf
  volset_size = (((volset_size - 1) / pagesize) + 1) * pagesize; // round up

  if (volset_size > systab->addsize)            // check avail additional mem.
  { return -(ERRZ60+ERRMLAST);
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

  bzero(systab + addoff, volset_size);		// zot the lot

  systab->vol[vol] = (vol_def *) ((void *)systab + addoff);
						// volume set memory
  systab->vol[vol]->vollab =
    (label_block *) ((void *)systab->vol[0] + sizeof(vol_def));
                                                // and point to label blk

  systab->vol[vol]->map =
    (void*)((void *)systab->vol[vol]->vollab + SIZEOF_LABEL_BLOCK);
						// and point to map
  systab->vol[vol]->first_free = systab->vol[vol]->map;	// init first free

  systab->vol[vol]->gbd_head =
    (gbd *) ((void *)systab->vol[vol]->vollab + hbuf[2]); // gbds
  systab->vol[vol]->num_gbd = n_gbd;		// number of gbds

  systab->vol[vol]->global_buf =
    (void *) &systab->vol[vol]->gbd_head[n_gbd + NUM_GBDRO];//glob buffs
  systab->vol[vol]->zero_block =
    (void *) &(((u_char *)systab->vol[vol]->global_buf)[gmb*MBYTE]);
						// pointer to zero blk

  systab->vol[vol]->jrnbuf =                    // JRN buf
    (void *)((void *)systab->vol[vol]->zero_block + hbuf[3]);
  systab->vol[vol]->jrnbufcap  = jrnkb * KBYTE; // the size
  systab->vol[vol]->jrnbufsize = 0;             // buffer empty
  systab->vol[vol]->syncjrn    = syncjrn;       // sync flag

  systab->vol[vol]->shm_id = systab->vol[0]->shm_id;	// set up share id
  systab->vol[vol]->map_dirty_flag = 0;		// clear dirty map flag
  systab->vol[vol]->hash_start = 0;             // start searching here
  systab->vol[vol]->gmb        = gmb;           // global bufffer cache in MB
  systab->vol[vol]->jrnkb      = jrnkb;         // jrn buffer cache in KB

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
    { // XXX - SEM i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
      return -(errno+ERRMLAST+ERRZLAST);	// exit with error
    }

  i = lseek(dbfd, 0, SEEK_SET);			// re-point at start of file
  i = read(dbfd, systab->vol[vol]->vollab, hbuf[2]); // read label & map block
  if (i < hbuf[2])                              // in case of error
  { fprintf( stderr, "Read of label/map block failed\n - %s\n", //complain
            strerror(errno));                   // what was returned
    // XXX - SEM = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
    return -(errno+ERRMLAST+ERRZLAST);          // exit with error
  }

  if (systab->vol[vol]->vollab->clean == 0)	// if not a clean dismount
  { fprintf(stderr, "WARNING: Volume was not dismounted properly!\n");
    systab->vol[vol]->upto = 1;			// mark for cleaning
  }
  else
  { systab->vol[vol]->vollab->clean = 0;	// mark as mounted, was 1
    systab->vol[vol]->map_dirty_flag = 1;	// and map needs writing
  }
  systab->vol[vol]->num_of_daemons = 0;	        // initalise this
  systab->vol[vol]->volset_size = volset_size;  // save volset_size

  systab->addoff  += volset_size;                // adjust additional memory
  systab->addsize -= volset_size;

  curr_lock = 0;                                // clear lock on globals
  // XXX - SEM
#if 0
#ifdef MV1_SHSEM
  for (i = 0; i < SEM_MAX; i++)                 // init shared IPC
    LatchInit(&systab->shsem[i]);
  RWLockInit(&systab->glorw, systab->maxjob);
#ifdef MV1_BLKSEM
  for (i = 0; i < BLKSEM_MAX; i++)              // init shared block semaphores
    LatchInit(&systab->blksem[i]);
#endif
#endif

#ifdef MV1_CKIT
  ck_ring_init(&systab->vol[vol]->dirtyQ, NUM_DIRTY);
  ck_ring_init(&systab->vol[vol]->garbQ,  NUM_GARB);
#ifdef MV1_GBDRO
  ck_ring_init(&systab->vol[vol]->rogbdQ, NUM_GBDRO);
#endif
#endif
#endif

  while (SemOp( SEM_WD, WRITE));		// lock WD
  // XXX - SEM
#if 0
  for (indx=0; indx<jobs; indx++)		// for each required daemon
  { i = DB_Daemon(indx, volnum);		// start each daemon (volume 1)
    if (i != 0)                                 // in case of error
    { fprintf( stderr, "**** Died on error - %s ***\n\n", // complain
              strerror(errno));                 // what was returned
      i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
      return(errno);                            // exit with error
    }
  }                                             // all daemons started
#endif

  if ((systab->vol[vol]->vollab->journal_requested) &&
           (systab->vol[vol]->vollab->journal_file[0]))
  { struct stat   sb;				// File attributes
    off_t jptr;					// file ptr
    jrnrec jj;					// to write with
    int jfd;					// file descriptor

    while (SemOp( SEM_GLOBAL, WRITE));		// lock GLOBAL
    systab->vol[vol]->vollab->journal_available = 0; // assume fail
    i = stat(systab->vol[vol]->vollab->journal_file, &sb ); // check for file
    if ((i < 0) && (errno != ENOENT))		// if that's junk
    { fprintf(stderr, "Failed to access journal file %s\n",
    		systab->vol[vol]->vollab->journal_file);
    }
    else					// do something
    { if (i < 0)				// if doesn't exist
      { ClearJournal(0, vol);			// create it
      }						// end create code
      jfd = open(systab->vol[vol]->vollab->journal_file, O_RDWR);
      if (jfd < 0)				// on fail
      { fprintf(stderr, "Failed to open journal file %s\nerrno = %d\n",
		systab->vol[vol]->vollab->journal_file, errno);
      }
      else					// if open OK
      { u_char tmp[sizeof(u_int) + sizeof(off_t)];

#ifdef MV1_F_NOCACHE
        i = fcntl(jfd, F_NOCACHE, 1);
#endif
	lseek(jfd, 0, SEEK_SET);
	errno = 0;
	i = read(jfd, tmp, sizeof(u_int));	// read the magic
	if ((i != sizeof(u_int)) || (*(u_int *) tmp != (MUMPS_MAGIC - 1)))
	{ fprintf(stderr, "Failed to open journal file %s\nWRONG MAGIC\n",
		  systab->vol[vol]->vollab->journal_file);
	  close(jfd);
	}
	else
	{ i = read(jfd, &systab->vol[vol]->jrn_next, sizeof(off_t));
	  if (i != sizeof(off_t))
	  { fprintf(stderr, "Failed to use journal file %s\nRead failed - %d\n",
		    systab->vol[vol]->vollab->journal_file, errno);
	    close(jfd);
	  }
	  else
	  { jptr = lseek(jfd, systab->vol[vol]->jrn_next, SEEK_SET);
	    if (jptr != systab->vol[vol]->jrn_next)
	    { fprintf(stderr, "Failed journal file %s\nlseek failed - %d\n",
		      systab->vol[vol]->vollab->journal_file, errno);
	      close(jfd);
	    }
	    else
	    { jj.action = JRN_START;
	      jj.time = MTIME(0);
	      jj.uci = 0;
	      jj.size = MIN_JRNREC_SIZE;
	      i = write(jfd, &jj,               // write the create record
                                MIN_JRNREC_SIZE);
	      systab->vol[vol]->jrn_next += 
                                MIN_JRNREC_SIZE;// adjust pointer
	      lseek(jfd, sizeof(u_int), SEEK_SET);
	      i = write(jfd, &systab->vol[vol]->jrn_next, sizeof(off_t));
	      i = close(jfd);			// and close it
	      systab->vol[vol]->vollab->journal_available = 1;
	      fprintf(stderr, "Journaling started to %s.\n",
		      systab->vol[vol]->vollab->journal_file); // say it worked
	    }
	  }
	}
      }
    }
    SemOp( SEM_GLOBAL, -WRITE);			// unlock global
  }						// end journal stuff
  
  SemOp( SEM_WD, -WRITE);			// release WD lock

  gptr = systab->vol[vol]->gbd_head; 		// get start of gbds
  ptr = (u_char *) systab->vol[vol]->global_buf;	// get start of Globuff
  for (i = 0; i < systab->vol[vol]->num_gbd; i++)	// for each GBD
  { gptr[i].mem = (struct DB_BLOCK *) ptr;	// point at block
    ptr += systab->vol[vol]->vollab->block_size;	// point at next
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
    ptr += systab->vol[vol]->vollab->block_size;	// point at next
    Free_GBDRO(&gptr[i]);
  }
#endif

  i = close(dbfd);                              // close the database
  return (0);                                   // indicate success
}
