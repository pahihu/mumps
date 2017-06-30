// File: mumps/init/init_start.c
//
// module MUMPS - startup code

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
void Routine_Init();                            // proto for routine setup
#ifdef MV1_GBDRO
void Free_GBDRO(gbd *ptr);                      // proto for R/O GBD release
#endif

//****************************************************************************
// Init an environment
//
int INIT_Start( char *file,                     // database
                int jobs,                       // number of jobs
                int gmb,                        // mb of global buf
                int rmb,                        // mb of routine buf
                int addmb,                      // mb of additional buf
                int jrnkb)                      // kb of jrn buf
{ int dbfd;                                     // database file descriptor
  int hbuf[SIZEOF_LABEL_BLOCK/sizeof(int)];     // header buffer
  int i, j;                                     // usefull int
  int n_gbd;                                    // number of gbd
  int addoff;                                   // offset for add buff
  int indx;                                     // loop control
  key_t shar_mem_key;                           // memory "key"
  int shar_mem_id;                              // memory id
  int sem_id;					// semaphore id
  u_short sem_val[SEM_MAX];			// to init them
  union semun semvals;				// for a ptr to sem_val
  int share_size;                               // size of share (bytes)
  int locksize;					// size of locktab
  int sjlt_size;                                // size of systab+jobtab+locktab
  int volset_size;                              // size of volset struct (bytes)
  int pagesize;                                 // system pagesize (bytes)
  struct shmid_ds sbuf;				// for shmctl
  char fullpathvol[MAXPATHLEN];			// full pathname of vol file
  gbd *gptr;					// a gbd pointer
  u_char *ptr;					// and a byte one
  label_block *labelblock;			// label block pointer
  int syncjrn;                                  // sync jrn buf
  int jrnsize;                                  // JRN buf size

  if ((jobs < 1)||(jobs > 256))                 // check number of jobs
  { fprintf(stderr, "Invalid number of jobs %d - must be 1 to 256\n", jobs);
    return(EINVAL);                             // exit with error
  }
  pagesize = getpagesize();                     // get sys pagesize (bytes)
  if (rmb == 0) rmb = jobs/8;                   // pick a default
  if (rmb < 1) rmb = 1;                         // but at least 1mb
  if (gmb == 0) gmb = jobs/2;                   // default global buffers
  if (gmb < 1) gmb = 1;                         // but at least 1mb
  locksize = jobs * LOCKTAB_SIZE;		// what we need for locktab
  locksize = (((locksize - 1) / pagesize) + 1) * pagesize; // round up

  dbfd = open(file, O_RDWR);                    // open the database read/write
  if (dbfd < 1)                                 // if that failed
  { fprintf( stderr,
             "Open of database %s failed\n - %s\n", // complain
             file,                              // what we tried
             strerror(errno));                  // what was returned
    return(errno);                              // exit with error
  }                                             // end file create test
  // i = fcntl(dbfd, F_NOCACHE, 1);
  i = read(dbfd, hbuf, SIZEOF_LABEL_BLOCK);     // read label block
  if (i < SIZEOF_LABEL_BLOCK)                   // in case of error
  { fprintf( stderr, "Read of label block failed\n - %s\n", // complain
            strerror(errno));                   // what was returned
    return(errno);                              // exit with error
  }

  labelblock = (label_block *) hbuf;		// point label block at it
  if (labelblock->db_ver != DB_VER)		// if we need to upgrade
  { fprintf(stderr,
	 "Database is version %04x, image requires version %04x - start failed!!\n",
	 labelblock->db_ver, DB_VER);
      return (-1);
  }						// end upgrade proceedure
  if (labelblock->magic != MUMPS_MAGIC)
  { fprintf( stderr, "Invalid MUMPS database (wrong magic) - start failed!!\n");
    return (-1);
  }

  shar_mem_key = ftok(file, MUMPS_SYSTEM);      // get a unique key
  if (shar_mem_key == -1)                       // die on error
  { fprintf( stderr, "Unable to access database file %s - %s\n", file,
            strerror(errno));                   // give an error
    return(errno);                              // and return with error
  }
  shar_mem_id = shmget(shar_mem_key, 0, 0);     // attach to existing share
  if (shar_mem_id != -1)			// check to see if it's there
  { fprintf(stderr, "Environment is already initialized.\n");
    return (EEXIST);				// exit with error
  }

  n_gbd  = (gmb * MBYTE) / hbuf[3];		// number of gbd
  while (n_gbd < MIN_GBD)			// if not enough
  { gmb++;					// increase it
    n_gbd = (gmb * MBYTE) / hbuf[3];		// number of gbd
  }
  n_gbd -= NUM_GBDRO;                           // remove the R/O GBDs

  if (jrnkb < 0)                                // if negative
  { syncjrn = 1;                                //   we requested fsync() on
    jrnkb   = -jrnkb;                           //   buffer flush
  }
  jrnsize = ((MIN(hbuf[3], MAX_STR_LEN) + sizeof(jrnrec)) * MIN_JRNREC) / 1024;
  if (jrnsize < jrnkb)
  { jrnsize = jrnkb;
  }
  jrnsize *= 1024;
  jrnsize = (((jrnsize - 1) / pagesize) + 1) * pagesize;

  printf( "Creating share for %d jobs with %dmb routine space,\n", jobs, rmb);
  printf( "%dmb (%d) global buffers, %dkb label/map space\n", gmb,
  	   n_gbd, hbuf[2]/1024);
  printf( "and %dkb for locktab.\n", locksize/1024);
  if (jrnsize) printf("With %dkb of journal buffer.\n", jrnsize/1024);
  if (addmb > 0) printf("With %d MB of additional buffer.\n", addmb);

  for (i = 0; i < SEM_MAX; i++)                 // setup for sem init
#ifdef MV1_SHSEM
    sem_val[i] = 0;
#else
    sem_val[i] = jobs;
#endif
  semvals.array = sem_val;

  sjlt_size = sizeof(systab_struct)		// size of Systab
	    + (sizeof(u_int) * (jobs - 1))	// adj for last_blk_used[1]
            + (sizeof(u_int) * (jobs))          // adj for last_blk_written
            + (sizeof(jobtab) * jobs)		// size of JOBTAB
            + locksize 				// size of LOCKTAB
            + jrnsize;                          // size of JRN buf

  sjlt_size = (((sjlt_size - 1) / pagesize) + 1) * pagesize; // round up
  volset_size = sizeof(vol_def)			// size of VOL_DEF (one for now)
	      + hbuf[2]				// size of head and map block
	      + ((n_gbd + NUM_GBDRO) * sizeof(struct GBD))	// the gbd
              + (gmb * MBYTE)		  	// mb of global buffers
              + hbuf[3]			       	// size of block (zero block)
              + (rmb * MBYTE);		 	// mb of routine buffers
  volset_size = (((volset_size - 1) / pagesize) + 1) * pagesize; // round up
  share_size = sjlt_size + volset_size;         // shared memory size
  addoff = share_size;                              // where add buff starts
  share_size = share_size + ( addmb * MBYTE );      // and the additional

  shar_mem_id = shmget(shar_mem_key,
                       share_size,
                       (SHM_R|SHM_W|(SHM_R>>3)
                        |(SHM_W>>3)|IPC_CREAT)); // create share memory
  if (shar_mem_id == -1)                        // die on error
  { fprintf( stderr, "Unable to create shared memory section - %s\n",
            strerror(errno));                   // give an error
#if defined (__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
	if (errno == EINVAL)
#if defined(__APPLE__)
		fprintf(stderr, "\tIncrease kern.sysv.shmmax\n");
#else
		fprintf(stderr, "\tIncrease kern.ipc.shmmaxpgs\n");
#endif
	else if (errno == ENOMEM)
		fprintf(stderr, "\tInsufficient shared memory remaining\n");
#endif
    return(errno);                              // and return with error
  }
  sem_id = semget(shar_mem_key, SEM_MAX,	// Use SMH_ not SEM_ as
                  (SHM_R|SHM_W|(SHM_R>>3)	// linux needs that
                  |(SHM_W>>3)|IPC_CREAT)); 	// create semaphores
  if (sem_id < 0)
  { fprintf( stderr, "Unable to create semaphore set - %s\n",
            strerror(errno));                   // give an error
    i = shmctl(shar_mem_id, (IPC_RMID), &sbuf); // remove the share
    return(errno);                              // and return with error
  }
  i = semctl(sem_id, 0, SETALL, semvals);	// clear the values
  if (i < 0)					// check for error
  { fprintf( stderr, "Unable to clear semaphores - %s\n",
            strerror(errno));                   // give an error
    i = shmctl(shar_mem_id, (IPC_RMID), &sbuf); // remove the share
    i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
    return(errno);                              // and return with error
  }
  
  systab = shmat(shar_mem_id, SHMAT_SEED, 0);	// map it
  if (systab == (void *)-1) 	                // die on error
  { fprintf( stderr, "Unable to attach to systab correctly\n"); // give error
    fprintf( stderr, "error may be: %s\n", strerror(errno)); // give error
    i = shmctl(shar_mem_id, (IPC_RMID), &sbuf); // remove the share
    i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
    return(1);                                  // and return with error
  }

  fprintf( stderr, "Systab attached at %lx\n", (u_long) systab);

  bzero(systab, share_size);			// zot the lot
  systab->address = systab;                     // store the address for ron

  systab->last_blk_written =
     (u_int*)&systab->last_blk_used[jobs];      // setup last_blk_written
  // systab->jobtab =
  //   (jobtab *)&systab->last_blk_used[jobs + 1]; // setup jobtab pointer
  systab->jobtab =
     (jobtab *)&systab->last_blk_written[jobs]; // setup jobtab pointer
  systab->maxjob = jobs;                        // save max jobs
  systab->start_user = getuid();		// remember who started this
  systab->precision = DEFAULT_PREC;		// decimal precision
  systab->WDPtime = WDP_TIME_MAX;               // write daemon poll time
  systab->ZMinSpace = DEFAULT_ZMINSPACE;        // Min. Space for Compress()
  systab->ZotData = 1;                          // Kill zeroes data blocks

  systab->lockstart =
    (void *)((void *)systab->jobtab + (sizeof(jobtab)*jobs)); //locktab
  systab->locksize = locksize;			// the size
  systab->lockhead = NULL;			// no locks currently
  systab->lockfree = (locktab *) systab->lockstart; // free space
  systab->lockfree->fwd_link = NULL;		// only one
  systab->lockfree->size = locksize;		// the whole space
  systab->lockfree->job = -1;			// means free
  systab->jrnbuf    =                           // JRN buf
    (void *)((void *)systab->lockstart + systab->locksize);
  systab->jrnbufcap  = jrnsize;                 // the size
  systab->jrnbufsize = 0;                       // buffer empty
  systab->syncjrn    = syncjrn;                 // sync flag
  systab->addoff = addoff;                      // Add buffer offset
  systab->addsize = addmb * MBYTE;              // and size in bytes
  systab->vol[0] = (vol_def *) ((void *)systab + sjlt_size);// jump to start of
						// volume set memory

  systab->vol[0]->vollab =
    (label_block *) ((void *)systab->vol[0] + sizeof(vol_def));
						// and point to label blk

  systab->vol[0]->map =
    //(void*)((void *)systab->vol[0]->vollab + sizeof(label_block));
    (void*)((void *)systab->vol[0]->vollab + SIZEOF_LABEL_BLOCK);
						// and point to map
  systab->vol[0]->first_free = systab->vol[0]->map;	// init first free

  systab->vol[0]->gbd_head =
    (gbd *) ((void *)systab->vol[0]->vollab + hbuf[2]); // gbds
  systab->vol[0]->num_gbd = n_gbd;		// number of gbds

  systab->vol[0]->global_buf =
    (void *) &systab->vol[0]->gbd_head[n_gbd + NUM_GBDRO];//glob buffs
  systab->vol[0]->zero_block =
    (void *)&(((u_char *)systab->vol[0]->global_buf)[gmb*MBYTE]);
						     // pointer to zero blk

  systab->vol[0]->rbd_head = 
    (void *) ((void*)systab->vol[0]->zero_block + hbuf[3]); //rbds
  systab->vol[0]->rbd_end = ((void *)systab 
	+ share_size - systab->addsize); 	// end of share

  systab->vol[0]->shm_id = shar_mem_id;		// set up share id
  systab->sem_id = sem_id;			// set up semaphore id
  systab->vol[0]->map_dirty_flag = 0;		// clear dirty map flag
  systab->hash_start = 0;                       // start searching here

  bzero(semtab, sizeof(semtab));

  if ( (realpath( file, fullpathvol) ) )	// get full path
    { if (strlen(fullpathvol) < VOL_FILENAME_MAX) // if can fit in our struct
        { strcpy( systab->vol[0]->file_name, 	// copy this full path into
		  fullpathvol );		// the vol_def structure
        }					// end if path will fit
      else						// otherwise
        { i = VOL_FILENAME_MAX - strlen(fullpathvol);	// copy as much as
          strcpy( systab->vol[0]->file_name,		// is possible, thats
		  &fullpathvol[i] );			// the best we can do
        }						// end length testing
    }							// end realpath worked
  else						// otherwise there was an error
    { i = shmdt(systab);			// detach the shared mem
      i = shmctl(shar_mem_id, (IPC_RMID), &sbuf); // remove the share
      i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
      return(errno);				// exit with error
    }

  i = shmctl(shar_mem_id, (IPC_STAT), &sbuf);	// get status for later
  i = lseek(dbfd, 0, SEEK_SET);			// re-point at start of file
  i = read(dbfd, systab->vol[0]->vollab, hbuf[2]); // read label & map block
  if (i < hbuf[2])                              // in case of error
  { fprintf( stderr, "Read of label/map block failed\n - %s\n", //complain
            strerror(errno));                   // what was returned
    i = shmdt(systab);                     // detach the shared mem
    i = shmctl(shar_mem_id, (IPC_RMID), &sbuf);	// remove the share
    i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
    return(errno);                              // exit with error
  }

  if (systab->vol[0]->vollab->clean == 0)	// if not a clean dismount
  { fprintf(stderr, "WARNING: Volume was not dismounted properly!\n");
    systab->vol[0]->upto = 1;			// mark for cleaning
  }
  else
  { systab->vol[0]->vollab->clean = 0;		// mark as mounted, was 1
    systab->vol[0]->map_dirty_flag = 1;		// and map needs writing
  }
  jobs = jobs/DAEMONS;                          // number of daemons
  if (jobs < MIN_DAEMONS) jobs = MIN_DAEMONS;   // minimum of MIN_DAEMONS
  if (jobs > MAX_DAEMONS) jobs = MAX_DAEMONS;	// and the max
  systab->vol[0]->num_of_daemons = jobs;	// initalise this

  systab->Mtime = time(0);                      // init MUMPS time
  curr_lock = 0;                                // clear lock on globals
#ifdef MV1_SHSEM
  for (i = 0; i < SEM_MAX; i++)                 // init shared IPC
    LatchInit(&systab->shsem[i]);
  RWLockInit(&systab->glorw, systab->maxjob);
#ifdef MV1_BLKSEM
  for (i = 0; i < BLKSEM_MAX; i++)              // init shared block semaphores
    LatchInit(&systab->blksem[i]);
#endif
#endif

  volnum = 1;

#ifdef MV1_CKIT
  ck_ring_init(&systab->vol[volnum-1]->dirtyQ, NUM_DIRTY);
  ck_ring_init(&systab->vol[volnum-1]->garbQ,  NUM_GARB);
#ifdef MV1_GBDRO
  ck_ring_init(&systab->vol[volnum-1]->rogbdQ, NUM_GBDRO);
#endif
#endif

  while (SemOp( SEM_WD, WRITE));		// lock WD
  for (indx=0; indx<jobs; indx++)		// for each required daemon
  { i = DB_Daemon(indx, volnum);		// start each daemon (volume 1)
    if (i != 0)                                 // in case of error
    { fprintf( stderr, "**** Died on error - %s ***\n\n", // complain
              strerror(errno));                 // what was returned
      i = shmdt(systab);                        // detach the shared mem
      i = shmctl(shar_mem_id, (IPC_RMID), &sbuf);// remove the share
      i = semctl(sem_id, 0, (IPC_RMID), NULL);	// and the semaphores
      return(errno);                            // exit with error
    }
  }                                             // all daemons started
  if (systab->maxjob == 1)			// if in single user mode
  { fprintf(stderr, "WARNING: Single user, journaling not started.\n");
  }
  else if ((systab->vol[0]->vollab->journal_requested) &&
           (systab->vol[0]->vollab->journal_file[0]))
  { struct stat   sb;				// File attributes
    off_t jptr;					// file ptr
    jrnrec jj;					// to write with
    int jfd;					// file descriptor

    while (SemOp( SEM_GLOBAL, WRITE));		// lock GLOBAL
    systab->vol[0]->vollab->journal_available = 0; // assume fail
    i = stat(systab->vol[0]->vollab->journal_file, &sb ); // check for file
    if ((i < 0) && (errno != ENOENT))		// if that's junk
    { fprintf(stderr, "Failed to access journal file %s\n",
    		systab->vol[0]->vollab->journal_file);
    }
    else					// do something
    { if (i < 0)				// if doesn't exist
      { ClearJournal(0, 0);			// create it
      }						// end create code
      jfd = open(systab->vol[0]->vollab->journal_file, O_RDWR);
      if (jfd < 0)				// on fail
      { fprintf(stderr, "Failed to open journal file %s\nerrno = %d\n",
		systab->vol[0]->vollab->journal_file, errno);
      }
      else					// if open OK
      { u_char tmp[sizeof(u_int) + sizeof(off_t)];

        // i = fcntl(jfd, F_NOCACHE, 1);
	lseek(jfd, 0, SEEK_SET);
	errno = 0;
	i = read(jfd, tmp, sizeof(u_int));	// read the magic
	if ((i != sizeof(u_int)) || (*(u_int *) tmp != (MUMPS_MAGIC - 1)))
	{ fprintf(stderr, "Failed to open journal file %s\nWRONG MAGIC\n",
		  systab->vol[0]->vollab->journal_file);
	  close(jfd);
	}
	else
	{ i = read(jfd, &systab->vol[0]->jrn_next, sizeof(off_t));
	  if (i != sizeof(off_t))
	  { fprintf(stderr, "Failed to use journal file %s\nRead failed - %d\n",
		    systab->vol[0]->vollab->journal_file, errno);
	    close(jfd);
	  }
	  else
	  { jptr = lseek(jfd, systab->vol[0]->jrn_next, SEEK_SET);
	    if (jptr != systab->vol[0]->jrn_next)
	    { fprintf(stderr, "Failed journal file %s\nlseek failed - %d\n",
		      systab->vol[0]->vollab->journal_file, errno);
	      close(jfd);
	    }
	    else
	    { jj.action = JRN_START;
	      jj.time = MTIME(0);
	      jj.uci = 0;
	      jj.size = MIN_JRNREC_SIZE;
	      i = write(jfd, &jj,               // write the create record
                                MIN_JRNREC_SIZE);
	      systab->vol[0]->jrn_next += 
                                MIN_JRNREC_SIZE;// adjust pointer
	      lseek(jfd, sizeof(u_int), SEEK_SET);
	      i = write(jfd, &systab->vol[0]->jrn_next, sizeof(off_t));
	      i = close(jfd);			// and close it
	      systab->vol[0]->vollab->journal_available = 1;
	      fprintf(stderr, "Journaling started to %s.\n",
		      systab->vol[0]->vollab->journal_file); // say it worked
	    }
	  }
	}
      }
    }
    SemOp( SEM_GLOBAL, -WRITE);			// unlock global
  }						// end journal stuff
  
  SemOp( SEM_WD, -WRITE);			// release WD lock

  gptr = systab->vol[0]->gbd_head; 		// get start of gbds
  ptr = (u_char *) systab->vol[0]->global_buf;	// get start of Globuff
  for (i = 0; i < systab->vol[0]->num_gbd; i++)	// for each GBD
  { gptr[i].mem = (struct DB_BLOCK *) ptr;	// point at block
    ptr += systab->vol[0]->vollab->block_size;	// point at next
    if (i < (systab->vol[0]->num_gbd - 1))	// all but the last
    { gptr[i].next = &gptr[i+1];		// link to next
    }
    else					// the last
    { gptr[i].next = NULL;			// end of list
    }
    systab->vol[0]->gbd_hash[ GBD_HASH ] = gptr; // head of free list
#ifdef MV1_REFD
    gptr[i].prev = NULL;                        // no prev in free list
    gptr[i].hash = GBD_HASH;                    // store hash
#endif
#ifdef MV1_BLKSEM
    gptr[i].curr_lock = 0;                      // block lock flag
#endif
  }						// end setup gbds
#ifdef MV1_GBDRO
  for (j = 0; j < NUM_GBDRO - 1; j++, i++)      // setup R/O GBDs at the tail
  { gptr[i].mem = (struct DB_BLOCK *) ptr;
    ptr += systab->vol[0]->vollab->block_size;	// point at next
    Free_GBDRO(&gptr[i]);
  }
#endif

  Routine_Init();				// and the routine junk
  i = shmdt(systab);                            // detach the shared mem
  i = close(dbfd);                              // close the database
  printf( "MUMPS environment initialized.\n");  // say something
  return (0);                                   // indicate success
}
