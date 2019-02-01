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
#include <sys/time.h>			// for gettimeofday()
#include "mumps.h"                      // standard includes
#include "proto.h"                      // standard includes
#include "database.h"                   // database includes
#include "dgp.h"			// DGP stuff

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
                int jkb,                        // kb of jrn buf
		int netdaemons,			// no of network daemons
		int sysid,			// system ID
		char *srvurl,			// server URL
		int srvport)			// server port
{ int dbfd;                                     // database file descriptor
  int hbuf[SIZEOF_LABEL_BLOCK/sizeof(int)];     // header buffer
  int i;                                        // usefull int
  int n_gbd;                                    // number of gbd
  size_t addoff;                                // offset for add buff
  int indx;                                     // loop control
  key_t shar_mem_key;                           // memory "key"
  int shar_mem_id;                              // memory id
  int sem_id;					// semaphore id
  u_short sem_val[SEM_MAX];			// to init them
  union semun semvals;				// for a ptr to sem_val
  size_t share_size;                            // size of share (bytes)
  size_t locksize;				// size of locktab
  size_t sjlt_size;                             // size of systab+jobtab+locktab
  size_t volset_size;                           // size of volset struct (bytes)
  size_t pagesize;                              // system pagesize (bytes)
  struct shmid_ds sbuf;				// for shmctl
  char fullpathvol[MAXPATHLEN];			// full pathname of vol file
  gbd *gptr;					// a gbd pointer
  u_char *ptr;					// and a byte one
  label_block *labelblock;			// label block pointer
  int syncjrn;                                  // sync jrn buf
  int minjkb;                                   // min JRN buf size
  int netjobs;					// #jobs inc. network daemons

  if (!sysid)					// system ID not specified ?
  { struct timeval tv;
    gettimeofday(&tv, NULL);
    sysid = 1 + (tv.tv_usec % 254);		//   select random value
  }

  if ((sysid < 1) || (sysid > 254))
  { fprintf(stderr, "Invalid system ID %d - must be 1 to 254\n", sysid);
    return(EINVAL);                             // exit with error
  }

  if (1 == jobs) netdaemons = 0;		// single user: do NOT start net

  if ((netdaemons < 0) || (netdaemons > MAX_NET_DAEMONS))
  { fprintf(stderr, "Invalid number of network daemons %d - must be 0 to %d\n", 
		netdaemons, MAX_NET_DAEMONS);
    return(EINVAL);                             // exit with error
  }

  if (netdaemons)				// check params for net daemons
  { if (0 == strlen(srvurl))
      strcpy(srvurl, "tcp://127.0.0.1");

    if (-1 == srvport)
      srvport = 1966;

    if ((srvport < 0) || (srvport > 65535))
    { fprintf(stderr, "Invalid server port %d - must be 0 to 65535\n", srvport);
      return(EINVAL);                           // exit with error
    }
  } else {					// no net daemons
    strcpy(srvurl, "");				//   clear params
    srvport = -1;
  }

  if (jobs + netdaemons > 256)
    jobs = 256 - netdaemons;
  netjobs = jobs + netdaemons;			// #jobs incl. network daemons

  if ((jobs < 1)||(jobs > 256))                 // check number of jobs
  { fprintf(stderr, "Invalid number of jobs %d - must be 1 to 256\n", jobs);
    return(EINVAL);                             // exit with error
  }
  pagesize = getpagesize();                     // get sys pagesize (bytes)
  if (rmb == 0) rmb = jobs/8;                   // pick a default
  if (rmb < 1) rmb = 1;                         // but at least 1mb
  if (gmb == 0) gmb = netjobs/2;    		// default global buffers
  if (gmb < 1) gmb = 1;                         // but at least 1mb
  locksize = netjobs * LOCKTAB_SIZE;		// what we need for locktab
  locksize = (((locksize - 1) / pagesize) + 1) * pagesize; // round up

  dbfd = open(file, O_RDWR);                    // open the database read/write
  if (dbfd < 1)                                 // if that failed
  { fprintf( stderr,
             "Open of database %s failed\n - %s\n", // complain
             file,                              // what we tried
             strerror(errno));                  // what was returned
    return(errno);                              // exit with error
  }                                             // end file create test
#ifdef MV1_F_NOCACHE
  i = fcntl(dbfd, F_NOCACHE, 1);
#endif
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

  if (sysid) printf("Starting system ID=%d\n", sysid);
  printf( "Creating share for %d jobs with %dmb routine space,\n", jobs, rmb);
  printf( "%dmb (%d) global buffers, %dkb label/map space\n", gmb,
  	   n_gbd, hbuf[2]/1024);
  printf( "and %lukb for locktab.\n", (u_long) (locksize/1024));
  if (jkb) printf("With %dkb of journal buffer (%s flush).\n",
                        jkb, syncjrn ? "sync" : "async");
  if (addmb > 0) printf("With %d MB of additional buffer.\n", addmb);
  if (netdaemons > 0)
  { printf("With %d network daemon(s) listening on %s",
			netdaemons, DGP_GetServerURL(srvurl, srvport));
    if (netdaemons > 1) printf("-%d", srvport + netdaemons - 1);
    printf("\n");
  }

  for (i = 0; i < SEM_MAX; i++)                 // setup for sem init
#ifdef MV1_SHSEM
    sem_val[i] = 0;
#else
    sem_val[i] = jobs;
#endif
  semvals.array = sem_val;

  sjlt_size = sizeof(systab_struct)		// size of Systab
            + (sizeof(jobtab_t) * netjobs)	// size of JOBTAB
            + locksize;				// size of LOCKTAB

  sjlt_size = (((sjlt_size - 1) / pagesize) + 1) * pagesize; // round up
  volset_size = MAX_VOL * sizeof(vol_def)	// size of VOL_DEF
	      + hbuf[2]				// size of head and map block
              + (hbuf[2] - SIZEOF_LABEL_BLOCK)	// size of change map
	      + ((n_gbd + NUM_GBDRO) * sizeof(struct GBD))	// the gbd
              + (gmb * MBYTE)		  	// mb of global buffers
              + hbuf[3]			       	// size of block (zero block)
              + jkb * KBYTE                     // size of JRN buf
	      + (sizeof(u_int) * (netjobs))	// size of last_blk_used[]
              + (sizeof(u_int) * (netjobs))     // size of last_blk_written[]
              + (rmb * MBYTE);                  // mb of routine buffers
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

  systab->jobtab =                              // setup jobtab pointer
     (jobtab_t *)((void *)systab + sizeof(systab_struct));
  systab->maxjob = jobs;                        // save max jobs
  systab->start_user = getuid();		// remember who started this
  systab->precision = DEFAULT_PREC;		// decimal precision
  systab->ZMinSpace = DEFAULT_ZMINSPACE;        // Min. Space for Compress()
  systab->ZotData = 0;                          // Kill zeroes data blocks
  systab->ZRestTime = MAX_REST_TIME;		// Daemon rest time
  systab->dgpID = sysid;			// system ID
  strcpy((char*) systab->dgpURL, srvurl);	// server URL
  systab->dgpPORT = srvport;			// server base port
  systab->dgpLOCKTO = 0;			// default LOCK timeout
  systab->dgpRESTART = time(0) + DGP_RESTARTTO + 1;// DGP RESTART phase timeout
  for (i = 0; i < 256; i++)			// clear DGP table
    systab->dgpSTART[i] = 0;

  systab->lockstart =				// locktab
    (void *)((void *)systab->jobtab + (sizeof(jobtab_t)*netjobs));
  systab->locksize = locksize;			// the size
  systab->lockhead = NULL;			// no locks currently
  systab->lockfree = (locktab *) systab->lockstart; // free space
  systab->lockfree->fwd_link = NULL;		// only one
  systab->lockfree->size = locksize;		// the whole space
  systab->lockfree->job = -1;			// means free
  systab->addoff = addoff;                      // Add buffer offset
  systab->addsize = addmb * MBYTE;              // and size in bytes

  for (i = 0; i < MAX_VOL; i++)                 // foreach vol[]
    systab->vol[i] = (vol_def *) ((void *)systab + sjlt_size
                                        + i * sizeof(vol_def));
						// volume set memory

  systab->vol[0]->vollab =
    (label_block *) ((void *)systab->vol[0] + MAX_VOL * sizeof(vol_def));
						// and point to label blk

  systab->vol[0]->map =
    //(void*)((void *)systab->vol[0]->vollab + sizeof(label_block));
    (void*)((void *)systab->vol[0]->vollab + SIZEOF_LABEL_BLOCK);
						// and point to map
  systab->vol[0]->first_free = systab->vol[0]->map;	// init first free

  systab->vol[0]->chgmap =			// change map
    (u_char *)((void *)systab->vol[0]->vollab + hbuf[2]);

  systab->vol[0]->gbd_head =			// gbds
    (gbd *) ((void *)systab->vol[0]->vollab + 
		hbuf[2] + 			// label blk + map
		hbuf[2] - SIZEOF_LABEL_BLOCK);	// change map
  systab->vol[0]->num_gbd = n_gbd;		// number of gbds

  systab->vol[0]->global_buf =
    (void *) &systab->vol[0]->gbd_head[n_gbd + NUM_GBDRO];//glob buffs
  systab->vol[0]->zero_block =
    (void *) &(((u_char *)systab->vol[0]->global_buf)[gmb*MBYTE]);
						// pointer to zero blk

  systab->vol[0]->jrnbuf =                      // JRN buf
    (void *)((void *)systab->vol[0]->zero_block + hbuf[3]);
  systab->vol[0]->jrnbufcap  = jkb * KBYTE;     // the size
  systab->vol[0]->jrnbufsize = 0;               // buffer empty
  systab->vol[0]->syncjrn    = syncjrn;         // sync flag

  systab->vol[0]->last_blk_used =               // setup last_blk_used[]
    (void *) ((void*)systab->vol[0]->jrnbuf + jkb * KBYTE);
  systab->vol[0]->last_blk_written =            // setup last_blk_written[]
    (void *) ((void*)systab->vol[0]->last_blk_used + sizeof(u_int)*netjobs);

  systab->vol[0]->rbd_head =                    //rbds
    (void *) ((void*)systab->vol[0]->last_blk_written + sizeof(u_int)*netjobs);
  systab->vol[0]->rbd_end = ((void *)systab 
	+ share_size - systab->addsize); 	// end of share

  systab->vol[0]->shm_id = shar_mem_id;		// set up share id
  systab->sem_id = sem_id;			// set up semaphore id
  systab->vol[0]->map_dirty_flag = 0;		// clear dirty map flag
  systab->vol[0]->hash_start = 0;               // start searching here
  systab->vol[0]->volset_size = volset_size;    // save vol_def size
  systab->vol[0]->gmb         = gmb;            // global bufffer cache in MB
  systab->vol[0]->jkb         = jkb;            // jrn buffer cache in KB
  systab->vol[0]->gbsync      = DEFAULT_GBSYNC; // global buffer sync in sec

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
    systab->vol[0]->map_dirty_flag |= VOLLAB_DIRTY; // and map needs writing
  }
  jobs = jobs/DAEMONS;                          // number of daemons
  if (jobs < MIN_DAEMONS) jobs = MIN_DAEMONS;   // minimum of MIN_DAEMONS
  if (jobs > MAX_DAEMONS) jobs = MAX_DAEMONS;	// and the max
  systab->vol[0]->num_of_daemons = jobs;	// initalise this
  systab->vol[0]->num_of_net_daemons = netdaemons;

  systab->Mtime = time(0);                      // init MUMPS time
  curr_lock = 0;                                // clear lock on globals
#ifdef MV1_SHSEM
  for (i = 0; i < SEM_GLOBAL; i++)              // init shared IPC
    LatchInit(&systab->shsem[i]);
  for (i = 0; i < MAX_VOL; i++)
    RWLockInit(&systab->glorw[i], systab->maxjob);
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
  for (indx=0; indx<jobs + netdaemons; indx++)	// for each required daemon
  { i = indx >= jobs				// start each daemon (volume 1)
	?  Net_Daemon(indx, volnum) 		// network daemon
	:  DB_Daemon(indx, volnum);		// write daemon
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
  { while (SemOp( SEM_GLOBAL, WRITE));		// lock GLOBAL
    OpenJournal(0, 1);
    if (systab->vol[0]->vollab->journal_available)// available ?
      systab->vol[0]->jnl_seq = 1;		//   start jnl sequence
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
    gptr[i].vol = 0;                            // vol[] index
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
