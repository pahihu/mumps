// File: mumps/include/mumps.h
//
// module MUMPS header file - standard includes

/*      Copyright (c) 1999 - 2016
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

// sizeof() for structures added 23 Sep 2013.  NOTE: will change with changes
// to constant defs and the sizeof() info below will not be updated - rdn

#ifndef _MUMPS_MUMPS_H_                         // only do this once
#define _MUMPS_MUMPS_H_

#if defined(__linux__) && defined(__PPC__)
#undef MV1_SHSEM
#define MEM_BARRIER
#else
#define MV1_SHSEM       1
#define MEM_BARRIER     __sync_synchronize()
#endif

#ifdef MV1_CKIT
#include <ck_ring.h>
#endif

// #define MV1_PROFILE     1

#include <stdint.h>
#ifdef MV1_SHSEM
# ifdef MV1_CKIT
#  include "usync_ck.h"
# else
#  include "usync_mv1.h"
# endif
#else
# define inter_add   __sync_add_and_fetch
#endif

#ifndef VOLATILE
#define VOLATILE
#endif

// #define __PACKED__      __attribute__ ((__packed__))
#define __PACKED__

//** general constant definitions ***/

#define FALSE           0                       // nicer than using 0
#define TRUE            1                       // or 1

#define MUMPS_MAGIC     4155766917U             // seems unique
#define MUMPS_SYSTEM    50                      // MDC assigned number
#define MAX_MAP_CHUNKS	32			// 32 * 32 4K blocks for map
#define MAP_CHUNK	(4*1024)		// map is written in 4K chunks
#define MAX_MAP_BYTES	(MAX_MAP_CHUNKS * 32 * MAP_CHUNK) // 4MB for now
#define MAX_DATABASE_BLKS 2147483647            // max of 2**31-1 for now
#define VERSION_MAJOR   1                       // Major version number
#define VERSION_MINOR   70                      // Minor version number
#define VERSION_TEST	1                       // Test version number
#define KBYTE           ((size_t) 1024)         // 1024
#define MBYTE           ((size_t) 1048576)      // 1024*1024
#define DAEMONS         10                      // Jobs per daemon
#define MIN_DAEMONS     2                       // minimum of these
#define MAX_DAEMONS     10                      // maximum of these
#define MAX_NET_DAEMONS	10			// maximum of these
// #define STORAGE         1024                    // what $STORAGE returns
#ifdef  __APPLE__
#define PRVGRP          80                      // admin in OSX
#else
#define PRVGRP          0                       // Priv group FreeBSD and linux
#endif                                          // darwin

#define MAX_INT_DIGITS	9                       // can be an int at 9
#define DEFAULT_PREC    12                      // number of decimal places
#define MAX_NUM_BYTES	63                      // max size of a number
#define MAX_STR_LEN     32767                   // max size of a string
#define MAX_ECODE       1024                    // max len for $ECODE
#define MAX_NAME_BYTES  32                      // max len for names
#define MAX_SUBSCRIPTS  63                      // max no. of subscripts
#define DEFAULT_ZMINSPACE 1024                  // Min. space for Compress
#define DEFAULT_GBSYNC  300                     // Global Buffer Sync in sec

#define SECDAY          86400                   // seconds per day ($H)
#define YRADJ           47117                   // days from 1 Jan 1841 to 1970

#define UCIS            64                      // always 64

// KeyCmp outputs
#define KEQUAL           0                      // outputs for..
#define K2_LESSER        1                      // .. KeyCmp function
#define K2_GREATER      -1                      // ***
#define KNOTEQUAL        2

#define MAX_DO_FRAMES   256                     // maximum permitted do_frame
#define STM1_FRAME	MAX_DO_FRAMES-1             // where $ST(-1) data goes
#define MAX_SEQ_IO      16                      // maximum sequential io chans
#define MAX_SEQ_NAME	256                     // max file name size
#define MAX_SEQ_OUT     6                       // max output terminator size
#define MAX_DKEY_LEN	16                      // max $KEY seq stored
#define MAX_IN_BUF	256			// max input buffer length
#define MAX_OUT_BUF	4096			// max output buffer length
#define SQ_FREE		0                       // SQ_Chan->type - free
#define SQ_FILE		1                       // SQ_Chan->type - disk file
#define SQ_TCP		2                       // SQ_Chan->type - tcpip
#define SQ_PIPE		3                       // SQ_Chan->type - local pipe
#define SQ_TERM		4                       // SQ_Chan->type - device
#define SQ_LF		-1                      // WRITE !
#define SQ_FF		-2                      // WRITE #

#define SQ_USE_ECHO     1                       // turn echo on
#define SQ_USE_NOECHO	2                       // turn echo off
#define SQ_USE_ESCAPE	4                       // turn escape on
#define SQ_USE_NOESCAPE	16                      // turn escape off
#define SQ_USE_DISCON	128                     // disconnect client from sock
#define SQ_USE_DELNONE	256                     // no delete function
#define SQ_USE_DEL8     512                     // use backspace as delete
#define SQ_USE_DEL127	1024                    // use delete as delete
#define SQ_USE_DELBOTH	2048                    // use both as delete
#define SQ_CONTROLC     4096                    // enable control c trapping
#define SQ_NOCONTROLC	8192                    // no control c trap, ignore it
#define SQ_CONTROLT     16384                   // enable control t status
#define SQ_NOCONTROLT	32768                   // disable control t status

#define IN_TERMS_SIZE   32                      // Input Terminator Array size

#if defined(__linux__) && !defined(__LP64__)
#define SHMAT_SEED      (void *)0x10000000
#elif defined(__APPLE__) 			// OS X 10.11 El Capitan
#if defined(__LP64__)   
#define SHMAT_SEED      (void *)0x200000000
#else
#define SHMAT_SEED      (void *)0x1800000
#endif
#endif

#ifndef SHMAT_SEED
#define SHMAT_SEED      (void *)0
#endif

#ifdef MV1_GBDRO
#define NUM_GBDRO       32                      // no. of R/O GBDs
#else
#define NUM_GBDRO        0                      // no. of R/O GBDs
#endif

#define MIN_GBD		(40 + NUM_GBDRO)        // minumum number GBDs

#define MIN_REST_TIME	   8			// min. daemon rest time
#define MAX_REST_TIME	1000			// max. daemon rest time

#define VOLLAB_DIRTY	(1U<<31)		// volume label dirty

// Note the following three MUST be a power of 2 as they are masks for &
#define GBD_HASH        4096                    // hash size for global buffers
#define NUM_DIRTY       8192                    // max queued dirty chains
#define NUM_GARB        8192                    // max queued garbage blocks
#define GBD_HASH_SEED   0xBEEFCACEU             // hash seed for GBD
#define GBD_BUCKET(i)   (((i) ^ GBD_HASH_SEED) & (GBD_HASH - 1))

#define RBD_HASH        1023                    // hash size for routine names
#define GBD_FREE        GBD_HASH                // head of GBD free list

#define AVROUSIZ        3072                    // average compiled routine size
#define MAXROUSIZ       32767                   // max compiled rou size
#define MAXROULIN       32767                   // max rou lines
#define COMP_VER        (8+(MAX_NAME_BYTES-8)*256)  // compiler version
#define DB_VER          (3+(MAX_NAME_BYTES-8)*256)  // database version

// Global flags (from Global Directory) follow
#define GL_JOURNAL      1                       // Journal global flag
#define GL_TOP_DEFINED	2                       // Top node of global defined
#define GL_STAMP        4                       // Automatic stamp subscript
#define GL_FLAGS        7

#define LOCKTAB_SIZE    1024                    // 1kb per job
#define UCI_IS_LOCALVAR	255                     // for struct mvar
#define VAR_UNDEFINED	-1                      // undefined variable

#define MAX_ASTK        256                     // max depth of astk
#define MAX_SSTK        MBYTE                   // max string stack (1 MB)
#define MAX_ISTK        32768                   // max indirect stack

// Do frame types - negative numbers are error codes
#define TYPE_RUN        1                       // normal mumps startup
#define TYPE_JOB        2                       // got jobbed [0] only
#define TYPE_DO         3                       // DO
#define TYPE_EXTRINSIC	4                       // Extrinsic
#define TYPE_XECUTE     5                       // execute

#define DO_FLAG_TEST	1                       // $TEST value (0/1)
#define DO_FLAG_ATT     2                       // sym attach done
#define DO_FLAG_FOR     4                       // called from a FOR (infor)
#define DO_FLAG_ERROR	8                       // this is an error frame

// Signals we do something with (see jobtab->trap).  (Add as required)
#define SIG_HUP         1                       // SIGHUP	(ERR Z66)
#define SIG_CC          (1 << 2)                // control c signal (sigint)
#define SIG_QUIT        (1 << 3)                // SIGQUIT	(HALT)
#define SIG_TERM        (1 << 15)               // SIGTERM	(HALT)
#define SIG_STOP        (1 << 17)               // SIGSTOP	(HALT)
#define SIG_WS          (1 << 28)               // window size changes (ignore)
#define SIG_CT          (1 << 29)               // control t signal (siginfo)
#define SIG_U1          (1 << 30)               // user signal 1 (ERR Z67)
#define SIG_U2          (1 << 31)               // user signal 2 (ERR Z68)
// Unknown signals generate error Z69

#define MAX_VOL             	16              // max number of vols
#define VOL_FILENAME_MAX	256             // max chars in stored filename
//#define JNL_FILENAME_MAX	226             // max chars in journal filenam
#define JNL_FILENAME_MAX   (234-MAX_NAME_BYTES) // max chars in journal filenam
#define MAX_REPLICAS		16		// max number of replica envs

// systab->historic bit flag meanings
#define	HISTORIC_EOK		1               // E syntax flag
#define HISTORIC_OFFOK		2               // GO/DO with offset OK
#define HISTORIC_DNOK		4               // $NEXT OK runtime/runtime_ssvn

// Semaphore defines
// Semaphores are setup with a value equal to systab->maxjob
// A read takes one semaphore unit
// A write takes systab->maxjob units

#define SEM_SYS         0                       // Systab Semaphore
#define SEM_LOCK        1                       // Lock Table Semaphore
#define SEM_ROU         2                       // routine buffers
#define SEM_WD          3                       // write daemons
#define SEM_GLOBAL      4                       // global database module

// #define SEM_GBDRO       5                       // read-only GBDs
// #define SEM_GBDGET      6                       // get GBDs

#define SEM_MAX         (SEM_GLOBAL + MAX_VOL) // total number of these

#ifdef MV1_BLKSEM
#define BLK_WRITE       ((short) 0x7FFF)        // block WRITE lock
#define BLKSEM_MAX      16                      // total no. of block semaphores
#endif

#define KILL_VAL        1                       // kill only value
#define KILL_SUBS       2                       // kill only subscripts
#define KILL_ALL        (KILL_VAL + KILL_SUBS)  // kill all

#if defined(__sun__) || defined(__NetBSD__)
union semun {
        int             val;            /* value for SETVAL */
        struct semid_ds *buf;           /* buffer for IPC_STAT & IPC_SET */
        unsigned short  *array;         /* array for GETALL & SETALL */
};
typedef union semun semun_t;

#endif

#define MAX_TRANTAB	256                     // total number of entries

typedef unsigned long long      u_int64;        // unix unsigned quadword

//typedef u_int64 chr_q;                        // our quadword special
typedef struct __attribute__ ((__packed__)) CHR_X
{
  u_char buf[MAX_NAME_BYTES];
} chr_x;                                        // our variable length special

typedef union __attribute__ ((__packed__)) VAR_U // get at this two ways
{ chr_x var_xu;                                 // variable name (quadword)
  u_char var_cu[MAX_NAME_BYTES];                // variable name (as char[])
} var_u;                                        // variable name union

typedef struct __attribute__ ((__packed__)) CSTRING // our string type
{ short len;                                    // length of it
  u_char buf[32768];                            // and the content
} cstring;                                      // end counted string

#define MV1_SUBSPOS     1

typedef struct __attribute__ ((__packed__)) MVAR // subscripted MUMPS var
{ var_u name;                                   // variable name
  u_char volset;                                // volset number
  u_char uci;                                   // uci# -> 255 = local var
#ifdef MV1_SUBSPOS
  u_char nsubs;                                 // no. of subscripts
  u_char subspos[MAX_SUBSCRIPTS+1];             // subscript positions
#endif
  u_char slen;                                  // subs (key) length
  u_char key[256];                              // the subs (key) - allow for 0
} mvar;                                         // end MUMPS subs var

#ifdef MV1_SUBSPOS
#define MVAR_SIZE    (sizeof(var_u) + 3*sizeof(u_char) + (MAX_SUBSCRIPTS + 1 + 1)*sizeof(u_char) + 2*sizeof(u_char))
#else
#define MVAR_SIZE    (sizeof(var_u) + 3*sizeof(u_char) + 2*sizeof(u_char))
#endif
						// sizeof(mvar) = 267

//** common memory structures ***/

typedef struct __attribute__ ((__packed__)) UCI_TAB
{ var_u name;                                   // uci name
  u_int global;                                 // ptr to global directory
} uci_tab;                                      // define the uci table

typedef union __PACKED__ DATA_UNION             // diff types of msg data
{ struct GBD *gbddata;                          // a gbd pointer
  u_int intdata;                                // or an integer (block number)
} msg_data;                                     // end data msg union

#define WD_WRITE	0
#define WD_NET		1

typedef struct __PACKED__ WD_TAB                // write daemon table
{ int pid;                                      // the wd's pid
  int type;					// 0 - write, 1 - network
  VOLATILE int doing;                           // what we are doing
  VOLATILE msg_data currmsg;                    // the current gbd */block#
} wdtab_struct;                                 // end write daemon structure

typedef struct __attribute__ ((__packed__)) LABEL_BLOCK
{ u_int magic;                                  // mumps magic number
  u_int max_block;                              // maximum block number
  int header_bytes;                             // bytes in label/map
  int block_size;                               // bytes per data block
  VOLATILE var_u volnam;                        // volume name
  						//   (MAX_NAME_BYTES bytes)
  short db_ver;                                 // database version
  u_char journal_available;                     // jrnl turned on at startup
  u_char journal_requested;                     // && journal_available = ON
  VOLATILE u_char clean;                        // clean dismount flag
  char journal_file[JNL_FILENAME_MAX + 1];      // journal file name
  uci_tab uci[UCIS];                            // current ucis (at 256!!!)
  VOLATILE u_int64 txid;                        // TX id
  VOLATILE u_char bkprevno;                    	// Backup Revision No.
} label_block;         				// define the label block
						// sizeof(label_block) = 1024
#if MAX_NAME_BYTES == 8
#define SIZEOF_LABEL_BLOCK	1024            // (sizeof(label_block)+1023)
                                                //      / 1024
#else
#define SIZEOF_LABEL_BLOCK	4096
#endif

typedef struct __PACKED__ DB_STAT
{ u_int dbget;                                  // Global Gets
  u_int dbset;                                  // Global Sets
  u_int dbkil;                                  // Global Kills
  u_int dbdat;                                  // Global $DATAs
  u_int dbord;                                  // Global $ORDERs
  u_int dbqry;                                  // Global $QUERYs

  u_int lasttry;                                // Search Last Tries
  u_int lastok;                                 // Search Last Successes

  u_int lastwttry;                              // Write Last Tries
  u_int lastwtok;                               // Write Last Successes
  u_int eventcnt;                               // Event Count

  u_int logrd;                                  // Logical Block Reads
  u_int phyrd;                                  // Physical Block Reads

  u_int logwt;                                  // Logical Block Writes
  u_int phywt;                                  // Physical Block Writes

  u_int blkalloc;                               // Block Allocates
  u_int blkdeall;                               // Block Deallocates
  u_int blkreorg;                               // Block Reorganizes
  u_int diskerrors;                             // Disk write errors
  u_int dqstall;                                // No. of dirtyQ stalls
  u_int gqstall;                                // No. of garbQ stalls
  u_int gbwait;                                 // No. of waits for Get_GBD()
  u_int gbswait;                                // No. of waits for Get_GBDs()
  u_int rdwait;                                 // No. of waits for read()
  u_int lckwait;                                // No. of wait for LOCK
  u_int brdwait;                                // No. of waits for block LCK_SH
  u_int bwrwait;                                // No. of waits for block LCK_EX
} db_stat;                                      // database statistics

typedef struct MSEM_STAT
{
  u_int tryfailed_count;                        // no. of failed try locks
  u_int backoff_time;                           // usecs exp backoff time
  u_int held_time;                              // usecs to held the lock
  u_int held_count;                             // no. of times held
  u_int semop_time;
} sem_stat;

struct GBD;                                     // defined in "db_util.h"
struct RBD;                                     // see compile.h

typedef struct __PACKED__ VOL_DEF
{ VOLATILE label_block *vollab;                 // ptr to volset label block
  void *map;                                    // start of map area
  VOLATILE void *first_free;                    // first word with free bits
  u_char *chgmap;				// start of change map
  VOLATILE u_int blkchanged;			// no. of changed blocks
  VOLATILE int track_changes;			// track chg (chgmap/nchanged)
  struct GBD *gbd_hash[GBD_HASH+1];             // gbd hash table
  struct GBD *gbd_head;                         // head of global buffer desc
  int num_gbd;                                  // number of global buffers
  int hash_start;                               // GBD search starts here
  void *global_buf;                             // start of global buffers
  void *zero_block;                             // empty block in memory
  struct RBD *rbd_hash[RBD_HASH+1];             // head of routine buffer desc
  void *rbd_head;                               // head of routine buffer desc
  void *rbd_end;                                // first addr past routine area
  int num_of_daemons;                           // number of daemons
  int num_of_net_daemons;			// number of network daemons
  wdtab_struct wd_tab[MAX_DAEMONS + MAX_NET_DAEMONS];// wr/net daemon info table
  VOLATILE int dismount_flag;                   // flag to indicate dismounting
  VOLATILE u_int map_dirty_flag;                // set if map is dirty
  VOLATILE int writelock;                       // MUMPS write lock
  VOLATILE int bkprunning;			// backup running
  u_int upto;                                   // validating map up-to block
  int shm_id;                                   // GBD share mem id
#ifdef MV1_CKIT
  ck_ring_t dirtyQ;
  ck_ring_buffer_t dirtyQBuffer[NUM_DIRTY];     // dirty queue (for daemons)
  ck_ring_t garbQ;
  ck_ring_buffer_t garbQBuffer[NUM_GARB];       // garbage queue (for daemons)
  ck_ring_t rogbdQ;
#ifdef MV1_GBDRO
  ck_ring_buffer_t rogbdQBuffer[NUM_GBDRO];     // R/O GBD queue (for readers)
#endif
#else
  struct GBD *dirtyQ[NUM_DIRTY];                // dirty que (for daemons)
  VOLATILE int dirtyQw;                         // write ptr for dirty que
  VOLATILE int dirtyQr;                         // read ptr for dirty que
  u_int garbQ[NUM_GARB];                        // garbage que (for daemons)
  VOLATILE int garbQw;                          // write ptr for garbage que
  VOLATILE int garbQr;                          // read ptr for garbage que
#endif
  int syncjrn;                                  // fsync() jrn file
  VOLATILE u_int jrnbufsize;                    // current jrn buffer size
  u_int   jrnbufcap;                            // jrn buffer capacity
  u_char *jrnbuf;                               // jrn buffer
  VOLATILE off_t jrn_next;                      // next free offset in jrn file
  VOLATILE time_t lastdojrn;                    // last DoJournal() time
  VOLATILE u_char jnl_seq;			// jrn sequence
  u_int *last_blk_used;                         // actually setup for real jobs
  u_int *last_blk_written;                      // actually setup for real jobs
  size_t volset_size;                           // shared memory size of vol_def
  int gmb;                                      // global buffer cache in MB
  int jkb;                                      // jrn buffer cache in KB
  int gbsync;                                   // global buffer sync in sec
  char file_name[VOL_FILENAME_MAX];             // absolute pathname of volfile
  u_char local_name[MAX_NAME_BYTES];		// local VOL name for remote VOL
  u_char remote_vollab[SIZEOF_LABEL_BLOCK];	// remote VOL label
  db_stat stats;                                // database statistics
  u_int map_chunks[MAX_MAP_CHUNKS];		// bitmap for dirty map blocks in 4K chunks
} vol_def;                                      // end of volume def
						// sizeof(vol_def) = 57948

typedef struct __PACKED__ DO_FRAME
{ u_char *routine;                              // addr of rou (or X src)
  u_char *pc;                                   // current mumps pc
  short *symbol;                                // process space sym ptrs
  u_char *newtab;                               // process space new table
  u_char *endlin;                               // address of current ENDLIN
  var_u rounam;                                 // routine name
  u_char vol;                                   // rou source vol set #
  u_char uci;                                   // rou source uci #
  u_short line_num;                             // current routine line#
  u_char estack;                                // current estack offset
  u_char type;                                  // see TYPE_??? def
  u_char level;                                 // current argless do level
  u_char flags;                                 // flags for this frame
  long savasp;                                  // saved asp
  long savssp;                                  // saved ssp
  long asp;                                     // entry asp
  long ssp;                                     // entry ssp
  long isp;                                     // entry indirect pointer
} do_frame;            				// do frame
						// sizeof(do_frame) = 96

// *** SEQIO specific *** //

typedef struct __PACKED__ FORKTAB
{ int job_no;
  int pid;
} forktab;

typedef struct __PACKED__ SERVERTAB
{ int slots;
  int taken;
  int cid;
  u_char name[MAX_SEQ_NAME];
  forktab *forked;
} servertab;

typedef struct __PACKED__ SQ_CHAN
{ u_char type;                                  // type of device
  u_char options;                               // type specific options
  u_char mode;                                  // how object is opened
  int fid;                                      // os supplied file id

  servertab s;

  u_short dx;                                   // $X
  u_short dy;                                   // $Y
  u_char name[MAX_SEQ_NAME];                    // name of what was opened
  short dkey_len;                               // $KEY length stored
  u_char dkey[MAX_DKEY_LEN+1];                  // stored $KEY (null term)
  short out_len;                                // length of output terminator
  u_char out_term[MAX_SEQ_OUT];                 // the output terminator
  u_char in_terms[IN_TERMS_SIZE];               // input terminator bit mask
  int    in_terms_crlf;                         // Input Terminator is CRLF
  u_char inbuf[MAX_IN_BUF];			// Input buffer
  int    ninbuf;				// Input buffer length
  int    inpos;					// Input buffer read position
  u_char outbuf[MAX_OUT_BUF];			// Output buffer
  int    outpos;				// Output buffer write position
  var_u nmspace;                                // routine for namespace
} SQ_Chan;                                      // define the $I stuf

// *** End SEQIO specific *** //

typedef struct __PACKED__ JOBTAB
{ int pid;                                      // O/S PID (0 if unused)
  int cur_do;	                             	// current do frame addr
  u_int commands;                               // commands executed
  u_int grefs;                                  // global references
  u_int last_block_flags[MAX_VOL];              // journal etc of last db block
  u_int last_written_flags[MAX_VOL];            // last written db blk flags
  short error_frame;                            // frame error happened in
  short etrap_at;                               // where $ET was invoked
  int trap;                                     // outstanding traps
  int attention;                                // do something
  short async_error;                            // async erors
  uid_t user;                                   // user number
  short priv;                                   // privs this job
  short precision;                              // decimal precision
  u_char io;                                    // current io index
  u_char test;                                  // current $TEST (0/1)
  u_char uci;                                   // current uci number
  u_char vol;                                   // current volset number
  u_char luci;                                  // current lock uci number
  u_char lvol;                                  // current lock volset number
  u_char ruci;                                  // current rou uci number
  u_char rvol;                                  // current rou volset number
  mvar last_ref;                                // $REFERENCE
  short start_len;                              // length start data
  u_char start_dh[14];                          // store start time here
  do_frame dostk[MAX_DO_FRAMES];                // the do stack
  SQ_Chan seqio[MAX_SEQ_IO];                    // sequential io stuf
  struct GBD *view[MAX_VOL];                    // locked view buffers
} jobtab_t;            				// define jobtab
						// sizeof(jobtab) = 21939

typedef struct __PACKED__ LOCKTAB               // internal lock tables
{ struct LOCKTAB *fwd_link;                     // point at next one
  int size;                                     // how many bytes
  int job;                                      // int job (-1 = free)
  short lock_count;                             // how many times locked by job
  short byte_count;                             // size of following reference
  short dummy1;
  u_char vol;                                   // vol number
  u_char uci;                                   // uci number (255 = local)
  var_u name;                                   // var name
  u_char key[256];                              // and the key
} locktab;             				// define locktab

typedef struct __PACKED__ TRANTAB               // translation table
{ var_u  from_global;                           // from global
  u_char from_vol;                              //      volumeset#
  u_char from_uci;                              //      uci#
  var_u  to_global;                             //   to global
  u_char to_vol;                                //      volumeset#
  u_char to_uci;                                //      uci#
} trantab;             				// define trantab

typedef struct __PACKED__ TRANHASH              // trantab hash entry
{ int    tti;                                   // trantab[] index
  var_u  from_global;                           // from global
  u_char from_vol;                              //      volumeset#
  u_char from_uci;                              //      uci#
} tranhash;                                     // define tranhash

typedef struct __PACKED__ REPLTAB
{ char connection[VOL_FILENAME_MAX];		// connection URL in nanomsg fmt
  int  typ;					// DGP_SYNC_REQ, DGP_SYNC_OPT
} repltab;

typedef struct __PACKED__ SYSTAB                // system tables
{ void *address;
  jobtab_t *jobtab;                             // address of jobtab
  int maxjob;                                   // maximum jobs permitted
  int sem_id;                                   // GBD semaphore id
  int historic;                                 // Enn, tag+off, $NEXT etc
  int precision;                                // decimal precision
  int max_tt;                                   // max TRANTAB used
  trantab tt[MAX_TRANTAB];                      // translation tables
  tranhash tthash[2 * MAX_TRANTAB];             // trantab hash
  uid_t start_user;                             // he's priv too
  void *lockstart;                              // head of lock table
  size_t locksize;                              // how many bytes
  locktab *lockhead;                            // head of used locks
  locktab *lockfree;                            // head of lock free space
  size_t addoff;                                // off from systab to add buff
  size_t addsize;                               // add buff size
  VOLATILE u_int64 TxId;                        // TX id
  VOLATILE time_t Mtime;                        // Mtime, updated by daemon 0
  VOLATILE int ZMinSpace;                       // Min. Space for Compress()
  VOLATILE int ZotData;                         // Kill zeroes data blocks
  VOLATILE int ZRestTime;			// Current daemon rest time
  u_char dgpURL[64];				// DGP URL base
  int dgpPORT;					// DGP port base
  u_short dgpID;				// DGP system ID
  u_char dgpLOCKTO;				// DGP LOCK timeout (0-60)
  u_char dgpULOK;				// DGP local ULOK in progress
  VOLATILE time_t dgpRESTART;			// DGP RESTART phase timeout
  u_char dgpSTART[256];				// client: MV1_PIDs (0-255)
#ifdef MV1_SHSEM
  LATCH_T shsem[SEM_GLOBAL];                    // shared semaphores
  RWLOCK_T glorw[MAX_VOL];
#ifdef MV1_BLKSEM
  LATCH_T blksem[BLKSEM_MAX];                   // block semaphores
#endif
#endif
  vol_def *vol[MAX_VOL];                        // array of vol ptrs
  VOLATILE u_int delaywt;                       // delay WRITEs
  repltab replicas[MAX_REPLICAS];		// database replicas
  char bkpfile[VOL_FILENAME_MAX];		// backup file name
  u_int bkpvolmask;				// backup VOL mask
  int bkptyp;					// backup type
  // values: 0 - FULL, 1 - CUMULATIVE, 2 - SERIAL
} systab_struct;                                // end of systab
                                                // Followed by jobtab.
						// sizeof(systab_struct) = 256

extern systab_struct *systab;                   // make its ptr external
extern sem_stat semtab[2*SEM_MAX];
extern int sem_id;                              // global semaphore id


// MUMPS time
#define MTIME(x)        systab->Mtime           // updated by daemon 0

//** process memory structures ***/
//** PARTAB definitions **

typedef struct __PACKED__ PARTAB                // define the partition table
{ jobtab_t *jobtab;                             // our jobtab entry
  int vol_fds[MAX_VOL];                         // the filedes for the volumes
  int jnl_fds[MAX_VOL];                         // the filedes for journals
  u_char jnl_seq[MAX_VOL];			// the seq. numbers of journals
  int dgp_sock[MAX_VOL];			// DGP sockets for remote VOLs
  int dgp_repl[MAX_REPLICAS];			// DGP sockets for replicas
  int debug;                                    // debug in progress
  u_char *sstk_start;                           // start of string stack
  u_char *sstk_last;                            // last byte of sstk
  var_u *varlst;                                // var list for compiler
  int checkonly;                                // used by compiler
  u_char **sp;                                  // source ptr for compile
  cstring **lp;                                 // start of the line (ditto)
  int *ln;                                      // line num for $&%ROUCHK()
  mvar src_var;                                 // temp space for src mvar
}partab_struct;        				// end of partab type
						// sizeof(partab) = 339

extern partab_struct partab;                    // globalize partab
extern u_char *astk[];                          // address stack
extern u_char sstk[];                           // string stack
extern u_char *mumpspc;                         // mumps prog pointer

#define MV1_PID (partab.jobtab - systab->jobtab)


#endif                                          // !_MUMPS_MUMPS_H_
