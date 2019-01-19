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
#include <stdarg.h>                                     // for va_ macros
#include <sys/time.h>                                   // for gettimeofday()
#include <sys/shm.h>
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include <sys/stat.h>                                   // for stat()
#include <arpa/inet.h>					// for ntoh() stuff
#include "mumps.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

#ifdef MV1_DGP
#include <nanomsg/nn.h>					// nanomsg lib
#include <nanomsg/reqrep.h>				// REQREP
#include "dgp.h"					// DGP protos
#endif

static int dbfds[MAX_VOL];				// global db file desc
static int myslot;					// my slot in WD table
static int myslot_net;					// my slot in network
static int SLOT_JRN;                                    // JRN daemon
static int SLOT_VOLSYNC;                                // VOLSYNC daemon

void do_daemon();					// do something
void do_dismount();					// dismount volnum
void do_write();					// write GBDs
void do_garb();						// garbage collect
int do_zot(int vol, u_int gb);				// zot block and lower
void do_free(int vol, u_int gb);			// free from map et al
void do_mount(int vol);                                 // mount db file
void ic_map(int flag, int vol, int dbfd);		// check the map
void daemon_check();					// ensure all running
static time_t last_daemon_check;                        // last daemon_check()
static time_t last_map_write[MAX_VOL];                  // last map write
int open_jrn(int vol);                                  // open journal file
static int jnl_fds[MAX_VOL];                            // jrn file desc.
static char jnl_files[MAX_VOL][JNL_FILENAME_MAX + 1];	// jrn file names
void do_quiescence(void);                               // reach quiet point
static time_t last_sync[MAX_VOL];                       // last database sync
void do_jrnflush(int vol);                              // flush jrn to disk
void do_volsync(int vol);                               // flush vol to disk
void do_map_write(int vol);				// write label/map

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
// Descript: sleep given milliseconds
// Input(s): milliseconds to wait
// Return:   return value of usleep() system call
//

static
u_int MSLEEP(u_int mseconds)
{
  if (!myslot)                                          // update M time
    systab->Mtime = time(0);
  return usleep(1000 * mseconds);                       // sleep
}


extern int   curr_sem_init;
extern pid_t mypid;

//-----------------------------------------------------------------------------
// Function: do_netdaemon
// Descript: do network daemon type things
// Input(s): none
// Return:   none
//

extern short getvol(cstring *);
extern int dump_msg;

void do_netdaemon(void)
{ 
#ifdef MV1_DGP
  int rv;						// return value
  int sock;						// NN socket
  char msg[256];					// message buffer
  char url[128];					// server URL
  int bytes;						// bytes sent
  DGPRequest req;					// DGP request
  DGPReply rep;						// DGP reply
  cstring cstr, *ptr1;					// a cstring
  mvar var, *varptr;					// M variable and ptr
  short s;						// status
  time_t t;						// current time
  u_char old;						// handy u_char
  int remjob;						// remote JOB no.
  int lck_count;					// no. of LOCKs
  cstring *lck_list;					// LOCK list
  DGPData *data;
  label_block *remote_label;				// remote VOL label
  int i;						// handy int
  u_short msg_len;					// message length
  int client;						// client system ID
  int send_START;					// send START msg

  sock = nn_socket(AF_SP, NN_REP);
  if (sock < 0)
  { sprintf(msg, "nn_socket(): %s", nn_strerror(nn_errno()));
    panic(msg);
  }

  sprintf(url, "%s%c%d",				// construct URL
               systab->dgpURL, 
               (strncmp((char *) systab->dgpURL,"ipc://",6) ? ':' : '.'),
	       systab->dgpPORT + myslot_net);

  rv = nn_bind(sock, url);
  if (rv < 0)
  { sprintf(msg, "nn_bind(%s): %s", url, nn_strerror(nn_errno()));
    panic(msg);
  }

  do_log("accepting connections on %s\n", url);
  for (;;)
  { bytes = nn_recv(sock, &req, sizeof(req), 0);	// receive request
    if (systab->vol[0]->dismount_flag)		        // dismounting?
    { t = time(0);					// for ctime()
      do_log("Network Daemon %d shutting down\n", myslot);// log success
      SemStats();                                     	// print sem stats
      systab->vol[0]->wd_tab[myslot].pid = 0;	        // say gone
      exit (0);						// and exit
    }
    if (bytes < 0)
    { do_log("nn_recv(): %s\n", nn_strerror(nn_errno()));
      continue;
    }

    req.header.remjob = ntohs(req.header.remjob);	// cvt to host fmt
    req.header.msglen = ntohs(req.header.msglen);
    req.data.len      = ntohs(req.data.len);

    if (bytes != req.header.msglen)			// check request
    { do_log("message size mismatch: received %d, msglen is %d\n",
		bytes, req.header.msglen);
      DGP_MkError(&rep, -(ERRZ81+ERRMLAST));
    }

    rep.header.code = DGP_ERR;
    s = 0;
    varptr = 0;
    remjob = req.header.remjob + 1;			// remote JOB no.

    // do_log("received request %d(len=%d)\n", req.header.code, req.header.msglen);
    if (dump_msg)
    { DGP_MsgDump(0, &req.header, req.data.len);
    }

    // NB. when ULOK' system ID is 255, the LO(remjob) contains
    //     the DGP SYSID of the remote system
    if (req.header.code != DGP_ULOK)
    { ASSERT((256 < remjob) && (remjob < 65281));	// validate
    }

    send_START = 0;
    client = DGP_SYSID(remjob);
    MEM_BARRIER;
    if ((255 != client) &&				// not a system msg
        (0 == systab->dgpSTART[client]))		//   AND START not sent?
    { SemOp( SEM_WD, WRITE);
      if (0 == systab->dgpSTART[client])		// START still not sent?
      { systab->dgpSTART[client] = 1;			// mark as sent
	// if client crashed during server restart, or just mounts the volume
	// do NOT send START message back
        send_START = req.header.code != DGP_MNTV;
      }
      SemOp( SEM_WD, -WRITE);

      if (send_START)					// send START ?
      { // fprintf(stderr,"sending restart to %d (code=%d)\n",client,req.header.code); fflush(stderr);
        DGP_MkError(&rep, -(ERRZ85+ERRMLAST));		//   make reply
        goto Send;					//   and send
      }
    }

    if ((req.header.code > DGP_ZDAL) && (DGP_MNTV != req.header.code))
    { // fprintf(stderr,"got GLB=[%s] (len=%d)\n", (char *) &req.data.buf[0], req.data.len);
      old = req.data.buf[req.data.len];			// patch GLB name
      req.data.buf[req.data.len] = '\0';
      s = UTIL_MvarFromCStr((cstring *) &req.data, &var);
      // fprintf(stderr,"GLB=[%s]\n", &db_var.name.var_cu[0]); fflush(stderr);
      if (s < 0) goto Error;
      req.data.buf[req.data.len] = old;
      varptr = &var;
    }
    if (req.header.code <= DGP_ZDAL)
    { data = (DGPData *) &(req.data.buf[req.data.len]);
      lck_count = ntohs(data->len);			// #LOCK entries
      ptr1 = (cstring *) &req.data.buf[0];		// convert LOCK entries
      s = LCK_StringToLock(lck_count, ptr1, &cstr);
      if (s < 0) goto Error;
      lck_list = &cstr;
    }

    switch (req.header.code)
    { case DGP_MNTV:
        req.data.buf[req.data.len] = '\0';		// terminate VOL name
        s = getvol((cstring *) &req.data);		// get volume by name
        if (s < 0) goto Error;				// not found ?
	if (systab->vol[s-1]->local_name[0])		// remote volume ?
	{ s = -(ERRZ84+ERRMLAST);			//   report error
	  goto Error;
	}
	SemOp( SEM_SYS, -systab->maxjob);
        DGP_MkValue(&rep, SIZEOF_LABEL_BLOCK,		// copy VOL label
		    (u_char *) systab->vol[s-1]->vollab);
	SemOp( SEM_SYS, systab->maxjob);
        remote_label = (label_block *) &rep.data.buf[0];
							// cvt to network fmt
	remote_label->magic        = htonl(remote_label->magic);
	remote_label->max_block    = htonl(remote_label->max_block);
	remote_label->header_bytes = htonl(remote_label->header_bytes);
	remote_label->block_size   = htonl(remote_label->block_size);
        remote_label->db_ver	   = htons(remote_label->db_ver);
        for (i = 0; i < UCIS; i++)
          remote_label->uci[i].global = htonl(remote_label->uci[i].global);
	remote_label->txid         = htonll(remote_label->txid);
        break;
      case DGP_LOKV: 
        s = LCK_Old(lck_count, lck_list, systab->dgpLOCKTO, remjob);
        if (s < 0) goto Error;
        DGP_MkStatus(&rep, partab.jobtab->test);
        break;
      case DGP_ULOK: 
        LCK_Remove(remjob);
        DGP_MkStatus(&rep, 0);
        break;
      case DGP_ZALL: 
        s = LCK_Add(lck_count, lck_list, systab->dgpLOCKTO, remjob);
        if (s < 0) goto Error;
        DGP_MkStatus(&rep, partab.jobtab->test);
        break;
      case DGP_ZDAL: 
        s = LCK_Sub(lck_count, lck_list, remjob);
        if (s < 0) goto Error;
        DGP_MkStatus(&rep, s);
        break;
      case DGP_GETV:
	s = DB_Get(varptr, &rep.data.buf[0]);
	if (s < 0) goto Error;
	DGP_MkValue(&rep, s, &rep.data.buf[0]);
        break;
      case DGP_SETV:
        ptr1 = (cstring *) (&req.data.buf[req.data.len]);
	ptr1->len = ntohs(ptr1->len);			// cvt to host fmt
	s = DB_Set(varptr, ptr1);
	if (s < 0) goto Error;
	DGP_MkStatus(&rep, s);
        break;
      case DGP_ZINC:
        ptr1 = (cstring *) (&req.data.buf[req.data.len]);
	ptr1->len = ntohs(ptr1->len);			// cvt to host fmt
	// fprintf(stderr,"ptr1->len=%d ptr1->buf=%s\n",ptr1->len,&ptr1->buf[0]); fflush(stderr);
        s = Dzincrement2((cstring *) &rep.data, varptr, ptr1);
        if (s < 0) goto Error;
        DGP_MkValue(&rep, s, &rep.data.buf[0]);
        break;
      case DGP_KILV:
	s = DB_KillEx(varptr, req.header.msgflag);
	if (s < 0) goto Error;
	DGP_MkStatus(&rep, s);
	break;
      case DGP_ORDV: 
        s = DB_OrderEx(varptr, 
                       &rep.data.buf[0],
                       DGP_F_PREV & req.header.msgflag ? -1 : 1,
		       DGP_F_RDAT & req.header.msgflag ? &cstr : NULL);
        if (s < 0) goto Error;
        DGP_MkValue(&rep, s, &rep.data.buf[0]);
        if (DGP_F_RDAT & req.header.msgflag)
        { DGP_AppendValue(&rep, cstr.len, &cstr.buf[0]);
        }
        break;
      case DGP_QRYV:
        s = DB_QueryEx(varptr, 
                       &rep.data.buf[0],
                       DGP_F_PREV & req.header.msgflag ? -1 : 1,
		       1, /* docvt */
		       DGP_F_RDAT & req.header.msgflag ? &cstr : NULL);
        if (s < 0) goto Error;
        DGP_MkValue(&rep, s, &rep.data.buf[0]);
        if (DGP_F_RDAT & req.header.msgflag)
        { DGP_AppendValue(&rep, cstr.len, &cstr.buf[0]);
        }
        break;
      case DGP_DATV: 
        s = DB_DataEx(varptr, 
                      &rep.data.buf[0],
		      DGP_F_RDAT & req.header.msgflag ? &cstr : NULL);
        if (s < 0) goto Error;
        DGP_MkValue(&rep, s, &rep.data.buf[0]);
        if (DGP_F_RDAT & req.header.msgflag)
        { DGP_AppendValue(&rep, cstr.len, &cstr.buf[0]);
        }
	break;
      default:
        do_log("unknown message code: %d\n", req.header.code);
        s = -(ERRZ82+ERRMLAST);
Error:  DGP_MkError(&rep, s);
    }

Send:
    if (DGP_ERR == rep.header.code)
      DGP_MkError(&rep, -(ERRZ82+ERRMLAST));

    if (dump_msg)
    { DGP_MsgDump(1, &rep.header, rep.data.len);
    }
    msg_len = rep.header.msglen;
    rep.header.remjob = htons(rep.header.remjob);	// cvt to network fmt
    rep.header.msglen = htons(rep.header.msglen);
    rep.data.len      = htons(rep.data.len);
    bytes = nn_send(sock, &rep, msg_len, 0);
    if (bytes < 0)
    { do_log("nn_send: %s\n", nn_strerror(nn_errno()));
      if (send_START)					// failed to send START
      { SemOp( SEM_WD, WRITE);
	systab->dgpSTART[client] = 0;			// clear START sent
	SemOp( SEM_WD, -WRITE);
      }
    }
  }
#else
  for (;;)
    MSLEEP(1000);
#endif
}


//-----------------------------------------------------------------------------
// Function: Net_Daemon
// Descript: Start network daemon for passed in slot and vol#
// Input(s): slot# and Vol#
// Return:   0 -> Ok, any non-zero = error
//

int Net_Daemon(int slot, int vol)			// start a daemon
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
    systab->vol[volnum-1]->wd_tab[slot].type = WD_NET;	// network daemon
    return (0);						// return (am parent)
  }							// end parent code
  if (fit < 0)
  { return (errno);					// die on error
  }

  curr_lock = 0;					// clear lock flag
  bzero(semtab, sizeof(semtab));
  curr_sem_init = 1;
  for (i = 0; i < MAX_VOL; i++)
  { last_map_write[i] = (time_t) 0;                     // clear last map write
    dbfds[i] = 0;                                       // db file desc.
    jnl_fds[i] = 0;					// jrn file desc.
  }
  mypid = 0;

  // -- Create log file name --
  k = strlen(systab->vol[0]->file_name);		// get len of filename
  for (i=(k-1); (systab->vol[0]->file_name[i] != '/') && (i > -1); i--);
  							// find last '/'
  strncpy( logfile, systab->vol[0]->file_name, (i+1) );	// copy to log filename
  logfile[(i+1)] = (char) '\0';				// terminate JIC

  sprintf(&logfile[strlen(logfile)],"netdaemon_%d.log",slot); // add slot to nm
  myslot = slot;					// remember my slot
  myslot_net = myslot - systab->vol[0]->num_of_daemons; // remember network slot

  partab.jobtab = &systab->jobtab[systab->maxjob + myslot_net];// dummy jobtab
  partab.jobtab->pid = getpid();

  // setup jobtab entries
  partab.jobtab->precision = systab->precision;		// decimal precision
  partab.jobtab->start_len = 				// store start date/time
    Vhorolog(partab.jobtab->start_dh);
  partab.jobtab->dostk[0].type = TYPE_JOB;		// ensure slot 0 has val
  systab->vol[0]->last_blk_used[MV1_PID] = 0;		// clear last glb block
  systab->vol[0]->last_blk_written[MV1_PID] = 0;	// clear last written

  partab.debug = 0;					// clear debug flag
  partab.sstk_start = &sstk[0];				// address of sstk
  partab.sstk_last = &sstk[MAX_SSTK];			// and the last char
  partab.varlst = NULL;

  partab.jobtab->attention = 0;
  partab.jobtab->trap = 0;
  partab.jobtab->async_error = 0;

  // --- Reopen stdin, stdout, and stderr ( logfile ) ---
  a = freopen("/dev/null","r",stdin);			// stdin to bitbucket
  a = freopen("/dev/null","w",stdout);			// stdout to bitbucket
  a = freopen(logfile,"a",stderr);			// stderr to logfile
  if (!a) return (errno);			        // check for error

  dbfds[0] = open(systab->vol[0]->file_name, O_RDONLY);	// open database rdonly
  if (dbfds[0] < 0)
  { do_log("Cannot open database file %s\n",
                  systab->vol[0]->file_name);
    return(errno);					// check for error
  }
  partab.vol_fds[0] = dbfds[0];				// make sure fd is right

  t = time(0);						// for ctime()
  do_log("Network Daemon %d started successfully\n", myslot);   // log success

  i = MSLEEP(1000);					// wait a bit

  do_netdaemon();					// do something

  return 0;						// never gets here
}

//-----------------------------------------------------------------------------
// Function: DB_Daemon
// Descript: Start daemon for passed in slot and vol#
// Input(s): slot# and Vol#
// Return:   0 -> Ok, any non-zero = error
//

u_char *wrbuf = 0;

int DB_Daemon(int slot, int vol)			// start a daemon
{ int i;						// a handy int 
  int k;						// and another
  int fit;						// for fork ret
  char logfile[100];					// daemon log file name
  FILE *a;						// file pointer
  time_t t;						// for ctime()
  int rest;						// rest time
  unsigned stalls, old_stalls;				// no. of stalls

  volnum = vol;						// save vol# here

  fit = ForkIt(-1);					// start a daemon
  if (fit > 0)						// check for ok (parent)
  { systab->vol[volnum-1]->wd_tab[slot].pid = fit;	// put in childs pid
    systab->vol[volnum-1]->wd_tab[slot].type = WD_WRITE;// write daemon
    return (0);						// return (am parent)
  }							// end parent code
  if (fit < 0)
  { return (errno);					// die on error
  }

  curr_lock = 0;					// clear lock flag
  bzero(semtab, sizeof(semtab));
  curr_sem_init = 1;
  last_daemon_check = (time_t) 0;
  for (i = 0; i < MAX_VOL; i++)
  { last_map_write[i] = (time_t) 0;                     // clear last map write
    dbfds[i] = 0;                                       // db file desc.
    jnl_fds[i] = 0;                                     // clear jrn file desc.
    jnl_files[i][0] = 0;				// clear jrn file name
    last_sync[i] = time(0) + DEFAULT_GBSYNC;            // global buffer sync
  }
  mypid = 0;

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

#ifdef MV1_BLKSEM
  wrbuf = (u_char *) mv1malloc(                         // alloc a write buffer
   		 systab->vol[volnum-1]->vollab->block_size);
  if (0 == wrbuf)
  { do_log("Cannot alloc write buffer\n");
    return(ENOMEM);					// check for error
  }
#endif

  dbfds[0] = open(systab->vol[0]->file_name, O_RDWR);	// open database r/wr
  if (dbfds[0] < 0)
  { do_log("Cannot open database file %s\n",
                  systab->vol[0]->file_name);
    return(errno);					// check for error
  }

#ifdef MV1_F_NOCACHE
  i = fcntl(dbfds[0], F_NOCACHE, 1);
#endif
  t = time(0);						// for ctime()
  do_log("Daemon %d started successfully\n", myslot);   // log success

  if (!myslot)
    systab->Mtime = time(0);                            // update M time

  SLOT_JRN       =  0;                                  // default Daemon 0
  SLOT_VOLSYNC   = -1;                                  // default none
  if (systab->vol[0]->num_of_daemons > 1)               // more than 1 daemons ?
    SLOT_JRN     =  1;                                  //   be Daemon 1
  if (systab->vol[0]->num_of_daemons > 2)               // more than 2 daemons ?
    SLOT_VOLSYNC =  2;                                  //   be Daemon 2

  if ((systab->vol[0]->upto) && (!myslot))		// if map needs check
  { ic_map(-3, 0, dbfds[0]);				// doit
  }

  i = MSLEEP(1000);					// wait a bit

  old_stalls = 0;
  while (TRUE)						// forever
  { if (!myslot)					// daemon 0 calculates
    { stalls = 0; 					//   rest time
      for (i = 0; i < MAX_VOL; i++)
      { if (systab->vol[vol]->local_name[0])		// remote VOL ?
	  continue;					//   skip it
        if (NULL == systab->vol[i]->vollab)		// stop at first
          break;					//   unallocated vol.
        stalls += systab->vol[i]->stats.dqstall +	// check dirtyQ stalls
		  systab->vol[i]->stats.gbswait;	//   and GBDs waits
      }
      rest = systab->ZRestTime;
      if (stalls == old_stalls)
        rest *= 1.1;
      else
        rest /= 2;
      if (rest < MIN_REST_TIME)
        rest = MIN_REST_TIME;
      else if (rest > MAX_REST_TIME)
        rest = MAX_REST_TIME;
      systab->ZRestTime = rest;
      old_stalls = stalls;
    }
    i = MSLEEP(systab->ZRestTime);                      // rest
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
{ time_t t;						// for ctime()
#ifdef MV1_CKIT
  void *qentry;                                         // queue entry
#endif
  int vol;                                              // vol[] index
  int dirtyQr, garbQr;                                  // queue read indices

start:
  if (!myslot)                                          // update M time
  { systab->Mtime = time(0);
  }

  daemon_check();					// ensure all running
  if (systab->vol[0]->wd_tab[myslot].doing == DOING_NOTHING)
  { for (vol = 0; vol < MAX_VOL; vol++)
    { if (systab->vol[vol]->local_name[0])		// remote VOL ?
	continue;					//   skip it
      if (NULL == systab->vol[vol]->vollab)             // stop at first
        break;                                          //   not mounted
      if ((!myslot) &&                                  // first daemon ?
          (systab->vol[vol]->map_dirty_flag) &&         //   vol map dirty ?
          (MTIME(0) != last_map_write[vol]))            //     not updated ?
      { do_map_write(vol);				// write map
      }							// end map write
      if ((!myslot) && (systab->vol[vol]->writelock < 0)) // check wrtlck
      { do_quiescence();                                // reach quiet point
        inter_add(&systab->delaywt, -1);                // decr. delay WRITEs
        MEM_BARRIER;
        // Set the writelock to a positive value when all quiet
        systab->vol[vol]->writelock = abs(systab->vol[vol]->writelock);
      }							// end wrtlock
      if ((SLOT_JRN == myslot) &&
          (systab->vol[vol]->vollab->journal_available) && // jrn available ?
          (systab->vol[vol]->lastdojrn < MTIME(0)) &&   // DoJrn is old ?
          (systab->vol[vol]->jrnbufsize))               // has records ?
      { do_jrnflush(vol);                               // do jrn flush
      }
      if ((SLOT_VOLSYNC == myslot) &&
          (systab->vol[vol]->gbsync) &&                 // need sync ?
          (0 == systab->vol[vol]->writelock) &&         //   not locked ?
          (last_sync[vol] < MTIME(0)))                  //     sync is over ?
      { do_volsync(vol);                                // do volume sync
      }
    }                                                   // end foreach vol
#ifdef MV1_CKIT
    if (ck_ring_dequeue_spmc(                           // any writes?
                &systab->vol[0]->dirtyQ,
                &systab->vol[0]->dirtyQBuffer[0],
                &qentry))
    { systab->vol[0]->wd_tab[myslot].currmsg.gbddata
        = qentry;                                       // get
      systab->vol[0]->wd_tab[myslot].doing = DOING_WRITE;
    } else if (ck_ring_dequeue_spmc(                    // any garbage?
                &systab->vol[0]->garbQ,
                &systab->vol[0]->garbQBuffer[0],
                &qentry))
    { systab->vol[0]->wd_tab[myslot].currmsg.intdata
        = (u_int) qentry;                               // get
      systab->vol[0]->wd_tab[myslot].doing = DOING_GARB;
    }
#else
    while (SemOp(SEM_WD, WRITE));			// lock WD
    dirtyQr = systab->vol[0]->dirtyQr;
    garbQr  = systab->vol[0]->garbQr;
    if (systab->vol[0]->dirtyQ[dirtyQr] != NULL)	// any writes?
    { systab->vol[0]->wd_tab[myslot].currmsg.gbddata
        = systab->vol[0]->dirtyQ[dirtyQr];              // get
      systab->vol[0]->wd_tab[myslot].doing = DOING_WRITE;
      systab->vol[0]->dirtyQ[dirtyQr] = NULL;
      systab->vol[0]->dirtyQr = (dirtyQr+1) & (NUM_DIRTY-1); // increment ptr
    }
    else if (systab->vol[0]->garbQ[garbQr]) 	        // any garbage?
    { systab->vol[0]->wd_tab[myslot].currmsg.intdata
        = systab->vol[0]->garbQ[garbQr];                // get
      systab->vol[0]->wd_tab[myslot].doing = DOING_GARB;
      systab->vol[0]->garbQ[garbQr] = 0;
      systab->vol[0]->garbQr = (garbQr+1) & (NUM_GARB-1); // increment ptr
    }
    SemOp( SEM_WD, -WRITE);				// release WD lock
#endif
  }							// end looking for work

  // XXX most ne lehessen kulon/kulon dismount-olni, csak egyben
  if (systab->vol[0]->wd_tab[myslot].doing == DOING_NOTHING)
  { if (systab->vol[0]->dismount_flag)		        // dismounting?
    { if (myslot)					// first?
      { systab->vol[0]->wd_tab[myslot].pid = 0;	        // say gone
        t = time(0);					// for ctime()
	do_log("Daemon %d shutting down\n", myslot);	// log success
        SemStats();                                     // print sem stats
        exit (0);					// and exit
      }
      do_dismount(vol);				        // dismount it
      exit (0);					        // and exit
    }							// end dismount code
    else
    { return;						// nothing to do
    }
  }
  if (systab->vol[0]->wd_tab[myslot].doing == DOING_WRITE)
  { ASSERT(systab->vol[0]->wd_tab[myslot].currmsg.gbddata != NULL);
    do_write();						// do it 
    goto start;						// try again
  }
  if (systab->vol[0]->wd_tab[myslot].doing == DOING_GARB)
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
  int vol;                                              // vol[] index
  char msg[128];                                        // msg buffer
  int jfd;                                              // jrn file desc.
  int num_daemons;					// num of all daemons

  i = shmctl(systab->vol[0]->shm_id, (IPC_RMID), &sbuf); //remove share
  // NB. network daemons have jobtab entries, they are stopped here
							// for each job
  for (i = 0; i < systab->maxjob + systab->vol[0]->num_of_net_daemons; i++)
  { pid = systab->jobtab[i].pid;			// get pid
    if (pid)						// if pid != 0
    if (kill( pid, SIGTERM))				// kill this one
    { systab->jobtab[i].trap = 1 << SIGTERM;		// say go away
      systab->jobtab[i].attention = 1;			// look at it
    }
  }

  for (vol = 0; vol < MAX_VOL; vol++)                   // flush journal
  { if (systab->vol[vol]->local_name[0])		// remote VOL ?
      continue;						//   skip it
    if (NULL == systab->vol[vol]->vollab)               // stop if not mounted
      break;
    jfd = open_jrn(vol);                                // open jrn file
    if (jfd)
    { j = FlushJournal(vol, jfd, 1);
      if (j < 0)
      { do_log("Failed to flush journal file %s: Last failed - %d\n",
	          systab->vol[vol]->vollab->journal_file, errno);
      }
      close(jfd);
    }
  }                                                     // end journal stuff 

  for (vol = 0; vol < MAX_VOL; vol++)                   // check vol maps
  { if (systab->vol[vol]->local_name[0])		// remote VOL ?
      continue;						//   skip it
    if (NULL == systab->vol[vol]->vollab)               // stop at first
      break;                                            //   not mounted
    if (systab->vol[vol]->map_dirty_flag)               // vol map dirty ?
    { do_mount(vol);                                    // mount db file
      off = lseek( dbfds[vol], 0, SEEK_SET);	        // move to start of file
      if (off < 0)
      { sprintf(msg, "do_daemon: lseek() to start of vol %d failed", vol);
        panic(msg);
      }
      i = write( dbfds[vol], systab->vol[vol]->vollab,
	     systab->vol[vol]->vollab->header_bytes);   // map/label
      if (i < 0)
      { sprintf(msg, "do_daemon: write() map block of vol %d failed", vol);
        panic(msg);
      }
      systab->vol[vol]->map_dirty_flag = 0;	        // unset dirty flag
    }
  }

  for (vol = 0; vol < MAX_VOL; vol++)
  { if (systab->vol[vol]->local_name[0])		// remote VOL ?
      continue;						//   skip it
    if (NULL == systab->vol[vol]->vollab)               // stop if not mounted
      break;
    for (i=0; i<systab->vol[vol]->num_gbd; i++)	        // look for unwritten
    { if ((systab->vol[vol]->gbd_head[i].block) && 	// if there is a blk
          (systab->vol[vol]->gbd_head[i].last_accessed != (time_t) 0) &&
	  (systab->vol[vol]->gbd_head[i].dirty))
      { systab->vol[vol]->gbd_head[i].dirty
          = &systab->vol[vol]->gbd_head[i]; 	        // point at self

        systab->vol[0]->wd_tab[0].currmsg.gbddata     // add to our struct
	  = &systab->vol[vol]->gbd_head[i];
        do_write();					// write it
      }							// end gbd has blk
    }							// end blk search
  }

  i = 1;
  while (i)						// while theres pids
  { i = 0;						// reset pid counter
    while (SemOp( SEM_WD, WRITE ));			// lock daemon table
    num_daemons = systab->vol[0]->num_of_daemons +	// write daemons
		  systab->vol[0]->num_of_net_daemons;	// network daemons
    for (j=1; j<num_daemons; j++)    			// search
    { if (systab->vol[0]->wd_tab[j].pid)
      { if (kill(systab->vol[0]->wd_tab[j].pid, 0))
        { if (errno == ESRCH)				// if no such
          { systab->vol[0]->wd_tab[j].pid = 0;	        // clear it
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
  do_log("Writing out clean flag as clean\n");          // operation
  for (vol = 0; vol < MAX_VOL; vol++)
  { if (systab->vol[vol]->local_name[0])		// remote VOL ?
      continue;						//   skip it
    if (NULL == systab->vol[vol]->vollab)               // stop if not mounted
      break;
    systab->vol[vol]->vollab->clean = 1;		// set database as clean
    do_mount(vol);                                      // mount db file
    off =lseek( dbfds[vol], 0, SEEK_SET);		// seek to start of file
    if (off < 0)
    { do_log("do_dismount: lseek() to start of file failed");
    }
    i = write( dbfds[vol], 				// write to database
	 systab->vol[vol]->vollab, 		        // the map/label block
	 systab->vol[vol]->vollab->		        // with the clean
	 header_bytes );				// flag set.
    if (i != systab->vol[vol]->vollab->header_bytes )
    { do_log("do_dismount: write() map block failed");
    }   
  }
  i = semctl(systab->sem_id, 0, (IPC_RMID), NULL);	// remove the semaphores
  if (i)
  { do_log("do_dismount: semctl() failed to remove semaphores");
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
  u_int blkno;                                          // block#
  DB_Block *wrbuf;                                      // write buffer
  int vol;                                              // vol[] index
  char msg[128];                                        // msg buffer

  gbdptr = systab->vol[0]->			        // get the gbdptr
  		wd_tab[myslot].currmsg.gbddata;		// from daemon table

  if (!gbdptr)
  { systab->vol[0]->wd_tab[myslot].doing = DOING_NOTHING;
    panic("Daemon: write msg gbd is NULL");		// check for null
  }

  // NB. egy lancban egy volume-hoz tartoznak!
  volnum = gbdptr->vol + 1;                             // set volnum

  if (!curr_lock)					// if we need a lock
  { while (SemOp( SEM_GLOBAL, READ));			// take a read lock
  }
  while (TRUE)						// until we break
  { if (gbdptr->last_accessed == (time_t) 0)		// if garbaged
    { gbdptr->block = 0;				// just zot the block
    }
    else						// do a write
    { blkno = gbdptr->block;                            // get blkno
      vol   = gbdptr->vol;                              //   and volume
      do_mount(vol);                                    // mount db file
      Check_BlockNo(vol, blkno,				// because of ^IC
		    CBN_INRANGE,			// check only range
                    "Write_Chain", 0, 0, 1);     	// validity
#ifdef MV1_BLKSEM
      while (BLOCK_TRYREADLOCK(gbdptr) < 0)             // wait for read lock
      { ATOMIC_INCREMENT(systab->vol[vol]->stats.brdwait);// count a wait
        SchedYield();                                   //   release quant
      }                                                 //   if failed
      bcopy(gbdptr->mem, wrbuf,                         // copy block
		 systab->vol[vol]->vollab->block_size);
      BLOCK_UNLOCK(gpdptr);
#else
      wrbuf = gbdptr->mem;
#endif
      file_off = (off_t) blkno - 1;	                // block#
      file_off = (file_off * (off_t)
    			systab->vol[vol]->vollab->block_size)
		 + (off_t) systab->vol[vol]->vollab->header_bytes;
      file_off = lseek( dbfds[vol], file_off, SEEK_SET);// Seek to block
      if (file_off < 1)
      { systab->vol[vol]->stats.diskerrors++;	        // count an error
        sprintf(msg, "lseek of vol %d failed in Write_Chain()!!", vol);
        panic(msg);                                     // die on error
      }
      wrbuf->blkrevno = systab->vol[vol]->vollab->blkrevno;
      i = write( dbfds[vol], wrbuf,
		 systab->vol[vol]->vollab->block_size); // write it
      if (i < 0)
      { systab->vol[vol]->stats.diskerrors++;	        // count an error
        sprintf(msg, "write of vol %d failed in Write_Chain()!!", vol);   
        panic(msg);                                     // die on error
      }
      ATOMIC_INCREMENT(systab->vol[vol]->stats.phywt);  // count a write
    }							// end write code

    if (!gbdptr->dirty)
    { systab->vol[0]->wd_tab[myslot].		        // update the daemon
		currmsg.gbddata = NULL;			// table JIC I vanish
      systab->vol[0]->wd_tab[myslot].
      		doing = DOING_NOTHING;			// and here
      MEM_BARRIER;
      break;						// break from while
    }
    lastptr = gbdptr;					// remember this ptr
    gbdptr = gbdptr->dirty;  				// get next in list

    if (lastptr != gbdptr)  				// if not at end
    { systab->vol[0]->wd_tab[myslot].		        // update the daemon
		currmsg.gbddata = gbdptr;		// table JIC I vanish
    }
    else
    { systab->vol[0]->wd_tab[myslot].		        // update the daemon
		currmsg.gbddata = NULL;			// table JIC I vanish
      systab->vol[0]->wd_tab[myslot].
      		doing = DOING_NOTHING;			// and here
    }
    lastptr->dirty = NULL;				// clear old dirtyptr
    MEM_BARRIER;
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
  int vol;                                              // vol[] being garbed

  if (systab->vol[0]->wd_tab[myslot].currmsg.intdata == 0) // done?
  { systab->vol[0]->wd_tab[myslot].doing = DOING_NOTHING;// yes
    return;						// and exit
  }

  gb = systab->vol[0]->wd_tab[myslot].currmsg.intdata;  // get block
  systab->vol[0]->wd_tab[myslot].currmsg.intdata = 0;   // clear slot

  vol = VOLNO(gb);
  gb  = BLKNO(gb);
  do_mount(vol);                                        // mount db file
  gb = do_zot(vol, gb);			                // do it

  systab->vol[0]->wd_tab[myslot].doing = DOING_NOTHING; // flag done
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: do_zot
// Descript: Zot block(s)
//	     At this stage:	there is NO recovery.
//				there is no integrity check.
// Input(s): vol[] index and block number to zot
// Return:   negative error number or type byte of block zotted.
//

int do_zot(int vol,u_int gb)				// zot block
{ u_int i;						// a handy int
  int ret;						// for returns
  int Idx;						// the index
  DB_Block *bptr;					// block pointer
  off_t file_off;                               	// for lseek() et al
  off_t file_ret;                               	// for lseek() et al
  int typ;						// block type
  int zot_data = 0;					// bottom level flag
  gbd *ptr;						// a handy pointer

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(0 == systab->vol[vol]->local_name[0]);		// not remote VOL
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  bptr = mv1malloc(systab->vol[vol]->vollab->block_size);// get some memory
  if (bptr == NULL)					// if failed
  { do_log("do_zot: malloc for block %d failed\n", gb);
    return -1;						// return fail
  }

  file_off = (off_t) gb - 1;				// the block#
  file_off = (file_off * (off_t) systab->vol[vol]->vollab->block_size)
		+ (off_t) systab->vol[vol]->vollab->header_bytes;

  volnum = vol + 1;                                     // set volnum
  while (SemOp(SEM_GLOBAL, WRITE));			// take a global lock
  ptr = systab->vol[vol]->gbd_hash[GBD_BUCKET(gb)];     // get head
  while (ptr != NULL)					// for entire list
  { if (ptr->block == gb)				// found it?
    { bcopy(ptr->mem, bptr, systab->vol[vol]->vollab->block_size);
      ptr->last_accessed = (time_t) 0;			// mark as zotted
      ptr->block = 0;
      MEM_BARRIER;
      break;						// exit
    }
    ptr = ptr->next;					// point at next
  }							// end memory search
  SemOp(SEM_GLOBAL, -curr_lock);			// release the lock

  if (ptr == NULL)					// if not found
  { file_ret = lseek( dbfds[vol], file_off, SEEK_SET);	// seek to block
    if (file_ret < 1)
    { do_log("do_zot: seek to block %d:%d failed\n", vol, gb);
      mv1free(bptr);					// free memory
      return -1;					// return error
    }
    ret = read( dbfds[vol], bptr,
	       systab->vol[vol]->vollab->block_size);   // read it
    if (ret < 0)					// if it failed
    { do_log("do_zot: read of block %d:%d failed\n", vol, gb);
      mv1free(bptr);					// free memory
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
    if (zot_data)			                // if we are zotting
    { ret = 0;
      if (systab->ZotData)
      { file_ret = (off_t) i - 1;			// block#
        file_ret = (file_ret *(off_t) systab->vol[vol]->vollab->block_size)
		 + (off_t) systab->vol[vol]->vollab->header_bytes;
        file_ret = lseek( dbfds[vol], file_ret, SEEK_SET);// Seek to block
        if (file_ret < 1)	        		// check for fail
        { do_log("do_zot: seek to block %d:%d failed\n", vol, i);
          ret = -1;                                     // flag an error
        }
        else						// looks ok
        { ret = write( dbfds[vol], systab->vol[vol]->zero_block,
	     systab->vol[vol]->vollab->block_size);     // write zeroes
          if (ret < 0)					// fail ?
          { do_log("do_zot: zero of block %d:%d failed\n", vol, i);
          }
        }
      }
      if (!(ret < 0))
      { if (systab->ZotData)                            // count a write
          ATOMIC_INCREMENT(systab->vol[vol]->stats.phywt);
        ATOMIC_INCREMENT(systab->vol[vol]->stats.logwt);// and a logical
        do_free(vol, i);				// free the block
      }
    }							// end zotting
    else						// give to lower level
    { ret = do_zot(vol, i);				// re-call
      if (ret > 64)					// data block?
      { zot_data = TRUE;				// do the rest here
      }
    }
  }							// end of indexes

zotit:
  if (systab->ZotData)                                  // if zero data
  { file_ret = lseek( dbfds[vol], file_off, SEEK_SET);	// seek to block
    if (file_ret < 1)
    { do_log("do_zot: zeroing seek to block %d:%d failed\n", vol, gb);
      mv1free(bptr);					// free memory
      return -1;					// return error
    }
    ret = write( dbfds[vol], systab->vol[vol]->zero_block,
	       systab->vol[vol]->vollab->block_size); // write zeroes
    if (ret < 0)					// if it failed
    { do_log("do_zot: zero of block %d:%d failed\n", vol, gb);
      typ = -1;						// flag fail
    }
  }
  if (systab->ZotData)                                  // count a write
    ATOMIC_INCREMENT(systab->vol[vol]->stats.phywt);
  ATOMIC_INCREMENT(systab->vol[vol]->stats.logwt);	// and a logical
  mv1free(bptr);					// free memory
  do_free(vol, gb);					// and the block
  return typ;						// return the type
}

//-----------------------------------------------------------------------------
// Function: do_free
// Descript: Free a block block in the map and GBDs (if reqd)
// Input(s): block number to free
// Return:   none
//

void do_free(int vol, u_int gb)				// free from map et al
{ gbd *ptr;						// GBD ptr

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(0 == systab->vol[vol]->local_name[0]);		// not remote VOL
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  volnum = vol + 1;                                     // set volnum
  while (TRUE)						// a few times
  { daemon_check();					// ensure all running
    if (!SemOp( SEM_GLOBAL, WRITE))			// gain write lock
    { break;						// it worked
    }
    MSLEEP(1000);					// wait a bit
  }
  
  Free_block(vol, gb);					// free the block

  ptr = systab->vol[vol]->gbd_hash[GBD_BUCKET(gb)];     // get listhead
  while (ptr != NULL)					// for each in list
  { if (ptr->block == gb)				// found it
    { if (ptr->dirty < (gbd *) 5)			// not in use
      { Free_GBD(vol, ptr);				// free it
      }
      else						// in use or not locked
      { ptr->last_accessed = (time_t) 0;		// mark as zotted
        ptr->block = 0;
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
  int dtyp;						// daemon type
  static char *dtypes[] = { "Daemon", "Network Daemon" };
  int num_daemons;					// no of all daemons

  if (last_daemon_check == MTIME(0))
    return;

  if (systab->vol[0]->dismount_flag)			// do NOT restart if
    return;						//   dismounting

  while (SemOp(SEM_WD, WRITE));			        // lock WD
  num_daemons = systab->vol[0]->num_of_daemons +	// write daemons
                systab->vol[0]->num_of_net_daemons;	// network daemons
  for (i = 0; i < num_daemons; i++)
  { if (i != myslot)					// don't check self
    { fit = kill(systab->vol[0]->wd_tab[i].pid, 0);
      if (systab->vol[0]->dismount_flag)		// leave if
        break;						//   dismounting
      if ((fit < 0) && (errno == ESRCH))                // if gone
      { dtyp = systab->vol[0]->wd_tab[i].type;		// daemon type
        fit = dtyp ?  Net_Daemon(i, 1) : DB_Daemon(i, 1);
        if (fit != 0)
        { do_log("daemon_check: failed to start %s %d\n", dtypes[dtyp], i);
        } else
        { do_log("daemon_check: restarted %s %d\n", dtypes[dtyp], i);
        }
      }
    }
  }							// end daemon check
  SemOp(SEM_WD, -WRITE);				// release lock

  last_daemon_check = MTIME(0);
  return;
}


//-----------------------------------------------------------------------------
// Function: do_mount
// Descript: Mount a volume, ie. open the corresponding file desc.
// Input(s): vol to mount
// Return:   none
//

void do_mount(int vol)                                  // mount volume
{ char msg[VOL_FILENAME_MAX + 128];                     // msg buffer

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(0 == systab->vol[vol]->local_name[0]);		// not remote VOL
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  if (dbfds[vol])
    return;

  dbfds[vol] = open(systab->vol[vol]->file_name, O_RDWR);// open database r/wr
  if (dbfds[vol] < 0)
  { do_log("Cannot open database file %s - %s\n",
                  systab->vol[vol]->file_name,
                  strerror(errno));
    sprintf(msg, "do_mount: open() of database file %s failed",
                    systab->vol[vol]->file_name);
    panic(msg);                                         // die on error
  }

#ifdef MV1_F_NOCACHE
  i = fcntl(dbfds[vol], F_NOCACHE, 1);
#endif

  if ((systab->vol[vol]->upto) && (!myslot))		// if map needs check
  { ic_map(-3, vol, dbfds[vol]);			// doit
  }

  return;
}


//-----------------------------------------------------------------------------
// Function: open_jrn
// Descript: Open the jrn file of the given volume.
// Input(s): vol - the volume
// Return:   jrn file desc.
//

int open_jrn(int vol)
{ int j;                                                // handy int

  ASSERT(0 <= vol);                                     // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(0 == systab->vol[vol]->local_name[0]);		// not remote VOL
  ASSERT(NULL != systab->vol[vol]->vollab);             // mounted

  if (jnl_fds[vol] && 					// already open ?
      (0 == strcmp(jnl_files[vol], 			//   AND same name ?
		   systab->vol[vol]->vollab->journal_file)))
    return jnl_fds[vol];                                //   return file desc.

  if (jnl_fds[vol])					// jrn file changed
  { close(jnl_fds[vol]);				//   close old fd
    jnl_fds[vol] = 0;					// clear jrn fds
    jnl_files[vol][0] = 0;				//   AND jrn file name
  }

  if ((systab->vol[vol]->vollab->journal_available) &&
      (systab->vol[vol]->vollab->journal_requested) &&
         (systab->vol[vol]->vollab->journal_file[0]))
  { struct stat   sb;				        // File attributes
    int jfd;					        // file descriptor
    u_char tmp[sizeof(u_int) + sizeof(off_t)];

    j = stat(systab->vol[vol]->vollab->journal_file, &sb ); // check for file
    if (j < 0)		                                // if that's junk
    { do_log("Failed to access journal file %s\n",
  		  systab->vol[vol]->vollab->journal_file);
      return 0;
    }
    jfd = open(systab->vol[vol]->vollab->journal_file, O_RDWR);
    if (jfd < 0)				        // on fail
    { do_log("Failed to open journal file %s\nerrno = %d\n",
		  systab->vol[vol]->vollab->journal_file, errno);
      return 0;
    }

#ifdef MV1_F_NOCACHE
    j = fcntl(jfd, F_NOCACHE, 1);
#endif
    lseek(jfd, 0, SEEK_SET);
    errno = 0;
    j = read(jfd, tmp, sizeof(u_int));	        	// read the magic
    if ((j != sizeof(u_int)) || (*(u_int *) tmp != (MUMPS_MAGIC - 1)))
    { do_log("Failed to open journal file %s: WRONG MAGIC\n",
		    systab->vol[vol]->vollab->journal_file);
      close(jfd);
      return 0;
    }
    jnl_fds[vol] = jfd;					// save jrn fd
    strcpy(jnl_files[vol], 				//   AND jrn file name
	   systab->vol[vol]->vollab->journal_file);
  }
  return jnl_fds[vol];
}


//-----------------------------------------------------------------------------
// Function: do_quiescence
// Descript: Reach a quiet point, ie. the dirty and garbage queues are empty.
// Input(s): None
// Return:   None
//

void do_quiescence(void)
{ int i, j;                                     // handy ints

  ASSERT(systab->delaywt != 0);                 // only with delay WRITEs

  while (TRUE)					// loop
  { 
#ifdef MV1_CKIT
    i = (0 < ck_ring_size(&systab->vol[0]->dirtyQ));
    i = (i) ||
        (0 < ck_ring_size(&systab->vol[0]->garbQ));
#else
    i = (systab->vol[0]->dirtyQ[systab->vol[0]->dirtyQr]
	       != NULL);			// check dirty que
    i = ((i) ||
	 (systab->vol[0]->garbQ[systab->vol[0]->garbQr]
	 != 0));				// and garbQ
#endif
    for (j = 1; j < systab->vol[0]->num_of_daemons; j++) // each one
    { i = ((i) || (systab->vol[0]->wd_tab[j].doing != 0));
    }
    if (!i)					// if all clear
    { break;					// leave loop
    }
    daemon_check();				// ensure all running
    i = MSLEEP(1000);				// wait a bit
  }						// end while (TRUE)
  i = MSLEEP(1000);				// just a bit more
}


//-----------------------------------------------------------------------------
// Function: do_jrnflush
// Descript: Flush the jrn file of vol to disk, sync the data.
// Input(s): vol - the volume
// Return:   None
//

void do_jrnflush(int vol)
{ int old_volnum;                               // old volnum
  int jfd;                                      // jrn file desc.

  ASSERT(0 <= vol);                             // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(0 == systab->vol[vol]->local_name[0]);	// not remote VOL
  ASSERT(NULL != systab->vol[vol]->vollab);     // mounted

  old_volnum = volnum;                          // save current vol
  jfd = open_jrn(vol);                          // open jrn file
  if (jfd) 
  { volnum = vol + 1;                           // set volnum
    while (SemOp( SEM_GLOBAL, WRITE));          // lock GLOBALs
    FlushJournal(vol, jfd, 0);                  // flush journal
    SemOp( SEM_GLOBAL, -curr_lock);             // release GLOBALs
    fsync(jfd);                                 // sync to disk
  }
  volnum = old_volnum;                          // restore volnum
}


//-----------------------------------------------------------------------------
// Function: do_volsync
// Descript: Flush the changes of a volume to disk, sync data.
// Input(s): vol - the volume
// Return:   None
//

void do_volsync(int vol)
{ int old_volnum;                               // old volnum
  int jfd;                                      // jrn file desc.

  ASSERT(0 <= vol);                             // valid vol[] index
  ASSERT(vol < MAX_VOL);
  ASSERT(0 == systab->vol[vol]->local_name[0]);	// not remote VOL
  ASSERT(NULL != systab->vol[vol]->vollab);     // mounted

  old_volnum = volnum;
  do_mount(vol);                                // mount vol. 
  inter_add(&systab->delaywt, 1);               // delay WRITEs
  MEM_BARRIER;
  do_quiescence();                              // reach quiet point
  if (systab->vol[vol]->vollab->journal_available) // journal available ?
  { jfd = open_jrn(vol);                        // open journal
    if (jfd)
    { volnum = vol + 1;
      while (SemOp( SEM_GLOBAL, WRITE));        // lock GLOBALs
      FlushJournal(vol, jfd, 0);                // flush journal
      SemOp( SEM_GLOBAL, -curr_lock);           // release GLOBALs
      fsync(jfd);                               // sync JRN to disk
    }
  }
  fsync(dbfds[vol]);                            // sync volume to disk
  if (systab->vol[vol]->vollab->journal_available) // journal available ?
  { jfd = open_jrn(vol);                        // open journal
    if (jfd)
    { jrnrec jj;                                // write SYNC record
      jj.action = JRN_SYNC;
      jj.time = MTIME(0);
      jj.uci = 0;
      volnum = vol + 1;
      while (SemOp( SEM_GLOBAL, WRITE));
      DoJournal(&jj, 0);                        // do journal
      FlushJournal(vol, jfd, 0);                // flush journal
      SemOp( SEM_GLOBAL, -curr_lock);           // release GLOBALs
      fsync(jfd);                               // sync to disk
    }
  }
  inter_add(&systab->delaywt, -1);              // release WRITEs
  MEM_BARRIER;
  last_sync[vol] = time(0) + systab->vol[vol]->gbsync; // next sync time
  volnum = old_volnum;
}


//-----------------------------------------------------------------------------
// Function: do_map_write
// Descript: Write the dirty map portions to disk
// Input(s): vol - the volume
// Return:   None
//

void do_map_write(int vol)
{ int i, j;					// a handy int
  int ret;					// for return value
  int block;					// block position
  off_t file_off;				// for lseek()
  u_int dirty_flag;				// map_dirty_flag
  u_int chunk;					// chunk bitmap
  char msg[128];				// msg buffer

  do_mount(vol);                                // mount db file
  dirty_flag = systab->vol[vol]->map_dirty_flag;
  if (dirty_flag & VOLLAB_DIRTY)		// check label block
  { while (SemOp( SEM_GLOBAL, READ));		// take a READ lock
    file_off = lseek( dbfds[vol], 0, SEEK_SET);	// move to start of file
    if (file_off < 0)
    { systab->vol[vol]->stats.diskerrors++;	// count an error
      sprintf(msg, "do_daemon: lseek() to start of vol %d failed", vol);
      panic(msg);
    }
    i = write( dbfds[vol], systab->vol[vol]->vollab, SIZEOF_LABEL_BLOCK);
    if ((i < 0) || (i != SIZEOF_LABEL_BLOCK))
    { systab->vol[vol]->stats.diskerrors++;	// count an error
      sprintf(msg, "do_daemon: write() map block of vol %d failed", vol);
      panic(msg);
    }
    systab->vol[vol]->map_dirty_flag ^= VOLLAB_DIRTY;
    SemOp( SEM_GLOBAL, -curr_lock);		// release lock
    dirty_flag ^= VOLLAB_DIRTY;			// clear VOLLAB flag
  }
  if (dirty_flag != VOLLAB_DIRTY)		// check map chunks
  { block = 0;
    for (i = 0; i < MAX_MAP_CHUNKS; i++)
    { chunk = systab->vol[vol]->map_chunks[i];	// check bitmap
      if (0 == chunk)				// skip if empty
      { block += 32;
	continue;
      }
      while (SemOp( SEM_GLOBAL, READ));		// take a READ lock
      for (j = 0; j < 32; j++)			// check every bit
      { if (chunk & 1)				// needs writing ?
        { file_off = (off_t) SIZEOF_LABEL_BLOCK + // calc. map block offset
		     block * MAP_CHUNK;
	  file_off = lseek( dbfds[vol], file_off, SEEK_SET); // move to block
          if (file_off < 0)
          { systab->vol[vol]->stats.diskerrors++; // count an error
            sprintf(msg, "do_daemon: lseek() to map block (%d) of vol %d failed", block, vol);
            panic(msg);
          }
          ret = write( dbfds[vol], 		// write the map chunk
		       systab->vol[vol]->map + block * MAP_CHUNK,
	               MAP_CHUNK);
  	  if (ret < 0)
          { systab->vol[vol]->stats.diskerrors++; // count an error
            sprintf(msg, "do_daemon: write() map block (%d) of vol %d failed", block, vol);
            panic(msg);
          }
	}
	block++; chunk >>= 1;
      }
      systab->vol[vol]->map_chunks[i] = 0;	// clear map chunk
      SemOp( SEM_GLOBAL, -curr_lock);		// release lock
    }
  }
/*
  file_off = lseek( dbfds[vol], 0, SEEK_SET);	// move to start of file
  if (file_off<0)
  { systab->vol[vol]->stats.diskerrors++;	// count an error
    sprintf(msg, "do_daemon: lseek() to start of vol %d failed", vol);
    panic(msg);
  }
  i = write( dbfds[vol], systab->vol[vol]->vollab,
	     systab->vol[vol]->vollab->header_bytes);// map/label
  if (i < 0)
  { systab->vol[vol]->stats.diskerrors++;	// count an error
    sprintf(msg, "do_daemon: write() map block of vol %d failed", vol);
    panic(msg);
  }
*/
  inter_add(&systab->vol[vol]->map_dirty_flag, -dirty_flag); // alter dirty flag
  ATOMIC_INCREMENT(systab->vol[vol]->stats.phywt); // count a write
  last_map_write[vol] = MTIME(0);
}
