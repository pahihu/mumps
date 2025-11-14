// File: mumps/init/init_run.c
//
// module MUMPS - startup (main) code

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
#include <signal.h>
#include <errno.h>                              // error stuf
#include <fcntl.h>                              // file stuff
#include <string.h>				// for bcopy()
#include <strings.h>
#include <unistd.h>                             // database access
#include <sys/ipc.h>                            // shared memory
#include <sys/shm.h>                            // shared memory
#include <sys/sem.h>                            // semaphores
#include <termios.h>				// for tcgetattr
#include "mumps.h"                              // standard includes
#include "proto.h"				// standard prototypes
#include "init.h"				// initialization prototypes
#include "error.h"				// standard errors
#include "compile.h"
#include "opcodes.h"
#include "database.h"

partab_struct partab;                           // setup partab
systab_struct *systab;
sem_stat semtab[2*SEM_MAX];

u_char *astk[MAX_ASTK];                         // address stack
u_char sstk[MAX_SSTK + MAX_STR_LEN];		// string stack + fence
u_char istk[MAX_ISTK];				// indirect stack
long isp;					// indirect stack pointer
int failed_tty = -1;				// flag for tty reset
int gbd_expired = GBD_EXPIRED;			// Set this

u_char *mumpspc;				// mumps prog pointer

void ser(short s)                               // display errors
{ cstring *cptr;                                // cstring ptr
  u_char junk[100];

  if (s == -(ERRMLAST + ERRZ27))		// if totally confused
  { mv1_panic("Chanel zero has gone away");	// die
  }
  cptr = (cstring *) junk;			// some space
  if (s < 0) s = -s;                            // make error positive
  (void)UTIL_strerror(s, &cptr->buf[0]);          // get the text
  fprintf( stderr, "\r\nERROR occured %d\r\n%s\r\n",
  	   s, &cptr->buf[0]);			// print it
  return;                                       // and return
}

void controlc()					// say ^C
{ cstring *sptr;				// string ptr
  u_char junk[10];
  short s;					// for returns
  sptr = (cstring *) junk;			// where to put it
  s = SQ_WriteFormat(SQ_LF);			// new line
  if (s < 0) ser(s);				// check for error
  bcopy("^C", sptr->buf, 2);			// copy in the prompt
  sptr->buf[3] = '\0';				// null terminate
  sptr->len = 2;				// and the length
  s = SQ_Write(sptr);				// write the prompt
  if (s < 0) ser(s);				// check for error
  return;					// exit
}


//****************************************************************************
// Attach to an environment - switches are:
//                            x = xecute command                Opt
//                            e = environment (UCI)             Opt
int INIT_Run( char *file,                       // database file
              char *env,                        // environment (UCI)
              char *cmd)                        // command

{ int i;                                        // an int
  int dbfd = 0;                                 // database file descriptor
  int ret = 0;					// return value
  int env_num = 1;				// startup environment number
  var_u tmp;					// temp descriptor
  uci_tab *uci_ptr;				// for uci search
  int pid;					// job number
  int ssp = 0;					// string stack ptr
  int asp = 0;					// address stack ptr
  mvar *var;					// a variable pointer
  cstring *cptr;				// a handy pointer
  cstring *sptr;                                // cstring ptr
  short s;					// for functions
  short start_type = TYPE_RUN;			// how we started
  gid_t gidset[MAX_GROUPS];			// for getgroups()
  struct termios tty_settings;			// man 4 termios
  char stderr_file[128];

start:
#if defined(__APPLE__) || defined(__FreeBSD__)
  //srandomdev();					// randomize
  srandom(MUMPS_MAGIC);
#endif

  bzero(semtab, sizeof(semtab));

  partab.jobtab = (jobtab_t *) NULL;		// clear jobtab pointer
  dbfd = OpenFile(file, O_RDONLY);              // open the database for read
  if (dbfd < 0) return (errno);                 // if that failed
  if (start_type == TYPE_RUN)			// if not from JOB
  { i = UTIL_Share(file);                       // attach to shared mem
    if (i != 0) return(i);                      // quit on error
  }
  if (env != NULL)				// passed in uci ?
  { env_num = 0;				// clear uci number
    uci_ptr = &systab->vol[0]->vollab->uci[0];	// get ptr to uci table
    //tmp.var_qu = 0;				// zero entire name
    X_Clear(tmp.var_xu);			// zero entire name
    for (i = 0; i < MAX_NAME_BYTES; i++)	// copy in name
    { if (env[i] == '\0') break;		// done at null
      tmp.var_cu[i] = env[i];			// copy character
    }
    for (i = 0; i < UCIS; i++)			// scan all ucis
     //if (uci_ptr[i].name.var_qu == tmp.var_qu)	// if we have a match
     if (X_EQ(uci_ptr[i].name.var_xu, tmp.var_xu))      // if we have a match
     { env_num = i + 1;				// save it
       break;					// and exit loop
     }
    if (env_num == 0)
    { ret = ENOENT;				// complain on fail
      goto exit;				// and exit
    }
  }

  pid = (int) getpid();				// get process id
  for (i = 0; i < systab->maxjob; i++)		// scan the slots
  { ret = systab->jobtab[i].pid;		// get pid
    if ((ret != pid) && (ret))			// if one there and not us
    { if (kill(ret, 0))				// check the job
      { if (errno == ESRCH)			// doesn't exist
        { ret = CleanJob(i + 1);		// zot if not there
          if (0 == ret)                         // success ?
	    break;				//   have at least one
        }
      }
    }
    else					// it's free or ours
    { break;					// quit
    }
  }

  ret = SemOp(SEM_SYS, -systab->maxjob);	// lock systab
  if (ret < 0) goto exit;			// give up on error
  for (i = 0; i < systab->maxjob; i++)		// look for a free slot
  { if (((systab->jobtab[i].pid == 0) &&	// this one ?
	 (start_type == TYPE_RUN))    ||
	((systab->jobtab[i].pid == pid) &&	// or already done (JOB)
	 (start_type == TYPE_JOB)))
    { bzero(&systab->jobtab[i], sizeof(jobtab_t)); // yes - zot the lot
      partab.jobtab = &systab->jobtab[i];	// and save our jobtab address
      partab.jobtab->pid = pid;			// copy in our pid
      break;					// end loop
    }
  }
  ret = SemOp(SEM_SYS, systab->maxjob);		// unlock systab
  if (partab.jobtab == NULL)			// if that failed
  { ret = ENOMEM;				// error message
    goto exit;					// and exit
  }

  partab.jobtab->user = getuid();	        // get user number

  if ((partab.jobtab->user == systab->start_user) || // if he started it
      (partab.jobtab->user == 0))		// or is root
  { partab.jobtab->priv = 1;			// say yes
  }
  else 
  { if (systab->maxjob == 1)			// if single job
    { ret = ENOMEM;				// error message
      partab.jobtab = NULL;			// clear this
      goto exit;				// and exit
    }

    i = getgroups(MAX_GROUPS, gidset);		// get groups
    if (i < 0)					// if an error
    { ret = errno;				// get the error
      goto exit;				// and exit
    }
    while (i > 0)				// for each group
    { if (gidset[i - 1] == PRVGRP)		// if it's "wheel" or "admin"
      { partab.jobtab->priv = 1;		// say yes
        break;					// and exit
      }
      i--;					// decrement i
    }
  }

  partab.jobtab->precision = systab->precision;	// decimal precision

  partab.jobtab->uci = env_num;			// uci number
  partab.jobtab->vol = 1;			// volset
  partab.jobtab->luci = env_num;		// uci number
  partab.jobtab->lvol = 1;			// volset
  partab.jobtab->ruci = env_num;		// uci number
  partab.jobtab->rvol = 1;			// volset

  partab.jobtab->start_len =
    Vhorolog(partab.jobtab->start_dh);		// store start date/time

  partab.jobtab->zjuDATA_len = 0;               // init ZJU
  partab.jobtab->zjuDATA[0] = '\0';

  partab.jobtab->dostk[0].type = TYPE_RUN;	// ensure slot 0 has a value

  failed_tty = tcgetattr ( 0, &tty_settings );
  i = SQ_Init();				// have seqio setup chan 0

  systab->vol[0]->last_blk_used[MV1_PID] = 0;   // clear last global block
  systab->vol[0]->last_blk_written[MV1_PID] = 0; // clear last global blk written

  partab.debug = 0;				// clear debug flag
  partab.sstk_start = &sstk[0];			// address of sstk
  partab.sstk_last =  &sstk[MAX_SSTK];		// and the last char
  partab.varlst = NULL;				// used by compiler

  for (i = 0; i < MAX_VOL; i++)			// clear DGP socks
  { partab.dgp_sock[i] = -1;
    partab.vol_fds[i] = 0;
    partab.jnl_fds[i] = 0;
  }
  for (i = 0; i < MAX_REPLICAS; i++)
  { partab.dgp_repl[i] = -1;
  }

  partab.vol_fds[0] = dbfd;			// make sure fd is right
  partab.lenseq = 0;				// no SEQUENCE
  partab.lastseq = 0;

  partab.compmsg = (cstring*)malloc(sizeof(cstring)); // compiler msg

  LB_Init();					// init local buffers

  ST_Init();					// initialize symbol table

  if ((systab->vol[0]->vollab->journal_available) &&
      (systab->vol[0]->vollab->journal_requested)) // if journaling
  { partab.jnl_fds[0] = OpenFile(systab->vol[0]->vollab->journal_file, O_RDWR);
    if (partab.jnl_fds[0] < 0)
    { fprintf(stderr, "Failed to open journal file %s\nerrno = %d\n",
		systab->vol[0]->vollab->journal_file, errno);
      ret = -1;
      if (cmd != NULL) goto exit;
    }
    else
    { partab.jnl_seq[0] = systab->vol[0]->jnl_seq;
    }
  }

  DBG(
  sprintf(stderr_file, "%d.stderr", getpid());
  (void)freopen(stderr_file, "w", stderr);	// redirect stderr
  );

  if (cmd != NULL)				// command specified ?
  {
    source_ptr = (u_char *) cmd;		// where the code is
    cptr = (cstring *) &sstk[ssp];		// where the compiled goes
    comp_ptr = cptr->buf;			// the data bit
    parse();
    *comp_ptr++ = CMQUIT;			// add the quit
    *comp_ptr++ = ENDLIN;			// JIC
    *comp_ptr++ = ENDLIN;			// JIC
    i = &comp_ptr[0] - &cptr->buf[0];		// get number of bytes
    cptr->len = i;				// save for ron
    ssp = ssp + i + sizeof(short) + 1;		// point past it
    mumpspc = &cptr->buf[0];			// setup the mumpspc
    partab.jobtab->dostk[0].routine = (u_char *) cmd; 	// where we started
    partab.jobtab->dostk[0].pc = mumpspc;	// where we started
    partab.jobtab->dostk[0].symbol = NULL;	// nowhere
    partab.jobtab->dostk[0].newtab = NULL;	// nowhere
    partab.jobtab->dostk[0].endlin = mumpspc + i - 4; // ENDLIN
    //partab.jobtab->dostk[0].rounam.var_qu = 0;// zero the routine name
    X_Clear(partab.jobtab->dostk[0].rounam.var_xu);	// zero the routine name
    partab.jobtab->dostk[0].vol = partab.jobtab->vol; // current volume
    partab.jobtab->dostk[0].uci = partab.jobtab->uci; // current uci
    partab.jobtab->dostk[0].line_num = 0;	// no line number
    partab.jobtab->dostk[0].type = start_type;	// how we started
    partab.jobtab->dostk[0].estack = 0;		// estack offset
    partab.jobtab->dostk[0].level = 0;		// where we started
    partab.jobtab->dostk[0].flags = 0;		// no flags
    partab.jobtab->dostk[0].savasp = asp;	// address stack ptr
    partab.jobtab->dostk[0].savssp = ssp;	// string stack
    partab.jobtab->dostk[0].asp = asp;		// address stack ptr
    partab.jobtab->dostk[0].ssp = ssp;		// string stack

    partab.jobtab->attention = 0;
    partab.jobtab->trap = 0;
    partab.jobtab->async_error = 0;
    isp = 0;					// clear indirect pointer
    s = run(asp, ssp);
    if (s == OPHALT) goto exit;			// look after halt
    if (s == JOBIT) goto jobit;			// look after JOB
    partab.jobtab->io = 0;			// force chan 0
    var = (mvar *) &sstk[0];			// space to setup a var
    X_set("$ECODE\0\0", &var->name.var_cu[0], 8);
    var->volset = 0;
    var->uci = UCI_IS_LOCALVAR;
    var->slen = 0;				// setup for $EC
    cptr = (cstring *) &sstk[sizeof(mvar)];	// for result
    bcopy("$ECODE=", cptr->buf, 7);
    s = ST_Get(var, &cptr->buf[7]);
    if (s > 1) 					// ignore if nothing there
    { cptr->len = s + 7;
      s = SQ_WriteFormat(SQ_LF);		// new line
      s = SQ_Write(cptr);			// write the prompt
      s = SQ_WriteFormat(SQ_LF);		// new line
      cptr = (cstring *) (((u_char *) cptr) + 8);
      if (cptr->buf[0] != 'U')
      { cptr->len = 4;				// max error size
        cptr->len = Xcall_errmsg((char *) cptr->buf, cptr, cptr); // cvt to str
        s = SQ_Write(cptr);			// write the error
        s = SQ_WriteFormat(SQ_LF);		// new line
      }
      ret = ESRCH;				// set an error for exit
    }
    goto exit;					// and halt
  }

  while (TRUE)					// forever
  { sptr = (cstring *) &sstk[0];		// front of string stack
    asp = 0;					// zot address stack
    ssp = 0;					// and the string stack
    bcopy("M> ", sptr->buf, 3);			// copy in the prompt
    sptr->buf[3] = '\0';			// null terminate
    sptr->len = 3;				// and the length
    partab.jobtab->io = 0;			// force chan 0
    if (partab.jobtab->seqio[0].dx)		// if not at right margin
    { s = SQ_WriteFormat(SQ_LF);		// new line
      if (s < 0) ser(s);			// check for error
    }
    s = SQ_Write(sptr);				// write the prompt
    if (s < 0) ser(s);				// check for error
    s = SQ_Read(sptr->buf, -1, -1);		// get a string
    i = attention();				// check signals
    if (i == OPHALT) break;			// exit on halt
    if (i == -(ERRZ51+ERRMLAST))		// control c
      controlc();				// say
    if (s < 0)
    { ser(s);					// complain on error
      s = 0;
    }
    sptr->len = s;				// save the length
    if (s == 0) continue;			// ignore null
    astk[asp++] = (u_char *) sptr;		// save address of string
    ssp = ssp + s + sizeof(short) + 1;		// point past it
    s = SQ_WriteFormat(SQ_LF);			// new line
    if (s < 0) ser(s);				// check for error
    source_ptr = sptr->buf;			// where the code is
    cptr = (cstring *) &sstk[ssp];		// where the compiled goes
    comp_ptr = cptr->buf;			// the data bit
    parse();
    *comp_ptr++ = CMQUIT;			// add the quit
    *comp_ptr++ = ENDLIN;			// JIC
    *comp_ptr++ = ENDLIN;			// JIC
    i = &comp_ptr[0] - &cptr->buf[0];		// get number of bytes
    cptr->len = i;				// save for ron
    ssp = ssp + i + sizeof(short) + 1;		// point past it

    mumpspc = &cptr->buf[0];			// setup the mumpspc
    partab.jobtab->dostk[0].routine = sptr->buf; // where we started
    partab.jobtab->dostk[0].pc = mumpspc;	// where we started
    partab.jobtab->dostk[0].symbol = NULL;	// nowhere
    partab.jobtab->dostk[0].newtab = NULL;	// nowhere
    partab.jobtab->dostk[0].endlin = mumpspc + i - 4; // ENDLIN
    //partab.jobtab->dostk[0].rounam.var_qu = 0;// zero the routine name
    X_Clear(partab.jobtab->dostk[0].rounam.var_xu);// zero the routine name
    partab.jobtab->dostk[0].vol = partab.jobtab->vol; // current volume
    partab.jobtab->dostk[0].uci = partab.jobtab->uci; // current uci
    partab.jobtab->dostk[0].line_num = 0;	// no line number
    partab.jobtab->dostk[0].type = TYPE_RUN;	// how we started
    partab.jobtab->dostk[0].estack = 0;		// estack offset
    partab.jobtab->dostk[0].level = 0;		// where we started
    partab.jobtab->dostk[0].flags = 0;		// no flags
    partab.jobtab->dostk[0].savasp = asp;	// address stack ptr
    partab.jobtab->dostk[0].savssp = ssp;	// string stack
    partab.jobtab->dostk[0].asp = asp;		// address stack ptr
    partab.jobtab->dostk[0].ssp = ssp;		// string stack

    partab.jobtab->attention = 0;
    partab.jobtab->trap = 0;
    partab.jobtab->async_error = 0;
    isp = 0;					// clear indirect pointer
    s = run(asp, ssp);
    if (s == JOBIT) goto jobit;			// look after JOB
    if (s == OPHALT) break;			// exit on halt
    partab.jobtab->io = 0;			// force chan 0
    if (s == -(ERRZ51+ERRMLAST))		// control c
      controlc();				// say
    else if (s < 0) ser(s);
    partab.jobtab->error_frame = 0;		// and that one
    var = (mvar *) &sstk[0];			// space to setup a var
    X_set("$ECODE\0\0", &var->name.var_cu[0], 8);
    var->volset = 0;
    var->uci = UCI_IS_LOCALVAR;
    var->slen = 0;				// setup for $EC
    cptr = (cstring *) &sstk[sizeof(mvar)];	// for result
    bcopy("$ECODE=", cptr->buf, 7);
    s = ST_Get(var, &cptr->buf[7]);
    if (s < 1) continue;			// ignore if nothing there
    cptr->len = s + 7;
    s = SQ_Write(cptr);				// write the prompt
    if (s < 0) ser(s);				// check for error
    s = SQ_WriteFormat(SQ_LF);			// new line
    if (s < 0) ser(s);				// check for error
    s = ST_Kill(var);				// dong $EC
    cptr = (cstring *) (((u_char *) cptr) + 8);
    if (cptr->buf[0] != 'U')
    { cptr->len = 4;				// max error size
      cptr->len = Xcall_errmsg((char *) cptr->buf, cptr, cptr); // cvt to str
      s = SQ_Write(cptr);			// write the error
      if (s < 0) ser(s);			// check for error
      s = SQ_WriteFormat(SQ_LF);		// new line
    }
  }						// end command level loop

exit:						// general exit code
  if (partab.jobtab != NULL)			// if we have a jobtab
    CleanJob(0);				// remove all locks etc
  i = shmdt(systab);                       // detach the shared mem
  if (dbfd)
    i = close(dbfd);                            // close the database
  if (!failed_tty)				// reset terminal if possible
  { failed_tty = tcsetattr ( 0, TCSANOW, &tty_settings );
  }
  if (start_type == TYPE_JOB) return 0;		// no error from JOB
  return ret;                                  	// and exit

jobit:						// code for JOB
  start_type = TYPE_JOB;			// what we are doing
  env_num = partab.jobtab->ruci;		// remember (current) rou uci
  cmd = (char *) &sstk[0];			// where the command is
  ssp = strlen((const char *) sstk);		// protect original command
  isp = 0;					// clear all these
  asp = 0;
  ret = 0;
  env = NULL;
  goto start;					// go do it
}
