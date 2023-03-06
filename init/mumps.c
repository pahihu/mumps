// File: mumps/init/mumps.c
//
// module MUMPS - startup (main) code

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


#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include <string.h>
#include <ctype.h>
#include <unistd.h>				// for getopt
#include <errno.h>                              // error stuf
#include "mumps.h"                              // standard includes
#include "proto.h"                              // function prototypes
#include "init.h"                               // init prototypes

//****************************************************************************
// Give help if they entered -h or similar
//
void help(void)                                 // give some help
{ char str[80];                                 // a string
  (void)mumps_version((u_char *)str);           // get version into str[]
  printf( "----------------------------------------------------\n");
  printf( "This is %s\n\n", str);               // print version string
  printf( "Copyright (c) 1999 - 2016 Raymond Douglas Newman.\n");
  printf( "Copyright (c) 2016 - 2023 Andras Pahi.\n");
  printf( "All rights reserved.\n\n");
  printf( "To create a database:\n");
  printf( "> mumps -v volnam -b blocksize(kb) -s dbsize(Blocks) filename\n");
  printf( "                   and optionally -m mapblocksize(kb)\n");
  printf( "  volnam is 1 to %d alpha characters\n\n",MAX_NAME_BYTES);
  printf( "To initialize an environment:\n");
  printf( "> mumps -j maxjobs  -r routinemb\n");
  printf( "        -g globalmb -a addmb -l [-]jrnkb -i sysid\n");
  printf( "        -n #netdaemons -u srvurl -p srvport -t srvsndto database\n");
  printf( "  routinemb, globalmb, addmb, jrnkb, sysid,\n");
  printf( "  #netdaemons, srvurl, srvport, srvsndto are optional\n\n");
  printf( "To attach to an environment:\n");
  printf( "> mumps -x command -e environment(uci) database\n" );
  printf( "               where both switches are optional\n\n");
  printf( "      see http://github.com/pahihu/mumps/tree/development\n");
  printf( "----------------------------------------------------\n");
  exit (0);                                     // give help and exit
}

//****************************************************************************
// Main entry for create, init and run

int main(int argc,char **argv)                  // main entry point
{ int c;                                        // for case
  int bsize = 0;                                // block size
  char *env = NULL;                             // start environment name
  int gmb = 0;                                  // global buf MB
  int jobs = 0;                                 // max jobs
  int map = 0;                                  // map/header block bytes
  int rmb = 0;                                  // routine buf MB
  int addmb = 0;                                // additional buffer in MB
  int blocks = 0;                               // number of data blocks
  char *volnam = NULL;                          // volume name
  char *cmd = NULL;                             // startup command
  char *cmd1 = "D ^%1MV1LGI\0";                 // cmd for one
  char *db1 = "/one/onedb\0";                   // db for one
  int jrnkb = 0;                                // jrn buf KB
  char srvurl[128];				// DGP server URL
  int srvport = -1;				// base DGP server port
  int srvsndto = -1;                            // DGP server send timeout
  int sysid = 0;				// DGP system ID (random)
  int netdaemons = 0;				// no network daemons

  strcpy(srvurl, "");

  if (argc == 1)
  { if (strcmp( argv[0], "one\0" ) == 0 )       // allow for a name of 'one'
    { cmd = cmd1;                               // use this command
      argv[0] = db1;                            // and this as a database
      goto runit;                               // and go do it
    }
  }
  if (argc < 2) help();                         // they need help
  while ((c = getopt(argc, argv, "a:b:e:g:hi:j:l:m:n:p:r:s:t:u:v:x:")) != EOF)
  { switch(c)
    { case 'a':                                 // switch -a
        addmb = atoi(optarg);                   // additional buffer
        break;
      case 'b':                                 // switch -b
        bsize = atoi(optarg);                   // database block size  (creDB)
        break;
      case 'e':                                 // switch -e
        env = optarg;                           // environment name     (run)
        break;
      case 'g':                                 // switch -g
        gmb = atoi(optarg);                     // global buffer MB     (init)
        break;
      case 'h':                                 // switch -h
        help();                                 // exit via help()
        break;
      case 'i':					// switch -i
	sysid = atoi(optarg);			// system ID		(init)
	break;
      case 'j':                                 // switch -j
        jobs = atoi(optarg);                    // max number of jobs   (init)
        break;
      case 'l':                                 // switch -l
        jrnkb = atoi(optarg);                   // jrn buffer KB        (init)
        break;
      case 'm':                                 // switch -m
        map = atoi(optarg);                     // size of map block    (creDB)
        break;
      case 'n':					// switch -n            (init)
        netdaemons = atoi(optarg);		// number of network daemons
        break;
      case 'p':					// switch -p		(init)
	srvport = atoi(optarg);			// server base port
	break;
      case 'r':                                 // switch -r
        rmb = atoi(optarg);                     // routine buffer MB    (init)
        break;
      case 's':                                 // switch -s
        blocks = atoi(optarg);                  // number of data blks  (creDB)
        break;
      case 't':                                 // switch -t
        srvsndto = atoi(optarg);                // server send timeout  (init)
        break;
      case 'u':					// switch -u		(init)
        strncpy(srvurl, optarg, 127);		// server URL
	srvurl[127] = '\0';
	break;
      case 'v':                                 // switch -v
        volnam = optarg;                        // volume name          (creDB)
        break;
      case 'x':                                 // switch -x
        cmd = optarg;                           // initial command      (run)
        break;
      default:                                  // some sort of error
        help();                                 // just give help
        break;
    }
  }
  argc -= optind;                               // adjust for used args
  argv += optind;                               // should point at parameter
  if (argc != 1) help();                        // must have database name

  runtime_math_init();                          // initialize MAPM lib

  if (volnam != NULL) exit(                     // do a create
          INIT_Create_File( blocks,             // number of blocks
                            bsize*1024,         // block size in bytes
                            map*1024,           // map size in bytes
                            volnam,             // volume name
                            *argv));            // file name

  if (jobs > 0) exit(                           // do an init
          INIT_Start( *argv,                    // database
                      jobs,                     // number of jobs
                      gmb,                      // mb of global buf
                      rmb,                      // mb of routine buf
                      addmb,                    // mb of additional buf
                      jrnkb,			// kb of jrn buf
                      netdaemons,		// # of network daemons
		      sysid,			// system ID
                      srvurl,			// server URL
		      srvport,                  // server port
                      srvsndto));    		// server send timeout

runit:
  c = INIT_Run( *argv, env, cmd);               // run a job
  if (c != 0) fprintf( stderr,
                       "Error occured in process - %s\n", // complain
                       strerror(c));            // what was returned
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__linux__)
	if (c == ENOENT)
		fprintf( stderr, "\tMumps database not loaded\n");
	else if (c == ENOMEM)
		fprintf( stderr, "\tMumps job table is full\n");
#endif
  exit (c);                                     // exit with value
}
