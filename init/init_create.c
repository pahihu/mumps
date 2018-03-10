// File: mumps/init/init_create.c
//
// module init - create a database file

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
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>                              // error stuf
#include <fcntl.h>                              // file stuff
#include <unistd.h>                             // database access
#include <sys/types.h>
#include <sys/ipc.h>                            // shared memory
#include <sys/shm.h>                            // shared memory
#include "mumps.h"                              // standard includes
#include "database.h"				// for init MGR block
#include "proto.h"				// for X_set

//****************************************************************************
// Create a database - switches are:
//      -v volset name          (1 to MAX_NAME_BYTES chars)             Reqd
//      -b blocksize in kb      (4 to 256)                              Reqd
//      -s db size in blocks    (100 to MAX_DATABASE_BLKS)      	Reqd
//      -m map block size in kb (dbsize/8192+1 to MAX_MAP_BYTES)        Opt
//

int INIT_Create_File(int blocks,                // number of blocks
                     int bsize,                 // block size in bytes
                     int map,                   // map size in bytes may be 0
                     char *volnam,              // volume name
                     char *file)                // file name
{ int namlen;                                   // length of volume name
  int i;                                        // for loops
  union temp_tag
  { int *buff;      				// map kbytes buffer
    char *cuff;         			// remap front bit
  } x;                                         	// end of union stuff
  int ret;                                      // for return values
  int fid;                                      // file handle
  DB_Block *mgrblk;				// mgr block ptr 
  label_block *labelblock;			// first LABEL_BLOCK_SIZE bytes
  cstring * chunk;

  namlen = strlen(volnam);                      // get the name length
  if ((namlen < 1)||(namlen > MAX_NAME_BYTES))  // check name length
  { fprintf(stderr, "Volume set name must from 1 to %d alpha characters\n\n",
                  MAX_NAME_BYTES);
    return (-1);                                // return an error
  }                                             // end name length check
  for (i = 0; i < namlen; i++)                  // check all chars in name
  { if (!isalpha((int)volnam[i]))               // must be alpha
    { fprintf(stderr, "Volume set name must from 1 to %d alpha characters\n\n",
                    MAX_NAME_BYTES);
    return (-1);                                // return an error
    }                                           // end fail code
  }                                             // end alpha check

  if (((bsize/1024) < 4)||((bsize/1024) > 256)) // check block size
  { fprintf(stderr, "Blocksize must be from 4 to 256KB\n\n" ); // complain
    return (-1);                                // return an error
  }                                             // end block size check

  blocks |= 7;					// ensure low 3 bits are set

  if ((blocks < 100)||
      (blocks > MAX_DATABASE_BLKS))		// check db size
  { fprintf(stderr,
            "Database size must be 100 to %d blocks\n\n",
            MAX_DATABASE_BLKS);       		// complain
    return (-1);                                // return an error
  }                                             // end db size check

  if (map == 0)                                 // if map not sepecified
  { map = (blocks+7)/8+SIZEOF_LABEL_BLOCK+1;	// see what we need
    if (map & 1023)				// if not even K
    { map = ((map / 1024) + 1) * 1024;		// round up
    }
    if (map < bsize) map = bsize;               // at least bsize
  }
  if ((map < (blocks+7)/8+SIZEOF_LABEL_BLOCK+1)||  // if less than reqd
      (map < bsize)||                           // or less than bsize
      (map > MAX_MAP_BYTES))                    // or too big
  { fprintf(stderr, "Invalid map block size %d\n\n", map); // complain
    return (-1);                                // return an error
  }                                             // end map size check
  printf( "Creating volumeset %s in file %s\n", volnam, file);
  printf( "with %d x %dkb blocks ", blocks, bsize/1024);
  printf( "and a %dkb map/label block.\n", map/1024); // say what we are doing

  ret = 0;
  errno = 0;                                    // clear error flag
  fid = open(file,                              // open this new file
             O_CREAT|O_TRUNC|O_WRONLY|O_EXCL,   // create the file
             438);                              // rw everyone (for now)
  if (fid < 1)                                  // if that failed
  { fprintf( stderr, "Create of %s failed\n - %s\n", // complain
             file,                              // what we tried
             strerror(errno));                  // what was returned
    return(errno);                              // exit with error
  }                                             // end file create test
  x.buff = (int *) malloc(map * 1024);		// allocate map kbytes
  if (x.buff == NULL)
  { fprintf(stderr, "Cannot allocate map bytes\n - %s\n", // complain
	      strerror(errno));			// what was returned
    return(errno);				// exit with error
  }
  x.cuff = (char *)x.buff;			// point to buff[]
  labelblock = (label_block *) x.buff;		// point structure at it
  bzero(x.buff, map);				// clear it
  labelblock->magic = MUMPS_MAGIC;		// mumps magic number
  labelblock->max_block = blocks;		// maximum block number
  labelblock->header_bytes = map;		// bytes in label/map
  labelblock->block_size = bsize;		// bytes per data block
  strncpy((char *) labelblock->volnam.var_cu,   // vol name
        volnam, MAX_NAME_BYTES);                //  (MAX_NAME_BYTES bytes)
  labelblock->db_ver = DB_VER;			// database version
  labelblock->clean = 1;			// clean dismount flag

  bcopy("MGR", labelblock->uci[0].name.var_cu, 3);
  labelblock->uci[0].global = 1;		// setup MGR UCI

  x.cuff[SIZEOF_LABEL_BLOCK] = 3;               // mark blocks 0 & 1 as used

  ret = write(fid, x.buff, map);                // write out the header
  if (ret < 1)                                  // if that failed
  { i = close(fid);                             // close the file
    fprintf( stderr, "File write failed - %s\n", // complain
             strerror(errno));                  // what was returned
    free(x.buff);				// release buff[]
    return(errno);                              // and return
  }                                             // probably should delete it
  // make MGR block & $GLOBAL record

  mgrblk = (DB_Block *) x.buff;			// find block 1 (mgr)
  bzero(x.buff, bsize);				// clear it
  mgrblk->type = 65;				// type is data blk + uci
  mgrblk->last_idx = LOW_INDEX;			// have one rec
  mgrblk->last_free = (u_short) (bsize/4) - 7;	// minus extra 6 for rec length
  X_set( "$GLOBAL\0", &(mgrblk->global), 8);	// init the name
  i = (bsize / 4) - 6;				// point at the record
  *(u_short *) &x.cuff[BLK_HDR_SIZE] = (u_short) i;  // save in index
  chunk = (cstring *) &x.buff[i];		// point at the chunk
  chunk->len = 24;				// size of this (incl self)
  chunk->buf[1] = 9;				// key length
  bcopy("\200$GLOBAL\0",&chunk->buf[2], 9);	// the key
  i += 4;					// point at block#
  x.buff[i] = 1;				// block 1
  ret = write(fid, x.buff, bsize);              // write MGR block

  // Now do the rest as zeroed blocks

  bzero(x.buff, bsize);				// clear it
  for (i = 0; i < blocks-1; i++)                  // for each data block
  { ret = write(fid, x.buff, bsize);            // write a block
    if (ret < 1)                                // if that failed
    { i = close(fid);                           // close the file
    fprintf( stderr, "File write failed - %s\n", // complain
             strerror(errno));                  // what was returned
    free(x.buff);				// release buff[]
    return(errno);                              // and return
    }                                           // probably should delete it
  }                                             // end of write code
  i = close(fid);                               // close file
  printf( "Database file created.\n");          // say we've done that
  free(x.buff);					// release buff[]
  return (0);                                   // indicate success
}
