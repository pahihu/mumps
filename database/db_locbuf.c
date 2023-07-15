#include <stdio.h>
#include <stdlib.h>				// for malloc()
#include <strings.h>                            // for bcopy()
#include <sys/types.h>				// for types
#include "mumps.h"
#include "database.h"
#include "error.h"
#include "proto.h"


void LB_CleanUp(void)
{ if (partab.gbd_local)
    free(partab.gbd_local);
  if (partab.gbd_mem)
    free(partab.gbd_mem);
  partab.gbd_local = NULL;
  partab.gbd_mem   = NULL;
}


void LB_Init(void)
{ int i;

  partab.gbd_mem   = NULL;
  partab.gbd_local = NULL;

  partab.gbd_mem = 				// allocate memory
	(u_char *) malloc(MAX_LOCBUF * MAX_LOCBLK);
  if (0 == partab.gbd_mem)
    goto ErrOut;
  partab.gbd_local = 
	(struct GBD*) malloc(MAX_LOCBUF * sizeof(struct GBD));
  if (0 == partab.gbd_local)
    goto ErrOut;

  partab.gbd_nlocal = 0;			// no local buffers
  for (i = 0; i < MAX_LOCBUF; i++)		// init GBD structs
  { partab.gbd_local[i].block = 0;
    partab.gbd_local[i].mem =
	(struct DB_BLOCK *) (partab.gbd_mem + i * MAX_LOCBLK);
  }
  gbd_local_state = LB_DISABLED;
  return;

ErrOut:
  LB_CleanUp();
}


void LB_Clear(void)
{ partab.gbd_nlocal = 0;
}


gbd* LB_GetBlock(u_int blknum)
{ usec_t now;
  gbd *ptr;
  int i;
  u_char vol;

  if (0 == partab.gbd_local)			// no local buffer?
    return 0;					//   just return

  // fprintf(stderr,"LB_GetBlock(%u)\r\n",blknum); fflush(stderr);
  ptr = 0;
  vol = volnum - 1;
  now = UTIL_GetMicroSec() >> 10;		// ~ millisec timestamp
  for (i = 0; i < partab.gbd_nlocal; i++)	// check each local buffer
  { ptr = &partab.gbd_local[i];			// point to local GBD
    if ((vol == ptr->vol) &&			// volumes match?
        (blknum == ptr->block))			//   AND block numbers match?
    { if (now < ptr->last_accessed)		// not expired yet?
      { return ptr;				//   return it
      }
      break;					// expired, finished
    }
  }
  LB_Clear();					// clear local buffers
  return NULL;					// return
}


void LB_AddBlock(gbd *ptr)
{ usec_t now;					// timestamp
  int i;					// handy int
  int block_size;				// block size

  if (0 == partab.gbd_local)			// no local buffer?
    return;					//   just return

  block_size = systab->vol[volnum-1]->vollab->block_size;
  if (block_size > MAX_LOCBLK)			// if block size is over
    return;					//   don't add

  i = partab.gbd_nlocal;
  if (i == MAX_LOCBUF)				// if local buffer is full
    return;					//   just return

  // fprintf(stderr,"LB_AddBlock(%u)\r\n",ptr->block); fflush(stderr);

  now = UTIL_GetMicroSec() >> 10;		// ~ millisec timestamp
  partab.gbd_local[i].block = ptr->block;	// copy block no.
  bcopy(ptr->mem, partab.gbd_local[i].mem, block_size); // memory
  partab.gbd_local[i].dirty = 0;		// it is clean
  partab.gbd_local[i].last_accessed =		// set up expiration time
	now + systab->locbufTO;
  partab.gbd_local[i].refd = 1;			// it is referenced
  partab.gbd_local[i].vol = ptr->vol;		// set vol[] index
  partab.gbd_nlocal++;				// incr. local buffer no.
}

#define LB_FAILED (-(ERRZ94 + ERRMLAST))
#define LB_WRAP(fn) \
{ short s; \
\
  gbd_local_state = systab->locbufTO ? LB_ENABLED : LB_DISABLED; \
Restart: \
  s = fn; \
  if (LB_FAILED == s) \
  { gbd_local_state = LB_FILL; \
    /* systab->vol[volnum-1]->stats.eventcnt++; */ \
    goto Restart; \
  } \
  gbd_local_state = LB_DISABLED; \
  return s; \
}

short LDB_Get(mvar *var, u_char *buf)
LB_WRAP(DB_Get(var,buf))

short LDB_GetEx(mvar *var, u_char *buf, int wrlock, int old_stat)
LB_WRAP(DB_GetEx(var,buf,wrlock,old_stat))

short LDB_Data(mvar *var, u_char *buf)
LB_WRAP(DB_Data(var,buf))

short LDB_DataEx(mvar *var, u_char *buf, cstring *dat)
LB_WRAP(DB_DataEx(var,buf,dat))

short LDB_Order(mvar *var, u_char *buf, int dir)
LB_WRAP(DB_Order(var,buf,dir))

short LDB_OrderEx(mvar *var, u_char *buf, int dir, cstring *dat)
LB_WRAP(DB_OrderEx(var,buf,dir,dat))

short LDB_Query(mvar *var, u_char *buf, int dir, int docvt)
LB_WRAP(DB_Query(var,buf,dir,docvt))

short LDB_QueryEx(mvar *var, u_char *buf, int dir, int docvt, cstring *dat)
LB_WRAP(DB_QueryEx(var,buf,dir,docvt,dat))

