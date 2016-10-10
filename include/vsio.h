// VolumeSet I/O functions
// Copyright (c) 2016 Andras Pahi
//
#ifndef _VSIO_H
#define _VSIO_H

#include <unistd.h>

#define MAX_VOLPATH     127

typedef struct _VSFD            // VolumeSet file descriptor
{ int   nvols;                  // #volumes
  struct                        // for each volume
  { int   fd;                   //   fd
    off_t start;                //   start
    off_t size;                 //   size
    char  path[MAX_VOLPATH+1];  //   path
  } vols[8];
  off_t pos;                    // current VolumeSet position
} VSFD;

int     vs_open(VSFD *vsfd, const char *path, int flags, ...);
off_t   vs_lseek(VSFD *vsfd, off_t offset, int whence);
ssize_t vs_read(VSFD *vsfd, void *buf, size_t count);
ssize_t vs_write(VSFD *vsfd, const void *buf, size_t count);
int     vs_close(VSFD *vsfd);

int     vs_voladd(VSFD *vsfd, const char *path);

int     vs_volcount(VSFD *vsfd);
off_t   vs_volsize(VSFD *vsfd, int vol);
int     vs_volpath(VSFD *vsfd, int vol, char *path_out);

#endif
