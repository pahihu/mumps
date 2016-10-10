// VolumeSet I/O function
// Copyright (c) 2016 Andras Pahi
//
#include <errno.h>                              // for errno
#include <fcntl.h>                              // for open()
#include <unistd.h>                             // for close(), lseek() etc.
#include <sys/types.h>
#include <string.h>                             // for str...()
#include <strings.h>                            // for bzero()
#include <stdarg.h>                             // for va_ macros

#include "vsio.h"


#define VOLSET_MAGIC    1915778837UL            // MAGIC of VOLSET

typedef struct _VSHDR                           // VS HEADER struct def
{ u_int magic;                                  //   magic number
  u_int nvols;                                  //   no. of volumes
  struct
  { off_t size;                                 //   vol. size, 0 on last vol.
    char path[MAX_VOLPATH+1];                   //   vol. path
  } vols[8];
} VSHDR;

#define VSHDR_SIZE      4096                    // HEADER block stored in 4K


int Offset2FD(VSFD *vsfd, off_t off)            // offset to volume index
{ int i;

  for (i = vsfd->nvols; i; i--)
  { if (off >= vsfd->vols[i - 1].start)
      return i - 1;
  }
  return 0;
}


int vs_open(VSFD *vsfd, const char *path, int flags, ...)
{ u_char buf[VSHDR_SIZE];                       // HEADER buffer
  VSHDR *hdr;
  ssize_t size;
  int fd, i, ret;
  int mode;
  va_list ap;

  bzero(vsfd, sizeof(VSFD));                    // clear descriptor
  if (strlen(path) > MAX_VOLPATH)               // max. 127 char path
  { errno = EINVAL;                             //   complain
    return -1;
  }

  ret = 0;                                      // default return value
  if (flags & O_CREAT)                          // create ?
  { va_start(ap, flags);                        // get mode parameter
    mode = va_arg(ap, int);
    va_end(ap);
    fd = open(path, flags, mode);               // open primary volume
    if (fd < 0)                                 // complain on error
      return fd;
    bzero(buf, VSHDR_SIZE);                     // clear HEADER buffer
    hdr = (VSHDR*) &buf[0];                     // setup hdr
    hdr->magic = VOLSET_MAGIC;                  //   set magic
    hdr->nvols = 1;                             //   1 volume
    strcpy((char *) &hdr->vols[0].path[0], path); // set name
    hdr->vols[0].size = 0;                      //   zero size
    size = write(fd, buf, VSHDR_SIZE);          // write header
    if (size != VSHDR_SIZE)                     // check size written
    { close(fd);                                // complain on error
      return size;
    }
    close(fd);                                  // close primary
    flags &= ~(O_CREAT | O_TRUNC);              // clear creation flags
  }

  fd = open(path, flags);                       // open primary
  if (fd < 0)                                   // complain
    return fd;                                  //   on error
  size = read(fd, &buf, VSHDR_SIZE);            // read HEADER
  if (size != VSHDR_SIZE)                       // complain
  { ret = size;                                 //   on error
    goto ErrOut;
  }
  hdr = (VSHDR*) &buf[0];                       // check HEADER
  if (VOLSET_MAGIC != hdr->magic)               // check magic
  { errno = EINVAL;
    ret = -2;
    goto ErrOut;
  }
  if ((hdr->nvols < 1) || (hdr->nvols > 8))     // check no. of volumes
  { errno = EINVAL;
    ret = -3;
    goto ErrOut;
  }
  if (strcmp((char *) &hdr->vols[0].path[0], path)) // check primary path
  { errno = EINVAL;
    ret = -4;
    goto ErrOut;
  }

  vsfd->nvols = hdr->nvols;                     // have nvols
  vsfd->vols[0].fd    = fd;                     // the 1st is the primary
  vsfd->vols[0].start = 0;                      // begins at offset 0
  vsfd->vols[0].size  = hdr->vols[0].size;      // read size from HEADER
  strcpy(&vsfd->vols[0].path[0], &hdr->vols[0].path[0]);
  vsfd->pos = VSHDR_SIZE;                       // we are at HEADER end
  for (i = 1; i < hdr->nvols; i++)              // for each volume
  { vsfd->vols[i].fd = open(hdr->vols[i].path, flags); //  open volume
    if (vsfd->vols[i].fd < 0)                   // complain on error
    { ret = vsfd->vols[i].fd;
      goto ErrOut;
    }
    vsfd->vols[i].start = vsfd->vols[i-1].start + // adjust vols[].start
                          hdr->vols[i].size;
    strcpy(&vsfd->vols[i].path[0], &hdr->vols[i].path[0]); // set path
    vsfd->vols[i].size = hdr->vols[i].size;     // set size
  }
  return ret;
ErrOut:                                         // error handling
  for (i = 1; i < vsfd->nvols; i++)             // close open volumes
  { if (vsfd->vols[i].fd <= 0) continue;
    close(vsfd->vols[i].fd);
  }
  if (fd) close(fd);                            // close primary
  return ret;                                   // return error code
}


off_t vs_lseek(VSFD *vsfd, off_t offset, int whence)
{ int x;
  off_t ret;

  if (SEEK_SET != whence)                       // only SEEK_SET supported
  { errno = EINVAL;
    return -1;
  }

  offset += VSHDR_SIZE;                         // adjust offset w/ HEADER size
  x = Offset2FD(vsfd, offset);                  // get volume index
  ret = lseek(vsfd->vols[x].fd, offset - vsfd->vols[x].start, SEEK_SET); // seek
  if (ret >= 0)                                 // if OK
  { vsfd->pos = vsfd->vols[x].start + ret;      //   adjust pos
  }
  return (ret < 0) ? ret : vsfd->pos - VSHDR_SIZE;                      
}


ssize_t vs_read(VSFD *vsfd, void *buf, size_t count)
{ int x;
  ssize_t ret;

  x = Offset2FD(vsfd, vsfd->pos);               // get volume index
  ret = read(vsfd->vols[x].fd, buf, count);     // read()
  if (ret >= 0)                                 // if OK
    vsfd->pos += ret;                           //   adjust pos
  return ret;
}


ssize_t vs_write(VSFD *vsfd, const void *buf, size_t count)
{ int x;
  ssize_t ret;

  x = Offset2FD(vsfd, vsfd->pos);               // get volume index
  ret = write(vsfd->vols[x].fd, buf, count);    // write()
  if (ret >= 0)                                 // if OK
    vsfd->pos += ret;                           //   adjust pos
  return ret;
}


int vs_close(VSFD *vsfd)
{ int i, s, ret;
  int sav_errno;

  ret = 0;                                      // default return value
  for (i = 0; i < vsfd->nvols; i++)             // for each volume
  { s = close(vsfd->vols[i].fd);                // close volume
    if (s < 0)                                  // if error, 
    { ret = s;                                  //   save error code
      sav_errno = errno;                        //     and errno
    }
  } 
  if (ret)                                      // there was an error
    errno = sav_errno;                          //   restore errno
  return ret;
}


int vs_voladd(VSFD *vsfd, const char *path)
{ u_char buf[VSHDR_SIZE];                       // HEADER buffer
  VSHDR *hdr;
  ssize_t size;
  off_t pos, volsize;
  int fd, ret;
  u_int nvols;

  fd = 0;                                       // empty filedesc
  nvols = vsfd->nvols;                          // set nvols
  if ((8 == nvols) ||                           // max. 8 volumes and
      (strlen(path) > MAX_VOLPATH))             //   max. 127 char path
  { errno = EINVAL;                             //   complain
    return -1;
  }

  ret = 0;                                      // default return value
  pos = lseek(vsfd->vols[0].fd, 0, SEEK_SET);   // read HEADER
  if (pos != 0)                                 //   failed to position
  { ret = pos;
    goto ErrOut;
  }
  size = read(vsfd->vols[0].fd, buf, VSHDR_SIZE);
  if (size != VSHDR_SIZE)                       //   failed to read
  { ret = size;
    goto ErrOut;
  }

  volsize = lseek(vsfd->vols[nvols - 1].fd, 0, SEEK_END); // last volume size
  if (volsize < 0)                              // complain if error
  { ret = volsize;
    goto ErrOut;
  }

  fd = open(path, O_CREAT | O_TRUNC | O_RDWR, 438); // open new volume
  if (fd < 0)
  { ret = fd;
    goto ErrOut;
  }

  hdr = (VSHDR *) &buf[0];                      // setup VSHDR pointer
  hdr->vols[nvols - 1].size = volsize;          //   set size of last volume
  strcpy(&hdr->vols[nvols].path[0], path);      //   set volume path
  hdr->vols[nvols].size = 0;                    //   clear size
  hdr->nvols++;                                 //   incr. #volumes

  pos = lseek(vsfd->vols[0].fd, 0, SEEK_SET);   // go back to HEADER
  if (pos != 0)                                 //   complain if failed
  { ret = pos;
    goto ErrOut;
  }
  size = write(vsfd->vols[0].fd, buf, VSHDR_SIZE); // write back HEADER
  if (size != VSHDR_SIZE)                       //  complain if failed
  { ret = size;
    goto ErrOut;
  }
                                                // update in-memory VSFD
  vsfd->vols[nvols - 1].size = volsize;         //    set prev. size
  strcpy(&vsfd->vols[nvols].path[0], path);     //    set path
  vsfd->vols[nvols].fd = fd;                    //    set fd
  vsfd->vols[nvols].start =                     //    set start
                vsfd->vols[nvols - 1].start + volsize;
  vsfd->vols[nvols].size = 0;                   //    open size
  vsfd->nvols++;                                // incr. #volumes

ErrOut:
  if ((ret) && (fd)) close(fd);                 // close fd on error
  vs_lseek(vsfd, vsfd->pos, SEEK_SET);          // reposition VSFD
  return ret;                                   // return status 
}


int vs_volcount(VSFD *vsfd)
{ return vsfd->nvols;                           // return #volumes
}


off_t vs_volsize(VSFD *vsfd, int vol)
{
  if ((vol < 0) ||                              // check volume index
      (vol > vsfd->nvols - 1))
  { errno = EINVAL;
    return -1;
  }
  return vsfd->vols[vol].size;                  // return size
}


int vs_volpath(VSFD *vsfd, int vol, char *path_out)
{
  if ((vol < 0) ||                              // check volume index
      (vol > vsfd->nvols - 1))
  { errno = EINVAL;
    return -1;
  }
  strcpy(path_out, &vsfd->vols[vol].path[0]);   // copy volume path
  return 0;
}


