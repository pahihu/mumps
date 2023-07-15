#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

typedef unsigned char u_char;

/* generic RLE */
void rleEncode(const char *inPath, const char *outPath)
{ FILE *fin = NULL, *fout = NULL;
  unsigned char ibuf[256], obuf[4];
  unsigned char prev;
  int cnt, i, n;

  fin = fopen(inPath, "rb");
  fout = fopen(outPath, "wb");

  prev = fgetc(fin); cnt = 1;
  while (!feof(fin)) {
    n = fread(ibuf, sizeof(unsigned char), sizeof(ibuf), fin);
    for (i = 0; i < n; i++) {
      if (ibuf[i] == prev) {
        if (256 == cnt) {
          obuf[0] = cnt-1; obuf[1] = prev;
          fwrite(obuf, sizeof(unsigned char), 2, fout);
          cnt = 0;
        }
        cnt++;
      } else {
        obuf[0] = cnt-1; obuf[1] = prev;
        fwrite(obuf, sizeof(unsigned char), 2, fout);
        prev = ibuf[i]; cnt = 1;
      }
    }
  }
  if (cnt) {
    obuf[0] = cnt-1; obuf[1] = prev;
    fwrite(obuf, sizeof(unsigned char), 2, fout);
  }

  fclose(fin);
  fclose(fout);
}

void rleDecode(const char *inPath, const char *outPath)
{ FILE *fin = NULL, *fout = NULL;
  unsigned char prev;
  int cnt, i;

  fin = fopen(inPath, "rb");
  fout = fopen(outPath, "wb");

  while (!feof(fin)) {
    cnt = fgetc(fin) + 1; prev = fgetc(fin);
    for (i = 0; i < cnt; i++) fputc(prev, fout);
  }

  fclose(fin);
  fclose(fout);
}

/* escaped RLE */
void writeRL(FILE *fout, int cnt, unsigned char ch)
{ unsigned char obuf[4];
  int n;

  n = 3;
  if (1 == cnt) {
    obuf[0] = ch; n = 1;
  } else {
    obuf[0] = ch; obuf[1] = ch; obuf[2] = cnt - 1;
  }
  fwrite(obuf, sizeof(unsigned char), n, fout);
}

void rleEncode2(const char *inPath, const char *outPath)
{ FILE *fin = NULL, *fout = NULL;
  unsigned char ibuf[256];
  unsigned char prev;
  int cnt, i, n;

  fin = fopen(inPath, "rb");
  fout = fopen(outPath, "wb");

  prev = fgetc(fin); cnt = 1;
  while (!feof(fin)) {
    n = fread(ibuf, sizeof(unsigned char), sizeof(ibuf), fin);
    for (i = 0; i < n; i++) {
      if (ibuf[i] == prev) {
        if (256 == cnt) {
          writeRL(fout, cnt, prev);
          cnt = 0;
        }
        cnt++;
      } else {
        writeRL(fout, cnt, prev);
        prev = ibuf[i]; cnt = 1;
      }
    }
  }
  if (cnt) {
    writeRL(fout, cnt, prev);
  }

  fclose(fin);
  fclose(fout);
}

void rleDecode2(const char *inPath, const char *outPath)
{ FILE *fin = NULL, *fout = NULL;
  int prev, cnt, i, filled;

  fin = fopen(inPath, "rb");
  fout = fopen(outPath, "wb");

  prev = fgetc(fin); filled = 1;
  while (!feof(fin)) {
    cnt = fgetc(fin);
    if (prev == cnt) {
      cnt = fgetc(fin) + 1;
      for (i = 0; i < cnt; i++) fputc(prev, fout);
      prev = fgetc(fin); filled = prev != EOF;
    } else {
      fputc(prev, fout);
      prev = cnt; filled = 1;
    }
  }

  if (filled)
    fputc(prev, fout);

  fclose(fin);
  fclose(fout);
}

u_char *fslurp(const char *path, unsigned *bufLen)
{ FILE *fin;
  u_char *buf;
  struct stat stbuf;
  int rc;

  buf = NULL;
  *bufLen = 0;

  fin = fopen(path, "rb");
  if (fin == NULL)
    goto ErrOut;

  rc = fstat(fileno(fin), &stbuf);
  if (rc < 0)
    goto ErrOut;

  buf = (u_char *)malloc(stbuf.st_size * sizeof(u_char));
  if (buf == NULL) goto ErrOut;
  if (0 == fread(buf, sizeof(u_char), stbuf.st_size, fin))
    goto ErrOut;

  *bufLen = stbuf.st_size;

  fclose(fin);
  return buf;

ErrOut:
  if (fin) fclose(fin);
  if (buf) free(buf);
  return NULL;
}

/* Apple PackBits */
void writePB(FILE *fout, unsigned char *obuf, int cnt, int state)
{ int n;

  if (0 == cnt)
    return;

  n = 2;
  if (0 == state) {
    obuf[0] = 1 - cnt;
  } else {
    obuf[0] = cnt - 1;
    n = cnt + 1;
  }
  fwrite(obuf, sizeof(unsigned char), n, fout);
/*
  cnt = obuf[0];
  if (cnt & 128) cnt -= 256;
  fprintf(stderr,"header: %d\n", cnt);
*/
}

unsigned MaxPackBytes(unsigned len)
{
  return (len + (len + 126) / 127);
}

unsigned writePB2(u_char *dst, unsigned dstLen, u_char *obuf, int cnt, int state)
{ int n;

  if (0 == cnt)
    return dstLen;

  n = 2;
  if (0 == state) {
    obuf[0] = 1 - cnt;
  } else {
    obuf[0] = cnt - 1;
    n = cnt + 1;
  }
  memcpy(dst + dstLen, obuf, n);
/*
  cnt = obuf[0];
  if (cnt & 128) cnt -= 256;
  fprintf(stderr,"header: %d\n", cnt);
*/
  return dstLen + n;
}

unsigned PackBits(const u_char *src, unsigned srcLen, u_char *dst)
{ unsigned dstLen;

  u_char obuf[129];
  u_char ch, prev;
  int next_state, state, cnt, i;

  cnt = 0; obuf[++cnt] = prev = *src++; state = 1;
  for (i = 1; i < srcLen; i++)
  { ch = *src++;
    if (ch == prev) {
      if (1 == state) {
        if (1 < cnt) {
          dstLen = writePB2(dst, dstLen, obuf, cnt-1, 1);
          obuf[1] = obuf[cnt];
          cnt = 1;
        }
      }
      if (128 == cnt) {
        dstLen = writePB2(dst, dstLen, obuf, cnt, 0);
        cnt = 0;
      }
      ++cnt;
      next_state = 0;
    } else {
      if (0 == state) {
        dstLen = writePB2(dst, dstLen, obuf, cnt, 0);
        cnt = 0;
      }
      if (128 == cnt) {
        dstLen = writePB2(dst, dstLen, obuf, cnt, 1);
        cnt = 0;
      }
      prev = obuf[++cnt] = ch;
      next_state = 1;
    }
    state = next_state;
  }

  if (cnt)
    dstLen = writePB2(dst, dstLen, obuf, cnt, state);

  return dstLen;
}

unsigned UnpackBits(const u_char *src, unsigned srcLen, u_char *dst)
{ int ch, cnt, i, j;
   u_char *p;

  // fprintf(stderr,"UnpackBits: srcLen = %u\n", srcLen);
  p = dst;
  cnt = src[0];
  for (i = 1; i < srcLen; ) 
  { if (128 & cnt)
      cnt -= 256;
    // fprintf(stderr,"header: %d\n", cnt);
    if (-128 == cnt) {
      cnt = src[i++];
    } else if (cnt < 0) {
      cnt = 1 - cnt;
      ch = src[i++];
      for (j = 0; j < cnt; j++)
        *p++ = ch;
    } else {
      cnt = cnt + 1;
      for (j = 0; j < cnt; j++)
        *p++ = src[i++];
    }
    cnt = src[i++];
  }
  return p - dst;
}

void bitsPack(const char *inPath, const char *outPath)
{ FILE *fin = NULL, *fout = NULL;
  unsigned char ibuf[256], obuf[129];
  unsigned char prev;
  int next_state, state, cnt, i, n;

  fin = fopen(inPath, "rb");
  fout = fopen(outPath, "wb");

  cnt = 0; obuf[++cnt] = prev = fgetc(fin); state = 1;
  while (!feof(fin)) {
    n = fread(ibuf, sizeof(unsigned char), sizeof(ibuf), fin);
    for (i = 0; i < n; i++) {
      if (ibuf[i] == prev) {
        if (1 == state) {
          if (1 < cnt) {
            writePB(fout, obuf, cnt-1, 1);
            obuf[1] = obuf[cnt];
            cnt = 1;
          }
        }
        if (128 == cnt) {
          writePB(fout, obuf, cnt, 0);
          cnt = 0;
        }
        obuf[++cnt] = ibuf[i];
        next_state = 0;
      } else {
        if (0 == state) {
          writePB(fout, obuf, cnt, 0);
          cnt = 0;
        }
        if (128 == cnt) {
          writePB(fout, obuf, cnt, 1);
          cnt = 0;
        }
        obuf[++cnt] = ibuf[i];
        prev = ibuf[i];
        next_state = 1;
      }
      state = next_state;
    }
  }

  if (cnt)
    writePB(fout, obuf, cnt, state);

  fclose(fin);
  fclose(fout);
}

void bitsUnpack(const char *inPath, const char *outPath)
{ FILE *fin = NULL, *fout = NULL;
  int prev, cnt, i;

  fin = fopen(inPath, "rb");
  fout = fopen(outPath, "wb");

  cnt = fgetc(fin);
  while (!feof(fin)) {
    if (cnt & 128) cnt -= 256;
/*
    fprintf(stderr,"header: %d\n", cnt);
*/
    if (-128 == cnt) {
      cnt = fgetc(fin);
    } else if (cnt < 0) {
      cnt = 1 - cnt;
      prev = fgetc(fin);
      for (i = 0; i < cnt; i++) fputc(prev, fout);
    } else {
      cnt = cnt + 1;
      for (i = 0; i < cnt; i++) fputc(fgetc(fin), fout);
    }
    cnt = fgetc(fin);
  }

  fclose(fin);
  fclose(fout);
}


void bitsPackEx(const char *inPath, const char *outPath)
{ FILE *fout;
  unsigned srcLen, dstLen;
  u_char *src, *dst;

  srcLen = 0;
  src = fslurp(inPath, &srcLen);
  // fprintf(stderr,"srcLen = %u\n", srcLen);
  dst = (u_char *)malloc(MaxPackBytes(srcLen) + sizeof(unsigned));
  dstLen = PackBits((u_char *)src, srcLen, dst + sizeof(unsigned));
  // fprintf(stderr,"dstLen = %u\n", dstLen);
  memcpy(dst, &srcLen, sizeof(unsigned));

  fout = fopen(outPath, "wb");
  fwrite(dst, sizeof(u_char), dstLen + sizeof(unsigned), fout);
  fclose(fout);

  free(dst);
  free(src);
}


void bitsUnpackEx(const char *inPath, const char *outPath)
{ FILE *fout;
  unsigned srcLen, dstLen, outLen;
  u_char *src, *dst;

  srcLen = 0;
  src = fslurp(inPath, &srcLen);
  memcpy(&dstLen, src, sizeof(unsigned));
  // fprintf(stderr,"dstLen = %u\n", dstLen);
  dst = (u_char *)malloc(sizeof(u_char) * dstLen);
  outLen = UnpackBits(src + sizeof(unsigned), srcLen - sizeof(unsigned), dst);
  // fprintf(stderr,"outLen = %u\n", outLen);

  fout = fopen(outPath, "wb");
  fwrite(dst, sizeof(u_char), outLen, fout);
  fclose(fout);

  free(dst);
  free(src);
}


void usage(void)
{
  fprintf(stderr,"rle encode[2]|decode[2]|pack|unpack <input> <output>\n");
  exit (1);
}

typedef void (*Fn)(const char*,const char*);

int main(int argc, char*argv[])
{ Fn fns[] = {
    rleEncode,
    rleDecode,
    rleEncode2,
    rleDecode2,
    // bitsPack,
    // bitsUnpack
    bitsPackEx,
    bitsUnpackEx
  };
  int which;

  if (argc != 4)
    usage();

  which = -1;

       if (0 == strcmp(argv[1], "encode"))  which = 0;
  else if (0 == strcmp(argv[1], "decode"))  which = 1;
  else if (0 == strcmp(argv[1], "encode2")) which = 2;
  else if (0 == strcmp(argv[1], "decode2")) which = 3;
  else if (0 == strcmp(argv[1], "pack"))    which = 4;
  else if (0 == strcmp(argv[1], "unpack"))  which = 5;

  if (-1 == which)
    usage();

  (*fns[which])(argv[2], argv[3]);

  return 0;
}
