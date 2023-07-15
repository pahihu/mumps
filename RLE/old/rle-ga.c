#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    bitsPack,
    bitsUnpack
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
