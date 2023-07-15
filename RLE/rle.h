#ifndef _RLE_H_
#define _RLE_H_

short RLE_Pack(u_char *src, short srclen, u_char *dst);
short RLE_Unpack(u_char *src, short srclen, u_char *dst);
short RLE_Length(u_char *src, short srclen);

typedef struct _RLE {
  int i, j;
  u_char *src;
  short srclen;
  int counter;
  int state;
  u_char ch;
} RLE;

int RLE_Init(RLE *rle, u_char *src, short srclen);
int RLE_Next(RLE *rle);

#endif
