typedef unsigned char u_char;
#include "rle.h"

short RLE_Pack(u_char *src, short srclen, u_char *dst)
{ int flag, counter, i, j;
  u_char ch;
  
  j = 0; flag = j++; counter = 0;
  for (i = 0; i < srclen; )
  { ch = src[i++];
    if (i + 1 < srclen && ch == src[i] && ch == src[i+1])
    { if (counter)
      { dst[flag] = counter - 1;
        flag = j++; counter = 0;
      }
      counter = 3; i = i + 2;
      while (i < srclen && ch == src[i] && counter < 128)
      { counter++; i++;
      }
      dst[flag] = 257 - counter;
      dst[j++]  = ch;
      flag = j++; counter = 0;
    }
    else if (counter < 128)
    { counter++;
      dst[j++] = ch;
    }
    else
    { dst[flag] = counter - 1;
      flag = j++; counter = 1;
      dst[j++] = ch;
    }
  }
  if (counter)
  { dst[flag] = counter - 1;
  }
  return counter ? j : j - 1;
}

short RLE_Unpack(u_char *src, short srclen, u_char *dst)
{ short dstlen;
  int i, j, counter;
  u_char ch;

  dstlen = 0;
  for (i = 0; i < srclen; )
  { counter = src[i++];
    if (0x80 == counter)
    { continue;
    }
    else if (counter < 0x80)
    { counter++;
      for (j = 0; j < counter; j++)
        *dst++ = src[i++];
      dstlen += counter;
    }
    else
    { counter = 257 - counter;
      ch = src[i++];
      for (j = 0; j < counter; j++)
        *dst++ = ch;
      dstlen += j;
    }
  }
  return dstlen;
}

short RLE_Length(u_char *src, short srclen)
{ short dstlen;
  int i, counter;

  dstlen = 0;
  for (i = 0; i < srclen; )
  { counter = src[i++];
    if (0x80 == counter)
    { continue;
    }
    else if (counter < 0x80)
    { counter++;
      i += counter;
      dstlen += counter;
    }
    else
    { counter = 257 - counter;
      i++;
      dstlen += counter;
    }
  }
  return dstlen;
}

static int RLE_NextSegment(RLE *rle)
{
InitCounter:
  if (rle->i < rle->srclen)
  { rle->counter = rle->src[rle->i++];
    if (0x80 == rle->counter)
      goto InitCounter;
    if (rle->counter < 0x80)
    { rle->counter++;
      rle->state = 0;
    }
    else
    { rle->counter = 257 - rle->counter;
      rle->ch = rle->src[rle->i++];
      rle->state = 1;
    }
    rle->j = 0;
    return 0;
  }
  return -1;
}

int RLE_Init(RLE *rle, u_char *src, short srclen)
{
  rle->i = 0;
  rle->src = src;
  rle->srclen = srclen;
  return RLE_NextSegment(rle);
}

int RLE_Next(RLE *rle)
{ int ret;

NextByte:
  if (rle->j < rle->counter)
  { rle->j++;
    return rle->state ? rle->ch : rle->src[rle->i++];
  }
  ret = RLE_NextSegment(rle);
  if (ret < 0)
    return ret;
  goto NextByte;
}


#ifdef TESTING

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

int rnd(int n)
{ return random() % n;
}

void bufdump(int seq, u_char *buf, int len)
{ int i;

  fprintf(stderr,"u_char buf%d[] = {",seq);
  for (i = 0; i < len; i++)
  { if (i) fprintf(stderr,", ");
    if ((i % 8) == 0) fprintf(stderr,"\n\t");
    fprintf(stderr,"0x%02x", buf[i]);
  }
  fprintf(stderr,"};\n");
}

void test(int p_prev, int seq)
{ u_char buf1[4096+64], buf2[4096+64], buf3[4096+64];
  int len, elen, dlen, rlen, i, x;
  u_char ch;
  int ret;
  RLE rle;

  len = rnd(4096);
  buf1[0] = rnd(256);
  for (i = 1; i < len; i++)
  { x = rnd(100);
    if (x < p_prev)
      buf1[i] = buf1[i-1];
    else
    { ch = rnd(256);
      while (ch == 0x80)
        ch = rnd(256);
      buf1[i] = ch;
    }
  }
  elen = RLE_Pack(buf1, len, buf2);
  rlen = RLE_Length(buf2, elen);
  if (rlen != len)
  { fprintf(stderr,"// failed: prev=%d seq=%d len=%d rlen=%d\n",p_prev,seq,len,rlen);
    bufdump(seq, buf1, len);
    return;
  }
  if (elen)
  { ret = RLE_Init(&rle, buf2, elen);
    assert(ret >= 0);
    dlen = 0; ret = RLE_Next(&rle);
    while (ret >= 0)
    { buf3[dlen++] = ret;
      ret = RLE_Next(&rle);
    }
    if (len != dlen)
    { fprintf(stderr,"// failed: RLE_Next prev=%d seq=%d len=%d dlen=%d\n",p_prev,seq,len,dlen);
      bufdump(seq, buf1, len);
      return;
    }
    if (memcmp(buf1, buf3, len))
    { fprintf(stderr,"// failed: RLE_Next prev=%d seq=%d len=%d compare\n",p_prev,seq,len);
    }
  }
  dlen = RLE_Unpack(buf2, elen, buf3);
  if (len != dlen)
  { fprintf(stderr,"// failed: prev=%d seq=%d len=%d dlen=%d\n",p_prev,seq,len,dlen);
    bufdump(seq, buf1, len);
    return;
  }
  if (memcmp(buf1, buf3, len))
  { fprintf(stderr,"// failed: prev=%d seq=%d len=%d compare\n",p_prev,seq,len);
  }
}

int main(int argc, char *argv[])
{
  u_char input1[24] =
  { 0xAA, 0xAA, 0xAA, 0x80,
    0x00, 0x2A, 0xAA, 0xAA,
    0xAA, 0xAA, 0x80, 0x00,
    0x2A, 0x22, 0xAA, 0xAA,
    0xAA, 0xAA, 0xAA, 0xAA,
    0xAA, 0xAA, 0xAA, 0xAA
  };
  u_char output1[15] =
  { 0xFE, 0xAA, 0x02, 0x80,
    0x00, 0x2A, 0xFD, 0xAA,
    0x03, 0x80, 0x00, 0x2A,
    0x22, 0xF7, 0xAA
  };
  u_char buf1[4096+64], buf2[4096+64];
  short len;
  const int N = 100000;
  int i;
  
  len = RLE_Pack(input1, 24, buf1);
  printf("packed len=%d\n", len);
  assert(len == 15);
  assert(0 == memcmp(output1, buf1, 15));

  len = RLE_Unpack(buf1, 15, buf2);
  printf("unpacked len=%d\n", len);
  assert(len == 24);
  assert(0 == memcmp(input1, buf2, 24));

  srandom(0x9ac8175);
  for (i = 0; i < N; i++)
    test(0, i);
  for (i = 0; i < N; i++)
    test(30, i);
  for (i = 0; i < N; i++)
    test(50, i);
  for (i = 0; i < N; i++)
    test(70, i);
  for (i = 0; i < N; i++)
    test(100, i);

  return 0;
}
#endif

// vim:ts=2:sw=2:et:
