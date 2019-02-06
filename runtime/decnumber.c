/* DECIMAL numbers: 64bit mantissa, 8bit exponent */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "decnumber.h"

#define DEC_MAX_MANTISSA 	(999999999999999999ULL)
#define DEC_NANEXP	(255)


static
DECIMAL *decMakeNaN(DECIMAL *z)
{ z->m = 0;
  z->x = DEC_NANEXP;
  return z;
}

int decIsNaN(DECIMAL *a)
{
  return (a->x < DEC_MINEXP) || (a->x > DEC_MAXEXP);
}


static
DECIMAL *decCHK(DECIMAL *a)
{ if (decIsNaN(a))
    return a;

  if (0 == a->m)
    a->x = 0;
  else if (a->x < DEC_MINEXP)
  { a->m = 0;
    a->x = 0;
  }
  else if (a->x > DEC_MAXEXP)
  { a->m = -(DECmantissa_t)(-1);
    a->x = DEC_MAXEXP;
  }
  return a;
}


DECIMAL* decNORM(DECIMAL *z)
{ if (decIsNaN(z))
    return z;

  while (z->m && (0 == z->m % 10))
  { z->m /= 10;
    z->x++;
  }

  return decCHK(z);
}


DECIMAL* decADD(DECIMAL *a, DECIMAL *b, DECIMAL *z)
{ DECmantissa_t am, bm;
  DECexponent_t ax, bx;

  if (decIsNaN(a) || decIsNaN(b))
  { return decMakeNaN(z);
  }

  if (0 == b->m)
  { *z = *a;
    return z;
  }
  else if (0 == a->m)
  { *z = *b;
    return z;
  }

  am = a->m; ax = a->x;
  bm = b->m; bx = b->x;
  if (ax < bx)
  { while (ax != bx)
    { bm *= 10; bx--;
    }
  }
  else if (ax > bx)
  { while (bx != ax)
    { am *= 10; ax--;
    }
  }
  z->m = am + bm;
  z->x = ax;

  return decCHK(z);
}


DECIMAL* decSUB(DECIMAL *a, DECIMAL *b, DECIMAL *z)
{ DECmantissa_t am, bm;
  DECexponent_t ax, bx;

  if (decIsNaN(a) || decIsNaN(b))
  { return decMakeNaN(z);
  }

  if (0 == b->m)
  { *z = *a;
    return z;
  }
  else if (0 == a->m)
  { *z = *b;
    return z;
  }

  am = a->m; ax = a->x;
  bm = b->m; bx = b->x;
  if (ax < bx)
  { while (ax != bx)
    { bm *= 10; bx--;
    }
  }
  else if (ax > bx)
  { while (bx != ax)
    { am *= 10; ax--;
    }
  }
  z->m = am - bm;
  z->x = ax;

  return decCHK(z);
}


int decCMP(DECIMAL *a, DECIMAL *b)
{ DECIMAL z;

  if (decIsNaN(a))
    return 1;
  else if (decIsNaN(b))
    return -1;

  if (a->x == b->x)
  { z.m = a->m - b->m;
  }
  else
    decSUB(a, b, &z);

  if (z.m < 0)
    return -1;
  else if (z.m > 0)
    return  1;

  return 0;
}


#define HI32(x)		((x) >> 32)
#define LO32(x)		((x) & 0x0FFFFFFFFULL)
#define HILO(x,y)	(((x) << 32) + (y))

static
uint64_t binADD(uint64_t a, uint64_t b, int *cy)
{ uint64_t ret;

  ret = a + b;
  *cy = 0;
  if (ret < a)
    *cy = 1;
  return ret;
}


static
int64_t binMUL(int64_t a, int64_t b, int *x)
{ uint64_t ua, ub;
  int signa, signb;
  uint64_t ret[3];
  uint64_t h[5];
  int i, cy;

  signa = 0;
  ua = a;
  if (a < 0)
  { signa = 1;
    ua = -a;
  }

  signb = 0;
  ub = b;
  if (b < 0)
  { signb = 1;
    ub = -b;
  }

  // (2^32*ua_hi + ua_lo) * (2^32*ub_hi + ub_lo)

  // 2^64 * ua_hi * ub_hi
  // 2^32 * (ua_hi * ub_lo + ua_lo * ub_hi)
  // ua_lo * ub_lo
#if defined(DECNUMBER_DEBUG)
  fprintf(stderr, "ua = %llu\n", ua);
  fprintf(stderr, "ub = %llu\n", ub);
  fprintf(stderr, "HI(ua) = %llu\n", HI32(ua));
  fprintf(stderr, "LO(ua) = %llu\n", LO32(ua));
  fprintf(stderr, "HI(ub) = %llu\n", HI32(ub));
  fprintf(stderr, "LO(ub) = %llu\n", LO32(ub));
#endif

  ret[0] = HI32(ua) * HI32(ub);
  ret[1] = binADD(HI32(ua) * LO32(ub), LO32(ua) * HI32(ub), &cy);
  ret[2] = LO32(ua) * LO32(ub);
  if (cy)
    ret[0]++;

#if defined(DECNUMBER_DEBUG)
  fprintf(stderr, "ret0 = %llu\n", ret[0]);
  fprintf(stderr, "ret1 = %llu\n", ret[1]);
  fprintf(stderr, "ret2 = %llu\n", ret[2]);
#endif

  // <--ret0-->
  //      <--ret1-->
  //           <--ret2-->

  ret[2] = binADD(ret[2], LO32(ret[1]) << 32, &cy);
  ret[1] = binADD(HI32(ret[1]) + cy, ret[0], &cy);

#if defined(DECNUMBER_DEBUG)
  fprintf(stderr, "res1 = %llu\n", ret[1]);
  fprintf(stderr, "res2 = %llu\n", ret[2]);
#endif

  if (ret[1])
  { h[0] = HI32(ret[1]);
    h[1] = LO32(ret[1]); 
    h[2] = HI32(ret[2]);
    h[3] = LO32(ret[2]);
    h[4] = 0;
    while ((h[0] | h[1]) || HILO(h[2],h[3]) > DEC_MAX_MANTISSA)
    { for (i = 0; i < 4; i++)
      { h[i+1] += (h[i] % 10) << 32;
        h[i  ]  =  h[i] / 10;
      }
      (*x)++;
    }
    ret[2] = HILO(h[2], h[3]);
  }

  if (signa != signb)
  { ret[2] = 1 + ~ret[2];
  }
  return (int64_t) ret[2];
}


DECIMAL* decMUL(DECIMAL *a, DECIMAL *b, DECIMAL *z)
{ int scale;

  if (decIsNaN(a) || decIsNaN(b))
  { return decMakeNaN(z);
  }

  if ((0 == a->m) || (0 == b->m))
  { z->m = 0;
    z->x = 0;
    return z;
  }

  scale = 0;
  z->m = binMUL(a->m, b->m, &scale);
  z->x = a->x + b->x + scale;
  return decCHK(z);
}
  

DECIMAL* decDIV(DECIMAL *a, DECIMAL *b, DECIMAL *z)
{
  DECexponent_t x;
  DECumantissa_t am, bm, zm;
  int signa, signb;
  int done;

  if (decIsNaN(a) || decIsNaN(b))
  { return decMakeNaN(z);
  }

  if (0 == b->m)
  { if (0 == a->m)
    { z->x = 0;
      z->m = 0;
      return z;
    }
    return decMakeNaN(z);
  }

  signa = 0;
  am = a->m;
  if (a->m < 0)
  { am = -(a->m);
    signa = 1;
  }
  signb = 0;
  bm = b->m;
  if (b->m < 0)
  { bm = -(b->m);
    signb = 1;
  }

#if defined(DECNUMBER_DEBUG)
  fprintf(stderr, "am = %llu\n", am);
  fprintf(stderr, "bm = %llu\n", bm);
#endif

  x = a->x - b->x;
  while (am < bm)
  { am *= 10; x--;
  }

#if defined(DECNUMBER_DEBUG)
  fprintf(stderr, "scaled am = %llu\n", am);
  fprintf(stderr, "scaled bm = %llu\n", bm);
  fprintf(stderr, "x = %d\n", x);
#endif

  done = 0;
  zm = 0;
  do 
  { if (am < bm)
    { if (zm * 10 < DEC_MAX_MANTISSA)
      { am *= 10;
        x--;
        zm *= 10;
      }
      else
        done = 1;
    }
    else
    { zm = zm + (am / bm);
      am = am % bm;
#if defined(DECNUMBER_DEBUG)
      fprintf(stderr, "zm = %llu\n", zm);
      fprintf(stderr, "am = %llu\n", am);
#endif
    }
  } while (am && !done);
  if (signa != signb)
  { zm = ~zm;
    zm++;
  }
  z->m = (DECmantissa_t) zm;
  z->x = x;
#if defined(DECNUMBER_DEBUG)
  fprintf(stderr, "z.m = %lld\n", z->m);
  fprintf(stderr, "z.x = %d\n", z->x);
#endif
  return decCHK(z);
}


void decTRUNC(DECIMAL *a, DECIMAL *intZ, DECIMAL *fracZ)
{ char buf[256];
  char *p;

  if (decIsNaN(a))
  { if (intZ)  decMakeNaN(intZ);
    if (fracZ) decMakeNaN(fracZ);
    return;
  }

  dectoa(a, buf, 10);
  p = strchr(buf, '.');
  if (!p)
  { if (intZ)
      *intZ = *a;
    if (fracZ)
    { fracZ->m = 0;
      fracZ->x = 0;
    } 
    return;
  }
  if (intZ)
  { *p = '\0';
    atodec(buf, p - buf, intZ);
  }
  if (fracZ)
  { *p = '.';
    atodec(p, -1, fracZ);
  }
}


DECIMAL *decINT(DECIMAL *a, DECIMAL *z)
{ decTRUNC(a, z, 0);
  return z;
}


DECIMAL* decMOD(DECIMAL *a, DECIMAL *b, DECIMAL *z)
{ DECIMAL quo, iquo, tmp;

  if (decIsNaN(a) || decIsNaN(b))
  { return decMakeNaN(z);
  }

  decDIV(a, b, &quo);
  decINT(&quo, &iquo);
  return decSUB(a, decMUL(b, &iquo, &tmp), z);
}


DECIMAL* decIPOW(DECIMAL *a, DECIMAL *b, DECIMAL *z)
{ DECIMAL bMINUS1, halfB, tmp;

  if (decIsNaN(a) || decIsNaN(b))
  { return decMakeNaN(z);
  }

  if (0 == b->x)
  { if (0 == b->m)
    { z->m = 1;
      z->x = 0;
      return z;
    }
    if (1 == b->m)
    { *z = *a;
      return z;
    }
  }

  if (1 & b->m)
  { bMINUS1.m = b->m - 1;
    bMINUS1.x = 0;
    return decMUL(a, decIPOW(a, &bMINUS1, &tmp), z);
  }
  halfB.m = b->m >> 1;
  halfB.x = 0;
  decIPOW(a, &halfB, &tmp);
  return decMUL(&tmp, &tmp, z);
}


DECIMAL* decPOW(DECIMAL *a, DECIMAL *b, DECIMAL *z)
{ DECIMAL intB, fracB, dff, tmp;
  double fa, ffb;
  char buf[256];
  int n;

  decTRUNC(b, &intB, &fracB);
  decIPOW(a, &intB, &tmp);
  if (fracB.m)
  { dectoa(a, buf, 10);
    fa = atof(buf);
    dectoa(&fracB, buf, 10);
    ffb = atof(buf);
    n = sprintf(buf, "%.15lf", pow(fa, ffb));
    atodec(buf, n, &dff);
    decMUL(&tmp, &dff, z);
  }
  else
  { *z = tmp;
  }
  return z;
}


char* dectoa(DECIMAL *a, char *buf, int radix)
{
  DECmantissa_t am;
  DECexponent_t ax;
  char tmp[256], *p, *q;
  int ndigits, sign, i;

  am = a->m; ax = a->x;
  if (!am)
    return strcpy(buf, "0");

  while (0 == am % 10)
  { am /= 10; ax++;
  }

  sign = am < 0;
  if (sign)
    am = -am;

  ndigits = 0;
  p = tmp + 256;
  *--p = '\0';
  do
  { *--p = '0' + am % radix;
    ndigits++;
    am /= radix;
  } while (am);

  // n - num of digits
  if (ax >= 0)
  { if (sign)
    { *--p = '-';
      ndigits++;
    }
    strcpy(buf, p);
    p = buf + ndigits;
    for (i = 0; i < ax; i++)
      *p++ = '0';
    *p = '\0';
  }
  else if (ax < 0)
  { ax = -ax;
    if (ax < ndigits)
    { // [ndigits - ax] "." [ax]
      q = buf;
      if (sign)
        *q++ = '-';
      for (i = 0; i < ndigits - ax; i++)
        *q++ = *p++;
      *q++ = '.';
      for (i = 0; i < ax; i++)
        *q++ = *p++;
      *q = '\0';
    }
    else
    { // [ax - ndigits] "0" "." [ax]
      q = buf;
      if (sign)
        *q++ = '-';
      *q++ = '0';
      *q++ = '.';
      for (i = 0; i < ax - ndigits; i++)
        *q++ = '0';
      strcpy(q, p);
    }
  }

  return buf;
}


#define NEXT ch = *buf++; len--;

static
int digit(int ch)
{
  return ('0' <= ch) && (ch <= '9');
}


DECIMAL* atodec(char *buf, int len, DECIMAL *z)
{
  DECmantissa_t m;
  DECexponent_t x, mx;
  int ch, sign, ndigits, half_digit;
  DECIMAL *ret;

  if (len < 0)
    len = strlen(buf);
  m = 0; mx = 0; x = 0;
  ret = 0;
  if (!len) return ret;
  len++;
  NEXT;
  sign = 0;
  if ('+' == ch || '-' == ch)
  { sign = '-' == ch; NEXT;
  }
  if (!len) return ret;
  half_digit = 0;
  ndigits = 0;
  while (len && digit(ch))
  { if (++ndigits <= DEC_MAXDIGITS)
    { m *= 10; m += ch - '0';
    }
    else
    { mx++;
      if (0 == half_digit)
        half_digit = ch;
    }
    NEXT;
  }
  if (half_digit)
  { if (half_digit - '0' > 4)
    { m++;
      half_digit = 0;
    }
  }
  if (len)
  { if (ch == '.')
    { NEXT;
      if (!len || !digit(ch)) return ret;
      while (len && digit(ch))
      { if (++ndigits <= DEC_MAXDIGITS)
        {
          m *= 10; m += ch - '0';
	  mx--;
          half_digit = 0;
        }
        else if (0 == half_digit)
          half_digit = ch;
        NEXT;
      }
      if (half_digit)
      { if (half_digit - '0' > 4)
        { m++;
          half_digit = 0;
        }
      }
    }
    if (sign)
      m = -m;
    if ('e' == ch || 'E' == ch)
    { NEXT;
      if (!len) return ret;
      sign = 0;
      if ('+' == ch || '-' == ch)
      { sign = '-' == ch; NEXT;
        if (!len || !digit(ch)) return ret;
      }
      ndigits = 0;
      while (len && digit(ch))
      { if (++ndigits < 4)
        { x *= 10; x += ch - '0';
        }
        else
          return ret;
        NEXT;
      }
      if (sign)
        x = -x;
      mx += x;
    }
  }
  else if (sign)
    m = -m;

  ret = z;
  z->m = m;
  z->x = mx;

  decCHK(z);

  return ret;
}


