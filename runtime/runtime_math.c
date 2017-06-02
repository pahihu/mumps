// File: mumps/runtime/runtime_math.c
//
// module runtime_math - decimal arithmetic adapted from FreeMUMPS

//   FreeMUMPS is not to be placed under the GNU General Public Licence.
//   Anybody who wants to produce a proprietary product with parts of
//   FreeMUMPS is free to do so: Either with FreeMUMPS as part of
//   application programs or with FreeMUMPS as the basis of a new
//   commercial MUMPS implementation. However, who passes FreeMUMPS
//   or products based on it, must inform the recipient of the
//   FreeMUMPS Copyright, and he has to provide the FreeMUMPS
//   source codes without additional charges.


#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "opcodes.h"				// the op codes
#include "mumps.h"                              // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"				// standard errors

#define PLUS	'+'
#define MINUS	'-'
#define POINT	'.'
#define point	(POINT-ZERO)
#define ZERO	'0'
#define ONE	(ZERO+1)
#define TWO	(ZERO+2)
#define THREE	(ZERO+3)
#define FIVE	(ZERO+(NUMBASE/2))
#define NINE	(ZERO+NUMBASE-1)
#define NUMBASE	10
#define EOL ((char)'\000')
#define toggle(A) (A^=01)

// The following sub-routines are called internally in this file only
//
short g_sqrt (char *a);                          /* square root */
short root (char *a, int n);                     /* n.th root */
void roundit (char *a, int digits);

// #define MV1_MAPM        1

#ifdef MV1_MAPM

#define MV1_LONG_DIGITS         (8 == sizeof(long) ? 19 : 9)

#include "m_apm.h"
static M_APM aa, bb, cc, dummy;                 // MAPM args
static long laa, lbb;                           // long args
static int  laaq, lbbq;

static
void m_apmtom(char *buf, M_APM cc)
{
    int dplaces;

    if (m_apm_is_integer(cc))
      m_apm_to_integer_string(buf, cc);
    else
    { dplaces = m_apm_significant_digits(cc) - m_apm_exponent(cc) - 1;
      if (dplaces > partab.jobtab->precision)
        dplaces = partab.jobtab->precision;
      m_apm_to_fixpt_string(buf, dplaces, cc);
    }
}

static
int m_mtoapm(M_APM out, long *lout, char *buf)
{
    int len;

    len = strlen(buf);
    if (1 == len)
    { *lout = (*buf) - '0';
      return len;
    }
    else if ((len < 10) && (0 == strchr(buf,'.')))
    { *lout = atol(buf);
      return len;
    }
    else
      m_apm_set_string(out, buf);
    return 0;
}

char* ltoa(char *buf, long n)
{
    int   sign = 0, docpy = 0;
    char  *ptr, tmp[32];

    if (n < 0)
    { sign = 1;
      n = -n;
    }
    
         if (n <         10L) ptr = &buf[ 2 + sign];
    else if (n <        100L) ptr = &buf[ 3 + sign];
    else if (n <       1000L) ptr = &buf[ 4 + sign];
    else if (n <      10000L) ptr = &buf[ 5 + sign];
    else if (n <     100000L) ptr = &buf[ 6 + sign];
    else if (n <    1000000L) ptr = &buf[ 7 + sign];
    else if (n <   10000000L) ptr = &buf[ 8 + sign];
    else if (n <  100000000L) ptr = &buf[ 9 + sign];
    else if (n < 1000000000L) ptr = &buf[10 + sign];
    else if (8 == sizeof(long))
    {
           if (n <         10000000000L) ptr = &buf[11 + sign];
      else if (n <        100000000000L) ptr = &buf[12 + sign];
      else if (n <       1000000000000L) ptr = &buf[13 + sign];
      else if (n <      10000000000000L) ptr = &buf[14 + sign];
      else if (n <     100000000000000L) ptr = &buf[15 + sign];
      else if (n <    1000000000000000L) ptr = &buf[16 + sign];
      else if (n <   10000000000000000L) ptr = &buf[17 + sign];
      else if (n <  100000000000000000L) ptr = &buf[18 + sign];
      else if (n < 1000000000000000000L) ptr = &buf[19 + sign];
      else
      { ptr = &tmp[32];
        docpy = 1;
      }
    }
    else
    { ptr = &tmp[32];
      docpy = 1;
    }

    *--ptr = '\0';
    do
    { *--ptr = '0' + (n % 10);
      n /= 10;
    } while (n);
    
    if (sign)
      *--ptr = '-';

    return docpy ? memmove(buf, ptr, &tmp[31] - ptr + 1) : ptr;
}

#endif

#ifdef MV1_INTMATH
typedef long long int64;

int64 aa, bb, cc;

int strton(char *a, int64 *ret)
{
  char *ptr;

  *ret = strtoll(a, &ptr, 10);
  if (errno)
    return -1;
  if (*ptr != '\0')
    return -1;

  return 0;
}

int ntostr(char *buf, int64 n)
{ static char tmp[64];
  char *p;
  int neg = 0;

  if (n < 0)
  { neg = 1;
    n = -n;
  }

  tmp[63] = '\0';
  p = &tmp[62];
  do
  { *--p = '0' + (n % 10);
    n = n / 10;
  } while (n);
  if (neg) *--p = '-';

  strcpy(buf, p);
  return &tmp[63] - p;
}

#endif


void runtime_math_init(void)
{
#ifdef MV1_MAPM
    aa = m_apm_init();
    bb = m_apm_init();
    cc = m_apm_init();
    dummy = m_apm_init();
#endif
    return;
}


short runtime_add(char *a, char *b)		// add b to a
{
    int done = 0;

    if (b[0] == ZERO)
	return strlen(a);
    if (a[0] == ZERO) {
	strcpy (a, b);
	return strlen(a);
    }

#ifdef MV1_MAPM
    laaq = m_mtoapm(aa, &laa, a);
    lbbq = m_mtoapm(bb, &lbb, b);
    if (laaq && lbbq)
    { if ((laaq < MV1_LONG_DIGITS) && (lbbq < MV1_LONG_DIGITS))
      { ltoa(a, laa + lbb);
        done = 1;
      }
    }
    if (!done)
    { if (laaq)
        m_apm_set_long(aa, laa);
      if (lbbq)
        m_apm_set_long(bb, lbb);
   
      m_apm_add(cc, aa, bb);
      m_apmtom(a, cc);
    }


    return strlen(a);
#endif

#ifdef MV1_INTMATH
    if (!strton(a, &aa) && !strton(b, &bb))
      return ntostr(a, aa + bb);
#endif

    {
	short   dpa,			/* decimal point of 'a' */
	        dpb,			/* decimal point of 'b' */
	        lena,			/* length of 'a' */
	        lenb;			/* length of 'b' */
	int     mi;			/* minus flag */
	short   sign;			/* sign flag  if a<0<b sign=-1; */

/*            if a>0>b sign=1;  */
/*            else     sign=0;  */

	int i;
	int ch;
	int j;
	int carry;


/* look at the signs */
	mi = 0;
	sign = 0;
	if (a[0] == b[0] && a[0] == MINUS) {
	    mi++;
	    a[0] = b[0] = ZERO;
	} else if (a[0] == MINUS) {
	    sign--;
	    a[0] = NINE;
	    i = 0;
	    while ((ch = a[++i]) != EOL)
		if (ch != POINT)
		    a[i] = ZERO + NINE - ch;
	    a[--i]++;
	} else if (b[0] == MINUS) {
	    sign++;
	    b[0] = NINE;
	    i = 0;
	    while ((ch = b[++i]) != EOL)
		if (ch != POINT)
		    b[i] = ZERO + NINE - ch;
	    b[--i]++;
	}

/* search decimal points and length */
	dpa = dpb = (-1);
	i = 0;
	while (a[i] != EOL) {
	    if (a[i] == POINT)
		dpa = i;
	    i++;
	}
	lena = i;
	if (dpa < 0)
	    dpa = i;
      again:;
	i = 0;
	while (b[i] != EOL) {
	    if (b[i] == POINT)
		dpb = i;
	    i++;
	}
	lenb = i;
	if (dpb < 0)
	    dpb = i;
	if (i == 1) {
	    if (b[0] == ONE && sign == 0 && dpa > 0) {
	    /* frequent special case: add 1 */
		i = dpa - 1;
		while (++a[i] > NINE) {
		    a[i--] = ZERO;
		    if (i < 0) {
			i = lena;
			while (i >= 0) {
			    a[i + 1] = a[i];
			    i--;
			}
			a[0] = ONE;
			return (short) strlen(a);
		    }
		}
		return (short) strlen(a);
	    }
	}

/* copy additional trailing digits from b to a */
	if (lenb - dpb > lena - dpa) {
	    j = dpa - dpb;
	    if (lenb + j > MAX_NUM_BYTES) {	/* round off that monster ! */
		i = MAX_NUM_BYTES - j;
		if (b[i] < FIVE) {
		    b[i] = EOL;
		    lenb--;
		    while (b[--i] == ZERO) {
			b[i] = EOL;
			lenb--;
		    }
		} else {
		    for (;;)
		    {
			if (i >= dpb) {
			    b[i] = EOL;
			    lenb--;
			} else
			    b[i] = ZERO;
			if (--i < 0) {
			    for (i = lenb; i >= 0; i--)
				b[i + 1] = b[i];
			    b[0] = ONE;
			    dpb = ++lenb;
			    break;
			}
			if ((ch = b[i]) == POINT) {
			    dpb = i;
			    continue;
			}
			if (ch < NINE && ch >= ZERO) {
			    b[i] = ++ch;
			    break;
			}
		    }
		}
		goto again;		/* look what's left from b */
	    }
	    lenb = i = lena - dpa + dpb;
	    j = lena;
	    while ((a[j++] = b[i++]) != EOL) ;
	    lena = (--j);
	    b[lenb] = EOL;
	}

/* $justify a or b */
	i = dpa - dpb;
	if (i < 0) {
	    j = lena;
	    if ((i = (lena -= i)) > (MAX_NUM_BYTES - 2) /*was 253*/) {

		return -(ERRM75);
	    }
	    ch = (sign >= 0 ? ZERO : NINE);
	    while (j >= 0)
		a[i--] = a[j--];
	    while (i >= 0)
		a[i--] = ch;
	    dpa = dpb;
	} else if (i > 0) {
	    j = lenb;
	    if ((lenb = (i += lenb)) > (MAX_NUM_BYTES - 2)/*was 253*/) {

		return -(ERRM75);
	    }
	    ch = (sign <= 0 ? ZERO : NINE);
	    while (j >= 0)
		b[i--] = b[j--];
	    while (i >= 0)
		b[i--] = ch;
	    dpb = dpa;
	}

/* now add */
	carry = 0;

	for (i = lenb - 1; i >= 0; i--) {
	    if ((ch = a[i]) == POINT)
		{ continue; }
	    ch += b[i] - ZERO + carry;
	    if ((carry = (ch > NINE)))
		{ ch -= NUMBASE; }

	    a[i] = ch;

	}

	while (a[lena] != EOL)
	    lena++;
	if (carry) {
	    if ((i = (++lena)) > (MAX_NUM_BYTES - 2)) {

		return -(ERRM75);
	    }
	    while (i > 0) {
		a[i] = a[i - 1];
		i--;
	    }
	    a[0] = ONE;
	}

	if (sign) {
	    if (a[0] == ONE) {
		a[0] = ZERO;
	    } else {
		i = 0;
		carry = 0;
		while ((ch = a[++i]) != EOL)
		    if (ch != POINT)
			a[i] = ZERO + NINE - ch;
		while (--i > 0) {
		    if (a[i] != POINT) {
			if (++a[i] <= NINE)
			    break;
			a[i] = ZERO;
		    }
		}
		mi = 1;
		a[0] = ZERO;
	    }

	    while (a[mi] == ZERO) {
		//strcpy (&a[mi], &a[mi + 1]); undefined behavior XXX
		memmove (&a[mi], &a[mi + 1], strlen(&a[mi + 1]) + 1);
		dpa--;
	    }

	    if (dpa < 0)
		dpa = 0;
	}

/* remove trailing zeroes */
	i = dpa;
	while (a[i] != EOL)
	    i++;
	if (--i > dpa) {
	    while (a[i] == ZERO)
		a[i--] = EOL;
	}

/* remove trailing point */
	if (a[i] == POINT)
	    a[i] = EOL;
	if (mi) {
	    if (a[0] != ZERO) {
		i = 0;
		while (a[i++] != EOL) ;
		while (i > 0) {
		    a[i] = a[i - 1];
		    i--;
		}
	    }
	    a[0] = MINUS;
	}
	if (a[mi] == EOL) {
	    a[0] = ZERO;
	    a[1] = EOL;
	}

        return (short) strlen(a);
    }
}


short runtime_mul(char *a, char *b)             // multiply a by b
{
        char    c[2*(MAX_NUM_BYTES+1)];
    short   alen,
            blen,
            clen,
            mi,
            tmpx;
    int acur;
    int bcur;
    int ccur;
    int carry;
    int done = 0;

#ifdef MV1_MAPM
    laaq = m_mtoapm(aa, &laa, a);
    lbbq = m_mtoapm(bb, &lbb, b);
    if (laaq && lbbq)
    { if (laaq + lbbq < MV1_LONG_DIGITS + 1)
      { ltoa(a, laa * lbb);
        done = 1;
      }
    }
    if (!done)
    { if (laaq)
        m_apm_set_long(aa, laa);
      if (lbbq)
        m_apm_set_long(bb, lbb);
      m_apm_multiply(cc, aa, bb);
      m_apmtom(a, cc);
    }

    return strlen(a);
#endif

#ifdef MV1_INTMATH
    if (!strton(a, &aa) && !strton(b, &bb))
      return ntostr(a, aa * bb);
#endif

/* if zero or one there's not much to do */
    if (b[1] == EOL) {
        if (b[0] == ZERO) {
            a[0] = ZERO;
            a[1] = EOL;
            return 1;
        }
        if (b[0] == ONE) {
            return strlen(a); }
        if (b[0] == TWO) {
multwo:     acur = 0;
            while (a[++acur] != EOL) ;
            mi = (a[acur - 1] == FIVE);
            carry = 0;
            ccur = acur;
/*            while (acur >= 0) {	*/
            while (acur > 0) {
                if ((bcur = a[--acur]) < ZERO)
                    continue;
                bcur = bcur * 2 - ZERO + carry;
                carry = 0;
                if (bcur > NINE) {
                    carry = 1;
                    bcur -= NUMBASE;
                }
                a[acur] = bcur;
            }
            if (carry) {
                acur = ccur;
                if (acur > (MAX_NUM_BYTES - 1)) {
                    return -(ERRM75);
                }
                while (acur >= 0) {
                    a[acur + 1] = a[acur];
                    acur--;
                }
                a[a[0] == MINUS] = ONE;
            }
            if (mi) {
                if (carry)
                    ccur++;
                acur = 0;
                while (acur < ccur)
                    if (a[acur++] == POINT) {
                        a[--ccur] = EOL;
                        if (acur == ccur) {
                            a[--ccur] = EOL;
                        return strlen(a); }
                    }
            }
            return strlen(a);
        }
    }
    if (a[1] == EOL) {
        if (a[0] == ZERO) {
            return strlen(a);
        }
        if (a[0] == ONE) {
            strcpy (a, b);
            return strlen(a);
        }
        if (a[0] == TWO) {
            strcpy (a, b);
            goto multwo;
        }
    }
/* get length of strings and convert ASCII to decimal */
/* have a look at the signs */
    if ((mi = (a[0] == MINUS))) {
        a[0] = ZERO;
    }
    if (b[0] == MINUS) {
        b[0] = ZERO;
        toggle (mi);
    }
    carry = 0;
    alen = 0;
    while (a[alen] != EOL) {
        a[alen] -= ZERO;
        if (a[alen++] == point)
            carry = alen;
    }
/* append a point on the right side if there was none */
    if (--carry < 0) {
        carry = alen;
        a[alen++] = point;
        a[alen] = 0;
    }
    ccur = 0;
    blen = 0;
    while (b[blen] != EOL) {
        b[blen] -= ZERO;
        if (b[blen++] == point)
            ccur = blen;
    }
    if (--ccur < 0) {
        ccur = blen;
        b[blen++] = point;
        b[blen] = 0;
    }
    carry += ccur;
    if (carry > (MAX_NUM_BYTES - 3) ) {
        a[0] = EOL;
        return -(ERRM75);
    }
    ccur = clen = alen + blen;
/* init c to zero */
    while (ccur >= 0)
        c[ccur--] = 0;
    c[carry] = point;

    bcur = blen;
    clen = alen + blen - 1;
    carry = 0;
    while (bcur > 0) {
        if (b[--bcur] == point) {
            continue;
        }
        if (c[clen] == point)
            clen--;
        acur = alen;
        ccur = clen--;
        while (acur > 0) {
            if (a[--acur] == point)
                continue;
            if (c[--ccur] == point)
                --ccur;
            tmpx = a[acur] * b[bcur] + c[ccur] + carry;
            carry = tmpx / NUMBASE;
            c[ccur] = tmpx % NUMBASE;
        }
        while (carry) {
            if (c[--ccur] == point)
                ccur--;
            if ((c[ccur] += carry) >= NUMBASE) {
                c[ccur] -= NUMBASE;
                carry = 1;
            } else
                carry = 0;
        }
    }
/* copy result to a and convert it */
    a[ccur = clen = acur = (alen += blen)] = EOL;
    while (--ccur >= 0) {
        if (c[ccur] < NUMBASE)
            a[ccur] = c[ccur] + ZERO;
        else
            a[alen = ccur] = POINT;
    }
/* oversize string */
    if (acur > MAX_NUM_BYTES) {
        if (a[alen = acur = MAX_NUM_BYTES] >= FIVE) {
            int     l1;

            l1 = MAX_NUM_BYTES;
            if (a[l1] >= FIVE) {
                for (;;)
                {
                    if (a[--l1] == POINT)
                        l1--;
                    if (l1 < (a[0] == MINUS)) {
                        for (l1 = MAX_NUM_BYTES; l1 > 0; l1--)
                            a[l1] = a[l1 - 1];
                        a[a[0] == MINUS] = ONE;
                        break;
                    }
                    if ((++a[l1]) == (NINE + 1))
                        a[l1] = ZERO;
                    else
                        break;
                }
            }
        }
        a[acur] = EOL;
    }
/* remove trailing zeroes */
    if (acur >= alen) {
        while (a[--acur] == ZERO)
            a[acur] = EOL;
    }
/* remove trailing point */
    if (a[acur] == POINT)
        a[acur] = EOL;
/* remove leading zeroes */
    while (a[mi] == ZERO) {
        acur = mi;
        while ((a[acur] = a[acur + 1]) != EOL)
            acur++;
    }
    if (a[0] == EOL) {
        a[0] = ZERO;
        a[1] = EOL;
        mi = 0;
    }
    if (mi) {
        if (a[0] != ZERO) {
            acur = clen;
            while (acur > 0) {
                a[acur] = a[acur - 1];
                acur--;
            }
        }
        a[0] = MINUS;
    }
    return strlen(a);
}



short runtime_div (char *uu, char *v, short typ) /* divide string arithmetic */
//        char   *uu,                     /* dividend and result */
//               *v;                      /* divisor */
//        short   typ;                    /* type: '/' or '\' or '#' */
					/*     OPDIV, OPINT, OPMOD */
{
    char    q[MAX_NUM_BYTES + 2];  	/* quotient */
    char    u[2*(MAX_NUM_BYTES + 1)];	/* intermediate result */
    char    vv[MAX_NUM_BYTES +1];
    short   d,
            d1,
            k1,
            m,
            ulen,
            vlen,
            dpu,
            dpv,
            guess,
            mi,
            plus,
            v1,
            s;
    int i;
    int j;
    int k;
    int carry = 0;
    int ulast;
    int done = 0;

    if (uu[0] == ZERO)
        return 1;
    if ((v[0] == '0') && (!v[1]))
	return -ERRM9;

#ifdef MV1_MAPM
    laaq = m_mtoapm(aa, &laa, uu);
    lbbq = m_mtoapm(bb, &lbb,  v);

    switch (typ)
    { case OPDIV: 
        if (laaq && lbbq)
        { if (((2 == lbb) && (0 == (laa & 1))) ||
              (0 == laa % lbb))
          { ltoa(uu, laa / lbb);
            done = 1;
          }
        }
        if (!done)
        { if (laaq)
            m_apm_set_long(aa, laa);
          if (lbbq)
            m_apm_set_long(bb, lbb);

          m_apm_divide(cc, partab.jobtab->precision, aa, bb);
          m_apmtom(uu, cc);
        }
        break;
      case OPINT:
        if (laaq && lbbq)
        { ltoa(uu, laa / lbb);
          done = 1;
        }
        if (!done)
        { if (laaq)
            m_apm_set_long(aa, laa);
          if (lbbq)
            m_apm_set_long(bb, lbb);

          m_apm_integer_divide(cc, aa, bb);
          m_apmtom(uu, cc);
        }
        break;
      case OPMOD: 
        if (laaq && lbbq)
        { if (2 == lbb)
            ltoa(uu, laa & 1);
          else if (4 == lbb)
            ltoa(uu, laa & 3);
          else
            ltoa(uu, laa % lbb);
          done = 1;
        }
        if (!done)
        { if (laaq)
            m_apm_set_long(aa, laa);
          if (lbbq)
            m_apm_set_long(bb, lbb);

          m_apm_absolute_value(dummy, bb);
          if (0 == m_apm_compare(dummy, MM_Two))
          { m_apm_copy(cc, m_apm_is_odd(aa) ? MM_One : MM_Zero);
            if (-1 == m_apm_sign(bb))
            { m_apm_copy(dummy, cc);
              m_apm_negate(cc, dummy);
            }
          }
          else
          { // aa - bb * floor(aa / bb)
            m_apm_divide(dummy, partab.jobtab->precision, aa, bb);
            m_apm_floor(cc, dummy);
            m_apm_multiply(dummy, bb, cc);
            m_apm_subtract(cc, aa, dummy);
          }
          m_apmtom(uu, cc);
        }
    }

    return strlen(uu);
#endif

#ifdef MV1_INTMATH
    if (!strton(uu, &aa) && !strton(v, &bb))
    { int done = 1;
      switch (typ)
      { case OPDIV:
          cc = aa / bb;
          done = 0 == aa % bb;
          break;
        case OPINT:
          cc = aa / bb;
          break;
        case OPMOD:
          if (2 == llabs(bb))
            cc = bb < 0 ? -(aa & 1) : (aa & 1);
          else
            cc = aa - bb * (aa / bb);
          break;
      }
      if (done)
        return ntostr(uu, cc);
    }
#endif

/* look at the signs */
    strcpy (u, uu);
    mi = 0;
    plus = 0;
    if (typ != OPMOD) {
        if (u[0] == MINUS) {
            u[0] = ZERO;
            mi = 1;
        }
        if (v[0] == MINUS) {
            v[0] = ZERO;
            toggle (mi);
        }
    } else {
        strcpy (vv, v);
        if (u[0] == MINUS) {
            u[0] = ZERO;
            plus = 1;
        }
        if (v[0] == MINUS) {
            v[0] = ZERO;
            mi = 1;
            toggle (plus);
        }
    }
/* convert from ASCII to 'number' */
    i = 0;
    dpv = (-1);
    k = 0;
    while ((j = v[i]) != EOL) {
        j -= ZERO;
        if (j == point)
            dpv = i;
        if (j == 0)
            k++;
        v[i++] = j;
    }

    v[vlen = i] = 0;
    v[i + 1] = 0;
    v[i + 2] = 0;
    if (v[0] != 0) {
        while (i >= 0) {
            v[i + 1] = v[i];
            i--;
        }
        v[0] = 0;
        dpv++;
    } else {
        vlen--;
    }
    d1 = 0;

    i = 0;
    dpu = (-1);
    while (u[i] != EOL) {
        u[i] -= ZERO;
        if (u[i] == point)
            dpu = i;
        i++;
    }
    if (dpu < 0) {
        u[dpu = i++] = point;
    }
/*      u[ulen=i]=0; u[i+1]=0; u[i+2]=0;        */
    ulen = i;
    // fprintf(stderr, "dpu=%d ulen=%d u[last]=%d mi=%d dpv=%d vlen=%d v[1]=%d\r\n",
    //                  dpu, ulen, u[ulen-2], mi, dpv, vlen, v[1]);
    if ((OPMOD == typ) &&       // modulo
        (dpv <= 0) &&           // no frac in v
        (ulen == dpu + 1) &&    // no frac in u
        (1 == vlen) &&          // frac is 1 digit
        (2 == v[1])
)
    { ulast = u[ulen - 2];
      if (0 == (ulast & 1))
      { uu[0] = '0';
        uu[1] = '\0';
        return 1;
      }
      i = 0;
      if (mi)
        uu[i++] = '-';
      uu[i++] = '0' + (ulast & 1);
      uu[i] = '\0';
      return i;
    }
    while (i < (2*MAX_NUM_BYTES))
        u[i++] = 0;
    i = ulen;            /* somehow that's necessary - sometimes I check why */
    if (u[0] != 0) {
        while (i >= 0) {
            u[i + 1] = u[i];
            i--;
        }
        u[0] = 0;
        dpu++;
    } else {
        ulen--;
    }
    if ((vlen + partab.jobtab->precision) > MAX_NUM_BYTES && (dpv + partab.jobtab->precision) < vlen)
        vlen -= partab.jobtab->precision;

    if (dpv > 0) {                      /* make v an integer *//* shift v */
        d1 = vlen - dpv;
        for (i = dpv; i < vlen; i++)
            v[i] = v[i + 1];
        vlen--;
/* remove leading zeroes */
        while (v[1] == 0) {
            for (i = 1; i <= vlen; i++)
                v[i] = v[i + 1];
            vlen--;
        }
        v[vlen + 1] = 0;
        v[vlen + 2] = 0;
/* shift u */
        i = dpu;
        for (j = 0; j < d1; j++) {
            if (i >= ulen) {
                u[i + 1] = 0;
                ulen++;
            }
            u[i] = u[i + 1];
            i++;
        }
        u[i] = point;
        dpu = i;
    }
    d = dpu + 1 - ulen;
    if (dpv > dpu)
        d += dpv - dpu;
    if (typ == OPDIV)
        d += partab.jobtab->precision;
    if ((d + ulen) > MAX_NUM_BYTES) {
        u[0] = EOL;
        return -ERRM75;
    }
    while (d > 0) {
        u[++ulen] = 0;
        d--;
    }
/* normalize */
    if ((d = NUMBASE / (v[1] + 1)) > 1) {
        i = ulen;
        carry = 0;
        while (i > 0) {
            if (u[i] != point) {
                carry += u[i] * d;
                u[i] = carry % NUMBASE;
                carry = carry / NUMBASE;
            }
            i--;
        }
        u[0] = carry;
        i = vlen;
        carry = 0;
        while (i > 0) {
            carry += v[i] * d;
            v[i] = carry % NUMBASE;
            carry = carry / NUMBASE;
            i--;
        }
        v[0] = carry;
    }
/* initialize */
    j = 0;
    m = ulen - vlen + 1;
    if (m <= dpu)
        m = dpu + 1;
    for (i = 0; i <= m; q[i++] = ZERO) ;
    if (typ == OPMOD) {
        m = dpu - vlen;
    }
    v1 = v[1];

    while (j < m) {
        if (u[j] != point) {            /* calculate guess */
        if ((k = u[j] * NUMBASE + (u[j + 1] == point ? u[j + 2] : u[j + 1]))
            == 0) {
                j++;
                continue;
            }
            k1 = (u[j + 1] == point || u[j + 2] == point ? u[j + 3] : u[j + 2]);
            guess = (u[j] == v1 ? (NUMBASE - 1) : k / v1);
            if (v[2] * guess > (k - guess * v1) * NUMBASE + k1) {
                guess--;
                if (v[2] * guess > (k - guess * v1) * NUMBASE + k1)
                    guess--;
            }
/* multiply and subtract */
            i = vlen;
            carry = 0;
            k = j + i;
            if (j < dpu && k >= dpu)
                k++;
            while (k >= 0) {
                if (u[k] == point)
                    k--;
                if (i >= 0) {
                    u[k] -= v[i--] * guess + carry;
                } else {
                    if (carry == 0)
                        break;
                    u[k] -= carry;
                }
                carry = 0;
                while (u[k] < 0) {
                    u[k] += NUMBASE;
                    carry++;
                }
                k--;
            }
/* test remainder / add back */
            if (carry) {
                guess--;
                i = vlen;
                carry = 0;
                k = j + i;
                if (j < dpu && k >= dpu)
                    k++;
                while (k >= 0) {
                    if (u[k] == point)
                        k--;
                    if (i >= 0) {
                        u[k] += v[i--] + carry;
                    } else {
                        if (carry == 0)
                            break;
                        u[k] += carry;
                    }
                    carry = u[k] / NUMBASE;
                    u[k] = u[k] % NUMBASE;
                    k--;
                }
            }
            q[j++] = guess + ZERO;
            u[0] = 0;
        } else {
            q[j++] = POINT;
        }
    }
/* unnormalize */
    if (typ != OPMOD) {
        i = 0;
        while (i <= m) {
            if ((u[i] = q[i]) == POINT)
                dpv = i;
            i++;
        }
        k = vlen;
        k1 = dpv;
        while (k-- > 0) {
            while (k1 <= 0) {
                for (i = (++m); i > 0; i--)
                    u[i] = u[i - 1];
                k1++;
                u[0] = ZERO;
            }
            u[k1] = u[k1 - 1];
            u[--k1] = POINT;
            dpv = k1;
        }
        u[m] = EOL;
/* rounding */

        if (typ != OPDIV)
            u[dpv + 1] = EOL;
        else {
            k = dpv + partab.jobtab->precision;
            k1 = u[k + 1] >= FIVE;
            u[k + 1] = EOL;
            if (k1) {
                do {
                    if (u[k] != POINT) {
                        if ((carry = (u[k] == NINE)))
                            u[k] = ZERO;
                        else
                            u[k]++;
                    }
                    k--;
                } while (carry);
            }
        }
    } else {                            /* return the remainder */
        carry = 0;
        if (d > 1) {
            for (i = 0; i <= ulen; i++) {
                if (u[i] == point) {
                    u[i] = POINT;
                    dpu = i;
                } else {
                    u[i] = (j = carry + u[i]) / d + ZERO;
                    carry = j % d * NUMBASE;
                }
            }
        } else {
            for (i = 0; i <= ulen; i++)
                if (u[i] == point)
                    u[dpu = i] = POINT;
                else
                    u[i] += ZERO;
        }
        u[i] = EOL;
        if (d1 > 0) {
            u[i + 1] = EOL;
            u[i + 2] = EOL;
            i = dpu;
            for (j = 0; j < d1; j++) {
                u[i] = u[i - 1];
                i--;
            }
            u[i] = POINT;
        }
    }
/* remove trailing zeroes */
    i = 0;
    k = (-1);
    while (u[i] != EOL) {
        if (u[i] == POINT)
            k = i;
        i++;
    }
    i--;
    if (k >= 0) {
        while (u[i] == ZERO && i > k)
            u[i--] = EOL;
    }
/* remove trailing point */
    if (u[i] == POINT)
        u[i] = EOL;
/* remove leading zeroes */
    while (u[0] == ZERO) {
        i = 0;
        while ((u[i] = u[i + 1]) != EOL)
            i++;
    }
    if (u[0] == EOL) {
        u[0] = ZERO;
        u[1] = EOL;
        mi = 0;
    }
    if ((mi || plus) && (u[0] != ZERO)) {
        if (mi != plus) {
            i = strlen (u) + 1;
            do {
                u[i] = u[i - 1];
                i--;
            } while (i > 0);
            u[0] = MINUS;
        }
        if (plus)
        { s = runtime_add (u, vv);
	  if (s < 0) return s;
	}
    }
    strcpy (uu, u);
    return strlen(uu);
}                                       /* end runtime_div() */



short runtime_power (char *a, char *b)    /* raise a to the b-th power */
{   short s;
    char    c[MAX_NUM_BYTES + 2];
    char    d[4*(MAX_NUM_BYTES + 1)];

/* is a memory leak resulting in wrong results   */
/* with fractional powers, e.g. 2**(3/7)         */
/* even a value of 513 is too small              */
    char    e[MAX_NUM_BYTES + 2];
    register long i;
    register long j;

/* if zero or one there's not much to do */
    if (a[1] == EOL) {
	if (a[0] == ZERO) {
	    if (b[1] == EOL && b[0] == ZERO)
		return -ERRM9;
	    return 1;
	}				/* undef */
	if (a[0] == ONE)
	    return 1;
    }

#ifdef MV1_MAPM
    laaq = m_mtoapm(aa, &laa, a);
    if (laaq)
      m_apm_set_long(aa, laa);
    lbbq = m_mtoapm(bb, &lbb, b);
    if (lbbq)
      m_apm_set_long(bb, lbb);

    m_apm_pow(cc, partab.jobtab->precision, aa, bb);
    m_apmtom(a, cc);

    return strlen(a);
#endif

    if (b[0] == MINUS) {
	s = runtime_power (a, &b[1]);
	if (s < 0) return s;
	strcpy (c, a);
	a[0] = ONE;
	a[1] = EOL;
	s = runtime_div (a, c, OPDIV);
	return s;
    }
    if (b[1] == EOL) {
	switch (b[0]) {
	case ZERO:
	    a[0] = ONE;
	    a[1] = EOL;
	case ONE:
	    return strlen(a);
	case TWO:
	    strcpy (c, a);
	    return runtime_mul (a, c);
	}
    }
/* look for decimal point */
    e[0] = EOL;
    i = 0;
    while (b[i] != EOL) {
	if (b[i] == POINT) {
	    if (a[0] == MINUS) {
		return -ERRM9;
	    }				/* undefined case */
	    if (b[i + 1] == FIVE && b[i + 2] == EOL) {	/* half-integer: extra solution */
		if (i) {
		    strcpy (c, b);
		    s = runtime_add (b, c);
		    if (s < 0) return s;
		    s = runtime_power (a, b);
		    if (s < 0) return s;
		}
		s = g_sqrt (a);
		return s;
	    }
	    strcpy (e, &b[i]);
	    b[i] = EOL;
	    break;
	}
	i++;
    }
    strcpy (d, a);
    i = atoi (b);
    if (i == INT_MAX) return -ERRM92;

/* do it with a small number of multiplications                       */
/* the number of multiplications is not optimum, but reasonably small */
/* see donald e. knuth "The Art of Computer Programming" Vol.II p.441 */
    if (i == 0) {
	a[0] = ONE;
	a[1] = EOL;
    } else {
	j = 1;
	while (j < i) {
	    j = j * 2;
	    if (j < 0) {
		return -ERRM92;
	    }
	}
	if (i != j)
	    j = j / 2;
	j = j / 2;
	while (j) {
	    strcpy (c, a);
	    s = runtime_mul (a, c);
	    if (s < 0) return s;
	    if (i & j) {
		strcpy (c, d);
		runtime_mul (a, c);
		if (s < 0) return s;
	    }
	    j = j / 2;
	}
	if (e[0] == EOL)
	    return strlen(a);
    }
/* non integer exponent */
/* state of computation at this point: */
/* d == saved value of a */
/* a == d^^int(b); *//* e == frac(b); */
    {
	char    Z[MAX_NUM_BYTES + 2];

/* is fraction the inverse of an integer? */
	Z[0] = ONE;
	Z[1] = EOL;
	strcpy (c, e);
	s = runtime_div (Z, c, OPDIV);
	if (s < 0) return s;
	i = 0;
	for (;;)
	{
	    if ((j = Z[i++]) == EOL) {
		j = atoi (Z);
		if (j == INT_MAX) return -ERRM92;
		break;
	    }
	    if (j != POINT)
		continue;
	    j = atoi (Z);
	    if (j == INT_MAX) return -ERRM92;
	    if (Z[i] == NINE)
		j++;
	    break;
	}
	Z[0] = ONE;
	Z[1] = EOL;
	s = itocstring((u_char *) c, j);
	if (s < 0) return s;
	s = runtime_div (Z, c, OPDIV);
/* if integer */

	if (strcmp (Z, e) == 0) {
	    strcpy (Z, d);
	    s = root (Z, j);
	    if (s < 0) {
		s = runtime_mul (a, Z);
		return s;
	    }				/* on error try other method */
	}
	Z[0] = ONE;
	Z[1] = EOL;
	partab.jobtab->precision += 2;
	for (;;)
	{
	    c[0] = TWO;
	    c[1] = EOL;
	    s = runtime_mul (e, c);
	    if (s < 0) return s;
	    s = g_sqrt (d);
	    if (s < 0) return s;
	    if (e[0] == ONE) {
		e[0] = ZERO;
//		numlit (e);		// not required !!
		strcpy (c, d);
		s = runtime_mul (Z, c);
		if (s < 0) return s;
		roundit (Z, partab.jobtab->precision);
	    }
	    if (e[0] == ZERO)
		break;
	    i = 0;
	    j = (d[0] == ONE ? ZERO : NINE);
	    for (;;) {
		++i;
		if (d[i] != j && d[i] != '.')
		    break;
	    }
	    if (d[i] == EOL || (i > partab.jobtab->precision))
		break;
	}
	partab.jobtab->precision -= 2;
	s = runtime_mul (a, Z);
	if (s < 0) return s;
	roundit (a, partab.jobtab->precision + 1);
    }
    return strlen(a);


}                                       /* end power() */

short runtime_comp (char *s, char *t)   /* s and t are strings representing */
                                        /* MUMPS numbers. comp returns t>s  */
{   int s1;
    int t1;

#ifdef MV1_MAPM
    int ret;

    laaq = m_mtoapm(aa, &laa, s);
    lbbq = m_mtoapm(bb, &lbb, t);

    if (laaq && lbbq)
    { ret = lbb > laa ? 1 : 0;
    }
    else
    { if (laaq)
        m_apm_set_long(aa, laa);
      if (lbbq)
        m_apm_set_long(bb, lbb);

      ret = m_apm_compare(bb, aa);
    }

    return ret > 0 ? TRUE : FALSE;
#endif

#ifdef MV1_INTMATH
    if (!strton(s, &aa) && !strton(t, &bb))
      return bb > aa ? TRUE : FALSE;
#endif

    s1 = s[0];
    t1 = t[0];

    if (s1 != t1) {
        if (s1 == '-')
            return TRUE;                /* s<0<t */
        if (t1 == '-')
            return FALSE;               /* t<0<s */
        if (s1 == POINT && t1 == '0')
            return FALSE;               /* s>0; t==0 */
        if (t1 == POINT && s1 == '0')
            return TRUE;                /* t>0; s==0 */
    }
    if (t1 == '-') {
        char   *a;

        a = &t[1];
        t = &s[1];
        s = a;
    }
    s1 = 0;
    while (s[s1] > POINT)
        s1++;                           /* Note: EOL<'.' */
    t1 = 0;
    while (t[t1] > POINT)
        t1++;
    if (t1 > s1)
        return TRUE;
    if (t1 < s1)
        return FALSE;
    while (*t == *s) {
        if (*t == EOL)
            return FALSE;
        t++;
        s++;
    }
    if (*t > *s)
        return TRUE;
    return FALSE;

}                                       /* end of runtime_comp() */


/******************************************************************************/

short g_sqrt (char *a)			/* square root */
{   int i, ch;
    short s;


    if (a[0] == ZERO)
	return 1;
    if (a[0] == MINUS) {
	return -ERRM9;
    }

#ifdef MV1_MAPM
    laaq = m_mtoapm(aa, &laa, a);
    if (laaq)
      m_apm_set_long(aa, laa);
    m_apm_sqrt(cc, partab.jobtab->precision, aa);
    m_apmtom(a, cc);

    return strlen(a);
#endif

    {
	char    tmp1[MAX_NUM_BYTES +2 ],
	        tmp2[MAX_NUM_BYTES +2 ],
	        XX[MAX_NUM_BYTES +2 ],
	        XXX[MAX_NUM_BYTES +2 ];

	strcpy (XX, a);
/* look for good initial value */
	if (a[0] > ONE || (a[0] == ONE && a[1] != POINT)) {
	    i = 0;
	    while ((ch = a[i++]) != EOL) {
		if (ch == POINT)
		    break;
	    }
	    if ((i = (i + 1) / 2))
		a[i] = EOL;
	} else if (a[0] != ONE) {
	    a[0] = ONE;
	    a[1] = EOL;
	}
/* "Newton's" algorithm with quadratic convergence */
	partab.jobtab->precision++;
	do {
	    strcpy (XXX, a);
	    strcpy (tmp1, XX);
	    strcpy (tmp2, a);
	    s = runtime_div (tmp1, tmp2, OPDIV);
	    if (s < 0) return s;
	    s = runtime_add (a, tmp1);
	    if (s < 0) return s;
	    tmp2[0] = TWO;
	    tmp2[1] = EOL;
	    s = runtime_div (a, tmp2, '/');
	    if (s < 0) return s;
	} while (runtime_comp (a, XXX));
	partab.jobtab->precision--;
	return strlen(a);
    }
}					/* end g_sqrt() */
/******************************************************************************/

short root (char *a, int n)		/* n.th root */
{   int i, ch;
    short s;

    if (a[0] == ZERO)
	return 1;
    if (a[0] == MINUS || n == 0) {
	return -ERRM9;
    }

#ifdef MV1_MAPM
    laaq = m_mtoapm(aa, &laa, a);
    if (laaq)
      m_apm_set_long(aa, laa);
    m_apm_set_double(bb, 1.0 / (double) n);

    m_apm_pow(cc, partab.jobtab->precision, aa, bb);
    m_apmtom(a, cc);

    return strlen(a);
#endif

    {
	char    tmp1[MAX_NUM_BYTES +2],
	        tmp2[MAX_NUM_BYTES +2],
	        XX[MAX_NUM_BYTES +2],
	        XXX[MAX_NUM_BYTES +2];
	short   again;

	strcpy (XX, a);
/* look for good initial value */
	if (a[0] > ONE || (a[0] == ONE && a[1] != POINT)) {
	    i = 0;
	    while ((ch = a[i++]) != EOL && ch != POINT) ;
	    if ((i = (i + n - 2) / n) > 0) {
		a[0] = THREE;
		a[i] = EOL;
	    }
	} else if (a[0] != ONE) {
	    a[0] = ONE;
	    a[1] = EOL;
	}
/* "Newton's" algorithm with quadratic convergence */

	if (partab.jobtab->precision <= 3)
	    again = 0;			/* speedup div with small zprec. */
	else {
	    again = partab.jobtab->precision;
	    partab.jobtab->precision = 2;
	}
      second:;
	partab.jobtab->precision++;
	for (;;) {
	    strcpy (XXX, a);
	    s = itocstring((u_char *) tmp1, n - 1);
	    if (s < 0) return s;
	    strcpy (tmp2, a);
	    s = runtime_power (tmp2, tmp1);
	    if (s < 0) return s;
	    strcpy (tmp1, XX);
	    s = runtime_div (tmp1, tmp2, OPDIV);
	    if (s < 0)
		break;
	    s = itocstring((u_char *) tmp2, n - 1);
	    if (s < 0) return s;
	    s = runtime_mul (a, tmp2);
	    if (s < 0) return s;
	    s = runtime_add (a, tmp1);
	    if (s < 0) return s;
	    s = itocstring((u_char *) tmp2, n);
	    if (s < 0) return s;
	    s = runtime_div (a, tmp2, OPDIV);
	    if (s < 0) return s;
	    strcpy (tmp2, a);
	    s = runtime_div (XXX, tmp2, OPDIV);
	    if (s < 0) return s;
	    tmp2[0] = ONE;
	    if (partab.jobtab->precision <= 0)
		tmp2[1] = EOL;
	    else {
		tmp2[1] = POINT;
		for (i = 2; i < partab.jobtab->precision; i++)
		    tmp2[i] = ZERO;
		tmp2[i++] = FIVE;
		tmp2[i] = EOL;
	    }
	    if (!runtime_comp (XXX, tmp2))
		continue;
	    if (partab.jobtab->precision <= 0)
		break;
	    tmp2[0] = POINT;
	    for (i = 1; i < partab.jobtab->precision; i++)
		tmp2[i] = NINE;
	    tmp2[i - 1] = FIVE;
	    tmp2[i] = EOL;
	    if (runtime_comp (tmp2, XXX))
		break;
	}
	partab.jobtab->precision--;
	if (again) {
	    partab.jobtab->precision = again;
	    again = 0;
	    goto second;
	}
	return strlen(a);
    }
}					/* end root() */

/******************************************************************************/
        /* rounding */
        /* 'a' is assumed to be a 'canonic' numeric string  */
        /* it is rounded to 'digits' fractional digits      */
void roundit (char *a, int digits)
{
    int     ch,
            i,
            pointpos,
            lena;

#ifdef MV1_MAPM
    laaq = m_mtoapm(aa, &laa, a);
    if (laaq)
      m_apm_set_long(aa, laa);
    m_apm_round(cc, digits, aa);

    m_apmtom(a, cc);

    return;
#endif

    pointpos = -1;
    i = 0;
    i = 0;
    while (a[i] != EOL) {
        if (a[i] == POINT)
            pointpos = i;
        i++;
    }
    lena = i;
    if (pointpos < 0)
        pointpos = i;
    if ((pointpos + digits + 1) >= i)
        return;                         /* nothing to round */
    i = pointpos + digits + 1;
    if (a[i] < FIVE) {
        a[i] = EOL;
        while (a[--i] == ZERO)
            a[i] = EOL;
        if (a[i] == POINT) {
            a[i] = EOL;
            if (i == 0 || (i == 1 && a[0] == MINUS))
                a[0] = ZERO;
        }
        return;
    }
    for (;;)
    {
        if (i >= pointpos)
            a[i] = EOL;
        else
            a[i] = ZERO;
        if (--i < (a[0] == MINUS)) {
            for (i = lena; i >= 0; i--)
                a[i + 1] = a[i];
            a[a[0] == '-'] = ONE;
            break;
        }
        if ((ch = a[i]) == POINT)
            continue;
        if (a[i] < NINE && ch >= ZERO) {
            a[i] = ++ch;
            break;
        }
    }
    return;
}                                       /* end roundit */
