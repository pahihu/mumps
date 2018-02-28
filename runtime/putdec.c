/* Douglas W. Jones
 *
 * http://homepage.divms.uiowa.edu/~jones/bcd/decimal.html
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

static
void putdec16( uint16_t n, char *buf )
{
        uint8_t d4, d3, d2, d1, q;
        uint16_t d0;

        d0 = n       & 0xF;
        d1 = (n>>4)  & 0xF;
        d2 = (n>>8)  & 0xF;
        d3 = (n>>12) & 0xF;

        d0 = 6*(d3 + d2 + d1) + d0;
        q = d0 / 10;
        d0 = d0 % 10;

        d1 = q + 9*d3 + 5*d2 + d1;
        q = d1 / 10;
        d1 = d1 % 10;

        d2 = q + 2*d2;
        q = d2 / 10;
        d2 = d2 % 10;

        d3 = q + 4*d3;
        q = d3 / 10;
        d3 = d3 % 10;

        d4 = q;

        // buf[0] =  d4 + '0';
#ifdef PUTDEC_TEST
	// printf("%u %u %u %u\n", d3, d2, d1, d0);
#endif
        buf[0] =  d3 + '0';
        buf[1] =  d2 + '0';
        buf[2] =  d1 + '0';
        buf[3] =  d0 + '0';
}

static
int putdec16sup( uint16_t n, char *buf )
{
        uint8_t d4, d3, d2, d1, q;
        uint16_t d0;
	int i;

        d0 = n       & 0xF;
        d1 = (n>>4)  & 0xF;
        d2 = (n>>8)  & 0xF;
        d3 = (n>>12) & 0xF;

        d0 = 6*(d3 + d2 + d1) + d0;
        q = d0 / 10;
        d0 = d0 % 10;

        d1 = q + 9*d3 + 5*d2 + d1;
        q = d1 / 10;
        d1 = d1 % 10;

        d2 = q + 2*d2;
        q = d2 / 10;
        d2 = d2 % 10;

        d3 = q + 4*d3;
        q = d3 / 10;
        d3 = d3 % 10;

        d4 = q;

#ifdef PUTDEC_TEST
	// printf("%u %u %u %u\n", d3, d2, d1, d0);
#endif

	i = 0;
	if ( d3 )
	{ buf[i++] = d3 + '0';
	}
        if ( d2 || i )
	  buf[i++] = d2 + '0';
        if ( d1 || i )
	  buf[i++] = d1 + '0';
        buf[i++] =  d0 + '0';
	return i;
}

int putdec( uint64_t n, char *buf )
{
	uint32_t d4, d3, d2, d1, d0, q;
	int i;

        d0 = n       & 0xFFFF;
        d1 = (n>>16) & 0xFFFF;
        d2 = (n>>32) & 0xFFFF;
        d3 = (n>>48) & 0xFFFF;

        d0 = 656 * d3 + 7296 * d2 + 5536 * d1 + d0;
        q = d0 / 10000;
        d0 = d0 % 10000;

        d1 = q + 7671 * d3 + 9496 * d2 + 6 * d1;
        q = d1 / 10000;
        d1 = d1 % 10000;

        d2 = q + 4749 * d3 + 42 * d2;
        q = d2 / 10000;
        d2 = d2 % 10000;

        d3 = q + 281 * d3;
        q = d3 / 10000;
        d3 = d3 % 10000;

        d4 = q;

#ifdef PUTDEC_TEST
	// printf("%u %u %u %u %u\n", d4, d3, d2, d1, d0);
#endif

	i = 0;

	if ( d4 )
          i = putdec16sup( d4, &buf[0] );

	if ( i )
  	{ putdec16( d3, &buf[i] );
	  i += 4;
	}
        else if ( d3 )
	  i = putdec16sup( d3, &buf[0] );

	if ( i )
        { putdec16( d2, &buf[i] );
	  i += 4;
	}
	else if ( d2 )
	  i = putdec16sup( d2, &buf[0] );

	if ( i )
  	{ putdec16( d1, &buf[i] );
	  i += 4;
	}
	else if ( d1 )
	  i = putdec16sup( d1, &buf[0] );

	if ( i )
	{ putdec16( d0, &buf[i] );
	  i += 4;
	}
	else
	  i = putdec16sup( d0, &buf[0] );

	buf[i] = 0;
	return i;
}


#if defined(PUTDEC_TEST)

int main(int argc, char *argv[])
{
	int i, j, N;
	uint64_t n;
	char buf[21];
	clock_t st, et;

	N = atoi(argv[1]);
	printf("sizeof(long) = %ld\n", sizeof(long));
 	srandom(31415926);
	st = clock();
	for (i = 0; i < N; i++) {
		n = (uint64_t) i /*random()*/;
		buf[20] = 0;
		putdec(n, &buf[0]);
		// j = 0; while (buf[j] && buf[j] == '0') j++;
		// if (buf[j] == 0) j--;
		// printf("%lu = %s\n", n, (char *)&buf[0]);
	}
	et = clock();
	printf("putdec() = %lu\n", (long)(et - st));

	st = clock();
	for (i = 0; i < N; i++) {
		n = random();
		sprintf((char *)&buf[0], "%lu", n);
	}
	et = clock();
	printf("sprintf() = %lu\n", (long)(et - st));

	return 0;
}

#endif
