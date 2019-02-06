#ifndef _DECNUMBER_H
#define _DECNUMBER_H

#include <stdint.h>

#define DECmantissa_t	int64_t
#define DECumantissa_t	uint64_t
#define DECexponent_t	int

typedef struct _DECIMAL {
  DECmantissa_t m;
  DECexponent_t x;
} DECIMAL ;

#define DEC_MINEXP	(-128)
#define DEC_MAXEXP	( 127)
#define DEC_MAXDIGITS	(  18)

DECIMAL* decADD(DECIMAL *a, DECIMAL *b, DECIMAL *z);
DECIMAL* decSUB(DECIMAL *a, DECIMAL *b, DECIMAL *z);
DECIMAL* decMUL(DECIMAL *a, DECIMAL *b, DECIMAL *z);
DECIMAL* decDIV(DECIMAL *a, DECIMAL *b, DECIMAL *z);

int      decCMP(DECIMAL *a, DECIMAL *b);
void     decTRUNC(DECIMAL *a, DECIMAL *intZ, DECIMAL *fracZ);
DECIMAL* decINT(DECIMAL *a, DECIMAL *z);
DECIMAL* decMOD(DECIMAL *a, DECIMAL *b, DECIMAL *z);
DECIMAL* decNORM(DECIMAL *z);
DECIMAL* decPOW(DECIMAL *a, DECIMAL *b, DECIMAL *z);
DECIMAL* decIPOW(DECIMAL *a, DECIMAL *b, DECIMAL *z);

char *dectoa(DECIMAL *a, char *buf, int radix);
DECIMAL* atodec(char *buf, int len, DECIMAL *z);

#endif
