#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "decnumber.h"

int main(int argc, char *argv[])
{
  DECIMAL a, b, z;
  char tmp[256];

  switch(argc) {
  case 1:
  default:
    fprintf(stderr,"usage: test_decnumber n [<op> m]\n");
    return -1;
  case 2:
    atodec(argv[1], strlen(argv[1]), &a);
    printf("%s\n", dectoa(&a, tmp, 10));
    break;
  case 3:
    atodec(argv[1], strlen(argv[1]), &a);
    switch (*argv[2]) {
    case '_': decINT(&a, &z); break;
    default:
      fprintf(stderr,"unknown op: %c\n", *argv[2]);
      exit(-1);
    }
    printf("(%lld,%d) %s\n", z.m, z.x, dectoa(&z, tmp, 10));
    break;
  case 4:
    atodec(argv[1], strlen(argv[1]), &a);
    atodec(argv[3], strlen(argv[3]), &b);
    switch (*argv[2]) {
    case '+': decADD(&a, &b, &z); break;
    case '-': decSUB(&a, &b, &z); break;
    case '*': decMUL(&a, &b, &z); break;
    case '/': decDIV(&a, &b, &z); break;
    case '%': decMOD(&a, &b, &z); break;
    case '^': decPOW(&a, &b, &z); break;
    default:
      fprintf(stderr,"unknown op: %c\n", *argv[2]);
      exit(-1);
    }
    printf("(%lld,%d) %s\n", z.m, z.x, dectoa(&z, tmp, 10));
    break;
  }
  return 0;
}
