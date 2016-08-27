#include <stdio.h>
#include <stdlib.h>
#include "mv1api.h"

int main(int argc,char **argv)
{
  MV1DB hnd;
  int res;

  res = mv1_initialize(&hnd, "testdb", "MGR");
  printf("connect: %d\n", res);
  res = mv1_xecute(&hnd, "s ^conn=42");
  printf("xecute: %d\n", res);
  res = mv1_rundown(&hnd);
  printf("disconnect: %d\n", res);
  return 0;
}
