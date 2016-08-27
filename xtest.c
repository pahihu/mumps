#include <stdio.h>
#include <stdlib.h>
#include "mv1conn.h"

int main(int argc,char **argv)
{
  MV1CONN conn;
  int res;

  res = MV1_Initialize(&conn, "testdb", "MGR");
  printf("connect: %d\n", res);
  res = MV1_Xecute(&conn, "s ^conn=42");
  printf("xecute: %d\n", res);
  res = MV1_Rundown(&conn);
  printf("disconnect: %d\n", res);
  return 0;
}
