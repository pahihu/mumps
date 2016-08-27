#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mv1api.h"


void apierr(const char* msg, int res)
{
  fprintf(stderr,"%s: %d\r\n", msg, res);
}

int main(int argc,char **argv)
{
  MV1DB hnd;
  MV1VAR var;
  u_char buf[128];
  int nbuf;

  apierr("connect", mv1_initialize(&hnd, "testdb", "MGR"));

  apierr("xecute", mv1_xecute(&hnd, "s ^conn=42"));
  apierr("var_init", mv1_var_init(&var));

  apierr("var_insert", mv1_var_insert(&var, "conn", NULL, NULL));
  apierr("global_get", mv1_global_get(&hnd, &var, buf, &nbuf));
  apierr((char*) buf, 999);

  strcpy((char*) buf, "121"); nbuf = 3;
  apierr("global_set", mv1_global_set(&hnd, &var, buf, nbuf));

  apierr("global_get", mv1_global_get(&hnd, &var, buf, &nbuf));
  apierr((char*) buf, 999);

  apierr("subs_count", mv1_subs_count(&var, &nbuf));
  apierr("count", nbuf);
  apierr("subs_append", mv1_subs_append(&var, (u_char*)"alpha", 0));
  apierr("subs_count", mv1_subs_count(&var, &nbuf));
  apierr("count", nbuf);
  apierr("subs_append", mv1_subs_append(&var, (u_char*)"4", 0));
  apierr("subs_count", mv1_subs_count(&var, &nbuf));
  apierr("count", nbuf);
  apierr("global_set", mv1_global_set(&hnd, &var, (u_char*)"4", 0));

  apierr("subs_clear", mv1_subs_clear(&var));
  apierr("global_data", mv1_global_data(&hnd, &var, &nbuf));
  apierr("data", nbuf);

  apierr("subs_append", mv1_subs_append(&var, (u_char*)"beta", 0));
  apierr("subs_append", mv1_subs_append(&var, (u_char*)"5", 0));
  apierr("global_set", mv1_global_set(&hnd, &var, (u_char*)"5", 0));

  apierr("subs_clear", mv1_subs_clear(&var));
  apierr("subs_append", mv1_subs_append(&var, (u_char*)"", 0));
  apierr("global_order", mv1_global_order(&hnd, &var, 1, buf, &nbuf));
  apierr((char*) buf, 999);
  //
  // FIXME: doesn't seems to be working
  apierr("subs_append", mv1_subs_insert(&var, 0, (u_char*)"", 0));
  apierr("subs_count", mv1_subs_count(&var, &nbuf));
  apierr("count", nbuf);
  apierr("global_order", mv1_global_order(&hnd, &var, -1, buf, &nbuf));
  apierr((char*) buf, 999);

  apierr("subs_clear", mv1_subs_clear(&var));
  apierr("subs_append", mv1_subs_append(&var, (u_char*)"", 0));
  apierr("global_order", mv1_global_order(&hnd, &var, -1, buf, &nbuf));
  apierr((char*) buf, 999);

  apierr("subs_clear", mv1_subs_clear(&var));
  apierr("subs_append", mv1_subs_append(&var, (u_char*)"alpha", 0));
  apierr("subs_append", mv1_subs_append(&var, (u_char*)"4", 0));
  // apierr("global_kill", mv1_global_kill(&hnd, &var));

  apierr("disconnect",  mv1_rundown(&hnd));
  return 0;
}
