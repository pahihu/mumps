#ifndef _MUMPS_DGP_DATABASE_H_
#define _MUMPS_DGP_DATABASE_H_

short DGP_Kill(int vol, mvar *var, int what);
short DGP_Get(int vol, mvar *var, u_char *buf);
short DGP_Set(int vol, mvar *var, cstring *data);

#endif
