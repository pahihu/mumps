#ifndef _MUMPS_DGP_DATABASE_H_
#define _MUMPS_DGP_DATABASE_H_

short DGP_Kill(int vol, mvar *var, int what);
short DGP_Get(int vol, mvar *var, u_char *buf);
short DGP_Set(int vol, mvar *var, cstring *data);
short DGP_Order(int vol, mvar *var, u_char *buf, int dir, cstring *dat);
short DGP_Query(int vol, mvar *var, u_char *buf, int flags, cstring *dat);
short DGP_Data(int vol, mvar *var, u_char *buf, cstring *dat);
short DGP_ZIncrement(int vol, cstring *ret, mvar *var, cstring *expr);
short DGP_Lock(int vol, int count, cstring *list, int job);
short DGP_LockAdd(int vol, int count, cstring *list, int job);
short DGP_LockSub(int vol, int count, cstring *list, int job);
short DGP_UnLock(int vol, int job);

short DGP_ReplSet(mvar *var, cstring *data);
short DGP_ReplKill(mvar *var, int what);
short DGP_ReplSYSID(int i);

#endif
