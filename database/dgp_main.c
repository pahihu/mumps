#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include "error.h"
#include "mumps.h"
#include "dgp_database.h"
#include "dgp.h"


short DGP_Kill(int vol, mvar *var, int what)
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkRequest(&req, DGP_KILV, what, var, 0, NULL);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;
  return rep.data.len;
}


short DGP_Get(int vol, mvar *var, u_char *buf)
{ DGPRequest req;
  DGPReply rep;
  short s;
  
  DGP_MkRequest(&req, DGP_GETV, 0, var, 0, NULL);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;
  bcopy(&rep.data.buf[0], buf, rep.data.len);
  return rep.data.len;
}


short DGP_Set(int vol, mvar *var, cstring *data)
{ 
  DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkRequest(&req, DGP_SETV, 0, var, data->len, &data->buf[0]);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;
  return rep.data.len;
}


short DGP_ZIncrement(int vol, cstring *ret, mvar *var, cstring *expr)
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkRequest(&req, DGP_ZINC, 0, var, expr->len, &expr->buf[0]);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;
  ret->len = rep.data.len;
  bcopy(&rep.data.buf[0], &ret->buf[0], ret->len);
  return ret->len;
}


short DGP_Order(int vol, mvar *var, u_char *buf, int dir, cstring *dat)
{ DGPRequest req;
  DGPReply rep;
  short s;
  u_char flag;

  flag = 0;
  if (  -1 == dir) flag += DGP_F_PREV;
  if (NULL != dat) flag += DGP_F_RDAT;
  DGP_MkRequest(&req, DGP_ORDV, flag, var, 0, NULL);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;

  bcopy(&rep.data.buf[0], buf, rep.data.len);
  if (dat)
  { dat->len = DGP_GetValue(&rep, &dat->buf[0]);
  }
  return rep.data.len;
}


short DGP_Query(int vol, mvar *var, u_char *buf, int dir, cstring *dat)
{ DGPRequest req;
  DGPReply rep;
  short s;
  u_char flag;

  flag = 0;
  if (  -1 == dir) flag += DGP_F_PREV;
  if (NULL != dat) flag += DGP_F_RDAT;
  DGP_MkRequest(&req, DGP_QRYV, flag, var, 0, NULL);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;

  bcopy(&rep.data.buf[0], buf, rep.data.len);
  if (dat)
  { dat->len = DGP_GetValue(&rep, &dat->buf[0]);
  }
  return rep.data.len;
}


short DGP_Data(int vol, mvar *var, u_char *buf, cstring *dat)
{ DGPRequest req;
  DGPReply rep;
  short s;
  u_char flag;

  flag = 0;
  if (NULL != dat) flag += DGP_F_RDAT;
  DGP_MkRequest(&req, DGP_DATV, flag, var, 0, NULL);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;

  bcopy(&rep.data.buf[0], buf, rep.data.len);
  if (dat)
  { dat->len = DGP_GetValue(&rep, &dat->buf[0]);
  }
  return rep.data.len;
}


short DGP_Lock(int vol, int count, cstring *list, int job)
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkLockRequest(&req, DGP_LOKV, count, list, job);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;

  return rep.data.len;
}


short DGP_LockAdd(int vol, int count, cstring *list, int job)
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkLockRequest(&req, DGP_ZALL, count, list, job);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;

  return rep.data.len;
}


short DGP_LockSub(int vol, int count, cstring *list, int job)
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkLockRequest(&req, DGP_ZDAL, count, list, job);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;

  return rep.data.len;
}


short DGP_UnLock(int vol, int job)
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkLockRequest(&req, DGP_ULOK, 0, NULL, job);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;

  return rep.data.len;
}


short DGP_ReplSet(mvar *var, cstring *data)
{ DGPRequest req;
  DGPReply rep;
  short s;
  int i;

  DGP_MkRequest(&req, DGP_SETV, 0, var, data->len, &data->buf[0]);
  // required members
  for (i = 0; i < MAX_REPLICAS; i++)
  { if (0 == systab->replicas[i].connection[0])
      break;
    if (DGP_REPL_REQ == systab->replicas[i].typ)
    { s = DGP_ReplDialog(i, &req, &rep);
      if (s < 0) return -(ERRZ88+ERRMLAST);
    }
  }
  // optional members
  for (i = 0; i < MAX_REPLICAS; i++)
  { if (0 == systab->replicas[i].connection[0])
      break;
    if (DGP_REPL_OPT == systab->replicas[i].typ)
    { s = DGP_ReplDialog(i, &req, &rep);
    }
  }
  return 0;
}


short DGP_ReplKill(mvar *var, int what)
{ DGPRequest req;
  DGPReply rep;
  short s;
  int i;

  DGP_MkRequest(&req, DGP_KILV, what, var, 0, NULL);
  // required members
  for (i = 0; i < MAX_REPLICAS; i++)
  { if (0 == systab->replicas[i].connection[0])
      break;
    if (DGP_REPL_REQ == systab->replicas[i].typ)
    { s = DGP_ReplDialog(i, &req, &rep);
      if (s < 0) return -(ERRZ88+ERRMLAST);
    }
  }
  // optional members
  for (i = 0; i < MAX_REPLICAS; i++)
  { if (0 == systab->replicas[i].connection[0])
      break;
    if (DGP_REPL_OPT == systab->replicas[i].typ)
    { s = DGP_ReplDialog(i, &req, &rep);
    }
  }
  return 0;
}


