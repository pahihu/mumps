#include <strings.h>
#include <sys/types.h>
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
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkRequest(&req, DGP_SETV, 0, var, data->len, &data->buf[0]);
  s = DGP_Dialog(vol, &req, &rep);
  if (s < 0) return s;
  return rep.data.len;
}

