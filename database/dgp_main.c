#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include "error.h"
#include "mumps.h"
#include "proto.h"
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
  if (buf)
  { bcopy(&rep.data.buf[0], buf, rep.data.len);
  }
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
  if (  -1 == dir)
  { if (var->slen &&
        var->key[var->slen - 2] == 255 &&
        var->key[var->slen - 1] == 0)
    { var->key[var->slen - 2] = 0;
    }
    flag += DGP_F_PREV;
  }
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


short DGP_Query(int vol, mvar *var, u_char *buf, int flags, cstring *dat)
{ DGPRequest req;
  DGPReply rep;
  short s;
  u_char flag;

  flag = 0;
  if (flags & GLO_PREV)
  { if (var->slen &&
        var->key[var->slen - 2] == 255 &&
        var->key[var->slen - 1] == 0)
    { var->key[var->slen - 2] = 0;
    }
    flag += DGP_F_PREV;
  }
  if (flags & GLO_NOUCI) flag += DGP_F_NOUCI;
  if (flags & GLO_NOVOL) flag += DGP_F_NOVOL;
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


short DGP_Translate(trantab *tt, mvar *var, cstring *varstr)
{ int i;                                        // handy int
  short s;                                      // for functions

  if (tt->ntab)
  { s = UTIL_TTFindIdx(tt, var);
    if (s < 0)
    { i = -s; i--;
      varstr->len = tt->tab[i].to_var_len;
      bcopy(&tt->tab[i].to_var[0], &varstr->buf[0], varstr->len);
      if (var->slen != 0)                       // if there are subscripts
      { i = UTIL_String_Key( &var->slen, &varstr->buf[varstr->len],
                                MAX_SUBSCRIPTS); //do the subscripts
        if (i < 0) return i;			// quit on error
        varstr->len += i;
      }
      varstr->buf[varstr->len] = '\0';          // terminate
      return varstr->len;                       // return len
    }
    return -1;
  }
  s = UTIL_String_Mvar(var, &varstr->buf[0], MAX_SUBSCRIPTS);
  if (s < 0) return s;
  varstr->buf[s] = '\0';
  varstr->len = s;

  return s;
}


DGPRequest replreq[MAX_REPLICAS];                       // repl. messages

short DGP_ReplSet(mvar *var, cstring *data)
{ DGPRequest req;
  DGPReply rep;
  short s;
  int i;
  cstring varstr;

  // fprintf(stderr,"DGP_ReplSet: SYSID=%X REPL_SYSID=%X data=%s\r\n",
  //            systab->dgpID, partab.dgpREPL_SYSID, data->buf); fflush(stderr);

  if (partab.daemon && (0 == partab.dgpREPL_SYSID))
    return 0;

  for (i = 0; i < MAX_REPLICAS; i++)
  { if (0 == systab->replicas[i].connection[0])         // no connection?
      break;                                            //   done
    if (0 == systab->replicas[i].enabled)               // not enabled?
      continue;                                         //   skip
    if (0 == systab->dgp_repl_clients[i])               // no SYSID yet?
    { s = DGP_ReplConnect(i, 1);                        //   connect
      if (s < 0) return s;                              // error? done
    }
    if (partab.dgpREPL_SYSID == systab->dgp_repl_clients[i])// do not send back
    { // fprintf(stderr,"DGP_ReplSet: dgpREPL_SYSID = %d, SYSID = %d\r\n",
      //        partab.dgpREPL_SYSID, systab->dgpID); fflush(stderr);
      continue;                                         //   REPL messages
    }
    s = DGP_Translate(&systab->replicas[i].tt,          // translate global
                                        var, &varstr);
    // fprintf(stderr,"DGP_ReplSet: Translate %d\r\n",s); fflush(stderr);
    if (s < 0) continue;                                // failed?, continue
    DGP_MkRequestStr(                                   // make request
                partab.daemon ? &replreq[i] : &req, DGP_SETV,
                systab->replicas[i].cascaded ? DGP_F_CASD : 0,
                &varstr, data->len, &data->buf[0]);     // repl. REQd
    if (!partab.daemon)
    { s = DGP_ReplDialog(i, &req, &rep);                // communicate
      // fprintf(stderr,"DGP_ReplSet: ReplDialog %d\r\n",s); fflush(stderr);
      if (s < 0) return -(ERRZ88+ERRMLAST);             // failed?, error
    }
  }
  return 0;
}


short DGP_ReplKill(mvar *var, int what)
{ DGPRequest req;
  DGPReply rep;
  short s;
  int i;
  cstring varstr;
  int cascade;

  if (partab.daemon && (0 == partab.dgpREPL_SYSID))
    return 0;

  for (i = 0; i < MAX_REPLICAS; i++)
  { if (0 == systab->replicas[i].connection[0])         // no connection?
      break;                                            //   done
    if (0 == systab->replicas[i].enabled)               // not enabled?
      continue;                                         //   skip
    if (0 == systab->dgp_repl_clients[i])               // no SYSID yet?
    { s = DGP_ReplConnect(i, 1);                        //   connect
      if (s < 0) return s;                              // error? done
    }
    if (partab.dgpREPL_SYSID == systab->dgp_repl_clients[i])// don't send back
      continue;                                         // REPL messages
    s = DGP_Translate(&systab->replicas[i].tt,          // translate global
                                        var, &varstr);
    if (s < 0) continue;                                // failed? skip
    cascade = systab->replicas[i].cascaded;
    DGP_MkRequestStr(                                   // make request
        partab.daemon ? &replreq[i] : &req, DGP_KILV,
        (cascade ? DGP_F_CASD : 0) | what,
        &varstr, 0, NULL);                              // repl. REQd
    if (!partab.daemon)
    { s = DGP_ReplDialog(i, &req, &rep);                // communicate
      if (s < 0) return -(ERRZ88+ERRMLAST);             // failed?, error
    }
  }
  return 0;
}


