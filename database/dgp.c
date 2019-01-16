#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include "error.h"
#include "mumps.h"
#include "proto.h"
#include "dgp.h"

#ifdef MV1_DGP
#include <nanomsg/nn.h>
#include <nanomsg/reqrep.h>
#else
#include <errno.h>
#define NN_HAUSNUMERO	0
#define AF_SP 		0
#define NN_REQ 		0
int nn_errno() { return EINVAL; }
int nn_socket(int domain, int protocol) { return EINVAL; }
int nn_close(int s);
int nn_bind(int s, const char *addr) { return EINVAL; }
int nn_connect(int s, const char *addr) { return EINVAL; }
int nn_shutdown(int s, int how) { return EINVAL; }
int nn_send(int s, const void *buf, size_t len, int flags) { return EINVAL; }
int nn_recv(int s, void *buf, size_t len, int flags) { return EINVAL; }
#endif

int dump_msg = 0;

/*
   - zarolas
     * helyben is kell zarolni, ha az sikerul, akkor lehet a tavoli zart
       probalni
     * tavoli rendszereknek kell sysid, ezzel a tavoli processzek egyediek
       lesznek

*/

static
short DGP_ErrNo(void)
{ int lasterr;

  lasterr = nn_errno() - NN_HAUSNUMERO;
  return lasterr + 4096;
}


short DGP_GetConnectionURL(const char *uri, char *buf)
{ char *ptr;
  int len;

  ptr = strrchr(uri, '/');
  if (NULL == ptr)
    return -(ERRZ83+ERRMLAST);
  len = ptr - uri;
  if (len + 1 > VOL_FILENAME_MAX)
    return -(ERRZ83+ERRMLAST);
  if (buf)
  { strncpy(buf, uri, len);
    buf[len] = '\0';
  }
  return len;
}


short DGP_GetRemoteName(const char* uri, char *buf)
{ char *ptr;
  int len;

  ptr = strrchr(uri, '/');
  if (NULL == ptr)
    return -(ERRZ83+ERRMLAST);
  ptr++;
  len = strlen(ptr);
  if (len + 1 > MAX_NAME_BYTES)
    return -(ERRZ83+ERRMLAST);
  if (buf)
  { strcpy(buf, ptr);
  }
  return len;
}


short DGP_Connect(int vol)
{ int sock;						// NN socket
  int rv;						// return value
  short s;						// status
  u_char remote_vollab[SIZEOF_LABEL_BLOCK];		// remote VOL label
  DGPRequest req;
  DGPReply rep;
  char conn_url[VOL_FILENAME_MAX];			// NN URL
  char remote_name[MAX_NAME_BYTES];

  if (strlen(systab->vol[vol]->file_name) < 6)		// tcp://
  { return -(ERRM38);
  }
  s = DGP_GetConnectionURL(systab->vol[vol]->file_name, conn_url);
  if (s < 0) return s;
  s = DGP_GetRemoteName(systab->vol[vol]->file_name, remote_name);
  if (s < 0) return s;
  // fprintf(stderr,"VOL%d ConnectionURL=[%s] RemoteName=[%s]\r\n", vol, conn_url, remote_name);
  sock = nn_socket(AF_SP, NN_REQ);
  if (sock < 0)
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  rv = nn_connect(sock, conn_url);
  if (rv < 0)
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  if (systab->vol[vol]->vollab == NULL)			// no remote VOL label
  { DGP_MkRequest(&req, DGP_MNTV, 0, NULL, -1, (u_char *) &remote_name[0]);
    s = DGP_Dialog(-(sock + 1), &req, &rep);
    if (s < 0)
    { nn_shutdown(sock, 0);
      return s;
    }
    if (SIZEOF_LABEL_BLOCK != rep.data.len)
    { s = -(ERRZ81+ERRMLAST);
      nn_shutdown(sock, 0);
      return s;
    }
    bcopy(&rep.data.buf[0], &remote_vollab[0], SIZEOF_LABEL_BLOCK);
  }
  SemOp( SEM_SYS, -systab->maxjob);
  if (systab->vol[vol]->vollab == NULL)
  { // fprintf(stderr,"VOL%d copying vollab\r\n",vol);
    systab->vol[vol]->vollab =
      (label_block *) ((void *) systab->vol[vol]->remote_vollab);
    bcopy(remote_vollab, systab->vol[vol]->vollab, SIZEOF_LABEL_BLOCK);
  }
  SemOp( SEM_SYS, systab->maxjob);
  partab.dgp_sock[vol] = sock;
  return 0;
}


short DGP_Disconnect(int vol)
{ int rv;

  rv = nn_shutdown(partab.dgp_sock[vol], 0);
  partab.dgp_sock[vol] = -1;
  if (rv < 0)
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  return 0;
}


short DGP_MkRequest(DGPRequest *req,
		   u_char code,
		   u_char flag,
		   mvar *var,
		   short len,
		   const u_char *buf)
{ DGPData *data;
  int s;						// status
  int remjob;

  remjob = systab->dgpID * 256 + MV1_PID;		// system JOB number
  ASSERT((255 < remjob) && (remjob < 65280));		// validate

  if (buf && (-1 == len))
    len = strlen((char *) buf);

  req->header.code = code;
  req->header.version = DGP_VERSION;
  req->header.remjob  = remjob;
  req->header.hdrlen  = sizeof(DGPHeader);
  req->header.msgflag = flag;
  req->header.msglen  = sizeof(DGPHeader);

  if (var)
  { 
#if 1
    s = UTIL_String_Mvar(var, &req->data.buf[0], MAX_SUBSCRIPTS);
    if (s < 0) return s;
    req->data.len = s;
#else
    // fprintf(stderr,"old var->volset=%d\n",var->volset);
    var->volset = systab->vol[var->volset-1]->vollab->clean;
    // fprintf(stderr,"new var->volset=%d\n",var->volset);
    req->data.len = MVAR_SIZE + var->slen;
    // fprintf(stderr,"req->data.len=%d\r\n", req->data.len); fflush(stderr);
    bcopy(var, &req->data.buf[0], req->data.len);
#endif
    req->header.msglen += sizeof(short) + req->data.len;
  }

  if (buf)
  { if (var)							// use space
      data = (DGPData *) &req->data.buf[req->data.len];		//   after var
    else
      data = (DGPData *) &req->data;				// use data
    data->len = len;
    bcopy(buf, &data->buf[0], data->len);
    req->header.msglen += sizeof(short) + data->len;
  }

  return 0;
}


short DGP_MkLockRequest(DGPRequest *req,
		   u_char code,
		   int count,
		   const cstring *list,
		   int job)
{ DGPData *data;

  ASSERT((0 < systab->dgpID) && (systab->dgpID < 255));// valid DGP system ID

  job--;
  if (255 != job / 256)					// not a system message
  { ASSERT((0 <= job) && (job < 256));			// valid job no.
    job += systab->dgpID * 256;				// system JOB number
  }

  req->header.code = code;
  req->header.version = DGP_VERSION;
  req->header.remjob  = job;
  req->header.hdrlen  = sizeof(DGPHeader);
  req->header.msgflag = 0;
  req->header.msglen  = sizeof(DGPHeader);

  if (list)
  { bcopy(list, &req->data, list->len + sizeof(short));
    req->header.msglen += sizeof(req->data.len) + req->data.len;
  } else
  { req->data.len = 0;					// ULOK has no list
    req->header.msglen += sizeof(req->data.len);
  }
  data = (DGPData *) &(req->data.buf[req->data.len]);
  data->len = count;
  req->header.msglen += sizeof(data->len);

  return 0;
}


static
void DGP_MkReply(DGPReply *rep,
		 u_char code,
		 short len,
		 const u_char *buf)
{ if (buf && (-1 == len ))
    len = strlen((char *) buf);

  rep->header.code    = code;
  rep->header.version = DGP_VERSION;
  rep->header.remjob  = 65280 /* 0xFF00 */ + systab->dgpID - 1;
  rep->header.hdrlen  = sizeof(DGPHeader);
  rep->header.msgflag = 0;
  rep->header.msglen  = sizeof(DGPHeader);

  if (buf)
  { rep->data.len = len;
    if (buf != &rep->data.buf[0])
      bcopy(buf, &rep->data.buf[0], rep->data.len);
    rep->header.msglen += sizeof(rep->data.len) + rep->data.len;
  } else
  { rep->data.len = len;
    rep->header.msglen += sizeof(rep->data.len);
  }
}


void DGP_AppendValue(DGPReply *rep, short len, const u_char *buf)
{ DGPData *val;

  val = (DGPData *) &(rep->data.buf[rep->data.len]);
  val->len = len;
  rep->header.msglen += sizeof(val->len);
  if (VAR_UNDEFINED != val->len)
  { bcopy(buf, &val->buf[0], val->len);
    rep->header.msglen += val->len;
  }
}


short DGP_GetValue(DGPReply *rep, u_char *buf)
{ DGPData *val;

  val = (DGPData *) &(rep->data.buf[rep->data.len]);
  if (VAR_UNDEFINED != val->len)
  { bcopy(&val->buf[0], buf, val->len);
  }
  return val->len;
}


void DGP_MkError(DGPReply *rep, short s)
{ DGP_MkReply(rep, DGP_SER, s, NULL);
}


void DGP_MkValue(DGPReply *rep, short len, const u_char *buf)
{ DGP_MkReply(rep, DGP_SRV, len, buf);
}


void DGP_MkStatus(DGPReply *rep, short s)
{ DGP_MkReply(rep, DGP_SRV, s, NULL);
}


short DGP_Dialog(int vol, DGPRequest *req, DGPReply *rep)
{ int bytes;						// bytes sent/received
  int sock;						// NN socket
  short s;						// status

  if (vol < 0)						// called with socket ?
  { // fprintf(stderr,"DGP_Dialog(%d): socket call\r\n",vol);
    sock = -vol - 1;
  }
  else
  { // fprintf(stderr,"DGP_Dialog(%d): volume call\r\n",vol); fflush(stderr);
    ASSERT(systab->vol[vol]->local_name[0]);		// ensure remote data
    sock = partab.dgp_sock[vol];			// called with vol
    if (-1 == sock)					// not connected yet ?
    { s = DGP_Connect(vol);				// connect DGP
      if (s < 0) return s;				// failed ? return
      sock = partab.dgp_sock[vol];			// get NN socket
    }
  }
  if (dump_msg)
  { DGP_MsgDump(1, &req->header, req->data.len);
  }
  bytes = nn_send(sock, req, req->header.msglen, 0);	// send request
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }
  // fprintf(stderr, "sent request %d(len=%d)\r\n", req->header.code, req->header.msglen);
  bytes = nn_recv(sock, rep, sizeof(DGPReply), 0);	// receive reply
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }
  // fprintf(stderr, "received reply %d(len=%d)\r\n", rep->header.code, rep->header.msglen);
  if (dump_msg)
  { DGP_MsgDump(0, &rep->header, rep->data.len);
  }
  if (DGP_SER == rep->header.code)			// error reply ?
  { return rep->data.len;				//   return error code
  }
  return 0;						// done
}

void DGP_MsgDump(int dosend, DGPHeader *header, short status)
{
  fprintf(stderr, dosend
		  ? ">>>>>>>>>>>>>\r\n"
		  : "<<<<<<<<<<<<<\r\n");
  fprintf(stderr, "    code = %d\r\n", header->code);
  fprintf(stderr, " version = %d\r\n", header->version);
  fprintf(stderr, "  remjob = %d\r\n", header->remjob);
  fprintf(stderr, "  hdrlen = %d\r\n", header->hdrlen);
  fprintf(stderr, " msgflag = %d\r\n", header->msgflag);
  fprintf(stderr, "  msglen = %d\r\n", header->msglen);
  fprintf(stderr, "data.len = %d\r\n", status);
  fflush(stderr);
}


