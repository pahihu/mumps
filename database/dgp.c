#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>					// for u_char
#include <arpa/inet.h>					// for ntoh() stuff
#include <time.h>					// for time()
#include "error.h"
#include "mumps.h"
#include "proto.h"
#include "dgp.h"
#include "database.h"
#include "dgp_database.h"

#ifdef MV1_DGP
#include <nanomsg/nn.h>
#include <nanomsg/reqrep.h>
#else
#include <errno.h>
#define NN_HAUSNUMERO	0
#define AF_SP 		0
#define NN_REQ 		0
#define NN_SOL_SOCKET   0
#define NN_SNDTIMEO     0
#define NN_RCVTIMEO     0
int nn_errno() { return EINVAL; }
const char* nn_strerror { return NULL; }
int nn_socket(int domain, int protocol) { return EINVAL; }
int nn_close(int s) { return EBADF; }
int nn_bind(int s, const char *addr) { return EINVAL; }
int nn_connect(int s, const char *addr) { return EINVAL; }
int nn_shutdown(int s, int how) { return EINVAL; }
int nn_send(int s, const void *buf, size_t len, int flags) { return EINVAL; }
int nn_recv(int s, void *buf, size_t len, int flags) { return EINVAL; }
int nn_setsockopt(int s, int lvl, int opt, const void *optval, size_t optvallen) { return EINVAL; }
#endif

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

  lasterr = nn_errno();
  if (lasterr > NN_HAUSNUMERO)
    lasterr -= NN_HAUSNUMERO;
  return lasterr + DGP_HAUSNUMERO;
}


const char*
DGP_StrError(int err)
{
  err -= DGP_HAUSNUMERO;
  return nn_strerror(err);
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


short DGP_GetRemoteVOL(const char* uri, char *buf)
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


const char* DGP_GetServerURL(const char* base_url, int port)
{ static char buf[256];

  sprintf(buf, "%s%c%d",
		base_url,
		(strncmp(base_url,"ipc://",6) ? ':' : '.'),
		port);
  return buf;
}


static
short DGP_Dialog2(int vol, DGPRequest *req, DGPReply *rep, int do_restart);


short DGP_Connect(int vol)
{ int sock;						// NN socket
  int rv;						// return value
  short s;						// status
  u_char remote_vollab[SIZEOF_LABEL_BLOCK];		// remote VOL label
  label_block *remote_label;
  DGPRequest req;
  DGPReply rep;
  char conn_url[VOL_FILENAME_MAX];			// NN URL
  char remote_name[MAX_NAME_BYTES];
  int i;						// handy int
  int to;                                               // timeout

  if (strlen(systab->vol[vol]->file_name) < 6)		// tcp://
  { return -(ERRM38);
  }
  s = DGP_GetConnectionURL(systab->vol[vol]->file_name, conn_url);
  if (s < 0) return s;
  s = DGP_GetRemoteVOL(systab->vol[vol]->file_name, remote_name);
  if (s < 0) return s;
  // fprintf(stderr,"VOL%d ConnectionURL=[%s] RemoteVOL=[%s]\r\n", vol, conn_url, remote_name);
  sock = nn_socket(AF_SP, NN_REQ);
  if (sock < 0)
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  rv = nn_connect(sock, conn_url);
  if (rv < 0)
  { nn_close(sock);
    return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  to = systab->dgpRCVTO;                                // set recv timeout
  if (-1 != to) to *= 1000;                             //   in milliseconds
  rv = nn_setsockopt(sock, NN_SOL_SOCKET, NN_RCVTIMEO, &to, sizeof(to));
  if (rv < 0)
  { nn_close(sock);
    return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  if (systab->vol[vol]->vollab == NULL)			// no remote VOL label
  { DGP_MkRequest(&req, DGP_MNTV, 0, NULL, -1, (u_char *) &remote_name[0]);
    s = DGP_Dialog2(-(sock + 1), &req, &rep, 0);
    if (s < 0)
    { nn_shutdown(sock, 0);
      nn_close(sock);
      return s;
    }
    if (SIZEOF_LABEL_BLOCK != rep.data.len)
    { s = -(ERRZ81+ERRMLAST);
      nn_shutdown(sock, 0);
      nn_close(sock);
      return s;
    }
    bcopy(&rep.data.buf[0], &remote_vollab[0], SIZEOF_LABEL_BLOCK);
    // cvt to host fmt
    remote_label = (label_block *) &remote_vollab[0];
    remote_label->magic        = ntohl(remote_label->magic);
    remote_label->max_block    = ntohl(remote_label->max_block);
    remote_label->header_bytes = ntohl(remote_label->header_bytes);
    remote_label->block_size   = ntohl(remote_label->block_size);
    remote_label->db_ver       = ntohs(remote_label->db_ver);
    for (i = 0; i < UCIS; i++)
      remote_label->uci[i].global = ntohl(remote_label->uci[i].global);
    remote_label->txid         = ntohll(remote_label->txid);
  }
  while (SemOp( SEM_SYS, -systab->maxjob));
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

  ASSERT((0 <= vol) && (vol < MAX_VOL));

  if (-1 == partab.dgp_sock[vol])
    return -(ERRZ89+ERRMLAST);
  rv = nn_shutdown(partab.dgp_sock[vol], 0);
  nn_close(partab.dgp_sock[vol]);
  partab.dgp_sock[vol] = -1;
  if (rv < 0)
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  return 0;
}


short DGP_ReplSYSID(int i)                              // return client SYSID
{ short s;

  if (systab->dgp_repl_clients[i])                      // alread connected?
    return systab->dgp_repl_clients[i];                 //   return client SYSID

  s = DGP_ReplPing(i);                                  // ping client
  if (s < 0)
    return s;
  systab->dgp_repl_clients[i] = s;                      // save client SYSID
  return s;
}


short DGP_ReplPing(int i)                               // get repl.client SYSID
{ DGPRequest req;
  DGPReply rep;
  short s;

  DGP_MkRequest(&req, DGP_PING, 0, NULL, 0, NULL);
  s = DGP_ReplDialog(i, &req, &rep);
  if (s < 0)
     return s;
  bcopy(&rep.data.buf[0], &s, sizeof(short));
  s = ntohs(s);
  // fprintf(stderr,"ReplSYSID: replica%d got %d\r\n",i,s);
  systab->dgp_repl_lastclnchk[i] = MTIME(0);            // save time
  return s;
}


short DGP_ReplPingSend(int i)                           // get repl.client SYSID
{ DGPRequest req;

  DGP_MkRequest(&req, DGP_PING, 0, NULL, 0, NULL);
  return DGP_ReplDialogSend(i, &req);
}


short DGP_ReplPingRecv(int i)
{ short s;
  DGPReply rep;

  s = DGP_ReplDialogRecv(i, &rep);
  if (s < 0)
     return s;
  bcopy(&rep.data.buf[0], &s, sizeof(short));
  s = ntohs(s);
  // fprintf(stderr,"ReplSYSID: replica%d got %d\r\n",i,s);
  systab->dgp_repl_lastclnchk[i] = MTIME(0);            // save time
  return s;
}


short DGP_ReplStart(int i)                              // start replication
{ DGPRequest req;
  DGPReply rep;

  // on the CLIENT (ie. the replica)
  // - enter server SYSID in dgp_repl_servers[]
  // - save time in dgp_repl_lastsrvchk[]
  DGP_MkRequest(&req, DGP_RSTA, 0, NULL, 0, NULL);
  return DGP_ReplDialog(i, &req, &rep);
}


short DGP_ReplEnd(int i)                                // end replication
{ DGPRequest req;
  DGPReply rep;

  // on the CLIENT (ie. the replica)
  // - remove server SYSID from dgp_repl_servers[]
  // - clear time in dgp_repl_lastsrvchk[]
  DGP_MkRequest(&req, DGP_REND, 0, NULL, 0, NULL);
  return DGP_ReplDialog(i, &req, &rep);
}


short DGP_ReplConnect(int i, int p_getsysid)
{ int sock;						// NN socket
  int rv;						// return value
  short s;                                              // for functions
  int to;                                               // timeout

  ASSERT((0 <= i) && (i < MAX_REPLICAS));

  if (strlen(systab->replicas[i].connection) < 6)	// tcp://
  { return -(ERRM38);
  }
  sock = nn_socket(AF_SP, NN_REQ);
  if (sock < 0)
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  to = systab->dgpREPCLNTO;                             // set recv timeout
  if (-1 != to) to *= 1000;                             //   in milliseconds
  rv = nn_setsockopt(sock, NN_SOL_SOCKET, NN_RCVTIMEO,  // RECV timeout
                                        &to, sizeof(to));
  if (rv < 0)
  { nn_close(sock);
    return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  rv = nn_setsockopt(sock, NN_SOL_SOCKET, NN_SNDTIMEO,  // SEND timeout
                                        &to, sizeof(to));
  if (rv < 0)
  { nn_close(sock);
    return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  rv = nn_connect(sock, systab->replicas[i].connection);
  if (rv < 0)
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);
  }
  // fprintf(stderr,"DGP_ReplConnect: %d @ %s\r\n",sock,systab->replicas[i].connection);
  partab.dgp_repl[i] = sock;
  if (p_getsysid)                                       // get client SYSID?
  { s = DGP_ReplSYSID(i);                               // get it!
    if (s < 0) return s;                                // failed? return
  }
  return 0;
}


short DGP_ReplDisconnect(int i)
{ int rv;
  short s;

  ASSERT((0 <= i) && (i < MAX_REPLICAS));

  if (DGP_REPL_DISCONNECTED == partab.dgp_repl[i])
    return -(ERRZ89+ERRMLAST);
  if (systab->replicas[i].enabled)                      // enabled?
  { s = DGP_ReplEnd(i);                                 // repl.end to CLIENT
  }
  rv = nn_shutdown(partab.dgp_repl[i], 0);
  nn_close(partab.dgp_repl[i]);

  partab.dgp_repl[i] = DGP_REPL_DISCONNECTED;
  systab->dgp_repl_clients[i] = 0;                      // clear client SYSID
  systab->dgp_repl_lastclnchk[i] = 0;                   // clear last client chk
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
{ short s;
  cstring varstr;

  if (var)
  { s = UTIL_String_Mvar(var, &varstr.buf[0], MAX_SUBSCRIPTS);
    if (s < 0) return s;
    varstr.buf[s] = '\0';
    varstr.len = s;
  }
  return DGP_MkRequestStr(req, code, flag, var ? &varstr : NULL, len, buf);
}


short DGP_MkRequestStr(DGPRequest *req,
		   u_char code,
		   u_char flag,
		   cstring *varstr,
		   short len,
		   const u_char *buf)
{ DGPData *data;
  int remjob;

  if (DGP_F_CASD & flag)                                // cascaded replication?
  { remjob = DGP_SYSJOB - 1;                            // current system job
  }
  else
  { remjob = systab->dgpID * MAX_JOB + MV1_PID;         // remote job number
  }
  ASSERT((MAX_JOB-1 < remjob) && (remjob < 0xFF100));   // validate

  if (buf && (-1 == len))
    len = strlen((char *) buf);

  req->header.code = code;
  req->header.version = DGP_VERSION;
  req->header.remjob  = remjob;
  req->header.hdrlen  = sizeof(DGPHeader);
  req->header.msgflag = flag;
  req->header.msglen  = sizeof(DGPHeader);

  req->data.len = 0;
  if (varstr)
  { bcopy(&varstr->buf[0], &req->data.buf[0], varstr->len);
    req->data.len = varstr->len;
    req->header.msglen += sizeof(short) + req->data.len;
  }

  if (buf)
  { if (varstr)						// use space
      data = (DGPData *) &req->data.buf[req->data.len];	//   after var
    else
      data = (DGPData *) &req->data;			// use data
    data->len = len;
    bcopy(buf, &data->buf[0], data->len);
    req->header.msglen += sizeof(short) + data->len;
    if (data != &req->data)				// NB. send() will cvt	
      data->len = htons(data->len);			// cvt to network fmt
  }

  return 0;
}


short DGP_MkLockRequest(DGPRequest *req,
		   u_char code,
		   int count,
		   const cstring *list,
		   int job)
{ DGPData *data;
  cstring cstr;

  ASSERT((0 < systab->dgpID) && (systab->dgpID < 255));// valid DGP system ID

  job--;
  if (255 != job / MAX_JOB)				// not a system message
  { ASSERT((0 <= job) && (job < MAX_JOB));		// valid job no.
    job += systab->dgpID * MAX_JOB;			// system JOB number
  }

  req->header.code    = code;
  req->header.version = DGP_VERSION;
  req->header.remjob  = job;
  req->header.hdrlen  = sizeof(DGPHeader);
  req->header.msgflag = 0;
  req->header.msglen  = sizeof(DGPHeader);

  if (list)
  { // NB. we send #count null terminated cstrings
    cstr.len = LCK_LockToString(count, (cstring *) &list->buf[0], &cstr.buf[0]);
    bcopy(&cstr, &req->data, cstr.len + sizeof(cstr.len));
    req->header.msglen += sizeof(req->data.len) + req->data.len;
  } else
  { req->data.len = 0;					// ULOK has no list
    req->header.msglen += sizeof(req->data.len);
  }
  data = (DGPData *) &(req->data.buf[req->data.len]);
  data->len = count;
  req->header.msglen += sizeof(data->len);

  data->len = htons(data->len);		                // cvt to network fmt

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
  rep->header.remjob  = DGP_SYSJOB - 1;		// send server ID
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
  val->len = htons(val->len);				// cvt to network fmt
}


short DGP_GetValue(DGPReply *rep, u_char *buf)
{ DGPData *val;
  short val_len;

  val = (DGPData *) &(rep->data.buf[rep->data.len]);
  val_len = ntohs(val->len);				// cvt to host fmt
  if (VAR_UNDEFINED != val_len)
  { bcopy(&val->buf[0], buf, val_len);
  }
  return val_len;
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


static
short DGP_Dialog2(int vol, DGPRequest *req, DGPReply *rep, int do_restart)
{ int bytes;						// bytes sent/received
  int sock;						// NN socket
  short s;						// status
  u_short msg_len;
  time_t wait_start;					// start of wait status

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
  if (systab->dgpDUMPMSG)
  { DGP_MsgDump("Dialog2   ", 1, &req->header, req->data.len, sock);
  }
  
  msg_len = req->header.msglen;				// cvt to network fmt
  req->header.remjob = htonl(req->header.remjob);
  req->header.msglen = htons(req->header.msglen);
  req->data.len      = htons(req->data.len);

ReSend:
  if (do_restart)
  { MEM_BARRIER;
    if (systab->dgpRESTART)				// RESTART phase ?
    { wait_start = MTIME(0);				// remember time
      while (systab->dgpRESTART)			// wait
      { if ((MTIME(0) - wait_start) > 2*(DGP_RESTARTTO))//   wait over ?
        { systab->dgpSTART[MV1_PID] = 0;		//   clear LOCK lost
          return -(ERRZ87+ERRMLAST);			//   send restart failed
        }
        Sleep(1);
        MEM_BARRIER;
      }
    }
    MEM_BARRIER;
    if (systab->dgpSTART[MV1_PID])			// got local START msg?
    { // NB. this is due to removed local LOCK
      //     which corresponds to dropped remote LOCK
      //	   due to server restart
      // fprintf(stderr,"send restart to JOB %d\r\n",MV1_PID+1); fflush(stderr);
      systab->dgpSTART[MV1_PID] = 0;			//  send only once
      return -(ERRZ86+ERRMLAST);			//  lost remove LOCKs
    }
  }

  bytes = nn_send(sock, req, msg_len, 0);		// send request
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }
  // fprintf(stderr, "sent request %d(len=%d)\r\n", req->header.code, req->header.msglen);
  rep->data.len = 0;
  bytes = nn_recv(sock, rep, sizeof(DGPReply), 0);	// receive reply
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }

  rep->header.remjob = ntohl(rep->header.remjob);	// cvt to host fmt
  rep->header.msglen = ntohs(rep->header.msglen);
  rep->data.len      = ntohs(rep->data.len);

  // fprintf(stderr, "received reply %d(len=%d)\r\n", rep->header.code, rep->header.msglen);
  if (systab->dgpDUMPMSG)
  { DGP_MsgDump("Dialog2   ", 0, &rep->header, rep->data.len, sock);
  }
  if (DGP_SER == rep->header.code)			// error reply ?
  { if (-(ERRZ85+ERRMLAST) == rep->data.len)		// server (re)START ?
    { int do_drop = 0;					// assume no drop
      if (0 == do_restart)				// skip RESTART ?
      { Sleep(1);					//   just wait a bit
      }
      // fprintf(stderr, "got server restart\r\n"); fflush(stderr);
      MEM_BARRIER;
      if (0 == systab->dgpRESTART)			// not in RESTART phase?
      { while (SemOp( SEM_SYS, WRITE));
        if (0 == systab->dgpRESTART)			//   still not?
        { systab->dgpRESTART = time(0) + DGP_RESTARTTO + 1;// do local RESTART
	  do_drop = 1;					//    drop remote LOCKs
        }
        SemOp( SEM_SYS, -WRITE);
        if (do_drop)
        { // fprintf(stderr,"before LCK_RemoveVOL(%d)\r\n",vol+1); fflush(stderr);
	  // NB. all local jobs who have a remote LOCK 
	  // on VOL got marked in dgpSTART[]
          LCK_RemoveVOL(vol + 1);			// remove LOCKs for VOL
          // fprintf(stderr,"after LCK_RemoveVOL\r\n"); fflush(stderr);
        }
      }
      goto ReSend;
    }
    else
      return rep->data.len;
  }
  return 0;						// done
}


short DGP_Dialog(int vol, DGPRequest *req, DGPReply *rep)
{ return DGP_Dialog2(vol, req, rep, 1);
}


short DGP_ReplDialog(int i, DGPRequest *req, DGPReply *rep)
{ int bytes;						// bytes sent/received
  int sock;						// NN socket
  short s;						// status
  u_short msg_len;

  ASSERT((0 <= i) && (i < MAX_REPLICAS));
  ASSERT(systab->replicas[i].connection[0]);		// ensure replica name

  sock = partab.dgp_repl[i];			
  if (DGP_REPL_DISCONNECTED == sock)			// not connected yet ?
  { s = DGP_ReplConnect(i, 1);				// connect DGP
    if (s < 0) return s;				// failed? return
    sock = partab.dgp_repl[i];				// get NN socket
  }
  if (systab->dgpDUMPMSG)
  { DGP_MsgDump("ReplDialog", 1, &req->header, req->data.len, sock);
  }
  
  msg_len = req->header.msglen;				// cvt to network fmt
  req->header.remjob = htonl(req->header.remjob);
  req->header.msglen = htons(req->header.msglen);
  req->data.len      = htons(req->data.len);

  bytes = nn_send(sock, req, msg_len, 0);		// send request
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }

  if (systab->dgpDUMPMSG)
  { req->header.remjob = ntohl(req->header.remjob);	// cvt to host fmt
    req->header.msglen = ntohs(req->header.msglen);
    req->data.len      = ntohs(req->data.len);
    // fprintf(stderr, "sent request %d(len=%d)\r\n", req->header.code, req->header.msglen);
  }

  rep->data.len = 0;
  bytes = nn_recv(sock, rep, sizeof(DGPReply), 0);	// receive reply
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }

  rep->header.remjob = ntohl(rep->header.remjob);	// cvt to host fmt
  rep->header.msglen = ntohs(rep->header.msglen);
  rep->data.len      = ntohs(rep->data.len);

  // fprintf(stderr, "received reply %d(len=%d)\r\n", rep->header.code, rep->header.msglen);
  if (systab->dgpDUMPMSG)
  { DGP_MsgDump("ReplDialog", 0, &rep->header, rep->data.len, sock);
  }
  if (DGP_SER == rep->header.code)			// error reply ?
  { return rep->data.len;				//   return error code
  }
  return 0;						// done
}


short DGP_ReplDialogSend(int i, DGPRequest *req)
{ int bytes;						// bytes sent/received
  int sock;						// NN socket
  short s;						// status
  u_short msg_len;

  ASSERT((0 <= i) && (i < MAX_REPLICAS));
  ASSERT(systab->replicas[i].connection[0]);		// ensure replica name

  sock = partab.dgp_repl[i];
  if (DGP_REPL_DISCONNECTED == sock)			// not connected yet ?
  { s = DGP_ReplConnect(i, 1);				// connect DGP
    if (s < 0) return s;				// failed? return
    sock = partab.dgp_repl[i];				// get NN socket
  }
  if (systab->dgpDUMPMSG)
  { DGP_MsgDump("RepDialogS", 1, &req->header, req->data.len, sock);
  }

  msg_len = req->header.msglen;				// cvt to network fmt
  req->header.remjob = htonl(req->header.remjob);
  req->header.msglen = htons(req->header.msglen);
  req->data.len      = htons(req->data.len);

  bytes = nn_send(sock, req, msg_len, 0);		// send request
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }

  if (systab->dgpDUMPMSG)
  { req->header.remjob = ntohl(req->header.remjob);	// cvt to host fmt
    req->header.msglen = ntohs(req->header.msglen);
    req->data.len      = ntohs(req->data.len);
    // fprintf(stderr, "sent request %d(len=%d)\r\n", req->header.code, req->header.msglen);
  }
  return 0;
}


short DGP_ReplDialogRecv(int i, DGPReply *rep)
{ int bytes;						// bytes sent/received
  int sock;						// NN socket

  ASSERT((0 <= i) && (i < MAX_REPLICAS));
  ASSERT(partab.dgp_repl[i] >= 0);                      // ensure connected

  sock = partab.dgp_repl[i];
  rep->data.len = 0;
  bytes = nn_recv(sock, rep, sizeof(DGPReply), 0);	// receive reply
  if (bytes < 0)					// failed ?
  { return -(DGP_ErrNo()+ERRMLAST+ERRZLAST);		//   return error
  }

  rep->header.remjob = ntohl(rep->header.remjob);	// cvt to host fmt
  rep->header.msglen = ntohs(rep->header.msglen);
  rep->data.len      = ntohs(rep->data.len);

  // fprintf(stderr, "received reply %d(len=%d)\r\n", rep->header.code, rep->header.msglen);
  if (systab->dgpDUMPMSG)
  { DGP_MsgDump("RepDialogR", 0, &rep->header, rep->data.len, sock);
  }
  if (DGP_SER == rep->header.code)			// error reply ?
  { return rep->data.len;				//   return error code
  }
  return 0;						// done
}


void DGP_MsgDump(const char *fn, int dosend, DGPHeader *header, short status, int sock)
{ const char *msg;
  const char* msgs[] = {
        "ERR ",
        "LOKV",
        "ULOK",
        "ZALL",
        "ZDAL",
        "GETV",
        "SETV",
        "KILV",
        "ORDV",
        "QRYV",
        "DATV",
        "ZINC",
        "-24-",
        "rSRV",
        "rSER",
        "PING",
        "RSTA",
        "REND"
  };

  msg = header->code == DGP_MNTV ? "MNTV" : msgs[header->code / 2];
  if (systab->dgpDUMPMSG > 1)
  { fprintf(stderr, dosend
		  ? ">>>SEND>>> (%s using %d @ %d)\r\n"
		  : "<<<RECV<<< (%s using %d @ %d)\r\n", fn, sock, getpid());
    fprintf(stderr, "    code = %d (%s)\r\n", header->code, msg);
    fprintf(stderr, " version = %d\r\n", header->version);
    fprintf(stderr, "  remjob = %d ($%X)\r\n", header->remjob, header->remjob);
    fprintf(stderr, "  hdrlen = %d\r\n", header->hdrlen);
    fprintf(stderr, " msgflag = %d\r\n", header->msgflag);
    fprintf(stderr, "  msglen = %d\r\n", header->msglen);
    fprintf(stderr, "data.len = %d\r\n", status);
  }
  else
  { fprintf(stderr, dosend
		  ? ">>>SEND>>> (%s using %d @ %d) "
		  : "<<<RECV<<< (%s using %d @ %d) ", fn, sock, getpid());
    fprintf(stderr, " %s", msg);
    fprintf(stderr, " $%05X", header->remjob);
    fprintf(stderr, " len = %d\r\n", status);
  }
  fflush(stderr);
}

#include "bswap.h"

uint64_t HToNLL(uint64_t hostlonglong)
{
#if __BYTE_ORDER == __BIG_ENDIAN
  return hostlonglong;
#else
  return bswap64(hostlonglong);
#endif
}


uint64_t NToHLL(uint64_t netlonglong)
{
#if __BYTE_ORDER == __BIG_ENDIAN
  return netlonglong;
#else
  return bswap64(netlonglong);
#endif
}


