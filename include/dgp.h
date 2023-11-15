#ifndef _MUMPS_DGP_H_
#define _MUMPS_DGP_H_

#include <unistd.h>
#include <stdint.h>
#include <arpa/inet.h>					// for ntoh() stuff

#include "mumps.h"

#if !defined(htonll) || !defined(ntohll)
uint64_t HToNLL(uint64_t hostlonglong);
uint64_t NToHLL(uint64_t netlonglong);
#define htonll	HToNLL
#define ntohll	NToHLL
#endif


#define DGP_VERSION	 1

#define DGP_ERR	 	 0
#define DGP_LOKV	 2	/* L ^GLB 	*/
#define DGP_ULOK	 4	/* L     	*/
#define DGP_ZALL	 6	/* L +^GLB 	*/
#define DGP_ZDAL	 8	/* L -^GLB	*/
#define DGP_GETV	10	/* $G(^GLB)	*/
#define DGP_SETV	12	/* S ^GLB=VAL	*/
#define DGP_KILV	14	/* K ^GLB	*/
#define DGP_ORDV	16	/* $O(^GLB[,DIR[,VAR]]) */
#define DGP_QRYV	18	/* $Q(^GLB[,DIR[,VAR]]) */
#define DGP_DATV	20	/* $D(^GLB[,VAR])	*/
#define DGP_ZINC	22	/* $ZINCREMENT(^GLB,DAT) */

#define DGP_SRV		26	/* normal response */
#define DGP_SER		28	/* error response, only status is sent */
#define DGP_SIDV        30      /* system id */

#define DGP_MNTV	64	/* mount remote VOL */

#define DGP_F_RDAT	128	/* request data for ORDV/QRYV/DATV */
#define DGP_F_PREV	 64	/* reverse direction for ORDV/QRYV  */
#define DGP_F_NOUCI      32     /* original var didn't had UCI */
#define DGP_F_NOVOL      16     /* original var didn't had VOL */
#define DGP_F_CASD        8     /* cascaded replication */

/* NB. vvv--- same encoding as in mumps.h ---vvv */ 
#define DGP_F_KSUBS	  2	/* KSUBSCRIPTS ^GLB */
#define DGP_F_KVAL	  1	/* KVALUE ^GLB */
#define DGP_F_KALL	(DGP_F_KSUBS+DGP_F_KVAL)	/* K ^GLB */

#define DGP_SYSJOB	(0x0FF000 + systab->dgpID)
#define DGP_SYSID(x)	(((x)-1)/MAX_JOB)
#define DGP_MAX_LOCKTO	60
#define DGP_MAX_ROUAGE	60
#define DGP_MAX_REPLTO  60
#define DGP_RESTARTTO	 5

#define ATTR_PACKED	__attribute__ ((__packed__))

typedef struct ATTR_PACKED DGPDATA
{ short len;
  u_char  buf[32768];
} DGPData;

typedef struct ATTR_PACKED DGPHEADER
{ u_char  code;			/* message code */
  u_char  version;		/* DGP version */
  u_int   remjob;		/* client remote JOB number,
				     OR JOB parameter for LOCK commands,
				     OR server PID */
  u_char  hdrlen;		/* length of the header */
  u_char  msgflag;		/* message flags */
  u_short msglen;		/* length of message including header */
} DGPHeader;

typedef struct ATTR_PACKED DGPREQUEST
{ DGPHeader header;
  DGPData   data;
  u_char    buf[1024];
} DGPRequest;

typedef struct ATTR_PACKED DGPREPLY
{ DGPHeader header;
  DGPData   data;
  u_char    buf[1024];
} DGPReply;

#define DGP_HAUSNUMERO  4096

const char *DGP_StrError(int err);

short DGP_GetConnectionURL(const char* uri, char *buf);
short DGP_GetRemoteVOL(const char *uri, char *buf);
const char* DGP_GetServerURL(const char* base_url, int port);
short DGP_Connect(int vol);
short DGP_Disconnect(int vol);

short DGP_Translate(trantab *tt, mvar *var, cstring *varstr);

short DGP_MkRequest(DGPRequest *req,
		    u_char code,
		    u_char flag,
		    mvar *var,
		    short len,
		    const u_char *buf);

short DGP_MkRequestStr(DGPRequest *req,
		    u_char code,
		    u_char flag,
		    cstring *varstr,
		    short len,
		    const u_char *buf);

short DGP_MkLockRequest(DGPRequest *req,
		    u_char code,
		    int count,
		    const cstring *list,
		    int job);

void DGP_MkError(DGPReply *rep, short s);
void DGP_MkValue(DGPReply *rep, short len, const u_char *buf);
void DGP_MkStatus(DGPReply *rep, short s);

void DGP_AppendValue(DGPReply *rep, short len, const u_char *buf);
short DGP_GetValue(DGPReply *rep, u_char *buf);

short DGP_Dialog(int vol, DGPRequest *req, DGPReply *rep);

void  DGP_MsgDump(const char* fn, int dosend, DGPHeader *header, short status, int sock);


short DGP_ReplConnect(int i);
short DGP_ReplDisconnect(int i);
short DGP_ReplDialog(int i, DGPRequest *req, DGPReply *rep);

#endif
