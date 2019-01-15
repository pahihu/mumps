#ifndef _MUMPS_DGP_H_
#define _MUMPS_DGP_H_

#include <unistd.h>

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

#define DGP_MNTV	64	/* mount remote VOL */

#define DGP_F_RDAT	128	/* request data for ORDV/QRYV/DATV */
#define DGP_F_PREV	 64	/* reverse direction for ORDV/QRYV  */

/* NB. vvv--- same encoding as in mumps.h ---vvv */ 
#define DGP_KSUBS	  2	/* KSUBSCRIPTS ^GLB */
#define DGP_KVAL	  1	/* KVALUE ^GLB */
#define DGP_KALL	(DGP_KSUBS+DGP_KVAL)	/* K ^GLB */

#define ATTR_PACKED	__attribute__ ((__packed__))

typedef struct ATTR_PACKED DGPDATA
{ short len;
  u_char  buf[32768];
} DGPData;

typedef struct ATTR_PACKED DGPHEADER
{ u_char  code;
  u_char  version;
  u_short sysjob;
  u_char  hdrlen;
  u_char  msgflag;
  u_short msglen;
} DGPHeader;

typedef struct ATTR_PACKED DGPREQUEST
{ DGPHeader header;
  DGPData   data;
} DGPRequest;

typedef struct ATTR_PACKED DGPREPLY
{ DGPHeader header;
  DGPData   data;
  u_char    buf[1024];
} DGPReply;

short DGP_GetConnectionURL(const char* uri, char *buf);
short DGP_GetRemoteName(const char *uri, char *buf);
short DGP_Connect(int vol);
short DGP_Disconnect(int vol);

short DGP_MkRequest(DGPRequest *req,
		    u_char code,
		    u_char flag,
		    mvar *var,
		    short len,
		    const u_char *buf);

void DGP_MkError(DGPReply *rep, short s);
void DGP_MkValue(DGPReply *rep, short len, const u_char *buf);
void DGP_MkStatus(DGPReply *rep, short s);

void DGP_AppendValue(DGPReply *rep, short len, const u_char *buf);
short DGP_GetValue(DGPReply *rep, u_char *buf);

short DGP_Dialog(int vol, DGPRequest *req, DGPReply *rep);


#endif
