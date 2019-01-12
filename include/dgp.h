#ifndef _MUMPS_DGP_H_
#define _MUMPS_DGP_H_

#include <unistd.h>

#define DGP_VERSION	 1

#define DGP_ERR	 	 0
#define DGP_LOKV	 2
#define DGP_ULOK	 4
#define DGP_ZALL	 6
#define DGP_ZDAL	 8
#define DGP_GETV	10
#define DGP_SETV	12
#define DGP_KILV	14
#define DGP_ORDV	16
#define DGP_QRYV	18
#define DGP_DATV	20

#define DGP_SRV		26
#define DGP_SER		28

#define DGP_VLAB	64

#define DGP_RVAL	128
#define DGP_PREV	 64

#define ATTR_PACKED	__attribute__ ((__packed__))

typedef struct ATTR_PACKED DGPDATA
{ u_short len;
  u_char  buf[32768];
} DGPData;

typedef struct ATTR_PACKED DGPHEADER
{ u_char  code;
  u_char  version;
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
  u_char    buf[1024+32768];
} DGPReply;

short dgp_connect(int vol);
short dgp_mkrequest(DGPRequest *req,
		    u_char code,
		    u_char flag,
		    mvar *var,
		    short len,
		    const u_char *buf);
short dgp_disconnect(int vol);

#endif
