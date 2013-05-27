#ifndef __LINUX_KRPC_H
#define __LINUX_KRPC_H
/*
 * Copyright (C) 1999, 2007 IBM Corporation.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA
 *
 * Author: IBM Corporation
 * This module is part of the IBM (R) Rational (R) ClearCase (R)
 * Multi-version file system (MVFS).
 * For support, please visit http://www.ibm.com/software/support
 */

/*
 * The linux kernel build environment doesn't have a proper XDR or RPC
 * interface (all their NFS code is in-lined), so we declare a bunch of
 * cleanups for it here.
 */

typedef int bool_t;
typedef int enum_t;
enum xdr_op {
    XDR_ENCODE,                         /* XXX don't need values? */
    XDR_DECODE,
    XDR_FREE
};

typedef struct XDR {
    enum xdr_op x_op;
    struct rpc_rqst *x_rq;              /* in case we need it down
                                           the chain... */
    u8 *x_data;                         /* move to/from this location */
    u8 *x_origdata;                     /* starting point */
    u8 *x_limit;                        /* limit of buffer space available */
} XDR;
#define xdr_getpos(xdr) ((xdr)->x_data - (xdr)->x_origdata)

typedef int (*xdrproc_t)(XDR *xdr_ptr, void *obj_p);

#define CLIENT struct rpc_clnt

enum clnt_stat {
    MVFS_RPC_SUCCESS = 0,
    MVFS_RPC_PROGUNAVAIL,
    MVFS_RPC_PROCUNAVAIL,
    MVFS_RPC_AUTHERROR,
    MVFS_RPC_CANTENCODEARGS,
    MVFS_RPC_CANTDECODERES,
    MVFS_RPC_CANTDECODEARGS,
    MVFS_RPC_VERSMISMATCH,
    MVFS_RPC_PROGVERSMISMATCH,
    MVFS_RPC_INTR,
    MVFS_RPC_TIMEDOUT
};
/* Work around linux bogons: their success and reject stats are different
 * types with overlapping values.  classic CLNT stuff wants them merged
 * and distinct values.
 */
#define RPC_SUCCESS MVFS_RPC_SUCCESS
#define RPC_PROGUNAVAIL MVFS_RPC_PROGUNAVAIL
#define RPC_PROCUNAVAIL MVFS_RPC_PROCUNAVAIL
#define RPC_AUTHERROR MVFS_RPC_AUTHERROR
#define RPC_CANTENCODEARGS MVFS_RPC_CANTENCODEARGS
#define RPC_CANTDECODERES MVFS_RPC_CANTDECODERES
#define RPC_CANTDECODEARGS MVFS_RPC_CANTDECODEARGS
#define RPC_VERSMISMATCH MVFS_RPC_VERSMISMATCH
#define RPC_PROGVERSMISMATCH MVFS_RPC_PROGVERSMISMATCH
#define RPC_INTR MVFS_RPC_INTR
#define RPC_TIMEDOUT MVFS_RPC_TIMEDOUT

struct rpc_err {
    enum clnt_stat re_status;
    int re_errno;
};

#define XDR_INLINE(xdrs,size) NULL      /* can't do it */
#define IXDR_PUT_U_LONG(buf,what) /*nothing */
#define IXDR_PUT_LONG(buf,what) /*nothing */
#define IXDR_GET_U_LONG(buf) 0
#define IXDR_GET_LONG(buf) 0

#define xdr_short undefined_mvfs_rhat_xdr_short
/*#define xdr_opaque mvfs_rhat_xdr_opaque*/

#define DECL_XDR_OP_TYPE(type,objtype)          \
extern bool_t                                   \
xdr_##type(                                     \
    XDR *x,                                     \
    objtype obj                                 \
)

#define DECL_XDR_OP(type) DECL_XDR_OP_TYPE(type,type *)

DECL_XDR_OP(u_long);
DECL_XDR_OP(void);
DECL_XDR_OP(u_int);
DECL_XDR_OP(u_short);
DECL_XDR_OP(long);
DECL_XDR_OP(int);
DECL_XDR_OP_TYPE(enum,int *);
DECL_XDR_OP_TYPE(timeval,struct timeval *);
DECL_XDR_OP_TYPE(bool,bool_t *);

#define NULLPROC 0

extern bool_t
xdr_string(
    XDR *x,
    char **objp,
    u_int max_size
);

#define UDPMSGSIZE 8800
struct mdki_linux_rpc_procinfo {
    char *proc_name;
    unsigned int proc;
    xdrproc_t args;
    size_t args_size;
    xdrproc_t res;
    size_t res_size;
};

extern int
mdki_linux_clntkudp_create(
    struct sockaddr *addr,
    const int version,
    struct rpc_program *prog,
    const int retrans_count,
    const bool_t intr,
    CLIENT **cl_pp
);

#endif /* __LINUX_KRPC_H */
/* $Id: a3477e06.9c9311dd.9a62.00:01:83:29:c0:fc $ */
