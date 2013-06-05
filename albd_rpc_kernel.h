/* * (C) Copyright IBM Corporation 1991, 2012. */
/*
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA


 Author: IBM Corporation
 This module is part of the IBM (R) Rational (R) ClearCase (R)
 Multi-version file system (MVFS).
 For support, please visit http://www.ibm.com/software/support

*/
#if !defined(_ALBD_RPC_KERNEL_H_)
#define _ALBD_RPC_KERNEL_H_

#include <tbs_rpc_kernel.h>
#include <albd_base.h>

#define ALBD_SERVICE_WK_PORT 371
#define ALBD_SERVER_PROTOCOL_VERSION 6              /* V7.1 version */
#define ALBD_SERVER_PROTOCOL_VERSION_V5_COMPAT 5    /* V5 compatible version */

/*******************************
 *
 */

EZ_XDR_ROUTINE(albd_protocol_in_t);

EZ_XDR_ROUTINE(albd_rpc_trait_t);

typedef struct sockaddr_in albd_sockaddr_in_t;
typedef struct sockaddr albd_sockaddr_t;

EZ_XDR_ROUTINE(albd_sockaddr_in_t);

EZ_XDR_ROUTINE(albd_server_port_t);

EZ_XDR_ROUTINE(albd_server_port_list_t);

/*
 * All albd request must start with this header
 */

typedef struct albd_hdr_req {
    time_t boot_time;	/* boot time of requesting node */
    u_long xid;		/* transaction id, not currently used for albd */
} albd_hdr_req_t;
EXTERN bool_t
xdr_albd_hdr_req_t(
    XDR *xdrs,
    albd_hdr_req_t *objp
);

/*
 * All albd replys start with this header
 */
typedef struct albd_hdr_reply {
    u_long xid;		/* senders transaction id */
    tbs_status_t status;/* reply status */
} albd_hdr_reply_t;
EXTERN bool_t
xdr_albd_hdr_reply_t(
    XDR *xdrs,
    albd_hdr_reply_t *objp
);

/****************************************************************************
 * albd_find_server
 */
typedef struct albd_find_server_req {
    albd_hdr_req_t hdr;
    albd_rpc_trait_t rpc_trait;
    tbs_uuid_t uuid;
    char *path;
} albd_find_server_req_t;
typedef albd_find_server_req_t albd_find_server_v70_req_t;
#define xdr_albd_find_server_v70_req_t xdr_albd_find_server_req_t
EXTERN bool_t 
xdr_albd_find_server_req_t(
    XDR *xdrs,
    albd_find_server_req_t *objp
);

typedef struct albd_find_server_v70_reply {
    albd_hdr_reply_t hdr;
    time_t coming_up;
    tbs_uuid_t uuid;
    char *path;
    albd_sockaddr_in_t saddr;
} albd_find_server_v70_reply_t;
EXTERN bool_t
xdr_albd_find_server_v70_reply_t(
    XDR *xdrs,
    albd_find_server_v70_reply_t *objp
);
typedef struct albd_find_server_reply {
    albd_hdr_reply_t hdr;
    time_t coming_up;
    tbs_uuid_t uuid;
    char *path;
    albd_server_port_list_t port_list;
} albd_find_server_reply_t;
EXTERN bool_t
xdr_albd_find_server_reply_t(
    XDR *xdrs,
    albd_find_server_reply_t *objp
);

/****************************************************************************
 */
#define ALBD_SERVER 390512 /* Sun supplied rpc number */
#define ALBD_SERVER_VERS 3


typedef enum albd_server_proc {
    ALBD_CONTACT = 1,
    ALBD_REGISTER_SERVER,
    ALBD_FIND_SERVER_V70,
    ALBD_SERVER_IDLE,
    ALBD_SERVER_BUSY,
    ALBD_UNUSED_6,
    ALBD_UNUSED_7,
    ALBD_UNUSED_8,
    ALBD_UNUSED_9,
    ALBD_UNUSED_10,
    ALBD_SCHED_INFO,
    ALBD_UNUSED_12,
    ALBD_REGISTRY_GET_ID,
    ALBD_REGISTRY_FINDBYSTRING,
    ALBD_REGISTRY_FINDBYUUID,
    ALBD_REGISTRY_GET,
    ALBD_REGISTRY_ADD,
    ALBD_REGISTRY_REMOVE,
    ALBD_SERVER_ALTERNATE_UUID,
    ALBD_REGISTRY_CHK_ACCESS,
    ALBD_REGISTRY_GET_DTM,
    ALBD_LIST_SERVERS_V70,
    ALBD_GET_LOCAL_PATH,
    ALBD_CLNT_LIST_LOOKUP_V70,
    ALBD_LICENSE_GET_PRODUCT,
    ALBD_CLNT_LIST_GET_V70,
    ALBD_HOSTINFO,
    ALBD_CLNT_LIST_REGISTER,
    ALBD_REGISTRY_GET_DB_LIST,
    ALBD_REGISTRY_CLNT_CONF,
    ALBD_REGISTRY_SVR_CONF,
    ALBD_REGISTRY_GET_BACKUP,
    ALBD_REGISTRY_SET_BACKUP,
    ALBD_REGISTRY_FINDBYATTR,
    /* 35 */ ALBD_UNUSED_35,
    /* 36 */ ALBD_SCHED_GET_JOBS,
    /* 37 */ ALBD_SCHED_HAS_INFO_CHANGED,
    /* 38 */ ALBD_SCHED_GET_TASKS,
    /* 39 */ ALBD_SCHED_GET_ACL,
    /* 40 */ ALBD_SCHED_SET_ACL,
    /* 41 */ ALBD_SCHED_JOB_CREATE,
    /* 42 */ ALBD_SCHED_JOB_DELETE,
    /* 43 */ ALBD_SCHED_JOB_LOOKUP_BY_ID,
    /* 44 */ ALBD_SCHED_JOB_GET_PROPERTIES,
    /* 45 */ ALBD_SCHED_JOB_SET_PROPERTIES,
    /* 46 */ ALBD_SCHED_JOB_HAS_INFO_CHANGED,
    /* 47 */ ALBD_SCHED_RJOB_GET_HANDLE,
    /* 48 */ ALBD_SCHED_RJOB_GET_COMPLETION_INFO,
    /* 49 */ ALBD_SCHED_RJOB_RUN_JOB,
    /* 50 */ ALBD_SCHED_RJOB_TERMINATE,
    /* 51 */ ALBD_SCHED_JOB_LOOKUP_BY_NAME,
    /* 52 */ ALBD_SCHED_TASK_EXISTS,
    /* 53 */ ALBD_SCHED_TASK_NAME_TO_ID,
    /* 54 */ ALBD_SCHED_CHECK_ACC,
    /* 55 */ ALBD_SCHED_TASK_ID_TO_NAME,
    /* 56 */ ALBD_SCHED_GET_APP_PERMS,
    /* 57 */ ALBD_LICENSE_CLEARCASE_AUTHENTICATED_URL,
    /* 58 */ ALBD_SCHED_GET_TIME,
    /* 59 */ ALBD_SCHED_CONTACT,
    /* 60 */ ALBD_REMOTE_BUILD_HI,
    /* 61 */ ALBD_LICENSE_CHECK_SID,
    /* 62 */ ALBD_LICENSE_SID_STATS,
    /* 63 */ ALBD_LICENSE_REVOKE_SID,
    /* 64 */ ALBD_ELCC_FIND_SERVER_V70,
    /* 65 */ ALBD_ELCC_IS_ELCC,
    /* 66 */ ALBD_TZINFO,
    /* 67 */ ALBD_TOGGLE_SERVER_RESTART,
    /* 68 */ ALBD_SCHED_JOB_CREATE_UTC,
    /* 69 */ ALBD_SCHED_JOB_GET_PROPERTIES_UTC,
    /* 70 */ ALBD_SCHED_JOB_SET_PROPERTIES_UTC,
    /* 71 */ ALBD_FIND_SERVER,
    /* 72 */ ALBD_ELCC_FIND_SERVER,
    /* 73 */ ALBD_LIST_SERVERS,
    /* 74 */ ALBD_CLNT_LIST_LOOKUP,
    /* 75 */ ALBD_CLNT_LIST_GET,
    /* 76 */ ALBD_REGISTRY_GET_PATTERN,
    ALBD_NUM_PROCS
} albd_server_proc_t;

#endif /* _ALBD_RPC_KERNEL_H_ */
/* $Id: 7232f08e.d67011e1.9c09.00:01:84:c3:8a:52 $ */
