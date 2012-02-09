/* * (C) Copyright IBM Corporation 1990, 2005. */
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

#if !defined(VIEW_RPC_KERNEL_H)
#define VIEW_RPC_KERNEL_H

#include <tbs_rpc_kernel.h>
#include <view_base.h>

/*********************************************************************
 * view_server protocol version number.  The view_server and the
 * view clients can determine at what level to interoperate by knowing
 * what the other systems protocol is.  These numbers need to change
 * when ever XREV rpc issues are involved.
 **********************************************************************/

#define VIEW_SERVER_PROTOCOL_VERSION 8
#define VIEW_SERVER_PROTOCOL_VERSION_V6 7  /* Used for both V5 and V6 */

/*
 * VIEW_MAX_UDP_XDR_SIZE represents the maximum size of the
 * encoded packet that we can send on UDP
 * before we will get a failure.  RPC overhead - 6, view_hdr_reply_t  - 2.
 * Because of a bug on HP, it needs to limit 16 below KS_UDPMSGSIZE that
 * rpc says that we can.
 */
#define HP_BUG_ADJUST 16
#define VIEW_MAX_UDP_XDR_SIZE (KS_UDPMSGSIZE - 8*4 - HP_BUG_ADJUST)

EZ_XDR_ROUTINE(view_fhandle_t);
EZ_XDR_ROUTINE(view_vstat_t);
EZ_XDR_ROUTINE(view_set_attr_t);
EZ_XDR_ROUTINE(view_build_session_t);
EZ_XDR_ROUTINE(view_handle_t);
EZ_XDR_ROUTINE(view_name_state_t);
EZ_XDR_ROUTINE(view_bhandle_t);

typedef struct view_hdr_req {
    time_t boot_time;
    XID_T xid;
    view_handle_t view;
    view_bhandle_t build_handle;
} view_hdr_req_t;

/* This XDR routine is for call or reply structures, and must
   remain as a 2-arg variant.  See <ks_rpc.h> for details. */
EXTERN bool_t
    xdr_view_hdr_req_t(XDR *xdrs,
		       view_hdr_req_t *objp);

typedef struct view_hdr_reply {
    XID_T xid;
    tbs_status_t status;
} view_hdr_reply_t;

/* This XDR routines is for call or reply structures, and must
   remain as a 2-arg variant.  See <ks_rpc.h> for details. */
EXTERN bool_t
    xdr_view_hdr_reply_t(XDR *xdrs,
			 view_hdr_reply_t *objp);

/****************************************************************************
 * view_null
 */
typedef struct view_null_req {
    view_hdr_req_t hdr;
} view_null_req_t;

EXTERN bool_t
    xdr_view_null_req_t(XDR *xdrs,
			view_null_req_t *objp);

typedef struct view_null_reply {
    view_hdr_reply_t hdr;
} view_null_reply_t;

EXTERN bool_t
    xdr_view_null_reply_t(XDR *xdrs,
			  view_null_reply_t *objp);

/****************************************************************************
 * xdr_view_dir_arg_t
 */
typedef struct view_dir_arg {
    view_fhandle_t d_fhandle;
    char *name;					/* tbs_name_t */
} view_dir_arg_t;

EZ_XDR_ROUTINE(view_dir_arg_t);

/****************************************************************************
 * view_dir
 */
typedef struct view_dir_req {
    view_hdr_req_t hdr;
    view_dir_arg_t file;
    struct timeval dir_dtm;
} view_dir_req_t;

EXTERN bool_t
    xdr_view_dir_req_t(XDR *xdrs,
		       view_dir_req_t *objp);

typedef struct view_dir_reply {
    view_hdr_reply_t hdr;
    view_vstat_t vstat;
} view_dir_reply_t;

EXTERN bool_t
    xdr_view_dir_reply_t(XDR *xdrs,
			 view_dir_reply_t *objp);

typedef struct view_dir_mod {
    view_vstat_t dvstat;
    tbs_boolean_t dir_dtm_valid;
} view_dir_mod_t;

EZ_XDR_ROUTINE(view_dir_mod_t);

typedef struct view_dir_mod_reply {
    view_hdr_reply_t hdr;
    view_dir_mod_t dir_mod;
} view_dir_mod_reply_t;

EXTERN bool_t
    xdr_view_dir_mod_reply_t(XDR *xdrs,
			     view_dir_mod_reply_t *objp);

EZ_XDR_ROUTINE(view_hm_warp_opt_t);
#define xdr_view_hm_warp_opt_t(xdrs, objp) xdr_enum((xdrs), (enum_t *)(objp))

/****************************************************************************
 * view_root
 */
typedef struct view_root_req {
    view_hdr_req_t hdr;
    tbs_oid_t vob_root;
    char *host_name;		/* tbs_name_t */
    char *host_pathname;	/* tbs_pname_t */
    char *pname;		/* tbs_pname_t */
    view_hm_warp_opt_t hm_warp_opt;
} view_root_req_t;
EXTERN bool_t
    xdr_view_root_req_t(XDR *xdrs,
			view_root_req_t *objp);

typedef struct view_root_reply {
    view_hdr_reply_t hdr;
    view_fhandle_t fhandle;
    view_vstat_t vstat;
    tbs_boolean_t bh_invariant;
} view_root_reply_t;

EXTERN bool_t
    xdr_view_root_reply_t(XDR *xdrs,
			  view_root_reply_t *objp);

/****************************************************************************
 * view_lookup 
 */
typedef struct view_lookup_req {
    view_hdr_req_t hdr;
    view_fhandle_t d_fhandle;
    char *name;			/* tbs_name_t */
    view_hm_warp_opt_t hm_warp_opt;
    char *residual_pname;	/* tbs_pname_t */
} view_lookup_req_t;

EXTERN bool_t
    xdr_view_lookup_req_t(XDR *xdrs,
			  view_lookup_req_t *objp);

typedef struct view_lookup_reply {
    view_hdr_reply_t hdr;
    view_name_state_t name_state;	/* always encoded, even on errors */
    view_fhandle_t fhandle;
    view_vstat_t vstat;
    size_t residual_resolved;
    tbs_boolean_t bh_invariant;
    struct timeval lvut;
} view_lookup_reply_t;
EXTERN bool_t
    xdr_view_lookup_reply_t(XDR *xdrs,
			    view_lookup_reply_t *objp);

/****************************************************************************
 * view_getattr
 */
typedef struct view_getattr_req_t {
    view_hdr_req_t hdr;
    view_fhandle_t fhandle;
} view_getattr_req_t;

EXTERN bool_t
    xdr_view_getattr_req_t(XDR *xdrs,
			   view_getattr_req_t *objp);

typedef struct view_getattr_reply {
    view_hdr_reply_t hdr;
    view_fhandle_t fhandle;
    view_vstat_t vstat;
    tbs_boolean_t bh_invariant;
    struct timeval lvut;
} view_getattr_reply_t;

EXTERN bool_t
    xdr_view_getattr_reply_t(XDR *xdrs,
			  view_getattr_reply_t *objp);

/****************************************************************************
 * view_replica_root
 */
typedef struct view_replica_root_req {
    view_hdr_req_t hdr;
    tbs_oid_t vob_root;
    tbs_uuid_t replica_root;
    char *host_name;		/* tbs_name_t */
    char *host_pathname;	/* tbs_pname_t */
    char *pname;		/* tbs_pname_t */
    view_hm_warp_opt_t hm_warp_opt;
} view_replica_root_req_t;
EXTERN bool_t
    xdr_view_replica_root_req_t(XDR *xdrs,
                                view_replica_root_req_t *objp);
typedef view_getattr_reply_t view_replica_root_reply_t;
#define xdr_view_replica_root_reply_t xdr_view_getattr_reply_t

/****************************************************************************
 * view_setattr
 */
typedef struct view_setattr_req {
    view_hdr_req_t hdr;
    view_fhandle_t fhandle;
    view_set_attr_t sattr;
} view_setattr_req_t;

EXTERN bool_t
    xdr_view_setattr_req_t(XDR *xdrs,
			   view_setattr_req_t *objp);

typedef view_dir_reply_t view_setattr_reply_t;
#define xdr_view_setattr_reply_t xdr_view_dir_reply_t

/****************************************************************************
 * view_readlink
 */
typedef struct view_readlink_req {
    view_hdr_req_t hdr;
    view_fhandle_t fhandle;
    size_t max_text_size;
} view_readlink_req_t;
EXTERN bool_t
    xdr_view_readlink_req_t(XDR *xdrs,
			    view_readlink_req_t *objp);

typedef struct view_readlink_reply {
    view_hdr_reply_t hdr;
    size_t text_size;
    char *text;					/* tbs_pname_t */
} view_readlink_reply_t;
EXTERN bool_t
    xdr_view_readlink_reply_t(XDR *xdrs,
			      view_readlink_reply_t *objp);

/****************************************************************************
 * view_create 
 */
typedef struct view_create_req {
    view_hdr_req_t hdr;
    view_dir_arg_t create;
    view_set_attr_t iattr;
    size_t max_text_size;
    struct timeval dir_dtm;
} view_create_req_t;

EXTERN bool_t
    xdr_view_create_req_t(XDR *xdrs,
			  view_create_req_t *objp);

typedef struct view_create_reply {
    view_hdr_reply_t hdr;
    view_fhandle_t fhandle;
    view_vstat_t vstat;
    view_dir_mod_t dir_mod;
    size_t text_size;
    char *text;					/* tbs_pname_t */
} view_create_reply_t;

EXTERN bool_t
    xdr_view_create_reply_t(XDR *xdrs,
			    view_create_reply_t *objp);

/****************************************************************************
 * view_remove 
 */
typedef view_dir_req_t view_remove_req_t;
#define xdr_view_remove_req_t xdr_view_dir_req_t
typedef view_dir_mod_reply_t view_remove_reply_t;
#define xdr_view_remove_reply_t xdr_view_dir_mod_reply_t 

/****************************************************************************
 * view_rename
 */
typedef struct view_rename_req {
    view_hdr_req_t hdr;
    view_dir_arg_t from;
    view_dir_arg_t to;
    struct timeval dir_dtm;
} view_rename_req_t;
EXTERN bool_t
    xdr_view_rename_req_t(XDR *xdrs,
			  view_rename_req_t *objp);
typedef view_dir_mod_reply_t view_rename_reply_t;
#define xdr_view_rename_reply_t xdr_view_dir_mod_reply_t

/****************************************************************************
 * view_link
 */
typedef struct view_link_req {
    view_hdr_req_t hdr;
    view_fhandle_t fhandle;
    view_dir_arg_t to;
    struct timeval dir_dtm;
} view_link_req_t;
EXTERN bool_t
    xdr_view_link_req_t(XDR *xdrs,
			view_link_req_t *objp);
typedef view_dir_mod_reply_t view_link_reply_t;
#define xdr_view_link_reply_t xdr_view_dir_mod_reply_t

/****************************************************************************
 * view_symlink
 */
typedef struct view_symlink_req {
    view_hdr_req_t hdr;
    view_dir_arg_t create;
    char *text;					/* tbs_pname_t */
    view_set_attr_t iattr;
    struct timeval dir_dtm;
} view_symlink_req_t;
EXTERN bool_t
    xdr_view_symlink_req_t(XDR *xdrs,
			   view_symlink_req_t *objp);

typedef struct view_symlink_reply {
    view_hdr_reply_t hdr;
    view_fhandle_t fhandle;
    view_vstat_t vstat;
    view_dir_mod_t dir_mod;
} view_symlink_reply_t;

EXTERN bool_t
    xdr_view_symlink_reply_t(XDR *xdrs,
			     view_symlink_reply_t *objp);

/****************************************************************************
 * view_mkdir 
 */
typedef struct view_mkdir_req {
    view_hdr_req_t hdr;
    view_dir_arg_t create;
    view_set_attr_t iattr;
    struct timeval dir_dtm;
} view_mkdir_req_t;

EXTERN bool_t
    xdr_view_mkdir_req_t(XDR *xdrs,
			 view_mkdir_req_t *objp);

typedef view_symlink_reply_t view_mkdir_reply_t;
#define xdr_view_mkdir_reply_t xdr_view_symlink_reply_t

/****************************************************************************
 * view_rmdir 
 */
typedef view_dir_req_t view_rmdir_req_t;
#define xdr_view_rmdir_req_t xdr_view_dir_req_t
typedef view_dir_mod_reply_t view_rmdir_reply_t;
#define xdr_view_rmdir_reply_t xdr_view_dir_mod_reply_t

/****************************************************************************
 * view_readdir
 *
 * This is ugly, but I couldn't figure out the correct thing todo.
 * XDR_SIZE_VIEW_DIRENT_T represents the "wire" size of a view_dirent_t.
 * d_namlen, d_name bytes, d_ino, d_off
 */
#define XDR_SIZE_VIEW_DIRENT_T(namelen) ((namelen+3+4*3) & ~3)
/*
 * MAX_XDR_SIZE_VIEW_READDIR_REPLY_T represents the maximum size of the
 * encoded packet (from xdr_getpos) that we can send on UDP
 * before we will get a failure. 
 * offset - 1, eof - 1,
 * 
 */

#define MAX_XDR_SIZE_VIEW_READDIR_REPLY_T   (VIEW_MAX_UDP_XDR_SIZE - 2*4)
/*
 * MAX request size that will not overflow what a dgram packet can send.
 * TCP requests are unlimited in size.
 */ 
#define VIEW_READDIR_MAX_SIZE VIEW_MAXDATA

typedef struct view_readdir_req {
    view_hdr_req_t hdr;
    view_fhandle_t d_fhandle;
    u_long offset;
    size_t max_dirent_size;
} view_readdir_req_t;
EXTERN bool_t
    xdr_view_readdir_req_t(XDR *xdrs,
			   view_readdir_req_t *objp);

typedef struct view_readdir_reply {
    view_hdr_reply_t hdr;
    u_long offset;		/* next offset in directory (opaque) */
    size_t size;		/* number of bytes read */
    bool_t eof;			/* true if last entry in result */
    size_t max_dirent_size;	/* max number of requested bytes*/
    view_dirent_t *ents;	/* max of VIEW_READDIR_MAX_SIZE bytes */
} view_readdir_reply_t;
EXTERN bool_t
    xdr_view_readdir_reply_t(XDR *xdrs,
			     view_readdir_reply_t *objp);

/****************************************************************************
 * view_cltxt
 */
typedef struct view_cltxt_reply {
    view_hdr_reply_t hdr;
    size_t text_size;
    char *text;					/* tbs_pname_t */
    tbs_boolean_t vob;
} view_cltxt_reply_t;

EXTERN bool_t
    xdr_view_cltxt_reply_t(XDR *xdrs,
			   view_cltxt_reply_t *objp);
typedef view_readlink_req_t view_cltxt_req_t;
#define xdr_view_cltxt_req_t xdr_view_readlink_req_t

/****************************************************************************
 * view_invalidate
 * Cause the view invalidate its cache
 */

typedef struct view_invalidate_req {
    view_hdr_req_t hdr;
    view_invalidate_type_t type;
    tbs_oid_t vob_oid;
    tbs_oid_t obj_oid;
    char *name;
} view_invalidate_req_t;
EXTERN bool_t
    xdr_view_invalidate_req_t(XDR *xdrs,
			      view_invalidate_req_t *objp);
typedef view_null_reply_t view_invalidate_reply_t;
#define xdr_view_invalidate_reply_t xdr_view_null_reply_t

/****************************************************************************
 * view_change_oid
 */
typedef struct view_change_oid_req_t {
    view_hdr_req_t hdr;
    view_fhandle_t fhandle;
    view_change_oid_option_t option;
} view_change_oid_req_t;

EXTERN bool_t
    xdr_view_change_oid_req_t(XDR *xdrs,
			      view_change_oid_req_t *objp);

typedef view_dir_reply_t view_change_oid_reply_t;
#define xdr_view_change_oid_reply_t xdr_view_dir_reply_t

/****************************************************************************
 * view_change_mtype
 */
typedef struct view_change_mtype_req_t {
    view_hdr_req_t hdr;
    view_fhandle_t fhandle;
    vob_mtype_t mtype;
} view_change_mtype_req_t;

EXTERN bool_t
    xdr_view_change_mtype_req_t(XDR *xdrs,
			      view_change_mtype_req_t *objp);

typedef view_dir_reply_t view_change_mtype_reply_t;
#define xdr_view_change_mtype_reply_t xdr_view_dir_reply_t

/****************************************************************************
 * view_gpath
 */
typedef struct view_gpath_req {
    view_hdr_req_t hdr;
    view_fhandle_t fhandle;
    tbs_boolean_t hm_force;
    size_t max_path_size;
    size_t max_ext_path_size;
} view_gpath_req_t;
EXTERN bool_t
    xdr_view_gpath_req_t(XDR *xdrs,
			 view_gpath_req_t *objp);

typedef struct view_gpath_reply {
    view_hdr_reply_t hdr;
    size_t path_size;
    char *path;					/* tbs_pname_t */
    size_t ext_path_size;
    char *ext_path;				/* tbs_pname_t */
} view_gpath_reply_t;
EXTERN bool_t
    xdr_view_gpath_reply_t(XDR *xdrs,
			   view_gpath_reply_t *objp);

/****************************************************************************
 * view_revalidate
 */
typedef view_getattr_req_t view_revalidate_req_t;
#define xdr_view_revalidate_req_t xdr_view_getattr_req_t
typedef view_root_reply_t view_revalidate_reply_t;
#define xdr_view_revalidate_reply_t xdr_view_root_reply_t

/****************************************************************************
 * view_getprop
 */
typedef struct view_getprop_req_t {
    view_hdr_req_t hdr;
    view_prop_t vprop;
} view_getprop_req_t;

EXTERN bool_t
    xdr_view_getprop_req_t(XDR *xdrs,
			   view_getprop_req_t *objp);

typedef struct view_getprop_reply_t {
    view_hdr_reply_t hdr;
    u_long pvalue;
} view_getprop_reply_t;

EXTERN bool_t
    xdr_view_getprop_reply_t(XDR *xdrs,
			     view_getprop_reply_t *objp);

/****************************************************************************
 * view RPC program number assignments. 
 * *** Don't change these numbers or Atria xrev algorithms will not be
 * able to find a common protocol via the contact call because the rpc
 * will be rejected at the rpc layer ****
 */
#define VIEW_SERVER 390513 /* Sun supplied rpc number */
#define VIEW_SERVER_VERS 4 /* rpc version number */

typedef enum view_server_proc {
    VIEW_CONTACT = 1,
    VIEW_SERVER_EXIT,		/* 002: */
    VIEW_SETATTR,		/* 003: MVFS */
    VIEW_CREATE,		/* 004: MVFS */
    VIEW_REMOVE,		/* 005: MVFS */
    VIEW_RENAME,		/* 006: MVFS */
    VIEW_SYMLINK,		/* 007: MVFS */
    VIEW_MKDIR,			/* 008: MVFS */
    VIEW_RMDIR,			/* 009: MVFS */
    VIEW_READDIR,		/* 010: MVFS */
    VIEW_STATFS,		/* 011: MVFS */
    VIEW_CLTXT,			/* 012: MVFS */
    VIEW_CHANGE_OID,		/* 013: MVFS */
    VIEW_READDIR_EXT,		/* 014: MVFS */
    VIEW_GPATH,			/* 015: MVFS */
    VIEW_REVALIDATE,		/* 016: MVFS */
    VIEW_CLTXT_PNAME,		/* 017: */
    VIEW_CHANGE_MTYPE,		/* 018: MVFS */
    VIEW_INVALIDATE_UUID,	/* 019: MVFS */
    VIEW_LINK,			/* 020: MVFS */
#if defined(XREV_SERVERS_SUPPORT_V6_CLIENTS)
    VIEW_LOOKUP_V6,		/* 021: MVFS */
#else
    VIEW_UNUSED_21,		/* 021: MVFS */
#endif
    VIEW_GETATTR,		/* 022: MVFS */
    VIEW_REPLICA_ROOT,		/* 023: MVFS */
#if defined(XREV_SERVERS_SUPPORT_V6_CLIENTS)
    VIEW_LOOKUP_EXT_V6,		/* 024: */
#else
    VIEW_UNUSED_24,		/* 024: */
#endif
    VIEW_CREATE_CONTAINER,	/* 025: */
    VIEW_REMOVE_CONTAINER,	/* 026: */
    VIEW_RENAME_CONTAINER,	/* 027: */
    VIEW_WINK,			/* 028: */
    VIEW_READLINK,		/* 029: MVFS */
    VIEW_RELOAD_SPEC,		/* 030: */
    VIEW_BLD_SESSION_FREE,	/* 031: */
    VIEW_EVENTS_GET_VOB,	/* 031: */
    VIEW_VOB_CREATE,		/* 033: */
    VIEW_FIND_OID,		/* 034: */
    VIEW_SPEC_RULE_FORMAT,	/* 035: */
    VIEW_WHITE_OUT,		/* 036: */
    VIEW_UNWHITE_OUT,		/* 037: */
    VIEW_BLD_GET_REF_TIME,	/* 038: */
    VIEW_RECOVER,		/* 039: */
    VIEW_DUMP,			/* 040: */
    VIEW_LOAD,			/* 041: */
    VIEW_CR_GET,		/* 042: */
    VIEW_DO_PROMOTED,		/* 043: */
    VIEW_DO_PURGE_CLTXT,	/* 044: */
    VIEW_VOB_SAVE_PATH,		/* 045: */
    VIEW_BLD_SESSION_FLAGS,	/* 046: */
    VIEW_GET_TEXT_MODE,		/* 047: */
    VIEW_CR_ADD_VOB_REF_UUID,	/* 048: */
    VIEW_CR_RM_VOB_REF_UUID,	/* 049: */
    VIEW_VOB_GET_PATH_UUID,	/* 050: */
    VIEW_VOB_RM_PATH_UUID,	/* 051: */
    VIEW_VOB_RECOVER_OBJECT_UUID,	/* 052: */
    VIEW_CR_SAVE,		/* 053: */
    VIEW_GETPROP,		/* 054: */
    VIEW_SETPROP,		/* 055: */
    VIEW_LIC_CHECK,		/* 056: */
    VIEW_INVENTORY_UUIDS,	/* 057: */
    VIEW_SETWORK_GET_VOBS,	/* 058: */
    VIEW_SERVER_EXIT_RMTAG,	/* 059: */
    VIEW_STATISTICS,		/* 060: */
    VIEW_SET_VALUES,		/* 061: */
    VIEW_INVENTORY_VOBS,	/* 062: */
    VIEW_SETWORK_SET_VOBS,	/* 063: */
    VIEW_SETWORK_CLEANUP,	/* 064: */
    VIEW_HLINK_SET_VOBS,	/* 065: */
    VIEW_HLINK_CLEANUP,		/* 066: */
    VIEW_HLINK_GET_VOBS,	/* 067: */
    VIEW_WS_CREATE_DB,		/* 068: */
    VIEW_FRZ_GET_SCOPES,	/* 069: */
    VIEW_FRZ_GET_FREEZE_STATE,	/* 070: */
    VIEW_FRZ_GET_RELOAD_INFO,	/* 071: */
    VIEW_WS_BEGIN_LOAD_SESSION,	/* 072: */
    VIEW_WS_END_LOAD_SESSION,	/* 073: */
    VIEW_FRZ_OK_TO_SET_SPEC,	/* 074: */
    VIEW_WS_UPD_WSO_ATTRS,	/* 075: */
    VIEW_WS_GET_SCOPE_ALIASES,	/* 076: */
    VIEW_WS_GET_UNVISITED_WSOS,	/* 077: */
    VIEW_WS_INVALIDATE_OBJECT,	/* 078: */
    VIEW_WS_UNLOAD_ONE_OBJECT,	/* 079: */
    VIEW_FRZ_GET_NUM_FROZEN,	/* 080: */
    VIEW_WS_GET_OBJ_SCOPES,	/* 081: */
    VIEW_WS_RENAME_OBJECT,	/* 082: */
    VIEW_WS_IS_FROZEN_OBJECT,	/* 083: */
    VIEW_RELOAD_SPEC_EXT,	/* 084: */
    VIEW_WS_IS_MODIFIED_WSO_EXT,	/* 085: */
    VIEW_PROTECT_STG_AS_CLIENT,	/* 086: */
    VIEW_PROTECT_STG_CHECK,	/* 087: */
    VIEW_UNPROTECT_STG_AS_CLIENT,	/* 088: */
    VIEW_UNPROTECT_STG_CHECK,	/* 089: */
    VIEW_REPARENT_VOB,		/* 090: */
    VIEW_GETOWN_SID,		/* 091: */
    VIEW_WS_LOAD_ONE_OBJECT,	/* 092: */
    VIEW_WS_LOAD_ONE_SLINK,	/* 093: */
    VIEW_GET_CONFIG_SPEC,	/* 094: */
    VIEW_SET_CONFIG_SPEC,	/* 095: */
    VIEW_REPLACE_CONTAINER,	/* 096: */
    VIEW_PROTECT_CONTAINER,	/* 097: */
    VIEW_FSTAT_CONTAINER,	/* 098: */
    VIEW_WS_UNLOAD_ONE_OBJECT_EXT,	/* 099: */
    VIEW_LOOKUP,                /* 100: */
    VIEW_LOOKUP_EXT,            /* 101: */
    VIEW_DOS_TO_UNSHAREABLE,    /* 102: */
    VIEW_GET_DOS,               /* 103: */
    VIEW_DOS_FOUND,             /* 104: */
    VIEW_NUM_PROCS		/* 105: */
} view_server_proc_t;

#endif
/* $Id: f092bbc4.637a11da.8655.00:01:83:a6:4c:63 $ */
