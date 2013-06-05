/* * (C) Copyright IBM Corporation 1991, 2005. */
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
#if !defined(VIEW_BASE_H)
#define VIEW_BASE_H

#include <linux/errno.h>
#include <linux/time.h>
#include <tbs_base.h>
#include <credutl_kernel.h>
#include <vob_mtype.h>

#ifdef __cplusplus
extern "C" {
#endif

#define _CFS_RULE_NUM_DEFINED

/* cfs_rule_num_t needs to show up in the API's version of the header file. */
typedef ks_uint32_t cfs_rule_num_t;

#define CFS_RULE_NUM_NULL ((cfs_rule_num_t) 0)
/* N.B. view server treats KS_UINT32_T_MAX specially */
#define CFS_RULE_NUM_FRZ_UNK ((cfs_rule_num_t) (KS_UINT32_T_MAX - 1))
#define CFS_RULE_NUM_FRZ_CHECKEDOUT ((cfs_rule_num_t) (KS_UINT32_T_MAX - 2))

typedef struct view_server_handle *view_server_handle_t;

#define VIEW_MAXDATA 8192
#define SERVER_MAX_DOS 32

typedef struct view_fhandle {
    tbs_uuid_t vob_uuid;
    tbs_dbid_t ver_dbid;
    tbs_dbid_t elem_dbid;
    u_long gen;
    u_long flags;
#define VIEW_FHANDLE_FLAGS_NULL (0L)
#define VIEW_FHANDLE_FLAGS_HISTORY_MODE	(0x1L)	/* history mode directory */
#define VIEW_FHANDLE_FLAGS_VIEW_OBJ (0x2L)	/* is a view object */
    u_long pad0;
} view_fhandle_t;

#define VIEW_ISA_HISTORY_MODE(fh) (((fh)->flags & VIEW_FHANDLE_FLAGS_HISTORY_MODE) != 0)
#define VIEW_ISA_VIEW_OBJ(fh) (((fh)->flags & VIEW_FHANDLE_FLAGS_VIEW_OBJ) != 0)
#define VIEW_FHANDLE_EQUAL(a, b)		\
 (tbs_uuid_eq(&(a)->vob_uuid, &(b)->vob_uuid) &&\
  (a)->ver_dbid == (b)->ver_dbid &&		\
  (a)->elem_dbid == (b)->elem_dbid &&		\
  (a)->gen == (b)->gen &&			\
  (a)->flags == (b)->flags)

/*---------------------------------------------------------------------------
 * View stat structure	A rich stat structure that describes clearcase file objects.
 *
 * view_vstat_t
 *  .fstat 		tbs_fstat_db_t structure of the associated object.
 *  .mtype		Meta-type of associated object.
 *  .elem_oid		Element id of object associated with the event if it
 *			is a versioned object, else TBS_OID_NULL.
 *  .obj_oid		Object id of object associated with the event.
 *  .event_time         Time of the last vob event that might have altered
 *			which version of associated object selected or the
 *			object itself. The time may change more often than
 *			necessary.
 */
typedef struct view_vstat {
    tbs_fstat_db_t fstat;
    vob_mtype_t mtype;
    tbs_oid_t elem_oid;
    tbs_oid_t obj_oid;
    struct timeval event_time;	
} view_vstat_t;
/*
 * File attributes which can be set.
 * The mask field is a bit mask of which fields in the attributes are valid.
 */
#define VIEW_ATTR_NULL		(u_long)0x0000
#define VIEW_ATTR_TYPE		(u_long)0x0001
#define VIEW_ATTR_MODE		(u_long)0x0002
#define VIEW_ATTR_USID		(u_long)0x0004
#define VIEW_ATTR_GSID		(u_long)0x0008
#define VIEW_ATTR_FSID		(u_long)0x0010
#define VIEW_ATTR_NODEID	(u_long)0x0020
#define VIEW_ATTR_NLINK		(u_long)0x0040
#define VIEW_ATTR_SIZE		(u_long)0x0080
#define VIEW_ATTR_ATIME		(u_long)0x0100
#define VIEW_ATTR_MTIME		(u_long)0x0200
#define VIEW_ATTR_CTIME		(u_long)0x0400
#define VIEW_ATTR_RDEV		(u_long)0x0800
#define VIEW_ATTR_BLKSIZE	(u_long)0x1000
#define VIEW_ATTR_NBLOCKS	(u_long)0x2000
#define VIEW_ATTR_AUDITED	(u_long)0x80000000

#define VIEW_ATTR_ALL  (VIEW_ATTR_TYPE|VIEW_ATTR_MODE|VIEW_ATTR_USID|\
			VIEW_ATTR_GSID|VIEW_ATTR_FSID|VIEW_ATTR_NODEID|\
			VIEW_ATTR_NLINK|VIEW_ATTR_SIZE|VIEW_ATTR_ATIME|\
			VIEW_ATTR_MTIME|VIEW_ATTR_CTIME|VIEW_ATTR_RDEV|\
			VIEW_ATTR_BLKSIZE|VIEW_ATTR_NBLOCKS|VIEW_ATTR_AUDITED)

#define VIEW_ATTR_STAT (VIEW_ATTR_MODE|VIEW_ATTR_USID|VIEW_ATTR_GSID|\
			VIEW_ATTR_FSID|VIEW_ATTR_NODEID|VIEW_ATTR_NLINK|\
			VIEW_ATTR_SIZE|VIEW_ATTR_ATIME|VIEW_ATTR_MTIME|\
			VIEW_ATTR_CTIME|VIEW_ATTR_RDEV|VIEW_ATTR_AUDITED)

#define VIEW_ATTR_TIMES (VIEW_ATTR_ATIME|VIEW_ATTR_MTIME|VIEW_ATTR_CTIME)

#define VIEW_ATTR_NOSET (VIEW_ATTR_NLINK|VIEW_ATTR_RDEV|VIEW_ATTR_FSID|\
			 VIEW_ATTR_NODEID|VIEW_ATTR_TYPE|\
			 VIEW_ATTR_BLKSIZE|VIEW_ATTR_NBLOCKS)
#define VIEW_ATTR_FMODES_MASK (TBS_FMODE_ISUID|TBS_FMODE_ISGID|TBS_FMODE_ISVTX|\
			       TBS_FMODE_IRWXU|TBS_FMODE_IRWXG|TBS_FMODE_IRWXO)

typedef struct view_set_attr {
    u_long		mask;	/* bit mask for attributes */
    tbs_ftype_t		type;	/* file type (for create only) */
    tbs_fmode_t		mode;	/* protection mode bits */
    credutl_sid_t	usid;	/* owner user sid */
    credutl_sid_t	gsid;	/* owner group sid */
    ks_uint64_t		size;	/* file size in bytes */
    struct timeval	atime;	/* time of last access */
    struct timeval	mtime;	/* time of last modification */
} view_set_attr_t;

typedef u_long view_build_session_t;
#define VIEW_BUILD_SESSION_NONE 0L		/* No build session */
/*
 * This needs to be no bigger than a u_long on the XDR wire, i.e. 32 bits.
 * otherwise, it can end up out of range of XDR and the xdr_u_long
 * routine may complain.
 */
#define VIEW_BUILD_SESSION_INVALID (0xffffffff)	/* Invalid build session number */

typedef struct view_handle {
    tbs_uuid_t view_uuid;
} view_handle_t;

typedef struct view_bhandle {
    view_build_session_t build_session;
    u_long target_id;		/* target identifier, used by clearmake to
				   identify a particular target being built */
} view_bhandle_t;

/*
 * For LFS support, we decided not to increase the size of the 
 * dirent structure.  That is, the directory size can grow only
 * to 2GB.
 */
typedef ks_off32_t view_dir_off_t;
#define xdr_view_dir_off_t xdr_ks_off32_t

/****************************************************************************
 * view_readdir types
 */
typedef struct view_dirent {	/* data from readdir() */
    u_long d_ino;	/* inode number of entry */
    view_dir_off_t d_off;       /* offset of disk direntory entry */
    u_short d_reclen; 	/* length of this record */
    u_short d_namlen;   /* name length */
    tbs_name_t d_name;  /* name of file */
} view_dirent_t;

/* The view_dir_off_t is the key value for alignment */
#define	VIEW_DIRENT_SIZE(namelen) \
	(((((struct view_dirent *) 0)->d_name - \
        (char *) 0 + 1 + (sizeof(view_dir_off_t) - 1)) + (namelen)) & ~(sizeof(view_dir_off_t) - 1))
/*
 * To step to the next view_dirent_t
 */
#define VIEW_DIRENT_NEXT_ENTRY(dirent) \
        ((view_dirent_t *) ((char *)(dirent) + \
        VIEW_DIRENT_SIZE((dirent)->d_namlen)))

/****************************************************************************
 * view_readdir_ext types
 */
typedef u_long view_name_state_t;
#define VIEW_NAME_STATE_NULL		(0L)
#define VIEW_NAME_STATE_VIEW		(0x1L) /* This name is from the view */
#define VIEW_NAME_STATE_HIDDEN		(0x2L) /* This name is hidden by another name shown by readdir_ext */
#define VIEW_NAME_STATE_NONE		(0x4L) /* CFS rule says not seen */
#define VIEW_NAME_STATE_ERROR		(0x8L) /* CFS rule says the name errors */
#define VIEW_NAME_STATE_REMOVABLE	(0x10L) /* This name is removable */
#define VIEW_NAME_STATE_CHECKED_OUT	(0x20L) /* This name is checked out */
#define	VIEW_NAME_STATE_NOCHECKOUT	(0x40L)	/* This name may not be checked out */
#define VIEW_NAME_STATE_MKBRANCH	(0x80L)	/* Branch is to be automatically created on checkout */
#define VIEW_NAME_STATE_ENOTENT		(0x100L) /* This name will return ENOENT */
#define VIEW_NAME_STATE_FROZEN		(0x200L) /* This name has frozen config */
#define VIEW_NAME_STATE_UCM             (0x00080000) /* Name is selected by a UCM element rule */
/* NOTE WELL: other name state flag bits are defined in view_pvt.h! */
#define VIEW_NAME_STATE_NOT_VISIBLE 	(VIEW_NAME_STATE_HIDDEN | \
    VIEW_NAME_STATE_NONE | VIEW_NAME_STATE_ERROR | VIEW_NAME_STATE_ENOTENT)

typedef struct view_dirent_ext {/* data from readdir_ext() */
    u_long ino;                 /* inode number of entry */
    view_dir_off_t off;		/* offset of disk direntory entry */
    view_name_state_t state;
    cfs_rule_num_t rule_number;
    tbs_oid_t cataloged_oid;	/* oid of the object that this name catalogs */
    vob_mtype_t cataloged_mtype;/* mtype of the cataloged object */
    tbs_oid_t config_match_oid;	/* if the cataloged object was a versioned
				   element, the configuration matched oid */
    vob_mtype_t config_match_mtype;
    u_short namlen;		/* name length */
    tbs_name_t name; 		/* name of file */
} view_dirent_ext_t;

/* The view_dir_off_t is the key value for alignment */
#define	VIEW_DIRENT_EXT_SIZE(namelen) \
	(((((struct view_dirent_ext *) 0)->name - (char *) 0 + 1 + \
	   (sizeof(view_dir_off_t) - 1)) + (namelen)) & ~(sizeof(view_dir_off_t) - 1))
/*
 * To step to the next view_dirent_ext_t
 */
#define VIEW_DIRENT_EXT_NEXT_ENTRY(dirent) \
        ((view_dirent_ext_t *) ((char *)(dirent) + \
        VIEW_DIRENT_EXT_SIZE((dirent)->namlen)))

/****************************************************************************
 * View history mode warping options.
 */
typedef enum view_hm_warp_opt {
    VIEW_HM_WARP_OPT_NONE,		/* No history mode change */
    VIEW_HM_WARP_OPT_ELEMENT_WARP,	/* return history mode element dir, if
					   not already in history mode */
    VIEW_HM_WARP_OPT_VERSION_WARP	/* return history mode version dir, if
					   not already in history mode */
} view_hm_warp_opt_t;

/* readdir flags:
 *    By default, the ino, off, state, namelen and name are returned.
 *    VIEW_READDIR_FLAG_ACTIVATE - Activates the object pointed to by ino.
 *        cataloged_mtype and cataloged_oid are valid.
 *    VIEW_READDIR_FLAG_CM - Activates the object and config matches
 *        vob elements versions. config_match_oid, config_match_mtype
 */
typedef u_long view_readdir_flag_t;

#define VIEW_READDIR_FLAG_NULL		((view_readdir_flag_t)0x0) 
#define VIEW_READDIR_FLAG_ACTIVATE	((view_readdir_flag_t)0x1) 
#define VIEW_READDIR_FLAG_CM		((view_readdir_flag_t)0x2) 
/*
 * This needs to be no bigger than a u_long on the XDR wire, i.e. 32 bits.
 * otherwise, it can end up out of range of XDR and the xdr_u_long
 * routine may complain.
 */
#define VIEW_READDIR_FLAG_ALL		((view_readdir_flag_t)0xffffffff) 

typedef enum view_invalidate_type {
    VIEW_INVALIDATE_TYPE_NULL,
    VIEW_INVALIDATE_TYPE_VIEW,	/* Invalidate all objects in a particular view.
				   No args are needed.
				   the VOB query. */
    VIEW_INVALIDATE_TYPE_VOB,	/* Invalidate all objects in a particular vob.
				   The replica_oid needs to be filled in to identify
				   the intended VOB.
				   */
    VIEW_INVALIDATE_TYPE_OBJ, 	/* Invalidate an object in a particular vob.
				   The replica_oid and obj_oid need to be filled
				   in.  If the obj_oid is an element, it 
				   invalidates every it knows about any 
				   versions, branched, checkouts related
				   to that element.
				   */
    VIEW_INVALIDATE_TYPE_NAME	/* Invalidate a particular name in a directory.
				   The replica_oid, obj_oid and name need to be
				   filled in.  The obj_oid represents the oid
				   of the directory the name is in.  The
				   name is the leaf name to invalidate. */
} view_invalidate_type_t;

typedef enum view_change_oid_option {
    VIEW_CHANGE_OID_NULL,
    VIEW_CHANGE_OID_SET_AUDIT,
    VIEW_CHANGE_OID_CLR_AUDIT
} view_change_oid_option_t;

/*
 * Properties of views/view servers that can be queried/modified.
 *
 *  VIEW_PROP_READONLY,     is view read-only?
 *  VIEW_PROP_WORKSPACE,    was view created for a workspace? (not modifiable,
 *			    use view_ws_is_ws_view() to query)
 *  VIEW_PROP_WS_PTIME,	    preserve (copy) version times to workspace files?
 *			    (use ws_{get,set}_ptime_prop() to query/modify)
 *  VIEW_PROP_BUILDS_NSHAREABLE_DOS
 *                          Audited builds in this view created derived objects
 *                          that are not shareable by other views?
 *  VIEW_PROP_WS_USES_SLINKS ws is configured to use slinks (readonly, use
 *                          ws_get_uses_slinks() to query)
 *  VIEW_PROP_WS_USES_HARDLINKS ws is configured to use hard links (readonly,
 *                          use ws_get_uses_hardlinks() to query)
 *  VIEW_PROP_WS_HIJACKED_MODE ws hijacked object detection mode.
 *                          See view_ws_hijacked_mode_t.
 *                          (readonly, use ws_get_hijacked_mode() to query)
 *  VIEW_PROP_SUMVIEW       View is controlled by UCM
 *  VIEW_PROP_WEBVIEW       View was created for access via the web interface
 *  VIEW_PROP_CFS_VERSION   config spec identity version number (readonly)
 *                          If the view doesn't have a config spec set,
 *                          view_getprop() returns the status TBS_ST_NO_CFS_SET.
 *                          If the set cspec doesn't have any identity info,
 *                          the value CFS_IDENTITY_VER_NONE is returned.
 *  VIEW_PROP_LFS           View supports Large Files.
 */

typedef enum {
    VIEW_PROP_NULL = 0,
    VIEW_PROP_READONLY = 1,
    VIEW_PROP_WORKSPACE = 2,
    VIEW_PROP_WS_PTIME = 3,
    VIEW_PROP_BUILDS_NSHAREABLE_DOS = 4,
    VIEW_PROP_WS_USES_SLINKS = 5,
    VIEW_PROP_WS_USES_HARDLINKS = 6,
    VIEW_PROP_REUSABLE1_POST_V4_0 = 7,
    VIEW_PROP_REUSABLE2_POST_V4_0 = 8,
    VIEW_PROP_WS_HIJACKED_MODE = 9,
    VIEW_PROP_SUMVIEW = 10,
    VIEW_PROP_WEBVIEW = 11,
    VIEW_PROP_CFS_VERSION = 12,
    VIEW_PROP_LFS = 13,
    VIEW_NUM_PROPS
} view_prop_t;

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* VIEW_BASE_H */
/* $Id: 35a12eaa.296a11e0.81f3.00:01:83:0a:3b:75 $ */
