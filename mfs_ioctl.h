/* * (C) Copyright IBM Corporation 1991, 2010. */
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
#ifndef MFS_IOCTL_H_
#define MFS_IOCTL_H_

/* Description
 *	This header file defines the ioctl() commands supported
 *	by the Atria FS.  All of these ioctl() commands 
 *	are used by first opening an object on the special viewroot
 *	mount point (/view/.specdev) and then performing the ioctl. 
 *      The first arg of the structure must be the pname of the
 *	object (for those ioctl's that affect objects).
 */

#include <linux/types.h>
#include <linux/param.h>
#include <linux/in.h>

#include "tbs_base.h"

#include <view_base.h>

#define CASEIS(x)

#define UNIONIS(x)

/* 
 * MVFS i/o controls
 *
 * 	The char 'M' is not unique on all systems.
 *
 *	Since the object which is opened by the user is a 
 *	known MVFS object for all ioctl's (either the special view mount 
 *      point or .specdev), then there should be no possibility of confusion
 *	due to ioctl number overlap.  I also start at '100'
 * 	just to be extra sure.
 *
 *	All ioctl's must be done on the special device file
 *	MVFS_SPECDEV on the view root mount.  The object to 
 *	perform the ioctl on is indicated by a pathname in
 *	the structure passed to the ioctl.
 *
 *	For maximum compatibility, no IOCTL structure should
 *	be larger than 127 bytes.
 *
 *	Arguments are always passed as a pointer to the structure.
 *
 *	For those structures that contain "struct mfs_strbuf"
 *	see the comment before the mfs_strbuf definition below.
 *
 * TBS_ST_* status codes:
 *	Several of the newer ioctls return a 0 errno status, and a
 *	TBS_ST_* error status in the ioctl data block.  Generally, the
 *	returns are as follows:  specific additions/variations are documented
 *	with the specific ioctl.
 *	TBS_ST_E* (unix error code): 	Returned for any lookup failure if the
 *					ioctl takes a pathname to lookup
 *	TBS_ST_NOT_AN_OBJ:		Returned if the object looked up must
 *					be an MVFS object, and it isn't.
 */

/*
 * Local definitions of IOR, IOW, IOWR macros so this hdr file
 * is insensitive to ANSI vs. non-ANSI systems  (There is no
 * way to support the old syntax in ANSI, so systems going to ANSI
 * C are changing the interface incompatibly with nothing to check
 * for this case!)  This hdr file creates an ANSI form of these.
 * ANSI is unable to '' a macro argument, so it requires that the
 * char arg be 'M' instead of just M with the macro doing the quoting
 * (like the old forms of IOW, IOR etc.)
 */

#ifndef _IOCTYPE
#define _IOCTYPE	0xff00
#endif
#ifndef _IOC_VOID
#define _IOC_VOID	IOC_VOID
#define _IOC_OUT	IOC_OUT
#define _IOC_IN		IOC_IN
#define _IOC_INOUT	IOC_INOUT
#endif
#ifndef _IOCPARM_MASK
#define _IOCPARM_MASK	IOCPARM_MASK
#endif
/*
 * WARNING:  These need to match up with the system's definitions,
 * e.g. in <sys/ioctl.h>, to insure proper action in any copying/checking
 * done by the system layers before calling VOP_IOCTL().
 *
 * Make sure these are ints since some compilers get picky with compares because
 * some IOCTLs set the high order bit.
 */
#define MIO(x,y)        ((int)(_IOC_VOID|((x)<<8)|y))
#define MIOR(x,y,t)     ((int)(_IOC_OUT|((sizeof(t)&_IOCPARM_MASK)<<16)|((x)<<8)|y))
#define MIOW(x,y,t)     ((int)(_IOC_IN|((sizeof(t)&_IOCPARM_MASK)<<16)|((x)<<8)|y))
#define MIOWR(x,y,t)    ((int)(_IOC_INOUT|((sizeof(t)&_IOCPARM_MASK)<<16)|((x)<<8)|y))

#ifndef MVFS_IOCTL_MAXLEN
#define MVFS_IOCTL_MAXLONGS	32
#define MVFS_IOCTL_MAXLEN	MVFS_IOCTL_MAXLONGS*sizeof(u_long)
#endif

/*
 * IOCTL command for UNIX and optionally NT.
 */

#define MVFS_IOCTL_CMD	MIOWR('M', 101, struct mvfscmd_block)

/*
 * Name of file on viewroot to open for all ioctl's
 */

#define MVFS_SPECDEV ".specdev"

/*
 * Name of canned pathname relative to the
 * 'hostdata' dir for the specdev device.
 * This is actually a symlink created by mount_mfs
 * to the <viewroot_mount_point>/.specdev
 * Normal usage is:
 *	tbs_product_hostdata_dir(pname, MVFS_SPECDEV_HOSTDATA_RELPN)
 */
#define MVFS_SPECDEV_HOSTDATA_RELPN	"clearcase_specdev"

#define MVFS_CMD_PN_NOFOLLOW_SYMLINK	0x0001	/* Don't follow symlink on lookup */

#define MVFS_CMD_PN_CS_LOOKUP		0x0002	/* case sensitive lookup */

typedef u_long mvfs_cmd_pn_flags_t;

/* 
 * This structure is used everywhere for passing in/out strings (use
 * mfs_strbufpn to pass pathnames).  On input (usually a set
 * operation) the "s" field is set up with the ptr, and "l" with the
 * length (as returned from strlen not including the null).  
 * On output, the "s" field is set up with a ptr to the return
 * buffer, and the "m" with the maximum size of that buffer.
 * On output, if the "s" field is NULL, or the "m" field is
 * 0, then that parameter is not returned.  This allows the
 * caller to avoid allocating space for return strings he is
 * not interested in.
 */
struct mfs_strbuf {
	KS_CHAR *s;	/* string ptr */
	size_t 	l;	/* Length in/out */
	size_t 	m;	/* buffer size */
};
typedef struct mfs_strbuf mfs_strbuf_t;

#define MFS_INIT_STRBUF_IN(BP, S) \
    { \
	(BP)->s = (KS_CHAR *) S; \
	(BP)->l = (size_t) STRLEN((char *) S); \
	(BP)->m = (BP)->l + 1; \
    }
#define MFS_INIT_STRBUF_OUT(BP, S, M) \
    { \
	(BP)->s = (KS_CHAR *) S; \
	(BP)->l = (size_t) 0; \
	(BP)->m = (size_t) M; \
    }

/* Inform Purify which bytes were written. */

#define MFS_DECLARE_WRITTEN_STRBUF(BP) \
    { \
        if ((BP)->s != NULL) { \
            PURIFY_MARK_AS_INITIALIZED((BP)->s, (BP)->m); \
            PURIFY_MARK_AS_INITIALIZED(&((BP)->l), sizeof ((BP)->l)); \
        } \
    }

/* 
 * This structure is used everywhere for passing in/out
 * pathnames (use mfs_strbuf to pass strings).  On input (usually a
 * set operation) the "s" field is set up with the ptr, and "l" with
 * the length (as returned from strlen not including the null).  
 * On output, the "s" field is set up with a ptr to the return
 * buffer, and the "m" with the maximum size of that buffer.
 * On output, if the "s" field is NULL, or the "m" field is
 * 0, then that parameter is not returned.  This allows the
 * caller to avoid allocating space for return strings he is
 * not interested in.
 */
struct mfs_strbufpn {
	ks_canon_pname_p_t s;	/* Pathname or name ptr */
	size_t 	l;	/* Length in/out */
	size_t 	m;	/* buffer size */
};
typedef struct mfs_strbufpn mfs_strbufpn_t;

#define MFS_INIT_STRBUFPN(BP) \
    ( \
        (BP)->s = NULL, \
        (BP)->l = 0, \
        (BP)->m = 0 \
    )

#define MFS_INIT_STRBUFPN_IN(BP, PNP) \
    ( \
	(BP)->s = (ks_canon_pname_p_t) PNP, \
	(BP)->l = (size_t) STRLEN(PNP), \
	(BP)->m = (BP)->l + 1 \
    )
#define MFS_INIT_STRBUFPN_OUT(BP, S, M) \
    ( \
	(BP)->s = (ks_canon_pname_p_t) S, \
	(BP)->l = (size_t) 0, \
	(BP)->m = (size_t) M \
    )

/*
 * Following is a structure used for passing pairs of strbuf_pn
 * in/out of the kernel.
 */
struct mfs_strbufpn_pair {
    mfs_strbufpn_t upn; /* userspace */
    mfs_strbufpn_t kpn; /* kernelspace */
};
typedef struct mfs_strbufpn_pair mfs_strbufpn_pair_t;

#define MFS_INIT_STRBUFPN_PAIR(BP) \
    ( \
        (BP)->upn.s = (BP)->kpn.s = NULL, \
        (BP)->upn.l = (BP)->kpn.l = 0, \
        (BP)->upn.m = (BP)->kpn.m = 0 \
    )

#define MFS_INIT_STRBUFPN_PAIR_IN(BP, UPNP, KPNP) \
    { \
	size_t ul, kl; \
 \
	ul = kl = (size_t) STRLEN(UPNP); \
	if ((UPNP) != (KPNP)) \
	    kl = (size_t) STRLEN(KPNP); \
	(BP)->upn.s = (ks_canon_pname_p_t) UPNP; \
	(BP)->upn.l = ul; \
	(BP)->upn.m = ul + 1; \
	(BP)->kpn.s = (ks_canon_pname_p_t) KPNP; \
	(BP)->kpn.l = kl; \
	(BP)->kpn.m = kl + 1; \
    }

/* Inform Purify which bytes were written.
 */
#define MFS_DECLARE_WRITTEN_STRBUFPN_PAIR(BP) \
{ \
    MFS_DECLARE_WRITTEN_STRBUF(&((BP)->upn)); \
    MFS_DECLARE_WRITTEN_STRBUF(&((BP)->kpn)); \
}

#define MFS_INIT_STRBUFPN_PAIR_IN_ABSOBJPN(PAIRP, PATHP, RC) \
    { \
	RC = 0; \
	MFS_INIT_STRBUFPN_PAIR_IN (PAIRP, PATHP, PATHP); \
    }

/* not allocated on UNIX; don't free--just null out */
#define MFS_FREE_STRBUFPN_PAIR_IN_ABSOBJPN(BP) \
    { \
	    (BP)->upn.s = (BP)->kpn.s = NULL; \
    }

/* We only care about upn on Unix/Linux (even though the macros above always
** fill in both of them.
*/
#define MFS_STRBUFPN_PAIR_GET_KPN(PP) (PP)->upn
#define MFS_STRBUFPN_PAIR_GET_UPN(PP) (PP)->upn

/* We need a macro to copyin the strings pointed to by one of these pathname
** pairs after the actual pair has been copyin'd by CopyInMfs_strbufpn_pair().
** There is a little trickiness involved to handle errors and cleanup, so the
** order of things is important here.  The pnp_p (pathname pair pointer)
** argument is a "mfs_strbufpn_pair_t *pn_pair_p" that has been filled in by
** CopyInMfs_strbufpn_pair.  The macro updates the pointers in the structure to
** point to kernel versions of the strings (created by mfs_copyin_strbufpn,
** which returns an error if the input pointer is NULL and sets the returned
** pointer to NULL) and it assumes the .l and .m fields are set correctly
** (usually by user-space code using one of the macros above,
** e.g. MFS_INIT_STRBUFPN_PAIR_IN_ABSOBJPN).  It further assumes that the
** caller will take care of freeing stuff (even if there is an error, i.e. ec
** != 0) by using PNPAIR_STRFREE (which, for some reason, lives in mvfs.h even
** though mfs_strbufpn_pair_t is declared in this file).  Note, if the string
** pointers are different in user-space (one of them could even be NULL), they
** should be different after the copyin (unless there's an error, in which case
** they could both be NULL), and if they were the same (even if they're both
** NULL), they should be the same after the copyin.
*/
#define MFS_STRBUFPN_PAIR_COPYIN_STRS(pnp_p, ec) \
    if ((pnp_p)->kpn.s != (pnp_p)->upn.s) { /* Copyin both strings (one could be NULL). */ \
        if ((pnp_p)->upn.s != NULL) { /* upn is non-NULL. */ \
            (ec) = mfs_copyin_strbufpn((pnp_p)->upn, &((pnp_p)->upn.s)); \
            if ((ec) == 0) { /* upn is good.  */ \
                if ((pnp_p)->kpn.s != NULL) { /* Copyin kpn. */ \
                    (ec) = mfs_copyin_strbufpn((pnp_p)->kpn, &((pnp_p)->kpn.s)); \
                } /* else kpn is NULL, but that's OK (we have upn). */ \
            } else { /* It failed (upn set NULL), set kpn for cleanup (erase user-space ptr). */ \
                (pnp_p)->kpn.s = NULL; \
            } \
        } else { /* upn is NULL, kpn determines the final result. */ \
            (ec) = mfs_copyin_strbufpn((pnp_p)->kpn, &((pnp_p)->kpn.s)); \
        } \
    } else { /* Copyin one string (could be NULL), set the other the same. */ \
        (ec) = mfs_copyin_strbufpn((pnp_p)->upn, &((pnp_p)->upn.s)); \
        (pnp_p)->kpn.s = (pnp_p)->upn.s; \
    }
#define MFS_INIT_STRBUFPN_IN_ABSOBJPN(BP, PATHP, RC) \
    { \
	RC = 0; \
	MFS_INIT_STRBUFPN_IN (BP, PATHP); \
    }
#define MFS_FREE_STRBUFPN_IN_ABSOBJPN(BP) \
    { \
	    (BP)->s = NULL; \
    }

/*
 * Used to indicate optional fields solely for documentation
 * purposes.
 */
#ifndef OPTIONAL
#define OPTIONAL
#endif

struct mvfscmd_header {
    u_long cmd;
    mvfs_cmd_pn_flags_t pnflags;
    mfs_strbufpn_pair_t objnmpair;
};
typedef struct mvfscmd_header mvfscmd_header_t;

struct mvfscmd_block {
    mvfscmd_header_t hdr;
    u_long infolen;
    void *infop;
    tbs_status_t  status;
};
typedef struct mvfscmd_block mvfscmd_block_t;

#define MCB_CMD(MCBP) (MCBP)->hdr.cmd
#define MCB_PNFLAGS(MCBP) (MCBP)->hdr.pnflags

#define MCB_OBJPN(MCBP) (MCBP)->hdr.objnmpair.upn

struct mvfs_viewinfo {
	OPTIONAL mfs_strbuf_t	vname;		/* Name of view tag */
	OPTIONAL mfs_strbufpn_t spath;		/* Path to view storage */
	OPTIONAL mfs_strbuf_t	host;		/* Host name of view server */
	OPTIONAL mfs_strbufpn_t rpath;		/* Remote path of view storage */
	tbs_uuid_t	uuid;		/* View UID */
};
typedef struct mvfs_viewinfo mvfs_viewinfo_t;

#define MVFS_CMD_GET_VIEWINFO 1
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_VIEWINFO,
 *		MVFS_CMD_PN_(NO)FOLLOW_SYMLINK,
 *		pairp, viewinfop, sizeof(*viewinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/* 
 * Xstat structure.  Used for extra MVFS-specific stat information
 * about an object which is cached in the MVFS.  This structure
 * is padded out for future extensions.  As new fields are added,
 * replace spare words at the end.
 *
 * Currently the vstat only includes the low-order TBS_FMODE bits.
 * This call returns the high 16 bits (TBS_FMODE_AUDITED OBJ)
 * int he "xmode" field below.
 */

/* Object flag bits */

#define MVFS_XISROOT	0x0001	/* Object is a version of VOB root */
#define MVFS_XVXOBJ	0x0002	/* Version extended name object */
#define MVFS_XAUDITED	0x0004	/* "audited" bit set  */

struct mvfs_xstat {
	view_vstat_t	*vstat;		/* Extended view stat struct ptr */
	tbs_oid_t	vob_oid;	/* VOB oid of object */
	tbs_uuid_t	view_uuid;	/* View uuid of object */
	tbs_boolean_t	view_hm;	/* HM or not HM view reference */
	ks_uint32_t	xmode;		/* High 16 bits of TBS_FMODE */
	ks_uint32_t	flags;		/* object flags */
	tbs_uuid_t	replica_uuid;	/* Replica uuid of object */
	ks_uint32_t	spare[6];	/* Spare space for additions */
};
typedef struct mvfs_xstat mvfs_xstat_t;

#define MVFS_CMD_XSTAT 2
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_XSTAT,
 *		MVFS_CMD_PN_(NO)FOLLOW_SYMLINK,
 *		pnamep, xstatp, sizeof(*xstatp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/* Cleartext pool values */

#define MVFS_XCTNONE	0	/* No cleartext for this object */
#define MVFS_XCTVIEW	1	/* In View cleartext pool */
#define MVFS_XCTVOB	2	/* In VOB  cleartext pool */
#define MVFS_XCTERROR    3	/* Error on cleartext fetch */

struct mvfs_clrname_info {
	u_short		clrpool; /* Cleartext pool */
	mfs_strbufpn_t	clrname; /* Cleartext name */
};

typedef struct mvfs_clrname_info mvfs_clrname_info_t;

#define MVFS_CMD_GET_CLRNAME 3
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_CLRNAME,
 *		MVFS_CMD_PN_(NO)FOLLOW_SYMLINK,
 *		pnamep, clrnamep, sizeof(*clrnamep));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * iovfh structure for getting the view file handle
 */
struct mvfs_iovfh {
	view_fhandle_t	vfh;		/* View fhandle */
};
typedef struct mvfs_iovfh mvfs_iovfh_t;

#define MVFS_CMD_GET_VFH 4
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_VIEWINFO,
 *		MVFS_CMD_PN_(NO)FOLLOW_SYMLINK,
 *		pnamep, vhinfop, sizeof(*vhinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Cache invalidation ioctl.  These perform specific
 * cache invalidates.  Note that symlinks are always chased. 
 *
 */
struct  mvfs_ioinval {
	int		utype;		/* VOB oid/replica uuid */
	UNIONIS(utype) union		{
	    CASEIS(MVFS_IOINVAL_VOB_OID) tbs_oid_t vob_oid; /* VOB oid */
	    CASEIS(MVFS_IOINVAL_REPLICA_UUID) tbs_uuid_t replica_uuid; /* replica uuid */
	} un;
	u_long		invaltype;	/* Invalidate type */
	tbs_oid_t	obj_oid;	/* Object or element oid */
	OPTIONAL mfs_strbufpn_t	nm;		/* Name for name cache invalidate */
};
typedef struct mvfs_ioinval mvfs_ioinval_t;

#define MVFS_INV_NC	1	/* Invalidate name cache entry */
#define MVFS_INV_OBJ	2	/* Invalidate cache info for obj */
#define MVFS_INV_VIEW	3	/* Invalidate cache info for view */
#define MVFS_INV_VFS	4	/* Invalidate cache info for vfs */
#define MVFS_INV_ELEM   5       /* Invalidate cache info during "rmelem" and "unco" cmds */

#define MVFS_IOINVAL_VOB_OID 1
#define MVFS_IOINVAL_REPLICA_UUID 2

#define MVFS_CMD_IOINVAL 5
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_IOINVAL,
 *		0,
 *		pairp, ioinval_infop, sizeof(*ioinval_infop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_REVALIDATE is preferred way for individual processes
 * to cause the MVFS to revalidate its caches for that process
 * tree.  Flushes should only be used when you want to change
 * the cache evaluations for all processes.
 * What 'revalidate' does is require the MVFS to refetch any
 * cached attributes retained from before this call was made.
 * Basically it guarantees that this process will see the
 * latest and greatest truth from the view on objects it
 * touches after making this call.
 */
#define MVFS_CMD_REVALIDATE 6
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_REVALIDATE,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, NULL, 0);
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * On UNIX, we don't need the credentials when making a view tag.
 * If we try to pass them in, they won't fit inside the ioctl size limit.
 * So, for UNIX we just make the type very small and ignore it.  On NT, we
 * use the real type.
 */
typedef int viewtag_cred_t;
/*
 * MVFS i/o control for "view" objects themselves.  
 * The pathname must refer to a view tag in the viewroot mount.
 *
 * MVFS_CMD_MKVIEWTAG:	Must be used to create viewtags in /view.
 *			It creates the object if it does not exist and sets
 *			all the pathname/uuid/net address parameters.  If the object
 *			exists and the parameters match (with the exception of the
 *			net address), then the net address is simply updated with
 *			the new address information.  If the object exists and
 *			any pathname is different, or uuid is different, then
 *			TBS_ST_EEXIST is returned
 *			TBS_ST_MFS_ERR is returned if /view is not mounted.
 *			TBS_ST_E* status are returned for FS errors from making the
 *				viewtag in /view (i.e. the mkdir call inside the MVFS).
 * MVFS_CMD_RMVIEWTAG:	removes the viewtag
 */

struct mvfs_mkviewtag_info {
        mfs_strbuf_t	viewtag;	/* (IN)	 view tag */
	mfs_strbufpn_pair_t spath;	/* (IN)  Path to view storage */
        mfs_strbuf_t	host;		/* (IN)	 Host name of view server */
	mfs_strbufpn_t	rpath;		/* (IN)  Remote path of view storage */
	tbs_uuid_t	uuid;		/* (IN)  View UUID */
	ks_sockaddr_storage_t addr;	/* (IN)  Addr */
        u_int windows_view : 1;         /* (IN) Windows view */
        u_int pad : 31;                 /* (IN) filler */
}; 
typedef struct mvfs_mkviewtag_info mvfs_mkviewtag_info_t;

struct mvfs_viewaddr {
	ks_sockaddr_storage_t addr;	/* Internet address */
};
typedef struct mvfs_viewaddr mvfs_viewaddr_t;

#define MVFS_CMD_MKVIEWTAG 7
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_MKVIEWTAG,
 *		0,
 *		pairp, viewtaginfop, sizeof(*viewtaginfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

struct mvfs_viewtag_info {
        mfs_strbuf_t	viewtag;	/* (IN)	 view tag */
}; 
typedef struct mvfs_viewtag_info mvfs_viewtag_info_t;

#define MVFS_CMD_RMVIEWTAG 8
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_RMVIEWTAG,
 *		0,
 *		pairp, viewtaginfop, sizeof(*viewtaginfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_GET_VIEWADDR 9
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GETVIEWADDR,
 *		0,
 *		pairp, viewaddrp, sizeof(*viewaddrp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 *	Get VOB info by OID
 *	Scan all mounted VOB's to get VOB info
 * OID means VOB family oid (i.e. any replica)
 * UUID means VOB instance UUID (specific replica)
 *
 * Additional TBS status codes:
 *	TBS_ST_NOT_AN_OBJ:	If the object is not at or below the root of
 *				a vob. (e.g. a viewtag, /view dir, etc.)
 *	TBS_ST_NOT_FOUND:	For VOBINFO_OID/VOBINFO_UUID this means that
 *				a mount with the specified uuid or oid was
 *				not found.  For VOBINFO_ENT this means indicates
 *				the end of the VOB mount list (i.e. no more VOBS)
 *				and the current vobinfo is not valid.
 */
/* 
 * VOB information.  Takes pname, voboid, or cookie (scan)
 * to lookup list of vobs.  For scans:
 *    Initially set the cookie to 0 for the first vob.  
 *    On each return the cookie will be set to a value that will scan the
 *    next VOB.  
 *    After all the vobs have been scanned, an entry will be returned
 *    with an ioctl error of 0 (no error) and a tbs_status_t of
 *    TBS_ST_NOT_FOUND.  This indicates the end of the scan.
 *
 * Note: assumes ptr type size >= u_long size in the union.
 */

struct mvfs_vobinfo {
	int		utype;		/* union type */
	UNIONIS(utype) union		{
	    CASEIS(MVFS_VOBINFO_IN_PNAME) struct {
		mfs_strbufpn_pair_t pair; /* (IN) pname of object */
		mvfs_cmd_pn_flags_t pnflags; /* (IN) */
	    } pn_s;
	    CASEIS(MVFS_VOBINFO_IN_UUID) A_CONST tbs_uuid_t *vob_uuid; /* (IN) UUID of vob instance */
	    CASEIS(MVFS_VOBINFO_IN_OID) A_CONST tbs_oid_t  *vob_oid;  /* (IN) OID of vob family */
	    CASEIS(MVFS_VOBINFO_IN_COOKIE) u_long  cookie;		/* (IN/OUT) cookie for list of vobs */
        } vobid;
	OPTIONAL mfs_strbufpn_t mntpath;	/* Mount dir pname (mnt_dir) */
	OPTIONAL mfs_strbufpn_t spath;		/* Path to vob storage */
	OPTIONAL mfs_strbuf_t	host;		/* Host name */
	OPTIONAL mfs_strbufpn_t rpath;		/* Remote path of vob storage */
	tbs_oid_t	oid;		/* VOB family oid */
	tbs_uuid_t	uuid;		/* VOB instance uuid */
	tbs_boolean_t   unique;		/* if request was based on family oid, is the
					   result unique? */
};
typedef struct mvfs_vobinfo mvfs_vobinfo_t;

#define MVFS_VOBINFO_IN_PNAME 1
#define MVFS_VOBINFO_IN_UUID 2
#define MVFS_VOBINFO_IN_OID 3
#define MVFS_VOBINFO_IN_COOKIE 4

#define MVFS_CMD_GET_VOBINFO 10
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_VOBINFO,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, vobinfop, sizeof(*vobinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * ioctl to return pname to viewroot mountpoint.  Really this
 * is the pname prefix to the view tagnames, and should only
 * be used when you want to get a pname to contruct absolute
 * pathnames to a view tag.  A null string is returned
 * if /view is not mounted.
 */
#define MVFS_CMD_GET_VIEWTAG_DIR 11
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_VIEWTAG_DIR,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, pairp, sizeof(*pairp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS IO change mtype structure 
 */

struct mvfs_iochange_mtype {
        vob_mtype_t     mtype;		/* (IN) Mtype to change it to */
};
typedef struct mvfs_iochange_mtype mvfs_iochange_mtype_t;

#define MVFS_CMD_CHANGE_MTYPE 12
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_CHANGE_MTYPE,
 *		0,
 *		pnamep, mtypeinfop, sizeof(*mtypeinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_GET_AFILE 13
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_AFILE,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, pathp, sizeof(*pathp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_SET_AFILE 14
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_AFILE,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, pairp, sizeof(*pairp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS proc flags 
 * Pass in the addr of these flags to set/get proc flags and
 * start audit.
 */

#define MVFS_PF_USER	   0x0000ffff	/* Read/Write proc flags */
#define MVFS_PF_AUDITVOB	   0x00000001	/* Audit VOB objects */
#define MVFS_PF_AUDITNONVOB 0x00000002	/* Audit non-VOB objects */
#define MVFS_PF_SYS	   0xffff0000	/* Read-only proc flags */
#define MVFS_PF_AUDITON	   0x00010000	/* Auditing enabled */

#define MVFS_CMD_GET_PROCF 15
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_PROCF,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, audit_flagsp, sizeof(*audit_flagsp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_SET_PROCF 16
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_PROCF,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, audit_flagsp, sizeof(*audit_flagsp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_START_AUDIT 17
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_START_AUDIT,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, audit_flagsp, sizeof(*audit_flagsp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_STOP_AUDIT 18
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_STOP_AUDIT,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, NULL, 0);
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_SYNC_AUDIT 19
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SYNC_AUDIT,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, NULL, 0);
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Get system-wide history mode (extended name) suffix.
 */
/* Combination of MVFS_VX_VERS_CHAR and vxsuffix equals a name
 * which gets you the current version (in version extended namespace)
 * of the directory.  .@@-> element (needs main/LATEST after it), but
 * ^@@ goes to version (goes right to file element next).
 */
#define MVFS_VX_VERS_CHAR	'^'

#define MVFS_CMD_GET_VXSUFFIX 20
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_VXSUFFIX,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, suffixp, sizeof(*suffixp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Cache control.
 * Must be superuser to set cache enables.
 *
 */
/*
 * Constants for get/set cache enable mask word.
 *   If a bit is on, that cache is enabled for the system, but may
 *	be disabled by a mount option.
 *   If a bit is off, that cache is disabled for the system.  This
 *	overrides any mount options.
 *
 *   The default for the system is all caches enabled.
 */
#define MVFS_CE_ATTR	0x00000001	/* Enable attribute caching */
#define MVFS_CE_NAME	0x00000002	/* Enable name caching */
#define MVFS_CE_NOENT	0x00000004	/* Enable "not found" name caching */
#define MVFS_CE_RVC	0x00000008	/* Enable "root version cache" */
#define MVFS_CE_SLINK	0x00000010	/* Enable symlink text caching */
#define MVFS_CE_CTO	0x00000020	/* Enable "close to open" consistency */
#define MVFS_CE_CWDREBIND 0x0000040	/* Enable "cwd rebinding" */

#define MVFS_CMD_GET_CACHE_ENB 21
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_CACHE_ENB,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, cache_maskp, sizeof(*cache_maskp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_SET_CACHE_ENB 22
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_CACHE_ENB,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, cache_maskp, sizeof(*cache_maskp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Constants for flush command mask word.  OR multiple
 * values together to flush multiple MVFS caches at once.
 */
#define MVFS_CF_MN	0x00000001	/* Flush cached free mnodes */
#define MVFS_CF_NC	0x00000002	/* Flush the name cache */
#define MVFS_CF_RVC	0x00000004	/* Flush the root version cache */
#define MVFS_CF_LKUP	0x00000008	/* Flush the lookup cred cache */

#define MVFS_CMD_FLUSH_CACHE 23
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_FLUSH_CACHE,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, cache_maskp, sizeof(*cache_maskp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Structures for reading the MVFS Name Cache Read
 *
 * Notes:
 * 	BHLIST: 0xffffffff.0xffffffff is used for invalid entries.
 *	        The BHlist never includes the Null BH (0.0).  Whether this
 *		BH is valid or not is indicated by the MVFS_IONCNULLBH bit 
 *		in the flags word.
 *
 *	For ENOENT (e.g. cached name not found returns) the following
 *	fields do not have meaningful information:
 *	    vw:		Will return a null string
 *	    fid:	Will return 0.0
 *	    evtime:	Will return 0.0
 */

#define MVFS_IONC_NULLBH	0x0001	/* Null BH is a valid build handle */
#define MVFS_IONC_ENOENT	0x0002	/* ENOENT cache entry */
#define MVFS_IONC_INVALID       0x0004  /* Entry has been invalidated */
#define MVFS_IONC_BHINVARIANT   0x0008  /* Entry is BH invariant */
#define MVFS_IONC_NOTINDIR      0x0010  /* ENOENT entry cause name not in dir */
#define MVFS_IONC_CASE_INSENSITIVE 0x0020   /* Case insensitive NC entry */
#define MVFS_IONC_RVC		0x0040  /* entry is an RVC */

struct mvfs_iofid {
	u_long	dbid;		/* Version dbid */
	u_long  gen;		/* Generation number */
};
typedef struct mvfs_iofid mvfs_iofid_t;

#define MVFS_IONCBHMAX	3	/* Should match MFS_DNCBHMAX */

struct mfs_ioncent {
	u_long	     offset;	/* (INOUT) offset of next entry  (1) */
	u_short	     eocache;	/* If non-zero, no entry - at EOCACHE */
	u_short	     flags;	/* flags  (2) */
	time_t	     addtime;	/* Time entry added (3) */
	OPTIONAL mfs_strbufpn_t dvw;	/* Directory view tag name (5) */
	OPTIONAL mfs_strbufpn_t mp;	/* Mount point pname (7) */
	OPTIONAL mvfs_iofid_t  dfid;	/* Dir FID info (9) */
	OPTIONAL mfs_strbufpn_t nm;	/* Name in dir (11) */
	view_bhandle_t bhlist[MVFS_IONCBHMAX]; /* Valid build handles (17) */
	OPTIONAL mfs_strbufpn_t vw;	/* Result view tag name (19) */
	mvfs_iofid_t  fid;	/* Result FID info (21) */
	struct timeval evtime;	/* Result VOB event time (23) */
};
typedef struct mfs_ioncent mfs_ioncent_t;

#define MVFS_CMD_READ_DNC 24
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_READ_DNC,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, dncinfop, sizeof(*dncinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Get version strings
 */
#define MVFS_CMD_GET_RCSID 25
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_RCSID,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, rcsidp, sizeof(*rcsidp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_GET_SCCSID 26
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_SCCSID,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, sccsidp, sizeof(*sccsidp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Get/set error logging info
 */
/*
 * Error logging levels 
 */

#define MFS_LOG_NONE	0	/* Turn off mvfs logging */
#define MFS_LOG_ERR	3	/* Log errors */
#define MFS_LOG_WARN	4	/* Log warnings */
#define MFS_LOG_INFO	5	/* Log info messages */
#define MFS_LOG_ESTALE	6	/* Log ESTALE/EINTR errs (pseudo-level) */
#define MFS_LOG_DEBUG	7	/* Log debug messages/traps */
#define MFS_LOG_ENOENT  8	/* Log name not found errors */

#define MVFS_LOGINFO_PRIORITY		0x00000001
#define MVFS_LOGINFO_IOOPS		0x00000002 /* used on Windows */
#define MVFS_LOGINFO_VOPS		0x00000004
#define MVFS_LOGINFO_VFSOPS		0x00000008
#define MVFS_LOGINFO_XOPS		0x00000010
#define MVFS_LOGINFO_TRAPS		0x00000040
#define MVFS_LOGINFO_KERNLOGFILE	0x00000100
#define MVFS_LOGINFO_PROCONOFF		0x00000200  /* NT PROC STATS */
#define MVFS_LOGINFO_PROCPRINT		0x00000400  /* NT PROC STATS */
#define MVFS_LOGINFO_PROCLEVEL		0x00000800  /* NT PROC STATS */
#define MVFS_LOGINFO_PANICONOFF         0x00001000  /* ASSERT PANICS ON-OFF */

struct mvfs_loginfo {
    u_long mask;
    u_long priority;
    u_long io_ops_mask;
    u_long vops_mask;
    u_long vfsops_mask;
    u_long xops_mask;
    u_long traps_mask;
    tbs_boolean_t assert_panic_on;
    mfs_strbufpn_t kernlog_pn;
};

typedef struct mvfs_loginfo mvfs_loginfo_t;

#define MVFS_CMD_GET_LOGINFO 27
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_LOGINFO,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, infop, sizeof(*infop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_SET_LOGINFO 28
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_LOGINFO,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, infop, sizeof(*infop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * BH ioctls.  Takes the "reference time" and flags
 * to enable/disable the use of various name cache optimizations.
 * 
 *    MVFS_CMD_BHF_REVALIDATE - sets cache revalidation time to
 *	the current time.  This should be set for parallel distributed
 *	builds to force the MVFS to get the truth from the view for
 *	all referenced objects by this process (or its children)
 *	after this point and avoid inconsistencies in the build
 *	from changes on other 3'rd party nodes.
 *	(This is the same as doing the BHINVAL ioctl.)
 *    MVFS_CMD_BHF_DNC_REFTIME - enables name cache optimizations 
 *	that are valid when ref time is the only modification
 *	to the config spec that the build handle makes.  Do not
 *	set this bit if the build handle has a target stack.
 *	Currently "ref_time" is ignored unless this bit is set.
 *
 */
struct mvfs_bhinfo {
    view_bhandle_t bh;
    struct timeval bh_ref_time;
    u_long	   flags;
};
typedef struct mvfs_bhinfo mvfs_bhinfo_t;

#define MVFS_BHF_REVALIDATE	0x00000001
#define MVFS_BHF_DNC_REFTIME	0x00000002

#define MVFS_CMD_GET_BH 29
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_BH,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, bhinfop, sizeof(*bhinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_SET_BH 30
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_BH,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, bhinfop, sizeof(*bhinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Structure for fetching stats
 * Only those buffers with non-NULL ptrs are fetched.
 */

struct mvfs_statbufs {
	OPTIONAL mfs_strbuf_t clntstat;
	OPTIONAL mfs_strbuf_t mnstat;
	OPTIONAL mfs_strbuf_t clearstat;
	OPTIONAL mfs_strbuf_t rvcstat;
	OPTIONAL mfs_strbuf_t dncstat;
	OPTIONAL mfs_strbuf_t acstat;
	OPTIONAL mfs_strbuf_t rlstat;
	OPTIONAL mfs_strbuf_t austat;
	OPTIONAL mfs_strbuf_t vnopcnt;
	OPTIONAL mfs_strbuf_t vfsopcnt;
	OPTIONAL mfs_strbuf_t viewopcnt;
	OPTIONAL mfs_strbuf_t viewoptime;
	OPTIONAL mfs_strbuf_t viewophist;
};
typedef struct mvfs_statbufs mvfs_statbufs_t;
	
#define MVFS_CMD_GET_STATS 31
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GETSTATS,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, statsp, sizeof(*statsp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Get/set process "view" state.  These calls can
 * be used to set the process 'view' instead of doing a 
 * chroot to the viewtag.  They are intended for server processes
 * that want to service multiple views without exec'ing all the
 * time to do a new chroot.  Child procs  will inherit
 * the parents view state AS LONG AS THE PARENT HAS NOT CHANGED
 * ITS CURRENT PROCESS VIEW TO ANOTHER VIEW!  If the parent does 
 * do this, the child proc will unpredictably latch onto one view or
 * the other.  Recommended usage is:
 *    ioctl (MVFS_SETPROCVIEW, <view1>)
 *    fork/exec process
 *    parent waits for children.
 *    ioctl(MVFS_SETPROCVIEW, <view2>)
 *
 * If the server must run asyncronously with the child, then 
 * the child should set its process view explicitly before doing
 * the exec.  i.e.
 *    fork
 *    in child, 
 *	ioctl(MVFS_SETPROCIVEW, <view1>)
 *      exec(.....)
 *    in parent,
 *      parent can keep running and set proc view freely without
 *	adversely affecting the child process.
 *
 * Note: a 'view' set by 'setprocview' will be eclipse (i.e. 
 * override) a view set by chroot.
 *
 * Setprocview returns a tbs_status_t.
 * Getprocview takes in a viewinfo struct, and copies out the current
 * procview info to it.  If no process view is set, then ENOENT returned.
 *
 * Additional TBS status codes for SETPROCVIEW:
 *    TBS_ST_E*:	Any error in looking up the viewtag in /view.
 */

#define MVFS_CMD_SETPROCVIEW 32
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SETPROCVIEW,
 *		0,
 *		pairp, viewtag_infop, sizeof(*viewtag_infop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_GET_PROCVIEWINFO 33
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_PROCVIEWINFO,
 *		0,
 *		MFS_NULL_STRBUFPN_PAIR, viewinfop, sizeof(*viewinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * Extended file object attributes.  For now, only
 * get/set an unsigned 32 bit value.  Later on, we may
 * add a 'ptr' form to allow pointers to more exotic types.
 */
/*
 * MVFS IO set/get integer extended attribute field.
 * Note: this should be used to replace the change_mtype call
 * in the future.
 */
typedef enum {
	MVFS_IO_XATTR_NULL,			/* Unused */
	MVFS_IO_XATTR_MTYPE,		/* Mtype attribute */
	MVFS_IO_XATTR_XMODE,		/* Extended mode bits (audited) */
	MVFS_IO_XATTR_NTFILEATTRS		/* Reserved for NT port */
} mvfs_io_xattr_type_t;

struct mvfs_io_xattr  {
	mvfs_io_xattr_type_t	xattr_type;	/* (IN) extended attr type */
	u_long			xvalue;		/* (IN/OUT) attr value */
};
typedef struct mvfs_io_xattr mvfs_io_xattr_t;
	
#define MVFS_CMD_GET_XATTR 34
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_XATTR,
 *		MVFS_CMD_PN_(NO)FOLLOW_SYMLINK,
 *		pnamep, ioxattrp, sizeof(*ioxattrp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_SET_XATTR 35
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_XATTR,
 *		MVFS_CMD_PN_(NO)FOLLOW_SYMLINK,
 *		pnamep, ioxattrp, sizeof(*ioxattrp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_MOUNT 36
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_MOUNT,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, mntargsp, sizeof(*mntargsp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

struct mvfs_unmount_info {
    mfs_strbufpn_t tag;
    tbs_uuid_t	replica_uuid;		/* replica UUID */
};

typedef struct mvfs_unmount_info mvfs_unmount_info_t;

#define MVFS_CMD_UNMOUNT 37
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_UNMOUNT,
 *		0,
 *		MFS_NULL_STRBUFPN_PAIR, infop, sizeof(*infop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_RMALLVIEWTAGS 38
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_RMALLVIEWTAGS,
 *		0,
 *		pairp, NULL, 0);
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_UNMOUNTALL 39
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_UNMOUNTALL,
 *		0,
 *		MFS_NULL_STRBUFPN_PAIR, infop, sizeof(*infop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS IO get pool map information
 */

struct mvfs_ioget_poolmaps {
    tbs_uuid_t	replica_uuid;		/* IN: which vob replica do we want */
    int		mapcount;		/* (IN/OUT) count of space/#returned */
    mfs_strbufpn_t	*patterns;	/* (IN/OUT) base ptr to array  */
    mfs_strbufpn_t	*replacements;	/* (IN/OUT) base ptr to array  */
};
typedef struct mvfs_ioget_poolmaps mvfs_ioget_poolmaps_t;

#define MVFS_CMD_GET_POOLMAPS 40
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_POOLMAPS,
 *		0,
 *		MFS_NULL_STRBUFPN_PAIR, poolmapinfop, sizeof(*poolmapinfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_VDM 41

#define MVFS_VDM_INITIALIZING	0x00000001
#define MVFS_VDM_TERMINATING	0x00000002
#define MVFS_VDM_BLOCKING	0x00000004
#define MVFS_VDM_RESUMING	0x00000008
#define MVFS_VDM_PDBCREATED	0x00000010
#define MVFS_VDM_PDBTERMINATED	0x00000020

struct mvfs_vdminfo {
    u_long action;
    u_long info;
};

typedef struct mvfs_vdminfo mvfs_vdminfo_t;

/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_VDM,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, vdminfop, sizeof(*vdminfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_ABORT aborts the MVFS and attempts to force a system crash dump.
 */
#define MVFS_CMD_ABORT 42
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_ABORT,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, NULL, 0);
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

struct mvfs_export_viewinfo {
	mfs_strbuf_t	viewtag;	/* (IN)  View tag name */
 	int		exportid;	/* (IN)  export ID for view */
}; 
typedef struct mvfs_export_viewinfo mvfs_export_viewinfo_t;

/*
 * MVFS_CMD_EXPORTVIEWTAG marks a view for export.
 */
#define MVFS_CMD_EXPORTVIEWTAG 43
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_EXPORTVIEWTAG,
 *		0,
 *		pairp, export_info, sizeof(*export_info));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_UNEXPORTVIEWTAG removes the view from the export list.
 */
#define MVFS_CMD_UNEXPORTVIEWTAG 44
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_UNEXPORTVIEWTAG,
 *		0,
 *		pairp, viewtag_info, sizeof(*viewtag_info));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_ZERO_STATS clears the statistics kept by the MVFS.
 */
#define MVFS_CMD_ZERO_STATS 45
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_ZERO_STATS, 0,
 *		&MFS_NULL_STRBUFPN_PAIR, NULL, 0);
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

typedef struct mvfs_cache_usage {
    u_int cache_usage[2][16];
} mvfs_cache_usage_t;
/* first dimension */
#define MVFS_CACHE_INUSE	0
#define MVFS_CACHE_MAX		1
/* second dimension */
#define	MVFS_CACHE_MNODE_TBL	0
#define	MVFS_CACHE_DNCDIR	1
#define	MVFS_CACHE_DNCREG	2
#define	MVFS_CACHE_DNCNOENT	3
#define	MVFS_CACHE_MFREE	4
#define	MVFS_CACHE_CTFREE	5
#define	MVFS_CACHE_RPCHANDLES	6

/*
 * MVFS_CMD_GET_CACHE_USAGE fetches the various cache sizes and usage counts
 * in the MVFS.
 */
#define MVFS_CMD_GET_CACHE_USAGE 46
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_CACHE_USAGE, 0,
 *		&MFS_NULL_STRBUFPN_PAIR, mvfs_cacheusep,
 *		sizeof(*mvfs_cacheusep));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CACHE_SIZES_COUNT 22
typedef struct mvfs_cache_sizes {
    ks_uint32_t	version;		/* which version (how many words) */
    ks_uint32_t mask;                   /* which are present (for set) */
    /*
     * if bit A is set in mask, then size[A] should be applied
     */
    ks_uint32_t	size[MVFS_CACHE_SIZES_COUNT];	/* extra room for later slop */
} mvfs_cache_sizes_t;
#define	MVFS_SETCACHE_MNMAX		0
#define	MVFS_SETCACHE_DNCDIRMAX		1
#define	MVFS_SETCACHE_DNCREGMAX		2
#define	MVFS_SETCACHE_DNCNOENTMAX	3
#define	MVFS_SETCACHE_VOBFREEMAX	4 /* high water mark */
#define	MVFS_SETCACHE_CVPFREEMAX	5 /* high water mark */
#define	MVFS_SETCACHE_RPCHANDLES	6
#define MVFS_SETCACHE_RDDIR_BLOCKS      7
#define MVFS_SETCACHE_VOBFREEMIN        8 /* low water mark */
#define MVFS_SETCACHE_CVPFREEMIN        9 /* low water mark */
#define MVFS_SETCACHE_AGE_CVP_TIME      10
#define MVFS_SETCACHE_VOBHASHTAB_SZ     11 /* mount-time only */
#define MVFS_SETCACHE_CVPHASHTAB_SZ     12 /* mount-time only */
#define MVFS_SETCACHE_DNCHASHTAB_SZ     13 /* mount-time only */
#define MVFS_SETCACHE_THREADHASHTAB_SZ  14 /* mount-time only */
#define MVFS_SETCACHE_PROCHASHTAB_SZ    15 /* mount-time only */
#define MVFS_SETCACHE_LARGEINIT         16 /* mount-time only */
#define MVFS_SETCACHE_OTHERHASHTAB_SZ	17 /* mount-time only */
#define MVFS_SETCACHE_VOBFREEHASHTAB_SZ 18 /* mount-time only */
#define MVFS_SETCACHE_CTXT_ATIME_REFRESH 19
#define MVFS_SETCACHE_COUNT             20
/* see also SET_CACHE_ENB command above for cache enable state */

/*
 * These settings may only be changed persistently (they're passed to
 * the MVFS module when viewroot is mounted.)
 */
#define MVFS_SETCACHE_PERSISTENT_ONLY_MASK      \
 (MVFS_CACHEBIT(VOBHASHTAB_SZ) |                  \
  MVFS_CACHEBIT(CVPHASHTAB_SZ) |                  \
  MVFS_CACHEBIT(OTHERHASHTAB_SZ) |                \
  MVFS_CACHEBIT(VOBFREEHASHTAB_SZ) |              \
  MVFS_CACHEBIT(DNCHASHTAB_SZ) |                \
  MVFS_CACHEBIT(THREADHASHTAB_SZ) |             \
  MVFS_CACHEBIT(PROCHASHTAB_SZ) |               \
  MVFS_CACHEBIT(RDDIR_BLOCKS) |                 \
  MVFS_CACHEBIT(LARGEINIT))

/*
 * These settings may be adjusted at run-time but have secondary effects
 * which are not adjusted at run-time.
 */
#define MVFS_SETCACHE_SECONDARY_EFFECTS_MASK    \
 (MVFS_CACHEBIT(MNMAX) |                        \
  MVFS_CACHEBIT(DNCDIRMAX) |                    \
  MVFS_CACHEBIT(DNCREGMAX) |                    \
  MVFS_CACHEBIT(DNCNOENTMAX))

#define MVFS_SETCACHE_VERSION		7
/*
 * This must be a 32-bit constant, so that 32-bit user space generates
 * a matching value for 64-bit kernel modules
 */
#define MVFS_SETCACHE_DEFAULT_VALUE     (ks_uint32_t)-1
#define MVFS_SETCACHE_UNSET_VALUE       (ks_uint32_t)-2
#define MVFS_CACHEBIT(x) (1 << MVFS_SETCACHE_##x)
#define MVFS_SETCACHE_ALLBITS ((1 << MVFS_SETCACHE_COUNT) - 1)

/*
 * MVFS_CMD_SET_CACHE_SIZES sets the various cache sizes in the MVFS.
 */
#define MVFS_CMD_SET_CACHE_SIZES 47
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_CACHE_SIZES, 0,
 *		&MFS_NULL_STRBUFPN_PAIR, mvfs_sizesp,
 *		sizeof(*mvfs_sizesp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_AUDIT_MARKER 48
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_AUDIT_MARKER,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, marker_flagsp, sizeof(*marker_flagsp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

#define MVFS_CMD_IOD_NULL 49
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_IOD_NULL, 0,
 *		&MFS_NULL_STRBUFPN_PAIR, NULL, 0);
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

struct mvfs_viewstats {
	mfs_strbuf_t	viewtag;	/* (IN) view tag */
	mvfs_statbufs_t	stats;		/* (OUT) statistics pointers */
};
typedef struct mvfs_viewstats mvfs_viewstats_t;

#define MVFS_CMD_GET_VIEW_STATS 50
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_VIEW_STATS,
 *              0,
 *              &MFS_NULL_STRBUFPN_PAIR, viewstatsp, sizeof(*viewstatsp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_ZERO_VIEW_STATS clears the per-view statistics kept by the MVFS.
 */

struct mvfs_zero_viewstat {
	mfs_strbuf_t	viewtag;	/* (IN) viewtag */
};

typedef struct mvfs_zero_viewstat mvfs_zero_viewstat_t;

#define MVFS_CMD_ZERO_VIEW_STATS 51
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_ZERO_VIEW_STATS, 0,
 *              &MFS_NULL_STRBUFPN_PAIR, viewtagp, sizeof(* viewtagp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_SIDHOST_CREDMAPPING registers a SID/HOST/CRED mapping
 *            for the NT mvfs.
 */

struct mvfs_sidhost_cred {
	mfs_strbuf_t	host;		/* (IN) hostname */
	char		*sid;			/* (IN) DUMMY for UNIX */
	size_t		sidlen;			/* (IN) Sid Length */
	viewtag_cred_t	creds;		/* (IN) creds */
};

typedef struct mvfs_sidhost_cred mvfs_sidhost_cred_t;

#define MVFS_CMD_SIDHOST_CREDMAPPING 52
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SIDHOST_CREDMAPPING, 0,
 *            &MFS_NULL_STRBUFPN_PAIR, sidhostcred_p, sizeof(* sidhostcred_p));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_DELETE_SIDHOST_CREDMAPPING deletes all SID/HOST/CRED mapping
 *            in NT mvfs for the specified SID.
 */

struct mvfs_sid {
	char		*sid;			/* (IN) DUMMY for UNIX */
	size_t		sidlen;			/* (IN) Sid Length */
};

typedef struct mvfs_sid mvfs_sid_t;

#define MVFS_CMD_DELETE_SIDHOST_CREDMAPPING 53
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_DELETE_SIDHOST_CREDMAPPING, 0,
 *            &MFS_NULL_STRBUFPN_PAIR, sid_p, sizeof(* sid_p));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */
/*
 * MVFS_CMD_GET_VIEWTAG_EXPORT fetches a view tag's export ID
 */
#define MVFS_CMD_GET_VIEWTAG_EXPORT 54
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_VIEWTAG_EXPORT,
 *		0,
 *		pairp, export_info, sizeof(*export_info));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_GET_GFSINFO is an ioctl added for AIX. It is needed to get the new
 * gfsno once the mvfs extension is loaded. gfsno is required to update the
 * /etc/vfs file, among other things.
 */
struct mvfs_gfsinfo {
    int gfsno;
    /* only one for the time being... */
};
typedef struct mvfs_gfsinfo mvfs_gfsinfo_t;

#define MVFS_CMD_GET_GFSINFO 55
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_GFSNO,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, vob_path, sizeof(*vob_path));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_SET_VOBRT_VFSMNT is an ioctl added for Linux support for kernels
 * beyon 2.4.8.  It is needed to set the pointer to the vfsmount structure
 * in the vob root mmi structure.  It cannot be done as part of the mount
 * because the vfsmount structure is not set up until after the mvfs 
 * specific mount code is called.
 */
#define MVFS_CMD_SET_VOBRT_VFSMNT 56
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_SET_VOBRT_VFSMNT,
 *		0,
 *		&MFS_NULL_STRBUFPN_PAIR, vob_path, sizeof(*vob_path));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */

/*
 * MVFS_CMD_GET_CACHE_SIZES gets the various cache sizes from the MVFS.
 */
#define MVFS_CMD_GET_CACHE_SIZES 57
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_GET_CACHE_SIZES, 0,
 *		&MFS_NULL_STRBUFPN_PAIR, mvfs_sizesp,
 *		sizeof(*mvfs_sizesp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */
/*
 * MVFS_CMD_COMPUTE_CACHE_DEFAULTS gets the various default cache sizes
 * from the MVFS.  The mask indicates which values should be taken as
 * presented; any value not indicated in the mask is computed based
 * on the values which are provided and the values computed as if there
 * were no other tuning.
 */
#define MVFS_CMD_COMPUTE_CACHE_DEFAULTS 60
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_COMPUTE_CACHE_DEFAULTS, 0,
 *		&MFS_NULL_STRBUFPN_PAIR, mvfs_sizesp,
 *		sizeof(*mvfs_sizesp));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */
struct mvfs_mkviewtag_info_ex {
        mfs_strbuf_t	viewtag;	/* (IN)	 view tag */
	mfs_strbufpn_pair_t spath;	/* (IN)  Path to view storage */
        mfs_strbuf_t	host;		/* (IN)	 Host name of view server */
	mfs_strbufpn_t	rpath;		/* (IN)  Remote path of view storage */
	mfs_strbufpn_t  net_path;	/* (IN)  Net path to view storage */
	tbs_uuid_t	uuid;		/* (IN)  View UUID */
	ks_sockaddr_storage_t addr;	/* (IN)  Addr */
        u_int windows_view : 1;         /* (IN) Windows view */
        u_int pad : 31;                 /* (IN) filler */
}; 
typedef struct mvfs_mkviewtag_info_ex mvfs_mkviewtag_info_ex_t;

#define MVFS_CMD_MKVIEWTAG_EX 62
/*
 * {
 *     int rc;
 *
 *     MVFS_CMD(mh, rc, status, MVFS_CMD_MKVIEWTAG_EX,
 *		0,
 *		pairp, viewtaginfop, sizeof(*viewtaginfop));
 *     if (rc != 0) {
 *         <error handling>
 *     }
 * }
 */
#define MVFS_FILEUTL_ABSOBJPN(AP, AOP, SZAOP, RC) *(AOP) = NULLC, (RC) = 0

/*
 * Used for validation in mfs_vnodeops.c
 */
#define MVFS_CMD_MIN 1
#define MVFS_CMD_MAX 62

#endif /* MFSMIOCTL_H_ */
/* $Id: 1393a11f.a23a11df.8bc7.00:01:84:7a:f2:e4 $ */
