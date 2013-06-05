/* * (C) Copyright IBM Corporation 2006, 2013. */
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
#ifndef MVFS_BASE_H_
#define MVFS_BASE_H_


#include "view_rpc_kernel.h"
#include "mfs_stats.h"
#include "mfs_ioctl.h"
#include "mfs_mount.h"
#include "mfs_audit.h"

#define MFS_MAXRPCDATA	8192	/* Max data in clnt calls */
#define MFS_BLOCKSIZE	8192	/* FS block size */


typedef char mfs_pn_char_t;		/* Pathname character type */
typedef char mfs_hn_char_t;		/* Hostname character type */
#define PN_STRDUP(s)		STRDUP(s)
#define PN_STRFREE(s)		STRFREE(s)
/*
 * Case-insensitive string comparison.
 */
#define STRCASECMP mvfs_ansi_strcasecmp
/* 
 * First arg is boolean whether case-insensitive pathname compare
 * or not
 */
#define PN_STRCMP(ci,s1,s2)	((ci) ? STRCASECMP(s1,s2) : STRCMP(s1,s2))
/*
 * The following macro is tricky, because the PN_STRFREE
 * will NULL out the pointers we are trying to compare.
 * Therefore, all comparisons must be done BEFORE any calls
 * to PN_STRFREE
 */
#define PNPAIR_STRFREE(p)	{ \
	if ((p)->kpn.s != (p)->upn.s) { 	/* Free both */ \
	    if ((p)->kpn.s != NULL) PN_STRFREE((p)->kpn.s); \
	    if ((p)->upn.s != NULL) PN_STRFREE((p)->upn.s); \
	} else { \
	    if ((p)->kpn.s != NULL) PN_STRFREE((p)->kpn.s); \
	    (p)->upn.s = NULL; \
	} \
    }

#define PN_SET_CASE_CORRECT_COMP(pnp, nm)  (0)

#define PN_GET_CASE_CORRECT_COMP(pnp, nm, bufp)   (nm)

#define HN_STRDUP(s)		STRDUP(s)
#define HN_STRFREE(s)		STRFREE(s)
#define HN_STRCMP(s1,s2)	STRCMP(s1,s2)

#define MVFS_INIT_TIMEVAL(tv) { \
         tv.tv_sec = 0; \
         tv.tv_usec = 0; \
}

/*
 * PROTOTYPES and subroutine definitions
 */
#include <stdarg.h>

#include "mvfs_debug.h"
#include "mvfs_param.h"

/*
 * Common list macros for circularly linked object lists.
 *	LISTHDR defines the list head.  Initialize the lock, and
 *	next=prev=&listhdr.
 *      ADD_TO_EOL - add an object to the end of the list
 *	ADD_TO_BOL - add an object to the beginning of the list
 *	RM_LIST - remove an object from its list.
 *
 *	Second version is for a "freelist" for objects that have
 *	one (mnodes and name cache entries)
 */
#define LISTHDR(name, t) \
    struct { \
	t *next; \
	t *prev; \
    } name

#define INIT_LIST(name, t) \
    (name).next = (name).prev = (t *) &(name)

#define ADD_TO_EOL(lp, item_p) { \
    (item_p)->prev = (lp)->prev; \
    (item_p)->next = (lp)->prev->next; \
    (lp)->prev->next = item_p; \
    (lp)->prev = item_p; \
    }
#define ADD_TO_BOL(lp, item_p) { \
    (item_p)->next = (lp)->next; \
    (item_p)->prev = (lp)->next->prev; \
    (lp)->next->prev = item_p; \
    (lp)->next = item_p; \
    }
#define RM_LIST(item_p) if ((item_p)->next != NULL) { \
    (item_p)->next->prev = (item_p)->prev; \
    (item_p)->prev->next = (item_p)->next; \
    (item_p)->next = NULL; \
    (item_p)->prev = NULL; \
    }

/*
 * Flag manipulation macros
 */
#define MVFS_FLAGON(flags, flagvalue) \
    (((flags) & (flagvalue)) != 0)
#define MVFS_FLAGOFF(flags, flagvalue) \
    (((flags) & (flagvalue)) == 0)
#define MVFS_SETFLAG(flags, flagvalue) { \
    (flags) |= (flagvalue); \
    }
#define MVFS_CLEARFLAG(flags, flagvalue) { \
    (flags) &= ~(flagvalue); \
    }

/*
 * MFS structures for mfscall().
 *    mfs_callinfo:	from procedure code.  Indicates the 'RPC trait' which
 *			is being called for mfscall() error messages.
 *			Currently one of mfs_viewcall, mfs_vobcall.
 *    mfs_svr:  	from an object (like view obj or vob mount point).
 *			Identifies whom to make the call to.
 *    mfs_retryinfo: 	from the mount point.  Indicates retry handling
 *			parameters for the call.
 */

struct mfs_callinfo {
	u_long	proto;			/* Server protocol */
	u_long	version;		/* Server protocol version */
	char    *svrname;		/* Trait name */
	char	**opnames;		/* Trait operation names */
	int	*optimeoshft;		/* Trait timeout shift per-op */
	int     (*get_status)(P1(void *resp));	/* Get status from response */
	XID_T	(*get_xid)(P1(void *resp));	/* Get XID from response */
	void    (*set_xid)(P1(void *req)
			   PN(time_t bt)
			   PN(XID_T xid));	/* Set XID in rqst */
};

/* Following are used in mnt args from mount command too */

struct mfs_svr {
	u_int	down : 1;		/* Server down */
	u_int	dprinted : 1;		/* Server down msg printed */
	u_int	uprinted : 1;		/* Server down msg printed to user */
	u_int	svrbound : 1;		/* Server addr valid (else find) */
	u_int	mbz : 28;
	ks_sockaddr_storage_t addr;	/* Server address */
	mfs_strbufpn_pair_t	lpn;	/* Local pathname (to server dir) */
	mfs_hn_char_t   *host;		/* Server host name */
	mfs_pn_char_t	*rpn;		/* Remote pathname (for svr to use) */
	mfs_pn_char_t	*net_pn;	/* Remote pathname (to server dir) */
	tbs_uuid_t uuid;		/* UID for albd (location daemon) */
};

struct mfs_retryinfo {
	u_int	soft : 1;		/* Server soft mount */
	u_int	nointr : 1;		/* Don't allow intr on RPC calls */
	u_int	rebind : 1;		/* Return EAGAIN and timeo for rebind */
	u_int	mbz : 29;	
	u_long	timeo;			/* Server timeout base in .1 secs */
	u_long	retries;		/* Server retry count */
};

/* RPC traits used from MFS code */

extern struct mfs_callinfo *mfs_viewcall;
extern struct mfs_callinfo *mfs_albdcall;

/* A cache of RPC handles, number of handles to keep is tunable */
typedef struct client_cache {
    int proto;
    int version;
    int inuse;
    int used;
    /*
     * XXX worry about 64-bit alignment/padding? usually the cache
     * isn't huge, probably not a lot of wasted space.
     */
    ks_uint32_t boottime;
    CLIENT *client;
} client_cache_t;

#define CLIENT_CACHE_SIZE_SMALL 5
#define CLIENT_CACHE_SIZE_LARGE 10
#define CLIENT_CACHE_SIZE_AUTOMAX 240

typedef struct mvfs_rpc_data {
    LOCK_T mfs_client_lock;
    sa_family_t mvfs_client_cache_family;
    client_cache_t *mvfs_client_cache;
} mvfs_rpc_data_t; 

/*
 * Define the structure of an MFS FID & Export FID
 *    For non-Atria (XFID) support, the fid is limited to 12 bytes  
 *    (including the len field) so that NFS can construct a file handle from it.
 */

#define MFS_UNK_DBID	0	/* Unkown value for partial vfh */
#define MFS_UNK_GEN	0	/* Unkown value for partial vfh */
#define MFS_NULL_DBID	0	/* Value for "null" fid */
#define MFS_NULL_GEN	0	/* Value for "null" gen */

struct mfs_fid {
	union {
	    u_long	dbid;		/* Database ID  (VOB) */
	    CLR_VNODE_T     *realvp;	/* Cleartext ptr (LOOP) */
	    u_long	mnum;		/* Mnode number (others) */
        } fid_un;
	union {
            ks_uint32_t  gen;           /* Generation number (others) */
            VFS_T *realvfsp;            /* realvp VFS pointer (LOOP) */
        } fid_un2;
};
#define mf_dbid		fid_un.dbid
#define mf_realvp 	fid_un.realvp
#define mf_mnum		fid_un.mnum
#define mf_gen		fid_un2.gen
#define mf_realvfsp     fid_un2.realvfsp

typedef struct mfs_fid mfs_fid_t;

#define MFS_FIDEQ(fid1, fid2) \
	((fid1).mf_dbid == (fid2).mf_dbid && (fid1).mf_gen == (fid2).mf_gen)
#define MFS_FIDNULL(fid) \
	((fid).mf_dbid == MFS_NULL_DBID && (fid).mf_gen == MFS_NULL_GEN)
#define MFS_FID_TO_PARTIAL_VFH(fid, vfsp, vw, vfh) { \
	(vfh).vob_uuid = VFS_TO_MMI(vfsp)->mmi_svr.uuid; \
	(vfh).ver_dbid = (fid).mf_dbid; \
	(vfh).elem_dbid = MFS_UNK_DBID; \
        (vfh).gen = (fid).mf_gen; \
        (vfh).flags=VTOM(vw)->mn_view.hm ? VIEW_FHANDLE_FLAGS_HISTORY_MODE:0; \
	(vfh).pad0 = 0; \
    }
#define MFS_UPDATE_PARTIAL_VFH(pvfh, fvfh) { \
	if ((pvfh).elem_dbid == MFS_UNK_DBID) { \
	    (pvfh).elem_dbid = (fvfh).elem_dbid; \
	    if ((fvfh).elem_dbid == MFS_UNK_DBID) \
		mvfs_log(MFS_LOG_INFO, \
		    "update vfh: no elem dbid for dbid 0x%x\n", \
			(pvfh).ver_dbid); \
	} \
	if ((pvfh).gen == MFS_UNK_GEN) { \
	    (pvfh).gen = (fvfh).gen; \
	} \
        if (((pvfh).flags & VIEW_FHANDLE_FLAGS_HISTORY_MODE) == \
	    	((fvfh).flags & VIEW_FHANDLE_FLAGS_HISTORY_MODE)) \
	    (pvfh).flags = (fvfh).flags; \
    }
	    
/*
 * NFS (non-atria) fid format.  Only VOB objects are "exportable".
 *
 */
struct mfs_xfid {		/* FID for NFS support (vfs_vget/vop_fid ops) */
    u_short	mfx_len; 	/* Must match struct fid in vfs.h */
    u_short	mfx_vid;	/* View id of view object is in */
    ks_uint32_t	mfx_dbid;	/* Database ID (VOB objs only) */
    ks_uint32_t	mfx_gen;	/* Generation number (for stale detection) */
};

typedef struct mfs_xfid mfs_xfid_t;

/* mfs_xfid_t data size (size of fields after mfx_len) */
#define MFS_XFIDDATASZ		(sizeof(mfs_xfid_t) - sizeof(u_short))

#define MFS_VIDINRANGE(vid) \
	((vid) >= 0 && (vid) <= 0xfffe)
#define MFS_RESERVED_DBIDS 0xfffffc00	/* Reserved for MFS above here */
#define MVFS_ROOTDBID	0xffffffff	/* unbound Vob Root dbid */
#define MVFS_ROOTGEN	0		/* unbound Vob Root generation */

#define MFS_NULLVID	0		/* Null view id */

typedef fsid_t	mfs_fsid_t;		/* FSID from mount table */

typedef int mfs_class_t;

#define MVFS_LH(x) /**/

/*
 * MFS vnode info structure.  All the info structs must
 * have the same "hdr" in them so they can be manipulated
 * in basic ways without testing the type of MVFS object.
 *
 * This is also used to determine (with other optional structures)
 * the size of FS-dependent data to allocate.
 *
 * Locking:
 *    Different fields of the mnode are protected by different locks.
 *    The "hdr" fields (with exceptions listed below) are protected
 *    the mnode header lock found in each header.  These are not marked.
 *
 *	The next and prev pointers are protected by the hash chain lock
 *	for the hash chain on which they reside.  Marked with "HS".
 *
 *	The free_next and free_prev link the mnode to either the free list
 *	lock or the destroy list lock.  Think of both these lists as lists 
 *	where free mnodes are linked; the vobfreelist is a special freelist
 *	used only by vob mnodes that still have a view attached.
 *	Marked with "FD".
 *
 *	The mcount and mnum are protected by the header lock (not marked).
 *
 *	The mfree and trans_destroy flags are protected by the mfs_vobfreelock.
 *	mfree marks the mnode as being on the freelist without searching
 *	down the free chain.  Similarly, trans_destroy indicates the mnode
 *	is (almost) on the destroy list and should be ignored in searches.
 *	Marked with "F".
 *
 *	The on_destroy flag is protected by the mvfs_mndestroylock.  Once set,
 *	it is never cleared.  Marked with a "D".
 *
 *	The stale flag is protected by the mnode lock as is the rest of the flag
 *	word.  Marked with "M". 
 *
 *	Several offsets: mclass, msize, dncgen are set in mvfs_mnnew when
 *	the mnode is allocated.  Since no one can have the mnode yet, no lock
 *	is used. If future enhancements required a lock for these offsets, it
 *	should be the header lock.  Not marked.
 *
 *	The lock offset (mnode lock) needs to be taken *without* the
 *	mnode header lock for lock ordering reasons.  Marked with "*".
 *
 *	The vfsp is set in mvfs_mninit_new_mnode under no lock when the mnode
 *	is allocated.  It is not cleared until the mnode is destroyed, so it
 *	never changes for the life of the mnode.  Not marked.
 *
 *	The fid is set in mvfs_mninit_new_mnode under no lock when the mnode
 *	is allocated.  And largely it is read under the mnode lock.
 *	However, it is set to mnp->mn_hdr.mnum in the case that a fid wasn't 
 *	sent in (in mvfs_mninit_new_mnode) prior to the mnum being set.  
 *	XXX This seems like a bug, but has always been this way. 
 *	The mf_gen portion of the fid is updated in mfs_getmnode (mvfs_vfsops.c)
 *	under the mnode lock.  It is returned by mfs_vp2dbid in mvfs_utils.c
 *	look at this further. XXX  Marked with an "M".
 *
 *	The vp is protected by the mnode lock.  (It is set to null in 
 *	mvfs_mnnew under no lock.)  Marked with an "M".
 *
 *	The realvp continues to be protected by the mnode lock.  Marked "M".
 *
 *	The viewvp is set in mvfs_mninit_new_mnode and shouldn't change for
 *	the life of the mnode.  (There is a bit of code in mfs_makevobnode
 *	that thinks it is updating it, but I don't think we ever hit this.)
 *	It is cleared in mvfs_mnclean.  Not marked.
 *
 *	The m_bhv is protected by the mnode lock.  Marked with an "M".
 *
 *	freelist_time is set under the header lock.  Not marked.
 *
 *    Beware that some systems cannot lock bitfields
 *    on less than quadword (8 byte) boundaries due to vagaries of the
 *    compiler load/modify/store code sequences.  All processors
 *    without direct memory operations cannot lock anything less than
 *    a byte (or more likely, a 4-byte word).
 *
 */

struct mfs_mnhdr {			/* Basic hdr info for all mfs objects */
/*HS*/	struct mfs_mnode *next;		/* Next ptr */
/*HS*/	struct mfs_mnode *prev;		/* Prev ptr */
/*FD*/	struct mfs_mnode *free_next;	/* Free list next */
/*FD*/ 	struct mfs_mnode *free_prev;	/* Free list previous */
	LOCK_T		  hdr_lock;	/* Mnode header lock (see above) */
	u_int		  mcount;	/* Reference count */
	mfs_class_t	  mclass;	/* Mnode Class of MFS object */
	int		  mnum;		/* Mnode table number */
/*F*/	u_int		  mfree;	/* Mnode is on vobfree list flag */
/*F*/	u_int		  trans_destroy;/* Mnode moving to destroy list flag */
/*D*/	u_int		  on_destroy;	/* Mnode is on destroy list flag */
/*M*/	u_int		  stale;	/* Flag to mark stale mnodes in hash */
/*M*/	u_int		  cached_pages : 1; /* vnode has cached pages */
/*M*/	u_int		  clear_dirty : 1; /* cleartext pages are dirty */
/*M*/	u_int		  clear_mmap : 1; /* mmap switched to cleartext */
/*M*/	u_int		  pad : 29;	/* unused flag bits */
	size_t		  msize;	/* Size (in bytes) of this mnode */
/***/	LOCK_T		  lock;		/* Lock on structure */
MVFS_LH(LOCK_T		  lock_high;)	/* High Level lock on structure (for I/O) */
	VFS_T		  *vfsp;	/* VFSP of object */
/*M*/	mfs_fid_t	  fid;		/* File ID */
	u_long		  dncgen;	/* Mnode generation # for name cache */
/*M*/	VNODE_T	 	  *vp;		/* Back ptr to mfs vnode */
/*M*/	CLR_VNODE_T 	  *realvp;	/* Real object or cleartext vnode ptr */
#ifdef NFSV4_SHADOW_VNODE
/*M*/   CLR_VNODE_T       *realvp_master; /* Master "shadow" cleartext vnode ptr */
#endif
	VNODE_T		  *viewvp;	/* View for object (may be NULL) */
	time_t		  freelist_time; /* Time added to freelist */
};

/* Define the classes of MFS objects & macros to test for them.
   Use "class" to avoid confusion with object "type" */

#define MFS_SDEVCLAS	1	/* Special device vnode for ioctl's */
#define MFS_VIEWCLAS	2	/* Inode which represents a "view" itself */
#define MFS_LOOPCLAS	3	/* Cover vnode for auditing non-mfs files */
#define MFS_VIEWDIRCLAS 4	/* Dir containing "views" (view root for now) */
#define MFS_VOBRTCLAS	5	/* VOB root placehold inode */
#define MFS_VOBCLAS	6	/* Object in vob/view */
#define MFS_NTVWCLAS	7	/* New non-loopback NT-style view-tags */
#define MFS_MAXCLAS	7	/* Max class number */

#define MFS_ISSDEV(mnp) ((mnp)->mn_hdr.mclass == MFS_SDEVCLAS)
#define MFS_ISVOBRT(mnp) ((mnp)->mn_hdr.mclass == MFS_VOBRTCLAS)
#define MFS_ISLOOP(mnp) ((mnp)->mn_hdr.mclass == MFS_LOOPCLAS)
#define MFS_ISLOOPVIEW(mnp) ((mnp)->mn_hdr.mclass == MFS_VIEWCLAS)
#define MFS_ISVIEWDIR(mnp) ((mnp)->mn_hdr.mclass == MFS_VIEWDIRCLAS)
#define MFS_ISVOB(mnp)  ((mnp)->mn_hdr.mclass == MFS_VOBCLAS)

/* This macro checks for NT-style view for VOB tag mounts */
#define MFS_ISNTVIEW(mnp)  ((mnp)->mn_hdr.mclass == MFS_NTVWCLAS) 

/* Generic for either kind of view-class */
#define MFS_ISVIEW(mnp) (MFS_ISLOOPVIEW(mnp) || MFS_ISNTVIEW(mnp))

extern V_OP_T *mfs_vopp;	/* vnode ops ptr */
extern VFSOPS_T *mfs_vfsopp; 	/* vfs ops ptr */
/* see mdep or mvfs_systm.h for MFS_VPISMFS() */

/* 
 * Define macro to check for "root" synonyms.  A root synonym is
 * found as follows:
 * 	Not history mode:	Any version of the "mount point" element
 *				found by comparing element dbid's
 *	History mode:		Only the version with both the version and
 *				element dbid's matching the mount point.
 *				(The versions of the directory 2 levels down
 *				 e.g. <root>/main/2 have a version dbid 
 *				 different from the element dbid, but have
 *				 and element dbid matching the root!)
 */

#define MFS_ISROOTSYNONYM(vp, rootdbid) \
	((!VTOM(MFS_VIEW(vp))->mn_view.hm && \
          VTOM(vp)->mn_vob.vfh.elem_dbid == (rootdbid)) || \
         (VTOM(MFS_VIEW(vp))->mn_view.hm && \
          VTOM(vp)->mn_vob.vfh.elem_dbid == (rootdbid) && \
          VTOM(vp)->mn_vob.vfh.ver_dbid == (rootdbid))) 


struct mfs_rebindent {		/* Dir version cache entry */
	u_int	       valid : 1; 	/* Entry valid */
	u_int	       self : 1;	/* Entry rebinds to self */
	u_int	       mbz : 31;	/* unused */
	view_bhandle_t bh;	/* BuildHandle for entry */
	mfs_fid_t      fid;	/* Rebind vnode fid */
	struct timeval evtime;	/* Event time of dir rebound to */
};

typedef struct mvfs_clr_creds {
    struct mvfs_clr_creds *next;
    CRED_T *cred;
} mvfs_clr_creds_t;

struct mfs_clearinfo {			/* Cleartext information */
	mfs_pn_char_t	 *nm;		/* Cleartext pname */
	LOCK_T		 cl_info_lock;	/* Lock for cred cache */
	mvfs_clr_creds_t *ok_creds;     /* creds that have looked up the name */
	u_long		  revalidate_time;  /* Time (secs) to do revalidate */
	u_int		  isvob : 1;	/* Cleartext in vob */
	u_int		  rwerr : 1;	/* Cleartext RW error */
	u_int		  purge_nm : 1;	/* Cleartext pathname wrong */
	u_int		  purge_cvp : 1;  /* Cleartext vp wrong */
	u_int		  hadonce : 1;  /* had cltxt in past (for stats) */
	u_int		  used : 1;	/* used cltxt since last reclaim */
	u_int		  delete_on_close : 1; /* cltxt is marked for deletion */
	u_int		  ostale_logged : 1; /* have logged open stale warning */
	u_int		  pad : 24;	/* Pad space */
	VATTR_T	  	  va;		/* Stat of cleartext */
        time_t            atime_pushed; /* vob container setattr time, for scrubber */
};

#define MVFS_CTXT_ATIME_REFRESH_DEF 3600 /* default interval 1 hr between forced setattr */

#define MFS_REBINDINVAL(mnp) { \
	(mnp)->mn_vob.rebind.valid = 0; \
	(mnp)->mn_vob.rebind.self = 0; \
    }

/*
 * On unix, we need to check access to the cleartext path as well as to the 
 * MVFS path.  This gets tedious, but we try to keep track of creds we've already
 * looked at to save some redundant work.  See the comments near mfs_getcleartext()
 * for a full explanation. 
 */
#define MVFS_CRED_HASHSZ 61         /* FIXME: nice prime number? */

typedef struct mvfs_credlist_data {
    /* System-wide credlist chains and spinlock.
     * Mnode cred lists are protected by the mnode locks.
     */
    mvfs_clr_creds_t *mvfs_sys_credlist[MVFS_CRED_HASHSZ];
    mvfs_clr_creds_t *mvfs_free_creds;
    SPLOCK_T mvfs_sys_credlist_lock;

#ifdef MVFS_DEBUG
    /* buckets for chain length accounting.  Useful for determining if the hash
     * algorithm is distributing well.
     */
    ks_uint32_t mvfs_sys_crlen[MVFS_CRED_HASHSZ];
#endif

} mvfs_credlist_data_t;  

#define MFS_INDEX_CACHE_LOOKUP_BEST(vp,o,bip,bop) { *(bip) = *(bop) = o; }
#define mfs_index_cache_add(vp,i,o)	/* do nothing */
#define mfs_index_cache_flush(vp)	/* do nothing */
#define mfs_index_cache_destroy(mnp)	/* do nothing */

struct mvfs_rce { /* readdir cache entry */
    tbs_boolean_t valid;                /* entry valid? */
    tbs_boolean_t eof;                  /* is this the last block in dir? */
    MOFFSET_T offset;                   /* uio_offset for this block */
    MOFFSET_T endoffset;                /* uio_offset after reading this block */
    size_t size;                        /* size of entries to copy */
    size_t bsize;                       /* size of allocated block */
    void *block;                        /* block of entries */
};

struct mvfs_rddir_cache {
    int nentries;
    struct mvfs_rce entries[1];         /* really nentries long */
};

#define RDDIR_CACHE_SIZE(mrc) \
 (sizeof(*(mrc)) + ((mrc)->nentries - 1) * sizeof((mrc)->entries[0]))

#define RDDIR_CACHE_SIZE_N(N) \
 (sizeof(struct mvfs_rddir_cache) + ((N) - 1) * sizeof(((struct mvfs_rddir_cache*)0)->entries[0]))

EXTERN void
mvfs_rddir_cache_destroy(struct mfs_mnode *mnp);

/* call with mnode locked */
EXTERN void
mvfs_rddir_cache_flush(struct mfs_mnode *mnp);

EXTERN tbs_boolean_t
mvfs_rddir_cache_get(
    struct mfs_mnode *mnp,
    struct uio *uiop,
    CRED_T *cred,
    int *eofp,
    int *errorp
);

EXTERN void
mvfs_rddir_cache_enter(
    struct mfs_mnode *mnp,
    struct mvfs_rce *entryp
);

/* Expects, and asserts, that the given mnode is locked.
 */
EXTERN void
mvfs_rddir_cache_enter_mnlocked(
    struct mfs_mnode *mnp,
    struct mvfs_rce *entryp
);

EXTERN int
mvfs_rddir_cache_setcaches(mvfs_cache_sizes_t *szp);

EXTERN int
mvfs_rddir_cache_getcaches(mvfs_cache_sizes_t *szp);

EXTERN int
mvfs_rddir_compute_caches(
    ks_int32_t scale,
    mvfs_cache_sizes_t *szp
);

EXTERN void
mvfs_rddir_cache_init(mvfs_cache_sizes_t *szp);

EXTERN void
mvfs_rddir_cache_unload(void);

#define MVFS_CL(x) /**/

struct mfs_vobnode {
	u_int		  rcred : 1;	/* cred has read credentials */
	u_int		  wcred : 1;	/* cred has write credentials */
	u_int		  dir_eof : 1;  /* rddir_off (eof cookie) valid */
	u_int		  choid_audited: 1;	/* choid'd under audit */ 
	u_int		  sync_mtime : 1;  /* Mtime needs update */
	u_int		  sync_ctime : 1;  /* Ctime needs update (indirectly) */
	u_int		  pad : 26;
	view_fhandle_t	  vfh;		/* View object file handle */
	struct mfs_clearinfo cleartext;	/* Cleartext info */
	int		  open_count;	/* Count of open's done */
	int		  open_wcount;	/* Count of open's done with FWRITE */
	view_bhandle_t	  choid_bh;	/* BH choid'd under */
	u_long		  choid_bh_seq;	/* Audit sequence # for choid logic */
MVFS_CL(LOCK_T           cred_lock;)    /* Lock while changing cred */
	CRED_T		 *cred;		/* Cred for delayed IO/setattr */
	mfs_pn_char_t	 *rmv_name;	/* Name to remove on mfs_inactive */
	VNODE_T 	 *rmv_dvp;	/* Dir vnode for remove */
	CRED_T		 *rmv_cred; 	/* Credentials for remove */
	struct mfs_rebindent rebind;	/* Rebind info */
	u_long		  rddir_off;  	/* rddir EOF offset */
        struct mvfs_rddir_cache *rddir_cache; /* readdir results, if any */
	mfs_pn_char_t	 *slinktext;	/* Symlink text */
	int		  slinklen;	/* Symlink text length */
	u_long		  attrgen;	/* Attribute generation number */
	timestruc_t	  attrsettime;	/* Last time attrs set */
	timestruc_t	  attrtime;	/* Time attributes valid until */
	struct timeval    lvut;		/* last VOB update time from getattr */
	view_vstat_t  	  attr;		/* Cached attributes */
        /* 
         * The user and group identities in the view_vstat_t struct are not
         * in native form. Therefore the following two fields were added
         * to store the converted native ids. They are to be kept in sync
         * with view_vstat_t.
         */
        MVFS_USER_ID      user_id; /* uid or union of uid/user sid on NT */
        MVFS_GROUP_ID     group_id; /* gid or union of gid/group sid on NT */
	int		  pages_mapped; /* number of pages mapped */
};

/*
 * Name cache locking macros (may be overridden on a per-platform basis).
 */

#define DNCLOCK_T               SPLOCK_T
#define INITDNCLOCK(lnm, lstr)  
#define DNCLOCK(lnm,s)          
#define DNCUNLOCK(lnm,s)        
#define FREEDNCLOCK(lnm)       

/* Spinlock pools
 * To reduce contention for spinlocks on MP systems, in some places
 * we use a pool of spinlocks in place of one single lock.  
 * Then, given some value indicating one instance of an object, a mapping 
 * function (or macro) is used to select one of the spinlocks to protect
 * that instance.
 * For example, rather than using a single spinlock for an entire hash
 * table, we can use a pool of locks such that every n chains (n may be 1)
 * has its own spinlock.  This is designed to allow greater parallelism on
 * MP machines. 
 */ 
typedef struct splock_pool {
    SPLOCK_T    *(*spl_func)(struct splock_pool *, unsigned int);  /* ptr to selector function */
    int         spl_count;  /* num spinlocks in pool */
    SPLOCK_T    *spl_table; /* dynamic splock table */
} splock_pool_t;

/* Select a spinlock from a spinlock pool
 * IN: pool   -  ptr to the struct anchoring the pool
 * IN: val    -  value to be used in the selection function
 * IN: func   - macro or function to choose lock, given value
 * IN/OUT: lockpp - ptr to lock ptr (SPLOCK_T**), return lock addr to be
 *               used subsequently to unlock the spinlock
 *
 * Use SPLOCK, SPUNLOCK to obtain/release the returned lock
 */
#define SPLOCK_SELECT(pool, val, func, lockpp) \
  *(lockpp) = func(pool, val)

/* Templates for a mapping macro for a spinlock pool for hash tables, 
 * to be used as input to SPLOCK_SELECT().
 * The default is to have 2 SPLOCK_T per table; platforms
 * wishing to override this need to define:
 *   HASH_SPLOCK_MAP to be HASH_SPLOCK_PER_CHAIN or HASH_SPLOCK_PER_GROUP
 *   HASH_SPLOCK_RATIO - the desired ratio of hash chains per SPLOCK_T
 */
#define HASH_SPLOCK_PER_CHAIN(pool, hash_val)  (&(pool)->spl_table[hash_val])
#define HASH_SPLOCK_PER_GROUP(pool, hash_val)  (&(pool)->spl_table[(hash_val) % (pool)->spl_count])

#define HASH_SPLOCK_SET_POOLSIZE(sp_poolsize, hash_size) {sp_poolsize = hash_size;}

/* mvfs_lock pools
 * To reduce lock contention on MP systems, in some places
 * we use a pool of LOCK_T's in place of one single lock.  
 * Then, given some value indicating one instance of an object, a mapping 
 * function (or macro) is used to select one of the mvfs_lock to protect
 * that instance.
 * For example, rather than using a single mvfs_lock for an entire hash
 * table, we can use a pool of locks such that every n chains (n may be 1)
 * has its own mvfs_lock.  This is designed to allow greater parallelism on
 * MP machines. 
 * The difference between spinlock pools and mvfs_lock pools is that
 * mvfs_lock pools have locks that can pend; use this type of lock pool
 * when you have the potential to pend while holding the lock.
 */ 
typedef struct mvfs_lock_pool {
    LOCK_T	*(*mlp_func)(struct mvfs_lock_pool *, unsigned int); /* selector func ptr */
    int		mlp_count;	/* num mvfs_locks in pool */
    LOCK_T	*mlp_table;	/* dynamic mvfs_lock table */
} mvfs_lock_pool_t;

/* Select a mvfs_lock from a mvfs_lock pool
 * IN: pool	  - ptr to the struct anchoring the pool
 * IN: val	  - value to be used in the selection function
 * IN: func	  - macro or function to choose lock, given value
 * IN/OUT: lockpp - ptr to lock ptr (LOCK_T**), return lock addr to be
 *		    used subsequently to unlock the spinlock
 *
 * Use MVFS_LOCK, MVFS_UNLOCK to obtain/release the returned lock
 */
#define MVFS_LOCK_SELECT(pool, val, func, lockpp) \
  *(lockpp) = func(pool, val)

/*
 * Templates for a mapping macro for a mvfs_lock pool for hash tables, 
 * to be used as input to MVFS_LOCK_SELECT().
 * The default is to have 2 MVFS_LOCK_T per table; platforms
 * wishing to override this need to define:
 *   HASH_MVFS_LOCK_MAP to be HASH_MVFS_LOCK_PER_CHAIN or HASH_MVFS_LOCK_PER_GROUP
 *   HASH_MVFS_LOCK_RATIO - the desired ratio of hash chains per LOCK_T
 */
#define HASH_MVFS_LOCK_PER_CHAIN(pool, hash_val)  (&(pool)->mlp_table[hash_val])
#define HASH_MVFS_LOCK_PER_GROUP(pool, hash_val)  (&(pool)->mlp_table[(hash_val) % (pool)->mlp_count])

#define HASH_MVFS_LOCK_MAP HASH_MVFS_LOCK_PER_GROUP
#ifndef HASH_MVFS_LOCK_RATIO
#define HASH_MVFS_LOCK_RATIO -2    /* default: 2 locks in total */
#endif

#if HASH_MVFS_LOCK_RATIO < 0
#define HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, hash_size) {mlp_poolsize = -(HASH_MVFS_LOCK_RATIO);}
#endif
#if HASH_MVFS_LOCK_RATIO == 0
#define HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, hash_size) {mlp_poolsize = 1;}
#endif
#if HASH_MVFS_LOCK_RATIO == 1
#define HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, hash_size) {mlp_poolsize = hash_size;}
#endif
#if HASH_MVFS_LOCK_RATIO > 1
#define HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, hash_size) {mlp_poolsize = ((hash_size)/HASH_MVFS_LOCK_RATIO) + 1;}
#endif

/*
 * Macro to tell if copy-on-write required.  If we have an
 * "audited file" and it hasn't been choided under this build
 * handle, then the file must be choided.
 */

#define MFS_FSTAT_AUDITED(mnp) \
	(((mnp)->mn_vob.attr.fstat.mode & TBS_FMODE_AUDITED_OBJ) != 0)

#define MFS_REMEMBER_CHOID_BH(mth, mnp) \
	if ((mth)->thr_auditon) { \
	    (mnp)->mn_vob.choid_audited = 1; \
	    (mnp)->mn_vob.choid_bh = (mth)->thr_bh; \
	    (mnp)->mn_vob.choid_bh_seq = (mth)->thr_aud_seq; \
	}

#define MFS_CLRTEXT_RO(mnp) \
	((mnp)->mn_vob.cleartext.isvob)

/* A vobstamp structure holds a last VOB update time (LVUT) plus its
 * update timestamp.  All vobstamps in all view nodes, as well as all
 * vobstamp_next fields, are accessed under the mvfs_vobstamp_lock.
 */

extern SPLOCK_T mvfs_vobstamp_lock;

struct mvfs_vobstamp {
	struct timeval	lvut;		/* the LVUT itself */
	tbs_uuid_t	vobuuid;	/* VOB uuid for this VOB */
	time_t		valid_thru;	/* how long is this valid? */
};
#define	MVFS_NUM_VOB_STAMPS	20	/* FIXME: select a size? */

/* Structure for MVFS statistics.  The statistics are maintained on a per-CPU
 * basis.  They are allocated and initialized in mvfs_misc_init when the
 * viewroot is mounted.  Macros for initializing and manipulating the values
 * are also defined in this header file.  Preemption disabling is used to ensure
 * data integrity.  MVFS makes RPC calls to various view_servers and to the
 * albd server.  We keep stats only on the view RPCs for performance evaluation.
 * The ALBD RPCs are not considered an area of concern for performance.
 */
typedef struct mvfs_statistics_data {
        tbs_boolean_t zero_me;               /* set if stats should  be zerod */
        struct mfs_mnstat mfs_mnstat;        /* mnode statistics */
        struct mfs_dncstat mfs_dncstat;      /* name cache stats */
        struct mfs_rvcstat mfs_rvcstat;      /* RVC stats separate from DNLC */
        struct mfs_clntstat mfs_clntstat;    /* RPC stats */
        struct mfs_acstat mfs_acstat;        /* Attr cache */
        struct mfs_rlstat mfs_rlstat;        /* Readlink cache */
        struct mfs_clearstat mfs_clearstat;  /* Cleartext operations */
        struct mfs_austat mfs_austat;        /* Audit operations */

        MVFS_STAT_CNT_T mfs_vnopcnt[MFS_VNOPCNT];  /* Vnode op calls counted */
        MVFS_STAT_CNT_T mfs_vfsopcnt[MFS_VFSOPCNT];/* VFS op calls counted */
        MVFS_STAT_CNT_T mfs_viewopcnt[VIEW_NUM_PROCS]; /* RPC ops to viewserver */
        timestruc_t mfs_viewoptime[VIEW_NUM_PROCS];/* time for the RPCs */ 
        struct mfs_rpchist mfs_viewophist;    /* Histogram of the RPC times */
} mvfs_stats_data_t; 

/*
 * Per-view statistics structure.  These are maintained on a per-view basis and
 * not on a per-CPU basis.  So, we need a lock to protect these.  The mnode,
 * vnode structs could be allocated pageable memory.  Taking this pvstatlock
 * which is a spin lock and then accessing the paged memory could lead to
 * problems.  So, wherever this lock is used, care should be taken not to
 * touch any paged memory after taking this lock.  In some of the macros below
 * where this lock is used, the per-view stat pointer was read into a local
 * variable before taking the lock and that is used to access the stats after
 * the lock is taken.
 */
struct mvfs_pvstat {
	SPLOCK_T mvfs_pvstatlock;
	struct mfs_clntstat	clntstat;	/* Client Statistics */
	struct mfs_acstat	acstat;		/* Attribute Cache stats */
	struct mfs_dncstat      dncstat;	/* DNC stats */
};

/* Histogram of RPC delays.  This is used to initiliaze the corresponding
 * structure in mvfs_statistics_data.
 */
extern struct mfs_rpchist mvfs_init_viewophist;

/* View objects - vnodes that describe a view itself */

struct mfs_viewnode {
	view_handle_t	vh;	/* View handle */
	u_int		hm : 1;	/* History mode flag */
	u_int		nocfg : 1;	/* No config spec error printed */
	u_int		needs_recovery : 1;	/* Needs recovery printed */
	u_int		needs_reformat : 1;	/* Needs reformat printed */
	u_int		always_cover : 1;	/* Always make loopback vnodes */
	u_int		zombie_view : 1; 	/* View stg error printed */
        u_int           windows_view : 1;   /* View on Windows NT or 2k */
        u_int           lfs_view : 1;       /* LFS view                */
        u_int           downrev_view : 1;       /* view at prev release */
	u_int	        pad : 23;
	mfs_pn_char_t   *viewname;	/* View tag name */
	struct mfs_svr	svr;	/* View server info */
	u_int		id;	/* View index in /view (for making inums, etc) */
	u_int		exid;	/* View export ID (for making xfid's) */
	MVFS_USER_ID    cuid;	/* Creator's uid or uid/SID for nt view */
	MVFS_GROUP_ID   cgid;	/* Creator's gid or gid/SID for nt view */
	timestruc_t	ctime;  /* Created time */
	time_t          usedtime;  /* Last used time (for cleanup) */
	LOCK_T		 stamplock;	/* Lock on vobstamps */
	struct mvfs_vobstamp
			vobstamps[MVFS_NUM_VOB_STAMPS]; /* VOB update timestamps */
	int		vobstamp_next;	/* round-robin replacement ptr */
	time_t          rpctime;        /* Last RPC time (for cleanup) */
        void            *mdep_datap;	/* Machine dep data */
	struct mvfs_pvstat
			*pvstat;	/* Per-view statistics */
};


struct mfs_ramdirent {
	mfs_pn_char_t 	*nm;	/* Ptr to name */
        VNODE_T 	*vp;	/* Ptr to vnode */
};

typedef struct mfs_ramdirent mfs_ramdirent_t;

struct mfs_ramdirnode {
	int		max;		/* Max cnt of elements allowed */
	int		hwm;		/* High water mark for table */
	mfs_ramdirent_t *ents;		/* Ptr to table of entries */
	timestruc_t	atime;		/* Accessed time */
        timestruc_t 	mtime;		/* Last modified time */
	timestruc_t	ctime;		/* Created time */
	int		export_hwm;	/* High water mark for export table */
	int		*export_ents;	/* Export table mappings */
	int		lnk_cnt;	/* Current # of directory entries */
};
#define MVFS_EXPORT_CHUNK	16	/* grow export table in chunks of 16 */

/* MFS objects - have different extra info based on 'kind' of MFS object.
   Loopback, vobroot vnodes only have the header (no extra info) */

struct mfs_mnode {
	struct mfs_mnhdr mn_hdr;	/* MUST BE FIRST! */
	union mn_u {
	    struct mfs_vobnode   vob;
	    struct mfs_viewnode  view;
	    struct mfs_ramdirnode ramdir;  /* viewdir */
	} mn_un;
};

typedef struct mfs_mnode mfs_mnode_t;

/* Shorthands past the union.  Cdview's use the
   view structure, Other object classes just use the hdr. */

#define mn_vob		mn_un.vob
#define mn_view		mn_un.view
#define mn_ramdir	mn_un.ramdir

/*
 * Hash lists for mnodes.  All in-use mnodes reside on one of 3 hashed mnode 
 * lists: mvfs_vobhash, mvfs_cvphash, mvfs_otherhash.  Each of these lists are
 * made up of n mfs_mnhash_slot_t's (where n is the hash size for that list).
 */

typedef struct mfs_mnhash_slot {
    struct {
    	mfs_mnode_t *next;
    	mfs_mnode_t *prev;
    } mn_hdr;
} mfs_mnhash_slot_t;

/* 
 * VOB freelist.  VOB mnodes are cached on the freelist when no longer in use.
 * The VOB freelist is made up of n mvfs_vobfreehash_slot_t's (where n is the 
 * mvfs_vobhashsize).
 */

typedef struct mvfs_vobfreehash_slot {
    struct {
    	mfs_mnode_t *next;
    	mfs_mnode_t *prev;
    	mfs_mnode_t *free_next;
    	mfs_mnode_t *free_prev;
    } mn_hdr;
} mvfs_vobfreehash_slot_t;

/* 
 * The mnode destroy list.  See mvfs_mnode.c for full details on this LRU.
 */

typedef struct mvfs_mndestroylist {
    struct {
    	mfs_mnode_t *next;
    	mfs_mnode_t *prev;
    	mfs_mnode_t *free_next;		/* destroy uses the freelist links */
    	mfs_mnode_t *free_prev;		/* destroy uses the freelist links */
    } mn_hdr;
} mvfs_mndestroylist_t;

/*
 * Audit file structure.
 * There is one of these for each active auditfile.
 * All processes in the same audit will reference the same auditfile structure,
 * sharing one buffer, so that the auditfile entries are ordered, and properly
 * appended in a sequential manner to the file.
 *
 * Lock Ordering: The global mfs_aflock is taken before the individual 
 * auditfile's lock.
 */

struct mfs_auditfile {
	/*
	 * Following are protected by global mfs_aflock
	 */
	struct mfs_auditfile *next;	/* List of auditfile structs */
	struct mfs_auditfile *prev;

        /*
         * The following 3 flags are locked by the auditfile's own lock
         *
         * The 'obsolete' field is a little funky.  It is used
         * to tell other processes (long running daemons spawned
         * under an audit) that they should close out their reference
         * to this audit file, and stop auditing.  Even though it
         * is modified and readers hold no lock, this is OK since
         * a process will pick up the flag on the next attempted
         * audit write.
         */
        u_int        obsolete;		/* Audit stopped, cleanup reference */
        u_int        destroy;		/* destroy flag, 1 marked for destroy */
        u_int        refcnt;		/* Refcnt of procs using file */
        LOCK_T       lock;		/* Lock for the structure */

	/*
	 * The following are protected by the global mfs_aflock,
 	 * and in addition can be read without any locks held
 	 * as long as there is a reference count on the auditfile
	 * struct.
 	 */
	CLR_VNODE_T     *cvp;           /* Vnode of audit file */
	mfs_pn_char_t  	*path;		/* Audit output file pathname */
	mfs_pn_char_t  	*upath;		/* Audit output file pathname in uspace */
	CRED_T	        *cred;		/* Credentials from setaudit */
	mfs_auditrec_t  *buf;		/* Audit output buffer */
	u_long		 buflen;	/* Size of audit buffer */
	/*
	 * No locking for transtype flag, it is only set when audit started.
	 */
	u_int  		af_transtype:1; /* LP64 kernel with 32bit apps */
	u_int  		af_spare:31;
	/*
	 * Following are protected by the preceding lock in this
	 * structure
	 */
	u_int		 auditwerr;	/* Error on audit write */
	u_short		 lastsize;	/* Size of last record in buffer */
	mfs_auditrec_t  *lastpos;	/* Last record in buffer */
	mfs_auditrec_t  *curpos;	/* Current pos in buffer */
	VATTR_T		 va;		/* Vattr buf space */
};
typedef struct mfs_auditfile mfs_auditfile_t;


struct mvfs_proc_inherited {
    u_int	      mpi_spare1  :14;	/* Spare "system flags" */
    u_int	      mpi_usereftime : 1; /* (I) BH DNC optimizations on */
    u_int	      mpi_auditon : 1;	/* (I) Auditing on/off */
    u_int	      mpi_spare2  :14;	/* Spare "user flags" */
    u_int	      mpi_auditnv : 1;	/* (I) Also audit non-vob */
    u_int	      mpi_auditv  : 1;	/* (I) Audit vob objects */
    view_bhandle_t    mpi_bh;		/* (I) Build handle */
    u_long	      mpi_aud_seq;	/* (I) Start audit sequence number */
    struct timeval    mpi_bh_ref_time;	/* (I) Build ref time */
    u_long	      mpi_attrgen;	/* (I) Attr cache reval generation */
    mfs_auditfile_t * mpi_afp;		/* (I) Audit file ptr */
};

struct mvfs_proc {
    LOCK_T		       mp_lock; /* lock to protect reads/writes */
    MVFS_PROCID_T 	       mp_procid; /* Process tag for hashing */
    MVFS_PROCTAG_T 	       mp_proctag; /* Process tag to verify validity */
    struct mvfs_thread 	       *mp_threads; /* linked list of threads */
    struct mvfs_proc	       *mp_hashnxt; /* linked list on hash bucket */
    struct mvfs_proc_inherited mp_inherit; /* stuff above */
    int			       mp_refcnt; /* ref count on structure */
    void *                     mp_fl_owner; /* file_lock owner field for exit */
};

struct mvfs_thread {
    MVFS_THREADID_T	       thr_threadid; /* thread ID of this one */
    struct mvfs_proc   	       *thr_proc; /* pointer to proc */
    struct mvfs_thread 	       *thr_next; /* pointer to sibling thread */
    struct mvfs_thread 	       *thr_hashnxt; /* next thread in hash */
    struct mvfs_proc_inherited thr_inherit; /* stuff above: copied to thread */
    u_short		       thr_rebindinh; /* Inhibit ctr of rebinding */
    u_short		       thr_auditinh; /* Inhibit counter of auditing */
    int			       thr_activecount;	/* count of activations */
    unsigned int	       thr_hashbucket; /* hash bucket index */
    char	               thr_errstr[20]; /* Buffer for error string */
};
#define	thr_usereftime	thr_inherit.mpi_usereftime
#define	thr_auditon	thr_inherit.mpi_auditon
#define	thr_auditnv	thr_inherit.mpi_auditnv
#define	thr_auditv	thr_inherit.mpi_auditv
#define	thr_bh		thr_inherit.mpi_bh
#define	thr_aud_seq	thr_inherit.mpi_aud_seq
#define	thr_bh_ref_time	thr_inherit.mpi_bh_ref_time
#define	thr_attrgen	thr_inherit.mpi_attrgen
#define	thr_afp		thr_inherit.mpi_afp

#define	mp_usereftime	mp_inherit.mpi_usereftime
#define	mp_auditon	mp_inherit.mpi_auditon
#define	mp_auditnv	mp_inherit.mpi_auditnv
#define	mp_auditv	mp_inherit.mpi_auditv
#define	mp_bh		mp_inherit.mpi_bh
#define	mp_aud_seq	mp_inherit.mpi_aud_seq
#define	mp_bh_ref_time	mp_inherit.mpi_bh_ref_time
#define	mp_attrgen	mp_inherit.mpi_attrgen
#define	mp_afp		mp_inherit.mpi_afp

typedef struct mvfs_proc mvfs_proc_t;
typedef struct mvfs_thread mvfs_thread_t;

/*
 * Macros for the mfs_proc struct
 */
#define MFS_INHREBIND(tp)	(tp)->thr_rebindinh++
#define MFS_ENBREBIND(tp)	{ \
	ASSERT(tp->thr_rebindinh > 0); \
	--(tp)->thr_rebindinh; \
    }
#define MFS_INHAUDIT(tp)	(tp)->thr_auditinh++;
#define MFS_ENBAUDIT(tp)	{ \
	ASSERT(tp->thr_auditinh > 0); \
	--(tp)->thr_auditinh; \
    }

/* 
 * Consolidated structure with all data for process and thread management
 * within MVFS, allocated once per virtual system at MVFS init time.   
 * mvfs_proclock protects:
 * 	mvfs_thread_t creation
 * 	mvfs_proc_t creation
 * 	all operations on mvfs_procid_hashtable (including inheritance)
 *	periodic process reclamation
 */

#define MVFS_PROCHASH_SZ	511	/* size of generic proc hash table */

typedef struct mvfs_proc_thread_data {

    LOCK_T mvfs_proclock;
    /*
     *  mvfs_threadid_spl_pool -- splock pool -- protects:
     *  all operations on hash buckets in mvfs_threadid_hashtable
     *
     * If you need both this and the mvfs_proclock, you must get the
     * mvfs_proclock (sleep lock) first.
     */
    splock_pool_t mvfs_threadid_spl_pool;
    mvfs_thread_t **mvfs_threadid_hashtable;
    mvfs_proc_t **mvfs_procid_hashtable;
    SPLOCK_T mvfs_proc_alloclock;

    int mvfs_nproc_alloced;
    int mvfs_nthr_alloced;

} mvfs_proc_thread_data_t;

/*
 * General comparison macros
 */
#define MFS_TVEQ(tv1,tv2) \
    ((tv1).tv_sec == (tv2).tv_sec && (tv1).tv_usec == (tv2).tv_usec)
/* 
 * Build handle compare macros.
 * Build handles are used as tags in several different
 * ways in the MFS, and these macros compare for those
 * particular uses.
 *
 * There are two compare macros here:
 *    MFS_BHEQ compares the whole build handle and returns TRUE
 *	if there is no difference in any build handle field.
 *    MFS_BH_SAMECONFIG compares the build_session (and NYI, but later
 *	the target stack ID) and returns TRUE if the two build handles
 *	represent the same configuration matching in the view. 
 */
#define MFS_BHEQ(bh1,bh2) \
    ((bh1).build_session == (bh2).build_session && \
     (bh1).target_id == (bh2).target_id)
/* Compare two build handles for equivalent config spec */
#define MFS_BH_SAMECONFIG(bh1,bh2) \
    ((bh1).build_session == (bh2).build_session)
#define MFS_BHNULL(bh) \
    ((bh).build_session == 0 && (bh).target_id == 0)
#define MFS_UUIDEQ(uuid1, uuid2) \
	((uuid1).time_low == (uuid2).time_low && \
        BCMP(&(uuid1), &(uuid2), sizeof(tbs_uuid_t)) == 0)
#define MFS_OIDEQ(oid1, oid2) MFS_UUIDEQ((oid1).obj_uuid, (oid2).obj_uuid)
/* Must cast OIDNULL, UUIDNULL because of CONST on var decl */
#define MFS_OIDNULL(oid) \
	((oid).obj_uuid.time_low == 0 && \
	BCMP(&(oid), (tbs_oid_t *)(&TBS_OID_NULL), sizeof(tbs_oid_t)) == 0)
#define MFS_UUIDNULL(uuid) \
	((uuid).time_low == 0 && \
	 BCMP(&(uuid), (tbs_uuid_t *)(&TBS_UUID_NULL), sizeof(tbs_uuid_t)) == 0)

/*
 * General VNODE/MNODE Macros
 */

#define MTOV(mnp)	((mnp)->mn_hdr.vp)

/* Define Vnode size so mnode is sufficiently aligned if allocated together */
#define VSIZE	((sizeof(VNODE_T) + (MVFS_VNODE_ALIGNMENT-1)) & ~(MVFS_VNODE_ALIGNMENT-1))

/* Define mn_hdr size so the class-specific portion of the mnode is 
 * aligned on an 8-byte boundary.  This is required for platforms that
 * have a 64-bit quantity somewhere in the class-specific portion of the
 * mnode.  This is typically found in the VATTR struct in the mn_vob
 * if Largefiles is supported.  
 */
#define MNHSIZE	((sizeof(struct mfs_mnhdr) + (MVFS_MNODE_ALIGNMENT-1)) & ~(MVFS_MNODE_ALIGNMENT-1))

/*
 * Not all systems have vnodes, Linux for example.  So we will create macros
 * To access fields in the Vnodes.  Inode based systems will use their own
 * versions.
 */

#define V_TO_MMI(vp)	 VFS_TO_MMI((vp)->v_vfsp)

/*
 * Map from MFS vnode to real (cleartext) vnode
 */
#define MFS_REALVP(vp)	MVFS_CVP_TO_VP(VTOM(vp)->mn_hdr.realvp)
#define MFS_CVP(vp)	MVFS_CVP_TO_VP(VTOM(vp)->mn_hdr.realvp)
#define MFS_CLRVP(vp)	VTOM(vp)->mn_hdr.realvp
#define MFS_VIEW(vp)	(MFS_ISVIEW(VTOM(vp)) ? vp : VTOM(vp)->mn_hdr.viewvp)
#define MFS_HOLDVW(vw)	{ \
	ASSERT(MFS_ISVIEW(VTOM(vw))); \
	VN_HOLD(vw); \
	VTOM(vw)->mn_view.usedtime = MDKI_CTIME(); \
    }

/* Determine if a vnode belongs to a windows view */

#define MVFS_VIEW_IS_WINDOWS_VIEW(vp) \
    (MFS_VIEW((vp)) != NULL ? VTOM((MFS_VIEW((vp))))->mn_view.windows_view : 0)

/*
 * Fetch current system-wide attribute generation number.  This is used
 * force attribute revalidation after a 'marked point' by the builders.
 */

#define MFS_MNATTRGEN(_mndp)	(_mndp)->mfs_attrgen
extern u_long mfs_mn_newattrgen(P_NONE);


/*
 * Mnode locking macros
 */
#define MHDRLOCK_PREFIX  "mh"
#define MLOCK_PREFIX     "mn"
#define STAMPLOCK_PREFIX "vs"

#define MLOCK_ADDR(mnp)		&(mnp)->mn_hdr.lock
#define STAMPLOCK_ADDR(mnp)	&(mnp)->mn_view.stamplock
#define MHDRLOCK_ADDR(mnp)	&(mnp)->mn_hdr.hdr_lock

#define MLOCK(mnp)	  MVFS_LOCK(MLOCK_ADDR(mnp))
#define MLOCK_NOWAIT(mnp) CONDITIONAL_LOCK(MLOCK_ADDR(mnp))
#define MUNLOCK(mnp)      MVFS_UNLOCK(MLOCK_ADDR(mnp))
#define MISLOCKED(mnp)    ISLOCKED(MLOCK_ADDR(mnp))
#define MLOCKHI(mnp)   /* nothing */
#define MUNLOCKHI(mnp) /* nothing */

#define MHDRLOCK(mnp)	  MVFS_LOCK(MHDRLOCK_ADDR(mnp))
#define MHDRLOCK_NOWAIT(mnp) CONDITIONAL_LOCK(MHDRLOCK_ADDR(mnp))
#define MHDRUNLOCK(mnp)      MVFS_UNLOCK(MHDRLOCK_ADDR(mnp))
#define MHDRISLOCKED(mnp)    ISLOCKED(MHDRLOCK_ADDR(mnp))

#define MLOCK2(mnp1,mnp2) mfs_mlock2(mnp1, mnp2)

/*
 * Macros Specific to VOB/View objects
 * View file handle for RPC to view
 */

#define MFS_VFH(vp)	(VTOM(vp)->mn_vob.vfh)
#define MFS_BH(cd)	mfs_getbh(cd)

/* Test VFH for history mode bit */

#define MFS_HMVFH(vfhp) VIEW_ISA_HISTORY_MODE(vfhp)

/*
 * Credentials manipulation for deferred operations on VOB objects
 */
#define MCRED(mnp) ((mnp)->mn_vob.cred)
#define MWCRED(mnp) ((mnp)->mn_vob.wcred)
#define MRCRED(mnp) ((mnp)->mn_vob.rcred)

#define MSETCRED(mnp, w, xxcred) \
	ASSERT(MFS_ISVOB(mnp)); \
	if (!MCRED(mnp)) { \
	    (mnp)->mn_vob.cred = MDKI_CRDUP((xxcred)); \
	    if (w) (mnp)->mn_vob.wcred = 1; \
	    else (mnp)->mn_vob.rcred = 1; \
	} else if ((mnp)->mn_vob.rcred && (w)) { \
	    MDKI_CRFREE((mnp)->mn_vob.cred); \
	    (mnp)->mn_vob.rcred = 0; \
	    (mnp)->mn_vob.cred = MDKI_CRDUP((xxcred)); \
	    (mnp)->mn_vob.wcred = 1; \
	}

#define MCLRCRED(mnp) { \
	ASSERT(MFS_ISVOB(mnp)); \
	if (MCRED(mnp)) MDKI_CRFREE((mnp)->mn_vob.cred); \
	(mnp)->mn_vob.cred = NULL; \
	(mnp)->mn_vob.rcred = (mnp)->mn_vob.wcred = 0; \
	}

/* Clearinfo lock macros.  Lock is used when manipulating the cred cache. */

#define MCILOCK_PREFIX "cl"

#define MCILOCK_ADDR(mnp)	&(mnp)->mn_vob.cleartext.cl_info_lock

#define MCILOCK(mnp)		MVFS_LOCK(MCILOCK_ADDR(mnp))
#define MCIUNLOCK(mnp)		MVFS_UNLOCK(MCILOCK_ADDR(mnp))

/* 
 * Invalidate attributes on a vob object.
 * Note: MLOCK not required, because simple 1 word write.
 */
#define MFS_ATTRINVAL(vp) { \
	ASSERT(MFS_ISVOB(VTOM(vp))); \
	VTOM(vp)->mn_vob.attrtime.tv_sec=0; \
    }

/* 
 * Check for valid attrs on an object.
 * if attrs are valid (but unknown timedout state), use routine
 * mfs_ac_timedout() if want to know if invalid OR timed out.
 */
#define MFS_ATTRISVALID(vp) (VTOM(vp)->mn_vob.attrtime.tv_sec != 0)

/* Check for stale on a view/vob object.
 * If stale, then flush name caches user only sees the
 * stale-ness once.
 */

#define MFS_CHK_STALE(err,vp) \
	if ((err) == ESTALE) mfs_fix_stale(vp);

/* Following macro saves a stack layer when data needs
 * to be flushed to the audit file.  mfs_audit returns 1
 * if there is no room in the buffer.
 *
 * It also makes sure the 'audited' bit is set for
 * any file audited with read/write/truncate/choid.
 * This bit is used in the determination of whether a 
 * choid is needed or not.
 *
 */

#define MFS_AUDIT_EXT(k,dvp,nm,dvp2,nm2,vp,flags,cd) {		\
	ASSERT(MVFS_MDEP_ENTER_FS()->thr_activecount > 0); \
	while (mfs_audit(k,dvp,nm,dvp2,nm2,vp,flags,cd)) {	\
	    mvfs_auditwrite(MVFS_MYTHREAD(cd));		\
	} \
	if (k == MFS_AR_READ || k == MFS_AR_WRITE || \
		k == MFS_AR_TRUNCATE || k == MFS_AR_CHOID) { \
	    mfs_set_audited(vp, cd); \
	} \
    }

#define MFS_AUDIT(k,dvp,nm,dvp2,nm2,vp,cd) \
	MFS_AUDIT_EXT(k,dvp,nm,dvp2,nm2,vp,0,cd)

/* 
 * Definition of lookup options.  The "NF_SYMLINK" (no-follow symlink)
 * is folded into the pnp struct options on some ports as a better
 * way to carry the SYMFOLLOW_T value.
 */

#define MVFS_CI_LOOKUP	0x0001	/* Case insensitive ioctl lookup */
#define MVFS_RO_LOOKUP  0x0002  /* Readonly ioctl lookup */
#define MVFS_NF_SYMLINK 0x0004  /* Don't follow symlinks */
#define MVFS_NB_LOOKUP  0x0008  /* Don't bind root on lookup */

/*
 * Macros for looking up names different ways.  mdep file may override some
 * or all of these if the OS needs special handling.
 */

/****************************************************************************
 * LOOKUP_FOR_IOCTL - do an arbitrary pathname lookup for an MVFS ioctl
 * IN pn	Ptr to null terminated ANSI string for the pathname to
 *		lookup.  On Unix, this can be a relative or absolute
 *		pathname.
 * IN s		UIO_SYSSPACE - pathname ptr is in system space
 *		UIO_USERSPACE - pathname ptr is in user space
 * IN f		SYMFOLLOW_T - whether to follow symlinks or not
 * IN opt	Lookup options
 *		MVFS_CI_LOOKUP - Case insensitive lookup (ignored on Unix)
 *		MVFS_RO_LOOKUP - Readonly lookup (ignored on Unix)
 *              MVFS_NB_LOOKUP - Don't call bindroot (Linux only)
 * OUT dvpp	(Optional) Ptr to "dir vnode ptr" to return.
 * OUT vpp	Ptr to "vnode ptr" to return for the object looked up.
 * IN cd	Ptr to a call_data structure that contains the credentials
 *              to use for looking up the object.
 *              For platforms that have not converted to using call_data
 *              this will be a pointer to a cred structure.
 * RESULT:      Unix filesystem error code
 *
 * This call is used to lookup objects from the MVFS extended 
 * ioctls.  The result may be either an MVFS or a non-MVFS vnode.
 * It is up to the caller to check.
 *
 * If dvpp != NULL, and the parent directory of the specified pathname
 * exists, this call will return a "parent dir vnode ptr" even if
 * the final component does not exist.
 *
 * If this call returns a dvp or a vp, they will be returned with
 * their reference counts incremented.
 *
 * On an unsuccessful call, the returned dvp (if dvpp !=NULL) and vp
 * will be NULL.
 *
 */
/****************************************************************************
 * LOOKUP_CHASE_MOUNTS - do a lookup chasing mountpoints
 * IN dvp	Ptr to a directory vnode in which the "nm" component
 *		should be looked up.
 * IN nm	Ptr to null terminated ANSI string for the 
 *		single component name to lookup.   This name is
 *		always in kernel space.
 * OUT vpp	Ptr to returned "vnode ptr" for the object looked up
 * IN cred	Ptr to a credentials struct to perform the lookup as
 * RESULT:      Unix filesystem error code
 *
 * This call is used by the "loopback" vnode code in the MVFS
 * to lookup single component names, while chasing mountpoints
 * across to other filesystems (including our own).  The resulting
 * vnode may be either an MVFS or a non-MVFS vnode.  It is up
 * to the caller to check.
 *
 * This call never chases symlinks.
 *
 * On a successful call, the returned vp will be returned with
 * an incremented reference count.
 * 
 * On an unsuccessful call, the returned vp will be NULL.
 *
 */
#define LOOKUP_CHASE_MOUNTS(dvp,nm,vpp,cred)  \
	mfs_lookupvp(dvp,nm,UIO_SYSSPACE,NO_FOLLOW,NULL,vpp,cred)
/****************************************************************************
 * LOOKUP_STORAGE_FILE - do an internal lookup of a storage file.
 * IN ro	TRUE - Stg file is in read-only VOB storage.
 *	        FALSE - Stg file is in writable View Storage or local
 *			file-system storage
 * IN pn	Ptr to null terminated ANSI string for the 
 *		absolute pathname to lookup.   This name is
 *		always in kernel space.
 * OUT dvpp	(Optional) Ptr to a returned "directory vnode ptr" for 
 *		the parent directory of the file (even if not found).
 * OUT vpp	(Optional) Ptr to returned "vnode ptr" for the object
 * IN cred	Ptr to a credentials struct to perform the lookup as
 * RESULT:      Unix filesystem error code
 *
 * This call is used by the MVFS to lookup internal storage files 
 * such as:
 *	- view storage cleartext files
 *	- vob storage cleartext files
 *	- audit output files
 *
 * This call will never create loopback vnodes, audit any of
 * its operations or walk down the MVFS filesystem, even if the user 
 * is currently in a setview context.
 *
 * This call may be used to lookup the "parent directory" of a
 * pathname that does not exist, so that a file may be created
 * relative to that returned dvp.
 *
 * If a dvp or vp is returned, it will be returned with its
 * reference count incremented.
 * 
 * On an unsuccessful call, the returned vp will be NULL.
 *
 * The "ro" (read-only) flag is used to tell the underlying
 * system (if needed) what kind of protection to access the
 * file with depending on the assumed access that may be
 * required to the storage area.
 *
 */

/****************************************************************************
 * LOOKUP_AUDIT_FILE - do an internal lookup of a audit file.
 * IN pn	Ptr to null terminated ANSI string for the 
 *		absolute pathname to lookup.   This name is
 *		always in kernel space.
 * OUT vpp	(Optional) Ptr to returned "vnode ptr" for the object
 * IN cred	Ptr to a credentials struct to perform the lookup as
 * RESULT:      Unix filesystem error code
 *
 * This call is used by the MVFS to lookup audit output files.
 *	- audit output files
 *
 * This call will never create loopback vnodes, audit any of
 * its operations or walk down the MVFS filesystem, even if the user 
 * is currently in a setview context.
 *
 * If a vp is returned, it will be returned with its
 * reference count incremented.
 * 
 * On an unsuccessful call, the returned vp will be NULL.
 *
 */

/*
 * To support split storage pool mappings, the VOB storage-relative pathname
 * of a cleartext is mashed against an array of mvfs_sp_ent_t's.
 * a matching prefix is replaced with the translation.  If there are no
 * matching prefixes, the vob storage pathname is prepended to the target.
 */
typedef struct mvfs_sp_ent {
    mfs_pn_char_t	*sp_prefix;	/* prefix for matching */
    mfs_pn_char_t	*sp_target;	/* target for replacement */
    mfs_pn_char_t	*sp_usertarget;	/* user-space replacment--for user queries */
    size_t		sp_prlen;	/* precomputed STRLEN()s */
    size_t		sp_tglen;
    size_t		sp_usrlen;
} mvfs_sp_ent_t;

#define VFS_FSID(vfsp)		MDKI_VFSID(vfsp)

/*
 * MFS mount (vfs) info.  This is the same for both a VOB
 * mount and the viewserver root mount.  A lot of fields are
 * not used in the viewserver root mount case.
 */

struct mfs_mntinfo {
	int		 mmi_mnttype;	/* VOB vs. Viewserver mount */
	int		 mmi_mntindex;	/* Mount table index (vob mounts) */
	VNODE_T         *mmi_rootvp;	/* Root vnode ptr */
	u_long		 mmi_refcnt;	/* # vnodes active on mount */
	u_long		 mmi_gen;	/* Mount generation number */
	int		 mmi_minor;	/* Minor dev # for this mount */
	int		 mmi_nvminor;   /* Minor dev # when no view */
	fsid_t		 mmi_nvfsid;	/* fsid when no view */
	dev_t		 mmi_rdev;	/* For rdev in stat calls */
	struct mfs_svr	 mmi_svr;	/* Mount point server info */
	struct mfs_retryinfo mmi_retry;	/* Mount point retry info */
        int              mmi_zoneid;    /* zone id of the mount */
	mfs_pn_char_t   *mmi_mntpath;	/* Mount point pathname */
	mfs_pn_char_t	*mmi_vobtag;	/* Vob-tag (for nt-viewtag support) */
	tbs_oid_t	 mmi_voboid;	/* VOB family OID */
	tbs_uuid_t	 mmi_vobuuid;	/* VOB instance uuid */
	tbs_dbid_t	 mmi_root_edbid; /* Root element dbid */
	u_int		 mmi_noac : 1;	/* No attribute cache */
	u_int		 mmi_nodnlc : 1; /* No name lookup cache */
	u_int		 mmi_needs_recovery : 1;  /* needs recovery printed */
	u_int		 mmi_needs_reformat : 1;  /* needs reformat printed */
	u_int		 mmi_vobstale_err : 1;	/* Vob stale error printed */
        u_int            mmi_mbz : 27;
	u_long		 mmi_ac_regmax;	/* File attr cache max timeout */
	u_long  	 mmi_ac_regmin;	/* File attr cache min timeout */
	u_long		 mmi_ac_dirmax;	/* Dir attr cache max timeout */
	u_long  	 mmi_ac_dirmin;	/* Dir attr cache min timeout */
        time_t           mmi_ac_dir_ftime; /* dir attribute cache flush time */
	mfs_pn_char_t	*mmi_hmsuffix;	/* History mode suffix string */
	size_t		 mmi_hmsuffixlen;   /* Pre-computed length */
	mfs_pn_char_t	*mmi_hmvers_nm; /* Nm in a dir for HM ver of dir */
	mfs_class_t	 mmi_default_vwtag_kind;  /* Kind of vw-tags to make */
        int		 mmi_sptable_len; /* length of table */
        mvfs_sp_ent_t	*mmi_sptable;	/* split pool table entries;
					   NULL if none  */
	SPLOCK_T	 mmi_rclock;	/* spinlock for mmi_refcnt */
};

#define MFS_VOBMNT	1
#define MFS_VIEWSVRMNT	2

#define MFS_ISVOBMNT(mmip)	((mmip)->mmi_mnttype == MFS_VOBMNT)
#define MFS_ISVIEWSVRMNT(mmip)	((mmip)->mmi_mnttype == MFS_VIEWSVRMNT)

/* Mount generation number.  This is incremented on every mfs
   unmount.  The combination (vfs_fsid, mmi_gen) is a suitable
   cache tag for  caching info about this mount. */

extern u_long mfs_mntgen;

/*
 * FSS type ID and device major number
 */
extern MVFS_FSTYP_T	mfs_fstyp;
extern MVFS_MAJOR_T mvfs_major;
extern MVFS_MAJOR_T * mvfs_majortbl;

extern view_bhandle_t mfs_null_bh;

/*
 * Global cache enable/disable flags
 */
extern int mvfs_acenabled;	/* In mfs_vnodeops.c */
extern int mvfs_rlenabled;
extern int mvfs_rdcenabled;
extern int mvfs_ctoenabled;
extern int mvfs_dncenabled;	/* In mfs_dncops.c */
extern int mvfs_dncnoentenabled;
extern int mvfs_rvcenabled;	/* In mfs_rvc.c */

/* Turn on/off panic for assert calls */
extern tbs_boolean_t mvfs_panic_assert;

/*
 * Generic MFS flag options for routines called from mfs_sync()
 * that may not want to wait on locks.
 */
#define MFS_SLEEP	0
#define MFS_NOSLEEP	1

/*
 * If machine has at least this many kilobytes of main memory,
 * force "largeinit".
 */
#define	MVFS_MINIMUM_LARGEMEM	(24*1024)

/* The upper limit for auto scaling of mvfs_largeinit. */
#define MVFS_LARGEINITMAX 24

/*
 * The step size (in KBytes) for the step function in mvfs_compute_largeinit()
 * used to auto scale mvfs_largeinit.  Currently 1 GB (expressed in KB).
 */
#define MVFS_LARGEINITBLK (1024*1024)

/*
 * Minor map structure for vfs_getnum/vfs_putnum
 */
#define MVFS_MINMAPSIZE	mvfs_minmapsize
typedef struct mvfs_minmap {		/* struct so can take its address */
    SPLOCK_T lock;
    u_char *vec;
} mvfs_minmap_t;
#define MVFS_MINMAP_T	mvfs_minmap_t

/*
 * MVFS_MAJDYNMAX: The max number of major devices can be used by non-exported
 *                 vobs.
 * MVFS_MAJFIXMAX: The max number of major devices can be used by ncaexported
 *                 vobs.
 * MVFS_MINORMAX: The max number of minors to differentiate vobs within a
 *                major number.
 */
#define MVFS_MAPINIT(map, error)	{ \
        MVFS_MINMAPSIZE = (MVFS_MAJDYNMAX+MVFS_MAJFIXMAX)*MVFS_MINORMAX/NBBY; \
        if (((map)->vec = (u_char *)KMEM_ALLOC(MVFS_MINMAPSIZE, KM_SLEEP)) == NULL) { \
            error = ENOMEM; \
        } \
        else { \
            BZERO((caddr_t)((map)->vec), MVFS_MINMAPSIZE); \
	    INITSPLOCK((map)->lock,"mvfs_map_spl"); \
            MVFS_MDEP_MAPINIT((map)); \
            error = 0; \
        } \
    }
#define MVFS_MAPFREE(map)	{ \
        KMEM_FREE((map)->vec, MVFS_MINMAPSIZE); \
        FREESPLOCK((map)->lock); \
    }

extern MVFS_MINMAP_T mvfs_minormap;

/* In mfs_mioctl.c */

EXTERN int
mvfs_ioctl_copyin(
    mvfscmd_block_t *data,
    mvfscmd_block_t *iocbuf,
    caddr_t *infop,
    MVFS_CALLER_INFO *callinfo
);

EXTERN int
mvfs_ioctl_lookup(
    mvfscmd_block_t *iocbuf,
    VNODE_T **vpp,
    CLR_VNODE_T **cvpp,                 /* may be null if MFS_VPISMFS(*vpp) */
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
);

EXTERN int
mvfs_ioctl_copyout(
    mvfscmd_block_t *data,
    mvfscmd_block_t *iocbuf,
    caddr_t infop,
    MVFS_CALLER_INFO *callinfo
);

EXTERN int
mvfs_mioctl(
    VNODE_T *vp,
    CLR_VNODE_T *cvp,
    mvfscmd_block_t *data,
    int flag,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
);

/* In mfs_vnodeops.c */

/* Generic sleep/nosleep flags used in many places */

#define MFS_SLEEP	0
#define MFS_NOSLEEP	1
 
#define MVFS_PROCPURGE_SLEEP	0
#define MVFS_PROCPURGE_NOSLEEP	1
#define MVFS_PROCPURGE_FLUSH	2

#define MFS_INACTIVE_SLEEP 	MFS_SLEEP
#define MFS_INACTIVE_NOSLEEP 	MFS_NOSLEEP

/* Flags for change_oid */

#define MFS_CHOID_FORCE 1	/* Force choid (ignore dupl reduction) */
#define MFS_CHOID_TRUNC	2	/* Truncate op - no need to copy for COW */

EXTERN int
mvfs_sync_attr(
    mfs_mnode_t *mnp,
    VATTR_T *vap,
    int bhflag,
    u_int saflag,
    CALL_DATA_T *cd
);

EXTERN int
mfs_ac_timedout(
    struct mfs_mnode *mnp,
    tbs_boolean_t evmiss_flag,
    CALL_DATA_T *cd
);

EXTERN void
mvfs_set_ac_timeout(
    struct mfs_mnode *,
    VFS_T *,
    time_t not_later_than,
    int bumpgen,
    int goodlvut
);

EXTERN int
mvfs_ac_set_stat(
    struct mfs_mnode *mnp,
    view_vstat_t *vstat,
    int goodlvut,
    CRED_T *cred
);

EXTERN void
mfs_attrcache(
    VNODE_T *vp,
    view_vstat_t *vstat,
    int expmod,     /* Indicates caller expects this modification */
    CRED_T *cred
);

EXTERN int
mfs_chkaccess(P1(VNODE_T *vp)
	      PN(int mode)
	      PN(u_long vuid)
	      PN(u_long vgid)
	      PN(int vmode)
	      PN(CRED_T *cred));

EXTERN int
mfs_evtime_valid(
    VNODE_T *vp,
    struct timeval *tvp,
    CALL_DATA_T *cd
);

EXTERN void
mfs_set_audited(
    VNODE_T *vp,
    CALL_DATA_T *cd
);

EXTERN int
mfs_change_oid(
    VNODE_T *vp,
    int choidflag,
    int sleepflag,
    CALL_DATA_T *cd
);

EXTERN int
mfs_changeattr_eval_choid(P1(VNODE_T *vp)
			  PN(VATTR_T *vap)
			  PN(int flag)
			  PN(CRED_T *cred)
			  PN(tbs_boolean_t *choid_needed_p)
			  PN(u_long *view_db_mask_p)
			  PN(u_long *clear_mask_p));

#ifdef MVFS_MMAP_CVP
EXTERN int
mvfs_mmap_getcvp(
    VNODE_T *vp,
    CLR_VNODE_T **cvpp,
    u_int mflags,
    u_int prot,
    CALL_DATA_T *cd
);

EXTERN void
mvfs_mmap_no_audit(
    VNODE_T *vp,
    u_int mflags,
    u_int prot,
    CALL_DATA_T *cd
);
#endif

EXTERN int
mfs_inactive_common(
    VNODE_T *vp,
    int sleep_flag,
    CALL_DATA_T *cd
);
EXTERN int
mfs_std_inactive(
    VNODE_T *vp,
    CALL_DATA_T *cd
);
EXTERN VNODE_T *
mfs_getview(P1(VNODE_T *) 
	    PN(CRED_T *)
	    PN(int));

EXTERN VNODE_T *
mfs_bindroot(
    VNODE_T *vp,
    CALL_DATA_T *cd,
    int *errp
);
/*
 * Used in vnget routines to init vnode to disallow
 * mapping by default if flag reset (by hand) to 0
 */

extern int mfs_map_enabled;

EXTERN int 
mfs_makevobrtnode(P1(VNODE_T *) 
		  PN(VFS_T *) 
		  PN(VNODE_T **));

EXTERN int 
mfs_makevobnode(
    view_vstat_t *vstatp,
    struct timeval *lvut,
    VNODE_T *vw,
    view_fhandle_t *vfhp,
    VFS_T *vfsp,
    CRED_T *cred,
    VNODE_T **vpp
);

EXTERN int 
mfs_makeloopnode(
    VNODE_T *,
    CLR_VNODE_T *,
    VNODE_T **, 
    CRED_T *
);

EXTERN int 
mfs_makespecnode(P1(mfs_class_t) 
		 PN(VNODE_T *) 
		 PN(void *)
		 PN(VFS_T *) 
		 PN(VNODE_T **));

EXTERN void
mfs_rebind_self(
    VNODE_T *vp,
    CALL_DATA_T *cd
);

EXTERN int
mfs_rebind_vpp(
    int release,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

EXTERN dev_t
mvfs_devadjust(P1(dev_t dev)
	       PN(VNODE_T *vp));

/* In mfs_vfsops.c */

EXTERN int 
mvfsinit(
    VFSSW_T *vswp,
    int fsnum
);

EXTERN int
mfs_unload(P1(VFSSW_T *vswp)
	   PN(int fsnum));

EXTERN int
mfs_getmnode(
    VFS_T *vfsp,
    VNODE_T *vw,
    mfs_fid_t *fidp,
    mfs_mnode_t **mnpp,
    int *nnp,           /* Newnode flag ptr */
    CALL_DATA_T *cd     /* May be NULL */
);

EXTERN int
mfs_getvnode(
    VFS_T *vfsp,
    VNODE_T *vw,
    mfs_fid_t *fidp,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

EXTERN int
mvfs_vfsgetnum(P1(MVFS_MINMAP_T *map)
	       PN(int size));
EXTERN int
mvfs_vfsputnum(P1(MVFS_MINMAP_T *map)
	       PN(int size)
	       PN(int index));
EXTERN int
mvfs_get_minorpair(P1(MVFS_MINMAP_T *map)
		   PN(int size)
		   PN(int index));

EXTERN void
mvfs_compute_largeinit(u_long kbytes_available);

/* In mfs_clnt.c */

#define MFS_USE_NULLBH		1
#define MFS_USE_PROCBH		0

EXTERN int
mvfs_clnt_init(mvfs_cache_sizes_t *mma_sizes);
EXTERN void mvfs_clnt_destroy(void);

EXTERN int
mfs_clnt_getattr_mnp(
    mfs_mnode_t *mnp,
    VFS_T *vfsp,
    CALL_DATA_T *cd
);
EXTERN int 
mfs_clnt_getattr(
    VNODE_T *vp,
    CALL_DATA_T *cd
);

EXTERN int 
mvfs_clnt_setattr_locked(
    VNODE_T *vp, 
    VATTR_T *vap,
    u_long xmode,
    int bhflag,
    int wcred, 
    CALL_DATA_T *cd,
    u_int saflag
);

#define MVFS_SATTR_ATIME_EROFS_OK 0x1 /* suppress EROFS when set atime only */

EXTERN int 
mfs_clnt_readlink(
    register VNODE_T *vp,
    mfs_pn_char_t *lnkbuf, 
    int *lnklenp,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_remove(
    register VNODE_T *dvp,
    mfs_pn_char_t *nm,
    int bhflag,
    int sleep,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_lookup(
    VNODE_T *dvp,
    mfs_pn_char_t *nm,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_create(
    VNODE_T *dvp,
    mfs_pn_char_t *nm,
    VATTR_T *va,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_link(
    register VNODE_T *vp,
    VNODE_T *tdvp,
    mfs_pn_char_t *tnm,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_rename(
    VNODE_T *odvp,
    mfs_pn_char_t *onm,
    VNODE_T *tdvp,
    mfs_pn_char_t *tnm,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_mkdir(
    VNODE_T *dvp,
    mfs_pn_char_t *nm,
    VATTR_T *va,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_rmdir(
    VNODE_T *dvp,
    mfs_pn_char_t *nm,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_symlink(
    VNODE_T *dvp,
    mfs_pn_char_t *lnm,
    VATTR_T *tva,
    mfs_pn_char_t *tnm,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_readdir(
    VNODE_T *dvp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    int *eofp
);

EXTERN int 
mfs_clnt_readdirx(
    VNODE_T *dvp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    view_readdir_flag_t flags,
    int *eofp
);

EXTERN int 
mfs_clnt_inval(
    VNODE_T *vw,
    view_invalidate_type_t itype,
    A_CONST tbs_oid_t *voboidp,
    A_CONST tbs_oid_t *oidp,
    mfs_pn_char_t *nm,
    CALL_DATA_T *cd
);

#define MFS_CHOID_SETAUDIT	1
#define MFS_CHOID_CLRAUDIT   	0
EXTERN int
mfs_clnt_choid(
    VNODE_T *vp,
    view_change_oid_option_t opts,
    tbs_oid_t *prevoidp,
    CALL_DATA_T *cd
);

EXTERN int
mfs_clnt_choid_locked(
    register VNODE_T *vp,
    view_change_oid_option_t opts,
    tbs_oid_t *prevoidp,
    CALL_DATA_T *cd
);

EXTERN int
mfs_clnt_bindroot(
    int root,
    VNODE_T *vw,
    VFS_T *vfsp,
    mfs_pn_char_t *nm,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);
EXTERN int
mfs_clnt_rebind_dir(
    VNODE_T *dvp,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

EXTERN int
mfs_clnt_gpath_elem(
    VNODE_T *vp,
    mfs_pn_char_t **nmp,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clnt_cltxt_locked(
    VNODE_T *vp,
    CALL_DATA_T *cd
);
EXTERN int
mfs_clnt_change_mtype(
    register VNODE_T *vp,
    vob_mtype_t mtype,
    tbs_status_t *statusp,      /* Returned tbs_status */
    CALL_DATA_T *cd
);

EXTERN int
mvfs_clnt_ping_server(
    struct mfs_svr *svr,
    CRED_T * cred
);

/* In mfs_clearops.c -- require mnode locked */

EXTERN void
mfs_clearperr(P1(VNODE_T *)
	      PN(char *)
	      PN(int));

EXTERN int
mfs_set_cpname(	P1(struct mfs_mnode *)
	  	PN(mfs_pn_char_t *)
	   	PN(size_t));

EXTERN void
mfs_clear_error(P1(VNODE_T *vp)
		PN(char *s)
		PN(int error));

EXTERN void
mfs_clear_mark_doc(P1(VNODE_T *));

EXTERN void
mfs_clear_mark_purge(P1(VNODE_T *));

EXTERN void
mfs_clear_mark_name_purge(P1(VNODE_T *));

EXTERN void
mfs_clear_mark_rwerr(P1(VNODE_T *));

EXTERN void 
mfs_clear_rele(
    VNODE_T *vp,
    CRED_T *cred
);

EXTERN int
mfs_clear_writable(P1(VNODE_T *vp));

EXTERN int 
mvfs_clearattr(
    VNODE_T *vp,
    VATTR_T *vap,
    CRED_T *cred
);

EXTERN int
mfs_clearowner(
    VNODE_T *vp,
    u_long mask,
    CALL_DATA_T *cd
);
EXTERN void
mvfs_new_cltxt(
    VNODE_T *vp,
    CLR_VNODE_T *new_cvp
);

EXTERN void
mvfs_clear_release_mnode_credlist(
    struct mfs_mnode *mnp
);

EXTERN int
mvfs_clear_init(mvfs_cache_sizes_t *mma_sizes);

EXTERN void
mvfs_clear_free(void);

EXTERN void
mvfs_flush_credlists(
    tbs_boolean_t force
);

EXTERN void
mvfs_record_cred(
    struct mfs_mnode *mnp,
    CRED_T *cred
);

extern ks_uint32_t
mvfs_hash_cred(
    CRED_T *cred
);

EXTERN void
mvfs_clear_log_stale(
    VNODE_T *vp
);

/* Utility routines -- the macros may be overridden in an mdep file */

EXTERN tbs_boolean_t
mvfs_ansi_str_islower(P1(char *s));

EXTERN mfs_pn_char_t *
mfs_getleaf_ansi(P1(mfs_pn_char_t *));

EXTERN int
mvfs_ansi_trailing_seps(P1(char *pn) PN(size_t pn_len));

#define MVFS_STR_ISLOWER(s)  mvfs_ansi_str_islower(s)

#define MVFS_FIND_LEAF(s)  mfs_getleaf_ansi(s)

#define MVFS_TRAILING_SEPS(pn, pn_len)  mvfs_ansi_trailing_seps(pn, pn_len)

int
mfs_getcleartext_nm(
    VNODE_T *vp,
    CALL_DATA_T *cd
);

EXTERN int 
mfs_getcleartext(
    VNODE_T *vp,
    CLR_VNODE_T **cvpp,                 /* return */
    CALL_DATA_T *cd
);

EXTERN int 
mfs_clear_create(
    VNODE_T *vp,
    VATTR_T *vap,
    CLR_VNODE_T **cvpp,
    CALL_DATA_T  *cd,
    int flag
);

EXTERN int
mfs_copyvp(P1(CLR_VNODE_T *ocvp)
	   PN(CLR_VNODE_T *ncvp)
	   PN(VATTR_SIZE_T len)
	   PN(CRED_T *cred));

/* In mfs_viewdirops.c */

#define NORELEASE 	0
#define RELEASE		1

EXTERN int 
mfs_ramdir_add(P1(VNODE_T *) 
	       PN(mfs_pn_char_t *)
	       PN(VNODE_T *)
	       PN(int *));

EXTERN int 
mfs_ramdir_remove(P1(VNODE_T *) 
		  PN(mfs_pn_char_t *)
		  PN(VNODE_T **));

EXTERN void 
mfs_ramdir_purgevp(P1(VNODE_T *)
		   PN(VNODE_T *));

EXTERN void 
mfs_ramdir_purge(P1(VNODE_T *) 
		 PN(int));

EXTERN int
mfs_ramdir_refchk(P1(VNODE_T *)
		  PN(int *));

EXTERN int 
mfs_viewdirmkdir(P1(VNODE_T *dvp)
		 PN(mfs_pn_char_t *nm)
		 PN(VATTR_T *vap)
		 PN(VNODE_T **vpp)
		 PN(CRED_T *cred)
		 PN(mfs_pn_char_t *hostnm)
		 PN(tbs_boolean_t is_windows_view));

EXTERN int 
mfs_viewdirrmdir(P1(VNODE_T *dvp) 
		 PN(mfs_pn_char_t *nm)
		 PN(CRED_T *cred));
EXTERN int 
mvfs_viewdirunexport(P1(VNODE_T *) 
		    PN(VNODE_T *));

EXTERN int 
mvfs_viewdirexport(P1(VNODE_T *) 
		   PN(VNODE_T *)
		   PN(int));

EXTERN int 
mfs_viewdiropen(P1(VNODE_T **) 
		PN(int) 
		PN(CRED_T *));

EXTERN int 
mfs_viewdirclose(P1(VNODE_T *)
		 PN(int) 
		 PN(int) 
		 PN(CRED_T *));

EXTERN int 
mfs_viewdirgetattr(P1(VNODE_T *)
		   PN(VATTR_T *)
		   PN(CRED_T *));

EXTERN int 
mfs_viewdirsetattr(P1(VNODE_T *)
		   PN(VATTR_T *)
		   PN(CRED_T *));

EXTERN int 
mvfs_viewdirreaddir(
    VNODE_T *dvp,
    struct uio *uiop,
    CRED_T *cred,
    int *eofp
);

EXTERN int 
mfs_viewdirreaddirx(P1(VNODE_T *)
		   PN(struct uio *)
		   PN(CRED_T *));

EXTERN int
mfs_viewtaglookup(P1(mfs_pn_char_t *nm) 
		  PN(VNODE_T **vpp)
		  PN(CRED_T *cred));
EXTERN int
mvfs_viewuuidrecover(
    mfs_pn_char_t *nm,
    tbs_uuid_t *vwuuid,
    char *host,
    mfs_strbufpn_pair_t *lpn,
    char *rpn,
    VNODE_T **vwpp,
    CRED_T *cred
);

EXTERN int 
mfs_viewdirlookup(P1(VNODE_T *)
		  PN(mfs_pn_char_t *)
		  PN(VNODE_T **)
		  PN(CRED_T *)
		  PN(struct pathname *)
		  PN(int));

EXTERN int
mfs_viewdirhmview(P1(VNODE_T *)
		  PN(VNODE_T **)
		  PN(CRED_T *cred));

EXTERN void
mfs_viewdircleanhm(P1(VNODE_T *));	

EXTERN void
mfs_viewdirflushrvc(P_NONE);

/***************************************************************************
 * MVFS_RVCFLUSH - flush all root version cache entries for the specified view.
 *
 * IN vw        Ptr to view-tag vnode to invalidate.  This may be either
 *              a loop-back view or an NT-style view-tag.
 * IN vfsp      pointer to VFS (limit flushing to that VOB), or NULL for all
 *              VOBs
 */

EXTERN void
mvfs_rvcflush(
    VNODE_T *vw,
    VFS_T *vfsp
);

EXTERN int
mvfs_viewdir_find_vobstamp(P1(VNODE_T *vw)
			   PN(tbs_uuid_t *vobuuid)
			   PN(struct timeval *outstamp)
			   PN(time_t *outlife));
EXTERN void
mvfs_viewdir_save_vobstamp(P1(VNODE_T *vw)
			   PN(tbs_uuid_t *vobuuid)
			   PN(time_t timeout)
			   PN(struct timeval *newlvut));

EXTERN int
mvfs_viewinit(mvfs_cache_sizes_t *mma_sizes);
EXTERN void mvfs_viewfree(P_NONE);

/* In mvfs_ntvwops.c */

EXTERN int
mvfs_ntvw_getattr(P1(VNODE_T *vp)
		  PN(VATTR_T *vap)
		  PN(CRED_T *cred));

EXTERN int
mvfs_ntvw_lookup(P1(VNODE_T *dvp)
		 PN(char *nm)
		 PN(VNODE_T **vpp)
		 PN(struct pathname *pnp)
		 PN(int flags)
		 PN(VNODE_T *rdir)
		 PN(CRED_T *cred));

EXTERN int
mvfs_ntvw_readdir(
    VNODE_T *dvp,
    struct uio *uiop,
    CRED_T *cred,
    int *eofp
);

EXTERN int
mvfs_ntvw_readdirx(P1(VNODE_T *dvp)
		  PN(struct uio *uiop)
		  PN(CRED_T *cred));

/* In mfs_procops.c */

EXTERN int
mvfs_procinit(mvfs_cache_sizes_t *mma_sizes);
EXTERN void mvfs_procpurge(P1(int slp));
EXTERN int
mvfs_proc_setview(
    VNODE_T *vw,
    tbs_status_t *status_p
);
EXTERN void mvfs_procdata_free(P_NONE);
EXTERN mvfs_thread_t *mvfs_enter_fs(P_NONE);
EXTERN mvfs_thread_t *mvfs_mythread(P_NONE);
EXTERN void mvfs_exit_fs(P1(mvfs_thread_t *mth));
EXTERN void mvfs_sync_procstate(P1(mvfs_thread_t *mth));
EXTERN void mvfs_sync_procstate_locked(P1(mvfs_thread_t *mth));
EXTERN mvfs_proc_t * mvfs_findproc(P1(MVFS_PROCID_T *procid)
				   PN(MVFS_PROCTAG_T *tagp)
				   PN(unsigned int *hashp));
EXTERN void mvfs_zap_proc(P1(mvfs_proc_t *proc));
EXTERN void mvfs_procinherit(P1(mvfs_proc_t *mp));
EXTERN mvfs_proc_t * mvfs_procinherit_from(P1(mvfs_proc_t *mp));

EXTERN int
mvfs_proc_getcaches(mvfs_cache_sizes_t *szp);
EXTERN int
mvfs_proc_compute_caches(
    ks_int32_t scale,
    mvfs_cache_sizes_t *szp
);

/* In mfs_mnode.c */

EXTERN int
mvfs_mninit(mvfs_cache_sizes_t *mma_sizes);
EXTERN void mfs_mlock2(P1(mfs_mnode_t *) PN(mfs_mnode_t *));

EXTERN mfs_mnode_t *
mfs_mnget(P1(mfs_class_t) 
	  PN(VNODE_T *)
	  PN(mfs_fid_t *)
	  PN(VFS_T *)
	  PN(int *));

EXTERN void
mvfs_mnsyncmnodes(
    VFS_T *vfsp
);

EXTERN mfs_mnode_t *
mfs_mngetnextoid(P1(int *)
		 PN(VNODE_T *)
		 PN(tbs_oid_t *)
	      	 PN(tbs_oid_t *));

EXTERN mfs_mnode_t *
mvfs_mngetnextview(int *mnum);

#if defined(MVFS_DEBUG) && defined(MVOP_PRINT)
EXTERN void
mvfs_mnreport_leftover_vnodes(
    VFS_T *vfsp
);
#endif

EXTERN void 
mfs_mnrele(P1(mfs_mnode_t *));

EXTERN void
mfs_mninvaloid(P1(tbs_oid_t *)
	       PN(tbs_oid_t *));

#define MVFS_MN_CVPFLUSH_LOWMARK 1
#define MVFS_MN_CVPFLUSH_HALF 2
#define MVFS_MN_CVPFLUSH_ALL 3
#define MVFS_MN_CVPFLUSH_AGED 4
#define MVFS_DEFAULT_AGE_CVP_TIME (60*60*24*3)

EXTERN void
mvfs_mnflush_cvpfreelist(int flush_type);

EXTERN void
mfs_mnflush(P_NONE);

EXTERN void 
mfs_mnflushvfs(P1(VFS_T *));

EXTERN void 
mfs_mnflushvw(P1(VNODE_T *));

EXTERN void 
mfs_dnc_flushvw(P1(VNODE_T *));

EXTERN int mfs_mncount(P_NONE);

EXTERN void mvfs_mndata_free(P_NONE);

EXTERN void mvfs_mn_count(P1(mvfs_cache_usage_t *));

EXTERN int mvfs_mn_setcaches(P1(mvfs_cache_sizes_t *szp));
EXTERN int
mvfs_mn_getcaches(mvfs_cache_sizes_t *szp);
EXTERN int
mvfs_mn_compute_caches(
    ks_int32_t scale,
    mvfs_cache_sizes_t *szp
);

EXTERN void
mvfs_mnclear_logbits(void);

/* In mfs_auditops.c */

EXTERN int
mvfs_auditinit(mvfs_cache_sizes_t *mma_sizes);
EXTERN view_bhandle_t
mfs_getbh(CALL_DATA_T *cd);

EXTERN int 
mfs_auditioctl(
    mvfscmd_block_t *kdata,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
);

EXTERN void
mfs_init_rmstat(P1(VNODE_T *vp)
		PN(struct mfs_auditrmstat *rmstatp));

EXTERN int 
mfs_audit(
    int kind,
    VNODE_T *dvp,
    mfs_pn_char_t *nm1,
    VNODE_T *dvp2,
    mfs_pn_char_t *nm2,
    VNODE_T *vp,
    u_long flags,
    CALL_DATA_T *cd
);

EXTERN void 
mfs_afphold(P1(mfs_auditfile_t *));

EXTERN void 
mvfs_afprele_proc(P1(mvfs_proc_t *) PN(mvfs_thread_t *));

EXTERN void 
mvfs_afprele_thr(P1(mvfs_thread_t *));

EXTERN void
mvfs_afprele(P1(mfs_auditfile_t *afp)
	     PN(mvfs_thread_t *thr));

EXTERN void 
mvfs_auditwrite(P1(mvfs_thread_t *));

EXTERN void mvfs_auditfree(P_NONE);
EXTERN int mfs_v2objtype(P1(VTYPE_T vtype));

/* In mfs_rpcutl.c */

#define mfs_geterrno(s)	tbs_status2errno(s)

EXTERN int
mvfs_bindsvr_port(
    struct mfs_svr *svr,
    VFS_T *vfsp,
    CRED_T *cred,
    VNODE_T *vw
);

EXTERN int
mfs_vwcall(P1(VNODE_T *)
	   PN(VFS_T *)
	   PN(int)
	   PN(xdrproc_t)
	   PN(void *)
	   PN(xdrproc_t)
	   PN(void *)
	   PN(CRED_T *));

EXTERN int 
mfscall(P1(struct mfs_callinfo *)
	PN(int op)
	PN(XID_T xid)
	PN(struct mfs_svr *)
	PN(struct mfs_retryinfo *)
	PN(xdrproc_t)
	PN(void *)
	PN(xdrproc_t)
	PN(void *)
	PN(CRED_T *)
	PN(VNODE_T *));

EXTERN void
mvfs_bumptime(timestruc_t *,
              timestruc_t *,
              timestruc_t *
);

EXTERN int
mvfs_rpc_setcaches(P1(mvfs_cache_sizes_t *szp));
EXTERN int
mvfs_rpc_getcaches(mvfs_cache_sizes_t *szp);
EXTERN int
mvfs_rpc_compute_caches(
    ks_int32_t scale,
    mvfs_cache_sizes_t *szp
);

EXTERN int
mvfs_rpc_count(P1(mvfs_cache_usage_t *));

EXTERN int
mvfs_clnt_get(
    struct mfs_callinfo *trait,
    struct mfs_svr *svr,
    struct mfs_retryinfo *rinfo,
    CRED_T *cred,
    VNODE_T *view,
    CLIENT **client_p
);

EXTERN void
mvfs_clnt_free(
    CLIENT *client,
    int error,
    VNODE_T *view
);

/* In mfs_utils.c */

EXTERN VTYPE_T 
mfs_ftype_to_vtype(P1(tbs_ftype_t));

EXTERN void 
mfs_fstat_to_vattr(P1(tbs_fstat_t *) 
		   PN(VATTR_T *));

EXTERN void
mfs_mn_to_vattr(P1(mfs_mnode_t *mnp)
		PN(VATTR_T *vap));

EXTERN void 
mfs_vattr_to_sattr(P1(VATTR_T *)
		   PN(view_set_attr_t *));

EXTERN void
mfs_sattr_null(P1(view_set_attr_t *));

EXTERN int
mfs_sattr_is_null(P1(view_set_attr_t *));

EXTERN int
mvfs_ansi_strcasecmp(P1(char *s1) PN(char *s2));

EXTERN int
mvfs_ansi_strncasecmp(P1(char *s1) PN(char *s2) PN(size_t n));

EXTERN mfs_pn_char_t *
mfs_uniq_name(P_NONE);

EXTERN int
mvfs_util_init(mvfs_cache_sizes_t *mma_sizes);

EXTERN void
mvfs_util_free(P_NONE);

EXTERN int
mfs_hmsuffix_len(P_NONE);

EXTERN int
mfs_hmname(P1(mfs_pn_char_t *)
	   PN(mfs_pn_char_t **));

EXTERN int
mfs_hmcmp(P1(mfs_pn_char_t *)
	  PN(mfs_pn_char_t *));

EXTERN int
mfs_hmstrcat(P1(mfs_pn_char_t *));

EXTERN char *
mfs_hmappend(P1(mfs_pn_char_t *));

EXTERN void
mfs_perror(P1(int)
	   PN(char *));

EXTERN char *
mfs_strerr(P1(int));

EXTERN char *
mfs_vw2nm(P1(VNODE_T *vw));

EXTERN char *
mfs_vp2vw(P1(VNODE_T *vp));

EXTERN char *
mfs_vp2dev(P1(VNODE_T *vp));

EXTERN u_long
mfs_vp2dbid(P1(VNODE_T *vp));

EXTERN char *
mfs_strdup(P1(char *));

EXTERN void
mfs_fix_stale(P1(VNODE_T *));

EXTERN int 
mfs_copyin_strbuf(P1(struct mfs_strbuf)
		  PN(char **));

EXTERN int 
mfs_copyin_strbufpn(P1(struct mfs_strbufpn)
		    PN(char **));

EXTERN int 
mfs_copyout_strbuf(P1(struct mfs_strbuf)
		   PN(char *));

EXTERN int 
mfs_copyout_strbufpn(P1(struct mfs_strbufpn)
		     PN(char *));

EXTERN int
mfs_copyout_viewtag(P1(int)
		    PN(mfs_strbufpn_t)
		    PN(VNODE_T *));

EXTERN void 
mfs_svrdestroy(P1(struct mfs_svr *));

EXTERN LOCK_T mvfs_printf_lock;		/* lock for printing routines */
EXTERN LOCK_T mvfs_printstr_lock;       /* lock for temporary print string */

EXTERN int
mvfs_logfile_set(
    char *logfile,
    CALL_DATA_T *cd
);
EXTERN void
mvfs_logfile_close(P_NONE);

EXTERN int
mvfs_logfile_get(P1(char *pn)
		 PN(size_t *len));
EXTERN void
mvfs_logfile_putstr(P1(A_CONST char *str)
		    PN(u_int len)
		    PN(int nofileoffset));

EXTERN int
mvfs_snprintf(P1(char *str)
	      PN(int limit)
	      PN(A_CONST char *fmt)
	      PN(...));

EXTERN int
mvfs_vsnprintf(P1(char *str)
	       PN(int limit)
	       PN(A_CONST char *fmt)
	       PN(va_list ap));

EXTERN void
mvfs_logfile_printf(P1(A_CONST char *fmt)
		    PN(...));

EXTERN void
mvfs_logfile_vprintf_3(P1(int pri)            /* priority */
		       PN(A_CONST char *msg1) /* before formatted stuff */
		       PN(A_CONST char *msg2) /* after formatted stuff */
		       PN(A_CONST char *fmt)
		       PN(va_list ap));

EXTERN void mfs_mn_to_vattr(P1(struct mfs_mnode *mnp)
			    PN(VATTR_T *vap));

EXTERN char *
mvfs_oid_to_str(
    A_CONST tbs_oid_t *oid_p,
    tbs_oid_str_t str
);

EXTERN char *
mvfs_uuid_to_str(
    A_CONST tbs_uuid_t *uuid_p,
    tbs_uuid_str_t str
);

EXTERN int 
mvfs_splock_pool_init(
    splock_pool_t *pool,
    int size,
    SPLOCK_T *(*func)(splock_pool_t *, unsigned int),
    char *locknm
);

EXTERN void
mvfs_splock_pool_free(splock_pool_t *pool);

EXTERN int 
mvfs_lock_pool_init(
    mvfs_lock_pool_t *pool,
    int size,
    LOCK_T *(*func)(mvfs_lock_pool_t *, unsigned int),
    char *locknm
);

EXTERN void
mvfs_lock_pool_free(mvfs_lock_pool_t *pool);

#ifndef MVFS_SYSTEM_KMEM
/* mfs_kmem.c/KMEMDEBUG routines */
struct mvfs_slab_list;		/* forward decl */
struct mvfs_slab_list * 
mvfs_create_slablist(
    unsigned int size,
    short traced,
    char *lock_name
);
void mvfs_destroy_slablist(struct mvfs_slab_list *slist);
caddr_t 
mvfs_slab_getchunk(
    struct mvfs_slab_list *slistp,
    unsigned int size
 );
void 
mvfs_slab_freechunk(
    struct mvfs_slab_list *slistp,
    caddr_t elt,
    unsigned int size
);

EXTERN void mfs_kmem_init(P_NONE);
#ifdef KMEMDEBUG
EXTERN void *
mfs_kalloc(
    size_t size,
    int flag,
    caddr_t ra,
    caddr_t rara
);

EXTERN void
mfs_kfree(
    void *ptr,
    size_t size,
    caddr_t ra,
    caddr_t rara
);

EXTERN char *
mfs_kmstrdup(
    char *str,
    caddr_t ra,
    caddr_t rara
);

#endif

/*
 * Alloc ring buffer
 */

struct _mvfs_kmem_rbuffer {
    caddr_t	block;
    caddr_t	ra;
    caddr_t	ra2;
    size_t	size;
};

extern struct _mvfs_kmem_rbuffer mvfs_kmem_rbuffer[];
extern struct _mvfs_kmem_rbuffer *mvfs_kmem_rbuffer_curr;
extern struct _mvfs_kmem_rbuffer *mvfs_kmem_rbuffer_end;

#define MVFS_KMEM_ADDALLOC(_block, _ra, _rara, _size) { \
    if (mvfs_kmem_rbuffer_curr >= mvfs_kmem_rbuffer_end) \
        mvfs_kmem_rbuffer_curr = mvfs_kmem_rbuffer; \
    mvfs_kmem_rbuffer_curr->block = (_block); \
    mvfs_kmem_rbuffer_curr->ra = (_ra); \
    mvfs_kmem_rbuffer_curr->ra2 = (_rara); \
    mvfs_kmem_rbuffer_curr->size = (_size); \
    mvfs_kmem_rbuffer_curr++; \
}

#else
#define MVFS_KMEM_ADDALLOC(_block,_ra,_rara, _size)
#endif

EXTERN void mfs_prkmem(P_NONE);
EXTERN void mfs_kmem_unload(P_NONE);

EXTERN mvfs_thread_t *
mvfs_thread_cachealloc(P_NONE);
EXTERN void
mvfs_thread_cachefree(P1(mvfs_thread_t *));

EXTERN mvfs_proc_t *
mvfs_proc_cachealloc(P_NONE);
EXTERN void
mvfs_proc_cachefree(P1(mvfs_proc_t *));

EXTERN void 
mfs_uioset(P1(struct uio *uiop) 
	   PN(caddr_t addr)
	   PN(size_t size)
	   PN(MOFFSET_T offset)
	   PN(int segflg));


EXTERN int 
mfs_lookupvp(P1(VNODE_T *)
	     PN(mfs_pn_char_t *)
	     PN(int)
   	     PN(SYMFOLLOW_T)
	     PN(VNODE_T **)
	     PN(CLR_VNODE_T **)
	     PN(CRED_T *));
/* In mfs_vnodeops.c 
 */

EXTERN int 
mfs_owner(
    VNODE_T *vp,
    CALL_DATA_T *cd
);

EXTERN VTYPE_T
mfs_mn_vtype(P1(mfs_mnode_t *mnp));

/* Vop open */

EXTERN int
mfs_openv(
    VNODE_T **vpp,
    int mode,
    CALL_DATA_T *cd
);

EXTERN int
mfs_openv_subr(
    VNODE_T **vpp,
    int mode,
    CALL_DATA_T *cd,
    int do_vop
);
EXTERN int
mvfs_openv_ctx(
    VNODE_T **vpp,
    int mode,
    CALL_DATA_T *cd,
    int do_vop,
    MVFS_OPEN_CTX_T *ctxp
);

EXTERN int
mvfs_closev_ctx(
    VNODE_T *avp,
    int flag,
    MVFS_LASTCLOSE_T count,
    MOFFSET_T o,                   /* SVR4 only -- currently unused */
    CALL_DATA_T *cd,
    MVFS_CLOSE_CTX_T *ctxp
);
EXTERN int
mfs_create(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);
EXTERN int
mvfs_create_subr(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    int flag
);
EXTERN int
mvfs_create_subr_ctx(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    MVFS_CREATE_CTX_T *ctxp
);
EXTERN int
mvfs_create_subr_with_cvp(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    int flag,
    CLR_VNODE_T **cvpp,
    MVFS_CREATE_CTX_T *ctxp
);
EXTERN int
mfs_pre_closev(
    VNODE_T *vp,
    int flag,
    VNODE_T **bvpp,
    CALL_DATA_T *cd
);
EXTERN int
mfs_post_closev(
    VNODE_T *vp,
    int flag,
    MVFS_LASTCLOSE_T count,
    int ct_stat,
    CALL_DATA_T *cd
);
EXTERN int
mfs_closev(
    VNODE_T *avp,
    int flag,
    MVFS_LASTCLOSE_T count,
    MOFFSET_T o,                   /* SVR4 only -- currently unused */
    CALL_DATA_T *cd
);
EXTERN int
mvfs_pre_rdwr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    CALL_DATA_T *cd,
    CLR_VNODE_T **cvpp,
    ssize_t *ucp,
    MVFS_RDWR_CTX_T *ctxp
);

EXTERN int
mvfs_post_rdwr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    CALL_DATA_T *cd,
    CLR_VNODE_T *cvp,
    ssize_t uc,
    int error,
    MVFS_RDWR_CTX_T *ctxp
);

EXTERN int 
mfs_read(
    VNODE_T *vp,
    struct uio *uiop,
    int ioflag, 
    CALL_DATA_T *cd
);
EXTERN int 
mfs_write(
    VNODE_T *vp,
    struct uio *uiop,
    int ioflag, 
    CALL_DATA_T *cd
);
EXTERN int
mfs_rdwr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    CALL_DATA_T *cd
);
EXTERN int
mfs_rdwr_subr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd
);
EXTERN int
mvfs_rdwr_ctx(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd,
    MVFS_RDWR_CTX_T *ctxp
);
EXTERN int
mfs_ioctlv(
    VNODE_T *avp,
    int com,
    caddr_t data,
    int flag,
    CALL_DATA_T *cd,
    MVFS_IOCTL_RVALP_T rvalp                    /* NYI; currently unused */
);
EXTERN int
mvfs_ioctlv_subr(
    VNODE_T *avp,
    int com,
    caddr_t data,
    int flag,
    CALL_DATA_T *cd,
    MVFS_IOCTL_RVALP_T rvalp,                   /* NYI; currently unused */
    VOPBD_T *vopbdp,
    MVFS_CALLER_INFO *callinfo
);
EXTERN int
mvfs_ioctl_validate(mvfscmd_block_t *mcbp);

EXTERN int 
mfs_getattr(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag, 
    CALL_DATA_T *cd
);
EXTERN int
mvfs_changeattr(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag,
    CALL_DATA_T *cd,
    MVFS_CALLER_CONTEXT_T *ctxp
);
EXTERN int
mfs_accessv(
    VNODE_T *vp,
    int mode,
    int flag,
    CALL_DATA_T *cd
);
EXTERN int
mvfs_accessv_ctx(
    VNODE_T *vp,
    int mode,
    int flag,
    CALL_DATA_T *cd,
    MVFS_ACCESS_CTX_T *ctxp
);
EXTERN int
mfs_lookup(
    VNODE_T *advp,
    char *nm,
    VNODE_T **vpp,
    struct pathname *pnp,
    int flags,
    ROOTDIR_T *rdir,
    CALL_DATA_T *cd
);
EXTERN int
mvfs_lookup_ctx(
    VNODE_T *advp,
    char *nm,
    VNODE_T **vpp,
    struct pathname *pnp,
    int flags,
    ROOTDIR_T *rdir,
    CALL_DATA_T *cd,
    MVFS_LOOKUP_CTX_T *ctxp
);
EXTERN int 
mfs_link(
    VNODE_T *atdvp,
    VNODE_T *vp,
    char *tnm,
    CALL_DATA_T *cd
);
EXTERN int
mvfs_link_ctx(
    VNODE_T *atdvp,
    VNODE_T *vp,
    char *tnm,
    CALL_DATA_T *cd,
    MVFS_LINK_CTX_T *ctxp
);
EXTERN int 
mfs_rename(
    VNODE_T *aodvp,
    char *onm,
    VNODE_T *atdvp,
    char *tnm,
    CALL_DATA_T *cd
);
EXTERN int 
mvfs_rename_ctx(
    VNODE_T *aodvp,
    char *onm,
    VNODE_T *atdvp,
    char *tnm,
    CALL_DATA_T *cd,
    MVFS_RENAME_CTX_T *ctxp
);
EXTERN int 
mvfs_mkdir(
    VNODE_T *advp,
    char *nm,
    VATTR_T *va,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);
EXTERN int 
mvfs_mkdir_ctx(
    VNODE_T *advp,
    char *nm,
    VATTR_T *va,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    MVFS_MKDIR_CTX_T *ctxp
);
EXTERN int
mfs_rmdir(
    VNODE_T *advp,
    char *nm,
    VNODE_T *cdir,
    CALL_DATA_T *cd
);
EXTERN int
mvfs_rmdir_ctx(
    VNODE_T *advp,
    char *nm,
    VNODE_T *cdir,
    CALL_DATA_T *cd,
    MVFS_RMDIR_CTX_T *ctxp
);
EXTERN int 
mfs_readdir(
    VNODE_T *advp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    int *eofp
);
EXTERN int 
mvfs_readdir_ctx(
    VNODE_T *advp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    int *eofp,
    MVFS_READDIR_CTX_T *ctxp
);
EXTERN int 
mfs_readdirx(
    VNODE_T *advp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    view_readdir_flag_t flags,
    int *eofp
);
/* for now define it here */
#define MVOP_READDIRX(a, u, c, f, e) mfs_readdirx(a, u, c, f, e)

EXTERN int 
mfs_remove(
    VNODE_T *advp,
    char *nm,
    CALL_DATA_T *cd
);
EXTERN int 
mvfs_remove_ctx(
    VNODE_T *advp,
    VNODE_T *avp,
    char *nm,
    CALL_DATA_T *cd,
    MVFS_REMOVE_CTX_T *ctxp
);
EXTERN int 
mfs_symlink(
    VNODE_T *advp,
    char *lnm,
    VATTR_T *tva,
    char *tnm,
    CALL_DATA_T *cd
);
EXTERN int 
mvfs_symlink_ctx(
    VNODE_T *advp,
    char *lnm,
    VATTR_T *tva,
    char *tnm,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    MVFS_SYMLINK_CTX_T *ctxp
);
EXTERN int 
mfs_readlink(
    VNODE_T *avp,
    struct uio *uiop,
    CALL_DATA_T *cd
);
EXTERN int 
mfs_fsync(
    VNODE_T *vp,
    int flag,
    CALL_DATA_T *cd
);
EXTERN int
mvfs_fsync_ctx(
    VNODE_T *vp,
    int flag,
    CALL_DATA_T *cd,
    MVFS_FSYNC_CTX_T *ctxp
);
EXTERN int
mfs_lockctl(
    VNODE_T *vp,
    struct flock *ld,
    int cmd,
    CALL_DATA_T *cd,
    int clid
);
EXTERN int
mvfs_lockctl_ctx(
    VNODE_T *vp,
    void *ld,
    int cmd,
    CALL_DATA_T *cd,
    MVFS_LOCKCTL_CTX_T *ctxp
);
EXTERN int mfs_pathconf(P1(VNODE_T *)
			PN(int cmd)
			PN(MVFS_PATHCONF_VAL_T *valp)
			PN(CRED_T *));
EXTERN int mfs_vfid(P1(VNODE_T *)
		    PN(struct fid **fidpp));
EXTERN int 
mfs_putpage(VNODE_T *vp);
EXTERN int 
mfs_getpage(VNODE_T *vp);
EXTERN int 
mfs_map(VNODE_T *vp);
EXTERN int 
mfs_cmp(
    VNODE_T *vp,
    VNODE_T *vp2
);
EXTERN int 
mfs_realvp(
    VNODE_T *vp,
    VNODE_T **vpp
);

EXTERN void mfs_rwlock(P1(VNODE_T *vp)
		       PN(int w));
EXTERN void mfs_rwunlock(P1(VNODE_T *vp)
			 PN(int w));
EXTERN int mfs_seek(P1(VNODE_T *vp)
		    PN(MOFFSET_T ooff)
		    PN(MOFFSET_T *noffp));
EXTERN int
mvfs_seek_ctx(
    VNODE_T *vp,
    MOFFSET_T ooff,
    MOFFSET_T *noffp,
    MVFS_SEEK_CTX_T *ctxp
);
EXTERN int 
mfs_noerr(VNODE_T *vp);
EXTERN void 
mfs_noval(VNODE_T *vp);

EXTERN VTYPE_T mfs_mn_vtype(P1(mfs_mnode_t *));
EXTERN void
mfs_ac_modevents(
    register VNODE_T *vp,
    int flags,
    CRED_T *cred
);
EXTERN int mfs_noview_readdir(P1(VNODE_T *dvp)
			      PN(struct uio *uiop)
			      PN(CRED_T *cred));
EXTERN int mfs_noview_readdirx(P1(VNODE_T *dvp)
			      PN(struct uio *uiop)
			      PN(CRED_T *cred));
EXTERN void
mvfs_noview_vobrt_getattr(
    VNODE_T *vp,
    VATTR_T *vap
);

/****************************************************************************
 * mfs_syncvp
 * Routine to sync modified pages and attributes of an MVFS vnode
 * to the cleartext, and then sync the cleartext to dis.
 * IN   vp              Vnode ptr
 * IN   flag		Flush flags
 *			0 	      	flush modified pages (making them pure)
 *			MFS_PVN_ASYNC	don't have to wait for flush to complete
 *			MFS_PVN_INVAL	invalidate (toss) all pages for the
 *					object from the VM caches.
 * IN   cred            Unix credentials to use
 * RESULT:              Unix error
 */
EXTERN int mfs_syncvp(P1(VNODE_T *vp) PN (int flag) PN(CRED_T *cred));

extern V_OP_T mvfs_vnodeops;


/*
 * MFS vfs operations.  Must modify the names to not conflict
 * with the FSS operations of the same name.
 * In mfs_vfsops.c
 */

EXTERN VNODE_T *
mfs_getviewroot(P_NONE);	/* Get viewroot vnode */

EXTERN VNODE_T *
mfs_getspecdev(P_NONE);		/* Get .specdev vnode */

EXTERN void
mfs_findvfs_lock(P_NONE);

EXTERN void
mfs_findvfs_unlock(P_NONE);

EXTERN VFS_T *
mfs_findvfs_nm(P1(tbs_boolean_t case_insensitive) 
               PN(mfs_pn_char_t *nm));

EXTERN VFS_T *
mfs_findvfs_oid(P1(tbs_oid_t *oid)
		PN(int *indx)
		PN(tbs_boolean_t *unique));

EXTERN VFS_T *
mfs_findvfs_uuid(P1(tbs_uuid_t *uuiid));

EXTERN VFS_T *
mfs_findvfs_cookie(P1(u_long *cookie));

EXTERN VFS_T *
mfs_findvfs_mntpath(P1(mfs_pn_char_t *mntpath));

/*
 * These macros are used to set tunable values from
 * mma_mntargs->mma_sizes when the viewroot is mounted.
 *
 */
/* Is a size setting present in the structure? */
#define MVFS_SIZE_PRESENT(sz,bit) (((sz)->mask & MVFS_CACHEBIT(bit)) != 0)
/* Is a size setting in the structure, and not set to "use default"
   distinguished value? */
#define MVFS_SIZE_VALID(sz,bit) (MVFS_SIZE_PRESENT(sz,bit) &&           \
                                 (sz)->size[MVFS_SETCACHE_##bit] !=     \
                                 MVFS_SETCACHE_DEFAULT_VALUE)
/* Unconditionally load a size from a setting structure */
#define MVFS_SIZE_LOAD(var,sz,bit) (var) = (sz)->size[MVFS_SETCACHE_##bit]
/*
 * Load a size from a setting structure for a run-time cache tuning.
 * Very similar to common-code MVFS_SIZE_CONDLOAD(), but on all platforms
 */
#define MVFS_SIZE_RUNTIME_SET(var,sz,bit)       \
 if (MVFS_SIZE_VALID(sz,bit))                   \
     MVFS_SIZE_LOAD(var,sz,bit)

EXTERN int mfs_vmount_subr(P1(VFS_T *)
			   PN(VNODE_T *)
			   PN(mfs_pn_char_t *)
			   PN(int)
			   PN(caddr_t)
			   PN(size_t)
			   PN(CRED_T *)
			   PN(MVFS_CALLER_INFO *));
EXTERN int mfs_vunmount(P1(VFS_T *)
			PN(CRED_T *));
EXTERN int mfs_root(P1(VFS_T *)
		    PN(VNODE_T **));
EXTERN int
mfs_register_mount(
    caddr_t mnt_data,
    size_t mnt_datalen,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

EXTERN int
mfs_unregister_mount(P1(VFS_T *vfsp)
		     PN(CRED_T *cred));

EXTERN int
mfs_unregister_all_vobs(P1(CRED_T *cred));

EXTERN int mfs_vstatfs(P1(VFS_T *)
		       PN(STATVFS_T *));
EXTERN int mfs_vsync(P1(VFS_T *)
    		     PN(short)
    		     PN(CRED_T *));

EXTERN int
mfs_vget(
    VFS_T *vfsp,
    VNODE_T **vpp,
    struct fid *xfidp
);

EXTERN int
mvfs_vget_cd(
    VFS_T *vfsp,
    VNODE_T **vpp,
    struct fid *xfidp,
    CALL_DATA_T *cd
);
/* these are not prototyped so that they can be used as stand-in VOP functions
   without causing type errors. */
#ifdef __GNUC__ /* but we prototype to VOID to shut up GCC */
EXTERN int
mfs_nosys(void);
EXTERN int
mfs_swapvp(void);
#else
EXTERN int
mfs_nosys();
EXTERN int
mfs_swapvp();
#endif

extern VFSOPS_T mvfs_vfsops;

EXTERN u_long mfs_uuid_to_hash32(P1(A_CONST tbs_uuid_t *uuid_p));

#ifdef MVFS_KMEMTRACE
#define MVFS_TRACEBUFFER_SIZE 10000
EXTERN VOID
mvfs_trace(P1(int data1)
            PN(int data2)
            PN(int data3)
            PN(int data4));
#endif /* MVFS_KMEMTRACE */

extern void
mvfs_credutl_unix_uid_to_sid(
    CRED_UID_T uid,
    credutl_sid_t *csidp
);

extern void
mvfs_credutl_unix_gid_to_sid(
    CRED_GID_T gid,
    credutl_sid_t *csidp
);

EXTERN int
mvfs_ensure_power2(
    int n
);
extern void
mvfs_clnt_support_lfs(
    struct mfs_svr *svr,
    VNODE_T *vw,
    CRED_T *cred
);

extern int
mvfs_stats_data_zero(mvfs_stats_data_t *sdp);

extern void
mvfs_pview_stat_zero(struct mvfs_pvstat *pvp);

extern mvfs_stats_data_t *
mvfs_stats_data_per_cpu_init(void);

/*
 * Macro to zero out the statistics structure.  To avoid duplication of the zero
 * out code in different places, another macro - MVFS_STAT_ZERO is used for the common
 * code.  For cases where we need to set the viewophist back to the initial viewop
 * histogram values we started with, we call this macro.  For the rest, MVFS_STAT_ZERO
 * macro is called directly.  MVFS_STAT_ZERO macro is defined in the mdep header files
 * where needed and for the rest of the platforms, it is defined in mvfs_systm.h
 */
#define MVFS_PERCPU_STAT_ZERO(sdp) \
        MVFS_STAT_ZERO(sdp) \
        sdp->mfs_viewophist = mvfs_init_viewophist; \
        mvfs_cpu_beyond_limit = FALSE;

/*
 * Macro to zero out the per-view statistics.  The per-view stat lock is taken
 * before zeroing out the structure.  The pointer to the per-view stat struct is
 * read into a local variable to avoid having to dereference the pointer after
 * taking the spin lock.
 */
#define MVFS_PVSTAT_ZERO(vw) { \
        SPL_T s; \
        struct mvfs_pvstat *pvp = VTOM(vw)->mn_view.pvstat; \
        MVFS_PVSTATLOCK_LOCK(s, pvp); \
        mvfs_pview_stat_zero(pvp); \
        MVFS_PVSTATLOCK_UNLOCK(s, pvp); \
}

/*
 * How We Keep Statistics
 * ----------------------
 * Warning: these two macros, MVFS_STAT_MEMALLOC1 and MVFS_STAT_MEMALLOC2, are
 * a matched pair and must only be used together, in that order.  Further, they
 * are only used below in the macros that keep statistics and shouldn't be used
 * anywhere else.  What follows is the background for understanding the macros.
 *
 * Data Structures
 * ---------------
 * To avoid locking, or possibly expensive atomic operations, on multi-cpu
 * machines, we are keeping statistics in per-cpu data structures.  The general
 * data structure assumed by these macros is an array of pointers to
 * mvfs_stats_data_t structures, indexed by a cpu identifier.  We assume that
 * we can find the maximum number of cpus for a platform at boot time (either
 * from an OS query or from a constant built into our code).  We store that
 * value in mvfs_max_cpus and allocate the array of pointers to be that size at
 * MVFS initialization time.  We further assume that we can get a cpu
 * identifier for the cpu we are currently running on and that:
 *
 *   0 < cpuid < mvfs_max_cpus - 1
 *
 * so it can serve as the array index.
 *
 * Saving Space
 * ------------
 * In order not to waste space by allocating a stat structure for all
 * mvfs_max_cpus (e.g. some platform may support more, possibly many more, cpus
 * than are actually available or could be added during this boot), we allocate
 * the stat structure for a cpu the first time we are running on that cpu and
 * need to keep statistics.  Note, analysis has shown that most of the stats
 * structure space is taken up by the view op counts, timings, and histogram
 * (i.e. roughly 15KB out of 17KB total in a 64-bit kernel).  Thus, if space
 * becomes a problem, it might make sense to split those out of the per-cpu
 * stats structure and keep them separately under a global lock.
 *
 * Data Integrity
 * --------------
 * To preserve the data integrity of the statistics, we still need to guarantee
 * that the actual statistics arithmetic (e.g. increment) happens "atomically".
 * We do this by disabling pre-emption (or all interrupts, if the platform
 * requires it) while managing the statistics for the cpu we are running on.
 * The idea is that disabling pre-emption on a cpu is "cheaper" than aquiring a
 * global statistics lock, or even than using atomic instructions (since those
 * may cause bus stalls and cache flushes).  We assume that the platform
 * provides a way to guarantee that we can run uninterruptibly on our current
 * cpu by using the MVFS_INTR_DISABLE() and MVFS_INTR_ENABLE() macros.
 *
 * Returning and Resetting Statistics
 * ----------------------------------
 * To return statistics, we now have to add up the counters from each per-cpu
 * stats structure.  This is done in mvfs_ioctl.c.  Note, this code only reads
 * the per-cpu stats structure pointers from the global array (to get the stats
 * for each cpu to add into its running total), it never sets them.  The
 * pointer for a particular cpu is only set by code running on that cpu while
 * pre-emption is disabled, and we never set the pointer for a particular CPU
 * to NULL, thus avoiding any races (the structures are all freed when the MVFS
 * is unloaded).
 *
 * To reset the statistics to zero, we have a problem since we have to be
 * running on a particular cpu in order to operate on its stats structure, and
 * the OS doesn't generally provide a way to run a particular routine (to zero
 * out the stats structure) on a particular cpu.  We solve this by using the
 * zero_me flag in the stats structure, which is only set (for all cpus) by the
 * routine that zeroes stats (running on any cpu), and is only reset by the
 * macro code while running disabled on a particular cpu (after zeroing the
 * stats for that cpu).  We're assuming that setting, resetting, or testing the
 * flag is "atomic" since it is just an int.  Further, when the statistics are
 * being added up, if the zero_me flag is set, we just ignore the statistics
 * for that cpu.  Then, the next time we run on that cpu we will actually zero
 * the stats and reset the zero_me flag so its stats will again be added to the
 * total.
 *
 * Little Details
 * --------------
 * We enclose the guts of the macro in a while statement so we can avoid goto
 * statements (which don't work too well in a macro) by using break and
 * continue statements.
 *
 * The variables used by the macro are not needed anywhere else, and they must
 * be initialized very carefully, so we enclose the macro in its own block so
 * we can declare and initialize the variables in one place.
 *
 * The MDKI_STAT_GET_DATAP macro is expected to be usable as a left-hand side
 * or right-hand side expression since we need to read it and write to it.  It
 * could be platform dependent, but generally just uses the cpu id as an index
 * into the array of pointers described above to get a pointer to the per-cpu
 * stats structure (that can be read from or stored through).
 *
 * Operating on the Per-Cpu Statistics
 * -----------------------------------
 * After the code in the first macro (which ends in the middle of an if
 * statement in the middle of a while statement), we have guaranteed that sdp
 * is not NULL and is pointing to the stats structure for this cpu and that
 * pre-emption is disabled so we are still running on this cpu.  Thus, at that
 * point we can insert code that uses sdp to manipulate the statistics in
 * "arbitrary" ways, as long as it doesn't take "too long" (since we're running
 * with pre-emption disabled).  For instance, at this point we have already run
 * code to zero all the statistics for this cpu if that was necessary.  The
 * second macro just finishes things up.
 *
 * "Fast Path" Analysis
 * --------------------
 * The normal case is that the per-cpu stat structure has already been
 * allocated, we're running on a cpu whose cpuid is less than mvfs_max_cpus,
 * and the zero_me flag is not set.  In this case we do the following:
 *
 *  - enter the block (we hope the compiler optimizes this away)
 *  - set new_sdp to NULL
 *  - enter the while loop (should be a no-op)
 *  - disable pre-emption (platform dependent, keeps us on this cpu)
 *  - get our current cpu id (platform dependent)
 *  - test that the cpuid is less than mvfs_max_cpus
 *  - get the current cpu's stats pointer (some pointer arithmetic)
 *  - test pointer for NULL
 *  - test the zero_me flag for this cpu
 *  - perform the per-cpu stat operation (e.g. increment/decrement a counter)
 *  - enable pre-emption (we can lose the cpu after this)
 *  - test new_sdp for NULL
 *  - break out of the while loop
 *  - exit the block (we hope this is a no-op)
 *
 * This is roughly 3 assignments (some with pointer arithmetic), 4 tests, the
 * disable/enable overhead, and the actual statistics operation (mostly an
 * increment or decrement).  The disable and enable could be subroutine calls,
 * depending on the platform, as could getting the current cpu id.
 *
 * Other Paths: Structure Allocation, Errors, Etc.
 * -----------------------------------------------
 * The first time we're running on a cpu and want to operate on its stats
 * structure, we have to allocate the structure and add it to the array of
 * pointers at the index for this cpu.  We recognize this case when we test the
 * current cpu's stats pointer and find it to be NULL.  The first time through
 * we will also find new_sdp to be NULL, meaning we haven't allocated any space
 * yet.  At this point, in order to allocate space, we have to enable
 * pre-emption because the OS may need to do extensive work to allocate the
 * space we've requested.  We then call a subroutine to allocate and initialize
 * a per-cpu stats structure (remember, we're no longer in the fast path, so we
 * can take our time).  If we can't get the space (new_sdp == NULL) we can't
 * keep any statistics at this time, so we just break out of the while loop
 * (which ends in the second macro).  We're already enabled and have nothing
 * allocated so we can just continue on and hope we'll be able to allocate some
 * space the next time.  If we allocated the structure (the normal case), we
 * continue in the while loop, which starts back at the beginning.  This time,
 * we proceed into the body of the while loop, disable pre-emption, and try to
 * get the per-cpu stats structure pointer again.  Since we have been enabled
 * since the last time we checked, some other process may have run on this cpu,
 * or we may be running on a different cpu, so the pointer might now be filled
 * in.  If it is, we proceed as if it had been filled in the first time, but
 * now we have new_sdp pointing to some allocated storage that we're not going
 * to use.  This is cleaned up in the second macro after we have enabled again.
 * If the per-cpu stats structure pointer is still NULL, we check new_sdp and
 * this time we find it is non-NULL.  We are still disabled, so we set the
 * pointer for our cpu id in the global array (and set sdp), and NULL new_sdp
 * because we've used the allocated storage it pointed to.  We then continue on
 * as before with sdp set and pre-emption disabled.
 *
 * Another "slow case" occurs if the zero_me flag is set.  In that case, while
 * still disabled, we zero out the stats for this cpu and reset the zero_me
 * flag, which will cause the code that returns stats to start adding in the
 * statistics for this cpu again.  We assume the zeroing is "fast enough" so
 * that it is all right to do while we are disabled.  This is another case
 * where the view op statistics will take most of the time to zero, so this
 * could be another argument for separating them out (as mentioned in "Saving
 * Space" above).
 *
 * The final error case occurs when the cpu id for the current cpu is larger
 * than mvfs_max_cpus.  This indicates some error in the code, e.g. we built in
 * a constant that was too small, or the OS returned us a number that was too
 * small.  It won't be fixed without a patch (if we picked a too small
 * constant), or maybe by rebooting the machine or reloading the MVFS (in case
 * the OS would give us a better number).  This error is in the else clause so
 * it has "skipped around" the stats operation code, but we are still disabled.
 * We will log a message at this point.  We keep a global variable so we only
 * log the message once.  We also use this global variable in the code that
 * returns statistics to return/print an error so that the caller is aware that
 * there is a problem (in case the log message is missed).
 *
 * As noted above, the final job of the second macro is to enable pre-emption
 * and then check new_sdp to see if we need to free space we allocated but
 * didn't use.
 */
#define MVFS_STAT_MEMALLOC1 { \
    MVFS_SAVE_PRIORITY_T orig_intr_level; \
    int cur_cpuid; \
    mvfs_stats_data_t *sdp; \
    mvfs_stats_data_t *new_sdp = NULL; \
    while (TRUE) { \
        MVFS_INTR_DISABLE(orig_intr_level); \
        cur_cpuid = MVFS_GET_CUR_CPUID; \
        if (cur_cpuid < mvfs_max_cpus) { \
            sdp = MDKI_STATS_GET_DATAP(cur_cpuid); \
            if (sdp == NULL) { \
                if (new_sdp == NULL) { \
                    MVFS_INTR_ENABLE(orig_intr_level); \
                    new_sdp = mvfs_stats_data_per_cpu_init(); \
                    if (new_sdp == NULL) { \
                        break; \
                    } \
                    continue; \
                } else { \
                    sdp =  MDKI_STATS_GET_DATAP(cur_cpuid) = new_sdp; \
                    new_sdp = NULL; \
                } \
            } \
            if (sdp->zero_me) { \
                MVFS_PERCPU_STAT_ZERO(sdp); \
                sdp->zero_me = FALSE; \
            }

#define MVFS_STAT_MEMALLOC2 \
        } else { \
            MVFS_INTR_ENABLE(orig_intr_level); \
            if (new_sdp != NULL) { \
                KMEM_FREE(new_sdp, (sizeof(mvfs_stats_data_t))); \
            } \
            if (!mvfs_cpu_beyond_limit) { \
                mvfs_log(MFS_LOG_ERR, \
                 "MVFS_STAT_MEMALLOC2: current cpuid=%d > mvfs_max_cpus-1=%d\n", \
                  cur_cpuid, mvfs_max_cpus - 1); \
                mvfs_cpu_beyond_limit = TRUE; /* Only log once */ \
            } \
            break; \
        } \
        MVFS_INTR_ENABLE(orig_intr_level); \
        if (new_sdp != NULL) { \
            KMEM_FREE(new_sdp, (sizeof(mvfs_stats_data_t))); \
        } \
        break; \
    } \
}

/*
 * The following macros, SETSTAT_MAX_DELAY, SETPVSTAT_MAX_DELAY and SET_MAXDELAY
 * are used in mvfs_rpcutl.c
 */ 
/* Macro to update the max delay stats. */
#define SETSTAT_MAX_DELAY(secs) \
        MVFS_STAT_MEMALLOC1 \
        sdp->mfs_clntstat.mfsmaxdelay++; \
        if ((secs) > sdp->mfs_clntstat.mfsmaxdelaytime) { \
            sdp->mfs_clntstat.mfsmaxdelaytime = (secs); \
        } \
        MVFS_STAT_MEMALLOC2

/* Macro to increment the perview maxdelay counts.  */
#define SETPVSTAT_MAX_DELAY(view, secs) { \
        MVFS_SAVE_PRIORITY_T spl; \
        struct mvfs_pvstat *pvp = VTOM(view)->mn_view.pvstat; \
        MVFS_PVSTATLOCK_LOCK(spl, pvp); \
        (pvp->clntstat.mfsmaxdelay)++; \
        if ((secs) > (pvp->clntstat.mfsmaxdelaytime)) { \
            (pvp->clntstat.mfsmaxdelaytime) = (secs); \
        } \
        MVFS_PVSTATLOCK_UNLOCK(spl, pvp); \
}

/* Macro to update the cleartext or rpc max delay counts. */
#define SET_MAXDELAY(secs, nsecs, field) \
    MVFS_STAT_MEMALLOC1 \
    { \
	int i; \
        for (i = 0; i < MFS_NUM_HISTX; i++) { \
	    if (((secs) <= sdp->mfs_viewophist.histval[i].tv_sec) && \
                ((nsecs) <= sdp->mfs_viewophist.histval[i].tv_nsec)) \
            { \
                (sdp->mfs_viewophist.field[i])++; \
                (sdp->mfs_viewophist.histperop[op][i])++; \
                break; \
            } \
        } \
    } \
    MVFS_STAT_MEMALLOC2

/*
 * For per-view statistics gathering, we lock the mvfs_pvstatlock.  Each view has
 * it's own statlock which is initiliazed during the initiliazation of the view
 * stats in the init code for viewclas and ntviewclas mnodes.  The lock is freed
 * when those mnodes are destroyed.  For proper usage of this lock, check the
 * comment above the pvstat structure declaration.
 */
#define MVFS_PVSTATLOCK_INIT(lock)      \
        INITSPLOCK(lock, "mvfs_pvstat_spl")

#define MVFS_PVSTATLOCK_FREE(lock)      \
        FREESPLOCK(lock)

#define MVFS_PVSTATLOCK_LOCK(spl, pvp) \
        SPLOCK(pvp->mvfs_pvstatlock, spl);

#define MVFS_PVSTATLOCK_UNLOCK(spl, pvp) \
        SPUNLOCK(pvp->mvfs_pvstatlock, spl);

/* Macros increment/decrement statistics.  */

/* 
 * BUMPSTAT and BUMPSTAT_VAL macros almost do the same thing except that
 * BUMPSTAT increments the stat value by 1 and BUMPSTAT_VAL increments
 * the value by a number passed in as the second arg.  To avoid having
 * identical macros, we define this _BUMPSTAT_VAL and use it from both
 * the increment macros to do the appropriate thing.
 */

#define _BUMPSTAT_VAL(nm, inc) \
        MVFS_STAT_MEMALLOC1 \
        (sdp->nm) += inc;   \
        MVFS_STAT_MEMALLOC2

#define BUMPSTAT(nm) _BUMPSTAT_VAL(nm, 1)

#define BUMPSTAT_VAL(nm, inc) _BUMPSTAT_VAL(nm, inc)

#define UPDATE_STAT_MAX(target, value) (target = value)

#define DECSTAT(nm) _BUMPSTAT_VAL(nm, -1)

/*
 * Macros to bump per view statistics.  Takes particular stat offset in view
 * mnode.  Covers per view stats with the mvfs_pvstatlock.  About using the
 * per-view stat lock appropriately, check out the comment above the pvstat
 * structure declaration.
 */
#define _BUMP_PVSTAT_VAL(view, nm, val) { \
        SPL_T _s; \
        struct  mvfs_pvstat *pvp = VTOM(view)->mn_view.pvstat; \
        MVFS_PVSTATLOCK_LOCK(_s, pvp); \
        (pvp->nm) += val; \
        MVFS_PVSTATLOCK_UNLOCK(_s, pvp); \
    }

#define BUMP_PVSTAT(view, nm) _BUMP_PVSTAT_VAL(view, nm, 1)

#define BUMP_PVSTAT_VAL(view, nm, val) _BUMP_PVSTAT_VAL(view, nm, val)

#define BUMP_PVSTAT_LOCKED(pvp, nm) { \
        (pvp->nm)++; \
    }

/*
 * Also for per view stats, but takes vnode.
 */

#define BUMPVSTAT(vnode, stat) \
    if (MFS_VIEW(vnode)) { \
       BUMP_PVSTAT(MFS_VIEW(vnode), stat); \
    }

/*
 * Macro as above to bump per view statistics except that it takes an mnode
 * pointer as an argument.  It will follow the viewvp pointer if it is set
 * and bump the statistics there without validating that it points to a view.
 */
#define BUMPVSTATM(mnode, stat) \
    if (mnode->mn_hdr.viewvp) { \
        BUMP_PVSTAT(mnode->mn_hdr.viewvp, stat);  \
    }

/*
 * Yet another variation on bumping view statistics.  Here we know that we
 * have a pointer to a view vnode.  We just go ahead and bump the counter.
 * This just saves some typing.
 */
#define BUMPVSTATV(vnode, stat) \
        BUMP_PVSTAT(vnode, stat); \
/*
 * Macro to accumulate real-time used statistics
 */
#define MVFS_BUMPTIME(stime, dtime, nm) \
        MVFS_STAT_MEMALLOC1 \
        mvfs_bumptime(&(stime), &(dtime), (&(sdp->nm))); \
        MVFS_STAT_MEMALLOC2

/*
 * Macro to calculate elapsed time, but without adding to cumulative stats
 */
#define MVFS_TIME_DELTA(stime, dtime, ztime) \
        (ztime).tv_sec = (ztime).tv_nsec = 0; \
        mvfs_bumptime(&(stime), &(dtime), &(ztime));

/*
 * Macro to get the maximum offset for a vnode
 */

/*
 * RATLC01031475: downrev view support - when 7.1 clients talk to 6.0
 * views, need to be restrict client to behaving as if largefiles not supported.
 * RATLC01069062: only need to check for objects with specific view vp, does   
 * not apply for specdev, loopback. viewroot, vob root directories
 */
#define MVFS_GET_MAXOFF(vp) (MFS_ISVOB(VTOM(vp)) ? \
  ((VTOM(MFS_VIEW(vp)))->mn_view.downrev_view ? \
      (MVFS_MAXOFF_32_T) : (MVFS_MAXOFF_T)) \
  : (MVFS_MAXOFF_T))

/*
 * Timeout Values for View Server GETPROP Call
 */
#define MVFSGETPROP_TIMEO_DEFAULT  50
#define MVFSGETPROP_RETRANS_DEFAULT 1

/* Internal flag of MVFS initialization state */
#define MVFS_NOT_INITIALIZED    0
#define MVFS_INIT_PHASE1        1
#define MVFS_INIT_COMPLETE      10

EXTERN int mvfs_init_state;
EXTERN int mvfs_max_cpus;

/* Flag to indicate if we encounter a cpuid greater than mvfs_max_cpus */
EXTERN tbs_boolean_t mvfs_cpu_beyond_limit;

#ifdef MVFS_NEEDS_UNLOAD_SYNC
extern LOCK_T mfs_unload_lock;
extern LOCK_T mvfs_unloading_lock; /* lock to protect unload_in_progress flag */
/* Flag to indicate that unload is in progress */
EXTERN tbs_boolean_t mvfs_unload_in_progress;
#endif

#endif /* MVFS_BASE_H_ */
/* $Id: fc099f79.64cc11e2.880d.00:01:83:0d:bf:e7 $ */
