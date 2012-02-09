/* * (C) Copyright IBM Corporation 1991, 2008. */
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

#ifndef MVFS_SYSTM_H_
#define MVFS_SYSTM_H_
/*
 * Description
 *	This header file is a wrapper header file that gets all
 *	the correct machine dependent hdr included.
 */

/* Following is included for prototype macros */
#include <ks_base.h>

#include "mvfs_mdep_linux.h"

/* For MVFS_DEBUG, allow ASSERTs to be defined */
#ifndef MVFS_DEBUG
#ifndef MVFS_INHOUSE_NONPRODUCTION_ASSERTS
#undef ASSERT
#define ASSERT(X) 
#endif
#endif

	 
/*
 * defaults for things that the mdep files didn't care to define:
 */

/*
 * PVN_TRUNC() must invalidate any cached pages beyond "newsize".
 */
#define	PVN_TRUNC(vp,flag,newsize,oldsize,cred) 0

#define MVFS_IS_LASTCLOSE(cnt)	(cnt == MVFS_LASTCLOSE_COUNT)
#define MVFS_KEEPHANDLE		0

/* VFS_T to MVFS Mount Information */
#define VFS_TO_MMI(vfsp) ((struct mfs_mntinfo *)((vfsp)->vfs_data))

/* Macro to check vnode for being an MFS vnode */

#define MVFS_VFS_NOSUID VFS_NOSUID

#define MVFS_PN_SEP_CHAR	'/'

#define PN_IS_SEPCHAR(c)        ((c) == '/')

/*
 * Default File Locking id type
 */

#define FLID_T int

/* 
 * Determines from pathname struct if lookup case-insensitive
 */
#define MVFS_PN_CI_LOOKUP(pnp)  FALSE

/* the type of the vfsp->v_data ptr */
#define MVFS_VFSDATA_T caddr_t

/* Define macro to set Vnode ops pointer */
#define MVFS_SET_VNOP(vp, vopp)	((vp)->v_op = (vopp))

/* Define VFS for loopclas mnodes.  We default to the viewroot's VFS */
#define MVFS_LOOPCLAS_VFSP(vrdp,vp,always_cover) ((vrdp)->mfs_viewroot_vfsp)

/* Default is to use provided vfs */
#define MVFS_ALLOC_LOOPCLAS_VFS(vrdp, vfsp)	(vfsp)

/* No allocation, no freeing */
#define MVFS_FREE_LOOPCLAS_VFS(vrdp, vfsp)

/* If we have a non-default vfs allocator, we might have a non-default way of
** comparing two filesystems to tell if they are the same.
** So, define this macro to do the comparison.
*/
#define MVFS_IS_SAME_VFS(vp1, vp2) ((vp1)->v_vfsp == (vp2)->v_vfsp)

/*
 * Override in your mdep if your port has a different interfaces 
 */
#define MVFS_VN_INIT(vp, vfsp, type, dev) VN_INIT(vp, vfsp, type, dev)

#define MVFS_VN_ALLOC()	(VNODE_T *)((caddr_t)mnp-VSIZE)

/* mark vnode as complete */
#define MVFS_VN_EXISTS(vp)

#define MVFS_CREATE_VFSOPS(type)

#define MVFS_CREATE_VNOPS()

#ifndef MVFS_CTXT_VN_DUP
#define MVFS_CTXT_VN_DUP(svp, cvp)  
#endif

#ifndef MVFS_CTXT_VN_DETACH
#define MVFS_CTXT_VN_DETACH(cvp)  
#endif

/* Default macro to set the VROOT flag in v_flag.  Needed because
 * Linux port needs to put this flag in the mnode header and we need
 * to be able to cast the pointers properly.
 */
#define MVFS_SET_VROOT(vp)	(vp)->v_flag |= VROOT

#define MVFS_ESTABLISH_FORK_HANDLER()	TBS_ST_OK

#define MVFS_PROCINHERIT_FROM(procp)	mvfs_procinherit_from(procp)
#define MVFS_GENERIC_PROCINHERIT_FROM

#define MVFS_PROCINHERIT_COPY(to, from)	mvfs_procinherit_copy(to, from)
#define MVFS_GENERIC_PROCINHERIT_COPY

#ifndef MVFS_MDEP_EXIT_FS
#define MVFS_MDEP_EXIT_FS(thr)
#endif

/*
 * Unless otherwise declared in the port mdep.h file, everybody can
 * wait.  Define MDKI_MYTHREAD_CANTWAIT() as a predicate deciding
 * whether you can wait in paging contexts (TRUE means the thread can
 * wait).  Define MDKI_NUM_CANTWAIT as the number of threads in the
 * system that must not wait in paging code.
 */
#define MDKI_MYTHREAD_CANTWAIT()	0
#define MDKI_NUM_CANTWAIT		0

/*
 *  EACH PORT is responsible for providing these macros/functions/values:
 *
 *
 *  MVFS_THREADID_T:			type for threadid (includes process
 *					id for stale comparisons)
 *
 *  MDKI_THREADHASH(threadid, _mcdp):	hash function for
 *					MVFS_THREADID_T -> hash bucket index
 *					returns unsigned int
 *
 *  MVFS_THREADHASH_SZ:			number of buckets for thread hash table
 *
 *  MDKI_MYTHREADID(&threadid):		function to fill in current threadid
 *
 *  MDKI_THREADID_EQ(&t1, &t2):		equality predicate
 *
 *
 *  MVFS_PROCESS_T:			type for system-maintained process
 *					struct
 *  MDKI_CURPROC():			returns MVFS_PROCESS_T for current
 *					thread's process, UNLOCKED.
 *  MDKI_CURPID():			returns current thread's process's id
 *
 *			(these all take MVFS_PROCESS_T ptrs:)
 *  MDKI_PRISACTIVE(proc):		predicate whether system proc is active
 *  MDKI_PARENT_PROC(proc):		return locked ptr to parent proc
 *  MDKI_PRUNLOCK(proc):		unlock system proc
 *  MDKI_PRPID(proc):			returns PID of process (for debugging
 *					prints only)
 *
 *  MVFS_PROCID_T:			type for process id; must be suitable
 *					for hashing with all its bits
 *
 *  MVFS_PROCTAG_T:			type for process tag; used to detect
 *					stale process state
 *
 *  MVFS_PROCVALID(&procp):		boolean predicate whether state in
 *					mvfs_proc_t *procp is for an active
 *					proc
 *
 *  MDKI_MYPROCID(&procid):		fill in current procid
 *  MDKI_MYPROCTAG(&proctag, &procid):	fill in current proc tag
 *
 *  MDKI_PROCID(&procid, procp):      fills in procid for given MVFS_PROCESS_T
 *  MDKI_PROCTAG(&proctag, procp):    fills in proctag for given MVFS_PROCESS_T
 *
 *  MDKI_PROCID_EQ(&p1, &p2):		equality predicate
 *  MDKI_PROCTAG_EQ(&t1, &t2):		equality predicate
 *
 *  MDKI_PROC_EQ(mprocp, &procid, &proctag):
 *                                      equality predicate of mvfs_process_t
 *                                      vs. whichever of procid/proctag
 *                                      makes sense for the port.
 *                                      (defaulted in this file to compare
 *                                      procid via MDKI_PROCID_EQ)
 *
 *  The base MVFS will provide:
 *       A hash function & hash table stuff for procid -> mvfs process state
 *       A hash table for threadid -> mvfs thread state
 *	 periodic scanning of process state hash table to clean out stale procs
 *
 * See mfs_procops.c for detailed comments on process/thread state stuff
 */

#ifndef MDKI_MYPROCTAG
#define MDKI_MYPROCTAG(tagp, pidp) *(tagp) = *(pidp)
#endif

/*
 * Credential macros used by the MVFS core.  These can be overridden
 * in the port specific mdep file.  The MVFS core MUST not use
 * MDKI_GET_UCRED() or MDKI_DUP_UCRED() macros as they do not exist
 * on it.
 */
#define MVFS_STORE_VIEW_CREDS(dcp, scp, errtag)

#define MVFS_FREE_VIEW_CREDS(cp) \
    { \
	MDKI_CRFREE(cp); \
	cp = NULL; \
    }

/*
 * Get current credentials (based on object view and current credentials)
 * The credentials are returned WITH NO ADDITIONAL REFERENCE COUNT
 */
#define MVFS_VIEW_CREDS(vp, cred)	(cred)

#define MVFS_DUP_VIEW_CREDS(vp, cred) 	MDKI_CRDUP(cred)

#define MVFS_DUP_DEFAULT_CREDS() 	MDKI_DUP_UCRED()

#define MVFS_REGISTER_VDMINFO(vp) 0

#define MVFS_FLUSH_MAPPINGS(vfsp)	/**/

#define MVFS_FLUSH_MAPPINGS_VW(vw)	/**/

#define MVFS_REGISTER_SIDHOST_CREDMAPS(cptr) 0

#define MVFS_UNREGISTER_SIDHOST_CREDMAPS(cptr) 0

#define MVFS_REGISTER_GRPLIST_ORDER(gptr) 0

#define MVFS_UNREGISTER_GRPLIST_ORDER(luid) 0

/*
 * Default variants for macros that synchronize modifications
 * of the cleartext vnode with paging.  Only defined in those
 * ports that have separate paging locks.
 */
#define MVFS_PVN_INH_PAGING(vp)     /* Nothing to do */
#define MVFS_PVN_ENB_PAGING(vp)     /* Nothing to do */

/*
 * Default variant for MVFS cleartext vnode copy routine used
 * for COW.
 */
#define MVFS_COPYVP(ovp,nvp,len,cred)	mfs_copyvp(ovp,nvp,len,cred)

#define MVFS_ADJUST_CACHESIZES mvfs_physmem_adj_caches

/* Default is to MDKI_GET_U_RDIR and MDKI_SET_U_RDIR  */

#define MDKI_GET_PROC_RDIR()	MDKI_GET_U_RDIR()

/* Default for PVN_FLUSHTEXT			 	*/
#define PVN_FLUSHTEXT(vp)

#define FLOCK_T		struct flock

/*
 * Default is that all files are audited.
 * 
 * Currently only NT defines its own routine so that when files are
 * renamed in the process of being deleted to the ".mvfsXXXX" 
 * files they will not be picked up by FindNextFile() and put 
 * into (and subsequently causing confusion) the audit file.
 * Other (UNIX) platforms do not encounter problems as the "." 
 * prefix effectively hides them from normal directory lookups.
 */
#define MVFS_SKIP_AUDIT(vp) (FALSE)

#define MVFS_THREAD_LOOKUP_ROOT(th)   NULL
#define MVFS_ROOT_LOOKUP(root,nm,vpp,pnp,flags,cred,ctx) 0

#define MVFS_LOGFILE_REINIT()

#define MVFS_LOGBUFFER_FLUSH()

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64) 
#define MVFS_VALIDATE_IOCTL(data,callinfo) mvfs_ioctl_validate_64bitos(data, callinfo)
#else
#define MVFS_VALIDATE_IOCTL(data,callinfo) mvfs_ioctl_validate(data)
#endif
#ifndef MVFS_IS_IT_OUR_IOCTL
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
#define MVFS_IS_IT_OUR_IOCTL(cmd,callinfo) mvfs_ioctl_chk_cmd(cmd,callinfo)
#else
#define MVFS_IS_IT_OUR_IOCTL(cmd,callinfo) (cmd == MVFS_IOCTL_CMD)
#endif
#endif

#if defined(ATRIA_LP64) || defined (ATRIA_LLP64)
/* LP64 kernel:  platforms which support both 32-bit and 64-bit applications
 * must define this macro to test the system call environment, if we need
 * to translate structs on the way in and/or out.  See mvfs_copy.c and
 * mvfs_transtype.c
 */
#ifndef MDKI_CALLER_IS_32BIT
#define MDKI_CALLER_IS_32BIT(x) (TRUE)
#endif
#else
#ifndef MDKI_CALLER_IS_32BIT
#define MDKI_CALLER_IS_32BIT(x) (FALSE)
#endif
#endif /* ATRIA_LP64 */

/* The following is for alignment within the mnode.  See note in mvfs.h */
#define MVFS_MNODE_ALIGNMENT 8

/*
 * likewise: for systems that allocate vnode+mnode, we need VSIZE
 * computed to be a multiple of the largest alignment
 */
#define MVFS_VNODE_ALIGNMENT 8

/* The default number of bits available for file sizes in MVFS is 64.
 * The MAXOFF correlates to the FILESIZE_BITS
 */

#define MVFS_FILESIZE_BITS_32 32

#define MVFS_FILESIZE_BITS 64

#define MVFS_MAXOFF_32_T 0x7fffffff

#define MVFS_MAX_DIRSIZE 0x7fffffff

/* The following test is sufficient when the offset type is signed.
 * Otherwise, the platform specific test is needed. Note: Do not
 * reference the parameter twice! */
#define MVFS_IS_INVALID_OFFSET(ofst) ((ofst) < 0)

/* The following test is sufficient when the size type is signed.
 * Otherwise, the platform specific test is needed. Note: Do not
 * reference the parameter twice! */
#define MVFS_IS_VALID_SIZE(sz) ((sz) >= 0)

#define MVFS_IOCTL_RVALP_T int *

/* The following for the mvfs_aligner struct -- notes in mvfs_kmem.c */
#define MVFS_MAX_ALIGN_T long

#define VOPBD_T    void /* IOCTL VOP backdoor param */

#define VRWLOCK_T int /* lock type in frlock */

#define MVFS_PATHCONF_VAL_T u_long

#define MVFS_MDEP_DNC_CAP()
#ifndef PTR32_TO_PTR
#define PTR32_TO_PTR(x) (void *) (x)
#endif
#ifndef PTR_TO_PTR32
#define PTR_TO_PTR32(x) (ptr32_t ) (x)
#endif

#define REAL_CVN_RELE CVN_RELE

#define MVFS_VP_TO_CVP(vp,cvpp) VN_HOLD(vp); (*(cvpp)) = (vp)

#define MVFS_FLK_CALLBACK_T void

#define MVFS_ATTACH_VFS_DATA(vfsp, mmi) vfsp->vfs_data = (MVFS_VFSDATA_T)mmi

#define MVFS_REMOVE_VFS_DATA(vfsp, mmi) vfsp->vfs_data = NULL

#define MVFS_USER_ID CRED_UID_T

#define MVFS_GROUP_ID CRED_GID_T

#define MVFS_IS_OWNER(cred, va_ptr) (MDKI_CR_GET_UID(cred) == VATTR_GET_UID(va_ptr))

#define MVFS_CHKACCESS(vp, mode, va_ptr, cred) \
    mfs_chkaccess(vp, mode, (VATTR_GET_UID((va_ptr))), (VATTR_GET_GID((va_ptr))), \
                  ((int) VATTR_GET_MODE((va_ptr))), cred) 
#define MFS_CHKACCESS_DEFAULT

/*
 * Some platforms have a kernel routine for checking groupmember,
 * define the default routine here.
 */
#define MVFS_GROUPMEMBER(gid, cred)	mvfs_groupmember(gid, cred)
#define MVFS_GROUPMEMBER_DEFAULT

#define MVFS_CHKACCESS_MNODE(vp, mode, user_id, group_id, mmode, cred) \
    mfs_chkaccess(vp, mode, user_id, group_id, mmode, cred)
#define MFS_CHKACCESS_DEFAULT

#define MVFS_COMPARE_MNODE_UID(cred, user_id) (MDKI_CR_GET_UID((cred)) == (user_id))

#define MVFS_CREDUTL_SIDS_TO_NATIVE_IDS(mnp, cred) { \
    if ( (CREDUTL_SID_IS_NOBODY((&(mnp)->mn_vob.attr.fstat.usid)) || \
          CREDUTL_SID_IS_DONTCARE((&(mnp)->mn_vob.attr.fstat.usid))) ) {\
        (mnp)->mn_vob.user_id = TBS_UID_NOBODY; \
    } else { \
        (mnp)->mn_vob.user_id = \
        credutl_sid_to_unix_uid(&(mnp)->mn_vob.attr.fstat.usid); \
    } \
    if ( (CREDUTL_SID_IS_NOBODY((&(mnp)->mn_vob.attr.fstat.gsid)) || \
          CREDUTL_SID_IS_DONTCARE((&(mnp)->mn_vob.attr.fstat.gsid))) ) {\
        (mnp)->mn_vob.group_id = TBS_GID_NOBODY; \
    } else { \
        (mnp)->mn_vob.group_id = \
        credutl_sid_to_unix_gid(&(mnp)->mn_vob.attr.fstat.gsid); \
    } \
} 

#define MVFS_COPY_UID_TO_VATTR(vap, p_user_id, mnp, p_error) { \
    ASSERT(MISLOCKED((mnp))); \
    VATTR_SET_UID(vap, (*(p_user_id))); \
    *(p_error) = 0; \
}

#define MVFS_COPY_GID_TO_VATTR(vap, p_group_id, mnp, p_error) { \
    ASSERT(MISLOCKED((mnp))); \
    VATTR_SET_GID(vap, (*(p_group_id))); \
    *(p_error) = 0; \
}

#define MVFS_FREE_VATTR_FIELDS(va_ptr)

#define MVFS_FREE_ID(id)

#define MVFS_COPY_UID_TO_VIEW(mnp, cred, p_error) { \
        ASSERT(MISLOCKED((mnp))); \
        (mnp)->mn_view.cuid = MDKI_CR_GET_UID((cred)); \
        *(p_error) = 0; \
}

#define MVFS_COPY_GID_TO_VIEW(mnp, cred, p_error) { \
        ASSERT(MISLOCKED((mnp))); \
        (mnp)->mn_view.cgid = MDKI_CR_GET_GID((cred)); \
        *(p_error) = 0; \
}

#define MVFS_COPY_UID(dest_vattr_p, src_vattr_p, mnp, p_error) { \
    ASSERT(MISLOCKED((mnp))); \
    VATTR_SET_UID((dest_vattr_p), VATTR_GET_UID((src_vattr_p))); \
    *(p_error) = 0; \
}

#define MVFS_COPY_GID(dest_vattr_p, src_vattr_p, mnp, p_error) { \
    ASSERT(MISLOCKED((mnp))); \
    VATTR_SET_GID((dest_vattr_p), VATTR_GET_GID((src_vattr_p))); \
    *(p_error) = 0; \
}

#define MVFS_COPY_VATTR(dest_vattr_p, src_vattr_p, p_error) { \
    *(dest_vattr_p) = *(src_vattr_p); \
    *(p_error) = 0; \
}

#define MVFS_VATTR_TO_SATTR_UID(mask, vap, sattr) { \
    if ((mask) & AT_UID) { \
        mvfs_credutl_unix_uid_to_sid(VATTR_GET_UID((vap)), &(sattr)->usid); \
    } else { \
        mvfs_credutl_unix_uid_to_sid(TBS_UID_NOBODY, &(sattr)->usid); \
    } \
}

#define MVFS_VATTR_TO_SATTR_GID(mask, vap, sattr) { \
    if ((mask) & AT_GID) { \
        mvfs_credutl_unix_gid_to_sid(VATTR_GET_GID((vap)), &(sattr)->gsid); \
    } else { \
        mvfs_credutl_unix_gid_to_sid(TBS_GID_NOBODY, &(sattr)->gsid); \
    } \
}

#define MVFS_VATTR_TO_FSTAT_DB_UID(vap, usid_p) \
    mvfs_credutl_unix_uid_to_sid(VATTR_GET_UID((vap)), (usid_p))

#define MVFS_VATTR_TO_FSTAT_DB_GID(vap, gsid_p) \
    mvfs_credutl_unix_gid_to_sid(VATTR_GET_GID((vap)), (gsid_p))

/*
 * Macros to bump per view statistics.  Takes particular stat offset in view
 * mnode.  Covers per view stats with the mfs_statlock.
 */

#ifndef BUMP_PVSTAT
#define BUMP_PVSTAT(nm, s) { \
        mvfs_stats_data_t *sdp = MDKI_STATS_GET_DATAP(); \
        SPLOCK(sdp->mfs_statlock, (s)); \
        (nm)++; \
        SPUNLOCK(sdp->mfs_statlock, (s)); \
    }
#endif

#ifndef BUMP_PVSTAT_LOCKED
#define BUMP_PVSTAT_LOCKED(nm, s) { \
        (nm)++; \
    }
#endif

/*
 * Also for per view stats, but takes vnode.
 */
#define BUMPVSTAT(vnode, stat, s) { \
      if (MFS_VIEW(vnode)) { \
	struct mvfs_pvstat * pvp = VTOM(MFS_VIEW(vnode))->mn_view.pvstat; \
	BUMP_PVSTAT(pvp->stat,(s)); \
      } \
    }

/*
 * Macro as above to bump per view statistics except that it takes an mnode
 * pointer as an argument.  It will follow the viewvp pointer if it is set
 * and bump the statistics there without validating that it points to a view.
 */

#define BUMPVSTATM(mnode, stat, s) { \
      if (mnode->mn_hdr.viewvp) { \
        struct mvfs_pvstat * pvp = VTOM(mnode->mn_hdr.viewvp)->mn_view.pvstat; \
	BUMP_PVSTAT(pvp->stat,(s)); \
      } \
}

/*
 * Yet another variation on bumping view statistics.  Here we know that we 
 * have a pointer to a view vnode.  We just go ahead and bump the counter.
 * This just saves some typing.
 */

#define BUMPVSTATV(vnode, stat, s) { \
	struct mvfs_pvstat * pvp = VTOM(vnode)->mn_view.pvstat; \
	BUMP_PVSTAT(pvp->stat,(s)); \
}

#ifndef MFS_TIME_DELTA
/*
 * Macro to calculate elapsed time, but without adding to cumulative stats
 */
#define MFS_TIME_DELTA(stime, dtime, ztime) { \
	(ztime).tv_sec = (ztime).tv_nsec = 0; \
	mfs_bumptime(&(stime), &(dtime), &(ztime)); \
    }
#endif

#define MVFS_MDEP_PROC_START_AUDIT()

#define MVFS_MDEP_PROC_STOP_AUDIT()

#define MDKI_ISVCEXCL(x) ((x) == EXCL)   /* check exclusive create bit */

#define MVFS_MDEP_INIT()

#define MVFS_MDEP_UNLOAD()

#ifndef MVFS_MDEP_MISC_FREE
#define MVFS_MDEP_MISC_FREE()
#endif

#define MVFS_MAJORTBL_FREE(mvfs_majortbl) KMEM_FREE(mvfs_majortbl, \
        (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * sizeof(MVFS_MAJOR_T))

#define MVFS_DUMMY_RELE(thr)

/* Load a value if it's present and valid in the sizes */
#define MVFS_SIZE_CONDLOAD(var,sz,bit)          \
 if (MVFS_SIZE_VALID(sz,bit))                   \
     MVFS_SIZE_LOAD(var,sz,bit)

/* Load a value if it's present and valid in the sizes, else set the
   value to the default */
#define MVFS_SIZE_DEFLOAD(var,sz,bit,default)   \
 if (MVFS_SIZE_VALID(sz,bit))                   \
     (var) = (sz)->size[MVFS_SETCACHE_##bit];   \
 else                                           \
     (var) = (default)

/*
 * Some tunables cannot tolerate being zero (e.g. hash table sizes,
 * VOB/CVP freelists).  They use this macro to avoid it (and blurt a
 * warning): load a value to the default if not passed in the sizes or
 * if passed but not valid or zero.  If passed in and non-zero, take
 * the provided size.
 */
#define MVFS_SIZE_DEFLOAD_NONZERO(var,sz,bit,def)               \
   if (MVFS_SIZE_VALID(sz,bit)) {                               \
       if ((sz)->size[MVFS_SETCACHE_##bit] == 0) {              \
           mvfs_log(MFS_LOG_WARN, "Zero value for " #bit        \
		    " ignored (using default)\n");              \
           (var) = (def);                                       \
       } else                                                   \
           (var) = (sz)->size[MVFS_SETCACHE_##bit];             \
   } else                                                       \
       (var) = (def)

/* General context type */
#define MVFS_CALLER_CONTEXT_T	void

#define MVFS_HASH_CRED(cr) mvfs_hash_cred(cr)
#define MVFS_RELEASE_CREDLIST(mnp) mvfs_clear_release_mnode_credlist(mnp)
#define MVFS_INIT_CREDLIST(mma_size) mvfs_clear_init(mma_size)
#define MVFS_FREE_CREDLIST() mvfs_clear_free()
#define MVFS_FLUSH_CREDLIST(force) mvfs_flush_credlists(force)
#define MVFS_RECORD_CREDLIST(mnp, record_creds, cred) \
    if (DO_CLTXT_CREDS() && record_creds) \
        mvfs_record_cred(mnp, cred); 
#ifdef MVFS_DEBUG
#define DO_CLTXT_CREDS() (mvfs_cltxt_creds_enabled != 0)
#else
#define DO_CLTXT_CREDS() (1)
#endif  /* MVFS_DEBUG */
/*
 * How many calls of periodic maintenance must occur between each time
 * we flush the system credlist?  (it's typically called every 30
 * seconds)
 */
#define MVFS_CREDLIST_FLUSH_INTERVAL    120 /* once per hour */

#define MVFS_ESTALE ESTALE

/*
 * SEEK defines
 */
#ifdef SEEK_SET
#define MVFS_SEEK_SET SEEK_SET
#else
#define MVFS_SEEK_SET 0
#endif

#ifdef SEEK_CUR
#define MVFS_SEEK_CUR SEEK_CUR
#else
#define MVFS_SEEK_CUR 1
#endif

#ifdef SEEK_END
#define MVFS_SEEK_END SEEK_END
#else
#define MVFS_SEEK_END 2
#endif

/*
 * Note all ports send the filemode to the
 * create call, so have the code set up to 
 * ignore.
 */

#define MVFS_LFS_OPENMODE(flag) (1)

#define MVFS_UIO_OFFSET(uiop) ((uiop)->uio_offset)

#define MVFS_UIO_RESID_T u_long

#define MVFS_VSTATFS_BZERO(sbp, size) BZERO(sbp, size)

#define MVFS_FSTYP_T short

#define MVFS_PROD_PARENT_DIR_CACHE mvfs_prod_parent_dir_cache

#ifndef STRRCHR
#define MVFS_GENERIC_STRRCHR
#define STRRCHR mvfs_strrchr
EXTERN char *
mvfs_strrchr(
    const char *str,
    int ch
);
#endif /* STRRCHR */

#ifndef MVFS_VFID_SET_ERROR
#define MVFS_VFID_SET_ERROR(error, mnp, vp)  \
  if (MFS_ISVIEWDIR(mnp))  \
    mvfs_log(MFS_LOG_ERR,   \
            "NFS access not supported for viewroot directory.\n");  \
  else  \
    mvfs_log(MFS_LOG_ERR,  \
            "NFS access not supported for non-VOB object in view %s\n",  \
            mfs_vp2vw(vp));  \
  (error) = EACCES
#endif

/* Some platforms are picky about stack size, so make these macros in case we
** want to make this use a slab someday.
*/
#ifndef VATTR_ALLOC
#define VATTR_ALLOC() KMEM_ALLOC(sizeof(VATTR_T), KM_SLEEP)
#define VATTR_FREE(vap) KMEM_FREE((vap), sizeof(VATTR_T))
#endif

#ifndef MVFS_SYSTEM_KMEM
/* For platforms that still use the MVFS slab allocator. */
#define MVFS_CLR_CRED_ALLOC() \
		mvfs_slab_getchunk(mvfs_cred_list_slabs, sizeof(mvfs_clr_creds_t))
#define MVFS_CLR_CRED_FREE(ptr, bsize) \
		mvfs_slab_freechunk(mvfs_cred_list_slabs, (caddr_t)(ptr), bsize);
#define MVFS_CLR_CRED_SLAB_INIT(clptr, size, flag, label) \
		clptr = mvfs_create_slablist(size, \
						flag, label)
#define MVFS_CLR_CRED_SLAB_DESTROY(clptr) \
		mvfs_destroy_slablist(clptr); \
		clptr = NULL;

#define MVFS_THREAD_ALLOC()		mvfs_thread_cachealloc()
#define MVFS_THREAD_FREE(thptr)		mvfs_thread_cachefree(thptr)
#define MVFS_THREAD_SLAB_INIT(tsptr, size, flag, label) \
		tsptr = mvfs_create_slablist(size, flag, label)
#define MVFS_THREAD_SLAB_DESTROY(tsptr) \
		mvfs_destroy_slablist(tsptr); \
		tsptr = NULL;

#define MVFS_PROC_ALLOC()		mvfs_proc_cachealloc()
#define MVFS_PROC_FREE(procptr)		mvfs_proc_cachefree(procptr)
#define MVFS_PROC_SLAB_INIT(psptr, size, flag, label) \
		psptr = mvfs_create_slablist(size, flag, label)
#define MVFS_PROC_SLAB_DESTROY(psptr) \
		mvfs_destroy_slablist(psptr); \
		psptr = NULL;

#define MFS_PRKMEM()		mfs_prkmem()
#define MVFS_KMEM_UNLOAD()	mfs_kmem_unload()

#endif

/* Some newer compilers allow "noinline" as a keyword, some don't. */
#ifndef MVFS_NOINLINE
#define MVFS_NOINLINE
#endif

/* This constant is used as the size of an array on the stack in mvfs_mnode.c
** to be used as a last resort if memory can't be allocated.  If stack size is
** a problem on a platform, this could be made smaller.
*/
#ifndef MVFS_STK_FREEQLEN
#define MVFS_STK_FREEQLEN		32
#endif

/* Dummy versions of utility macros */
#define MDKI_INGLOBALZONE() (TRUE)
#define MDKI_ZONE_READY() (mvfs_init_state == MVFS_INIT_COMPLETE)
#define MDKI_SETZONEID(id)  0
#define MDKI_GETZONEID()  0
#define MDKI_GET_ZONE_NAME()

/* Macros to allocate/free subsystem data don't do anything */
#define MDKI_MNODE_ALLOC_DATA()
#define MDKI_MNODE_FREE_DATA()
#define MDKI_VIEWROOT_ALLOC_DATA()
#define MDKI_VIEWROOT_FREE_DATA()
#define MDKI_DNLC_ALLOC_DATA() 
#define MDKI_DNLC_FREE_DATA()
#define MDKI_STATS_ALLOC_DATA() 
#define MDKI_STATS_FREE_DATA()
#define MDKI_AUDIT_ALLOC_DATA()
#define MDKI_AUDIT_FREE_DATA()

/* Macros to get a pointer to the subsystem data */
#define MDKI_COMMON_GET_DATAP() (&mvfs_common_data_var)
#define MDKI_VFS_GET_DATAP() (&mvfs_vfs_data_var)
#define MDKI_MNODE_GET_DATAP() (&mvfs_mnode_data_var)
#define MDKI_VIEWROOT_GET_DATAP() (&mvfs_viewroot_data_var)
#define MDKI_DNLC_GET_DATAP()  (&mvfs_dnlc_data_var)
#define MDKI_STATS_GET_DATAP()  (&mvfs_stats_data_var)
#define MDKI_AUDIT_GET_DATAP()  (&mvfs_audit_data_var)

#endif /* MVFS_SYSTM_H_ */
/* $Id: 94be0db4.07e011dd.9a30.00:01:83:09:5e:0d $ */
