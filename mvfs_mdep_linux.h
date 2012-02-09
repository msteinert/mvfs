/* * (C) Copyright IBM Corporation 1999, 2008. */
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

#ifndef MVFS_MDEP_LINUX_H_
#define MVFS_MDEP_LINUX_H_
/*
 * Description:
 *      This hdr file encapsulates all of the MFS interface to
 *      the kernel procedures which vary from system to system.
 *
 *      It also includes definitions mfs routines to fixup
 *      problems on a given system, or fill in a missing
 *      kernel function.
 *
 *      The goal is to keep all porting specific calls and
 *      data types in this file.  The general form of any
 *      calls/macros added should be in V.4 form.
 */

#define INO_FS
/* We allocate the vnode (actually inode) separately from the mnode */
#define VNO_ALLOC
#define MVFS_NO_HILOCKS

/* We don't need to copy in the mntargs.  The OS has already done this */
#define MVFS_MNTARGS_COPYBYSYS
#define MFS_DONT_FREE_CLNT_AUTH
#define MFS_NONSTD_CLEAR_CREATE
#define MVFS_NONSTD_UIOSET

#define MVFS_MULTI_MAJOR
#define MFS_NO_PATHCONF /* it's user-space except stuff found via statfs */
#define MFS_NONSTD_INACTIVE
/* All linux's, especially __x86_64__ and __s390__, have heavy stack pressure,
** so just turn this on for everybody.
*/
#define MVFS_RPC_ARG_HEAP_STORAGE
#define MVFS_VFSVNODE_OPS

#if defined(__x86_64__) || defined(__s390x__) || defined(__powerpc64__)
#define ATRIA_LP64 1
#endif

#include <stdarg.h>
/* end of system headers
 */

/* Needed for our wrapper routines */
#include "vnode_linux.h"

#define _IOCPARM_MASK _IOC_SIZEMASK

/* needed for some core MVFS files */
#define OLD_TIME_STRUCT

#define xdr_opaque mdki_xdr_opaque

#define UIO_RW_T uio_rw_t
#define IOVEC_T iovec_t

struct flock;                           /* anonymous for mfs_lockctl() prototype */
struct in_addr;                         /* anonymous for tbs_rpc.h */

#include "linux_krpc.h"                 /* patches to it */

/* We need tbs_status_t for the prototype of mvfs_linux_proc_setview */

#include <tbs_base.h>

/* 
 * MVFS base types for various data types.
 */

/* Data types */

#define MVFS_PROCTAG_T mvfs_proctag_t
#define MVFS_PROCESS_T mvfs_process_t

typedef MVFS_PROCESS_T *MVFS_OWNER_T;

#define MVFS_PROCID_T mvfs_procid_t

/*
 * Locking stuff
 */

typedef struct mfs_lock {
    mdki_sleeplock_t slock;	/* Sleep lock */
    MVFS_OWNER_T owner;	        /* Lock owner. */
    long count;			/* recursion count */
    long recursive;		/* Allow recursive lock */
    const char *locker;         /* lock source address */
    caddr_t retpc;		/* Return PC of locker */
    caddr_t retpc2;		/* Return PC^2 of locker */
#ifdef MVFS_DEBUG
    MVFS_OWNER_T oldowner;	        /* Lock owner. */
    const char *oldlocker;              /* lock source address */
    caddr_t oldretpc;		/* Return PC of locker */
    caddr_t oldretpc2;		/* Return PC^2 of locker */
#endif
    char name[8];               /* Name of lock */
} mfs_lock_t;

#define LOCK_T          mfs_lock_t	/* Mutex sleep-lock typedef */
#define SPLOCK_T        spinlock_t
#define SPL_T           int		/* Processor priority type */

/* 
 * Miscellaneous definitions
 */

#define STATVFS_FILL(statp) 0           /* nothing needed */

#define MVFS_SIZE_TIME_MASK (AT_MTIME_SET|AT_ATIME_SET|AT_MTIME|AT_ATIME|\
                             AT_CTIME)

#ifndef MIN
#define MIN(a, b)   ((a) < (b) ? (a) : (b))
#endif

#define DEV_BSIZE 4096
#define DEV_BSIZE_BITS 12

struct mvfs_minmap;
EXTERN void
mvfs_linux_mapinit(struct mvfs_minmap *map);
#define MVFS_MDEP_MAPINIT(map) mvfs_linux_mapinit(map)

#define MVFS_FMT_UIO_RESID_X "x"
#define MVFS_FMT_UIO_RESID_D "d"
#define MVFS_FMT_VATTR_SIZE_T_X VNODE_FMT_VATTR_SIZE_T_X
#define MVFS_FMT_VATTR_SIZE_T_D VNODE_FMT_VATTR_SIZE_T_D
#define MVFS_FMT_UIO_OFFSET_X "llx"
#define MVFS_FMT_UIO_OFFSET_D "lld"
#if (defined(__i386__) || (defined(__s390__) && !defined(__s390x__)) || (defined(__powerpc__) && !defined(__powerpc64__)))
#define MVFS_FMT_SSIZE_T_X "x"
#define MVFS_FMT_SSIZE_T_D "d"
#elif (defined(__x86_64__) || defined(__powerpc64__) || defined(__s390x__))
#define MVFS_FMT_SSIZE_T_X "lx"
#define MVFS_FMT_SSIZE_T_D "ld"
#else
#error need CPU-specific defines fixed up
#endif

#define UIOMOVE(kbuf,klen,direction,uiop) \
    mdki_linux_readlink_uiomove(kbuf, klen, direction, uiop)

/*
 * Now for some mvfs things that need defining.  These are types
 * for which the standard definitions won't work.
 */

#define MVFS_SET_FSTYPE(vfsp) (vfsp)->vfs_fstype = VFS_MVFS

/* Use a function to create the current-view FSID from the MMI, rather
 * than accidentally giving out the system-assigned major/minor
 * number.
 */
extern fsid_t /* gross to return it, but so's the macro XXX */
mvfs_linux_vfsp_to_fsid(VFS_T *vfsp);

#define MDKI_VFSID(vfsp) mvfs_linux_vfsp_to_fsid(vfsp)

#define FSID_TO_DEV(fsid)  (fsid).val[0]

#define MAKEFSID(fsid, maj, min)  {\
    (fsid).val[0] = mdki_makedevice(maj, min); \
    (fsid).val[1] = 0; \
}

/* Starting in the 2.4 kernel, Linux no longer sets the device number
 * after calling read_super.  This will allow us to assign our own
 * device numbers so we don't have to use the unamed device.
 */

#define MAKEVFSDEV(vfsp, maj, min, nvmaj, nvmin) mdki_makevfsdev(vfsp, maj, min, nvmaj, nvmin)
#define VFSSW_T void

#define MFS_VFSSW_ASSIGN(sw,fstyp,ent)  /* no-op */
#define MFS_VFSSW_CLEAR(sw,fsnum)       /* no-op */

#define MVFS_ROOT(vfsp, vpp)    mfs_root((vfsp), (vpp))
#define MVFS_INITVFS(vfsp,opp,data)     {       \
	BZERO(vfsp, sizeof(*(vfsp)));           \
	(vfsp)->vfs_op = (opp);                 \
	(vfsp)->vfs_data = (data);              \
}
#define MVFS_FINISHVFS(vfsp)            /* nothing */

#define MVFS_STATFS(vfsp, cvn, statb) mdki_linux_statvfs_vnode(cvn, statb)

#define MVFS_GETVTYPE(vp)  ((vp)->v_type)
#define MVFS_ISVTYPE(vp, type) (MVFS_GETVTYPE(vp) == (type))

#define VTOM(vp)	((struct mfs_mnode *)((vp)->v_data))

#define MVFS_VNGET(mnvfsp, lovfsp, mnp, vpp) \
	mvfs_linux_vnget(mnvfsp, lovfsp, mnp, vpp)
struct mfs_mnode;                       /* forward decl */
extern int
mvfs_linux_vnget(
    VFS_T *mnvfsp,
    VFS_T *lovfsp,
    struct mfs_mnode *mnp,
    VNODE_T **vpp
);

#define MFS_VPISMFS(vp) mdki_vpismfs(vp)

#define MVFS_DECREMENT_LINK(vp) mdki_decrement_link(vp)
#define MVFS_INCREMENT_LINK(vp) mdki_increment_link(vp)

/*#define ROOTDIR		(mdki_sysroot_dentry->d_inode)*/
#define ROOTDIR CLR_ROOTDIR

/*
 * Now for the translation of vnode operations to the corresponding
 * Linux inode (or other) operations.  There may be a wrapper function
 * involved.
 */
#define MVOP_ACCESS VOP_ACCESS
#define MVOP_GETATTR(VP,CVP,VAP,F,CR)	VOP_GETATTR(VP,VAP,F,CR)
#define MVOP_SETATTR(VP,VAP,F,CR,CTXP)	VOP_SETATTR(VP,VAP,F,CR)
#define MVOP_READDIR VOP_READDIR
#define MVOP_OPEN VOP_OPEN
#define MVOP_CLOSE VOP_CLOSE
#define MVOP_READ VOP_READ
#define MVOP_WRITE VOP_WRITE
#define MVOP_IOCTL VOP_IOCTL
#define MVOP_MMAP VOP_MMAP
#define MVOP_PRINT VOP_PRINT

#define MVOP_CREATE(DVP,P,VAP,EX,MODE,VPP,CR,FL,DENTRY) VOP_CREATE(DVP, P, VAP, EX, MODE, VPP, CR, DENTRY)
#define MVOP_REMOVE VOP_REMOVE
#define MVOP_LINK VOP_LINK
#define MVOP_RENAME VOP_RENAME
#define MVOP_MKDIR VOP_MKDIR
#define MVOP_RMDIR VOP_RMDIR
#define MVOP_SYMLINK VOP_SYMLINK
#define MVOP_READLINK(VP,UIOP,CR)       (ENOSYS)
#define MVOP_SEEK VOP_SEEK
#define MVOP_FSYNC VOP_FSYNC
#define MVOP_FSYNC_KERNEL(VP, F, CR, FP) mvop_linux_fsync_kernel(VP, F, CR, FP)
#define MVOP_LOCKCTL VOP_LOCKCTL
/* MVOP_INACTIVE not used */
/* MVOP_FID not used */
/* MVOP_CMP not used */
/* MVOP_PATHCONF not needed until MFS_NO_PATHCONF turned off */
/* MVOP_SPACE not used */

/*
 * renaming for remove can't depend on vnode use count == 1, since the
 * dcache holds extra counts.
 */
#define MVFS_DEL_RENAME_NEEDED(vp,ctx) mdki_linux_rename_needed(vp,ctx)

#define LOOKUP_STORAGE_FILE(ro, pn,dvpp,vpp,cred)                       \
 mvop_linux_lookup_storage_file(pn, vpp, cred)

#define CLR_VNODE_T VNODE_T
#define CVN_HOLD VN_HOLD
#define CVN_RELE VN_RELE
#define CLR_ROOTDIR mvfs_sysroot_cvp
extern CLR_VNODE_T *mvfs_sysroot_cvp;

#define MVOP_OPEN_KERNEL(CVP,FLAGS,CRED,filp) mvop_linux_open_kernel(CVP,FLAGS,CRED,filp)
#define MVOP_CLOSE_KERNEL(CVP,FLAGS,COUNT,OFF,CRED,filp) mvop_linux_close_kernel(CVP,FLAGS,COUNT,OFF,CRED,filp,NULL)
#define MVOP_READ_KERNEL(CVP,UIOP,IOF,VAP,CRED,filp) mvop_linux_read_kernel(UIOP,IOF,CRED,filp)
#define MVOP_WRITE_KERNEL(CVP,UIOP,IOF,VAP,CRED,filp) mvop_linux_write_kernel(UIOP,IOF,CRED,filp)

/*
 * 2.4 generic file reading/writing routines do the locking of the cleartext
 * inodes themselves.
 */
#define MVOP_RWRDLOCK(dp,ctxp)
#define MVOP_RWWRLOCK(dp,ctxp)
#define MVOP_RWRDUNLOCK(dp,ctxp)
#define MVOP_RWWRUNLOCK(dp,ctxp)

#define MVOP_REALVP(VP, RVPP)           mvop_linux_realvp(VP, RVPP)
/*
 * MVOP_REALCVP() is called in mvfs_ioctl_lookup() and
 * mfs_auditioctl().  We need to be sure no other callers could use
 * the returned vnode as an inode, otherwise they'll run into
 * bogus_ops.
 *
 * auditioctl() never gets shadow files (it works to avoid that by
 * calling lookup specially), and only uses the object as a vnode.
 *
 * ioctl_lookup() only looks at a non-MVFS clrvnode in the case of
 * xstat, in which case it only needs to call VOP_GETATTR, which will
 * work OK with a layered clrvnode over a shadow vnode.  (It's not as
 * efficient as unwrapping a layer, however.)  So a failure to unwrap
 * in REALCVP is not crucial--we can let it just return the existing
 * object.
 *
 * We pass the address of cvp as the rcvpp, so that the function can
 * replace it for us and also return an "error".
 */
EXTERN int
mvfs_linux_realcvp(
    VNODE_T *vp,
    VNODE_T **rvp
);

#define MVOP_REALCVP(CVP, RCVPP)        mvfs_linux_realcvp(CVP, &(CVP))

#define MVFS_MAKE_DCACHE(dvp, vp, nm)	mdki_make_dcache(dvp, vp, nm)

#define MVFS_RM_DCACHE(vp)		mdki_rm_dcache(vp)
#define MVFS_START_FLUSH_DCACHE(vp,arg) mdki_start_flush_dcache(vp,arg)
/* cast return value (void) since generic code can't make use of it right now */ 
#define MVFS_FINISH_FLUSH_DCACHE(arg)   (void) mdki_finish_flush_dcache(arg)

/*
 * Misc inode parameters
 */

#define NINODE(_mcdp) ((_mcdp)->mvfs_mnmax) /* Can't seem to get one from Linux */
#define NINODE_NAME     "max_inodes"

#define MVFS_UMOUNT_EXPECTED_REFCHK	3
#define MVFS_UMOUNT_BUSY_REFCHK		1
#define MVFS_LASTCLOSE_T VNODE_LASTCLOSE_T
#define MVFS_LASTCLOSE_COUNT VNODE_LASTCLOSE_COUNT
#define MVFS_MMAP_CVP

extern int mvfs_lastmaj;
extern int mvfs_majdynmax;
extern int mvfs_majfixmax;
#define MVFS_MAJDYNMAX mvfs_majdynmax
#define MVFS_MAJFIXMAX mvfs_majfixmax
#define MVFS_MINORMAX mdki_maxminor
#define MVFS_VIEW_MASK_BITS 0
#define MVFS_VIEW_SHIFT_BITS mvfs_view_shift_bits
#define MDKI_SET_MINORMAX(minormax, view_shift_bits) \
    minormax = 1 << view_shift_bits;
#define MDKI_SET_VIEW_MASK_BITS(view_mask_bits, view_shift_bits)
#define MDKI_GET_MAJOR_DEV mdki_linux_get_major
#define MDKI_MAKEDEVICE(major, minor)   mdki_makedevice((major),(minor))
#define MDKI_MAJOR(dev) mdki_major(dev)
#define MDKI_MINOR(dev) mdki_minor(dev)
#define VATTR_ADJUST_DEV(dev, vp)       dev = mvfs_devadjust(dev, vp)

extern int
mvfs_linux_majortbl_init(MVFS_MAJOR_T **mvfs_majortbl_p);

#define MVFS_MAJORTBL_INIT mvfs_linux_majortbl_init

/* Declared in mvfs_mdki.h */
#define MVFS_RELEASE_MAJORS mdki_linux_release_majors

extern void
mvfs_linux_update_attrs(
    VNODE_T *vp,
    const char *where
);
extern int
mvfs_linux_set_modified(VNODE_T *vp);
extern int
mvfs_linux_set_accessed(VNODE_T *vp);
extern int
mvfs_linux_set_ichg(VNODE_T *vp);

extern void
mvfs_linux_getattr_cleanup(
    VNODE_T *origvn,
    VNODE_T *boundvn,
    VATTR_T *vap,
    int flag,
    CRED_T *cred
);
extern int
mvfs_linux_sync_size(
    VNODE_T *vp,
    loff_t size,
    int actual
);

#define MVFS_MDEP_GETATTR_CLEANUP(avp,vp,vap,flag,cred) mvfs_linux_getattr_cleanup(avp,vp,vap,flag,cred)

#define MVFS_WRAP_SYNC_SIZE(vp, size, actual)   mvfs_linux_sync_size(vp, size, actual)
#define MVFS_LINUX_UPDATE_ATTRS(vp, fn, line)	mvfs_linux_update_attrs(vp, fn ":" #line )
#define MVFS_WRAP_UPDATE_ATTRS(vp)	MVFS_LINUX_UPDATE_ATTRS(vp, __FILE__, __LINE__)
#define MVFS_WRAP_SET_MODIFIED(vp)      mvfs_linux_set_modified(vp)
#define MVFS_WRAP_SET_ACCESSED(vp)      mvfs_linux_set_accessed(vp)
#define MVFS_WRAP_SET_INODE_CHANGED(vp) mvfs_linux_set_ichg(vp)

/*
 * General Kernel Calls
 */

/*
 * MDKI definitions
 */

#define MDKI_PAGESIZE()		mdki_linux_page_size()
#define MDKI_PANIC		mdki_linux_panic

/* We only care about PANICSTR to try and avoid doing work below
 * panic() so that crash dumps will succeed.  However, Linux doesn't
 * do crash dumps, so we can't tell if we're in panic().  But we don't
 * want to keep recursing to panic() which can call into a VFS sync()
 * routine, so we only abort syncing if it's MVFS which has paniced
 * (e.g. ASSERT within periodic maintenance code)
 * after panic().
 */
#define MDKI_PANICSTR()         mdki_panicstr

#define MVFS_REAL_PRINTF mdki_linux_printf

/* PVN definitions
 * These handle things like flushing and syncing pages.  They are
 * OS specific.  They will be implemented as needed
 */

/* Flags for PVN_FLUSH */
#define MFS_PVN_FLUSH 0

/* A fancy nop because gcc doesn't like empty braces */
#define PVN_FLUSH(vp, f, cred) (0)
/* This will drop the pages from the queue but not save data to disk */
#define PVN_FLUSHINACTIVE(vp, f, cred) mdki_invalidate_vnode_pages(vp)

/* Time stuff */

typedef struct timespec timestruc_t;

/*
 * Time functions.
 *    ctime:  return current time in seconds
 *    hrtime: return timestruc time (time in secs and nsecs)
 *    boottime: seconds since epoch of system boot
 */

#define BOOTTIME()      	(mdki_linux_boottime)
#define MVFS_FMT_CTIME_X KS_FMT_TIME_T_X

/* Process definitions */

struct mvfs_proc;                       /* forward decl */
extern tbs_boolean_t
mvfs_linux_procvalid(struct mvfs_proc *mprocp);

#define MDKI_PRUNLOCK(p)

#define MDKI_CURPID()	        mdki_curpid()
#define MDKI_CURPROC()          mdki_curproc()
#define MDKI_PARENT_PROC(p)     mdki_get_parent(p)
#define MDKI_MYPROCID(tagp)     *(tagp) = MDKI_CURPID()
#define MDKI_PROCID(tagp, procp) *(tagp) = mdki_get_proc_pid(procp)
#define MDKI_PROCID_EQ(a, b)     (*(a) == *(b))
#define MDKI_MYPROCTAG(tagp, procid) *(tagp) = mdki_get_proctag(MDKI_CURPROC())
#define MDKI_PROCTAG(tagp, procp) *(tagp) = mdki_get_proctag(procp)
#define MDKI_PROCTAG_EQ(a,b)    (*(a) == *(b))

/*
 * Match and hash process objects based on PID.  This used to be based
 * on proc tag (FS struct from task) but that was prone to reuse
 * problems/false matches on dead processes.
 */
#define MDKI_PROC_EQ(mprocp, procidp, proctagp) MDKI_PROCID_EQ(&(mprocp)->mp_procid, (procidp))
#define MVFS_PROCHASH_PID
#define MVFS_FMT_PROCID_T_X	"x"
#define MVFS_FMT_PROCID_T_D	"d"

#define MDKI_PRISACTIVE(p) mdki_linux_procactive(p)
#define MDKI_PRPID(p)	mdki_get_proc_pid(p)
#define MDKI_PRSTATE(p)	(int)mdki_get_proc_state(p)
#define MVFS_PROCVALID(mprocp)   mvfs_linux_procvalid(mprocp)

/* We need to provide our own mvfs_proc_setview routine because we are
 * subtly different, (the root dir field is never NULL.)
 */

extern int
mvfs_linux_proc_setview(
    VNODE_T *vw,
    tbs_status_t *status_p
);

#define MVFS_SET_PROCVIEW(vw, statusp)    mvfs_linux_proc_setview(vw, statusp)
#define MVFS_GET_PROCVIEW() mdki_linux_get_procview()

/* Flag for mfs_getattr. These are bitmasks.  This must not conflict with
 * any mvfs_vnode getattr flags.
 */
#define MVFS_GETATTR_NO_BINDROOT        0x80000000
#if (MVFS_GETATTR_NO_BINDROOT == GETATTR_FLAG_UPDATE_ATTRS) || (MVFS_GETATTR_NO_BINDROOT == GETATTR_FLAG_PULLUP_ATTRS)
#error getattr flag conflict
#endif

typedef struct mvfs_linux_threadid {
    MVFS_PROCID_T tid_pid;
    MVFS_OWNER_T  tid_thread;
    mdki_boolean_t no_bindroot;
#if defined(MVFS_DEBUG) || defined(STACK_CHECKING)
    void *stack_check_id;
#endif
} mvfs_linux_threadid_t;

#define MVFS_THREADID_T	    mvfs_linux_threadid_t
#define MVFS_THREADHASH_SZ(_mcdp) ((_mcdp)->mvfs_threadhash_sz)
#define MVFS_THREADHASH_SZ_DEFAULT 511

/* to hash, drop the bottom 2 bits of the thread ID (they're
   zero--it's a pointer), and add in the pid. Then take modulo hash
   table size. */
/* threadid hash size should be 2^n-1, such as 511, 1023, 2047, etc. */
#define MDKI_THREADHASH(tagp, _mcdp) \
        (((tagp)->tid_pid + (((unsigned long)(tagp)->tid_thread)>>2)) \
         % MVFS_THREADHASH_SZ(_mcdp))

#define MDKI_MYTHREADID(tagp)                           \
        (tagp)->tid_pid = MDKI_CURPID(),                \
        (tagp)->tid_thread = MDKI_CURPROC(),            \
        (tagp)->no_bindroot = FALSE

#define MDKI_THREADID_EQ(a,b) \
        ((a)->tid_pid == (b)->tid_pid && (a)->tid_thread == (b)->tid_thread)

/*
 * If we set s_dirty to true and leave it alone, we will hang because
 * the Linux kernel won't proceed to the next file system until the
 * dirty bit is cleared.  So we have to set dirty whenever
 * mvfs_enter_fs is called, hence the linux specific function.
 */

EXTERN struct mvfs_thread *
mvfs_linux_enter_fs(void);

#define MVFS_MDEP_ENTER_FS()  mvfs_linux_enter_fs()

#if defined(MVFS_DEBUG) || defined(STACK_CHECKING)
/*
 * stack checking on thread exits
 */
EXTERN void
mvfs_linux_exit_fs(struct mvfs_thread *thr);
#define MVFS_MDEP_EXIT_FS(thr) mvfs_linux_exit_fs(thr)
#endif

#define MVFS_MDEP_COVER_CVIEW()       mvfs_linux_should_cover()
/*
 * Should do a cover check of loopback (automount) nodes.  How can we
 * do this without a real VOP_REALVP()?  (We don't have to--loopback
 * automounts are done with bind mounts and don't have any wrapper
 * nodes.)
 */
/*
 * Some clrvnodes are transient and shouldn't be used (e.g. mkdir results
 * when the underlying file system drops dentries like NFS used to do).
 * Check whether that's the case before covering and potentially putting
 * them into a hash chain where someone else might find and trip over them.
 */
#define MVFS_COVER_CHECK(vp)    (mdki_cvn_is_stable(vp) ? 0 : ENOENT)

extern int
mvfs_linux_should_cover(void);

#define LOOKUP_FOR_IOCTL(pn,s,f,opt,dvpp,vpp,cred)              \
	mvfs_linux_lookup_ioctl(pn,s,f,opt,dvpp,vpp,cred)

#define LOOKUP_AUDIT_FILE(pn,vpp,cred)                                  \
        mvop_linux_lookup_storage_file(pn, vpp, cred)

extern int 
mvfs_linux_lookup_ioctl(
    /* mfs_pn_char_t */char *path,
    int segflag,
    SYMFOLLOW_T follow,
    int opt,
    VNODE_T **dvpp,
    CLR_VNODE_T **vpp,
    CRED_T *cred
);

extern char *
mfs_makesname(
   char *nm,
   const char *p,
   int num
);

extern void
mvfs_linux_initlock(
    LOCK_T *lptr,
    char *name
);

extern void
mvfs_linux_freelock(
    LOCK_T *lptr
);

extern int
mvfs_linux_condlock(
    LOCK_T *lptr,
    caddr_t retpc,
    caddr_t retpc2,
    const char *where
);

/* Define 1 spinlock per hash chain */
#define HASH_SPLOCK_MAP HASH_SPLOCK_PER_CHAIN
#define HASH_SPLOCK_RATIO 1

/* using default for HASH_MVFS_LOCK_RATIO */

/* Read/write locks */
/* Note that Red Hat and SuSE have different implementations of the
 * reader/writer semaphores.  They are presumed to be equivalent.
 */
#define MVFS_RW_LOCK_T  struct rw_semaphore

#define MVFS_RW_LOCK_INIT(rw_lptr, name) init_rwsem(rw_lptr)
#define MVFS_RW_LOCK_DESTROY(rw_lptr) /* nothing */
#define MVFS_RW_READ_LOCK(rw_lptr, opl) down_read(rw_lptr) 
#define MVFS_RW_WRITE_LOCK(rw_lptr, opl) down_write(rw_lptr) 
#define MVFS_RW_READ_UNLOCK(rw_lptr, opl) up_read(rw_lptr)
#define MVFS_RW_WRITE_UNLOCK(rw_lptr, opl) up_write(rw_lptr)

#define MAKESNAME(nm,p,num)	mfs_makesname(nm,p,num)

#define INITLOCK(lptr,name)	mvfs_linux_initlock((lptr),name)
#define FREELOCK(lptr)          mvfs_linux_freelock((lptr))

#define ISLOCKED(lptr) MDKI_SLEEP_ISLOCKED(&(lptr)->slock)
#define NOTLOCKEDBYME(lptr) ((lptr)->owner != MDKI_CURPROC())
#define ISLOCKEDBYME(lptr) (ISLOCKED(lptr) && ((lptr)->owner == MDKI_CURPROC()))

#define MVFS_LOCK__(lptr,line)         {                        \
        if (NOTLOCKEDBYME(lptr))                                \
                MDKI_SLEEP_LOCK(&(lptr)->slock);                 \
        if (++(lptr)->count > 1 && !(lptr)->recursive) {        \
                MDKI_PANIC("mvfs: LOCK: recursive lock %p:" __FILE__ ": %s :" #line ", locker %s", (void *)(lptr), __func__, (lptr)->locker);       \
        }                                                       \
        (lptr)->owner = MDKI_CURPROC();                         \
        (lptr)->locker = __FILE__ ":" #line;                    \
        (lptr)->retpc = mdki_getreturn();                        \
        (lptr)->retpc2 = mdki_getmycaller();                     \
        DEBUG_ASSERT((lptr)->count > 0);                              \
    }

#define MVFS_LOCK_(lptr,line) MVFS_LOCK__(lptr,line)
#define MVFS_LOCK(lptr) MVFS_LOCK_(lptr,__LINE__)

#define MVFS_RDDIR_MNLOCK_SET_RECURSIVE(mnp)                            \
    tbs_boolean_t was_recursive = LOCK_IS_RECURSIVE(MLOCK_ADDR(mnp));   \
    DEBUG_ASSERT(ISLOCKEDBYME(MLOCK_ADDR(mnp)));                              \
    LOCK_SET_RECURSIVE(MLOCK_ADDR(mnp))

#define MVFS_RDDIR_MNLOCK_CLEAR_RECURSIVE(mnp)                  \
    if (!was_recursive) LOCK_CLEAR_RECURSIVE(MLOCK_ADDR(mnp))

#define LOCK_SET_RECURSIVE(lptr) (lptr)->recursive = 1
#define LOCK_CLEAR_RECURSIVE(lptr) (lptr)->recursive = 0
#define LOCK_IS_RECURSIVE(lptr)	(lptr)->recursive

#define CONDITIONAL_LOCK__(lptr,line)	mvfs_linux_condlock((lptr),mdki_getreturn(),mdki_getmycaller(),__FILE__":" #line)
#define CONDITIONAL_LOCK(lptr) CONDITIONAL_LOCK_(lptr,__LINE__)
#define CONDITIONAL_LOCK_(lptr,line) CONDITIONAL_LOCK__(lptr,line)

#ifdef MVFS_DEBUG
#define DO_OLD_LOCKS(lptr)                      \
 (lptr)->oldlocker = (lptr)->locker;            \
 (lptr)->oldretpc = (lptr)->retpc;              \
 (lptr)->oldretpc2 = (lptr)->retpc2;            \
 (lptr)->oldowner = (lptr)->owner
#else
#define DO_OLD_LOCKS(lptr) /**/
#endif

#define MVFS_UNLOCK(lptr)       {                       \
        DEBUG_ASSERT((lptr)->count > 0);                      \
        DEBUG_ASSERT(ISLOCKEDBYME(lptr));                     \
        if (--(lptr)->count == 0) {                     \
                 DO_OLD_LOCKS(lptr);                    \
                 (lptr)->locker = NULL;                 \
                 (lptr)->retpc = NULL;                  \
                 (lptr)->retpc2 = NULL;                 \
                 (lptr)->owner = NULL;                  \
                 MDKI_SLEEP_UNLOCK(&(lptr)->slock);      \
        }                                               \
    }

#define INITSPLOCK(lock,lnm)    spin_lock_init(&(lock))
#define SPLOCK(lock, i)         spin_lock(&(lock))
#define SPUNLOCK(lock, i)       spin_unlock(&(lock))
#define FREESPLOCK(lock)        /* nothing */

/*
 * Ioctl definitions
 */

#define MFS_IOCTL_COPYBYME 1

/*
 * U-area manipulation functions.  Except, of course that Linux doesn't
 * have a specific u-area to manipulate.
 */

/*
 * Since we can fabricate CLR_VNODE_T's for ucdir and urdir vnodes,
 * we implicitly take a reference on them, so we must provide macros for
 * core code to release the references.
 */
#define MDKI_VNRELE_RCDIR(vp) VN_RELE(vp)
#define MDKI_VNHOLD_RCDIR(vp) (void) VN_HOLD(vp)

#define MDKI_GET_U_CDIR()       mdki_get_ucdir_vnode()
/*
 * error to use in case MDKI_GET_U_CDIR() returns NULL (only happens
 * when no cltxt inode can be allocated)
 */
#define MVFS_NULL_U_CDIR_ERR ENOMEM      /* XXXJTK ESRCH? */
#define MDKI_GET_U_RDIR()	mdki_get_urdir_vnode()

#define MDKI_GET_U_COMM_PTR()   mdki_get_ucomm_ptr()
#define MDKI_GET_U_CMASK()      mdki_get_ucmask()
#define MDKI_SET_U_ERROR(errnum) /* no error side effects to undo; can't undo suser() check flag */

#if	defined(ATRIA_64BIT_LONGS)
/* Directories need to be modulo 8 in size */
#define MFS_AUDIT_ALIGN_BDRY 7		/* long-aligned requires diddling
					   by 7's */
#endif

/*
 * Dirent things.
 */

#define READDIR_UIOMOVE(FROM, CNTP, FLAG, UIOP, OFFSET) \
	mdki_linux_readdir_uiomove((FROM), *(CNTP), (FLAG), (UIOP), (OFFSET))

#define READDIR_BUF_FULL(UIOP) (UIOP)->uio_rddir_full

/* 
 * Other functions
 */

#define LOOKUP_COMPONENT(vp, nm, cvpp, pnp, rdir, cr, ctx)    \
        mvop_linux_lookup_component((vp), (nm), (cvpp), (cr), (ctx))

/* Special macros to handle special escapes such as ROOTDIR^ in
 * mfs_lookup.
 */

#define MVFS_SAVE_ROOTDIR(ctx)  mdki_get_rootdir((ctx)->dentrypp)

/*
 * RPC definitions (make Linux kernel RPC look like Sun ONC RPC API).
 */

struct rpc_clnt;
#define MDKI_CLNTKUDP_CREATE(bogus,a,t,r,i,c)	\
    mvfs_linux_clntkudp_create(a,t,r,i)
#define MDKI_CLNTKUDP_INIT(h,a,r,c,i,t,bogus_p,bogus_v,bogus_n) 	\
    mdki_linux_clntkudp_init(h,a,r,i)

#define MDKI_CLNTKUDP_INTR(h,i) mdki_linux_clnt_set_intr(h, i)
#define MDKI_CLNTKUDP_DESTROY(cl) mdki_linux_destroy_client(cl)
#define MDKI_CLNTKUDP_FREE(cl)  /* destroy will free the auth stuff */
#define MFS_DONT_FREE_CLNT_AUTH         /* let it handle them */
/* MDKI_AUTH_DESTROY is not used by us */
#define MDKI_AUTH_DESTROY(au)
/* AUTHKERN_CREATE() not used */
#define MDKI_CLNT_CALL(cl,op,xa,ra,xr,rr,w,rt,m,i,cr,s) \
    mdki_linux_clnt_call(cl,op,ra,rr,rt,/*m,*/i,cr,&s)
#define MDKI_CLNT_GETERR(cl,errp)		mvfs_linux_clnt_geterr(errp)
#define MDKI_CLNT_SPERRNO(status)		mvfs_linux_clnt_sperrno(status)
/*
 * (cl)->cl_auth is only used to compare vs. NULL to spit out some
 * error cases.  Linux has it always set, but we can't access it
 * through the anonymous type in the MVFS core, so we just compare
 * the client handle itself to satisfy the checks.
 */
#define MDKI_CLNT_AUTH_VALID(cl) ((cl) != NULL)
#define MDKI_ALLOC_XID()			mvfs_linux_alloc_xid()
#define MDKI_SET_XID(cl,xid)
u_long mvfs_linux_alloc_xid(void);

void
mvfs_linux_clnt_geterr(
    struct rpc_err *errp
);

#define MDKI_CLNTKUDP_ADDR_T			int /* not really used */
#define MDKI_SIGMASK_T                          int /* not really used */

#define MVFS_GENERIC_PROD_PARENT_DIR_CACHE

struct mfs_callinfo;                    /* forward decl */

const char *
mvfs_linux_clnt_sperrno(enum clnt_stat status);

CLIENT *
mvfs_linux_clntkudp_create(
    struct sockaddr_in *addr,
    struct mfs_callinfo *trait,
    int retrans_count,
    bool_t intr
);

/*
 * General Networking definitions
 */

#define MVFS_SIN_CVT(sockaddrp) /* no conversion needed */

/* This is the count of physical pages on the system. */

#define MVFS_PHYSMEM_PAGECNT	mdki_physmem_pagecnt()

/* The following are things that are normally defined in sys/param.h.
 * However, in linux, user space includes and kernel includes don't
 * mix well so we have to define them here.
 */

#define MAXNAMELEN mdki_linux_maxnamelen()
#define MAXPATHLEN mdki_linux_maxpathlen()
#define AUDIT_MAXPATH 4096 /* NB: should match MAXPATHLEN from user-space */
#define NBBY 8

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
/* This is used to declare an array of pointers on the stack in mvfs_mnode.c.
** For 64-bit kernels, especially s390x, it is just too big.  This space is
** only used as a last resort if memory allocation fails when freeing mnodes,
** so it should be OK to make it smaller (i.e. there are probably bigger
** problems to worry about if this happens).
*/
#define MVFS_STK_FREEQLEN		12
#else
#define MVFS_STK_FREEQLEN               32
#endif

#define STK_M_GET       0
#define STK_CLNTCALL    0+STK_M_GET
#define STK_VOPLOOKUP   STK_CLNTCALL+0
#define STK_VOPCREATE   STK_CLNTCALL+0
#define STK_LOOKUPNAME  STK_CLNTCALL+0
#define STK_GETATTR     STK_CLNTCALL+0
#define STK_MFSGETATTR  STK_CLNTCALL+0

/*
 * SVR4 has a pathname structure for passing around pathnames.  Linux 
 * doesn't.  For now it will just contain a pointer to a string.
 */

typedef struct pathname {
    void *dummy;
} pathname_t;

/* This is a new ioctl that we had to add to compensate for the 
 * hashing of the vfsmount structures added in the 2.4.8 kernel.
 */

#define MVFS_SET_VOBRT_VFSMNT(vob_path) \
    mdki_linux_set_vobrt_vfsmnt((vob_path)->kpn.s)

#define MDKI_USECDELAY(us)      mdki_delay_usec(us)
#define MDKI_SUSER(cred)        mdki_suser() /* no creds! */

/* uprintf is supposed to print on the current user's terminal.  Some systems
 * don't support it, including Linux.  Make it a no-op.
 */

#define UPRINTF(s)                      /* nothing */

#define MVFS_VPRINTF_3		mvfs_logfile_vprintf_3
#define MVFS_PRINTF		mvfs_logfile_printf
#define	MVFS_GENERIC_LOGFILE_PRINTF

#define MVFS_MDEP_MISC_FREE() mvfs_linux_misc_free()
#define MVFS_MDEP_MISC_INIT(sizes) mvfs_linux_misc_init(sizes)

struct mvfs_cache_sizes;
extern int
mvfs_linux_misc_init(struct mvfs_cache_sizes *sizes);
extern void
mvfs_linux_misc_free(void);

#define MVFS_CREATEVP(nm, vap, vpp, cred, flag) \
	mdki_linux_createvp(nm, vap, vpp, cred, flag)

/*
 * close log file at viewroot-unmount time to avoid problems with
 * loopback vnode initialization/cleanup order.
 */
#define MVFS_UNMOUNT_CLOSELOG

/* 
 * VNODEOPS PASS THROUGH PARAMETERS.
 */

#define MVFS_OPEN_CTX_T    file_ctx
#define MVFS_CLOSE_CTX_T   file_ctx
#define MVFS_RDWR_CTX_T    file_ctx
#define MVFS_RMDIR_CTX_T   dent_ctx
#define MVFS_LOOKUP_CTX_T  lookup_ctx
#define MVFS_SEEK_CTX_T    seek_ctx
#define MVFS_MMAP_CTX_T    mmap_ctx
#define MVFS_READDIR_CTX_T readdir_ctx
#define MVFS_CREATE_CTX_T  create_ctx
#define MVFS_LINK_CTX_T    link_ctx
#define MVFS_REMOVE_CTX_T  unlink_ctx
#define MVFS_SYMLINK_CTX_T symlink_ctx
#define MVFS_FSYNC_CTX_T   file_ctx
#define MVFS_LOCKCTL_CTX_T file_ctx
#define MVFS_MKDIR_CTX_T   mkdir_ctx
#define MVFS_RENAME_CTX_T  rename_ctx
#define MVFS_ACCESS_CTX_T  nameidata_ctx

#define MVFS_RCSID_STRING_VAR mvfs_linux_rcsid_string
#define MVFS_SCCSID_STRING_VAR mvfs_linux_sccsid_string
extern char *mvfs_linux_rcsid_string;
extern char *mvfs_linux_sccsid_string;

#define MVFS_SNPRINTF mdki_snprintf
#define MVFS_VSNPRINTF mdki_vsnprintf

#ifdef ATRIA_LP64
#define MDKI_CALLER_IS_32BIT(x) mdki_curproc_is_32bit(x)
typedef __u32	ptr32_t;
/* Oddball casting to make GCC shut up */
#define PTR32_TO_PTR(x) (void *)((unsigned long) (x))
#define PTR_TO_PTR32(x) (ptr32_t)((unsigned long) (x))
#endif
#ifdef RATL_COMPAT32
#define MVFS_IS_IT_OUR_IOCTL(cmd,callinfo) \
  mvfs_linux_ioctl_chk_cmd(cmd,callinfo)
tbs_boolean_t
mvfs_linux_ioctl_chk_cmd(
    int cmd,
    MVFS_CALLER_INFO *callinfo
);
#endif

/*
 * Macros to retrieve Maximum offset. 
 * For Linux, we need the file pointer to
 * determine whether or not the file has been
 * open for largefile support.
 */

#define MVFS_GET_MAXOFF_SEEK_CTX(vp, ctx) \
    ((mdki_seek_ctx_open_for_lfs(ctx))?(MVFS_GET_MAXOFF(vp)):MVFS_MAXOFF_32_T)

#define MVFS_GET_MAXOFF_FILE_CTX(vp, ctx) \
    ((mdki_file_ctx_open_for_lfs(ctx))?(MVFS_GET_MAXOFF(vp)):MVFS_MAXOFF_32_T)

#define MVFS_RDWR_GET_MAXOFF(vp, uiop, maxoff, ctx) \
    maxoff = (MVFS_GET_MAXOFF_FILE_CTX(vp, ctx))

/*
** Macros to increment/decrement statistics.  They are all declared as
** MVFS_ATOMIC_T types.  The mfs_statlock (defined in mvfs_utils.c) is only
** used to "chunk" stat updates (rather than locking around each one).  It is
** not used to make a group of updates "consistent" (either we're not worried
** about that level of consistency, or a higher level lock already provides
** it).
*/
#define MVFS_ATOMIC_T atomic_t
#define MVFS_ATOMIC_INIT(i) ATOMIC_INIT(i)
#define MVFS_ATOMIC_READ(v) atomic_read(&(v))

#define MVFS_STATLOCK_INIT()
#define MVFS_STATLOCK_FREE()
#define MVFS_STATLOCK_LOCK(spl)
#define MVFS_STATLOCK_UNLOCK(spl)

#define BUMPSTAT(nm, s) {mvfs_stats_data_t *sdp = MDKI_STATS_GET_DATAP(); \
                         atomic_inc(&(sdp->nm));  }
#define BUMPSTAT_LOCKED(nm, inc) {mvfs_stats_data_t *sdp = MDKI_STATS_GET_DATAP(); \
                                  atomic_add((inc), &(sdp->nm)); }
#define DECSTAT(nm, s)          {mvfs_stats_data_t *sdp = MDKI_STATS_GET_DATAP(); \
                                 atomic_dec(&(sdp->nm)); }

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0) && \
    (defined(__s390__) || defined(__s390x__))
/* Earlier versions for s390 don't have a cmpxchg() macro defined, so
** just do the "simple" thing (like the default in mvfs_systm.h).
*/
#define XCHG_STAT32_CMP_LOCKED(target, cmp, value) \
        (target) = (value)
#else
#define XCHG_STAT32_CMP_LOCKED(target, cmp, value) \
                cmpxchg(&(target), (cmp), (value))
#endif

/*
 * Macros to bump per view statistics.  Takes particular stat offset
 * in view mnode.
 */
#define BUMP_PVSTAT(nm, s) atomic_inc(&(nm)) 

#define BUMP_PVSTAT_LOCKED(nm, s) atomic_inc(&(nm)) 

/* Since it might not be the case that an atomic_t is the same size as the
** fields in a timestruct_t, we'll just use a spinlock here to make this easy.
*/
extern void
mvfs_linux_bumptime(
    timestruc_t *stp,
    timestruc_t *dtp,
    timestruc_t *tvp
);
#define MFS_BUMPTIME(stime, dtime, nm)  { \
                mvfs_stats_data_t *sdp = MDKI_STATS_GET_DATAP(); \
                mvfs_linux_bumptime(&(stime), &(dtime), &(sdp->nm)); \
                }

#define MFS_TIME_DELTA(stime, dtime, ztime) {  \
                (ztime).tv_sec = (ztime).tv_nsec = 0; \
                mvfs_linux_bumptime(&(stime), &(dtime), &(ztime)); \
                }

#endif
/* $Id: d54dcdd3.541d11dd.90ce.00:01:83:09:5e:0d $ */
