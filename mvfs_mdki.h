#ifndef MVFS_MDKI_H_
#define MVFS_MDKI_H_
/*
 * Copyright (C) 1999, 2012 IBM Corporation.
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

#include <linux/types.h>
#include <linux/param.h>
#include <linux/time.h>
#include <linux/in.h>
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24) 
#include <linux/exportfs.h>
#include <linux/hash.h>
#endif


#define MDKI_PTR_TYPE(name) struct name ## _struct *name
#define MDKI_GEN_TYPE(name) struct name ## _struct name

/*
 * types, macros, functions, etc. which are exported from the adapter
 * to the MVFS core.
 */
/*
 * VATTR definitions and macros.
 * Linux stores attributes in struct inode, not in separate objects.
 * Our glue layers need to pull up attributes into an inode whenever
 * it's returned to a linux kernel caller.
 * We define our own struct vattr for internal use, and convert it to
 * struct iattr (when setting underlying attributes) or copy it up into
 * the inode (when returning values).
 */

struct mvfs_linux_vattr;                 /* forward decl */
#define VATTR_T struct mvfs_linux_vattr
#define VATTR_MODE_T    mode_t         /* mode as stored in vattr struct */
#define VATTR_UID_T     uid_t           /* UID as stored in vattr struct */
#define VATTR_GID_T     gid_t           /* GID as stored in vattr struct */
#define VATTR_SIZE_T    loff_t          /* Size data type */
#define VNODE_FMT_VATTR_SIZE_T_X "llx"
#define VNODE_FMT_VATTR_SIZE_T_D "lld"
#define VATTR_TIME_T    time_t          /* Time data type */

/*
 * Attributes of interest to the caller of setattr or getattr.
 */
#undef AT_TYPE
#undef AT_MODE
#undef AT_UID
#undef AT_GID
#undef AT_FSID
#undef AT_NODEID
#undef AT_NLINK
#undef AT_SIZE
#undef AT_ATIME
#undef AT_MTIME
#undef AT_CTIME
#undef AT_RDEV
#undef AT_BLKSIZE
#undef AT_NBLOCKS

#define AT_TYPE         0x0001
#define AT_MODE         0x0002
#define AT_UID          0x0004
#define AT_GID          0x0008
#define AT_FSID         0x0010
#define AT_NODEID       0x0020
#define AT_NLINK        0x0040
#define AT_SIZE         0x0080
#define AT_ATIME        0x0100
#define AT_MTIME        0x0200
#define AT_CTIME        0x0400
#define AT_RDEV         0x0800
#define AT_BLKSIZE      0x1000
#define AT_NBLOCKS      0x2000
/*
 * These are not really attributes, but flags that Linux uses to
 * control the access checking.  I will put these in the upper part
 * of the word.
 */
#define AT_ATIME_SET    0x20000000
#define AT_MTIME_SET    0x40000000

#define AT_ALL          (AT_TYPE|AT_MODE|AT_UID|AT_GID|AT_FSID|AT_NODEID|\
                        AT_NLINK|AT_SIZE|AT_ATIME|AT_MTIME|AT_CTIME|\
                        AT_RDEV|AT_BLKSIZE|AT_NBLOCKS)

/* The following is the same as AT_ALL but it excludes the FSID (dev) field */
#define AT_ALLNODEV     (AT_ALL & ~AT_FSID)

#define AT_STAT         (AT_MODE|AT_UID|AT_GID|AT_FSID|AT_NODEID|AT_NLINK|\
                        AT_SIZE|AT_ATIME|AT_MTIME|AT_CTIME)

#define AT_TIMES        (AT_ATIME|AT_MTIME|AT_CTIME)

#define AT_NOSET        (AT_NLINK|AT_FSID|AT_NODEID|AT_TYPE|AT_BLKSIZE|AT_NBLOCKS)

#define VATTR_NULL(vap)                 BZERO((vap), sizeof *(vap))
#define VATTR_GET_MASK(vap)		((vap)->va_mask)
#define VATTR_SET_MASK(vap, mask)	(vap)->va_mask = (mask)
#define VATTR_GET_TYPE(vap)		((vap)->va_type)
#define VATTR_SET_TYPE(vap, vtype)	((vap)->va_type = (vtype))
#define VATTR_GET_MODE(vap)		((vap)->va_mode & ~S_IFMT)
#define VRIGHTSMASK ( S_ISUID | S_ISGID | S_ISVTX | S_IRWXU | S_IRWXG | S_IRWXO )
#define VATTR_SET_MODE_RIGHTS(vap, m)	\
    (vap)->va_mode = (VATTR_MODE_T)(((vap)->va_mode & ~VRIGHTSMASK) | ((m) & VRIGHTSMASK))
#define VATTR_SET_MODE_TYPE(vap, m)	\
    (vap)->va_mode = (VATTR_MODE_T)(((vap)->va_mode & VRIGHTSMASK) | ((m) & ~VRIGHTSMASK))
#define VATTR_GET_UID(vap)		((vap)->va_uid)
#define VATTR_SET_UID(vap, u)		(vap)->va_uid = (VATTR_UID_T)(u)
#define VATTR_GET_GID(vap)		((vap)->va_gid)
#define VATTR_SET_GID(vap, gid)		(vap)->va_gid = (VATTR_GID_T)(gid)
#define VATTR_GET_FSID(vap)		((vap)->va_fsid)
#define VATTR_SET_FSID(vap, fsidp)	(vap)->va_fsid = *(fsidp)
#define VATTR_GET_NODEID(vap)		((vap)->va_nodeid)
#define VATTR_SET_NODEID(vap, nid)	(vap)->va_nodeid = (nid)
#define VATTR_GET_NLINK(vap)		((vap)->va_nlink)
#define VATTR_SET_NLINK(vap, n)		(vap)->va_nlink = (n)
#define VATTR_GET_SIZE(vap)		((vap)->va_size)
#define VATTR_SET_SIZE(vap, s)		(vap)->va_size = (s)
#define VATTR_GET_BLKSIZE(vap)		((vap)->va_blksize)
#define VATTR_SET_BLKSIZE(vap, bs)	(vap)->va_blksize = (bs)
#define VATTR_GET_NBLOCKS(vap)		((vap)->va_nblocks)
#define VATTR_SET_NBLOCKS(vap, nb)	(vap)->va_nblocks = (nb)
/* Convert bytes to blocks - on Linux, the kstat block size is hardcoded
 * as 512.  See ST_NBLOCKSIZE in $COREUTILS/src/system.h, S_BLKSIZE in
 * /usr/include/sys/stat.h, and especially the code in inode_add_bytes()
 * in $LINUX/fs/stat.c.
 */
#define VATTR_BTODB(n)                  (((n) + 512 - 1) >> 9)

#define VATTR_GET_ATIME(vap)		((vap)->va_atime.tv_sec)
#define VATTR_GET_ATIME_TV(vap, tvp)	*(tvp) = (vap)->va_atime
#define VATTR_GET_ATIME_TS(vap, tsp)	MDKI_TIMEVAL_TO_TIMESPEC(&(vap)->va_atime, tsp)
#define VATTR_SET_ATIME_TV(vap, tvp)	(vap)->va_atime = *(tvp)
#define VATTR_SET_ATIME_TS(vap, tsp)	MDKI_TIMESPEC_TO_TIMEVAL(tsp, &(vap)->va_atime)

#define VATTR_GET_MTIME(vap)		((vap)->va_mtime.tv_sec)
#define VATTR_GET_MTIME_TV(vap, tvp)	*(tvp) = (vap)->va_mtime
#define VATTR_GET_MTIME_TS(vap, tsp)	MDKI_TIMEVAL_TO_TIMESPEC(&(vap)->va_mtime, tsp)
#define VATTR_SET_MTIME_VATTR(vap1, vap2) (vap1)->va_mtime = (vap2)->va_mtime
#define VATTR_SET_MTIME_TV(vap, tvp)	(vap)->va_mtime = *(tvp)
#define VATTR_SET_MTIME_TS(vap, tsp)	MDKI_TIMESPEC_TO_TIMEVAL(tsp, &(vap)->va_mtime)

#define VATTR_GET_CTIME(vap)		((vap)->va_ctime.tv_sec)
#define VATTR_GET_CTIME_TV(vap, tvp) 	*(tvp) = (vap)->va_ctime
#define VATTR_GET_CTIME_TS(vap, tsp)	MDKI_TIMEVAL_TO_TIMESPEC(&(vap)->va_ctime, tsp)
#define VATTR_SET_CTIME_TV(vap, tvp)	(vap)->va_ctime = *(tvp)
#define VATTR_SET_CTIME_TS(vap, tsp)	MDKI_TIMESPEC_TO_TIMEVAL(tsp, &(vap)->va_ctime)

#define VATTR_GET_RDEV(vap)		((vap)->va_rdev)
#define VATTR_SET_RDEV(vap, dev)	(vap)->va_rdev = (dev)
#define VATTR_FILL(vap) 0               /* nothing to do */

#define MDKI_TIMESPEC_TO_TIMEVAL(tsp, tvp) { \
	(tvp)->tv_sec = (tsp)->tv_sec; \
	(tvp)->tv_usec = (tsp)->tv_nsec / 1000; \
    }

#define MDKI_TIMEVAL_TO_TIMESPEC(tvp, tsp) { \
	(tsp)->tv_sec = (tvp)->tv_sec; \
	(tsp)->tv_nsec = (tvp)->tv_usec * 1000; \
    }

typedef enum vcexcl {
    NONEXCL, EXCL
} VCEXCL_T;

typedef enum symfollow {
    NO_FOLLOW, FOLLOW_LINK
} SYMFOLLOW_T;

#define VNODE_LF_LOOKUP  (1 << 0)
#define VNODE_LF_AUDIT   (1 << 1)

#define VNODE_NFS_FH_TYPE_RESERVED 0
#define VNODE_NFS_FH_TYPE_ERROR 255     /* see <linux/fs.h> dentry_to_fh() */

#define VFS_T struct vfs
#define VFSOPS_T struct vfsops
#define VNODE_T struct mdki_vnode
#define V_OP_T struct vnodeops
#include <linux/statfs.h>
#define LINUX_STATFS_T  struct kstatfs
#define STATVFS_T LINUX_STATFS_T

typedef u_short mdki_vtype_t;
#define VTYPE_T mdki_vtype_t

#define VNON			0
#define VREG			1
#define VDIR			2
#define VBLK			3
#define VCHR			4
#define VLNK			5
#define VFIFO			6
#define VSOCK			7
#define VBAD			8

struct vfs;
struct vfsops;
struct mdki_vnode;
struct vnodeops;

#define MVFS_MAJOR_T int
#define MOFFSET_T loff_t
#define MVFS_FMT_MOFFSET_T_X KS_FMT_OFF_T_X
#define MVFS_FMT_MOFFSET_T_D KS_FMT_OFF_T_D

#define MVFS_VFSISMVFS(vfsp, mfs_vfsopp) ((vfsp)->vfs_fstype == VFS_MVFS)

/* mvfs_linux_utils.c */
extern void
mdki_mark_vfs_dirty(VFS_T *vfsp);
extern void
mdki_inactive_finalize(VNODE_T *vp);
extern int
mdki_linux_statvfs_vnode(
    VNODE_T *cvn,
    STATVFS_T *sbp
);

extern int
init_mvfs_module(void);
extern void
cleanup_mvfs_module(void);

enum uio_rw {UIO_READ, UIO_WRITE};
typedef enum uio_rw uio_rw_t;
typedef struct iovec iovec_t;

typedef int mdki_boolean_t;

/*
 * UIO definitions.
 * This is either really ugly or a clever hack, your choice.  Linux doesn't
 * have a uio structure per-se.  It handles user I/O in a more ad-hoc manner.
 * At least for readdir, and probably for other functions as well, there
 * isn't a good mapping.  In readdir, we are called with a pointer to an
 * opaque data buffer and a pointer to a function to fill it in.  However,
 * mfs_readdir expects a pointer to a uio structure and does the actual
 * I/O from the lowest levels.  What I will do is to define a uio structure
 * that contains offsets that correspond roughly to a standard uio structure
 * and add to it useful offsets for Linux.  The various UIO macros will
 * then take care of doing the right thing for their requirements.
 */

typedef struct uio {
    iovec_t *uio_iov;               /* List of iovectors */
    int uio_iovcnt;                     /* Number of iovectors in list */
    /* First we add the canonical offsets as needed */
    int uio_resid;                      /* residual count */
    loff_t uio_offset;		/* File offset */
    int uio_segflg;                     /* segment flag - location of I/O buf */
    /* Then we add our extra stuff as needed.  This could become
     * a union structure here if this gets too unwieldy.
     */
    void *uio_buff;			/* opaque buffer */
    void *uio_func;			/* pointer to function to use */
    mdki_boolean_t uio_rddir_full;      /* TRUE if readdir filled the buffer */
} uio_t;

#define UIO_USERSPACE 1
#define UIO_SYSSPACE  2

/*
 * MVFS_LINUX_MAXRPCDATA is the maximum size for RPC data.  This is used
 * in linux_fop_readdir to set the buffer size when making the readdir
 * call.  It is also used in mvfs_mdep_linux.c when setting up the 
 * table of rpc reply sizes. 
 */

#define MVFS_LINUX_MAXRPCDATA    8192

extern int
mdki_linux_readlink_uiomove(
    char *from,
    int cnt,
    int uio_flag,
    uio_t *uiop
);

/* A debug tool that Linux doesn't define for the kernel
 */

extern void
mdki_assfail(
    const char *fmt,
    const char *asserted_str,
    const char *filename,
    const char *funcname,
    int lineno
) __attribute__((noreturn));

typedef struct mvfs_linux_vattr {
	u_int		va_mask;	/* Bit mask of attributes */
	VTYPE_T	        va_type;	/* Vnode type (mode & S_IFMT) */
	VATTR_MODE_T	va_mode;	/* file access mode (& ~S_IFMT) */
	VATTR_UID_T	va_uid;		/* owner user id */
	VATTR_GID_T	va_gid;		/* owner group id */
	dev_t           va_fsid;	/* file system id (dev for now) */
	dev_t           va_rdev;	/* device the file represents */
	u_long		va_nodeid;	/* node id */
	u_long		va_nlink;	/* number of links to file */
	VATTR_SIZE_T	va_size;	/* file size in bytes */
	struct timeval  va_atime;	/* access time */
	struct timeval  va_mtime;	/* mod time */
	struct timeval  va_ctime;	/* change vnode time */
	u_long		va_blksize;	/* block size */
	u_long		va_nblocks;	/* allocation size in blocks */
} vattr_t;

typedef struct semaphore mdki_sleeplock_t;

/*
 * should ISLOCKED count sema.waking as available?
 * I'd say no, since if it's set it means that someone is about to wake up
 * and get the lock anyway.
 */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27) 
# define MDKI_SLEEP_ISLOCKED(semap) (atomic_read(&(semap)->count) < 1)
#else
# define MDKI_SLEEP_ISLOCKED(semap) ((semap)->count < 1)
#endif
#define MDKI_SLEEP_LOCK(semap) down(semap)
#define MDKI_SLEEP_UNLOCK(semap) up(semap)
#define MDKI_SLEEP_TRYLOCK(semap) down_trylock(semap)

# if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,33)
#   define MDKI_INIT_SLEEPLOCK(semap) init_MUTEX(semap)
#   define MDKI_DECLARE_SLEEPLOCK(name) DECLARE_MUTEX(name)
# else
#   define MDKI_INIT_SLEEPLOCK(semap) sema_init(semap, 1)
# if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,36)
#   define MDKI_DECLARE_SLEEPLOCK(name) DEFINE_SEMAPHORE(name, 1)
# else
#   define MDKI_DECLARE_SLEEPLOCK(name) DEFINE_SEMAPHORE(name)
# endif /* else < KERNEL_VERSION(2,6,36)  */
# endif /* LINUX_VERSION_CODE < KERNEL_VERSION(2,6,33) */

#define MDKI_FREE_SLEEPLOCK(semap) /* nothing to do */

extern void
mdki_makevfsdev(
    VFS_T *vfsp,
    int major,
    int minor,
    int nvmajor,
    int nvminor
);

extern int
mdki_vfs_to_dev(VFS_T *vfsp);

extern int
mdki_vcount(VNODE_T *vp);

#define V_COUNT(vp)     mdki_vcount(vp)


extern VNODE_T *
mdki_vn_hold(VNODE_T *vp);
#ifdef MVFS_DEBUG
extern VNODE_T *
mdki_debug_vn_hold(
    VNODE_T *vp,
    char *file,
    const char *function,
    int line
);
#define VN_HOLD(vp) mdki_debug_vn_hold(vp, __FILE__, __func__, __LINE__)
#else
#define VN_HOLD(vp) mdki_vn_hold(vp)
#endif

/* The following is a debug version of this.  The do {...} while(0)
 * construct is to prevent compile errors in code fragments like this
 * if (vw) VN_RELE(vw);
 * (This can be an inline macro because it returns no value.)
 */

#ifdef MVFS_DEBUG
#define VN_RELE(vp) do {                                                \
    ASSERT(vp != NULL);                                                 \
    MDKI_TRACE(TRACE_VNODES,"mdki_vn_rele %s:%s:%d, vp=%p cnt=--%d\n",  \
               __FILE__, __func__, __LINE__, vp, V_COUNT(vp));          \
    ASSERT(V_COUNT(vp) != 0);                                           \
    mdki_vn_rele(vp);                                                   \
} while (0)
#else
#define VN_RELE(vp) mdki_vn_rele(vp)
#endif

#define REAL_VN_RELE(vp) mdki_vn_rele(vp)
extern void
mdki_vn_rele(VNODE_T *vp);

extern mdki_boolean_t
mdki_vpismfs(VNODE_T *vp);

extern void
mdki_decrement_link(VNODE_T *vp);
extern void
mdki_increment_link(VNODE_T *vp);

#if 0 /* disable, it won't work for NFS */
extern void
mdki_linux_stop_paging(
    VNODE_T *vp
);
extern void
mdki_linux_start_paging(
    VNODE_T *vp
);
#endif

#define FLAG_NODATASYNC 0               /* conversion of our flags to theirs */
#define FLAG_DATASYNC 1

struct unlink_ctx;

extern mdki_boolean_t
mdki_linux_rename_needed(
    VNODE_T *vp,
    struct unlink_ctx * context
);

extern VNODE_T *
mdki_make_sysroot_vnode(void);
extern void
mdki_linux_release_majors(MVFS_MAJOR_T *tbl);
extern void
mdki_release_sysroot_vnode(VNODE_T *sysrootvp);

#define MVFS_CVP_TO_VP(cvp) (cvp)

#define CVN_CMP(vp1, vp2)	((vp1) == (vp2) ||                      \
                                 ((vp1)->v_dent == (vp2)->v_dent &&     \
                                  (vp1)->v_vfsmnt == (vp2)->v_vfsmnt))
extern int
mvop_linux_realvp(
    VNODE_T *vp,
    VNODE_T **rvp
);

extern void
mdki_make_dcache(
    VNODE_T *dvp,
    VNODE_T *vp,
    const char *nm
);

extern void
mdki_rm_dcache(
    VNODE_T *vp
);

extern void
mdki_start_flush_dcache(
    VNODE_T *vp,
    void **arg
);

extern mdki_boolean_t
mdki_finish_flush_dcache(
    void *arg
);

extern MVFS_MAJOR_T
mdki_linux_get_major(void);

extern void
mdki_linux_release_majors(
    MVFS_MAJOR_T *tbl
);
extern int
mdki_makedevice(
    int major,
    int minor
);
extern int
mdki_major(
    int dev
);
extern int
mdki_minor(
    int dev
);
/* hook to clean up attributes in inode whenever we get them */
extern void
mdki_linux_vattr_pullup(
    VNODE_T *vp,
    VATTR_T *vap,
    int mask
);
extern int
mdki_sync_size(
    VNODE_T *vp,
    loff_t size,
    int actual
);
extern int
mdki_set_modified(
    VNODE_T *vp,
    VATTR_T *vap
);
extern int
mdki_set_accessed(
    VNODE_T *vp,
    VATTR_T *vap
);
extern int
mdki_set_ichg(
    VNODE_T *vp,
    VATTR_T *vap
);

/* FIXME:  We need to look at these routines.  They can cause faults. */
#define COPYIN(from,to,count) \
        mdki_copy_from_user((to),(from),(unsigned long)(count))
#define COPYOUT(from,to,count) \
        mdki_copy_to_user((to),(from),(unsigned long)(count))

#define REAL_KMEM_ALLOC(bsize,flag)  mdki_linux_kmalloc((bsize),(flag))
#define REAL_KMEM_FREE(ptr,bsize)    mdki_linux_kfree((ptr),(bsize))

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
#define MVFS_KMEM_CACHE_T kmem_cache_t
#else
#define MVFS_KMEM_CACHE_T struct kmem_cache
#endif

#ifndef KMEMDEBUG

#define KMEM_ALLOC(bsize,flag) REAL_KMEM_ALLOC(bsize,flag)
#define KMEM_FREE(ptr,bsize)   { REAL_KMEM_FREE(ptr,bsize); (ptr) = NULL; }
#define STRFREE(str)           { REAL_KMEM_FREE((str),0); (str) = NULL; }
#define STRDUP(str)            mfs_strdup(str)

#else /* KMEMDEBUG */

#define KMEM_ALLOC(bsize,flag)          mfs_kalloc(bsize, flag, mdki_getreturn(),mdki_getmycaller())
#define KMEM_FREE(ptr,bsize) \
        { mfs_kfree(ptr,bsize,mdki_getreturn(),mdki_getmycaller()); (ptr) = NULL; }
#define STRDUP(str)                     mfs_kmstrdup(str, mdki_getreturn(),mdki_getmycaller())
#define STRFREE(str)    \
        { mfs_kfree(str, STRLEN(str)+1, mdki_getreturn(),mdki_getmycaller()); (str) = NULL; }

#endif  /* KMEMDEBUG */

#define KM_SLEEP        1
#define KM_NOSLEEP      0
#define KM_PAGED        0               /* to be ORed with others */

extern int
mdki_linux_page_size(void);

extern void
mdki_linux_panic(
    const char *fmt,
    ...
) __attribute__((noreturn, format (printf, 1, 2)));

extern const char *mdki_panicstr;

extern void
mdki_invalidate_vnode_pages(VNODE_T *vp);

/* Credentials */
/* Linux doesn't have a real cred structure.  They have file system
 * creds stored in the task structure.  We will make our own structure
 * and fill it in as needed.
 */

#define MDKI_NGROUPS 32                 /* big enough to handle Linux group lists */
typedef struct vnode_cred {
        atomic_t cr_ref;            /* reference count */
        uid_t   cr_euid;            /* effective user id */
        gid_t   cr_egid;            /* effective group id */
        uid_t   cr_ruid;            /* real user id */
        gid_t   cr_rgid;            /* real group id */
        uid_t   cr_suid;            /* "saved" user id (from exec) */
        gid_t   cr_sgid;            /* "saved" group id (from exec) */
        uid_t   cr_fsuid;           /* file system user id */
        gid_t   cr_fsgid;           /* file system group id */
        unsigned int cr_ngroups;        /* number of groups in cr_groups */
        gid_t   cr_groups[MDKI_NGROUPS]; /* supplementary group list */
} vnode_cred_t;

#define CRED_T  vnode_cred_t
#define CRED_UID_T uid_t
#define CRED_GID_T gid_t

#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
extern CRED_T *
mdki_dup_default_creds(
    const char *file,
    const char *func,
    int line
);

extern CRED_T *
mdki_crdup(
    CRED_T *cred,
    const char *file,
    const char *func,
    int line
);
extern void
mdki_crfree(
    CRED_T *cred,
    const char *file,
    const char *func,
    int line
);
extern void
mdki_crhold(
    CRED_T *cred,
    const char *file,
    const char *func,
    int line
);

/* For now, we will always copy the creds from the task structure */
#define MDKI_GET_UCRED()                MDKI_DUP_UCRED()
#define MDKI_DUP_UCRED()  		mdki_dup_default_creds(__FILE__, __func__, __LINE__)
#define MDKI_CRDUP(c)			mdki_crdup(c, __FILE__, __func__, __LINE__)
#define MDKI_CRFREE(c)			mdki_crfree(c, __FILE__, __func__, __LINE__)
#define MDKI_CRHOLD(c)			mdki_crhold(c, __FILE__, __func__, __LINE__)

#else
extern CRED_T *
mdki_dup_default_creds(void);
extern CRED_T *
mdki_crdup(CRED_T *cred);
extern void
mdki_crfree(CRED_T *cred);
extern void
mdki_crhold(CRED_T *cred);

/* For now, we will always copy the creds from the task structure */
#define MDKI_GET_UCRED()                MDKI_DUP_UCRED()
#define MDKI_DUP_UCRED()  		mdki_dup_default_creds()
#define MDKI_CRDUP(c)			mdki_crdup(c)
#define MDKI_CRFREE(c)			mdki_crfree(c)
#define MDKI_CRHOLD(c)			mdki_crhold(c)
#endif

#define MDKI_CR_GET_UID(c)      (c)->cr_fsuid
#define MDKI_CR_GET_GID(c)      (c)->cr_fsgid
#define MDKI_CR_GET_GRPLIST(c)  (c)->cr_groups
#define MDKI_CR_END_GRPLIST(c)  &(c)->cr_groups[(c)->cr_ngroups]
/* We want to check the file system UID, not the true effective UID */
#define MDKI_CR_IS_SETUID_ROOT(c) ((c)->cr_fsuid == MDKI_ROOT_UID && (c)->cr_ruid != MDKI_ROOT_UID)
#define MDKI_CR_SET_E2RUID(c)   (c)->cr_fsuid = (c)->cr_ruid
#define MDKI_CR_SET_E2ROOTUID(c) (c)->cr_fsuid = MDKI_ROOT_UID
#define MDKI_CR_IS_ROOT(c)      ((c)->cr_fsuid == MDKI_ROOT_UID)
/* FIXME:  There is a potential buffer overrun problem here if we ever
 * create a cred with less than the full cr_groups array.  So far we
 * don't so it's not a problem, but we have to keep this in mind.
 */
#define MDKI_CR_EQUAL(c1,c2)                                    \
  (BCMP(&(c1)->cr_euid, &(c2)->cr_euid,                         \
       (caddr_t)&(c1)->cr_groups - (caddr_t)&(c1)->cr_euid +    \
       ((c1)->cr_ngroups)*sizeof((c1)->cr_groups[0])) == 0)

#define MDKI_ROOT_UID   0
#define MDKI_ROOT_GID   0

#define VNODE_LASTCLOSE_T	int
#define VNODE_LASTCLOSE_COUNT 0
#define VNODE_NOT_LASTCLOSE_COUNT 1

extern int
mdki_linux_proc_setview(
    VNODE_T *vw,
    VNODE_T *viewdir,
    const char *viewname,
    int *status_ok
);
extern VNODE_T *
mdki_linux_get_procview(void);
extern VNODE_T *
mdki_get_urdir_vnode(void);

extern VNODE_T *
mdki_get_ucdir_vnode(void);

static inline const char *
mdki_get_ucomm_ptr(void)
{   
    return(current->comm);
} 

static inline int 
mdki_get_ucmask(void)
{
    return(current->fs->umask);
}

extern int
mdki_linux_readdir_uiomove(
    char *from,
    int cnt,
    int uio_flag,
    uio_t *uiop,
    loff_t offset
);

struct lookup_ctx;

extern int
mvop_linux_lookup_component(
    VNODE_T *dvp,
    char *nm,
    VNODE_T **vpp,
    CRED_T *cred,
    struct lookup_ctx * ctx
);

extern void
mdki_get_rootdir(void *ctx);

extern int
mdki_linux_maxnamelen(void);

extern int
mdki_linux_maxpathlen(void);

/*
 * MVOP routines which need our type definitions
 */
extern int
mvop_linux_open_kernel(
    VNODE_T **vpp,                  /* call/return */
    int flags,
    CRED_T *cred,
    void **filp                         /* RETURN */
);

extern int
mvop_linux_close_kernel(
    VNODE_T *vp,
    int flags,
    VNODE_LASTCLOSE_T count,
    MOFFSET_T off,
    CRED_T *cred,
    void *filp,
    fl_owner_t owner_id
);

extern int
mvop_linux_read_kernel(
    struct uio *uiop,
    int flags,
    CRED_T *cred,
    char *filp
);

extern ssize_t
mvop_linux_write_kernel(
    struct uio *uiop,
    int flags,
    CRED_T *cred,
    char *filp
);

extern int
mvop_linux_lookup_ioctl(
    /* mfs_pn_char_t */char *path,
    int segflag,
    SYMFOLLOW_T follow,
    VNODE_T **dvpp,
    VNODE_T **vpp,
    CRED_T *cred
);

extern int
mvop_linux_lookup_storage_file(
    char *path,
    VNODE_T **vpp,                  /* return */
    CRED_T *cred
);

extern int
mvop_linux_fsync_kernel(
    VNODE_T *vp,
    int flag,
    CRED_T *cred,
    void *filp
);

/* read/write/access flags */

#define VREAD	0400
#define VWRITE	0200
#define VEXEC	0100

#define FREAD   0x0001
#define FWRITE  0x0002
#define FCREAT  0x1000
#define FTRUNC  0x2000
#define FAPPEND 0x4000

/* undefine them, IA64 defines them to no good end */
#undef MAP_SHARED
#undef PROT_READ
#undef PROT_WRITE
#undef PROT_EXEC
#define MAP_SHARED 0x00000001
#define PROT_READ VREAD
#define PROT_WRITE VWRITE
#define PROT_EXEC VEXEC

extern VFS_T *mdki_logging_vfsp;

extern void
mdki_set_clrvnode_vfsp(VFS_T *vfsp);
extern void
mdki_clear_clrvnode_vfsp(VFS_T *vfsp);
extern void
mdki_set_logging_vfsp(VFS_T *vfsp);
extern void
mdki_clear_logging_vfsp(VFS_T *vfsp);
extern void
mdki_set_looproot_vp(VNODE_T *vp);
extern void
mdki_clear_looproot_vp(VNODE_T *vp);

#define MDKI_VFS_LOG(level, str, ...) {                 \
    if (mdki_logging_vfsp != NULL)                           \
        VFS_LOG(mdki_logging_vfsp, level, str, __VA_ARGS__); \
    else                                                \
        mdki_linux_printf(str, __VA_ARGS__);            \
}

#define LOG_PROBLEM(str) MDKI_VFS_LOG(VFS_LOG_ERR,"%s: " str "\n", __func__ )
#define NOT_IMPLEMENTED() LOG_PROBLEM("not implemented yet")

#define VOPBD_T void
#define ROOTDIR_T int

#define TRACE_VNODES		0x00000001
#define TRACE_DCACHE            0x00000002
#define TRACE_XDR               0x00000004
#define TRACE_USERLOOKUP        0x00000008
#define TRACE_PNLOOKUP          0x00000010
#define TRACE_RPC               0x00000020
#define TRACE_CREDS             0x00000040
#define TRACE_RDWR              0x00000080
#define TRACE_OPEN              0x00000100
#define TRACE_CLOSE             0x00000200
#define TRACE_MAP               0x00000400
#define TRACE_ACCESS            0x00000800
#define TRACE_GETATTR           0x00001000
#define TRACE_SETATTR           0x00002000
#define TRACE_INACTIVE          0x00004000
#define TRACE_READDIR           0x00008000
#define TRACE_REMOVE            0x00010000
#define TRACE_VFSMNT            0x00020000
#define TRACE_PROC              0x00040000

extern unsigned int mdki_tracing;

#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
#define MDKI_TRACE(subsys, fmt, ...) do {                               \
    if ((mdki_tracing & (subsys)) != 0)                                 \
        MDKI_VFS_LOG(VFS_LOG_DEBUG, #subsys ": " fmt, __VA_ARGS__);     \
} while (0)
#else /* no logging */
#define MDKI_TRACE(subsys, fmt, ...) /*nothing*/
#endif

/*
 * Make sure we can see "static" symbols
 * (if really static, kernel debugger doesn't have them)
 */
#ifdef MVFS_DEBUG
#undef STATIC
#define STATIC /**/
#else
#define STATIC static
#endif

extern caddr_t
mdki_getreturn(void);
extern caddr_t
mdki_getmycaller(void);
extern caddr_t
mdki_getmycallerscaller(void);

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define MVFS_MAXOFF_T 0x7fffffffffffffffLL

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
# define MDKI_TASKLIST_LOCK()        read_lock(&tasklist_lock)   
# define MDKI_TASKLIST_UNLOCK()      read_unlock(&tasklist_lock)
#else
# define MDKI_TASKLIST_LOCK()        rcu_read_lock()
# define MDKI_TASKLIST_UNLOCK()      rcu_read_unlock()
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,32)
# define MDKI_GET_CURRENT_FSUID() current->fsuid
# define MDKI_GET_CURRENT_FSGID() current->fsgid
# define MDKI_GET_CURRENT_EUID()  current->euid
# define MDKI_GET_CURRENT_EGID()  current->egid
# define MDKI_GET_CURRENT_UID()   current->uid
# define MDKI_GET_CURRENT_GID()   current->gid
# define MDKI_GET_CURRENT_SUID()  current->suid
# define MDKI_GET_CURRENT_SGID()  current->sgid
#else
# define MDKI_GET_CURRENT_FSUID() current_fsuid()
# define MDKI_GET_CURRENT_FSGID() current_fsgid()
# define MDKI_GET_CURRENT_EUID()  current_euid()
# define MDKI_GET_CURRENT_EGID()  current_egid()
# define MDKI_GET_CURRENT_UID()   current_uid()
# define MDKI_GET_CURRENT_GID()   current_gid()
# define MDKI_GET_CURRENT_SUID()  current_suid()
# define MDKI_GET_CURRENT_SGID()  current_sgid()
#endif
            
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
/*
 * Use d_set_d_op instead of direct assigment, 
 * so we don't need to adjust the various flags.
 */
#if defined(MRG) && (LINUX_VERSION_CODE >= KERNEL_VERSION(3,0,25))
# define MDKI_SET_DOPS(D, OPS)   do {\
                                    seq_spin_lock(&(D)->d_lock);\
                                    d_set_d_op((D), (OPS));\
                                    seq_spin_unlock(&(D)->d_lock);\
                                } while(0)
#else /* defined(MRG) && (LINUX_VERSION_CODE >= KERNEL_VERSION(3,0,25)) */
# define MDKI_SET_DOPS(D, OPS)   do {\
                                    spin_lock(&(D)->d_lock);\
                                    d_set_d_op((D), (OPS));\
                                    spin_unlock(&(D)->d_lock);\
                                } while(0)
#endif /* else defined(MRG) && (LINUX_VERSION_CODE >= KERNEL_VERSION(3,0,25)) */
#define MDKI_UNSET_DOPS(D)      do {(D)->d_flags &= ~(DCACHE_OP_HASH | \
                                                      DCACHE_OP_COMPARE | \
                                                      DCACHE_OP_REVALIDATE | \
                                                      DCACHE_OP_DELETE); \
                                    (D)->d_op = NULL; \
                                }while(0)
#else
#define MDKI_SET_DOPS(D, OPS)   (D)->d_op = (OPS)
#define MDKI_UNSET_DOPS(D)      MDKI_SET_DOPS((D), NULL)
#endif

typedef MDKI_GEN_TYPE(vnode_kdirent_t);

#define KDIRENT_T	vnode_kdirent_t
#define KDIRENT_BASESIZE (mdki_dirent_basesize())

#define KDIRENT_RECLEN(namelen) mdki_linux_dirent_reclen(namelen)

#define KDIRENT_INIT(dentp, ino, nm, nmlen, next_offset) \
    mdki_linux_dirent_init(dentp, ino, nm, nmlen, next_offset)
#define KDIRENT_GET_INO(dp)     mdki_linux_dirent_get_ino(dp)
#define KDIRENT_GET_OFF(dp)     mdki_linux_dirent_get_off(dp)
#define KDIRENT_GET_NAMLEN(dp)  mdki_linux_dirent_get_namlen(dp)
#define KDIRENT_GET_RECLEN(dp)  mdki_linux_dirent_get_reclen(dp)
#define KDIRENT_GET_NAME(dp)    mdki_linux_dirent_get_name(dp)
#define KDIRENT_SET_INO(dp,ino) mdki_linux_dirent_set_ino(dp,ino)
#define KDIRENT_SET_OFF(dp,off) mdki_linux_dirent_set_off(dp,off)
#define KDIRENT_SET_NAMLEN(dp, nmlen)   /* no namlen in struct dirent */
#define KDIRENT_SET_RECLEN(dp, nmlen) mdki_linux_dirent_set_reclen(dp, nmlen)

extern int
mdki_linux_dirent_basesize(void);
extern int
mdki_linux_dirent_reclen(int namelen);
extern void
mdki_linux_dirent_init(
    KDIRENT_T *dp,
    long ino,
    const char *nm,
    int nmlen,
    long long next_offset
);

extern long
mdki_linux_dirent_get_ino(KDIRENT_T *dp);
extern long long
mdki_linux_dirent_get_off(KDIRENT_T *dp);
extern int
mdki_linux_dirent_get_namlen(KDIRENT_T *dp);
extern int
mdki_linux_dirent_get_reclen(KDIRENT_T *dp);
extern char *
mdki_linux_dirent_get_name(KDIRENT_T *dp);
extern void
mdki_linux_dirent_set_ino(
    KDIRENT_T *dp,
    long ino
);
extern void
mdki_linux_dirent_set_off(
    KDIRENT_T *dp,
    long long off
);
extern void
mdki_linux_dirent_set_reclen(
    KDIRENT_T *dp,
    int nmlen
);

#define mfs_uioset mdki_linux_uioset
extern void
mdki_linux_uioset(
    struct uio *uiop,
    caddr_t addr,
    size_t size,
    MOFFSET_T offset,
    int segflg
);

#define MDKI_CTIME()            mdki_ctime()
#define MDKI_HRTIME(tsp)        mdki_hrtime(tsp)

#define MVFS_DEF_BLKSIZE 4096
#define MVFS_DEF_BLKSIZE_BITS 12
#define MVFS_DEF_MAX_FILESIZE 0x7fffffffffffffffLL

extern MVFS_KMEM_CACHE_T *vnlayer_vnode_cache;

extern int
mdki_set_vfs_opvec(struct vfsops *vfsopp);

extern void
mdki_clear_vfs_opvec(struct vfsops *vfsopp);

#ifdef RATL_COMPAT32
struct ioctl_ctx; /* this type comes from vnode interface */
extern mdki_boolean_t
mdki_curproc_is_32bit(struct ioctl_ctx *);
#endif

extern int
mdki_linux_mdep_init(void);

extern void
mdki_linux_mdep_unload(void);

extern int
mdki_linux_set_vobrt_vfsmnt(const char *vpath);

#ifdef MVFS_DEBUG
extern void
mdki_linux_chksp(unsigned long val);
#define MFS_CHKSP(g) mdki_linux_chksp(g)
#else
#define MFS_CHKSP(g)
#endif

extern void *
mdki_linux_kmalloc(
    size_t size,
    int flag
);
extern void
mdki_linux_kfree(
    void *ptr,
    size_t size
);

extern void *
mdki_memset(
    void *to,
    int value,
    size_t size
);

#define wrap_printk mdki_linux_printf

extern void
mdki_linux_printf(
    const char *fmt,
    ...
) __attribute__((format (printf, 1, 2)));

extern void
mdki_delay_usec(unsigned long us);

extern size_t
mdki_strlen(const char *str);

extern time_t
mdki_ctime(void);

extern u_long
mdki_physmem_pagecnt(void);

extern int
mdki_memcmp(
    const void *b1,
    const void *b2,
    size_t size
);

extern void
mdki_hrtime(
    struct timespec *value
);

extern unsigned long
mdki_copy_from_user(
    void *to,
    void *from,
    unsigned long n
);

extern unsigned long
mdki_copy_to_user(
    void *to,
    void *from,
    unsigned long n
);

extern void *
mdki_memcpy(
    void *to,
    const void *from,
    size_t size
);

extern char *
mdki_strcpy(
    char *to,
    const char *from
);

extern char *
mdki_strncpy(
    char *to,
    const char *from,
    size_t size
);

extern int
mdki_suser(void);

typedef MDKI_PTR_TYPE(mvfs_proctag_t);
typedef MDKI_GEN_TYPE(mvfs_process_t);

typedef long mvfs_procid_t;

/* The following functions are inlined because they are called often
 * and they make no use of the stack.  This should improve usage of the
 * L1 cache.
 */

static inline mvfs_procid_t
mdki_curpid(void)
{
    return((mvfs_procid_t)current->pid);
}
    
static inline mvfs_process_t *
mdki_curproc(void)
{
    return((mvfs_process_t *)current);
}

static inline mvfs_process_t *
mdki_get_parent(mvfs_process_t *p)
{
    struct task_struct *task = (struct task_struct *) p;
#if defined(RATL_REDHAT) || (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0))
    return((mvfs_process_t *)task->parent);
#else
    return((mvfs_process_t *)task->p_pptr);
#endif
}

static inline mvfs_procid_t
mdki_get_proc_pid(mvfs_process_t *p)
{
    struct task_struct *task = (struct task_struct *) p;
    return((mvfs_procid_t)task->pid);
}


static inline mvfs_proctag_t
mdki_get_proctag(mvfs_process_t *p)
{   
    struct task_struct *task = (struct task_struct *) p;
    return((mvfs_proctag_t)task->fs);
}

extern mdki_boolean_t
mdki_linux_procactive(mvfs_process_t *p);

extern mdki_boolean_t
mdki_linux_procexists(
    mvfs_procid_t pid,
    mvfs_process_t *proc
);

extern inline unsigned long
mdki_get_proc_state(mvfs_process_t *p)
{
    struct task_struct *task = (struct task_struct *) p;
#ifdef EXIT_ZOMBIE
    /* In RHEL4 Update 1 Red Hat moved the TASK_ZOMBIE and TASK_DEAD
     * flags from the state field to the exit_state field and renamed
     * them EXIT_ZOMBIE and EXIT_DEAD.  For now, the bit offsets have
     * not changed and the bits that were moved out of the state word
     * have not been reused.  Of course, this can change at any time
     * but until then, this simple minded fix should work.
     */
    return(task->state | task->exit_state);
#else
    return(task->state);
#endif
}
 
/*
 * We use this in places where we switch between UNIX convention
 * (positive error codes) and Linux convention (negative error codes).
 */
#define mdki_errno_unix_to_linux(err) (-(err))

#include "linux_krpc.h"

extern void
mdki_linux_clnt_set_intr(
    CLIENT *cl,
    bool_t intr
);
extern void
mdki_linux_clnt_call(
    CLIENT *client,
    int procnum,
    void *args,
    void *results,
    int rpctimeout,
/*    MDKI_SIGMASK_T *holdsigs,*/
    bool_t intr,
    CRED_T *cred,
    enum clnt_stat *status
);

extern int
mdki_linux_destroy_client(CLIENT *cl);

extern int
mdki_linux_clntkudp_init(
    CLIENT *cl,
    struct sockaddr *addr,
    int retrans_count,
    bool_t intr
);

extern int
mdki_linux_createvp(
    char *pn,
    VATTR_T *vap,
    VNODE_T **vpp,
    CRED_T *cred,
    int flag
);

#define BCOPY(from,to,c) memcpy((char *)(to), (char *)(from), (size_t)(c))
#define BZERO(p,c)      memset((caddr_t)(p),0,(size_t)(c))
#define BCMP(p1,p2,c)   mdki_memcmp((char *)(p1), (char *)(p2), (size_t)(c))
#define STRLEN          strlen
#define STRCPY          strcpy
#define STRCMP          strcmp
#define STRNCPY         strncpy
#define STRNCMP         strncmp
#define STRRCHR         strrchr

#ifndef __x86_64__
/* x86_64 is special for memcmp, see mvfs_linux_builtins.c */
#define mdki_memcmp memcmp
#endif

extern mdki_boolean_t
mdki_cvn_is_stable(VNODE_T *cvn);

/* mvfs_view_shift_bits is in mvfs_param.c.  It is used by the MVFS in
 * calculating the value for mdki_maxminor during MVFS initialization.
 */
extern u_int
mvfs_view_shift_bits;
extern int mdki_maxminor;

typedef MDKI_GEN_TYPE(file_ctx);
typedef MDKI_GEN_TYPE(dent_ctx);
typedef MDKI_GEN_TYPE(nameidata_ctx);

/* We need our own implementation of xdr_opaque (used in the core), so here is
** the declaration (definition in mvfs_linux_xdr_opaque.c).
*/
extern bool_t
mdki_xdr_opaque(
    XDR *x,
    char *objp,
    u_int obj_size
);

/* This is used by the vnode layer and the core for a startup message. */
extern const char mdki_vnode_build_time[];

extern int
mdki_snprintf(
    char *str,
    size_t limit,
    const char *fmt,
    ...
);

extern int
mdki_vsnprintf(
    char *str,
    size_t limit,
    const char *fmt,
    va_list ap
);

#ifdef RATL_COMPAT32
extern int
mdki_handle_ioctl(unsigned int);
extern void
mdki_unhandle_ioctl(unsigned int);
#endif
#if defined(MVFS_DEBUG) || defined(STACK_CHECKING)
extern void *
mdki_get_stack_check_id(void);
#endif

struct seek_ctx;

extern mdki_boolean_t
mdki_seek_ctx_open_for_lfs(
     struct seek_ctx *ctx
);

extern mdki_boolean_t
mdki_file_ctx_open_for_lfs(
     file_ctx *ctx
);

/* The mvfs_call_data structure is used to passed thread and cred informaton
 * to users who need both.  In many cases, it is passed around instead of a
 * simple thread structure.  This reduces the number of times that we have to
 * call mvfs_enter_fs and mvfs_exit_fs.  See the comments in mvfs_systm.h for
 * how the functions and macros defined here should be used.
 */

struct mvfs_thread;

typedef struct mvfs_call_data {
    vnode_cred_t *cred;
    struct mvfs_thread *thr;
} mvfs_call_data_t;

#define CALL_DATA_T mvfs_call_data_t
#define MVFS_CD2CRED(cb) (cb)->cred
#define MVFS_CD2THREAD(cb) (cb)->thr

extern void
mdki_linux_init_call_data(CALL_DATA_T *cd);
extern void
mdki_linux_destroy_call_data(CALL_DATA_T *cd);
extern CALL_DATA_T *
mdki_linux_make_substitute_cred(
    CALL_DATA_T *ocd,
    CRED_T *cred
);
extern void
mdki_linux_free_substitute_cred(
    CALL_DATA_T *cd
);

#define MVFS_ALLOC_SUBSTITUTE_CRED(CD,CR) mdki_linux_make_substitute_cred(CD,CR)
#define MVFS_FREE_SUBSTITUTE_CRED(CD) mdki_linux_free_substitute_cred(CD)

/* Declare functions that will manipulate the thread structure when
 * initializing and releasing call data structures.
 */

struct mvfs_thread * 
mvfs_get_thread_ptr(void);
void
mvfs_release_thread_ptr(struct mvfs_thread *thr);

/* this is in mvfs_vfsops.c, but we have to call it directly */
extern void *
mvfs_find_mount(
    void *(* eval_func)(VFS_T *vfsp, void *data),
    void *data
);

/* This is put here because it needs the definition of file_ctx. */

extern int
mvop_linux_lockctl(
    VNODE_T *vp,
    void *ld,
    int cmd,
    CALL_DATA_T *cd,
    file_ctx *ctx
);

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
/* These functions are declared here so they can be called by
** mvop_linux_lockctl().  However, they are defined in mvfs_mdep_linux.c (like
** init_mvfs_module() above).
*/
extern void
mvfs_linux_save_fl_owner(void *fl_owner);

extern void *
mvfs_linux_find_fl_owner(void);
#endif

extern int mvfs_major;
extern int mvfs_majdynmax;
extern int mvfs_majfixmax;
extern u_int mvfs_view_shift_bits;

/* 64-bit architectures use more stack space for their larger types.  In order
** to reduce stack usage in some cases we want to make sure some routines are
** not inlined.  Later versions of gcc allow a "noinline" keyword (which is a
** macro that expands to an attribute), so we use this macro to control what
** happens.  In particular, the SLES10 (and 2.6.14+ kernels in general) use the
** "-mwarn-framesize=256" option when building on s390x (which causes an error
** for us since we use "-Werror").
*/
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,14)) && \
    (defined(ATRIA_LP64) || defined(ATRIA_LLP64)) && defined(noinline)
#define MVFS_NOINLINE noinline
#else
#define MVFS_NOINLINE
#endif

/* The following macro will check if the current process has received
 * a signal.  It is used in the vwcall/rpc code to circumvent timeouts
 * and retries because the sunrpc code will not send RPCs if the process
 * has been signalled.  This is only implemented for kernels 2.6.27 
 * and higher because before that, the RPC code switched signal masks
 * so our signal pending mask does not match the RPC code so it is
 * of no use in determining if an EIO error was really ERESTARTSYS.
 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27))
#define MDKI_FATAL_SIGNAL_PENDING() (FALSE)
#else
#define MDKI_FATAL_SIGNAL_PENDING() fatal_signal_pending(current)
#endif

#endif /* MVFS_MDKI_H_ */
/* $Id: 28a0d345.ec6311e1.905b.00:01:84:c3:8a:52 $ */
