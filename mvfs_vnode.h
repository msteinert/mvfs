#ifndef _MVFS_VNODE_H_
#define _MVFS_VNODE_H_
/*
 * Copyright (C) 1994, 2012 IBM Corporation.
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
#if defined(ATRIA_LINUX) && LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
# define MDKI_FID fid
# define MDKI_FID_T fid_t
#else
   /*
    * renaming to coexist with other file systems that 
    * took these names already
    */
# define MDKI_FID mdki_fid
# define MDKI_FID_T mdki_fid_t
#endif /* ATRIA_LINUX && LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24) */

typedef struct MDKI_FID {           /* Dummy fid struct for lendgth */
	u_short		fid_len;		/* length of data in bytes */
	char 		fid_data[1];		/* data */
} MDKI_FID_T;

#define MDKI_FID_LEN(fidp) ((fidp)->fid_len + sizeof((fidp)->fid_len))
#define MDKI_FID_ALLOC_LEN(len) (len + sizeof(((MDKI_FID_T *)0)->fid_len))

typedef struct vfs {
	u_long		vfs_flag;		/* VFS flags */
	int		vfs_fstype;		/* VFS type */
	fsid_t          vfs_fsid;		/* file system id */
	struct vfsops  *vfs_op;			/* VFS operations */
	u_long		vfs_bsize;		/* VFS block size */
	caddr_t	        vfs_data;		/* private data */
	caddr_t         vfs_sb;
} vfs_t;

/*
 * VFS flags
 */
#define VFS_RDONLY	0x0001		/* VFS read-only */
#define VFS_NOSUID	0x0002		/* No setuid on VFS */
#define VFS_POSIXACL	0x8000		/* Support POSIX ACL's */

/*
 * Define the two types of VFS - MVFS vfs's and
 * even phonier VFS's for cleartext cover vnodes
 * These are put info "vfs_fstype", which is normally
 * the vfs switch table index.  This minimal implementation has no
 * vfs switch table, so these numbers are just tested
 * directly by code that cares.
 */
#define VFS_MVFS	1
#define VFS_NOT_MVFS	2

/* forward declarations */
struct mdki_vnode;
struct statvfs;
struct pathname;
struct uio;

typedef struct ioctl_ctx {
    void *filp;
    int caller_is_32bit;
} ioctl_ctx;

#ifndef MVFS_CALLER_INFO
#define MVFS_CALLER_INFO ioctl_ctx
#define MVFS_CALLER_INFO_STRUCT struct ioctl_ctx
#endif

/* Define the vfsops structure */

typedef int (*vfs_root_fn_t)(
    vfs_t *vfsp,
    struct mdki_vnode **vpp
);
typedef int (*vfs_statvfs_fn_t)(
    vfs_t *vfsp,
    STATVFS_T *statvfsp
);
typedef int (*vfs_sync_fn_t)(
    vfs_t *vfsarg,
    short flag,
    CRED_T *cr
);
typedef int (*vfs_mount_fn_t)(
    vfs_t *vfsp,
    struct mdki_vnode *mntpt,
    char *devpath,
    int flags,
    caddr_t data,
    size_t datalen,
    CRED_T *cr,
    MVFS_CALLER_INFO *ctx
);
typedef int (*vfs_unmount_fn_t)(
    vfs_t *vfsp,
    CRED_T *cr
);
typedef int (*vfs_vget_fn_t)(
    vfs_t *vfsp,
    struct mdki_vnode **vpp,
    MDKI_FID_T *fidp,
    CALL_DATA_T *cd
);
typedef int (*vfs_init_fn_t)(void);
typedef int (*vfs_log_fn_t)(
    vfs_t *vfsp,
    int level,
    const char *str, ...
)
#if __GNUC__ >= 3
 __attribute__((format(printf,3,4)))
#endif
    ;

typedef struct vfsops {
    vfs_root_fn_t vfs_root;
    vfs_statvfs_fn_t vfs_statvfs;
    vfs_sync_fn_t vfs_sync;
    vfs_mount_fn_t vfs_mount;
    vfs_unmount_fn_t vfs_unmount;
    vfs_vget_fn_t vfs_vget;
    vfs_init_fn_t vfs_init;
    vfs_log_fn_t vfs_log;
} vfsops_t;

/* Define the std vfs ops */

#define VFS_ROOT(vfsp, vpp)	(*(vfsp)->vfs_op->vfs_root)(vfsp, vpp)
#define VFS_STATVFS(vfsp, sbp)	(*(vfsp)->vfs_op->vfs_statvfs)(vfsp, sbp)
#define VFS_SYNC(vfsp, vfsarg, flag, cred)  	(*(vfsp)->vfs_op->vfs_sync)(vfsarg, flag, cred)
#define VFS_INIT(vfsp)  	(*(vfsp)->vfs_op->vfs_init)()
#define VFS_LOG(vfsp, level, str, ...)   (*(vfsp)->vfs_op->vfs_log)(vfsp, level, str, __VA_ARGS__)
#define VFS_MOUNT(vfsp, mntpt, devpath, flags, data, datalen, cr, ctx)   (*(vfsp)->vfs_op->vfs_mount)(vfsp, mntpt, devpath, flags, data, datalen, cr, ctx)
#define VFS_UNMOUNT(vfsp, cr)   (*(vfsp)->vfs_op->vfs_unmount)(vfsp, cr)
#define VFS_VGET(vfsp, vp, fidp, cd)   (*(vfsp)->vfs_op->vfs_vget)(vfsp, vp, fidp, cd)

#define VFS_LOG_ERR	1	/* Log errors */
#define VFS_LOG_WARN	2	/* Log warnings */
#define VFS_LOG_INFO	3	/* Log info messages */
#define VFS_LOG_ESTALE	4	/* Log ESTALE/EINTR errs (pseudo-level) */
#define VFS_LOG_DEBUG	5	/* Log debug messages/traps */
#define VFS_LOG_ENOENT  6	/* Log name not found errors */

typedef u_long fsblkcnt_t;
typedef u_long fsfilecnt_t;

/*
 * The vnode fields are protected by the locks as follows:
 *	v_type		No lock (constant from creation to destruction)
 *	v_flag		Changes protected by v_lock (add/rmv flag bit)
#if 0
 *	v_count		Changes protected by v_lock (inc/decr count)
#endif
 *	v_vfsp		No lock (constant from creation to destruction)
 *	v_op		No lock (constant from creation to destruction)
 *	v_rdev		"
 *	v_data		"
 * The fields that are constant from creation to destruction are
 * all protected from activate/inactivate races by a lock that is
 * implemented inside the VFS vnode ops (i.e. the mnode lock).
 */

typedef struct mdki_vnode {
	u_int           v_sanity; 	/* sanity number for vnode struct */
	VTYPE_T		v_type;		/* Vnode type */
	u_short		v_flag;		/* vnode flags */
	vfs_t 		*v_vfsp;	/* VFS pointer */
	struct vnodeops *v_op;		/* Vnode ops */
	caddr_t         v_data;		/* vnode data */
	caddr_t         v_dent;         /* Linux dentry (clrvnode) */
	caddr_t         v_vfsmnt;       /* Linux vfsmnt (clrvnode, vob roots) */
/* NB: no v_count on Linux; instead use attached inode's ref count */
} mdki_vnode_t;

#define VNODE_SANITY 0x564e4f44	/* vnode sanity number 'VNOD' (big-endian) */

/* vnode flags */

#define VROOT	0x0001		/* Root of its file system */
#define VTEXT	0x0002		/* Procedure text mapped */
#define VNOMAP	0x0004		/* file cannot be mapped */
#define VLOOPROOT 0x0008        /* vnode represents an alternate root */
#define VLOOP   0x0010          /* vnode is a cover-vnode of a loopback name
                                   elsewhere in the namespace */
#define VDOOMED 0x0020          /* vnode is temporary placeholder and
                                   shouldn't really be used */

typedef struct create_ctx {
    void *dentry;
    void *parent;
    int dev;
} create_ctx;
typedef struct link_ctx {
    void *olddent;
    void *parent;
    void *newdent;
    int done;
} link_ctx;
typedef struct unlink_ctx {
    void *dentry;
    int done;
} unlink_ctx;
typedef struct mkdir_ctx {
    void *dentry;
    int pleasedrop;
} mkdir_ctx;
typedef struct rename_ctx {
    void *odentry;
    void *ndentry;
} rename_ctx;
typedef struct seek_ctx {
    void *filep;
    int done;
    int origin;
    long long offset;
} seek_ctx;
typedef struct readdir_ctx {
    void *file;
    int done;
} readdir_ctx;
typedef struct mmap_ctx {
    void *file;
    void *mem;
} mmap_ctx;
typedef struct symlink_ctx {
    void *parent;
    void *new;
    int mode;
    int done;
} symlink_ctx;
typedef struct lookup_ctx {
    int flags;
#define LOOKUP_CTX_VALID        (1 << 0)
    void *dentrypp;
} lookup_ctx;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,35)
#define fsync_ctx file_ctx
#else
typedef struct fsync_ctx {
    struct file *file_p;
#if !defined(MRG)
    loff_t start;
    loff_t end;
#endif
} fsync_ctx;
#endif

/*
 * Define vnodeops struct.  Form of all vnode ops exactly matches
 * the arguments in mfs_vnodeops.c to make things easy.
 */

typedef int (*vop_open_fn_t)(
    struct mdki_vnode **vpp,
    int fmode,
    CALL_DATA_T *cd,
    file_ctx *ctx
);
typedef int (*vop_close_fn_t)(
    struct mdki_vnode *vp,
    int flag,
    int count,
    MOFFSET_T offset,
    CALL_DATA_T *cd,
    file_ctx *ctx 
);
typedef int (*vop_rdwr_fn_t)(
    struct mdki_vnode *vp,
    struct uio *uiop,
    uio_rw_t rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd,
    file_ctx *ctx
);
typedef int (*vop_ioctl_fn_t)(
    struct mdki_vnode *vp,
    int cmd,
    caddr_t data,
    int flag,
    CALL_DATA_T *cd,
    int *rvalp,
    VOPBD_T *vopbdp,
    MVFS_CALLER_INFO *callinfo
);
typedef int (*vop_getattr_fn_t)(
    struct mdki_vnode *vp,
    VATTR_T *vap,
    int flag,
    CALL_DATA_T *cd
);
#define GETATTR_FLAG_UPDATE_ATTRS 0x00000001
#define GETATTR_FLAG_PULLUP_ATTRS 0x00000002

typedef int (*vop_setattr_fn_t)(
    struct mdki_vnode *vp,
    VATTR_T *vap,
    int flag,
    CALL_DATA_T *cd
);
typedef int (*vop_access_fn_t)(
    struct mdki_vnode *vp,
    int mode,
    int flag,
    CALL_DATA_T *cd,
    nameidata_ctx *ctx
);
typedef int (*vop_lookup_fn_t)(
    struct mdki_vnode *dvp,
    char *nm,
    struct mdki_vnode **vpp,
    struct pathname *pnp,
    int flags,
    ROOTDIR_T *rootdir,
    CALL_DATA_T *cd,
    lookup_ctx *ctx
);
typedef int (*vop_create_fn_t)(
    struct mdki_vnode *dvp,
    char *nm,
    VATTR_T *vap,
    enum vcexcl excl,
    int mode,
    struct mdki_vnode **vpp,
    CALL_DATA_T *cd,
    create_ctx *ctx
);
typedef int (*vop_remove_fn_t)(
    struct mdki_vnode *dvp,
    struct mdki_vnode *vp,
    char *nm,
    CALL_DATA_T *cd,
    unlink_ctx *ctx 
);
typedef int (*vop_link_fn_t)(
    struct mdki_vnode *tdvp,
    struct mdki_vnode *vp,
    char *tnm,
    CALL_DATA_T *cd,
    link_ctx *ctx
);
typedef int (*vop_rename_fn_t)(
    struct mdki_vnode *odvp,
    char *onm,
    struct mdki_vnode *tdvp,
    char *tnm,
    CALL_DATA_T *cd,
    rename_ctx *ctx 
);
typedef int (*vop_mkdir_fn_t)(
    struct mdki_vnode *dvp,
    char *nm,
    VATTR_T *vap,
    struct mdki_vnode **vpp,
    CALL_DATA_T *cd,
    mkdir_ctx *ctx
);
typedef int (*vop_rmdir_fn_t)(
    struct mdki_vnode *dvp,
    char *nm,
    struct mdki_vnode *cdir,
    CALL_DATA_T *cd,
    dent_ctx *ctx
);
typedef int (*vop_readdir_fn_t)(
    struct mdki_vnode *dvp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    int *eofp,
    readdir_ctx *ctx
);
typedef int (*vop_symlink_fn_t)(
    struct mdki_vnode *dvp,
    char *lnm,
    VATTR_T *vap,
    char *tnm,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    symlink_ctx *ctx 
);
typedef int (*vop_readlink_fn_t)(
    struct mdki_vnode *vp,
    struct uio *uiop,
    CALL_DATA_T *cd
);
typedef int (*vop_fsync_fn_t)(
    struct mdki_vnode *vp,
    int flag,
    CALL_DATA_T *cd,
    fsync_ctx *ctx
);
typedef int (*vop_inactive_fn_t)(
    struct mdki_vnode *vp,
    CALL_DATA_T *cd
);
typedef int (*vop_fid_fn_t)(
    struct mdki_vnode *vp,
    struct MDKI_FID **
);
typedef int (*vop_realvp_fn_t)(
    struct mdki_vnode *vp,
    struct mdki_vnode **
);
typedef int (*vop_pathconf_fn_t)(
    struct mdki_vnode *vp,
    int cmd,
    u_long *valp,
    CRED_T *cred
);
typedef int (*vop_cmp_fn_t)(
    struct mdki_vnode *vp1,
    struct mdki_vnode *vp2
);
typedef int (*vop_mmap_fn_t)(
    struct mdki_vnode *vp,
    u_int sharing,
    u_int rwx,
    CALL_DATA_T *cd,
    mmap_ctx *ctx
);
typedef int (*vop_seek_fn_t)(
    struct mdki_vnode *vp,
    MOFFSET_T oldoffset,
    MOFFSET_T *newoffsetp,
    seek_ctx *ctx
);
typedef int (*vop_lockctl_fn_t)(
    struct mdki_vnode *vp,
    void *lockp,
    int cmd,
    CALL_DATA_T *cd,
    file_ctx *ctx
);
typedef void (*vop_print_fn_t)(
    struct mdki_vnode *vp
);

struct vnodeops {
    vop_open_fn_t vop_open;
    vop_close_fn_t vop_close;
    vop_rdwr_fn_t vop_rdwr;
    vop_ioctl_fn_t vop_ioctl;
    vop_getattr_fn_t vop_getattr;
    vop_setattr_fn_t vop_setattr;
    vop_access_fn_t vop_access;
    vop_lookup_fn_t vop_lookup;
    vop_create_fn_t vop_create;
    vop_remove_fn_t vop_remove;
    vop_link_fn_t vop_link;
    vop_rename_fn_t vop_rename;
    vop_mkdir_fn_t vop_mkdir;
    vop_rmdir_fn_t vop_rmdir;
    vop_readdir_fn_t vop_readdir;
    vop_symlink_fn_t vop_symlink;
    vop_readlink_fn_t vop_readlink;
    vop_fsync_fn_t vop_fsync;
    vop_inactive_fn_t vop_inactive;
    vop_fid_fn_t vop_fid;
    vop_realvp_fn_t vop_realvp;
    vop_pathconf_fn_t vop_pathconf;
    vop_cmp_fn_t vop_cmp;
    vop_mmap_fn_t vop_mmap;
    vop_seek_fn_t vop_seek;
    vop_lockctl_fn_t vop_lockctl;
    vop_print_fn_t vop_print;
};

/*
 * Vnode ops in MVOP form.
 */

#define VOP_OPEN(VPP,MODE,CD,CTX)	(*(*(VPP))->v_op->vop_open)(VPP,MODE,CD,CTX)
#define VOP_CLOSE(VP,F,C,O,CD,CTX)	(*(VP)->v_op->vop_close)(VP,F,C,O,CD,CTX)
#define VOP_RDWR(VP,UIOP,RW,F,VAP,CD,CTX) (*(VP)->v_op->vop_rdwr)(VP,UIOP,RW,F,VAP,CD,CTX)
/* Variants for Sysv.4 style calls of rdwr */
#define VOP_READ(VP,UIOP,F,VAP,CD,CTX) (*(VP)->v_op->vop_rdwr)(VP,UIOP,UIO_READ,F,VAP,CD,CTX)
#define VOP_WRITE(VP,UIOP,F,VAP,CD,CTX) (*(VP)->v_op->vop_rdwr)(VP,UIOP,UIO_WRITE,F,VAP,CD,CTX)

#define VOP_IOCTL(VP,C,D,F,CD,RVP,VBD,CINFO) (*(VP)->v_op->vop_ioctl)(VP,C,D,F,CD,RVP,VBD,CINFO)
#define VOP_GETATTR(VP,VAP,F,CD) (*(VP)->v_op->vop_getattr)(VP,VAP,F,CD)
#define VOP_SETATTR(VP,VAP,F,CD) (*(VP)->v_op->vop_setattr)(VP,VAP,F,CD)
#define VOP_ACCESS(VP,M,F,CD,CTX)       (*(VP)->v_op->vop_access)(VP,M,F,CD,CTX)
#define VOP_LOOKUP(VP,NM,VPP,PNP,F,RVP,CD,CTX) (*(VP)->v_op->vop_lookup)(VP,NM,VPP,PNP,F,RVP,CD,CTX)
#define VOP_CREATE(VP,NM,VAP,E,M,VPP,CD,CTX) (*(VP)->v_op->vop_create)(VP,NM,VAP,E,M,VPP,CD,CTX)
#define VOP_REMOVE(DVP,VP,NM,CD,CTX)	(*(DVP)->v_op->vop_remove)(DVP,VP,NM,CD,CTX)
#define VOP_LINK(TDVP,FVP,TNM,CD,CTX) (*(TDVP)->v_op->vop_link)(TDVP,FVP,TNM,CD,CTX)
#define VOP_RENAME(VP,NM,TDVP,TNM,CD,CTX) (*(VP)->v_op->vop_rename)(VP,NM,TDVP,TNM,CD,CTX)
#define VOP_MKDIR(VP,NM,VAP,VPP,CD,CTX) (*(VP)->v_op->vop_mkdir)(VP,NM,VAP,VPP,CD,CTX)
#define VOP_RMDIR(VP,NM,CDIR,CD,CTX)    (*(VP)->v_op->vop_rmdir)(VP,NM,CDIR,CD,CTX)
#define VOP_READDIR(VP,UIOP,CD,EOFP,CTX) (*(VP)->v_op->vop_readdir)(VP,UIOP,CD,EOFP,CTX)
#define VOP_SYMLINK(VP,LNM,VAP,TNM,VPP,CD,CTX) (*(VP)->v_op->vop_symlink)(VP,LNM,VAP,TNM,VPP,CD,CTX)
#define VOP_READLINK(VP,UIOP,CD) (*(VP)->v_op->vop_readlink)(VP,UIOP,CD)
#define VOP_FSYNC(VP,FL,CD,CTX)	(*(VP)->v_op->vop_fsync)(VP,FL,CD,CTX)
#define VOP_INACTIVE(VP,CD)	(*(VP)->v_op->vop_inactive)(VP,CD)
#define VOP_FID(VP,FIDPP)	(*(VP)->v_op->vop_fid)(VP,FIDPP)
#define VOP_PATHCONF(VP,CMD,VALP,CR) (*(VP)->v_op->vop_pathconf)(VP,CMD,VALP,CR)
#define VOP_CMP(VP1,VP2)	(*(VP1)->v_op->vop_cmp)(VP1,VP2)
#define VOP_REALVP(VP,VPP)	(*(VP)->v_op->vop_realvp)(VP,VPP)
#define VOP_MMAP(VP,SHR,RWX,CD,CTX)	(*(VP)->v_op->vop_mmap)(VP,SHR,RWX,CD,CTX)
#define VOP_SEEK(VP,OOFF,NOFFP,CTX)	(*(VP)->v_op->vop_seek)(VP,OOFF,NOFFP,CTX)
#define VOP_LOCKCTL(VP,LKP,CMD,CD,CTX)	(*(VP)->v_op->vop_lockctl)(VP,LKP,CMD,CD,CTX)
#define VOP_PRINT(VP)           (*(VP)->v_op->vop_print)(VP)

/* 
 * In most cases, we use the same vnode interface to dispatch calls from
 * the Linux wrapper code to the MVFS and back from the MVFS to the 
 * underlying Linux code.  Now that we are passing call_data blocks
 * instead of creds from the Linux code to the MVFS, this does not always
 * work because some functions that call MVOP macros do not have the 
 * call_data information readily at hand.  We declare those functions here.
 */

/* This function is sometimes called with cached creds */
extern int
mvop_linux_getattr(
    VNODE_T *vp,
    VATTR_T *vap,                       /* RETURN */
    int flags,
    CRED_T *cred
);

extern int
mvop_linux_close(
    VNODE_T *vp,
    int flags,
    VNODE_LASTCLOSE_T count,
    MOFFSET_T off,
    CRED_T *cred,
    file_ctx *ctx
);
#endif /* _MVFS_VNODE_H_ */
/* $Id: 0ed582bf.e6e311e1.8799.00:01:84:c3:8a:52 $ */
