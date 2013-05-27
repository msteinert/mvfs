/*
 * Copyright (C) 1999, 2010 IBM Corporation.
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
 * superblock operations (plus some utilities) for MVFS
 */
#include "vnode_linux.h"
#include "mvfs_linux_shadow.h"

void
vnlayer_put_super(SUPER_T *super_p);
void
vnlayer_write_super(SUPER_T *super_p);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) && !defined(SLES10SP2)
int
vnlayer_linux_statfs(
    SUPER_T *super_p,
    LINUX_STATFS_T *stat_p
);
#else
int
vnlayer_linux_statfs(
    DENT_T *dent_p,
    LINUX_STATFS_T *stat_p
);
#endif
int
vnlayer_remount_fs(
    SUPER_T *super_p,
    int *flags,
    char *data
);
void
mvfs_clear_inode(
    INODE_T *inode_p
);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) || \
    LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
void
mvfs_linux_umount_begin(SUPER_T *super_p);
#else
void
mvfs_linux_umount_begin(
    struct vfsmount *mnt,
    int flags);
#endif
int
vnlayer_fill_super(
    SUPER_T *super_p,
    void *data_p,
    int silent
);

int
vnlayer_dentry_to_fh(
    struct dentry *dent,
    __u32 *fh,
    int *lenp,
    int need_parent
);

struct dentry *
vnlayer_get_dentry(
    SUPER_T *sb,
    void *fhbits
);

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
struct dentry *
vnlayer_decode_fh(
    SUPER_T *sb,
    __u32 *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype,
    int (*acceptable)(void *context, struct dentry *de),
    void *context
);
#else /* LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24) */
struct dentry * vnlayer_fh_to_dentry(
    SUPER_T *sb,
    struct fid *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype
);

struct dentry * vnlayer_fh_to_parent(
    SUPER_T *sb,
    struct fid *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype
);
#endif /* else LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24) */

struct dentry *
vnlayer_get_parent(struct dentry *child);

static struct export_operations vnlayer_export_ops = {
    .encode_fh = &vnlayer_dentry_to_fh,
    .get_parent = &vnlayer_get_parent,
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
    .decode_fh = &vnlayer_decode_fh,
    .get_dentry = &vnlayer_get_dentry,
#else
    .fh_to_dentry = vnlayer_fh_to_dentry,
    .fh_to_parent = vnlayer_fh_to_parent,
#endif
};

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
SUPER_T *
vnlayer_get_sb(
    struct file_system_type *fs_type,
    int flags,
    const char *dev_name,
    void *raw_data
);
#else
int
vnlayer_get_sb(
    struct file_system_type *fs_type,
    int flags,
    const char *dev_name,
    void *raw_data,
    struct vfsmount *mnt
);
#endif /* else LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) */
void
vnlayer_kill_sb(SUPER_T *sbp);

INODE_T *
vnlayer_alloc_inode(SUPER_T *sbp);
void
vnlayer_destroy_inode(INODE_T *inode);
STATIC struct dentry *
vnlayer_find_dentry(VNODE_T *vp);

SB_OPS_T mvfs_super_ops = {
        /* no read_inode */
        /* no write_inode */
        /* No put_inode.  The work is done now in clear_inode */
        /* no delete_inode */
        .put_super =     &vnlayer_put_super,
        .write_super =   &vnlayer_write_super,
        .statfs =        &vnlayer_linux_statfs,
        .remount_fs =    &vnlayer_remount_fs,
        .clear_inode =   &mvfs_clear_inode,
        /* inodes no longer have space for fs data (e.g. our vnode). */
        .alloc_inode =   &vnlayer_alloc_inode,
        .destroy_inode = &vnlayer_destroy_inode,
	.umount_begin =  &mvfs_linux_umount_begin,
};

/* Define the filesytem type for loading
 * Note that the flag is set to require a device for loading.  Our
 * module initialization routine will register /dev/mvfs for us to
 * use.  This is done so that we can control our own minor numbers.
 */

struct file_system_type mvfs_file_system =
   {
     .name =       "mvfs",
     .fs_flags =   FS_REQUIRES_DEV | FS_BINARY_MOUNTDATA | FS_REVAL_DOT,
     .get_sb   =   &vnlayer_get_sb,
     .kill_sb  =   &vnlayer_kill_sb,
     .owner =      THIS_MODULE,
    };

void
vnlayer_put_super(struct super_block *super_p)
{
    int err;
    CRED_T *cred;

    ASSERT_KERNEL_LOCKED();
    ASSERT_SB_LOCKED(super_p);

    cred = MDKI_GET_UCRED();
    err = VFS_UNMOUNT(SBTOVFS(super_p), cred);
    MDKI_CRFREE(cred);

    if (err != 0) {
        VFS_LOG(SBTOVFS(super_p), VFS_LOG_ERR,
                "Linux file system interface doesn't let vnode/vfs reject unmounts: error %d\n", err);
    }
    return /* mdki_errno_unix_to_linux(err)*/;
}

void
vnlayer_write_super(struct super_block *super_p)
{
    CRED_T *cred;
    int err;

    ASSERT_SB_LOCKED(super_p);

    cred = MDKI_GET_UCRED();
    err = VFS_SYNC(SBTOVFS(super_p), SBTOVFS(super_p), 0, cred);
    MDKI_CRFREE(cred);
    /* They rewrote sync_supers so that it won't proceed through their loop
     * until the dirty bit is cleared.
     */
    super_p->s_dirt = 0;
    if (err != 0)
        VFS_LOG(SBTOVFS(super_p), VFS_LOG_ERR, "%s: error %d syncing\n",
                __func__, err);
    return /* mdki_errno_unix_to_linux(err) */;
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) && !defined(SLES10SP2)
int
vnlayer_linux_statfs(
    SUPER_T *super_p,
    LINUX_STATFS_T *stat_p
)
#else
int
vnlayer_linux_statfs(
    DENT_T *dent_p,
    LINUX_STATFS_T *stat_p
)
#endif
{
    int error;

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) && !defined(SLES10SP2)
    error = VFS_STATVFS(SBTOVFS(super_p), stat_p);
#else
    error = VFS_STATVFS(SBTOVFS(dent_p->d_sb), stat_p);
#endif
    return (-error);
}

int
vnlayer_remount_fs(
    struct super_block *super_p,
    int *flags,
    char *data
)
{
    VFS_T *vfsp = SBTOVFS(super_p);
    VFS_LOG(vfsp, VFS_LOG_ERR,
            "Vnode/VFS does not support remounting of file systems (sb=%p)\n",
            super_p);
    return -EINVAL;
}

void
mvfs_clear_inode(struct inode *inode_p)
{
    CALL_DATA_T cd;

    ASSERT(MDKI_INOISOURS(inode_p));

    if (MDKI_INOISMVFS(inode_p)) {
        /* If we're an mnode-base vnode, do all this stuff ... */

        VNODE_T *vp = ITOV(inode_p);
        int error;

        ASSERT(I_COUNT(inode_p) == 0);
        ASSERT(inode_p->i_state & I_FREEING);

        mdki_linux_init_call_data(&cd);

        /*
         * Do actual deactivation of the vnode/mnode
         */
        error = VOP_INACTIVE(vp, &cd);
        mdki_linux_destroy_call_data(&cd);

        if (error)
            MDKI_VFS_LOG(VFS_LOG_ERR, "mvfs_clear_inode: inactive error %d\n",
                     error);
    } else if (MDKI_INOISCLRVN(inode_p)) {
        /* cleartext vnode */
        vnlayer_linux_free_clrvnode(ITOV(inode_p));
    } else {
        MDKI_TRACE(TRACE_INACTIVE,"no work: inode_p=%p vp=%p cnt=%d\n", inode_p,
                  ITOV(inode_p), I_COUNT(inode_p));
    }
    MDKI_TRACE(TRACE_INACTIVE,"inode_p=%p vp=%p cnt=%d\n", inode_p,
               ITOV(inode_p), I_COUNT(inode_p));
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) || \
    LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
extern void
mvfs_linux_umount_begin(SUPER_T *super_p)
#else
extern void
mvfs_linux_umount_begin(
    struct vfsmount * mnt,
    int flags
)
#endif
{
    VNODE_T *vp;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) || \
    LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
    struct vfsmount *mnt;
#else
    /*
     * Since 2.6.18 and before 2.6.27 we have mnt as a parameter.
     * But we still need super_p.
     */
    SUPER_T *super_p = mnt->mnt_sb;
#endif

    ASSERT(super_p != NULL);
    ASSERT(super_p->s_root != NULL);
    vp = ITOV(super_p->s_root->d_inode);
    ASSERT(vp != NULL);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) || \
    LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
    mnt = VTOVFSMNT(vp);
#else
    /* Check that the mountpoint passed in matches the one
     * from the vp that we are going to clear.  Skip it otherwise.
     * We know from experience that this can happen when unmounting
     * loopback (bind) mounts.
     */
     if (mnt != VTOVFSMNT(vp))
         return;
#endif
    /* Note that there is no mechanism for restoring the mount pointer
     * in the vnode if an error happens later on in the umount.  This is
     * the only callback into the mvfs during umount.  So far this has not
     * been a problem and if we don't do this here, the umount will never
     * succeed because the Linux code expects the mnt_count to be 2.
     * The count is 3 at this point from the initial allocation of the 
     * vfsmnt structure, the path_lookup call in this umount call and 
     * from when we placed the pointer in the vp.  
     */
    if (mnt && atomic_read(&mnt->mnt_count) == 3) {
        MDKI_MNTPUT(mnt);
        SET_VTOVFSMNT(vp, NULL);
    }
}

/* vnlayer_linux_mount
 * This is a wrapper function to take the data passed in by the linux
 * mount and massage it into a form that is usable by our common routines
 *
 * IN SUPER_T *super_p
 *    void    *data_p
 *
 */

extern int
vnlayer_linux_mount(
    VFS_T *vfsp,
    void *data_p
)
{
    int err;
    CRED_T *cred;
    MVFS_CALLER_INFO_STRUCT ctx;

    cred = MDKI_GET_UCRED();
    BZERO(&ctx, sizeof(ctx));
    /* VFS_MOUNT method must detect 32- or 64-bit caller, if necessary */
    err = VFS_MOUNT(vfsp, NULL, NULL, vfsp->vfs_flag,
                          data_p, 0, cred, &ctx);
    err = mdki_errno_unix_to_linux(err);
    MDKI_CRFREE(cred);
    return(err);

}


int
vnlayer_fill_super(
    SUPER_T *super_p,
    void *data_p,
    int silent
)
{
    INODE_T *ino_p;
    VNODE_T *rootvp;
    VATTR_T va;
    VFS_T *vfsp;
    int err = 0;
    CALL_DATA_T cd;

    ASSERT_KERNEL_LOCKED();             /* sys_mount() */
    ASSERT_SB_MOUNT_LOCKED_W(super_p);

    /* can't assert on mount_sem, we don't have access to it. */

    if (vnlayer_vfs_opvec == NULL) {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                     "%s: VFS operation not set yet "
                     "(no file system module loaded?)\n", __func__);
        err = -ENODATA;
        goto return_NULL;
    }

    if (MDKI_INOISOURS(vnlayer_get_urdir_inode())) {
        /* can't handle this case */
        MDKI_VFS_LOG(VFS_LOG_ERR,
                    "%s: can't handle mounts inside setview.\n", __func__);
        err = -EINVAL;
        goto return_NULL;
    }

    /*
     * The only fields we have coming in are s_type and s_flags.
     */
    /* Verify this */

    super_p->s_blocksize = MVFS_DEF_BLKSIZE;
    super_p->s_blocksize_bits = MVFS_DEF_BLKSIZE_BITS;
    super_p->s_maxbytes = MVFS_DEF_MAX_FILESIZE;
    super_p->s_op = &mvfs_super_ops;
    super_p->s_export_op = &vnlayer_export_ops;
    super_p->dq_op = NULL;
    super_p->s_magic = MVFS_SUPER_MAGIC;

    /*
     * XXX This module is currently restricted to one client file system
     * type at a time, as registered via the vnlayer_vfs_opvec.
     */
    vfsp = KMEM_ALLOC(sizeof(*vfsp), KM_SLEEP);
    if (vfsp == NULL) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s failed: no memory\n", __func__);
        SET_SBTOVFS(super_p, NULL);
        err = -ENOMEM;
        goto return_NULL;
    }
    BZERO(vfsp, sizeof(*vfsp));
    SET_VFSTOSB(vfsp, super_p);
    SET_SBTOVFS(super_p, vfsp);
    vfsp->vfs_op = vnlayer_vfs_opvec;
    /* XXX fill in more of vfsp (flag?) */
    if (super_p->s_flags & MS_RDONLY)
        vfsp->vfs_flag |= VFS_RDONLY;
    if (super_p->s_flags & MS_NOSUID)
        vfsp->vfs_flag |= VFS_NOSUID;

    err = vnlayer_linux_mount(vfsp, data_p);

    if (err) {
        goto bailout;
    }

    /*
     * Now create our dentry and set that up in the superblock.  Get
     * the inode from the vnode at the root of the file system, and
     * attach it to a new dentry.
     */
    mdki_linux_init_call_data(&cd);
    err = VFS_ROOT(SBTOVFS(super_p), &rootvp);
    if (err) {
        err = mdki_errno_unix_to_linux(err);
        (void) VFS_UNMOUNT(vfsp, MVFS_CD2CRED(&cd));
        mdki_linux_destroy_call_data(&cd);
        goto bailout;
    }

    ino_p = VTOI(rootvp);

#ifdef CONFIG_FS_POSIX_ACL
    /* If the system supports ACLs, we set the flag in the superblock
     * depending on the ability of the underlying filesystem
     */
    if (vfsp->vfs_flag & VFS_POSIXACL) {
	super_p->s_flags |= MS_POSIXACL;
    }
#endif
    /*
     * Call getattr() to prime this inode with real attributes via the
     * callback to mdki_linux_vattr_pullup()
     */
    VATTR_NULL(&va);
    /* ignore error code, we're committed */
    (void) VOP_GETATTR(rootvp, &va, 0, &cd);

    /* This will allocate a dentry with a name of /, which is
     * what Linux uses in all filesystem roots.  The dentry is
     * also not put on the hash chains because Linux does not
     * hash file system roots.  It finds them through the super
     * blocks.
     */
    super_p->s_root = VNODE_D_ALLOC_ROOT(ino_p);
    if (super_p->s_root) {
        if (VFSTOSB(vnlayer_looproot_vp->v_vfsp) == super_p)
            /* loopback names are done with regular dentry ops */
            super_p->s_root->d_op = &vnode_dentry_ops;
        else
            /*
             * setview names come in via VOB mounts, they're marked
             * with setview dentry ops
             */
            super_p->s_root->d_op = &vnode_setview_dentry_ops;
        super_p->s_root->d_fsdata = NULL;
        atomic_set(&super_p->s_root->d_count, 1);
        /* d_alloc_root assumes that the caller will take care of
         * bumping the inode count for the dentry.  So we will oblige
         */
        igrab(ino_p);
    } else {
        VN_RELE(rootvp);
        (void) VFS_UNMOUNT(vfsp, MVFS_CD2CRED(&cd));
        mdki_linux_destroy_call_data(&cd);
        err = -ENOMEM;
        goto bailout;
    }
    mdki_linux_destroy_call_data(&cd);
    super_p->s_dirt = 1;            /* we want to be called on
                                       write_super/sync() */
    /*
     * release reference on rootvp--super block holds appropriate
     * references now
     */
    VN_RELE(rootvp);
    return(0);

  bailout:
    MDKI_VFS_LOG(VFS_LOG_ERR,
                 "%s failed: error %d\n", __func__,
                 vnlayer_errno_linux_to_unix(err));
    SET_SBTOVFS(super_p, NULL);
    KMEM_FREE(vfsp, sizeof(*vfsp));
  return_NULL:
    return(err);
}

/* This function is a callback from sget.  We will setup the device number
 * to be our default major and minor = 0.  We will set the real device number
 * later in vnlayer_fill_super when it calls into the mvfs proper.  The 
 * reason for not just making this the call to fill_super is that we are
 * called with the sb_lock spinlock held so we can't pend and vnlayer_fill_super
 * allocates memory.
 */

int vnlayer_set_sb(
    struct super_block *sb,
    void *data
)
{
    sb->s_dev = MKDEV(mvfs_major, 0);
    return 0;
}

/* We used to call get_sb_bdev but in 2.6 the block device interface was
 * changes such that get_sb_bdev would try to obtain partition information
 * for the block device in question.  The mvfs registers a block device
 * merely to establish a set of major device numbers.  We have no physical
 * device and no partitions so the get_sb_bdev() interface no longer works
 * for us.  Get_sb_nodev() explicitly uses major device 0 so that is not
 * an option for us.  Instead we now use the sget() interface which is new 
 * in the 2.6 kernel.  This gives us the ability to get a superblock and 
 * handle the device issues ourselves while the kernel handles getting it
 * placed on the appropriate lists.
 *
 * Since 2.6.18 they added vfsmount to the parameters and changed the
 * return type to int. Now it's clear when returning an error code
 * instead of "pointer meaning error" that was used before. But sget stills
 * using that technique, so we cast it if an error occurs.
 * We have to setup mnt param if everything is OK and we've chosen 
 * simple_set_mnt to do it.
 */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
SUPER_T *
vnlayer_get_sb(
    struct file_system_type *fs_type,
    int flags,
    const char *dev_name,
    void *raw_data
)
#else
int
vnlayer_get_sb(
    struct file_system_type *fs_type,
    int flags,
    const char *dev_name,
    void *raw_data,
    struct vfsmount *mnt
)
#endif
{
    SUPER_T *sb;
    int err;

    sb = sget(fs_type, NULL, vnlayer_set_sb, raw_data);
    if (IS_ERR(sb)) {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
        return sb;
#else
        return PTR_ERR(sb);
#endif
    }
    err = vnlayer_fill_super(sb, raw_data, 0);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
    if (err != 0) {
        generic_shutdown_super(sb);
        sb = ERR_PTR(err);
    }
    return sb;
#else
    if (err != 0) {
        generic_shutdown_super(sb);
    }
    else {
        /* We need to fill the mnt struct */
        simple_set_mnt(mnt, sb);
    }
    return err;
#endif
}

/* generic_shutdown_super() will call our put_super function which
 * will handle all of our specific cleanup.
 */
void
vnlayer_kill_sb(SUPER_T *sbp)
{
    generic_shutdown_super(sbp);
}

INODE_T *
vnlayer_alloc_inode(SUPER_T *sbp)
{
    vnlayer_vnode_t *vnlvp;

    vnlvp = (vnlayer_vnode_t *)kmem_cache_alloc(vnlayer_vnode_cache, GFP_KERNEL);
    if (vnlvp != NULL) {
        /* The inode used to be initialized by the kernel slab allocator
        ** because we used it to allocate an inode directly (see init_once() in
        ** fs/inode.c).  However, now we allocate our vnode with an inode
        ** inside of it so we have to do the initialization ourselves.
        **
        ** The caller must fully initialize the associated vnode.
        */
        inode_init_once(&vnlvp->vnl_inode);
        return(&vnlvp->vnl_inode);
    } else {
        return(NULL);
    }
}

void
vnlayer_destroy_inode(INODE_T *inode)
{
    ASSERT(I_COUNT(inode) == 0);
    kmem_cache_free(vnlayer_vnode_cache, (vnlayer_vnode_t *)ITOV(inode));
}

#ifndef roundup
#define roundup(val, upto) (( ((val) + (upto) - 1) / (upto)) * (upto))
#endif

/*
 * NFS access to vnode file systems.
 *
 * We provide dentry_to_fh() and fh_to_dentry() methods so that the
 * vnode-based file system can hook up its VOP_FID() and VFS_VGET()
 * methods.  The Linux NFS server calls these methods when encoding an
 * object into a file handle to be passed to the client for future
 * use, and when decoding a file handle and looking for the file
 * system object it describes.
 *
 * VOP_FID() takes a vnode and provides a file ID (fid) that can later
 * be presented (in a pair with a VFS pointer) to VFS_VGET() to
 * reconstitute that vnode.  In a Sun ONC-NFS style kernel, VOP_FID()
 * is used twice per file handle, once for the exported directory and
 * once for the object itself.  In Linux, the NFS layer itself handles
 * the export tree checking (depending on the status of
 * NFSEXP_NOSUBTREECHECK), so the file system only needs to fill in
 * the file handle with details for the object itself.  We always
 * provide both object and parent in the file handle to be sure that
 * we don't end up short on file handle space in a future call that
 * requires both.
 *
 * On a call from the NFS client, the Linux NFS layer finds a
 * superblock pointer from the file handle passed by the NFS client,
 * then calls the fh_to_dentry() method to get a dentry.  Sun ONC-NFS
 * kernels call VFS_VGET() on a vfsp, passing the FID portion of the
 * file handle.  In this layer, we unpack the file handle, determine
 * whether the parent or the object is needed, and pass the info along
 * to a VFS_VGET() call.  Once that returns, we look for an attached
 * dentry and use it, or fabricate a new one which NFS will attempt to
 * reconnect to the namespace.
 */
int
vnlayer_dentry_to_fh(
    struct dentry *dent,
    __u32 *fh,
    int *lenp,
    int need_parent
)
{
    int error;
    int type;
    int mylen;
    MDKI_FID_T *lfidp = NULL;
    MDKI_FID_T *parent_fidp = NULL;
    mdki_boolean_t bailout_needed = TRUE; /* Assume we'll fail. */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
    SUPER_T *sbp;
#endif

    /*
     * We use the type byte (return value) to encode the FH length.  Since we
     * always include two FIDs of the same size, the type must be even, so
     * that's how we "encode" the length of each FID (i.e. it is half the total
     * length).
     *
     * Always include parent entry; this makes sure that we only work with NFS
     * protocols that have enough room for our file handles.  (Without this, we
     * may return a directory file handle OK yet be unable to return a plain
     * file handle.)  Currently, we can just barely squeeze two standard
     * 10-byte vnode FIDs into the NFS v2 file handle.  The NFS v3 handle has
     * plenty of room.
     */
    ASSERT(ITOV(dent->d_inode));
    error = VOP_FID(ITOV(dent->d_inode), &lfidp);
    if (error != 0) {
        ASSERT(lfidp == NULL);
        goto bailout;
    }

    ASSERT(ITOV(dent->d_parent->d_inode));
    error = VOP_FID(ITOV(dent->d_parent->d_inode), &parent_fidp);
    if (error != 0) {
        ASSERT(parent_fidp == NULL);
        goto bailout;
    }

    /*
     * Our encoding scheme can't tolerate different length FIDs
     * (because otherwise the type wouldn't be guaranteed to be even).
     */
    if (parent_fidp->fid_len != lfidp->fid_len) {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                     "%s: unbalanced parent/child fid lengths: %d, %d\n",
                     __func__, parent_fidp->fid_len, lfidp->fid_len);
        goto bailout;
    }

    /* 
     * vnode layer needs to release the storage for a fid on
     * Linux.  The VOP_FID() function allocates its own fid in
     * non-error cases.  Other UNIX systems release this storage
     * in the caller of VOP_FID, so we have to do it here.  We
     * copy the vnode-style fid into the caller-allocated space,
     * then free our allocated version here.
     *
     * Remember: vnode lengths are counting bytes, Linux lengths count __u32
     * units.
     */
    type = parent_fidp->fid_len + lfidp->fid_len; /* Guaranteed even. */
    mylen = roundup(type + MDKI_FID_EXTRA_SIZE, sizeof(*fh));

    if (mylen == VNODE_NFS_FH_TYPE_RESERVED ||
        mylen >= VNODE_NFS_FH_TYPE_ERROR)
    {
        MDKI_VFS_LOG(VFS_LOG_ESTALE,
                     "%s: required length %d out of range (%d,%d)\n",
                     __func__, mylen,
                     VNODE_NFS_FH_TYPE_RESERVED, VNODE_NFS_FH_TYPE_ERROR);
        goto bailout;
    }
    if (((*lenp) * sizeof(*fh)) < mylen) {
        MDKI_VFS_LOG(VFS_LOG_ESTALE,
                     "%s: need %d bytes for FH, have %d\n",
                     __func__, mylen, (int) (sizeof(*fh) * (*lenp)));
        goto bailout;
    }
    /* Copy FIDs into file handle. */
    *lenp = mylen / sizeof(*fh); /* No remainder because of roundup above. */
    BZERO(fh, mylen);           /* Zero whole fh to round up to __u32 boundary */
    BCOPY(lfidp->fid_data, fh, lfidp->fid_len);
    BCOPY(parent_fidp->fid_data, ((caddr_t)fh) + (type / 2),
          parent_fidp->fid_len);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
    /* 
     * For 64 bits OS, use a 32 bits hash of the SB pointer.
     * For 32 bits OS, use the pointer itself.
     */
    if (ITOV(dent->d_inode) == NULL || 
        ITOV(dent->d_inode)->v_vfsmnt == NULL) {
        MDKI_VFS_LOG(VFS_LOG_ESTALE,
                     "%s: %p is this a MVFS inode?\n",
                     __func__, dent->d_inode);
        goto bailout;
    } else {
        sbp = ((struct vfsmount *)ITOV(dent->d_inode)->v_vfsmnt)->mnt_sb;
    }
    MDKI_FID_SET_SB_HASH(fh, type / 2, MDKI_FID_CALC_HASH(sbp));
#endif

    bailout_needed = FALSE;         /* We're home free now. */

    if (bailout_needed) {
  bailout:
        type = VNODE_NFS_FH_TYPE_ERROR;
        *lenp = 0;
    }
#ifdef KMEMDEBUG
    if (lfidp != NULL)
        REAL_KMEM_FREE(lfidp, MDKI_FID_LEN(lfidp));
    if (parent_fidp != NULL)
        REAL_KMEM_FREE(parent_fidp, MDKI_FID_LEN(parent_fidp));
#else
    if (lfidp != NULL)
        KMEM_FREE(lfidp, MDKI_FID_LEN(lfidp));
    if (parent_fidp != NULL)
        KMEM_FREE(parent_fidp, MDKI_FID_LEN(parent_fidp));
#endif
    return type;
}

STATIC int
vnlayer_unpack_fh(
    __u32 *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype,
    int fidlen,
    MDKI_FID_T *lfidp,
    MDKI_FID_T *plfidp
)
{
    if (len * sizeof(*fh) < fhtype) {
        /*
         * we put the size in the type on the way out;
         * make sure we have enough data returning now
         */
        MDKI_VFS_LOG(VFS_LOG_ESTALE,
                     "%s: FH doesn't have enough data for type:"
                     " has %d need %d\n",
                     __func__, (int)(len * sizeof(*fh)), fhtype);
        return -EINVAL;
    }

    if ((fhtype & 1) != 0) {
        /*
         * The type/length must be even (there are two equal-sized
         * halves in the payload).  Somebody might be fabricating file
         * handles?
         */
        MDKI_VFS_LOG(VFS_LOG_ESTALE,
                     "%s: FH type (%d) not even\n", __func__, fhtype);
        return -EINVAL;
    }

    if (lfidp != NULL) {
        /* object */
        lfidp->fid_len = fidlen;
        BCOPY(fh, lfidp->fid_data, fidlen);
    }
    if (plfidp != NULL) {
        /* parent */
        plfidp->fid_len = fidlen;
        BCOPY(((caddr_t)fh) + fidlen, plfidp->fid_data, fidlen);
    }
    return 0;
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)

#include <linux/sunrpc/svc.h>
#include <linux/nfsd/nfsd.h>

struct dentry *
vnlayer_decode_fh(
    SUPER_T *sb,
    __u32 *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype,
    int (*acceptable)(void *context, struct dentry *de),
    void *context
)
{
    MDKI_FID_T *lfidp, *plfidp;
    DENT_T *dp;
    int error, fidlen;
    struct svc_export *exp = context;   /* XXX cheating! */
    SUPER_T *realsb = exp->ex_dentry->d_inode->i_sb;

    fidlen = fhtype >> 1;
    if (fidlen == 0)
        return ERR_PTR(-EINVAL);

    lfidp = KMEM_ALLOC(MDKI_FID_ALLOC_LEN(fidlen), KM_SLEEP);
    if (lfidp == NULL)
        return ERR_PTR(-ENOMEM);

    plfidp = KMEM_ALLOC(MDKI_FID_ALLOC_LEN(fidlen), KM_SLEEP);
    if (plfidp == NULL) {
        KMEM_FREE(lfidp, MDKI_FID_ALLOC_LEN(fidlen));
        return ERR_PTR(-ENOMEM);
    }

    error = vnlayer_unpack_fh(fh, len, fhtype, fidlen, lfidp, plfidp);
    if (error == 0) {
        /*
         * We've extracted the identifying details from the
         * client-provided fid.  Now use the system routines to handle
         * dentry tree work, it will call back to
         * sb->s_export_op->get_dentry to interpret either the parent
         * or the object.
         */
        dp = (*realsb->s_export_op->find_exported_dentry)(realsb, lfidp,
                                                          plfidp, acceptable,
                                                          context);
        if (IS_ERR(dp)) {
            MDKI_VFS_LOG(VFS_LOG_ESTALE,
                "%s: pid %d call to find_exported_dentry returned error %ld\n",
                __FUNCTION__, current->pid, PTR_ERR(dp));
        }
    } else {
        dp = ERR_PTR(error);
    }

    KMEM_FREE(lfidp, MDKI_FID_ALLOC_LEN(fidlen));
    KMEM_FREE(plfidp, MDKI_FID_ALLOC_LEN(fidlen));
    return dp;
}
#else /* LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24) */
/* 
 *  Matching callback passed to mfs_find_mount, it will return
 *  the super_block pointer related to the VFS_T if it matches
 *  or NULL.
 */
static void *
vnlayer_eval_mount(
    VFS_T *vfsp,
    void *data
)
{
    SUPER_T *sb = VFSTOSB(vfsp);
    if(MDKI_FID_CALC_HASH(sb) == *(unsigned *)data) {
        lock_super(sb);
        return sb;
    }
    return NULL;
}

/* Common file handle decoding for both parent and dentry */
static struct dentry *
vnlayer_decode_fh(
    SUPER_T *sb,
    struct fid *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype,
    int is_parent)
{
    MDKI_FID_T *lfidp;
    DENT_T *dp;
    int error, fidlen;
    SUPER_T *realsb;
    unsigned realsb_hash;

    fidlen = fhtype >> 1;
    if (fidlen == 0) {
        return ERR_PTR(-EINVAL);
    }

    if (len * 4 < MDKI_FID_LEN_WITH_HASH(fidlen)) {
        MDKI_VFS_LOG(VFS_LOG_ESTALE,
                      "%s: FH too small to be a MVFS FH\n",
                      __FUNCTION__);
        return ERR_PTR(-EINVAL);
    }

    lfidp = KMEM_ALLOC(MDKI_FID_ALLOC_LEN(fidlen), KM_SLEEP);
    if (lfidp == NULL) {
        return ERR_PTR(-ENOMEM);
    }

    if (is_parent) {
        error = vnlayer_unpack_fh((__u32 *)fh, len, fhtype, fidlen,
                                  NULL, lfidp);
    } else {
        error = vnlayer_unpack_fh((__u32 *)fh, len, fhtype, fidlen,
                                  lfidp, NULL);
    }

    if (error == 0) {
        realsb_hash = MDKI_FID_SB_HASH(fh, fidlen);

        /*
         * Search in the VOB mount list for the super_block we encoded.
         * If the result is not NULL, the superblock was locked with
         * lock_super and should be unlocked.
         */
        realsb = (SUPER_T *) mvfs_find_mount(vnlayer_eval_mount,
                                             &realsb_hash);

        if (realsb != NULL) {
            /*
             * It found a matching VOB mount to this hash, we will leave to
             * vnlayer_get_dentry decides wether we can trust this FID, 
             * it should be able to smell any staleness.
             */
            dp = vnlayer_get_dentry(realsb, lfidp);
            unlock_super(realsb);
            if (IS_ERR(dp)) {
                MDKI_VFS_LOG(VFS_LOG_ESTALE,
                    "%s: pid %d vnlayer_get_dentry returned error %ld\n",
                    __FUNCTION__, current->pid, PTR_ERR(dp));
            }
        } else {
            dp = ERR_PTR(-EINVAL);
            MDKI_VFS_LOG(VFS_LOG_ESTALE, "%s SB not found, hash=%08x\n",
                         __FUNCTION__, realsb_hash);
        }
    } else {
        dp = ERR_PTR(error);
    }
    KMEM_FREE(lfidp, MDKI_FID_ALLOC_LEN(fidlen));
    return dp;
}

struct dentry *
vnlayer_fh_to_dentry(
    SUPER_T *sb,
    struct fid *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype)
{
    return vnlayer_decode_fh(sb, fh, len, fhtype, 0);
}

struct dentry *
vnlayer_fh_to_parent(
    SUPER_T *sb,
    struct fid *fh,
    int len,                            /* counted in units of 4-bytes */
    int fhtype)
{
    return vnlayer_decode_fh(sb, fh, len, fhtype, 1);
}

#endif /* else LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24) */

struct dentry *
vnlayer_get_parent(struct dentry *child)
{
    VNODE_T *parentvp;
    struct dentry *rdentp;
    struct lookup_ctx ctx;
    CALL_DATA_T cd;
    int err;

    if (!MDKI_INOISMVFS(child->d_inode))
        return ERR_PTR(-ESTALE);

    ctx.flags = 0;
    ctx.dentrypp = NULL;
    mdki_linux_init_call_data(&cd);

    err = VOP_LOOKUP(ITOV(child->d_inode), "..", &parentvp, NULL,
                     VNODE_LF_LOOKUP, NULL, &cd, &ctx);
    mdki_linux_destroy_call_data(&cd);
    if (err == 0) {
        ASSERT(ctx.dentrypp == NULL);
        ASSERT(parentvp != NULL);
        if (!MDKI_INOISMVFS(VTOI(parentvp))) {
            rdentp = ERR_PTR(-ESTALE);
        } else { 
            rdentp = vnlayer_find_dentry(parentvp);
        }
        /* always drop vnode's refcount */
        VN_RELE(parentvp);
    } else {
        rdentp = ERR_PTR(mdki_errno_unix_to_linux(err));
    }
    return rdentp;
}

extern int mfs_rebind_vpp(int release, VNODE_T **vpp, CALL_DATA_T *cd);

struct dentry *
vnlayer_get_dentry(
    SUPER_T *sb,
    void *fhbits
)
{
    int error;
    VFS_T *vfsp = SBTOVFS(sb);
    VNODE_T *vp;
    MDKI_FID_T *lfidp = fhbits;
    DENT_T *dp;
    CALL_DATA_T cd;

    mdki_linux_init_call_data(&cd);
    error = VFS_VGET(vfsp, &vp, lfidp, &cd);
    if (error == 0) {
        /* rebind if needed */
        if (mfs_rebind_vpp(1, &vp, &cd)) {
            MDKI_VFS_LOG(VFS_LOG_ESTALE,
                      "%s: vp %p rebound\n",
                      __FUNCTION__, vp);
        }
        dp = vnlayer_find_dentry(vp);
        /* always drop vnode's refcount */
        VN_RELE(vp);
    } else {
        dp = ERR_PTR(mdki_errno_unix_to_linux(error));
    }
    mdki_linux_destroy_call_data(&cd);
    return dp;
}

/*
 * We have a vnode/inode, now get the dentry.  If there isn't
 * one already pointing to the inode, we must fabricate a
 * disconnected entry for NFS to connect up to the known
 * dcache tree (in the paranoia/export directory checking
 * cases--but performance will be better if
 * NFSEXP_NOSUBTREECHECK is present in the export options--but
 * will this break our use of dentry->d_parent->d_inode in
 * dentry_to_fh()? XXX).
 * Returns a valid dentry pointer or a negative error code.
 * If the returned dentry is a new one, the inode's refcount is
 * incremented.
 */
STATIC struct dentry *
vnlayer_find_dentry(VNODE_T *vp)
{
    static const struct qstr this_is_anon  = { .name = ""};
    struct dentry *dp;
    struct dentry *dnew;
    INODE_T *ip;

    ip = VTOI(vp);
    /*
     * We create an anonymous dentry for NFS, but it will be used
     * only if we don't find a suitable one for this inode.
     * We have to do it here because d_alloc acquires dcache_lock.
     */
    dnew = d_alloc(NULL, &this_is_anon);
    if (dnew == NULL) {
        dp = ERR_PTR(-ENOMEM);
    } else {
        dnew->d_parent = dnew;
        spin_lock(&dcache_lock);
        /*
         * equivalent of d_splice_alias,
         * we only want view-extended dentries
         */
        dp = vnlayer_inode2dentry_internal_no_lock(ip,
                                                   NULL,
                                                   NULL,
                                                   &vnode_dentry_ops);
        if (dp == NULL) {
            /* new dentry, increase the refcount for this v/inode */
            VN_HOLD(vp);
            /* found no suitable dentry, add a new one */
            dnew->d_op = &vnode_dentry_ops;
            dnew->d_sb = ip->i_sb;
            dnew->d_inode = ip;
            dnew->d_flags |= NFSD_DCACHE_DISCON;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
            dnew->d_flags &= ~DCACHE_UNHASHED;
#endif
            list_add(&dnew->d_alias, &ip->i_dentry);
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
            hlist_add_head(&dnew->d_hash, &ip->i_sb->s_anon);
#endif
            dp = dnew;
            /* skip the dput call for dnew, we will need it */
            dnew = NULL;
        }
        spin_unlock(&dcache_lock);
    }
    /* dput dnew if we don't use it */
    if (dnew != NULL)
        dput(dnew);
    return dp;
}

static const char vnode_verid_mvfs_linux_sops_c[] = "$Id:  a7cbf7b3.dc5411df.9210.00:01:83:0a:3b:75 $";
