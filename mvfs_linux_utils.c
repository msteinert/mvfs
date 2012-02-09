/*
 * Copyright (C) 1999, 2008 IBM Corporation.
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

/* Miscellaneous utilities for MVFS */

#include "vnode_linux.h"
#include "mvfs_linux_shadow.h"

#ifdef MODULE_LICENSE
MODULE_LICENSE("GPL v2");
#endif
#ifdef MODULE_INFO
MODULE_INFO(supported,"external");
#endif
MODULE_AUTHOR("IBM Rational Software");
MODULE_DESCRIPTION("IBM Rational ClearCase Multi-Version File System");

VNODE_T *vnlayer_sysroot_clrvp;
VFS_T *vnlayer_clrvnode_vfsp;
VNODE_T *vnlayer_looproot_vp;

struct vfsops *vnlayer_vfs_opvec;

/* The following is provided a global wait queue to handle
 * synchronization between vnode file system nodes and the inodes
 * to which they point.
 */

WAIT_QUEUE_HEAD_T vnlayer_inactive_waitq;

/* The following are set up at initialization */

struct dentry * vnlayer_sysroot_dentry;
struct vfsmount * vnlayer_sysroot_mnt;

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
STATIC int
vnlayer_blkdev_open(
   INODE_T *inode,
   FILE_T *file
);

/* The following is a set to use when registering /dev/mvfs. */
struct block_device_operations vnlayer_device_ops = {
        .open = &vnlayer_blkdev_open,
};
F_OPS_T vnlayer_device_file_ops = {
};

STATIC int
vnlayer_blkdev_open(
   INODE_T *inode,
   FILE_T *file
)
{
    if (inode)
        inode->i_fop = &vnlayer_device_file_ops;
    if (file) {
        if (file->f_op)
            fops_put(file->f_op);
        file->f_op = fops_get(&vnlayer_device_file_ops);
    }
    return(0);
}
#endif

int
vnlayer_vtype_to_mode(
    VTYPE_T vtype
)
{
    switch (vtype) {
      case VREG:
        return S_IFREG;
      case VDIR:
        return S_IFDIR;
      case VCHR:
        return S_IFCHR;
      case VLNK:
        return S_IFLNK;
      case VFIFO:
        return S_IFIFO;
      case VNON:
        return 0;
      default:
        BUG();
        return 0;                       /* shut GCC up */
    }
}

VTYPE_T
vnlayer_mode_to_vtype(
    int mode
)
{
    switch (mode & S_IFMT) {
      case S_IFREG:
        return VREG;
      case S_IFDIR:
        return VDIR;
      case S_IFCHR:
        return VCHR;
      case S_IFBLK:
        return VBLK;
      case S_IFLNK:
        return VLNK;
      case S_IFIFO:
        return VFIFO;
      case S_IFSOCK:
        return VSOCK;
      default:
        return VNON;
    }
}

/*
 * Move pointers/counts in a uio structure as if a real uiomove() had
 * happened.  (Only works for single-vector UIOs)
 */
void
vnlayer_linux_adjust_uio(
    struct uio *uiop,
    ssize_t count,
    int do_offset
)
{
    ASSERT(uiop->uio_iovcnt == 1);
    uiop->uio_resid -= count;
    if (do_offset)
        uiop->uio_offset += count;
    uiop->uio_iov->iov_base += count;
    uiop->uio_iov->iov_len -= count;
    return;
}

/*
 * Utility routine to create and fill in a shadow inode from a real
 * one.  The shadow inode is passed in so that the caller can handle
 * allocation failure (and not call this routine).
 */
extern void
vnlayer_shadow_inode(
    INODE_T *real_inode,
    DENT_T *dentry,
    INODE_T *shadow_inode
)
{
    shadow_inode->i_sb = dentry->d_sb;
#ifdef HAVE_SHADOW_FILES
    /* Some inodes don't even have inode ops */
    /*
     * The only check in the lookup on whether we have a symlink to
     * follow is the existence of the follow symlink inode op.  So we
     * will need separate inode ops tables for symlinks and other
     * files.
     * Likewise for mmap support.
     */
    if (real_inode->i_op) {
        if (real_inode->i_op->follow_link)
            shadow_inode->i_op = &vnode_shadow_slink_inode_ops;
        else
            shadow_inode->i_op = &vnode_shadow_reg_inode_ops;
    }
    if (real_inode->i_fop) {
        if (real_inode->i_fop->mmap)
            shadow_inode->i_fop = &vnode_shadow_mapped_file_ops;
        else
            shadow_inode->i_fop = &vnode_shadow_file_ops;
    }
#else /* !HAVE_SHADOW_FILES */
    ASSERT(S_ISLNK(real_inode->i_mode));
    shadow_inode->i_fop = NULL;
    shadow_inode->i_op = &vnode_shadow_slink_inode_ops;
#endif /* HAVE_SHADOW_FILES */

    SHADOW_CP_INODAT(real_inode, shadow_inode);
}

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#if MDKI_NGROUPS < LINUX_NGROUPS
#error not enough room in creds structure for Linux groups
#endif

/*
 * MDKI_NGROUPS may be larger than NGROUPS, so we make sure
 * only to copy at most NGROUPS from creds to the task struct.
 */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
STATIC void
vnlayer_groups_cred_to_task(
    struct task_struct *task,
    CRED_T *cred
)
{
    register int i;

    LINUX_TASK_NGROUPS(task) = MIN(cred->cr_ngroups, LINUX_NGROUPS);
    for (i = 0; i < MIN(LINUX_TASK_NGROUPS(task), MIN(LINUX_NGROUPS,MDKI_NGROUPS)); i++)
        LINUX_TASK_GROUPS(task)[i] = cred->cr_groups[i];
    for (; i < LINUX_NGROUPS; i++)
        /* be clean and zap the rest of the structure */
        LINUX_TASK_GROUPS(task)[i] = 0;
}
#endif

mdki_boolean_t
vnlayer_fsuid_save(
    vnlayer_fsuid_save_t *save_p,
    CRED_T *cred
)
{
    mdki_boolean_t rv = FALSE;  /* Assume failure. */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
    int my_ngroups = LINUX_TASK_NGROUPS(current);
    gid_t *my_groups = LINUX_TASK_GROUPS(current);
#else
    int my_ngroups;
    gid_t *my_groups;
    struct group_info *my_group_info;

    /* On 2.6 we need to lock the task structure, do a get on the info
    ** we need, then unlock, and copy it to the local vars.  At the end
    ** we'll do the put to undo the get.
    */
    task_lock(current);
    my_group_info = current->group_info;
    get_group_info(my_group_info);
    task_unlock(current);

    my_ngroups = my_group_info->ngroups;

    /* The static init_groups is set with ngroups=0 and no nblocks array at all
    ** (because the compiler optimizes away an array with no elements at the
    ** end of a structure).  Thus, we have to be careful here (in case
    ** init_groups was at the end of a page and blocks[0] was on the next,
    ** unmapped, page) and in the code below (so as not to reference through a
    ** NULL ptr).
    */
    if (my_ngroups == 0) {
        my_groups = NULL;
    } else {
        my_groups = my_group_info->blocks[0];
    }
#endif
    *save_p = NULL;             /* Assume failure. */

    if (MDKI_CR_GET_UID(cred) != current->fsuid ||
        MDKI_CR_GET_GID(cred) != current->fsgid ||
        cred->cr_ngroups != my_ngroups ||
        (my_ngroups > 0 &&
         BCMP(cred->cr_groups, my_groups, 
              (cred->cr_ngroups * sizeof(cred->cr_groups[0]))) != 0))
    {
        vnlayer_fsuid_save_struct_t *save = NULL;

        save = KMEM_ALLOC(sizeof(*save), KM_SLEEP);
        if (save != NULL) {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
            save->ngroups = my_ngroups;
            /*
             * We checked the sizes were the same at load time, see
             * vnlayer_check_types().
             */
            BCOPY(my_groups, save->groups, sizeof(save->groups));
            vnlayer_groups_cred_to_task(current, cred);
#else
            struct group_info *gi_from_cred_p;

            /* Construct our own struct group_info from the creds we got. */
            ASSERT(cred->cr_ngroups <= LINUX_NGROUPS);
            if ((gi_from_cred_p = groups_alloc(cred->cr_ngroups)) == NULL) {
                goto done;
            }
            /* GID's are unsigned int (32 bits) for everybody, so bcopy is OK. */
            BCOPY(cred->cr_groups, gi_from_cred_p->blocks[0],
                  cred->cr_ngroups * sizeof(cred->cr_groups[0]));

            /* Make our creds the current ones.  Save the current group info
            ** and get it because set_current_groups will put it, and we're
            ** saving a pointer to it.  Also, it will do a get on our
            ** group_info (which will be undone in restore below), but we
            ** always need to put to get rid of the ref that alloc made.
            */
            save->saved_group_info = my_group_info;
            get_group_info(save->saved_group_info);
            if (set_current_groups(gi_from_cred_p) != 0) {
                put_group_info(save->saved_group_info);
                put_group_info(gi_from_cred_p);
                goto done;
            } else {
                put_group_info(gi_from_cred_p);
            }
#endif
            save->old_fsuid = current->fsuid;
            save->old_fsgid = current->fsgid;
            current->fsuid = MDKI_CR_GET_UID(cred);
            current->fsgid = MDKI_CR_GET_GID(cred);

            *save_p = save;
            rv = TRUE;
        }
    }
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
  done:                         /* Branching here assumes failure. */
    put_group_info(my_group_info);
#endif
    return(rv);
}

void
vnlayer_fsuid_restore(
    vnlayer_fsuid_save_t *saved_p
)
{
    vnlayer_fsuid_save_struct_t *saved = *saved_p;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    int err;
#endif

    *saved_p = NULL;
    current->fsuid = saved->old_fsuid;
    current->fsgid = saved->old_fsgid;

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
    LINUX_TASK_NGROUPS(current) = saved->ngroups;
    /*
     * We checked the sizes were the same at load time, see
     * vnlayer_check_types().
     */
    BCOPY(saved->groups, LINUX_TASK_GROUPS(current), sizeof(LINUX_TASK_GROUPS(current)));
#else
    /* This will do a put on our group_info (which is current), which should
    ** free it. But it does a get on the saved info which will prevent
    ** it from being released later. So we will need to do another put
    ** after we have done the switch. This mimics what sys_setgroups does.
    */
    if ((err = set_current_groups(saved->saved_group_info)) != 0) {
        MDKI_VFS_LOG(VFS_LOG_WARN,"%s: unable to restore group info, err=%d\n",
                     __func__, err);
    }
    /* We always do the put even on an error because we are about to release
     * the pointer to it so we had better free the memory too.
     */
    put_group_info(saved->saved_group_info);
#endif
    KMEM_FREE(saved, sizeof(*saved));
}

/*
 * Use a bogus operation to trap on any access to cleartext vnodes as
 * genuine inodes.  Cleartext vnodes may only be accessed via VOP_*()
 * calls.
 */

void
vnlayer_bogus_op(void)
{
    printk("Cleartext accessed via inode inode ops or address space ops.\n");
    BUG();
}

void
vnlayer_bogus_vnop(void)
{
    printk("Cleartext accessed via vnode/vfs operation.\n");
    BUG();
}

typedef int (*ino_create_fn_t)(
    struct inode *,
    struct dentry *,
    int
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    , struct nameidata *
#endif
);
typedef struct dentry * (*ino_lookup_fn_t)(
    struct inode *,
    struct dentry *
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    , struct nameidata *
#endif
);
typedef int (*ino_link_fn_t)(
    struct dentry *,
    struct inode *,
    struct dentry *
);
typedef int (*ino_unlink_fn_t)(
    struct inode *,
    struct dentry *
);
typedef int (*ino_symlink_fn_t)(
    struct inode *,
    struct dentry *,
    const char *
);
typedef int (*ino_mkdir_fn_t)(
    struct inode *,
    struct dentry *,
    int
);
typedef int (*ino_rmdir_fn_t)(
    struct inode *,
    struct dentry *
);
typedef int (*ino_mknod_fn_t)(
    struct inode *,
    struct dentry *,
    int,
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
    int
#else
    dev_t
#endif
);
typedef int (*ino_rename_fn_t)(
    struct inode *,
    struct dentry *,
    struct inode *,
    struct dentry *
);
typedef int (*ino_readlink_fn_t)(
    struct dentry *,
    char *,
    int
);
typedef int (*ino_follow_link_fn_t)(
    struct dentry *,
    struct nameidata *
);
typedef void (*ino_truncate_fn_t)(
    struct inode *
);
typedef int (*ino_permission_fn_t)(
    struct inode *,
    int
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    , struct nameidata *
#endif
);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
typedef int (*ino_revalidate_fn_t)(
    struct dentry *
);
#endif
typedef int (*ino_setattr_fn_t)(
    struct dentry *,
    struct iattr *
);
typedef int (*ino_getattr_fn_t)(
    struct dentry *,
    struct iattr *
);

IN_OPS_T vnlayer_clrvnode_iops = {
    .permission = (ino_permission_fn_t)&vnlayer_bogus_op,
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
    .revalidate = (ino_revalidate_fn_t)&vnlayer_bogus_op,
#endif
    .setattr = (ino_setattr_fn_t)&vnlayer_bogus_op,
    .create = (ino_create_fn_t)&vnlayer_bogus_op,
    .lookup = (ino_lookup_fn_t)&vnlayer_bogus_op,
    .link = (ino_link_fn_t)&vnlayer_bogus_op,
    .unlink = (ino_unlink_fn_t)&vnlayer_bogus_op,
    .symlink = (ino_symlink_fn_t)&vnlayer_bogus_op,
    .mkdir = (ino_mkdir_fn_t)&vnlayer_bogus_op,
    .rmdir = (ino_rmdir_fn_t)&vnlayer_bogus_op,
    .mknod = (ino_mknod_fn_t)&vnlayer_bogus_op,
    .rename = (ino_rename_fn_t)&vnlayer_bogus_op,
};

typedef loff_t (*file_llseek_fn_t)(
    struct file *,
    loff_t,
    int
);
typedef ssize_t (*file_read_fn_t)(
    struct file *,
    char *,
    size_t,
    loff_t *
);
typedef ssize_t (*file_write_fn_t)(
    struct file *,
    const char *,
    size_t,
    loff_t *
);
typedef int (*file_readdir)(
    struct file *,
    void *,
    filldir_t
);
typedef unsigned int (*file_poll_fn_t)(
    struct file *,
    struct poll_table_struct *
);
typedef int (*file_ioctl_fn_t)(
    struct inode *,
    struct file *,
    unsigned int,
    unsigned long
);
typedef int (*file_mmap_fn_t)(
    struct file *,
    struct vm_area_struct *
);
typedef int (*file_open_fn_t)(
    struct inode *,
    struct file *
);
typedef int (*file_flush_fn_t)(
    struct file *
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
    , fl_owner_t
#endif
);
typedef int (*file_release_fn_t)(
    struct inode *,
    struct file *
);
typedef int (*file_fsync_fn_t)(
    struct file *,
    struct dentry *,
    int
);
typedef int (*file_fasync_fn_t)(
    int,
    struct file *,
    int
);
typedef int (*file_lock_fn_t)(
    struct file *,
    int,
    struct file_lock *
);
typedef ssize_t (*file_readv_fn_t)(
    struct file *,
    const iovec_t *,
    unsigned long,
    loff_t *
);
typedef ssize_t (*file_writev_fn_t)(
    struct file *,
    const iovec_t *,
    unsigned long,
    loff_t *
);
typedef ssize_t (*file_sendpage_fn_t)(
    struct file *,
    struct page *,
    int,
    size_t,
    loff_t *,
    int
);
typedef unsigned long (*file_get_unmapped_area_fn_t)(
    struct file *,
    unsigned long,
    unsigned long,
    unsigned long,
    unsigned long
);

F_OPS_T vnlayer_clrvnode_fops = {
    .owner =                 THIS_MODULE,
    .llseek = (file_llseek_fn_t) &vnlayer_bogus_op,
    .read = (file_read_fn_t) &vnlayer_bogus_op,
    .write = (file_write_fn_t) &vnlayer_bogus_op,
    .poll = (file_poll_fn_t) &vnlayer_bogus_op,
    .ioctl = (file_ioctl_fn_t) &vnlayer_bogus_op,
    .open = (file_open_fn_t) &vnlayer_bogus_op,
    .flush = (file_flush_fn_t) &vnlayer_bogus_op,
    .release = (file_release_fn_t) &vnlayer_bogus_op,
    .fsync = (file_fsync_fn_t) &vnlayer_bogus_op,
    .lock = (file_lock_fn_t) &vnlayer_bogus_op
};

typedef int (*asop_writepage_fn_t)(
    struct page *
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    , struct writeback_control *
#endif
);
typedef int (*asop_readpage_fn_t)(
    struct file *,
    struct page *
);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
typedef int (*asop_sync_page_fn_t)(
    struct page *
);
#else
typedef void (*asop_sync_page_fn_t)(
    struct page *
);
#endif
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
typedef int (*asop_writepages_fn_t)(
    struct address_space *,
    struct writeback_control *
);
typedef int (*asop_set_page_dirty_fn_t)(
    struct page *page
);
typedef int (*asop_readpages_fn_t)(
    struct file *filp,
    struct address_space *mapping,
    struct list_head *pages,
    unsigned nr_pages
);
#endif
typedef int (*asop_prepare_write_fn_t)(
    struct file *,
    struct page *,
    unsigned,
    unsigned
);
typedef int (*asop_commit_write_fn_t)(
    struct file *,
    struct page *,
    unsigned,
    unsigned
);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
typedef int (*asop_bmap_fn_t)(
    struct address_space *,
    long
);
typedef int (*asop_flushpage_fn_t)(
    struct page *,
    unsigned long
);
#else
typedef sector_t (*asop_bmap_fn_t)(
    struct address_space *,
    sector_t
);
#endif
typedef int (*asop_releasepage_fn_t)(
    struct page *,
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
    int
#else
    gfp_t
#endif
);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
typedef int (*asop_direct_IO_fn_t)(
    int,
    struct file *,
    struct kiobuf *,
    unsigned long,
    int
);
#else /* 2.6.0 or later */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
typedef int (*asop_invalidatepage_fn_t)(
    struct page *,
    unsigned long
);
#else
typedef void (*asop_invalidatepage_fn_t)(
    struct page *,
    unsigned long
);
#endif
typedef ssize_t (*asop_direct_IO_fn_t)(
    int,
    struct kiocb *,
    const struct iovec *,
    loff_t,
    unsigned long
);
#endif /* 2.4 or 2.6 */

struct address_space_operations vnlayer_clrvnode_asops = {
    .writepage = (asop_writepage_fn_t) &vnlayer_bogus_op,
    .readpage = (asop_readpage_fn_t) &vnlayer_bogus_op,
    .sync_page = (asop_sync_page_fn_t) &vnlayer_bogus_op,
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    .writepages = (asop_writepages_fn_t) &vnlayer_bogus_op,
    .set_page_dirty = (asop_set_page_dirty_fn_t) &vnlayer_bogus_op,
    .readpages = (asop_readpages_fn_t) &vnlayer_bogus_op,
#endif
    .prepare_write = (asop_prepare_write_fn_t) &vnlayer_bogus_op,
    .commit_write = (asop_commit_write_fn_t) &vnlayer_bogus_op,
    .bmap = (asop_bmap_fn_t) &vnlayer_bogus_op,
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    .invalidatepage = (asop_invalidatepage_fn_t) &vnlayer_bogus_op,
    .releasepage = (asop_releasepage_fn_t) &vnlayer_bogus_op,
    .direct_IO = (asop_direct_IO_fn_t) &vnlayer_bogus_op,
#endif
};

atomic_t vnlayer_clrvnode_count = ATOMIC_INIT(0);

extern VNODE_T *
vnlayer_linux_new_clrvnode(
    DENT_T *dent,
    struct vfsmount *mnt
#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
    , const char *file,
    const char *func,
    int line
#endif
)
{
    INODE_T *clri;
    VNODE_T *res;

    /*
     * It'd be nice to be able to use vnlayer_cltxt_vfs as the vfsp for a
     * cleartext vnode, but that would require vnlayer_cltxt_vfs to have
     * a real superblock attached to it (for inode allocation) and it
     * doesn't.  So we require the file system using cltxt vnodes to
     * provide a vfsp to hang these cleartext vnodes off
     * (vnlayer_clrvnode_vfsp).
     */
    if (vnlayer_clrvnode_vfsp == NULL) {
        MDKI_VFS_LOG(VFS_LOG_ERR,"%s: clrvnodes not possible until mkdi_set_vnlayer_clrvnode_vfsp() called\n", __func__);
        return NULL;
    }
    if (vnlayer_sysroot_dentry == dent &&
        vnlayer_sysroot_mnt == mnt && vnlayer_sysroot_clrvp != NULL)
    {
        res = vnlayer_sysroot_clrvp;
        MDKI_TRACE(TRACE_VNODES,
                   "new_cvn sysroot=%p dent=%p mnt=%p @ %s:%s:%d\n",
                   res, res->v_dent, res->v_vfsmnt, file, func, line);
        return VN_HOLD(res);
    }
    /*
     * The inodes for cleartext vnodes are not expected ever to be
     * used as inodes by the rest of the system, but we need to
     * allocate an inode for them to handle the allocation for the vnode
     * and the activation/release semantics.
     *
     * Loopback code which uses these vnodes usually ends up going right to
     * the v_dent and v_vfsmnt entries as needed to operate on the
     * inode or dentry/vfsmnt pair representing the underlying object.
     */

    clri = new_inode(VFSTOSB(vnlayer_clrvnode_vfsp));
    if (clri == NULL) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s: out of inodes for clrinode!\n", __func__);
        /* BUG(); */
        return NULL;
    }

    atomic_inc(&vnlayer_clrvnode_count);
    res = ITOV(clri);
    clri->i_version = 0;

    ASSERT(dent);
    if (dent->d_inode) {
        /* copy up some stats in case anybody cares (nobody should) */
        SHADOW_CP_INODAT(dent->d_inode, clri);
        res->v_type = vnlayer_mode_to_vtype(dent->d_inode->i_mode);
    } else {
        /*
         * We sometimes create these objects to hold negative dentries.
         * Just choose an arbitrary mode.
         */
        clri->i_ino = 0;
        clri->i_mode = 0;
        res->v_type = VNON;
    }

    clri->i_data.a_ops = &vnlayer_clrvnode_asops;
    clri->i_fop = &vnlayer_clrvnode_fops;
    clri->i_op = &vnlayer_clrvnode_iops;

    res->v_sanity = VNODE_SANITY;
#if 0
    mdki_init_spinlock(&res->v_lock);
#endif
    res->v_flag = 0;
#if 0
    /*
     * NB: we don't use a vnode reference count, since they're 1-1
     * with inodes, and it simplifies allocation/inactivation to use
     * the inode's count instead.
     */
    res->v_count = 0;
    res->v_rdev = 0;
#endif
    res->v_vfsp = &vnlayer_cltxt_vfs;
    res->v_op = &mvop_cltxt_vnops;
    res->v_data = NULL;
    res->v_dent = (void *)VNODE_DGET(dent);
    res->v_vfsmnt = (void *)MDKI_MNTGET(mnt);

    MDKI_TRACE(TRACE_VNODES, "new_cvn cvp=%p dent=%p mnt=%p @ %s:%s:%d\n",
              res, res->v_dent, res->v_vfsmnt, file, func, line);

    return res;
}

/* wrapper layer */
extern void
vnlayer_linux_free_clrvnode(VNODE_T *cvp)
{
    atomic_dec(&vnlayer_clrvnode_count);
    MDKI_TRACE(TRACE_VNODES, "free_cvn cvp=%p dent=%p mnt=%p\n",
              cvp, cvp->v_dent, cvp->v_vfsmnt);
    ASSERT(cvp->v_sanity == VNODE_SANITY);
    VNODE_DPUT(CVN_TO_DENT(cvp));
    if (cvp->v_vfsmnt)
        MDKI_MNTPUT(CVN_TO_VFSMNT(cvp));
    cvp->v_dent = NULL;
    cvp->v_vfsmnt = NULL;
#if 0
    mdki_free_spinlock(&cvp->v_lock);
#endif
}

/* should be large enough, if not you'll get truncated messages, sorry! */
#define PRINTF_BUFSZ 1024
void
vnlayer_linux_vprintf(
    const char *fmt,
    va_list ap
)
{
    char *buf;

    /* Sigh, the Linux kernel vsnprintf() doesn't grok using null
     * pointer/zero length to inquire about necessary size (C
     * standard).  We use an estimated fixed buffer size.
     */
    buf = mdki_linux_kmalloc(PRINTF_BUFSZ, KM_SLEEP);
    buf[PRINTF_BUFSZ-1] = '\0';
    vsnprintf(buf, PRINTF_BUFSZ-1, fmt, ap);
    printk(buf);
    mdki_linux_kfree(buf, PRINTF_BUFSZ);
    return;
}

#ifdef MVFS_DEBUG
int vnlayer_logging_priority = VFS_LOG_INFO;
#else
int vnlayer_logging_priority = VFS_LOG_WARN;
#endif

const char *vnlayer_log_prefix[] = {
    "(Bad priority) ",
    "Error: ",
    "Warning: ",
    "Info: ",
    "ESTALE: ",
    "Debug: ",
    "ENOENT: "
};

/*
 * Not normally called; most logging goes via mdki_logging_vfsp into the vnode
 * file system.
 */
int
vnlayer_linux_log(
    VFS_T *vfsp,
    int level,
    const char *fmt,
    ...
)
{
    /* compare with mvfs_log() */
    va_list ap;
    if (level <= vnlayer_logging_priority) {
	va_start(ap, fmt);
        /* XXX we'd rather not do this in two pieces, but to do that
         * we would need to use our own printf.  Not worth it.
         */
        mdki_linux_printf((level > 0 && level < VFS_LOG_ENOENT) ?
                          vnlayer_log_prefix[level] : vnlayer_log_prefix[0]);
        vnlayer_linux_vprintf(fmt, ap);
	va_end(ap);
    }
    return 0;
}

#ifdef MVFS_DEBUG
struct vfsmount *vnlayer_debug_mnt = NULL;      /* patch with debugger */

extern struct vfsmount *
vnlayer_debug_mntget(
    struct vfsmount *mnt,
    const char *file,
    const char *func,
    int line
)
{
    if ((vnlayer_debug_mnt != NULL && mnt == vnlayer_debug_mnt) ||
        (mdki_tracing & TRACE_VFSMNT) != 0)
    {
        int cnt = atomic_read(&mnt->mnt_count);
        MDKI_VFS_LOG(VFS_LOG_DEBUG,"VFSMNT: mntget(%p) %d->%d from %p (%s:%s:%d)\n",
                     mnt, cnt, cnt+1, mdki_getmycaller(),
                     file, func, line);
    }
    return(mntget(mnt));
}

extern void
vnlayer_debug_mntput(
    struct vfsmount *mnt,
    const char *file,
    const char *func,
    int line
)
{
    if (mnt != NULL &&
        ((vnlayer_debug_mnt != NULL && mnt == vnlayer_debug_mnt) ||
         (mdki_tracing & TRACE_VFSMNT) != 0))
    {
        int cnt = atomic_read(&mnt->mnt_count);
        MDKI_VFS_LOG(VFS_LOG_DEBUG,"VFSMNT: mntput(%p) %d->%d from %p (%s:%s:%d)\n",
                     mnt, cnt, cnt-1, mdki_getmycaller(),
                     file, func, line);
    }
    mntput(mnt);
}
#endif

/* This function will allocate a new fs_struct and
 * copy into it the contents of the fs_struct for the
 * current process.  It will adjust counts as necessary.
 * It will set the root directory to the system root.
 * Use of this function will assure that all fields are filled
 * in for /proc and other people who may access fields in the
 * fs_struct while we have it switched for a lookup.
 */
extern struct fs_struct *
vnlayer_make_temp_fs_struct(void)
{
    struct fs_struct *new_fs;

    new_fs = mdki_linux_kmalloc(sizeof(*new_fs), KM_SLEEP);
    if (new_fs == NULL)
        return(new_fs);
    atomic_set(&new_fs->count, 1);
    rwlock_init(&new_fs->lock);
    read_lock(&current->fs->lock);
    new_fs->umask = current->fs->umask;
    new_fs->root = dget(vnlayer_sysroot_dentry);
    new_fs->pwd = dget(current->fs->pwd);
    if (current->fs->altroot) {
        new_fs->altroot = dget(current->fs->altroot);
    } else {
        new_fs->altroot = NULL;
    }
    new_fs->rootmnt = mntget(vnlayer_sysroot_mnt);
    new_fs->pwdmnt = mntget(current->fs->pwdmnt);
    if (current->fs->altrootmnt) {
        new_fs->altrootmnt = mntget(current->fs->altrootmnt);
    } else {
        new_fs->altrootmnt = NULL;
    }
    read_unlock(&current->fs->lock);
    return(new_fs);
}

/* Free up a previously allocated fs_struct.  Assumes that it has been
 * removed from the task structure and no one else is using it so that
 * no locking is needed.
 */
extern void
vnlayer_free_temp_fs_struct(struct fs_struct *fs)
{
    dput(fs->root);
    dput(fs->pwd);
    if (fs->altroot) {
        dput(fs->altroot);
    }
    MDKI_MNTPUT(fs->rootmnt);
    MDKI_MNTPUT(fs->pwdmnt);
    if (fs->altrootmnt) {
        MDKI_MNTPUT(fs->altrootmnt);
    }
    mdki_linux_kfree(fs, sizeof(*fs));
    return;
}

extern struct fs_struct *
vnlayer_swap_task_fs(
    struct task_struct *task,
    struct fs_struct *new_fs
)
{
    struct fs_struct *rv;

    task_lock(task);
    rv = task->fs;
    task->fs = new_fs;
    task_unlock(task);
    return rv;
}

/* This function will get a dentry from a given inode.  In Linux 2.4,
 * we will get the dcache_lock while manipulating the lists.  d_find_alias
 * does almost what we want, but it expects there to be multiple dentries.
 * This is not the case for file-system roots, so we will provide our own
 * function.
 * The dentry will be returned with its count incremented.
 */

extern struct dentry *
vnlayer_inode2dentry_internal(
    struct inode *inode,
    struct dentry *parent,
    struct qstr *name,
    struct dentry_operations *ops
)
{
    struct dentry *found = NULL;
    struct list_head *le;
    mdki_boolean_t want_connected = TRUE;

    spin_lock(&dcache_lock);
    if (!list_empty(&inode->i_dentry)) {
      retry:
        list_for_each(le, &inode->i_dentry) {
            found = list_entry(le, struct dentry, d_alias);
            if (ops != NULL && ops != found->d_op) {
                /* don't accept if it's not the right flavor ops */
                found = NULL;
                continue;
            }
            if (parent != NULL && found->d_parent != parent) {
                /* must match requested parent, if any */
                found = NULL;
                continue;
            }
            if (!S_ISDIR(inode->i_mode)) {
                /*
                 * If the object is not a directory, the caller may
                 * want an entry with the same parent and the same
                 * name.  (This will keep things like rename & unlink
                 * happy).
                 */
                if (name != NULL && !vnlayer_names_eq(&found->d_name, name)) {
                    /* must have same leaf name */
                    found = NULL;
                    continue;
                }
                /* don't accept it if it's not hashed (it wants to go away) */
                if (DENT_HASH_IS_EMPTY(found)) {
                    found = NULL;
                    continue;
                }
            }
            /*
             * There may be "temporary" dcache entries attached to
             * this inode which are being used by the NFS server (it
             * will build up the namespace moving up the directory
             * tree until it finds a connected dentry).  If there are
             * multiple dentries on this inode, we want to find a
             * connected one if it's present, since that simplifies the
             * NFS server's job.  If there are no connected entries,
             * we try again and accept a disconnected entry.
             */
            if (!want_connected || (found->d_flags & NFSD_DCACHE_DISCON) == 0)
            {
                dget_locked(found);
                /*
                 * remove REFERENCED flag; we have our own cache and
                 * we don't really even want to be in the dcache.
                 * (this is an attempt to age ourselves out of the
                 * dcache quickly; it may hurt NFS performance unless
                 * NFSEXP_NOSUBTREECHECK is on).
                 */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,7)
                found->d_flags &= ~DCACHE_REFERENCED;
#else
                found->d_vfs_flags &= ~DCACHE_REFERENCED;
#endif
                break;
            }
            found = NULL;
        }
        if (!found && want_connected) {
            /* try again, but accept a disconnected entry this time */
            want_connected = FALSE;
            goto retry;
        }
    }
    spin_unlock(&dcache_lock);
    return(found);
}

/*
 * VNLAYER_GET_UCDIR_INODE - get current dir of current process
 */

struct inode *
vnlayer_get_ucdir_inode(void)
{
    return(current->fs->pwd->d_inode);
}

/*
 * VNLAYER_GET_URDIR_INODE - get root dir of current process
 */

struct inode *
vnlayer_get_urdir_inode(void)
{
    return(current->fs->root->d_inode);
}

/* VNLAYER_GET_ROOT_MNT - Get the root vfsmnt structure for a process
 */
struct vfsmount *
vnlayer_get_root_mnt(void)
{
    return(mntget(current->fs->rootmnt));
}

/*
 * VNLAYER_GET_ROOT_DENTRY - gets the root dentry for a process
 */

struct dentry *
vnlayer_get_root_dentry(void)
{
    return(current->fs->root);
}

#ifdef MDKI_SET_PROC_RDIR
#ifdef MVFS_DEBUG
#define URDENT_DGET(dent,file,func,line) vnode_dget(dent,file,func,line)
#define URDENT_DPUT(dent,file,func,line) vnode_dput(dent,file,func,line)
#else
#define URDENT_DGET(dent,file,func,line) dget(dent)
#define URDENT_DPUT(dent,file,func,line) dput(dent)
#endif

void
vnlayer_set_urdent(
    struct dentry *new_rdir,
    struct vfsmount *new_rmnt
)
{
    if (new_rdir == NULL) {
        VNLAYER_SET_FS_ROOT(current->fs, vnlayer_sysroot_mnt,
                            vnlayer_sysroot_dentry);
        return;
    }
    VNLAYER_SET_FS_ROOT(current->fs, new_rmnt, new_rdir);
    /* release extra reference picked up by set_fs_root() */
    URDENT_DPUT(new_rdir,__FILE__,__func__,__LINE__);
    MDKI_MNTPUT(new_rmnt);
}
#undef URDENT_DGET
#undef URDENT_DPUT
#endif /* MDKI_SET_PROC_RDIR */

/*
 * Do we know how to create hard links to this object?
 */
mdki_boolean_t
vnlayer_link_eligible(const struct dentry *dent)
{
    if (MDKI_INOISOURS(dent->d_inode))
        return TRUE;
    switch (dent->d_inode->i_mode & S_IFMT) {
      case S_IFSOCK:
      case S_IFBLK:
      case S_IFCHR:
        if (dent->d_op == &vnode_shadow_dentry_ops)
            return TRUE;
        else
            return FALSE;
      default:
        return FALSE;
    }
}

int
mdki_snprintf(
    char *str,
    size_t limit,
    const char *fmt,
    ...
)
{
    int ret;
    va_list ap;

    va_start(ap, fmt);
    ret = vsnprintf(str, limit, fmt, ap);
    va_end(ap);
    return (ret);
}

int
mdki_vsnprintf(
    char *str,
    size_t limit,
    const char *fmt,
    va_list ap
)
{
    return vsnprintf(str, limit, fmt, ap);
}
static const char vnode_verid_mvfs_linux_utils_c[] = "$Id:  d8ddce93.541d11dd.90ce.00:01:83:09:5e:0d $";
