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
/*
 * In order to implement our recursion from /view/<viewtag> to / under
 * Linux, we have to create a separate shadow file hierarchy for the
 * dcache for each view in use.  For directories, we use the same
 * strategy as for other platforms (vnode referencing view context and
 * pointing to underlying object).  For non-directory files, we create
 * shadow dentries and inodes that serve only to redirect their calls
 * from the shadow objects to the real versions.  (We cannot use the
 * real dentry because it's linked to the real directory, which causes
 * problems with name space operations.  There may be other problems
 * with using real files' dentries.)
 */

#include "vnode_linux.h"
#include "mvfs_linux_shadow.h"

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,33)
spinlock_t vnlayer_cvn_spinlock = SPIN_LOCK_UNLOCKED;
#else
DEFINE_SPINLOCK(vnlayer_cvn_spinlock);
#endif

#if defined(CONFIG_SMP) && defined(MVFS_DEBUG)
struct vnlayer_cvn_debug_info vnlayer_cvn_debug = {0};
struct vnlayer_cvn_debug_info vnlayer_cvn_old_debug[HISTCOUNT] = {{0}};
#endif /* CONFIG_SMP && MVFS_DEBUG */

/* Dentry operations */

/* This function returns TRUE if the dentry is still valid and FALSE if not.
 * This is a much simplified version of vnode_dop_revalidate().
 * The basic principle is that it will let shadow_d_release actually do the
 * cleanup.
 */

extern int
vnode_shadow_dop_revalidate(
    DENT_T *dentry_p,
    struct nameidata *nd
)
{
    int err;
    DENT_T *real_dentry;
    VNODE_T *cvp;

    real_dentry = REALDENTRY_LOCKED(dentry_p, &cvp);
    if ((!real_dentry) || d_unhashed(real_dentry)) {
        err = FALSE;
 	if (real_dentry)
            /* drop the real entry, it's stale */
            REALDENTRY_UNLOCK_DETACH(dentry_p, cvp);
    } else {
        VNODE_DGET(real_dentry);
        err = TRUE;
        if (real_dentry->d_op && real_dentry->d_op->d_revalidate) {
            err = (*real_dentry->d_op->d_revalidate)(real_dentry, nd);
            /* Recreate what cached lookup would do to the real dentry.
             * Note that d_invalidate only returns non-zero if it is
             * called for a directory still in use.  We should never
             * have a directory here.
             */
            if (!err) {
                d_invalidate(real_dentry);
            }
        }
        /* if the dentry is still valid, we need to update the shadow
         * inode to match the values in the real inode.
         */
        if (err && dentry_p->d_inode && real_dentry->d_inode) {
            /* The dentry is valid, update the inode in case it has
             * changed since the last time we looked.
             */
            SHADOW_CP_INODAT(real_dentry->d_inode, dentry_p->d_inode);
        }
        VNODE_DPUT(real_dentry);
        REALDENTRY_UNLOCK(dentry_p, cvp);
    }
    return(err);
}


/*
 * shadow_dop_release doesn't bother with getting the mvfs_cvn_lock before
 * checking on the existence of the CVN because we are so far gone at this
 * point that nobody else should be looking at this entry anyway.
 */
extern void
vnode_shadow_dop_release(DENT_T *dentry)
{
    MDKI_TRACE(TRACE_DCACHE, "%s: dp %p parent %p real %p\n", __func__,
               dentry, dentry->d_parent, REALCVN(dentry));
    ASSERT(!dentry->d_inode);
    if (dentry->d_inode)
        iput(dentry->d_inode);
    if (REALCVN(dentry)) {
/*        ASSERT(V_COUNT(REALCVN(dentry)) == 1);*/
        DENT_VN_RELE(dentry,TRUE);    /* Release both dent and mnt. */
    }
}

struct dentry_operations vnode_shadow_dentry_ops = {
    .d_revalidate =     vnode_shadow_dop_revalidate,
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
    .d_delete =         (int (*)(const struct dentry *)) vnode_dop_delete,
#else
    .d_delete =         vnode_dop_delete,
#endif
    .d_release =        vnode_shadow_dop_release,
};

#ifdef HAVE_SHADOW_FILES
/* File Operations */

extern int
vnode_shadow_fop_mmap(
    FILE_T *file_p,
    struct vm_area_struct *vm_p
)
{
    /*
     * I wrote this and then we figured out a way to make the open
     * point to the real file ops, so this should never get called.
     * But load_elf_binary wants to see an entry in the dispatch table
     * so we will leave a dummy entry behind
     */
    printk("%s: This function should never be called\n", __func__);
    return -EINVAL;
}

extern int
vnode_shadow_fop_open(
    INODE_T *inode,
    FILE_T *file
)
{
    int err = 0;
    INODE_T *real_inode;
    DENT_T *rdentry = NULL;
    DENT_T *oldent;
    struct file_operations *oldfops;
    struct vfsmount *oldmnt, *newmnt;
    VNODE_T *cvp;

    oldmnt = file->f_vfsmnt;

    oldent = file->f_dentry;
    ASSERT(D_COUNT(oldent));
    /* The Linux kernel has stopped ignoring the O_DIRECT flag.
     * The problem is that they wait until after they call the fop open
     * function to check the inode to see if it will support direct I/O.
     * But they get the inode pointer before they call us and check the
     * inode after we return so they never check the actual inode we open
     * but only the shadow one.  Their error handling never comes back to
     * us and they release their old pointers and not our new ones.  The
     * only choice we have is to not allow O_DIRECT on shadow files.
     */
    if (file->f_flags & O_DIRECT) {
        err = -EINVAL;
        goto out_nolock;
    }
    /* Get the real dentry */
    rdentry = REALDENTRY_LOCKED(oldent, &cvp);
    if (rdentry == NULL) {
        err = -ENOENT;
	goto out_nolock;
    }
    VNODE_DGET(rdentry);                 /* protect rdentry->d_inode */
    if (rdentry->d_inode == NULL) {
        /* delete race */
        err = -ENOENT;
        goto out;
    }
    newmnt = MDKI_MNTGET(REALVFSMNT(oldent));
    if (newmnt == NULL) {
        err = -EOPNOTSUPP;             /* XXX */
        goto out;
    }

    /* Check that we can write to this file.  Clean up the count on the
     * shadow inode.
     */
    if (file->f_mode & FMODE_WRITE) {
        err = get_write_access(rdentry->d_inode);
        if (err) {
            MDKI_MNTPUT(newmnt);
            goto out;
        }
    }
    real_inode = rdentry->d_inode;

    /* 
     * Swap the file structure contents to point at the underlying object.
     */
    /* In Linux 2.6 they added the mapping stuff to the file so we have to set
    ** that up here, too.
    */
    file->f_mapping = real_inode->i_mapping;
    VNLAYER_RA_STATE_INIT(&(file->f_ra), file->f_mapping);
    file->f_dentry = VNODE_DGET(rdentry);
    oldfops = file->f_op;
    file->f_vfsmnt = newmnt;
    file->f_op = fops_get(real_inode->i_fop);
    if (real_inode->i_fop && !file->f_op)
        /* If we failed to get the reference to a non-NULL op, bail out */
        err = -EIO;                     /* XXX? better error code */
    if (!err) {
	/* Move the file to the file list for the real superblock 
	 * and remove it from the shadow list
	 */
        /* It would be better to use file_move() but it's not exported */
	file_list_lock();
        list_del(&file->f_list);
        list_add(&file->f_list, &real_inode->i_sb->s_files);
	file_list_unlock();
	if (file->f_op && file->f_op->open) {
            err = (*file->f_op->open)(real_inode, file);
            if (err) {
	        /* restore our file to the list on our super block */
	        file_list_lock();
	        list_del(&file->f_list);
	        list_add(&file->f_list, &oldent->d_inode->i_sb->s_files);
	        file_list_unlock();
	    }
	}
    }
    if (err) {
        /* MUST put back old dentry/fops to get accounting right in upper
         * layer. */
        put_write_access(rdentry->d_inode);
        if (file->f_dentry)
            VNODE_DPUT(file->f_dentry);
        if (file->f_op)
            fops_put(file->f_op);
        MDKI_MNTPUT(file->f_vfsmnt);
        file->f_vfsmnt = oldmnt;
        file->f_dentry = oldent;
        file->f_op = oldfops;
    } else {
        put_write_access(oldent->d_inode);
        VNODE_DPUT(oldent);
        /* Drop reference now that we've dropped our use of the file ops */
        fops_put(oldfops);
        MDKI_MNTPUT(oldmnt);
    }
  out:
    VNODE_DPUT(rdentry);
    REALDENTRY_UNLOCK(oldent, cvp);
  out_nolock:
    MDKI_TRACE(TRACE_OPEN, "%s: opened vp=%p fp=%p rdent=%p rdcnt=%d fcnt=%d"
              ", err %d\n", __func__,
              inode, file, rdentry, rdentry ? D_COUNT(rdentry) : 0, F_COUNT(file), -err);
    return(err);
}

/*
 * We don't do any file ops on shadow files directly except the open.
 * At open time we substitute the real dentry for the shadow dentry in
 * the file structure and use the file ops from the real inode; then
 * all operations are done to the real object and our shadow code
 * isn't involved.  We need to provide a dummy mmap function because
 * load_elf_binary looks for the presence of that function before it
 * does the open.
 */

F_OPS_T vnode_shadow_file_ops ={
        .owner =        THIS_MODULE,
        .open =         &vnode_shadow_fop_open,
};

/* The following table is used for shadow files whose real files support
 * mapping opereations.
 */
F_OPS_T vnode_shadow_mapped_file_ops ={
        .owner =        THIS_MODULE,
        .mmap =         &vnode_shadow_fop_mmap,
        .open =         &vnode_shadow_fop_open,
};
#endif /* HAVE_SHADOW_FILES */

/* Inode Operations */

extern int
vnode_shadow_iop_readlink(
    DENT_T *dentry,
    char * buf,
    int buflen
)
{
    DENT_T *real_dentry;
    INODE_T *real_inode;
    VNODE_T *cvp;
    int err = 0;

    real_dentry = REALDENTRY_LOCKED(dentry, &cvp);
    if (real_dentry == NULL)
        /* We lost a race with delete. */
        return -ENOENT;

    VNODE_DGET(real_dentry);
    if (real_dentry->d_inode == NULL) {
        /* We lost a race with delete. */
        err = -ENOENT;
        goto out;
    }
    real_inode = real_dentry->d_inode;
    if (real_inode->i_op && real_inode->i_op->readlink) {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
        /* In the 2.6.16 kernel update_atime is replaced by touch_atime
         * which takes a pointer to the vfsmount structure as well.  We
         * don't have such a beast so we just punt since this will go
         * away when we get rid of the shadow filesystem.
         */
        update_atime(real_inode);
#endif
        /* Our caller has set the address space according to where the buffer is. */
        err = (*real_inode->i_op->readlink)(real_dentry, buf, buflen);
        dentry->d_inode->i_atime = real_inode->i_atime;
    } else {
        err = -EINVAL;
    }
  out:
    VNODE_DPUT(real_dentry);
    REALDENTRY_UNLOCK(dentry, cvp);
    return(err);
}


#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
/* The return pointer points to a cookie to be given to put_link() to clean
** stuff up if necessary.
*/
void *
#else
int
#endif
vnode_shadow_iop_follow_link(
    DENT_T *dentry,            /* entry we are trying to resolve */
    struct nameidata *nd       /* Contains parent dentry */
)
{
    int err = 0;
    int len = PATH_MAX;
    char *buff;
    mm_segment_t old_fs;        /* Because we provide a kernel buffer. */
    INODE_T *real_inode;
    DENT_T *real_dentry;
    VNODE_T *cvp;

    /* this function must consume a reference on base */
    /* We only path_release on error. */

    err = 0;

    real_dentry = REALDENTRY_LOCKED(dentry, &cvp);
    if (real_dentry == NULL) {
	err = -ENOENT;
        MDKI_PATH_RELEASE(nd);
        goto out_nolock;
    }
    VNODE_DGET(real_dentry);             /* protect inode */
    if (real_dentry->d_inode == NULL) {
        /* delete race */
	err = -ENOENT;
        MDKI_PATH_RELEASE(nd);
        goto out;
    }
    real_inode = real_dentry->d_inode;
    /* If there are no underlying symlink functions, we are done */
    if (real_inode->i_op && real_inode->i_op->readlink &&
        real_inode->i_op->follow_link)
    {
        buff = KMEM_ALLOC(len, KM_SLEEP);
        if (!buff) {
            MDKI_PATH_RELEASE(nd);
            err = -ENOMEM;
            goto out;
        }
        /* We're providing a kernel buffer to copy into, so let everyone know. */
        old_fs = get_fs();
        set_fs(KERNEL_DS);
        err = vnode_shadow_iop_readlink(dentry, buff, len);
        set_fs(old_fs);
        if (err < 0) {
            KMEM_FREE(buff, len);
            MDKI_PATH_RELEASE(nd);
            goto out;
        }
        /* done with dentry */
        /* Make sure string is null terminated */
        buff[err] = 0;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
        err = vfs_follow_link(nd, buff);
        KMEM_FREE(buff,len);
#else
        VNODE_DPUT(real_dentry);
        REALDENTRY_UNLOCK(dentry, cvp);
        nd_set_link(nd, buff);
        return(buff); /* vnop_iop_put_link() will free this buf. */
#endif
    }
out:
    VNODE_DPUT(real_dentry);
    REALDENTRY_UNLOCK(dentry, cvp);
out_nolock:
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
    return ERR_PTR(err);
#else
    return(err);
#endif
}

extern void
vnode_shadow_iop_truncate(INODE_T *inode)
{
    DENT_T *dentry;
    DENT_T *real_dentry;
    VNODE_T *cvp;
    int err = 0;

    dentry = MVOP_DENT(inode, NULL);
    ASSERT(dentry != NULL);
    real_dentry = REALDENTRY_LOCKED(dentry, &cvp);

    VNODE_DPUT(dentry);
    if (real_dentry == NULL) {
        /* Someone got rid of this file out from under us */
	return;
    }
    VNODE_DGET(real_dentry);             /* protect inode */
    if (real_dentry->d_inode != NULL) {
        err = vnlayer_truncate_inode(real_dentry, CVN_TO_VFSMNT(cvp),
                                     READ_I_SIZE(inode), FALSE);
        SHADOW_CP_CTIME(real_dentry->d_inode, inode);
    }
    VNODE_DPUT(real_dentry);
    REALDENTRY_UNLOCK(dentry, cvp);
    return;
}

/* This function will have to duplicate most of what permission does
 * if there isn't a permission function for the real file system.  In fact
 * we will have to do it twice.  While the modes and flags for the two
 * inodes should be identical, there are also checks against flags in the
 * super blocks.  These could differ, for instance if the underlying FS
 * is read only.  Our philosophy will be that permissions on the real
 * file system will take precedence over permissions on the shadow file.
 */

extern int
vnode_shadow_iop_permission(
    INODE_T *inode,
    int mask
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
    , struct nameidata *nd
#endif
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
    , unsigned int flags
#endif
)
{
    INODE_T *real_inode;
    DENT_T *dp;
    int err;

    /* we don't have multiple dentries per shadow inode, so this is OK */
    dp = vnlayer_inode2dentry_internal(inode, NULL, NULL, NULL);
    ASSERT(dp != NULL);
    real_inode = REALDENTRY(dp)->d_inode;

    if (real_inode == NULL) {        /* just in case; not expected! */
        VNODE_DPUT(dp);
        return -EACCES;
    }
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
    err = permission(real_inode, mask, nd);
#else
    err = inode_permission(real_inode, mask);
#endif

    if (err == 0) {
        /* don't call permission() on inode, it will call back to us! */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,10)
        err = vfs_permission(inode, mask);
#elif LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
        err = generic_permission(inode, mask, NULL);
#else
        err = generic_permission(inode, mask, flags, NULL);
#endif
    }
    VNODE_DPUT(dp);
    return(err);
}

extern int
vnode_shadow_iop_getattr(
    struct vfsmount *mnt,
    DENT_T *dentry,
    struct kstat *kstat
)
{
    DENT_T *rdentry;
    VNODE_T *cvp;
    int err;

    rdentry = REALDENTRY_LOCKED(dentry, &cvp);
    if (rdentry == NULL) {
        err = -EOPNOTSUPP;             /* XXX */
    } else {
        VNODE_DGET(rdentry);                 /* protect inode */
        if (rdentry != NULL && rdentry->d_inode != NULL) {
            err = vfs_getattr(CVN_TO_VFSMNT(cvp), rdentry, kstat);
            if (err == 0) {
                SHADOW_CP_INODAT(dentry->d_inode, rdentry->d_inode);
            }
        } else
            err = -EOPNOTSUPP;              /* XXX */
        VNODE_DPUT(rdentry);
        REALDENTRY_UNLOCK(dentry, cvp);
    }
    return(err);
}

/* All 4 of the xattr functions are called with the BKL held */

extern int
vnode_shadow_iop_setxattr(
    struct dentry *dentry,
    const char *name,
    const void *value,
    size_t size,
    int flags
)
{
    DENT_T *rdent;
    VNODE_T *cvp;
    int err;

    rdent = REALDENTRY_LOCKED(dentry, &cvp);
    err = vnlayer_do_setxattr(rdent, name, value, size, flags);
    REALDENTRY_UNLOCK(dentry, cvp);
    return err;
}

extern ssize_t
vnode_shadow_iop_getxattr(
    struct dentry *dentry,
    const char *name,
    void *value,
    size_t size
)
{
    DENT_T *rdent;
    VNODE_T *cvp;
    ssize_t rsize;

#if defined(RATL_SUSE)
    LOCK_INODE(dentry->d_inode);
#endif
    rdent = REALDENTRY_LOCKED(dentry, &cvp);
    rsize = vnlayer_do_getxattr(rdent, name, value, size);
    REALDENTRY_UNLOCK(dentry, cvp);
#if defined(RATL_SUSE)
    UNLOCK_INODE(dentry->d_inode);
#endif
    return rsize;
}

extern ssize_t
vnode_shadow_iop_listxattr(
    struct dentry *dentry,
    char *name,
    size_t size
)
{
    DENT_T *rdent;
    VNODE_T *cvp;
    ssize_t rsize;

#if defined(RATL_SUSE)
    LOCK_INODE(dentry->d_inode);
#endif
    rdent = REALDENTRY_LOCKED(dentry, &cvp);
    rsize = vnlayer_do_listxattr(rdent, name, size);
    REALDENTRY_UNLOCK(dentry, cvp);
#if defined(RATL_SUSE)
    UNLOCK_INODE(dentry->d_inode);
#endif
    return rsize;
}

extern int
vnode_shadow_iop_removexattr(
    struct dentry *dentry,
    const char *name
)
{
    DENT_T *rdent;
    VNODE_T *cvp;
    int err;

    rdent = REALDENTRY_LOCKED(dentry, &cvp);
    err = vnlayer_do_removexattr(rdent, name);
    REALDENTRY_UNLOCK(dentry,cvp);
    return err;
}

#ifdef HAVE_SHADOW_FILES
IN_OPS_T vnode_shadow_reg_inode_ops = {
    .truncate =         &vnode_shadow_iop_truncate,
    .permission =       &vnode_shadow_iop_permission,
    .getattr =          &vnode_shadow_iop_getattr,
    .setattr =          &vnode_iop_notify_change,
    .setxattr =         &vnode_shadow_iop_setxattr,
    .getxattr =         &vnode_shadow_iop_getxattr,
    .listxattr =        &vnode_shadow_iop_listxattr,
    .removexattr =      &vnode_shadow_iop_removexattr,
};
#endif /* HAVE_SHADOW_FILES */

IN_OPS_T vnode_shadow_slink_inode_ops = {
    .readlink =         &vnode_shadow_iop_readlink,
    .follow_link =      &vnode_shadow_iop_follow_link,
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
    .put_link = 	&vnode_iop_put_link,
#endif
    .permission =       &vnode_shadow_iop_permission,
    .getattr =          &vnode_shadow_iop_getattr,
    .setattr =          &vnode_iop_notify_change,
    .setxattr =         &vnode_shadow_iop_setxattr,
    .getxattr =         &vnode_shadow_iop_getxattr,
    .listxattr =        &vnode_shadow_iop_listxattr,
    .removexattr =      &vnode_shadow_iop_removexattr,
};

static const char vnode_verid_mvfs_linux_shadow_c[] = "$Id:  0fdb51bb.a4e111e1.89d5.00:01:84:c3:8a:52 $";
