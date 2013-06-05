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

#include "vnode_linux.h"

/* File Operations (and some utility functions) for MVFS */

extern loff_t
vnode_fop_llseek(
    FILE_T *file_p,
    loff_t offset,
    int origin
);
extern ssize_t
vnode_fop_read(
    FILE_T *file_p,
    char *buf,
    size_t buflen,
    loff_t *off_p
);
extern ssize_t
vnode_fop_write(
    FILE_T *file_p,
    const char * buf,
    size_t buflen,
    loff_t *off_p
);
extern ssize_t
vnode_fop_rdwr(
    FILE_T *file_p,
    char * buf,
    size_t buflen,
    loff_t *off_p,
    uio_rw_t dir
);
extern int
vnode_fop_readdir(
    FILE_T *file_p,
    void *dirent_p,
    filldir_t filldir_func
);
extern unsigned int
vnode_fop_poll(
    FILE_T *file_p,
    struct poll_table_struct *pt_p
);
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
extern long
vnode_fop_ioctl(
    FILE_T *file_p,
    uint cmd,
    ulong arg
);
#else /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36) */
extern int
vnode_fop_ioctl(
    INODE_T *ino_p,
    FILE_T *file_p,
    uint cmd,
    ulong arg
);
#endif /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36) */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16) && defined(RATL_COMPAT32)
extern long
vnode_fop_compat_ioctl(
    FILE_T *file_p,
    uint cmd,
    ulong arg
);
#endif
extern int
vnode_fop_mmap(
    FILE_T *file_p,
    struct vm_area_struct *mem_p
);
extern int
vnode_fop_open(
    INODE_T *ino_p,
    FILE_T *file_p
);
extern int
vnode_fop_flush(
    FILE_T *file_p
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
    , fl_owner_t id
#endif
);
extern int
vnode_fop_release(
    INODE_T *ino_p,
    FILE_T *file_p
);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,35)
extern int
vnode_fop_fsync(
    FILE_T *file_p,
    DENT_T *dentry_p,
    int datasync
);
#elif defined(MRG)
extern int
vnode_fop_fsync(
    FILE_T *file_p,
    int datasync
);
#else
extern int
vnode_fop_fsync(
    FILE_T *file_p,
    loff_t start,
    loff_t end,
    int datasync
);
#endif
#if defined(RATL_SUSE) && RATL_VENDOR_VER == 900

/* The this covers the SLES9 kernel which doesn't have the hooks needed
 * to provide locking on the underlying cleartext.
 */

F_OPS_T vnode_file_file_ops = {
        .owner =              THIS_MODULE,
        .llseek =             &vnode_fop_llseek,
        .read =               &vnode_fop_read,
        .write =              &vnode_fop_write,
        .poll =               &vnode_fop_poll,
# if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
        .unlocked_ioctl =     &vnode_fop_ioctl,
# else
        .ioctl =              &vnode_fop_ioctl,
# endif
        .open =               &vnode_fop_open,
        .flush =              &vnode_fop_flush,
        .release =            &vnode_fop_release,
        .fsync =              &vnode_fop_fsync,
};

F_OPS_T vnode_file_mmap_file_ops = {
        .owner =              THIS_MODULE,
        .llseek =             &vnode_fop_llseek,
        .read =               &vnode_fop_read,
        .write =              &vnode_fop_write,
        .poll =               &vnode_fop_poll,
# if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
        .unlocked_ioctl =     &vnode_fop_ioctl,
# else
        .ioctl =              &vnode_fop_ioctl,
# endif
        .mmap =               &vnode_fop_mmap,
        .open =               &vnode_fop_open,
        .flush =              &vnode_fop_flush,
        .release =            &vnode_fop_release,
        .fsync =              &vnode_fop_fsync,
};
F_OPS_T vnode_dir_file_ops = {
        .owner =              THIS_MODULE,
        .llseek =             &vnode_fop_llseek,
        /* generic_read_dir just returns EISDIR */
        .read =               &generic_read_dir,
        .readdir =            &vnode_fop_readdir,
        .open =               &vnode_fop_open,
        .release =            &vnode_fop_release,
        .fsync =              &vnode_fop_fsync,
};
#else /* Not SLES9 */
extern int
vnode_fop_lock(
    FILE_T *file_p,
    int cmd,
    struct file_lock *lock_p
);

F_OPS_T vnode_file_file_ops = {
        .owner =              THIS_MODULE,
        .llseek =             &vnode_fop_llseek,
        .read =               &vnode_fop_read,
        .write =              &vnode_fop_write,
        .poll =               &vnode_fop_poll,
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16) && defined(RATL_COMPAT32)
        .compat_ioctl =       &vnode_fop_compat_ioctl,
#else
# if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
        .unlocked_ioctl =     &vnode_fop_ioctl,
# else
        .ioctl =              &vnode_fop_ioctl,
# endif
#endif
        .open =               &vnode_fop_open,
        .flush =              &vnode_fop_flush,
        .release =            &vnode_fop_release,
        .fsync =              &vnode_fop_fsync,
        .lock =               &vnode_fop_lock,
};

F_OPS_T vnode_file_mmap_file_ops = {
        .owner =              THIS_MODULE,
        .llseek =             &vnode_fop_llseek,
        .read =               &vnode_fop_read,
        .write =              &vnode_fop_write,
        .poll =               &vnode_fop_poll,
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16) && defined(RATL_COMPAT32)
        .compat_ioctl =       &vnode_fop_compat_ioctl,
#elif LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
        .unlocked_ioctl =     &vnode_fop_ioctl,
#else
        .ioctl =              &vnode_fop_ioctl,
#endif
        .mmap =               &vnode_fop_mmap,
        .open =               &vnode_fop_open,
        .flush =              &vnode_fop_flush,
        .release =            &vnode_fop_release,
        .fsync =              &vnode_fop_fsync,
        .lock =               &vnode_fop_lock,
};
F_OPS_T vnode_dir_file_ops = {
        .owner =              THIS_MODULE,
        .llseek =             &vnode_fop_llseek,
        /* generic_read_dir just returns EISDIR */
        .read =               &generic_read_dir,
        .readdir =            &vnode_fop_readdir,
        .open =               &vnode_fop_open,
        .release =            &vnode_fop_release,
        .fsync =              &vnode_fop_fsync,
        .lock =               &vnode_fop_lock,
};
#endif

STATIC int
vnlayer_filep_to_flags(FILE_T *file_p)
{
    int flags = 0;
    /* fp->f_flags includes O_CREAT, etc. */
    /* fp->f_mode is FMODE_READ|FMODE_WRITE */
    if (file_p->f_mode & FMODE_READ)
        flags |= FREAD;
    if (file_p->f_mode & FMODE_WRITE)
        flags |= FWRITE;
    if (file_p->f_flags & O_CREAT)
        flags |= FCREAT;
    if (file_p->f_flags & O_APPEND)
        flags |= FAPPEND;
    if (file_p->f_flags & O_TRUNC)
        flags |= FTRUNC;
    return flags;
}

loff_t
vnode_fop_llseek(
    FILE_T *file_p,
    loff_t offset,
    int origin
)
{
    INODE_T *ip = file_p->f_dentry->d_inode;
    loff_t result;
    MOFFSET_T mresult;
    struct seek_ctx ctx;
    int err;

    ASSERT(MDKI_INOISMVFS(ip));

    switch (origin) {
      case /* SEEK_SET */ 0:
        result = offset;
        break;
      case /* SEEK_CUR */ 1:
        result = offset + file_p->f_pos;
        break;
      case /* SEEK_END */ 2:
        result = offset + READ_I_SIZE(ip);
        break;
      default:
#ifdef MVFS_DEBUG
        MDKI_VFS_LOG(VFS_LOG_INFO,
                     "%s: invalid origin %d, ra=%p\n",
                     __func__, origin, mdki_getmycaller());
#endif
        return -EINVAL;
    }

    ctx.filep = file_p;
    ctx.done = FALSE;
    ctx.offset = offset;
    ctx.origin = origin;
    mresult = result;
    err = VOP_SEEK(ITOV(ip), file_p->f_pos, &mresult, &ctx);
    err = mdki_errno_unix_to_linux(err);
    result = mresult;

    if (err) {
        ASSERT(err < 0);
        return err;
    }
    if (!ctx.done && result != file_p->f_pos) {
        file_p->f_pos = result;
        file_p->f_version = 0;  /* See default_llseek() in fs/read_write.c */
    }
    return result;
}

ssize_t
vnode_fop_read(
    FILE_T *file_p,
    char *buf,
    size_t buflen,
    loff_t *off_p
)
{
    ASSERT_KERNEL_UNLOCKED();
    return vnode_fop_rdwr(file_p, buf, buflen, off_p, UIO_READ);
}

ssize_t
vnode_fop_write(
    FILE_T *file_p,
    const char * buf,
    size_t buflen,
    loff_t *off_p
)
{
    ASSERT_KERNEL_UNLOCKED();
    return vnode_fop_rdwr(file_p, (char *)buf, buflen, off_p, UIO_WRITE);
}

ssize_t
vnode_fop_rdwr(
    FILE_T *file_p,
    char * buf,
    size_t buflen,
    loff_t *off_p,
    uio_rw_t dir
)
{
    int rval;
    int ioflag;
    INODE_T *ip;
    CALL_DATA_T cd;
    loff_t loff;
    struct uio uio;
    iovec_t iov;

    ip = file_p->f_dentry->d_inode;

    ASSERT(MDKI_INOISOURS(ip));

    if (MDKI_INOISMVFS(ip)) {

        uio.uio_iov = &iov;

        if (file_p->f_flags & O_APPEND) {
            ioflag = FAPPEND;
            if (dir == UIO_WRITE)
                loff = READ_I_SIZE(file_p->f_dentry->d_inode);
            else
                loff = *off_p;
        } else {
            ioflag = 0;
            loff = *off_p;
        }

        mdki_linux_uioset(&uio, buf, buflen, loff, UIO_USERSPACE);
        mdki_linux_init_call_data(&cd);
        rval = VOP_RDWR(ITOV(ip), &uio, dir, ioflag, NULL, &cd, 
                        (file_ctx *)file_p);
        rval = mdki_errno_unix_to_linux(rval);
        mdki_linux_destroy_call_data(&cd);
        if (rval == 0) {
            rval = buflen - uio.uio_resid; /* count of transferred bytes */
            *off_p = uio.uio_offset;    /* underlying FS sets it after write */
        }
    } else {
        MDKI_TRACE(TRACE_RDWR,"shadow rdwr? fp=%p ip=%p dir=%d\n",
                  file_p, ip, dir);
        rval = -ENOSYS;
    }
    return rval;
}

/* XXX code which calls us assigns the mask to an unsigned long.  duh. */
unsigned int
vnode_fop_poll(
    FILE_T *fp,
    struct poll_table_struct *pt_p
)
{
    FILE_T *realfp;
    /* claim these are ready unless we can find out better */
    unsigned int mask = (POLLIN | POLLOUT | POLLRDNORM | POLLWRNORM);

    ASSERT_KERNEL_UNLOCKED();
    ASSERT(MDKI_INOISMVFS(fp->f_dentry->d_inode));

    /*
     * Do all polling on the "real" file pointer.
     */
    realfp = REALFILE(fp);
    if (realfp) {
        /* object has an underlying thing to poll */
        if (realfp->f_op && realfp->f_op->poll) {
            mask = (*realfp->f_op->poll)(realfp, pt_p);
        }
    }
    return mask;
}

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
long
vnode_fop_ioctl(
    FILE_T *file_p,
    uint cmd,
    ulong arg
)
#else /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36) */
int
vnode_fop_ioctl(
    INODE_T *ino_p,
    FILE_T *file_p,
    uint cmd,
    ulong arg
)
#endif /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36) */
{
    int err;
    int rval;                           /* unused */
    CALL_DATA_T cd;
    struct ioctl_ctx ctx;

    ASSERT_KERNEL_LOCKED();
    mdki_linux_init_call_data(&cd);
    ctx.filp = file_p;
    ctx.caller_is_32bit = 0;            /* unknown as of yet */
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
    err = VOP_IOCTL(ITOV(file_p->f_path.dentry->d_inode), cmd, (void *)arg, 0, &cd, &rval, NULL, &ctx);
#else
    err = VOP_IOCTL(ITOV(ino_p), cmd, (void *)arg, 0, &cd, &rval, NULL, &ctx);
#endif
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);
    return err;
}

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16) && defined(RATL_COMPAT32)
long
vnode_fop_compat_ioctl(
    FILE_T *file_p,
    uint cmd,
    ulong arg
)
{
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,36)
    return vnode_fop_ioctl(file_p, cmd, arg);
#else
    return vnode_fop_ioctl(file_p->f_dentry->d_inode, file_p, cmd, arg);
#endif
}
#endif
int
vnode_fop_open(
    INODE_T *ino_p,
    FILE_T *file_p
)
{
    int status = 0;
    VNODE_T *avp;
    VNODE_T *vp;
    CALL_DATA_T cd;

    /* No asserts on BKL; locking protocol is changing */

    ASSERT(MDKI_INOISOURS(ino_p));
    if (!MDKI_INOISMVFS(ino_p)) {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                 "%s shouldn't be called on shadow?"
                 " (files swapped at open): vp %p fp %p\n",
                 __func__, ino_p, file_p);
        return -ENOSYS;
    }

    if ((status = generic_file_open(ino_p, file_p))) {
        return status;
    }

    avp = ITOV(ino_p);
    vp = avp;

    mdki_linux_init_call_data(&cd);
    status = VOP_OPEN(&vp, vnlayer_filep_to_flags(file_p), &cd, 
                     (file_ctx *)file_p);
    status = mdki_errno_unix_to_linux(status);
    mdki_linux_destroy_call_data(&cd);

    MDKI_TRACE(TRACE_OPEN, "%s opened vp=%p fp=%p pvt=%p pcnt=%d\n",
              __func__, vp, file_p, REALFILE(file_p),
              REALFILE(file_p) ? F_COUNT(REALFILE(file_p)) : 0);
    if (avp != vp) {
        printk("switcheroo on open? %p became %p\n", avp, vp); /* XXX */
        BUG();
    }

    return status;
}

/*
 * close path calls both flush and release, if last object reference.
 * Flush is really like a non-last close, and release is like a last
 * close.
 */

/*
 * This is VOP_CLOSE().  Called when a file pointer is being cleaned
 * up--guaranteed only once!
 */
int
vnode_fop_release(
    INODE_T *ino_p,
    FILE_T *file_p
)
{
    int status = 0;
    VNODE_T *vp;
    MOFFSET_T off = 0;
    CALL_DATA_T cd;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
    mdki_vop_close_ctx_t ctx;
#endif

    ASSERT_KERNEL_UNLOCKED();
    ASSERT(MDKI_INOISOURS(ino_p));
    if (!MDKI_INOISMVFS(ino_p)) {
        MDKI_TRACE(TRACE_CLOSE, "shadow no-op fp=%p ip=%p\n", file_p, ino_p);
        return 0;                       /* XXX shadow something? */
    }
    mdki_linux_init_call_data(&cd);
    vp = ITOV(ino_p);
    MDKI_TRACE(TRACE_CLOSE,
              "%s: fp=%p vp=%p fcount=%d pvt=%p rfcount=%d pid=%ld\n",
              __func__, file_p, vp, F_COUNT(file_p),
              REALFILE(file_p),
              REALFILE(file_p) ? F_COUNT(REALFILE(file_p)) : 0,
              (long) mdki_curpid());

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
    ctx.file_p = file_p;
    ctx.owner_id = NULL;
    status = VOP_CLOSE(vp, vnlayer_filep_to_flags(file_p),
                       VNODE_LASTCLOSE_COUNT, off, &cd, (file_ctx *)&ctx);
#else
    status = VOP_CLOSE(vp, vnlayer_filep_to_flags(file_p),
                       VNODE_LASTCLOSE_COUNT, off, &cd, (file_ctx *)file_p);
#endif

    status = mdki_errno_unix_to_linux(status);
    mdki_linux_destroy_call_data(&cd);
    return status;
}

extern int
vnode_fop_flush(
    FILE_T *fp
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
    , fl_owner_t id
#endif
)
{
    INODE_T *ip = fp->f_dentry->d_inode;
    int err;
    CALL_DATA_T cd;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
    mdki_vop_close_ctx_t ctx;
#endif

    ASSERT(MDKI_INOISOURS(ip));
    if (!MDKI_INOISMVFS(ip)) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s shouldn't be called? (files swapped at open): fp %p\n", __func__, fp);
        return 0;                       /* don't fail the operation, though */
    }
    mdki_linux_init_call_data(&cd);
    ASSERT(F_COUNT(fp) != VNODE_LASTCLOSE_COUNT);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
    ctx.file_p = fp;
    ctx.owner_id = id;
    err = VOP_CLOSE(ITOV(ip), vnlayer_filep_to_flags(fp), F_COUNT(fp),
                    (MOFFSET_T) 0, &cd, (file_ctx *)&ctx);
#else
    err = VOP_CLOSE(ITOV(ip), vnlayer_filep_to_flags(fp), F_COUNT(fp),
                    (MOFFSET_T) 0, &cd, (file_ctx *)fp);
#endif

    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);
    return err;
}

/* In Linux they have introduced the datasync field.  It is set to either
 * 0 or 1 but I don't see it ever being referenced in the standard file
 * systems.  fs.h says that we may be called without the big kernel lock
 * but we are always called with the inode locked, so locking should not be
 * an issue.
 */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,35)
extern int
vnode_fop_fsync(
    FILE_T *file_p,
    DENT_T *dentry_p,
    int datasync
)
#elif defined(MRG)
extern int
vnode_fop_fsync(
    FILE_T *file_p,
    int datasync
)
#else
extern int
vnode_fop_fsync(
FILE_T *file_p,
loff_t start,
loff_t end,
int datasync
)
#endif
{
    INODE_T *ip;
    int err;
    CALL_DATA_T cd;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,35)
    fsync_ctx ctx;
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,35)
    if (file_p == NULL) {
        /* NFSD sometimes calls with null file_p and dentry_p filled in. */
        ASSERT(dentry_p != NULL);
        ip = dentry_p->d_inode;
    } else
#endif
        ip = file_p->f_dentry->d_inode;

    ASSERT_I_SEM_MINE(ip);
    ASSERT(MDKI_INOISOURS(ip));
    if (!MDKI_INOISMVFS(ip)) {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,35)
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s shouldn't be called? (files swapped "
                "at open): file_p=%p dp=%p\n", __func__, file_p, dentry_p);
#else
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s shouldn't be called? (files swapped "
                "at open): file_p=%p dp=%p\n", __func__, file_p, file_p->f_dentry);
#endif
        return 0;                       /* don't fail the operation, though */
    }

    mdki_linux_init_call_data(&cd);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,35)
    err = VOP_FSYNC(ITOV(ip), datasync == 0 ? FLAG_NODATASYNC : FLAG_DATASYNC,
                    &cd, (file_ctx *)file_p);
#else
    ctx.file_p = file_p;
#if !defined (MRG)
    ctx.start = start;
    ctx.end = end;
#endif /* !defined (MRG) */
    err = VOP_FSYNC(ITOV(ip), datasync == 0 ? FLAG_NODATASYNC : FLAG_DATASYNC,
                    &cd, &ctx);
#endif /* else LINUX_VERSION_CODE < KERNEL_VERSION(2,6,35) */
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);
    return err;
}

#if !(defined(RATL_SUSE) && RATL_VENDOR_VER == 900)
/* This function used to call back to mvfs_locktl_ctx to validate the
 * vnode, to verify that we are being called on a MFS_LOOPCLAS or MFS_VOBCLAS
 * file, and to call mvfs_getcleartext to get the cleartext vnode pointer.  
 * If that call failed for any reason, we would never perform the locking 
 * operation.  In the case of an unlock called from the close code, this 
 * would panic the system because locks_remove_flock() would find dangling 
 * Posix locks.
 * One alternative would be to check for an error and if we had one, call 
 * mvop_linux_lockctl directly.  Since we only used the vnode returned
 * from mvfs_getcleartext to check that this is a regular file, there was
 * no longer any compelling reason to call into common code at all.
 * We assume that since the file is open, the cleartext is good.  We will
 * add code to mvop_linux_lockctl to verify the existence of the realfp
 * as a replacement for the check for mnode class since only loopback or
 * vob files will have a realfp.
 */
int
vnode_fop_lock(
    FILE_T *fp,
    int cmd,
    struct file_lock *lock_p
)
{
    INODE_T *ip = fp->f_dentry->d_inode;
    int err;
    CALL_DATA_T cd;

    ASSERT(MDKI_INOISMVFS(ip));

    mdki_linux_init_call_data(&cd);
    err = mvop_linux_lockctl(ITOV(ip), lock_p, cmd, &cd, (file_ctx *)fp);
    mdki_linux_destroy_call_data(&cd);

    return mdki_errno_unix_to_linux(err);
}
#endif

static inline int
vma_to_sharing(struct vm_area_struct *mem_p)
{
    return mem_p->vm_flags & VM_SHARED ? MAP_SHARED : 0;
}

static inline int
vma_to_rwx(struct vm_area_struct *mem_p)
{
    int rv = 0;
    if (mem_p->vm_flags & VM_READ)
        rv |= PROT_READ;
    if (mem_p->vm_flags & VM_WRITE)
        rv |= PROT_WRITE;
    if (mem_p->vm_flags & VM_EXEC)
        rv |= PROT_EXEC;
    return rv;
}

int
vnode_fop_mmap(
    FILE_T *fp,
    struct vm_area_struct *mem_p
)
{
    INODE_T *ip = fp->f_dentry->d_inode;
    int err = 0;
    loff_t len;
    loff_t offset = mem_p->vm_pgoff << PAGE_CACHE_SHIFT;
    loff_t maxoffset = MVFS_MAXOFF_T;
    VNODE_T *vp;
    struct mmap_ctx ctx;
    CALL_DATA_T cd;

    /* make sure offset, len are within our range */

    if (!(fp->f_flags & O_LARGEFILE))
        maxoffset = MAX_NON_LFS;

    len = mem_p->vm_end - mem_p->vm_start;

    if (offset < 0)
        return -EINVAL;

    if (offset >= maxoffset ||
        (offset + len) < 0 ||
        (offset + len) >= maxoffset)
    {
        return -EOVERFLOW;                 
    }

    ASSERT(MDKI_INOISMVFS(ip));
    vp = ITOV(ip);
    if (vp->v_type != VREG)
        return -ENODEV;

    ctx.file = fp;
    ctx.mem = mem_p;
    mdki_linux_init_call_data(&cd);
    err = VOP_MMAP(vp, vma_to_sharing(mem_p),
                   vma_to_rwx(mem_p),
                   &cd, &ctx);
    mdki_linux_destroy_call_data(&cd);

    MDKI_TRACE(TRACE_MAP, "vp=%p mflags=%x prot=%x error=%d\n",
              vp, vma_to_sharing(mem_p),
              vma_to_rwx(mem_p), err);
    return mdki_errno_unix_to_linux(err);
}

int
vnode_fop_readdir(
    FILE_T *file_p,
    void *dirent_p,
    filldir_t filldir_func
)
{
    uio_t uios;
    INODE_T *inode;
    DENT_T *dentry;
    int err;
    CALL_DATA_T cd;
    struct readdir_ctx ctx;

    dentry = file_p->f_dentry;
    inode = dentry->d_inode;

    ASSERT(MDKI_INOISMVFS(inode));

    ctx.file = file_p;
    ctx.done = FALSE;

    BZERO(&uios, sizeof(uios));
    uios.uio_offset = (loff_t)file_p->f_pos;
    /* This value cannot be larger than value in the view_v4_procinfo table 
     * It is used for maximum size of the return data.
     */
    uios.uio_resid = MVFS_LINUX_MAXRPCDATA;
    uios.uio_buff = dirent_p;
    uios.uio_func = filldir_func;

    mdki_linux_init_call_data(&cd);
    err = VOP_READDIR(ITOV(inode), &uios, &cd, NULL, &ctx);
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);

    if (!ctx.done)
        /* reset the file position */
        file_p->f_pos = (loff_t)uios.uio_offset;

    return err;
}

static const char vnode_verid_mvfs_linux_fops_c[] = "$Id:  0e15822f.e6e311e1.8799.00:01:84:c3:8a:52 $";
