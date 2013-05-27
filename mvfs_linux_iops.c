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

#include "vnode_linux.h"
#include "mvfs_linux_shadow.h"

/* Inode operations (and some utilities) for MVFS */

extern int
vnode_iop_create(
    INODE_T * parent,
    DENT_T * dentry,
    int mode,
    struct nameidata *nd
);

extern DENT_T *
vnode_iop_lookup(
    INODE_T *dir,
    DENT_T *file,
    struct nameidata *nd
);

extern int
vnode_iop_link(
    DENT_T * oldfile,
    INODE_T * parent,
    DENT_T * newfile
);
extern int
vnode_iop_unlink(
    INODE_T *dir,
    DENT_T *file
);
extern int
vnode_iop_symlink(
    INODE_T *parent,
    DENT_T *new,
    const char *oldname
);
extern int
vnode_iop_mkdir(
    INODE_T *parent,
    DENT_T *new,
    int mode
);
extern int
vnode_iop_rmdir(
    INODE_T *parent,
    DENT_T *target
);
extern int
vnode_iop_mknod(
    INODE_T *parent,
    DENT_T *new,
    int mode,
    dev_t dev
);
extern int
vnode_iop_rename(
    INODE_T *odir,
    DENT_T *odent,
    INODE_T *ndir,
    DENT_T *ndent
);
extern int
vnode_iop_permission(
    INODE_T *ip,
    int permtype
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
    , struct nameidata *nd
#endif
);

extern int
vnode_iop_getattr(
    struct vfsmount *mnt,
    DENT_T *dentry,
    struct kstat *kstat
);

/* vnode_iop_notify_change in common header for shadow code */

extern int
vnode_iop_readlink(
    DENT_T *dentry,
    char * buf,
    int buflen
);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
void *
#else
int
#endif
vnode_iop_follow_link(
    DENT_T *base,              /* parent */
    struct nameidata *nd       /* entry we are trying to resolve */
);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
extern void
vnode_iop_put_link(
    struct dentry *dentry,
    struct nameidata *nd,
    void *cookie
);
#endif

extern int
vnode_iop_setxattr(
    struct dentry *dentry,
    const char *name,
    const void *value,
    size_t size,
    int flags
);

extern ssize_t
vnode_iop_getxattr(
    struct dentry *dentry,
    const char *name,
    void *value,
    size_t size
);
extern ssize_t
vnode_iop_listxattr(
    struct dentry *dentry,
    char *name,
    size_t size
);
extern int
vnode_iop_removexattr(
    struct dentry *dentry,
    const char *name
);

IN_OPS_T vnode_file_inode_ops = {
    .permission =          &vnode_iop_permission,
    .getattr =             &vnode_iop_getattr,
    .setattr =             &vnode_iop_notify_change,
};

IN_OPS_T vnode_file_mmap_inode_ops = {
    .permission =          &vnode_iop_permission,
    .getattr =             &vnode_iop_getattr,
    .setattr =             &vnode_iop_notify_change,
};

IN_OPS_T vnode_slink_inode_ops = {
    .readlink =            &vnode_iop_readlink,
    .follow_link =         &vnode_iop_follow_link,
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
    .put_link =            &vnode_iop_put_link,
#endif
    .permission =          &vnode_iop_permission,
    .getattr =             &vnode_iop_getattr,
};

IN_OPS_T vnode_dir_inode_ops = {
    .create =              &vnode_iop_create,
    .lookup =              &vnode_iop_lookup,
    .link =                &vnode_iop_link,
    .unlink =              &vnode_iop_unlink,
    .symlink =             &vnode_iop_symlink,
    .mkdir =               &vnode_iop_mkdir,
    .rmdir =               &vnode_iop_rmdir,
    .mknod =               &vnode_iop_mknod,
    .rename =              &vnode_iop_rename,
    .permission =          &vnode_iop_permission,
    .getattr =             &vnode_iop_getattr,
    .setattr =             &vnode_iop_notify_change,
    .setxattr =            &vnode_iop_setxattr,
    .getxattr =            &vnode_iop_getxattr,
    .listxattr =           &vnode_iop_listxattr,
    .removexattr =         &vnode_iop_removexattr,

};

/* make this use a kmem_cache someday */
#define VATTR_ALLOC() KMEM_ALLOC(sizeof(VATTR_T), KM_SLEEP)
#define VATTR_FREE(vap) KMEM_FREE(vap, sizeof(VATTR_T))

/* Inode operations */

/* This is VOP_ACCESS().
 * permtype = bitwise-OR of MAY_READ, MAY_WRITE, MAY_EXEC
 * For 2.6.27 and beyond we may need to handle other
 * permission requests than the tradional MAY_[RWX], like
 * MAY_ACCESS.
 */
extern int
vnode_iop_permission(
    INODE_T *ip,
    int permtype
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
    , struct nameidata *nd
#endif
)
{
    int err;
    CALL_DATA_T cd;

    ASSERT_I_SEM_NOT_MINE(ip);

    mdki_linux_init_call_data(&cd);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
    /* we are not dealing with MAY_ACCESS and MAY_OPEN */
    permtype &= (MAY_READ | MAY_WRITE | MAY_EXEC);
#endif

    /*
     * Vnode core wants the mode test bits to be in the user position, not the
     * low bits.  Bits are in same order as standard UNIX rwx.
     */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
    err = VOP_ACCESS(ITOV(ip), permtype << 6, 0, &cd, (nameidata_ctx *)nd);
#else
    err = VOP_ACCESS(ITOV(ip), permtype << 6, 0, &cd, NULL);
#endif
    err = mdki_errno_unix_to_linux(err);

    mdki_linux_destroy_call_data(&cd);
    return err;
}

STATIC int
vnlayer_getattr(
    DENT_T *dentry,
    VATTR_T *vap
)
{
    INODE_T *ip;
    VNODE_T *vp;
    CALL_DATA_T cd;
    int err;

    ip = dentry->d_inode;

    ASSERT_KERNEL_UNLOCKED();

    mdki_linux_init_call_data(&cd);

    vp = ITOV(ip);
    /* implicit and explicit attribute pullup to the vnode */
    VATTR_SET_MASK(vap, AT_ALL);
    err = VOP_GETATTR(vp, vap, GETATTR_FLAG_PULLUP_ATTRS, &cd);
    err = mdki_errno_unix_to_linux(err);

    mdki_linux_destroy_call_data(&cd);

    return err;
}

STATIC void
vnlayer_linux_vattr2kstat(
    VATTR_T *src,
    struct kstat *dst
)
{
    BZERO(dst, sizeof(*dst));

    /* We could assume all the "pullup" stuff happened in getattr so the inode
    ** matches what we have in the vattr and then use generic_fillattr(ip, dst)
    ** (and fix the dev field since it's not in the inode anymore).  However,
    ** there could be a race on vob roots if we did that, so just get
    ** everything out of the passed in vattr structure.
    */
#define GET(lll, UUU) dst->lll = VATTR_GET_ ## UUU(src)
#define GET_TIME(lll, UUU) VATTR_GET_ ## UUU ## _TS(src, &(dst->lll))

    GET(dev, FSID);
    GET(ino, NODEID);
    GET(mode, MODE);
    dst->mode |= vnlayer_vtype_to_mode(VATTR_GET_TYPE(src));
    GET(nlink, NLINK);
    GET(uid, UID);
    GET(gid, GID);
    GET(rdev, RDEV);
    GET_TIME(atime, ATIME);
    GET_TIME(mtime, MTIME);
    GET_TIME(ctime, CTIME);
    GET(size, SIZE);
    GET(blocks, NBLOCKS);
    GET(blksize, BLKSIZE);

#undef GET_TIME
#undef GET
}

extern int
vnode_iop_getattr(
    struct vfsmount *mnt,
    DENT_T *dentry,
    struct kstat *kstat
)
{
    VATTR_T *vap;
    int err;

    vap = VATTR_ALLOC();
    if (vap == NULL)
        return -ENOMEM;
    err = vnlayer_getattr(dentry, vap);

    /* Fill in the return structure, if we should. */
    if ((err == 0) && (kstat != NULL)) {
        vnlayer_linux_vattr2kstat(vap, kstat);
    }
    VATTR_FREE(vap);
    return(err);
}

STATIC void
vnode_iop_iattr2vattr(
    struct iattr *src,
    VATTR_T *dst
)
{
    struct timespec curtime = CURRENT_TIME;

    VATTR_NULL(dst);
    if (src->ia_valid & ATTR_MODE) {
        VATTR_SET_TYPE(dst, vnlayer_mode_to_vtype(src->ia_mode));
        VATTR_SET_MODE_RIGHTS(dst, src->ia_mode);
        dst->va_mask |= AT_MODE|AT_TYPE;
    }
#define SET(UUU,lll)                                    \
    if (src->ia_valid & ATTR_ ## UUU) {                 \
        VATTR_SET_ ## UUU(dst, src->ia_ ## lll);        \
        dst->va_mask |= AT_ ## UUU;                     \
    }
    SET(UID,uid)
    SET(GID,gid)
    SET(SIZE,size)
#undef SET

    if (src->ia_valid & ATTR_ATIME) {
        /*
         * If ATTR_ATIME is provided, but not ATTR_ATIME_SET, then we need
         * the current time.  We have to pass ATTR_ATIME_SET through because
         * Linux uses it to control its validation.
         */
        if (src->ia_valid & ATTR_ATIME_SET) {
            VATTR_SET_ATIME_TS(dst, &(src->ia_atime));
        } else {
            VATTR_SET_ATIME_TS(dst, &curtime);
        }
        dst->va_mask |= AT_ATIME;
    }
    if (src->ia_valid & ATTR_ATIME_SET) dst->va_mask |= AT_ATIME_SET;
    if (src->ia_valid & ATTR_MTIME) {
        /*
         * If ATTR_MTIME is provided, but not ATTR_MTIME_SET, then we need
         * the current time.  We have to pass ATTR_MTIME_SET through because
         * Linux uses it to control its validation.
         */
        if (src->ia_valid & ATTR_MTIME_SET) {
            VATTR_SET_MTIME_TS(dst, &(src->ia_mtime));
        } else {
            VATTR_SET_MTIME_TS(dst, &curtime);
        }
        dst->va_mask |= AT_MTIME;
    }
    if (src->ia_valid & ATTR_MTIME_SET) dst->va_mask |= AT_MTIME_SET;
    /* No current time hack needed for ctime. */
    if (src->ia_valid & ATTR_CTIME) {
        VATTR_SET_CTIME_TS(dst, &(src->ia_ctime));
        dst->va_mask |= AT_CTIME;
    }
    if (src->ia_valid & ATTR_ATTR_FLAG) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "What to do with ATTR_FLAGs?: caller %p\n", mdki_getmycaller());
    }
    return;
}

/* This is really VOP_SETATTR() in sheep's clothing */
int
vnode_iop_notify_change(
    DENT_T *dent_p,
    struct iattr * iattr_p
)
{
    VNODE_T *vp;
    VATTR_T *vap;
    VNODE_T *cvp;
    int err;
    DENT_T *rdent;
    CALL_DATA_T cd;
    mdki_boolean_t tooksem = FALSE;

    if (iattr_p->ia_valid & ATTR_SIZE) {
        ASSERT_I_SEM_MINE(dent_p->d_inode);
    }

    if (MDKI_INOISMVFS(dent_p->d_inode)) {
        vap = VATTR_ALLOC();
	if (vap != NULL) {
            vnode_iop_iattr2vattr(iattr_p, vap);

            /* reject attempts to use setattr to change object type */
            vap->va_mask &= ~AT_TYPE;
            mdki_linux_init_call_data(&cd);
            vp = ITOV(dent_p->d_inode);
            err = VOP_SETATTR(vp, vap, 0, &cd);
            err = mdki_errno_unix_to_linux(err);
            /* Any underlying cleartxt got its inode truncated via changeattr
             * if there's a need to change its size.
             */
            if (!err)
                mdki_linux_vattr_pullup(vp, vap, vap->va_mask);
            VATTR_FREE(vap);
            mdki_linux_destroy_call_data(&cd);
	} else {
	    err = -ENOMEM;
	}
    } else {
        rdent = REALDENTRY_LOCKED(dent_p, &cvp);
        VNODE_DGET(rdent);
        if (rdent && rdent->d_inode) {
            err = inode_setattr(dent_p->d_inode, iattr_p);
            if (err == 0) {
                if (iattr_p->ia_valid & ATTR_SIZE) {
                    LOCK_INODE(rdent->d_inode);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
#if !defined RHEL_UPDATE || RHEL_UPDATE < 5
                    down_write(&rdent->d_inode->i_alloc_sem);
#endif
#endif
                    /*
                     * be paranoid and record the 'taken'ness in case
                     * the called function squashes ia_valid (as is
                     * done in nfs_setattr).
                     */
                    tooksem = TRUE;
                }
#if defined(SLES10SP2) || \
    (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24) && \
     LINUX_VERSION_CODE < KERNEL_VERSION(2,6,32))
                err = notify_change(rdent, CVN_TO_VFSMNT(cvp), iattr_p);
#else
                err = notify_change(rdent, iattr_p);
#endif
                if (tooksem) {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
#if !defined(RHEL_UPDATE) || RHEL_UPDATE < 5
                    up_write(&rdent->d_inode->i_alloc_sem);
#endif
#endif
                    UNLOCK_INODE(rdent->d_inode);
                }
            }
	} else {
            /* It looks as though someone removed the realdentry on us.
	     * I am not sure why this should happen.
	     */
            err = -ENOENT;
        }
        if (rdent) {
            VNODE_DPUT(rdent);
	    REALDENTRY_UNLOCK(dent_p, cvp);
        }
    }
    return err;
}

extern int
vnode_iop_readlink(
    DENT_T *dentry,
    char * buf,
    int buflen
)
{
    INODE_T *ip;
    struct uio uio;
    iovec_t iov;
    int err = 0;
    CALL_DATA_T cd;

    /*
     * This routine is not called for shadow objects which need
     * special handling; they're done in shadow_readlink.
     */

    uio.uio_iov = &iov;
    mdki_linux_uioset(&uio, buf, buflen, 0, UIO_USERSPACE);

    mdki_linux_init_call_data(&cd);
    ip = dentry->d_inode;

    ASSERT_KERNEL_UNLOCKED();
    ASSERT_I_SEM_NOT_MINE(ip);

    err = VOP_READLINK(ITOV(ip), &uio, &cd);
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);
    if (err == 0) {
        /* return count of bytes */
        err = buflen - uio.uio_resid;
    }
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
vnode_iop_follow_link(
    DENT_T *dentry,            /* link */
    struct nameidata *nd       /* link resolution */
)
{
    INODE_T *ip;
    struct uio uio;
    iovec_t iov;
    int err = 0;
    char *buf = KMEM_ALLOC(PATH_MAX, KM_SLEEP);
    CALL_DATA_T cd;

    if (buf == NULL)
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
        return ERR_PTR(-ENOMEM);
#else
        return -ENOMEM;
#endif
    uio.uio_iov = &iov;
    mfs_uioset(&uio, buf, PATH_MAX-1, 0, UIO_SYSSPACE);

    mdki_linux_init_call_data(&cd);
    ip = dentry->d_inode;

    ASSERT_KERNEL_UNLOCKED();
    ASSERT_I_SEM_NOT_MINE(ip);

    err = VOP_READLINK(ITOV(ip), &uio, &cd);
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);
    if (err == 0) {
        if (uio.uio_resid == 0)
            err = -ENAMETOOLONG;
        else {
            /* readlink doesn't copy a NUL at the end, we must do it */
            buf[uio.uio_offset] = '\0';
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
            /* follow the link */
            err = vfs_follow_link(nd, buf);
#else
            nd_set_link(nd, buf);
            return(buf); /* vnop_iop_put_link() will free this buf. */
#endif
        }
    }
    KMEM_FREE(buf, PATH_MAX);
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
    return ERR_PTR(err);
#else
    return(err);
#endif
}
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
void
vnode_iop_put_link(
    struct dentry *dentry,
    struct nameidata *nd,
    void *cookie)
{
    KMEM_FREE(cookie, PATH_MAX);
    return;
}
#endif

/* This function is called when a file is being created in a directory.
 * The dentry is the negative dentry to be filled in by the create.
 * The parent is a pointer to the parent inode.
 * We are called with the parent inode locked.  We have to lock the
 * real parent as well.  We run the risk of deadlock here.  However,
 * if we always lock the shadow inode before the real inode, we should
 * be OK.  (See mvop_linux_create().)
 * We did a dget on the real dentry when we put the pointer into the
 * shadow inode.  We shouldn't need to do it again, I don't think.
 */

extern int
vnode_iop_create(
    INODE_T * parent,
    struct dentry * dentry,
    int mode,
    struct nameidata *nd
)
{
    int err = 0;
    VATTR_T *vap;
    VNODE_T *newvp;
    struct create_ctx ctx;
    CALL_DATA_T cd;

    ASSERT_I_SEM_MINE(parent);
    ASSERT(MDKI_INOISMVFS(parent));

    vap = VATTR_ALLOC();
    if (vap == NULL)
        return -ENOMEM;
    VATTR_NULL(vap);
    mdki_linux_init_call_data(&cd);
    /*
     * Solaris sends only type, mode, size, so we will too.
     */
    vap->va_type = VREG;
    vap->va_mode = mode & ~S_IFMT;
    vap->va_size = 0;
    vap->va_mask = AT_TYPE|AT_MODE|AT_SIZE;
    newvp = NULL;
    dentry->d_inode = NULL;
    ctx.dentry = dentry;
    ctx.parent = parent;
    err = VOP_CREATE(ITOV(parent),
                     (/* drop const */ char *) dentry->d_name.name,
                     vap,
                     NONEXCL, /* XXX handled by generic layer? */
                     mode, /* not used except for passthrough, see vap->va_mode */
                     &newvp,
                     &cd,
                     &ctx);
    err = mdki_errno_unix_to_linux(err);

    /* dentry reference uses the hold count from a successful create */
    if (!err) {
        if (dentry->d_inode == NULL) {
            /* Not a shadow object */
            ASSERT(newvp != NULL);
            ASSERT(VTOI(newvp) != NULL);
            VNODE_D_INSTANTIATE(dentry, VTOI(newvp));
            VATTR_SET_MASK(vap, AT_ALL);
            if (VOP_GETATTR(newvp, vap, 0, &cd) == 0)
                mdki_linux_vattr_pullup(newvp, vap, AT_ALL);
        } else {
            /* drop the extra ref returned in newvp */
            VN_RELE(newvp);
        }
        /* I nuked the code checking not VCHR, VREG--we are always VREG */
    } else {
        ASSERT(!dentry->d_inode);
        ASSERT(!newvp);
    }
    VATTR_FREE(vap);
    mdki_linux_destroy_call_data(&cd);
    return(err);
}

/* vnode_iop_lookup
 * Wrapper routine for mfs_lookup.
 * Args:   INODE_T *dir    Directory inode.
 *         dentry *file    Dcache entry for file we are looking up.
 * FIXME:  We still have to figure out how to keep from cacheing this
 *         dentry.  We want the VFS to always call us to resolve
 *         pathnames. (not possible, we just try to keep the VFS
 *         calling us all the time by claiming dentries are not valid)
 */

DENT_T *
vnode_iop_lookup(
    INODE_T *dir,
    struct dentry *dent,
    struct nameidata *nd
)
{
    char *name;
    mdki_boolean_t rele = FALSE;
    int err;
    VNODE_T *dvp;
    VNODE_T *rt_vnode;                  /* returned vnode */
    INODE_T *rt_inode = NULL;           /* returned inode ptr */
    DENT_T * real_dentry;
    DENT_T *found_dentry = dent;
    VATTR_T *vap;
    struct lookup_ctx ctx;
    CALL_DATA_T cd;

    ASSERT_I_SEM_MINE(dir);
    /* We can find our parent entry via the dentry provided to us. */
    ASSERT(dent->d_parent->d_inode == dir);

    if (dent->d_name.len > NAME_MAX)
        return ERR_PTR(-ENAMETOOLONG);
    name = /* drop the const */(char *) dent->d_name.name;
    mdki_linux_init_call_data(&cd);

    /* We pass along the dentry, as well as the parent inode so that
     * mvop_linux_lookup_* has everything it needs, even if it is passed in
     * the realvp, and it gets back a negative dentry.
     */
    dvp = ITOV(dir);
    ctx.dentrypp = &found_dentry;
    ctx.flags = LOOKUP_CTX_VALID;

    err = VOP_LOOKUP(dvp, name, &rt_vnode, (struct pathname *)NULL,
                     VNODE_LF_LOOKUP, NULL, &cd, &ctx);
    err = mdki_errno_unix_to_linux(err);

    if (!err) {
        ASSERT(rt_vnode != NULL);
        if (MDKI_INOISCLRVN(VTOI(rt_vnode))) {
            /* unwrap to the real object */
            ASSERT(CVN_TO_DENT(rt_vnode));
            rt_inode = CVN_TO_INO(rt_vnode);
            if (MDKI_INOISMVFS(rt_inode)) {
                VN_HOLD(ITOV(rt_inode));
                VN_RELE(rt_vnode);
                rt_vnode = ITOV(rt_inode);
            } else {
                igrab(rt_inode);
                VN_RELE(rt_vnode);
                rt_vnode = NULL;
            }
        } else
            rt_inode = VTOI(rt_vnode);
    }
    if (!err && (found_dentry != dent)) {
        mdki_linux_destroy_call_data(&cd);
        /* The hold was granted in makeloopnode() in the 'nocover' case. */
        if (rt_vnode != NULL)
            VN_RELE(rt_vnode);
        else
            iput(rt_inode);
        /*
         * found_dentry is the real socket/block/char device node's dentry.
         * See mvop_linux_lookup_component().
         *
         * For sockets, we use a dentry in our tree (we fill in the
         * provided dentry "dent") linked to the inode of the real
         * object.  This lets file name operations work in our
         * namespace, and lets socket connections all work (as they're
         * keyed off of the inode address) from inside to outside &
         * v.v.
         *
         * We also do this for VCHR, VBLK devices, and it seems to work OK
         * (e.g. make a node the same as /dev/tty, you can write to it)
         */
        switch (found_dentry->d_inode->i_mode & S_IFMT) {
          case S_IFSOCK:
          case S_IFCHR:
          case S_IFBLK:
            ASSERT(dent->d_inode == NULL);
            dent->d_op = &vnode_shadow_dentry_ops;
            igrab(found_dentry->d_inode);
            VNODE_D_ADD(dent, found_dentry->d_inode);
            VNODE_DPUT(found_dentry);
            found_dentry = NULL; /* tell caller to use original dentry */
            break;
          default:
            /* use returned dentry */
            break;
        }
        return(found_dentry);
    }

    /* We need to pass back dentry ops even for negative dentries, I think.
     * Shadow inodes will have been taken care of in lookup_component.
     */
    if (dent->d_op != &vnode_shadow_dentry_ops) {
        if (dent->d_parent->d_op == &vnode_setview_dentry_ops)
            dent->d_op = &vnode_setview_dentry_ops;
        else
            dent->d_op = &vnode_dentry_ops;
    }
    vap = VATTR_ALLOC();
    if (vap == NULL) {
        err = -ENOMEM;
        goto alloc_err;
    }
    if (!err && MDKI_INOISMVFS(rt_inode)) {
        /* fetch attributes & place in inode */
        VATTR_SET_MASK(vap, AT_ALL);
        err = VOP_GETATTR(rt_vnode, vap, GETATTR_FLAG_UPDATE_ATTRS, &cd);
        err = mdki_errno_unix_to_linux(err);
        if (err == -EOPNOTSUPP)          /* ignore it */
            err = 0;
        else if (err)
            rele = TRUE;
        else if ((rt_vnode->v_flag & VLOOPROOT) != 0 &&
                 rt_inode == vnlayer_get_urdir_inode())
        {
            /* return the real root */
            VN_RELE(rt_vnode);
            VATTR_FREE(vap);
            mdki_linux_destroy_call_data(&cd);
            return VNODE_DGET(vnlayer_get_root_dentry());
        }
        else if (vnlayer_looproot_vp != NULL &&
                 rt_vnode == vnlayer_looproot_vp &&
                 (real_dentry = MVOP_DENT(rt_inode,
                                          &vnode_dentry_ops)) != NULL)
        {
            /* return the real /view */
            VN_RELE(rt_vnode);
            VATTR_FREE(vap);
            mdki_linux_destroy_call_data(&cd);
            return real_dentry;
        }
    }
    VATTR_FREE(vap);
alloc_err:
    mdki_linux_destroy_call_data(&cd);

    /* It's an mnode-based object, set up a dentry for it */

    /* We don't return ENOENT.  For Linux, the negative dentry is enough */
    switch (err) {
      case -ENOENT:
        err = 0;
        ASSERT(rt_inode == NULL);
        VNODE_D_ADD(dent, rt_inode);
        break;
      case 0:
        /* We will consume the count on rt_inode as a reference for dent */
        /*
         * For VOB vnodes, we maintain two separate dentry trees for
         * the vnodes.  One tree is for setview-mode names (process
         * sets to a view context, then looks directly at the VOB
         * mountpoint without any cover vnodes in the path).  The
         * other tree is for view-extended naming into a VOB, with
         * dentries starting at the view tag and covering non-VOB
         * objects until crossing a mount point into a VOB.
         *
         * Mostly the system doesn't care, as long as it goes down the
         * tree from parent to child, since it will be traversing only one
         * of the dentry trees.  But when the cache misses, the system calls
         * this lookup method and wants to get a dentry in return.
         * There are standard interfaces ( d_splice_alias() in 2.6) 
         * which can find a good dentry  referencing the inode returned
         * by the file system's lookup method, but these methods don't 
         * work right when we have VOB directory vnodes with both setview 
         * and view-extended dentries.  We implement our own function
         * [vnlayer_inode2dentry_internal()] which knows the
         * distinctions and the rules for determining that an existing
         * attached dentry is valid for the lookup request.
         *
         * We have our own d_compare() function which forces all VOB
         * lookups to come to the inode lookup method (this function),
         * and then we get to choose the right dentry to return.  We
         * have our own lookup cache inside MVFS so we don't care that
         * the dentry cache is always missing on our names.
         *
         * If we have to make a new dentry, we may need to merge it
         * with an NFS-created temporary dentry using d_move()
         * (d_splice_alias() would do this for us, but we can't use it
         * for reasons listed above).
         */
        /*
         * We want to find the "right" dentry (if there is one), so
         * look for one that has a d_parent with the same dentry ops
         * (indicating it's in the same dentry tree).
         */
        if (S_ISDIR(rt_inode->i_mode)) {
            /*
             * It has been empirically shown that we have to check the 
             * parent of the dentry.  If the parent has been checked out
             * it is possible for the cache lookup to return an inode
             * from the tree below the old parent directory.  If this 
             * happens on a rename, the system will panic because the
             * Linux rename code checks the parent of the returned
             * dentry to see that it matches what it has for a parent.
             */
            found_dentry = vnlayer_inode2dentry_internal(rt_inode,
                                                         dent->d_parent, NULL,
                                                         dent->d_op);
        } else {
            /*
             * For non-directories, we also need to consider the
             * parent & the requested name so that
             * vnlayer_inode2dentry_internal() finds the right dentry.
             * (There may be multiple hard links; we want the one in
             * the same directory with the same name)
             */
            found_dentry = vnlayer_inode2dentry_internal(rt_inode,
                                                         dent->d_parent,
                                                         &dent->d_name,
                                                         dent->d_op);
        }
        if (found_dentry != NULL) {
            ASSERT(found_dentry->d_inode == rt_inode);
            /*
             * If the existing one is a disconnected dentry, we need
             * to move the old one to the new one (just like
             * d_splice_alias) to get the proper name/parent attached
             * in the dcache.
             */
            if ((found_dentry->d_flags & DCACHE_DISCONNECTED) != 0) {
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,7)
                ASSERT((dent->d_flags & DCACHE_UNHASHED) != 0);
#else
                ASSERT((dent->d_vfs_flags & DCACHE_UNHASHED) != 0);
#endif
                d_rehash(dent);
                d_move(found_dentry, dent);
            } 
            /* Release our count.  found_dentry also references inode. */
            iput(rt_inode);
            return found_dentry;
        }
        /*
         * Nothing suitable, wire it up to the proposed dentry.
         */
        VNODE_D_ADD(dent, rt_inode);
        break;
      default:
        /* some other error case */
        if (rele)
            VN_RELE(rt_vnode);
        break;
    }
    if (err)
        return ERR_PTR(err);
    else
        return NULL;
}

extern int
vnode_iop_link(
    DENT_T * olddent,
    INODE_T * parent,
    DENT_T * newdent
)
{
    int err = 0;
    struct link_ctx ctx;
    VATTR_T *vap;
    VNODE_T *parentvp;

    ASSERT_I_SEM_MINE(olddent->d_inode);
    ASSERT_I_SEM_MINE(parent);
    ASSERT(MDKI_INOISMVFS(parent));

    if (!vnlayer_link_eligible(olddent))
        return -EXDEV;

    /* VOP_REALVP will check that the parent is a loopback directory and
     * return EINVAL if it isn't.
     */
    if (VOP_REALVP(ITOV(parent), &parentvp) == 0) {
        /* We are creating a shadow link so bypass the mvfs for the rest */
        err = vnlayer_do_linux_link(parentvp, olddent, parent, newdent);
        err = mdki_errno_unix_to_linux(err);
    } else {
        /* This needs to be passed on to the mvfs to deal with */
        CALL_DATA_T cd;
        INODE_T *inode;
        if (!MDKI_INOISOURS(olddent->d_inode))
            return -EXDEV;
        ctx.parent = parent;
        ctx.newdent = newdent;
        ctx.olddent = olddent;
        ctx.done = FALSE;

        mdki_linux_init_call_data(&cd);
	if (MDKI_INOISMVFS(olddent->d_inode)) {
            err = VOP_LINK(ITOV(parent), ITOV(olddent->d_inode),
                           (char *)newdent->d_name.name, &cd, &ctx);
            err = mdki_errno_unix_to_linux(err);
            if (err == 0 && !ctx.done) {
                /* Again, a heavy handed way of bumping the inode count and
                 * handling the locking (This will use the inode lock)
                 */
                inode = igrab(olddent->d_inode);
                VNODE_D_INSTANTIATE(newdent, inode);
                if ((vap = VATTR_ALLOC()) != NULL) {
                    VATTR_SET_MASK(vap, AT_ALL);
                    if (VOP_GETATTR(ITOV(inode), vap, 0, &cd) == 0)
                        mdki_linux_vattr_pullup(ITOV(inode), vap, AT_ALL);
                    VATTR_FREE(vap);
		}
            }
	} else {
	    err = -EXDEV;
	}
        mdki_linux_destroy_call_data(&cd);
    }
    return err;
}

STATIC struct dentry *
vnlayer_dentry_peer(struct dentry *dent)
{
    struct dentry *peer, *parent_of_peer;
    struct dentry_operations *ops;
    mdki_boolean_t isdir = S_ISDIR(dent->d_inode->i_mode);

    /* find dentry for any peer */
    /* XXX race conditions? */
    /*
     * If we have multiple links with the same name but in other
     * directories, we find the "right" peer by finding the one
     * attached to our parent's peer.
     */
    if (dent->d_op == &vnode_setview_dentry_ops)
        ops = &vnode_dentry_ops;
    else
        ops = &vnode_setview_dentry_ops;
    if (isdir) {
        peer = vnlayer_inode2dentry_internal(dent->d_inode, NULL, NULL, ops);
    } else {
        parent_of_peer = vnlayer_inode2dentry_internal(dent->d_parent->d_inode,
                                                       NULL, NULL, ops);
        if (parent_of_peer == NULL)
            peer =  NULL;
        else {
            peer = vnlayer_inode2dentry_internal(dent->d_inode, parent_of_peer,
                                                 &dent->d_name, ops);
            VNODE_DPUT(parent_of_peer);
        }
    }
    return peer;
}

extern int
vnode_iop_unlink(
    INODE_T *dir,
    DENT_T *dent
)
{
    int err = 0;
    VNODE_T *obj;
    struct unlink_ctx ctx;
    CALL_DATA_T cd;
    struct dentry *peer;

    ASSERT_I_SEM_MINE(dent->d_inode);
    ASSERT_I_SEM_MINE(dir);
    ASSERT(MDKI_INOISMVFS(dir));

    if (!S_ISDIR(dir->i_mode)) {
        /* bogus */
        return -ENOTDIR;
    }

    peer = vnlayer_dentry_peer(dent);

    mdki_linux_init_call_data(&cd);
    ctx.dentry = dent;
    ctx.done = FALSE;
    if (dent->d_inode && MDKI_INOISMVFS(dent->d_inode))
        obj = ITOV(dent->d_inode);      /* no extra reference (be careful) */
    else
        obj = NULL;
    err = VOP_REMOVE(ITOV(dir), obj, (char *)dent->d_name.name, &cd, &ctx);
    /* XXX pullup attributes on removed object, if it's not gone yet? */
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);
    /* XXX Don't d_delete(dentry), our caller will do that */

    if (peer != NULL) {
        /*
         * unhash peer name in other mode.  Don't d_delete in case
         * we're racing
         */
        if (err == 0)
            d_drop(peer);
        VNODE_DPUT(peer);
    }

    return err;
}

extern int
vnode_iop_symlink(
    INODE_T *parent,
    DENT_T *new,
    const char *targetname
)
{
    int err = 0;
    VATTR_T *vap;
    VNODE_T *linkvp;
    struct symlink_ctx ctx;
    CALL_DATA_T cd;

    ASSERT(MDKI_INOISMVFS(parent));

    vap = VATTR_ALLOC();
    if (vap == NULL)
        return -ENOMEM;
    VATTR_NULL(vap);
    VATTR_SET_MASK(vap, AT_MODE|AT_TYPE);
    VATTR_SET_TYPE(vap, VLNK);
    VATTR_SET_MODE_RIGHTS(vap, S_IRWXU|S_IRWXG|S_IRWXO);
    mdki_linux_init_call_data(&cd);
    ctx.new = new;
    ctx.parent = parent;
    ctx.done = 0;
    /*
     * 2.6.7 and later have a mode parameter to vfs_symlink(), but
     * they never pass it to the file system in the inode operation.
     * Once they do, we should pass it along in the context.
     * XXX For now, guess that mode 0777 is sufficient.
     */
    ctx.mode = 0777;
    err = VOP_SYMLINK(ITOV(parent), (char *)new->d_name.name,
                      vap, (char *)targetname, &linkvp, &cd, &ctx);
    if (err == 0 && !ctx.done) {
        ASSERT(linkvp != NULL);
        /* we need to fill in the inode in the passed-in dentry for NFS */
        VNODE_D_INSTANTIATE(new, VTOI(linkvp)); /* consumes ref count */
    }
    err = mdki_errno_unix_to_linux(err);
    VATTR_FREE(vap);
    mdki_linux_destroy_call_data(&cd);

    return err;
}

extern int
vnode_iop_mkdir(
    INODE_T *parent,
    DENT_T *new,
    int mode
)
{
    VNODE_T *new_vnode = NULL;
    int err = 0;
    VATTR_T *vap;
    struct mkdir_ctx mkctx;
    CALL_DATA_T cd;

    ASSERT_I_SEM_MINE(parent);
    ASSERT(MDKI_INOISMVFS(parent));

    vap = VATTR_ALLOC();
    if (vap == NULL)
        return -ENOMEM;
    mdki_linux_init_call_data(&cd);

    VATTR_NULL(vap);
    VATTR_SET_MASK(vap, AT_MODE|AT_TYPE);
    VATTR_SET_TYPE(vap, VDIR);
    VATTR_SET_MODE_RIGHTS(vap, mode);
    mkctx.dentry = new;
    mkctx.pleasedrop = FALSE;
    err = VOP_MKDIR(ITOV(parent), (char *)new->d_name.name,
                    vap, &new_vnode, &cd, &mkctx);
    err = mdki_errno_unix_to_linux(err);
    if (err == 0) {
        if (mkctx.pleasedrop) {
            /* the created object was dropped; we don't want to use it */
            VN_RELE(new_vnode);
            new_vnode = NULL;
        }
    }

    if (new_vnode)
        VNODE_D_INSTANTIATE(new, VTOI(new_vnode));
    VATTR_FREE(vap);
    mdki_linux_destroy_call_data(&cd);
    return(err);
}

extern int
vnode_iop_rmdir(
    INODE_T *parent,
    DENT_T *target
)
{
    int err = 0;
    struct dentry *peer;
    CALL_DATA_T cd;

    ASSERT_I_SEM_MINE(parent);
    ASSERT_I_SEM_MINE(target->d_inode);
    ASSERT(MDKI_INOISMVFS(parent));

    peer = vnlayer_dentry_peer(target);

    mdki_linux_init_call_data(&cd);
    err = VOP_RMDIR(ITOV(parent), (char *)target->d_name.name,
                    NULL /* don't care about cdir on Linux */,
                    &cd, (dent_ctx *) target);
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);

    if (peer != NULL) {
        /* get rid of peer name in other mode */
        /* don't d_delete in case we're racing */
        if (err == 0)
            d_invalidate(peer);
        VNODE_DPUT(peer);
    }

    return(err);
}

extern int
vnode_iop_mknod(
    INODE_T *parent,
    DENT_T *new,
    int mode,
    dev_t dev
)
{
    int err = 0;
    VATTR_T *vap;
    VNODE_T *newvp;
    struct create_ctx ctx;
    CALL_DATA_T cd;

    ASSERT_I_SEM_MINE(parent);
    ASSERT(MDKI_INOISMVFS(parent));

    vap = VATTR_ALLOC();
    if (vap == NULL)
        return -ENOMEM;
    VATTR_NULL(vap);
    switch (mode & S_IFMT) {
      case S_IFIFO:
        vap->va_type = VFIFO;
        break;
      case S_IFBLK:
        vap->va_type = VBLK;
        break;
      case S_IFCHR:
        vap->va_type = VCHR;
        break;
      case S_IFSOCK:
        vap->va_type = VSOCK;
        break;
      default:
        VATTR_FREE(vap);
        return -EINVAL;
    }
    vap->va_mode = mode & ~S_IFMT;
    vap->va_size = 0;
    vap->va_mask = AT_TYPE|AT_MODE|AT_SIZE;
    newvp = NULL;
    ASSERT(new->d_inode == NULL);
    ctx.dentry = new;
    ctx.parent = parent;
    ctx.dev = dev;

    mdki_linux_init_call_data(&cd);
    /* Only returns non-errors on loopback files, see
       mvop_linux_mknod() for real work */
    err = VOP_CREATE(ITOV(parent),
                     (/* drop const */ char *) new->d_name.name,
                     vap,
                     NONEXCL, /* XXX handled by generic layer? */
                     mode, /* not used except for passthrough, see vap->va_mode */
                     &newvp,
                     &cd,
                     &ctx);
    err = mdki_errno_unix_to_linux(err);
    if (!err && newvp != NULL)
        VN_RELE(newvp);
    mdki_linux_destroy_call_data(&cd);
    VATTR_FREE(vap);
    return(err);
}

STATIC DENT_T *
vnlayer_rename_peer(DENT_T *dent)
{
    DENT_T *parent;
    DENT_T *peer;

    /*
     * We found no name in the cache in the other space on
     * the target inode: either it's not cached, or there
     * was/is a negative dentry at that name.
     *
     * Try to create a new dentry with the same name as our dentry,
     * but attached to the peer's parent.
     */
    if (dent->d_parent == dent)
        return NULL;
    parent = vnlayer_dentry_peer(dent->d_parent);
    /*
     * if parent == NULL, then the directory where we have a name in
     * our space has no peer name in the other space.
     *
     * Ideally we'd like to walk up the tree to find an ancestor, then
     * rebuild the dentry tree for the peers. However, that's a lock
     * order violation (we would need to lock each inode as we walk up
     * the dentry tree and instantiate all the names), so we just give
     * up in such cases (our caller will either return EBUSY or let
     * the target name appear to be unlinked while in use).
     */
    if (parent != NULL) {
        /*
         * We have a parent, so we can build a new dentry with the
         * same name as the passed-in dentry.  Note that there may be
         * a negative dentry in peer space, but since we always use
         * our lookup routines and never hit in the cache, the old
         * negative entry will get released after it's out of use.  So
         * don't worry about finding such a negative dentry.
         *
         * Simply allocate a name to be merged as a result of the
         * rename.
         */
        peer = VNODE_D_ALLOC(parent, &dent->d_name);
        peer->d_op = parent->d_op;
        VNODE_DPUT(parent);
    } else
        peer = NULL;
    return peer;
}

extern int
vnode_iop_rename(
    INODE_T *odir,
    DENT_T *odent,
    INODE_T *ndir,
    DENT_T *ndent
)
{
    int err = 0;
    struct rename_ctx rnctx;
    CALL_DATA_T cd;
    struct dentry *opeer, *npeer;

    ASSERT_I_SEM_MINE(odir);
    ASSERT_I_SEM_MINE(ndir);
    if (ndent->d_inode) {
        ASSERT_I_SEM_MINE(ndent->d_inode);
    }
    if (S_ISDIR(odent->d_inode->i_mode)) {
        if (odir != ndir) {
            ASSERT_SEMA_MINE(&ndir->i_sb->s_vfs_rename_sem);
        }
    }
    ASSERT(MDKI_INOISMVFS(odir));
    if (!MDKI_INOISMVFS(ndir))
        return -EXDEV;

    rnctx.odentry = odent;
    rnctx.ndentry = ndent;

    opeer = vnlayer_dentry_peer(odent);
    if (ndent->d_inode != NULL)
        /* If we're replacing something, get the peer */
        npeer = vnlayer_dentry_peer(ndent);
    else
        /* May be a negative dentry in the peer space... see below */
        npeer = NULL;

    /*
     * If there is no opeer, but there is an npeer, we need
     * to build an opeer so we can merge it with npeer.
     */
    if (opeer == NULL && npeer != NULL) {
        opeer = vnlayer_rename_peer(odent);
        if (opeer != NULL)
            VNODE_D_INSTANTIATE(opeer, igrab(odent->d_inode));
    } else if (opeer != NULL && npeer == NULL) {
        /*
         * We have an opeer, we need to move it to be a child of ndir,
         * but only if ndir has a peer name.  If ndir has no peer,
         * there's nowhere to reattach npeer and we must fail a
         * directory rename with -EBUSY.  If we allow it to proceed,
         * we may still leave some dentries looking like they were
         * deleted even though they were really renamed, if the target
         * dir is not cached in peer space.  It's too hard to deal
         * with that (would have to run up the tree to instantiate
         * parents in alternate space), so we use the EBUSY clause of
         * the rename(2) documentation.
         */
        npeer = vnlayer_rename_peer(ndent);
        /*
         * Leave npeer as a negative dentry, we'll fill it in later.
         */
    }
    /*
     * OK, we have the peers for the old and new names, if they're available.
     * If there's an old peer but no new peer, then we don't have enough
     * information to rename the peer directories and we bail out.
     */
    if (opeer != NULL && npeer == NULL) {
        if (S_ISDIR(odent->d_inode->i_mode)) {
            err = EBUSY;
        }
    }

    mdki_linux_init_call_data(&cd);
    if (err == 0)
        err = VOP_RENAME(ITOV(odir), (char *)odent->d_name.name,
                         ITOV(ndir), (char *)ndent->d_name.name, &cd, &rnctx);
    err = mdki_errno_unix_to_linux(err);
    mdki_linux_destroy_call_data(&cd);
    if (err == 0) {
        if (opeer != NULL) {
            if (npeer != NULL) {
                ASSERT(npeer->d_op == opeer->d_op);
                if (d_unhashed(npeer)) {
                    /* 2.6 kernels no longer panic if you try to rehash
                     * a hashed dentry. Instead they just truncate the
                     * hash chain after this entry.
                     */
                    d_rehash(npeer);
                }
                d_move(opeer, npeer);
            } else {
                /* nowhere to attach the name, just drop the old one */
                d_drop(opeer);
            }
        } else if (npeer != NULL) {
            /* we replaced the object at npeer's name, drop it */
            d_drop(npeer);
        }
    }
    VNODE_DPUT(opeer);
    VNODE_DPUT(npeer);
    return(err);
}

#ifdef USE_ROOTALIAS_CURVIEW
/*
 * We don't need to hijack the object creation or deletion/rename ops,
 * they all use a lookup function to find their targets and we've
 * already done the swap when they start doing their work.
 */
DENT_T *
vnlayer_hijacked_lookup(
    INODE_T *dir,
    struct dentry *dent,
    struct nameidata *nd
)
{
    DENT_T *parent = dent->d_parent;
    vnlayer_root_alias_t *alias;

    if (!DENT_IS_ROOT_ALIAS(parent)) {
        /* This is not our faked-up dentry, use the provided function */
        return (*vnlayer_root_iops_copy.lookup)(dir, dent, nd);
    }
    alias = DENT_GET_ALIAS(parent);
    ASSERT(alias->rootdentry->d_inode == dir);
    /*
     * ignore proposed dentry, get a new one: we want to do the
     * lookup in the real object with the original parent dentry.
     *
     * this call may come back here, but with the real dentry so we'll call
     * the FS as above.
     */
    return lookup_one_len(dent->d_name.name, alias->rootdentry, 
                           dent->d_name.len);
}

#endif

/* All 4 of the xattr functions are called with the BKL held */
static inline DENT_T *
vnlayer_xattr_getdent(DENT_T * dentry)
{
    DENT_T *rdent;
    INODE_T *inode;
    VNODE_T *vnode;
    inode = dentry->d_inode;
    vnode = ITOV(inode);
    if ((vnode->v_flag & (VLOOP | VLOOPROOT)) != 0) {
        VOP_REALVP(vnode, &vnode);
        rdent = (DENT_T *)vnode->v_dent;
    } else {
        rdent = NULL;
    }
    return rdent;
}

extern int
vnode_iop_setxattr(
    struct dentry *dentry,
    const char *name,
    const void *value,
    size_t size,
    int flags
)
{

    DENT_T *rdent;
    int err = -EOPNOTSUPP;
    rdent = vnlayer_xattr_getdent(dentry);
    if (rdent != NULL) {
        err = vnlayer_do_setxattr(rdent, name, value, size, flags);
    }
    return err;
}

/* In SLES8 SP3 the inode is not locked on getxattr or listxattr.  On getxattr
 * reiserfs gets the lock but ext3 doesn't.  Neither get the lock on listxattr.
 * I get the lock in both cases here in case it matters to some other fs.
 */
extern ssize_t
vnode_iop_getxattr(
    struct dentry *dentry,
    const char *name,
    void *value,
    size_t size
)
{
    DENT_T *rdent;
    int err = -EOPNOTSUPP;

#if defined(RATL_SUSE)
    LOCK_INODE(dentry->d_inode);
#endif
    rdent = vnlayer_xattr_getdent(dentry);
    if (rdent != NULL) {
        err = vnlayer_do_getxattr(rdent, name, value, size);
    }
#if defined(RATL_SUSE)
    UNLOCK_INODE(dentry->d_inode);
#endif
    return err;
}

extern ssize_t
vnode_iop_listxattr(
    struct dentry *dentry,
    char *name,
    size_t size
)
{
    DENT_T *rdent;
    int err = -EOPNOTSUPP;

#if defined(RATL_SUSE)
    LOCK_INODE(dentry->d_inode);
#endif
    rdent = vnlayer_xattr_getdent(dentry);
    if (rdent != NULL) {
        err = vnlayer_do_listxattr(rdent, name, size);
    }
#if defined(RATL_SUSE)
    UNLOCK_INODE(dentry->d_inode);
#endif
    return err;
}

extern int
vnode_iop_removexattr(
    struct dentry *dentry,
    const char *name
)
{
    DENT_T *rdent;
    int err = -EOPNOTSUPP;

    rdent = vnlayer_xattr_getdent(dentry);
    if (rdent != NULL) {
        err = vnlayer_do_removexattr(rdent, name);
    }
    return err;
}
static const char vnode_verid_mvfs_linux_iops_c[] = "$Id:  a7cbf783.dc5411df.9210.00:01:83:0a:3b:75 $";
