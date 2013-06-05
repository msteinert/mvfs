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
#include "mvfs_linux_shadow.h"

/*
 * vnode operations (and related utilities) for stacking file systems.
 * These routines translate from SVR4-style vnode operations to their
 * Linux equivalents.  They're used by a vnode file system to access
 * another file system's inode/dentry/etc.  For example, a layering
 * vnode file system may need to use an underlying object as a
 * container file to hold cache files used for file I/O, while the
 * vnode FS itself handles name space operations.
 */

/*
 * We are trying to simplify the posix_lock_file over all kernel 
 * versions and lock flavors
 */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#if BITS_PER_LONG == 32
#define POSIX_LOCK_FILE(FP, LOCKP, CMD) posix_lock_file(FP, LOCKP, \
                       (((CMD) == F_SETLKW) || ((CMD) == F_SETLKW64)) ? 1 : 0)
#else
#define POSIX_LOCK_FILE(FP, LOCKP, CMD) posix_lock_file(FP, LOCKP, \
                       (((CMD) == F_SETLKW)) ? 1 : 0)
#endif /* else BITS_PER_LONG == 32 */
#else
#define POSIX_LOCK_FILE(FP, LOCKP, CMD) posix_lock_file_wait(FP, LOCKP)
#endif /* else LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0) */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
extern inline                    /* force inlining on all platforms */
#else
/* it needs to be static */
static inline
#endif
int
vnlayer_lookup_post(
    struct dentry *dentry,
    struct dentry *found,
    VNODE_T **cvpp,
    struct vfsmount *mnt
)
{
    int error = 0;
    STACK_CHECK_DECL()

    /* If we were passed in a dentry, attach its REALCVN to the found
     * object.  We don't need to do a dget because
     * lookup_dentry/path_walk/lookup_one_len did one for us.
     */
    if (dentry) {
        CVN_SPINLOCK();
        ASSERT(REALCVN(dentry) == NULL);
        if ((found->d_op == &vnode_dentry_ops ||
             found->d_op == &vnode_setview_dentry_ops ||
             found->d_op == &vnode_shadow_dentry_ops) &&
            REALCVN(found) != NULL)
        {
            /* break a cycle and reference the underlying thing */
            VN_HOLD(REALCVN(found));
            /*FIXME:  There is an assumption here that there was
             * no previous REALCVN for dentry.  Is this true?
             */
            SET_REALCVN(dentry, REALCVN(found));
            CVN_SPINUNLOCK();
        } else {
            /*
             * OK to unlock at this point, since we won't be using
             * REALCVN(found), and dentry is a private new
             * entry being created for/by us and we don't need
             * to interlock with anybody else yet.
             */
            CVN_SPINUNLOCK();
            if (found->d_inode != NULL) {
                /* XXX we want to wrap, we need a real cvn to
                 * deal with ref count problems
                 */
                /*
                 * Only wrap with a CVP if it's not one of our objects
                 */
                if (MDKI_INOISMVFS(found->d_inode)) {
                    MDKI_TRACE(TRACE_USERLOOKUP, "not wrapping our dent (1) %p\n", found);
                    SET_REALCVN(dentry, VN_HOLD(ITOV(found->d_inode)));
                    *cvpp = VN_HOLD(REALCVN(dentry));
                } else {
                    *cvpp = CVN_CREATE(found, mnt);
                    STACK_CHECK();
                    if (*cvpp == NULL)
                        error = ENFILE;
                    else
                        SET_REALCVN(dentry, VN_HOLD(*cvpp));
                }
            } else {
                /* link up CVN to missing object, for use by create */
                *cvpp = CVN_CREATE(found, mnt);
                STACK_CHECK();
                if (*cvpp == NULL)
                    error = ENFILE;
                else {
                    SET_REALCVN(dentry, *cvpp);
                    *cvpp = NULL;
                    /* we will return ENOENT below */
                }
            }
        }
    }
    /* We might be getting back a negative dentry.  Check first */
    if (found->d_inode) {
        if (error == 0 && *cvpp == NULL) {
            /*
             * Only wrap with a CVP if it's not one of our objects
             */
            if (MDKI_INOISMVFS(found->d_inode)) {
                MDKI_TRACE(TRACE_USERLOOKUP, "not wrapping our dent (2) %p\n", found);
                *cvpp = VN_HOLD(ITOV(found->d_inode));
            } else {
                *cvpp = CVN_CREATE(found, mnt);
                if (*cvpp == NULL)
                    error = ENFILE;
            }
            STACK_CHECK();
        }
        MDKI_TRACE(TRACE_PNLOOKUP,"returning dp=%p realdp=%p ip=%p cvp=%p vmode=%o mfs=%d ours=%d err=%d\n",
                   dentry, found, found->d_inode, *cvpp,
                   found->d_inode->i_mode,
                   MDKI_INOISMVFS(found->d_inode),
                   MDKI_INOISOURS(found->d_inode), error);
    } else {
        if (!error)
            error = ENOENT;
        *cvpp = NULL;
    }
    STACK_CHECK();
    return error;
}

INLINE_FOR_SMALL_STACK int
mvop_linux_lookupvp(
    VNODE_T *dvp,
    char *path,
    int segflg,
    SYMFOLLOW_T follow,
    VNODE_T **cvpp,
    CRED_T *cred,
    DENT_T *dentry,
    struct nameidata *nd
)
{
    char   *pn;
    DENT_T *found = NULL;
    STACK_CHECK_DECL()

    int error = 0;
    u_int flags = 0;
    int save_ids;
    vnlayer_fsuid_save_t saved_ids;
    struct nameidata *myndp = NULL;
    int recursion = 0;
    int total_links;

    *cvpp = NULL;

    if (segflg == UIO_USERSPACE) {
        pn = getname(path);
        if (IS_ERR(pn)) {
                error = vnlayer_errno_linux_to_unix(PTR_ERR(pn));
                pn = NULL;
        } else {
            MDKI_TRACE(TRACE_USERLOOKUP, "user lookup: pn=%s\n", pn);
        }
    } else {
        pn = path;
        MDKI_TRACE(TRACE_USERLOOKUP, "system lookup: pn=%s\n", pn);
    }

    if (follow == FOLLOW_LINK) flags |= LOOKUP_FOLLOW;

    /* Get the right nameidata to use. */
    if (!error) {
        if (nd != NULL) {
            myndp = nd;
        } else {
            myndp = KMEM_ALLOC(sizeof(*myndp), KM_SLEEP);
            if (myndp == NULL) {
                error = ENOMEM;
            }
        }
    }
    if (!error) {

        /* Lookup has gotten a little simpler.  We will start
         * looking from the dentry in the nameidata structure.
         * Don't ASSERT on the locking conditions; they're done by the
         * kernel's lookup code called beneath path_walk().
         */
        memset(myndp, 0, sizeof(struct nameidata));
#if defined(IT_LOOKUP)
        intent_init(&myndp->intent, IT_LOOKUP);
#endif
        save_ids = vnlayer_fsuid_save(&saved_ids, cred);
        STACK_CHECK();
        if (dvp) {
            myndp->flags = flags;
	    CVN_SPINLOCK();
            MDKI_NAMEI_SET_DENTRY(myndp, VNODE_DGET(CVN_TO_DENT(dvp)));
            MDKI_NAMEI_SET_MNT(myndp, MDKI_MNTGET(CVN_TO_VFSMNT(dvp)));
	    CVN_SPINUNLOCK();
            if (d_mountpoint(MDKI_NAMEI_DENTRY(myndp))) {
                MDKI_TRACE(TRACE_USERLOOKUP, "dvp %p, dent %p, mntpoint\n",
                          dvp, myndp->dentry);
                /* XXX not sure we should ever get here? */
                found = ERR_PTR(-EOPNOTSUPP);
                VNODE_DPUT(MDKI_NAMEI_DENTRY(myndp));
                MDKI_MNTPUT(MDKI_NAMEI_MNT(myndp));
            }
            MDKI_TRACE(TRACE_USERLOOKUP, "myndp->mnt %p\n", myndp->mnt);
        } else {
            /* In Linux 2.6 path_lookup does all the work so found is either
            ** the found dentry or it is an error, never NULL (thus skipping
            ** the code below).
            */
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38) 
            if ((error = kern_path(pn, flags, &myndp->path)) == 0) {
#else
            if ((error = path_lookup(pn, flags, myndp)) == 0) {
#endif
                found = MDKI_NAMEI_DENTRY(myndp);
            } else {
                found = ERR_PTR(error);
            }
        }
        STACK_CHECK();
        if (found == NULL) {
            /* Who said this was simpler?  Path_walk will always follow
             * symlinks once it has started and the link_count in the
             * current task structure has been incremented.  However,
             * when we are called from lookup_component, we are just
             * looking for the individual object.  We are called from
             * path_walk with the parent directory locked.  We need to
             * to clear the link_count for the duration of the following
             * call so that we don't try to resolve any symlinks here.
             * when real_lookup returns, path_walk will unlock the
             * parent and then follow the symlink.  If we don't do this
             * we will hang if we have recursive symlinks that include
             * the same path.
             */
            /* Linux has added a wrinkle to path_walk to
             * keep a total link count to prevent a lookup
             * that has more than 40 links.  Since we are called
             * from path_walk via a long and winding road, we need
             * to keep from clobbering that count here.
             */
            total_links = current->total_link_count;
            if ((follow != FOLLOW_LINK) &&
                (recursion = current->link_count) != 0)
            {
                current->link_count = 0;
            }
            /* Linux has added a global link count we don't want
             * to reset here.
             */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24) 
            found = ERR_PTR(path_walk(pn, myndp));
#else
            found = ERR_PTR(vfs_path_lookup(MDKI_NAMEI_DENTRY(myndp),
                            MDKI_NAMEI_MNT(myndp), pn, myndp->flags, myndp));
#endif
            STACK_CHECK();
            if (recursion)
                current->link_count = recursion;
            current->total_link_count = total_links;
            if (!IS_ERR(found)) {
                found = MDKI_NAMEI_DENTRY(myndp);
            }
        }
        if (save_ids)
            vnlayer_fsuid_restore(&saved_ids);
#ifdef MVFS_DEBUG
        if (!IS_ERR(found) && d_mountpoint(found)) {
            MDKI_TRACE(TRACE_USERLOOKUP, "found dent %p, mntpoint\n", found);
        }
#endif
        STACK_CHECK();

        /* If we succeeded, tell what we know. */
        if (IS_ERR(found)) {
            error = vnlayer_errno_linux_to_unix(PTR_ERR(found));
            /* path_walk() has released myndp */
        } else {
            error = vnlayer_lookup_post(dentry, found, cvpp, MDKI_NAMEI_MNT(myndp));
            /*
             * cleanup myndp, if either (1) it's private, or (2) we're
             * returning an error, in which case the caller doesn't
             * expect nd to be filled in.
             */
            if (nd != myndp || error != 0) {
                MDKI_TRACE(TRACE_DCACHE,
                           "%s: d_put %p cnt=%d--\n",
                           __func__, myndp->dentry, D_COUNT(myndp->dentry));
                MDKI_PATH_RELEASE(myndp);
                STACK_CHECK();
            }
        }
        /* Log error before free pname buf if debug logging */

        if (error == ENOENT) {
            MDKI_TRACE(TRACE_USERLOOKUP, "lookupname comp=%s ENOENT\n", pn);
        } else {
            MDKI_TRACE(TRACE_USERLOOKUP, "lookupname comp=%s rval=%d\n",
                       pn, error);
        }
    }
    if ((segflg == UIO_USERSPACE) && (pn != NULL)) {
        putname(pn);
    }
    if (nd != myndp) {
        /* If they're not equal, then myndp can't be NULL since we only
        ** allocate it if nd == NULL, and if there was an error during the
        ** allocation, then myndp would be NULL, and therefore, equal to nd.
        */
        KMEM_FREE(myndp, sizeof(*myndp));
    }

    STACK_CHECK();

    return(error);
}

typedef	ssize_t (*rwfunc_t)(struct file *, char *, size_t, loff_t *);

STATIC int
mvop_linux_rw_kernel(
    struct uio *uiop,
    int flags,
    struct file *fp,
    rwfunc_t funcp
);

/* XXX Should all vnlayer_ stuff be moved to mvfs_linux_utils.c/h? */
STATIC void
vnlayer_linux_inode2vattr(
    INODE_T *inode,
    VATTR_T *vattr
)
{

#define SET(UUU,lll)                            \
    VATTR_SET_ ## UUU(vattr,inode->i_ ## lll)

    VATTR_NULL(vattr);
    VATTR_SET_TYPE(vattr, vnlayer_mode_to_vtype(inode->i_mode));
    SET(MODE_RIGHTS, mode);
    SET(UID, uid);
    SET(GID, gid);
    /* We're only called with inodes representing some other file
    ** system's storage, so we trust their i_sb (although we probably
    ** should call the getattr method on the inode instead of copying
    ** i_sb->s_dev).  For other cases in the 2.6 kernel we pass the
    ** fsid to a struct kstat "directly" without having to go through
    ** the i_dev field (see vnode_iop_getattr).
    */
    VATTR_SET_FSID(vattr, &(inode->i_sb->s_dev));
    SET(RDEV, rdev);
    SET(NODEID, ino);
    SET(NLINK, nlink);
    SET(SIZE, size);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
    SET(BLKSIZE, blksize);
#else
    vattr->va_blksize = 1 << inode->i_blkbits;
#endif
    SET(NBLOCKS, blocks);
#undef SET
    VATTR_SET_ATIME_TS(vattr, &(inode->i_atime));
    VATTR_SET_MTIME_TS(vattr, &(inode->i_mtime));
    VATTR_SET_CTIME_TS(vattr, &(inode->i_ctime));
    vattr->va_mask = AT_ALL;
}

extern int
mvop_linux_access(
    VNODE_T *vp,
    int mode,
    int flags,
    CALL_DATA_T *cd,
    nameidata_ctx *ctx
)
{
    int err;
    int save_ids;
    STACK_CHECK_DECL()
    vnlayer_fsuid_save_t saved_ids;

    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));
    ASSERT(CVN_TO_INO(vp));

    save_ids = vnlayer_fsuid_save(&saved_ids, MVFS_CD2CRED(cd));
    /* permission() handles BKL */
    STACK_CHECK();
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
    err = permission(CVN_TO_INO(vp), mode >> 6, (struct nameidata *)ctx);
#else
    err = inode_permission(CVN_TO_INO(vp), mode >> 6);
#endif
    err = vnlayer_errno_linux_to_unix(err);
    STACK_CHECK();
    if (save_ids)
        vnlayer_fsuid_restore(&saved_ids);

    MDKI_TRACE(TRACE_ACCESS, "mvop_linux_access vp=%p ip=%p mode=%d err=%d\n",
              vp, CVN_TO_INO(vp), mode, err);
    return err;
}

extern int
mvop_linux_getattr(
    VNODE_T *vp,
    VATTR_T *vap,                       /* RETURN */
    int flags,
    CRED_T *cred
)
{
    struct kstat *kstatp;
    INODE_T *ip;
    int err;
    STACK_CHECK_DECL()

    /* Fill in VATTR_T structure for a given vnode. */

    ASSERT(vp && CVN_TO_DENT(vp));
    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));

    /* Get attributes from inode, fill in vap */
    ip = CVN_TO_INO(vp);
    VATTR_NULL(vap);
    /* We have a kernel interface to do all the work.  This also has filled
    ** in the inode, so we can use the code below to copy the info into the
    ** vattr structure (except for the fsid, which we handle specially).
    */
    kstatp = KMEM_ALLOC(sizeof(*kstatp), KM_SLEEP);
    if (kstatp == NULL)
        err = ENOMEM;
    else
        err = vfs_getattr(VTOVFSMNT(vp), CVN_TO_DENT(vp), kstatp);
    STACK_CHECK();

    if (!err) {
#define GET(lll,UUU) VATTR_SET_ ## UUU(vap, kstatp->lll)
        VATTR_SET_TYPE(vap, vp->v_type);
        GET(uid,UID);
        GET(gid,GID);
        /* For 2.6 there is no i_dev field, so get it from the kstat. */
	/* In fact, we will get everything from the kstat */
        VATTR_SET_FSID(vap, &(kstatp->dev));
        VATTR_SET_MODE_RIGHTS(vap, kstatp->mode);
        GET(rdev,RDEV);
        GET(ino,NODEID);
        GET(nlink,NLINK);
        GET(size,SIZE);
#define GET_TIME(lll, UUU) VATTR_SET_ ## UUU ## _TS(vap, &kstatp->lll);
        GET_TIME(atime,ATIME);
        GET_TIME(mtime,MTIME);
        GET_TIME(ctime,CTIME);
#undef GET_TIME
        GET(blksize,BLKSIZE);
        GET(blocks,NBLOCKS);
#undef GET
        vap->va_mask = AT_ALL;
        MDKI_TRACE(TRACE_GETATTR,
                   "%s cvp=%p ip=%p vap=%p sz=%"VNODE_FMT_VATTR_SIZE_T_D" err=0n",
                   __func__, vp, ip, vap, VATTR_GET_SIZE(vap));
    }
    if (kstatp != NULL)
        KMEM_FREE(kstatp, sizeof(*kstatp));
    return vnlayer_errno_linux_to_unix(err);
}

STATIC void
vnlayer_linux_vattr2iattr(
    INODE_T *ip,
    VATTR_T *src,
    struct iattr *dst
)
{
    BZERO(dst, sizeof(*dst));

    /* Only do things which have valid bits set in vattr */
    if ((src->va_mask & (AT_MODE|AT_TYPE)) == (AT_MODE|AT_TYPE)) {
        dst->ia_mode = VATTR_GET_MODE(src) | vnlayer_vtype_to_mode(VATTR_GET_TYPE(src));
        dst->ia_valid |= ATTR_MODE;
    } else {
        if (src->va_mask & AT_MODE) {
            dst->ia_mode = VATTR_GET_MODE(src) | (ip->i_mode & S_IFMT);
            dst->ia_valid |= ATTR_MODE;
        }
        if (src->va_mask & AT_TYPE) {
            dst->ia_mode = vnlayer_vtype_to_mode(VATTR_GET_TYPE(src)) | (ip->i_mode & ~S_IFMT);
            dst->ia_valid |= ATTR_MODE;
        }
    }

#define GET(lll,UUU)                                    \
    if (src->va_mask & AT_ ## UUU) {                    \
        dst->ia_ ## lll = VATTR_GET_ ## UUU(src);       \
        dst->ia_valid |= ATTR_ ## UUU;                  \
    }
    GET(uid,UID);
    GET(gid,GID);
    GET(size,SIZE);
#undef GET
#define GET(lll, UUU)                                      \
    if (src->va_mask & AT_ ## UUU) {                       \
        VATTR_GET_ ## UUU ## _TS(src, &(dst->ia_ ## lll)); \
        dst->ia_valid |= ATTR_ ## UUU;                     \
    }
    GET(atime,ATIME);
    GET(mtime,MTIME);
    GET(ctime,CTIME);
#undef GET

    /* Set the ATIME_SET flags and MTIME_SET flags as they were when they
     * came in.  If the ATIME_SET or MTIME_SET flags are set, you have to
     * be the owner of the file.  Otherwise, you just need write access.
     */
    if (src->va_mask & AT_ATIME_SET)
        dst->ia_valid |= ATTR_ATIME_SET;
    if (src->va_mask & AT_MTIME_SET)
        dst->ia_valid |= ATTR_MTIME_SET;

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,14)
    /* I guess nobody actually used this field so it's gone in 2.6.14. */
    dst->ia_attr_flags = 0;
#endif
    /* ignore fsid, nodeid, nlink, blksize, nblocks: they're all AT_NOSET */
}

extern int
mvop_linux_setattr(
    VNODE_T *vp,
    VATTR_T *vap,
    int flags,
    CALL_DATA_T *cd
)
{
    struct iattr ia;
    DENT_T *dent;
    int err = 0;
    int save_ids;
    STACK_CHECK_DECL()
    vnlayer_fsuid_save_t saved_ids;
    mdki_boolean_t tooksem = FALSE;

    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));
    MDKI_TRACE(TRACE_SETATTR,
              "%s: other node vp=%p mask=%x atime/mtime = %x/%x\n",
              __func__, vp, vap->va_mask, (u_int)VATTR_GET_ATIME(vap),
              (u_int)VATTR_GET_MTIME(vap));
    /* Take attributes from VAP, push into inode */
    if (vap->va_mask & AT_NOSET)
        return EINVAL;

    dent = CVN_TO_DENT(vp);
    ASSERT(dent);
    STACK_CHECK();
    vnlayer_linux_vattr2iattr(CVN_TO_INO(vp), vap, &ia);
    STACK_CHECK();

    save_ids = vnlayer_fsuid_save(&saved_ids, MVFS_CD2CRED(cd));
    STACK_CHECK();
    if (ia.ia_valid & ATTR_SIZE) {
        /* be paranoid and record the 'taken'ness in case the called
           function squashes ia.ia_valid. */
        tooksem = TRUE;
        /* XXX could this already be held? */
        LOCK_INODE(dent->d_inode);
    }
    err = MDKI_NOTIFY_CHANGE(dent, CVN_TO_VFSMNT(vp), &ia);
    STACK_CHECK();
    err = vnlayer_errno_linux_to_unix(err);
    if (tooksem)
        UNLOCK_INODE(dent->d_inode);
    if (save_ids)
        vnlayer_fsuid_restore(&saved_ids);

    STACK_CHECK();
    MDKI_TRACE(TRACE_SETATTR,
              "mvop_linux_setattr vp=%p dent=%p vap=%p err=%d\n",
              vp, dent, vap, err);
    return err;
}

extern int
mvop_linux_open(
    VNODE_T **vpp,
    int mode,
    CALL_DATA_T *cd,
    file_ctx *ctx
)
{
    int status;
    struct file *fp;
    void *realfp = NULL;
    VNODE_T *vp;                    /* the real dentry we want to open */
    STACK_CHECK_DECL()

    fp = (struct file *) ctx;

    /*
     * Some callers might pass null context, but we don't expect to be
     * called on Linux in such cases.
     */
    ASSERT(ctx != NULL);
    if (!fp)
        BUG();

    ASSERT(REALFILE(fp) == NULL);
    MDKI_TRACE(TRACE_OPEN, "%s: %p op=%p pd=%p\n",
              __func__, fp, fp->f_op, REALFILE(fp));
    /* Protect the vpp from being swapped by kernel open */
    vp = *vpp;
    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));
    VN_HOLD(vp);                    /* extra hold to avoid problems */
    status = mvop_linux_open_kernel(&vp, mode & (FWRITE|FREAD|FTRUNC|FAPPEND),
                                    MVFS_CD2CRED(cd), &realfp);
    STACK_CHECK();
    /*
     * linux_open either returned the same vnode, in which case
     * we can drop our extra count on it from above, or it
     * dropped a hold on the passed in vp (*vpp),
     * and returned a new vp with a hold on it, and
     * we mask the swap of entries by dropping the returned
     * vp.  Either way, vp is the right vnode to drop.
     */
    VN_RELE(vp);

    if (status == 0) {
        SET_REALFILE(fp, realfp);
        if (fp->f_op == &vnode_file_mmap_file_ops &&
            (!REALFILE(fp)->f_op || !REALFILE(fp)->f_op->mmap)) 
        {
            /* no mmap: swap file ops */
            MDKI_TRACE(TRACE_OPEN,
                      "%s: realvp %p has no mmap\n", __func__, *vpp);
            fp->f_op = &vnode_file_file_ops;
        }
        MDKI_TRACE(TRACE_OPEN,
                  "%s: opened realvp %p cnt %d realfp %p fcnt %d\n",
                  __func__, *vpp, D_COUNT(CVN_TO_DENT(*vpp)),
                  REALFILE(fp), F_COUNT(REALFILE(fp)));
    }

    return status; /* already converted by mvop_linux_open_kernel() */
}

extern int
mvop_linux_close(
    VNODE_T *vp,
    int flags,
    VNODE_LASTCLOSE_T count,
    MOFFSET_T off,
    CRED_T *cred,
    file_ctx *ctx
)
{
    int status = 0;
    struct file *realfp;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) && !defined(SLES10SP2)
    struct file *fp = (struct file *) ctx;
    fl_owner_t owner_id = NULL;
#else
    struct file *fp = ctx ? ((mdki_vop_close_ctx_t *)ctx)->file_p : NULL;
    fl_owner_t owner_id = ctx ? ((mdki_vop_close_ctx_t *)ctx)->owner_id : NULL;
#endif
    STACK_CHECK_DECL()

    if (fp == NULL) {
        /* called in a case where the caller doesn't have the file pointer
         * (attempted error cleanup).  Just complain and return.
         */
        MDKI_VFS_LOG(VFS_LOG_ERR,
                     "%s: attempted close of vp %p without fp\n",
                     __func__, vp);
        return EINVAL;
    }
    realfp = REALFILE(fp);
    ASSERT(realfp);
    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));
    MDKI_TRACE(TRACE_CLOSE, "%s: %p op=%p pd=%p clcount=%d pid=%ld\n",
              __func__, fp, fp->f_op, realfp, count,
               (long) mdki_curpid());

    /* XXX locking on file? associated file system object
     * should be locked. */
#ifdef MVFS_DEBUG
    if (F_COUNT(fp) != count) {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                     "%s: count mismatch fpcnt=%d lccnt=%d rfpcnt=%d "
                     "(nfsd botch?)\n",
                     __func__, F_COUNT(fp), count, F_COUNT(realfp));
    }
#endif

    /* We're being called as part of a filp_close(filp, id) on our file (fp).
    ** We need to do the same things for the cleartext file (realfp).  The
    ** sequence of calls in filp_close() is:
    **
    **   f_op->flush(filp)
    **   dnotify_flush(filp, id)
    **   locks_remove_posix(filp, id)
    **   fput(filp)
    **
    ** So, our flush() should call the cleartext flush().  The dnotify_flush()
    ** doesn't call us (it's all done in the VFS), and it isn't exported so we
    ** can't call it here, either (I hope that is OK...we'll call it from the
    ** filp_close() in mvop_linux_close_kernel()).  The locks_remove_posix()
    ** call will come to our mvop_linux_lockctl() function (via our
    ** f_op->lock() entry) and we can call the cleartext lock() function from
    ** there.  Finally, the last fput() on our file will come to our last close
    ** code below (via our f_op->release call) so we can do the fput() on the
    ** cleartext.
    **
    ** The assumption is that our file "keeps the count" and the cleartext file
    ** only ever has a count of 1 (i.e. we don't pass on fget() on our file to
    ** the cleartext).  This avoids a NFS screwup where it doesn't decrement
    ** the count on its private file pointer when calling f_op->release().
     */
    if (count == VNODE_LASTCLOSE_COUNT) { /* Really a release (last close). */
        SET_REALFILE(fp, NULL); /* "Disconnect" ctxt file from ours.  */
        MDKI_TRACE(TRACE_CLOSE, "%s: closing realfp %p cnt %d\n",
		   __func__, realfp, F_COUNT(realfp));
        status = mvop_linux_close_kernel(vp, flags, F_COUNT(realfp), off, cred,
                                         realfp, owner_id);
    } else { /* Really a flush (i.e. not the last close) */
        if (realfp->f_op && realfp->f_op->flush) {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) && !defined(SLES10SP2)
            status = 
                   vnlayer_errno_linux_to_unix((*realfp->f_op->flush)(realfp));
#else
            status = 
                   vnlayer_errno_linux_to_unix((*realfp->f_op->flush)(realfp,
                                                                      owner_id));
#endif
        }
        /* See above for why we don't call anything else here. */
    }
    STACK_CHECK();

    MDKI_TRACE(TRACE_CLOSE, "%s: closed realvp %p cnt %d st=%d\n",
	       __func__, vp, D_COUNT(CVN_TO_DENT(vp)), status);

    return status;                      /* already converted */
}

#define COPIED_FLAGS (O_APPEND|O_NONBLOCK) /* copied on each operation */
/*
 * Linux also copies in FASYNC (we don't support it) and O_NDELAY, which is
 * for other OS binary compatibility only.
 */

/*
 * Cleartext read/write.
 */
extern int
mvop_linux_rdwr(
    VNODE_T *vp,
    struct uio *uiop,
    uio_rw_t rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd,
    file_ctx *ctx
)
{
    struct file *fp = (struct file *) ctx;
    struct file *realfp;
    int err;
    STACK_CHECK_DECL()

    MDKI_TRACE(TRACE_RDWR, "%s: fp=%p\n", __func__, fp);
    ASSERT(REALFILE(fp));
    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));

    realfp = REALFILE(fp);
    ASSERT(CVN_TO_DENT(vp) == realfp->f_dentry);
    realfp->f_pos = fp->f_pos;
    realfp->f_ra = fp->f_ra;    /* Copy struct file_ra_state */
    realfp->f_version = 0;      /* See default_llseek() in fs/read_write.c */
    realfp->f_flags = (realfp->f_flags & ~COPIED_FLAGS) |
        (fp->f_flags & COPIED_FLAGS);
    if (realfp->f_op != NULL) {
        err = mvop_linux_rw_kernel(uiop, ioflag, realfp,
                  rw == UIO_READ ?
                  realfp->f_op->read : (rwfunc_t)realfp->f_op->write);
        STACK_CHECK();
    } else {
        err = EIO;
    }
    if (err == 0 && vap != NULL) {
        vnlayer_linux_inode2vattr(realfp->f_dentry->d_inode, vap);
        STACK_CHECK();
    }
    /* our f_pos is handled by vnode_fop_rdwr() */
    /* XXX copy up other real file pointer stuff, e.g. f_reada? */
    return err;
}

extern int
mvop_linux_ioctl(
    VNODE_T *vp,
    int cmd,
    caddr_t data,
    int flag,
    CALL_DATA_T *cd,
    int *rvalp,
    VOPBD_T *vopbdp,
    MVFS_CALLER_INFO *callinfo
)
{
    int err = -ENOTTY;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
    FILE_T *fp;
    INODE_T *rvp;
    DENT_T *dp;
    STACK_CHECK_DECL()

    /* I don't know if the code for shadow inodes can be reached.  We only
     * come here from VOB files.  A shadow file has the dentry  switched
     * to the real dentry at open time so its ioctl path should never enter
     * mvfs code at all.  With the 2.6.16 interface changes it won't
     * work anyway because the new functions only get the inode from the
     * file pointer so a call to it would recurse.  So we leave the old code
     * in place and for new code return ENOTTY in all cases.
     */
    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));
    fp = callinfo->filp;
    /*
     * If we have one, we will try to pass the ioctl on to the real
     * file.  This assumes that this file would have used the default
     * file ops if it had been opened directly.
     */
    dp = CVN_TO_DENT(vp);
    if (MDKI_INOISSHADOW(dp->d_inode)) {
        rvp = REALDENTRY(dp)->d_inode;
        if (rvp->i_fop && rvp->i_fop->ioctl)
            err = rvp->i_fop->ioctl(rvp, fp, cmd, (unsigned long)data);
        STACK_CHECK();
    }
#endif
    return (vnlayer_errno_linux_to_unix(err));
}

extern int
mvop_linux_lookup_ioctl(
    char *path,
    int segflg,
    SYMFOLLOW_T follow,
    VNODE_T **dvpp,
    VNODE_T **vpp,
    CRED_T *cred
)
{
    int rv;

    ASSERT(dvpp == NULL);
    rv = mvop_linux_lookupvp(NULL, path, segflg, follow, vpp, cred,
                            NULL, NULL);

    return rv;
}

/*
 * In order to perform the lookup in the system namespace, we switch
 * the process root to the system root for the duration of the lookup.
 * At the end we swap the process root back to the original root.
 */
extern int
mvop_linux_lookup_storage_file(
    char *path,
    VNODE_T **vpp,                      /* return */
    CRED_T *cred
)
{
    int error = 0;
    struct fs_struct *temp_fs, *my_fs;
    struct nameidata nd;
    STACK_CHECK_DECL()

    ASSERT (vpp != NULL);
    ASSERT(path[0] == '/');

    /*
     * Swap in a totally new struct fs, to make sure we don't
     * interfere with lookups in another process sharing our struct.
     *
     * I worry that it's not safe to put something referenced by
     * the task structure on the stack, so I allocate it from heap instead.
     */
    my_fs = vnlayer_make_temp_fs_struct();
    if (my_fs == NULL) {
        return ENOMEM;
    }

    temp_fs = vnlayer_swap_task_fs(current, my_fs);

    STACK_CHECK();

    /* set up to do the lookup from the system root */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,9)
    memset(&nd, 0, sizeof(struct nameidata));
#endif
#if defined(IT_LOOKUP)
    /*
     * The intent_* stuff is part of support in SLES for Lustre clustering.
     */
    intent_init(&nd.intent, IT_LOOKUP);
#endif
    MDKI_NAMEI_SET_DENTRY(&nd, dget(vnlayer_sysroot_dentry));
    MDKI_NAMEI_SET_MNT(&nd, MDKI_MNTGET(vnlayer_sysroot_mnt));
    nd.flags = LOOKUP_FOLLOW;

    /* Both path_walk and vfs_path_lookup have a bogus interface.
     * They release the nameidata on errors but not on success.
     */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
    error = path_walk(path, &nd);
#else
    error = vfs_path_lookup(MDKI_NAMEI_DENTRY(&nd),
                    MDKI_NAMEI_MNT(&nd), path, nd.flags, &nd);
#endif
    STACK_CHECK();
    error = vnlayer_errno_linux_to_unix(error);
    if (error == 0) {
        if (MDKI_NAMEI_DENTRY(&nd)->d_inode == 0) {
            /* path walk returned a negative dentry and no error.
             * Turn this into an error.
             */
            error = ENOENT;
            *vpp = NULL;
        } else {
            *vpp = CVN_CREATE(MDKI_NAMEI_DENTRY(&nd),
                              MDKI_NAMEI_MNT(&nd));
            STACK_CHECK();
            if (*vpp == NULL) {
                error = ENFILE;
            } else {
                ASSERT(CVN_TO_INO(*vpp));
                ASSERT(!mdki_vpismfs(*vpp));
                ASSERT(!MDKI_INOISOURS(CVN_TO_INO(*vpp)));
            }
        }
        MDKI_PATH_RELEASE(&nd);
    }

    (void) vnlayer_swap_task_fs(current, temp_fs);

    vnlayer_free_temp_fs_struct(my_fs);

    STACK_CHECK();
    return error;
}

/* This is a stripped down lookup function to get a single component.
 * It does not follow links or deal with full pathnames. It was prompted
 * because in 2.6, path_walk now returns an error if not found.  This is
 * OK for ioctl lookups but for lookup component, we want the negative
 * dentry to use for create later on.
 *
 * Note:  This function returns with the parent inode locked.
 */
STATIC int MVFS_NOINLINE
mvop_linux_lookup_single(
    VNODE_T *dvp,
    char *path,
    VNODE_T **cvpp,
    CRED_T *cred,
    DENT_T *dentry
)
{
    int error = 0;
    int save_ids;
    vnlayer_fsuid_save_t saved_ids;
    DENT_T *pdent;
    DENT_T *found = NULL;
    struct vfsmount *mnt;
    int saved_link_cnt;
    struct nameidata *ndp;
    STACK_CHECK_DECL()
    
    if ((ndp = KMEM_ALLOC(sizeof(*ndp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    *cvpp = NULL;

    save_ids = vnlayer_fsuid_save(&saved_ids, cred);
    STACK_CHECK();
    /* path walk assumes that the nameidata is set up and the counts are
     * incremented.  vfs_path_lookup will initialize the nameidata and
     * bump the counts itself.  We still bump the counts here just to prevent
     * the unlikely case of the dentries and vfsmount structures from going
     * away in the short window before vfs_path_lookup bumps the counts itself.
     */
    CVN_SPINLOCK();
    pdent = VNODE_DGET(CVN_TO_DENT(dvp));
    mnt = MDKI_MNTGET(CVN_TO_VFSMNT(dvp));
    CVN_SPINUNLOCK();
    STACK_CHECK();
    ASSERT(pdent->d_parent->d_inode != NULL); /* make sure autofs4 will be happy */
    saved_link_cnt = current->total_link_count;
    memset(ndp, 0, sizeof(*ndp));
    MDKI_NAMEI_SET_DENTRY(ndp, pdent);
    MDKI_NAMEI_SET_MNT(ndp, mnt);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
    error = path_walk(path, ndp);
#else
    error = vfs_path_lookup(MDKI_NAMEI_DENTRY(ndp),
                    MDKI_NAMEI_MNT(ndp), path, 0, ndp);
#endif
    current->total_link_count += saved_link_cnt;
    STACK_CHECK();
    /* If we succeeded, tell what we know. */
    /* But it is now more complicated now that we have replaced the call
     * to lookup_one_len with a call to path_walk.  We need path_walk to 
     * get the locking right for autofs4 which may pend in revalidate.
     * Path_walk does not hold the parent inode semaphore when it calls
     * cached_lookup.  The other lookup functions (lookup_one_len, lookup_hash)
     * do.
     * It used to be that on the ENOENT case, we redid the lookup by calling
     * lookup_one_len and creating a CVP to connect the negative loopback
     * dentry and the negative real dentry.  This had the unfortunate side
     * effect of raising the d_count on the real dentry which caused it to
     * stay around forever.  (Part of the problem is that we don't release
     * the dentry pointer in the CVP until i_clear is called releasing the
     * inode. Since negative dentries do not have an inode pointer, this 
     * was never happening.  I have changed our d_delete function to 
     * put the dummy inode pointer into the d_inode field on negative
     * dentries so that i_clear does get called.
     * While the d_delete change may have been sufficient, I have chosen to 
     * not create a CVP for negative dentries. This will keep the counts 
     * correct for lookups in calls such as stat without creating and 
     * then immediately freeing a real negative dentry.
     * Lookup_one_len or lookup_hash will need to be called at the point at 
     * which a real negative dentry is required for file creation.  As long 
     * as we cannot open shadow files, this is  only required for mkdir and 
     * for rename.  This has the added advantage
     * that the final lookup (at least in rename) is done under the real
     * parent inode lock.
     * As long as we only deal with loopback directories for view-extended
     * path names, I do not expect a problem with delaying the final lookup
     * causing the hangs described above.
     */
    if (error) {
        error = vnlayer_errno_linux_to_unix(error);
        /* No need to release nameidata because path_walk does it on error.*/
    } else {
        found = MDKI_NAMEI_DENTRY(ndp);
        error = vnlayer_lookup_post(dentry, found, cvpp, MDKI_NAMEI_MNT(ndp));
        /* vnlayer_lookup_post() has dropped our reference on 'found' */
        MDKI_PATH_RELEASE(ndp);
    }
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24)
    /* Here we release the extra counts we got above.
     * The spinlock covered the the cvn and not the entries
     * We need to release these counts even in the error case.
     */
    VNODE_DPUT(pdent);
    MDKI_MNTPUT(mnt);
#endif
    STACK_CHECK();
    if (save_ids)
        vnlayer_fsuid_restore(&saved_ids);

    /* Log error before free pname buf if debug logging */
    if (error == ENOENT) {
        MDKI_TRACE(TRACE_USERLOOKUP, "lookupname comp=%s ENOENT\n", path);
    } else {
        MDKI_TRACE(TRACE_USERLOOKUP, "lookupname comp=%s rval=%d\n",
                   path, error);
    }
    STACK_CHECK();
    KMEM_FREE(ndp, sizeof(*ndp));
    return(error);
}

/*
 * MVOP_LINUX_REALVP
 *
 * This is a function to imitate the VOP_REALVP function on SVR4.  As far
 * as I can tell, Linux doesn't really do a lot to provide for stacked
 * filesystems.  This function will look to see if this is an mvfs file.
 * If it is, it will return the realvp pointer from the mnode, otherwise
 * it will return the same pointer that was passed in.
 */

extern int
mvop_linux_realvp(
    VNODE_T *vp,
    VNODE_T **rvp
)
{
    /*
     * This function is only used in ioctl inval path, in which case
     * we want to use the current object for MVFS nodes, and don't
     * care about any other type.
     */
    *rvp = vp;
    return(0);
}

/*
 * We assume callers are doing something like:
 * VOP_REALVP(cvp, &cvp)
 */
extern int
mvop_linux_realcvp(
    VNODE_T *cvp,
    VNODE_T **rcvpp
)
{
    VNODE_T *rcvp;
    INODE_T *ip;

    ASSERT(MDKI_INOISCLRVN(VTOI(cvp)));
    ASSERT(CVN_TO_DENT(cvp));
    ASSERT(*rcvpp == cvp);

    ip = CVN_TO_INO(cvp);

    if (MDKI_INOISSHADOW(ip)) {
        /*
         * For shadow nodes, unwrap one layer and return a CVN attached to
         * the real dentry underneath
         */
/*        ASSERT(REALDENTRY(cvp) == MFS_CLRVP(vp)); */
        /* reference shadow's cvn */
        /*
         * Callers which use VOP_REALVP() or MVOP_REALCVP()
         * must be willing to live without a vfsmnt (can't use the
         * node for name lookup operations).
         */
        rcvp = CVN_CREATE(REALDENTRY(CVN_TO_DENT(cvp)), NULL);
        if (rcvp != NULL) {
            VN_RELE(cvp);
            *rcvpp = rcvp;
        } else {
            /*
             * allocation failure.  Oh well, let it use the original
             * object, it's not fatal, just inefficient.
             */
        }
    }
    return 1;                           /* take our results as-is */
}

static inline void
vnlayer_raw_fixup(
    DENT_T **dentrypp,
    VNODE_T **cvpp
)
{
    if (dentrypp) {
        /*
         * set ops on caller's initialized dentry; it will be dput() shortly
         * after mvfs_linux_lookup() returns
         */
        MDKI_SET_DOPS(*dentrypp, &vnode_shadow_dentry_ops);
        /* indicate the real object's dentry */
        *dentrypp = VNODE_DGET(CVN_TO_DENT(*cvpp));
        /* XXX can we do something with the vfsmnt? */
    }
}

/* mvop_linux_lookup_component
 * This is a special function for looking up single pathname components.
 * It will lookup the filename in the directory specified by passing the
 * request on to mvop_linux_lookupvp.  If the file returned is not a directory,
 * then we will create our own inode for this file and copy the relevant
 * data to it.  We will set this inode up with pointer to the proper shadow
 * inode and file ops.  The shadow ops will know how to get from the
 * inode that we return to the actual underlying ops.  This works on the
 * assumption that we are only called for components that do not already
 * have dcache entries.
 */

extern int
mvop_linux_lookup_component(
    VNODE_T *dvp,
    char *nm,
    VNODE_T **cvpp,
    CRED_T *cred,
    lookup_ctx *ctx
)
{
    DENT_T **dentrypp;
    int error = 0;
    INODE_T *shadow_inode;
    STACK_CHECK_DECL()

    if ((ctx->flags & LOOKUP_CTX_VALID) != 0)
        /* indicates we should use dentry context */
        dentrypp = (DENT_T **)ctx->dentrypp;
    else
        dentrypp = NULL;

    error = mvop_linux_lookup_single(dvp, nm, cvpp, cred,
		                     dentrypp ? *dentrypp : NULL);
    if (error) {
	return(error);
    }
    switch ((*cvpp)->v_type) {
      case VDIR:
        /* This is a directory, linking is handled in makeloopnode */
        break;
      case VSOCK:
      case VBLK:
      case VCHR:
        /* We handle sockets and devices differently.  We never cover them
         * and always return the real dentry.  This will allow connect and
         * other code which has put data in the inode to access it directly
         * from the dentry.  Otherwise we have problems trying to access
         * them when set to a view.
         */
        /* If we have a dentrypp, it's a fresh dentry to be set up by
         * mvfs_linux_lookup() */
        vnlayer_raw_fixup(dentrypp, cvpp);
        break;
      default:
#ifndef HAVE_SHADOW_FILES
        /*
         * RHEL4 kernel is based on later 2.6.x kernels which have removed
         * exported symbols we previously used.  We use a different
         * setview mechanism which works OK but doesn't allow us to shadow
         * plain files in view-extended naming.  We only shadow symlinks.
         */
        MDKI_TRACE(TRACE_PNLOOKUP,"failing lookup of plain file %s\n", nm);
        error = EACCES;       /* like what we have to do on AIX */
        VN_RELE(*cvpp);
        *cvpp = NULL;
        break;
      case VLNK:
#endif /* !HAVE_SHADOW_FILES */
        /*
         * This is a normal file, we have to make the linkage
         * ourselves because makeloopnode won't cover it.
         */

        /* set up the new dentry */

        /* callers that don't pass a dentrypp are looking up `..' in a
           VOB root, and don't get to this code. */
        ASSERT(*dentrypp);
        MDKI_SET_DOPS(*dentrypp, &vnode_shadow_dentry_ops);
        /* get a shadow inode, and shadow the cvp */
        /*
         * We don't bother to use a hash table/chain to give the same
         * results in case of duplicates, since the linux dcache does
         * duplicate detection at a higher level
         */
        ASSERT((*dentrypp)->d_sb);
        STACK_CHECK();
        shadow_inode = new_inode((*dentrypp)->d_sb);
        STACK_CHECK();
        if (shadow_inode == NULL) {
            error = ENFILE;
        } else {
            /* Callers expect to get back a CLR_VNODE, from which
             * they'll extract a VNODE_T to use.
             * this vnode gets returned to mvfs_linux_lookup() which
             * instantiates it for the system's dentry.  We have to
             * fake up a dentry and clrvnode to satisfy our caller,
             * but since the clrvnode is short-lived (discarded by our
             * caller), this should not cause any problems.
             * (this clrvn gets returned to linux_lookup, but it will unwrap it)
             */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16)
            DENT_T *parent = (*dentrypp)->d_parent;
#else
            DENT_T *parent = NULL;
#endif
            DENT_T *dent;
            MDKI_TRACE(TRACE_VNODES,"shadowvn_alloc vp=%p\n", shadow_inode);
            /*
             * We could use d_alloc_root here, but it does a
             * d_instantiate internally which we don't want to do
             * until after the vnlayer_shadow_inode call sets up the
             * shadow inode's i_sb.
             */
            STACK_CHECK();
            /*
             * OK to leave this dentry with no ops, we don't have special
             * stuff that needs revalidate or release handling
             */
            dent = VNODE_D_ALLOC(parent, &(*dentrypp)->d_name);
            if (dent == NULL) {
                error = ENOMEM;
                iput(shadow_inode);
                STACK_CHECK();
            } else {
                ASSERT(CVN_TO_DENT(*cvpp));
                vnlayer_shadow_inode(CVN_TO_INO(*cvpp), *dentrypp,
                                  shadow_inode);
                STACK_CHECK();
                /*
                 * lookupvp already saved the real cvn (mnt, dentry)
                 * of the object in caller's dentry.  Drop the cvpp
                 * for the underlying object, we don't need it.
                 */
                ASSERT(REALCVN(*dentrypp));
                VN_RELE(*cvpp);
                /* return new cvpp, from which caller will extract
                   dentry->d_inode, then release the cvpp */
                STACK_CHECK();
                VNODE_D_INSTANTIATE(dent, shadow_inode);
                STACK_CHECK();
                *cvpp = CVN_CREATE(dent, NULL);
                STACK_CHECK();
                if (*cvpp == NULL) {
                    error = ENFILE;
                    iput(shadow_inode);
                }
                if (dent->d_parent == NULL) /* Provide a parent pointer for */
                    dent->d_parent = dent;  /* dput() if we don't have one  */
                d_drop(dent);               /* make sure it goes away */
                VNODE_DPUT(dent);           /* drop extra ref from d_alloc() */
            }
        }
        if (error) {
            VN_RELE(*cvpp);
            *cvpp = NULL;
            /*  *dentrypp gets cleaned up when caller releases it. */
        }
    }
    return (error);
}

STATIC int
vnlayer_flags_to_fmode(int flags)
{
    int rv = 0;
    /* fp->f_flags includes O_CREAT, etc. */
    /* fp->f_mode is FMODE_{READ,WRITE}|O_{APPEND,TRUNC,CREAT} */
    if (flags & FREAD)
        rv |= FMODE_READ;
    if (flags & FWRITE)
        rv |= FMODE_WRITE;
    if (flags & FAPPEND)
        rv |= O_APPEND;
    if (flags & FTRUNC)
        rv |= O_TRUNC;
    if (flags & FCREAT)
        rv |= O_CREAT;
    return rv;
}

/*
 * loop-back file IO kludges.  The Linux file system interface is not
 * strictly inode/vnode based; callers need to deal with file
 * interfaces as well.  For kernel internal uses we can keep track
 * fairly easily with these glue routines.
 */

/* Returns BSD error codes */
extern int
mvop_linux_open_kernel(
    VNODE_T **dpp,                      /* call/return */
    int flags,
    CRED_T *cred,
    void **filp                         /* RETURN */
)
{
    int err;
    struct file *fp;
    INODE_T *ip;
    DENT_T *dentry;
    VNODE_T *cvp = *dpp;
    vnlayer_fsuid_save_t oldfsuid;
    mdki_boolean_t swap_ids;
#if (defined RATL_REDHAT && RATL_VENDOR_VER >= 600) || \
    (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32))
    /* Declare a type so that we can do one allocation and save stack space */
    struct {
        struct path path;
        struct nameidata nd;
    } *alloc_unitp;
#endif
    STACK_CHECK_DECL()

/*    ASSERT(flags == FREAD || flags == FWRITE || flags == (FWRITE|FTRUNC));*/

    if (flags & FCREAT) {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                 "%s: can't handle CREAT flag, flags=%x",
                 __func__, flags);
        return EINVAL;
    }
#if (defined RATL_REDHAT && RATL_VENDOR_VER >= 600) || \
    (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32))
    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    memset(alloc_unitp, 0, sizeof(*alloc_unitp));
#endif

    ASSERT(MDKI_INOISCLRVN(VTOI(cvp)));
    ASSERT(CVN_TO_DENT(cvp) && CVN_TO_INO(cvp));
    ip = CVN_TO_INO(cvp);
    dentry = CVN_TO_DENT(cvp);
    if (ip->i_op == NULL || ip->i_fop == NULL) {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                 "%s: vnode %p has bad ops/default files (%p,%p)?\n",
                 __func__, ip, ip->i_op, ip->i_fop);
    }

    *filp = NULL; /* clean out caller's return before doing real work */
    /*
     * simulate work from open_namei() for truncate: check locks, then call
     * truncate routine.
     */
    /* XXX check leases */
    /* I don't know that there are any cases in which we are called with
     * the FTRUNC flag set where the size will not already be 0.  I will
     * leave the code in in case there is but there is no reason to explicitly
     * truncate a zero length file and it causes an error if the file was
     * created with 0444 permissions.
     */
    if ((flags & FTRUNC) && (READ_I_SIZE(ip) != 0)) {
        MDKI_TRACE(TRACE_OPEN,"%s: truncating ip=%p", __func__, ip);
        STACK_CHECK();
        if (IS_IMMUTABLE(ip) || IS_APPEND(ip)) {
            err = -EPERM;
            goto done;
        }
        if ((err = get_write_access(ip)) != 0)
            goto done;
        /*
         * locks_verify_locked() is inline, and it calls
         * the unexported locks_mandatory_locked() which we
         * redefine/reimplement.
         */
        if ((err = vnlayer_has_mandlocks(ip)) != 0) {
            put_write_access(ip);
            goto done;
        }
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,32)
        DQUOT_INIT(ip);
#elif LINUX_VERSION_CODE < KERNEL_VERSION(2,6,34)
        vfs_dq_init(ip);
#else
        dquot_initialize(ip);
#endif
        STACK_CHECK();
        swap_ids = vnlayer_fsuid_save(&oldfsuid, cred);
        STACK_CHECK();
        err = vnlayer_truncate_inode(dentry, CVN_TO_VFSMNT(cvp), 0, TRUE);
        STACK_CHECK();
        if (swap_ids)
            vnlayer_fsuid_restore(&oldfsuid);
        STACK_CHECK();
        put_write_access(ip);
        if (err)
            goto done;
    }
      else { 
        /* NFS does not go over the wire on open in the 2.6 kernel.  It
         * assumes the validation was done during lookup.  Since we 
         * don't look up the cleartext every time, we need to revalidate
         * it on open to make sure it is still there.  If we already
         * did a truncate, this is not necessary.
         */
        struct kstat *kp = KMEM_ALLOC(sizeof(*kp), KM_SLEEP);
        if (kp != NULL) {
            err = vfs_getattr(VTOVFSMNT(cvp), dentry, kp);
            KMEM_FREE(kp, sizeof(*kp));
        } else {
            err = -ENOMEM;
        }
        if (err)
	    goto done;
    }
    if (flags & FWRITE) {
        err = get_write_access(ip);
        if (err) {
            goto done;
        }
    }
    STACK_CHECK();
    swap_ids = vnlayer_fsuid_save(&oldfsuid, cred);
    STACK_CHECK();
    /* Caller's credentials get attached to the file */
#if (defined RATL_REDHAT && RATL_VENDOR_VER >= 600) || \
    (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32))
    alloc_unitp->path.mnt = MDKI_MNTGET((struct vfsmount *)cvp->v_vfsmnt);
    alloc_unitp->path.dentry = VNODE_DGET(dentry);
    fp = alloc_file(&(alloc_unitp->path), vnlayer_flags_to_fmode(flags),
                    fops_get(ip->i_fop));
#else
    fp = get_empty_filp();
#endif /* RHEL6 or 2.6.33 and beyond */
    if (swap_ids)
        vnlayer_fsuid_restore(&oldfsuid);

    if (fp == NULL) {
        if (flags & FWRITE)
            put_write_access(ip);
        err = -ENFILE;
        goto done;
    }
    ASSERT(F_COUNT(fp) == 1);

    /*
     * initialize our new file pointer by hand--can't use
     * init_private_file() as it erases the existing contents
     * (including UIDs) and it calls the open routine (we're not ready
     * to do that)
     */

    STACK_CHECK();
    fp->f_flags = O_LARGEFILE;
    /* the following is only needed when using get_empty_filp */
#if !(defined RATL_REDHAT && (RATL_VENDOR_VER >= 600)) && \
    (LINUX_VERSION_CODE < KERNEL_VERSION(2,6,33))
    fp->f_dentry = VNODE_DGET(dentry);
    fp->f_mode = vnlayer_flags_to_fmode(flags);
    fp->f_op = fops_get(ip->i_fop);
    fp->f_vfsmnt = MDKI_MNTGET(/* don't use CVN_TO_VFSMNT, it complains about the NULL case */(struct vfsmount *)cvp->v_vfsmnt);
    fp->f_mapping = fp->f_dentry->d_inode->i_mapping;
#endif
    VNLAYER_RA_STATE_INIT(&fp->f_ra, fp->f_mapping->host->i_mapping);
    if (fp->f_vfsmnt == NULL) {
        /*
         * only happens if cvp->mnt is NULL, which shouldn't happen unless
         * we couldn't find the vfsmnt via vnlayer_dent2vfsmnt().  This can be
         * made to happen by loopback mounting from a non-mountpoint.
         * e.g. mount --bind /usr/blah/blah /mnt
         * (when /usr is a mounted file system, but /usr/blah/blah is just
         * a regular directory inside the mounted file system).
         */
        err = -EOPNOTSUPP;               /* XXX not the best choice of code */
    } else {
        STACK_CHECK();
        if ((fp->f_op != NULL) && (fp->f_op->open != NULL))
            err = (*fp->f_op->open)(ip, fp);
        else
            err = 0;
        STACK_CHECK();
    }
#if (defined RATL_REDHAT && (RATL_VENDOR_VER >= 600)) || \
    (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32))
    /* The following little bit of foolishness is because NFSv4 has decided
     * that there is no reason for anyone to call their open function, so they
     * their file open function returns -ENOTDIR to tell us that this is so.
     * They expect that all of the work of their open would have been handled
     * in the d_revalidate call they make in the lookup code.  So we have to
     * check and call d_revalidate on the dentry so that NFS4 can put their
     * context pointer into the private_data field of the file structure 
     * that is squirreled away in intent structure of the nameidata.
     *
     * We should never get -ENOTDIR from any other filesystem because that 
     * error should only come on lookup.  I suspect NFSv4 chose -ENOTDIR 
     * just because it would be unique in this case. 
     */
    if (err == -ENOTDIR) {
        alloc_unitp->nd.path = alloc_unitp->path; 
        alloc_unitp->nd.flags = LOOKUP_OPEN;
        alloc_unitp->nd.intent.open.flags = flags;
        alloc_unitp->nd.intent.open.file = fp;
        if ((dentry->d_op != NULL) && (dentry->d_op->d_revalidate != NULL)) {
            err = dentry->d_op->d_revalidate(dentry, &(alloc_unitp->nd)); 
            if (err == 1) {
                /* A return of 1 is goodness in this case */
                err = 0;
            } else if (err == 0) {
                /* A return of 0 means the parent directory changed or
                 * something.  If we were lookup we could call d_invalidate
                 * and try again.
                 */
                err = -ESTALE;
            }
        }
    }
#endif
    if (err == 0) {
        *filp = (void *)fp;
        if (dentry != fp->f_dentry) {
            /* replace caller's hold on old entry with hold on new entry */
            /* We assume that the open method swapped f_vfsmnt and adjusted
             * ref counts on it too.
             */
            STACK_CHECK();
            *dpp = CVN_CREATE(fp->f_dentry, fp->f_vfsmnt);
            if (*dpp == NULL) {
                fput(fp);
                err = -ENFILE;
            } else {
                /* release the hold the caller had (we replaced it via dpp) */
                VN_RELE(cvp);
            }
            STACK_CHECK();
        }
    } else {
        fput(fp); /*includes a dput on fp->f_dentry & put_write_access() */
    }
done:
#if (defined RATL_REDHAT && (RATL_VENDOR_VER >= 600)) || \
    (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32))
    KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
#endif
    return vnlayer_errno_linux_to_unix(err);
}

#if defined RHEL_VERSION && RHEL_VERSION == 4 && RHEL_UPDATE >= 7
/* This may be needed for RHEL5 once the changes are migrated. 
 * If an errata is applied to your kernel that requires the defensive code
 * to be removed add -DMVFS_REMOVE_FLOCK_DEFENSIVE_CODE to the compile line in
 * the Makefile for the MVFS.
 */
#define MVFS_REMOVE_FLOCK_DEFENSIVE_CODE
#endif
/* returns normal (positive) error codes */
extern int
mvop_linux_close_kernel(
    VNODE_T *vp,
    int flags,
    VNODE_LASTCLOSE_T count,
    MOFFSET_T off,
    CRED_T *cred,
    void *filp,
    fl_owner_t owner_id
)
{
    struct file *fp = (struct file *)filp;
    STACK_CHECK_DECL()
    int rv;

    /* ASSERT() is OK, it uses native print routines, not our log routines */
    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));
    ASSERT(fp->f_dentry == CVN_TO_DENT(vp));
    STACK_CHECK();

#ifndef MVFS_REMOVE_FLOCK_DEFENSIVE_CODE
    /* The existance of locks at this point was a bug and Linux would panic
     * so we put in this code to prevent the panics.  But now the operation
     * of locks in the NFS has changed so that we can indeed have locks at
     * this point which will be freed in just a bit.  Just removing those
     * locks leaves the locks stranded.  The defensive code is not required
     * in RHEL4 Update 7 or if certain errata patches are applied.
     */

    /* This is supposed to be the final close, so see if there's still a lock,
    ** which would panic in locks_remove_flock() from __fput() from fput() from
    ** filp_close().
    */
    if (fp != NULL &&
        fp->f_dentry != NULL &&
        fp->f_dentry->d_inode != NULL &&
        fp->f_dentry->d_inode->i_flock != NULL &&
        fp->f_dentry->d_inode->i_flock->fl_file == fp)
    {
        /* There could be multiple locks on the inode and we only care about
        ** ones for this file (since that's what locks_remove_flock() cares
        ** about and will panic if not).  However, to look at the potential
        ** linked list of locks we would have to lock_kernel() and follow the
        ** list, just like locks_remove_flock() does, and that seems like too
        ** much trouble.  This test will catch the common (we hope) case of the
        ** "leftover lock" being the first one on the list (and I guess we'll
        ** just panic if things don't work out right).
        */
        MDKI_VFS_LOG(VFS_LOG_ERR,
                     "%s: Locks should be gone, continuing anyway. fp=%p vp=%p "
                     "cnt=%d fcnt=%ld po=%p pid=%d tgid=%d fl=%p flo=%p\n",
                     __func__, fp, vp, count, (long) F_COUNT(fp),
                     current->files, current->pid, current->tgid,
                     fp->f_dentry->d_inode->i_flock->fl_file,
                     fp->f_dentry->d_inode->i_flock->fl_owner);
        /* Prevent the panic, although this will leak memory. */
        fp->f_dentry->d_inode->i_flock = NULL;
    }
#endif
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
    /* Do the filp_close() on the cleartext.  This will also do the
    ** dnotify_flush(), among other things, that we couldn't do above (see
    ** mvfs_linux_close()).  We need the POSIX lock owner (fl_owner) that we
    ** saved away for this thread (current->files) in mvop_linux_lockctl() when
    ** a (any) lock was set.
    */
    owner_id = mvfs_linux_find_fl_owner();
#endif
    if (owner_id == NULL) {
        /* This is the best we can do, even though it might be NULL if we're in
        ** the __exit_files() path.
        */
        owner_id = current->files;
    }
    rv = filp_close(fp, owner_id);

    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(rv);
}

/* returns normal (positive) error codes */
STATIC int
mvop_linux_rw_kernel(
    struct uio *uiop,
    int flags,
    struct file *fp,
    rwfunc_t funcp
)
{
    ssize_t err = 0;
    mm_segment_t old_fs = MAKE_MM_SEG(0);
    STACK_CHECK_DECL()

    if (uiop->uio_iovcnt != 1) {
        MDKI_VFS_LOG(VFS_LOG_DEBUG, "%s: iovcnt=%d, caller=%p\n",
                     __func__, uiop->uio_iovcnt, mdki_getmycaller());
        return EINVAL;                  /* XXX */
    }
    if (funcp == NULL)
        return EIO;

    if (uiop->uio_segflg == UIO_SYSSPACE) {
        /* set up for kernel space read/write */
        old_fs = get_fs();
        set_fs(KERNEL_DS);
    }
    STACK_CHECK();
    /* already locked for I/O by upper layers */
    err = (*funcp)(fp, uiop->uio_iov->iov_base, uiop->uio_resid,
                   &uiop->uio_offset);
    STACK_CHECK();
    if (uiop->uio_segflg == UIO_SYSSPACE) {
        set_fs(old_fs);
    }

    if (err < 0)
        return vnlayer_errno_linux_to_unix(err);
    vnlayer_linux_adjust_uio(uiop, err, FALSE);

    return 0; /* don't return err, it's a count! */
}

/* returns normal (positive) error codes */
extern int
mvop_linux_read_kernel(
    struct uio *uiop,
    int flags,
    CRED_T *cred,
    char *filp
)
{
    struct file *fp = (struct file *)filp;
    int err;
    STACK_CHECK_DECL()

    if (!(fp->f_mode & FMODE_READ)) {
        MDKI_VFS_LOG(VFS_LOG_DEBUG, "%s: mode %o not FMODE_READ %o\n",
                     __func__, fp->f_mode, FMODE_READ);
        return EBADF;
    }
    if ((fp->f_op == NULL) || (fp->f_op->read == NULL))
        return EIO;                 /* XXX */

    STACK_CHECK();
    err = mvop_linux_rw_kernel(uiop, flags, fp, fp->f_op->read);
    STACK_CHECK();
    return err;
}

/* returns normal (positive) error codes */
extern ssize_t
mvop_linux_write_kernel(
    struct uio *uiop,
    int flags,
    CRED_T *cred,
    char *filp
)
{
    struct file *fp = (struct file *)filp;
    int err;
    STACK_CHECK_DECL()

    if (!(fp->f_mode & FMODE_WRITE)) {
        MDKI_VFS_LOG(VFS_LOG_DEBUG, "%s: mode %o not FMODE_WRITE %o\n",
                     __func__, fp->f_mode, FMODE_WRITE);
        return EBADF;
    }
    if ((fp->f_op == NULL) || (fp->f_op->write == NULL))
        return EIO;                 /* XXX */

    /* cast ...->write to a (rwfunc_t) since its (const char *) parameter
     * is technically incompatible.
     */
    err = mvop_linux_rw_kernel(uiop, flags, fp, (rwfunc_t)fp->f_op->write);
    STACK_CHECK();
    return err;
}

extern int
mdki_linux_createvp(
    char *path,
    VATTR_T *vap,
    VNODE_T **newdpp,
    CRED_T *cred,
    int flag                            /* unused */
)
{
    STACK_CHECK_DECL()
    int error = 0;
    vnlayer_fsuid_save_t oldfsuid;
    mdki_boolean_t swap_ids;
    struct fs_struct *temp_fs, *my_fs;
    struct file *fp;

    /*
     * Swap in a totally new struct fs, to make sure we don't
     * interfere with lookups in another process sharing our struct.
     */
    my_fs = vnlayer_make_temp_fs_struct();
    if (my_fs == NULL) {
        return ENOMEM;
    }

    swap_ids = vnlayer_fsuid_save(&oldfsuid, cred);
    STACK_CHECK();
    temp_fs = vnlayer_swap_task_fs(current, my_fs);

    STACK_CHECK();
    fp = filp_open(path, FMODE_READ | O_CREAT, vap->va_mode);
    if (IS_ERR(fp)) {
        error = PTR_ERR(fp);
    } else {
        *newdpp = CVN_CREATE(fp->f_dentry, fp->f_vfsmnt);
        /* CVN_CREATE dgets the dentry, drop fp since it is not needed */
        fput(fp);
        if (*newdpp == NULL) {
            error = -ENFILE;
        }
    }
    (void) vnlayer_swap_task_fs(current, temp_fs);
    vnlayer_free_temp_fs_struct(my_fs);

    STACK_CHECK();

    if (swap_ids)
        vnlayer_fsuid_restore(&oldfsuid);
    STACK_CHECK();

    return vnlayer_errno_linux_to_unix(error);
}

/* This is very similar to mvop_linux_mkdir(), so check there if you change
** something here.
*/
extern int
mvop_linux_mknod(
    VNODE_T *dvp,
    char *nm,
    vattr_t *vap,
    enum vcexcl excl,
    int mode,
    VNODE_T **vpp,
    CRED_T *cred,
    create_ctx *acontext
)
{
    int err = 0;
    INODE_T *parent;
    INODE_T *real_parent;
    DENT_T *real_dentry;
    DENT_T *parent_dentry;
    DENT_T *new;
    struct create_ctx *context = acontext;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(dvp)));

    /*
     * When making socket/device files, we create them in the underlying
     * file system, then make a dentry in our layer pointing to the inode
     * underneath.
     */
    new = context->dentry;
    parent = context->parent;

    parent_dentry = CVN_TO_DENT(dvp);
    real_parent = parent_dentry->d_inode;

    STACK_CHECK();
    LOCK_INODE(real_parent);
    /* There would be a possibility of a deadlock here if we are in an
     * autofs4 filesystem.  That is because autofs4 may pend in revalidate
     * without releasing the parent inode lock.  If this proves to be a 
     * problem, we should be able to go with the pre 2.6.0 code.
     */
    real_dentry = lookup_one_len(new->d_name.name, parent_dentry,
                                 new->d_name.len);
    STACK_CHECK();
    if (IS_ERR(real_dentry)) {
        err = PTR_ERR(real_dentry);
	real_dentry = NULL;
        UNLOCK_INODE(real_parent);
    }
    if (!err) {
        /*
         * We don't link up the new dentry to the real dentry,
         * because we don't shadow device nodes.
         */
#ifdef CONFIG_FS_POSIX_ACL
	if (!IS_POSIXACL(real_parent)) {
            mode &= ~mdki_get_ucmask();
	}
#endif
        STACK_CHECK();
        err = MDKI_VFS_MKNOD(real_parent, real_dentry, CVN_TO_VFSMNT(dvp),
                        mode, context->dev);
        STACK_CHECK();
        UNLOCK_INODE(real_parent);
    }
    if (!err) {
        STACK_CHECK();
        *vpp = CVN_CREATE(new, CVN_TO_VFSMNT(dvp));
        STACK_CHECK();
        if (*vpp == NULL) {
            err = -ENFILE;
        } else {
            MDKI_SET_DOPS(new, &vnode_shadow_dentry_ops);
            igrab(real_dentry->d_inode);
            STACK_CHECK();
            VNODE_D_INSTANTIATE(new, real_dentry->d_inode);
            STACK_CHECK();
        }
    }
    VNODE_DPUT(real_dentry);
    /* Note that lookup dentry dputs the parent_dentry */
    STACK_CHECK();

    return vnlayer_errno_linux_to_unix(err);
}

extern int
mvop_linux_create(
    VNODE_T *dvp,
    char *nm,
    vattr_t *vap,
    enum vcexcl excl,
    int mode,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    create_ctx *acontext
)
{
    /* Create shadow files */
#ifdef HAVE_SHADOW_FILES
    DENT_T *real_dentry, *dentry;
    INODE_T *parent;
    INODE_T *real_parent;
    INODE_T *shadow_inode;
    VNODE_T *cvp;
    struct create_ctx *context = acontext;
    STACK_CHECK_DECL()
    int err = 0;
#endif /* HAVE_SHADOW_FILES */

    ASSERT(MDKI_INOISCLRVN(VTOI(dvp)));

    if (VATTR_GET_TYPE(vap) == VCHR || VATTR_GET_TYPE(vap) == VBLK ||
        VATTR_GET_TYPE(vap) == VFIFO || VATTR_GET_TYPE(vap) == VSOCK)
    {
        return mvop_linux_mknod(dvp, nm, vap, excl, mode, vpp,
                                MVFS_CD2CRED(cd), acontext);
    }

#ifdef HAVE_SHADOW_FILES
    real_parent = CVN_TO_DENT(dvp)->d_inode;
    dentry = context->dentry;
    parent = context->parent;

    STACK_CHECK();
    if ((shadow_inode = new_inode(parent->i_sb)) == NULL) {
        return ENFILE;
    }
    LOCK_INODE(real_parent);

    /* We won't repeat most of the checking done in open_namei.  The
     * assumption is that the permissions on the parent and the real
     * parent are kept in sync, so we should have failed already if
     * we were going to.  This means that we will need to provide
     * an mvfs_revalidate dentry function for the loopclass directories.
     */
    ASSERT(REALVFSMNT(dentry));

    /* We do need to do some validation to make sure that the
     * real dentry is still good.  Someone may have removed the
     * real file while we waited for our lock, and a d_revalidate
     * would have would have cleared the REALCVN.
     * FIXME:  There is still a problem here because shadow_d_revalidate
     * does no locking that I can see so another processor could still
     * be racing with us.
     */
    real_dentry = REALDENTRY_LOCKED(dentry, &cvp);
    STACK_CHECK();
    VNODE_DGET(real_dentry);             /* hold inode, if any */
    if ((real_dentry == NULL) || (d_unhashed(real_dentry))) {
        /* Someone nuked the real dentry on us.  Time to go. */
        err = -ENOENT;
    } else {
        /* It shouldn't happen, but maybe someone created the file
         * while we were waiting, so take advantage of our good fortune
         */
#ifdef CONFIG_FS_POSIX_ACL
	if (!IS_POSIXACL(real_parent)) {
            mode &= ~mdki_get_ucmask();
	}
#endif
        if (!real_dentry->d_inode) {
            STACK_CHECK();
            /* NFSD passes a null nameidata to vfs_create, so we will too. */
            err = vfs_create(real_parent, real_dentry, mode, NULL);
            STACK_CHECK();
        }
    }
    UNLOCK_INODE(real_parent);
    if (err != 0) {
        iput(shadow_inode);
    } else {
        STACK_CHECK();
        *vpp = CVN_CREATE(dentry, CVN_TO_VFSMNT(dvp));
        STACK_CHECK();
        if (*vpp == NULL) {
            err = -ENFILE;
            iput(shadow_inode);
            STACK_CHECK();
        } else {
            vnlayer_shadow_inode(real_dentry->d_inode, dentry, shadow_inode);
            STACK_CHECK();
            MDKI_SET_DOPS(dentry, &vnode_shadow_dentry_ops);
            VNODE_D_INSTANTIATE(dentry, shadow_inode);
        }
        STACK_CHECK();
    }
    if (real_dentry) {
        VNODE_DPUT(real_dentry);
        REALDENTRY_UNLOCK(dentry, cvp);
    }
    return vnlayer_errno_linux_to_unix(err);
#else /* ! HAVE_SHADOW_FILES */
    return EROFS;
#endif /* HAVE_SHADOW_FILES */
}

extern int
vnlayer_do_linux_link(
    VNODE_T *tdvp, /* vnode of the real parent */
    DENT_T *olddent,
    INODE_T *parent, /* inode of shadow parent */
    DENT_T *newdent
)
{
#ifdef HAVE_SHADOW_FILES
    /* Create shadow files */
    DENT_T *rold;
    DENT_T *rnew = NULL;
    INODE_T *real_parent;
    INODE_T *shadow_inode;
    VNODE_T *oldcvp, *newcvp;
    int err = 0;

    STACK_CHECK_DECL()
#endif /* HAVE_SHADOW_FILES */

    ASSERT(MDKI_INOISCLRVN(VTOI(tdvp)));
    ASSERT(REALVFSMNT(newdent));

    if (olddent->d_op != &vnode_shadow_dentry_ops)
        /* just in case we get here for a non-shadow object */
        return -EXDEV;

#ifdef HAVE_SHADOW_FILES
    rold = REALDENTRY_LOCKED(olddent, &oldcvp);
    if (rold == NULL) {
        err = -ENOENT;
        goto out;
    }
    STACK_CHECK();
    VNODE_DGET(rold);                    /* hold inode, if any */
    if (!rold->d_inode) {
        err = -ENOENT;
        goto out;
    }
    STACK_CHECK();
    rnew = REALDENTRY_LOCKED(newdent, &newcvp);
    if (rnew == NULL) {
        err = -ENOENT;
        goto out;
    }
    /* don't need rnew's inode, so no VNODE_DGET  */

    STACK_CHECK();
    real_parent = CVN_TO_INO(tdvp);
    /* Now verify that this link doesn't cross file systems */
    if (real_parent->i_sb->s_dev != rold->d_inode->i_sb->s_dev) {
        err = -EXDEV;
        goto out;
    }
    shadow_inode = new_inode(parent->i_sb);
    STACK_CHECK();
    if (shadow_inode == NULL) {
        err = -ENFILE;
        goto out;
    }
    LOCK_INODE(real_parent);
    err = vfs_link(rold, real_parent, rnew);
    STACK_CHECK();
    UNLOCK_INODE(real_parent);
    /* Now this is ugly.  NFS will d_drop the dentry we
     * pass in to  force a new lookup.  So we will do the
     * same with our dentry and let our dentry get instantiated
     * on the next lookup.
     */
    if (!err) {
        if (rnew->d_inode == NULL || d_unhashed(rnew)) {
            d_drop(rnew);           /* XXX should be d_drop(newdent)? */
            STACK_CHECK();
            iput(shadow_inode);
        } else {
            /*
             * we had a negative dentry passed to us in "newdent";
             * hook it up to the shadow node.
             */
            vnlayer_shadow_inode(rnew->d_inode, newdent, shadow_inode);
            STACK_CHECK();
            MDKI_SET_DOPS(newdent, &vnode_shadow_dentry_ops);
            VNODE_D_INSTANTIATE(newdent, shadow_inode);
        }
    } else {
        iput(shadow_inode);
        STACK_CHECK();
    }
    STACK_CHECK();
  out:
    if (rold) {
        VNODE_DPUT(rold);
        REALDENTRY_UNLOCK(olddent, oldcvp);
    }
    if (rnew) {
        REALDENTRY_UNLOCK(newdent, newcvp);
    }
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
#else /* !HAVE_SHADOW_FILES */
    return EROFS;
#endif /* HAVE_SHADOW_FILES */
}

extern int
mvop_linux_link(
    VNODE_T *tdvp, /* use this to find the real parent */
    VNODE_T *vp, /* ignore it, use ctx instead */
    char *tnm,
    CALL_DATA_T *cd,
    link_ctx *context
)
{
    DENT_T *olddent, *newdent;
    INODE_T *parent;
    struct link_ctx *ctx = context;

    olddent = ctx->olddent;
    newdent = ctx->newdent;
    parent = ctx->parent;
    ctx->done = TRUE;                   /* tell high layer not to compensate */
    /* Stack checking is done in vnlayer_do_linux_link */
    return(vnlayer_do_linux_link(tdvp, olddent, parent, newdent));
}

extern int
mvop_linux_remove(
    VNODE_T *dvp,
    VNODE_T *vp,                        /* ignore, use dentry */
    char *nm,                           /* ignore, use dentry */
    CALL_DATA_T *cd,
    unlink_ctx *ctx
)
{
    INODE_T *rdir;
    DENT_T *parent, *dent, *rdent;
    VNODE_T *cvp;
    struct unlink_ctx *uctx = ctx;
    int err = 0;
    INODE_T *rinode = NULL;
    STACK_CHECK_DECL()

    uctx->done = TRUE;
    dent = uctx->dentry;

    ASSERT(MDKI_INOISCLRVN(VTOI(dvp)));

    parent = CVN_TO_DENT(dvp);
    ASSERT(parent);
    rdir = parent->d_inode;

    ASSERT(rdir);
    /*
     * All objects we unlink have dentries with a REALDENTRY
     * attached.  Use the parent dentry from the CVN and the
     * realdentry from the object, and pass them to vfs_unlink().
     */
    rdent = REALDENTRY_LOCKED(dent, &cvp);
    STACK_CHECK();
    /* We might be in a race here, in which case shadow_d_revalidate
     * may already have broken our connection with the real dentry.
     * In that case, just skip the call to vfs_unlink.
     */
    /* You might ask why we don't clear the realdentry field
     * and dput the real dentry before we call vfs_unlink.  It
     * turns out that this won't work.  In the case of an unlink
     * of an open file, the extra count caused by the real dentry
     * mimics the bump in the count caused by the lookup if the
     * file were not shadowed.  Thus, if the file is open, the
     * count on the dentry will be 2 which will keep it from getting
     * dropped by the unlink.  Otherwise, we drop it here and
     * panic when we close the file because the file structure
     * contains a pointer to a negative dentry.
     */

    /* WARNING: this vfs_unlink may release rdent->d_inode.
     * We do not want to bump the ref count on rdent, because that will
     * yield NFS silly-renames when the file is not really in use.
     */
    LOCK_INODE(rdir);
    /* For those filetypes which we don't shadow, we will need to
     * release the inode semaphore before we call vfs_unlink.  Since
     * both our dentry and the real dentry point to the same inode,
     * if we don't do this we create a deadlock, since the vfs_unlink
     * that called us originally already locked the inode.  This should
     * be ok because creates and deletes already hold the locks on 
     * the parents as well.  The only exception is link() which should
     * still be ok because it will still be properly serialized.
     */
    switch (rdent->d_inode->i_mode & S_IFMT) {
        case S_IFCHR:
        case S_IFBLK:
        case S_IFIFO:
        case S_IFSOCK:
            ASSERT(rdent->d_inode == dent->d_inode);
            /* Save the inode before vfs_unlink d_deletes the dentry */
	    rinode = rdent->d_inode;
	    UNLOCK_INODE(rinode);
	    break;
	  default:
	    break;
    }
    err = MDKI_VFS_UNLINK(rdir, rdent, CVN_TO_VFSMNT(cvp));
    if (rinode) {
        LOCK_INODE(rinode);
    }
    STACK_CHECK();
    UNLOCK_INODE(rdir);
    REALDENTRY_UNLOCK(dent, cvp);
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
}

extern int
mvop_linux_symlink(
    VNODE_T *dvp,
    char *linkname,
    VATTR_T *vap,
    char *targname,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    symlink_ctx *context
)
{
    /* Create shadow files */
    DENT_T *real_dentry;
    INODE_T *real_parent;
    INODE_T *shadow_inode;
    DENT_T *new;
    INODE_T *parent;
    VNODE_T *cvp;
    int err;
    struct symlink_ctx *ctx = context;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(dvp)));

    new = ctx->new;
    parent = ctx->parent;

    ASSERT(new);
    ASSERT(REALCVN(new));
    ASSERT(REALVFSMNT(new));

    /*
     * whether we do it ourselves or the vfs_symlink() callee does
     * it (leaving real_dentry unused), we don't want to
     * instantiate the new entry again.
     */
    ctx->done = TRUE;

    real_parent = CVN_TO_INO(dvp);
    shadow_inode = new_inode(parent->i_sb);
    STACK_CHECK();
    if (shadow_inode == NULL) {
        return ENFILE;
    }
    LOCK_INODE(real_parent);

    real_dentry = REALDENTRY_LOCKED(new, &cvp);
    if (!real_dentry || d_unhashed(real_dentry)) {
        /* Someone did in the real dentry on us.  Time to go. */
        UNLOCK_INODE(real_parent);
        iput(shadow_inode);
        STACK_CHECK();
        if (real_dentry)
            REALDENTRY_UNLOCK(new, cvp);
        return ENOENT;
    }
    VNODE_DGET(real_dentry);             /* hold inode, if any */
    /* It shouldn't happen, but maybe someone created the file
     * while we were waiting.
     */
    if (real_dentry->d_inode) {
        UNLOCK_INODE(real_parent);
        iput(shadow_inode);
        STACK_CHECK();
        VNODE_DPUT(real_dentry);
        REALDENTRY_UNLOCK(new, cvp);
        return EEXIST;
    }
    STACK_CHECK();

    err = MDKI_VFS_SYMLINK(real_parent, real_dentry, CVN_TO_VFSMNT(dvp), targname,
                      ctx->mode);
    STACK_CHECK();
    UNLOCK_INODE(real_parent);
    if (!err) {
        /* see comments below about NFS and mkdir */
        if ((real_dentry->d_inode == NULL) || d_unhashed(real_dentry)) {
            /* XXX Should we do anything different if it's hashed but
             * has a NULL inode (negative dentry)?
             */
            VNODE_DPUT(real_dentry);
            REALDENTRY_UNLOCK(new, cvp);
            d_drop(new);
            STACK_CHECK();
            iput(shadow_inode);
            STACK_CHECK();
        } else {
            vnlayer_shadow_inode(real_dentry->d_inode, new, shadow_inode);
            STACK_CHECK();
            MDKI_SET_DOPS(new, &vnode_shadow_dentry_ops);
            VNODE_D_INSTANTIATE(new, shadow_inode);
            if (vpp)
                *vpp = VN_HOLD(ITOV(shadow_inode));
            VNODE_DPUT(real_dentry);
            REALDENTRY_UNLOCK(new, cvp);
        }
    } else {
        VNODE_DPUT(real_dentry);
        REALDENTRY_UNLOCK(new, cvp);
        iput(shadow_inode);
    }
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
}

/* This is very similar to mvop_linux_mknod(), so check there if you change
** something here.
*/
extern int
mvop_linux_mkdir(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    mkdir_ctx *ctx                           /* new dentry */
)
{
    DENT_T *new;
    INODE_T *real_parent;
    DENT_T *real_dentry;
    DENT_T *parent_dentry;
    struct mkdir_ctx *mkctx = ctx;
    int err = 0;
    VATTR_MODE_T mode;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(advp)));

    new = mkctx->dentry;

    parent_dentry = CVN_TO_DENT(advp);
    real_parent = parent_dentry->d_inode;

    STACK_CHECK();
    LOCK_INODE(real_parent);
    /* There would be a possibility of a deadlock here if we are in an
     * autofs4 filesystem.  That is because autofs4 may pend in revalidate
     * without releasing the parent inode lock.  If this proves to be a 
     * problem, we should be able to go with the pre 2.6.0 code.
     */
    real_dentry = lookup_one_len(new->d_name.name, parent_dentry,
                                 new->d_name.len);
    STACK_CHECK();
    if (IS_ERR(real_dentry)) {
        err = PTR_ERR(real_dentry);
	real_dentry = NULL;
        UNLOCK_INODE(real_parent); /* An error, so unlock. */
    }
    if (err) {
        /* The real_parent semaphore is not held in this case. */
        return vnlayer_errno_linux_to_unix(err);
    } else {
        /* Make sure path_walk or lookup_one_len don't return err == 0 
         * and NULL. 
         */
        ASSERT(real_dentry != NULL);
    }
    mode = VATTR_GET_MODE(vap);
#ifdef CONFIG_FS_POSIX_ACL
    if (!IS_POSIXACL(real_parent)) {
        mode &= ~mdki_get_ucmask();
    }
#endif
    err = MDKI_VFS_MKDIR(real_parent, real_dentry, CVN_TO_VFSMNT(advp), mode);
    STACK_CHECK();
    UNLOCK_INODE(real_parent);    /* We're done with this now. */
    if (!err) {
        /* some filesystems, e.g. NFS, discard the real_dentry
         * passed in--they leave the inode NULL, and d_drop()
         * the entry to make its hash chains empty.  If we
         * encounter one of those, we'll do the same to our
         * dentry--let our caller re-lookup if it needs anything.
         * (The theory is that callers should tolerate this behavior
         * because NFS does it.)
         */
        if (real_dentry->d_inode == NULL || d_unhashed(real_dentry)) {
            /* XXX Should we do anything different if it's hashed but
             * has a NULL inode (negative dentry)?
             */
            d_drop(new);
            /* We don't want someone else to find this one
             * accidentally as it's really not correct (looks like a
             * negative dentry).  However, that seems to be OK because
             * they'd only find it by lookup.  The top-level dentry
             * won't be hooked up to this entry--it'll get dropped
             * instead in vnode_iop_mkdir().  But a middle layer must
             * not hold onto this thing--it can control that by querying
             * mdki_cvn_is_stable() for FALSE.
             */
            mkctx->pleasedrop = TRUE;
        } else
            mkctx->pleasedrop = FALSE;
        STACK_CHECK();
        *vpp = CVN_CREATE(real_dentry, CVN_TO_VFSMNT(advp));
        STACK_CHECK();
        if (*vpp == NULL)
            err = -ENFILE;
        else if (mkctx->pleasedrop)
            /*
             * It's OK to add flag without lock since nobody else can
             * see this vnode yet.
             */
            (*vpp)->v_flag |= VDOOMED;
    }
    VNODE_DPUT(real_dentry);
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
}

extern int
mvop_linux_rmdir(
    VNODE_T *advp,
    char *nm,
    VNODE_T *cdir,
    CALL_DATA_T *cd,
    dent_ctx *ctx
)
{
    int err;
    INODE_T *real_parent;
    DENT_T *real_dentry;
    DENT_T *real_parent_dent;
    DENT_T *target;
    VNODE_T *cvp;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(advp)));

    target = (DENT_T *) ctx;

    real_parent_dent = VNODE_DGET(CVN_TO_DENT(advp));
    real_parent = real_parent_dent->d_inode;
    real_dentry = REALDENTRY_LOCKED(target, &cvp);
    VNODE_DGET(real_dentry);             /* hold inode, if any */
    if (!real_dentry || real_dentry->d_inode == NULL) {
        if (real_dentry) {
            VNODE_DPUT(real_dentry);
            REALDENTRY_UNLOCK(target, cvp);
        }
        VNODE_DPUT(real_parent_dent);
        return ENOENT;
    }
    if ((real_dentry->d_inode->i_op == NULL) ||
        (real_dentry->d_inode->i_op->rmdir == NULL))
    {
        VNODE_DPUT(real_dentry);
        REALDENTRY_UNLOCK(target, cvp);
        VNODE_DPUT(real_parent_dent);
        return EPERM;
    }
    /* We need to lock the real parent inode to mirror the locking
     * that happens on non-shadowed directories.
     */
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
    mutex_lock_nested(&real_parent->i_mutex, I_MUTEX_PARENT);
#else
    LOCK_INODE(real_parent);
#endif
    STACK_CHECK();
    err = MDKI_VFS_RMDIR(real_parent, real_dentry, CVN_TO_VFSMNT(advp));
    STACK_CHECK();
    UNLOCK_INODE(real_parent);

    VNODE_DPUT(real_dentry);
    REALDENTRY_UNLOCK(target, cvp);
    VNODE_DPUT(real_parent_dent);

    return vnlayer_errno_linux_to_unix(err);
}

extern int
mvop_linux_rename(
    VNODE_T *odvp,
    char *onm,
    VNODE_T *tdvp,
    char *tnm,
    CALL_DATA_T *cd,
    rename_ctx *context
)
{
    int err;
    INODE_T *real_odir;
    DENT_T *real_odent;
    INODE_T *real_ndir;
    DENT_T *real_ndent;
    DENT_T *odent, *ndent;
    VNODE_T *odentcvp, *ndentcvp;
    DENT_T *real_opdent;
    DENT_T *test;
    DENT_T *real_npdent;
    struct rename_ctx *ctx = context;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(odvp)));
    ASSERT(MDKI_INOISCLRVN(VTOI(tdvp)));
    ASSERT(ctx);
    odent = ctx->odentry;
    ndent = ctx->ndentry;

    /*
     * First, verify that targets are on the same file system.
     * (2.6 doesn't do this for us anymore)
     */
    if (CVN_TO_VFSMNT(odvp) != CVN_TO_VFSMNT(tdvp)) {
        return EXDEV;
    }
    real_opdent = CVN_TO_DENT(odvp);		/* old parent dentry */

    real_npdent = CVN_TO_DENT(tdvp);		/* new parent dentry */
    real_odir = CVN_TO_DENT(odvp)->d_inode;
    real_ndir = CVN_TO_DENT(tdvp)->d_inode;

    /* We need to get the parent locks and then verify that the dentries
     * are good.  This is done at the higher level by doing the final
     * lookups with the parents locked. If we don't have a target, we 
     * look it up now.  We verify the source under the lock before we 
     * call vfs_rename so that the locking semantics match and the files 
     * are known good.
     */
    test = lock_rename(real_npdent, real_opdent);
    real_odent = REALDENTRY_LOCKED(odent, &odentcvp);
    if (real_odent == NULL) {
        err = -ENOENT;
        goto out_unlock1;
    }
    /* We now lookup the real new dentry and check it against the one
     * pointed to by our loopback dentry.  If they don't match we 
     * replace the old one.  
     */
    real_ndent = lookup_one_len(ndent->d_name.name, real_npdent,
                                ndent->d_name.len);
    if (IS_ERR(real_ndent)) {
        err = PTR_ERR(real_ndent);
        real_ndent = NULL;
        REALDENTRY_UNLOCK(odent, odentcvp);
        goto out_unlock1;
    }
    /* See if it matches.  */
    if ((ndentcvp = REALCVN(ndent)) == NULL) {
        /* No CVP, create one and link up the dentries */
        ndentcvp = CVN_CREATE(real_ndent, CVN_TO_VFSMNT(tdvp));
        SET_REALCVN(ndent, ndentcvp);
        REALDENTRY_LOCKED(ndent, &ndentcvp);
    } else {
        REALDENTRY_LOCKED(ndent, &ndentcvp);
        /* Replace the real dentry if it has changed*/ 
        if ((DENT_T *)ndentcvp->v_dent != real_ndent) {
            VNODE_DPUT((DENT_T *)ndentcvp->v_dent);
            ndentcvp->v_dent = (void *)VNODE_DGET(real_ndent);
        }
    }
    /* Lock rename returns the child of a common parent if there is one.  The
     * check below is to prevent either file from being a parent of the other.
     * See namei.c:do_lookup for details.
     */
    if ((test == real_ndent) || (test == real_odent)) {
        err = -EINVAL;
        goto out_unlock;
    }
    ASSERT(REALVFSMNT(ndent));

    /* If the source dentries have gone bad on us, return an error. */
    if ((real_odent->d_inode == NULL) || d_unhashed(real_odent)) {
        err = -ENOENT;
        goto out_unlock;
    }
    STACK_CHECK();
    err = MDKI_VFS_RENAME(real_odir, real_odent, CVN_TO_VFSMNT(odvp),
                     real_ndir, real_ndent, CVN_TO_VFSMNT(tdvp));
    STACK_CHECK();
out_unlock:
    /* Drop the count from the lookup */
    VNODE_DPUT(real_ndent);
    REALDENTRY_UNLOCK(odent, odentcvp);
    REALDENTRY_UNLOCK(ndent, ndentcvp);
out_unlock1:
    unlock_rename(real_npdent, real_opdent);
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
}

STATIC loff_t
vnlayer_internal_llseek(
    FILE_T *file_p,
    loff_t offset,
    int origin
)
{
    loff_t err;
    FILE_T *real_file;
    STACK_CHECK_DECL()

    real_file = REALFILE(file_p);

    if (real_file->f_op && real_file->f_op->llseek) {
        err = real_file->f_op->llseek(real_file, offset, origin);
    } else {
        err = default_llseek(real_file, offset, origin);
    }
    STACK_CHECK();
    file_p->f_pos = real_file->f_pos;
    file_p->f_ra = real_file->f_ra; /* Copy struct file_ra_state */
    file_p->f_version = real_file->f_version;

    return(err);
}

extern int
mvop_linux_seek(
    VNODE_T *vp,
    MOFFSET_T ooff,
    MOFFSET_T *noffp,
    seek_ctx *ctx
)
{
    struct seek_ctx *sctx = ctx;

    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));

    *noffp = vnlayer_internal_llseek(sctx->filep, sctx->offset, sctx->origin);
    sctx->done = TRUE;
    if (*noffp < 0)
        return vnlayer_errno_linux_to_unix(*noffp);
    else
        return 0;
}

extern int
mvop_linux_fsync(
    VNODE_T *vp,
    int datasync,
    CALL_DATA_T *cd,
    fsync_ctx *ctx
)
{
    struct file *fp;
    struct file *realfp;
    INODE_T *ip;
    int err;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));

    ASSERT(datasync == FLAG_NODATASYNC || datasync == FLAG_DATASYNC);

    ASSERT(ctx != NULL);

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,35)
    fp = ctx->file_p;
#else
    fp = (struct file *) ctx;
#endif

    ip = fp->f_dentry->d_inode;
    ASSERT_I_SEM_MINE(ip);

    realfp = REALFILE(fp);
    ASSERT(realfp);
    if (realfp->f_op && realfp->f_op->fsync) {
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,35)
        /*
        ** no need to lock, more than that, ext3 acquires
        ** the inode's lock in its fsync callback
        */
        err = realfp->f_op->fsync(realfp,
# if !defined (MRG)
                                  ctx->start,
                                  ctx->end,
# endif
                                  datasync == FLAG_DATASYNC ? 1 : 0);
#else
        /* XXX locking hierarchy?  our semaphore is held ... */
        LOCK_INODE(realfp->f_dentry->d_inode);
        err = realfp->f_op->fsync(realfp, realfp->f_dentry,
                                     datasync == FLAG_DATASYNC ? 1 : 0);
        UNLOCK_INODE(realfp->f_dentry->d_inode);
#endif
    } else
        err = 0;
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
}

/*
 * internal fsync doesn't have a realfp, so we have to do the same stuff as
 * mvop_linux_fsync()
 */
extern int
mvop_linux_fsync_kernel(
    VNODE_T *vp,
    int flag,
    CRED_T *cred,
    void *filp
)
{
    INODE_T *ip;
    int err;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,35)
    struct fsync_ctx *ctx = (struct fsync_ctx *) filp;
#endif
    struct file *fp;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));

    ASSERT(flag == FLAG_NODATASYNC || flag == FLAG_DATASYNC);

    ASSERT(filp != NULL);

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,35)
    fp = ctx->file_p;
#else
    fp = (struct file *) filp;
#endif

    if (fp->f_op && fp->f_op->fsync) {
        ip = fp->f_dentry->d_inode;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,35)
        err = fp->f_op->fsync(fp,
# if !defined (MRG)
                              ctx->start,
                              ctx->end,
# endif 
                              flag == FLAG_DATASYNC ? 1 : 0);
#else
        LOCK_INODE(ip);
        err = fp->f_op->fsync(fp, fp->f_dentry,
                                 flag == FLAG_DATASYNC ? 1 : 0);
        UNLOCK_INODE(ip);
#endif
    } else
        err = 0;
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
}

#if defined(RATL_SUSE) && RATL_VENDOR_VER == 900
extern int
mvop_linux_lockctl(
    VNODE_T *vp,
    void *ld,
    int cmd,
    CALL_DATA_T *cd,
    file_ctx *ctx
)
{
    /* We don't support locking on SLES9 because it doesn't provide the 
     * hooks we need. 
     */
    return EINVAL;
}
#else /* not SLES9 */
extern int
mvop_linux_lockctl(
    VNODE_T *vp,
    void *ld,
    int cmd,
    CALL_DATA_T *cd,
    file_ctx *ctx
)
{
    struct file_lock *lock_p = ld;
    FILE_T *fp = (FILE_T *) ctx;
    FILE_T *realfp, *origfp;
    struct file_lock *cfl = NULL;
    int err = 0;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));

    if (vp->v_type != VREG)
        return EINVAL;

    /* The cleartext locks are the "master" since we need to handle sharing
    ** across machines.  We also maintain the locks on our file so that the VFS
    ** layer will call us at the right times (see the explanation in
    ** mvop_linux_close() above).  This is similar to the way NFS maintains
    ** locks on the client side and the server side.  So, to do the "get lock"
    ** operations we will use the cleartext locks.  For the "set lock"
    ** operations we will use both by locking the cleartext lock and then our
    ** lock, and unlocking in the reverse order.  We'll always use the status
    ** from the cleartext lock as the return value
    **
    ** We used to reuse the lock_p passed in to do locking on both the MVFS
    ** file and the cleartext.  We discovered that the NFS code would add 
    ** callbacks for nlmclnt operations to the lock structure and that they
    ** remained in place for subsequent MVFS operations.  There is no sign
    ** that this caused any problems but we changed the SETLK code to allocate
    ** and initialize a separate lock structure to prevent any possible 
    ** problems.
    **
    ** NB: the Linux kernel code does not utilize reference counts on the file 
    ** pointers in the lock structure.  If they did we
    ** would need F_COUNT_INC(realfp) and F_COUNT_DEC(realfp) calls around the
    ** places where we "switch" the lock_p->fl_file fields between our fp and
    ** the realfp below.  Note that these lock structures are copied one more
    ** time before they are queued up, so we don't need to worry about them
    ** being freed before the locks are released.
    */
    realfp = REALFILE(fp);
    /* This check is a proxy for a check of the mnode class done in 
     * mvfs_lockctl_ctx which we no longer call.
     */
    if (realfp == NULL)
        return ENXIO;

    origfp = lock_p->fl_file;
    ASSERT(origfp == fp);

    /* The following code is broadly similar to fcntl_getlk() and fcntl_setlk()
    ** since we're passing the locking calls on to the underlying file system.
    */
    switch (cmd) {
      case F_GETLK: 
#if BITS_PER_LONG == 32
      case F_GETLK64: 
#endif
        /* Get operation uses the cleartext locks. */
        lock_p->fl_file = realfp;

        if (realfp->f_op && realfp->f_op->lock) {
            err = (*realfp->f_op->lock)(realfp, cmd, lock_p);
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
            if (lock_p->fl_ops && lock_p->fl_ops->fl_release_private) {
                (*lock_p->fl_ops->fl_release_private)(lock_p);
            }
#endif
            if (err < 0) {
                break;
            }
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,11)
            else if (err == LOCK_USE_CLNT) {
                /* Bypass for NFS with no locking - 2.0.36 compat */
                cfl = posix_test_lock(realfp, lock_p);
            }
#endif
            else {
                cfl = (lock_p->fl_type == F_UNLCK ? NULL : lock_p);
            }
        } 
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) && !defined(SLES10SP2)
        else {
            cfl = posix_test_lock(realfp, lock_p);
        }
        /* This is set correctly in the non-error case, but we need to set it
        ** for the error cases, and it's OK to repeat the setting.
        */
        if (cfl == NULL) {
            lock_p->fl_type = F_UNLCK;
        } else {
            /* Copy the cleartext lock info into our lock so fcntl_getlk()
            ** in the Linux vfs layer can copy them into the user's
            ** struct flock.  Don't bother if we don't need to.
            */
            if (cfl != lock_p) {
                memcpy(lock_p, cfl, sizeof(*lock_p));
            }
        }
#elif LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24) || defined(SLES10SP2)
        /* SLES 10.2 does the copying inside of posix_test_lock */
        else {
            posix_test_lock(realfp, lock_p);
        }
#else /* LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) */
        else {
            /* posix_test_lock copies the content of the file_lock it finds
             * when walking over the i_lock list instead of returning the
             * pointer.  The former posix_test_lock used to return the actual
             * pointer found in the file inode i_lock list.  So, now we have to
             * pass a struct file_lock * to receive file_lock data.  As stack
             * is a scarce resource, we prefer to use KMEM_ALLOC.
             */
            cfl = KMEM_ALLOC(sizeof(*cfl), KM_SLEEP);
            if (cfl) {
                /* posix_test_lock return looks like a boolean */
                if (posix_test_lock(realfp, lock_p, cfl) != 0) {
                    /* Copy the cleartext lock info into our lock so fcntl_getlk()
                    ** in the Linux vfs layer can copy them into the user's
                    ** struct flock.  Don't bother if we don't need to.
                    */
                    memcpy(lock_p, cfl, sizeof(*lock_p));
                } else {
                    lock_p->fl_type = F_UNLCK;
                }
                KMEM_FREE(cfl, sizeof(*cfl));
            } else {
                err = -ENOMEM;
            }
        }
#endif /* else LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) */
        break;

      case F_SETLK:
      case F_SETLKW:
#if BITS_PER_LONG == 32
      case F_SETLK64:
      case F_SETLKW64:
#endif
        /* use a copy of the original lock_p so that we don't
         * get nlmclnt callbacks added to our lock structure. 
         */
        cfl = KMEM_ALLOC(sizeof(*cfl), KM_SLEEP);
        if (cfl == NULL) {
            err = -ENOMEM;
            break;
        }
        locks_init_lock(cfl);
        locks_copy_lock(cfl, lock_p);
        cfl->fl_file = realfp;

        if (lock_p->fl_type == F_UNLCK) {
            /* Unlock our file first. */
            err = POSIX_LOCK_FILE(fp, lock_p, cmd);
            if (err != 0) {
                MDKI_VFS_LOG(VFS_LOG_WARN, "%s: err=%d unlocking mvfs fp=%p\n",
                             __func__, -err, fp);
            }
            /* Unlock the cleartext file next.  We ignore any error above. */
            if (realfp->f_op && realfp->f_op->lock) {
                err = realfp->f_op->lock(realfp, cmd, cfl);
            } else {
                err = POSIX_LOCK_FILE(realfp, cfl, cmd);
            }
        } else {
            /* we don't want it going away while we are in the middle of something */
            get_file(realfp);
            /* This is the F_RDLCK or F_WRLCK case, go in the reverse order. */
            if (realfp->f_op && realfp->f_op->lock) {
                err = realfp->f_op->lock(realfp, cmd, cfl);
            } else {
                err = POSIX_LOCK_FILE(realfp, lock_p, cmd);
            }
            fput(realfp);

            if (err == 0) {
                err = POSIX_LOCK_FILE(fp, lock_p, cmd);
                if (err != 0) {
                    MDKI_VFS_LOG(VFS_LOG_WARN,
                                 "%s: err=%d locking mvfs fp=%p\n",
                                 __func__, -err, fp);
                }
                /* Return the cleartext lock results (we're only in here if
                ** err == 0 from the cleartext lock call).
                */
                err = 0;
            }
        }
        KMEM_FREE(cfl, sizeof(*cfl));
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
        /* This is the setlock case so we need to save the owner of this lock
        ** because at exit time __exit_files() is going to set current->files =
        ** NULL (which is the owner), and therefore, our "release" code in
        ** mvop_linux_close() won't be able to find the owner to pass on to
        ** filp_close() unless we save it somewhere so it can be found then.
        */
        if (current->files != 0 && lock_p->fl_owner != current->files) {
            MDKI_VFS_LOG(VFS_LOG_DEBUG,
                         "assert lk: err=%d fp=%p rfp=%p ltyp=%s "
                         "lo=%p po=%p lpid=%d pid=%d tgid=%d\n",
                         -err, fp, realfp, lock_p->fl_type == F_UNLCK ? "U" : "L",
                         lock_p->fl_owner, current->files, lock_p->fl_pid,
                         current->pid, current->tgid);
        }
        mvfs_linux_save_fl_owner(lock_p->fl_owner);
#endif
        break;

      default:
	err = -EINVAL;
    }
    STACK_CHECK();

    return vnlayer_errno_linux_to_unix(err);
}
#endif /* else !(defined(RATL_SUSE) && RATL_VENDOR_VER == 900) */

extern int
mvop_linux_readdir(
    VNODE_T *advp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    int *eofp,
    readdir_ctx *ctx
)
{
    /* formerly mvfs_do_readdir */
    struct readdir_ctx *rctx = ctx;
    FILE_T *file_p, *realfp;
    int err;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(advp)));

    rctx->done = TRUE;
    file_p = rctx->file;

    /* Use the real file pointer for the READDIR */

    realfp = REALFILE(file_p);
    ASSERT(realfp);
    ASSERT(realfp->f_dentry);
    if (realfp->f_op && realfp->f_op->readdir) {
        COPY_FILE_STRUCT(file_p, realfp);
        STACK_CHECK();
        err = (*realfp->f_op->readdir)(realfp, uiop->uio_buff,
                                       uiop->uio_func);
        if (err > 0) {
            /*
             * lame procfs readdir function doesn't return 0 in
             * success cases.  Compensate for it: treat only negative
             * values as errors, squash everything else to zero.
             */
            err = 0;
        }
        STACK_CHECK();
        COPY_FILE_STRUCT(realfp, file_p);
    } else
        err = -EOPNOTSUPP;
    STACK_CHECK();
    return vnlayer_errno_linux_to_unix(err);
}

extern int
mvop_linux_mmap(
    VNODE_T *vp,
    u_int sharing,
    u_int rwx,
    CALL_DATA_T *cd,
    mmap_ctx *ctx
)
{
    int err;
    int dump_fp;
    FILE_T *fp, *realfp;
    struct vm_area_struct *mem_p;
    struct mmap_ctx *mctx = ctx;
    STACK_CHECK_DECL()

    ASSERT(MDKI_INOISCLRVN(VTOI(vp)));

    fp = mctx->file;
    mem_p = mctx->mem;

    realfp = REALFILE(fp);
    ASSERT(realfp);

    /*
     * Pass the buck to the cleartext.  Beware of adjusting counts
     * appropriately for the upper layer (need to fake it out)
     */

    if (mem_p->vm_file != NULL) {
        ASSERT(mem_p->vm_file == fp);
        dump_fp = 1;
    } else
        dump_fp = 0;

    F_COUNT_INC(realfp);
    mem_p->vm_file = realfp;

    /* simulate VM_DENYWRITE adjustments from linux kernel's mmap.c */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,5)
#define WRITECOUNT_LOCK(i) spin_lock(&(i)->i_lock)
#define WRITECOUNT_UNLOCK(i) spin_unlock(&(i)->i_lock)
#else
    /* FIXME:  not locking-safe with arbitration_lock, which is not
     * exported.  Nor is deny_write_access exported, sigh.
     */
    /* FIXME: this is used in the "common case" of mapping a file
     * for execute permissions.  Sigh.
     */
#define WRITECOUNT_LOCK(i)
#define WRITECOUNT_UNLOCK(i)
#endif
    if ((mem_p->vm_flags & VM_DENYWRITE) != 0) {
        WRITECOUNT_LOCK(realfp->f_dentry->d_inode);
        if (I_WRITECOUNT(realfp->f_dentry->d_inode) > 0) {
            WRITECOUNT_UNLOCK(realfp->f_dentry->d_inode);
            /* restore file state for error returns */
            if (dump_fp)
                mem_p->vm_file = fp;
            else
                mem_p->vm_file = NULL;
            F_COUNT_DEC(realfp);
            return ETXTBSY;
        }
        I_WRITECOUNT_DEC(realfp->f_dentry->d_inode);
        WRITECOUNT_UNLOCK(realfp->f_dentry->d_inode);
    }

    STACK_CHECK();
    if (realfp->f_op && realfp->f_op->mmap)
        err = (*realfp->f_op->mmap)(realfp, mem_p);
    else
        err = -EIO
    STACK_CHECK();

    /*
     * un-do the VM_DENYWRITE adjustment on the lower level file here;
     * higher-level mmap code will un-do it later (in either the error
     * or non-error cases) for the original file pointer (MVFS node)
     */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,5)
    /*
     * (XXX FIXME locking problems: should hold it longer, beyond
     * the insert_vm_struct() call in mmap.)
     */
#endif
    if (mem_p->vm_flags & VM_DENYWRITE)
        I_WRITECOUNT_INC(realfp->f_dentry->d_inode);

    if (err) {
        /* restore file state for error returns */
        if (dump_fp)
            mem_p->vm_file = fp;
        else
            mem_p->vm_file = NULL;
        F_COUNT_DEC(realfp);
    } else if (dump_fp)
        fput(fp);              /* done with it for now */
    return vnlayer_errno_linux_to_unix(err);
}

extern int
mvop_linux_cvpcmp(
    VNODE_T *cvp1,
    VNODE_T *cvp2
)
{
    return CVN_CMP(cvp1, cvp2);
}

extern void
mvop_linux_print(
    VNODE_T *vp
)
{
    INODE_T *ip = VTOI(vp);

    ASSERT(MDKI_INOISCLRVN(ip));
    printk("clrvn: ip=%p vp=%p vfsp=%p sb=%p cnt=%d\n"
           "dent=%p vfsmnt=%p\n",
           ip, vp, vp->v_vfsp, VFSTOSB(vp->v_vfsp), V_COUNT(vp),
           vp->v_dent, vp->v_vfsmnt);
    if (vp->v_dent)
        printk("dent->d_inode=%p dent->d_name=%s\n",
               CVN_TO_DENT(vp)->d_inode, CVN_TO_DENT(vp)->d_name.name);
}

/* The calls marked direct call in the dispatch table below are used but
 * are not dispatched through this table because we need a different 
 * call sequence than provided by the default VOP interface.
 */
struct vnodeops mvop_cltxt_vnops = {
    .vop_open =         &mvop_linux_open,
    .vop_close =        (vop_close_fn_t)&vnlayer_bogus_vnop, /* direct call */
    .vop_rdwr =         &mvop_linux_rdwr,
    .vop_ioctl =        &mvop_linux_ioctl,
    .vop_getattr =      (vop_getattr_fn_t)&vnlayer_bogus_vnop, /*direct call*/
    .vop_setattr =      &mvop_linux_setattr,
    .vop_access =       &mvop_linux_access,
    .vop_lookup =       (vop_lookup_fn_t)&vnlayer_bogus_vnop, /* not used */
    .vop_create =       &mvop_linux_create,
    .vop_remove =       &mvop_linux_remove,
    .vop_link =         &mvop_linux_link,
    .vop_rename =       &mvop_linux_rename,
    .vop_mkdir =        &mvop_linux_mkdir,
    .vop_rmdir =        &mvop_linux_rmdir,
    .vop_readdir =      &mvop_linux_readdir,
    .vop_symlink =      &mvop_linux_symlink,
    .vop_readlink =     (vop_readlink_fn_t)&vnlayer_bogus_vnop, /* not used */
    .vop_fsync =        &mvop_linux_fsync,
    .vop_inactive =     (vop_inactive_fn_t)&vnlayer_bogus_vnop, /* not used */
    .vop_fid =          (vop_fid_fn_t)&vnlayer_bogus_vnop, /* not used */
    .vop_realvp =       &mvop_linux_realcvp,
    .vop_cmp =          &mvop_linux_cvpcmp,
    .vop_mmap =         &mvop_linux_mmap,
    .vop_seek =         &mvop_linux_seek,
    .vop_lockctl =      (vop_lockctl_fn_t)&vnlayer_bogus_vnop, /* not used */
    .vop_print =        &mvop_linux_print
};

extern int
vnlayer_cltxt_vstatfs(
    VFS_T *vfsp,
    STATVFS_T *sbp
)
{
    NOT_IMPLEMENTED();
    return ENOSYS;
}

struct vfsops vnlayer_linux_cltxt_vfsop = {
    .vfs_root =	        (vfs_root_fn_t) &vnlayer_bogus_vnop,
    .vfs_statvfs =      &vnlayer_cltxt_vstatfs,
    .vfs_sync =         (vfs_sync_fn_t) &vnlayer_bogus_vnop,
    .vfs_mount =        (vfs_mount_fn_t) &vnlayer_bogus_vnop,
    .vfs_unmount =      (vfs_unmount_fn_t) &vnlayer_bogus_vnop,
    .vfs_vget =         (vfs_vget_fn_t) &vnlayer_bogus_vnop,
    .vfs_init =         NULL,
    .vfs_log =          &vnlayer_linux_log
};

VFS_T vnlayer_cltxt_vfs = {
    .vfs_flag =   0,
    .vfs_fstype = VFS_NOT_MVFS,
    .vfs_op =     &vnlayer_linux_cltxt_vfsop
    /* initialize vfs_sb at runtime */
};
static const char vnode_verid_mvfs_linux_mvops_c[] = "$Id:  d1d26702.4af411e2.9c21.44:37:e6:71:2b:ed $";
