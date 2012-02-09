/*
 * Copyright (C) 2000, 2007 IBM Corporation.
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
 * In order to implement our recursion from /view/<viewtag> to /
 * under Linux, we have to create a separate shadow file hierarchy
 * for the dcache for each view in use.  There are special considerations
 * for the viewroot and viewtag files and for files within a vob.
 * For all other files, however, this can be done by creating dentries
 * inodes, and perhaps super blocks that serve no other purpose than
 * deflect their calls from the shadow files to the real versions.
 */

/* Translation macros */

#if defined(CONFIG_SMP) && defined(MVFS_DEBUG)
typedef void (*mdki_addr)(void);
struct vnlayer_cvn_debug_info {
    mdki_addr rv, caller, caller2;
    mvfs_process_t *holder;
    DENT_T *dent;
    int dcnt;
    VNODE_T *cvn;
    DENT_T *rdent;
    INODE_T *rinode;
    int rdcnt;
};
extern struct vnlayer_cvn_debug_info vnlayer_cvn_debug, vnlayer_cvn_old_debug[];

/* These need to be macros to capture the return addresses properly */
#define CVN_SPINLOCK() { spin_lock(&vnlayer_cvn_spinlock);                \
	vnlayer_cvn_debug.rv = (mdki_addr)mdki_getreturn();               \
	vnlayer_cvn_debug.caller = (mdki_addr)mdki_getmycaller();         \
	vnlayer_cvn_debug.caller2 = (mdki_addr)mdki_getmycallerscaller(); \
	vnlayer_cvn_debug.holder = mdki_curproc(); }
#define HISTCOUNT 256
#define CVN_SPINUNLOCK() {                                      \
        register int i;                                         \
        for (i = (HISTCOUNT-2); i >= 0; i--) {                  \
            vnlayer_cvn_old_debug[i+1] = vnlayer_cvn_old_debug[i];    \
        }                                                       \
        vnlayer_cvn_old_debug[0] = vnlayer_cvn_debug;                 \
        vnlayer_cvn_debug.rv = NULL;                               \
	vnlayer_cvn_debug.caller = NULL;                           \
	vnlayer_cvn_debug.caller2 = NULL;                          \
	vnlayer_cvn_debug.holder = NULL;                           \
	vnlayer_cvn_debug.dent = NULL;                             \
	vnlayer_cvn_debug.dcnt = 0;                                \
	vnlayer_cvn_debug.rdent = NULL;                            \
	vnlayer_cvn_debug.rinode = NULL;                           \
	vnlayer_cvn_debug.rdcnt = 0;                               \
	vnlayer_cvn_debug.cvn = NULL;                              \
	spin_unlock(&vnlayer_cvn_spinlock); }
#else /* Not SMP && MVFS_DEBUG */
#define CVN_SPINLOCK() spin_lock(&vnlayer_cvn_spinlock)
#define CVN_SPINUNLOCK() spin_unlock(&vnlayer_cvn_spinlock)
#endif /* SMP && MVFS_DEBUG */

extern spinlock_t vnlayer_cvn_spinlock;

#define REALCVN(dentryp) ((VNODE_T *)(dentryp)->d_fsdata)
/* Newer compilers deprecate the "use of cast expressions as lvalues", so use a
** macro to set one of these.
*/
#define SET_REALCVN(dentryp, value) do {(dentryp)->d_fsdata = (void *)(value);} while(0)
#define REALDENTRY(dentryp) (CVN_TO_DENT(REALCVN(dentryp)))

static inline void
vnlayer_dent_cvn_rele(
    DENT_T *dp,
    mdki_boolean_t detach
)
{
    VNODE_T *cvp;

    CVN_SPINLOCK();

#if defined(MVFS_DEBUG) && defined(CONFIG_SMP)
    vnlayer_cvn_debug.dent = dp;
    vnlayer_cvn_debug.dcnt = D_COUNT(dp);
#endif

    cvp = REALCVN(dp);
    if (cvp) {
#if defined(MVFS_DEBUG) && defined(CONFIG_SMP)
        vnlayer_cvn_debug.cvn = cvp;
        vnlayer_cvn_debug.rdent = (DENT_T *)cvp->v_dent;
        if (vnlayer_cvn_debug.rdent)
            vnlayer_cvn_debug.rinode = vnlayer_cvn_debug.rdent->d_inode;
        vnlayer_cvn_debug.rdcnt = D_COUNT((DENT_T *)cvp->v_dent);
#endif
        /*
         * this ASSERT is not true for unwrapped items (see
         * mvop_linux_lookupvp())
         */
        /* ASSERT(MDKI_INOISCLRVN(VTOI(cvp))); */
        if (detach) {
            SET_REALCVN(dp, NULL);
        }
        CVN_SPINUNLOCK();
        /*
         * Now drop the reference previously held by REALCVN(dp).
         * We have to release outside the lock since VN_RELE can call
         * into a file system which might have to do some work which
         * could block, and we don't want to do that since
         * vnlayer_cvn_spinlock is a spinlock.
         */
        if (detach)
            VN_RELE(cvp);
    } else {
        CVN_SPINUNLOCK();
    }
}

#define DENT_VN_RELE vnlayer_dent_cvn_rele

static inline DENT_T *
vnlayer_get_realdentry_locked(
    DENT_T *dentryp,
    VNODE_T **cvpp
)
{
    DENT_T *rdent;

    /* NOTE: shadow_d_revalidate does the same locking but can't call
     * us because it needs to hold the vnlayer_cvn_spinlock.  So if this
     * changes, look there as well.
     */
    /* First bump the count on the dentry to keep it around while
     * we are using its realdentry ptr.
     */
    VNODE_DGET(dentryp);
    /* Now get the global cvn lock */
    /* FIXME: Is this needed now that we are covered by the DGET above.
     * I don't think that it is if we only clear the cvn pointer when
     * the dcount goes to 0.  But it is easier to take out later than
     * to put in later.
     */
    CVN_SPINLOCK();
    if (REALCVN(dentryp)) {
        rdent = REALDENTRY(dentryp);
        if (rdent) {
            *cvpp = REALCVN(dentryp);
            /*
             * NB: this does not hold the realdentry like it used to in
             * previous releases
             */
            VN_HOLD(*cvpp);
#if defined(MVFS_DEBUG) && defined(CONFIG_SMP)
            vnlayer_cvn_debug.dent = dentryp;
            vnlayer_cvn_debug.dcnt = D_COUNT(dentryp);
            vnlayer_cvn_debug.cvn = *cvpp;
            vnlayer_cvn_debug.rdent = rdent;
            vnlayer_cvn_debug.rinode = rdent ? rdent->d_inode : NULL;
            vnlayer_cvn_debug.rdcnt = D_COUNT(rdent);
#endif
        } else {
            /* when we don't give our caller the realdentry, it won't
             * clean up cvpp, so we mustn't provide a hold count
             * nor a value.
             */
            *cvpp = NULL;
        }
    } else {
        *cvpp = NULL;
        rdent = NULL;
    }
    CVN_SPINUNLOCK();
    if (rdent == NULL) {
        /*
         * we're not using a realdentry, so release our hold on
         * original dentry
         */
        VNODE_DPUT(dentryp);
    }
    return rdent;
}

static inline void
vnlayer_realdentry_unlock(
    DENT_T *dentryp,
    VNODE_T *cvp,
    mdki_boolean_t detach
)
{
    ASSERT(cvp != NULL);
    ASSERT(dentryp != NULL);
    DENT_VN_RELE(dentryp, detach);
    /* release counts gathered in vnlayer_get_realdentry_locked */
    VN_RELE(cvp);
    VNODE_DPUT(dentryp);
    return;
}

/* The following three macros are to be used when accessing the
 * REALDENTRY from a clrvnode structure pointed to by a shadow
 * dentry.  This will keep the dentry counts consistent.  It
 * should not be used when accessing a clrvnode structure from
 * an MVFS mnode.  Every REALDENTRY_LOCKED call should have a
 * corresponding REALDENTRY_UNLOCK or REALDENTRY_UNLOCK_DETACH call.
 */

#define REALDENTRY_LOCKED(dentryp,cvpp) vnlayer_get_realdentry_locked(dentryp, cvpp)
#define REALDENTRY_UNLOCK(dentryp,cvp) vnlayer_realdentry_unlock(dentryp,cvp,FALSE)
#define REALDENTRY_UNLOCK_DETACH(dentryp,cvp) vnlayer_realdentry_unlock(dentryp,cvp,TRUE)

#define REALVFSMNT(dentryp) (CVN_TO_VFSMNT(REALCVN(dentryp))) /* not used on 2.2.x */

/* Utility functions */

#define SHADOW_CP_INODAT(src, dest) vnode_shadow_cp_inodat(src, dest)
static inline void
vnode_shadow_cp_inodat(
    INODE_T *src,
    INODE_T *dest
)
{
    /* The inode and i_dev numbers should  */
    /* match so that lstat and fstat match */
    (dest)->i_ino = (src)->i_ino;
    /* rdev is only valid for devices, which we do */  
    /* not shadow, but they will match.    */
    (dest)->i_rdev = (src)->i_rdev;

    (dest)->i_mode = (src)->i_mode;
    (dest)->i_nlink = (src)->i_nlink;
    (dest)->i_uid = (src)->i_uid;
    (dest)->i_gid = (src)->i_gid;
    WRITE_I_SIZE(dest, READ_I_SIZE(src));
    (dest)->i_atime = (src)->i_atime;
    (dest)->i_mtime = (src)->i_mtime;
    (dest)->i_ctime = (src)->i_ctime;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
    (dest)->i_blksize = (src)->i_blksize;
#else
    (dest)->i_blkbits = (src)->i_blkbits;
#endif
    (dest)->i_blocks = (src)->i_blocks;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
    (dest)->i_attr_flags = (src)->i_attr_flags;
    (dest)->i_dev = (src)->i_dev;
#endif
}

#define SHADOW_CP_ATIME(src, dest) (dest)->i_atime = (src)->i_atime
#define SHADOW_CP_CTIME(src, dest) (dest)->i_ctime = (src)->i_ctime

#define COPY_FILE_STRUCT(src,dest) vnode_copy_file_struct(src, dest)
static inline void
vnode_copy_file_struct(
    struct file *src,
    struct file *dest
)
{
    /* do not copy f_count, it's a ref count! */
    (dest)->f_mode = (src)->f_mode;
    (dest)->f_pos = (src)->f_pos;
    (dest)->f_flags = (src)->f_flags;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
    (dest)->f_reada = (src)->f_reada;
    (dest)->f_ramax = (src)->f_ramax;
    (dest)->f_raend = (src)->f_raend;
    (dest)->f_ralen = (src)->f_ralen;
    (dest)->f_rawin = (src)->f_rawin;
#else
    (dest)->f_ra = (src)->f_ra;       /* Copy struct file_ra_state */
#endif
    (dest)->f_owner = (src)->f_owner;
    (dest)->f_uid = (src)->f_uid;
    (dest)->f_gid = (src)->f_gid;
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,12)
    (dest)->f_error = (src)->f_error;
#endif
}

/* Dispatch Tables */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13)
extern void
vnode_iop_put_link(
    DENT_T *dentry,
    struct nameidata *nd,
    void *cookie
);
#endif

extern IN_OPS_T vnode_shadow_slink_inode_ops;
extern struct dentry_operations vnode_shadow_dentry_ops;
#ifdef HAVE_SHADOW_FILES
extern IN_OPS_T vnode_shadow_reg_inode_ops;
extern F_OPS_T vnode_shadow_file_ops;
extern F_OPS_T vnode_shadow_mapped_file_ops;
#endif /* HAVE_SHADOW_FILES */

/* Dentry operations */

extern void
vnode_shadow_dop_release(DENT_T *dentry);
/* $Id: 87a6021c.66c511dc.9bbb.00:01:83:09:5e:0d $ */
