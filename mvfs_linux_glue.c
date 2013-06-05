/*
 * Copyright (C) 2003, 2012 IBM Corporation.
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


#ifndef MANDATORY_LOCK
# define MANDATORY_LOCK(X) mandatory_lock(X)
#endif

/**********************************************************************
 * Reimplement truncate an inode.
 */
int
vnlayer_truncate_inode(
    struct dentry *dentry,
    struct vfsmount *mnt,
    loff_t length,
    mdki_boolean_t from_open
)
{
    struct inode *inp;
    struct iattr iat;
    int status;

    if (length < 0) {
        return -EINVAL;
    }
    inp = dentry->d_inode;

    LOCK_INODE(inp);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,5) && LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
#if !defined(RHEL_UPDATE) || RHEL_UPDATE < 5
    down_write(&inp->i_alloc_sem);
#endif
#endif

    iat.ia_size  = length;
    iat.ia_valid = (ATTR_SIZE | ATTR_CTIME);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,5) && defined(ATTR_FROM_OPEN)
    if (from_open) {
        iat.ia_valid |= ATTR_FROM_OPEN;
    }
    if (inp->i_op->setattr_raw != NULL) {
        iat.ia_valid |= ATTR_RAW;
        iat.ia_ctime = CURRENT_TIME;
        status = inp->i_op->setattr_raw(inp, &iat);
    }
    else {
        status = MDKI_NOTIFY_CHANGE(dentry, mnt, &iat);
    }
#else
    status = MDKI_NOTIFY_CHANGE(dentry, mnt, &iat);
#endif /* end  KERNEL_VERSION >= (2,6,5) && defined(ATTR_FROM_OPEN) */

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,5) && LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
#if !defined(RHEL_UPDATE) || RHEL_UPDATE < 5
    up_write(&inp->i_alloc_sem);
#endif
#endif

    UNLOCK_INODE(inp);

    return status;
}

/**********************************************************************
 * Reimplement looking for mandatory locks.
 */
int
vnlayer_has_mandlocks(struct inode *ip)
{
    struct file_lock *flock;
    int status;

    if (MANDATORY_LOCK(ip) == 0) {
        return 0;
    }
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,37)
    lock_kernel();
#else
    lock_flocks();
#endif

    status = 0;
    for (flock = ip->i_flock; flock != NULL; flock = flock->fl_next) {
        if ((flock->fl_flags & FL_POSIX) &&
	    (flock->fl_owner != current->files)) {
	    status = -EAGAIN;
	    break;
	}
    }
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,37)
    unlock_kernel();
#else
    unlock_flocks();
#endif
    return(status);
}

#ifdef NO_EXPORTED_LOOKUP_CREATE
/**********************************************************************
 * vnlayer_lookup_create()
 */
int
vnlayer_lookup_create(
    struct nameidata *nd,
    mdki_boolean_t is_dir,
    struct dentry **dpp
)
{
    struct dentry *d;

    *dpp = NULL;

    if (nd->last_type != LAST_NORM) {
        return(-EEXIST);
    }

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,5)
    nd->flags &= ~LOOKUP_PARENT;
#endif

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,9)
    nd->intent.open.flags = O_EXCL;
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
    d = lookup_hash(&nd->last, nd->dentry);
#else
    d = lookup_one_len(nd->last.name, MDKI_NAMEI_DENTRY(nd), nd->last.len);
#endif

    if (IS_ERR(d)) {
        return(PTR_ERR(d));
    }

    if ( ! is_dir && d->d_inode != NULL && nd->last.name[nd->last.len] != '\0') {
        dput(d);
	return(-ENOENT);
    }

    *dpp = d;
    return 0;
}
#endif

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,9)
#include <linux/backing-dev.h>
/*
 * Reimplement trivial function no longer exported from base kernel.
 */
void
vnlayer_ra_state_init(
    struct file_ra_state *ra,
    struct address_space *mapping
)
{
	ra->ra_pages = mapping->backing_dev_info->ra_pages;
# if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24)
        ra->prev_pos = -1;
# elif LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,11)
        ra->prev_page = -1;
# else
        ra->average = ra->ra_pages / 2;
# endif /* LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24) */
}
#endif

/**********************************************************************
 * vnlayer_set_fs_root()
 *
 * Reimplementation of set_fs_root() function for certain kernels.  
 */

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,10) || \
    (defined(RATL_REDHAT) && (RATL_VENDOR_VER >= 400))
extern void
vnlayer_set_fs_root(
    struct fs_struct *fs,
    struct vfsmount *mnt,
    struct dentry *dent
)
{
    struct dentry *old_root;
    struct vfsmount *old_rootmnt;

    MDKI_FS_LOCK_W(fs);

    old_root = MDKI_FS_ROOTDENTRY(fs);
    old_rootmnt = MDKI_FS_ROOTMNT(fs);

    MDKI_FS_SET_ROOTDENTRY(fs, dget(dent));
    MDKI_FS_SET_ROOTMNT(fs, MDKI_MNTGET(mnt));

    MDKI_FS_UNLOCK_W(fs);

    dput(old_root);
    MDKI_MNTPUT(old_rootmnt);
}
#endif

/**********************************************************************
 * bool_t mdki_xdr_opaque(XDR *xdrp, char *charp, u_int count)
 *
 * Reimplementation of xdr_opaque() function
 */
extern bool_t
mdki_xdr_opaque(
    XDR *xdrp,
    char *charp,
    u_int count
)
{
    u_int padcount;

    if (count == 0) {
        return TRUE;
    }

    padcount = count % BYTES_PER_XDR_UNIT;
    if (padcount != 0) {
        padcount = BYTES_PER_XDR_UNIT - padcount;
    }

    switch (xdrp->x_op) {
      case XDR_DECODE:
        if (! XDR_GETBYTES(xdrp, charp, count)) {
            return FALSE;
        }
        else if (padcount == 0) {
            return TRUE;
        }
        else {
            char ignorebytes[BYTES_PER_XDR_UNIT];
            return XDR_GETBYTES(xdrp, (caddr_t)ignorebytes, padcount);
        }
        break;

      case XDR_ENCODE:
        if (! XDR_PUTBYTES(xdrp, charp, count)) {
            return FALSE;
        }
        else if (padcount == 0) {
            return TRUE;
        }
        else {
            static const char nullbytes[BYTES_PER_XDR_UNIT];
            return XDR_PUTBYTES(xdrp, (caddr_t)nullbytes, padcount);
        }
        break;

      case XDR_FREE:
        MDKI_VFS_LOG(VFS_LOG_DEBUG, "%s called with free?\n", __func__);
        return FALSE;

      default:
        return FALSE;
    }
}
static const char vnode_verid_mvfs_linux_glue_c[] = "$Id:  0ef582d7.e6e311e1.8799.00:01:84:c3:8a:52 $";
