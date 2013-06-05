/* * (C) Copyright IBM Corporation 1999, 2012. */
/*
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA


 Author: IBM Corporation
 This module is part of the IBM (R) Rational (R) ClearCase (R)
 Multi-version file system (MVFS).
 For support, please visit http://www.ibm.com/software/support

*/
/* mvfs_mdep_linux.c */

#include "mvfs_systm.h"
#include "mvfs.h"
#include <ks_base.h>
#include "mfs_mount.h"
#include "linux_krpc.h"
#include "mvfs_linux_shadow.h"
#include <albd_rpc_kernel.h>
#include "view_rpc_kernel.h"
#include "mvfs_transtype.h"

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#error pre-2.6 kernels not supported
#endif

/* mdep types/decls for this file */

int
mvfs_linux_mmap_ctx(
    VNODE_T *vp,
    u_int sharing,
    u_int rwx,
    CALL_DATA_T *cd,
    MVFS_MMAP_CTX_T *ctxp
);

int
mvfs_linux_inactive(
    VNODE_T *vp,
    CALL_DATA_T *cd
);

int
mvfs_linux_lookup_wrapper(
    VNODE_T *advp,
    char *nm,
    VNODE_T **vpp,
    struct pathname *pnp,
    int flags,
    ROOTDIR_T *rdir,
    CALL_DATA_T *cd,
    MVFS_LOOKUP_CTX_T *ctxp
);

int
mvfs_linux_open_wrapper(
    VNODE_T **vpp,
    int mode,
    CALL_DATA_T *cd,
    MVFS_OPEN_CTX_T *ctxp
);

EXTERN int
mvfs_linux_rdwr_wrapper(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd,
    MVFS_RDWR_CTX_T *ctxp
);

int 
mvfs_linux_remove_wrapper(
    VNODE_T *advp,
    VNODE_T *avp,
    char *nm,
    CALL_DATA_T *cd,
    MVFS_REMOVE_CTX_T *ctxp
);

int
mvfs_linux_vstatfs_wrapper(
    VFS_T *vfsp,
    STATVFS_T *sbp
);

int
mvfs_linux_vsync_wrapper(
    VFS_T *vfsp,
    short flag,
    CRED_T *acred
);

int
mvfs_linux_mount_wrapper(
    VFS_T *vfsp,
    VNODE_T *mvp,
    mfs_pn_char_t *pn,
    int flags,
    caddr_t data,
    size_t datalen,
    CRED_T *cred,
    MVFS_CALLER_INFO *ctxp
);

EXTERN int
mvfs_linux_umount_wrapper(
    VFS_T *vfsp,
    CRED_T *cred
);

EXTERN int
mvfs_linux_realvp(
    VNODE_T *vp,
    VNODE_T **rvp
);

EXTERN int
mvfs_linux_realcvp(
    VNODE_T *vp,
    VNODE_T **rvp
);

EXTERN int 
mvfs_linux_getattr_wrapper(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag,
    CALL_DATA_T *cd
);

EXTERN int 
mvfs_linux_setattr_wrapper(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag,
    CALL_DATA_T *cd
);

EXTERN int
mvfs_linux_init(void);

EXTERN int
mvfs_linux_log(
    VFS_T *vfsp,
    int pri,
    const char *fmt,
    ...
);

EXTERN void
mvfs_linux_vnprint(VNODE_T *vp);

struct vnodeops mvfs_vnodeops = {
    .vop_open = &mvfs_linux_open_wrapper,
    .vop_close = &mvfs_closev_ctx,
    .vop_rdwr = &mvfs_linux_rdwr_wrapper,
    .vop_ioctl = &mvfs_ioctlv_subr,
    .vop_getattr = &mvfs_linux_getattr_wrapper,
    .vop_setattr = &mvfs_linux_setattr_wrapper,
    .vop_access = &mvfs_accessv_ctx,
    .vop_lookup = &mvfs_linux_lookup_wrapper,
    .vop_create = &mvfs_create_subr_ctx,
    .vop_remove = &mvfs_linux_remove_wrapper,
    .vop_link = &mvfs_link_ctx,
    .vop_rename = &mvfs_rename_ctx,
    .vop_mkdir = &mvfs_mkdir_ctx,
    .vop_rmdir = &mvfs_rmdir_ctx,
    .vop_readdir = &mvfs_readdir_ctx,
    .vop_symlink = &mvfs_symlink_ctx,
    .vop_readlink = &mfs_readlink,
    .vop_fsync = &mvfs_fsync_ctx,
    .vop_inactive = &mvfs_linux_inactive,
    .vop_fid = (vop_fid_fn_t) &mfs_vfid,
    .vop_realvp = &mvfs_linux_realvp,
    .vop_cmp = &mfs_cmp,
    .vop_mmap = &mvfs_linux_mmap_ctx,
    .vop_seek = &mvfs_seek_ctx,
    .vop_lockctl = &mvfs_lockctl_ctx,
    .vop_print = &mvfs_linux_vnprint
};

struct vfsops mvfs_vfsops = {
    .vfs_root = &mfs_root,
    .vfs_statvfs = &mvfs_linux_vstatfs_wrapper,
    .vfs_sync = &mvfs_linux_vsync_wrapper,
    .vfs_mount = &mvfs_linux_mount_wrapper,
    .vfs_unmount = &mvfs_linux_umount_wrapper,
    .vfs_vget = (vfs_vget_fn_t) &mvfs_vget_cd,
    .vfs_init = &mvfs_linux_init,
    .vfs_log =  &mvfs_linux_log
};

/* Other utility functions
 */
/*
 * Data structures
 */
CLR_VNODE_T *mvfs_sysroot_cvp;

/* Initialization routines */

void
mvfs_linux_mapinit(MVFS_MINMAP_T *map)
{
    /* We need to set the minor device code 0 for /dev/mvfs to allocated
     * so that we never actually use it.  We will set /view to use minor
     * device code 1 and all subsequent mounts will proceed from there.
     * We will always update the device code in the superblock.  This will
     * cause the mount code to always allocate a new super block when we
     * request a mount of /dev/mvfs.  If we didn't do this, the kernel
     * would keep trying to re use the super block for /view, which 
     * would be bad.
     */
    map->vec[0] = 1;
}

/*
 * MVFS_MAJORTBL_INIT - the Linux version.
 * Assign -1 for the non-exported vobs, indicating it should be
 * allocated when needed. The first vob gets mvfs_major.  There
 * are no ncaexported vobs.
 */
int
mvfs_linux_majortbl_init(MVFS_MAJOR_T **mvfs_majortbl_p)
{
    MVFS_MAJOR_T *major_array;
    int i;

    if (((*mvfs_majortbl_p) = (MVFS_MAJOR_T *)KMEM_ALLOC(
        (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * sizeof(MVFS_MAJOR_T), KM_SLEEP)) == NULL)
        return ENOMEM;
    major_array = *mvfs_majortbl_p;
    major_array[0] = mvfs_major;
    for (i=1; i < MVFS_MAJDYNMAX; i++)
        major_array[i]= -1;
    return 0;
}
 
/* 
 * This routine makes a name string from a prefix and a number.
 * The resulting string is 8 characters long.
 * FIXME: This routine should be put into mvfs_utils.c since it is
 * the same on all platforms.
 */

char *
mfs_makesname(
    char nm[8],
    const char *prefix,
    int num
)
{
    int  i,len;

    len = STRLEN(prefix);

    BCOPY((char *)prefix, nm, len);
    for (i = 7; i >= len; i--, num = num/10) {
        nm[i] = '0' + (char)(num % 10);
    }
    return(nm);
}

void
mvfs_linux_initlock(
    LOCK_T *lptr,
    char *name
)
{
    MDKI_INIT_SLEEPLOCK(&lptr->slock);
    lptr->owner = 0;
    lptr->count = 0;
    lptr->recursive = 0;
    STRNCPY(lptr->name, name, 8);
    return;
}

void
mvfs_linux_freelock(
    LOCK_T *lptr
)
{
    ASSERT(lptr->owner == 0);
    ASSERT(lptr->count == 0);
    MDKI_FREE_SLEEPLOCK(&lptr->slock);
    return;
}

/* This is a conditional locking routine.
 */

int
mvfs_linux_condlock(
    LOCK_T *lptr,
    caddr_t retpc,
    caddr_t retpc2,
    const char *where
)
{
    if (ISLOCKEDBYME(lptr))
        MDKI_PANIC("mvfs: LOCK: recursive lock");
    if (MDKI_SLEEP_TRYLOCK(&lptr->slock)) {
        return(0);
    }
    if (++lptr->count > 1)  
        MDKI_PANIC("mvfs: LOCK: recursive lock"); 
    lptr->owner = MDKI_CURPROC();
    lptr->locker = where;
    lptr->retpc = retpc;
    lptr->retpc2 = retpc2;
    return(1);
}

/* This function will determine if an MVFS process structure refers to
 * a valid Linux task (process)
 */
/*
 * MVFS process structures have PIDs in them, but we don't use that for much
 * except hashing and for this check.  They also have a process tag, which is 
 * the pointer to 'struct fs_struct' Linux tasks which share CLONE_FS state 
 * (cwd, rootdir, umask) are treated as threads in a process as far as MVFS 
 * cares (they all need to be interlocked against each other when we start 
 * fiddling with the state stored in struct fs_struct).
 *
 * To determine if an mvfs_proce_T is still valid, we will look look at the 
 * PID in the proc structure.  If it is valid we will look for the existence 
 * of a thread structure.  If there is no thread structure, this proc structure
 * is still in the process of being initialized and so we know that there is 
 * no reuse issue.  If there is a thread structure we will verify that the 
 * task pointer in the thread structure matches the one for this pid.  If this 
 * doesn't match than we will return invalid.
 */

tbs_boolean_t
mvfs_linux_procvalid(mvfs_proc_t *mprocp)
{
    mvfs_thread_t *thr;
    tbs_boolean_t rv;
    MVFS_PROCID_T pid;
    MVFS_OWNER_T task;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP(); 

    ASSERT(ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));

    MVFS_LOCK(&mprocp->mp_lock);
    pid = mprocp->mp_procid;
    if (mprocp->mp_threads != NULL) {
        task = mprocp->mp_threads->thr_threadid.tid_thread;
    } else {
        task = NULL;
    }
    rv = mdki_linux_procexists(pid, task);
    MVFS_UNLOCK(&mprocp->mp_lock);
    MDB_XLOG((MDB_PROCOPS2,"pid %d task %x alive=%d\n", pid, task, rv));
    return rv;
}

EXTERN struct mvfs_thread *
mvfs_linux_enter_fs(void)
{
    struct mvfs_thread *mth;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if (vrdp->mfs_viewroot_vfsp)
        mdki_mark_vfs_dirty(vrdp->mfs_viewroot_vfsp);
    mth = mvfs_mythread();
#if defined(MVFS_DEBUG) || defined(STACK_CHECKING)
    mth->thr_threadid.stack_check_id = mdki_get_stack_check_id();
#endif
    return mth;
}

#if defined(MVFS_DEBUG) || defined(STACK_CHECKING)
typedef unsigned long int uintptr_t;    /* XXX why not available in kernel headers? */
EXTERN void
mvfs_linux_exit_fs(struct mvfs_thread *thr)
{
    void *stack_check_id = mdki_get_stack_check_id();

    /*
     * MFS_AUDIT_ALIGN_BDRY is 7 on 64-bit platforms, 3 on 32-bit
     * platforms.  We're checking to be sure the pointer is aligned by
     * looking for all-zeroes in the low-order bits.
     */
    if ((thr->thr_threadid.stack_check_id != NULL &&
         thr->thr_threadid.stack_check_id != stack_check_id)
        || (((uintptr_t)stack_check_id) & MFS_AUDIT_ALIGN_BDRY))
    {
        MDKI_PANIC("stack check ID trashed: now=%p stored=%p\n",
                   stack_check_id, thr->thr_threadid.stack_check_id);
    }
}
#endif

void
mvfs_linux_drop_mnode_lock(void *arg)
{
    mfs_mnode_t *mnp = arg;
    MUNLOCK(mnp);
}

void
mvfs_linux_get_mnode_lock(void *arg)
{
    mfs_mnode_t *mnp = arg;
    MLOCK(mnp);
}

/* mvfs_wait_on_inactive
 *
 * This function is called with a locked file system node and will
 * create a waitq entry for this task and add it to the global vnode
 * waitq before releasing the file system node's lock.  This will
 * guarantee that a wake up will not be missed because the wake up
 * code is also covered with the file system's lock.  This essentially
 * mimics sleep_on.
 */
STATIC void
mvfs_wait_on_inactive(mfs_mnode_t *mnp)
{
    DECLARE_WAITQUEUE(mwait, current);

    add_wait_queue(&vnlayer_inactive_waitq, &mwait);
    set_current_state(TASK_UNINTERRUPTIBLE);
    MUNLOCK(mnp);
    schedule();
    remove_wait_queue(&vnlayer_inactive_waitq, &mwait);
    set_current_state(TASK_RUNNING);
    MLOCK(mnp);
    return;
}

/* This is our inode get routine disguised a little bit to fit into the
 * vnode world of the mvfs.
 * This is what is called by the MFS_VNGET macro.
 */

int
mvfs_linux_vnget(
    VFS_T *mnvfsp,
    VFS_T *lovfsp,
    mfs_mnode_t *mnp,
    VNODE_T **vpp
)
{
    VTYPE_T vtype;
    VNODE_T *vp;
    VFS_T *vfsp;
    INODE_T *ip;

    /*
     * We don't do anything odd in MVFS_LOOPCLAS_VFSP, so we should get
     * equal vfsp's for loopback nodes
     */
    ASSERT(lovfsp == NULL || lovfsp == mnvfsp);
    vfsp = mnvfsp;

    ASSERT(ISLOCKEDBYME(MLOCK_ADDR(mnp)));

    *vpp = NULL;

    /* If mnode already attached to vnode, then just
     * bump the vnode refcount protected by the mnode
     * lock, and then drop the extra mnode refcount
     * that was taken until we could be hooked up to
     * the vnode.
     *
     * On some systems this quite a bit more complicated.
     */

retry:
    if (mnp->mn_hdr.vp) {
        /* First thing we have to is verify that the VP is not in the process
         * of going away.  If it is, we need to wait for it to do so.
         */
        vp = VN_HOLD(mnp->mn_hdr.vp);
        if (!vp) {
            mvfs_wait_on_inactive(mnp);
            goto retry;
        }
        /* ASSERT(!(ip->i_state & (I_FREEING | I_CLEAR))); */
        ASSERT(vp->v_sanity == VNODE_SANITY);
        *vpp = vp;
        ASSERT(VTOM(vp) == mnp);
        ASSERT(mnp->mn_hdr.mcount > 1);
        if (mnp->mn_hdr.realvp != NULL) {
            switch(mnp->mn_hdr.mclass) {
              case MFS_VIEWCLAS:
                /* For views we cannot just copy the inode data because it
		 * clobbers our device number.  All we have to do is to update
		 * the attributes that might have changed.
		 */
                MVFS_WRAP_UPDATE_ATTRS(vp);
                break;
              case MFS_VOBCLAS:
                break;
              default:
                /* copy up inode data again */
                ASSERT(MDKI_INOISCLRVN(VTOI(mnp->mn_hdr.realvp)));

                SHADOW_CP_INODAT(CVN_TO_INO(mnp->mn_hdr.realvp), VTOI(vp));
                break;
            }
        }
        MUNLOCK(mnp);
        mfs_mnrele(mnp);  /* Drop extra mnode refcount */
        MDB_VFSLOG((MFS_VGET,"vfsp=%lx mnp=%lx vp=%lx attached\n", vfsp, mnp, vp));
        return(0);
    }

    vtype = mfs_mn_vtype(mnp);

    switch(vtype) {
      case VREG:
      case VDIR:
      case VCHR:
      case VLNK:
        break;
      default:
        VFS_LOG(vfsp, VFS_LOG_ERR,
                "attempt to set unimplemented file type %o\n", vtype);
        MUNLOCK(mnp);
        mfs_mnrele(mnp);          /* drop */
        return ENOSYS;
    } /* end switch(vtype) */

    /* Now we have to get our inode */

    /* use mnum for now ... VOB nodes get whacked later in
     * mdki_linux_vattr_pullup()
     */
    ip = new_inode(VFSTOSB(vfsp));
    if (ip != NULL) {
        ip->i_ino = mnp->mn_hdr.mnum;
        ip->i_version = mnp->mn_hdr.fid.mf_gen;
        MDKI_TRACE(TRACE_VNODES,"%s: using %p\n", __func__, ITOV(ip));
        vp = ITOV(ip);
        /* This returns an inode that is mostly zeroed but it has a device
         * number taken from the super block.
         */
    } else {
        /* We ran out of inodes */
        MUNLOCK(mnp);
        mfs_mnrele(mnp);          /* drop */
        return ENFILE;
    }

    MDB_XLOG((MDB_VNODES,"vn_alloc vp=%p mnp=%p\n", vp, mnp));

    /* And initialize it. */

    vp->v_type = vtype;
    vp->v_vfsp = vfsp;
    vp->v_op = &mvfs_vnodeops;
    vp->v_sanity = VNODE_SANITY;

    /*
     * It's OK to add flag without lock since nobody else can see this
     * vnode yet.
     */
    vp->v_flag = 0;
    if (MFS_ISLOOPVIEW(mnp))
        vp->v_flag |= VLOOPROOT;
    else if (MFS_ISLOOP(mnp))
        vp->v_flag |= VLOOP;

    if (!MFS_ISVOB(mnp)) {
        if (mnp->mn_hdr.realvp) {
            /* set up the inode data from the real inode if we have one */
            ASSERT(MDKI_INOISCLRVN(VTOI(mnp->mn_hdr.realvp)));

            SHADOW_CP_INODAT(CVN_TO_INO(mnp->mn_hdr.realvp), ip);
        } else {
            /*
             * There isn't a real inode to take things from.  For now
             * just pick some useful defaults.
             * FIXME:  There has to be a better way of setting the
             * mode at least.
             */
            ip->i_atime = ip->i_mtime = ip->i_ctime = CURRENT_TIME;
            ip->i_uid = MDKI_GET_CURRENT_FSUID();
            ip->i_gid = MDKI_GET_CURRENT_FSGID();
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
            ip->i_blksize = VFSTOSB(vfsp)->s_blocksize;
#else
            ip->i_blkbits = VFSTOSB(vfsp)->s_blocksize_bits;
#endif
            ip->i_mode = (vnlayer_vtype_to_mode(vtype) |
                          S_IRUGO | S_IXUGO | S_IWUSR);
            MDB_XLOG((MDB_VNODES,
                      "default vnode attributes vp=%p mnp=%p clas=%d\n",
                      vp, mnp, mnp->mn_hdr.mclass));
        }
    }

    /* Set up the type and the ops.  The ops vary depending on the 
     * file type.  We will add them as we need them.
     */

    switch(vtype) {
      case VREG:
        /*
         * We'd like to have separate ops depending on whether the
         * underlying object supports mmap or not, but that would
         * require cltxt activation at this point (we can't do that
         * here).  Just assume the underlying FS supports mmap.  Swap
         * at the time of an open if need be; the object can't be
         * mapped until after an open call anyway, so we can check
         * then and change to use a non-mappable operation vector if
         * necessary.
         */
        ip->i_op = &vnode_file_mmap_inode_ops;
        ip->i_fop = &vnode_file_mmap_file_ops;
        ip->i_data.a_ops = &mvfs_addrspace_ops;
        break;
      case VCHR:
        ip->i_op = &vnode_file_inode_ops;
        ip->i_fop = &vnode_file_file_ops;
        break;
      case VLNK:
        ip->i_op = &vnode_slink_inode_ops;
        ip->i_fop = &vnode_file_file_ops;
        break;
      case VDIR:
        ip->i_op = &vnode_dir_inode_ops;
        ip->i_fop = &vnode_dir_file_ops;
        break;
      default:
        /* no other types supported right now */
        BUG();
        break;
    } /* end switch(vtype) */

    /* Set forward and back ptrs */

    vp->v_data = (caddr_t)mnp;
    /* Get a hold on the VOB's vfsmount */
    if ((MFS_ISVOB(mnp) || MFS_ISVOBRT(mnp)) && mnp->mn_hdr.viewvp != NULL) {
        /* We will get the vfsmount structure from the VFS's root
         * vnode. [The root vnode holds it to prevent unmounting while in
         * use.]
         */
        struct vfsmount *mnt = VTOVFSMNT(VFS_TO_MMI(vfsp)->mmi_rootvp);
        SET_VTOVFSMNT(vp, MDKI_MNTGET(mnt));
    } else
        vp->v_vfsmnt = NULL;
    vp->v_dent = NULL;                  /* only for CVN's */
    mnp->mn_hdr.vp = vp;

    ASSERT(mnp->mn_hdr.mcount > 0);

    /*
     * Update size & other statistics from the mnode prior to making the new
     * vnode visible
     */
    MVFS_WRAP_UPDATE_ATTRS(vp);

    /* All initialized, now unlock the mnode lock */

    MUNLOCK(mnp);

    /* Leave mnode refcount until vnode inactivated and we
     * null out the vp->v_data ptr
     */

    *vpp = vp;
    MDB_VFSLOG((MFS_VGET,"vfsp=%lx mnp=%lx vp=%lx new\n", vfsp, mnp, *vpp));
    return(0);
}

/*
 * MFS_PROC_SETVIEW - set the view for the current process
 * Note: this routine always 'uses up' a refcount on the view
 * vnode passed into it!  The 'vw' arg may be NULL to indicate
 * resetting to no view.
 */
extern int
mvfs_linux_proc_setview(
    VNODE_T *vw,
    tbs_status_t *status_p
)
{
    int err;
    int ok = TRUE;
    VNODE_T *viewdir = NULL;
    const char *viewname = NULL;

    if (vw) {
        viewdir = mfs_getviewroot();
        if (viewdir == NULL) {
            *status_p = TBS_ST_MFS_ERR;
            return ESTALE;
        }
        viewname = VTOM(vw)->mn_view.viewname;
    }
    err = mdki_linux_proc_setview(vw, viewdir, viewname,
                                  status_p ? &ok : NULL);
    if (status_p)
        *status_p = (ok == TRUE) ? TBS_ST_OK : TBS_ST_MFS_ERR;

    if (viewdir)
        VN_RELE(viewdir);
    return err;
}

#ifdef MVFS_DEBUG
void
mfs_chkpages(
    VNODE_T *vp,
    int rw
)
{
    /* NYI */
}
#endif

extern int
mvfs_linux_should_cover(void)
{
    return TRUE;
}

extern int
mvfs_linux_sync_size(
    VNODE_T *vp,
    loff_t size,
    int actual
)
{
#ifdef MVFS_DEBUG
    mfs_mnode_t *mnp;
    ASSERT(MFS_VPISMFS(vp));
    mnp = VTOM(vp);
    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));
#endif
    return mdki_sync_size(vp, size, actual);
}

extern int
mvfs_linux_set_modified(VNODE_T *vp)
{
    mfs_mnode_t *mnp;
    VATTR_T va;

    ASSERT(MFS_VPISMFS(vp));
    mnp = VTOM(vp);
    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));
    
    mfs_mn_to_vattr(mnp, &va);

    return mdki_set_modified(vp, &va);
}

extern int
mvfs_linux_set_accessed(VNODE_T *vp)
{
    mfs_mnode_t *mnp;
    VATTR_T va;

    ASSERT(MFS_VPISMFS(vp));
    mnp = VTOM(vp);
    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));
    
    mfs_mn_to_vattr(mnp, &va);          /* XXX overkill? */

    return mdki_set_accessed(vp, &va);
}

extern int
mvfs_linux_set_ichg(VNODE_T *vp)
{
    mfs_mnode_t *mnp;
    VATTR_T va;

    ASSERT(MFS_VPISMFS(vp));
    mnp = VTOM(vp);
    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));

    mfs_mn_to_vattr(mnp, &va);

    return mdki_set_ichg(vp, &va);
}

extern void
mvfs_linux_update_attrs(
    VNODE_T *vp,
    const char *caller
)
{
    mfs_mnode_t *mnp;
    VATTR_T va;
    int error;
    CLR_VNODE_T *cvp = NULL;
    CALL_DATA_T cd;

    mnp = VTOM(vp);
    ASSERT(MISLOCKED(mnp));

    switch (mnp->mn_hdr.mclass) {
      case MFS_VOBCLAS:
        /* mnode has fresh attrs, we should copy them up */
        mfs_mn_to_vattr(mnp, &va);
        break;
      case MFS_VIEWCLAS:
        /* realvp is not yet connected to the view, but we know what
           it should be... XXX should be done more cleanly, but don't
           want to replicate getattr code for view dirs.... */
        if (mnp->mn_hdr.realvp == NULL) {
            cvp = CLR_ROOTDIR;
            CVN_HOLD(cvp);
            mnp->mn_hdr.realvp = cvp;
        }
        /* fall through */
      case MFS_SDEVCLAS:
      case MFS_VIEWDIRCLAS:
      case MFS_NTVWCLAS:
        /* All these types can get attributes locally (without an RPC) */
        /* XXX safe to hold mnode lock over this call? */
        /* It is ugly to have to call this here, but we would have to 
         * rototill a lot of code to pass in the cd here.  I hope this
         * does not eliminate all of our performance gains.
         */
        mdki_linux_init_call_data(&cd);
        if ((error = mfs_getattr(vp, &va, 0, &cd)) != 0) {
            mvfs_log(MFS_LOG_DEBUG,"getattr failed on update vp=%p err=%d\n",
                     vp, error);
        }
        mdki_linux_destroy_call_data(&cd);
        if (mnp->mn_hdr.mclass == MFS_VIEWCLAS && cvp != NULL) {
            mnp->mn_hdr.realvp = NULL;
            CVN_RELE(cvp);
        }
        if (error)
            return;                     /* no pullup, attributes invalid! */
        break;
      case MFS_VOBRTCLAS:
        mvfs_noview_vobrt_getattr(vp, &va);
        MDB_VLOG((MFS_VGETATTR,
                 "%s: vattr vobrt pullup %p from %s\n",
                 __func__,vp, caller));
        break;
      case MFS_LOOPCLAS:
        /* loopback nodes handled specially elsewhere */
        return;
    }
    mdki_linux_vattr_pullup(vp, &va, AT_ALLNODEV);
}

extern fsid_t /* gross to return it, but so's the macro XXX */
mvfs_linux_vfsp_to_fsid(VFS_T *vfsp)
{
    fsid_t fs;
    MAKEFSID(fs, mdki_major(mdki_vfs_to_dev(vfsp)), VFS_TO_MMI(vfsp)->mmi_minor);
    return fs;
}

extern int
mvfs_linux_mmap_ctx(
    VNODE_T *vp,
    u_int sharing,
    u_int rwx,
    CALL_DATA_T *cd,
    MVFS_MMAP_CTX_T *ctxp
)
{
    int err = 0;
    CLR_VNODE_T *cvp = NULL;

    BUMPSTAT(mfs_vnopcnt[MFS_VMAP]);

    if (!MFS_ISVOB(VTOM(vp))) {
        /* Not seen in the wild yet, just defensive programming. */
        mvfs_log(MFS_LOG_ERR, "mmap check on non-VOB object (vp=%p)\n", vp);
        return ENXIO;
    }

    if ((err = mvfs_mmap_getcvp(vp, &cvp, sharing, rwx, cd)) != 0)
        goto out;
    err = MVOP_MMAP(cvp, sharing, rwx, cd, ctxp);
    CVN_RELE(cvp);

    if (!err)
        mvfs_mmap_no_audit(vp, sharing, rwx, cd);
  out:
    MDB_VLOG((MFS_VMAP, "%s: vp=%p, cvp=%p, sharing=%x, rwx=%x, cred=%p, ctxp=%p, err=%d\n",
              __func__, vp, cvp, sharing, rwx, MVFS_CD2CRED(cd), ctxp, err));
    return err;
}

int
mvfs_linux_lookup_wrapper(
    VNODE_T *advp,
    char *nm,
    VNODE_T **vpp,
    struct pathname *pnp,
    int flags,
    ROOTDIR_T *rdir,
    CALL_DATA_T *cd,
    MVFS_LOOKUP_CTX_T *ctxp
)
{
    mvfs_thread_t *mth;
    VNODE_T *dvp = advp;
    VNODE_T *vp = *vpp;
    VATTR_T *vap;
    int error;

    /* For certain operations, we don't do a core lookup but just do
       some auditing things.  Key off the flag parameter. */
    if ((flags & VNODE_LF_AUDIT) != 0) {
        ASSERT(*vpp);
        mth = MVFS_CD2THREAD(cd);
        if (mth->thr_auditon && !mth->thr_auditinh) {
            ASSERT(MFS_VPISMFS(dvp));
            if (MFS_ISVOBRT(VTOM(dvp))) {
                dvp = mfs_bindroot(dvp, cd, &error);
                /* returns same object in error, i.e. unbound root */
            } else if (MFS_ISVOB(VTOM(dvp)))
                mfs_rebind_vpp(0, &dvp, cd);

            if (vp != NULL) {
                if (MFS_ISVOB(VTOM(vp)))
                    mfs_rebind_vpp(0, &vp, cd);
                MFS_AUDIT(MFS_AR_LOOKUP, dvp, nm, NULL, NULL, vp, cd);
            }
            if (advp != dvp)
                VN_RELE(dvp);
            if (vp != *vpp)
                VN_RELE(vp);
        }
        return 0;
    } else
        return mvfs_lookup_ctx(advp, nm, vpp, pnp, flags, rdir, cd, ctxp);
}

int
mvfs_linux_open_wrapper(
    VNODE_T **vpp,
    int mode,
    CALL_DATA_T *cd,
    MVFS_OPEN_CTX_T *ctxp
)
{
    return mvfs_openv_ctx(vpp, mode, cd, TRUE /* do_vop */, ctxp);
}

int
mvfs_linux_rdwr_wrapper(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd,
    MVFS_RDWR_CTX_T *ctxp
)
{
    return mvfs_rdwr_ctx(vp, uiop, rw, ioflag, vap, cd, ctxp);
}

int 
mvfs_linux_remove_wrapper(
    VNODE_T *advp,
    VNODE_T *avp,
    char *nm,
    CALL_DATA_T *cd,
    MVFS_REMOVE_CTX_T *ctxp
)
{
    return mvfs_remove_ctx(advp, avp, nm, cd, ctxp);
}

int
mvfs_linux_inactive(
    VNODE_T *vp,
    CALL_DATA_T *cd
)
{
    /* Modeled on  mfs_std_inactive() */
    mfs_mnode_t *mnp;
    int error;

    ASSERT(vp->v_sanity == VNODE_SANITY);

    /*
     * don't need to check V_COUNT(), the inactive protocol on Linux
     * protects us from reactivations
     */

    /*
     * Flush our pages before acquiring the lock
     * Pageout routines are responsible for logging
     * any errors.
     */

    mnp = VTOM(vp);
    if (mnp->mn_hdr.cached_pages) {
	(void) PVN_FLUSHINACTIVE(vp, MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
    }

    /*
     * Do common code.  In this template the common
     * code is allowed to sleep on lock/memory. 
     * Also, the common code logs the inactive operation.
     */

    MLOCK(mnp);

    error = mfs_inactive_common(vp, MFS_INACTIVE_SLEEP, cd);

    /*
     * Do actual deactivation of the vnode/mnode
     */

    /*
     * Retest fwd/back pointers in case someone else 
     * raced with us and already released the vnode to
     * mnode binding.
     */
    if (mnp->mn_hdr.vp) {
        ASSERT(mnp->mn_hdr.vp == vp);
        mnp->mn_hdr.vp = NULL;
        vp->v_data = NULL;
        mdki_inactive_finalize(vp);
        MUNLOCK(mnp);
        mfs_mnrele(mnp);
    } else {
        MUNLOCK(mnp);
    }

    return(error);
}

void
mvfs_linux_getattr_cleanup(
    VNODE_T *origvn,
    VNODE_T *boundvn,
    VATTR_T *vap,
    int flag,
    CRED_T *cred
)
{
    if ((flag & GETATTR_FLAG_UPDATE_ATTRS) == 0)
        return;

    /* if original vnode is a VOBRT, VOB or LOOPDIR, update its inodes' attrs */
    switch (VTOM(origvn)->mn_hdr.mclass) {
      case MFS_VOBRTCLAS:
        MDB_VLOG((MFS_VLOOKUP, "%s: vattr pullup vobroot %p\n",
                  __func__, origvn));
        /* need to pull bound stats into the unbound VOB root inode */
        mdki_linux_vattr_pullup(origvn, vap, AT_ALL);
        break;
      case MFS_VOBCLAS:
      case MFS_LOOPCLAS:
        MDB_VLOG((MFS_VLOOKUP, "%s: vattr pullup vob or loopdir %p class %d\n",
                  __func__, origvn, VTOM(origvn)->mn_hdr.mclass));
        /* need to pull up all stats for updating rebound directories */
        mdki_linux_vattr_pullup(origvn, vap, AT_STAT);
        break;
      default:
        break;
    }
    return;
}

int
mvfs_linux_vstatfs_wrapper(
    VFS_T *vfsp,
    STATVFS_T *sbp
)
{
    int error;
    struct mfs_mntinfo *mmi;

    error = mfs_vstatfs(vfsp, sbp);
    if (error != 0)
        return error;
    sbp->f_fsid = VFS_TO_MMI(vfsp)->mmi_nvfsid; /* XXX MDKI_VFSID()? */
    return (error);
}

int
mvfs_linux_mount_wrapper(
    VFS_T *vfsp,
    VNODE_T *mvp,
    mfs_pn_char_t *pn,
    int flags,
    caddr_t data,
    size_t datalen,
    CRED_T *cred,
    MVFS_CALLER_INFO *ctxp
)
{
    int err, size;
#ifdef RATL_COMPAT32
    struct mfs_mntargs_32 *ma32p;
    struct mfs_mntargs *ma64p;
#endif
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

#ifdef RATL_COMPAT32
    ma32p = (struct mfs_mntargs_32 *)data;
    ma64p = (struct mfs_mntargs *)data;
    if (ma32p->mma_mntvers == MFSMNT_VERSION &&
        ma32p->mma_mntsize == sizeof(*ma32p))
    {
        ctxp->caller_is_32bit = 1;
        size = sizeof(*ma32p);
    }
    else if (ma64p->mma_mntvers == MFSMNT_VERSION &&
             ma64p->mma_mntsize == sizeof(*ma64p))
    {
        ctxp->caller_is_32bit = 0;
        size = sizeof(*ma64p);
    }
    else {
        size = 0;                       /* indicate bad size for later */
        mvfs_log(MFS_LOG_ERR,
                 "mount_mvfs version/size mismatch "
                 "(ver=%d 32/64=%d/%d, sizeof=%d/%d sz=%d/%d)\n",
                 MFSMNT_VERSION, ma32p->mma_mntvers, ma64p->mma_mntvers,
                 (unsigned int)sizeof(*ma32p), (unsigned int)sizeof(*ma64p),
                 ma32p->mma_mntsize, ma64p->mma_mntsize);
    }
#else
    size = sizeof(struct mfs_mntargs);
#endif

    err = mfs_vmount_subr(vfsp, mvp, pn, flags, data,
                          size, cred, ctxp);
    if (err == 0 && vfsp == vrdp->mfs_viewroot_vfsp) {
        mdki_set_logging_vfsp(vfsp);
        mdki_set_clrvnode_vfsp(vfsp);
        ASSERT(vrdp->mfs_viewroot_vp != NULL);
        mdki_set_looproot_vp(vrdp->mfs_viewroot_vp);
        vfsp->vfs_flag |= VFS_POSIXACL;
    }
    if (err == 0 && mvfs_sysroot_cvp == NULL && vrdp->mfs_viewroot_vfsp != NULL) {
        mvfs_sysroot_cvp = mdki_make_sysroot_vnode();
        ASSERT(mvfs_sysroot_cvp != NULL);
    }
    return err;
}

int
mvfs_linux_umount_wrapper(
    VFS_T *vfsp,
    CRED_T *cred
)
{
    int err;
    int isviewroot;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if (vfsp == vrdp->mfs_viewroot_vfsp) {
        /* drop sysroot to ensure we can unmount this object */
        /* XXX locking? */
        isviewroot = TRUE;
        ASSERT(mvfs_sysroot_cvp != NULL);
        mdki_release_sysroot_vnode(mvfs_sysroot_cvp);
        mvfs_sysroot_cvp = NULL;
        mdki_clear_logging_vfsp(vfsp);
        mdki_clear_clrvnode_vfsp(vfsp);
        mdki_clear_looproot_vp(vrdp->mfs_viewroot_vp);
    } else
        isviewroot = FALSE;
    
    err = mfs_vunmount(vfsp, cred);

#ifdef MVFS_DEBUG
    if (err != 0) {
        mdki_linux_printf("unmount failure: %d\n", err);
        mvfs_mnreport_leftover_vnodes(vfsp);
    }
#endif
    if (err != 0 && isviewroot) {
        /* restore sysroot */
        mvfs_sysroot_cvp = mdki_make_sysroot_vnode();
        ASSERT(mvfs_sysroot_cvp != NULL);
        mdki_set_logging_vfsp(vfsp);
        mdki_set_clrvnode_vfsp(vfsp);
        mdki_set_looproot_vp(vrdp->mfs_viewroot_vp);
    }       
    return err;
}
/* We need to provide a function to get the realvp for a loopback directory
 * so that the vnode layer can query us.  With this addition, then we can
 * avoid having to pass functions through the mvfs to be passed back to 
 * the vnode layer where the only thing that the mvfs has added is the
 * tranlation of a vnode pointer to a realvp.  The first place where this 
 * is needed is in link, where the mvfs assumes that all the vnodes are
 * mvfs vnodes and it receives a shadow vnode as well, which doesn't work.
 * There may be other places where this can be used to short circuit the 
 * calls to the mvfs for shadow file operations.
 */

EXTERN int
mvfs_linux_realvp(
    VNODE_T *vp,
    VNODE_T **rvp
)
{
    int err = EINVAL;
    VNODE_T *cvp;

    /*
     * the realvp for MVFS mnode-based objects is the vp itself.  But
     * we can just return an error code and the caller will use the
     * original vp.  Compare to mfs_realvp() but we don't want the
     * "stray" message printed.
     * However, the realvp for Loopback directories is not the vp itself.
     * We get called to get the realvp of loopback directories so we need
     * to return real data in that case.
     */
    BUMPSTAT(mfs_vnopcnt[MFS_VREALVP]);
    if ((vp->v_flag & (VLOOP | VLOOPROOT)) != 0) {
	cvp = MFS_CLRVP(vp);
        if (cvp != NULL) {
            *rvp = cvp;
	    err = 0;
	}
    }
    return err;
}

EXTERN int
mvfs_linux_realcvp(
    VNODE_T *vp,
    VNODE_T **rvp
)
{
    /*
     * the realvp for MVFS mnode-based objects is the vp itself.  But
     * we can just return an error code and the caller will use the
     * original vp.  Compare to mfs_realvp() but we don't want the
     * "stray" message printed.
     * (We have to implement it because MVOP_REALCVP() will come here
     * in some cases.)
     */
    BUMPSTAT(mfs_vnopcnt[MFS_VREALVP]);
    return(EINVAL);
}

EXTERN int 
mvfs_linux_getattr_wrapper(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag,
    CALL_DATA_T *cd
)
{
    int err;
    if ((MVFS_MYTHREAD(cd))->thr_threadid.no_bindroot) {
        flag |= MVFS_GETATTR_NO_BINDROOT;
    }

    err = mfs_getattr(avp, vap, flag, cd);

    if (err == 0 && (flag & GETATTR_FLAG_PULLUP_ATTRS) != 0)
        switch (VTOM(avp)->mn_hdr.mclass) {
          case MFS_VOBRTCLAS:
            MDB_VLOG((MFS_VGETATTR, "%s: vattr pullup vobrt %p\n",
                      __func__, avp));
            mdki_linux_vattr_pullup(avp, vap, AT_ALL);
            break;
          case MFS_VOBCLAS:
          case MFS_LOOPCLAS:
            MDB_VLOG((MFS_VGETATTR, "%s: vattr pullup VOB or LOOPDIR fsid %p class %d\n",
                      __func__, avp, VTOM(avp)->mn_hdr.mclass));
            mdki_linux_vattr_pullup(avp, vap, AT_STAT);
            break;
          default:
            break;
        }
    return err;
}

EXTERN int 
mvfs_linux_setattr_wrapper(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag,
    CALL_DATA_T *cd
)
{
    return(mvfs_changeattr(avp, vap, flag, cd, NULL));
}

int
mvfs_linux_init(void)
{
    return(mdki_errno_unix_to_linux(mvfsinit(NULL, 0)));
}

EXTERN int
mvfs_linux_log(
    VFS_T *vfsp,
    int pri,
    const char *fmt,
    ...
)
{
    va_list ap;

#define CONVERT(val)                            \
        case VFS_LOG_##val:                     \
          pri = MFS_LOG_##val;                  \
          break

    switch (pri) {
        CONVERT(ERR);
        CONVERT(INFO);
        CONVERT(ESTALE);
        CONVERT(DEBUG);
        CONVERT(ENOENT);
      default:
        pri = MFS_LOG_INFO;
        break;
    }
#undef CONVERT
    va_start(ap, fmt);
    mvfs_vlog(pri, fmt, ap);
    va_end(ap);
    return 0;
}

char *mvfs_linux_rcsid_string;
char *mvfs_linux_sccsid_string;
static const char mvfs_vnode_tag[] = " built at ";

static void
mvfs_setup_id_strings(void)
{
    int len;

    len = STRLEN(mvfs_rcsID) + STRLEN(mvfs_vnode_tag) +
        STRLEN(mdki_vnode_build_time);
    mvfs_linux_rcsid_string = mdki_linux_kmalloc(len+1, KM_SLEEP);
    if (mvfs_linux_rcsid_string != NULL) {
        ASSERT(mvfs_linux_rcsid_string);
        STRCPY(mvfs_linux_rcsid_string, mvfs_rcsID);
        STRCPY(mvfs_linux_rcsid_string + STRLEN(mvfs_rcsID), mvfs_vnode_tag);
        STRCPY(mvfs_linux_rcsid_string + STRLEN(mvfs_rcsID) +
               STRLEN(mvfs_vnode_tag),
               mdki_vnode_build_time);
    } else {
        printk("mvfs_setup_id_strings: Failed to alloc %d bytes for mvfs_linux_rcsid_string\n", len);
    }

    len = STRLEN(mvfs_sccsID) + STRLEN(mvfs_vnode_tag) +
        STRLEN(mdki_vnode_build_time);
    mvfs_linux_sccsid_string = mdki_linux_kmalloc(len+1, KM_SLEEP);
    if (mvfs_linux_sccsid_string != NULL) {
        ASSERT(mvfs_linux_sccsid_string);
        STRCPY(mvfs_linux_sccsid_string, mvfs_sccsID);
        STRCPY(mvfs_linux_sccsid_string + STRLEN(mvfs_sccsID), mvfs_vnode_tag);
        STRCPY(mvfs_linux_sccsid_string + STRLEN(mvfs_sccsID) +
               STRLEN(mvfs_vnode_tag),
               mdki_vnode_build_time);
    } else {
        printk("mvfs_setup_id_strings: Failed to alloc %d bytes for mvfs_linux_sccsid_string\n", len);
    }
}

static void
mvfs_cleanup_id_strings(void)
{
    int len;

    mdki_linux_kfree(mvfs_linux_rcsid_string,
                     STRLEN(mvfs_linux_rcsid_string) + 1);
    mdki_linux_kfree(mvfs_linux_sccsid_string,
                     STRLEN(mvfs_linux_sccsid_string) + 1);
}

/*
 * Returns Linux error code (negative)
 */
int
init_mvfs_module(void)
{
    int err;

#ifdef RATL_COMPAT32
    err = mdki_handle_ioctl(MVFS_IOCTL_CMD_32);
    if (err != 0) {
        mdki_linux_printf("ERROR: MVFS compat32 ioctl failed: %d\n", err);
        return mdki_errno_unix_to_linux(err);
    }
#endif

    mvfs_setup_id_strings();
    err = mdki_set_vfs_opvec(&mvfs_vfsops);
    if (err == 0) {
        err = mdki_linux_mdep_init();
    } else {
        mdki_linux_printf("ERROR: MVFS set vfs opvec failed: %d\n", err);
    }
    if (err == 0) {
        (void) (*mvfs_vfsops.vfs_init)();
    } else {
        mdki_linux_printf("ERROR: MVFS mdep init failed: %d\n", err);
#ifdef RATL_COMPAT32
        mdki_unhandle_ioctl(MVFS_IOCTL_CMD_32);
#endif
    }
    return mdki_errno_unix_to_linux(err);
}

void
cleanup_mvfs_module(void)
{
    int err;
    err = mfs_unload(0,0);
    if (err != 0) {
        mdki_linux_printf("WARNING: MVFS cleanup/unload failed: %d\n", err);
    }
    mdki_clear_vfs_opvec(&mvfs_vfsops);
    mdki_linux_mdep_unload();
    mvfs_cleanup_id_strings();
#ifdef RATL_COMPAT32
    mdki_unhandle_ioctl(MVFS_IOCTL_CMD_32);
#endif
}

/* RPC glue functions */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
/* This routine allocates a unique XID every time it is called. */

ks_uint32_t
mvfs_linux_alloc_xid(void)
{
    MVFS_XID_T xid;
    static spinlock_t mvfs_xid_lock = SPIN_LOCK_UNLOCKED;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    spin_lock(&mvfs_xid_lock);
    xid = ++(mcdp->mvfs_xid);
    if (xid == 0) {
        mcdp->mvfs_boottime = MDKI_CTIME();
        xid = ++(mcdp->mvfs_xid);
    }
    spin_unlock(&mvfs_xid_lock);
    return((ks_uint32_t)xid);
}
#endif

/*
 * The Linux kernel RPC structure is different from standard ONC RPC
 * kernel structure in many ways.  This is the conversion between the
 * two.
 */

static struct rpc_stat view_rpc_stats;
static struct rpc_stat albd_rpc_stats;

#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,32)
#define kxdreproc_t kxdrproc_t
#define kxdrdproc_t kxdrproc_t
#endif

/*
 * The sizes listed in the RPC description should be the on-the-wire
 * encoding sizes.  That's not easy to compute, so we just make sure
 * we've got plenty of space using a similar scaling to what Linux
 * routines use.
 */
#define RPC_REQ_BUFSZ(type) (sizeof(type##_req_t)<<2)
#define RPC_REP_BUFSZ(type) (sizeof(type##_reply_t)<<2) 

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
#define XDR_RPC_FUNCS(type)                                             \
/* return Linux error codes to RPC runtime */                           \
STATIC void                                                             \
mvfs_linux_xdr_encode_##type(                                           \
void *rqstp,                                                            \
struct xdr_stream *xdr,                                                 \
void *obj                                                               \
)                                                                       \
{                                                                       \
    XDR x;                                                              \
    bool_t stat;                                                        \
    x.x_op = XDR_ENCODE;                                                \
    x.x_origdata = x.x_data = (u8 *) xdr->p;                            \
    x.x_rq = rqstp;                                                     \
    x.x_limit = (u8 *) xdr->end;                                        \
    stat = xdr_##type##_req_t(&x, (type##_req_t *)obj);                 \
    MDKI_TRACE(TRACE_XDR,                                               \
               "xdr_" #type "_req: rval %d, x_data %p,"                 \
               " x_origdata %p, obj %p, iov_base %p, limit %p\n",       \
               stat, x.x_data, x.x_origdata, obj,                       \
               xdr->iov[0].iov_base, x.x_limit);                        \
    if (!stat) {                                                        \
        mvfs_log(MFS_LOG_ERR, "Failed to encode " #type "\n");          \
    }                                                                   \
    ASSERT(x.x_data <= x.x_limit);                                      \
    xdr_adjust_iovec(xdr->iov, (u32 *)x.x_data);                        \
    xdr->buf->len += (x.x_data - x.x_origdata);                         \
}                                                                       \
                                                                        \
/* return Linux error codes to RPC runtime */                           \
STATIC int                                                              \
mvfs_linux_xdr_decode_##type(                                           \
void *rqstp,                                                            \
struct xdr_stream *xdr,                                                 \
void *obj                                                               \
)                                                                       \
{                                                                       \
    XDR x;                                                              \
    bool_t stat;                                                        \
    x.x_op = XDR_DECODE;                                                \
    x.x_data = x.x_origdata = (u8 *) xdr->p;                            \
    x.x_rq = rqstp;                                                     \
    x.x_limit = (u8 *) xdr->end;                                        \
    stat = xdr_##type##_reply_t(&x, (type##_reply_t *)obj);             \
    MDKI_TRACE(TRACE_XDR,                                               \
               "xdr_" #type "_reply rval %d, x_data %p, x_origdata %p," \
               " x_limit %p, obj %p, iov_base %p, rlen %x\n",           \
               stat, x.x_data, x.x_origdata, x.x_limit, obj,            \
               xdr->iov->iov_base, rq->rq_rlen);                        \
    ASSERT(x.x_data <= x.x_limit);                                      \
    if (!stat) /* failure */                                            \
        return -ERANGE /* MVFS_RPC_CANTDECODERES */;                    \
   return 0 /* MVFS_RPC_SUCCESS */;                                     \
}

STATIC int
mvfs_linux_xdr_void(void)
{
    return 0;
}
#else /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32) */

#ifndef rq_rlen
#define rq_rlen rq_rcv_buf.len
#endif
#ifndef rq_rvec
#define rq_rvec rq_rcv_buf.head
#endif

/* don't need DECL variants, we just emit the functions and let them be their
   own prototypes. */
#define DECL_MVFS_VIEW_XDRFUNCS(type) DECL_MVFS_TYPE_XDRFUNCS(view_##type)
#define DECL_ALBD_VIEW_XDRFUNCS(type) DECL_MVFS_TYPE_XDRFUNCS(albd_##type)
#define DECL_MVFS_TYPE_XDRFUNCS(type)           \
	int mvfs_rhat_xdr_encode_##type(        \
            struct rpc_rqst *rq,                \
            u32 *data,                          \
            void *obj                           \
        );                                      \
	int mvfs_rhat_xdr_decode_##type(        \
            struct rpc_rqst *rq,                \
            u32 *data,                          \
            void *obj                           \
        )

#ifndef MAX
#define MAX(a,b) ((a) < (b) ? (b) : (a))
#endif

/*
 * The sizes listed in the RPC description should be the on-the-wire
 * encoding sizes.  That's not easy to compute, so we just make sure
 * we've got plenty of space using a similar scaling to what Linux
 * routines use.
 */
#define RPC_BUFSZ(type) (MAX(sizeof(type##_req_t),sizeof(type##_reply_t))<<2)

/* The struct rpc_rqst changes based on kernel version, and we can't
** test for that in the macro below, so set it up here.
*/
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
#define RPC_RQ_LIMIT (u8 *)rq->rq_svec[0].iov_base + rq->rq_svec[0].iov_len
#else
#define RPC_RQ_LIMIT (u8 *)rq->rq_svec[0].iov_base + rq->rq_snd_buf.buflen
#endif

#define XDR_RPC_FUNCS(type)                                             \
/* return Linux error codes to RPC runtime */                           \
STATIC int                                                              \
mvfs_linux_xdr_encode_##type(                                           \
    struct rpc_rqst *rq,                                                \
    u32 *data,                                                          \
    void *obj                                                           \
)                                                                       \
{                                                                       \
    XDR x;                                                              \
    bool_t stat;                                                        \
    x.x_op = XDR_ENCODE;                                                \
    x.x_origdata = x.x_data = (u8 *)data;                               \
    x.x_rq = rq;                                                        \
    x.x_limit = RPC_RQ_LIMIT;                                           \
    stat = xdr_##type##_req_t(&x, (type##_req_t *)obj);                 \
    MDKI_TRACE(TRACE_XDR,                                               \
               "xdr_" #type "_req: rval %d, x_data %p,"                 \
               " x_origdata %p, obj %p, iov_base %p, limit %p\n",       \
               stat, x.x_data, x.x_origdata, obj,                       \
               rq->rq_svec[0].iov_base, x.x_limit);                     \
    ASSERT(x.x_data <= x.x_limit);                                      \
    rq->rq_slen = xdr_adjust_iovec(rq->rq_svec, (u32 *)x.x_data);       \
    if (!stat) /* failure */                                            \
        return -EOVERFLOW /* MVFS_RPC_CANTENCODEARGS */;                \
   return 0 /* MVFS_RPC_SUCCESS */;                                     \
}                                                                       \
                                                                        \
/* return Linux error codes to RPC runtime */                           \
STATIC int                                                              \
mvfs_linux_xdr_decode_##type(                                           \
    struct rpc_rqst *rq,                                                \
    u32 *data,                                                          \
    void *obj                                                           \
)                                                                       \
{                                                                       \
    XDR x;                                                              \
    bool_t stat;                                                        \
    x.x_op = XDR_DECODE;                                                \
    x.x_data = x.x_origdata = (u8 *)data;                               \
    x.x_rq = rq;                                                        \
    x.x_limit = (u8 *)rq->rq_rvec[0].iov_base + rq->rq_rlen;            \
    stat = xdr_##type##_reply_t(&x, (type##_reply_t *)obj);             \
    MDKI_TRACE(TRACE_XDR,                                               \
               "xdr_" #type "_reply rval %d, x_data %p, x_origdata %p," \
               " x_limit %p, obj %p, iov_base %p, rlen %x\n",           \
               stat, x.x_data, x.x_origdata, x.x_limit, obj,            \
               rq->rq_rvec[0].iov_base, rq->rq_rlen);                   \
    ASSERT(x.x_data <= x.x_limit);                                      \
    if (!stat) /* failure */                                            \
        return -ERANGE /* MVFS_RPC_CANTDECODERES */;                    \
   return 0 /* MVFS_RPC_SUCCESS */;                                     \
}

STATIC int
mvfs_linux_xdr_void(
    struct rpc_rqst *rq,
    u32 *data,
    void *obj
)
{
    return 0 /* MVFS_RPC_SUCCESS */;
}
#endif /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32) */

#define VIEW_XDR_FUNCS(type) XDR_RPC_FUNCS(view_##type)
#define ALBD_XDR_FUNCS(type) XDR_RPC_FUNCS(albd_##type)

/* The following macros lay down the actual entries in the
 * RPC tables used by linux.  These entries include the size of the
 * response buffer.  For most RPCs this can be calculated by taking
 * the sizeof the response structure.  There are some RPCs that return
 * variable length data.  Readdir is the prime example of this.  It
 * will return as much data as will fit in the response packet.  The
 * MVFS_RPC_PROCINFO_SZ macros adds a size argument to allow passing
 * a receive buffer size.  Linux RPC handling generates a CRC error and
 * a retry if the data received over the wire is longer than what is 
 * specified here.
 */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
# define MVFS_RPC_PROCINFO(proc, type) proc,    \
    (kxdreproc_t) mvfs_linux_xdr_encode_##type,  \
    (kxdrdproc_t) mvfs_linux_xdr_decode_##type,  \
    RPC_BUFSZ(type),                            \
    0
# define MVFS_RPC_PROCINFO_SZ(proc, type, size) \
    proc,                                       \
    (kxdreproc_t) mvfs_linux_xdr_encode_##type,  \
    (kxdrdproc_t) mvfs_linux_xdr_decode_##type,  \
    (size),                                     \
    0
#else
# define MVFS_RPC_PROCINFO(proc, type) proc,    \
    (kxdreproc_t) mvfs_linux_xdr_encode_##type,  \
    (kxdrdproc_t) mvfs_linux_xdr_decode_##type,  \
    RPC_REQ_BUFSZ(type),                        \
    RPC_REP_BUFSZ(type),                        \
    0
# define MVFS_RPC_PROCINFO_SZ(proc, type, size) \
    proc,                                       \
    (kxdreproc_t) mvfs_linux_xdr_encode_##type,  \
    (kxdrdproc_t) mvfs_linux_xdr_decode_##type,  \
    (size),                                     \
    (size),                                     \
    0
#endif

#define MVFS_VIEW_PROCINFO(proc,type) MVFS_RPC_PROCINFO(VIEW_##proc,view_##type)
#define MVFS_VIEW_PROCINFO_SZ(proc,type,size) \
                             MVFS_RPC_PROCINFO_SZ(VIEW_##proc,view_##type,size)
#define MVFS_ALBD_PROCINFO(proc,type) MVFS_RPC_PROCINFO(ALBD_##proc,albd_##type)

/* MFS_MAXRPCDATA is defined in mvfs_base.h which is not included by
 * mvfs_linux_fops.c so we defined a platform specific macro in mvfs_mdki.h
 * This check is to make sure they stay in synch.
 */
#if MVFS_LINUX_MAXRPCDATA != MFS_MAXRPCDATA
#error MFS_MAXRPCDATA and MVFS_LINUX_MAXRPCDATA out of synch.
#endif

VIEW_XDR_FUNCS(change_mtype);
VIEW_XDR_FUNCS(change_oid);
VIEW_XDR_FUNCS(cltxt);
VIEW_XDR_FUNCS(create);
VIEW_XDR_FUNCS(getattr);
VIEW_XDR_FUNCS(getprop);
VIEW_XDR_FUNCS(gpath);
VIEW_XDR_FUNCS(invalidate);             /* used for INVALIDATE_UUID */
VIEW_XDR_FUNCS(link);
VIEW_XDR_FUNCS(lookup);
VIEW_XDR_FUNCS(mkdir);
VIEW_XDR_FUNCS(readdir);
VIEW_XDR_FUNCS(readlink);
VIEW_XDR_FUNCS(remove);
VIEW_XDR_FUNCS(rename);
VIEW_XDR_FUNCS(replica_root);
VIEW_XDR_FUNCS(revalidate);
VIEW_XDR_FUNCS(rmdir);
VIEW_XDR_FUNCS(setattr);
VIEW_XDR_FUNCS(symlink);

/* This table's order must match the view RPC order in <view_rpc_kernel.h> */
static struct rpc_procinfo view_v4_procinfo[VIEW_NUM_PROCS] = {
    /* name, rpc-encode-func, rpc-decode-func, bufsiz, call count(?) */
    /*
     * NULLPROC is special, it's always slot zero, and we don't have
     * albd XDR routines for it
     */
    {NULLPROC,
     (kxdreproc_t) mvfs_linux_xdr_void,
     (kxdrdproc_t) mvfs_linux_xdr_void,
     8, /* just in case we need spare space */
     0},
    {/*MVFS_VIEW_PROCINFO(CONTACT, contact)*/},
    {/*MVFS_VIEW_PROCINFO(SERVER_EXIT, server_exit)*/},
    {MVFS_VIEW_PROCINFO(SETATTR, setattr)},
    {MVFS_VIEW_PROCINFO(CREATE, create)},
    {MVFS_VIEW_PROCINFO(REMOVE, remove)},
    {MVFS_VIEW_PROCINFO(RENAME, rename)},
    {MVFS_VIEW_PROCINFO(SYMLINK, symlink)},
    {MVFS_VIEW_PROCINFO(MKDIR, mkdir)},
    {MVFS_VIEW_PROCINFO(RMDIR, rmdir)},
    {MVFS_VIEW_PROCINFO_SZ(READDIR, readdir, MVFS_LINUX_MAXRPCDATA)},
    {/*MVFS_VIEW_PROCINFO(STATFS, statfs)*/},
    {MVFS_VIEW_PROCINFO(CLTXT, cltxt)},
    {MVFS_VIEW_PROCINFO(CHANGE_OID, change_oid)},
    {/*MVFS_VIEW_PROCINFO(READDIR_EXT, readdir_ext)*/},
    {MVFS_VIEW_PROCINFO(GPATH, gpath)},
    {MVFS_VIEW_PROCINFO(REVALIDATE, revalidate)},
    {/*MVFS_VIEW_PROCINFO(CLTXT_PNAME, cltxt_pname)*/},
    {MVFS_VIEW_PROCINFO(CHANGE_MTYPE, change_mtype)},
    {MVFS_VIEW_PROCINFO(INVALIDATE_UUID, invalidate)},
    {MVFS_VIEW_PROCINFO(LINK, link)},
    {MVFS_VIEW_PROCINFO(LOOKUP_V6, lookup)},
    {MVFS_VIEW_PROCINFO(GETATTR, getattr)},
    {MVFS_VIEW_PROCINFO(REPLICA_ROOT, replica_root)},
    {/*MVFS_VIEW_PROCINFO(lookup_ext)*/},
    {/*MVFS_VIEW_PROCINFO(create_container)*/},
    {/*MVFS_VIEW_PROCINFO(remove_container)*/},
    {/*MVFS_VIEW_PROCINFO(rename_container)*/},
    {/*MVFS_VIEW_PROCINFO(wink)*/},
    {MVFS_VIEW_PROCINFO(READLINK, readlink)},
    {/*MVFS_VIEW_PROCINFO(reload_spec)*/},
    {/*MVFS_VIEW_PROCINFO(bld_session_free)*/},
    {/*MVFS_VIEW_PROCINFO(events_get_vob)*/},
    {/*MVFS_VIEW_PROCINFO(vob_create)*/},
    {/*MVFS_VIEW_PROCINFO(find_oid)*/},
    {/*MVFS_VIEW_PROCINFO(spec_rule_format)*/},
    {/*MVFS_VIEW_PROCINFO(white_out)*/},
    {/*MVFS_VIEW_PROCINFO(unwhite_out)*/},
    {/*MVFS_VIEW_PROCINFO(bld_get_ref_time)*/},
    {/*MVFS_VIEW_PROCINFO(recover)*/},
    {/*MVFS_VIEW_PROCINFO(dump)*/},
    {/*MVFS_VIEW_PROCINFO(load)*/},
    {/*MVFS_VIEW_PROCINFO(cr_get)*/},
    {/*MVFS_VIEW_PROCINFO(do_promoted)*/},
    {/*MVFS_VIEW_PROCINFO(do_purge_cltxt)*/},
    {/*MVFS_VIEW_PROCINFO(vob_save_path)*/},
    {/*MVFS_VIEW_PROCINFO(bld_session_flags)*/},
    {/*MVFS_VIEW_PROCINFO(get_text_mode)*/},
    {/*MVFS_VIEW_PROCINFO(cr_add_vob_ref_uuid)*/},
    {/*MVFS_VIEW_PROCINFO(cr_rm_vob_ref_uuid)*/},
    {/*MVFS_VIEW_PROCINFO(vob_get_path_uuid)*/},
    {/*MVFS_VIEW_PROCINFO(vob_rm_path_uuid)*/},
    {/*MVFS_VIEW_PROCINFO(vob_recover_object_uuid)*/},
    {/*MVFS_VIEW_PROCINFO(cr_save)*/},
    {MVFS_VIEW_PROCINFO(GETPROP, getprop)},
    {/*MVFS_VIEW_PROCINFO(setprop)*/},
    {/*MVFS_VIEW_PROCINFO(lic_check)*/},
    {/*MVFS_VIEW_PROCINFO(inventory_uuids)*/},
    {/*MVFS_VIEW_PROCINFO(setwork_get_vobs)*/},
    {/*MVFS_VIEW_PROCINFO(server_exit_rmtag)*/},
    {/*MVFS_VIEW_PROCINFO(statistics)*/},
    {/*MVFS_VIEW_PROCINFO(set_values)*/},
    {/*MVFS_VIEW_PROCINFO(inventory_vobs)*/},
    {/*MVFS_VIEW_PROCINFO(setwork_set_vobs)*/},
    {/*MVFS_VIEW_PROCINFO(setwork_cleanup)*/},
    {/*MVFS_VIEW_PROCINFO(hlink_set_vobs)*/},
    {/*MVFS_VIEW_PROCINFO(hlink_cleanup)*/},
    {/*MVFS_VIEW_PROCINFO(hlink_get_vobs)*/},
    {/*MVFS_VIEW_PROCINFO(ws_create_db)*/},
    {/*MVFS_VIEW_PROCINFO(frz_get_scopes)*/},
    {/*MVFS_VIEW_PROCINFO(frz_get_freeze_state)*/},
    {/*MVFS_VIEW_PROCINFO(frz_get_reload_info)*/},
    {/*MVFS_VIEW_PROCINFO(ws_begin_load_session)*/},
    {/*MVFS_VIEW_PROCINFO(ws_end_load_session)*/},
    {/*MVFS_VIEW_PROCINFO(frz_ok_to_set_spec)*/},
    {/*MVFS_VIEW_PROCINFO(ws_upd_wso_attrs)*/},
    {/*MVFS_VIEW_PROCINFO(ws_get_scope_aliases)*/},
    {/*MVFS_VIEW_PROCINFO(ws_get_unvisited_wsos)*/},
    {/*MVFS_VIEW_PROCINFO(ws_invalidate_object)*/},
    {/*MVFS_VIEW_PROCINFO(ws_unload_one_object)*/},
    {/*MVFS_VIEW_PROCINFO(frz_get_num_frozen)*/},
    {/*MVFS_VIEW_PROCINFO(ws_get_obj_scopes)*/},
    {/*MVFS_VIEW_PROCINFO(ws_rename_object)*/},
    {/*MVFS_VIEW_PROCINFO(ws_is_frozen_object)*/},
    {/*MVFS_VIEW_PROCINFO(reload_spec_ext)*/},
    {/*MVFS_VIEW_PROCINFO(ws_is_modified_wso_ext)*/},
    {/*MVFS_VIEW_PROCINFO(protect_stg_as_client)*/},
    {/*MVFS_VIEW_PROCINFO(protect_stg_check)*/},
    {/*MVFS_VIEW_PROCINFO(unprotect_stg_as_client)*/},
    {/*MVFS_VIEW_PROCINFO(unprotect_stg_check)*/},
    {/*MVFS_VIEW_PROCINFO(reparent_vob)*/},
    {/*MVFS_VIEW_PROCINFO(getown_sid)*/},
    {/*MVFS_VIEW_PROCINFO(ws_load_one_object)*/},
    {/*MVFS_VIEW_PROCINFO(ws_load_one_slink)*/},
    {/*MVFS_VIEW_PROCINFO(get_config_spec)*/},
    {/*MVFS_VIEW_PROCINFO(set_config_spec)*/},
    {/*MVFS_VIEW_PROCINFO(replace_container)*/},
    {/*MVFS_VIEW_PROCINFO(protect_container)*/},
    {/*MVFS_VIEW_PROCINFO(fstat_container)*/},
    {/*MVFS_VIEW_PROCINFO(ws_unload_one_object_ext)*/},
    {MVFS_VIEW_PROCINFO(LOOKUP, lookup)},
};

#if VIEW_SERVER_VERS != 4
#error Must change this file when view_server RPC program version changes
#endif
static struct rpc_version view_version_4 = {
    VIEW_SERVER_VERS,
    sizeof(view_v4_procinfo)/sizeof(view_v4_procinfo[0]),
    view_v4_procinfo
};

/* This is stupid, why can't the RPC code look for a version pointed
 * to by an element here with the right version # within?
 * (see linux/net/sunrpc/clnt.c:rpc_create_client().  sigh).
 */
static struct rpc_version *view_versions[] = {
    NULL,
    NULL,
    NULL,
    NULL,
    &view_version_4
};

struct rpc_program mvfs_view_program = {
    "view",
    VIEW_SERVER,
    sizeof(view_versions)/sizeof(view_versions[0]),
    view_versions,
    &view_rpc_stats
};

ALBD_XDR_FUNCS(find_server_v70);

ALBD_XDR_FUNCS(find_server);

static struct rpc_procinfo albd_v3_procinfo[ALBD_NUM_PROCS] = {
    /* name, rpc-encode-func, rpc-decode-func, bufsiz, call count(?) */
    {/*NULL*/},
    {/*MVFS_ALBD_PROCINFO(CONTACT, contact)*/},
    {/*MVFS_ALBD_PROCINFO(REGISTER_SERVER, register_server)*/},
    {MVFS_ALBD_PROCINFO(FIND_SERVER_V70,find_server_v70)},
    {/*ALBD_SERVER_IDLE,*/},
    {/*ALBD_SERVER_BUSY,*/},
    {/*ALBD_UNUSED_6,*/},
    {/*ALBD_UNUSED_7,*/},
    {/*ALBD_UNUSED_8,*/},
    {/*ALBD_UNUSED_9,*/},
    {/*ALBD_UNUSED_10,*/},
    {/*ALBD_SCHED_INFO,*/},
    {/*ALBD_UNUSED_12,*/},
    {/*ALBD_REGISTRY_GET_ID,*/},
    {/*ALBD_REGISTRY_FINDBYSTRING,*/},
    {/*ALBD_REGISTRY_FINDBYUUID,*/},
    {/*ALBD_REGISTRY_GET,*/},
    {/*ALBD_REGISTRY_ADD,*/},
    {/*ALBD_REGISTRY_REMOVE,*/},
    {/*ALBD_SERVER_ALTERNATE_UUID,*/},
    {/*ALBD_REGISTRY_CHK_ACCESS,*/},
    {/*ALBD_REGISTRY_GET_DTM,*/},
    {/*ALBD_LIST_SERVERS_V70,*/},
    {/*ALBD_GET_LOCAL_PATH,*/},
    {/*ALBD_CLNT_LIST_LOOKUP_V70,*/},
    {/*ALBD_LICENSE_GET_PRODUCT,*/},
    {/*ALBD_CLNT_LIST_GET_V70,*/},
    {/*ALBD_HOSTINFO,*/},
    {/*ALBD_CLNT_LIST_REGISTER,*/},
    {/*ALBD_REGISTRY_GET_DB_LIST,*/},
    {/*ALBD_REGISTRY_CLNT_CONF,*/},
    {/*ALBD_REGISTRY_SVR_CONF,*/},
    {/*ALBD_REGISTRY_GET_BACKUP,*/},
    {/*ALBD_REGISTRY_SET_BACKUP,*/},
    {/*ALBD_REGISTRY_FINDBYATTR,*/},
    {/* ALBD_UNUSED_35,*/},
    {/* ALBD_SCHED_GET_JOBS,*/},
    {/* ALBD_SCHED_HAS_INFO_CHANGED,*/},
    {/* ALBD_SCHED_GET_TASKS,*/},
    {/* ALBD_SCHED_GET_ACL,*/},
    {/* ALBD_SCHED_SET_ACL,*/},
    {/* ALBD_SCHED_JOB_CREATE,*/},
    {/* ALBD_SCHED_JOB_DELETE,*/},
    {/* ALBD_SCHED_JOB_LOOKUP_BY_ID,*/},
    {/* ALBD_SCHED_JOB_GET_PROPERTIES,*/},
    {/* ALBD_SCHED_JOB_SET_PROPERTIES,*/},
    {/* ALBD_SCHED_JOB_HAS_INFO_CHANGED,*/},
    {/* ALBD_SCHED_RJOB_GET_HANDLE,*/},
    {/* ALBD_SCHED_RJOB_GET_COMPLETION_INFO,*/},
    {/* ALBD_SCHED_RJOB_RUN_JOB,*/},
    {/* ALBD_SCHED_RJOB_TERMINATE,*/},
    {/* ALBD_SCHED_JOB_LOOKUP_BY_NAME,*/},
    {/* ALBD_SCHED_TASK_EXISTS,*/},
    {/* ALBD_SCHED_TASK_NAME_TO_ID,*/},
    {/* ALBD_SCHED_CHECK_ACC,*/},
    {/* ALBD_SCHED_TASK_ID_TO_NAME,*/},
    {/* ALBD_SCHED_GET_APP_PERMS,*/},
    {/* ALBD_LICENSE_CLEARCASE_AUTHENTICATED_URL,*/},
    {/* ALBD_SCHED_GET_TIME,*/},
    {/* ALBD_SCHED_CONTACT,*/},
    {/* ALBD_REMOTE_BUILD_HI,*/},
    {/* ALBD_LICENSE_CHECK_SID,*/},
    {/* ALBD_LICENSE_SID_STATS,*/},
    {/* ALBD_LICENSE_REVOKE_SID,*/},
    {/* ALBD_ELCC_FIND_SERVER_V70,*/},
    {/* ALBD_ELCC_IS_ELCC,*/},
    {/* ALBD_TZINFO,*/},
    {/* ALBD_TOGGLE_SERVER_RESTART,*/},
    {/* ALBD_SCHED_JOB_CREATE_UTC,*/},
    {/* ALBD_SCHED_JOB_GET_PROPERTIES_UTC,*/},
    {/* ALBD_SCHED_JOB_SET_PROPERTIES_UTC,*/},
    {MVFS_ALBD_PROCINFO(FIND_SERVER,find_server)},
    /* Don't need any of the rest */
};

#if ALBD_SERVER_VERS != 3
#error Must change this file when albd_server RPC program version changes
#endif
static struct rpc_version albd_version_3 = {
    ALBD_SERVER_VERS,
    sizeof(albd_v3_procinfo)/sizeof(albd_v3_procinfo[0]),
    albd_v3_procinfo
};

/* This is stupid, why can't the RPC code look for a version pointed
 * to by an element here with the right version # within?
 * (see linux/net/sunrpc/clnt.c:rpc_create_client().  sigh).
 */
static struct rpc_version *albd_versions[] = {
    NULL,
    NULL,
    NULL,
    &albd_version_3
};

struct rpc_program mvfs_albd_program = {
    "albd",
    ALBD_SERVER,
    sizeof(albd_versions)/sizeof(albd_versions[0]),
    albd_versions,
    &albd_rpc_stats
};

/*
 * There do not appear to be any locking protocols needed for RPC
 * client stuff.  RPC code handles internal structure locking itself
 */
int
mvfs_linux_clntkudp_create(
    struct sockaddr *addr,
    struct mfs_callinfo *trait,
    int retrans_count,
    bool_t intr,
    CLIENT **cl_pp
)
{
    struct rpc_program *prog;

    switch (trait->proto) {
      case VIEW_SERVER:
        prog = &mvfs_view_program;
        break;
      case ALBD_SERVER:
        prog = &mvfs_albd_program;
        break;
      default:
        mvfs_log(MFS_LOG_ERR,
                 "RPC: can't talk to anybody but view or albd,"
                 " asked for program %d!\n", trait->proto);
        return EINVAL;
    }

    return mdki_linux_clntkudp_create(addr, trait->version, prog,
                                      retrans_count, intr, cl_pp);
}

#define MVFS_XDR_INTEGRAL_TYPE_OBJ(type,objtype)                        \
bool_t                                                                  \
xdr_##type(                                                             \
    XDR *x,                                                             \
    objtype *obj                                                        \
)                                                                       \
{                                                                       \
    u_int local;                                                        \
    u_int *publicp;                                                     \
                                                                        \
    /* assume we have space */                                          \
    switch (x->x_op) {                                                  \
      case XDR_ENCODE:                                                  \
        local = (u_int)*obj;                                            \
        publicp = (u_int *)x->x_data;                                   \
        *publicp = htonl(local);                                        \
        x->x_data += sizeof(u_int);                                     \
        return TRUE;                                                    \
      case XDR_DECODE:                                                  \
        publicp = (u_int *)x->x_data;                                   \
        local = *publicp;                                               \
        x->x_data += sizeof(u_int);                                     \
        *obj = (objtype)ntohl(local);                                   \
        return TRUE;                                                    \
      default:                                                          \
        mvfs_log(MFS_LOG_DEBUG, "xdr_" #type " called with op=%d!\n",   \
                 x->x_op);                                              \
        return FALSE;                                                   \
    }                                                                   \
}

#define MVFS_XDR_INTEGRAL_TYPE(type) MVFS_XDR_INTEGRAL_TYPE_OBJ(type,type)

MVFS_XDR_INTEGRAL_TYPE(u_long)
MVFS_XDR_INTEGRAL_TYPE(u_int)
MVFS_XDR_INTEGRAL_TYPE(u_short)
MVFS_XDR_INTEGRAL_TYPE(long)
MVFS_XDR_INTEGRAL_TYPE(int)
MVFS_XDR_INTEGRAL_TYPE_OBJ(bool,bool_t)
MVFS_XDR_INTEGRAL_TYPE_OBJ(enum,int)

extern bool_t
xdr_opaque(
    XDR *xdrs,
    char *cp,
    u_int cnt
);
/*
 * A string is encoded as a count plus an opaque collection of its size.
 */
bool_t
xdr_string(
    XDR *x,
    char **obj,
    u_int maxsize
)
{
    int len;
    bool_t val;
    char *cp = *obj;
    u8 *odata;

    switch (x->x_op) {
      case XDR_ENCODE:
        len = STRLEN(cp);
      default:
        break;
      case XDR_FREE:
        mvfs_log(MFS_LOG_DEBUG, "%s called with free?\n", __func__);
        return FALSE;
    }
    
    val = xdr_u_int(x, &len);
    odata = x->x_data;
/*     mdki_linux_printf("xdr_string at obj=%lx, data=%lx, len=%d, max=%d\n", */
/*            *obj, x->x_public, len, maxsize); */
    if (!val || len > maxsize)
        return FALSE;                   /* encode or decode, wouldn't fit! */
    val = xdr_opaque(x, cp, len);
    if (val && x->x_op == XDR_DECODE) {
        *(cp + len) = '\0';
    }
/*     mdki_linux_printf("xdr_string rval=%d new data=%lx, packed=%d\n", */
/*            val, x->x_public, (u8 *)x->x_public - odata); */
    return val;
}

extern bool_t
xdr_void(
    XDR *xdrs,
    void *cp
)
{
    return TRUE;
}

extern void
mvfs_linux_clnt_get_servaddr(
    CLIENT *cl,
    char *saddr
)
{
    /* Needs updating once the kernel RPC code handles IPv6 */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
    *((struct sockaddr_in *)saddr) = cl->cl_xprt->addr;
#else
    memcpy(saddr, &cl->cl_xprt->addr, sizeof(struct sockaddr_in));
#endif
}

void
mvfs_linux_clnt_geterr(
/*    CLIENT *cl,*/
    struct rpc_err *errp
)
{
    /* Can't get details out of the client struct? None to be found. */
    errp->re_errno = EIO;               /* return this to RPC caller */
}

const char *
mvfs_linux_clnt_sperrno(enum clnt_stat status)
{
    switch (status) {
      case RPC_SUCCESS:
        return "RPC successful";
      case RPC_PROGUNAVAIL:
        return "Program unavailable";
      case RPC_PROCUNAVAIL:
        return "Procedure unavailable";
      case RPC_AUTHERROR:
        return "Authentication error";
      case RPC_CANTENCODEARGS:
        return "Cannot encode arguments";
      case RPC_CANTDECODEARGS:
        return "Cannot decode arguments";
      case RPC_CANTDECODERES:
        return "Cannot decode results";
      case RPC_VERSMISMATCH:
        return "RPC version mismatch";
      case RPC_PROGVERSMISMATCH:
        return "RPC program version mismatch";
      case RPC_INTR:
        return "RPC interrupted";
      case RPC_TIMEDOUT:
        return "RPC timed out";
      default:
        return "Unknown RPC error";
    }
}

void
mvfs_linux_vnprint(VNODE_T *vp)
{
    mfs_mnode_t *mnp;
    VNODE_T *vw;

    ASSERT(MFS_VPISMFS(vp));
    
    mnp = VTOM(vp);
    vw = MFS_VIEW(vp);
    mdki_linux_printf("mnvp=%p mnp=%p clas=%d cnt=%d vw=%p\n"
                      "vpvfs=%p mnvfs=%p realvp=%p\n"
                      "free=%d tdest=%d"
                      " odest=%d stale=%d\n",
                      vp, mnp, mnp->mn_hdr.mclass, mnp->mn_hdr.mcount, vw,
                      vp->v_vfsp, mnp->mn_hdr.vfsp, mnp->mn_hdr.realvp,
                      mnp->mn_hdr.mfree, mnp->mn_hdr.trans_destroy,
                      mnp->mn_hdr.on_destroy, mnp->mn_hdr.stale
    );
    if (mnp->mn_hdr.realvp)
        VOP_PRINT(mnp->mn_hdr.realvp);
}

mdki_boolean_t mvfs_linux_sync_print_vnodes = FALSE;

int
mvfs_linux_vsync_wrapper(
    VFS_T *vfsp,
    short flag,
    CRED_T *acred
)
{
    int err;
    err = mfs_vsync(vfsp, flag, acred);
#ifdef MVFS_DEBUG
    if (err != 0 || mvfs_linux_sync_print_vnodes) {
        /* XXX need a way to find all clrvnodes */
        mvfs_mnreport_leftover_vnodes(vfsp);
    }
#endif
    return err;
}

extern int 
mvfs_linux_lookup_ioctl(
    /* mfs_pn_char_t */char *path,
    int segflag,
    SYMFOLLOW_T follow,
    int opt,
    VNODE_T **dvpp,
    CLR_VNODE_T **vpp,
    CALL_DATA_T *cd
)
{
    int oldprog;
    mvfs_thread_t *mth = MVFS_CD2THREAD(cd);
    int rv;
    
    oldprog = mth->thr_threadid.no_bindroot;
    if (opt & MVFS_NB_LOOKUP)
        mth->thr_threadid.no_bindroot = TRUE;

    rv = mvop_linux_lookup_ioctl(path, segflag, follow, dvpp, vpp, 
                                 MVFS_CD2CRED(cd));

    mth->thr_threadid.no_bindroot = oldprog;
    return rv;
}

int
mvfs_linux_misc_init(mvfs_cache_sizes_t *sizes)
{
    return vnlayer_hijack_root_inode();
}

void
mvfs_linux_misc_free(void)
{
    vnlayer_restore_root_inode();
}

#ifdef RATL_COMPAT32
/*
 * In opteron kernels, this is called by our ioctl handler via the
 * MVFS_IS_IT_OUR_IOCTL() macro.
 *
 * We hijack that for the side-effect of recording the bit-size of our
 * ioctl command, since that determines the translation requirements.
 * (IOCTL_CMD_32 means it's a 32-bit layout in the process's address space)
 * See also mdki_curproc_is_32bit() (called by the MDKI_CALLER_IS_32BIT()
 * macro in all the copyin/copyout routines)
 */
tbs_boolean_t
mvfs_linux_ioctl_chk_cmd(
    int cmd,
    MVFS_CALLER_INFO *callinfo
)
{
    switch (cmd) {
      case MVFS_IOCTL_CMD_32:
        callinfo->caller_is_32bit = 1;
        return TRUE;
      case MVFS_IOCTL_CMD:
        callinfo->caller_is_32bit = 0;
        return TRUE;
      default:
        return FALSE;                   /* not ours */
    }
}
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
/* These functions are needed to save the file_lock owner so we can find it
** again at exit time (see mvop_linux_lockctl()).  They are defined here, but
** declared in mvfs_mdki.h, which is only included in vnlayer stuff.  In this
** case, in mvfs_linux_mvops.c.  The info is kept in the struct mvfs_proc in a
** Linux-only field.  We use mvfs_enter_fs() to make sure everything (i.e. the
** struct mvfs_proc) is set up.
*/
void
mvfs_linux_save_fl_owner(void *fl_owner)
{
    mvfs_thread_t *mth = mvfs_enter_fs();
    ASSERT(mth->thr_proc != NULL);
    /* We should never set the fl_owner to NULL */
    if (fl_owner == NULL) {
        mvfs_log(MFS_LOG_WARN,
                 "Attempting to set file lock owner %p to NULL\n",
                 mth->thr_proc->mp_fl_owner);
        goto err_exit;
    }

    /* If we're changing owners, then that's bad because we will lose track of
    ** the current one.  This shouldn't ever happen if I understand things
    ** correctly, so at least log it.
    */
    if (mth->thr_proc->mp_fl_owner != NULL &&
        mth->thr_proc->mp_fl_owner != fl_owner)
    {
        mvfs_log(MFS_LOG_ERR,
                 "file lock owner (fl_owner) changing from %p to %p\n",
                 mth->thr_proc->mp_fl_owner, fl_owner);
    }
    mth->thr_proc->mp_fl_owner = fl_owner;
err_exit:
    mvfs_exit_fs(mth);
}

void *
mvfs_linux_find_fl_owner(void)
{
    void *fl_owner;

    mvfs_thread_t *mth = mvfs_enter_fs();
    ASSERT(mth->thr_proc != NULL);
    fl_owner = mth->thr_proc->mp_fl_owner;
    mvfs_exit_fs(mth);
    return fl_owner;
}
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) */

/* Routines called from mvfs_linux_mdki.c to manipulate the thread
 * structures placed in the cred structures.  These functions
 * are declared in mvfs_mdki.h
 */

extern struct mvfs_thread *
mvfs_get_thread_ptr(void)
{
    if (mvfs_init_state == MVFS_INIT_COMPLETE)
        return((struct mvfs_thread *)mvfs_enter_fs());
    else
        return(NULL);
}

extern void
mvfs_release_thread_ptr(struct mvfs_thread *thr)
{
    mvfs_exit_fs(thr);
    return;
}

/*
 * This function replaces the default implementation of MVFS_STAT_ZERO.
 * See mvfs_mdep_linux.h.
 */
void
mvfs_linux_stat_zero(struct mvfs_statistics_data *sdp)
{
    BZERO(sdp, sizeof(*sdp));
    sdp->mfs_clntstat.version = MFS_CLNTSTAT_VERS;
    sdp->mfs_mnstat.version = MFS_MNSTAT_VERS;
    sdp->mfs_clearstat.version = MFS_CLEARSTAT_VERS;
    sdp->mfs_rvcstat.version = MFS_RVCSTAT_VERS;
    sdp->mfs_dncstat.version = MFS_DNCSTAT_VERS;
    sdp->mfs_acstat.version = MFS_ACSTAT_VERS;
    sdp->mfs_rlstat.version = MFS_RLSTAT_VERS;
    sdp->mfs_austat.version = MFS_AUSTAT_VERS;
    sdp->mfs_viewophist.version = MFS_RPCHIST_VERS;
}

/*
 * Linux NFS caches ENOENT pretty aggressively, without
 * reasonable revalidation behavior. 
 * What we will do is to walk up the stream to find a directory that
 * exists and call shrink_dcache_parent() to flush out the negative 
 * dentries.
 */
int
mvfs_linux_prod_parent_dir_cache(
    struct mfs_mnode *mnp,
    CRED_T *cred
)
{
    mfs_pn_char_t *dirname;
    int error = ENOENT;
    mfs_pn_char_t *tmpslash, *lastslash;
    void *rdfp;
    CLR_VNODE_T *cvp;

    dirname = PN_STRDUP(mnp->mn_vob.cleartext.nm);
    lastslash = tmpslash = STRRCHR(dirname, MVFS_PN_SEP_CHAR);
    mvfs_log(MFS_LOG_DEBUG, " Walking over %s to force NFS cache renewing\n",
             dirname);

    while (lastslash != NULL && lastslash != dirname) {
        *lastslash = '\0';

        error = LOOKUP_STORAGE_FILE(MFS_CLRTEXT_RO(mnp),
                                    dirname,
                                    NULL, &cvp, cred);
        if (error == 0) {
            /* shrink the dcache. */
            if (cvp->v_dent != NULL) {
                shrink_dcache_parent((DENT_T *)cvp->v_dent);
                CVN_RELE(cvp);
                break;
            }
            CVN_RELE(cvp);
        }
        /*
         * If we did not find a v_dent walk up the tree until we
         * get one.
         */
        lastslash = STRRCHR(dirname, MVFS_PN_SEP_CHAR);
        /*
         * restore original last slash (prepare string for STRFREE();
         * some platforms care about length)
         */
        *tmpslash = MVFS_PN_SEP_CHAR;
        tmpslash = lastslash;
    }
    if (tmpslash != NULL)
        *tmpslash = MVFS_PN_SEP_CHAR;

    PN_STRFREE(dirname);
    return error;
}

static const char vnode_verid_mvfs_mdep_linux_c[] = "$Id:  0da3739c.1df011e2.8579.00:01:84:c3:8a:52 $";
