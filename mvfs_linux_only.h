#ifndef MVFS_LINUX_ONLY_H_
#define MVFS_LINUX_ONLY_H_
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

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,4,21)
#error kernel version not supported
#endif

#if defined(SLE_VERSION_CODE) && defined(SLE_VERSION)
#if SLE_VERSION_CODE >= SLE_VERSION(10,2,0)
#define SLES10SP2
#endif
#endif

/*
 * types, macros, etc. for Linux MVFS.
 */
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,15)
#define LOCK_INODE(inode) down(&(inode)->i_sem)
#define UNLOCK_INODE(inode) up(&(inode)->i_sem)
#else
#define LOCK_INODE(inode) mutex_lock(&(inode)->i_mutex)
#define UNLOCK_INODE(inode) mutex_unlock(&(inode)->i_mutex)
#endif

#if !defined(USE_CHROOT_CURVIEW) && !defined(USE_ROOTALIAS_CURVIEW)
#define USE_ROOTALIAS_CURVIEW
#endif /* !USE_CHROOT_CURVIEW && !USE_ROOTALIAS_CURVIEW */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0) || (defined(RATL_REDHAT) && RATL_VENDOR_VER < 500)
#define NO_EXPORTED_LOOKUP_CREATE
#endif

/* Types for the various linux file dispatch tables */

#define SB_OPS_T struct super_operations
#define F_OPS_T  struct file_operations
#define IN_OPS_T struct inode_operations
#define DQ_OPS_T struct dquot_operations

#define FILE_T struct file
#define INODE_T struct inode
#define DENT_T struct dentry
#define SUPER_T struct super_block

/* Testing for an empty hash list in a dentry can be different on different
** versions.  In 2.6 they changed the dentry hash list to use a different kind
** of list.  Also, you might think hlist_empty() would be the thing to use, but
** that code assumes the d_bucket is non-NULL (and contains the addr of the
** hash bucket to which this dentry is linked).  So, a better test is if this
** dentry is currently unhashed (i.e. not on a hash chain), so that's what we
** use.
*/
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define DENT_HASH_IS_EMPTY(dent) list_empty(&((dent)->d_hash))
#else
#define DENT_HASH_IS_EMPTY(dent) hlist_unhashed(&((dent)->d_hash))
#endif

/* magic number for the mvfs superblock. */
#define MVFS_SUPER_MAGIC 0xc1eaca5e

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define LINUX_SB_PVT_FIELD u.generic_sbp
#else
#define LINUX_SB_PVT_FIELD s_fs_info
#endif
#define SBTOVFS(sb)     ((VFS_T *)(sb)->LINUX_SB_PVT_FIELD)
#define VFSTOSB(vfsp)   ((SUPER_T *)(vfsp)->vfs_sb)
/* Newer compilers deprecate the "use of cast expressions as lvalues", so use
** macros to set these.
*/
#define SET_SBTOVFS(sb, value) do {(sb)->LINUX_SB_PVT_FIELD = (void *)(value);} while(0)
#define SET_VFSTOSB(vfsp, value) do {(vfsp)->vfs_sb = (caddr_t)(value);} while(0)

/* There are different constants for ngroups, so let's define our own. */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define LINUX_NGROUPS NGROUPS
#define LINUX_TASK_NGROUPS(task) ((task)->ngroups)
#define LINUX_TASK_GROUPS(task) ((task)->groups)

typedef struct {
    uid_t old_fsuid;
    uid_t old_fsgid;
    int ngroups;
    gid_t groups[LINUX_NGROUPS];
} vnlayer_fsuid_save_struct_t, *vnlayer_fsuid_save_t;
#else
#define LINUX_NGROUPS NGROUPS_SMALL
#define LINUX_TASK_NGROUPS(task) ((task)->group_info->ngroups)
#define LINUX_TASK_GROUPS(task) ((task)->group_info->blocks[0])

typedef struct {
    uid_t old_fsuid;
    uid_t old_fsgid;
    struct group_info *saved_group_info;
} vnlayer_fsuid_save_struct_t, *vnlayer_fsuid_save_t;
#endif

extern mdki_boolean_t
vnlayer_fsuid_save(
    vnlayer_fsuid_save_t *save,
    CRED_T *cred
);

extern void
vnlayer_fsuid_restore(
    vnlayer_fsuid_save_t *saved
);

extern int
vnode_dop_delete(DENT_T *dentry);

extern int
vnode_iop_notify_change(
    DENT_T *dent_p,
    struct iattr * iattr_p
);

extern void
vnlayer_linux_adjust_uio(
    struct uio *uiop,
    ssize_t count,
    int do_offset
);

extern void
vnlayer_shadow_inode(
    INODE_T *real_inode,
    DENT_T *dentry,
    INODE_T *shadow_inode
);

struct vfsmount *
vnlayer_dent2vfsmnt(DENT_T *dentry);

extern DENT_T *vnlayer_sysroot_dentry;
extern DENT_T *
vnlayer_get_root_dentry(void);

extern struct vfsmount *vnlayer_sysroot_mnt;
extern struct vfsmount *
vnlayer_get_root_mnt(void);

extern V_OP_T mvop_cltxt_vnops;

#define VTOVFSMNT(vp)   ((struct vfsmount *)((vp)->v_vfsmnt))
/* Newer compilers deprecate the "use of cast expressions as lvalues", so use a
** macro to set one of these.
*/
#define SET_VTOVFSMNT(vp, value) do {(vp)->v_vfsmnt = (caddr_t)(value);} while(0)

#define I_COUNT(ip)     atomic_read(&(ip)->i_count)
/* SLES8 and 2.6 kernels have an inline function to better handle proper
 * reading and writing of the 64-bit i_size field on 32-bit systems.
 */
#if defined(SLES8) || LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,5)
#define READ_I_SIZE(ip) (i_size_read(ip))
#define WRITE_I_SIZE(ip, size) i_size_write(ip, size)
#else
#define READ_I_SIZE(ip) (ip)->i_size
#define WRITE_I_SIZE(ip, size) (ip)->i_size = size
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define ITOV(ip)        ((struct mdki_vnode *)(&(ip)->u))
#define VTOI(vp)        ((struct inode *)((caddr_t)(vp) - offsetof(struct inode, u)))
#else
/* In Linux 2.4 there was enough space in the inode to hold our vnode, so we
** managed them as a "unit".  In 2.6 they've done the right thing and just put
** a generic_ip pointer in the inode (which we will use to point to the vnode).
** However, since we need to be able to get from an inode to a vnode even if
** the inode isn't filled in yet, we will define a structure to "keep them
** together" in consecutive storage for allocation and freeing purposes and let
** these macros sort everything out.  We define VNODE_T in mvfs_mdki.h, but we
** don't use this struct very many places, so we don't need a macro for it.
*/
typedef struct vnlayer_vnode {
    VNODE_T       vnl_vnode;
    struct inode  vnl_inode;    /* This should always be last for VTOI. */
} vnlayer_vnode_t;

/* I think it could be "ITOV(ip) ((VNODE_T *)((ip)->generic_ip))", but
** I'm not sure (e.g. it might not be filled in yet), so let's be safe.
*/
#define ITOV(ip)    ((VNODE_T *)container_of((ip), vnlayer_vnode_t, vnl_inode))
#define VTOI(vp)    ((struct inode *)(&(((vnlayer_vnode_t *)(vp))->vnl_inode)))
#endif

extern struct file_system_type mvfs_file_system;

extern IN_OPS_T vnode_file_inode_ops;
extern IN_OPS_T vnode_file_mmap_inode_ops;
extern IN_OPS_T vnode_dir_inode_ops;
extern IN_OPS_T vnode_slink_inode_ops;
extern IN_OPS_T vnlayer_clrvnode_iops;

#if !(defined(RATL_REDHAT) && (RATL_VENDOR_VER >= 400)) && \
	LINUX_VERSION_CODE < KERNEL_VERSION(2,6,10)
#define HAVE_SHADOW_FILES
#endif

/* This needs to be true only if this is a real mnode-type vnode (don't
 * return true for a shadow vnode!
 */
#define MDKI_INOISMVFS(ino) (((ino) != NULL) &&                            \
                             ((ino)->i_sb->s_type == &mvfs_file_system) && \
                             ((ino)->i_op == &vnode_file_inode_ops ||      \
                              (ino)->i_op == &vnode_file_mmap_inode_ops || \
                              (ino)->i_op == &vnode_dir_inode_ops ||       \
                              (ino)->i_op == &vnode_slink_inode_ops))
#define MDKI_SBISMVFS(sb) ((sb)->s_type == &mvfs_file_system)
#define MDKI_INOISOURS(ino) (((ino) != NULL) && ((ino)->i_sb->s_type == &mvfs_file_system))
#ifdef HAVE_SHADOW_FILES
#define MDKI_INOISSHADOW(ino)    (((ino) != NULL) && ((ino)->i_sb->s_type == &mvfs_file_system) && ((ino)->i_op == &vnode_shadow_reg_inode_ops || (ino)->i_op == &vnode_shadow_slink_inode_ops))
#else /* ! HAVE_SHADOW_FILES */
#define MDKI_INOISSHADOW(ino)    (((ino) != NULL) && ((ino)->i_sb->s_type == &mvfs_file_system) && (ino)->i_op == &vnode_shadow_slink_inode_ops)
#endif /* HAVE_SHADOW_FILES */
#define MDKI_INOISCLRVN(ino) (((ino) != NULL) && (((ino)->i_sb->s_type == &mvfs_file_system) && ((ino)->i_op == &vnlayer_clrvnode_iops)))

extern DENT_T *
vnlayer_inode2dentry(
    INODE_T *ip,
    struct dentry_operations *ops,
    const char *file,
    const char *func,
    int line,
    char *retpc
);
extern DENT_T *
vnode_dget(
    DENT_T *dentry,
    const char *file,
    const char *func,
    int line
);
extern void
vnode_dput(
    DENT_T *dentry,
    const char *file,
    const char *func,
    int line
);
extern DENT_T *
vnode_d_alloc_root(
    INODE_T * rootvp,
    const char *file,
    const char *func,
    int line
);
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
extern DENT_T *
vnode_d_alloc_anon(
    INODE_T * rootvp,
    const char *file,
    const char *func,
    int line
);
extern DENT_T *
vnode_d_splice_alias(
    INODE_T *ip,
    DENT_T *dent,
    const char *file,
    const char *func,
    int line
);
#endif
extern DENT_T *
vnode_d_alloc(
    DENT_T *parent,
    const struct qstr *name,
    const char *file,
    const char *func,
    int line
);
extern void
vnode_d_add(
    DENT_T *dent,
    INODE_T *ip,
    const char *file,
    const char *func,
    int line
);
extern void
vnode_d_instantiate(
    DENT_T *dent,
    INODE_T *ip,
    const char *file,
    const char *func,
    int line
);

#define MVOP_DENT(ip,ops) vnlayer_inode2dentry(ip, ops, __FILE__,__func__,__LINE__, __builtin_return_address(0))
#ifdef MVFS_DEBUG
#define VNODE_DGET(dent) vnode_dget(dent,__FILE__,__func__,__LINE__)
#define VNODE_DPUT(dent) vnode_dput(dent,__FILE__,__func__,__LINE__)
#define VNODE_D_ALLOC_ROOT(vp) vnode_d_alloc_root(vp,__FILE__,__func__,__LINE__)
#define VNODE_D_ALLOC_ANON(ip) vnode_d_alloc_anon(ip,__FILE__,__func__,__LINE__)
#define VNODE_D_ALLOC(parent,name) vnode_d_alloc(parent, name,__FILE__,__func__,__LINE__)
#define VNODE_D_ADD(dent,ino) vnode_d_add(dent,ino,__FILE__,__func__,__LINE__)
#define VNODE_D_SPLICE_ALIAS(ino,dent) vnode_d_splice_alias(ino,dent,__FILE__,__func__,__LINE__)
#define VNODE_D_INSTANTIATE(dent,ino) vnode_d_instantiate(dent,ino,__FILE__,__func__,__LINE__)
#else
#define VNODE_DGET(dent) dget(dent)
#define VNODE_DPUT(dent) dput(dent)
#define VNODE_D_ALLOC_ROOT(vp) d_alloc_root(vp)
#define VNODE_D_ALLOC_ANON(ip) d_alloc_anon(ip)
#define VNODE_D_ALLOC(parent,name) d_alloc(parent, name)
#define VNODE_D_ADD(dent,ino) d_add(dent,ino)
#define VNODE_D_SPLICE_ALIAS(ino,dent) d_splice_alias(ino,dent)
#define VNODE_D_INSTANTIATE(dent,ino) d_instantiate(dent,ino)
#endif

#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
#define CVN_CREATE(dent, mnt) vnlayer_linux_new_clrvnode(dent, mnt, __FILE__, __func__, __LINE__)
extern VNODE_T *
vnlayer_linux_new_clrvnode(
    DENT_T *dent,
    struct vfsmount *mnt,
    const char *file,
    const char *func,
    int line
);
#else
#define CVN_CREATE(dent,mnt) vnlayer_linux_new_clrvnode(dent,mnt)
extern VNODE_T *
vnlayer_linux_new_clrvnode(
    DENT_T *dent,
    struct vfsmount *mnt
);
#endif
extern void
vnlayer_linux_free_clrvnode(VNODE_T *cvp);

static inline DENT_T *
cvn_to_dent(VNODE_T *cvn)
{
    if (cvn == NULL) {
        return NULL;
    }
    return (struct dentry *)cvn->v_dent;
}

#define CVN_TO_DENT(cvn) (cvn_to_dent(cvn))
#define CVN_TO_INO(cvn) (CVN_TO_DENT(cvn)->d_inode)

static inline struct vfsmount *
cvn_to_vfsmnt(VNODE_T *cvn)
{
    if (cvn->v_vfsmnt == NULL)
        BUG();
    return (struct vfsmount *)cvn->v_vfsmnt;
}
#define CVN_TO_VFSMNT(cvn) cvn_to_vfsmnt(cvn)

extern DENT_T *
vnlayer_make_dcache(
    VNODE_T *dvp,
    VNODE_T *vp,
    const char *nm
);

extern INODE_T *
vnlayer_get_urdir_inode(void);

extern void
vnlayer_set_urdent(
    DENT_T *new_rdir,
    struct vfsmount *new_rmnt
);
extern INODE_T *
vnlayer_get_ucdir_inode(void);

#define F_COUNT(x) file_count(x)
#define F_COUNT_INC(x) get_file(x)
#define F_COUNT_DEC(x) atomic_dec(&(x)->f_count)
#define D_COUNT(x) ((x) ? atomic_read(&(x)->d_count) : 0)
#define D_COUNT_INC(x) atomic_inc(&(x)->d_count)
#define D_COUNT_DEC(x) atomic_dec(&(x)->d_count)
#define I_WRITECOUNT(x) atomic_read(&(x)->i_writecount)
#define I_WRITECOUNT_DEC(x) atomic_dec(&(x)->i_writecount)
#define I_WRITECOUNT_INC(x) atomic_inc(&(x)->i_writecount)

extern int
vnlayer_truncate_inode(
    struct dentry *dentry,
    struct vfsmount *mnt,
    loff_t length,
    mdki_boolean_t from_open
);

extern int
vnlayer_lookup_create(
    struct nameidata *nd,
    mdki_boolean_t is_dir,
    struct dentry **dpp
);

extern int
vnlayer_do_linux_link(
    VNODE_T *tdvp,
    DENT_T *olddent,
    INODE_T *parent,
    DENT_T * newdent
);

extern int
vnlayer_has_mandlocks(struct inode *ip);

#define REALFILE(filep)      ((FILE_T *)(filep)->private_data)
/* Newer compilers deprecate the "use of cast expressions as lvalues", so use a
** macro to set one of these.
*/
#define SET_REALFILE(filep, value) do {(filep)->private_data = (void *)(value);} while(0)

extern struct dentry_operations vnode_dentry_ops;
extern struct dentry_operations vnode_setview_dentry_ops;
extern struct address_space_operations mvfs_addrspace_ops;
extern F_OPS_T vnode_file_file_ops, vnode_file_mmap_file_ops, vnode_dir_file_ops;
extern SB_OPS_T mvfs_super_ops;

extern mdki_boolean_t mvfs_panic_assert;
#if defined(MVFS_CRITICAL_SECTION_ASSERTS) || defined(MVFS_DEBUG)
#define DEBUG_ASSERT(EX) { \
    if (!(EX))  \
        mdki_assfail("Assertion failed: %s,%s,%s,line=%d\n", \
                #EX, __FILE__, __func__, __LINE__); \
}
#else
#define DEBUG_ASSERT(EX)
#endif

#ifdef MVFS_INHOUSE_NONPRODUCTION_ASSERTS
#undef ASSERT
#define ASSERT(EX)  {  \
   if (mvfs_panic_assert) {  \
      if (!(EX))  \
        mdki_assfail("Assertion failed: %s,%s,%s,line=%d\n", \
                      #EX, __FILE__, __func__, __LINE__); \
   }   \
   else {   \
      if (!(EX))  \
         MDKI_VFS_LOG(VFS_LOG_ERR, "ASSERT FAILED: [%s #%d]\n", __FILE__, __LINE__);   \
   }   \
}
#else
#undef ASSERT
#define ASSERT(EX)
#endif

#if defined(MVFS_DEBUG)

#if defined(CONFIG_SMP) && (CONFIG_SMP != 0)
#define ASSERT_KERNEL_LOCKED()   ASSERT(kernel_locked())
/* It's not possible to safely assert BKL not held, because another
 * CPU might hold it for another process.  (No ownership is recorded
 * on spinlocks in Linux.  Grr.)  Or, some high-level routine might
 * hold it but the lower-layers don't need it.
 */
#define ASSERT_KERNEL_UNLOCKED() /* ASSERT(!kernel_locked()) */
#define ASSERT_DCACHE_LOCKED()   ASSERT(spin_is_locked(&dcache_lock))
/* Likewise */
#define ASSERT_DCACHE_UNLOCKED() /* ASSERT(!spin_is_locked(&dcache_lock))*/
#else /* not SMP */
/* non-SMP kernels do not include any spinlocks, and they define
 * spin_is_locked() to always return false.  So we don't do any asserts
 * for BKL or spin-locks on non-SMP kernels.
 */
#define ASSERT_KERNEL_LOCKED()
#define ASSERT_KERNEL_UNLOCKED()
#define ASSERT_DCACHE_LOCKED()
#define ASSERT_DCACHE_UNLOCKED()
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define ASSERT_KERNEL_LOCKED_24x() ASSERT_KERNEL_LOCKED()
#else
#define ASSERT_KERNEL_LOCKED_24x()
#endif

/* So far, all CPUs use similar semaphore internals, defined in
 * CPU-specific header files (in asm/semaphore.h).
 */

/*
 * Linux doesn't provide a very rich set of functions on semaphores, currently
 * only: down, down_interruptible, down_trylock, and up.  Thus, we may have to
 * reach into the semaphore structure and look at the count to do some of our
 * checks (which are still too crude without OS support).  We isolate it all
 * here in these macros so we can fix when they add something like
 * sema_is_locked, and sema_is_locked_by_me.  Note, there is also a window if
 * you only test a lock, i.e. it could change state after you test.  Therefore,
 * you should probably only test a semaphore (as opposed to locking it) when
 * you have analyzed all the races and determined that it is OK.
 */
/*
 * XXX: for now, we can't tell that a lock is "mine", so we just
 * assert it's locked by *somebody*.  This means that the
 * _SEM_NOT_MINE() tests are a problem, so just leave them empty.
 *
 * Note, using the current functions, we could do something like the following
 * to actually test a semaphore.  down_trylock just gets the lock if it can
 * without sleeping and returns 0, otherwise, if would have to wait it returns
 * 1; thus, it is a "cheap" test (no waiting):
 *
 *   if (down_trylock(&sema) == 0) {
 *      <do your "it wasn't locked thing" now that it's locked>
 *      up(&sema);
 *   } else {
 *      <do your "it was locked thing">
 *   }
 */
#define TEST_SEMA_LOCKED(sema)   (atomic_read(&((sema)->count)) <= 0)
/* The s_lock is now a struct semaphore. */
#define TEST_SB_LOCKED(sb)     TEST_SEMA_LOCKED(&((sb)->s_lock))
#define ASSERT_SEMA_MINE(sema)   ASSERT(TEST_SEMA_LOCKED(sema))
#define ASSERT_RWSEMA_MINE_R(sema)   ASSERT(TEST_RWSEMA_LOCKED_R(sema))
#define ASSERT_RWSEMA_MINE_W(sema)   ASSERT(TEST_RWSEMA_LOCKED_W(sema))
#if defined(CONFIG_RWSEM_GENERIC_SPINLOCK) && \
    !(defined(RATL_SUSE) && (RATL_VENDOR_VER < 900))
#define TEST_RWSEMA_LOCKED_W(sema)   ((sema)->activity < 0)
#define TEST_RWSEMA_LOCKED_R(sema)   ((sema)->activity > 0)
#elif defined(__i386__) || defined(__s390__) || defined(__x86_64__) || defined(__powerpc64__)
#define TEST_RWSEMA_LOCKED_W(sema)   ((sema)->count < 0)
#define TEST_RWSEMA_LOCKED_R(sema)   ((sema)->count > 0)
#else
#error need definitions for TEST_RWSEMA_LOCKED_{R,W}
#endif

#define ASSERT_I_SEM_MINE(ino)   ASSERT_SEMA_MINE(&(ino)->i_sem)
#define ASSERT_I_SEM_NOT_MINE(ino)
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define ASSERT_I_ZOMB_MINE(ino)  ASSERT_SEMA_MINE(&(ino)->i_zombie)
#else
/* No more i_zombie field in Linux 2.6. */
#define ASSERT_I_ZOMB_MINE(ino)
#endif
#define ASSERT_SB_MOUNT_LOCKED_W(sb) ASSERT_RWSEMA_MINE_W(&(sb)->s_umount)
#define ASSERT_I_ZOMB_NOT_MINE(ino)

#define ASSERT_SB_LOCKED(sb)     ASSERT(TEST_SB_LOCKED(sb))
#define ASSERT_SB_UNLOCKED(sb)   ASSERT(!TEST_SB_LOCKED(sb))

/* XXX need to find locking document on address_space_operations */
#define ASSERT_PAGE_LOCKED(pg)   ASSERT(PageLocked(pg))
#define ASSERT_PAGE_UNLOCKED(pg) ASSERT(!PageLocked(pg))

#else /* No debug */

#define ASSERT_KERNEL_LOCKED()
#define ASSERT_KERNEL_LOCKED_24x()
#define ASSERT_KERNEL_UNLOCKED()
#define ASSERT_DCACHE_LOCKED()
#define ASSERT_DCACHE_UNLOCKED()
#define ASSERT_SEMA_MINE(sema)
#define ASSERT_I_SEM_MINE(ino)
#define ASSERT_I_SEM_NOT_MINE(ino)
#define ASSERT_I_ZOMB_MINE(ino)
#define ASSERT_I_ZOMB_NOT_MINE(ino)
#define ASSERT_SB_LOCKED(sb)
#define ASSERT_SB_MOUNT_LOCKED_W(sb)
#define ASSERT_SB_UNLOCKED(sb)
#define ASSERT_PAGE_LOCKED(pg)
#define ASSERT_PAGE_UNLOCKED(pg)

#endif /* debug */

/* These are defined in mvfs_linux_utils.c */
extern VNODE_T *vnlayer_sysroot_clrvp;
extern VFS_T *vnlayer_clrvnode_vfsp;
extern VNODE_T *vnlayer_looproot_vp;

extern struct vfsops *vnlayer_vfs_opvec;

/* Macros to handle the module ref count.
 *
 * Because MVFS module is loaded separately, and is not the module
 * with the inode operations/etc., it won't have reference counts on
 * it when a file system is mounted and in use.  We need to handle
 * that ourselves in our superblock read (i.e. mount) handler.
 */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define MDKI_MODULE_GET(modp) __MOD_INC_USE_COUNT(modp)
#define MDKI_MODULE_PUT(modp) __MOD_DEC_USE_COUNT(modp)
#else
/* If the try fails, we're sunk since that means MODULE_STATE_GOING is
** true, which means we're unloading even though we haven't even
** gotten loaded yet (see <linux/module.h> for details).
*/
#define MDKI_MODULE_GET(modp) if (!try_module_get(modp)) {BUG();}
#define MDKI_MODULE_PUT(modp) module_put(modp)
#endif

#define LINUX_DIRENT_T	struct dirent

#ifdef MVFS_DEBUG
extern struct vfsmount *
vnlayer_debug_mntget(
    struct vfsmount *mnt,
    const char *file,
    const char *func,
    int line
);
extern void
vnlayer_debug_mntput(
    struct vfsmount *mnt,
    const char *file,
    const char *func,
    int line
);
#define MDKI_MNTGET(mnt) vnlayer_debug_mntget(mnt,__FILE__,__func__,__LINE__)
#define MDKI_MNTPUT(mnt) vnlayer_debug_mntput(mnt,__FILE__,__func__,__LINE__)
#else
#define MDKI_MNTGET(mnt) mntget(mnt)
#define MDKI_MNTPUT(mnt) mntput(mnt)
#endif

extern struct inode *
vnlayer_new_inode(struct super_block *sb);

extern struct fs_struct *
vnlayer_make_temp_fs_struct(void);

extern void
vnlayer_free_temp_fs_struct(struct fs_struct *fs);

extern struct fs_struct *
vnlayer_swap_task_fs(
    struct task_struct *task,
    struct fs_struct *new_fs
);

extern struct dentry *
vnlayer_inode2dentry_internal(
    struct inode *inode,
    struct dentry *parent,
    struct qstr *name,
    struct dentry_operations *ops
);

static inline mdki_boolean_t
vnlayer_names_eq(
    struct qstr *name1,
    struct qstr *name2
)
{
    if (name1->len == name2->len &&
        mdki_memcmp(name1->name, name2->name, name1->len) == 0)
    {
        return TRUE;
    }
    return FALSE;
}

extern int
vnlayer_linux_log(
    VFS_T *vfsp,
    int level,
    const char *str,
    ...
);
extern VFS_T vnlayer_cltxt_vfs;
extern int
vnlayer_vtype_to_mode(VTYPE_T type);

extern VTYPE_T
vnlayer_mode_to_vtype(int mode);
/*
 * We use this in places where we switch between UNIX convention
 * (positive error codes) and Linux convention (negative error codes).
 */
#define vnlayer_errno_linux_to_unix(err) (-(err))

extern struct block_device_operations vnlayer_device_ops;

void
vnlayer_linux_vprintf(
    const char *fmt,
    va_list ap
);

extern void
vnlayer_bogus_op(void);

extern void
vnlayer_bogus_vnop(void);

extern mdki_boolean_t
vnlayer_link_eligible(const struct dentry *dent);

/* Note that SLES8 SP3 and SLES9 do not get the i_sem in getxattr or listxattr
 * and so reiserfs gets that lock in their code.  But it does get the
 * i_sem on setxattr and removexattr.  RedHat on the other hand
 * gets the lock on all xattr calls.
 */
static inline int
vnlayer_do_setxattr(
    struct dentry *dentry,
    const char *name,
    const void *value,
    size_t size,
    int flags
    )
{
    INODE_T *inode;
    int err = -EOPNOTSUPP;
 
    inode = dentry->d_inode;
    if (inode->i_op && inode->i_op->setxattr) {
        LOCK_INODE(inode);
        err = (*inode->i_op->setxattr)(dentry, name, value, size, flags);
        UNLOCK_INODE(inode);
    }
    return err;
}

static inline ssize_t
vnlayer_do_getxattr(
    struct dentry *dentry,
    const char *name,
    void *value,
    size_t size
)
{
    INODE_T *inode;
    ssize_t err = -EOPNOTSUPP;

    inode = dentry->d_inode;
    if (inode->i_op && inode->i_op->getxattr) {
#if defined(RATL_SUSE)
	/* SLES8 SP3 does the locking at the filesystem level */
#else
        LOCK_INODE(inode);
#endif
        err = (*inode->i_op->getxattr)(dentry, name, value, size);
#if defined(RATL_SUSE)
	/* SLES8 SP3 does the locking at the filesystem level */
#else
        UNLOCK_INODE(inode);
#endif
    }
    return err;
}
static inline ssize_t
vnlayer_do_listxattr(
    struct dentry *dentry,
    char *name,
    size_t size
)
{
    INODE_T *inode;
    ssize_t err = -EOPNOTSUPP;

    inode = dentry->d_inode;
    if (inode->i_op && inode->i_op->listxattr) {
#if defined(RATL_SUSE)
        /* SLES8 SP3 does the locking at the filesystem level */
#else
        LOCK_INODE(inode);
#endif
        err = (*inode->i_op->listxattr)(dentry, name, size);
#if defined(RATL_SUSE)
	/* SLES8 SP3 does the locking at the filesystem level */
#else
        UNLOCK_INODE(inode);
#endif
    }
    return err;
}
static inline int
vnlayer_do_removexattr(
    struct dentry *dentry,
    const char *name
)
{
    INODE_T *inode;
    int err = -EOPNOTSUPP;
    
    inode = dentry->d_inode;
    if (inode->i_op && inode->i_op->removexattr) {
        LOCK_INODE(inode);
        err = (*inode->i_op->removexattr)(dentry, name);
        UNLOCK_INODE(inode);
    }
    return err;
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,9)
#define VNLAYER_RA_STATE_INIT(ra, mapping) file_ra_state_init(ra, mapping)
#else /* 2.6.9 or later */
#define VNLAYER_RA_STATE_INIT(ra, mapping) vnlayer_ra_state_init(ra, mapping)
extern void
vnlayer_ra_state_init(
    struct file_ra_state *ra,
    struct address_space *mapping
);
#endif /* 2.6.9 or later */

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,10) || \
	(defined(RATL_REDHAT) && (RATL_VENDOR_VER >= 400))

extern void
vnlayer_set_fs_root(
    struct fs_struct *fs,
    struct vfsmount *mnt,
    struct dentry *dent
);
#define VNLAYER_SET_FS_ROOT vnlayer_set_fs_root
#else
#define VNLAYER_SET_FS_ROOT set_fs_root
#endif /* 2.6.10 or later, or RHEL4 */

#ifdef USE_ROOTALIAS_CURVIEW
extern struct inode_operations vnlayer_hijacked_iops;
extern struct inode_operations vnlayer_root_iops_copy;
typedef struct vnlayer_root_alias {
    struct dentry_operations dops;
    DENT_T *curview;
    struct vfsmount *mnt;
    DENT_T *rootdentry;
} vnlayer_root_alias_t;

extern struct dentry_operations vnlayer_root_alias_dops;

#define DENT_IS_ROOT_ALIAS(dent)                                        \
  ((dent)->d_op != NULL &&                                              \
   (dent)->d_op->d_release == vnlayer_root_alias_dops.d_release)

#define DENT_GET_ALIAS(dentry) ((vnlayer_root_alias_t *)dentry->d_op)
extern DENT_T *
vnlayer_hijacked_lookup(
    INODE_T *dir,
    struct dentry *dent
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0)
    , struct nameidata *nd
#endif
);
#endif /* USE_ROOTALIAS_CURVIEW */
extern int
vnlayer_hijack_root_inode(void);
extern void
vnlayer_restore_root_inode(void);

#undef MDKI_SET_PROC_RDIR
#ifdef USE_ROOTALIAS_CURVIEW
#define MDKI_SET_PROC_RDIR(dp) \
	vnlayer_set_urdent(dp, MDKI_MNTGET(vnlayer_sysroot_mnt))
#endif /* USE_ROOTALIAS_CURVIEW */
#ifdef USE_CHROOT_CURVIEW
#define MDKI_SET_PROC_RDIR(dp) \
	vnlayer_set_urdent(dp, dp ? vnlayer_dent2vfsmnt(dp) : NULL)
#endif /* USE_CHROOT_CURVIEW */

#define WAIT_QUEUE_HEAD_T wait_queue_head_t
#define WAIT_QUEUE_T wait_queue_t

extern WAIT_QUEUE_HEAD_T vnlayer_inactive_waitq;

#define BYTES_PER_XDR_UNIT (4)
#define XDR_GETBYTES mvfs_linux_xdr_getbytes
#define XDR_PUTBYTES mvfs_linux_xdr_putbytes
static __inline__ bool_t
mvfs_linux_xdr_getbytes(
    XDR *x,
    char *cp,
    u_int cnt
)
{
    if (x->x_data + cnt > x->x_limit) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s: data underflow\n", __func__);
        return FALSE;
    }
    BCOPY(x->x_data, cp, cnt);
    x->x_data += cnt;
    return TRUE;
}

static __inline__ bool_t
mvfs_linux_xdr_putbytes(
    XDR *x,
    char *cp,
    u_int cnt
)
{
    if (x->x_data + cnt > x->x_limit) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s: data overflow\n", __func__);
        return FALSE;
    }
    BCOPY(cp, x->x_data, cnt);
    x->x_data += cnt;
    return TRUE;
}

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) || defined(SLES10SP2)
/*
 * This struct is being used to add the posix lock owner on close
 * operations using the context parameter.  In older versions we were
 * passing just the file_p.
 */
typedef struct mdki_vop_close_ctx {
    FILE_T *file_p;
    fl_owner_t owner_id;
} mdki_vop_close_ctx_t;
#endif

#if defined(__x86_64__) && !defined(CONFIG_IA32_EMULATION)
#error this module requires IA32 emulation on x86_64
#endif
/*
 * On PPC64, the 32-bit system calls are always present, so we don't need
 * to check anything.
 */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)
#define NFSD_DCACHE_DISCON DCACHE_NFSD_DISCONNECTED
#else
#define NFSD_DCACHE_DISCON DCACHE_DISCONNECTED
#endif

#if defined(__s390__) || defined(__s390x__)
#define INLINE_FOR_SMALL_STACK extern inline
#else
#define INLINE_FOR_SMALL_STACK STATIC
#endif

/*
 * NO_STACK_CHECKING can be turned on (before including this header
 * file) in individual source modules to delete checks from those
 * modules.
 */
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,6,0)) && (defined(MVFS_DEBUG) || defined(STACK_CHECKING)) && !defined(NO_STACK_CHECKING)
/*
 * The journal_info field is near the end of the task structure and is
 * likely to be trashed on a stack overflow.  If it's trashed, ext3
 * will crash on the next file system access.  These macros help identify
 * when it's been trashed before we unwind the stack and lose the stack trace
 * related to the overflow.
 */
#ifdef __x86_64__
#define ALIGNMENT_MASK 7
#else
#define ALIGNMENT_MASK 3
#endif
#if defined(RATL_REDHAT) && (RATL_VENDOR_VER < 400)
#define VN_STACK_CHECK1 signal
#define VN_STACK_CHECK2 last_siginfo
#define VN_STACK_CHECK3 sighand
#define VN_JOURNAL_CHECK
#else
#define VN_STACK_CHECK1 notifier_mask
#define VN_STACK_CHECK2 sig
#define VN_STACK_CHECK3 namespace
#endif
#ifdef VN_JOURNAL_CHECK
#define STACK_CHECK_DECL()  void *journal_info = current->journal_info;
#define STACK_JOURNAL_CHECK()                                              \
    if ((journal_info != NULL && current->journal_info != journal_info) || \
        (((long)current->journal_info) & ALIGNMENT_MASK))                  \
    {                                                                      \
        printk("ji=%p cji=%p\n", journal_info, current->journal_info);     \
        BUG();                                                             \
    }
#else
#define STACK_CHECK_DECL()  /* skip */
#define STACK_JOURNAL_CHECK() /* skip */
#endif
#define STACK_CHECK() do {                                              \
    STACK_JOURNAL_CHECK()                                               \
    if ((((long)current->VN_STACK_CHECK1) & ALIGNMENT_MASK) ||          \
	(((long)current->VN_STACK_CHECK2) & ALIGNMENT_MASK) ||          \
        (((long)current->VN_STACK_CHECK3) & ALIGNMENT_MASK))            \
    {                                                                   \
        printk("pointers trashed: %p %p %p\n",                          \
	       current->VN_STACK_CHECK1, current->VN_STACK_CHECK2,      \
               current->VN_STACK_CHECK3);                               \
        BUG();                                                          \
    }                                                                   \
} while (0)
#else
#define STACK_CHECK_DECL()
#define STACK_CHECK()
#endif /* STACK_CHECKING */

#endif /* MVFS_LINUX_ONLY_H_ */
/* $Id: 241422a8.8be611dd.851a.00:01:6c:81:c6:90 $ */
