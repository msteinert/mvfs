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
 * Utility functions available to file system implementations.
 */

#include "vnode_linux.h"
#include "mvfs_linux_shadow.h"
#include <linux/delay.h>
#include <linux/seq_file.h>
#if defined(RATL_COMPAT32) && LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
/*
 * For 64-bit kernels, include the definitions for ioctl 32-bit
 * compatibility,  Since 2.6.22 compat.c and compat_ioctl.c were
 * unified, and ioctl32.h was removed.
 */
# include <linux/ioctl32.h>
#endif /* RATL_COMPAT32 */
#include <linux/swap.h>

STATIC MVFS_KMEM_CACHE_T *vnlayer_cred_cache;
/* Defined here because we initialize it below, but it is used in
** mvfs_linux_sops.c
*/
MVFS_KMEM_CACHE_T *vnlayer_vnode_cache;

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38)
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/kprobes.h>
#include <linux/kallsyms.h>
#include <linux/mempolicy.h>

#define KPROBE_TYPE_J 0
#define KPROBE_TYPE_R 1

#if defined(CONFIG_X86_32) || defined(CONFIG_X86_64) 
#define SET_RETURN_VALUE(R, V) (R)->ax = (V)
#elif defined(__powerpc64__)
#define SET_RETURN_VALUE(R, V) regs_return_value(R) = (V)
#elif defined(__s390x__)
#define SET_RETURN_VALUE(R, V) regs_return_value(R) = (V)
#endif

static int
mdki_probe_prepend_path_return(
    struct kretprobe_instance *ri,
    struct pt_regs *regs
);

static struct {
	int type;
	const char *fname;
	union {
	struct kretprobe kretprobe;
        struct jprobe jprobe;
	} p;
} probed_funcs[] = {
    {
        .type = KPROBE_TYPE_R,
        .fname = "prepend_path",
        .p.kretprobe = {
            .entry_handler = NULL,
            .handler = mdki_probe_prepend_path_return,
            .maxactive = 0,
            .data_size = 0,
        }
    },
    {
        .fname = NULL,
    },
};

static int
mdki_probe_dummy_pre(
    struct kprobe *p,
    struct pt_regs *regs
)
{
    return 0;
}

static void
mdki_probe_dummy_post(
    struct kprobe *p,
    struct pt_regs *regs,
    unsigned long flags
)
{
}

static int
mdki_probe_dummy_fault(
    struct kprobe *p,
    struct pt_regs *regs,
    int trapnr
)
{
    return 0;
}

static int
mdki_probe_prepend_path_return(
    struct kretprobe_instance *ri,
    struct pt_regs *regs
)
{
    if (current->fs && current->fs->root.dentry && DENT_IS_ROOT_ALIAS(MDKI_FS_ROOTDENTRY(current->fs))) {
        if (regs_return_value(regs) == 1) {
            SET_RETURN_VALUE(regs, 0);
        }
    }
    return 0;
}

static int
mdki_probe_init(void)
{
    int i;
    int ret;
    struct kprobe k;

    for (i = 0; probed_funcs[i].fname; i++) {
        switch (probed_funcs[i].type) {
            case KPROBE_TYPE_R:
                probed_funcs[i].p.kretprobe.kp.symbol_name = probed_funcs[i].fname;
                if ((ret = register_kretprobe(&probed_funcs[i].p.kretprobe)) < 0) {
                    MDKI_VFS_LOG(VFS_LOG_ERR, "%s: register_kretprobe for "
                                              "\"%s\" failed with result=%d\n",
                                              __func__, probed_funcs[i].fname,
                                              ret);
                    continue;
                }
            break;

            case KPROBE_TYPE_J:
                /*
                 * Without kallsyms_lookup_name, use register_kprobe
                 * to find symbol's address. I know, it is ugly.
                 */
                memset(&k, 0, sizeof(k));
                k.symbol_name = probed_funcs[i].fname;
                k.pre_handler = mdki_probe_dummy_pre;
                k.post_handler = mdki_probe_dummy_post;
                k.fault_handler = mdki_probe_dummy_fault;
                if (register_kprobe(&k) == 0) {
                    probed_funcs[i].p.jprobe.kp.addr = k.addr;
                    unregister_kprobe(&k);
                } else {
                    MDKI_VFS_LOG(VFS_LOG_ERR, "%s: address of "
                                              "\"%s\" not found\n",
                                              __func__, probed_funcs[i].fname);
                }
                if ((ret = register_jprobe(&probed_funcs[i].p.jprobe)) < 0) {
                    MDKI_VFS_LOG(VFS_LOG_ERR, "%s: register_jprobe for "
                                              "\"%s\" failed with result=%d\n",
                                              __func__, probed_funcs[i].fname,
                                              ret);
                    continue;
                }
            break;
        }
    }
    return 0;
}

static void
mdki_probe_fini(void)
{
    int i;

    for (i = 0; probed_funcs[i].fname; i++) {
        if (probed_funcs[i].type == KPROBE_TYPE_R) {
            unregister_kretprobe(&probed_funcs[i].p.kretprobe);
            /*
             * printk("kretprobe at %s(%p) unregistered\n",
             *       probed_funcs[i].fname, probed_funcs[i].p.kretprobe.kp.addr);
             */
            /* nmissed > 0 suggests that maxactive was set too low. */
            if (probed_funcs[i].p.kretprobe.nmissed > 0)
                MDKI_VFS_LOG(VFS_LOG_ERR, "%s: missed probing %d instances of "
                                          "\"%s\"\n", __func__,
                                          probed_funcs[i].p.kretprobe.nmissed,
                                          probed_funcs[i].fname);
        }
    }
}
#else
    #define mdki_probe_init()
    #define mdki_probe_fini()
#endif  /* else LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38) */

#ifdef MVFS_DEBUG
#if defined(__i386__)
static inline caddr_t
get_sp(void)
{
	char * sp;
	__asm__("orl %%esp,%0; ":"=r" (sp) : "0" (0));
	return sp;
}

void
mdki_linux_chksp(
    unsigned long val
)
{
    /* In Linux 2.6 they moved the struct task_struct out of the stack pages
    ** and now there is just an 8 word struct thread_info there (the first word
    ** of which points to the task_struct).  Therefore, we need to do a
    ** slightly different check.
    */
    int sleft = get_sp() - ((char *)current_thread_info() + sizeof(struct thread_info));
    if (sleft  < 400) {
        printk("stack check %ld too low\n", val);
        BUG();
    }
}
#elif defined(__ia64__) || defined(__x86_64__) || defined(__powerpc64__)
void
mdki_linux_chksp(
    unsigned long val
)
{
    /*ARGSUSED*/
    /* FIXME: figure out how to check it */
}
#elif defined(__s390__)
void
mdki_linux_chksp(
    unsigned long val
)
{
    /*ARGSUSED*/
    /* FIXME: gcc provides a __builtin_frame_address function similar
    ** to the __builtin_return_address function we use above.  The addr
    ** for the current frame would be __builtin_frame_address(0), etc.
    ** If the Linux stack layout is generic enough, and we still want
    ** to do this test, we should probably use that function and make
    ** this whole batch of code not be machine architecture dependent.
    */
}
#else
#error unknown platform for stack checks
#endif
#endif /* MVFS_DEBUG */

#define LINUX_DIRENT_BASESIZE \
        (((LINUX_DIRENT_T *) 0)->d_name - (char *) 0)

#define NBPLW sizeof(long)
#define LINUX_DIRENT_RECLEN(namelen) \
        ((LINUX_DIRENT_BASESIZE + (namelen) + NBPLW) & ~(NBPLW - 1))

#define LINUX_DIRENT_SET_RECLEN(dp, nmlen)   (dp)->d_reclen = LINUX_DIRENT_RECLEN(nmlen)

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18) 
static inline unsigned int
mdki_linux_blksize_to_bits(unsigned int blksize)
{
    unsigned int i;
    for(i = 0; blksize > 1; blksize >>= 1, i++);
    return i;
}
#endif

int
mdki_linux_dirent_basesize(void)
{
    return LINUX_DIRENT_BASESIZE;
}

int
mdki_linux_dirent_reclen(int namelen)
{
    return LINUX_DIRENT_RECLEN(namelen);
}

void
mdki_linux_dirent_init(
    KDIRENT_T *dp,
    long ino,
    const char *nm,
    int nmlen,
    long long next_offset
)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    dentp->d_ino = (ino);
    dentp->d_off = (next_offset);
    dentp->d_reclen = LINUX_DIRENT_RECLEN(nmlen);
    ASSERT(nmlen < 256);
    STRCPY(dentp->d_name, nm);
}

extern long
mdki_linux_dirent_get_ino(KDIRENT_T *dp)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    return dentp->d_ino;
}

extern long long
mdki_linux_dirent_get_off(KDIRENT_T *dp)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    return dentp->d_off;
}

extern int
mdki_linux_dirent_get_namlen(KDIRENT_T *dp)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    return STRLEN(dentp->d_name);
}

extern int
mdki_linux_dirent_get_reclen(KDIRENT_T *dp)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    return dentp->d_reclen;
}

char *
mdki_linux_dirent_get_name(KDIRENT_T *dp)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    return dentp->d_name;
}

void
mdki_linux_dirent_set_ino(
    KDIRENT_T *dp,
    long ino
)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    dentp->d_ino = (ino);
}

void
mdki_linux_dirent_set_off(
    KDIRENT_T *dp,
    long long off
)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    dentp->d_off = (off);
}

void
mdki_linux_dirent_set_reclen(
    KDIRENT_T *dp,
    int nmlen
)
{
    LINUX_DIRENT_T *dentp = (LINUX_DIRENT_T *)dp;
    dentp->d_reclen = LINUX_DIRENT_RECLEN(nmlen);
}

int mvfs_lastmaj = 0;

static const char vnlayer_device_name[] = "mvfs";

STATIC int
vnlayer_register_blkdev(int dev)
{
    /*
     * register_blkdev no longer needs the
     * device_ops ptr and it returns a major number just the same.
     * Without operations, the device cannot be opened (fails with ENXIO).
     */
    dev = register_blkdev(dev, vnlayer_device_name);
    return dev;
}

/*
 * Callers must protect this function and mdki_linux_release_majors()
 * to prevent multiple simultaneous calls.
 */
MVFS_MAJOR_T
mdki_linux_get_major(void)
{
    int dev = vnlayer_register_blkdev(0);

    if (dev < 0) {
        return -1;
    }
    mvfs_lastmaj++;                 /* protected by MVFS mount lock */
    return dev;
}

/*
 * The following two functions are wrappers for kmalloc and kfree
 * which allow for large allocations.
 */

#define SIZE_BRKPOINT (128 * 1024)

#if 0
/* PERIODIC_ERROR is a count of when to fail.  Preferrably a prime number */
/* This provides a simple way to trigger memory allocation failures that is
 * friendly to kernel debuggers.
 */
#define PERIODIC_ERROR 3943
unsigned int
mdki_linux_periodic_error = PERIODIC_ERROR;
mdki_linux_periodic_error_cnt = 0;
mdki_linux_emomem_generated = 0;
#endif

extern void *
mdki_linux_kmalloc(
    size_t size,
    int flag
)
{
    void * ptr;

#ifdef PERIODIC_ERROR
    if (mdki_linux_periodic_error_cnt++ >= mdki_linux_periodic_error) {
        mdki_linux_periodic_error_cnt = 0;
        mdki_linux_emomem_generated++;
	return NULL;
    }
#endif
    if (size > SIZE_BRKPOINT) {
         ptr = vmalloc(size);
    } else {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,32)
         ptr = kmalloc(size, flag == KM_SLEEP ? (GFP_KERNEL|__GFP_NOFAIL) : GFP_ATOMIC);
#else
         ptr = kmalloc(size, flag == KM_SLEEP ? GFP_KERNEL : GFP_ATOMIC);
#endif
    }
    return(ptr);
}

extern void
mdki_linux_kfree(
    void * ptr,
    size_t size
)
{
    if (size > SIZE_BRKPOINT)
        vfree(ptr);
    else
        kfree(ptr);
}

int
mdki_linux_maxnamelen(void)
{
    return NAME_MAX;
}

int
mdki_linux_maxpathlen(void)
{
    return PATH_MAX;
}

STATIC void
vnlayer_release_root_dentry(void)
{
    /* use real dput, internal printf is closed down at this point */
    dput(vnlayer_sysroot_dentry);
    vnlayer_sysroot_dentry = NULL;
    MDKI_MNTPUT(vnlayer_sysroot_mnt);
    vnlayer_sysroot_mnt = NULL;
}

STATIC int
vnlayer_init_root_dentry(void)
{
    vnlayer_sysroot_dentry = VNODE_DGET(vnlayer_get_root_dentry());
    vnlayer_sysroot_mnt = vnlayer_get_root_mnt();
    if ((vnlayer_sysroot_dentry->d_parent != vnlayer_sysroot_dentry)
        || (vnlayer_sysroot_mnt->mnt_root != vnlayer_sysroot_dentry))
    {
        printk("ERROR: MVFS loaded with root directory not at file system root.\n");
        vnlayer_release_root_dentry();
        return EINVAL;
    }
    return 0;
}

#ifdef USE_ROOTALIAS_CURVIEW
/* These are protected by root inode's semaphore */

struct inode_operations *vnlayer_original_root_iops = NULL;
struct inode_operations vnlayer_root_iops_copy;
struct inode_operations vnlayer_hijacked_iops;
struct inode_operations vnlayer_hijacked_iops_copy;

/*
 * We supplant the root inode's lookup routine with our own.  To do
 * that, we need to replace the inode's operation vector with ours:
 * copy the existing one, insert our lookup routine in the copy, and
 * point to the copy.  We keep an extra copy of our modified vector to
 * detect any other changes someone might try to make directly to the
 * root inode vector after we've taken it over.  If we find anything
 * wrong when un-hijacking, we keep a reference so that we don't get
 * unloaded.  That way anybody else playing games like we did should
 * still keep working.
 *
 * (We depend on everybody else playing by similar rules.  If someone
 * else changes the root vector without similar care, we may get
 * detached unwittingly or we may continue using existing vectors that
 * they've unloaded.
 */
#endif /* USE_ROOTALIAS_CURVIEW */

int
vnlayer_hijack_root_inode(void)
{
#ifdef USE_ROOTALIAS_CURVIEW
    LOCK_INODE(vnlayer_sysroot_dentry->d_inode);
    if (vnlayer_original_root_iops == NULL) {
        vnlayer_original_root_iops = (struct inode_operations *) vnlayer_sysroot_dentry->d_inode->i_op;
        vnlayer_root_iops_copy = *vnlayer_original_root_iops;
        vnlayer_hijacked_iops = vnlayer_root_iops_copy;
        vnlayer_hijacked_iops.lookup = &vnlayer_hijacked_lookup;
        vnlayer_hijacked_iops_copy = vnlayer_hijacked_iops;
        vnlayer_sysroot_dentry->d_inode->i_op = &vnlayer_hijacked_iops;
        MDKI_MODULE_GET(THIS_MODULE);
    }
    UNLOCK_INODE(vnlayer_sysroot_dentry->d_inode);
#endif /* USE_ROOTALIAS_CURVIEW */
    return 0;
}

void
vnlayer_restore_root_inode(void)
{
#ifdef USE_ROOTALIAS_CURVIEW
    LOCK_INODE(vnlayer_sysroot_dentry->d_inode);
    if (vnlayer_original_root_iops != NULL) {
        if (vnlayer_sysroot_dentry->d_inode->i_op != &vnlayer_hijacked_iops ||
            mdki_memcmp(&vnlayer_hijacked_iops, &vnlayer_hijacked_iops_copy,
                        sizeof(vnlayer_hijacked_iops)) != 0)
        {
            printk(KERN_WARNING "WARNING: other code changed root directory inode"
                   " operations, unable to unload %s.\n", THIS_MODULE->name);
        } else {
            vnlayer_sysroot_dentry->d_inode->i_op = vnlayer_original_root_iops;
            vnlayer_original_root_iops = NULL;
            MDKI_MODULE_PUT(THIS_MODULE);
        }
    }
    UNLOCK_INODE(vnlayer_sysroot_dentry->d_inode);
#endif /* USE_ROOTALIAS_CURVIEW */
}

/*
 * Callback from file system module to set up some additional items
 * not needed when this adapter is initialized.
 */
int
mdki_linux_mdep_init(void)
{
    int err;

    err = vnlayer_init_root_dentry();
    if (err != 0)
        return err;

    /* vnlayer_cltxt_vfs.vfs_sb = something; ??? */

    init_waitqueue_head(&vnlayer_inactive_waitq);
    mdki_probe_init();
    return 0;
}

void
mdki_linux_mdep_unload(void)
{
    mdki_probe_fini();
    vnlayer_release_root_dentry();
}

extern int
mdki_linux_page_size(void)
{
    return PAGE_SIZE;
}

void
mdki_linux_printf(
    const char *fmt,
    ...
)
{
    va_list ap;

    va_start(ap, fmt);
    vnlayer_linux_vprintf(fmt, ap);
    va_end(ap);
    return;
}

void
mdki_linux_panic(
    const char *fmt,
    ...
)
{
    va_list ap;

    mdki_panicstr = fmt;

    va_start(ap, fmt);
    vnlayer_linux_vprintf(fmt, ap);
    va_end(ap);

    panic("VNODE panic");             /* panic will stop the system */
}

MDKI_DECLARE_SLEEPLOCK(vnlayer_view_dent_lock);

/*
 * Get the dentry for a view tag directory, making one if necessary.
 */
STATIC DENT_T *
vnlayer_get_view_dentry(
    VNODE_T *viewdir,
    VNODE_T *vw,
    const char *viewname
)
{
    INODE_T *vwip;
    DENT_T *tmp;

    /* Lock to prevent add/add races */
    MDKI_SLEEP_LOCK(&vnlayer_view_dent_lock);
    vwip = VTOI(vw);
    tmp = MVOP_DENT(vwip, &vnode_dentry_ops);
    if (tmp == NULL) {
        /* whoops! Need to create a new dcache.  Luckily we have all
         * the necessary information--we know the /view vnode will
         * have a dcache entry attached, as it's a file system root.
         */
        tmp = vnlayer_make_dcache(viewdir, vw, viewname);
        /* tmp comes back with a hold count on it for us to use */
    }
    MDKI_SLEEP_UNLOCK(&vnlayer_view_dent_lock);
    MDKI_TRACE(TRACE_VNODES,"setview %p dent %p vcnt=%d\n", vwip, tmp,
               I_COUNT(vwip));
    ASSERT(tmp && tmp->d_inode == vwip);
    return tmp;
}


#ifdef USE_ROOTALIAS_CURVIEW

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38) 
STATIC int
vnlayer_root_alias_hash(
    struct dentry *base,
    struct qstr *name
)
#else
STATIC int
vnlayer_root_alias_hash(
    const struct dentry *base,
    const struct inode *inode,
    struct qstr *name
)
#endif
{
    vnlayer_root_alias_t *alias;
    ASSERT(DENT_IS_ROOT_ALIAS(base));
    alias = DENT_GET_ALIAS(base);
    ASSERT(alias->rootdentry->d_op->d_hash != NULL)
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38) 
    return (*alias->rootdentry->d_op->d_hash)(alias->rootdentry, name);
#else
    return (*alias->rootdentry->d_op->d_hash)(alias->rootdentry, inode, name);
#endif
}

STATIC struct dentry *
vnlayer_find_alias(
    INODE_T *inode,
    struct dentry *viewdent
)
{
    struct dentry *found = NULL;
    struct list_head *le;
    vnlayer_root_alias_t *alias;

    /*
     * Scan through the dentry aliases for the root inode, looking for an
     * alias dentry that matches the view we want to use.
     */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
    /* i_lock protects i_dentry */
    spin_lock(&inode->i_lock);
#else
    spin_lock(&dcache_lock);
#endif
    if (!list_empty(&inode->i_dentry)) {
        list_for_each(le, &inode->i_dentry) {
            found = list_entry(le, struct dentry, d_alias);
            if (DENT_IS_ROOT_ALIAS(found)) {
                alias = DENT_GET_ALIAS(found);
                if (alias->curview == viewdent) {
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
                    dget(found);
#else
                    dget_locked(found);
#endif
                    break;
                }
            }
            found = NULL;
        }
    }
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
    spin_unlock(&inode->i_lock);
#else
    spin_unlock(&dcache_lock);
#endif
    return found;
}

/*
 * We can't use a bogus name here, since that would show up in
 * /proc/<pid>/root.  But if we use an empty name, all the path
 * constructors will glom that onto the / from the real root, and we
 * then look indistinguishable from the real root dentry to user-space
 * programs!
 */
static const struct qstr vnlayer_root_aliasname =
{ .name = "",
  .len = 0,
  .hash = 0
};

MDKI_DECLARE_SLEEPLOCK(vnlayer_dop_setup_lock);

/*
 * Consume the count on vw unless we encounter an error.
 */
STATIC struct dentry *
vnlayer_root_dop_setup(
    struct dentry *root,
    VNODE_T *vw,
    VNODE_T *viewdir,
    const char *viewname
)
{
    vnlayer_root_alias_t *alias;
    struct dentry *dentry;
    struct dentry *viewdent;

    ASSERT(root->d_inode != NULL);
    ASSERT(vw != NULL);

    viewdent = vnlayer_get_view_dentry(viewdir, vw, viewname);
    if (viewdent == NULL)
        return NULL;

    /* Lock to prevent add/add races */
    MDKI_SLEEP_LOCK(&vnlayer_dop_setup_lock);
    /*
     * Look for an existing alias in the same view, and reuse it if we
     * find it.  This makes things like /proc/<pid> work better for
     * unrelated processes set to the same view.
     */
    dentry = vnlayer_find_alias(root->d_inode, viewdent);
    if (dentry != NULL) {
        MDKI_SLEEP_UNLOCK(&vnlayer_dop_setup_lock);
        VNODE_DPUT(viewdent);
        VN_RELE(vw);                    /* consume the reference */
        return dentry;
    }

    alias = kmalloc(sizeof(*alias), GFP_KERNEL);
    if (alias == NULL) {
        MDKI_SLEEP_UNLOCK(&vnlayer_dop_setup_lock);
        VNODE_DPUT(viewdent);
        return NULL;
    }
    /*
     * We set up a dentry in the root file system, hooked up to the
     * real root inode, but with our dops on it.  We've previously
     * hijacked the root directory's lookup routine for ours, which
     * calls the real one with appropriate args to accomplish all the
     * namespace games we need (keep all objects attached to the real
     * root dentry, but maintain our dentry for inheritance).
     *
     * We hide the view context in a structure following the dentry
     * operations, thus leaving the d_fsdata available if for some
     * reason the file system ever gets hold of it.
     */
    dentry = VNODE_D_ALLOC(root, &vnlayer_root_aliasname);
    if (dentry == NULL) {
        MDKI_SLEEP_UNLOCK(&vnlayer_dop_setup_lock);
        kfree(alias);
        VNODE_DPUT(viewdent);
        return NULL;
    }
    igrab(root->d_inode);
    VNODE_D_INSTANTIATE(dentry, root->d_inode);
    /* have to hash it so that /proc doesn't show it as "/ (deleted)" */
    d_rehash(dentry);
    /*
     * NB: when this dentry's ref count goes to zero, its d_delete() method
     * is called, which will ask the dcache to free it immediately.
     */

    alias->dops = vnlayer_root_alias_dops;
    alias->curview = viewdent;
    alias->mnt = MDKI_MNTGET(VTOVFSMNT(viewdir));
    VN_RELE(vw);                        /* consume the reference */
    alias->rootdentry = VNODE_DGET(root);
    if (alias->rootdentry->d_op &&
        alias->rootdentry->d_op->d_hash != NULL)
    {
        alias->dops.d_hash = &vnlayer_root_alias_hash;
    }
    MDKI_SET_DOPS(dentry, &alias->dops);
    /* leave dentry->d_fsdata for file system, if it wants it ... */
    MDKI_SLEEP_UNLOCK(&vnlayer_dop_setup_lock);

    return dentry;
}

STATIC void
vnlayer_root_dop_release(struct dentry *dentry)
{
    vnlayer_root_alias_t *alias = DENT_GET_ALIAS(dentry);

    MDKI_UNSET_DOPS(dentry);
    VNODE_DPUT(alias->curview);
    VNODE_DPUT(alias->rootdentry);
    MDKI_MNTPUT(alias->mnt);
    kfree(alias);
}

STATIC int
vnlayer_root_dop_delete(struct dentry *dentry)
{
    /* please trash it immediately upon disuse */
    return 1;
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38) 
int
vnlayer_root_dop_compare(
    struct dentry *dent,
    struct qstr *name1,
    struct qstr *name2
)
#else
int
vnlayer_root_dop_compare(
    const struct dentry *dparent,
    const struct inode *iparent,
    const struct dentry *dentry,
    const struct inode *inode,
    unsigned int tlen,
    const char *tname,
    const struct qstr *namep
)
#endif
{
    return 1;                      /* please use our lookup routine */
}

struct dentry_operations vnlayer_root_alias_dops = {
    .d_compare = &vnlayer_root_dop_compare,
    .d_release = &vnlayer_root_dop_release,
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
    .d_delete = (int (*)(const struct dentry *)) &vnlayer_root_dop_delete,
#else
    .d_delete = &vnlayer_root_dop_delete,
#endif
};

/*
 * Take vw and use it to create an alias for / which will carry
 * setview state.
 * Consume the count on vw unless we encounter an error.
 * If vw is NULL, clear the setview state.
 */
STATIC int
vnlayer_set_proc_view(
    VNODE_T *viewdir,
    VNODE_T *vw,
    const char *viewname
)
{
    int error;
    DENT_T *alias_dent;

    if (vw == NULL) {
        /* drop current alias */
        MDKI_SET_PROC_RDIR(VNODE_DGET(vnlayer_sysroot_dentry));
        error = 0;
    } else {
        alias_dent = vnlayer_root_dop_setup(vnlayer_sysroot_dentry, vw,
                                            viewdir, viewname);
        if (alias_dent != NULL) {
            /* set_proc_rdir consumes our reference on dentry */
            MDKI_SET_PROC_RDIR(alias_dent);
            error = 0;
        } else {
            error = ENOMEM;
        }
    }
    return error;
}

extern int
mdki_linux_proc_setview(
    VNODE_T *vw,
    VNODE_T *viewdir,
    const char *viewname,
    int *status_ok
)
{
    INODE_T *rdir;
    struct dentry *cur_root_dent;
    vnlayer_root_alias_t *alias;
    int error = EPERM;

    ASSERT(!vw || (vw->v_flag & VLOOPROOT));

    /*
     * Now, do the work to set the current process view
     */

    /* get current root dentry.  don't increment count.  */
    cur_root_dent = vnlayer_get_root_dentry(); 
    rdir = cur_root_dent->d_inode;      /* don't hold */

    if (cur_root_dent == vnlayer_sysroot_dentry) {
        /* Set over no chroot() */
        /* consume count on vw */
        error = vnlayer_set_proc_view(viewdir, vw, viewname);
        ASSERT(error != 0 || vw == NULL || vw == mdki_linux_get_procview());
    } else if (DENT_IS_ROOT_ALIAS(cur_root_dent)) {
        /* Already set to some view */
        alias = DENT_GET_ALIAS(cur_root_dent);
        if (ITOV(alias->curview->d_inode) == vw) {
            /* already set to same view: do nothing */
            ASSERT(vw != NULL);
            VN_RELE(vw);                /* release count on vw */
            error = 0;
        } else {
            /* Changing view or clearing current view */
            error = vnlayer_set_proc_view(viewdir, vw, viewname);
        }
        ASSERT(error != 0 || vw == mdki_linux_get_procview());
    }
    if (error != 0) { 
        if (vw != NULL)
            VN_RELE(vw);            /* Use up refcount on view */
        if (status_ok) {
            *status_ok = FALSE;
            return(0);
        } else {
            return(error);          /* No rights to chroot */
        }
    }
    return(0);
}

extern VNODE_T *
mdki_linux_get_procview(void)
{
    DENT_T *rdent = vnlayer_get_root_dentry();

    if (DENT_IS_ROOT_ALIAS(rdent)) {
        vnlayer_root_alias_t *alias = DENT_GET_ALIAS(rdent);
        ASSERT(MDKI_INOISMVFS(alias->curview->d_inode));
        return ITOV(alias->curview->d_inode);
    }
    return NULL;
}

#endif /* USE_ROOTALIAS_CURVIEW */

#ifdef USE_CHROOT_CURVIEW
/* old style */
extern int
mdki_linux_proc_setview(
    VNODE_T *vw,
    VNODE_T *viewdir,
    const char *viewname,
    int *status_ok
)
{
    INODE_T *rdir;
    struct dentry *tmp, *cur_rdir;

    ASSERT(!vw || (vw->v_flag & VLOOPROOT));

    /*
     * Now, do the work to set the current process view
     */

    if (vw) {
        tmp = vnlayer_get_view_dentry(viewdir, vw, viewname);
    } else
        tmp = VNODE_DGET(vnlayer_sysroot_dentry);

    cur_rdir = vnlayer_get_root_dentry();  /* current root dentry.  don't increment count.  */
    rdir = cur_rdir->d_inode;           /* don't hold */

    if (cur_rdir == vnlayer_sysroot_dentry) {
        /* Set over no chroot() */
        MDKI_SET_PROC_RDIR(tmp);         /* consume count on tmp */
        ASSERT(vw == NULL || vw == mdki_linux_get_procview());
    } else if (cur_rdir == tmp) {
        /* Already set to same view */
        VNODE_DPUT(tmp);
    } else if (MDKI_INOISMVFS(rdir) && (ITOV(rdir)->v_flag & VLOOPROOT)) {
        /* Changing view */
        MDKI_SET_PROC_RDIR(tmp);        /* consume count on tmp */
        ASSERT(vw == NULL || vw == mdki_linux_get_procview());
    } else {                            /* Chrooted and not a view-tag */
        VNODE_DPUT(tmp);                 /* drop refcnt on dentry */
        if (vw) VN_RELE(vw);            /* Use up refcount on view */
        if (status_ok) {
            *status_ok = FALSE;
            return(0);
        } else {
            return(EPERM);          /* No rights to chroot */
        }
    }
    if (vw) VN_RELE(vw);            /* Use up refcount on view */
    return(0);
}

extern VNODE_T *
mdki_linux_get_procview(void)
{
    INODE_T *ip = vnlayer_get_urdir_inode();
    VNODE_T *vw;

    if (MDKI_INOISMVFS(ip)) {
        vw = ITOV(ip);
        return vw;
    }
    return NULL;
}

#endif /* USE_CHROOT_CURVIEW */

/*
 * Only the zombie state is a "non-existent" process.  Others
 * (TASK_RUNNING, TASK_INTERRUPTIBLE, TASK_UNINTERRUPTIBLE,
 * TASK_STOPPED, TASK_SWAPPING) are still valid active states.  If a
 * process is really gone, then find_task_by_pid() won't find it,
 * and/or the parent proc pointers won't lead to it.
 */
extern mdki_boolean_t
mdki_linux_procactive(mvfs_process_t *p)
{
#ifdef EXIT_ZOMBIE
    return (mdki_get_proc_state(p) != EXIT_ZOMBIE);
#else
    return (mdki_get_proc_state(p) != TASK_ZOMBIE);
#endif
}

/* This function will check if the given pid exists.  If the proc argument
 * is supplied it will also verify that it matches that of the pid.
 */

extern mdki_boolean_t
mdki_linux_procexists(
    mvfs_procid_t pid,
    mvfs_process_t *proc
)
{

    mdki_boolean_t exists = TRUE;
    mvfs_process_t *task;

    MDKI_TASKLIST_LOCK();
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,27)
    task = (mvfs_process_t *)find_task_by_pid(pid);
#else
    task = (mvfs_process_t *)pid_task(find_pid_ns(pid, &init_pid_ns), PIDTYPE_PID);
#endif
    MDKI_TASKLIST_UNLOCK();
    if ((task == NULL) || ((proc != NULL) && (proc != task)))
        exists = FALSE;
    return(exists);
}

/* The following function will handle copying out the directory entries.
 * We are passed a function to do this inside the uio structure.
 * This is modeled on the existing uiomove functions.
 * Arguments:
 *     KDIRENT_T *from        dirent to copy out
 *     int *cntp              pointer to total record length
 *     int uio_flag           unused in this implementation
 *     uio_t *uiop            pointer to uio structure
 *     off_t offset           offset of this copy
 *
 * This function will set uiop->uio_resid to 0 on an error or if the
 * filldir function indicates that its buffer space is filled.
 * If the destination buffer space is full, filldir will return -EINVAL.
 * For our purposes, the destination buffer in the uio structure is an
 * opaque type.  When filldir returns -EINVAL we will check our current
 * offset passed in with the one in the uiop structure.  If they are
 * the same, then we will return the error because it means we were
 * passed in a buffer that couldn't take even one entry.  If the offset
 * is past the offset in the uiop structure, it means we have filled
 * the buffer.  In this case we return 0 and set the cnt value to 0.
 * This will cause the loop in mfs_viewdirreaddir to exit and still point
 * to the offset of the next entry.  For other callers, this will be
 * harmless.
 */

extern int
mdki_linux_readdir_uiomove(
    char *from,
    int cnt,
    int uio_flag,
    uio_t *uiop,
    loff_t offset
)
{
    int err = 0;
    int namelen;
    KDIRENT_T *cursor;
    filldir_t ffunc = uiop->uio_func;
    loff_t dirent_offset, prev_offset;

    if (uio_flag != UIO_READ) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s: only good for user reads\n", __func__);
        return EINVAL;
    }
    /*
     * We've been passed in a compact array of entries.
     * call the filldir routine once per entry.
     */

    /*
     * The unconventional Linux-ites strike again.  Other UNIX
     * operating systems set the d_off of an object to be the offset
     * pointing to the *NEXT* dirent, and use that internally and
     * return it to user space.  This is convenient because it's
     * available right in the record at hand (it knows its own length
     * and can add it to a given offset to compute the next record's
     * offset.)
     *
     * One of linux's internal copy routines want the offset passed it
     * to be the offset for *this* dirent, and it takes care of
     * setting that offset for the previous
     * item. (readdir.c:filldir()) It has to handle stuffing the
     * current offset into the output buffer for the previous entry.
     *
     * Another internal copy routine, used for backward compat with
     * old binaries, considers the offset like all other UNIXes, as the
     * offset of the next entry. (readdir.c:fillonedir())
     *
     * It is therefore not possible to have the file system return a
     * directory entry which is correctly interpreted by all callers.
     *
     * Sigh.  We'll make our code work with the "native" dirents
     * style, on the observation that this is the common case we see
     * with our own test tools.
     */

    cursor = (KDIRENT_T *)from;
    dirent_offset = prev_offset = offset;
    while (cursor < (KDIRENT_T *)(from + cnt)) {
        namelen = KDIRENT_GET_NAMLEN(cursor);
        err = (*ffunc)(uiop->uio_buff, KDIRENT_GET_NAME(cursor),
                       namelen,
                       dirent_offset,   /* current entry's starting offset */
                       (unsigned long)KDIRENT_GET_INO(cursor),
                       DT_UNKNOWN
        	      );

        MDKI_TRACE(TRACE_READDIR,
                   "copyout name %.*s @ %p/%p, %lx %lx: err %d\n",
                   namelen, KDIRENT_GET_NAME(cursor), from, cursor,
                   (u_long) dirent_offset, (u_long)KDIRENT_GET_OFF(cursor),
                   -err);
        if (err < 0) {
            /* There wasn't room for this in the buffer. */
            if (dirent_offset != offset) {
                /* We got at least one entry out already */
                uiop->uio_resid = 0; /* indicate buffer full for other methods */
            }
            uiop->uio_rddir_full = TRUE; /* terminate other loops */
            /*
             * Always squash the error code.  Most other Linux FSes
             * interpret a non-zero return from the filldir function
             * as an "I'm full" indication, and not an error.  Some of
             * them treat it as an error code to be returned.  We must
             * treat it as "I'm full" so that NFS exports work.  We
             * are called from NFS server code with filldir ==
             * nfsfh.c:filldir_one() (which returns -1 as an early
             * termination indicator).  The NFS server expects to get
             * a non-error indication back from vfs_readdir() in such
             * a case.
             *
             * We're also called with filldir = nfs3svc_encode_entry()
             * which can return -EINVAL if its buffer is full.
             */
            err = 0;
            break;
        } else {
            prev_offset = dirent_offset;
            dirent_offset = KDIRENT_GET_OFF(cursor); /* get offset of next entry */
            /*
             * indicate next starting point in case this is the last
             * one that fits
             */
            uiop->uio_offset = dirent_offset;
            /* advance the cursor */
            cursor = (KDIRENT_T *)((char *)cursor +
                                   KDIRENT_GET_RECLEN(cursor));
        }
    }
    return vnlayer_errno_linux_to_unix(err);
}

extern int
mdki_linux_readlink_uiomove(
    char *from,
    int cnt,
    int uio_flag,
    uio_t *uiop
)
{

    int len;
    mm_segment_t old_addlim = MAKE_MM_SEG(0);
    int err = 0;

    if (uio_flag != UIO_READ) {
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s: only good for user reads\n", __func__);
        return EINVAL;
    }

    if (cnt > uiop->uio_iov->iov_len)
        len = uiop->uio_iov->iov_len;
    else
        len = cnt;
    if (uiop->uio_segflg == UIO_SYSSPACE) {
        old_addlim = get_fs();
        set_fs(KERNEL_DS);
    }
    if (!access_ok(VERIFY_WRITE, uiop->uio_iov->iov_base, len))
        err = EFAULT;
    else {
        /* don't worry about locking around access/copy checks;
           copy_to_user() should be atomic. */
        if (mdki_copy_to_user(uiop->uio_iov->iov_base, from, len))
            err = EFAULT;
    }
    if (uiop->uio_segflg == UIO_SYSSPACE)
        set_fs(old_addlim);
    if (!err) {
        vnlayer_linux_adjust_uio(uiop, len, TRUE);
    }
    return(err);
}

/*
 * Callers must protect this function and mdki_linux_release_majors()
 * to prevent multiple simultaneous calls.
 */
void
mdki_linux_release_majors(MVFS_MAJOR_T *tbl)
{
    int i;

    for (i = 1; i <= mvfs_lastmaj; i++) {
        unregister_blkdev(tbl[i], vnlayer_device_name);
    }
    mvfs_lastmaj = 0;
}

/*
 * Check in-use counts to determine whether a file is open while being
 * deleted, in which case we return TRUE and the caller has to provide
 * a way to preserve the file contents until the close completes.
 */
extern mdki_boolean_t
mdki_linux_rename_needed(
    VNODE_T *vp,
    unlink_ctx *context
)
{
    mdki_boolean_t rv;
    DENT_T *dp;
    INODE_T *ip = VTOI(vp);
    struct unlink_ctx *ctx = context;

    if (!context) {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                     "%s: no saved remove dentry %p\n",
                     __func__, mdki_getmycaller());
        rv = TRUE;
    } else {
        dp = ctx->dentry;
        ASSERT(dp->d_inode == ip);
        if (I_COUNT(ip) > 2 || D_COUNT(dp) > 1)
            rv = TRUE;
        else
            rv = FALSE; /* no other users--so really delete it */

        MDKI_TRACE(TRACE_REMOVE,
                   "%s: %s removing vp=%p vcnt=%d dp=%p cnt=%d `%s'\n",
                   __func__,
                   rv ? "not" : "",
                   dp->d_inode,
                   I_COUNT(dp->d_inode),
                   dp, D_COUNT(dp), dp->d_name.name);
    }

    return rv;
}

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38)
extern int
mdki_linux_set_vobrt_vfsmnt(const char *vpath)
{
    int err = 0;
    VFS_T *vfsp;
    VNODE_T *rootvp;
    struct path path;

    err = kern_path(vpath, LOOKUP_FOLLOW, &path);
    if (!err) {
        if (!MDKI_SBISMVFS(path.dentry->d_sb))
            err = -EINVAL;
        else {
            vfsp = SBTOVFS(path.dentry->d_sb);
            err = VFS_ROOT(vfsp, &rootvp);
            err = mdki_errno_unix_to_linux(err);
            if (err == 0) {
                ASSERT(!VTOVFSMNT(rootvp));
                SET_VTOVFSMNT(rootvp, MDKI_MNTGET(path.mnt));
                VN_RELE(rootvp);
            }
        }
	/* only needs to put if lookup succeeded */
    	path_put(&path);
    }
    return vnlayer_errno_linux_to_unix(err);
}
#else /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38) */
extern int
mdki_linux_set_vobrt_vfsmnt(const char *vpath)
{
    int err = 0;
    struct nameidata nd;
    VFS_T *vfsp;
    VNODE_T *rootvp;

    /* path_lookup() now does all the work. */
    err = path_lookup(vpath, LOOKUP_FOLLOW, &nd);
    if (!err) {
        if (!MDKI_SBISMVFS(MDKI_NAMEI_DENTRY(&nd)->d_sb))
            err = -EINVAL;
        else {
            vfsp = SBTOVFS(MDKI_NAMEI_DENTRY(&nd)->d_sb);
            err = VFS_ROOT(vfsp, &rootvp);
            err = mdki_errno_unix_to_linux(err);
            if (err == 0) {
                ASSERT(!VTOVFSMNT(rootvp));
                SET_VTOVFSMNT(rootvp, MDKI_MNTGET(MDKI_NAMEI_MNT(&nd)));
                VN_RELE(rootvp);
            }
        }
    }
    MDKI_PATH_RELEASE(&nd);
    return vnlayer_errno_linux_to_unix(err);
}
#endif /* else LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38) */

int
mdki_linux_statvfs_vnode(
    VNODE_T *cvn,
    STATVFS_T *sbp
)
{
    SUPER_T *sb;
    int error;

    ASSERT(MDKI_INOISCLRVN(VTOI(cvn)));
    sb = CVN_TO_DENT(cvn)->d_inode->i_sb;
    BZERO(sbp, sizeof(*sbp));       /* accomodate lazy file systems */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18) && !defined(SLES10SP2)
    error = (*sb->s_op->statfs)(sb, sbp);
#else
    error = (*sb->s_op->statfs)(CVN_TO_DENT(cvn), sbp);
#endif
    if (error)
        return -error;

    return 0;
}

void
mdki_linux_uioset(uiop, addr, count, offset, segflg)
struct uio *uiop;
caddr_t addr;
size_t count;
MOFFSET_T offset;
int    segflg;
{
    ASSERT(uiop->uio_iov);	/* Set up outside */
    uiop->uio_iov->iov_base = addr;
    uiop->uio_iov->iov_len  = count;
    uiop->uio_iovcnt = 1;
    uiop->uio_offset = offset;
    uiop->uio_segflg = segflg;
    uiop->uio_resid = count;
    uiop->uio_rddir_full = FALSE;
}

/*
 * Fetch vnode attributes, and fill them in to appropriate things in the
 * inode.
 */
void extern
mdki_linux_vattr_pullup(
    VNODE_T *vp,
    VATTR_T *vap,
    int mask
)
{
    INODE_T *ip = VTOI(vp);
    /* XXX locking on inode? */

    MDKI_TRACE(TRACE_GETATTR,
              "%s: vp=%p ip=%p inum=%lu vap=%p nodeid=%lu mask=%x\n",
              __func__, vp, ip, ip->i_ino, vap, VATTR_GET_NODEID(vap),
              mask);

    if (!MDKI_INOISMVFS(ip))
        return;                         /* already set up OK */

    if (mask & AT_NODEID)
        ip->i_ino = VATTR_GET_NODEID(vap);

    /* There is no longer a i_dev field in the 2.6 kernel.  We now do the right
    ** thing in vnode_iop_getattr by putting the fsid into the struct kstat
    ** that we return there.
    */

#define GET(lll,UUU)                            \
    if (mask & AT_ ## UUU)                      \
        ip->i_ ## lll = VATTR_GET_ ## UUU(vap)

    GET(nlink,NLINK);
    GET(uid,UID);
    GET(gid,GID);
    GET(rdev,RDEV);
    GET(size,SIZE);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
    GET(blksize,BLKSIZE);
#else
    if(mask & AT_BLKSIZE) {
        ip->i_blkbits = mdki_linux_blksize_to_bits(vap->va_blksize);
    }
#endif
    GET(blocks,NBLOCKS);
#undef GET
#define GET(lll, UUU)                                    \
    if (mask & AT_ ## UUU) {                             \
        VATTR_GET_ ## UUU ## _TS(vap, &(ip->i_ ## lll)); \
    }
    GET(atime,ATIME);
    GET(mtime,MTIME);
    GET(ctime,CTIME);
#undef GET

    if (mask & AT_TYPE)
        ip->i_mode = vnlayer_vtype_to_mode(VATTR_GET_TYPE(vap)) | (ip->i_mode & ~S_IFMT);
    if (mask & AT_MODE)
        ip->i_mode = VATTR_GET_MODE(vap) | (ip->i_mode & S_IFMT);
    return;
}

extern int
mdki_makedevice(
    int major,
    int minor
)
{
    return MKDEV(major, minor);
}

extern int
mdki_major(
    int dev
)
{
    return MAJOR(dev);
}

extern int
mdki_minor(
    int dev
)
{
    return MINOR(dev);
}

extern void
mdki_make_dcache(
    VNODE_T *dvp,
    VNODE_T *vp,
    const char *nm
)
{
    struct dentry *dent;
    dent = vnlayer_make_dcache(dvp, vp, nm);
    if (dent != NULL) {
        VNODE_DPUT(dent);
    }
    return;
}

extern VNODE_T *
mdki_make_sysroot_vnode(void)
{
    VNODE_T *scvp;
    vnlayer_sysroot_clrvp = scvp = CVN_CREATE(vnlayer_sysroot_dentry,
                                              vnlayer_sysroot_mnt);
#if 0   /* XXX ignore failures, can happen on unmount refchk bug */
    if (!scvp)
        BUG();               /* XXXJTK dies in low memory situation */
#endif
    return scvp;
}


extern void
mdki_release_sysroot_vnode(VNODE_T *sysrootvp)
{
    ASSERT(vnlayer_sysroot_clrvp == sysrootvp);
    VN_RELE(sysrootvp);
    vnlayer_sysroot_clrvp = NULL;
}

extern void
mdki_makevfsdev(
    VFS_T *vfsp,
    int major,
    int minor,
    int nvmajor,
    int nvminor
)
{
    SUPER_T *sb;
    sb = VFSTOSB(vfsp);
    /*
     * To make NFS exports work, the device in sb_dev needs to be the
     * same as the view-extended results.  Do that with the
     * "other-view" device number.
     */
    sb->s_dev = MKDEV(major, minor);
    return;
}

extern int
mdki_vfs_to_dev(VFS_T *vfsp)
{
    SUPER_T *sb;
    sb = VFSTOSB(vfsp);
    return(sb->s_dev);
}

extern void
mdki_mark_vfs_dirty(VFS_T *vfsp)
{
    SUPER_T *sb;

    /*
     * We cannot just set the viewroot s_dirty flag as dirty and leave it 
     * alone so that file system sync routines can be called on every sync.  
     * If we do that, we will hang because linux rewrote the loop that calls
     * us so that it won't move on until the dirty bit is cleared.  So we will 
     * now have to set it when the file system asks us to.
     */

    /* XXX You would think that there would be some locking needed
     * for the s_dirt flag, but I haven't noticed anyone using one.
     */
    sb = VFSTOSB(vfsp);
    sb->s_dirt = 1;
    return;
}

extern u_long
mdki_physmem_pagecnt(void)
{
    return totalram_pages;
}

extern int
mdki_set_accessed(
    VNODE_T *vp,
    VATTR_T *vap
)
{
    struct iattr attr;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
    int err;
#endif

    attr.ia_valid = ATTR_ATIME;
    VATTR_GET_ATIME_TS(vap, &(attr.ia_atime));
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
    err = inode_setattr(VTOI(vp), &attr);
    return vnlayer_errno_linux_to_unix(err);
#else
    setattr_copy(VTOI(vp), &attr);
    mark_inode_dirty(VTOI(vp));
    return 0;
#endif
}

extern int
mdki_set_ichg(
    VNODE_T *vp,
    VATTR_T *vap
)
{
    struct iattr attr;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
    int err;
#endif

    attr.ia_valid = ATTR_CTIME;
    VATTR_GET_CTIME_TS(vap, &(attr.ia_ctime));
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
    err = inode_setattr(VTOI(vp), &attr);
    return vnlayer_errno_linux_to_unix(err);
#else
    setattr_copy(VTOI(vp), &attr);
    mark_inode_dirty(VTOI(vp));
    return 0;
#endif
}

extern int
mdki_set_modified(
    VNODE_T *vp,
    VATTR_T *vap
)
{
    struct iattr attr;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
    int err;
#endif

    attr.ia_valid = ATTR_MTIME;
    VATTR_GET_MTIME_TS(vap, &(attr.ia_mtime));
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
    err = inode_setattr(VTOI(vp), &attr);
    return vnlayer_errno_linux_to_unix(err);
#else
    setattr_copy(VTOI(vp), &attr);
    mark_inode_dirty(VTOI(vp));
    return 0;
#endif
}

int
mdki_set_vfs_opvec(struct vfsops *vfsopp)
{
    if (vnlayer_vfs_opvec != NULL) {
        return EBUSY;
    }
    vnlayer_vfs_opvec = vfsopp;
    return 0;
}

extern int
mdki_suser(void)
{
    /* capable() sets the PF_SUPERPRIV flag for this process and returns 1
    ** if we have CAP_SYS_ADMIN rights.
    */
    return(capable(CAP_SYS_ADMIN) || (MDKI_GET_CURRENT_EUID() == 0));
}

extern int
mdki_sync_size(
    VNODE_T *vp,
    loff_t size,
    int actual
)
{
    struct iattr attr;
    INODE_T *ip;
    int err;
    ip = VTOI(vp);

    if (actual || size > READ_I_SIZE(ip)) {
        attr.ia_valid = ATTR_SIZE;
        attr.ia_size = size;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
        err = inode_setattr(ip, &attr);
#else
        truncate_setsize(ip, attr.ia_size);
        mark_inode_dirty(ip);
        err = 0;
#endif
    } else {
        err = 0;
    }
    return vnlayer_errno_linux_to_unix(err);
}

extern int
mdki_vcount(VNODE_T *vp)
{
    return atomic_read(&((VTOI(vp))->i_count));
}

mdki_boolean_t
mdki_vpismfs(VNODE_T *vp)
{
    if (vp == NULL)
        return FALSE;
    return MDKI_INOISMVFS(VTOI(vp));
}

/************************************************************************
 *
 * The following items are used by vnode file system implementations (and
 * maybe also by the vnode layer)
 */

int mdki_maxminor;		/* This is set during MVFS initialization */

VFS_T *mdki_logging_vfsp;
const char *mdki_panicstr;              /* NULL unless we're panic()ing */
unsigned int mdki_tracing = 0;

extern void
mdki_assfail(
    const char *fmt,
    const char *asserted_str,
    const char *filename,
    const char *funcname,
    int lineno
)
{
    mdki_panicstr = asserted_str;
    printk(fmt, asserted_str, filename, funcname, lineno);
    panic("VNODE assertion failure");
}

/*
 * Be careful when using __builtin_return_address() with a parameter >
 * 0.  The default kernel compile option is -fomit-frame-pointer which
 * will (may?) cause problems (including a potential panic) if we call
 * __builtin_return_address() to look beyond a caller's frame when it
 * has no frame pointer.  So when using __builtin_return_address(n)
 * with n > 0, be sure that there's enough depth to adapter-layer
 * functions that you don't try to get back to a non-adapter function.
 *
 * In fact, in Linux 2.6 with the new kernel build environment, we also get
 * built with -fomit-frame-pointer, so we can't really use these unless we
 * figure out some way to work around the problem.
 */

/*
 * This must be a real function--the return address of this function
 * is the caller's PC (used for debug).
 */
caddr_t
mdki_getreturn(void)
{
    return __builtin_return_address(0);
}

caddr_t
mdki_getmycaller(void)
{
    return 0;
}

caddr_t
mdki_getmycallerscaller(void)
{
    return 0;
}

void
mdki_set_logging_vfsp(VFS_T *vfsp)
{
    mdki_logging_vfsp = vfsp;
}

void
mdki_clear_logging_vfsp(VFS_T *vfsp)
{
    if (mdki_logging_vfsp == vfsp)
        mdki_logging_vfsp = NULL;
}

void
mdki_set_clrvnode_vfsp(VFS_T *vfsp)
{
    vnlayer_clrvnode_vfsp = vfsp;
}

void
mdki_clear_clrvnode_vfsp(VFS_T *vfsp)
{
    if (vnlayer_clrvnode_vfsp == vfsp)
        vnlayer_clrvnode_vfsp = NULL;
}

void
mdki_set_looproot_vp(VNODE_T *vp)
{
    /*
     * take no reference on it (would screw up accounting);
     * we only do equality testing of the pointer value and we depend on the
     * file system to tell us when to delete this.
     */
    vnlayer_looproot_vp = vp;
}

void
mdki_clear_looproot_vp(VNODE_T *vp)
{
    if (vnlayer_looproot_vp == vp)
        vnlayer_looproot_vp = NULL;
}

extern unsigned long
mdki_copy_from_user(
    void *to,
    void *from,
    unsigned long n
)
{
    return(copy_from_user(to, from, n));
}

extern unsigned long
mdki_copy_to_user(
    void *to,
    void *from,
    unsigned long n
)
{
    return(copy_to_user(to, from, n));
}

void
mdki_clear_vfs_opvec(struct vfsops *vfsopp)
{
    if (vnlayer_vfs_opvec == vfsopp) {
        vnlayer_vfs_opvec = NULL;
    } else {
        MDKI_VFS_LOG(VFS_LOG_ERR,
                     "%s: mismatch of opvec reference\n",
                     __func__);
    }
    return;
}

#ifdef RATL_COMPAT32
mdki_boolean_t
mdki_curproc_is_32bit(MVFS_CALLER_INFO *callinfo)
{
    return (callinfo->caller_is_32bit);
}
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

STATIC void
vnlayer_groups_task_to_cred(
    CRED_T *cred,
    struct task_struct *task
)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,32)
    register int i;

    cred->cr_ngroups = LINUX_TASK_NGROUPS(task);
    for (i = 0; i < MIN(cred->cr_ngroups, MDKI_NGROUPS); i++) {
        cred->cr_groups[i] = LINUX_TASK_GROUPS(task)[i];
    }
    /* be clean and zap the rest of the array (if any) */
    for (; i < MDKI_NGROUPS; i++) {
        cred->cr_groups[i] = 0;
    }
#else
    struct cred *task_cred;
    struct group_info *gi_p;
    int i;

    task_cred = prepare_kernel_cred(task);
    if (task_cred) {
        gi_p = get_group_info(task_cred->group_info);
        put_cred(task_cred);
        cred->cr_ngroups = MIN(gi_p->ngroups, MDKI_NGROUPS);
        for (i = 0; i < cred->cr_ngroups; i++) {
            cred->cr_groups[i] =  gi_p->blocks[0][i];
        }
        /* be clean and zap the rest of the array (if any) */
        for (; i < MDKI_NGROUPS; i++) {
            cred->cr_groups[i] = 0;
        }
        put_group_info(gi_p);
    } else {
        /* not enough memory? Can't do anything but log the error */
        MDKI_VFS_LOG(VFS_LOG_ERR, "%s: prepare_kernel_cred returned NULL\n",
                     __func__)
        /* clean stale data */
        for (i = 0; i < MDKI_NGROUPS; i++) {
                cred->cr_groups[i] = 0;
        }
    }
#endif
}

/*
 * mdki_dup_default_creds
 *
 * This function will create a new cred structure and copy the current
 * cred data from te task structure.
 */

extern CRED_T *
mdki_dup_default_creds(
#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
    const char *file,
    const char *func,
    int line
#else
    void
#endif
)
{
    CRED_T *cred;

    if ((cred = kmem_cache_alloc(vnlayer_cred_cache, GFP_KERNEL)) != NULL) {
        atomic_set(&cred->cr_ref, 1);   /* One user for now */
        cred->cr_euid = MDKI_GET_CURRENT_EUID();
        cred->cr_egid = MDKI_GET_CURRENT_EGID();
        cred->cr_ruid = MDKI_GET_CURRENT_UID();
        cred->cr_rgid = MDKI_GET_CURRENT_GID();
        cred->cr_suid = MDKI_GET_CURRENT_SUID();
        cred->cr_sgid = MDKI_GET_CURRENT_SGID();
        cred->cr_fsuid = MDKI_GET_CURRENT_FSUID();
        cred->cr_fsgid = MDKI_GET_CURRENT_FSGID();
        vnlayer_groups_task_to_cred(cred, current);

        MDKI_TRACE(TRACE_CREDS,"crdefault %p (%d/%d) from %s:%s:%d\n",
                   cred, cred->cr_fsuid, cred->cr_fsgid,
                   file, func, line);
    }
    return(cred);
}

extern CRED_T *
mdki_crdup(
    CRED_T *cred
#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
    , const char *file,
    const char *func,
    int line
#endif
)
{
    CRED_T *new_cred;

    if ((new_cred = kmem_cache_alloc(vnlayer_cred_cache, GFP_KERNEL)) != NULL) {
        /* Our caller must have a ref for the original cred, so it won't go
        ** away during this copy.  Also, I guess nobody else can be changing
        ** it, which might make the copy inconsistent (at least we didn't worry
        ** about it before).  The new cred only has one ref.
        */
        BCOPY(cred, new_cred, sizeof(CRED_T));
        atomic_set(&new_cred->cr_ref, 1);
    }
    MDKI_TRACE(TRACE_CREDS,"crdup %p (%d/%d) => %p from %s:%s:%d\n",
              cred, cred->cr_fsuid, cred->cr_fsgid, new_cred,
              file, func, line);
    return(new_cred);
}

extern void
mdki_crhold(
    CRED_T *cred
#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
    , const char *file,
    const char *func,
    int line
#endif
)
{
    MDKI_TRACE(TRACE_CREDS,"crhold %p ref=%u from %s:%s:%d\n",
              cred, atomic_read(&cred->cr_ref), file, func, line);
    atomic_inc(&cred->cr_ref);
}

extern void
mdki_crfree(
    CRED_T *cred
#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
    , const char *file,
    const char *func,
    int line
#endif
)
{
    MDKI_TRACE(TRACE_CREDS,"crfree %p ref=%u from %s:%s:%d\n",
              cred, atomic_read(&cred->cr_ref), file, func, line);
    if (atomic_dec_and_test(&cred->cr_ref)) {
        /* Ref count is 0, we can destroy it now.  Note, there is no
        ** reactivation race here because the ref count starts at 1 when the
        ** cred is created, so if it is going to zero now there can't be
        ** anybody else with a reference to it.  Also, this atomic op is
        ** defined to do (the equivalent of) memory barriers before and after
        ** so all CPUs will see the same count.
        */
        kmem_cache_free(vnlayer_cred_cache, cred);
    }
}

extern time_t
mdki_ctime(void)
{
    /* current_kernel_time reads xtime as does get_seconds, <linux/time.h>. */
    return(get_seconds());
}

/*
 * Return TRUE if the CVN has enough detail attached to the dentry that we
 * should use it.
 */
mdki_boolean_t
mdki_cvn_is_stable(VNODE_T *cvn)
{
    if (MDKI_INOISCLRVN(VTOI(cvn)) &&
        !(cvn->v_flag & VDOOMED))
    {
        return TRUE;
    }
    return FALSE;
}

extern VNODE_T *
mdki_vn_hold(VNODE_T *vp)
{
    INODE_T *ip = igrab(VTOI(vp));

    if (ip != NULL && ip != VTOI(vp))
        BUG();
    if (ip == NULL)
        return NULL;
    ASSERT(!(ip->i_state & (I_FREEING | I_CLEAR)));
    /* shadow inodes sometimes get their counts manipulated by vn_hold/rele */
    ASSERT(vp->v_sanity == VNODE_SANITY || MDKI_INOISSHADOW(ip));
    return vp;
}

#ifdef MVFS_DEBUG
/*
 * This function needs to be real and not inlined because of include file
 * ordering.  We don't want this in <mvfs_vnode.h> because that file
 * doesn't know about the inode/vnode embedding, and the need to call
 * vnlayer_vn_hold().
 */
extern VNODE_T *
mdki_debug_vn_hold(
    VNODE_T *vp,
    char *file,
    const char *function,
    int line
)
{
    if (MDKI_INOISCLRVN(VTOI(vp))) {
        MDKI_TRACE(TRACE_VNODES,"vnlayer_vn_hold %s:%s:%d, vp=%p type=%d d_name=%s cnt=++%d\n",
                   file, function, line, vp, vp->v_type,
                   CVN_TO_DENT(vp)->d_name.name,
                   V_COUNT(vp));
    } else {
        MDKI_TRACE(TRACE_VNODES,"vnlayer_vn_hold %s:%s:%d, vp=%p type=%d cnt=++%d\n",
                   file, function, line, vp, vp->v_type, V_COUNT(vp));
    }
    return mdki_vn_hold(vp);
}
#endif

extern void
mdki_vn_rele(VNODE_T *vp)
{
    /* shadow inodes sometimes get their counts manipulated by vn_hold/rele */
    ASSERT(vp->v_sanity == VNODE_SANITY || MDKI_INOISSHADOW(VTOI(vp)));
    iput(VTOI(vp));
    return;
}

extern void
mdki_decrement_link(VNODE_T *vp)
{
    VTOI(vp)->i_nlink--;
}

extern void
mdki_increment_link(VNODE_T *vp)
{
    VTOI(vp)->i_nlink++;
}

#if HZ == 1000
#define MDKI_MSECS_TO_JIFFIES(X) (X)
#else
#define MDKI_MSECS_TO_JIFFIES(X) (((X) * HZ) / 1000)
#endif

/* We don't expect to be called with a value of less
 * than about 100000.  If this changes, you will need
 * to make sure that we always have a non-zero timeout
 * value.
 */

extern void
mdki_delay_usec(unsigned long us)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,9)
    unsigned long timeout = MDKI_MSECS_TO_JIFFIES(us/1000);

    set_current_state(TASK_UNINTERRUPTIBLE);
    while (timeout)
        timeout = schedule_timeout(timeout);
#else
    msleep(us/1000);
#endif

}

extern void
mdki_get_rootdir(void *context)
{
    DENT_T **ctx = context;
    *ctx = VNODE_DGET(vnlayer_sysroot_dentry);
}

extern VNODE_T *
mdki_get_ucdir_vnode(void)
{
    INODE_T *ip = vnlayer_get_ucdir_inode();
    VNODE_T *vp;

    if (MDKI_INOISMVFS(ip))
        return VN_HOLD(ITOV(ip));
    /* make a clrvnode out of current->fs->pwd{,mnt} */
    vp = CVN_CREATE(MDKI_FS_PWDDENTRY(current->fs),
                    MDKI_FS_PWDMNT(current->fs));
    return vp;
}

extern VNODE_T *
mdki_get_urdir_vnode(void)
{
    INODE_T *ip = vnlayer_get_urdir_inode();
    VNODE_T *vp;

    if (MDKI_INOISMVFS(ip))
        return VN_HOLD(ITOV(ip));
    /* make a clrvnode out of current->fs->root{,mnt} */
    vp = CVN_CREATE(MDKI_FS_ROOTDENTRY(current->fs),
                    MDKI_FS_ROOTMNT(current->fs));
    return vp;
}

extern void
mdki_hrtime(
    struct timespec *value
)
{
    jiffies_to_timespec(jiffies, value);
}

void
mdki_inactive_finalize(VNODE_T *vp)
{
    /*
     * Clean up inode/vnode state
     */

    if (VTOVFSMNT(vp) != NULL) {
        struct vfsmount *mnt;
        mnt = VTOVFSMNT(vp);
        SET_VTOVFSMNT(vp, NULL);
        MDKI_MNTPUT(mnt);
    }

    /*
     * The grab_inode in vnget is atomic in regards to the i_count
     * going to 0 and the I_FREEING bit being set.  Since vnget holds
     * the file system node's lock from before doing the grab_inode
     * until it calls schedule, and since we hold the lock until after
     * we call wake up, we should never miss a wake up.
     */
    wake_up(&vnlayer_inactive_waitq);
}

extern void
mdki_invalidate_vnode_pages(VNODE_T *vp)
{
    invalidate_remote_inode(VTOI(vp));
}

/*
 * Utility functions probably not useful for non-MVFS vnode file systems.
 */

int
mvfs_unlink_dev_file(void)
{
    int err = 0;
    DENT_T *dent;
    struct nameidata nd;

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38) 
    err = kern_path_parent("/dev/mvfs", &nd);
#else
    /* path_lookup() now does all the work. */
    err = path_lookup("/dev/mvfs", LOOKUP_PARENT, &nd);
#endif
    if (err)
        return(err);
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
    mutex_lock_nested(&MDKI_NAMEI_DENTRY(&nd)->d_inode->i_mutex, I_MUTEX_PARENT);
#else
    LOCK_INODE(MDKI_NAMEI_DENTRY(&nd)->d_inode);
#endif
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
    dent = lookup_hash(&nd.last, nd.dentry);
#else
    dent = lookup_one_len(nd.last.name, MDKI_NAMEI_DENTRY(&nd), nd.last.len);
#endif
    if (IS_ERR(dent))
        err = PTR_ERR(dent);
    if (!err) {
        err = MDKI_VFS_UNLINK(MDKI_NAMEI_DENTRY(&nd)->d_inode, dent,
                         MDKI_NAMEI_MNT(&nd));
        VNODE_DPUT(dent);
    }
    UNLOCK_INODE(MDKI_NAMEI_DENTRY(&nd)->d_inode);
    MDKI_PATH_RELEASE(&nd);
    return(err);
}

int
mvfs_make_dev_file(void)
{
    int err = 0;
    DENT_T *dent;
    struct nameidata nd;

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,38) 
    err = kern_path_parent("/dev/mvfs", &nd);
#else
    /* path_lookup() now does all the work. */
    err = path_lookup("/dev/mvfs", LOOKUP_PARENT, &nd);
#endif
    if (err)
        return(err);
    LOCK_INODE(MDKI_NAMEI_DENTRY(&nd)->d_inode);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
    dent = lookup_hash(&nd.last, nd.dentry);
#else
    dent = lookup_one_len(nd.last.name, MDKI_NAMEI_DENTRY(&nd), nd.last.len);
#endif
    if (IS_ERR(dent))
        err = PTR_ERR(dent);
    if (!err) {
        err = MDKI_VFS_MKNOD(MDKI_NAMEI_DENTRY(&nd)->d_inode, dent,
                        MDKI_NAMEI_MNT(&nd), S_IFBLK|S_IRUGO,
                        MKDEV(mvfs_major, 0));
        if (err == -EEXIST) {
            err = MDKI_VFS_UNLINK(MDKI_NAMEI_DENTRY(&nd)->d_inode, dent,
                             MDKI_NAMEI_MNT(&nd));
            /* Get rid of the old dentry and find a new one */
            VNODE_DPUT(dent);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
            dent = lookup_hash(&nd.last, nd.dentry);
#else
            dent = lookup_one_len(nd.last.name, MDKI_NAMEI_DENTRY(&nd),
                                  nd.last.len);
#endif
            err = MDKI_VFS_MKNOD(MDKI_NAMEI_DENTRY(&nd)->d_inode,
                            dent, MDKI_NAMEI_MNT(&nd),
                            S_IFBLK|S_IRUGO, MKDEV(mvfs_major, 0));
        }
        VNODE_DPUT(dent);
    }
    UNLOCK_INODE(MDKI_NAMEI_DENTRY(&nd)->d_inode);
    MDKI_PATH_RELEASE(&nd);
    return(err);
}

STATIC int
vnlayer_check_types(void)
{
    int err = 0;
    return err;
}

/*
 * kmem_cache_destroy() leaves a stub cache in cases where memory
 * isn't cleaned up fully.  That would be OK except that
 * kmem_cache_create() will BUG() if it finds an identical cache name,
 * meaning that we're subject to crashing when loading a future
 * instance of the vnode module if a previous instance didn't
 * completely clean up.  So we make the names unique by tacking on a
 * hex encoding of the current epoch.
 */
#define SUFFIX "XXXXXXXX"

/*
 * prior to 2.6, the kmem cache name is limited to 19 bytes, including
 * terminator, so make sure these names (plus suffix) fit.
 */
STATIC char vnlayer_cred_cache_name[]   = "vn_cred" SUFFIX;
STATIC char vnlayer_vnode_cache_name[]  = "vn_vnode" SUFFIX;

STATIC char *
vnlayer_encode_epoch_suffix(
    char *name,
    int namelen,
    time_t epoch
)
{
    int indx = namelen - sizeof(SUFFIX);
    snprintf(&name[indx], sizeof(SUFFIX), "%08X", (unsigned int)epoch);
    return name;
}

#define encode_epoch_suffix(cachename)                                       \
  vnlayer_encode_epoch_suffix(vnlayer_##cachename##_cache_name,              \
                              sizeof(vnlayer_##cachename##_cache_name), now)

#if defined(for_each_process)
# define do_all_processes(task,body) for_each_process(task) {body;}
#elif defined(for_each_task)
# define do_all_processes(task, body) for_each_task(task) {body;}
#else
# error need do_all_processes implementation
#endif


#ifdef USE_ROOTALIAS_CURVIEW
static inline mdki_boolean_t
vnlayer_task_matches(struct task_struct *task)
{
    struct fs_struct *fs = task->fs;
    mdki_boolean_t val = FALSE;
    MDKI_FS_LOCK_R_VAR(seq);

    if (fs != NULL) {
        MDKI_FS_LOCK_R(fs, seq);
        val = DENT_IS_ROOT_ALIAS(MDKI_FS_ROOTDENTRY(fs));
        MDKI_FS_UNLOCK_R(fs, seq);
    }
    return val;
}
#endif /* USE_ROOTALIAS_CURVIEW */

#ifdef USE_CHROOT_CURVIEW
static inline mdki_boolean_t
vnlayer_task_matches(struct task_struct *task)
{
    struct fs_struct *fs = task->fs;
    mdki_boolean_t val = FALSE
    MDKI_FS_LOCK_R_VAR(seq);

    if (fs != NULL) {
        MDKI_FS_LOCK_R(fs, seq);
        val = MDKI_INOISMVFS(fs->root->d_inode);
        MDKI_FS_UNLOCK_R(fs, seq);
    }
    return val;
}
#endif /* USE_CHROOT_CURVIEW */

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
typedef unsigned long int uintptr_t;    /* XXX why not available in kernel headers? */
#endif

STATIC void *
vnlayer_nth_proc(loff_t offset)
{
    loff_t idx = 0;
    int pid;
    struct task_struct *task;

    MDKI_TASKLIST_LOCK();
    do_all_processes(task,
        task_lock(task);
        if (vnlayer_task_matches(task)) {
            if (offset == idx) {
                pid = task->pid;
                task_unlock(task);
                MDKI_TASKLIST_UNLOCK();
                return (void *)(uintptr_t)pid;
            }
            idx++;
        }
        task_unlock(task);
    );
    MDKI_TASKLIST_UNLOCK();
    return NULL;
}

STATIC void *
vnlayer_proclist_start(
    struct seq_file *sf,
    loff_t *pos
)
{
    /*
     * pos is an integer index into proclist.
     */
    void *result;
    result = vnlayer_nth_proc(*pos);
    if (result != NULL)
        (*pos)++;
    return result;
}

STATIC void *
vnlayer_proclist_next(
    struct seq_file *sf,
    void *p,
    loff_t *pos
)
{
    void *result;
    result = vnlayer_nth_proc(*pos);
    if (result != NULL)
        (*pos)++;
    return result;
}

STATIC void
vnlayer_proclist_stop(
    struct seq_file *sf,
    void *p
)
{
    return;
}

STATIC int
vnlayer_proclist_show(
    struct seq_file *sf,
    void *p
)
{
    uintptr_t pid = (uintptr_t)p;
    seq_printf(sf, "%d\n", (int)pid);
    return 0;
}

static struct seq_operations vnlayer_proclist_ops = {
    .start = vnlayer_proclist_start,
    .next = vnlayer_proclist_next,
    .stop = vnlayer_proclist_stop,
    .show = vnlayer_proclist_show
};

STATIC int
vnlayer_proclist_open(
    struct inode *inode,
    struct file *file
)
{
    return seq_open(file, &vnlayer_proclist_ops);
}

static struct file_operations setview_operations = {
    .open = vnlayer_proclist_open,
    .read = seq_read,
    .llseek = seq_lseek,
    .release = seq_release,
};

#define MVFS_PROCFS_DIR "fs/mvfs"
#define PROCS "/setviewprocs"
STATIC int
vnlayer_register_procfs(void)
{
    struct proc_dir_entry *myent;
    if (!proc_mkdir(MVFS_PROCFS_DIR, 0))
        return -EINVAL;

    myent = create_proc_entry(MVFS_PROCFS_DIR PROCS, S_IFREG | S_IRUSR, NULL);
    if (myent != NULL) {
        myent->proc_fops = &setview_operations;
    } else {
        remove_proc_entry(MVFS_PROCFS_DIR, NULL);
        return -EINVAL;
    }
    return 0;
}

STATIC void
vnlayer_unregister_procfs(void)
{
    remove_proc_entry(MVFS_PROCFS_DIR PROCS, NULL);
    remove_proc_entry(MVFS_PROCFS_DIR, NULL);
}

enum err_state {
    DONE_NOTHING,
    CHECKED_TYPES,
    REGISTERED_PROCFS,
    REGISTERED_BLKDEV,
    REGISTERED_FILESYS,
    CALLED_MKDEV,
    CALLED_CRED_CACHE_CREATE,
    CALLED_VNODE_CACHE_CREATE,
    INITIALIZED_MVFS,
    DONE_EVERYTHING
};

STATIC void
vnlayer_cleanup_vnode(enum err_state init_state)
{
    switch(init_state) {
      case DONE_EVERYTHING:
      case INITIALIZED_MVFS:
        cleanup_mvfs_module();
      case CALLED_VNODE_CACHE_CREATE:
        kmem_cache_destroy(vnlayer_vnode_cache);
      case CALLED_CRED_CACHE_CREATE:
        kmem_cache_destroy(vnlayer_cred_cache);
      case CALLED_MKDEV:
        mvfs_unlink_dev_file();
      case REGISTERED_FILESYS:
        /* XXX still MVFS-specific: */
        unregister_filesystem(&mvfs_file_system);
      case REGISTERED_BLKDEV:
        unregister_blkdev(mvfs_major, vnlayer_device_name);
      case REGISTERED_PROCFS:
        vnlayer_unregister_procfs();
      case CHECKED_TYPES:
      case DONE_NOTHING:
      default:
        break;
    }
}

int
init_module(void)
{
    enum err_state init_state = DONE_NOTHING;
    int err = 0;
    time_t now = mdki_ctime();
    const char *name;

    err = vnlayer_check_types();
    if (err != 0)
        goto cleanup;
    init_state = CHECKED_TYPES;

    err = vnlayer_register_procfs();
    if (err != 0)
        goto cleanup;
    init_state = REGISTERED_PROCFS;

    err = vnlayer_register_blkdev(mvfs_major);
    if ((mvfs_major == 0) && (err > 0)) {
        /*
         * We let the kernel choose a major device number for us,
         * now record it
         */
        mvfs_major = err;
        err = 0;
    }
    if (err != 0)
        goto cleanup;
    init_state = REGISTERED_BLKDEV;

    err = register_filesystem(&mvfs_file_system);
    if (err != 0)
        goto cleanup;
    init_state = REGISTERED_FILESYS;

    /* Now create /dev/mvfs so that mounts will work */
    err = mvfs_make_dev_file();
    if (err != 0)
        goto cleanup;
    init_state = CALLED_MKDEV;

    name = encode_epoch_suffix(cred);
    if (name == NULL)
        err = -EINVAL;
    else {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
        vnlayer_cred_cache =
            kmem_cache_create(name,
                              sizeof(CRED_T),
                              0, 0, NULL, NULL);
#else
        vnlayer_cred_cache =
            kmem_cache_create(name,
                              sizeof(CRED_T),
                              0, 0, NULL);
#endif
        if (vnlayer_cred_cache == NULL)
            err = -ENOMEM;
    }
    if (err != 0)
        goto cleanup;
    init_state = CALLED_CRED_CACHE_CREATE;

    name = encode_epoch_suffix(vnode);
    if (name == NULL)
        err = -EINVAL;
    else {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
        vnlayer_vnode_cache =
            kmem_cache_create(name,
                              sizeof(vnlayer_vnode_t),
                              0, 0, NULL, NULL);
#else
        vnlayer_vnode_cache =
            kmem_cache_create(name,
                              sizeof(vnlayer_vnode_t),
                              0, 0, NULL);
#endif
        if (vnlayer_vnode_cache == NULL)
            err = -ENOMEM;
    }
    if (err != 0)
        goto cleanup;
    init_state = CALLED_VNODE_CACHE_CREATE;


    err = init_mvfs_module();
    if (err != 0) {
        printk(KERN_ERR "MVFS error: unable to initialize MVFS core\n");
        goto cleanup;
    } else {
        printk("MVFS initialized--adapter built at %s\n",
               mdki_vnode_build_time);
    }
    init_state = DONE_EVERYTHING;

  cleanup:
    if (err != 0) {
        vnlayer_cleanup_vnode(init_state);
    }
    return(err);
}

void
cleanup_module(void)
{
    vnlayer_cleanup_vnode(DONE_EVERYTHING);
}

#ifdef RATL_COMPAT32
extern int
mdki_handle_ioctl(unsigned int cmd)
{
    int err = 0;
/* 
 * 2.6 kernels accept NULL to mean sys_ioctl until 2.6.16 which puts the 
 * handler in the file ops compat_ioctl field.
 */
#if  LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
    err = register_ioctl32_conversion(cmd, NULL /* use sys_ioctl() */);
#endif
    return vnlayer_errno_linux_to_unix(err);
}

extern void
mdki_unhandle_ioctl(unsigned int cmd)
{
#if  LINUX_VERSION_CODE < KERNEL_VERSION(2,6,16)
    unregister_ioctl32_conversion(cmd);
#endif
}
#endif
#if defined(MVFS_DEBUG) || defined(STACK_CHECKING)
/* This is for debugging only, to help check for stack overflows */
void *
mdki_get_stack_check_id(void)
{
#ifdef VN_JOURNAL_CHECK
    return current->journal_info;
#else
    /* can't do anything useful */
    return 0;
#endif
}
#endif /* MVFS_DEBUG || STACK_CHECKING */

mdki_boolean_t
mdki_seek_ctx_open_for_lfs(
    struct seek_ctx *ctx
)
{

    struct file *filep;

    filep = (struct file *) ctx->filep;

    return(filep->f_flags & O_LARGEFILE);
}

mdki_boolean_t
mdki_file_ctx_open_for_lfs(
    file_ctx *ctx
)
{
    struct file *filep;

    filep = (struct file *) ctx;

    return(filep->f_flags & O_LARGEFILE);
}

/* These two functions will initialze and release a call_data structure.
 * The structure pointer is passed in in both cases so that the code will
 * work even if the actual structures are allocated on the stack.
 */

extern void
mdki_linux_init_call_data(CALL_DATA_T *cd)
{
    cd->cred = MDKI_GET_UCRED();
    cd->thr = mvfs_get_thread_ptr();
    return;
}

extern void
mdki_linux_destroy_call_data(CALL_DATA_T *cd)
{
    MDKI_CRFREE(cd->cred);
    cd->cred = NULL;
    mvfs_release_thread_ptr(cd->thr);
    cd->thr = NULL;
    return;
}

/* This function will take a pointer to a call_data structure and to
 * a cred and return a newly allocated call_data structure that is a 
 * copy of the original but with the subsitute cred in it.  This is
 * used in places where we need to pass a cached cred to a function
 * that takes a call_data structure.  It returns a NULL pointer if it
 * cannot allocate a new call_data structure.
 *
 * This function does not get holds on the cred and thread structures.
 * The thread structure is for the currently running process so it should
 * not go away on us.  The cred structure is being picked up from 
 * somewhere before we were called and it presumably had a hold placed
 * on it when it was stored.  The substitute call_data being created is
 * a short lived structure being used to encapsulate to values that would
 * otherwise be passed as separate arguments without holds.  If any of
 * these assumptions are no longer valid, change these functions to 
 * increment and decrement the appropriate counts.
 */

extern CALL_DATA_T *
mdki_linux_make_substitute_cred(
    CALL_DATA_T *ocd,
    CRED_T *cred
)
{
    CALL_DATA_T *ncd;

    ncd = KMEM_ALLOC(sizeof(*ncd), KM_SLEEP);
    if (ncd != NULL) {
        ncd->thr = MVFS_CD2THREAD(ocd);
        ncd->cred = cred;
    }
    return ncd;
}

extern void
mdki_linux_free_substitute_cred(
    CALL_DATA_T *cd
)
{
    KMEM_FREE(cd, sizeof(*cd));
}

static const char vnode_verid_mvfs_linux_mdki_c[] = "$Id:  2900d35d.ec6311e1.905b.00:01:84:c3:8a:52 $";
