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
 * Dentry operations (and some utility functions) for MVFS
 */

extern int
vnode_dop_revalidate(
    DENT_T *dentry,
    struct nameidata *nd
);

extern void
vnode_dop_release(DENT_T *dentry);

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
extern int
vnode_dop_hash(
    const DENT_T *dentry,
    const struct inode *inode,
    struct qstr *namep
);

extern int
vnode_dop_compare(
    const struct dentry *dparent,
    const struct inode *iparent,
    const struct dentry *dentry,
    const struct inode *inode,
    unsigned int tlen,
    const char *tname,
    const struct qstr *namep
);
#else
extern int
vnode_dop_hash(
    DENT_T *dentry,
    struct qstr *namep
);

extern int
vnode_dop_compare(
    struct dentry *dent,
    struct qstr *name1,
    struct qstr *name2
);
#endif
/* other declarations in common code, used by shadow code */

/* This is our oportunity to provide special handling for our dentries.
 * In case we are pointing to a real dentry, we will use the shadow
 * d_release function to get rid of it.
 */

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
#define DOPS_INITIALIZER                                                \
    .d_revalidate = vnode_dop_revalidate,                               \
    .d_hash = vnode_dop_hash, /* hash (we overload it for auditing) */  \
    .d_delete = (int (*)(const struct dentry *)) vnode_dop_delete,      \
    .d_release = vnode_dop_release,                                     \
    .d_compare = vnode_dop_compare,
    /* leave iput NULL */
#else /* LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32) */
#define DOPS_INITIALIZER                                                \
    .d_revalidate = vnode_dop_revalidate,                               \
    .d_hash = vnode_dop_hash, /* hash (we overload it for auditing) */  \
    .d_delete = vnode_dop_delete,                                       \
    .d_release = vnode_dop_release,                                     \
    .d_compare = vnode_dop_compare,
    /* leave iput NULL */
#endif /* else LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32) */

struct dentry_operations vnode_dentry_ops = { DOPS_INITIALIZER };

/*
 * Use a distinct dentry op vector so we can distinguish dentries for
 * setview and non-setview trees
 */
struct dentry_operations vnode_setview_dentry_ops = { DOPS_INITIALIZER };

/* return values:
   0:   entry should be tossed
   1:   entry can be trusted
   However, just because we want it invalidated doesn't mean it will be
   tossed--there may be other entries that point to it which can't be
   flushed (e.g. in use) so the entry may still be considered valid.
   Therefore we need to do lookup auditing here.
   */

/* make this use a kmem_cache someday */
#define VATTR_ALLOC() KMEM_ALLOC(sizeof(VATTR_T), KM_SLEEP)
#define VATTR_FREE(vap) KMEM_FREE(vap, sizeof(VATTR_T))
/* Somewhere between 2.6.9 and 2.6.16 the d_child offset in the
 * struct dentry was put into a union.
 */
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,9)
#define D_CHILD d_child
#else
#define D_CHILD d_u.d_child
#endif

extern int
vnode_dop_revalidate(
    DENT_T *dentry,
    struct nameidata *nd
)
{
    INODE_T *parent, *ip;
    VNODE_T *dvp, *vp;
    DENT_T *rdent;
    DENT_T *rpdent;
    VNODE_T *cvp;
    VNODE_T *pcvp;
    int error;
    int ok = 1;
    struct lookup_ctx ctx;
    CALL_DATA_T cd;

    ASSERT_DCACHE_UNLOCKED();
    /* always claim file entries are invalid (we have a better name cache
     * with ClearCase specific information).
     * We would like to be able to just claim that directory entries are
     * invalid, but even if we do so, they will not be tossed away if the
     * directories are still in use.  So for mvfs directories we will 
     * update the inode data if we cannot get rid of it.
     */

    parent = dentry->d_parent->d_inode;
    ip = dentry->d_inode;
    vp = ITOV(ip);
    dvp = ITOV(parent);
    if (ip == NULL) {                   /* negative dentry */
	/* There is no need to do anything on a negative dentry.  Any
	 * auditing will happen when we do the real lookup.
	 * Returning 0 will cause cached_lookup to invalidate this
	 * dentry and real_lookup will allocate a new one.  
	 */
        ok = 0;
        return ok;
    }
    mdki_linux_init_call_data(&cd);
    ASSERT(MDKI_INOISMVFS(parent));
    ASSERT(MDKI_INOISMVFS(ip));
    /* For loopback nodes, call the underlying revalidate */
    if ((vp->v_flag & VLOOP) != 0) {
        /* locking OK: don't use d_inode */
        rdent = REALDENTRY_LOCKED(dentry, &cvp);
        if (rdent == 0) {
            ok = 0;   /* if no realdentry, get rid of it */
        } else {
            if (rdent->d_op && rdent->d_op->d_revalidate) {
                ok = (*rdent->d_op->d_revalidate)(rdent, nd);
            } else {
                /* Neither ext3 nor reiserfs provide a d_revalidate
                 * function.  We need to verify the real dentry ourselves
                 * I will assume that our loopback parent has already
                 * validated its real dentry
                 */
                if (rdent != rdent->d_parent) {
                    /* We will trust mountpoints to be valid */
                    rpdent = REALDENTRY_LOCKED(dentry->d_parent, &pcvp);
                    /* Check if there is one.  Our parent might not be a
                     * loopback directory */
                    if (rpdent != 0) {
                        /*
                         * We used to have conditional code that would either
                         * call d_validate or check to see if the dentry is in
                         * the parent's children list.  There is a bug in 
                         * d_validate in kernels before 2.6.38 which can cause
                         * a kernel panic.  The kernel fix for this was to 
                         * have d_validate verify that the dentry is in the 
                         * parent's child list and the function is marked
                         * as deprecated.  So we will just stop calling
                         * d_validate in any case.
                         */
                        ok = 0;
                        if (rdent->d_parent == rpdent) {
                            struct dentry *dp;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
                            spin_lock(&dcache_lock);
#else
                            spin_lock(&rdent->d_lock);
#endif
                            list_for_each_entry(dp, &rpdent->d_subdirs,
                                                D_CHILD) {
                                if (rdent == dp) {
                                    /* mimic d_validate's behaviour */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
                                    dget_locked(rdent);
#else
                                    dget_dlock(rdent);
#endif
                                    ok = 1;
                                    break;
                                }
                            }
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
                            spin_unlock(&dcache_lock);
#else
                            spin_unlock(&rdent->d_lock);
#endif
                        }
                        if (ok != 0) {
                            VNODE_DPUT(rdent);
                        } else {
                        /* This is no good.  Get another one. */
                            if (VTOVFSMNT(pcvp) != VTOVFSMNT(cvp)) {
                                MDKI_MNTPUT(VTOVFSMNT(cvp));
                                SET_VTOVFSMNT(cvp, MDKI_MNTGET(VTOVFSMNT(pcvp)));
                            }
                            VNODE_DPUT((DENT_T *)cvp->v_dent);
                            rdent = lookup_one_len(dentry->d_name.name,
                                                    rpdent,
                                                    dentry->d_name.len);
                            cvp->v_dent = (caddr_t)rdent; 
                        }
                        REALDENTRY_UNLOCK(dentry->d_parent, pcvp);
                    }
                }
            }
            REALDENTRY_UNLOCK(dentry, cvp);
        }
        /* Just call lookup for the auditing. */
        /* LF_AUDIT means no extra reference returned on vpp */
        ctx.flags = 0;
        ctx.dentrypp = NULL;
        (void) VOP_LOOKUP(ITOV(parent), (char *)dentry->d_name.name,
                          &vp, NULL, VNODE_LF_AUDIT, NULL, &cd, &ctx);
    } else { /* Not Loopback, one of our files */
        /*
         * Call VFS layer to inform it of a lookup of the name being
         * revalidated.
         */
        VNODE_T *rvp = NULL;
        ctx.flags = 0;
        ctx.dentrypp = NULL;
        /* LF_LOOKUP includes full auditing */
        error = VOP_LOOKUP(dvp, (char *)dentry->d_name.name, &rvp,
                           NULL, VNODE_LF_LOOKUP, NULL, &cd, &ctx);
        if (error) {
            ASSERT(rvp == NULL);
	    if (vp->v_type != VDIR)
                ok = 0;
        } else {
            if (VTOI(rvp) != dentry->d_inode) {
		VATTR_T *vap;

                MDKI_VFS_LOG(VFS_LOG_INFO, "revalidate lookup changed objects,"
                             " dvp=%p name=%s old=%p new=%p\n",
                             dvp, dentry->d_name.name, VTOI(rvp),
                             dentry->d_inode);
                /* lookup resulted in a different inode! */
		if (vp->v_type == VDIR) {
		    /* If the directory has changed, we might not be able
		     * to get rid of it, so let's update the data.
		     * We also cannot just return 0 because if we do,
		     * and we are looking up ., we will give a bogus
		     * ESTALE error.  So we will do a two step process.
		     * First we will try to get rid of the dentry, and
		     * if we can't, we will update the attributes in the
		     * inode.  We will never invalidate . because of its
		     * use count.
		     */
		    if (!d_invalidate(dentry)) {
		        ok = 0;
		    } else {
			vap = VATTR_ALLOC();
			if (vap == NULL) {
                            /* Can't invalidate.  Can't update.  What to do?
			     * Log the error and punt.
			     */
                            MDKI_VFS_LOG(VFS_LOG_WARN,
			    "%s:Unable to allocate memory for attributes.\n",
			      __func__);
			    ok = 0;
			} else {
                            VATTR_SET_MASK(vap, AT_ALL);
		            error = VOP_GETATTR(vp, vap, 
				        GETATTR_FLAG_UPDATE_ATTRS, &cd);
			    VATTR_FREE(vap);
			}
		    }
		} else {
		    ok = 0;
		}
            }
            VN_RELE(rvp);
        }
    }
    mdki_linux_destroy_call_data(&cd);
    MDKI_TRACE(TRACE_DCACHE,
              "%s: ok %d dp %p cnt %d vp %p parent %p real %p \"%s/%s\"\n",
              __func__, ok, dentry, D_COUNT(dentry), dentry->d_inode,
              dentry->d_parent, REALCVN(dentry),
              dentry->d_parent->d_name.name, dentry->d_name.name);
    return(ok);
}

#define CALLER_IS_NFSD()                                                \
	(mdki_get_ucomm_ptr() != NULL &&                                \
         (*(uint32_t *)mdki_get_ucomm_ptr() == *(uint32_t *)"nfsd"      \
          && mdki_get_ucomm_ptr()[4] == '\0'))

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
extern int
vnode_dop_hash(
    const DENT_T *dentry,
    const struct inode *inode,
    struct qstr *namep
)
#else
extern int
vnode_dop_hash(
    DENT_T *dentry,
    struct qstr *namep                  /* unused */
)
#endif
{
    VNODE_T *dvp, *vp;
    struct lookup_ctx ctx;
    CALL_DATA_T cd;

    ASSERT_DCACHE_UNLOCKED();
    ASSERT_KERNEL_UNLOCKED();
    /*
     * No auditing for nfs daemon (lookups here interfere with dentry
     * splicing).
     */
    if (CALLER_IS_NFSD())
        return 0;
    dvp = ITOV(dentry->d_parent->d_inode);
    ASSERT(MDKI_INOISMVFS(dentry->d_parent->d_inode));
    mdki_linux_init_call_data(&cd);
    vp = ITOV(dentry->d_inode);
    ctx.flags = 0;
    ctx.dentrypp = NULL;
    (void) VOP_LOOKUP(dvp, (char *)dentry->d_name.name,
                      &vp, NULL, VNODE_LF_AUDIT, NULL, &cd, &ctx);
    /* LF_AUDIT means no extra reference returned on vpp */
    mdki_linux_destroy_call_data(&cd);
    return 0;
}

/* Before 2.6.38 we may be called with the dcache_lock already held.
 * In that case we cannot block and we cannot call d_drop directly because it
 * will try to get the dcache_lock.  Therefore we return 1 if we want the VFS
 * to handle unhashing this for us. After 2.6.37 only the dentry's spinlock is
 * acquired, but we do the same as before, just return 1 for unhashing.
 */
extern int
vnode_dop_delete(DENT_T *dentry)
{
    int dropit;

    /*
     * this is called when the d_count is going to zero on an entry.
     * Unhash it, so that it will be recognized as garbage and cleaned
     * up.  (We don't want them hanging around since they consume
     * extra counts and make unmounting, etc. not work very well).
     */
    ASSERT(D_COUNT(dentry) == 0);       /* decrement already done */
    ASSERT_DCACHE_LOCKED();
    ASSERT_KERNEL_UNLOCKED();
    /* This routine must not block, it holds a spinlock */

    if (!d_unhashed(dentry)) {
        dropit = 1;
        /* If this is a negative dentry but there is a cvp pointer in the
         * d_fsdata field, set up d_inode so that the subsequent iput will
         * clean it up for us.
         */
        if ((dentry->d_inode == NULL) && (REALCVN(dentry) != NULL)) {
            dentry->d_inode = VTOI(REALCVN(dentry));
            SET_REALCVN(dentry, NULL); 
        }
    } else {
        dropit = 0;
    }
#if 0
    /* XXX may block when logging, so leave this disabled by default. */
    MDKI_TRACE(TRACE_DCACHE,
              "%s: dp %p cnt %d parent %p real %p \"%s/%s\"\n",
              __func__, dentry, D_COUNT(dentry), dentry->d_parent,
              REALCVN(dentry),
              dentry->d_parent ?
                dentry->d_parent->d_name.name : (const unsigned char *) "",
              dentry->d_name.name);
#endif
    return(dropit);
}

extern void
vnode_dop_release(DENT_T *dentry)
{
    ASSERT_DCACHE_UNLOCKED();
    ASSERT_KERNEL_UNLOCKED();
    MDKI_TRACE(TRACE_DCACHE, "%s %p vp %p parent %p shadow %p shadow dent %p\n",
              __func__, dentry, dentry->d_inode, dentry->d_parent,
              REALCVN(dentry),
              REALCVN(dentry) ? REALDENTRY(dentry) : 0);
    vnode_shadow_dop_release(dentry);
}

/*
 * We may want to have multiple dentries with the same names, to handle
 * multiple view contexts in the same VOB (could have different
 * elements or view-private directories in multiple views).
 * For non-loop objects, fail the compare to force us into our lookup
 * routine.
 * 
 * The input dentry is the dentry of the parent.
 * The output semantics are a variation on those of memcmp.
 * 0 == match non-zero == no match.  Most filesystems seem to use just
 * 0 and 1 and do not deal with the signed results of memcmp.
 * 
 */
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,32)
extern int
vnode_dop_compare(
    const struct dentry *dparent,
    const struct inode *iparent,
    const struct dentry *dentry,
    const struct inode *inode,
    unsigned int tlen,
    const char *tname,
    const struct qstr *namep
)
{
    if (MDKI_INOISMVFS(iparent)) {
        VNODE_T *vp = ITOV(iparent);
        if ((vp->v_flag & (VLOOP | VLOOPROOT)) == 0) {
            /*
             * don't call non-loop objects the same, it may not be (we
             * can't tell in this layer).  Let the lookup routine find
             * the same dentry if there's one that works.
             */
            return 1;
        }
    }
    /*
     * We can use the existing dentries, so compare them and return the
     * result.
     */
    return (tlen != namep->len) || memcmp(namep->name, tname, tlen);
}
#else
extern int
vnode_dop_compare(
    struct dentry *dent,
    struct qstr *name1,
    struct qstr *name2
)
{
    if (MDKI_INOISMVFS(dent->d_inode)) {
        VNODE_T *vp = ITOV(dent->d_inode);
        if ((vp->v_flag & (VLOOP | VLOOPROOT)) == 0) {
            /*
             * don't call non-loop objects the same, it may not be (we
             * can't tell in this layer).  Let the lookup routine find
             * the same dentry if there's one that works.
             */
            return 1;
        }
    }
    /*
     * We can use the existing dentries, so compare them and return the
     * result, switching the values for true and false.
     */
    return !vnlayer_names_eq(name1, name2);
}
#endif

/*
 * This function will make a dcache entry for the specified directory.
 * dvp is a pointer to the parent vnode
 * vp is a pointer to the directory vnode
 * nm is a pointer to the name of the entry
 */

extern DENT_T *
vnlayer_make_dcache(
    VNODE_T *dvp,
    VNODE_T *vp,
    const char *nm
)
{
    struct qstr dname;
    struct dentry *parent;
    struct dentry *dentry;
    struct list_head *list;
    INODE_T *ip = VTOI(vp);

    dname.name = nm;
    dname.len = strlen(nm);
    dname.hash = full_name_hash(dname.name, dname.len);
    list = VTOI(dvp)->i_dentry.next;
    parent = list_entry(list, struct dentry, d_alias);
    /* dcache code takes its lock as required */
    if ((dentry = d_lookup(parent, &dname)) != NULL) {
        MDKI_TRACE(TRACE_DCACHE,"d_lookup found this already? dvp %p vp %p dp %p d->ip %p\n",
                 dvp, vp, dentry, dentry->d_inode);
        (void) d_invalidate(dentry);
        d_drop(dentry);                /* unhash so it can't be found again */
        d_delete(dentry);              /* zap its inode just in case */
        VNODE_DPUT(dentry);             /* let go */
    }

    dentry = VNODE_D_ALLOC(parent, &dname);
    MDKI_TRACE(TRACE_DCACHE,"%s vp %p dp %p\n",__func__, vp, dentry);
    if (dentry) {
        igrab(ip);                 /* bump count for dcache */
        MDKI_SET_DOPS(dentry, &vnode_dentry_ops);
        VNODE_D_ADD(dentry, ip);
    }
    return dentry;
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,38)
#define LOCK_DCACHE()   spin_lock(&dcache_lock)
#define UNLOCK_DCACHE() spin_unlock(&dcache_lock)
#endif

/*
 * Remove the dcache entry pointing to this vnode, dropping its count
 * (caller must keep a reference if it needs to keep using the vnode).
 */
extern void
mdki_rm_dcache(VNODE_T *vp)
{
    DENT_T *dp;
    INODE_T *ip = VTOI(vp);

    /* only called on viewdir nodes */
    dp = MVOP_DENT(ip, &vnode_dentry_ops);
    MDKI_TRACE(TRACE_DCACHE,"rm_dcache %p, dent=%p\n", vp, dp);
    if (!dp)
        return;
    if (dp->d_inode != ip) {
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
        /* i_lock protects i_dentry */
        spin_lock(&ip->i_lock);
#else
        LOCK_DCACHE();
#endif
        /* We're on its alias list, just remove ourselves */
        list_del(&ip->i_dentry);
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
        spin_unlock(&ip->i_lock);
#else
        UNLOCK_DCACHE();
#endif
    } else {
        /* dcache code takes its lock as required */
        if (!d_unhashed(dp)) {
            (void) d_invalidate(dp);
            d_drop(dp);
        }
        /* try to get rid of it, make it a negative entry if not droppable */
        d_delete(dp);
    }
    MDKI_TRACE(TRACE_DCACHE,
              "%s cnt %d inode %p\n", __func__, D_COUNT(dp), dp->d_inode);
    VNODE_DPUT(dp);                      /* drops its hold */
    return;
}

/*
 * Try to drop the dcache entry pointing to this vnode.  Do this in two parts,
 * one which gets the dentry and puts it into a cookie, and the other
 * which does the actual release work.  This is needed for file systems
 * that have to drop some locks before they can flush the dentry.
 */
extern void
mdki_start_flush_dcache(
    VNODE_T *vp,
    void **cookie
)
{
    MDKI_TRACE(TRACE_DCACHE,"flush_dcache vp=%p\n", vp);
    *cookie = VN_HOLD(vp);

    return;
}

extern mdki_boolean_t
mdki_finish_flush_dcache(void *cookie)
{
    DENT_T *dp;
    VNODE_T *vp;
    INODE_T *ip;
    mdki_boolean_t released = FALSE;
    struct list_head *lh, *next;

    if (cookie != NULL) {
        /*
         * We want to remove this entry from the hash chains so it won't
         * be found by dentry lookups.  However, once we remove the dentry
         * from the hash chains, if there are no other references
         * (e.g. from child dentries) the next dput will destroy the
         * dentry and release the inode, which may lead to a call to
         * VOP_INACTIVE().
         *
         * The file system may need to tolerate a recursive lock.
         * Note: d_invalidate() may call VOP_INACTIVE() on other
         * child inodes as it tries to scrub the cache
         */
        vp = (VNODE_T *)cookie;
        ip = VTOI(vp);

        /*
         * Find all dentries, trying to invalidate/drop them one by
         * one.  Instead of using vnlayer_inode2dentry_internal(), we
         * roll our own specialized version in-line here.  Ours does
         * no match filtering--it takes any dentry and tries to free
         * it.
         */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
        spin_lock(&ip->i_lock);
#else
        LOCK_DCACHE();
#endif
        if (!list_empty(&ip->i_dentry)) {
            list_for_each_safe(lh, next, &ip->i_dentry) {
                dp = list_entry(lh, struct dentry, d_alias);
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
                dget(dp);
                spin_unlock(&ip->i_lock);
#else
                dget_locked(dp);
                UNLOCK_DCACHE();
#endif
                (void) d_invalidate(dp);
                d_drop(dp);
                MDKI_TRACE(TRACE_DCACHE,
                           "%s cnt %d inode %p\n", __func__, D_COUNT(dp), dp->d_inode);
                VNODE_DPUT(dp);
                released = TRUE;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
                spin_lock(&ip->i_lock);
#else
                LOCK_DCACHE();
#endif
            }
        }
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,38)
        spin_unlock(&ip->i_lock);
#else
        UNLOCK_DCACHE();
#endif

        VN_RELE(vp);
    }
    return released;
}

/* This function will take an inode(vnode) as input and get its dcache
 * entry if there is one, or  NULL if there isn't one.
 * This function will do a dget on the returned dentry.
 */

DENT_T *
vnlayer_inode2dentry(
    INODE_T *ip,
    struct dentry_operations *ops,
    const char *file,
    const char *func,
    int line,
    char *retpc
)
{
    struct dentry *found = NULL;

    if (list_empty(&(ip->i_dentry))) {
        if (MDKI_INOISMVFS(ip)) {
            /* don't have/need a dentry for MVFS nodes */
            MDKI_TRACE(TRACE_DCACHE, "%s: no dent for ip %p\n", __func__, ip);
        } else if (MDKI_INOISOURS(ip)) {
            MDKI_TRACE(TRACE_DCACHE,
                       "%s: no dent for shadow node ip %p\n",
                       __func__, ip);
        } else {
            MDKI_TRACE(TRACE_DCACHE,
                      "%s: no dent for other node ip %p sb %p ops %p\n",
                      __func__, ip, ip->i_sb, ip->i_op);
        }
    } else {
        found = vnlayer_inode2dentry_internal(ip, NULL, NULL, ops);
    }
    MDKI_TRACE(TRACE_DCACHE, "i2d(%p) = %p, vcnt=%d dcnt=%d @ %s:%s:%d (ra2=%p)\n",
              (ip), found, I_COUNT(ip), D_COUNT(found), file, func, line, retpc);
    return found;
}

#ifdef MVFS_DEBUG
extern DENT_T *
vnode_dget(
    DENT_T *dentry,
    const char *file,
    const char *func,
    int line
)
{
    DENT_T *rd = dget(dentry);
    if (dentry) {
        MDKI_TRACE(TRACE_DCACHE,"dget %p vp %p parent=%p cnt=%d @ %s:%s:%d\n",
                  dentry, dentry->d_inode, dentry->d_parent, D_COUNT(dentry), file, func, line);
    } else {
        MDKI_TRACE(TRACE_DCACHE,"null dget @ %s:%s:%d\n", file, func, line);
    }
    return rd;
}

extern void
vnode_dput(
    DENT_T *dentry,
    const char *file,
    const char *func,
    int line
)
{
    if (dentry) {
        MDKI_TRACE(TRACE_DCACHE,"dput %p vp=%p parent=%p cnt=%d-- @ %s:%s:%d\n",
                  dentry, dentry->d_inode, dentry->d_parent, D_COUNT(dentry), file, func, line);
    } else {
        MDKI_TRACE(TRACE_DCACHE,"null dput @ %s:%s:%d\n", file, func, line);
    }
    dput(dentry);
    return;
}

extern DENT_T *
vnode_d_alloc_root(
    INODE_T * rootip,
    const char *file,
    const char *func,
    int line
)
{
    DENT_T *dent = d_alloc_root(rootip);
    MDKI_TRACE(TRACE_DCACHE,"d_root rip=%p dent=%p @ %s:%s:%d\n",
              rootip, dent, file, func, line);
    return dent;
}

extern DENT_T *
vnode_d_alloc_anon(
    INODE_T * anonip,
    const char *file,
    const char *func,
    int line
)
{
    DENT_T *dent = d_alloc_anon(anonip);
    MDKI_TRACE(TRACE_DCACHE,"d_anon ip=%p dent=%p @ %s:%s:%d\n",
              anonip, dent, file, func, line);
    return dent;
}

extern DENT_T *
vnode_d_splice_alias(
    INODE_T *ip,
    DENT_T *dent,
    const char *file,
    const char *func,
    int line
)
{
    DENT_T *mydent = d_splice_alias(ip, dent);
    MDKI_TRACE(TRACE_DCACHE,
               "d_splice_alias ip=%p mydent=%p reval=%p @ %s:%s:%d\n",
              ip, dent, mydent, file, func, line);
    return mydent;
}

extern DENT_T *
vnode_d_alloc(
    DENT_T *parent,
    const struct qstr *name,
    const char *file,
    const char *func,
    int line
)
{
    DENT_T *dent = d_alloc(parent, name);
    MDKI_TRACE(TRACE_DCACHE,"d_alloc pdent=%p pcnt=%d nm=%s dent=%p @ %s:%s:%d\n",
              parent, D_COUNT(parent), name->name, dent, file, func, line);
    return dent;
}

extern void
vnode_d_add(
    DENT_T *dent,
    INODE_T *ip,
    const char *file,
    const char *func,
    int line
)
{
    d_add(dent, ip);
    /* is this assert still valid?  XXX? shadow? */
    ASSERT(!ip || MDKI_INOISMVFS(ip) ||
           dent->d_sb == ip->i_sb ||
           (ip->i_mode & S_IFMT) == S_IFSOCK ||
           (ip->i_mode & S_IFMT) == S_IFCHR ||
           (ip->i_mode & S_IFMT) == S_IFBLK);

    MDKI_TRACE(TRACE_DCACHE,"d_add pdent=%p pcnt=%d vp=%p cnt=%d dent=%p @ %s:%s:%d\n",
              dent->d_parent, D_COUNT(dent->d_parent), ip, D_COUNT(dent), dent,
              file, func, line);
    return;
}

extern void
vnode_d_instantiate(
    DENT_T *dent,
    INODE_T *ip,
    const char *file,
    const char *func,
    int line
)
{
    if (dent->d_inode) {
        MDKI_VFS_LOG(VFS_LOG_ERR,"%s: vp %p in dp %p!\n", __func__, ip, dent);
    }
    if (!list_empty(&ip->i_dentry)) {
        /* Expected for vnode ops like link, and that is the only trap
         * where I've found it so far.  Not on all the misc failures.
         */
        MDKI_TRACE(TRACE_DCACHE,"d_instantiate already-known inode %p\n", ip);
    }
        
    d_instantiate(dent, ip);
    MDKI_TRACE(TRACE_DCACHE,"d_instantiate pdent=%p pcnt=%d vp=%p cnt=%d dent=%p @ %s:%s:%d\n",
              dent->d_parent, D_COUNT(dent->d_parent), ip, D_COUNT(dent), dent,
              file, func, line);
    return;
}
#endif /* MVFS_DEBUG */

/*
 * This function will find the vfsmount structure directly associated
 * with the dentry.
 */
struct vfsmount *
vnlayer_dent2vfsmnt(DENT_T *dentry)
{

    INODE_T *ip;
    struct vfsmount *mnt;

    ASSERT(dentry->d_inode);
    ip = dentry->d_inode;
    /* If the VP isn't for the mvfs, we are setting back to the system
     * root.  Just get the system vfsmount.
     */
    if (MDKI_INOISOURS(ip)) {
        mnt = VTOVFSMNT(ITOV(ip));
        if (mnt == NULL) {
            /* If we don't have one, try our parents.  This might not
             * be necessary, every vnode should have one, but if it
             * is only directories we will be covered.
             */
            ip = dentry->d_parent->d_inode;
            if (MDKI_INOISOURS(ip))
                mnt = VTOVFSMNT(ITOV(ip));
        }
    } else {
        mnt = vnlayer_sysroot_mnt;
    }
    if (mnt != NULL)
        mnt = MDKI_MNTGET(mnt);
    return(mnt);
}
static const char vnode_verid_mvfs_linux_dops_c[] = "$Id:  07638734.1df111e2.8579.00:01:84:c3:8a:52 $";
