/* * (C) Copyright IBM Corporation 1991, 2010. */
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
/* mvfs_dncops.c */
#include "mvfs_systm.h"
#include "mvfs.h"
#include "mvfs_dnc.h"

/*
 * THEORY OF OPERATION:
 * Prolog:
 *  As of 05/01/95 this name cache got a lot more complicated 
 *  to support case-insensitive entries for NT name-not-found
 *  caching.  What follows will first describe the basic operation
 *  of the name cache, and then the additions/changes made to
 *  support case-insensitive lookups.
 *  
 *  Locking in the name cache is redesigned in v5.0.  Previous
 *  versions use a single global spinlock for all cache operations.
 *  For improved concurrency and scalability on MP systems, this 
 *  single lock is replaced with several different locks, described
 *  below in the section on locking.
 *
 * Case-sensitive name cache:
 *  Basic task:
 *      The basic task of the name cache is to accelerate the single
 *      component VOP_LOOKUP call in the core.  This call takes
 *      (dvp, nm) and returns a (error, vp).  The name cache caches
 *      both valid translations (name found, vp != NULL) and ENOENT
 *      translations (ENOENT, vp == NULL).
 *
 *      This cache is the most heavily performance sensitive portion
 *      of the MVFS.  The basic criteria are:
 *          1) Ideally the cache should NEVER miss.  Even tiny miss-rates
 *             rates can result in significantly degraded performance.
 *             This drives two major themes of this cache:
 *                 - Cache size must be able to hold more translations
 *                   than the system has active vnodes (for those systems
 *                   that have a max number of active vnodes).  This means
 *                   the cache must not hold refcounts on large numbers
 *                   of vnodes (only view-tag vnodes are held, because there
 *                   are only a small number of view-tags in use at a time).
 *                 - Misses should be avoided at all costs.  Therefore, there
 *                   are lots of optimizations to use entries as much
 *                   possible.  If you can think of new ones, please add them!
 *          2) CPU time usage should not be excessive.  This cache
 *             may be hit 5-6 times for a 5 or 6 component pathname
 *             to stat one object.  Hence, spinlocks are used instead
 *             of sleep locks to make this cache reasonably CPU efficient.
 *             This desire, however, is MUCH LESS IMPORTANT that (1) 
 *             above.  A single miss can result in many RPC's that dwarf
 *             the time required to do all kinds of work (like activate
 *             vnodes).
 * 
 *  Partitioned name cache:
 *      The name cache is partitioned into 3 parts (just like Gaul for
 *      aficionados of Julius Caesar).  The purpose of this partitioning
 *      is to minimize the effects of large working sets on overall
 *      cache miss rates.  The three partitions are:
 *          1) Name lookup that results in a dir object
 *          2) Name lookup that results in a file object
 *          3) Name lookup that results in ENOENT returns
 *
 *      Here is how these work.  (1) is the most important to be large
 *      enough.  Even if the working set is 2000 files and won't fit
 *      in the cache, most lookups will be of the form /foo/bar/baz/file0000
 *      to /foo/bar/baz/file1999.  Even with a 0% hit rate on file0000
 *      to file1999 (because the application goes through too many filenames
 *      in succession such that file0000 has been replaced in the cache
 *      by the time the app reaches file1999 and goes back to lookup file0000),
 *      you still have a 75% hit-rate in the name cache because 
 *      the lookups of foo, bar, and baz weren't flushed from the separate
 *      partition of dir lookups.  This effect depends on the normal
 *      use of a modest number of dirs in most builds.
 *
 *      The regular file cache is also very important, and ideally is
 *      normally larger than the working set of files used in a single
 *      edit-compile-debug cycle, so that there are high hit rates on
 *      all translations.
 *
 *      The ENOENT partition holds name translations that return name-not-found.
 *      Particularly in software development, the tools (compilers/linkers)
 *      tend to use search rules, and frequently lookup up names for which
 *      the answer is name-not-found.  Because search rules can get long
 *      and thrash (working set larger than cache), this is also made
 *      a separate partition.  For best performance, this cache
 *      should be very large (number of dirs searched in vain * number
 *      of files looked for.)  As an example: for a 3-level search rule
 *      for header files, for which the header file is usually in the
 *      last dir, there will be twice as much demand for ENOENT 
 *      entries as for found-file entries (first 2 dirs in search rule).
 *
 *  Cache tags:
 *      Entries in the name cache are invalidated on lots of
 *      conditions.  As a result there are a pile of cache tags
 *      that are checked on entries to validate entries:
 *      (to be added later)
 *
 *  Case-insensitive entries
 *      At present (05/01/95) case-insensitive entries are supported
 *      only for ENOENT translations.  (That is because I haven't
 *      resolved all the invalidation issues for other entries, and
 *      I can get enough of a performance improvement from ENOENT 
 *      entries).
 *
 *      The basic rules that have been added to the cache for 
 *      case-insensitive entries are:
 *         a) Case-insensitive entries are actually a flag on
 *            a case-sensitive entry that indicates this is the
 *            correct item for a case-insensitive search.  This
 *            saves storage for the case-correct name which must
 *            be returned in the pathname struct for the caller.
 *         b) Case-insensitive entries in the ENOENT cache are
 *            always stored downcased.  This prevents the addition
 *            of multiple name-cache entries for a single translation
 *            (i.e. for case-insensitive lookups of "foo.DLL" and
 *             "FOO.dll" only a "foo.dll" name cache entry is used, 
 *             using 1 entry instead of two.  This improves cache
 *             hit rates for a given cache size.)
 *         As a result of (a) and (b) there should be only 1 valid
 *         entry for a case-insensitive translation for any given
 *         (dvp, nm) in the cache, and for a name-found translation,
 *         the entry is the same entry as the case-sensitive entry,
 *         so there is no extra cache demand.
 *
 *         c)  The name hash is not sensitive to case.
 *             All case-insensitive synonyms will be on a single hash
 *             chain in the cache, making invalidation simpler.
 *         d)  Whenever an entry (case-sensitive or case-insensitive)
 *             is actually added or removed from the cache (explicitly), then
 *             all case-insensitive synonyms for this translation must be
 *             invalidated.  The reason for this is that adding or 
 *             removing a case-sensitive item may change the results
 *             of a case-insensitive lookup (add name to dir or remove
 *             a file from a dir).
 *
 *  Locking:
 *      mvfs_dnc_rwlock: write lock (exclusive access) for cache
 *        invalidation functions, which need to be guaranteed that a single
 *        pass through the cache can find all pertinent entries; read lock 
 *        (shared access) for operations which add/remove entries.  This
 *        lock is not used for lookup operations
 *      hash chain lock(s):  On NT, a mutex; on UNIX, a pool of SPLOCK_T locks.
 *        NT cannot use a spin lock here because some of the case-insensitive
 *        comparison operations get into trouble with wrong IRQ level when 
 *        under a spin lock.  On UNIX, the ratio of locks to hash chains is 
 *        platform-specific, refer to each mdep header file.  
 *        This lock is used to protect the sanity of each hash chain.  Lookup 
 *        operations will use this lock once the appropriate chain is 
 *        identified.  Operations which add or remove entries also use this 
 *        when modifying the chain.  Protocol is to acquire the mvfs_dnc_rwlock
 *        first if it is required.
 *      mvfs_dnc_lru_lock: a SPLOCK_T, used to protect the sanity of the
 *        cache's LRU list and each entry's in_trans bit.  
 *        Protocol is to acquire the hash chain lock, if any, before acquiring 
 *        the LRU SPLOCK.
 *
 *  Statistics:
 *      This cache has a lot of statistics.  The important stats
 *      are the miss-rate and the causes of the miss.  Ideally, 
 *      every miss should have a statistic, so that one can tell
 *      easily why misses are happening to see what can be done
 *      to minimize them.  Every miss is a performance problem...
 *      so the goal of statistics is to quickly tell why misses 
 *      are happening.  
 *
 *      With the changes of 05/01/95, I added the ability to track
 *      misses due to no entry in the cache from misses due to
 *      invalidations in the cache (previously, invalidate would
 *      purge the entry, so a miss would then look like an ordinary
 *      miss).  The reason this is important, is because an ordinary
 *      miss might be helped by a larger cache, but an invalidate 
 *      caused miss will never be helped by a larger cache.  This should
 *      also help to tell developers quickly if folks in application
 *      space are getting a little invalidation happy to the detriment
 *      of product performance.
 */

/*
 * Internal routine prototypes
 */

STATIC struct mfs_dncent *
mfs_dncfind(
    mfs_fid_t *dvfidp,
    VFS_T *dvfsp,
    VNODE_T *vw, 
    char *nm, 
    int len,
    tbs_boolean_t case_insensitive,
    int hash, 
    CRED_T *cred
);

STATIC void 
mvfs_dncadd_subr(
    VNODE_T *dvp,
    VNODE_T *vw,
    u_int dnc_flags,
    char *nm,
    int len,
    mfs_fid_t *vfidp,
    VTYPE_T type,
    struct timeval *evtp,
    int hash,
    CRED_T *cred
);

STATIC void 
mfs_dnc_inval_case_synonyms(
    mfs_fid_t *dvfidp,
    VNODE_T *vw,
    char *nm,
    int len,
    int hash
);

STATIC int
mvfs_dnclookup_subr(
    struct mfs_dncent *dnp,
    VNODE_T *vw,
    VNODE_T **vpp,
    CALL_DATA_T *cd
);

STATIC int 
mvfs_dncflush_subr(VFS_T *vfsp);
            
STATIC void 
mfs_dncbhadd(
    mfs_dncent_t *dnp,
    view_bhandle_t *bhp
);
STATIC void 
mfs_dncbhset(
    mfs_dncent_t *dnp,
    view_bhandle_t *bhp
);
STATIC int  
mfs_dncbhcheck(
    mfs_dncent_t *dnp,
    mvfs_thread_t *mth
);
STATIC int  
mfs_dnc_nullbhcheck(
    mfs_dncent_t *dnp,
    mvfs_thread_t *mth
);
STATIC void 
mfs_dncrele(struct mfs_dncent *dnp);
STATIC u_long 
mfs_namehash(
    char *nm,
    int *lenp
);
STATIC int  
mvfs_dnclist_init(void);
STATIC int
mvfs_find_dnchashsize(int maxentry);

/*
 * Hash for name cache entries.  All names are hashed in 1 table.
 * Hash size should be a prime number, and the nmhash value should
 * come from the "mfs_namehash()" routine (which also returns length of
 * the string).
 */

#define MFS_DNCHASHMAX 18181    /* max hash size */
#define MFS_DNCHASHMIN 509      /* min hash size */
#define MFS_DNC_AVECHAIN 10     /*average chain length for each hash slot*/
#define MFS_DNCHASH(dvp, nmhash, len, _ncdp)     \
        ((nmhash + len + \
	    VTOM(dvp)->mn_hdr.fid.mf_dbid + \
	    VTOM(dvp)->mn_hdr.fid.mf_gen) % _ncdp->mvfs_dnchashsize)

#define MVFS_RVCHASH(vw, vfsp, _ncdp)                                  \
 (((mfs_uuid_to_hash32(&(VTOM(vw)->mn_view.svr.uuid)) +         \
    mfs_uuid_to_hash32(&(VFS_TO_MMI(vfsp)->mmi_svr.uuid))))     \
  % _ncdp->mvfs_dnchashsize)

mvfs_dnlc_data_t mvfs_dnlc_data_var;

#define NC_SPLOCK_LRU(_dp,_s)   SPLOCK(*(((mfs_dnclru_t *)((_dp)->lruhead))->lruspl),(_s))
#define NC_SPUNLOCK_LRU(_dp,_s) SPUNLOCK(*(((mfs_dnclru_t *)((_dp)->lruhead))->lruspl),(_s))

/*
 * Macros for list management.  All macros insert "after" the
 * element.  These macros make a copy of the "element" to insert after
 * to allow that macro arg to be a ptr in the linked list that is
 * updated by the macro.
 */
#undef NC_INSLRU_LOCKED
#undef NC_RMLRU_LOCKED
#undef NC_INSHASH_LOCKED
#undef NC_RMHASH_LOCKED

#define NC_INSLRU_LOCKED(lrup, dp) { \
        register mfs_dncent_t *LRUP = (mfs_dncent_t *)lrup;  \
        DEBUG_ASSERT((dp)->lrunext == NULL); \
        DEBUG_ASSERT((dp)->lruprev == NULL); \
        (dp)->lrunext = (LRUP)->lrunext; \
        (dp)->lruprev = (LRUP); \
        (LRUP)->lrunext->lruprev = (dp); \
        (LRUP)->lrunext = (dp); \
    }

#define NC_RMLRU_LOCKED(dp) { \
        DEBUG_ASSERT((dp)->lrunext); \
        DEBUG_ASSERT((dp)->lruprev); \
        (dp)->lrunext->lruprev = (dp)->lruprev; \
        (dp)->lruprev->lrunext = (dp)->lrunext; \
        (dp)->lrunext = (dp)->lruprev = NULL; \
    }

#define NC_INSHASH_LOCKED(hp, dp) { \
        register mfs_dncent_t *HP = (mfs_dncent_t *)hp; \
        DEBUG_ASSERT((dp)->next == NULL); \
        DEBUG_ASSERT((dp)->prev == NULL); \
        (dp)->next = (HP)->next; \
        (dp)->prev = (HP); \
        (HP)->next->prev = (dp); \
        (HP)->next = (dp); \
    }

#define NC_RMHASH_LOCKED(dp) {  \
        DEBUG_ASSERT((dp)->next);     \
        DEBUG_ASSERT((dp)->prev);     \
        (dp)->next->prev = (dp)->prev;  \
        (dp)->prev->next = (dp)->next;  \
        (dp)->next = (dp)->prev = NULL; \
        (dp)->dnc_hash = -1;  \
    }

#define SET_IN_TRANS(dp) { \
	DEBUG_ASSERT((dp)->in_trans == 0); \
	(dp)->in_trans = 1; \
    }

#define CLR_IN_TRANS(dp,_ncdp) { \
	DEBUG_ASSERT((dp)->in_trans == 1); \
	(dp)->in_trans = 0; \
    } \
    if (_ncdp->mvfs_old_dnc) { MDB_XLOG((MDB_DNC_REALLOC,"clrintrans %"KS_FMT_PTR_T, dp));} \
    if (_ncdp->mvfs_old_dnc) /* caller provides {} */

#define DROP_REF_IN_TRANS(_ncdp) \
	if (--(_ncdp->mvfs_dnc_nintransit) == 0) { \
	    struct mfs_dncent *odnc = _ncdp->mvfs_old_dnc; \
	    int odncmax = _ncdp->mvfs_old_dncmax; \
	    _ncdp->mvfs_old_dnc = 0; \
	    _ncdp->mvfs_old_dncmax = 0; \
	    MDB_XLOG((MDB_DNC_REALLOC,"freeing dnc %d @ %"KS_FMT_PTR_T"\n", \
				  odncmax, odnc)); \
	    KMEM_FREE(odnc, odncmax*sizeof(struct mfs_dncent)); \
	}

#define UNUSED(dp) ((dp)->dvw == NULL && \
		    MVFS_FLAGOFF((dp)->flags,MVFS_DNC_RVC_ENT))

#define DNC_BUMPVW(vw, stat) \
        if (vw) {       \
           BUMP_PVSTAT(vw, dncstat.stat); \
        }       \
        BUMPSTAT(mfs_dncstat.stat);

/*
 * Same as above, but handles two stats at a time while under lock.
 * Care should be taken not to dereference the per-view stat pointer
 * which could be in paged memory after taking the pvstatlock.  For
 * detailed information on this, check the comment above the pvstat
 * declaration.
 */
#define DNC_BUMPVW_2(vw, stat1, stat2) { \
        if (vw) { \
           SPL_T _ss; \
           struct mvfs_pvstat *pvp = VTOM(vw)->mn_view.pvstat; \
           MVFS_PVSTATLOCK_LOCK(_ss, pvp); \
           BUMP_PVSTAT_LOCKED(pvp, dncstat.stat1); \
           BUMP_PVSTAT_LOCKED(pvp, dncstat.stat2); \
           MVFS_PVSTATLOCK_UNLOCK(_ss, pvp); \
        } \
        BUMPSTAT(mfs_dncstat.stat1); \
        BUMPSTAT(mfs_dncstat.stat2); \
    }

/* find a proper hash size, which should be a prime number, 
 * and satisfy ave chain lenth */
STATIC int
mvfs_find_dnchashsize(int maxentry)
{
    int num;
    int sqnum, i, isprime;

    num = maxentry/MFS_DNC_AVECHAIN;

    if (num <= MFS_DNCHASHMIN)
        return MFS_DNCHASHMIN;
    if (num >= MFS_DNCHASHMAX)
        return MFS_DNCHASHMAX;
    
    while (1) {
        isprime = 1; 
        sqnum = num/2;
        for (i = 2; i <= sqnum; i++)
            if (num%i == 0) {
                isprime = 0;
                break;
            }
        if (isprime) return num;
        num++;
    }
}

/*
 * Routine to set up headers for LRU lists and hash chains, and
 * put entries on LRU.  May be called by setcaches as well as at
 * initialization time, and so assumes that the global cache lock
 * is held exclusively. 
 */
STATIC int 
mvfs_dnclist_init()
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register int i;

    ncdp->mfs_dncmax = mcdp->mvfs_dncdirmax + mcdp->mvfs_dncregmax + mcdp->mvfs_dncnoentmax;
    ncdp->mfs_dnc_enoent_start = mcdp->mvfs_dncdirmax + mcdp->mvfs_dncregmax;

    /* The following code dynamically chooses dnc hash table size (prime 
     * number) based on the max number of entries (mfs_dncmax). So, the average
     * chain length will be kept in a proper range for good performance.
     */
    if (ncdp->mfs_dnchash != NULL) {
            /* free old hash table mem, if it's dynamically allocated */
        KMEM_FREE(ncdp->mfs_dnchash, (ncdp->mvfs_dnchashsize)*sizeof(mfs_dnchash_slot_t));
        ncdp->mfs_dnchash = NULL;
    }

    ncdp->mvfs_dnchashsize = mvfs_find_dnchashsize(ncdp->mfs_dncmax);
    
    ncdp->mfs_dnchash = (mfs_dnchash_slot_t *)
            KMEM_ALLOC((ncdp->mvfs_dnchashsize)*sizeof(mfs_dnchash_slot_t), KM_SLEEP);
    if (ncdp->mfs_dnchash == NULL) {
        mvfs_log(MFS_LOG_WARN, 
                 "Failed to allocate %d bytes for DNC Hash, trying minimum (%d) instead.\n",
                 ncdp->mvfs_dnchashsize*sizeof(mfs_dnchash_slot_t),
                 MFS_DNCHASHMIN*sizeof(mfs_dnchash_slot_t));
        ncdp->mvfs_dnchashsize = MFS_DNCHASHMIN;
        ncdp->mfs_dnchash = (mfs_dnchash_slot_t *)
            KMEM_ALLOC((ncdp->mvfs_dnchashsize)*sizeof(mfs_dnchash_slot_t), KM_SLEEP);
        if (ncdp->mfs_dnchash == NULL) {
            mvfs_log(MFS_LOG_ERR,
                 "Failed to allocate minimum memory for DNC Hash.\n");
            return(ENOMEM);
        }
    }
    
    NC_HASH_LOCK_INIT(ncdp);
    /* Initialize list hdrs */

    for (i=0; i < ncdp->mvfs_dnchashsize; i++) {
	ncdp->mfs_dnchash[i].next = ncdp->mfs_dnchash[i].prev = 
		(mfs_dncent_t *)&(ncdp->mfs_dnchash[i]);
    }
    ncdp->mfs_dncdirlru.lrunext = ncdp->mfs_dncdirlru.lruprev = 
		(mfs_dncent_t *)&(ncdp->mfs_dncdirlru);
    ncdp->mfs_dncdirlru.lruspl = &(ncdp->mvfs_dnc_dirlru_lock);

    ncdp->mfs_dncreglru.lrunext = ncdp->mfs_dncreglru.lruprev =
		(mfs_dncent_t *)&(ncdp->mfs_dncreglru);
    ncdp->mfs_dncreglru.lruspl = &(ncdp->mvfs_dnc_reglru_lock);

    ncdp->mfs_dncnoentlru.lrunext = ncdp->mfs_dncnoentlru.lruprev =
		(mfs_dncent_t *)&(ncdp->mfs_dncnoentlru);
    ncdp->mfs_dncnoentlru.lruspl = &(ncdp->mvfs_dnc_noentlru_lock);

    /* Add the entries to the "free" list */

    for (i=0; i < ncdp->mfs_dncmax; i++) {
	if (i < mcdp->mvfs_dncdirmax) {
	    ncdp->mfs_dnc[i].lruhead = (mfs_dncent_t *)&(ncdp->mfs_dncdirlru);
	    NC_INSLRU_LOCKED(&(ncdp->mfs_dncdirlru), &(ncdp->mfs_dnc[i]));
	} else if (i < (ncdp->mfs_dnc_enoent_start)) {
	    ncdp->mfs_dnc[i].lruhead = (mfs_dncent_t *)&(ncdp->mfs_dncreglru);
	    NC_INSLRU_LOCKED(&(ncdp->mfs_dncreglru), &(ncdp->mfs_dnc[i]));
	} else {
	    ncdp->mfs_dnc[i].lruhead = (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru);
	    NC_INSLRU_LOCKED(&(ncdp->mfs_dncnoentlru), &(ncdp->mfs_dnc[i]));
	}
	ncdp->mfs_dnc[i].nm_p = &(ncdp->mfs_dnc[i]).nm_inline[0];
	ncdp->mfs_dnc[i].dnc_hash = -1; /* not on a hash chain yet */
    }
    return(0);
}

#define DIRMAX_FORMULA(scale) (200 * ((scale)+1))
#define REGMAX_FORMULA(scale) (800 * ((scale)+1))
#define NOENTMAX_FORMULA(scale) (800 * ((scale)+1))

/*
 * Initialize the name lookup cache.
 * Note that this function allocates memory for the dnc with the KM_SLEEP
 * flag set.  This means that, although there is code here to back off the
 * size of the cache in case memory is not available, it will almost never
 * be executed.  In most cases this should not be a problem as we should 
 * already have our caches set to a reasonable size.  The setcache code
 * does use the KM_NOSLEEP flag so that an admin playing with the cache
 * sizes will not cause a hang by oversubscribing memory.
 */
int
mvfs_dncinit(mvfs_cache_sizes_t *mma_sizes)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register mvfs_dnlc_data_t *ncdp;
    struct mfs_dncent *dnp;
    SPL_T srw;
    int err = 0;

    MDKI_DNLC_ALLOC_DATA();
    ncdp = MDKI_DNLC_GET_DATAP();

    /* Default values for some variables.  May not be strictly necessary, 
     * but used to be defaulted at declaration, which has been shuffled due
     * to rearrangement of data definitions for virtualization support.
     * ncdp (name cache data ptr) points to correct data area to use and 
     * all other data is within that structure. 
     */ 
    ncdp->mvfs_dnchashsize = MFS_DNCHASHMIN;
    ncdp->mfs_dnchash = NULL;
    ncdp->mfs_dncmax = 0;     /* Total DNC ents - filled in on init */ 
    ncdp->mfs_dnc_enoent_start = 0;
    ncdp->mvfs_old_dnc = 0;
    ncdp->mvfs_dnc_initialzed = FALSE;
    ncdp->mvfs_dnc_noent_other = 0;

    /* Initialize the global cache lock */
    MVFS_RW_LOCK_INIT(&(ncdp->mvfs_dnc_rwlock), "mvfs_dnlc_lock");

    /* Initialze the locks for LRUs */
    INITSPLOCK(ncdp->mvfs_dnc_dirlru_lock, "mvfs_dirdnc_lru");
    INITSPLOCK(ncdp->mvfs_dnc_reglru_lock, "mvfs_regdnc_lru");
    INITSPLOCK(ncdp->mvfs_dnc_noentlru_lock, "mvfs_noentdnc_lru");

    /* 
     * Set largeinit values 
     * Now any non-zero specific value overrides largeinit default
     */

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCDIRMAX] = mcdp->mvfs_dncdirmax;
    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCREGMAX] = mcdp->mvfs_dncregmax;
    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCNOENTMAX] = mcdp->mvfs_dncnoentmax;
    MVFS_SIZE_DEFLOAD(mcdp->mvfs_dncdirmax, mma_sizes, DNCDIRMAX, 0);
    MVFS_SIZE_DEFLOAD(mcdp->mvfs_dncregmax, mma_sizes, DNCREGMAX, 0);
    MVFS_SIZE_DEFLOAD(mcdp->mvfs_dncnoentmax, mma_sizes, DNCNOENTMAX, 0);
retry:
    if (mcdp->mvfs_dncdirmax == 0) mcdp->mvfs_dncdirmax = DIRMAX_FORMULA(mcdp->mvfs_largeinit);
    if (mcdp->mvfs_dncregmax == 0) mcdp->mvfs_dncregmax = REGMAX_FORMULA(mcdp->mvfs_largeinit);
    if (mcdp->mvfs_dncnoentmax == 0)
        mcdp->mvfs_dncnoentmax = NOENTMAX_FORMULA(mcdp->mvfs_largeinit);

    MVFS_MDEP_DNC_CAP();

    /* Allocate and zero the name cache */

    ncdp->mfs_dncmax = mcdp->mvfs_dncdirmax + mcdp->mvfs_dncregmax + mcdp->mvfs_dncnoentmax;
    if (ncdp->mfs_dncmax == 0) return 0;	/* No name cache wanted */

    ncdp->mfs_dnc = (struct mfs_dncent *)
	KMEM_ALLOC(ncdp->mfs_dncmax*sizeof(struct mfs_dncent), KM_SLEEP);
    /*
     * If the memory allocation fails try lower large init values and 
     * if even 0 fails return with no cache enabled.
     */
    if (ncdp->mfs_dnc == NULL) {
	if (mcdp->mvfs_largeinit > 1) {
	    mvfs_log(MFS_LOG_ERR, "Failed to allocate %d bytes for DNC Cache lowering largeinit from %d to 1\n",
		(ncdp->mfs_dncmax)*sizeof(struct mfs_dncent), mcdp->mvfs_largeinit);
	    mcdp->mvfs_largeinit = 1;
	    mcdp->mvfs_dncdirmax = mcdp->mvfs_dncregmax = mcdp->mvfs_dncnoentmax = 0;
	    goto retry;
        } else if (mcdp->mvfs_largeinit) {
	    mvfs_log(MFS_LOG_WARN, "Failed to allocate %d bytes for DNC Cache lowering largeinit from %d to 0\n",
		(ncdp->mfs_dncmax)*sizeof(struct mfs_dncent), mcdp->mvfs_largeinit, 0);
	    mcdp->mvfs_largeinit = 0;
	    mcdp->mvfs_dncdirmax = mcdp->mvfs_dncregmax = mcdp->mvfs_dncnoentmax = 0;
	    goto retry;
	} else {
	    mvfs_log(MFS_LOG_ERR, "Failed to allocate %d bytes for DNC Cache with largeinit of %d\n",
		(ncdp->mfs_dncmax)*sizeof(struct mfs_dncent), mcdp->mvfs_largeinit);
	    ncdp->mfs_dncmax = 0;
            /* bail out */
            mcdp->mvfs_dncdirmax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCDIRMAX];
            mcdp->mvfs_dncregmax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCREGMAX];
            mcdp->mvfs_dncnoentmax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCNOENTMAX];
	    err = ENOMEM;
            goto nclockfree;
        }
    }
    BZERO(ncdp->mfs_dnc, (ncdp->mfs_dncmax)*sizeof(struct mfs_dncent));

    MVFS_RW_WRITE_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    if ((err = mvfs_dnclist_init()) == 0) { 
        ncdp->mvfs_dnc_initialzed = TRUE;
        MVFS_RW_WRITE_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
        return 0;
    } 

    /* Not successful in initializing DNLC, clean up anything that had 
     * been allocated.
     */
    ncdp->mvfs_dnc_initialzed = FALSE;
    MVFS_RW_WRITE_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
/* XXXX check this clean up code */
    KMEM_FREE(ncdp->mfs_dnc, (ncdp->mfs_dncmax)*sizeof(struct mfs_dncent)); 
nclockfree:
    NC_HASH_LOCK_FREE(&(ncdp->mvfs_dnc_hash_lock)); 
    FREESPLOCK(ncdp->mvfs_dnc_dirlru_lock);
    FREESPLOCK(ncdp->mvfs_dnc_reglru_lock);
    FREESPLOCK(ncdp->mvfs_dnc_noentlru_lock);
    MVFS_RW_LOCK_DESTROY(&(ncdp->mvfs_dnc_rwlock)); 
    return(err);
}

int
mvfs_dnc_setcaches(
    mvfs_cache_sizes_t *szp
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    struct mfs_dncent *new_mvfs_dnc, *free_mvfs_dnc = 0;
    int new_mvfs_dncmax, rval, free_mvfs_dncmax = 0 /* shut up GCC */;
    SPL_T  srw;
    NC_HASH_LOCK_T free_hash_locks;

    if ((!MVFS_SIZE_VALID(szp, DNCDIRMAX) ||
         szp->size[MVFS_SETCACHE_DNCDIRMAX] == mcdp->mvfs_dncdirmax) &&
        (!MVFS_SIZE_VALID(szp, DNCREGMAX) ||
	 szp->size[MVFS_SETCACHE_DNCREGMAX] == mcdp->mvfs_dncregmax) &&
        (!MVFS_SIZE_VALID(szp, DNCNOENTMAX) ||
	 szp->size[MVFS_SETCACHE_DNCNOENTMAX] == mcdp->mvfs_dncnoentmax))
    {
        /* Nothing needs changing */
        return 0;
    }

    new_mvfs_dncmax = MVFS_SIZE_VALID(szp, DNCDIRMAX) ? 
	szp->size[MVFS_SETCACHE_DNCDIRMAX] : mcdp->mvfs_dncdirmax;
    new_mvfs_dncmax += MVFS_SIZE_VALID(szp, DNCREGMAX) ? 
	szp->size[MVFS_SETCACHE_DNCREGMAX] : mcdp->mvfs_dncregmax;
    new_mvfs_dncmax += MVFS_SIZE_VALID(szp, DNCNOENTMAX) ? 
	szp->size[MVFS_SETCACHE_DNCNOENTMAX] : mcdp->mvfs_dncnoentmax;


    /* Note that this has changed to use the KM_NOSLEEP flag to prevent
     * possible hangs or panics if the user tries to overallocate memory.
     * Now if the user sets the cache size too large, they can take what 
     * actions they need to free up memory.  The initialization code will 
     * still sleep since it expects the system was properly tuned.
     */
    new_mvfs_dnc = (struct mfs_dncent *)
	KMEM_ALLOC(new_mvfs_dncmax*sizeof(struct mfs_dncent), KM_NOSLEEP);
    if (new_mvfs_dnc == NULL)
	return ENOMEM;
    BZERO(new_mvfs_dnc, new_mvfs_dncmax*sizeof(struct mfs_dncent));
    MDB_XLOG((MDB_DNC_REALLOC, "new dnc %d @ %"KS_FMT_PTR_T"\n",
			       new_mvfs_dncmax, new_mvfs_dnc));

    /* To avoid the necessity of returning EBUSY here in some cases,
     * we'll need instead to use a reader/writer lock to interlock
     * with the mvfs_dncadd_subr() path so that nothing else could get
     * added to the existing dnc table.  For now, we'll require that
     * cache size adjustments be handled on an idle ClearCase system
     * (no new lookups in progress--processes can still have current
     * directories or open files within the MVFS.)
     */
    rval = mvfs_dncflush_subr(NULL);
    if (rval == -1 || ncdp->mvfs_old_dnc != 0) {
	KMEM_FREE(new_mvfs_dnc, new_mvfs_dncmax*sizeof(struct mfs_dncent));
	MDB_XLOG((MDB_DNC_REALLOC, "EBUSY\n"));
	return EBUSY;
    }

    MVFS_RW_WRITE_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    /* OK, the only things left in the cache are those marked as in
     * transit.  The processes manipulating in-transit entries clean
     * up when clearing the in-transit bit if there is a pending DNC
     * switch.
     */
     
    ncdp->mvfs_old_dnc = ncdp->mfs_dnc;
    ncdp->mfs_dnc = new_mvfs_dnc;
    ncdp->mvfs_dnc_nintransit = rval;
    ncdp->mvfs_old_dncmax = ncdp->mfs_dncmax;
    if (ncdp->mvfs_dnc_nintransit == 0) {
	free_mvfs_dnc = ncdp->mvfs_old_dnc;
	free_mvfs_dncmax = ncdp->mvfs_old_dncmax;
	ncdp->mvfs_old_dnc = 0;
    }
    free_hash_locks =  ncdp->mvfs_dnc_hash_lock;

    MVFS_SIZE_RUNTIME_SET(mcdp->mvfs_dncdirmax, szp, DNCDIRMAX);
    MVFS_SIZE_RUNTIME_SET(mcdp->mvfs_dncregmax, szp, DNCREGMAX);
    MVFS_SIZE_RUNTIME_SET(mcdp->mvfs_dncnoentmax, szp, DNCNOENTMAX);


    mvfs_dnclist_init();
    MVFS_RW_WRITE_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);

    if (free_mvfs_dnc) {
	MDB_XLOG((MDB_DNC_REALLOC,"freeing dnc %d @ %"KS_FMT_PTR_T"\n",
				  free_mvfs_dncmax, free_mvfs_dnc));
	KMEM_FREE(free_mvfs_dnc, free_mvfs_dncmax*sizeof(struct mfs_dncent));
    }
    NC_HASH_LOCK_FREE(&free_hash_locks);

    if (MVFS_SIZE_PRESENT(szp, DNCDIRMAX))
	szp->size[MVFS_SETCACHE_DNCDIRMAX] = mcdp->mvfs_dncdirmax;
    if (MVFS_SIZE_PRESENT(szp, DNCREGMAX))
	szp->size[MVFS_SETCACHE_DNCREGMAX] = mcdp->mvfs_dncregmax;
    if (MVFS_SIZE_PRESENT(szp, DNCNOENTMAX))
	szp->size[MVFS_SETCACHE_DNCNOENTMAX] = mcdp->mvfs_dncnoentmax;

    return 0;
}

int
mvfs_dnc_getcaches(
    mvfs_cache_sizes_t *szp
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    szp->size[MVFS_SETCACHE_DNCDIRMAX] = mcdp->mvfs_dncdirmax;
    szp->size[MVFS_SETCACHE_DNCREGMAX] = mcdp->mvfs_dncregmax;
    szp->size[MVFS_SETCACHE_DNCNOENTMAX] = mcdp->mvfs_dncnoentmax;
    szp->size[MVFS_SETCACHE_DNCHASHTAB_SZ] = ncdp->mvfs_dnchashsize;
    return 0;
}

int
mvfs_dnc_compute_caches(
    ks_int32_t scale_factor,
    mvfs_cache_sizes_t *szp
)
{
#define FILL_IN(cacheval, formula)                              \
    if ((szp->mask & MVFS_CACHEBIT(cacheval)) == 0) {           \
        szp->size[MVFS_SETCACHE_ ## cacheval] = formula;        \
        szp->mask |= MVFS_CACHEBIT(cacheval);                   \
    }
    FILL_IN(DNCDIRMAX, DIRMAX_FORMULA(scale_factor));
    FILL_IN(DNCREGMAX, REGMAX_FORMULA(scale_factor));
    FILL_IN(DNCNOENTMAX, NOENTMAX_FORMULA(scale_factor));
    FILL_IN(DNCHASHTAB_SZ,
            mvfs_find_dnchashsize(szp->size[MVFS_SETCACHE_DNCDIRMAX] +
                                  szp->size[MVFS_SETCACHE_DNCREGMAX] +
                                  szp->size[MVFS_SETCACHE_DNCNOENTMAX]));
#undef FILL_IN
    return 0;
}

/*
 * MVFS_DNCFREE() - free dnc resources
 */
void
mvfs_dncfree()
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    ASSERT(ncdp->mvfs_old_dnc == NULL);
    if (ncdp->mfs_dnc != NULL) {	/* initialized */
        /* all entries' long names should have been flushed when the last
        MVFS mount was unmounted. */
        KMEM_FREE(ncdp->mfs_dnc, (ncdp->mfs_dncmax)*sizeof(struct mfs_dncent));
        NC_HASH_LOCK_FREE(&(ncdp->mvfs_dnc_hash_lock)); 
        FREESPLOCK(ncdp->mvfs_dnc_dirlru_lock);
        FREESPLOCK(ncdp->mvfs_dnc_reglru_lock);
        FREESPLOCK(ncdp->mvfs_dnc_noentlru_lock);
        MVFS_RW_LOCK_DESTROY(&(ncdp->mvfs_dnc_rwlock)); 
        
    }
    if (ncdp->mfs_dnchash != NULL) {
            /* free hash table mem, if it's dynamically allocated */
        KMEM_FREE(ncdp->mfs_dnchash, (ncdp->mvfs_dnchashsize)*sizeof(mfs_dnchash_slot_t));
        ncdp->mfs_dnchash = NULL;
    }
    mcdp->mvfs_dncdirmax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCDIRMAX];
    mcdp->mvfs_dncregmax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCREGMAX];
    mcdp->mvfs_dncnoentmax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_DNCNOENTMAX];

    ncdp->mfs_dncmax = 0;

    MDKI_DNLC_FREE_DATA();
    return;
}

/*
 * Read next NC entry for debug/testing ioctl to scan name cache
 */

int
mfs_dnc_getent(
    struct mfs_ioncent *ncp
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    int i;
    int got_one = FALSE;
    int error = 0;
    mfs_dncent_t *dncentp;

    ncp->eocache = 1;		/* Assume at end */

    if (ncdp->mfs_dnc == NULL) return(0);
   
    if ((dncentp = KMEM_ALLOC(sizeof(*dncentp), KM_SLEEP)) == NULL) {
        return(0);              /* Caller doesn't care. */
    }
    for (i=ncp->offset; i < ncdp->mfs_dncmax; i++) {
	if (ncdp->mfs_dnc[i].in_trans) continue;
	if (UNUSED(&(ncdp->mfs_dnc[i]))) continue;
	*dncentp = ncdp->mfs_dnc[i];
	ncp->offset = i+1;
	got_one = TRUE;
        break;
    }

    /* See if we got one */

    if (!got_one) goto cleanup;	/* Defaults to EOF */

    ncp->eocache = 0;		/* Not EOF */

    ncp->flags   = 0;
    if (dncentp->nullbh) ncp->flags |= MVFS_IONC_NULLBH;
    if (MFS_FIDNULL(dncentp->vfid)) ncp->flags |= MVFS_IONC_ENOENT;
    
    /* 
     * Parse out the new DNC flags 05/06/95
     * This is a pain that we can't just pass the flags out
     * to user-space, but for compatibility, I didn't want
     * to change the user-space structure, so I just added
     * the new flags as new flags in the existing ioctl.
     * As a result, I have to parse out each flag to its corresponding
     * ioctl flag individually.
     */
    if (dncentp->invalid) ncp->flags |= MVFS_IONC_INVALID;
    if (MVFS_FLAGON(dncentp->flags, MFS_DNC_BHINVARIANT))
            ncp->flags |= MVFS_IONC_BHINVARIANT;
    if (MVFS_FLAGON(dncentp->flags, MFS_DNC_NOTINDIR))
            ncp->flags |= MVFS_IONC_NOTINDIR;
    if (MVFS_FLAGON(dncentp->flags, MFS_DNC_CASE_INSENSITIVE))
            ncp->flags |= MVFS_IONC_CASE_INSENSITIVE;
    if (MVFS_FLAGON(dncentp->flags, MVFS_DNC_RVC_ENT))
            ncp->flags |= MVFS_IONC_RVC;

    /* Copy out the time added to the cache */
                    
    ncp->addtime = dncentp->addtime;

    error = mfs_copyout_viewtag(FALSE, ncp->dvw, dncentp->dvw);
    if (error) goto cleanup;

    ASSERT(dncentp->vfsp);
    ASSERT(VFS_TO_MMI(dncentp->vfsp));
    error = mfs_copyout_strbufpn(ncp->mp, VFS_TO_MMI(dncentp->vfsp)->mmi_mntpath);
    if (error) goto cleanup;

    ncp->dfid.dbid = dncentp->dfid.mf_dbid;
    ncp->dfid.gen  = dncentp->dfid.mf_gen;

    /* 
     * FIXME: getdncent - name truncated
     * Name may be truncated if too long.  This code only
     * copies out the name in nm_inline of the dncent, which
     * may be truncated if the real leaf-name is longer
     * than MFS_DNMAXSHORTNAME
     */
    error = mfs_copyout_strbufpn(ncp->nm, dncentp->nm_inline);
    if (error) goto cleanup;

    /*
     * First fill return BH list with -1.-1 and then
     * fill up with as many bh's as will fit/have.
     */

    for (i=0; i < MVFS_IONCBHMAX; i++) {
	ncp->bhlist[i].target_id = 0xffffffff;
	ncp->bhlist[i].build_session = 0xffffffff;
    }
    for (i=0; i < MVFS_IONCBHMAX && i < MFS_DNCBHMAX; i++) {
	ncp->bhlist[i] = dncentp->bh[i];
    }

    error = mfs_copyout_viewtag(FALSE, ncp->vw, dncentp->vvw);
    if (error) goto cleanup;

    ncp->fid.dbid = dncentp->vfid.mf_dbid;
    ncp->fid.gen  = dncentp->vfid.mf_gen;
    ncp->evtime   = dncentp->vevtime;

  cleanup:
    KMEM_FREE(dncentp, sizeof(*dncentp));
    return(error);
}

/*
 * Hash and get len of a name
 */

STATIC u_long MVFS_NOINLINE
mfs_namehash(
    char *nm,
    int *lenp
)
{
    int c;
    u_long hash = 0;
    u_long len = 0;

    /* 
     * To make hash ANSI case-insensitive without worrying about
     * multibyte character sets, we just ignore the bit that
     * discriminates upper from lower-case ANSI 7-bit characters (0x20)
     */
    while((c = *nm++) != '\0') {
	len++;
	hash = (hash >> 16 & 0xffff) + (hash << 3 & 0x7ffff) + (c & ~0x20);
    }

    *lenp = len;
    return (hash);
}

/*
 * MFS_DNCRELE - release resources held by a name cache ent
 *	The protocol for releasing resources is as follows:
 *	Under the SPINLOCK:
 *		a) Unlink from the hash and lru lists
 *		b) Set the "in_trans" bit using SET_IN_TRANS macro
 *		c) Release the SPINLOCK
 *      Call this routine with the (now private) item
 *      Reacquire the SPINLOCK:
 *	        a) Clear the in_trans bit using CLR_IN_TRANS macro
 *	        b) Return the cache entry to the appropriate lists
 */

STATIC void
mfs_dncrele( register struct mfs_dncent *dnp)
{
    ASSERT(dnp->in_trans);
    if (dnp->dvw != NULL) VN_RELE(dnp->dvw);
    dnp->dvw  = NULL;
    if (dnp->vvw != NULL) VN_RELE(dnp->vvw);
    dnp->vvw  = NULL;
    if (dnp->cred != NULL) MDKI_CRFREE(dnp->cred);
    dnp->cred = NULL;
    if (dnp->nm_p != NULL && dnp->nm_p != dnp->nm_inline) STRFREE(dnp->nm_p);
    dnp->nm_p = NULL;
    dnp->flags = 0;
}

/*
 * Add a name cache entry.  NULL value for vp indicates
 * that this should be a "name not found" entry.
 */
void
mfs_dncadd(
    register VNODE_T *dvp,
    u_int dnc_flags,
    register char *nm,
    VNODE_T *vp,
    CRED_T *cred
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    int len;
    register int hash;
    mfs_fid_t vfid;
    struct timeval vevtime;

    /* Require locking here to avoid races in dir leaving confused
       or stale info */

    ASSERT(MISLOCKED(VTOM(dvp)));
    ASSERT(MFS_ISVOB(VTOM(dvp)));

    if (vp) {
	ASSERT(MFS_ISVOB(VTOM(vp)));
	vfid = VTOM(vp)->mn_hdr.fid;
	vevtime = VTOM(vp)->mn_vob.attr.event_time;
    } else {
        vfid.mf_dbid = MFS_NULL_DBID;
        vfid.mf_gen  = MFS_NULL_GEN;
	vevtime.tv_sec = 0;
	vevtime.tv_usec = 0;
    }
    /*
     * Verify downcased name if ENOENT case-insensitive
     * entry.  This is a REQUIREMENT for correct name cache
     * operation.
     */

    if (MVFS_FLAGON(dnc_flags, MFS_DNC_CASE_INSENSITIVE) &&
            !MVFS_STR_ISLOWER(nm)) {
        MDKI_PANIC("dncadd of ENOENT called with mixed-case name");
    }

    /* 
     * Get name hash.
     */

    hash = mfs_namehash(nm, &len);
    if (len == 0) return;       /* Null name never cached */

    /* Modify name hash into DNC hash */

    hash = MFS_DNCHASH(dvp, hash, len, ncdp);

    /*
     * send in the target VP's view if we have one.
     * NOTE:  NT VC++ compiler bug wants the explicit VTYPE_T conversion
     * or it complains of integral size mismatch.
     */
    mvfs_dncadd_subr(dvp, vp ? MFS_VIEW(vp) : NULL,
		     dnc_flags, nm, len, &vfid,
		     (VTYPE_T) (vp ? MVFS_GETVTYPE(vp) : VNON), &vevtime,
		     hash, cred);
}

STATIC void MVFS_NOINLINE
mvfs_dncadd_subr(
    register VNODE_T *dvp,
    VNODE_T *tvw,
    u_int dnc_flags,
    register char *nm,
    int len,
    mfs_fid_t *vfidp,
    VTYPE_T type,
    struct timeval *evtp,
    register int hash,
    CRED_T *cred
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register struct mfs_dncent *dnp;
    SPL_T sh, sl, srw;
    SPLOCK_T *hash_spl;
    mfs_fid_t dvfid;
    VNODE_T *dvw, *stvw;
    VFS_T *dvfsp;
    register mvfs_thread_t *mth = mvfs_mythread();
    register int lru_hash; 
    int addbhinvar = 0;
    int addnoop = 0;
    int addbh = 0;

    /* See if name cache for this mount point */

    if (ncdp->mfs_dnc == NULL) return;	/* No NC at all! */
    if (!mcdp->mvfs_dncenabled) return;	/* Global enable flag */
    if (!mcdp->mvfs_dncnoentenabled && MFS_FIDNULL(*vfidp)) return;
    if (V_TO_MMI(dvp)->mmi_nodnlc) return;

    /* Copy possibly unwired fid & dir view ptr */

    dvfid = VTOM(dvp)->mn_hdr.fid;
    dvw = MFS_VIEW(dvp);
    dvfsp = dvp->v_vfsp;

    /* Count total adds */
    DNC_BUMPVW(dvw, dnc_add);

    /* Lock structures */

    MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    NC_HASH_LOCK(hash, &hash_spl, sh, ncdp);

    /* 
     * Check for entry already existing (under lock). This happens
     * for 3 possible reasons:
     *    1) An entry is being changed (usually create or some rebinding)
     *	     In this case the target fid will mismatch
     *    2) A new build handle or flag bits are being added to the translation
     *       In this case the target fid will match, but the BH will
     *       not be in the DNC entry, or the flag bits will be different.
     *       We unconditionally update the flag bits.
     *    3) A race condition and someone else won.  In this case
     *       the target fid will match, and the BH will be in the
     *       entry.
     *
     * Note: 
     *  adds always expects the case-sensitive name for items
     *  items that were found, and the lower-case name for case-insensitive
     *  adds of ENOENT entries.
     *
     *  The 'case-insensitive' dnc flag indicates that this is also
     *  the correct translation for case-insensitive lookups.
     */

    stvw = MVFS_FLAGON(dnc_flags, MVFS_DNC_RVC_ENT) ? tvw : dvw;
    if ((dnp = mfs_dncfind(&dvfid, dvfsp, stvw,
			   nm, len, FALSE, hash, cred))
	!= NULL)
    {
	if (MFS_FIDEQ(*vfidp, dnp->vfid) && MFS_TVEQ(*evtp, dnp->vevtime)) {

            /* 
             * If the entry was invalid, then we are reclaiming it
             * and we have to initialize some extra stuff in it
             * as if we were adding a new entry that could be
             * stale info.
             */

            if (dnp->invalid) {
                mfs_dncbhset(dnp, &mth->thr_bh);    /* Reset build handles */
                dnp->dncgen = VTOM(dvp)->mn_hdr.dncgen;
            }

            /* Entry now revalidated ... make sure invalid is off */

            dnp->invalid = 0;

	    /* 
             * If entry is bh invariant, then mark it valid for null
	     * bh regardless of bhcheck results.
	     */
	    if (MVFS_FLAGON(dnc_flags, MFS_DNC_BHINVARIANT)) {
		dnp->nullbh = 1;
                addbhinvar = 1;
	    }

            /* Always update the flags with the latest */

            dnp->flags = (u_short)dnc_flags;

            /* See if entry needs BH update */

	    if (mfs_dnc_nullbhcheck(dnp, mth) ||
	        mfs_dncbhcheck(dnp, mth)) {
		/* Don't need to add this bh to the entry because
	         *	it exists already OR
	         *	null bh to bh optimization would work
		 * This gets counted as just a regular add
                 */
                addnoop = 1;
	    } else {
		mfs_dncbhadd(dnp, &mth->thr_bh);
		/* Count adds of non-null bhs */
		if (!MFS_BHNULL(mth->thr_bh)) {
                    addbh = 1;
                }
	    }
            
            NC_HASH_UNLOCK(hash_spl, sh, ncdp);
            MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);

	    /*
	     * Count adds by objects type 
	     */
	    switch (type) {
	    case VNON:
                DNC_BUMPVW(stvw, dnc_addnoent);
		break;
	    case VDIR:
                DNC_BUMPVW(stvw, dnc_adddir);
		break;
	    default:
                DNC_BUMPVW(stvw, dnc_addreg);
                break;
	    }
            if (addbhinvar) {
                DNC_BUMPVW(stvw, dnc_addbhinvariant);
            }
            if (addnoop) {
                DNC_BUMPVW(stvw, dnc_addnoop);
            }
            if (addbh) {
                DNC_BUMPVW(stvw, dnc_addbh);
            }
	    return;
	} else {
	    NC_RMHASH_LOCKED(dnp);	/* FID mismatch, remove entry */
            NC_SPLOCK_LRU(dnp,sl);
            if (MFS_FIDNULL(dnp->vfid) && MVFS_FLAGOFF(dnp->flags, MFS_DNC_NOTINDIR)) {
                ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru));
                ncdp->mvfs_dnc_noent_other--;
                }
            if (!dnp->in_trans) {
	        NC_RMLRU_LOCKED(dnp);
	        SET_IN_TRANS(dnp);
                NC_SPUNLOCK_LRU(dnp,sl);
                NC_HASH_UNLOCK(hash_spl, sh, ncdp);
                MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
	        mfs_dncrele(dnp);
                MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
                NC_SPLOCK_LRU(dnp,sl);
	        CLR_IN_TRANS(dnp,ncdp) {
                    NC_SPUNLOCK_LRU(dnp,sl);
		    DROP_REF_IN_TRANS(ncdp);
	        } else {
		    ASSERT(dnp->lruhead);
		    NC_INSLRU_LOCKED(dnp->lruhead, dnp);
                    NC_SPUNLOCK_LRU(dnp,sl);
	        }
	    } else {
                /* someone beat us to the entry, just leave it */
                NC_SPUNLOCK_LRU(dnp,sl);
                NC_HASH_UNLOCK(hash_spl, sh, ncdp); 
                mvfs_log(MFS_LOG_DEBUG, "dncadd_subr, dropping in_trans ent %x\n",dnp);
            }
            DNC_BUMPVW(stvw, dnc_change);
	}
    } else {
        /* didn't find an existing match */
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
    }

    /* 
     * Take least recently used name ent and use it 
     * Dirs, Files, and NOENTs are separated into different
     * regions to minimize thrashing and keep dirs around a longer
     * time since dirs account for the bulk of the "looked-up" components.
     * Can't lock LRU till we have the pertinent hash chain locked, so
     * we do a little dance, taking the lru next pointer without lock,
     * and verifying it hasn't changed once we do have the lock.
     */

get_lru:
    switch (type) {
    case VDIR:
    	dnp = ncdp->mfs_dncdirlru.lrunext;
        lru_hash = dnp->dnc_hash;  
        if (dnp == NULL || dnp == (mfs_dncent_t *)&(ncdp->mfs_dncdirlru)) {
            MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
	    return;  /* ALL out!!!!???? */
        }
	ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncdirlru));
	break;
    case VNON:
	dnp = ncdp->mfs_dncnoentlru.lrunext;
        lru_hash = dnp->dnc_hash;  
	if (dnp == NULL || dnp == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru)) {
            MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
	    return;  /* ALL out!!!!???? */
        }
	ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru));
	break;
    default:
	dnp = ncdp->mfs_dncreglru.lrunext;
        lru_hash = dnp->dnc_hash;  
        if (dnp == NULL || dnp == (mfs_dncent_t *)&(ncdp->mfs_dncreglru)) {
            MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
	    return;  /* ALL out!!!!???? */
        }
	ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncreglru));
    }

    /* Now if on a hash chain, lock that, then lock LRU and make
     * sure we can still take same entry.
     * Remove from lrulist and hash under lock 
     * CMBU00067693: There is a window here while we are looking at the
     * LRU entry with no locks held.  Narrow the window by using a local
     * variable while selecting the hash lock; also corrected some of the
     *  validations below.
     */
    NC_HASH_LOCK(lru_hash, &hash_spl ,sh, ncdp);   
    NC_SPLOCK_LRU(dnp,sl);

    /* Because there is a window between when we grabbed the dnp from head of 
     * LRU and now, or in case we had to wait for either lock, verify entry 
     */
    if (dnp->in_trans || dnp->lruhead->lrunext != dnp ||  
        (lru_hash != -1 && dnp->dnc_hash != lru_hash)) { 

        NC_SPUNLOCK_LRU(dnp,sl);
        NC_HASH_UNLOCK(hash_spl, sh, ncdp); 
        mvfs_log(MFS_LOG_DEBUG,"dncadd_subr, lru lock race, get lru\n");
        goto get_lru;     /* try again  */ 
    }

    if (dnp->next) {
        NC_RMHASH_LOCKED(dnp);
        if (MFS_FIDNULL(dnp->vfid) && MVFS_FLAGOFF(dnp->flags, MFS_DNC_NOTINDIR)) {
            ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru));
            ncdp->mvfs_dnc_noent_other--;
        }
    }
    NC_RMLRU_LOCKED(dnp);
    SET_IN_TRANS(dnp);
    NC_SPUNLOCK_LRU(dnp,sl);
    NC_HASH_UNLOCK(hash_spl, sh, ncdp);
             
    /* 
     * For any real add (case-sensitive or case-insensitive)
     * we must invalidate any case insensitive synonyms in the
     * cache since they may no longer be valid
     * CMBU00059370: Lock the hash chain for the new hash value. 
     */
    NC_HASH_LOCK(hash, &hash_spl ,sh, ncdp);
    mfs_dnc_inval_case_synonyms(&dvfid, dvw, nm, len, hash);

    NC_HASH_UNLOCK(hash_spl, sh, ncdp);
    MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    mfs_dncrele(dnp);	/* Release vnodes etc. on entry */

    /* Fill in the entry with the new stuff */

    dnp->addtime = MDKI_CTIME();    /* Time added or changed */
    dnp->invalid = 0;               /* Clear bits */
    dnp->flags = (u_short)dnc_flags;         /* Set flag bits passed in */
    dnp->dvw = dvw;
    if (dnp->dvw) VN_HOLD(dnp->dvw);
    dnp->vfsp = dvp->v_vfsp;
    dnp->dfid = VTOM(dvp)->mn_hdr.fid;
    dnp->dncgen = VTOM(dvp)->mn_hdr.dncgen;
    dnp->dnc_hash = hash;  /* save for spinlock pool index */
    mfs_dncbhset(dnp, &mth->thr_bh);
    /* Also valid for null bh if bh_invariant flag passed in. */
    if (MVFS_FLAGON(dnc_flags, MFS_DNC_BHINVARIANT)) dnp->nullbh = 1;
    dnp->vfid = *vfidp;
    dnp->vevtime = *evtp;
    if (tvw) {				/* target's view provided? */
	dnp->vvw = tvw;
	VN_HOLD(dnp->vvw);
    } else {
	dnp->vvw = NULL;
    }
    dnp->len = len;
    if (len < MFS_DNMAXSHORTNAME) {
        BCOPY(nm, dnp->nm_inline, len);
        dnp->nm_inline[len] = '\0';
        dnp->nm_p = dnp->nm_inline;
    } else {
        dnp->nm_p = STRDUP(nm);
        ASSERT(dnp->nm_p);
        /* 
         * Put truncated copy of long name into
         * the stg for ioctl that fetches name-cache entries
         * This can be removed when the ioctl has a way
         * to handle the STRDUP'd name without it getting
         * freed while the copyouts are happening.
         */
        BCOPY(dnp->nm_p, dnp->nm_inline, MFS_DNMAXSHORTNAME-1);
        dnp->nm_inline[MFS_DNMAXSHORTNAME-1] = '\0';
    }

    dnp->cred = cred;
    if (cred) MDKI_CRHOLD(cred);

    /* 
     * Now insert on the new hash chains and at the end of
     * the lru list.  Put at the beginning of the hash
     * on the assumption we will want to find it quickly
     * again. 
     */

    MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    NC_HASH_LOCK(hash, &hash_spl, sh, ncdp);
    NC_SPLOCK_LRU(dnp,sl);
    CLR_IN_TRANS(dnp,ncdp) {
        NC_SPUNLOCK_LRU(dnp,sl);
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
        MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
	DROP_REF_IN_TRANS(ncdp);
	return;
    }
    NC_INSLRU_LOCKED(dnp->lruhead->lruprev, dnp);
    NC_INSHASH_LOCKED(&(ncdp->mfs_dnchash[hash]), dnp);
    if ((type == VNON) && MVFS_FLAGOFF(dnp->flags, MFS_DNC_NOTINDIR)) {
        ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru));
        ncdp->mvfs_dnc_noent_other++;      
    }
    NC_SPUNLOCK_LRU(dnp,sl);
    NC_HASH_UNLOCK(hash_spl, sh, ncdp);
    MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    /*
     * Count adds by objects type 
     */
    switch (type) {
    case VNON:
        DNC_BUMPVW(stvw, dnc_addnoent);
	break;
    case VDIR:
        DNC_BUMPVW(stvw, dnc_adddir);
	break;
    default:
        DNC_BUMPVW(stvw, dnc_addreg);
        break;
    }
    /* Count "long name" adds */
    if (len >= MFS_DNMAXSHORTNAME) {
        DNC_BUMPVW(stvw, dnc_addlong);
    }
	    
    /* count build-handle invariant adds */
    if (MVFS_FLAGON(dnc_flags, MFS_DNC_BHINVARIANT)) {
        DNC_BUMPVW(stvw, dnc_addbhinvariant);
    }
    return;
}

/*
 * MFS_DNC_ADD_FLAG - add a flag to an existing entry
 */

void
mfs_dnc_add_flag(
    VNODE_T *dvp,
    mfs_pn_char_t *nm,
    struct timeval *dtm_p,
    u_int dnc_flags,
    CRED_T *cred
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register struct mfs_dncent *dnp;
    mfs_fid_t dvfid;
    VNODE_T *vw;
    VFS_T *dvfsp;
    int len;
    register int hash;
    mfs_mnode_t *mnp;
    SPLOCK_T *hash_spl;
    SPL_T sh, srw;

    ASSERT(dtm_p != NULL);

    /* See if name cache for this mount point */

    if (ncdp->mfs_dnc == NULL) return;	/* No NC at all! */
    if (!mcdp->mvfs_dncenabled) return;	/* Global enable flag */
    if (V_TO_MMI(dvp)->mmi_nodnlc) return;

    /* 
     * Make sure dir is a VOB object.
     * Other kinds of objects are just a no-op since we don't
     * cache them.
     */

    mnp = VTOM(dvp);
    if (!MFS_ISVOB(mnp)) return;        /* Only vob dirs */

    /*
     * Check mtime of dir to make sure it hasn't changed.  If
     * it has, then the flag we are adding may not be valid due
     * to the changes in the dir while it was unlocked.  In that
     * case, just do nothing in this routine.
     *
     * Note: the dir must stay locked from the time we validate
     * the mtime until the flag has been correctly updated
     * in the name cache for consistency.
     */
    MLOCK(mnp);    
    if (!MFS_ATTRISVALID(dvp) || 
                        !MFS_TVEQ(*dtm_p, mnp->mn_vob.attr.fstat.mtime)) {
        MUNLOCK(mnp);
        return;
    }

    /* 
     * Get name hash.
     */

    hash = mfs_namehash(nm, &len);
    if (len == 0) goto done;        /* Null name never cached */

    /* Modify name hash into DNC hash */

    hash = MFS_DNCHASH(dvp, hash, len, ncdp);

    /* Copy fields from potentially unwired structures */

    dvfid = VTOM(dvp)->mn_hdr.fid;
    vw = MFS_VIEW(dvp);
    dvfsp = dvp->v_vfsp;

    /* Lock structures */

    MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    NC_HASH_LOCK(hash, &hash_spl, sh, ncdp);

    /* 
     * Look for the existing entry.  If not found, then this
     * operation is a no-op.
     *
     * Note that this routine requires the case-correct name,
     * and so we only do a case-sensitive lookup with dncfind.
     */

    if ((dnp = mfs_dncfind(&dvfid, dvfsp, vw, nm, len, FALSE, hash,
			   cred)) != NULL) {
        dnp->flags |= dnc_flags;

        /* Add in nullbh bit if adding bhinvariant flag */

        if (MVFS_FLAGON(dnc_flags, MFS_DNC_BHINVARIANT)) {
            dnp->nullbh = 1;
        }
    }
    NC_HASH_UNLOCK(hash_spl, sh, ncdp);
    MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
done:
    MUNLOCK(mnp);
    return;
}

/*
 * Look up a name in the name cache.
 */
VNODE_T *
mfs_dnclookup(
    register VNODE_T *dvp,
    register char *nm,
    struct pathname *pnp,
    CALL_DATA_T *cd
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register struct mfs_dncent *dnp;
    VNODE_T *vp;
    struct timeval vevtime;
    VNODE_T *vvw = NULL, *vw;
    mfs_fid_t vfid, dvfid;
    VFS_T *dvfsp;
    u_long dncgen;
    tbs_boolean_t notindir;
    int error, error2;
    int len;
    int hash;
    SPL_T sh, srw;
    SPLOCK_T *hash_spl;

    ASSERT(MFS_ISVOB(VTOM(dvp)));

    /* Check if dnlc for this mount point */

    if (ncdp->mfs_dnc == NULL) return(NULL);		/* No NC at all! */
    if (!mcdp->mvfs_dncenabled) return(NULL);
    if (V_TO_MMI(dvp)->mmi_nodnlc) return(NULL);

    /* Quick check for "." which always 'hits' */

    vw = MFS_VIEW(dvp);

    if (nm[0] == '.' && nm[1] == '\0') {
        DNC_BUMPVW_2(vw, dnc_hits, dnc_hitdot);
	VN_HOLD(dvp);
	return(dvp);
    }

    /*
     * Hash the name and find the name cache entry on
     * its hash chain.  For this initial search,
     * only use <parent dir>, <name> to find entry.
     * Later on we do all the build handle, event time etc.
     * checks.
     */
     
    hash = mfs_namehash(nm, &len);
    hash = MFS_DNCHASH(dvp, hash, len, ncdp);

    /* Copy fields from potentially unwired structures */

    dvfid = VTOM(dvp)->mn_hdr.fid;
    dvfsp = dvp->v_vfsp;

    NC_HASH_LOCK(hash, &hash_spl, sh, ncdp);
    if ((dnp = mfs_dncfind(&dvfid, dvfsp, vw, nm, len, 
            MVFS_PN_CI_LOOKUP(pnp), hash, MVFS_CD2CRED(cd))) == NULL) {
	/* Ordinary miss */
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
        DNC_BUMPVW(vw, dnc_misses);
	return (NULL);
    }

    error = mvfs_dnclookup_subr(dnp, vw, &vvw, cd);
    /* 
     * Must make a copy of dir cache info before releasing the lock
     * as it can go away anytime after then.
     */
    dncgen = dnp->dncgen;
    vfid = dnp->vfid;
    if (MFS_FIDNULL(vfid)) {
	vevtime.tv_sec = dnp->addtime;	/* for timeout of ENOENTs */
	vevtime.tv_usec = 0;
    } else {
        vevtime = dnp->vevtime;	/* for invalidation of obj on events */
    }
    notindir = MVFS_FLAGON(dnp->flags, MFS_DNC_NOTINDIR);

    /* 
     * Potentially valid name cache hit.
     * If this was a case-insensitive hit, then we
     * must make sure we set the case-correct name
     * in the pathname struct (if it exists) for our
     * caller.  Note: case-insensitive lookup implies
     * a valid pnp struct.
     */

    if (MVFS_PN_CI_LOOKUP(pnp)) {
        error2 = PN_SET_CASE_CORRECT_COMP(pnp, dnp->nm_p);
	if (error == 0)
	    error = error2;
    }
    NC_HASH_UNLOCK(hash_spl, sh, ncdp);
    if (error != 0)
	return NULL;

    /*
     * Check for "mnode generation" mismatch.  If the mnode has a 
     * different generation number than the name cache, it means
     * that the mnode has left the machine (been flushed) and reloaded.
     * Use the mtime/evtime in the name cache entry to try and validate
     * this vnode.  Note that "expected" modifieds (see mfs_attrcache) 
     * leave entries in the name cache, but won't update the mtime of
     * those, even though the dir mtime moves.  Thus a dir may be needlessly
     * flushed due to mtime mismatch if the mnode leaves and is reloaded
     * and all changes were due to "expected" modifications.
     *  10/23/91 - doesn't seem to be worth saving the info for.
     */

    if (VTOM(dvp)->mn_hdr.dncgen != dncgen) {
	MLOCK(VTOM(dvp));
	/* 
         * Dir mnode has been lost and reloaded.  We might have missed
         * getattr's that detected mtime or evtime changes and flushed
         * the cache while the mnode for this dir was not cached.
         */

	mfs_dnc_invalvp(dvp);      /* Invalidate entries from dir */
	MUNLOCK(VTOM(dvp));
	if (vvw) VN_RELE(vvw);
        DNC_BUMPVW_2(vw, dnc_misses, dnc_missdncgen);
	return(NULL);
    }

    /*
     * Check for ENOENT returns...
     * These are easy as there is no need to fetch any vnode
     * Also note that "vvw" must be NULL in this case so there
     * is no need to VN_RELE it.
     *
     * These must be "timed out" for third party changes that
     * make names "appear" as if by magic. (e.g. third party change
     * to something used in a config spec)  These normally cause
     * event time changes for objects (see below), but because the
     * ENOENT entries don't point at any object, we have to time them
     * out independently.  Note that "vevtime" was set to the "add time"
     * of the entry above.
     *
     * We don't have to timeout name not found entries for which
     * the name doesn't exist in the parent dir.  The only way
     * for these names to "magically appear" is for the dir to
     * be changed (mod-time or event time) in which case the invalidate
     * on the dir will flush the name-not-found entries that need
     * to be invalidated.
     */

    if (MFS_FIDNULL(vfid)) {
	if (!mcdp->mvfs_dncnoentenabled) {
	    DNC_BUMPVW(vw, dnc_misses);
	    return(NULL);
	}

	/* 
         * Use mount point's "regmax" attribute cache timeout to
         * timeout ENOENT entries.
	 */
	vevtime.tv_sec += V_TO_MMI(dvp)->mmi_ac_regmax;	/* timeout */
	if (notindir || MDKI_CTIME() <= vevtime.tv_sec) {
            DNC_BUMPVW_2(vw, dnc_hits, dnc_hitnoent);
            return(MFS_DNC_ENOENTVP);
	} else {
	    MLOCK(VTOM(dvp));
	    mfs_dncremove(dvp, nm, MVFS_CD2CRED(cd));	/* Flush translation */
	    MUNLOCK(VTOM(dvp));
            DNC_BUMPVW_2(vw, dnc_misses, dnc_missnoenttimedout);
	    return(NULL);
        }
    }

    /*
     * Get the vnode ptr from the FID information
     */

    error = mfs_getvnode(dvp->v_vfsp, vvw, &vfid, &vp, cd);
    if (vvw) VN_RELE(vvw);		/* Not needed any more */
    if (error) {			/* can't get it */
	MLOCK(VTOM(dvp));
	mfs_dncremove(dvp, nm, MVFS_CD2CRED(cd));	/* Flush translation */
	MUNLOCK(VTOM(dvp));
        DNC_BUMPVW_2(vw, dnc_misses, dnc_missnovp);
	return (NULL);
    }
    ASSERT(vp);		/* Must have a vnode (held) now */

    /*
     * We must check the event time on the object looked up
     * Vob events on an element (like moving a label etc.) can invalidate
     * a name translation for it.  It is much easier for the rest
     * of the system to maintain this time for the element, than to
     * try and update a evtime or mtime for the element's parent dir.
     * So... as a final check we validate the event time in the
     * name cache against the objects attributes (possibly cached)
     * to see if we should return this translation.   Whenever
     * new attributes are fetched for an element (and this check may
     * cause that), any change in the event time for the element causes
     * all name cache translations to be flushed for that vnode in
     * the lower level routine.  Thus we only have to free up the extra
     * hold count on the vnode here, the name cache entry is purged for us.
     *
     * Note:
     *    The view implementation of event_time is the latest of the
     *    following:
     *	      1) Time a specific event actually affects that element
     *           (like checkin/checkout)
     *        2) Time a VOB event to a type, label, etc. might affect
     *           an element
     *        3) Last event in the VOB for any element if the view
     *           lost track of VOB events (for all items in view cache)
     *        4) Time of activation of the object into the view's
     *           cache structures (since it doesn't know about events
     *	         on objects it doesn't have cached).
     *
     */

    if (!mfs_evtime_valid(vp, &vevtime, cd)) {
	VN_RELE(vp);	/* Can't trust this vnode, release it */
 	MLOCK(VTOM(dvp));
	mfs_dncremove(dvp, nm, MVFS_CD2CRED(cd));	/* Flush translation */
	MUNLOCK(VTOM(dvp));
        DNC_BUMPVW_2(vw, dnc_misses, dnc_missevtime);
	return(NULL);
    }

    /* 
     * Return the vnode ptr with the extra hold on it from
     * getvnode still there.
     */

    /* Counts hits by type */
    if (MVFS_ISVTYPE(vp, VDIR)) {
        DNC_BUMPVW_2(vw, dnc_hits, dnc_hitdir);
    } else {
        DNC_BUMPVW_2(vw, dnc_hits, dnc_hitreg);
    }

    return(vp);
}

STATIC int
mvfs_dnclookup_subr(
    struct mfs_dncent *dnp,
    VNODE_T *vw,
    VNODE_T **vpp,
    CALL_DATA_T *cd
)
{
    SPL_T sl;
    mvfs_thread_t *mth = MVFS_MYTHREAD(cd);

    /*
     * See if entry is marked as 'invalid'
     * We do this here so we can keep stats on name cache hits
     * that should have worked, except that we invalidated the entry
     * because of explicit ioctl, or because we can't be sure
     * the entry is still right.  This stat tells us how much
     * we could gain by better invalidations.  (It also tells
     * you whether a larger cache would help... if most of the misses
     * are due to invalidations, making the cache larger won't help).
     */
    if (dnp->invalid) {
        DNC_BUMPVW_2(vw, dnc_misses, dnc_missinvalid);
        return (1);
    }

    /*
     * Try to apply null bh to non-null process bh optimization
     */
    if (mfs_dnc_nullbhcheck(dnp, mth)) {
	DNC_BUMPVW(vw, dnc_hitbhfromnull);
    } else {

        /* 
         * Make sure the buildhandle checks out. 
         * 'mfs_dncbhcheck' returns true if the entry has been tagged as 
         *  valid for the current process' build handle.
         */

        if (!mfs_dncbhcheck(dnp, mth)) {
	    DNC_BUMPVW_2(vw, dnc_misses, dnc_missbh);
	    return(1);
        }
    }

    /* Move to end of freelist (which acts like an LRU list) */
    /* seems like it might make sense to do this shuffle a
     * little later in the lookup, after passing further 
     * validations, but leave it here for now for consistency 
     */
     
    NC_SPLOCK_LRU(dnp,sl);
    if (!dnp->in_trans) {
        NC_RMLRU_LOCKED(dnp);
        ASSERT(dnp->lruhead);
        NC_INSLRU_LOCKED(dnp->lruhead->lruprev, dnp);
        NC_SPUNLOCK_LRU(dnp,sl);
        if (vpp) {
	    *vpp = dnp->vvw;		/* View of result */
	    if (*vpp) VN_HOLD(*vpp);	/* Must hold or can lose it */
        }
        return 0;
    } else {
        /* someone else grabbed it from LRU, we are out of luck */
        NC_SPUNLOCK_LRU(dnp,sl);
        return 1;
    }
}

/*
 * Look up a view's selected version of a VOB root in the cache.
 *
 * This cache used to be separate, but is now part of the DNC cache
 * for compactness of view vnodes and to increase the potential size of the
 * RVC cache (let it share space with other directory DNC entries).
 *
 * The old RVC used to track fsid generations (which were incremented on each
 * unmount) but we don't have to since the DNC is flushed on every unmount.
 */
int
mvfs_rvclookup(
    VNODE_T *vw,
    VNODE_T *vobrtvp,
    mfs_fid_t *fidp,
    CALL_DATA_T *cd
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    int error;
    int hash;
    struct mfs_dncent *dnp;
    struct timeval vevtime;
    mfs_fid_t vobrtfid;
    VFS_T *dvfsp;
    SPLOCK_T *hash_spl;
    SPL_T sh;

    MVFS_INIT_TIMEVAL(vevtime);  /* The compiler wants these initialized. */

    if (ncdp->mfs_dnc == NULL) return(ENOENT);		/* No NC at all! */
    if (!mcdp->mvfs_dncenabled) return(ENOENT);
    if (V_TO_MMI(vobrtvp)->mmi_nodnlc) return(ENOENT);

    ASSERT(MFS_ISVIEW(VTOM(vw)));
    ASSERT(!MFS_VIEW(vobrtvp));
    ASSERT(vobrtvp == V_TO_MMI(vobrtvp)->mmi_rootvp);

    if (!mcdp->mvfs_rvcenabled) {
	BUMPSTAT(mfs_rvcstat.rvc_misses);
	return(ENOENT);
    }
    
    if (VTOM(vw)->mn_view.id == MFS_NULLVID) {
	return(ESTALE);
    }

    /*
     * hash on view uuid & vob uuid.  cast to int _after_ taking modulus on
     * unsigned value, to avoid signed-ness problems.
     */
    hash = (int) MVFS_RVCHASH(vw, vobrtvp->v_vfsp, ncdp);

    /* Copy possibly unwired fid */

    vobrtfid = VTOM(vobrtvp)->mn_hdr.fid;
    dvfsp = vobrtvp->v_vfsp;

    MDB_XLOG((MDB_RVC_DNC, "seeking vr %"KS_FMT_PTR_T" vw %"KS_FMT_PTR_T" on hash %x dfid %lx.%x\n",
			   V_TO_MMI(vobrtvp)->mmi_rootvp, vw, hash,
			   vobrtfid.mf_dbid, vobrtfid.mf_gen));

    NC_HASH_LOCK(hash, &hash_spl, sh, ncdp);
    if ((dnp = mfs_dncfind(&vobrtfid, dvfsp, vw, ".", 1, FALSE,
			   hash, MVFS_CD2CRED(cd))) == NULL) {
	/* Ordinary miss */
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
	BUMPSTAT(mfs_rvcstat.rvc_misses);
	return (ENOENT);
    }
    error = mvfs_dnclookup_subr(dnp, vw, NULL, cd);
    *fidp = dnp->vfid;
    if (MVFS_FLAGOFF(dnp->flags, MVFS_DNC_RVC_ENT))
	error = ENOENT;
    else
	vevtime = dnp->vevtime;
    NC_HASH_UNLOCK(hash_spl, sh, ncdp);

    if (error != 0 || MFS_FIDNULL(*fidp)) {
	BUMPSTAT(mfs_rvcstat.rvc_misses);
	return ENOENT;
    }
    if (vevtime.tv_sec + V_TO_MMI(vobrtvp)->mmi_ac_dirmax  < MDKI_CTIME()) {
	BUMPSTAT(mfs_rvcstat.rvc_misses);
	BUMPSTAT(mfs_rvcstat.rvc_misstimo);
	/* timed out */
	return ENOENT;
    }
    BUMPSTAT(mfs_rvcstat.rvc_hits);
    return 0;
}

int
mvfs_rvcenter(
    VNODE_T *vw,
    VNODE_T *vobrtvp,
    mfs_fid_t *fidp,
    CRED_T *cred
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    int hash;
    struct timeval vevtime;
    register mfs_mnode_t *mnp;

    ASSERT(MFS_VIEW(vobrtvp));

    /*
     * hash on view uuid & vob uuid.  cast to int _after_ taking modulus on
     * unsigned value, to avoid signed-ness problems.
     */
    hash = (int) MVFS_RVCHASH(vw, vobrtvp->v_vfsp, ncdp);

    MDB_XLOG((MDB_RVC_DNC,
	     "entering vr %"KS_FMT_PTR_T" vw %"KS_FMT_PTR_T" on hash %x fid %lx.%x dfid %lx.%x\n",
	      V_TO_MMI(vobrtvp)->mmi_rootvp, vw, hash,
	      fidp->mf_dbid, fidp->mf_gen,
	      VTOM(V_TO_MMI(vobrtvp)->mmi_rootvp)->mn_hdr.fid.mf_dbid,
	      VTOM(V_TO_MMI(vobrtvp)->mmi_rootvp)->mn_hdr.fid.mf_gen));
    vevtime.tv_sec = MDKI_CTIME();
    vevtime.tv_usec = 0;

    mvfs_dncadd_subr(V_TO_MMI(vobrtvp)->mmi_rootvp, vw, MVFS_DNC_RVC_ENT,
		     ".", 1, fidp, VDIR, &vevtime, hash, cred);
    return 0;
}

void
mvfs_rvcflush(
    register VNODE_T *vw,
    VFS_T *vfsp
)
{
    /* cribbed from mfs_dnc_invalvw() */

    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register int i, j;
    mfs_dncent_t *dnp;
    SPL_T srw;

    ASSERT(vw != NULL);
    ASSERT(MFS_ISVIEW(VTOM(vw)));

    if (ncdp->mfs_dnc == NULL) return;	/* No name cache */

    MVFS_RW_WRITE_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);

    for (i=0, j=0; i < mcdp->mvfs_dncdirmax; i++) { /* only search directory portion */
	dnp = &(ncdp->mfs_dnc[i]);
	if (dnp->in_trans) continue;	/* Skip in-transition cases */
	if (UNUSED(dnp)) {		/* Skip unused entries */
	    ASSERT(dnp->cred == NULL);
	    continue;
	}

        /* Invalidate entry if result in the specified view, for the
           specified VOB (if any) and it's an RVC */

        if (dnp->vvw == vw && MVFS_FLAGON(dnp->flags, MVFS_DNC_RVC_ENT) &&
            (vfsp == NULL || vfsp == dnp->vfsp))
        {
	    j++;			/* count them up */
            dnp->invalid = 1;
            dnp->nullbh = 0;
	}
    }
    MVFS_RW_WRITE_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    BUMPSTAT_VAL(mfs_rvcstat.rvc_purge, j);
    BUMPSTAT_VAL(mfs_dncstat.dnc_invalhits, j);
    if (vw) {
       BUMP_PVSTAT_VAL(vw, dncstat.dnc_invalhits, j);
    }
}

/*
 * Remove a name from the name cache.
 * Actually removes both "name" and "name@@" from either form.
 * and invalidates any case-insensitive synonyms for either name
 *
 * Return 1 if the non-HM name was found and removed (or it is
 * otherwise safe to believe that any aliases for the entry would have
 * been removed, e.g. the whole cache was flushed), and 0 if the name
 * is not found in the cache and aliases for its vnode may still be
 * cached.
 */

int
mfs_dncremove(
    register VNODE_T *dvp,
    register char *nm,
    CRED_T *cred
)
{
    char *xnm;
    int rval;
    
    if (mfs_hmname(nm, &xnm)) {
	rval = mfs_dncremove_one(dvp, xnm, cred); /* remove natural name */
	(void) mfs_dncremove_one(dvp, nm, cred); /* remove history mode name */
	STRFREE(xnm);
    } else {
	xnm = mfs_hmappend(nm);
        rval = mfs_dncremove_one(dvp, nm, cred); /* remove natural name */
	if (xnm != NULL) {
	    /* This may not be totally correct but I think it is the best that
	     * we can do given the limited options for return values and the
	     * fact that the existing code choses to ignore what happens to
	     * the history mode name.
	     */
	    (void) mfs_dncremove_one(dvp, xnm, cred); /* remove history mode name */
	    STRFREE(xnm);
	}
    }
    return rval;
}

/*
 * Remove one entry in the name cache.  Return 1 if the name was found and
 * removed (or it is otherwise safe to believe that any aliases for the entry
 * would have been removed, e.g. the whole cache was flushed), and 0 if the
 * name is not found in the cache and aliases for its vnode may still be
 * cached.
 */
int
mfs_dncremove_one(
    register VNODE_T *dvp,
    register char *nm,
    CRED_T *cred
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register struct mfs_dncent *dnp, *rvnp;
    mfs_fid_t dvfid, namefid, vobrtfid;
    VNODE_T *vw;
    VFS_T *dvfsp;
    int len;
    int hash, rvchash, rval = 0;
    tbs_boolean_t inval_rvc = 0;
    SPLOCK_T *hash_spl;
    SPL_T sh, sl;

    ASSERT(MFS_ISVOB(VTOM(dvp)));
    ASSERT(MISLOCKED(VTOM(dvp)));

    /* Check if no dnlc for this mount */

    if (ncdp->mfs_dnc == NULL) return 1;	/* No NC at all! */
    if (!mcdp->mvfs_dncenabled) return 1;
    dvfsp = dvp->v_vfsp;
    if (VFS_TO_MMI(dvfsp)->mmi_nodnlc) return 1;

    /* Check for max name len */

    hash = mfs_namehash(nm, &len);
    if (len == 0) return 0;

    hash = MFS_DNCHASH(dvp, hash, len, ncdp);

    /* Copy fields from potentially unwired structures */

    dvfid = VTOM(dvp)->mn_hdr.fid;
    vw = MFS_VIEW(dvp);

    rvchash = MVFS_RVCHASH(vw, dvfsp, ncdp);
    vobrtfid = VTOM(VFS_TO_MMI(dvfsp)->mmi_rootvp)->mn_hdr.fid;

    /* 
     * Remove one entry based on dir, nm.  This removal
     * is always case-sensitive.
     * 05/10/95 - jabs - ASSERT there is no second
     * entry on the hash chain with the same case-sensitive
     * dir, nm lookup.  This catches some bugs that can occur
     * in the case-insensitive name caching logic that would lead
     * to very subtle and hard to debug bugs.
     * Use lock to make sure name, RVC analog, and any synonyms are 
     * all removed before a lookup can find them
     */
    NC_HASH_LOCK(hash, &hash_spl, sh, ncdp);
    if ((dnp = mfs_dncfind(&dvfid, dvfsp, vw, nm, len, FALSE, hash,
			   NULL)) != NULL) {
	NC_RMHASH_LOCKED(dnp);
        NC_SPLOCK_LRU(dnp,sl);
        if (MFS_FIDNULL(dnp->vfid) && MVFS_FLAGOFF(dnp->flags, MFS_DNC_NOTINDIR)) {
            ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru));
            ncdp->mvfs_dnc_noent_other--;
        }
        if (dnp->in_trans) {
            /* We have write lock, so this shouldn't happen. */
            NC_SPUNLOCK_LRU(dnp,sl);
            NC_HASH_UNLOCK(hash_spl, sh, ncdp); 
            return 0; 
        }
	NC_RMLRU_LOCKED(dnp);
	SET_IN_TRANS(dnp);
        NC_SPUNLOCK_LRU(dnp,sl);
        /* 
         * Before we unlock the lock, we ASSERT there can't be
         * another identical item like this, or else the name
         * cache is very confused.  This won't find the one
         * we just unlinked from the chain, so if it finds anything,
         * something is seriously wrong.
         */
        ASSERT(mfs_dncfind(&dvfid, dvfsp, vw, nm, len, FALSE, hash,
			   NULL) == NULL);    

        /* Now, invalidate any case-insensitive synonyms for this name */
        mfs_dnc_inval_case_synonyms(&dvfid, vw, nm, len, hash);
       
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
        namefid = dnp->vfid;

        /* now remove an RVC entry, if the name resolved to a directory and
           it was an alias for the target of the RVC */
        NC_HASH_LOCK(rvchash, &hash_spl, sh, ncdp);
        if (dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncdirlru) &&
            (rvnp = mfs_dncfind(&vobrtfid, dvfsp, vw, ".", 1, FALSE,
                               rvchash, cred)) != NULL)
        {
            /* invalidate VOB root entry, if the same fid */
            if (MFS_FIDEQ(rvnp->vfid, namefid)) {
                inval_rvc = TRUE;
                rvnp->invalid = 1;
                rvnp->nullbh = 0;
            }
        }
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);

        /* and finally, put the entry we removed back onto LRU for reuse */
        mfs_dncrele(dnp);
        NC_SPLOCK_LRU(dnp,sl);
	CLR_IN_TRANS(dnp,ncdp) {
            /*
             * The whole DNC was replaced (flushed), so return
             * "safe" indication that no more flushing is needed.
             */
            NC_SPUNLOCK_LRU(dnp,sl);
            DROP_REF_IN_TRANS(ncdp);
	    return 1;
	}
	ASSERT(dnp->lruhead);
	NC_INSLRU_LOCKED(dnp->lruhead, dnp);
        NC_SPUNLOCK_LRU(dnp,sl);

        /* We found the name to be flushed, and were able to flush RVC
           if needed.  Indicate that caller need not flush RVC for
           this view and VOB. */
        if (inval_rvc) DNC_BUMPVW(vw, dnc_invalhits);
        rval = 1;
    } else {
        /* Not found, but still invalidate any synonyms for CI lookup */
        mfs_dnc_inval_case_synonyms(&dvfid, vw, nm, len, hash);
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
    }

    return rval;
}

/*
 * Flush the entire cache, returning a value indicating whether the lock
 * was dropped during the trip.
 * RETURN:	0:	nothing dropped, nothing intransit.
 *		-1:	lock was dropped, caller should circulate again
 *		>0:	count of intransit entries
 */
STATIC int
mvfs_dncflush_subr(
    VFS_T *vfsp
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register struct mfs_dncent *dnp;
    register int i;
    SPL_T sh, sl, srw;
    SPLOCK_T *hash_spl;
    int dropped = 0;
#ifdef MVFS_DEBUG
    int firstdrop = 1;
#endif
    if (ncdp->mfs_dnc == NULL) return 0;	/* No name cache */

    MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    for (i=0; i < ncdp->mfs_dncmax; i++) {
	dnp = &(ncdp->mfs_dnc[i]);
	if (dnp->in_trans) {
	    if (dropped != -1) {
		dropped++;
		MDB_XLOG((MDB_DNC_REALLOC,"flush intrans: %d\n", i));
	    }
	    continue;	/* Skip in-transition entries */
	}
	if (UNUSED(dnp)) {
	    /* Skip unused entries */
	    ASSERT(dnp->cred == NULL);
	    continue;
	}

        /* If vfsp supplied then skip entries not for this VOB */

        if ((vfsp != NULL) && (dnp->vfsp != vfsp)) continue;
  
        NC_HASH_LOCK(dnp->dnc_hash, &hash_spl, sh, ncdp);
        if ((vfsp != NULL) && (dnp->vfsp != vfsp)) {
            /* something changed while we waited for hash lock */
            NC_HASH_UNLOCK(hash_spl, sh, ncdp);
            continue;
        }
        NC_SPLOCK_LRU(dnp,sl);
        if (dnp->in_trans ) {
            /* race on LRU */
            NC_SPUNLOCK_LRU(dnp,sl);
            NC_HASH_UNLOCK(hash_spl, sh, ncdp);
            continue;
        }
	if (dnp->next) {
            NC_RMHASH_LOCKED(dnp);
            if (MFS_FIDNULL(dnp->vfid) && MVFS_FLAGOFF(dnp->flags, MFS_DNC_NOTINDIR)) {
                ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru));
                ncdp->mvfs_dnc_noent_other--;
            }
        }
	NC_RMLRU_LOCKED(dnp);
	SET_IN_TRANS(dnp);
        NC_SPUNLOCK_LRU(dnp,sl);
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
#ifdef MVFS_DEBUG
	if (firstdrop) {
	    MDB_XLOG((MDB_DNC_REALLOC,"dropped lock in dncflush_subr\n"));
	    firstdrop = 0;
	}
#endif
	dropped = -1;
        
        MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
        mfs_dncrele(dnp);
        MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
        NC_SPLOCK_LRU(dnp,sl);
	CLR_IN_TRANS(dnp,ncdp) {
            NC_SPUNLOCK_LRU(dnp,sl);
            MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw); 
	    DROP_REF_IN_TRANS(ncdp);
	    return dropped;
	}
	ASSERT(dnp->lruhead);
	NC_INSLRU_LOCKED(dnp->lruhead, dnp);
        NC_SPUNLOCK_LRU(dnp,sl);
    }
    if (vfsp == NULL) { 
        BUMPSTAT(mfs_dncstat.dnc_flush); /* Bump whole cache dumped count */
    }

    MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    return dropped;
}

/*
 * Flush the entire cache.  
 *
 * This routine guarantees 1 trip through the cache releasing all
 * translations that existed before it was called.  It is used to
 * free up resources that the name cache might be holding (refcounts etc.)
 * Other processes can come in in parallel and add new
 * entries while this routine is in progress.
 */
void
mfs_dncflush()
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
  
    if (ncdp->mfs_dnc == NULL) return;	/* No name cache */
    mvfs_dncflush_subr(NULL);
}

/*
 * Flush all entries for a specific view
 *
 * This guarantees one trip through the cache releasing all the name
 * cache entries related to a specific view.  It is used to free up
 * resources held by the name cache such as refcounts on the view-tag
 * before trying to reclaim a history mode view-tag.  
 * The invalidate calls should be used to invalidate translations
 */

void
mfs_dnc_flushvw(
    VNODE_T *vw
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register struct mfs_dncent *dnp;
    register int i;
    SPLOCK_T *hash_spl;
    SPL_T sh, sl, srw;

    if (ncdp->mfs_dnc == NULL) return;	/* No name cache */

    MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    for (i=0; i < ncdp->mfs_dncmax; i++) {
	dnp = &(ncdp->mfs_dnc[i]);
	if (dnp->in_trans) continue;	/* Skip in-transition entries */
	if (UNUSED(dnp)) {		/* Skip unused entries */
	    ASSERT(dnp->cred == NULL);
	    continue;
	}

        /* Skip entries not for this view */

        if (dnp->dvw != vw && dnp->vvw != vw) continue;

        /* Flush the entry */

        NC_HASH_LOCK(dnp->dnc_hash, &hash_spl, sh, ncdp);
        if (dnp->dvw != vw && dnp->vvw != vw) {
            /* something changed while we waited for hash lock */
            NC_HASH_UNLOCK(hash_spl, sh, ncdp);
            continue;
        }
        NC_SPLOCK_LRU(dnp,sl);
        if (dnp->in_trans) {
            /* race on LRU */
            NC_SPUNLOCK_LRU(dnp,sl);
            NC_HASH_UNLOCK(hash_spl, sh, ncdp);
            continue;
        }
	if (dnp->next) {
            NC_RMHASH_LOCKED(dnp);
            if (MFS_FIDNULL(dnp->vfid) && MVFS_FLAGOFF(dnp->flags, MFS_DNC_NOTINDIR)) {
                ASSERT(dnp->lruhead == (mfs_dncent_t *)&(ncdp->mfs_dncnoentlru));
                ncdp->mvfs_dnc_noent_other--;
            }
        }
	NC_RMLRU_LOCKED(dnp);
	SET_IN_TRANS(dnp);
        NC_SPUNLOCK_LRU(dnp,sl);
        NC_HASH_UNLOCK(hash_spl, sh, ncdp);
        MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
        mfs_dncrele(dnp);
        MVFS_RW_READ_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
        NC_SPLOCK_LRU(dnp,sl);
	CLR_IN_TRANS(dnp,ncdp) {
            NC_SPUNLOCK_LRU(dnp,sl);
            MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw); 
	    DROP_REF_IN_TRANS(ncdp);
	    return;
	}
	ASSERT(dnp->lruhead);
	NC_INSLRU_LOCKED(dnp->lruhead, dnp);
        NC_SPUNLOCK_LRU(dnp,sl);
    }
    MVFS_RW_READ_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw); 
    DNC_BUMPVW(vw, dnc_flushvw);
}

/*
 * Flush all entries for a specific VOB
 *
 * This guarantees one trip through the cache releasing all the name
 * cache entries related to a specific vob.  It is used to free up
 * resources held by the name cache when unmounting a vob.  Previously
 * it would flush all entries when unmounting any vob.
 */

void
mvfs_dnc_flushvfs(
    VFS_T *vfsp
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register struct mfs_dncent *dnp;
    register int i;

    if (ncdp->mfs_dnc == NULL) return;	/* No name cache */
    mvfs_dncflush_subr(vfsp);
    BUMPSTAT(mfs_dncstat.dnc_flushvfs);
}

/*
 *  Invalidate all entries referencing a specific vnode.
 *  This function marks an entry as invalid (meaning it can't be trusted)
 *  Invalid entries are left in the cache so we can count these
 *  cases, and also update them easily when they are revalidated.
 *  (i.e. dncadd but nothing has changed... no need to reallocate
 *  strings etc.)
 */
void
mfs_dnc_invalvp(
    register VNODE_T *vp
)
{

    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register int i, j;
    mfs_dncent_t *dnp;
    SPL_T srw;
    VNODE_T *vw;
    mfs_fid_t vfid;

    ASSERT(vp != NULL);

    if (ncdp->mfs_dnc == NULL) return;	/* No name cache */
    if (!MFS_ISVOB(VTOM(vp))) return;   /* Only VOB stuff in name cache */
    
    vfid = VTOM(vp)->mn_hdr.fid;
    vw = MFS_VIEW(vp);
    
    MVFS_RW_WRITE_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);

    for (i=0, j=0; i < ncdp->mfs_dncmax; i++) {
	dnp = &(ncdp->mfs_dnc[i]);
	if (dnp->in_trans) continue;	/* Skip in-transition cases */
	if (UNUSED(dnp)) {		/* Skip unused entries */
	    ASSERT(dnp->cred == NULL);
	    continue;
	}

  	/* Flush entry on any of the following conditions:
         *    Vfsp matches and either dir (view, fid) matches or
         *    result object (view, fid) matches.
         */

	if (dnp->vfsp == vp->v_vfsp &&
	         ((dnp->dvw == vw && MFS_FIDEQ(dnp->dfid, vfid)) ||
                  (dnp->vvw == vw && MFS_FIDEQ(dnp->vfid, vfid))))
        {
            dnp->invalid = 1;
            dnp->nullbh = 0;
            j++;  /* count hits, for stats */
	}
    }
    MVFS_RW_WRITE_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    DNC_BUMPVW(vw, dnc_invalvp); 
    BUMPSTAT_VAL(mfs_dncstat.dnc_invalhits, j);
    if (vw) {
        BUMP_PVSTAT_VAL(vw, dncstat.dnc_invalhits, j);
    }
}

/*
 * Invalidate name-not-found entries due to an 'object-not-found'
 * condition.  These entries can be affected by VOB events to
 * objects other than the dir, and so must be able to be invalidated
 * when any object in the VOB changes.
 */

void
mfs_dnc_inval_obj_not_found()
{

    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register int i, j;
    mfs_dncent_t *dnp;
    SPL_T ss, srw;
    mfs_fid_t vfid;

    if (ncdp->mfs_dnc == NULL) return;	/* No name cache */
    
    /* Entries with MFS_DNC_NOTINDIR *not* set should be fairly unusual.
     * We are keeping a count of them as we add and remove enoent
     * cache entries.  If there are none, we can skip this linear search.
     */
    SPLOCK(ncdp->mvfs_dnc_noentlru_lock, ss);
    i = ncdp->mvfs_dnc_noent_other;
    SPUNLOCK(ncdp->mvfs_dnc_noentlru_lock, ss); 
    ASSERT(i >= 0); 
    mvfs_log(MFS_LOG_DEBUG, "mfs_dnc_inval_obj_not_found, count = %d\n", i); 
    if (i == 0)  return; 

    MVFS_RW_WRITE_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    /* This is to shut up gcc which complains that dnp may be used
     * without being initialized, so I will initialize dnp before
     * entering the loop.  The real bug is that dnp is being used
     * improperly at the end of the loop.  See defect CMBU00063403
     * for details.
     */
    dnp = &(ncdp->mfs_dnc[ncdp->mfs_dnc_enoent_start]);

    for (i=ncdp->mfs_dnc_enoent_start, j=0; i < ncdp->mfs_dncmax; i++) {
	dnp = &(ncdp->mfs_dnc[i]);
	if (dnp->in_trans) continue;	/* Skip in-transition cases */
	if (UNUSED(dnp)) {		/* Skip unused entries */
	    ASSERT(dnp->cred == NULL);
	    continue;
	}

        /* 
         * Invalidate all entries for which we are not sure
         * that the cause of the name-not-found status was
         * due to the name not being in the dir.
         */
        if (MVFS_FLAGOFF(dnp->flags, MFS_DNC_NOTINDIR)) {
            dnp->invalid = 1;
            dnp->nullbh = 0;
            j++;  /* count hits, for stats */
        }
    }
    MVFS_RW_WRITE_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    BUMPSTAT(mfs_dncstat.dnc_invalnf);
    BUMPSTAT_VAL(mfs_dncstat.dnc_invalhits, j);
    if ((dnp) && dnp->vvw) {
        BUMP_PVSTAT_VAL((dnp->vvw), dncstat.dnc_invalhits, j);
    }
}

/*
 * Invalidate all name cache entries for the specified view
 */

void
mfs_dnc_invalvw(
    register VNODE_T *vw
)
{

    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register int i, j;
    mfs_dncent_t *dnp;
    SPL_T srw;

    ASSERT(vw != NULL);
    ASSERT(MFS_ISVIEW(VTOM(vw)));

    if (ncdp->mfs_dnc == NULL) return;	/* No name cache */

    MVFS_RW_WRITE_LOCK(&(ncdp->mvfs_dnc_rwlock), srw);

    for (i=0, j=0; i < ncdp->mfs_dncmax; i++) {
	dnp = &(ncdp->mfs_dnc[i]);
	if (dnp->in_trans) continue;	/* Skip in-transition cases */
	if (UNUSED(dnp)) {		/* Skip unused entries */
	    ASSERT(dnp->cred == NULL);
	    continue;
	}

        /* Invalidate entry if dir or result in the specified view */

        if (dnp->dvw == vw || dnp->vvw == vw) {
            dnp->invalid = 1;
            dnp->nullbh = 0;
            j++;  /* count hits, for stats */
	}
    }
    MVFS_RW_WRITE_UNLOCK(&(ncdp->mvfs_dnc_rwlock), srw);
    DNC_BUMPVW(vw, dnc_invalvw);
    BUMPSTAT_VAL(mfs_dncstat.dnc_invalhits, j);
    if (vw) {
       BUMP_PVSTAT_VAL(vw, dncstat.dnc_invalhits, j);
    }
}

/*
 * Internal routine to invalidate case-insensitive 
 * synonyms for a name cache translation.
 * Must be called with hash chain locked. 
 */

STATIC void
mfs_dnc_inval_case_synonyms(
    register mfs_fid_t *dvfidp,
    VNODE_T *vw,
    char *nm,
    int len,
    int hash
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register mfs_dncent_t *hp; 
    register mfs_dncent_t *dnp;
    register int j;
    SPLOCK_T *hash_spl; 

    ASSERT(len > 0);

    hp = (mfs_dncent_t *)&(ncdp->mfs_dnchash[hash]);
    for (dnp = hp->next,j=0; dnp != (struct mfs_dncent *) hp; dnp = dnp->next) {
	if (dnp->dvw == vw && dnp->len == len &&
            MFS_FIDEQ(dnp->dfid, *dvfidp) &&
            MVFS_FLAGON(dnp->flags, MFS_DNC_CASE_INSENSITIVE) &&
            STRCASECMP(dnp->nm_p, nm) == 0)
        {
            dnp->invalid = 1;
            j++;  /* count hits, for stats */
        }
    }
    BUMPSTAT_VAL(mfs_dncstat.dnc_invalhits, j);
    if (vw) {
        BUMP_PVSTAT(vw, dncstat.dnc_invalhits);
    }
    return;
}

/*
 * Internal routine to find a cache entry.
 * Note that in the name cache there can be both
 * a case-sensitive and case-insensitive entry for the same
 * name with different translation results.  There is no
 * sharing between these two sets of entries, so every find
 * will only find one kind or the other.
 *	MUST CALL WITH HASH CHAIN SPINLOCK HELD
 */
STATIC struct mfs_dncent *
mfs_dncfind(
    register mfs_fid_t *dvfidp,
    VFS_T *dvfsp,
    VNODE_T *vw,
    char *nm,
    int len,
    tbs_boolean_t case_insensitive,
    int hash,
    CRED_T *cred	/* Check credentials? */
)
{
    mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    register mfs_dncent_t *hp; 
    register mfs_dncent_t *dnp;
    int ci_flag;

    ASSERT(len > 0);

    ci_flag = case_insensitive ? MFS_DNC_CASE_INSENSITIVE : 0; 

    ASSERT(hash < ncdp->mvfs_dnchashsize);
    hp = (mfs_dncent_t *)&(ncdp->mfs_dnchash[hash]);
    MDB_XLOG((MDB_RVC_DNC,"hashchain %"KS_FMT_PTR_T" hval %x\n", hp, hash));
    for (dnp = hp->next; dnp != (struct mfs_dncent *) hp; dnp = dnp->next) {
	/*
	 * RVC entries have the target view set but not the directory view
	 * set since the bare vob root is entered in the DNC.
	 */
	if (dnp->len == len && MFS_FIDEQ(dnp->dfid, *dvfidp)) {
	    if (MVFS_FLAGON(dnp->flags, MVFS_DNC_RVC_ENT) &&
		dnp->vfsp == dvfsp && dnp->vvw == vw) {
		MDB_XLOG((MDB_RVC_DNC,"dnp %"KS_FMT_PTR_T" RVCmatch\n", dnp));
	    } else if (dnp->dvw != vw ||
		       MVFS_FLAGON(dnp->flags, MVFS_DNC_RVC_ENT)) {
		MDB_XLOG((MDB_RVC_DNC,
			 "dnp %"KS_FMT_PTR_T" wrong dvw %"KS_FMT_PTR_T"\n", dnp, dnp->dvw));
		continue;
            } else if (dnp->vfsp != dvfsp) {
                /*
                ** Given the way fid's are generated, an element could have the same
                ** name and fid in two different vobs.  In fact, a customer found this
                ** when splitting a vob into multiple vobs, and duplicating the directory
                ** names in all the vobs.  Thus, make sure this is *really* the entry
                ** we want by checking the vob as well as the fid and the name length.
                */
                MDB_XLOG((MDB_RVC_DNC, "dnp %"KS_FMT_PTR_T" wrong vob (vfsp = %"KS_FMT_PTR_T")\n", dnp, dnp->vfsp));
                continue;
	    } else {
		MDB_XLOG((MDB_RVC_DNC,"dnp %"KS_FMT_PTR_T" viewmatch\n", dnp));
	    }

            /* 
             * Correct view and dir, and name length.
             * If possible case-insensitive match (based on flag
             * in the DNC entry) then compare names in a case-insensitive
             * manner to see if a hit.
             */
            if (case_insensitive) {
                if (MVFS_FLAGON(dnp->flags, MFS_DNC_CASE_INSENSITIVE)) {
                    if (STRCASECMP(dnp->nm_p, nm) == 0) {
                        return(dnp);
                    }
                } else {
                    /*
                     * The following is an optimization for
                     * case-insensitive searches.  If the name
                     * is a case-sensitive match, and it results in
                     * a valid vnode (instead of ENOENT), then this
                     * must also be correct for case-insensitive
                     * lookups.  This is because case-correct names
                     * for files that exist are always the right
                     * choice in case-insensitive name lookup.
                     */
                    if (*dnp->nm_p == *nm &&    /* check first/last char quick */
                                dnp->nm_p[len-1] == nm[len-1] &&
                                !MFS_FIDNULL(dnp->vfid) &&
                                BCMP(dnp->nm_p, nm, len) == 0) {
                        return(dnp);
                    }
                }
            } else {
                /* 
                 * Don't care about case-insensitive flag in dnc ent
                 * for case-sensitive lookup... case-insensitive translations
                 * are always piggybacked on the correct case-sensitive
                 * translation, so they are good for case-sensitive
                 * lookups too.
                 */
                if (*dnp->nm_p == *nm &&	/* check first char/last char quickly */
	             dnp->nm_p[len-1] == nm[len-1] &&
                     BCMP(dnp->nm_p, nm, len) == 0) {
		    return(dnp);
                }
            }
	} else {
	    MDB_XLOG((MDB_RVC_DNC,
		     "dnp %"KS_FMT_PTR_T" len/fid mismatch: %x/%x %lx:%x/%lx:%x\n",
		      dnp,
		      dnp->len, len,
		      dnp->dfid.mf_dbid, dnp->dfid.mf_gen,
		      dvfidp->mf_dbid,
		      dvfidp->mf_gen));
	}
    }

    return(NULL);
}

/*
 * Build handle list routines 
 */

STATIC void
mfs_dncbhadd(
    mfs_dncent_t *dnp,
    view_bhandle_t *bhp
)
{
    if (MFS_BHNULL(*bhp)) {
	dnp->nullbh = 1;
	return;
    }

    /* Replace "next" entry in build handle list */

    dnp->bh[dnp->bhx] = *bhp;
    dnp->bhx++;
    if (dnp->bhx >= MFS_DNCBHMAX) dnp->bhx = 0;
}

STATIC void
mfs_dncbhset(
    mfs_dncent_t *dnp,
    view_bhandle_t *bhp
)
{
    int i;
 
    dnp->bhx = 0;

    dnp->nullbh = 0;
    for (i=0; i < MFS_DNCBHMAX; i++) {
	dnp->bh[i].build_session = (u_long) 0xffffffff;
	dnp->bh[i].target_id = (u_long) 0xffffffff;
    }
    
    if (MFS_BHNULL(*bhp)) {
	dnp->nullbh = 1;
	return;
    }

    dnp->bhx = 1;
    dnp->bh[0] = *bhp;
    return;
}

/* 
 * Check for using null bh entry under a bh
 */

STATIC int
mfs_dnc_nullbhcheck(
    mfs_dncent_t *dnp,
    mvfs_thread_t *mth
)
{
    /* If process says reftime is the only effect of the bh
     * on the config thread, then we can perform an important
     * optimization here.  If the nullbh is valid, and the event time
     * of the target object is less than the build handle reference 
     * time, then we can use the value looked up under the null
     * bh because nothing has happened in the VOB for that object
     * which would make the ref_time rule take effect and change
     * the results of the lookup.  I can use the evtime in the
     * name cache entry here with impunity because the name cache
     * will miss anyways on event time if it later turns out that this
     * event time is not the latest and greatest.
     */

    if (dnp->nullbh && mth->thr_usereftime) {
        if (dnp->vevtime.tv_sec < mth->thr_bh_ref_time.tv_sec ||
	 	(dnp->vevtime.tv_sec == mth->thr_bh_ref_time.tv_sec &&
	  	dnp->vevtime.tv_usec <= mth->thr_bh_ref_time.tv_usec)) {
	    MDB_XLOG((MDB_DNCBHUSE, 
	        "null bh hit: pid=%lx nm=%s bh=0x%x.%x ref=%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X" ev=%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X"\n",
		 mth->thr_proc->mp_procid,
		 mth->thr_bh.build_session, mth->thr_bh.target_id,
		mth->thr_bh_ref_time.tv_sec, mth->thr_bh_ref_time.tv_usec,
		dnp->vevtime.tv_sec, dnp->vevtime.tv_usec));
	    return(1);
        } else {
	    MDB_XLOG((MDB_DNCBHUSE,
	        "null bh miss: pid=%lx nm=%s bh=0x%x.%x ref=%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X" ev=%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X"\n",
		mth->thr_proc->mp_procid,
		dnp->nm_p, mth->thr_bh.build_session, mth->thr_bh.target_id,
		mth->thr_bh_ref_time.tv_sec, mth->thr_bh_ref_time.tv_usec,
		dnp->vevtime.tv_sec, dnp->vevtime.tv_usec));
	    return(0);
	}
    } else {
	MDB_XLOG((MDB_DNCBHUSE,
		"null bh not considered: pid=%lx nm=%s nullbh=%d usereftime=%d\n",
		 mth->thr_proc->mp_procid,
		 dnp->nm_p, dnp->nullbh, mth->thr_usereftime));
    }
    return(0);
}

STATIC int
mfs_dncbhcheck(
    mfs_dncent_t *dnp,
    mvfs_thread_t *mth
)
{
    int i;

    /* Null BH only valid if explicitly enabled */

    if (MFS_BHNULL(mth->thr_bh)) {
	return(dnp->nullbh ? 1 : 0 );	/* Return state of null bh valid */
    }

    /* Search explicit bh tags for a match */

    for (i=0; i < MFS_DNCBHMAX; i++) {
	if (MFS_BH_SAMECONFIG(dnp->bh[i], mth->thr_bh)) {
	    return(1);
	}
    }
    return(0);
}

void
mvfs_dnc_count(
    mvfs_cache_usage_t *usage
)
{
    register mvfs_dnlc_data_t *ncdp = MDKI_DNLC_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    u_int dirs = 0, regs = 0, noents = 0;
    register int i;

    for (i = 0; i < ncdp->mfs_dncmax; i++) {
	if ((ncdp->mfs_dnc[i]).in_trans || (ncdp->mfs_dnc[i]).invalid ||
	    UNUSED(&(ncdp->mfs_dnc[i])))
	    continue;
	if (MFS_FIDNULL((ncdp->mfs_dnc[i]).vfid))
	    noents++;
	else if (i < mcdp->mvfs_dncdirmax)
	    dirs++;
	else
	    regs++;
    }

    usage->cache_usage[MVFS_CACHE_INUSE][MVFS_CACHE_DNCDIR] = dirs;
    usage->cache_usage[MVFS_CACHE_INUSE][MVFS_CACHE_DNCREG] = regs;
    usage->cache_usage[MVFS_CACHE_INUSE][MVFS_CACHE_DNCNOENT] = noents;

    usage->cache_usage[MVFS_CACHE_MAX][MVFS_CACHE_DNCDIR] = mcdp->mvfs_dncdirmax;
    usage->cache_usage[MVFS_CACHE_MAX][MVFS_CACHE_DNCREG] = mcdp->mvfs_dncregmax;
    usage->cache_usage[MVFS_CACHE_MAX][MVFS_CACHE_DNCNOENT] = mcdp->mvfs_dncnoentmax;
    return;
}
static const char vnode_verid_mvfs_dncops_c[] = "$Id:  65abe643.dc5411df.9210.00:01:83:0a:3b:75 $";
