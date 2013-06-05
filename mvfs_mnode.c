/* * (C) Copyright IBM Corporation 1991, 2011. */
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
/* mvfs_mnode.c */

#include "mvfs_systm.h"
#include "mvfs.h"

/*
 * Prototypes for internal routines
 */
STATIC int 
mvfs_mninit_vobhash(mvfs_cache_sizes_t *mma_sizes);

STATIC int
mvfs_mninit_cvphash(mvfs_cache_sizes_t *mma_sizes);

STATIC int
mvfs_mninit_otherhash(mvfs_cache_sizes_t *mma_sizes);

STATIC int
mvfs_mninit_vobfreehash(mvfs_cache_sizes_t *mma_sizes);

STATIC int
mvfs_mnfind_vobhashsize(int maxentry);

STATIC int
mvfs_mnfind_vobfreehashsize(int vobhashsz);

STATIC mfs_mnode_t *
mvfs_mnfindexisting(
    mfs_class_t class, 
    VNODE_T *vw, 
    mfs_fid_t *fidp, 
    VFS_T *vfsp
);

STATIC mfs_mnode_t *
mvfs_mnfindexisting_subr1(
    mfs_class_t class, 
    VNODE_T *vw, 
    mfs_fid_t *fidp, 
    VFS_T *vfsp
);

STATIC mfs_mnode_t *
mvfs_mnfindexisting_subr2(
    mfs_class_t class, 
    VNODE_T *vw, 
    mfs_fid_t *fidp, 
    VFS_T *vfsp
);

STATIC mfs_mnode_t *
mvfs_mnfindexisting_subr3(
    mfs_class_t class, 
    VNODE_T *vw, 
    mfs_fid_t *fidp, 
    VFS_T *vfsp
);

STATIC mfs_mnode_t *
mvfs_mnnew(
    mfs_class_t class, 
    int mngen, 
    int mnum
);

STATIC int
mvfs_mninit_new_mnode(
    mfs_mnode_t *mnp, 
    mfs_class_t class, 
    VNODE_T *vw,
    mfs_fid_t *fidp, 
    VFS_T *nvfsp, 
    int mnum 
);

STATIC void 
mvfs_mndestroy_list(void);

STATIC void
mvfs_mndestroy(mfs_mnode_t *mnp);

STATIC void
mvfs_mnclean(mfs_mnode_t *mnp);

STATIC void
mvfs_mnfree(mfs_mnode_t *mnp);

STATIC void 
mvfs_mnfreelist_mgmt(void);

STATIC void
mvfs_mnhash(mfs_mnode_t *mnp);

STATIC void
mvfs_mnunhash(mfs_mnode_t *mnp);

STATIC void
mvfs_mn_clamp_vobfree(
    mvfs_cache_sizes_t *mma_sizes,
    tbs_boolean_t silent,
    u_long mnmax,
    u_long *vobfreemax_p,
    u_long *vobfreemin_p
);

STATIC void
mvfs_mn_clamp_cvpfree(
    mvfs_cache_sizes_t *mma_sizes,
    tbs_boolean_t silent,
    u_long vobfreemax,
    u_long *cvpfreemax_p,
    u_long *cvpfreemin_p
);

STATIC int
mvfs_mngrowtable(int mnmax);

STATIC int
mvfs_mngrowtable_to(
    int currentsz,
    int newsize,
    int flag
);

STATIC int
mfs_mngetmnumslot(void);

STATIC int
mvfs_mngetmnodesize(mfs_class_t class);

STATIC mfs_mnode_t *
mvfs_mnallocatemnode(int msize);

STATIC int
mvfs_mnverify_freelist(void);

STATIC int
mvfs_mnverify_destroy(void);

STATIC int
mvfs_compute_cvpfreemax(ks_int32_t scale_factor);

STATIC ks_int32_t
mvfs_compute_mnmax(ks_int32_t scale_factor);

STATIC void
mvfs_sync_mnode(mfs_mnode_t *mnp);

STATIC tbs_boolean_t
mvfs_ensure_flushable_mnode(
    VFS_T *vfsp, 
    mfs_mnode_t *mnp, 
    int *fail, 
    VNODE_T **vpp
);

/* 
 * Static variable that includes all data used by mnode sub-system.
 */
mvfs_mnode_data_t mvfs_mnode_data_var;

/* 
 * The following definitions deal with reserving/allocating entries in the mnum_to_mnode
 * table.  This table has a pointer to every mnode in the system.   The "in transition" 
 * flag is used to mark the mnode as "in transition" while the mfs_mnlock lock 
 * is not held.
 */

#define MFS_MN_INTRANS (mfs_mnode_t *) 1

/*
 * Macros to reserve and free slots in the mnode allocation table
 * and keep track of mfs_mncnt, high water mark, and possible free slot.
 * (Must hold mfs_mnlock.)
 */

#define MFS_MN_RSV_SLOT(dp, mnum) { \
	DEBUG_ASSERT(ISLOCKEDBYME(&dp->mfs_mnlock)); \
	dp->mnum_to_mnode[(mnum)] = MFS_MN_INTRANS; \
	if (mnum > dp->mvfs_mtmhwm) dp->mvfs_mtmhwm = mnum; \
	if (mnum >= dp->mvfs_mtmpfs) \
	    dp->mvfs_mtmpfs = mnum + 1; \
	dp->mfs_mncnt++; \
}

#define MFS_MN_FREE_SLOT(dp, mnum) { \
	DEBUG_ASSERT(ISLOCKEDBYME(&dp->mfs_mnlock)); \
	dp->mnum_to_mnode[(mnum)] = NULL; \
	if (mnum == dp->mvfs_mtmhwm) { \
	    while ((dp->mnum_to_mnode[--(dp->mvfs_mtmhwm)] == 0) && \
		   (dp->mvfs_mtmhwm > 0)) \
		{ } \
	} \
	if (mnum < dp->mvfs_mtmpfs) dp->mvfs_mtmpfs = mnum; \
	dp->mfs_mncnt--; \
	DEBUG_ASSERT (dp->mfs_mncnt >= 0); \
}

/*
 * MNODE LOCKING
 * The mnode subsystem uses several locks for managing the set of mnodes 
 * allocated in a system.  Several locks are used rather than one monolithic
 * lock to allow for greater concurrency on MP systems.  The locks and
 * what they protect are:
 * 
 * Lock			Protects
 * -----		--------
 * mfs_mnlock		mnum_to_mnode table, mfs_mncnt, mvfs_mtmhwm, mvfs_mtmpfs
 *			mfs_mngen, mfs_attrgen, mfs_growrestart, 
 *			mvfs_attrgenrollover
 * mvfs_vobhash_mlp	each lock protects 1-n vob hash chains and the next and
 *			prev links in the mnode header.
 * mvfs_cvphash_mlp	each lock protects 1-n cvp hash chains and the next and
 *			prev links in the mnode header.
 * mvfs_otherhash_mlp	each lock protects 1-n other hash chains and the next 
 *			and prev links in the mnode header.
 * mnode header lock	all offsets in the mnode header EXCEPT:
 *				next, prev, free_next, free_prev links,
 *				hdr_lock itself (obviously), mfree flag,
 *				trans_destroy flag, on_destroy flag,
 *				stale, flag containing bits for cached_pages,
 *				clear_dirty, and clear_mmap, fid?XXX, vp, 
 *				realvp, viewvp, freelist_time.
 * mvfs_vobfreehash_mlp	each lock protects 1-n vobfree hash chains,
 *			the free_next and free_prev links in the mnode header
 *			(when on mvfs_vobfree), mfree flag in the mnode header,
 *			trans_destroy flag in the mnode header.
 * mvfs_vobfreelock	mvfs_vobfreecnt, mvfs_cvpfreecnt, and the
 *			mvfs_vobfreemax, mvfs_cvpfreemax, mvfs_vobfreemin,
 *			mvfs_cvpfreemin counts (important while tuning).
 * mvfs_mndestroylock	mvfs_mndestroy, the free_next and free_prev links in the
 *			mnode header (when on mvfs_mndestroy), the 
 *			on_destroy flag, and mvfs_mndestroycnt.
 * mnode lock		has not been altered, it is still used to lock the
 *			content of the type-specific mnode and 1 field
 *			in the mnode header: stale flag
 *
 * Note: we assume that 1 word reads are atomic with respect to writes.
 *
 * Note: The free_next and free_prev offsets in the mnode header do double
 *	 duty - they are used when the mnode is on either the vobfree
 *	 list or when it is on the mvfs_mndestroy list.
 *
 * Note: The mfree flag in the mnode header is used by find/get routines to
 *	 check if an mnode is on the vob freelist.
 *	 The trans_destroy bit is set under the mvfs_vobfree hash lock as the 
 *	 first step to moving an mnode to the destroy list from the vob 
 *	 freelist.  If this flag is set, find/get routines will skip the mnode 
 *	 and not attempt to change its free_next and free_prev offsets which 
 *	 may be protected by either the mvfs_vobfreehash_mlp (if it is actually 
 *	 still on the vob freelist) or mvfs_mndestroy (if it has already
 *	 been moved over to the destroy list).
 * 
 *
 * General Locking Theory
 *
 * Try to use the smallest lock possible to get the job done.
 * 
 * The mnum_to_mnode table has a pointer to every mnode in the system.
 * The mfs_mnlock protects this table and several associated global counters.
 * This mfs_mnlock is taken whenever changes are made to the mnum_to_mnode
 * table and when using the counters.  Note this lock is not held when an
 * mnode is added/removed from the mnode hash table, the vobfree hash list or
 * the destroy list.
 *
 * Every mnode in the system is located on a hash chain in 1 of 3 hash tables.
 * Each of the hash tables are locked by an MVFS_LOCK pool.  An mvfs_lock
 * pool provides 1 mvfs_lock for every n hash chains where n is platform 
 * dependent.  To ensure the integrity of the given hash chain, the appropriate
 * hash chain lock is taken.  The hash chain lock is taken when adding/removing 
 * an mnode from its hash chain and when searching the hash chain.  When we are
 * searching for an mnode on a hash chain, we initially take the hash chain lock
 * to ensure the mnode doesn't appear/disappear during the search.  Once the 
 * mnode is located, we switch to using the mnode header lock to protect the 
 * integrity of the data in the mnode.  So the mnode header lock is taken and 
 * the hash chain lock is released.  This allows additional searches and 
 * modifications to the hash chain group to be done concurrently with accessing 
 * a particular mnode.
 * 
 * The mnode header lock is a new lock.  It is taken to ensure the integrity of
 * most of the contents of the mnode header.  Note that the chains in the
 * mnode header that link mnodes together on the hash, free and destroy lists
 * are locked by the lock for that list and not the mnode header lock.  The
 * exact lock used to protect each offset in the mnode header is specified
 * in the mnode header definition in mvfs.h.  
 *
 * When an mnode is no longer in use, it may be placed on the vob freelist or 
 * the destroylist.  When an mnode is on either of these lists, the mnode 
 * remains on the hash chain.  The vob freelist and the destroylist are linked 
 * through the free_next and free_prev links.  Both the vob freelist and the 
 * destroylist use the same link offsets in the mnode header since an mnode can
 * be on only 1 of these lists at a time.  The vob freelist and the destroylist 
 * are locked separately from each other to allow concurent processing of 
 * mnodes being destroyed with the search for an existing mnode on an open 
 * (this is a particular bottleneck in MP systems).  In addition, the vob
 * freelist is implemented as a hash table of linked lists.  Every mnode on 
 * the "vob freelist" is really on one of n hash chains that taken together 
 * conceptually make up the "vob freelist".  Each of the hash chains is locked 
 * by an mvfs_lock pool.  An mvfs_lock pool provides 1 mvfs_lock for every n 
 * hash chains where n is platform dependent.  To ensure the integrity of the 
 * given hash chain, the appropriate hash chain lock is taken.  The hash chain 
 * lock is taken when adding/removing an mnode from its hash chain and when 
 * searching the hash chain.
 *
 * The mvfs_vobfreelock is taken to ensure the integrity of the counters
 * associated with the conceptual "vob freelist".
 * 
 * And the mvfs_mndestroylock is taken to ensure the integrity of the
 * destroy list chain (mvfs_mndestroylist) and it's associated counter.
 * 
 *
 * Lock Ordering
 * We used to use 1 large-grained lock for the entire mnode cache.  This
 * has been broken up into a series of locks to allow more concurrency on
 * MP systems.  Given this the proper lock ordering rules must be followed
 * when using these locks.  
 *
 * The lock order is:
 * hash_chain->mnode_header->mfs_mnlock->mvfs_vobfreehash chain->mfs_mnstat
 *             mnode_header->            mvfs_vobfreehash chain->mvfs_mndestroylock->mfs_mnstat
 *
 * Intermediate steps in the lock order can be skipped.
 * 
 * The new cred cache lock is called from this module with the mvfs_vobfreehash
 * chain lock held, in other modules it is called with the mnode itself locked.
 * So the lock ordering for the cred cache lock is:
 *  mvfs_vobfreehash chain->cred_cache_lock
 *  individual_mnode_lock->cred_cache_lock
 * The later is listed for completeness and is not an attempt to document the
 * complete individual_mnode_lock ordering.
 *
 *
 * Old Deadlock with loopback nodes:
 *    The FSS, which is no longer used, had a nasty tendency to "lock" a 
 *    dir and then ask for a vnode (inode) recycle to make the name being
 *    looked up in the dir.  Releasing vnodes held in a "cover"
 *    vnode could deadlock in this case.  To avoid this I never use the
 *    inode cache in FSS systems.  In addition, one must be careful
 *    when freeing "realvp's" that are dirs as a side-effect of an
 *    operation (like too many realvp's active), as these are likely
 *    to deadlock!  So, I never "cache" the realvp if it is a cover
 *    vnode for a dir.. even in the mnode cache.
 *    XXX Should we consider "cache"ing the realvp when it is a cover node?
 *
 * Unmountable FS's, automount problems, busy on mount:
 *    Another problem with "cover" vnodes in the cache is that vnodes are held
 *    in a way undetectable to the rest of the system.  In
 *    particular, unmount of an NFS/FS doesn't know to ask us
 *    to free up our refcnts on vnodes (even though we really
 *    aren't using it).  Mount has problems if the dir was
 *    accidentally accessed via a cover vnode before attempting
 *    the mount .. it thinks the dir is busy!
 *    Never caching a dir vnode solves automount/busy on mount problems
 *    since they only involve dirs.  The "unmountable FS" problem
 *    is left unsolved, so users must be aware that they may have
 *    to unmount VOBS to get NFS/FS mounts unmounted.
 */

/*
 * Hash lists for mnodes.  
 * 
 * All mnodes are found on 1 of 3 hash tables.  The chart below shows
 * which hash table is used for each class of mnode.  Only VOB class
 * mnodes are placed on the "vob freelist".  This is managed as a hashed LRU.
 * The destroy list is used for VOB class mnodes when they are removed from 
 * the "vob freelist" LRU.  The use of a separate destroy list and lock 
 * allows the search for existing mnodes (which may have to reclaim from the 
 * "vob freelist") to run concurrently with the destroy of mnodes that are on 
 * the destroylist (who may have been recently removed from the "vob freelist"
 * LRU).  All non-VOB classes of mnodes are placed directly onto the 
 * destroylist when they are no longer in use so they can be deallocated 
 * (via kmem_free).
 *
 * Class		Hash Table	Freelist	 Destroylist
 * ------------		--------------	------------	 --------
 * SDEV			mvfs_otherhash	none 		 mvfs_mndestroy
 * VIEW			mvfs_otherhash	none		 mvfs_mndestroy
 * VIEWDIR		mvfs_otherhash	none		 mvfs_mndestroy
 * NTVW			mvfs_otherhash	none		 mvfs_mndestroy
 * LOOP			mvfs_cvphash	none		 mvfs_mndestroy
 * VOBRT		mvfs_vobhash	none		 mvfs_mndestroy
 * VOB			mvfs_vobhash	mvfs_vobfreehash mvfs_mndestroy
 *
 */

#define MFS_VOBHASHMIN 512     /* min mnode vobhash size */
#define MFS_VOBHASHMAX 32768   /* max mnode vobhash size, corresponding 
				* largeinit is about 100 */
#define MFS_CVPHASHMIN 128     /* min mnode cvphash size */
#define MFS_CVPHASHMAX 8192    /* max mnode cvphash size */
#define MFS_OTHERHASHMIN 64    /* min mnode otherhash size */
#define MFS_OTHERHASHMAX 4096  /* max mnode otherhash size */
#define MFS_MNVOB_AVECHAIN 4   /* average chain length for vob hash */
#define MFS_VOBCVP_RATIO 4     /* estimated ratio of:
				* (# vobhash entries) / (# cvphash entries)
				* keep ratio same as in static array */
#define MFS_VOBOTHER_RATIO 8   /* estimated ratio of:
				* (# vobhash entries) / (# "other" entries) */
#define MVFS_FLS_BFSZ_FACTOR 10/* used to calculate mnp buffer size when 
				* sync'ing. */
#define MVFS_DEFAULT_MNPLIST_SIZE (MFS_MNVOB_AVECHAIN * MVFS_FLS_BFSZ_FACTOR)

/* 
 * Hash table macros.  We use the standard MVFS lock pools.
 * Each lock in the mvfs_lock pool protects the hash chain links in the mnode 
 * header.
 */

#define MNVOBHASH_MVFS_LOCK(dp, hash_val, lockpp) { \
	MVFS_LOCK_SELECT(&(dp)->mvfs_vobhash_mlp, hash_val, HASH_MVFS_LOCK_MAP, lockpp); \
	MVFS_LOCK(*(lockpp)); }

#define MNVOBHASH_MVFS_UNLOCK(lockpp) { \
	MVFS_UNLOCK(*(lockpp)); }

#define MNCVPHASH_MVFS_LOCK(dp, hash_val, lockpp) { \
	MVFS_LOCK_SELECT(&(dp)->mvfs_cvphash_mlp, hash_val, HASH_MVFS_LOCK_MAP, lockpp); \
	MVFS_LOCK(*(lockpp)); }

#define MNCVPHASH_MVFS_UNLOCK(lockpp) { \
	MVFS_UNLOCK(*(lockpp)); }

#define MNOTHERHASH_MVFS_LOCK(dp, hash_val, lockpp) { \
	MVFS_LOCK_SELECT(&(dp)->mvfs_otherhash_mlp, hash_val, HASH_MVFS_LOCK_MAP, lockpp); \
	MVFS_LOCK(*(lockpp)); }

#define MNOTHERHASH_MVFS_UNLOCK(lockpp) { \
	MVFS_UNLOCK(*(lockpp)); }

/* 
 * The VOB hash must not include the generation number.  All generations
 * must hash to the same chain.  When we find a "stale" mnode entry, we will
 * purge it and create a "new one" in its place.
 */

#define MFS_VOBHASH(dp, fid) \
	((u_int)((fid).mf_dbid & ((dp)->mvfs_vobhashsize - 1)))
#define MFS_CVPHASH(dp, fid) \
	((u_int)(((size_t)((fid).mf_realvp) >> 2) & ((dp)->mvfs_cvphashsize - 1)))
#define MFS_OTHERHASH(dp, fid) \
	((u_int)(((fid).mf_mnum) & ((dp)->mvfs_otherhashsize - 1)))

/* 
 * Hash macros insert after the "HP" element. 
 * The vars and assignments in INSHASH allow links from the list itself to
 * be passed in as the ptr to insert after. 
 */

#define MN_INSHASH(hp, mnp) { \
	register mfs_mnode_t *HP = hp; \
	DEBUG_ASSERT((mnp)->mn_hdr.next == NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.prev == NULL); \
	(mnp)->mn_hdr.next = (HP)->mn_hdr.next; \
	(mnp)->mn_hdr.prev = (HP); \
	(HP)->mn_hdr.next->mn_hdr.prev = (mnp); \
	(HP)->mn_hdr.next = (mnp); \
    } 

#define MN_RMHASH(mnp) { \
	DEBUG_ASSERT((mnp)->mn_hdr.next);	\
	DEBUG_ASSERT((mnp)->mn_hdr.prev);	\
	(mnp)->mn_hdr.next->mn_hdr.prev = (mnp)->mn_hdr.prev; \
	(mnp)->mn_hdr.prev->mn_hdr.next = (mnp)->mn_hdr.next; \
	(mnp)->mn_hdr.next = (mnp)->mn_hdr.prev = NULL; \
    }

/* 
 * VOB freelist.  VOB mnodes are cached on the freelist when they are
 * freed up if they still have the associated view.  If a VOB entry on
 * the freelist has a cleartext vnode pointer that cleartext is considered
 * free.  There is no specific cvp freelist, but the count mvfs_cvpfreecnt
 * counts the number of cvps "on" the mvfs_vobfree list.
 * 
 * When releasing an mnode to the freelist, if the free mnode cnt > max then
 * some VOB mnodes are destroyed from the freelist.  And, if the free
 * cleartext vnode cnt > max, then some mnodes on the freelist are
 * purged of their held cleartext pointers.  This avoids problems
 * with caching too many "held" vnodes which can causing resource problems
 * (e.g. run out of handles for SMB client).
 *
 * In general, the freelist length bounces between the mins and max's.
 * There should be enough room between the min and the max to avoid
 * excessive CPU utilization managing the freelist.
 *
 * The "max" and "min" values are sysgen parameters (see mvfs_tunables.c
 * and the file mfs_param.h).
 *
 * The vob freelist is really a hashed vob freelist.  Every mnode on the "vob 
 * freelist" is on one of n hash chains.  Each of the hash chains are locked 
 * by an mvfs_lock pool.  An mvfs_lock pool provides 1 mvfs_lock for every n 
 * hash chains where n is platform dependent. 
 *
 * The global mvfs_vobfreelock only protects the counts: mvfs_vobfreecnt 
 * and mvfs_cvpfreecnt, as well as the high and low water marks for both
 * the vob and cvp freemin and freemax.
 */

#define MVFS_VOBFREEHASHMIN 16	   /* min vobfree hash size */
#define MVFS_VOBFREEHASHMAX 1024   /* max vobfree hash size, corresponding 
				    * largeinit is about 100 */
#define MVFS_MNVOBFREE_AVECHAIN 200/* average chain length for vob hash */
#define MVFS_VOBFREE_RATIO 16	   /* estimated ratio of:
				    * (size of vob hash) / (size of vobfree hash) */

/* 
 * Hash table macros.  We use the standard MVFS lock pools.
 * Each lock in the mvfs_lock pool protects the hash chain links in the mnode 
 * header.
 */

#define MNVOBFREEHASH_MVFS_LOCK(dp, hash_val, lockpp) { \
	MVFS_LOCK_SELECT(&(dp)->mvfs_vobfreehash_mlp, hash_val, HASH_MVFS_LOCK_MAP, lockpp); \
	MVFS_LOCK(*(lockpp)); }

#define MNVOBFREEHASH_MVFS_UNLOCK(lockpp) { \
	MVFS_UNLOCK(*(lockpp)); }

/* vob free hash algorithm */

#define MVFS_VOBFREEHASH(dp, mnp) \
	((u_int)((mnp)->mn_hdr.mnum & ((dp)->mvfs_vobfreehashsize-1)))

/* 
 * Freelist hash macros insert at the end of the hashed free list pointed to
 * by hp.
 */
#define ASSERT_ZONEID

#define MN_INSFREE(dp, hp, mnp) { \
	register mfs_mnode_t *HP = hp; \
	register mfs_mnode_t *FP;	\
	DEBUG_ASSERT((HP)->mn_hdr.next == NULL); \
	DEBUG_ASSERT((HP)->mn_hdr.prev == NULL); \
	DEBUG_ASSERT((HP)->mn_hdr.free_next); \
	DEBUG_ASSERT((HP)->mn_hdr.free_prev); \
	DEBUG_ASSERT((mnp)->mn_hdr.next); \
	DEBUG_ASSERT((mnp)->mn_hdr.prev); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_next == NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_prev == NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.mcount == 0); \
	DEBUG_ASSERT((mnp)->mn_hdr.on_destroy == 0); \
	ASSERT_ZONEID \
	FP = (HP)->mn_hdr.free_prev; \
	(mnp)->mn_hdr.free_next = (FP)->mn_hdr.free_next; \
	(mnp)->mn_hdr.free_prev = (FP); \
	DEBUG_ASSERT((FP) == (HP)->mn_hdr.free_prev); \
	DEBUG_ASSERT((FP)->mn_hdr.free_next->mn_hdr.free_prev); \
	DEBUG_ASSERT((FP)->mn_hdr.free_next->mn_hdr.free_prev == (FP)); \
	(FP)->mn_hdr.free_next->mn_hdr.free_prev = (mnp); \
	(FP)->mn_hdr.free_next = (mnp); \
	(mnp)->mn_hdr.mfree = 1; \
	MVFS_LOCK(&(dp)->mvfs_vobfreelock); \
	(dp)->mvfs_vobfreecnt++; \
	if ((mnp)->mn_hdr.realvp) \
	    (dp)->mvfs_cvpfreecnt++; \
	MVFS_UNLOCK(&(dp)->mvfs_vobfreelock); \
    }

#define MN_RMFREE(dp, LP, mnp) { \
	DEBUG_ASSERT(ISLOCKEDBYME(LP)); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_next); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_prev); \
	ASSERT_ZONEID \
	(mnp)->mn_hdr.free_next->mn_hdr.free_prev = (mnp)->mn_hdr.free_prev; \
	(mnp)->mn_hdr.free_prev->mn_hdr.free_next = (mnp)->mn_hdr.free_next; \
	(mnp)->mn_hdr.free_next = (mnp)->mn_hdr.free_prev = NULL; \
	MVFS_LOCK(&((dp)->mvfs_vobfreelock)); \
	if ((mnp)->mn_hdr.realvp != NULL) { \
	    (dp)->mvfs_cvpfreecnt --; \
	    DEBUG_ASSERT((dp)->mvfs_cvpfreecnt != ((u_long) -1)); \
	} \
	(dp)->mvfs_vobfreecnt--; \
	DEBUG_ASSERT((dp)->mvfs_vobfreecnt != ((u_long) -1)); \
	MVFS_UNLOCK(&((dp)->mvfs_vobfreelock)); \
	(mnp)->mn_hdr.mfree = 0; \
    }

/* 
 * The destroy list is a list of mnodes that need to be destroyed.  They have 
 * have either been "LRU'ed out" of the vob freelist, or are mnodes for a class 
 * of mnode that is not placed on the vob freelist.  The destroy list is 
 * maintained as a separate list from the freelist under a separate lock 
 * to allow mvfs_mnfindexisting to pull free mnodes off the vob freelist 
 * concurrently with mnodes being destroyed.
 *
 * The actual destruction (mvfs_mnclean and mvfs_mnfree) of mnodes from the 
 * mvfs_mndestroylist cannot be done under the mfs_mndestroylock because
 * the callouts to higher layers to release vnodes could aquire locks out
 * order and deadlock. (see mvfs_mnfree..)
 */

/* 
 * Destroylist macros insert at the end of the list.
 */
#define MN_INSDESTROY(dp, mnp) { \
	register mfs_mnode_t *DLP;	\
	MVFS_LOCK(&((dp)->mvfs_mndestroylock)); \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.next == NULL); \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.prev == NULL); \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.free_next); \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.free_prev); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_next == NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_prev == NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.mcount == 0); \
	DEBUG_ASSERT((mnp)->mn_hdr.vfsp != NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.on_destroy == 0); \
	DLP = (dp)->mvfs_mndestroylist.mn_hdr.free_prev; \
	(mnp)->mn_hdr.free_next = (DLP)->mn_hdr.free_next; \
	(mnp)->mn_hdr.free_prev = (DLP); \
	DEBUG_ASSERT((DLP)->mn_hdr.free_next != NULL); \
	DEBUG_ASSERT((DLP)->mn_hdr.free_prev != NULL); \
	(DLP)->mn_hdr.free_next->mn_hdr.free_prev = (mnp); \
	(DLP)->mn_hdr.free_next = (mnp); \
	(mnp)->mn_hdr.on_destroy = 1; \
	(dp)->mvfs_mndestroycnt++; \
	MVFS_UNLOCK(&((dp)->mvfs_mndestroylock)); \
    }

#define MN_RMDESTROY(dp, mnp) { \
	DEBUG_ASSERT(ISLOCKEDBYME(&((dp)->mvfs_mndestroylock))); \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.next == NULL); \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.prev == NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_next); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_prev); \
	DEBUG_ASSERT((mnp)->mn_hdr.on_destroy == 1); \
	(mnp)->mn_hdr.free_next->mn_hdr.free_prev = (mnp)->mn_hdr.free_prev; \
	(mnp)->mn_hdr.free_prev->mn_hdr.free_next = (mnp)->mn_hdr.free_next; \
	(mnp)->mn_hdr.free_next = (mnp)->mn_hdr.free_prev = NULL; \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.free_next != NULL); \
	DEBUG_ASSERT((dp)->mvfs_mndestroylist.mn_hdr.free_prev != NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_next == NULL); \
	DEBUG_ASSERT((mnp)->mn_hdr.free_prev == NULL); \
	(dp)->mvfs_mndestroycnt--; \
	DEBUG_ASSERT((dp)->mvfs_mndestroycnt != ((u_long) -1)); \
    }

/*
 * Mnode-related tuning formulae
 */
#define DEFAULT_CVP_MAXMIN_SPREAD 32

#define VOBHASHSIZE_FORMULA(mnmax) ((mnmax)/MFS_MNVOB_AVECHAIN)
#define CVPHASHSIZE_FORMULA(vobhashsz) ((vobhashsz)/MFS_VOBCVP_RATIO)
#define OTHERHASHSIZE_FORMULA(vobhashsz) ((vobhashsz)/MFS_VOBOTHER_RATIO)
#define VOBFREEHASHSIZE_FORMULA(vobhashsz) ((vobhashsz)/MVFS_VOBFREE_RATIO)

#define VOBFREEMAX_FORMULA(mnmax) ((22*(mnmax))/100)  
#define VOBFREEMIN_FORMULA(max) (((max)*9)/10)
#define VOBFREEMIN_FORMULA_2(max, vobfreehs) ((max) - (vobfreehs))
#define CVPFREEMIN_FORMULA_SUB(_mcdp) (((_mcdp)->mvfs_largeinit + 1) * DEFAULT_CVP_MAXMIN_SPREAD)
#define CVPFREEMIN_FORMULA(max, _mcdp) ((max) - CVPFREEMIN_FORMULA_SUB(_mcdp))
#define CVPFREEMIN_FORMULA_2(max, vobfreehs) ((max) - (vobfreehs))

/*
 * Mnode management initialization routines.
 */

/*
 * MFS_MNINIT - Initalize the MFS mnode structures.
 */
int
mvfs_mninit(mvfs_cache_sizes_t *mma_sizes)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    mvfs_mnode_data_t *mndp;
    int err;

    MDKI_MNODE_ALLOC_DATA();
    mndp = MDKI_MNODE_GET_DATAP();

    mndp->mvfs_vobhashsize = MFS_VOBHASHMIN;
    mndp->mvfs_cvphashsize = MFS_CVPHASHMIN;
    mndp->mvfs_otherhashsize = MFS_OTHERHASHMIN;
    mndp->mvfs_vobhash = NULL;
    mndp->mvfs_cvphash = NULL;
    mndp->mvfs_otherhash = NULL;
    mndp->mvfs_vobfreehashsize = MVFS_VOBFREEHASHMIN;
    mndp->mvfs_vobfreehash = NULL;
    mndp->mvfs_mnfreelist_mgmt_ip = 0;

    INITLOCK(&(mndp->mfs_mnlock), "mfs_mnlk");
    INITLOCK(&(mndp->mvfs_vobfreelock), "mfs_vfl");
    INITLOCK(&(mndp->mvfs_mndestroylock), "mfs_dl");

    mndp->mfs_mncnt = 1;	/* We never take slot 0 */

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_MNMAX] = mcdp->mvfs_mnmax;
    MVFS_SIZE_DEFLOAD(mcdp->mvfs_mnmax, mma_sizes, MNMAX, 0);
    if (mcdp->mvfs_mnmax == 0) {
	mcdp->mvfs_mnmax = mvfs_compute_mnmax(mcdp->mvfs_largeinit);
    }

    /* Allocate mnum_to_mnode and initialize pointers that improve linear 
     * searches. */
    mndp->mnum_to_mnode = (mfs_mnode_t **)
	KMEM_ALLOC((sizeof(mfs_mnode_t *)*mcdp->mvfs_mnmax), KM_SLEEP);
    if (mndp->mnum_to_mnode == NULL) {
	mvfs_log(MFS_LOG_ERR, "mvfs_mninit: null mnum_to_mnode table");
	return(ENOMEM);
    }
    BZERO(mndp->mnum_to_mnode, sizeof(mfs_mnode_t *)*mcdp->mvfs_mnmax);
    mndp->mvfs_mtmhwm = 0;	/* Initialize the high water mark to 0 */
    mndp->mvfs_mtmpfs = 1;	/* Initialize first possible free slot to 1
				 * (we don't use slot 0) */

    /*
     * Initialize the recommended size to start with for the mnplist used in 
     * mvfs_sync_mnodes.  The average hash chain is targetted to have
     * only MFS_MNVOB_AVECHAIN (4) items, start with a buffer 10 times this. 
     */
    mndp->mvfs_rec_fls_bufsz = MVFS_DEFAULT_MNPLIST_SIZE;
    mndp->mvfs_default_mnplist = 
        (mfs_mnode_t **)KMEM_ALLOC(MVFS_DEFAULT_MNPLIST_SIZE * sizeof(mfs_mnode_t *), 
					 KM_SLEEP);
    if (mndp->mvfs_default_mnplist == NULL) {
	mvfs_log(MFS_LOG_ERR, "mvfs_mninit: null mvfs_default_mnplist");
        KMEM_FREE(mndp->mnum_to_mnode, (sizeof(mfs_mnode_t *) * mcdp->mvfs_mnmax));
        return(ENOMEM);
    }
    /* Init the hash tables that hold the various classes of mnodes */
    if ((err = mvfs_mninit_vobhash(mma_sizes)) != 0)
	return(err);
    if ((err = mvfs_mninit_cvphash(mma_sizes)) != 0)
	return(err);
    if ((err = mvfs_mninit_otherhash(mma_sizes)) != 0)
	return(err);
    if ((err = mvfs_mninit_vobfreehash(mma_sizes)) != 0)
	return(err);

    /* Initialize the mvfs_mndestroy list */
    mndp->mvfs_mndestroylist.mn_hdr.free_next = 
		mndp->mvfs_mndestroylist.mn_hdr.free_prev =
			(mfs_mnode_t *)&(mndp->mvfs_mndestroylist);
    mndp->mvfs_mndestroycnt = 0;

    mvfs_rddir_cache_init(mma_sizes);

    /* Initialize mnode generation to something reasonable */
    mndp->mfs_mngen = 0;
    mndp->mfs_attrgen = 0;

    /* Initialize mfs_mnget() restart counters */
    mndp->mfs_growrestart = 0;		/* Restart after growtable */
    mndp->mvfs_attgenrollover = 0;	/* Attribute rollover count */

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_AGE_CVP_TIME] = 
		(ks_uint32_t)mndp->mvfs_nt_age_cvp_time;
    MVFS_SIZE_DEFLOAD_NONZERO(mndp->mvfs_nt_age_cvp_time, mma_sizes, 
			      AGE_CVP_TIME, MVFS_DEFAULT_AGE_CVP_TIME);
    return(0);
}

/*
 * MVFS_MNDATA_FREE - Dispose of mnode structures
 */
void
mvfs_mndata_free(void)
{
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /* 
     * Free vobhash table memory, if it was dynamically allocated and
     * the associated lock pool
     */
    if (mndp->mvfs_vobhash != NULL) {
	KMEM_FREE(mndp->mvfs_vobhash, 
		  mndp->mvfs_vobhashsize*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_vobhash = NULL;
    }
    mvfs_lock_pool_free(&(mndp->mvfs_vobhash_mlp));

    /* 
     * Free cvphash table memory, if it was dynamically allocated and
     * the associated lock pool
     */
    if (mndp->mvfs_cvphash != NULL) {
	KMEM_FREE(mndp->mvfs_cvphash, 
		  mndp->mvfs_cvphashsize*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_cvphash = NULL;
    }
    mvfs_lock_pool_free(&(mndp->mvfs_cvphash_mlp));

    /* 
     * Free otherhash table memory, if it was dynamically allocated and
     * the associated lock pool
     */
    if (mndp->mvfs_otherhash != NULL) {
	KMEM_FREE(mndp->mvfs_otherhash, 
		  mndp->mvfs_otherhashsize*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_otherhash = NULL;
    }
    mvfs_lock_pool_free(&(mndp->mvfs_otherhash_mlp));

    /*
     * Free vobfreehash table memory, if it was dynamically allocated and
     * the associated lock pool
     */
    if (mndp->mvfs_vobfreehash != NULL) {
        KMEM_FREE(mndp->mvfs_vobfreehash, 
		  mndp->mvfs_vobfreehashsize*sizeof(mvfs_vobfreehash_slot_t));
	mndp->mvfs_vobfreehash = NULL;
    }
    mvfs_lock_pool_free(&(mndp->mvfs_vobfreehash_mlp));

    /* Deallocate mvfs_default_mnplist if allocated */
    if (mndp->mvfs_default_mnplist != NULL) {
        KMEM_FREE(mndp->mvfs_default_mnplist, 
		  MVFS_DEFAULT_MNPLIST_SIZE * sizeof(mfs_mnode_t *));
        mndp->mvfs_default_mnplist = NULL;
    }

    /* Deallocated mnum_to_mnode table if it was allocated. */
    if (mndp->mnum_to_mnode != NULL) {	/* free mem if it's initialized */
	KMEM_FREE(mndp->mnum_to_mnode, sizeof(mfs_mnode_t *)*mcdp->mvfs_mnmax);
	mndp->mnum_to_mnode = NULL;

	FREELOCK(&(mndp->mvfs_mndestroylock));
	FREELOCK(&(mndp->mvfs_vobfreelock));
	FREELOCK(&(mndp->mfs_mnlock));
    }

    mndp->mfs_mncnt = 0;

    mvfs_rddir_cache_unload();

    mcdp->mvfs_mnmax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_MNMAX];
    mcdp->mvfs_cvpfreemax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_CVPFREEMAX];
    mcdp->mvfs_vobfreemax = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_VOBFREEMAX];
    mcdp->mvfs_cvpfreemin = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_CVPFREEMIN];
    mcdp->mvfs_vobfreemin = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_VOBFREEMIN];
    mndp->mvfs_nt_age_cvp_time = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_AGE_CVP_TIME];
    mndp->mvfs_vobhashsize = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_VOBHASHTAB_SZ];
    mndp->mvfs_cvphashsize = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_CVPHASHTAB_SZ];
    mndp->mvfs_otherhashsize = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_OTHERHASHTAB_SZ];
    mndp->mvfs_vobfreehashsize = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_VOBFREEHASHTAB_SZ];

    /* Free the memory for the mvfs_mnode_data_t if it was allocated. */
    MDKI_MNODE_FREE_DATA();

    return;
}

/*
 * MFS_MNINIT_VOBHASH - Internal routine to allocate and initialize the 
 *			vobhash table.
 */
STATIC int
mvfs_mninit_vobhash(mvfs_cache_sizes_t *mma_sizes)
{
    int i;
    int mlp_poolsize;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * Dynamically allocate the mnode vobhash table based on the maximum 
     * number of mnodes (mvfs_mnmax).  This keeps the average chain length 
     * fairly short for good performance.
     */

    /* First, free old vobhash table memory if it has been allocated. */
    if (mndp->mvfs_vobhash != NULL) {
	KMEM_FREE(mndp->mvfs_vobhash, 
		  mndp->mvfs_vobhashsize*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_vobhash = NULL;
    }

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_VOBHASHTAB_SZ] = mndp->mvfs_vobhashsize;
    MVFS_SIZE_DEFLOAD_NONZERO(mndp->mvfs_vobhashsize, mma_sizes, VOBHASHTAB_SZ,
			      mvfs_mnfind_vobhashsize(mcdp->mvfs_mnmax));

    if (mndp->mvfs_vobhashsize < MFS_VOBHASHMIN)
	mndp->mvfs_vobhashsize = MFS_VOBHASHMIN;
    
    mndp->mvfs_vobhash = (mfs_mnhash_slot_t *)
			 KMEM_ALLOC(mndp->mvfs_vobhashsize*sizeof(mfs_mnhash_slot_t),
			 KM_SLEEP);
    if (mndp->mvfs_vobhash == NULL) {
	mvfs_log(MFS_LOG_WARN, 
		 "Failed to allocate %d bytes for mnode vobhash, trying minimum (%d bytes) instead.\n",
		 mndp->mvfs_vobhashsize*sizeof(mfs_mnhash_slot_t), 
		 MFS_VOBHASHMIN*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_vobhashsize = MFS_VOBHASHMIN;
	mndp->mvfs_vobhash = (mfs_mnhash_slot_t *)
		       KMEM_ALLOC(mndp->mvfs_vobhashsize*sizeof(mfs_mnhash_slot_t), 
		       KM_SLEEP);
	if (mndp->mvfs_vobhash == NULL) {
	    mvfs_log(MFS_LOG_ERR, "Could not allocate minimum size for mnode vobhash.\n");
	    return(ENOMEM);
	}
    }

    for (i = 0; i < mndp->mvfs_vobhashsize; i++) {
	mndp->mvfs_vobhash[i].mn_hdr.next = mndp->mvfs_vobhash[i].mn_hdr.prev = 
			(mfs_mnode_t *)&(mndp->mvfs_vobhash[i]);
    }

    /* 
     * Now initialize the lock pool for this hash table. 
     * The actual size is dependant on the HASH_MVFS_LOCK_RATIO for the platform.
     */
    HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, mndp->mvfs_vobhashsize);
    if (mvfs_lock_pool_init(&(mndp->mvfs_vobhash_mlp), mlp_poolsize, NULL, 
			      "mvfs_vobhash_mlp") != 0) {
	mvfs_log(MFS_LOG_ERR, "Could not allocate vobhash_mlp.\n");
	return(ENOMEM);
    }

    return(0);
}

/*
 * MFS_MNINIT_CVPHASH - Internal routine to allocate and initialize the 
 * cvphash table.
 */
STATIC int
mvfs_mninit_cvphash(mvfs_cache_sizes_t *mma_sizes)
{
    int i;
    int mlp_poolsize;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * Dynamically allocate the mnode cvphash table based on the size
     * of the vobhash table.  This keeps the average chain length 
     * fairly short for good performance.  We don't know what portion
     * of all mnodes will be used for loopclass mnodes, so we guestimate.
     */

    /* First, free old cvphash table memory, if it exists. */
    if (mndp->mvfs_cvphash != NULL) {
	KMEM_FREE(mndp->mvfs_cvphash, 
		  mndp->mvfs_cvphashsize*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_cvphash = NULL;
    }

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_CVPHASHTAB_SZ] = mndp->mvfs_cvphashsize;
    /* if not set, calculate mvfs_cvphashsize based on mvfs_vobhashsize */
    MVFS_SIZE_DEFLOAD_NONZERO(mndp->mvfs_cvphashsize, mma_sizes, CVPHASHTAB_SZ,
                              CVPHASHSIZE_FORMULA(mndp->mvfs_vobhashsize));

    if (mndp->mvfs_cvphashsize < MFS_CVPHASHMIN)
	mndp->mvfs_cvphashsize = MFS_CVPHASHMIN;

    mndp->mvfs_cvphash = (mfs_mnhash_slot_t *)
		         KMEM_ALLOC(mndp->mvfs_cvphashsize*sizeof(mfs_mnhash_slot_t),
			 KM_SLEEP);
    if (mndp->mvfs_cvphash == NULL) {
	mvfs_log(MFS_LOG_WARN, 
		 "Failed to allocate %d bytes for mnode cvphash, trying minimum (%d bytes) instead.\n",
		 mndp->mvfs_cvphashsize*sizeof(mfs_mnhash_slot_t),
		 MFS_CVPHASHMIN*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_cvphashsize = MFS_CVPHASHMIN;
	mndp->mvfs_cvphash = (mfs_mnhash_slot_t *)
	 		 KMEM_ALLOC(mndp->mvfs_cvphashsize*sizeof(mfs_mnhash_slot_t),
			 KM_SLEEP);
	if (mndp->mvfs_cvphash == NULL) {
	    mvfs_log(MFS_LOG_ERR, "Could not allocate minimum size for mnode cvphash.\n");
	    return(ENOMEM);
	}
    }
    for (i = 0; i < mndp->mvfs_cvphashsize; i++) {
	mndp->mvfs_cvphash[i].mn_hdr.next = mndp->mvfs_cvphash[i].mn_hdr.prev =
			(mfs_mnode_t *)&(mndp->mvfs_cvphash[i]);
    }

    /* 
     * Now initialize the lock pool for this hash table. 
     * The actual size is dependant on the HASH_MVFS_LOCK_RATIO for the platform.
     */
    HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, mndp->mvfs_cvphashsize);
    if (mvfs_lock_pool_init(&(mndp->mvfs_cvphash_mlp), mlp_poolsize, NULL, 
			      "mvfs_cvphash_mlp") != 0) {
	mvfs_log(MFS_LOG_ERR, "Could not allocate cvphash_mlp.\n");
	return(ENOMEM);
    }

    return(0);
}

/*
 * MFS_MNINIT_OTHERHASH - Internal routine to allocate and initialize the 
 * otherhash table.
 */
STATIC int
mvfs_mninit_otherhash(mvfs_cache_sizes_t *mma_sizes)
{
    int i;
    int mlp_poolsize;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    /*
     * Dynamically allocate the mnode otherhash table based on the size
     * of the vobhash table.  This keeps the average chain length 
     * fairly short for good performance.  We don't know what portion
     * of all mnodes will be used for sdev, view, and viewdir class 
     * mnodes, so we guestimate.
     */

    /* First, free old otherhash table memory, if it exists. */
    if (mndp->mvfs_otherhash != NULL) {
	KMEM_FREE(mndp->mvfs_otherhash, 
		  mndp->mvfs_otherhashsize*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_otherhash = NULL;
    }

    /* calculate mvfs_otherhashsize based on mvfs_vobhashsize */
    mndp->mvfs_otherhashsize = OTHERHASHSIZE_FORMULA(mndp->mvfs_vobhashsize);

    mndp->mvfs_otherhash = (mfs_mnhash_slot_t *)
		     KMEM_ALLOC(mndp->mvfs_otherhashsize*sizeof(mfs_mnhash_slot_t),
		     KM_SLEEP);
    if (mndp->mvfs_otherhash == NULL) {
	mvfs_log(MFS_LOG_WARN, 
		 "Failed to allocate %d bytes for mnode otherhash, trying minimum (%d bytes) instead.\n",
		 mndp->mvfs_otherhashsize*sizeof(mfs_mnhash_slot_t),
		 MFS_OTHERHASHMIN*sizeof(mfs_mnhash_slot_t));
	mndp->mvfs_otherhashsize = MFS_OTHERHASHMIN;
	mndp->mvfs_otherhash = (mfs_mnhash_slot_t *)
		       KMEM_ALLOC(mndp->mvfs_otherhashsize*sizeof(mfs_mnhash_slot_t),
		       KM_SLEEP);
	if (mndp->mvfs_otherhash == NULL) {
	    mvfs_log(MFS_LOG_ERR, "Could not allocate minimum size for mnode otherhash.\n");
	    return(ENOMEM);
	}
    }

    for (i = 0; i < mndp->mvfs_otherhashsize; i++) {
	mndp->mvfs_otherhash[i].mn_hdr.next = mndp->mvfs_otherhash[i].mn_hdr.prev =
			(mfs_mnode_t *)&(mndp->mvfs_otherhash[i]);
    }

    /* 
     * Now initialize the lock pool for this hash table. 
     * The actual size is dependant on the HASH_MVFS_LOCK_RATIO for the platform.
     */
    HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, mndp->mvfs_otherhashsize);
    if (mvfs_lock_pool_init(&(mndp->mvfs_otherhash_mlp), mlp_poolsize, NULL, 
			      "mvfs_otherhash_mlp") != 0) {
	mvfs_log(MFS_LOG_ERR, "Could not allocate otherhash_mlp.\n");
	return(ENOMEM);
    }

    return(0);

}

/*
 * MFS_MNINIT_VOBFREEHASH - Internal routine to allocate and initialize the 
 *			vobfree hash table.
 */
STATIC int
mvfs_mninit_vobfreehash(mvfs_cache_sizes_t *mma_sizes)
{
    int i;
    int mlp_poolsize;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * Dynamically allocate the vobfree hash table based on the maximum 
     * size (mvfs_vobfreehashsize).  This keeps the average chain length 
     * reasonably long without causing a lock bottleneck.
     */

    /* Free old vobfree hash table memory, if it was dynamically allocated */
    if (mndp->mvfs_vobfreehash != NULL) {
	KMEM_FREE(mndp->mvfs_vobfreehash, 
		  mndp->mvfs_vobfreehashsize*sizeof(mvfs_vobfreehash_slot_t));
	mndp->mvfs_vobfreehash = NULL;
    }

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_VOBFREEHASHTAB_SZ] = mndp->mvfs_vobfreehashsize;
    MVFS_SIZE_DEFLOAD_NONZERO(mndp->mvfs_vobfreehashsize, mma_sizes, 
			      VOBFREEHASHTAB_SZ,
			      mvfs_mnfind_vobfreehashsize(mndp->mvfs_vobhashsize));

    mndp->mvfs_vobfreehash = (mvfs_vobfreehash_slot_t *)
	       KMEM_ALLOC(mndp->mvfs_vobfreehashsize*sizeof(mvfs_vobfreehash_slot_t),
	       KM_SLEEP);
    if (mndp->mvfs_vobfreehash == NULL) {
	mvfs_log(MFS_LOG_WARN, 
		 "Failed to allocate %d bytes for vobfree hash, trying minimum (%d bytes) instead.\n",
		 mndp->mvfs_vobfreehashsize*sizeof(mvfs_vobfreehash_slot_t),
		 MVFS_VOBFREEHASHMIN*sizeof(mvfs_vobfreehash_slot_t));
	mndp->mvfs_vobfreehashsize = MVFS_VOBFREEHASHMIN;
	mndp->mvfs_vobfreehash = (mvfs_vobfreehash_slot_t *)
	       KMEM_ALLOC(mndp->mvfs_vobfreehashsize*sizeof(mvfs_vobfreehash_slot_t),
	       KM_SLEEP);
	if (mndp->mvfs_vobfreehash == NULL) {
	    mvfs_log(MFS_LOG_ERR, "Could not allocate minimum size for mnode vobfreehash.\n");
	    return(ENOMEM);
	}
    }

    /* Initialize the vob freelist and running count to help manage freelist. */
    for (i = 0; i < mndp->mvfs_vobfreehashsize; i++) {
	mndp->mvfs_vobfreehash[i].mn_hdr.next = 0;
	mndp->mvfs_vobfreehash[i].mn_hdr.prev = 0;
	mndp->mvfs_vobfreehash[i].mn_hdr.free_next = 
				(mfs_mnode_t *)&(mndp->mvfs_vobfreehash[i]);
	mndp->mvfs_vobfreehash[i].mn_hdr.free_prev = 
				(mfs_mnode_t *)&(mndp->mvfs_vobfreehash[i]);
    }
    mndp->mvfs_vobfreecnt = 0;

    /* Initialize the vobfreemax and min. */
    MVFS_SIZE_DEFLOAD_NONZERO(mcdp->mvfs_vobfreemax, mma_sizes, VOBFREEMAX,
                              VOBFREEMAX_FORMULA(mcdp->mvfs_mnmax));
    mvfs_mn_clamp_vobfree(mma_sizes,
                          FALSE,        /* not silent */
                          mcdp->mvfs_mnmax,
                          &(mcdp->mvfs_vobfreemax),
                          &(mcdp->mvfs_vobfreemin));

    /* NOTE:  mvfs_cvpfreecnt is very sensitive to max number of inodes 
     * available in FS's */
    mndp->mvfs_cvpfreecnt = 0;

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_CVPFREEMAX] = mcdp->mvfs_cvpfreemax;
    /*
     * be silent if no user-specified min or max; blurt if needed when
     * either is set.
     */
    if (MVFS_SIZE_VALID(mma_sizes, CVPFREEMAX)) {
	MVFS_SIZE_LOAD(mcdp->mvfs_cvpfreemax, mma_sizes, CVPFREEMAX);
	mvfs_mn_clamp_cvpfree(mma_sizes, FALSE, /* not silent */
                              mcdp->mvfs_vobfreemax,
                              &(mcdp->mvfs_cvpfreemax), &(mcdp->mvfs_cvpfreemin));
    } else {
        mcdp->mvfs_cvpfreemax = mvfs_compute_cvpfreemax(mcdp->mvfs_largeinit);
	mvfs_mn_clamp_cvpfree(mma_sizes,
                              /* no max set, so be silent if no min either */
                              !MVFS_SIZE_VALID(mma_sizes, CVPFREEMIN),
                              mcdp->mvfs_vobfreemax,
                              &(mcdp->mvfs_cvpfreemax), &(mcdp->mvfs_cvpfreemin));
    }

    /* 
     * Now initialize the lock pool for this hash table. 
     * Actual size is dependant on the HASH_MVFS_LOCK_RATIO for the platform.
     */
    HASH_MVFS_LOCK_SET_POOLSIZE(mlp_poolsize, mndp->mvfs_vobfreehashsize);
    if (mvfs_lock_pool_init(&(mndp->mvfs_vobfreehash_mlp), mlp_poolsize, NULL, 
			      "mvfs_vobfreehash_mlp") != 0) {
	mvfs_log(MFS_LOG_ERR, "Could not allocate vobfreehash_mlp.\n");
	return(ENOMEM);
    }

    return(0);
}

/*
 * MVFS_MNFIND_VOBHASHSIZE - Internal routine to determine the size of the
 *			     vob hash table based on the maximum number of vob 
 *			     entries configured in the system.
 */
STATIC int
mvfs_mnfind_vobhashsize(int maxentry)
{
    int vhs;
    
    vhs = VOBHASHSIZE_FORMULA(maxentry);

    vhs = mvfs_ensure_power2(vhs);

    if (vhs < MFS_VOBHASHMIN)
	vhs = MFS_VOBHASHMIN;
    if (vhs > MFS_VOBHASHMAX)
	vhs = MFS_VOBHASHMAX;

    return(vhs);

}

/*
 * MVFS_MNFIND_VOBFREEHASHSIZE - Internal routine to determine the size of the
 *                             vobfree hash table
 */
STATIC int
mvfs_mnfind_vobfreehashsize(int vobhashsz)
{
    u_int vfhs;

    vfhs = VOBFREEHASHSIZE_FORMULA(vobhashsz);

    vfhs = mvfs_ensure_power2(vfhs);

    if (vfhs < MVFS_VOBFREEHASHMIN)
	vfhs = MVFS_VOBFREEHASHMIN;
    if (vfhs > MVFS_VOBFREEHASHMAX)
	vfhs = MVFS_VOBFREEHASHMAX;

    return(vfhs);

}

/*
 * MFS_MNGET - locate an existing, or create an MFS mnode.  If an mnode is
 *	       returned, it is locked.
 */
struct
mfs_mnode *
mfs_mnget(class, vw, fidp, vfsp, newnode)
mfs_class_t class;
VNODE_T *vw;
mfs_fid_t *fidp;
VFS_T *vfsp;
int *newnode;
{
    register mfs_mnode_t *mnp, *allocmnp;
    VFS_T *nvfsp;
    int mngen;
    int mnum = 0;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    *newnode = 0;		/* Assume not a new mnode */
    allocmnp = mnp = NULL;	/* Init both mnode pointers to null */

    /* Allocate/clone new vfs for loopclas mnodes */
    if (class == MFS_LOOPCLAS)
	nvfsp = MVFS_ALLOC_LOOPCLAS_VFS(vrdp, vfsp);
    else
	nvfsp = vfsp;
    /* Check for allocation failure */
    if (nvfsp == NULL) 
	return(NULL);

    /* If nvfsp ends up in an mnode->mn_hdr.vfsp, it will be assumed to be
     * an MVFS VFS pointer in other code.  Cloning systems must be sure
     * to make it an MVFS vfsp.
     */
    ASSERT(MVFS_VFSISMVFS(nvfsp, mfs_vfsopp));

    BUMPSTAT(mfs_mnstat.mnget);	/* Count gets */

    /*
     * The restart label is jumped to when we need to re-evaluate fetching 
     * the mnode.  Another thread may have allocated/activated the same 
     * object while we were growing the mnum_to_mnode table.
     */

restart:

    /* If NULL fidp, just create the mnode, it does not exist */
    if (fidp)
	/* 
	 * Try to find an existing mnode for this fid.  If one is found, the
	 * mnode header is locked and the mnode is not on the freelist.
	 */
	mnp = mvfs_mnfindexisting(class, vw, fidp, nvfsp);

    if (mnp == NULL) {
	if (mndp->mfs_mncnt >= mcdp->mvfs_mnmax) {
	    int mnmax = mcdp->mvfs_mnmax;
	    /* Grow table */
	    if (mvfs_mngrowtable(mnmax)) {
		/* Retry to see if an mnode for this fid been activated while 
		 * we were growing the table */
		MVFS_LOCK(&(mndp->mfs_mnlock));
		mndp->mfs_growrestart++;
		MVFS_UNLOCK(&(mndp->mfs_mnlock));
		goto restart;
	    }
	} else {
	    /* No table grow and not found */
	    mnum = mfs_mngetmnumslot();		/* Reserve a free slot */
	    if (mnum == 0) {
		mvfs_log(MFS_LOG_ERR, "mnget: unable to get mnode table slot\n");
	    } else {
		MVFS_LOCK(&(mndp->mfs_mnlock));
		mngen = ++(mndp->mfs_mngen);
		MVFS_UNLOCK(&(mndp->mfs_mnlock));
		/* Try to get a new one.  If an mnode is returned, the header
		 * is locked */
		/* This lock on the mnode header is not significant from a
		 * lock ordering perspective since the mnode is not in the
		 * hash table or mnum_to_mnode table. */
		allocmnp = mvfs_mnnew(class, mngen, mnum); 
		if (allocmnp != NULL) {
		    ASSERT(ISLOCKEDBYME(MHDRLOCK_ADDR(allocmnp)));
		    /* check for a fidp */
		    if (fidp) {
			/* 
			 * Since we allocated it, another thread may have
			 * filled in an mnode for this fid, check for this. 
			 */
			mnp = mvfs_mnfindexisting(class, vw, fidp, nvfsp);
			if (mnp == NULL) {
			    /* no existing mnode, use mnode just allocated */
			    mnp = allocmnp;
			    *newnode = 1;
			}
		    } else {
			/* No fid, so use the mnode we just allocated. */
			mnp = allocmnp;
			*newnode = 1;

		    }
		}
	    }
	} 
    }

    /* 
     * Bump reference count on mnode to reflect this "get".  This refcount 
     * is protected by the mnode header lock.
     */
    if (mnp) {
	ASSERT(ISLOCKEDBYME(MHDRLOCK_ADDR(mnp)));
	mnp->mn_hdr.mcount++;
    }

    /* If we created a new node initialize it. */
    if (mnp && *newnode == 1) {
	if (mvfs_mninit_new_mnode(mnp, class, vw, fidp, nvfsp, mnum) == 0) 
	{
	    /* Could not allocate rest of memory, free allocated mnode/slot */
	    ASSERT(allocmnp != NULL);
	    ASSERT(allocmnp == mnp);
	    MHDRUNLOCK(mnp);
	    MVFS_LOCK(&(mndp->mfs_mnlock));
	    MFS_MN_FREE_SLOT(mndp, mnum);
	    MVFS_UNLOCK(&(mndp->mfs_mnlock));
	    mvfs_mnfree(allocmnp);
	    return (NULL);
	}

	/*
	 * BEFORE putting new mnode onto it's hash chain, get the
	 * mnode lock on this mnode.  This protects the rest
	 * of the mnode initialization in mfs_makexxxnode from
	 * being seen by other "mnget"s.  This mnode lock
	 * CANNOT wait because nobody else can have this
	 * pointer yet to try and lock, so no deadlock possible.
	 */

	MLOCK(mnp);

	/* 
	 * Now make this mnode visible.  Place it in the mnum_to_mnode
	 * table first.  Searches through the mnum_to_mnode table (the get* 
	 * routines) can find this mnode as soon as the mfs_mnlock is released.
	 * These routines don't look at or care about the hash chains.
	 * Then place it in the proper hash chain.
	 * The other way, it would be in the hash table and could be found
	 * by mnget and used when it is still marked as in transition
	 * in the mnode table. 
	 */

	mnp->mn_hdr.mnum = mnum;
	MVFS_LOCK(&(mndp->mfs_mnlock));		/* Lock the monitor lock */
 	mndp->mnum_to_mnode[mnp->mn_hdr.mnum] = mnp;	
	MVFS_UNLOCK(&(mndp->mfs_mnlock));	/* Unlock the monitor lock */

	MHDRUNLOCK(mnp);

	mvfs_mnhash(mnp);

    } else {		/* end of If created mnp */

	/* Free allocated slot - we did not use it */
	if (mnum != 0) {
	    MVFS_LOCK(&(mndp->mfs_mnlock));	/* Lock the monitor lock */
	    MFS_MN_FREE_SLOT(mndp, mnum);	/* Remove from mnode table */
	    MVFS_UNLOCK(&(mndp->mfs_mnlock));	/* Unlock the monitor lock */
	}

	/* Free allocated vfs - we did not use it */
	if (class == MFS_LOOPCLAS)
	    MVFS_FREE_LOOPCLAS_VFS(vrdp, nvfsp);

	/*
	 * Lock new mnode.  We will wait here in races where
 	 * some other process created the mnode but hasn't finished
	 * initializing it yet.  Must unlock the header first in order
	 * to prevent a 3-party deadlock since mnget can be called with
	 * a directory mnode locked up in lookup. 
	 */
	if (mnp) {
	    ASSERT(mnp->mn_hdr.mcount > 0);
	    MHDRUNLOCK(mnp);
	    MLOCK(mnp);
	}
    }

    if (mnp) {
	if (fidp) {
	    MDB_XLOG((MDB_MNOPS,"get: mnp=%"KS_FMT_PTR_T", vw=%"KS_FMT_PTR_T", mmi=%"KS_FMT_PTR_T" (%s), fid=%x.%x, class=0x%x, *newnode=%x, mnum=0x%x\n",
		  mnp, vw, VFS_TO_MMI(mnp->mn_hdr.vfsp),
		  VFS_TO_MMI(mnp->mn_hdr.vfsp)->mmi_mntpath,
		  fidp->mf_dbid, fidp->mf_gen, 
		  class, *newnode, mnp->mn_hdr.mnum));
	}
	else {
	    MDB_XLOG((MDB_MNOPS, "get: mnp=%"KS_FMT_PTR_T", vw=%"KS_FMT_PTR_T", mmi=%"KS_FMT_PTR_T" (%s), class=0x%x, *newnode=%x, mnum=0x%x\n",
		  mnp, vw, VFS_TO_MMI(mnp->mn_hdr.vfsp),
		  VFS_TO_MMI(mnp->mn_hdr.vfsp)->mmi_mntpath, 
		  class, *newnode, mnp->mn_hdr.mnum));
	}
    }

    /* Free allocated mnode's memory if we did not use it */
    if ((allocmnp != NULL) && (mnp != allocmnp)) {
	MHDRUNLOCK(allocmnp);
	mvfs_mnfree(allocmnp);
    }	
    if (mnp) {
	ASSERT(ISLOCKEDBYME(MLOCK_ADDR(mnp)));
	ASSERT(mnp->mn_hdr.mcount >= 1);
    }
    return (mnp);
}

/*
 * MFS_MNFINDEXISTING - Internal routine to find a existing VOB or VOBRT mnode.
 *			If found, it returns an mnode with the mnode_header lock
 *			held.  The mnode has been removed from the vob freelist 
 *			if it was on the freelist.
 */

STATIC mfs_mnode_t *
mvfs_mnfindexisting(
    mfs_class_t class,
    VNODE_T *vw,
    register mfs_fid_t *fidp,
    VFS_T *vfsp
)
{
    register mfs_mnode_t *hp;
    register mfs_mnode_t *mnp;
    int hash_val;
    LOCK_T *mlplockp;

    ASSERT(fidp);

    switch (class) {
	case MFS_SDEVCLAS:
	case MFS_VIEWCLAS:
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    return(mvfs_mnfindexisting_subr1(class, vw, fidp, vfsp));

	case MFS_LOOPCLAS:
	    return(mvfs_mnfindexisting_subr2(class, vw, fidp, vfsp));

	case MFS_VOBRTCLAS:
	case MFS_VOBCLAS:
	    return(mvfs_mnfindexisting_subr3(class, vw, fidp, vfsp));

	default:
	    MDKI_PANIC("mvfs_mnfindexisting: illegal object class");
    }
    /* NOTREACHED */

}

STATIC mfs_mnode_t *
mvfs_mnfindexisting_subr1(
    mfs_class_t class,
    VNODE_T *vw,
    register mfs_fid_t *fidp,
    VFS_T *vfsp
)
{
    register mfs_mnode_t *hp;
    register mfs_mnode_t *mnp;
    int hash_val;
    LOCK_T *mlplockp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

	    ASSERT(fidp->mf_mnum > 0 && fidp->mf_mnum <= mndp->mvfs_mtmhwm);

	    /* Get hash value, lock proper hash lock and search for a match */
	    hash_val = MFS_OTHERHASH(mndp, *fidp);
	    MNOTHERHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    hp = (mfs_mnode_t *)&(mndp->mvfs_otherhash[hash_val]);

	    for (mnp = hp->mn_hdr.next; mnp != hp; mnp = mnp->mn_hdr.next) {
		if (mnp->mn_hdr.fid.mf_mnum == fidp->mf_mnum) {

		    /* Found it.  Lock the mnode header. */
		    MHDRLOCK(mnp);

		    /* If mnode is in the process of being destroyed, skip it */
		    /* This check is done prior to releasing the hash lock
		     * to ensure that we have a valid mnode that will not be
		     * released out from under us (by mvfs_mndestroy) prior
		     * to releasing the hash chain lock (releasing the hash
		     * chain will allow mvfs_mndestroy to proceed).
		     */
		    if (mnp->mn_hdr.on_destroy) {
			MHDRUNLOCK(mnp);
			continue;
		    }

		    MNOTHERHASH_MVFS_UNLOCK(&mlplockp);
		    return(mnp);
		}
	    }

	    /* No matching mnode found, unlock hash chain */
	    MNOTHERHASH_MVFS_UNLOCK(&mlplockp);
	    return(NULL);

}

STATIC mfs_mnode_t *
mvfs_mnfindexisting_subr2(
    mfs_class_t class,
    VNODE_T *vw,
    register mfs_fid_t *fidp,
    VFS_T *vfsp
)
{
    register mfs_mnode_t *hp;
    register mfs_mnode_t *mnp;
    int hash_val;
    LOCK_T *mlplockp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

	    /* Get hash value, lock proper hash lock and search for a match */
	    hash_val = MFS_CVPHASH(mndp, *fidp);
	    MNCVPHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    hp = (mfs_mnode_t *)&(mndp->mvfs_cvphash[hash_val]);

	    /*
	     * Search loopback hash chains for match.
             *
             * When loopback VFS cloning is in effect,
	     * mnp->mn_hdr.vfsp will be an equivalent but
	     * non-identical vfsp.  We could define a comparison macro
	     * for vfsp's which would ensure that the cloned VFSes are
	     * compatible, but that doesn't seem to be likely to
	     * change from platform to platform, so what I've done
	     * here is to check the mn_hdr.vfsp only if the mnode's
	     * vfsp is not a cloned vfsp (loopclas nodes should not
	     * have a mn_hdr.vfsp other than either mfs_viewroot_vfsp
	     * or a cloned vfsp) or the realvfsp's in the fids don't
	     * match.
	     */
	    for (mnp = hp->mn_hdr.next; mnp != hp; mnp = mnp->mn_hdr.next) {
		if (mnp->mn_hdr.viewvp == vw &&
		    mnp->mn_hdr.fid.mf_realvp == fidp->mf_realvp &&
		    (mnp->mn_hdr.vfsp == vfsp ||
		     /* check for adequately-matching cloned vfsp: */
		     ((mnp->mn_hdr.vfsp != vrdp->mfs_viewroot_vfsp &&
		       mnp->mn_hdr.fid.mf_realvfsp == fidp->mf_realvfsp))))
		{
		    MDB_XLOG((MDB_MNOPS,
			      "mnfind realvp %"KS_FMT_PTR_T" found %"KS_FMT_PTR_T"\n",
			      fidp->mf_realvp, mnp));

		    /* Found it.  Lock the mnode header, unlock hash chain */
		    MHDRLOCK(mnp);

		    /* If mnode is in the process of being destroyed, skip it */
		    /* This check is done prior to releasing the hash lock
		     * to ensure that we have a valid mnode that will not be
		     * released out from under us (by mvfs_mndestroy) prior
		     * to releasing the hash chain lock (releasing the hash
		     * chain will allow mvfs_mndestroy to proceed).
		     */
		    if (mnp->mn_hdr.on_destroy) {
			MHDRUNLOCK(mnp);
			continue;
		    }

		    MNCVPHASH_MVFS_UNLOCK(&mlplockp);
		    return(mnp);
		}
	    }

	    /* No matching mnode found, unlock hash chain */
	    MNCVPHASH_MVFS_UNLOCK(&mlplockp);
	    return(NULL);

}

STATIC mfs_mnode_t *
mvfs_mnfindexisting_subr3(
    mfs_class_t class,
    VNODE_T *vw,
    register mfs_fid_t *fidp,
    VFS_T *vfsp
)
{
    register mfs_mnode_t *hp;
    register mfs_mnode_t *mnp;
    int hash_val;
    LOCK_T *mlplockp;
    int f_hash_val;
    LOCK_T *flplockp;
    void *flusharg;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

	    /* Get hash value, lock the proper hash lock */
	    hash_val = MFS_VOBHASH(mndp, *fidp);
restart:
	    MNVOBHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    hp = (mfs_mnode_t *)&(mndp->mvfs_vobhash[hash_val]);

	    /* 
	     * Search vob/vob root hash chains for match.  Note that
	     *  the "view" is encoded in the fid for VOB objects. 
	     */
	    for (mnp = hp->mn_hdr.next; mnp != hp; mnp = mnp->mn_hdr.next) {
		if (mnp->mn_hdr.viewvp == vw &&
		    mnp->mn_hdr.fid.mf_dbid == fidp->mf_dbid &&
		    mnp->mn_hdr.stale == 0 &&
		    mnp->mn_hdr.vfsp == vfsp) {
		    /* XXX non-CC access problem: the generation for
		     * view-private objects can tick frequently.  An
		     * old handle from one NFS client may show up
		     * after the same view DBID has been issued a new
		     * generation number, yielding spurious markings
		     * of stale-ness here and maybe failure to find
		     * valid entries later when a request with the
		     * 'right' generation number shows up (especially
		     * since hash entries are inserted at the front of
		     * the chain).
		     */
		    if (fidp->mf_gen != MFS_UNK_GEN && 
				fidp->mf_gen != mnp->mn_hdr.fid.mf_gen) {
			/* Mark duplicate dbid's as stale to help FSS */
			BUMPSTAT(mfs_mnstat.mnfoundstale);

			/* 
			 * We manipulate mn_hdr.stale under the mnode lock.
			 * mngetnextoid is the only other place that cares
			 * about the stale bit, and it could return a stale 
			 * mnode already if nobody called mnfindexisting on the 
			 * particular mnode before it got called.  
			 * But since we may not have to wait on the
			 * mnode lock, we'll clean up the stale bit
			 * now. 
			 */
			if (MLOCK_NOWAIT(mnp)) {
			    mnp->mn_hdr.stale = 1;
			    /*
			     * Linux: We want to flush it from the dcache if
			     * possible.
			     */
			    if (mnp->mn_hdr.vp) {
				MVFS_START_FLUSH_DCACHE(mnp->mn_hdr.vp,
							&flusharg);
			    } else
				flusharg = NULL;
			    MUNLOCK(mnp);
			    if (flusharg != NULL) {
				/*
				 * We do the real work here after stopping
				 * use of mnp, since it may be released by
				 * the dcache and reclaimed. 
				 */
#ifdef MVFS_DEBUG
				mvfs_log(MFS_LOG_ESTALE,
					 "dcflush vp %p dp %p fid %x.%x",
					 mnp->mn_hdr.vp, flusharg,
					 mnp->mn_hdr.fid.mf_dbid,
					 mnp->mn_hdr.fid.mf_gen);
#endif
				MNVOBHASH_MVFS_UNLOCK(&mlplockp);
				MVFS_FINISH_FLUSH_DCACHE(flusharg);
				/*
				 * We dropped the hash lock, so we must
				 * always restart since the hash chain could
				 * have changed.  Rats.
				 */
				goto restart;
			    }
			}
			continue;
		    } else {
			/* Found it. Lock the mnode header, unlock hash chain */
			MHDRLOCK(mnp);

			/* If mnode is being destroyed, skip it */
			/* This check is done prior to releasing the hash lock
			 * to ensure that we have a valid mnode that will not be
			 * released out from under us (by mvfs_mndestroy) prior
			 * to releasing the hash chain lock (releasing the hash
			 * chain will allow mvfs_mndestroy to proceed).
			 */
			if (mnp->mn_hdr.on_destroy) {
			    MHDRUNLOCK(mnp);
			    continue;
			}

			/* If mnode is moving to the destroy list, skip it. */
			/* This flag is protected by the freelist hash lock.
			 * trans_destroy remains set after the mnode has been
			 * removed from the freelist (and so mn_hdr.mfree isn't
			 * set.) */
			f_hash_val = MVFS_VOBFREEHASH(mndp, mnp);
			MNVOBFREEHASH_MVFS_LOCK(mndp, f_hash_val, &flplockp);

			if (mnp->mn_hdr.trans_destroy) {
			    MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
			    MHDRUNLOCK(mnp);
			    continue;
			}

			/*
			 * Remove the mnode from the mvfs_vobfree hash list, if 
			 * it is there.  The mfree flag is protected by the 
			 * mvfs_vobfree hash lock.  Keep track of reclaims 
			 * from the "mvfs_vobfree list".
			 */
			if (mnp->mn_hdr.mfree) {
			    MN_RMFREE(mndp, flplockp, mnp);
			    BUMPSTAT(mfs_mnstat.mnreclaim);
			}
			MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);

			/* Now unlock the mnode hash chain. */
			MNVOBHASH_MVFS_UNLOCK(&mlplockp);

			BUMPSTAT(mfs_mnstat.mnfound);
			return(mnp);
		    }
		}
	    }

	    /* And unlock the mnode hash chain. */
	    MNVOBHASH_MVFS_UNLOCK(&mlplockp);
	    return(NULL);

}

/*
 * MFS_MNNEW - internal routine to create a new mnode and assign
 *	an mnode table slot.  Returns a clean mnode with the
 *	following mn_hdr fields initialized:
 *	    msize, mnum, lock, class, hdr_lock
 * and mnode header is locked.  The rest is zero'd.
 */
STATIC mfs_mnode_t *
mvfs_mnnew(
    mfs_class_t class,
    int mngen,
    int mnum
)
{
    int msize;
    register mfs_mnode_t *mnp;
    char name[8];	/* For lock name */

    /* Determine size of object we need */
    msize = mvfs_mngetmnodesize(class);

    /* Allocate the mnode and any other associated (gnode/vnode). */
    mnp = mvfs_mnallocatemnode(msize);

    /* Set up common mnode vars */
    if (mnp) {

	INITLOCK((MHDRLOCK_ADDR(mnp)), MAKESNAME(name, MHDRLOCK_PREFIX, mnum));
	mnp->mn_hdr.vp = NULL;	/* Not hooked to vnode yet (see makenode) */
	mnp->mn_hdr.msize = msize;
	INITLOCK(MLOCK_ADDR(mnp), MAKESNAME(name, MLOCK_PREFIX, mnum));
	mnp->mn_hdr.mclass = class;
	mnp->mn_hdr.dncgen = mngen;
	/* 
	 * Lock the mnode header so others won't access it when it goes on
	 * the hash list.
	 */
	MHDRLOCK(mnp);
	BUMPSTAT(mfs_mnstat.mncreate);
    }
    return(mnp);
}

/*
 * MVFS_MNINIT_NEW_MNODE - Internal routine to initialize an mnode after it has 
 *			   been allocated via mvfs_mnnew.  Called with mnode  
 *			   header locked.
 */

STATIC int
mvfs_mninit_new_mnode(mnp, class, vw, fidp, nvfsp, mnum) 
register mfs_mnode_t *mnp;
mfs_class_t class;
VNODE_T *vw;
mfs_fid_t *fidp;
VFS_T *nvfsp;
int mnum;
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    int len;
    char name[8];		/* for lock name */
    struct mfs_mntinfo *nmmi;	/* MFS mount info */
    SPL_T s;

    ASSERT(mnp->mn_hdr.mclass == class);
    ASSERT(mnp->mn_hdr.mcount == 1);
    ASSERT(mnum != 0);

    nmmi = VFS_TO_MMI(nvfsp);
    ASSERT(nmmi != 0);
    SPLOCK(nmmi->mmi_rclock, s);
    nmmi->mmi_refcnt++;   /* New vnode on mount */
    SPUNLOCK(nmmi->mmi_rclock, s);

    /* Do class independent init */

    if (vw) {
	MFS_HOLDVW(vw);
	mnp->mn_hdr.viewvp = vw;
    }

    /* Set fid with passed in one, or create the
       fid from the mnode number. */

    if (fidp) {
	mnp->mn_hdr.fid = *fidp;
    } else {
	mnp->mn_hdr.fid.mf_mnum = mnp->mn_hdr.mnum;
	mnp->mn_hdr.fid.mf_gen  = 0;
    }

    mnp->mn_hdr.vfsp = nvfsp;

    mnp->mn_hdr.mfree = 0;		/* not on the free list */
    mnp->mn_hdr.trans_destroy = 0;	/* not transitioning to destroy list */
    mnp->mn_hdr.on_destroy = 0;		/* not on the destroy list */

    /* Do class dependent init */

    switch (class) {
      case MFS_SDEVCLAS:
	break;
      case MFS_LOOPCLAS:
	ASSERT(fidp);
	mnp->mn_hdr.realvp = fidp->mf_realvp;
	CVN_HOLD(mnp->mn_hdr.realvp);
	break;
      case MFS_VIEWDIRCLAS:
	mnp->mn_ramdir.max = mcdp->mvfs_mnmax;
	len = mcdp->mvfs_mnmax * sizeof (struct mfs_ramdirent);
	mnp->mn_ramdir.ents = (struct mfs_ramdirent *) 
				KMEM_ALLOC(len, KM_SLEEP);
	if (mnp->mn_ramdir.ents == NULL) {
	    return (0);
	}
	BZERO(mnp->mn_ramdir.ents, len);
	mnp->mn_ramdir.atime.tv_sec = MDKI_CTIME();
	mnp->mn_ramdir.mtime.tv_sec = MDKI_CTIME();
	mnp->mn_ramdir.ctime.tv_sec = MDKI_CTIME();
	mnp->mn_ramdir.export_hwm = 0;
	mnp->mn_ramdir.export_ents = 0;
	mnp->mn_ramdir.lnk_cnt = 0;
	break;
      case MFS_VIEWCLAS:
      case MFS_NTVWCLAS:
	INITLOCK(STAMPLOCK_ADDR(mnp), MAKESNAME(name, STAMPLOCK_PREFIX, mnum));
	mnp->mn_view.pvstat =
	    (struct mvfs_pvstat *)KMEM_ALLOC(
					sizeof(*mnp->mn_view.pvstat),
					KM_NOSLEEP);
	if (mnp->mn_view.pvstat == (struct mvfs_pvstat *)NULL) {
	    return (0);
	}
	MVFS_PVSTATLOCK_INIT(mnp->mn_view.pvstat->mvfs_pvstatlock);
	mvfs_pview_stat_zero(mnp->mn_view.pvstat);
	break;
      case MFS_VOBRTCLAS:
	break;
      case MFS_VOBCLAS:
	INITLOCK(MCILOCK_ADDR(mnp), MAKESNAME(name, MCILOCK_PREFIX, mnum));
	break;
      default:
	MDKI_PANIC("mfs_mnget: unexpected object class\n");
	break;
    }	/* end of switch */

    return (1);

}

/*
 * MFS_MNRELE - drop mnode refcount and free up/destroy if
 * the refcount goes to 0.
 */

/*ARGSUSED*/

void
mfs_mnrele(mnp)
mfs_mnode_t *mnp;
{
    int call_destroy = 0;
    int runfree = 0;
    int hash_val;
    LOCK_T *flplockp;
    mfs_mnode_t *hp;
    mvfs_mnode_data_t *mndp;

    MHDRLOCK(mnp);

    /* Check refcounts before freeing.  Just decrement refcount and
       if non-zero, return. */

    mnp->mn_hdr.mcount--;	/* Decrement the mnode refcount */
    if (mnp->mn_hdr.mcount > 0) {
	MHDRUNLOCK(mnp);
	return;
    }

    /* 
     * When freeing, mnode MUST be disassociated from vnode or else
     * this is an inconsistency.  See the discussion of mnode activation
     * and deactivation at the mfs_makeXXXnode routines.
     */

    ASSERT(mnp->mn_hdr.vp == NULL);
    ASSERT(mnp->mn_hdr.mcount == 0);

   /* Now mndp will be for the proper Zone in all cases */
    mndp = MDKI_MNODE_GET_DATAP();

    /* Put on freelist or destroy list according to mnode type */
    switch (mnp->mn_hdr.mclass) {
	case MFS_VOBCLAS:

	    /*
	     * If view is there, add the mnode to the freelist, otherwise
	     * add the mnode to the destroylist
	     */
	    if (VTOM(mnp->mn_hdr.viewvp)->mn_view.id != MFS_NULLVID) {
		mnp->mn_hdr.freelist_time = MDKI_CTIME();
		BUMPSTAT(mfs_mnstat.mnfree);

		/* Get hash value, lock the proper hash lock, and insert */
		hash_val = MVFS_VOBFREEHASH(mndp, mnp);
		MNVOBFREEHASH_MVFS_LOCK(mndp, hash_val, &flplockp);
		hp = (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[hash_val]);
		/* Put the mnode on the back of the freelist LRU. */
		MN_INSFREE(mndp, hp, mnp); 
		MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);

		runfree = 1;
	    } else {
		/* Insert mnode at the end of the destroy list. */ 
		MN_INSDESTROY(mndp, mnp); 
		/* At this point, mvfs_findexisting and the mvfs_mngetnext***
		 * routines should skip this mnode if they find it in their
		 * search */
		call_destroy = 1;
	    }
	    break;

	default:
	    /* Insert mnode at the end of the destroy list. */
	    MN_INSDESTROY(mndp, mnp); 
	    /* At this point, mvfs_findexisting and the mvfs_mngetnext***
	     * routines should skip this mnode if they find it in their
	     * search */
	    call_destroy = 1;
	    break;
    }
    ASSERT(mnp->mn_hdr.vp == NULL);
	
    MHDRUNLOCK(mnp);

    /*
     * If a mnode needs to be destroyed, destroy it.  We call destroy this
     * way to avoid lock ordering problems.  Calling mvfs_mndestroy 
     * above would deadlock by attempting to take the hash chain and 
     * mfs_mnlock locks with the mnode header lock held.
     */
    if (call_destroy) mvfs_mndestroy_list();

    /* 
     * Call routine to "manage" the freelist.
     */

    if (runfree) mvfs_mnfreelist_mgmt();

    return;
}

/*
 * MVFS_MNDESTROY_LIST - Internal routine to call mvfs_mndestroy on all the 
 * items on the mnode destroy list. These destroys are done under the 
 * mvfs_mndestroylist lock to allow mvfs_mnfindexisting (which needs to take
 * the freelist lock) to run concurrently with mvfs_mndestroy.
 */

STATIC void
mvfs_mndestroy_list(void)
{

    register mfs_mnode_t *mnp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    MVFS_LOCK(&(mndp->mvfs_mndestroylock));
    while ((mnp = mndp->mvfs_mndestroylist.mn_hdr.free_next) 
				!= (mfs_mnode_t *)&(mndp->mvfs_mndestroylist)) {
	/* The mvfs_mndestroylock is dropped and re-obtained by mvfs_mndestroy,
	 * so the list can change. */
	mvfs_mndestroy(mnp);
    }
    MVFS_UNLOCK(&(mndp->mvfs_mndestroylock));

}

/*
 * MFS_MNDESTROY - release resources and free up memory for an mnode
 *		   Called with mvfs_mndestroylock held.  We drop this lock
 * 		   but re-obtain it for our caller's convenience.
 */

STATIC void
mvfs_mndestroy(mnp)
register mfs_mnode_t *mnp;
{
    VFS_T *vfsp;
    SPL_T s;
    struct mfs_mntinfo *mmi;	/* MFS mount info */
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    ASSERT(ISLOCKEDBYME(&(mndp->mvfs_mndestroylock)));
    BUMPSTAT(mfs_mnstat.mndestroy);

    /* 
     * Take the mnode off the destroy list and unlock the mvfs_mndestroylock.
     * The on_destroy flag remains set in the mnode header, so any searches 
     * will skip this mnode.  For lock ordering, the mvfs_mndestroylock must 
     * not be held when we lock the hash chain (creates a 3-party deadlock).
     */
    MN_RMDESTROY(mndp, mnp);
    ASSERT(mnp->mn_hdr.vp == NULL);

    MVFS_UNLOCK(&(mndp->mvfs_mndestroylock));

    /* 
     * Now remove the mnode from the hash chain and the mnode table so
     * it will not be found in any searches by mvfs_findexisting or
     * the mfs_mngetnextxxx routines.
     */
    mvfs_mnunhash(mnp);	
    ASSERT(mnp->mn_hdr.next == NULL);
    ASSERT(mnp->mn_hdr.prev == NULL);
    MVFS_LOCK(&(mndp->mfs_mnlock));
    MFS_MN_FREE_SLOT(mndp, mnp->mn_hdr.mnum);
    MVFS_UNLOCK(&(mndp->mfs_mnlock));

    /* 
     * Update refcount on vfs, we are destroying the ptr to it.
     * At this point nobody can find this mnode.
     */

    ASSERT(mnp->mn_hdr.vfsp != NULL);	
    if (mnp->mn_hdr.vfsp != NULL) {
	vfsp = mnp->mn_hdr.vfsp;

	mmi = VFS_TO_MMI(vfsp);
	SPLOCK(mmi->mmi_rclock, s);
	mmi->mmi_refcnt--;
	if ((long)mmi->mmi_refcnt < 0) {
	    mmi->mmi_refcnt = 0;
	    SPUNLOCK(mmi->mmi_rclock, s);
	    mvfs_log(MFS_LOG_ERR, "mndestroy: refcnt < 0: vob=%s\n",
		     mmi->mmi_mntpath);
	} else {
	    SPUNLOCK(mmi->mmi_rclock, s);
        }
	mnp->mn_hdr.vfsp = NULL;
    }
    else
	/* should never occur, but code defensively */
	vfsp = NULL;

    /* 
     * Release all resources.  
     * the mnode has been removed from the hash list, mnode table 
     * and destroy list.  It is just "meat" now, ready to be cleaned and 
     * deallocated. 
     */

    /* Release allocated loopclas vfs */
    if (vfsp != NULL && MFS_ISLOOP(mnp))
	MVFS_FREE_LOOPCLAS_VFS(vrdp, vfsp);

    mvfs_mnclean(mnp);

    /* Back pointer must be NULL here e.g. mnode not "attached" to vnode */
    ASSERT(mnp->mn_hdr.vp == NULL);

    mvfs_mnfree(mnp);

    /*
     * Re-acquire destroy lock for our caller's convenience.
     */

    MVFS_LOCK(&(mndp->mvfs_mndestroylock));
    MDB_XLOG((MDB_MNOPS, "mndestroy: mnp=%lx mmi=%lx\n",
	      mnp, vfsp ? VFS_TO_MMI(vfsp) : 0));
}

/*
 *	mvfs_mnclean   - release all resources (except lock/mnum) for
 *		  	an mnode hdr.  MUST NOT HOLD THE MNODE LOCK.
 */

STATIC void
mvfs_mnclean(mnp)
register mfs_mnode_t *mnp;
{
    int len;

    /* 
     * Take the header lock here; this is important to do even when the ASSERT
     * statements are not turned on.  Taking the header lock ensures that 
     * we don't attempt to release the memory for the header lock while another
     * thread is still in the mfs_mnrele code (which releases the lock on the
     * destroylist prior to unlocking the header lock).
     */
    MHDRLOCK(mnp);
    ASSERT(mnp->mn_hdr.mfree == 0);
    ASSERT(mnp->mn_hdr.on_destroy == 1);  /* just because its never cleared */
    MHDRUNLOCK(mnp);

    /* Destroy class dependent info */

    switch (mnp->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    break;		/* Nothing to do */
	case MFS_LOOPCLAS:
	    break;
	case MFS_VOBRTCLAS:
	    mfs_index_cache_destroy(mnp);
	    break;
	case MFS_VIEWDIRCLAS:
	    /* Fixme: can't do this here - no vp! */
	    if (mnp->mn_hdr.vp)
		mfs_ramdir_purge(mnp->mn_hdr.vp, RELEASE);
	    len = mnp->mn_ramdir.max * sizeof(struct mfs_ramdirent);
	    if (mnp->mn_ramdir.ents)
		KMEM_FREE(mnp->mn_ramdir.ents, len);
	    mnp->mn_ramdir.ents = NULL;
	    len = mnp->mn_ramdir.export_hwm * sizeof(int);
	    if (mnp->mn_ramdir.export_ents)
		KMEM_FREE(mnp->mn_ramdir.export_ents, len);
	    mnp->mn_ramdir.export_ents = NULL;
	    mfs_index_cache_destroy(mnp);
	    break;
	case MFS_VIEWCLAS:
	case MFS_NTVWCLAS:
	    mfs_svrdestroy(&mnp->mn_view.svr);
	    if (mnp->mn_view.viewname) PN_STRFREE(mnp->mn_view.viewname);
	    FREELOCK(STAMPLOCK_ADDR(mnp)); /* Free lock resources */
	    if (mnp->mn_view.pvstat != NULL) {
                /*
                 * It would be good to have an ASSERT statement here to assert
                 * that the pvstatlock is not locked before freeing it.  But,
                 * at this time, there are no macros available to check the
                 * lock owners on a spin lock in our code.  If such macros are
                 * added in the future, include an assert statement here on the
                 * pvstatlock.
                 */
                MVFS_PVSTATLOCK_FREE(mnp->mn_view.pvstat->mvfs_pvstatlock);
		KMEM_FREE(mnp->mn_view.pvstat, sizeof(*mnp->mn_view.pvstat));
		mnp->mn_view.pvstat = NULL;
	    }
	    MVFS_FREE_ID(&mnp->mn_view.cuid);
	    MVFS_FREE_ID(&mnp->mn_view.cgid);
	    break;
	case MFS_VOBCLAS:
	    if (mnp->mn_vob.cleartext.nm) 
		PN_STRFREE(mnp->mn_vob.cleartext.nm);
	    MCLRCRED(mnp);      /* Free the cred */
	    if (mnp->mn_vob.rmv_name) 
		PN_STRFREE(mnp->mn_vob.rmv_name);
	    if (mnp->mn_vob.rmv_dvp) {
		VN_RELE(mnp->mn_vob.rmv_dvp);
		mnp->mn_vob.rmv_dvp = NULL;
	    }
	    if (mnp->mn_vob.rmv_cred) {
		MDKI_CRFREE(mnp->mn_vob.rmv_cred);
		mnp->mn_vob.rmv_cred = NULL;
	    }
	    MFS_REBINDINVAL(mnp);
	    if (mnp->mn_vob.slinktext) {
		KMEM_FREE(mnp->mn_vob.slinktext, mnp->mn_vob.slinklen);
		mnp->mn_vob.slinktext = NULL;
	    }
	    mfs_index_cache_destroy(mnp);
	    mvfs_rddir_cache_destroy(mnp);
	    MVFS_FREE_ID(&mnp->mn_vob.user_id);
	    MVFS_FREE_ID(&mnp->mn_vob.group_id);
	    /*
	     * Release the cleartext's vattr information stored
	     * by mvfs_clearattr.
	     */
	    MVFS_FREE_VATTR_FIELDS(&mnp->mn_vob.cleartext.va);
            if (mnp->mn_hdr.realvp) {
                MVFS_CTXT_VN_DETACH(mnp);
            }
	    break;
	default:
	    MDKI_PANIC("MVFS: unexpected object class\n");
	    break;
    }

    /* Release vnodes held in hdr and finally the lock */

#ifdef NFSV4_SHADOW_VNODE
    /* NFSv4 Compliance: RATLC01011478: NFSv4 introduces the concept of shadow
     * vnodes. Refer mvfs_openv_ctx() and the CR for the considerations and
     * changes in MVFS to handle shadow vnodes. 'realvp_master' is unset in 
     * mvfs_closev_ctx() after the mn_vob.open_count drops to zero. Thus, 
     * 'realvp_master' is not expected to be set here.  
     */
    if (mnp->mn_hdr.realvp_master) {
        mvfs_log(MFS_LOG_ERR,
            "new cltxt: unexpected NFSv4 master vnode in cleartext cache. master="
             KS_FMT_PTR_T", realvp="KS_FMT_PTR_T"\n",
             mnp->mn_hdr.realvp_master, mnp->mn_hdr.realvp);
        CVN_RELE(mnp->mn_hdr.realvp_master);
        mnp->mn_hdr.realvp_master = NULL;
    }
#endif

    if (mnp->mn_hdr.realvp) {
	CVN_RELE(mnp->mn_hdr.realvp);
	mnp->mn_hdr.realvp = NULL;
	if (MFS_ISVOB(mnp))
	    MVFS_RELEASE_CREDLIST(mnp);
    }
    if (MFS_ISVOB(mnp)) {
	FREELOCK(MCILOCK_ADDR(mnp));
    }
    if (mnp->mn_hdr.viewvp) {
	VN_RELE(mnp->mn_hdr.viewvp);
	mnp->mn_hdr.viewvp = NULL;
    }
}

/*
 * MFS_MNFREE - Clean up mfs resources allocated in mvfs_mnnew and free mnode
 */
STATIC void
mvfs_mnfree(
    mfs_mnode_t *mnp
)
{
    VNODE_T *vp;

    /* free both the mnode header lock and the mnode lock */
    FREELOCK(MHDRLOCK_ADDR(mnp));  /* Free hdr lock resource before memory */
    FREELOCK(MLOCK_ADDR(mnp));	   /* Free lock resource before memory */

    KMEM_FREE(mnp, mnp->mn_hdr.msize);	/* Free up memory */
}

/*
 * MVFS_MNFREELIST_MGMT - manage the mnode freelist.
 */

STATIC void
mvfs_mnfreelist_mgmt(void)
{
    mfs_mnode_t *mnp;
    int vobf_count, vobf_npc;
    int i, j;
    LOCK_T *flplockp;
    mfs_mnode_t *hp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * Code used to decide if there was any need to free anything before 
     * taking the lock.  It's ok to be a bit sloppy, but it seemed this 
     * could get out of hand with large MP systems and the lock granularity
     * has been greatly reduced, so we take the lock.
     */

    MVFS_LOCK(&(mndp->mvfs_vobfreelock));

    if (mndp->mvfs_cvpfreecnt <= mcdp->mvfs_cvpfreemax && 
	mndp->mvfs_vobfreecnt <= mcdp->mvfs_vobfreemax) 
    {
	MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));
	return;
    }

    /*
     * If some other thread is reducing the freelist, don't bother to do it.
     */
    if (mndp->mvfs_mnfreelist_mgmt_ip == 1) {
	MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));
	return;
    } else {
	mndp->mvfs_mnfreelist_mgmt_ip = 1;	/* we are reducing the freelist */
	vobf_count = mndp->mvfs_vobfreecnt - mcdp->mvfs_vobfreemin;
	/*
	* Note: the difference between mvfs_vobfreemin and mvfs_vobfreemax is at
	* least the mvfs_vobfreehashsize (see clamp routines), so vobf_npc 
	* should typically be 1 or more.)
	*/
	vobf_npc = vobf_count/mndp->mvfs_vobfreehashsize;
	MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));
    }

    /*
     * Something needs "freeing".  Check for mnodes first, since
     * freeing mnodes may drop the cleartexts that must be freed.
     * Go through each vobfree hash chain and remove vobf_npc items from it.
     */

    for (i = 0; i < mndp->mvfs_vobfreehashsize; i++) {
	for (j = 0; j < vobf_npc; j++) {


	    /* Lock the hash chain. */
	    MNVOBFREEHASH_MVFS_LOCK(mndp, i, &flplockp);
	    hp = (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[i]);
	    mnp = hp->mn_hdr.free_next;

	    /*
	     * If there aren't enough on this hash chain, continue with next
	     * hash chain.  This means we won't reduce the list to the min,
	     * but that should be fine.
	     */ 
	    if (mnp == (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[i])) {
		MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
		continue;
	    }

	    /*
	     * Set the transitioning to destroy flag (which is protected by the 
	     * mvfs_vobfree hash lock) to ensure no searches (mvfs_findexisting 
	     * or mfs_mngetnext*) pick up this mnode while it is being 
	     * transitioned from the mvfs_vobfree hash list to the 
	     * mvfs_mndestroy list.
	     */
	    mnp->mn_hdr.trans_destroy = 1; /* protected by vobfreehash lock */
	    MN_RMFREE(mndp, flplockp, mnp);

	    /* Unlock mvfs_vobfree hashed lock.  For concurrency on MT systems 
	     * we want to allow processing the vob freelist concurrently with 
	     * processing of the destroylist, so we don't want to take any
	     * mvfs_mndestroylock while a mvfs_vobfree hash lock is held.
	     */
	    MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);

	    MN_INSDESTROY(mndp, mnp);
	}
    }

    /*
     * Now all the mnodes we wanted to destroy are on the destroy list.
     * Process these destroys under the mvfs_mndestroylock so there is no
     * lock contention with mvfs_vobfree hashed locks.
     */
    mvfs_mndestroy_list();
    
    /*
     * While we are here freeing stuff, drop the cleartexts back
     * down to the min, even if not over the max yet.  This
     * helps us to run freelist mgmt less.
     */
    mvfs_mnflush_cvpfreelist(MVFS_MN_CVPFLUSH_LOWMARK);

    MVFS_LOCK(&(mndp->mvfs_vobfreelock));
    mndp->mvfs_mnfreelist_mgmt_ip = 0;	/* we are done reducing the freelist */
    MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));

}

void
mvfs_mnflush_cvpfreelist(int type)
{

    mfs_mnode_t *mnp;
    int count;
    int orig_count;
    int count_per_chain;	/* max # of cvp's to remove from a hash chain */
    int cur_cpc;		/* Current count per chain */
    int cvpcnt;
    int hash_num;		/* hash chain number */
    LOCK_T *hash_lockp;		/* pointer to the mutex lock for a given hash */
    mfs_mnode_t *hp;		
    CLR_VNODE_T **cvplist;
    CLR_VNODE_T *stack_cvplist[MVFS_STK_FREEQLEN];
    time_t purge_time;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /* 
     * Determine how many cvp's to purge from the freelist.
     */
    count = 0;
    purge_time = MDKI_CTIME();

    MVFS_LOCK(&(mndp->mvfs_vobfreelock));
    /* Calculate the overall number of cvps to flush. */
    switch(type) {
	case MVFS_MN_CVPFLUSH_HALF:
	    count = mndp->mvfs_cvpfreecnt/2;
	    break;
	case MVFS_MN_CVPFLUSH_ALL:
	case MVFS_MN_CVPFLUSH_AGED:
	    count = mndp->mvfs_cvpfreecnt;
	    break;
	case MVFS_MN_CVPFLUSH_LOWMARK:
	default:
	    if (mndp->mvfs_cvpfreecnt > mcdp->mvfs_cvpfreemax)
		count = mndp->mvfs_cvpfreecnt - mcdp->mvfs_cvpfreemin;
	    else {
		MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));
		return;
	    }
    }
    MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));

    /* 
     * Check limits first before allocating memory, etc.
     */
    if (count == 0) {
	/* cvpfreecnt must have been 0 (AGED/ALL), or 1 (HALF) */
	return;
    }

    if ((cvplist = (CLR_VNODE_T **)KMEM_ALLOC(sizeof(CLR_VNODE_T *) * count, 
					      KM_NOSLEEP)) == NULL)
    {
	/*
	 * Some is better than none
	 */
	cvplist = &stack_cvplist[0];
	/* reduce the number of cvps to flush so they fit in 
	 * stack_cvplist. */
	if (count > MVFS_STK_FREEQLEN)
	    count =  MVFS_STK_FREEQLEN;
    }

    orig_count = count;

    /* Determine how many cvps to flush from each hash chain. */
    switch(type) {
	case MVFS_MN_CVPFLUSH_HALF:
	    /* Calculate the average number of cvps to take
	     * from each hash chain and round up since there
	     * won't be a perfect distribution on every hash 
	     * chain. */
	    count_per_chain = (count/mndp->mvfs_vobfreehashsize) + 1;
	    break;
	case MVFS_MN_CVPFLUSH_AGED:
	case MVFS_MN_CVPFLUSH_ALL:
	    /* Since we want to flush every cvp, make the 
	     * count_per_chain equal to the max we want to 
	     * take in case all the cvp's are on 1 hash chain. */
	    count_per_chain = count;
	    break;
	case MVFS_MN_CVPFLUSH_LOWMARK:
	default:
	    /* Since the split between cvpfreemin and cvpfreemax is at 
	     * least mvfs_vobfreehashsize, we will always take at least
	     * 1 cvp off each hash chain.  Since we are rounding down,
	     * we will typically not reduce the number of cvps to the
	     * exact minimum, but we will be within mvfs_vobfreehashsize
	     * of the minimum. */
	    count_per_chain = count/mndp->mvfs_vobfreehashsize;
	    break;
    }
    cvpcnt = 0;

    /* Run through every vobfree hash chain */
    for (hash_num = 0; hash_num < mndp->mvfs_vobfreehashsize; hash_num++) {

	/* Lock the specific vob hash chain */
	MNVOBFREEHASH_MVFS_LOCK(mndp, hash_num, &hash_lockp);
	hp = (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[hash_num]);

	/* 
	 * Search every entry on the hash chain.  Compile a list of mnodes
	 * that need to be flushed, and ensure they stay around until we 
	 * flush them.
	 */
	for (mnp = hp->mn_hdr.free_next, cur_cpc = 0; 
	     mnp != hp && cur_cpc < count_per_chain && count > 0; 
	     mnp = mnp->mn_hdr.free_next) 
	{

	    ASSERT(MFS_ISVOB(mnp));

	    if (((type == MVFS_MN_CVPFLUSH_AGED) &&
		((mnp->mn_hdr.freelist_time + mndp->mvfs_nt_age_cvp_time) > 
								purge_time)))
	    {
		break;
	    }
	    if (mnp->mn_hdr.realvp != NULL) {
		count--;
		cur_cpc++;
		cvplist[cvpcnt++] = mnp->mn_hdr.realvp;
		/* Assume OK without MLOCK because nobody should have this mnode
		 * active and still on the freelist.  Also, nobody can activate
		 * this mnode while we hold the vobfree hash list lock. */
                if (mnp->mn_hdr.mclass == MFS_VOBCLAS) {
                    MVFS_CTXT_VN_DETACH(mnp);
                }
		mnp->mn_hdr.realvp = NULL;

#ifdef NFSV4_SHADOW_VNODE
                /* NFSv4 Compliance: RATLC01011478: NFSv4 introduces the concept
                 * of shadow vnodes. Refer mvfs_openv_ctx() and the CR for the
                 * considerations and changes in MVFS to handle shadow vnodes. 
                 * 'realvp_master' is unset in mvfs_closev_ctx() after the
                 * mn_vob.open_count drops to zero. Thus, 'realvp_master' is not
		 * expected to be set here.  
                 */
                if (mnp->mn_hdr.realvp_master) {
                    mvfs_log(MFS_LOG_ERR,
                       "new cltxt: unexpected NFSv4 master vnode in cleartext cache. master="
                        KS_FMT_PTR_T", realvp="KS_FMT_PTR_T"\n",
                        mnp->mn_hdr.realvp_master, mnp->mn_hdr.realvp);
                    CVN_RELE(mnp->mn_hdr.realvp_master);
                    mnp->mn_hdr.realvp_master = NULL;
	        }
#endif

		MVFS_LOCK(&(mndp->mvfs_vobfreelock));
		mndp->mvfs_cvpfreecnt--;
		MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));
		if (mnp->mn_vob.cleartext.nm) {	/* Free cltxt name too! */
		   PN_STRFREE(mnp->mn_vob.cleartext.nm);
		}
		MVFS_RELEASE_CREDLIST(mnp);
	    }
	}
	MNVOBFREEHASH_MVFS_UNLOCK(&hash_lockp);
    }

    /* 
     * Now that we don't have any of the vobfree hash locks, we can VN_RELE 
     * the vnode ptrs we took out of the mnodes.
     */
    while (--cvpcnt >= 0) {
	CVN_RELE(cvplist[cvpcnt]);
    }

    if (cvplist != &stack_cvplist[0])
	KMEM_FREE(cvplist, (sizeof(CLR_VNODE_T *) * orig_count));

    return;
}

/*
 * Count active mnodes.
 * Note: only called during the unload.
 */

int
mfs_mncount(void)
{
    int i;
    int count = 0;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if (mndp->mnum_to_mnode == NULL) 
	return(0);
    MVFS_LOCK(&(mndp->mfs_mnlock));
    /* Look at all entries in mnum_to_mnode table to ensure none are missed */
    for (i = 0; i < mcdp->mvfs_mnmax; i++) {
	if (mndp->mnum_to_mnode[i] != NULL) 
	    count++;
    }
    MVFS_UNLOCK(&(mndp->mfs_mnlock));
    return(count);
}

/* 
 * Generate a new attribute generation number
 */

u_long
mfs_mn_newattrgen(void)
{
    int mnum;
    mfs_mnode_t *mnp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    MVFS_LOCK(&(mndp->mfs_mnlock));
    mndp->mfs_attrgen++;
    /* 
     * Check for rollover 
     * Handle by invalidating attrs in all VOB mnodes
     */
    if (mndp->mfs_attrgen == 0) {
	mndp->mvfs_attgenrollover++;
	/* search all of the active mnodes */
	for (mnum = 1; mnum <= mndp->mvfs_mtmhwm; mnum++) {
	    mnp = mndp->mnum_to_mnode[mnum];
	    if ((mnp == NULL) || (mnp == MFS_MN_INTRANS)) continue;
	    if (MFS_ISVOB(mnp)) {
		/* Typically attrtime is locked by the mnode lock,
		 * but since this is a 1 word write, we don't need
		 * a lock here. */
		mnp->mn_vob.attrtime.tv_sec = 0;
	    }
	}
    }
    MVFS_UNLOCK(&(mndp->mfs_mnlock));
    return(mndp->mfs_attrgen);
}

/*
 * Return current counts and maximums for mnode cache.
 */

void
mvfs_mn_count(usage)
mvfs_cache_usage_t *usage;
{
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    usage->cache_usage[MVFS_CACHE_INUSE][MVFS_CACHE_MNODE_TBL] = mndp->mfs_mncnt;
    usage->cache_usage[MVFS_CACHE_MAX][MVFS_CACHE_MNODE_TBL] = mcdp->mvfs_mnmax;

    usage->cache_usage[MVFS_CACHE_INUSE][MVFS_CACHE_MFREE] = mndp->mvfs_vobfreecnt;
    usage->cache_usage[MVFS_CACHE_MAX][MVFS_CACHE_MFREE] = mcdp->mvfs_vobfreemax;

    usage->cache_usage[MVFS_CACHE_INUSE][MVFS_CACHE_CTFREE] = mndp->mvfs_cvpfreecnt;
    usage->cache_usage[MVFS_CACHE_MAX][MVFS_CACHE_CTFREE] = mcdp->mvfs_cvpfreemax;
}

/*
 * Set new mnode cache sizes.
 */

int
mvfs_mn_setcaches(szp)
mvfs_cache_sizes_t *szp;
{
    int grew;
    MVFS_WATERMARK_TYPE vobfreemax, vobfreemin, cvpfreemax, cvpfreemin;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /* Work with a snapshot of mvfs_mnmax, so that we don't ask
     * the table to grow to passed in value which was greater than mvfs_mnmax
     * when first we looked but is less than mvfs_mnmax by the time
     * we call growtable_to().
     */
    u_long mnmax = mcdp->mvfs_mnmax;

    if (MVFS_SIZE_VALID(szp, MNMAX)) {
	if (szp->size[MVFS_SETCACHE_MNMAX] > mnmax) {
	    /* FIXME: clamp to some maximum size? */
	    grew = mvfs_mngrowtable_to(mnmax,
				       szp->size[MVFS_SETCACHE_MNMAX], 
				       KM_NOSLEEP);
	    if (grew == 0)
		return ENOMEM;
	} else if (szp->size[MVFS_SETCACHE_MNMAX] < mnmax)
	    return EINVAL;
    }
    szp->size[MVFS_SETCACHE_MNMAX] = mcdp->mvfs_mnmax;
    MVFS_LOCK(&(mndp->mvfs_vobfreelock));
    /* 
     * The lock prevents other changes, but other users can read values
     * while we're adjusting them (before applying clamp rules)
     * so we work on intermediate values and write back when done.
     * (If they read at just the time we are writing the rest, they still
     * may see an inconsistency; see comments below.)
     */
    vobfreemax = mcdp->mvfs_vobfreemax;
    vobfreemin = mcdp->mvfs_vobfreemin;
    cvpfreemax = mcdp->mvfs_cvpfreemax;
    cvpfreemin = mcdp->mvfs_cvpfreemin;
    MVFS_SIZE_RUNTIME_SET(vobfreemax, szp, VOBFREEMAX);
    MVFS_SIZE_RUNTIME_SET(vobfreemin, szp, VOBFREEMIN);
    if (MVFS_SIZE_VALID(szp, VOBFREEMAX) || MVFS_SIZE_VALID(szp, VOBFREEMIN)) {
        mvfs_mn_clamp_vobfree(szp,
                              TRUE,     /* SILENT */
                              mcdp->mvfs_mnmax,
                              &vobfreemax,
                              &vobfreemin);
	szp->size[MVFS_SETCACHE_VOBFREEMAX] = vobfreemax;
	szp->size[MVFS_SETCACHE_VOBFREEMIN] = vobfreemin;
    }

    MVFS_SIZE_RUNTIME_SET(cvpfreemax, szp, CVPFREEMAX);
    MVFS_SIZE_RUNTIME_SET(cvpfreemin, szp, CVPFREEMIN);

    /* mvfs_mn_clamp_cvpfree() is called even if user doesn't change them,
       since vobfreemax can be set to a value smaller than cvpfreemax  */
    mvfs_mn_clamp_cvpfree(szp, TRUE, /* SILENT */
                          vobfreemax,
                          &cvpfreemax,
                          &cvpfreemin);
    szp->size[MVFS_SETCACHE_CVPFREEMAX] = cvpfreemax;
    szp->size[MVFS_SETCACHE_CVPFREEMIN] = cvpfreemin;

    /* Careful: we want to avoid tricking other code into computing a
     * negative spread between current values and the low-water mark.
     * Because we may currently be in an over-full condition (free
     * list management is about to flush the lists), the start case
     * might be min < max < current.  If the caller is increasing min
     * and max above current, then (current - min) would be negative.
     * We can't do much to avoid this without making the freelist code
     * contend for the vobfreelock (which we don't want to do) so that
     * it gets consistent values for the min & max values.  So we put
     * the onus on users of these variables to be ready to handle
     * "unusual" relationships between min, max and current.
     */
    mcdp->mvfs_vobfreemin = vobfreemin;
    mcdp->mvfs_vobfreemax = vobfreemax;
    mcdp->mvfs_cvpfreemin = cvpfreemin;
    mcdp->mvfs_cvpfreemax = cvpfreemax;
    MVFS_UNLOCK(&(mndp->mvfs_vobfreelock));

    mvfs_mnfreelist_mgmt();
    return 0;
}

int
mvfs_mn_getcaches(
    mvfs_cache_sizes_t *szp
)
{
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    szp->size[MVFS_SETCACHE_MNMAX] = mcdp->mvfs_mnmax;
    szp->size[MVFS_SETCACHE_VOBFREEMAX] = mcdp->mvfs_vobfreemax;
    szp->size[MVFS_SETCACHE_VOBFREEMIN] = mcdp->mvfs_vobfreemin;
    szp->size[MVFS_SETCACHE_CVPFREEMAX] = mcdp->mvfs_cvpfreemax;
    szp->size[MVFS_SETCACHE_CVPFREEMIN] = mcdp->mvfs_cvpfreemin;
    szp->size[MVFS_SETCACHE_AGE_CVP_TIME] = (ks_uint32_t)mndp->mvfs_nt_age_cvp_time;
    szp->size[MVFS_SETCACHE_VOBHASHTAB_SZ] = mndp->mvfs_vobhashsize;
    szp->size[MVFS_SETCACHE_CVPHASHTAB_SZ] = mndp->mvfs_cvphashsize;

    return(0);
}

/*
 * UTILITY ROUTINES.  Call these routines with the mnode lock held!
 */

/*
 * MFS_MNHASH: hash an mnode onto the right hash chain
 */

STATIC void
mvfs_mnhash(
    register mfs_mnode_t *mnp
)
{
    int hash_val;
    LOCK_T *mlplockp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    ASSERT(mnp->mn_hdr.next == NULL);	/* Not already hashed */
    ASSERT(mnp->mn_hdr.prev == NULL);

    /* 
     * Add to hash table if appropriate.  Add to beginning of hash 
     * chain on assumption we will want to find it again quickly.
     */

    switch (mnp->mn_hdr.mclass) {
	case MFS_VOBCLAS:
	case MFS_VOBRTCLAS: 
	    hash_val = MFS_VOBHASH(mndp, mnp->mn_hdr.fid); 
	    MNVOBHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    MN_INSHASH((mfs_mnode_t *)&(mndp->mvfs_vobhash[hash_val]), mnp); 
	    MNVOBHASH_MVFS_UNLOCK(&mlplockp);
	    BUMPSTAT(mfs_mnstat.mnvobhashcnt);
	    break;
	case MFS_LOOPCLAS:  
	    hash_val = MFS_CVPHASH(mndp, mnp->mn_hdr.fid); 
	    MNCVPHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    MN_INSHASH((mfs_mnode_t *)&(mndp->mvfs_cvphash[hash_val]), mnp); 
	    MNCVPHASH_MVFS_UNLOCK(&mlplockp);
	    BUMPSTAT(mfs_mnstat.mncvphashcnt);
	    break;
	default:
	    hash_val = MFS_OTHERHASH(mndp, mnp->mn_hdr.fid); 
	    MNOTHERHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    MN_INSHASH((mfs_mnode_t *)&(mndp->mvfs_otherhash[hash_val]), mnp); 
	    MNOTHERHASH_MVFS_UNLOCK(&mlplockp);
	    BUMPSTAT(mfs_mnstat.mnotherhashcnt);
	    break;
    }
}

/*
 * MFS_MNUNHASH: unhash an mnode from hash chain.
 */

STATIC void
mvfs_mnunhash(mnp)
register mfs_mnode_t *mnp;
{
    int hash_val;
    LOCK_T *mlplockp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    /* Remove from hash chain */

    switch (mnp->mn_hdr.mclass) {
	case MFS_VOBCLAS:
	case MFS_VOBRTCLAS: 
	    hash_val = MFS_VOBHASH(mndp, mnp->mn_hdr.fid); 
	    MNVOBHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    MN_RMHASH(mnp); 
	    MNVOBHASH_MVFS_UNLOCK(&mlplockp);
	    DECSTAT(mfs_mnstat.mnvobhashcnt);
	    break;
	case MFS_LOOPCLAS:  
	    hash_val = MFS_CVPHASH(mndp, mnp->mn_hdr.fid); 
	    MNCVPHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    MN_RMHASH(mnp); 
	    MNCVPHASH_MVFS_UNLOCK(&mlplockp);
	    DECSTAT(mfs_mnstat.mncvphashcnt);
	    break;
	default:
	    hash_val = MFS_OTHERHASH(mndp, mnp->mn_hdr.fid); 
	    MNOTHERHASH_MVFS_LOCK(mndp, hash_val, &mlplockp);
	    MN_RMHASH(mnp); 
	    MNOTHERHASH_MVFS_UNLOCK(&mlplockp);
	    DECSTAT(mfs_mnstat.mnotherhashcnt);
	    break;
    }
}

/*
 * Clamp vobfreemax and vobfreemin at "reasonable" numbers.
 */

#define MIN_RESERVED_MNODE_SLOTS 200
#define MINIMUM_VOBFREE 48
#define MINIMUM_CVPFREE 48

STATIC void
mvfs_mn_clamp_vobfree(
    mvfs_cache_sizes_t *mma_sizes,
    tbs_boolean_t silent,
    u_long mnmax,
    u_long *vobfreemax_p,
    u_long *vobfreemin_p
)
{
    u_long limit; /* tmp var for upper/lower limit */
    u_long oldmin;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    /* At least 200 non-freelist mnode table slots */
    limit = mnmax - MIN_RESERVED_MNODE_SLOTS;
    if (*vobfreemax_p > limit) {
	if (!silent)
	    mvfs_log(MFS_LOG_WARN,
		     "VOB freelist maximum size reduced from %ld to %ld due to"
		     " mnmax of %ld\n",
		     *vobfreemax_p, limit, mnmax);
	*vobfreemax_p = limit;
    }
    /* Apply freelist minimum size */
    /* Adjust limit so VOBFREEMIN_FORMULA_2 doesn't become negative */
    limit = (MINIMUM_VOBFREE > mndp->mvfs_vobfreehashsize) ? 
					MINIMUM_VOBFREE : mndp->mvfs_vobfreehashsize;
    if (*vobfreemax_p < limit) {
	*vobfreemax_p = limit;
	if (!silent)
	    mvfs_log(MFS_LOG_WARN,
		     "VOB freelist maximum size raised to hard minimum %ld\n",
		     *vobfreemax_p);
    }

    /*
     * Allow setting of vobfreemin for tuning large systems (fetched
     * and tuned here so it can take account of the adjusted
     * vobfreemax in the formula).
     */
    MVFS_SIZE_DEFLOAD_NONZERO(*vobfreemin_p, mma_sizes, VOBFREEMIN,
			      VOBFREEMIN_FORMULA(*vobfreemax_p));
    /* But if it is greater than vobfreemax, set back to 90% */
    if (*vobfreemin_p >= *vobfreemax_p) {
	*vobfreemin_p = VOBFREEMIN_FORMULA(*vobfreemax_p); /* 90% */
	if (!silent)
	    mvfs_log(MFS_LOG_WARN,
		     "VOB freelist minimum size reduced to %ld due to "
		     "vobfreemax of %ld\n",
		     *vobfreemin_p, *vobfreemax_p);
    }

    /* 
     * But vobfreemax needs to be at least mvfs_vobfreehashsize bigger
     * than vobfreemin so the reduction algorithm gets at least 1 mnp to 
     * flush on each hash chain of the vobfree hash list.  See mvfs_mnflush. 
     */
    if ((*vobfreemax_p - *vobfreemin_p) < mndp->mvfs_vobfreehashsize) {
	oldmin = *vobfreemin_p;
	*vobfreemin_p = VOBFREEMIN_FORMULA_2(*vobfreemax_p, 
						mndp->mvfs_vobfreehashsize);
	if (!silent)
	    mvfs_log(MFS_LOG_WARN,
		     "VOB freelist minimum size reduced from %ld to %ld since "
		     "vobfreemax must be at least (vobfreemin+vobfreehashsize)\n",
		     oldmin, *vobfreemin_p);
    }
}

/*
 * Clamp cvpfreemax and cvpfreemin at "reasonable" numbers.
 */

#define MINIMUM_NON_MVFS_INODES 200

STATIC void
mvfs_mn_clamp_cvpfree(
    mvfs_cache_sizes_t *mma_sizes,
    int silent,
    u_long vobfreemax,
    u_long *cvpfreemax_p,
    u_long *cvpfreemin_p
)
{
    u_long limit; /* tmp var for upper/lower limit */
    A_CONST char *name;
    u_long oldmin;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /* At least MINIMUM_NON_MVFS_INODES free non-mfs held inodes */
    if ((NINODE(mcdp) - MINIMUM_NON_MVFS_INODES) < vobfreemax) {
	limit = NINODE(mcdp) - MINIMUM_NON_MVFS_INODES;
	name = NINODE_NAME;
	silent = 0;			/* force complaint if changing values */
    } else {
	limit = vobfreemax;
	name = "vobfreemax";
    }
    if (limit < MINIMUM_CVPFREE) {
	/* This is a bit disingenuous...if someone passed in
	 * vobfreemax which is less than MINIMUM_CVPFREE, or NINODE -
	 * MIN_NON_MVFS is that small, then we state the reduction is
	 * due to a small NINODE or vobfreemax, but we're not reducing
	 * it as low as it would be if we went all the way to the
	 * limit's value.  It is still appropriate to blame the
	 * limitation on the identified variable, however, since if it
	 * had been set higher we wouldn't have reduced cvpfreemax.
	 */
	limit = MINIMUM_CVPFREE;
    }
    if (*cvpfreemax_p > limit) {
	if (!silent)
	    mvfs_log(MFS_LOG_WARN,
		     "cleartext freelist size reduced from %ld to %ld entries"
		     " due to small %s parameter\n",
		     *cvpfreemax_p, limit, name);
	*cvpfreemax_p = limit;
    }
    /* Apply hard minimums */
    limit = MINIMUM_CVPFREE;
    /* Adjust limit so CVPFREEMIN_FORMULA doesn't become negative */
    if (CVPFREEMIN_FORMULA_SUB(mcdp) > limit)
        limit = CVPFREEMIN_FORMULA_SUB(mcdp);
    /* Adjust limit so CVPFREEMIN_FORMULA_2 doesn't become negative */
    if (mndp->mvfs_vobfreehashsize > limit)
        limit = mndp->mvfs_vobfreehashsize;
    if (*cvpfreemax_p < limit) {
	*cvpfreemax_p = limit; 
	if (!silent) {
	    mvfs_log(MFS_LOG_WARN,
		     "cleartext freelist size increased to hard minimum %ld\n",
		     *cvpfreemax_p);
	}
    }

    MVFS_SIZE_DEFLOAD_NONZERO(*cvpfreemin_p, mma_sizes, CVPFREEMIN, 
			      CVPFREEMIN_FORMULA(*cvpfreemax_p, mcdp));
    /* Don't let cvpfreemin be greater than the formula */
    if (*cvpfreemin_p > CVPFREEMIN_FORMULA(*cvpfreemax_p, mcdp)) {
	if (!silent) {
	    mvfs_log(MFS_LOG_WARN,
		     "cleartext freelist minimum size reduced from %ld to %ld"
		     " due to cvpfreemax parameter of %ld\n",
		     *cvpfreemin_p, CVPFREEMIN_FORMULA(*cvpfreemax_p, mcdp),
		     *cvpfreemax_p);
	}
        *cvpfreemin_p = CVPFREEMIN_FORMULA(*cvpfreemax_p, mcdp);
    }

    /* 
     * (cvpfreemax - cvpfreemin) must be at least mvfs_vobfreehashsize so
     * the reduction algorithm gets at least 1 cvp to flush on each hash
     * chain of the vobfree hash list.  See mvfs_mnflush_cvpfreelist. 
     */
    if ((*cvpfreemax_p - *cvpfreemin_p) < mndp->mvfs_vobfreehashsize) {
	oldmin = *cvpfreemin_p;
	*cvpfreemin_p = CVPFREEMIN_FORMULA_2(*cvpfreemax_p, 
						mndp->mvfs_vobfreehashsize);
	if (!silent) {
	    mvfs_log(MFS_LOG_WARN,
		     "cleartext freelist minimum size reduced from %ld to %ld "
		     "since cvpfreemax must be at least (cvpfreemin+mvfs_vobfreehashsize)\n",
		     oldmin, *cvpfreemin_p);
	}
    }

}

/*
 * MVFS_MNGROWTABLE - internal routine to grow mnode table.
 */
STATIC int
mvfs_mngrowtable(mnmax)
int mnmax;		/* Current table size */
{
    return mvfs_mngrowtable_to(mnmax, mnmax + (mnmax/2), KM_SLEEP);
}

STATIC int
mvfs_mngrowtable_to(
    int currentsz,		/* Current table size */
    int newsize,		/* desired size */
    int flag			/* KM_SLEEP or KM_NOSLEEP */
)
{
    mfs_mnode_t **newtab, **freetab;
    int freesize;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /* Allocate table */
    mvfs_log(MFS_LOG_INFO, "mngrowtable: growing mnode table, new size is %d\n", newsize);
    newtab = (mfs_mnode_t **)KMEM_ALLOC((sizeof(mfs_mnode_t *) * newsize), 
					 flag);
    if (newtab == NULL)
	return(0);

    MVFS_LOCK(&(mndp->mfs_mnlock));
    if (currentsz == mcdp->mvfs_mnmax) {
	ASSERT(newsize >= mcdp->mvfs_mnmax);  /* no shrinking allowed */
	/*
	 * If table has not grown while we were unlocked, copy it, we are done
	 */
	BCOPY(mndp->mnum_to_mnode, newtab, sizeof(mfs_mnode_t *) * mcdp->mvfs_mnmax);
	BZERO(newtab + mcdp->mvfs_mnmax, sizeof(mfs_mnode_t *) * (newsize - mcdp->mvfs_mnmax));
	freetab = mndp->mnum_to_mnode;
	mndp->mnum_to_mnode = newtab;
	freesize = sizeof(mfs_mnode_t *) * mcdp->mvfs_mnmax;
	/* Now set new size */
	mcdp->mvfs_mnmax = newsize;
	/* No need to update mvfs_mtmhwm or mvfs_mtmpfs */
    } else {
	/*
	 * We've been beaten to growing the table, free storage allocated.
	 */
	freetab = newtab;
	freesize = sizeof(mfs_mnode_t *) * newsize;
    }
    MVFS_UNLOCK(&(mndp->mfs_mnlock));

    KMEM_FREE(freetab, freesize);
    return(1);
}

/*
 * MFS_GETMNUMSLOT - Allocate an mnode table slot.
 */

STATIC int
mfs_mngetmnumslot(void)
{
    int mnum;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    MVFS_LOCK(&(mndp->mfs_mnlock));

    /*
     * Search table from the first slot that is potentially available.
     * MFS_MN_RSV_SLOT sets mvfs_mtmpfs to the slot after the one that is
     * reserved, and updates the high water mark if necessary.
     */
    for (mnum = mndp->mvfs_mtmpfs; mnum < mcdp->mvfs_mnmax; mnum++) {   /* Allocate mnum */
	if (mndp->mnum_to_mnode[mnum] == NULL) {
	    MFS_MN_RSV_SLOT(mndp, mnum);		/* Reserve slot */
	    MVFS_UNLOCK(&(mndp->mfs_mnlock));
	    return(mnum);
	}
    }
    /* Not found */
    MVFS_UNLOCK(&(mndp->mfs_mnlock));
    return(0);
}

/*
 * MVFS_GETMNODESIZE: determines the mnode size based on the class.
 */
STATIC int
mvfs_mngetmnodesize(class)
mfs_class_t class;
{

    int msize = 0;

    switch (class) {
	case MFS_SDEVCLAS:
	case MFS_VOBRTCLAS:
	case MFS_LOOPCLAS: 
	    msize = MNHSIZE;
	    break;
	case MFS_VIEWDIRCLAS:
	    msize = MNHSIZE + sizeof(struct mfs_ramdirnode);
	    break;
	case MFS_VIEWCLAS:
	case MFS_NTVWCLAS:
	    msize = MNHSIZE + sizeof(struct mfs_viewnode);
	    break;
	case MFS_VOBCLAS:
	    msize = MNHSIZE + sizeof(struct mfs_vobnode);
	    break;
	default:
	    MDKI_PANIC("mvfs_mngetmnodesize: illegal class");
    }

    return(msize);
}

STATIC ks_int32_t
mvfs_compute_mnmax(ks_int32_t scale_factor)
{
    /*
     * This step function has different slope before/after 5.  The constant
     * 7 is used below simply because it causes the two line segments to be
     * linked together since each function has the same value at
     * mvfs_largeinit = 5.
     */
    if (scale_factor <= 4)
	return 4096*(scale_factor+1);
    else
	return 2048*(scale_factor+7);
}

STATIC int
mvfs_compute_cvpfreemax(ks_int32_t scale_factor)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if (scale_factor > 0)
	return MVFS_DEF_CVPFREEMAX_LARGE(mcdp);
    else 
	return MVFS_DEF_CVPFREEMAX_SMALL(mcdp);
}

/*
 * mvfs_mnallocatemnode: allocates a new mnode and any other required
 * 			*nodes based on platform.  These can be either
 *			gnodes, or mnodes.
 */

STATIC mfs_mnode_t *
mvfs_mnallocatemnode(
    int msize
)
{

    mfs_mnode_t *mnp;
    VNODE_T *vp;
    /*
     * Vnode allocated separately from mnode in VNO_ALLOC system.
     * mn_hdr.vp will be filled in by makenode call
     */
    vp = NULL; 
    mnp = (mfs_mnode_t *)KMEM_ALLOC(msize, MNODE_ALLOC_FLAG);
    if (mnp) {
	BZERO(mnp, msize);
    }

    return(mnp);
}

int
mvfs_mn_compute_caches(
    ks_int32_t scale_factor,
    mvfs_cache_sizes_t *szp
)
{
    u_long freemax, freemin;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /* If the caller (user) didn't provide a proposed value for
     * an individual item, we compute that item based on a formula
     * which is an argument to this macro.
     * If the caller did provide a proposed value, take it.
     *
     * In all cases, apply clamping rules to restrict the range
     * and replace the proposed/computed values with clamped values.
     */

#define FILL_IN(cacheval, formula)                              \
    if ((szp->mask & MVFS_CACHEBIT(cacheval)) == 0) {           \
        szp->size[MVFS_SETCACHE_ ## cacheval] = formula;        \
        szp->mask |= MVFS_CACHEBIT(cacheval);                   \
    }

    FILL_IN(MNMAX, mvfs_compute_mnmax(scale_factor));
    FILL_IN(VOBFREEMAX, VOBFREEMAX_FORMULA(szp->size[MVFS_SETCACHE_MNMAX]));
    FILL_IN(VOBFREEMIN,
            VOBFREEMIN_FORMULA(szp->size[MVFS_SETCACHE_VOBFREEMAX]));

    /*
     * Alas, we can't pass the address of szp->size[x] directly
     * because of differing types of the tunables and the structure
     * members in some 64-bit kernels.  (We could fix this by changing
     * all tuning variables to 32-bit hardened types.)
     */
    freemax = szp->size[MVFS_SETCACHE_VOBFREEMAX];
    freemin = szp->size[MVFS_SETCACHE_VOBFREEMIN];

    mvfs_mn_clamp_vobfree(szp, TRUE,    /* SILENT */
                          szp->size[MVFS_SETCACHE_MNMAX],
                          &freemax,
                          &freemin);
    szp->size[MVFS_SETCACHE_VOBFREEMAX] = freemax;
    szp->size[MVFS_SETCACHE_VOBFREEMIN] = freemin;

    FILL_IN(CVPFREEMAX,
            mvfs_compute_cvpfreemax(szp->size[MVFS_SETCACHE_LARGEINIT]));
    FILL_IN(CVPFREEMIN,
            CVPFREEMIN_FORMULA(szp->size[MVFS_SETCACHE_CVPFREEMAX], mcdp));
    freemax = szp->size[MVFS_SETCACHE_CVPFREEMAX];
    freemin = szp->size[MVFS_SETCACHE_CVPFREEMIN];

    mvfs_mn_clamp_cvpfree(szp, TRUE,    /* SILENT */
                          szp->size[MVFS_SETCACHE_VOBFREEMAX],
                          &freemax, &freemin);

    szp->size[MVFS_SETCACHE_CVPFREEMAX] = freemax;
    szp->size[MVFS_SETCACHE_CVPFREEMIN] = freemin;

    FILL_IN(AGE_CVP_TIME, MVFS_DEFAULT_AGE_CVP_TIME);
    FILL_IN(VOBHASHTAB_SZ,
            mvfs_mnfind_vobhashsize(szp->size[MVFS_SETCACHE_MNMAX]));
    FILL_IN(CVPHASHTAB_SZ,
            CVPHASHSIZE_FORMULA(szp->size[MVFS_SETCACHE_VOBHASHTAB_SZ]));

#undef FILL_IN
    return(0);
}

/*
 * MVFS_MNSYNCMNODES - locate all mnodes that match the vfsp (a NULL vfsp 
 * selects all) and need to be sync'ed (VOB mnodes with cached_pages), and 
 * flush them.
 * Called from sync().  Bypasses "locked" mnodes!
 */
void
mvfs_mnsyncmnodes(
    VFS_T *vfsp
)
{

    int hash_num;		/* hash chain number */
    LOCK_T *hash_lockp;		/* pointer to the mutex lock for a given hash */
    mfs_mnode_t *hp;		
    register mfs_mnode_t *mnp;
    mfs_mnode_t **mnplist;
    int count;
    int num_alloc;
    int f_hash_val;
    LOCK_T *flplockp;
    int i;
    int increased_size = 0;	/* flag if allocated larger buffer */
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /* Only allow 1 sync at a time.  Protected by mvfs_mnlock.  */
    MVFS_LOCK(&(mndp->mfs_mnlock));

    if (mndp->mvfs_sync_in_progress == 0)
	mndp->mvfs_sync_in_progress = 1;
    else {
	MVFS_UNLOCK(&(mndp->mfs_mnlock));
	return;
    }
    MVFS_UNLOCK(&(mndp->mfs_mnlock));

    /*
     * Allocate a list of pointers to the mnodes (selected below) that need
     * to be flushed.  Since we are covered by the mvfs_sync_in_progress
     * flag, we can use a common buffer in the default case.  That is allocated
     * at inititialization time so we don't need to worry about dealing with
     * allocation failures.  The default buffer is 10 times as large 
     * as the expected average hash chain length to accomodate uneven hash 
     * chains.  If this is ever found to be too small, we will allocate a
     * larger buffer.  If memory is not available we will use the smaller 
     * buffer.
     */
    num_alloc = mndp->mvfs_rec_fls_bufsz;
    if (mndp->mvfs_mnplist_toosmall == 0) {
        mnplist = mndp->mvfs_default_mnplist;
    } else {
        mnplist = (mfs_mnode_t **)KMEM_ALLOC(num_alloc * sizeof(*mnplist), KM_SLEEP);
        if (mnplist == NULL) {
            /* if we cannot get memory for bigger list, use default */
            mnplist = mndp->mvfs_default_mnplist;
	    num_alloc = MVFS_DEFAULT_MNPLIST_SIZE;
        }
    }

    /* Run through every vob hash chain */
    for (hash_num = 0; hash_num < mndp->mvfs_vobhashsize; hash_num++) {

	/* Lock the specific vob hash chain */
	MNVOBHASH_MVFS_LOCK(mndp, hash_num, &hash_lockp);
	hp = (mfs_mnode_t *)&(mndp->mvfs_vobhash[hash_num]);

	/* 
	 * Search every entry on the hash chain.  Compile a list of mnodes
	 * that need to be flushed, and ensure they stay around until we 
	 * flush them.
	 */
	for (mnp = hp->mn_hdr.next, count = 0; 
	     mnp != hp && count < num_alloc; 
	     mnp = mnp->mn_hdr.next) 
	{

	    if (((vfsp == NULL) || (mnp->mn_hdr.vfsp == vfsp)) &&
		(mnp->mn_hdr.cached_pages) && (MFS_ISVOB(mnp)))
	    {
		/*
		 * Have found a likely mnode.  If we can't get the lock, bypass
		 * this mnode for now (it can be sync'ed next time).
		 */
		if (!MHDRLOCK_NOWAIT(mnp)) {
		    continue;
		}

		/* Skip this mnode if its already on the destroy list */
		if (mnp->mn_hdr.on_destroy) {
		    MHDRUNLOCK(mnp);
		    continue;
		}

		/*
		 * VOB mnodes may be on the freelist.  Skip this mnode if is
		 * is on the freelist but transitioning to the destroy list.
		 */
		f_hash_val = MVFS_VOBFREEHASH(mndp, mnp);
		MVFS_LOCK_SELECT(&(mndp->mvfs_vobfreehash_mlp), f_hash_val,
				 HASH_MVFS_LOCK_MAP, &flplockp);
		if (!CONDITIONAL_LOCK((flplockp))) {
		    MHDRUNLOCK(mnp);
		    continue;
		}
		if (mnp->mn_hdr.trans_destroy) {
		    MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
		    MHDRUNLOCK(mnp);
		    continue;
		}

		/* Remove this mnode from the freelist, if there. */
		if (mnp->mn_hdr.mfree) {
		    /* Update refcnt and VOB freelist */
		    MN_RMFREE(mndp, flplockp, mnp);
		}
		MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);

		/* 
		 * Try to lock the mnode, increment its use count, and unlock.
		 */
		if (!MLOCK_NOWAIT(mnp)) {
		    MHDRUNLOCK(mnp);
		    /* XXX this leaves an mnode taken off the freelist that
		     * really still needs to be sync'ed XXX what to do? 
		     * should we just wait for the header lock?  We wait for
		     * other locks. */
		    continue;
		}
		mnp->mn_hdr.mcount++;
		MHDRUNLOCK(mnp);

		/*
		 * Now that we have a locked mnode that needs to be flushed,
		 * put it in the list to flush once we have released the
		 * hash chain lock.
		 */
		mnplist[count++] = mnp;

		if (count == num_alloc) {

		    /* Can't handle any more mnodes from this hash chain in 
		     * the mplist.  Setup to double the size of the mplist 
		     * buffer for the next sync (if we haven't already done it
		     * in this call to sync).
		     */
		    mndp->mvfs_mnplist_toosmall++;
		    if (!increased_size) {
			mndp->mvfs_rec_fls_bufsz = mndp->mvfs_rec_fls_bufsz * 2;
			/* But shouldn't need to be above mvfs_mnmax */
			if (mndp->mvfs_rec_fls_bufsz > mcdp->mvfs_mnmax)
			    mndp->mvfs_rec_fls_bufsz = mcdp->mvfs_mnmax;
			increased_size = 1;
		    }

		    /*
		     * Check to see if count is already at mvfs_mnmax.
		     * This would indicate a problem.  It would mean every
		     * possible mnode (actually every possible mnode+1 since 
		     * the 0 offset in mnum_to_mnode table is not used) is on 
		     * this one hash chain, they are all dirty, and we may not 
		     * be done with the loop yet.  
		     */
		    if (count == mcdp->mvfs_mnmax) 
			mvfs_log(MFS_LOG_WARN, "sync: mvfs_mnmax mnodes found on 1 hash chain (hash chain = 0x%x\n", 
				 hash_num);
		    continue;
		}
	    }
	}

	/* And unlock the hash chain to allow better concurrency */
	MNVOBHASH_MVFS_UNLOCK(&hash_lockp);

	/*
	 * Now have all the mnodes that need to be flushed in mnplist,
	 * and each is locked.  Now we need to flush them.
	 */
	for (i = 0; i < count; i++) {
	    /* each mnode is unlocked and rele'ed. */
	    mvfs_sync_mnode(mnplist[i]);
	}

    }

    /* Free the buffer allocated to hold the mnode pointers. */
    if (mnplist != mndp->mvfs_default_mnplist)
        KMEM_FREE(mnplist, num_alloc * sizeof(*mnplist));

    /* Only allow 1 sync at a time.  Protected by mvfs_mnlock.  */
    MVFS_LOCK(&(mndp->mfs_mnlock));
    mndp->mvfs_sync_in_progress = 0;
    MVFS_UNLOCK(&(mndp->mfs_mnlock));

    return;
}

/*
 * MVFS_SYNC_MNODE: Flushes one mnode.  Must be called with a pointer to
 * a locked mnode that needs to be flushed.  Upon return, the mnode has
 * been unlocked and released.
 */

STATIC void
mvfs_sync_mnode(mfs_mnode_t *mnp)
{
    VNODE_T *vp;
    CRED_T *cred;
    int error;

    ASSERT (mnp != 0);

    /*
     * Get the vnode associated with this mnode.  MVFS_VNGET unlocks and
     * releases the mnode.
     * FIXME: needs nowait option 
     */
    error = MVFS_VNGET(mnp->mn_hdr.vfsp, NULL, mnp, &vp);
    if (error) {
	mvfs_logperr(MFS_LOG_DEBUG, error, 
		     "sync: can't sync vob %s dbid 0x%x\n", 
		     VFS_TO_MMI(mnp->mn_hdr.vfsp)->mmi_mntpath,
		     mnp->mn_hdr.fid.mf_dbid);
	ASSERT(vp == NULL);	/* MVFS_VNGET has released mnode on error */
	return;
    }
    ASSERT(vp != NULL);

    if (!MLOCK_NOWAIT(mnp)) {
	VN_RELE(vp);
	return;
    }

    /*
     * Get saved credentials.
     */
    cred = MCRED(mnp);
    if (cred == NULL) {
	mvfs_log(MFS_LOG_INFO, "sync: no cred: vw=%s vob=%s dbid=0x%x\n",
		 mfs_vp2vw(vp), mfs_vp2dev(vp), mnp->mn_hdr.fid.mf_dbid);
	MUNLOCK(mnp);
	VN_RELE(vp);
	return;		/* Skip this vnode */
    }
    MDKI_CRHOLD(cred);
    MUNLOCK(mnp);

    /* Flush MFS vnode first (with mnode unlocked).  */
    (void) PVN_FLUSH(vp, MFS_PVN_FLUSH, cred);

    /* Release hold on cred */
    MDKI_CRFREE(cred);
	 
    /* Release hold on vnode */
    VN_RELE(vp);
    return;
}

/*
 * GETNEXT routines: lock the mfs_mnlock and search linearly through
 * the mnum_to_mnode table.
 * XXX should probably all be converted to looking through the hash table
 * at some point. This also allows normal lock ordering.
 */
#define MVFS_MNGETNEXTOID_MAXRETRY 10
/*
 * MFS_MNGETNEXTOID - locate next mnode by vw, vob, oid in the mnode table.
 * Used from invaloid code for scanning the mnode table.
 */
mfs_mnode_t *
mfs_mngetnextoid(mnump, vw, voboidp, oidp)
register int *mnump;
VNODE_T *vw;
tbs_oid_t *voboidp;
tbs_oid_t *oidp;
{
    mfs_mnode_t *mnp = NULL;
    int f_hash_val;
    LOCK_T *flplockp;
    int retry_counter = 0;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    ASSERT(vw);
    ASSERT(MFS_ISVIEW(VTOM(vw)));

    retry:
    if (*mnump < 0) *mnump = 0;
    if (*mnump > mndp->mvfs_mtmhwm) return(NULL);

    MVFS_LOCK(&(mndp->mfs_mnlock));
    for (; *mnump <= mndp->mvfs_mtmhwm; (*mnump)++) {
	mnp = mndp->mnum_to_mnode[*mnump]; 
	if (mnp == (mfs_mnode_t *)MFS_MN_INTRANS) mnp = NULL;
	if (mnp && MFS_ISVOB(mnp) && 
	    !mnp->mn_hdr.stale && 
	    (mnp->mn_hdr.viewvp == vw ||
	     MFS_UUIDEQ(VTOM(mnp->mn_hdr.viewvp)->mn_view.vh.view_uuid,
			VTOM(vw)->mn_view.vh.view_uuid)) &&
	     MFS_OIDEQ(*voboidp, 
		      *(tbs_oid_t *)&(VFS_TO_MMI(mnp->mn_hdr.vfsp)->mmi_vobuuid)) &&
	    (MFS_OIDEQ(*oidp, mnp->mn_vob.attr.elem_oid) ||
	     MFS_OIDEQ(*oidp, mnp->mn_vob.attr.obj_oid))) 
	{
	    /* 
             * Found the mnode, lock its header.
             * In normal lock order, mnode header lock is acquired before 
             * mfs_mnlock, so we can't wait for mnheader lock here while 
             * holding mfs_mnlock. To prevent deadlock, we release mfs_mnlock 
             * if we can't acquire mnheader lock, and retry search from the
             * same slot. This would allow the table to change, but since we
             * are doing a linear search, only new mnodes could fill in 
             * locations in table we have passed, so they can be viewed as 
             * having occurred after this call. 
             */
	    if (!MHDRLOCK_NOWAIT(mnp)) {
                if ((++retry_counter) <= MVFS_MNGETNEXTOID_MAXRETRY) {
                    mvfs_log(MFS_LOG_INFO,
                             "mngetnextoid: mnode %d (%"KS_FMT_PTR_T") locked, retry:%d\n",
                             *mnump, mnp, retry_counter);
                    mnp = NULL;
                    MVFS_UNLOCK(&(mndp->mfs_mnlock));
                    goto retry;
                } else {
                    mvfs_log(MFS_LOG_ERR,
                             "mngetnextoid: mnode %d (%"KS_FMT_PTR_T") locked, exceeded retry limit, ignored.\n",
                             *mnump, mnp);
                    mnp = NULL;
                    continue;
                }
            }

	    /* Skip this mnode if its already on the destroy list */
	    if (mnp->mn_hdr.on_destroy) {
		MHDRUNLOCK(mnp);
		mnp = NULL;
		continue;
	    }

	    /*
	     * Skip this mnode if is on the freelist but transitioning to 
	     * the destroy list.
	     * XXX this pends too.  How bad is this?  Given that we didn't
	     * have to wait nearly as long to get the mfs_mnlock in the first
	     * place, perhaps not so bad.
	     */
	    f_hash_val = MVFS_VOBFREEHASH(mndp, mnp);
	    MNVOBFREEHASH_MVFS_LOCK(mndp, f_hash_val, &flplockp);
	    if (mnp->mn_hdr.trans_destroy) {
		MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
		MHDRUNLOCK(mnp);
		mnp = NULL;
		continue;
	    }

	    /* Remove this mnode from the freelist, if there. */
	    if (mnp->mn_hdr.mfree) {
		/* Update refcnt and VOB freelist */
		MN_RMFREE(mndp, flplockp, mnp);
		/* XXX why don't we count this as a mfs_mnstat.mnreclaim? */
	    }
	    MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);

	    MVFS_UNLOCK(&(mndp->mfs_mnlock));

	    /* Update refcnt */
	    mnp->mn_hdr.mcount++;

	    (*mnump)++;
	    MHDRUNLOCK(mnp);
	    break;
	} else
	    mnp = NULL;		/* in case of loop termination, don't
				   return it--it's not a match */
    }

    if (mnp == NULL)
	MVFS_UNLOCK(&(mndp->mfs_mnlock));

    /* Lock the mnode.  Protects callers from seeing partially
       initialized mnodes */

    if (mnp) MLOCK(mnp);	

    /* Logging would be silly here! */
    return(mnp);
}

#if defined(MVFS_DEBUG) && defined(MVOP_PRINT)
/*
 * Run through mnode list looking for mnodes active on a VFS.  They
 * should have been cleaned up during an unmount, but something
 * croaked so we want to help debug what's going wrong.
 *
 * Cribbed from mfs_mngetnextoid() and slimmed down a lot.
 *
 * XXX would it be better to run down hash chains instead?
 */
void
mvfs_mnreport_leftover_vnodes(
    VFS_T *vfsp
)
{
    mfs_mnode_t *mnp = NULL;
    int mnum = 0;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    MVFS_LOCK(&(mndp->mfs_mnlock));
    for (; mnum <= mndp->mvfs_mtmhwm; mnum++) {
	mnp = mndp->mnum_to_mnode[mnum]; 
	if (mnp == (mfs_mnode_t *)MFS_MN_INTRANS) mnp = NULL;
	if (mnp && mnp->mn_hdr.vfsp == vfsp) {
	    /* Found the mnode, lock its header. */
	    if (!MHDRLOCK_NOWAIT(mnp)) {
		mvfs_log(MFS_LOG_ERR, 
			 "mnreport_leftovers: mnode %d (%"KS_FMT_PTR_T") locked, ignored\n", 
			 mnum, mnp);
		continue;
	    }
            if (mnp->mn_hdr.vp)
                MVOP_PRINT(mnp->mn_hdr.vp);
            else
                mvfs_log(MFS_LOG_ERR,
                         "mnreport_leftovers: vfsp=%"KS_FMT_PTR_T" "
                         "mnp %"KS_FMT_PTR_T" on freelist\n",
                         vfsp, mnp);
	    MHDRUNLOCK(mnp);
	}
    }
    MVFS_UNLOCK(&(mndp->mfs_mnlock));
}
#endif

/*
 * MVFS_MNGETNEXTVIEW - iterate over view mnodes (by number) in the mnode
 * table. 
 */
mfs_mnode_t *
mvfs_mngetnextview(register int *mnump)
{
    mfs_mnode_t *mnp = NULL;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    if (*mnump < 0) *mnump = 0;
    if (*mnump > mndp->mvfs_mtmhwm) return(NULL);

    MVFS_LOCK(&(mndp->mfs_mnlock));
    for (; *mnump <= mndp->mvfs_mtmhwm; (*mnump)++) {
	mnp = mndp->mnum_to_mnode[*mnump]; 
	if (mnp == (mfs_mnode_t *)MFS_MN_INTRANS) mnp = NULL;
	if (mnp) {
	    if (!MFS_ISVIEW(mnp)) {
		mnp = NULL;
		continue;
	    }

	    /* Found the mnode, lock its header */
	    if (!MHDRLOCK_NOWAIT(mnp)) {
		mnp = NULL;
		continue;
	    }

	    /* Skip this mnode if its already on the destroy list */
	    if (mnp->mn_hdr.on_destroy) {
		MHDRUNLOCK(mnp);
		mnp = NULL;
		continue;
	    }

	    /* 
	     * Try to lock the mnode.  This is OK with the mnlock
	     * held because it is a conditional lock and does not wait.
	     * If we can't get the lock, bypass this mnode for now.
	     * (If the user really needs to reconnect the view for some
	     * stranded process, they can exit all the other processes in the
	     * view, end the new version of the view, and try setview again
	     * to come here again.)
	     */
	    if (!MLOCK_NOWAIT(mnp)) {
		MHDRUNLOCK(mnp);
		mnp = NULL;
		continue;
	    }
	    (*mnump)++;

	    MVFS_UNLOCK(&(mndp->mfs_mnlock));

	    mnp->mn_hdr.mcount++;

	    MHDRUNLOCK(mnp);
	    break;
	}
    }

    if (mnp == NULL)
	MVFS_UNLOCK(&(mndp->mfs_mnlock));

    return(mnp);
}

void
mvfs_mnclear_logbits(void)
{
    /* Run through active mnodes, resetting log state bits.  It's more
     * cumbersome to have a caller iterate over this (as is done for
     * mfs_mngetnext()), since we only want to lock mnodes briefly (to
     * prevent other changes such as inactivation) while editing bits.
     */
    mfs_mnode_t *mnp = NULL;
    int mnum;
    int f_hash_val;
    LOCK_T *flplockp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    MDB_XLOG((MDB_CLEAROPS,"clearing log bits\n"));
    MVFS_LOCK(&(mndp->mfs_mnlock));
    for (mnum = 0; mnum < mndp->mvfs_mtmhwm; mnum++) {
	mnp = mndp->mnum_to_mnode[mnum]; 
	if (mnp == (mfs_mnode_t *)MFS_MN_INTRANS) mnp = NULL;
	if (mnp) {
	    /* we only care about vob nodes */
	    if (!MFS_ISVOB(mnp))
		continue;

	    /* Try to lock the mnode header, but don't wait.
	     * If we can't get the lock, bypass this mnode.
	     * (Let its log messages still be suppressed--the user can
	     * tweak log state again to try and reset the bits with
	     * another run through here.)
	     */
	    if (!MHDRLOCK_NOWAIT(mnp)) {
		MDB_XLOG((MDB_CLEAROPS,"mnode %d (%"KS_FMT_PTR_T") locked, ignored\n", 
			  mnum, mnp));
		continue;
	    }

	    /* Skip this mnode if its already on the destroy list */
	    if (mnp->mn_hdr.on_destroy) {
		MHDRUNLOCK(mnp);
		continue;
	    }

	    /*
	     * Skip this mnode if is on the freelist but transitioning to 
	     * the destroy list.
	     * XXX again this pends.
	     */
	    f_hash_val = MVFS_VOBFREEHASH(mndp, mnp);
	    MNVOBFREEHASH_MVFS_LOCK(mndp, f_hash_val, &flplockp);
	    if (mnp->mn_hdr.trans_destroy) {
		MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
		MHDRUNLOCK(mnp);
		continue;
	    }

	    MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);

	    /* Try to lock the mnode.  This is OK with the mnlock
	     * held because it is a conditional lock and does not wait.
	     */
	    if (!MLOCK_NOWAIT(mnp)) {
		MDB_XLOG((MDB_CLEAROPS,"mnode %d (%"KS_FMT_PTR_T") locked, ignored\n",
			  mnum, mnp));
		MHDRUNLOCK(mnp);
		continue;
	    }

	    MHDRUNLOCK(mnp);

	    /* Found an mnode to clear bits in. */
	    mnp->mn_vob.cleartext.ostale_logged = 0;
	    MUNLOCK(mnp);
	}
    }
    MVFS_UNLOCK(&(mndp->mfs_mnlock));

    return;
}

/*
 * FLUSH routines.
 */

/*
 * MFS_MNFLUSH - flush all free mnodes
 */

void
mfs_mnflush(void)
{
    int hash_val;
    LOCK_T *flplockp;
    mfs_mnode_t *hp;
    register mfs_mnode_t *mnp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    MVFS_FLUSH_MAPPINGS(NULL);		/* toss any pages incore on all VOBs */

    /* 
     * Go through each vob freelist hash chain and release all the VOB mnodes 
     * on the freelist 
     */
    for (hash_val = 0; hash_val < mndp->mvfs_vobfreehashsize; hash_val++) {
	MNVOBFREEHASH_MVFS_LOCK(mndp, hash_val, &flplockp);
	hp = (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[hash_val]);
	while ((mnp = hp->mn_hdr.free_next) != hp) {

	    /* Set transition to destroy flag (protected by freelist lock) */
	    mnp->mn_hdr.trans_destroy = 1;

	    MN_RMFREE(mndp, flplockp, mnp);

	    /*
	     * Unlock the appropriate vobfree hash lock prior to locking the 
	     * mvfs_mndestroylock to allow for greater concurrency.  
	     * Allows the vobfree list to change while it is being flushed, 
	     * but this is ok; even with the old, large-grained locking scheme 
	     * (v5.0 and before), the vobfree list could change while we were 
	     * flushing.
	     */
	    MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);

	    MN_INSDESTROY(mndp, mnp);

	    /* And re-obtain the mvfs_vobfree hash lock for the loop. */
	    MNVOBFREEHASH_MVFS_LOCK(mndp, hash_val, &flplockp);
	}
	/* Unlock this hash chain. */
	MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
    }

    mvfs_mndestroy_list();

}

/*
 * MFS_MNFLUSHVFS - flush all free mnodes for a VFS
 */

void
mfs_mnflushvfs(vfsp)
VFS_T *vfsp;
{
    int hash_val;
    LOCK_T *flplockp;
    mfs_mnode_t *hp;
    register mfs_mnode_t *mnp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    MVFS_FLUSH_MAPPINGS(vfsp);		/* toss any pages incore */

    BUMPSTAT(mfs_mnstat.mnflushvfscnt);

    /*
     * Go through each vob freelist hash chain and release all the VOB mnodes
     * that apply to this vfsp.
     */
    for (hash_val = 0; hash_val < mndp->mvfs_vobfreehashsize; hash_val++) {
	MNVOBFREEHASH_MVFS_LOCK(mndp, hash_val, &flplockp);
	hp = (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[hash_val]);
	mnp = hp->mn_hdr.free_next;

	while (mnp != hp) {

	    if (mnp->mn_hdr.vfsp == vfsp) {

		/* Set transition to destroy flag (protected by freelist lock)*/
		mnp->mn_hdr.trans_destroy = 1;

		MN_RMFREE(mndp, flplockp, mnp);

		/*
		 * Really want to unlock mvfs_vobfree hash lock prior 
		 * to locking mvfs_mndestroylock to allow for greater 
		 * concurrency.  This will allow the mvfs_vobfree hash
		 * list to change, so we will need to go back to the 
		 * front to process the next entry since the current
		 * next mnp could be reclaimed while the lock is not 
		 * held.
		 * Still necessary with hashed locks?  Probably XXX
		 */
		MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
		MN_INSDESTROY(mndp, mnp);
		MNVOBFREEHASH_MVFS_LOCK(mndp, hash_val, &flplockp);

		/* Process the first entry on the freelist. */
		mnp = hp->mn_hdr.free_next;

	    } else {
		/* vobfree hash list hasn't been unlocked, so process next 
		 * entry. */
		mnp = mnp->mn_hdr.free_next;
	    }
	}
	/* Unlock this hash chain. */
	MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
    }

    mvfs_mndestroy_list();
}

/*
 * MFS_MNFLUSHVIEW - flush all free mnodes for a view
 */

void
mfs_mnflushvw(vw)
VNODE_T *vw;
{
    int hash_val;
    LOCK_T *flplockp;
    mfs_mnode_t *hp;
    register mfs_mnode_t *mnp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    BUMPSTAT(mfs_mnstat.mnflushvwcnt);

    MVFS_FLUSH_MAPPINGS_VW(vw);

   /*
     * Go through each vob freelist hash chain and release all the VOB mnodes
     * that apply to this vw.
     */
    for (hash_val = 0; hash_val < mndp->mvfs_vobfreehashsize; hash_val++) {
	MNVOBFREEHASH_MVFS_LOCK(mndp, hash_val, &flplockp);
	hp = (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[hash_val]);
	mnp = hp->mn_hdr.free_next;

	while (mnp != hp) {

	    if (mnp->mn_hdr.viewvp == vw) {

		/* Set transition to destroy flag (protected by freelist lock)*/
		mnp->mn_hdr.trans_destroy = 1;

		MN_RMFREE(mndp, flplockp, mnp);

		/*
		 * Really want to unlock mvfs_vobfree hash lock prior 
		 * to locking mvfs_mndestroylock to allow for greater 
		 * concurrency.  This will allow the mvfs_vobfree hash
		 * list to change, so we will need to go back to the 
		 * front to process the next entry since the current
		 * next mnp could be reclaimed while the lock is not 
		 * held.
		 * Still necessary with hashed locks?  Probably XXX
		 */
		MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
		MN_INSDESTROY(mndp, mnp);
		MNVOBFREEHASH_MVFS_LOCK(mndp, hash_val, &flplockp);

		/* Process the first entry on the freelist. */
		mnp = hp->mn_hdr.free_next;

	    } else {
		/* vobfree hash list hasn't been unlocked, so process next 
		 * entry. */
		mnp = mnp->mn_hdr.free_next;
	    }
	}

	/* Unlock this hash chain. */
	MNVOBFREEHASH_MVFS_UNLOCK(&flplockp);
    }

    mvfs_mndestroy_list();

    /* Release all DNC entries that apply to this vw */
    mfs_dnc_flushvw(vw);

}

/*
 * MFS_MLOCK2 - lock 2 mnodes in a deadlock-free manner.  Used
 *	in routines where parent/child relationship of mnodes in
 *	dir tree is not known for lock ordering i.e. rename!
 *      Call with neither mnode locked.
 */

void
mfs_mlock2(mnp1, mnp2)
register mfs_mnode_t *mnp1;
register mfs_mnode_t *mnp2;
{
    ASSERT(mnp1 != mnp2);
    for (;;) {
	MLOCK(mnp1);
	if (MLOCK_NOWAIT(mnp2)) break;
	MUNLOCK(mnp1); 
	MLOCK(mnp2);
	if (MLOCK_NOWAIT(mnp1)) break;
	MUNLOCK(mnp2);
    }
}

/* Verify the freelist.  */
STATIC int
mvfs_mnverify_freelist(void)
{
    int hash_num;		/* hash chain number */
    LOCK_T *hash_lockp;		/* pointer to the mutex lock for a given hash */
    mfs_mnode_t *hp;		
    int count = 0;
    int cnt;
    register mfs_mnode_t *mnp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    /* Run through every vobfree hash chain */
    for (hash_num = 0; hash_num < mndp->mvfs_vobfreehashsize; hash_num++) {

	/* Lock the specific vob hash chain */
	MNVOBFREEHASH_MVFS_LOCK(mndp, hash_num, &hash_lockp);
	hp = (mfs_mnode_t *)&(mndp->mvfs_vobfreehash[hash_num]);

	/* Run down the chain and check it */
	for (mnp = hp->mn_hdr.free_next, cnt = 1; 
	     mnp != hp; 
	     mnp = mnp->mn_hdr.free_next) 
	{
	    cnt++;
	    ASSERT(mnp->mn_hdr.on_destroy == 0);
	    ASSERT(mnp->mn_hdr.mfree == 1);
	    ASSERT(mnp->mn_hdr.mcount == 0);
	    ASSERT(mnp->mn_hdr.mclass == MFS_VOBCLAS);
	}
	MNVOBFREEHASH_MVFS_UNLOCK(&hash_lockp);
	count = count + cnt;
    }

    return (count);
}

/*
 * Verify the destroy list.  This is called with only the mnode header locked
 * for an mnode we are about to add to the destroy list. 
 */
STATIC int
mvfs_mnverify_destroy(void)
{
    int cnt;
    register mfs_mnode_t *dmnp;
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

	MVFS_LOCK(&(mndp->mvfs_mndestroylock));

	for (cnt = 0, dmnp = mndp->mvfs_mndestroylist.mn_hdr.free_next;
	     ((dmnp != (mfs_mnode_t *)&(mndp->mvfs_mndestroylist)) && 
	     (cnt <= mndp->mfs_mncnt));
	     dmnp = dmnp->mn_hdr.free_next, cnt++) 
	{
		ASSERT(dmnp->mn_hdr.on_destroy == 1);
		ASSERT(dmnp->mn_hdr.mfree == 0);
		ASSERT(dmnp->mn_hdr.mcount == 0);
	}
	ASSERT(cnt <= mndp->mfs_mncnt);
	ASSERT(cnt == mndp->mvfs_mndestroycnt);

	MVFS_UNLOCK(&(mndp->mvfs_mndestroylock));

	return (cnt);
}
static const char vnode_verid_mvfs_mnode_c[] = "$Id:  c020e85c.737211e1.90e6.00:01:83:0a:3b:75 $";
