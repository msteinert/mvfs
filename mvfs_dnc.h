/* * (C) Copyright IBM Corporation 1991, 2006. */
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

#ifndef MFS_DNC_H_
#define MFS_DNC_H_

#include "mvfs_base.h"

/*
 * Define some constants.  The max namelen is actually the
 * max namelen cached in the entry itself.  It is short to
 * conserve storage.  Longer names require allocating a string
 * for the name cache entry.o
 * The "ENOENT VP" is a return value from lookup that indicates
 * a cached "name not found" entry was hit.
 */
#define MFS_DNMAXSHORTNAME	24
#define MFS_DNCBHMAX		3  /* Match MFS_IONCBHMAX in sys/mfs_ioctl.h */
#define MFS_DNC_ENOENTVP	((VNODE_T *)-1L)

/*
 * Define flags for name cache entries (dncadd call and dnc struct)
 */
#define MFS_DNC_BHINVARIANT		0x0001 /* Name lookup build-handle invariant */
#define MFS_DNC_NOTINDIR		0x0002 /* ENOENT because name does not exist in the dir */
#define MFS_DNC_CASE_INSENSITIVE	0x0004 /* Result valid for case-insensitive lookup of name (only) */
#define MVFS_DNC_RVC_ENT		0x0008 /* RVC entry */

/*
 * Cache entry structure.
 */

struct mfs_dncent {
	struct mfs_dncent *next;	/* Next on hash chain */
	struct mfs_dncent *prev;	/* Prev on hash chain */
	struct mfs_dncent *lrunext;	/* LRU list next */
	struct mfs_dncent *lruprev;	/* LRU list prev */
	u_long		   addtime;	/* Last add/change time (secs) */
	struct mfs_dncent *lruhead;	/* Which LRU list DNC is on */
	u_int		   in_trans : 1; /* dnc entry in transition - ignore */
        u_int              nullbh : 1;  /* dnc entry valid for null bh */
        u_int              invalid : 1; /* Marks invalidated entries */
	u_int              mbz : 5;	/* Unused flag bits */
        u_char		   bhx;		/* Build handle list index */
        u_short            flags;       /* Name cache entry flags */
        VNODE_T	          *dvw;		/* Dir view */
	VFS_T		  *vfsp;	/* VFS ptr */
	mfs_fid_t	   dfid;	/* Dir FID */
        u_long		   dncgen;	/* Dir mnode generation */
	view_bhandle_t	   bh[MFS_DNCBHMAX];	/* Non-null build handle list */
	int		   len;		/* Name length */
        char              *nm_p;        /* Ptr to the name */
	char	           nm_inline[MFS_DNMAXSHORTNAME];	/* Stg for short names*/
	VNODE_T 	  *vvw;		/* Vnode result view */
	mfs_fid_t	   vfid;	/* Vnode result FID */
	struct timeval     vevtime;	/* Vob event time of vp when added */
	CRED_T      *cred;	/* Credentials */
        int		   dnc_hash;        /* Name hash value, use as splock index */
};

typedef struct mfs_dncent mfs_dncent_t;

/*
 * Hash table and LRU lists.  Field names must match the dncent field names!
 * Also, the LRU list doubles as a "free list" of entries to use.
 * Dirs and reg files (really non-dir objects) are separated out in
 * the lru lists to avoid flushing dir names cached during a ls -F
 */

typedef struct mfs_dnchash_slot {
    mfs_dncent_t *next;
    mfs_dncent_t *prev;
} mfs_dnchash_slot_t;

typedef struct mfs_dnclru {
    mfs_dncent_t *next;
    mfs_dncent_t *prev;
    mfs_dncent_t *lrunext;
    mfs_dncent_t *lruprev;
    SPLOCK_T     *lruspl;
} mfs_dnclru_t;

/* DNC hash table locking. On most platforms, we use a pool of spinlocks 
 * for the hash chains, for improved granularity. 
 */
/* Pool of spinlocks.  Some pool parameters are controlled in mdep files */
#define NC_HASH_LOCK_T  splock_pool_t

#define NC_HASH_LOCK(hash_val, lockpp, st, _ncdp) { \
        if (hash_val != -1) {  \
            SPLOCK_SELECT(&(_ncdp->mvfs_dnc_hash_lock), hash_val, HASH_SPLOCK_MAP, lockpp); \
            SPLOCK(**(lockpp), st);  \
        } else {  \
            *lockpp = NULL;  \
        } }

#define NC_HASH_UNLOCK(lockp, st, _ncdp) { \
        if (lockp != NULL) SPUNLOCK(*(lockp), st); }
 
#define NC_HASH_LOCK_INIT(_ncdp)   { \
        int hash_poolsize; \
        HASH_SPLOCK_SET_POOLSIZE(hash_poolsize, _ncdp->mvfs_dnchashsize); \
        if (mvfs_splock_pool_init(&(_ncdp->mvfs_dnc_hash_lock), hash_poolsize, NULL, "mvfs_dnc_hash_spl") != 0) \
            MDKI_PANIC("mfs_dncinit: no memory (hash locks)"); \
        }

#define NC_HASH_LOCK_FREE(lockp) mvfs_splock_pool_free(lockp)

typedef struct mvfs_dnlc_data
{
    int mvfs_dnchashsize;
    mfs_dnchash_slot_t *mfs_dnchash;
    mfs_dnclru_t mfs_dncdirlru;
    mfs_dnclru_t mfs_dncreglru;
    mfs_dnclru_t mfs_dncnoentlru;

    /* All platforms use some type of r/w lock for high level cache arbitration
     * and a spin lock type for the LRU.  Hash locking may be different on
     * different platforms, see mvfs_dnc.h and the mdep include files.
     */
    MVFS_RW_LOCK_T mvfs_dnc_rwlock;
    SPLOCK_T mvfs_dnc_dirlru_lock;
    SPLOCK_T mvfs_dnc_reglru_lock;
    SPLOCK_T mvfs_dnc_noentlru_lock;
    NC_HASH_LOCK_T mvfs_dnc_hash_lock;
    int mfs_dncmax;
    int mvfs_old_dncmax;
    int mfs_dnc_enoent_start;
    struct mfs_dncent *mfs_dnc;
    struct mfs_dncent *mvfs_old_dnc;
    int mvfs_dnc_nintransit;
    tbs_boolean_t mvfs_dnc_initialzed;

    /* Count a rare variety of noent entry, to avoid searches for them
     * when none exist.
     * Protected by LRU spinlock, since incr/decr happens when adding/
     * removing from cache, and we will already be using LRU lock there
     */
    int mvfs_dnc_noent_other;
} mvfs_dnlc_data_t;

/*
 * Do prototypes
 */

/**************************************************************************
 * MFS_DNCINIT - initialize the name cache.
 *
 * The parameters for name-cache initialization are taken from the
 * function argument, if set, and from the global sysgen parameters otherwise:
 *      mvfs_largeinit  -   Initialize with large caches
 *                          At least 400/1600/1600 for dir/reg/noent max
 *      mvfs_dncdirmax  -   Number of entries for dir results
 *      mvfs_dncregmax  -   Number of entries for reg file results
 *      mvfs_dncnoentmax -  Number of entries for enoent results
 *
 */
EXTERN int
mvfs_dncinit(mvfs_cache_sizes_t *mma_sizes);

/**************************************************************************
 * MFS_DNCADD - add an entry to the DNC cache
 *
 * IN dvp       Directory vnode ptr
 * IN dnc_flags DNC flags
 *              MFS_DNC_BH_INVARIANT - name lookup result is bh invariant
 *              MFS_DNC_NOTINDIR - name not found entry is because the
 *                  name did not exist in the dir (vp must be NULL)
 *              MFS_DNC_CASE_INSENSITIVE - name lookup translation is
 *                  for a case-insensitive lookup. (Note: nm must
 *                  be the case-correct name for the translation if
 *                  vp is not NULL)
 * IN nm        Leaf name of component to cache translation for
 * IN vp        Vnode ptr of result (if name found) or NULL if the name
 *              lookup should return ENOENT (name not found cache)
 * IN cred      Credentials ptr used in lookup
 * Result:      None
 *
 * Description:
 *  This routine adds a new name cache entry if none exists, or updates
 *  and existing name cache entry with the results passed in if a match
 *  is found, but the existing result is different.  In all cases, at
 *  the completion of this call, a valid name cache entry should be in
 *  the cache.
 *
 *  As of 05/01/95 the only exception to this is a case-insensitive
 *  add of a vnode with a non-null result.  This is currently a noop
 *  since it is not yet supported in the name cache.
 */

EXTERN void 
mfs_dncadd(
    VNODE_T *dvp,
    u_int dnc_flags,
    char *nm,
    VNODE_T *vp,
    CRED_T *cred
);

/**************************************************************************
 * MFS_DNC_ADD_FLAG - add a flag bits to a dnc entry
 *
 * IN dvp       Ptr to dir vnode of entry to update
 * IN nm        Ptr to leaf name of entry to update (case-correct)
 * IN dtm_p     Ptr to DTM of dir from before started case-insensitive
 *              heuristics. (optional)
 * IN dnc_flags DNC flags to add to entry
 *
 * Description:
 *  This routine updates an existing dnc cache entry (presumably from
 *  a previously successful lookup) by adding (anding in) the specified
 *  flags.  Normally, adding state to the name cache requires that
 *  the dir mnode remain locked between the lookup and the addition
 *  of information to the name cache for consistency.  To allow adding
 *  flags (like MFS_DNC_CASE_INSENSITIVE) that are the result of
 *  algorithms that don't keep the dir mnode locked, the 'mtime' of
 *  the dir at the start of the algorithm may be passed in, and this
 *  call is only effective if the dir has not changed (no names
 *  created or destroyed in the dir), thus making the add of
 *  the flag consistent.
 *
 * Side effects:
 *  This call locks the dir mnode.
 *  This call is a noop for any object type other than VOB vnodes.
 *
 */

EXTERN void
mfs_dnc_add_flag(
        VNODE_T *dvp,
        mfs_pn_char_t *nm,
        struct timeval *dtm_p,
        u_int dnc_flags,
        CRED_T *cred
);

/**************************************************************************
 * MFS_DNCLOOKUP - lookup a name translation in the name cache
 *
 * IN dvp       Ptr to dir vnode lookup is for
 * IN nm        Ptr to leaf name to lookup in cache
 * IN OUT pnp   Ptr to 'pathname struct' from VOP_LOOKUP call.
 *              This struct is a no-op on Unix, but is used on NT
 *              to pass in the case-insensitive lookup flag, and to
 *              return the case-correct component for a successful
 *              lookup.  (This struct is used because it passes 
 *              through the VOP_LOOKUP call in the Unix SVR4-style 
 *              MVFS core)
 * IN cred      Ptr to credentials lookup is being done with
 * Result:      NULL means no cached translation was found
 *              MFS_DNC_ENOENTVP means the cached result was name-not-found
 *              Otherwise a vnode ptr to the result of the lookup is returned
 *
 */

EXTERN VNODE_T *
mfs_dnclookup(
    VNODE_T *dvp,
    char *nm,
    struct pathname *pnp,
    CRED_T *cred
);
/***************************************************************************
 * MFS_DNCREMOVE_ONE - remove one translation from the cache
 *
 * IN dvp       Directory vnode ptr
 * IN nm        Leaf name to remove from cache
 * IN cred      Credentials ptr
 *
 * Description:
 *  This routine removes and releases the resources for the single
 *  case-sensitive name passed in as "nm".  All case-insensitive
 *  synonyms for this nm are also invalidated, since they may
 *  no longer be correct if reason for the remove is because the
 *  object is being deleted, or its version is changing.
 *
 * RESULT:              non-zero if the name was found and removed (along
 *                      with a corresponding RVC entry, if applicable)
 */
EXTERN int
mfs_dncremove_one(
    VNODE_T *dvp,
    char *nm,
    CRED_T *cred
);

/***************************************************************************
 * MFS_DNCREMOVE - remove both the natural name and history mode name
 *                 from the name cache
 * 
 * IN dvp       Directory vnode ptr
 * IN nm        Ptr to leaf name to remove
 * IN cred      Ptr to credentials
 *
 * Description:
 *  This routine removes both the natural and history mode name
 *  from the name cache, regardless of whether the natural or
 *  history mode name is presented.  i.e.
 *  If nm is "foo.c" both "foo.c" and "foo.c@@" entries for the dir
 *  are invalidated.  If nm is "foo.c@@" then both "foo.c@@" and "foo.c"
 *  are invalidated.  This routine is used when the element may
 *  be changing, so that both the name and its history mode counterpart
 *  may change.
 *
 *  Like mfs_dncremove_one, this routine invalidates any case-insensitive
 *  synonyms for both foo.c and foo.c@@ (name and history mode form of name).
 *
 * RESULT:              non-zero if the regular name was found and removed
 *                      (along with a corresponding RVC entry, if applicable) 
 */

EXTERN int
mfs_dncremove(
    VNODE_T *dvp,
    char *nm,
    CRED_T *cred
);

/**************************************************************************
 * MFS_DNCFLUSH - flush name cache
 *
 * Description:
 *      This routine flushes and releases all resources held by
 *      the name cache entries.  It is intended to be used only
 *      when the name cache must be completely cleaned out
 *      (i.e. for an unmount etc.)
 */
EXTERN void 
mfs_dncflush(void);

/**************************************************************************
 * MFS_DNC_FLUSHVW(VNODE_T *vw);
 *
 * IN vw    Ptr to view-tag vnode.  This may be either a loop-back view-tag
 *          or an NT style view-tag.
 *
 * Description:
 *      This routine flushes and releases all resources held
 *      in the name cache for the view vnode.  It is intended to
 *      be used only when the name cache must be completely cleaned
 *      out for the selected view (i.e. for cleaning up history 
 *      mode views.)  Any entry which has either the dir or target
 *      result in the specified view is flushed.
 */

EXTERN void 
mfs_dnc_flushvw(VNODE_T *vw);

/**************************************************************************
 * MVFS_DNC_FLUSHVFS(VFS_T *vfsp);
 *
 * IN vfsp    Ptr to VOB vfs structure.
 *
 * Description:
 *      This routine flushes and releases all resources held
 *      in the name cache for the specified VOB handle. It is intended to
 *      be used only when the name cache must be completely cleaned
 *      out for the selected VOB (i.e. when unmounting a VOB) 
 */

EXTERN void 
mvfs_dnc_flushvfs(VFS_T *vfsp);

/**************************************************************************
 * MFS_DNC_INVALVP - invalidate name cache entries for an object
 *
 * IN vp    Vnode ptr of object to invalidate name cache entries for
 *
 * Description:
 *      This routine invalidates name cache entries.  It leaves
 *      the resources held (until name cache reuse), so that we
 *      can detect and count name cache misses due to invalidated
 *      entries.  Name cache entries for which this vnode is
 *      either the dir or the result of the name cache entry are 
 *      flushed.
 */
EXTERN void
mfs_dnc_invalvp(VNODE_T *vp);

/************************************************************************
 * MFS_DNC_INVAL_OBJ_NOT_FOUND - invalidate object not found name cache entries
 *
 * Description:
 *      This routine invalidates all the name cache entries that return
 *      name not found if the cause of that status was due to the name
 *      being in the directory, but the config spec currently said
 *      that the object should not be found.  Since we don't have the
 *      target oid for the name not found, we must invalidate all of 
 *      these entries any time any object is invalidated.  This routine
 *      does not invalidate name-not-found entries that are the
 *      result of the name not existing in the directory.  Only
 *      an invalidation of the dir can invalidate those entries.
 */
EXTERN void 
mfs_dnc_inval_obj_not_found(void);

/***************************************************************************
 * MFS_DNC_INVALVW - invalidate all name cache entries for a view
 *
 * IN vw        Ptr to view-tag vnode to invalidate.  This may be either
 *              a loop-back view or an NT-style view-tag.
 * Description:
 *      This routine invalidates all of the name cache entries for
 *      the view-vnode argument.
 */

EXTERN void 
mfs_dnc_invalvw(VNODE_T *vw);

/***************************************************************************
 * MFS_DNC_GETENT - get a name-cache entry for the ioctl that
 *                  reads the name cache entries
 * IN OUT ncp       Ptr to (kernel copy) of ioctl name cache entry
 *                  structure.  This has the cookie of the next
 *                  entry to get, and is used to both return data,
 *                  and for the ptrs to user-space buffers for names et al.
 */

EXTERN int 
mfs_dnc_getent(struct mfs_ioncent *ncp);

/***************************************************************************
 * MVFS_DNCFREE - clean up and release all resources used by the name cache
 */

EXTERN void 
mvfs_dncfree(void);

/***************************************************************************
 * MVFS_DNC_COUNT - count up current DNC usage
 */

EXTERN void 
mvfs_dnc_count(mvfs_cache_usage_t *curusage);

/***************************************************************************
 * MVFS_DNC_SETCACHES - change the sizes of the DNC
 * IN szp		ptr to new cache sizes structure
 */
EXTERN int 
mvfs_dnc_setcaches(mvfs_cache_sizes_t *szp);

/***************************************************************************
 * MVFS_DNC_GETCACHES - fetch the sizes of the DNC
 * IN szp		ptr to cache sizes structure
 */
EXTERN int
mvfs_dnc_getcaches(mvfs_cache_sizes_t *szp);

/***************************************************************************
 * MVFS_DNC_COMPUTE_CACHES - compute what the sizes of the DNC would be
 * IN  scale_factor	scaling factor on which to base computations
 * OUT szp		ptr to cache sizes structure
 */
EXTERN int
mvfs_dnc_compute_caches(
    ks_int32_t scale_factor,
    mvfs_cache_sizes_t *szp
);

/***************************************************************************
 * MVFS_RVCLOOKUP - look in cache for VOB root in specified view
 *
 * IN vw        Ptr to view-tag vnode of the view in question.  This may be
 *		either a loop-back view or an NT-style view-tag.
 * IN vobrtvp	Ptr to vnode of bare VOB root.
 * OUT fidp	Ptr to fid to be filled in with bound VOB root node's fid
 * IN cred	Credentials
 *
 * On success, returns 0 and fills in *fidp.
 * If no VOB root is found in the cache, returns an error code.
 *
 */

EXTERN int 
mvfs_rvclookup(
    VNODE_T *vw,
    VNODE_T *vobrtvp,
    mfs_fid_t *fidp,
    CRED_T *cred
);

/***************************************************************************
 * MVFS_RVCENTER - create cache entry for VOB root in specified view
 *
 * IN vw        Ptr to view-tag vnode of the view in question.  This may be
 *		either a loop-back view or an NT-style view-tag.
 * IN vobrtvp	Ptr to vnode of bound VOB root.
 * IN fidp	Ptr to fid of bound VOB root node
 * IN cred	Credentials
 *
 * On success, returns 0.
 */

EXTERN int 
mvfs_rvcenter(
    VNODE_T *vw,
    VNODE_T *vobrtvp,
    mfs_fid_t *fidp,
    CRED_T *cred
);

#endif /* MFS_DNC_H_ */
/* $Id: 96d36d8c.66b911dc.9bbb.00:01:83:09:5e:0d $ */
