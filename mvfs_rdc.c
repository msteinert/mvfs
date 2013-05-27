/* * (C) Copyright IBM Corporation 2001, 2009. */
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
/* mvfs_rdc.c */

#include "mvfs_systm.h"
#include "mvfs.h"

/*
 * Prototypes for internal routines
 */

STATIC void
mvfs_rddir_cache_empty(struct mfs_mnode *mnp);

/*
 * READDIR CACHE:
 *
 * The readdir cache attempts to hold the results of readdirs on VOB
 * directories.  It is implemented by keeping blocks of entries (as
 * returned by the VIEW_READDIR RPC) tagged with their offset, size,
 * and end-of-file indication.  When a VOP_READDIR() is called for a
 * VOB directory, the cache is checked first for a block of sufficient
 * size and matching offset.  If found, the block is returned and
 * UIOMOVEd to the calling process.
 *
 * The cache is implemented per mnode, with up to (mvfs_rddir_blocks)
 * saved per mnode.  The per-mnode size can be tuned, if zero it
 * defaults to 2*(largeinit+1).  [Note: There is no global size limit
 * on total rddir cache consumption, because there is no global limit
 * on the number of mnodes in a system and the readdir cache is per
 * mnode.]
 *
 * There is no way to flush the readdir caches directly--they are
 * flushed when an mnode is destroyed or the attributes are changed.
 * Since the attributes are checked regularly to update in case of
 * another client changing a directory and the attributes are updated
 * whenever this client changes a directory, this ensures the readdir
 * cache is no more stale than the directory attributes.
 *
 * The code path for cache flushing after RPCs which successfully
 * modify directories is:
 * mfs_clnt_xxx()->mfs_attrcache()->mfs_ac_modevents()->mvfs_rddir_cache_flush()
 * [We do not flush the cache if the attempted modification of the
 * directory failed, because mfs_attrcache() is not called.]
 * 
 * The cache slots are filled up as VOP_READDIR() calls are made.  When
 * full, the replacement strategy is to replace the last entry.  The
 * code assumes that directory reads are (1) sequential, so replacing
 * only the last entry will avoid thrashing, and (2) usually the same
 * size blocks are requested, because a C library function is used to
 * invoke the system call with standard sizes.
 */

STATIC void
mvfs_rddir_cache_empty(struct mfs_mnode *mnp)
{
    register int i;
    struct mvfs_rce *ep;

    for (i = 0, ep = &mnp->mn_vob.rddir_cache->entries[0];
	 i < mnp->mn_vob.rddir_cache->nentries;
	 i++, ep++)
    {
	if (ep->valid) {
	    if (ep->block != NULL)
		KMEM_FREE(ep->block, ep->bsize);

	    ep->valid = FALSE;
	    ep->block = NULL;
	}
    }
}

void
mvfs_rddir_cache_flush(struct mfs_mnode *mnp)
{
    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));

    MDB_XLOG((MDB_MNOPS, "rddir cache flush mnp %lx\n", mnp));
    if (mnp->mn_vob.rddir_cache) {
	mvfs_rddir_cache_empty(mnp);
    }
}

void
mvfs_rddir_cache_destroy(struct mfs_mnode *mnp)
{
    ASSERT(MFS_ISVOB(mnp));
    /* mnode lock is not necessary (nor is it held).  This routine is
       only called from mnode destroy code, so the mnode can't be
       found by other processes. */
    /* ASSERT(MISLOCKED(mnp)); */

    MDB_XLOG((MDB_MNOPS, "rddir cache destroy mnp %lx\n", mnp));
    if (mnp->mn_vob.rddir_cache) {
	mvfs_rddir_cache_empty(mnp);
	KMEM_FREE(mnp->mn_vob.rddir_cache,
		  RDDIR_CACHE_SIZE(mnp->mn_vob.rddir_cache));
	mnp->mn_vob.rddir_cache = NULL;
    }
}

tbs_boolean_t
mvfs_rddir_cache_get(
    struct mfs_mnode *mnp,
    struct uio *uiop,
    CRED_T *cred,
    int *eofp,
    int *errorp
)
{
    register int i;
    struct mvfs_rce *ep;
    int size;

    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));

    if (mnp->mn_vob.rddir_cache != NULL)
    {
	for (i = 0, ep = &mnp->mn_vob.rddir_cache->entries[0];
	     i < mnp->mn_vob.rddir_cache->nentries;
	     i++, ep++)
	{
	    /* if valid, at the right offset, and small enough, use it */
	    if (ep->valid &&
		MVFS_UIO_OFFSET(uiop) == ep->offset &&
		uiop->uio_resid >= ep->size)
	    {
		MDB_XLOG((MDB_MNOPS,
			  "rddir cache hit mnp %"MVFS_FMT_UIO_OFFSET_X" off %"MVFS_FMT_MOFFSET_T_X" size %lx\n", mnp,
			  MVFS_UIO_OFFSET(uiop), ep->size));
		if (ep->size) {
                    /* We put the size on the stack instead of using it
                     * directly because the linux readdir_uiomove will 0
                     * the size value passed in on a buffer overflow so
                     * that we don't skip entries.  So we need a temp
                     * value so that the readdir cache is not trashed.
                     */
                    size = ep->size;
                    *errorp = READDIR_UIOMOVE(ep->block, &size,
                                              UIO_READ, uiop, ep->offset);
                    if (!READDIR_BUF_FULL(uiop))
                        /* Should we advance offset if there was
                         * an error?  The previous code did this
                         * unconditionally (before addition of
                         * READDIR_BUF_FULL() macro).
                         */
                        MVFS_UIO_OFFSET(uiop) = ep->endoffset;
		} else
		    *errorp = 0;
		if (eofp != NULL)
		    *eofp = ep->eof;
		BUMPSTAT(mfs_acstat.ac_rddirhit);
		return TRUE;
	    }
	}
    } else {
	MDB_XLOG((MDB_MNOPS, "rddir cache get (empty) mnp %lx\n", mnp));
    }
    BUMPSTAT(mfs_acstat.ac_rddirmiss);
    MDB_XLOG((MDB_MNOPS, "rddir cache miss mnp %lx off %"MVFS_FMT_UIO_OFFSET_X" size %"MVFS_FMT_UIO_RESID_X"\n", mnp,
	      MVFS_UIO_OFFSET(uiop), uiop->uio_resid));
    return FALSE;
}

void
mvfs_rddir_cache_enter(
    struct mfs_mnode *mnp,
    struct mvfs_rce *entryp
)
{
    register int i;
    register struct mvfs_rce *ep;
    register mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    ASSERT(MFS_ISVOB(mnp));
    MLOCK(mnp);

    if (mnp->mn_vob.rddir_cache == NULL) {
	mnp->mn_vob.rddir_cache = (struct mvfs_rddir_cache *)
	    KMEM_ALLOC(RDDIR_CACHE_SIZE_N(mcdp->mvfs_rddir_blocks),
		       KM_SLEEP|KM_PAGED);
	mnp->mn_vob.rddir_cache->nentries = mcdp->mvfs_rddir_blocks;
	for (i = 0, ep = &mnp->mn_vob.rddir_cache->entries[0];
	     i < mnp->mn_vob.rddir_cache->nentries;
	     i++, ep++)
	{
	    ep->valid = FALSE;
	    ep->block = NULL;
	}
    }
    if (entryp->offset == (MOFFSET_T)0) {
	/* always use first entry for offset 0 (try to keep it around
	   since it probably has `.' and `..') */
	ep = &mnp->mn_vob.rddir_cache->entries[0];
    } else {
	/* If cache has an unused slot, use it.
	 *
	 * If it's full, replace the last entry.  readdir() is almost
	 * always used sequentially and traverses the entire
	 * directory, so if it won't all fit, leave at least the first
	 * portion in cache with the hope that it will find `..'  in a
	 * cached block (for pwd)
	 */
	for (i = 1, ep = &mnp->mn_vob.rddir_cache->entries[0];
	     i < mnp->mn_vob.rddir_cache->nentries;
	     i++, ep++)
	{
	    if (!ep[1].valid) {
		ep++;
		break;
	    }
	}
    }

    if (ep->valid && ep->block != NULL)
	KMEM_FREE(ep->block, ep->bsize);

    *ep = *entryp;
    MUNLOCK(mnp);
    MDB_XLOG((MDB_MNOPS, "rddir cache enter mnp %lx off %lx size %lx\n", mnp,
	      ep->offset, ep->size));
}

int
mvfs_rddir_cache_getcaches(
    mvfs_cache_sizes_t *szp
)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    szp->size[MVFS_SETCACHE_RDDIR_BLOCKS] = mcdp->mvfs_rddir_blocks;

    return 0;
}

int
mvfs_rddir_cache_setcaches(
    mvfs_cache_sizes_t *szp
)
{
    /* not supported at run time (yet)... */
/*    MVFS_SIZE_RUNTIME_SET(mvfs_rddir_blocks, szp, RDDIR_BLOCKS); */

    return 0;
}

#define RDDIR_FORMULA(scale) (((scale) > 2 ) ? 6 : (2*((scale)+1)))

void
mvfs_rddir_cache_init(
    mvfs_cache_sizes_t *mma_sizes
)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_RDDIR_BLOCKS] = mcdp->mvfs_rddir_blocks;

    /* Set number of blocks to cache for readdir if not already tuned. */
    MVFS_SIZE_DEFLOAD_NONZERO(mcdp->mvfs_rddir_blocks, mma_sizes, RDDIR_BLOCKS,
                              RDDIR_FORMULA(mcdp->mvfs_largeinit));
    return;
}

void
mvfs_rddir_cache_unload(void)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    mcdp->mvfs_rddir_blocks = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_RDDIR_BLOCKS];
    return;
}

int
mvfs_rddir_compute_caches(
    ks_int32_t scale_factor,
    mvfs_cache_sizes_t *szp
)
{
    if ((szp->mask & MVFS_CACHEBIT(RDDIR_BLOCKS)) == 0) {
        szp->size[MVFS_SETCACHE_RDDIR_BLOCKS] = RDDIR_FORMULA(scale_factor);
        szp->mask |= MVFS_CACHEBIT(RDDIR_BLOCKS);
    }
    return 0;
}
static const char vnode_verid_mvfs_rdc_c[] = "$Id:  922924ad.b44911de.8ddb.00:01:83:29:c0:fc $";
