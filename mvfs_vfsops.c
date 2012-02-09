/* * (C) Copyright IBM Corporation 1990, 2008. */
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

/* mvfs_vfsops.c */
#include "mvfs_systm.h"
#include "mvfs.h"
#include "mvfs_dnc.h"
#include "mvfs_copy.h"

/* Static variable that includes all data used by vfs and statistics subsystems. */
mvfs_vfs_data_t mvfs_vfs_data_var;
mvfs_stats_data_t mvfs_stats_data_var;

MVFS_MINMAP_T mvfs_minormap;	/* mvfs minor device map */

int mvfs_minmapsize; /* mvfs minor device map size */
MVFS_MAJOR_T *mvfs_majortbl; /* mvfs major number table */
#define VOBMOUNT_TABLESIZE \
    ((MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * MVFS_MINORMAX * sizeof(VFS_T *))

/* VFS mount generation number.  This is incremented on
   every unmount.  By using (fsid, gen) caches can be
   kept without any confusion when fsid's are reused because
   of unmounts. */

u_long mfs_mntgen = 0;

/* Our fs number and switch ptr */

MVFS_FSTYP_T	mfs_fstyp = 0;		/* MFS filesystem type */

V_OP_T  *mfs_vopp;		/* Ptr to mfs vnode ops switch */
VFSOPS_T *mfs_vfsopp;		/* Ptr to mfs vfs ops switch */
extern V_OP_T mvfs_vnodeops;

int mvfs_init_state = MVFS_NOT_INITIALIZED;
LOCK_T mfs_unload_lock;		/* lock between mount and unload procedures */

/* Turn on panics for Assertions by default */
tbs_boolean_t mvfs_panic_assert = FALSE;

#define FREE_INIT_LOCKS() { \
	FREELOCK(&mvfs_printf_lock); \
	FREELOCK(&mvfs_printstr_lock); \
	FREELOCK(&(MDKI_VFS_GET_DATAP()->mvfs_mountlock)); \
	MVFS_UNLOCK(&mfs_unload_lock); \
	FREELOCK(&mfs_unload_lock); \
}

extern struct mfs_rpchist mfs_init_viewophist;

#ifndef MVFS_SYSTEM_KMEM
extern struct mvfs_slab_list *mvfs_thread_slabs; 
extern struct mvfs_slab_list *mvfs_proc_slabs;
extern struct mvfs_slab_list *mvfs_cred_list_slabs; 
#endif

/* MFS init routine in SYS V.4 form.  */

/* Local function prototypes */

STATIC int
mvfs_vobmount_init(void);

STATIC void
mvfs_vobmount_free(void);

STATIC int
mvfs_find_devnum(
    int mmi_minor, 
    dev_t *majorp, 
    int *minorp
);

int
mvfsinit(
    VFSSW_T *vswp,
    int fstype
)
{
        int error = 0;
        mvfs_common_data_t *mcdp;
        mvfs_vfs_data_t *mvdp;

	/* If already set up, don't do it again */
	if (mvfs_init_state != MVFS_NOT_INITIALIZED) return(0);

	/* 
	 * First, do any machine dependent init.
 	 * This is an optional macro in the mdep.h file for the port.
         */
	MVFS_MDEP_INIT();

	/*
	 * adjust cache sizes might print, so init the lock here.
	 */
	INITLOCK(&mvfs_printf_lock, "mvfspnrt");
	INITLOCK(&mvfs_printstr_lock, "mvfspstr");

        /* Init the "unload lock" for locking between mount
	 * and unload procedure
         */
	INITLOCK(&mfs_unload_lock, "mfsunload");

        mcdp = MDKI_COMMON_GET_DATAP();
        BZERO(mcdp, sizeof(mvfs_common_data_t));
        mvdp = MDKI_VFS_GET_DATAP();
        BZERO(mvdp, sizeof(mvfs_vfs_data_t));

        mvfs_copy_tunable(mcdp);

	/* Init the mount lock for locking between mount/unmount
	 * and when people want to hold the viewroot mount.
	 */
	INITLOCK(&(mvdp->mvfs_mountlock), "mvfsmount");
	/* 
         * Lock the unload lock to prevent racy mounts, and/or unloads.
         */
        MVFS_LOCK(&mfs_unload_lock);

        /* Now, set the initted flag */
	mvfs_init_state = MVFS_INIT_PHASE1;

	/* Display our version on initialization */
	mvfs_log(MFS_LOG_INFO, "%s", mvfs_version);

        /* Set up fs type and switch ptr */
	mfs_fstyp = (MVFS_FSTYP_T)fstype;

	MFS_VFSSW_ASSIGN(vswp, mfs_fstyp, mvfs_vfsops);
	mfs_vfsopp = &mvfs_vfsops;
	mfs_vopp = &mvfs_vnodeops;

#ifndef MVFS_SYSTEM_KMEM
	mfs_kmem_init();	/* Init kmem alloc (debug) before all else */
#endif

        /* everything else is deferred until viewroot mount--see
           mvfs_misc_init() below */

	MVFS_UNLOCK(&mfs_unload_lock);

	MVFS_THREAD_SLAB_INIT(mvfs_thread_slabs, sizeof(mvfs_thread_t), TRUE,
                                                 "mvfs_thread_slabs");
	MVFS_PROC_SLAB_INIT(mvfs_proc_slabs, sizeof(mvfs_proc_t), TRUE,
                                               "mvfs_proc_slabs");
        MVFS_CLR_CRED_SLAB_INIT(mvfs_cred_list_slabs, sizeof(mvfs_clr_creds_t), 
						TRUE, "mvfs_cl_slab");

	return(0);
}

int
mvfs_minormap_init(mvfs_cache_sizes_t *mma_sizes)
{

        int error;
        MDKI_SET_MINORMAX(MVFS_MINORMAX, MVFS_VIEW_SHIFT_BITS);
        MDKI_SET_VIEW_MASK_BITS(MVFS_VIEW_MASK_BITS, MVFS_VIEW_SHIFT_BITS);

	/* Init the minor map */
	MVFS_MAPINIT(&mvfs_minormap, error);
        if (error != 0)
            return error;

        /* Init the major device number table */
        error = MVFS_MAJORTBL_INIT(&mvfs_majortbl);
        if (error != 0) {
            MVFS_MAPFREE(&mvfs_minormap);
            return error;
        }
        
        return 0;
}

void
mvfs_minormap_free(void)
{

    MVFS_RELEASE_MAJORS(mvfs_majortbl);
    MVFS_MAJORTBL_FREE(mvfs_majortbl);
    MVFS_MAPFREE(&mvfs_minormap);
}

/*
 * This routine does the per-zone initialization of the mvfs_vobmount table.
 */
STATIC int
mvfs_vobmount_init(void)
{
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();
        
    /* 
     * Init the vob mount table (VOBMOUNT_TABLESIZE may not evaluate to a 
     * constant, but rather to a computation using a tunable value.  So
     * this routine must be called after the tunables in VOBMOUNT_TABLESIZE
     * have been set.
     */
    if ((mvdp->mfs_vobmounts = (VFS_T **)KMEM_ALLOC(VOBMOUNT_TABLESIZE,
					      KM_SLEEP)) == NULL)
    {
	MVFS_MAPFREE(&mvfs_minormap);
	MVFS_MAJORTBL_FREE(mvfs_majortbl);
	return ENOMEM;
    }
    /* Init the vob mount table */
    BZERO(mvdp->mfs_vobmounts, VOBMOUNT_TABLESIZE);
    return 0;
}

/*
 * This routine frees the mvfs_vobmount table.
 */
STATIC void
mvfs_vobmount_free(void)
{
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    KMEM_FREE(mvdp->mfs_vobmounts,
              (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * MVFS_MINORMAX * sizeof(VFS_T *));
}

/* 
 * Clean up various structures no longer needed once we unmount all VOBs and
 * viewroot. 
 */
void
mvfs_misc_free(void)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    mvfs_stats_data_t *sdp = MDKI_STATS_GET_DATAP();

    /* 
     * Time to unload our data structures
     */

    MVFS_FREE_CREDLIST();
    mvfs_clnt_free();
    mvfs_dncfree();

    mvfs_viewfree();
    /*
     *  We must unload the audit after proc stuff: proc stuff might
     *  release things via audit, and that requires manipulating the
     *  audit locks that would be freed by mvfs_auditfree().
     */
    mvfs_procdata_free();
    mvfs_auditfree();
    mvfs_mndata_free();
    mvfs_util_free();
    mvfs_vobmount_free();
    mvfs_minormap_free();
    MDKI_STATS_FREE_DATA();
    MVFS_MDEP_MISC_FREE();
    mcdp->mvfs_largeinit = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_LARGEINIT];

    return;
}

/* Unload the file system.  If vswp is NULL, this is a non-VFSSW platform. */

int
mfs_unload(vswp, fsnum)
VFSSW_T *vswp;
int fsnum;
{
    int num;

    /* If no init, only undo the vfssw */

    if (mvfs_init_state == MVFS_NOT_INITIALIZED) {
	if (vswp != NULL)
            MFS_VFSSW_CLEAR(vswp, fsnum);
	return(0);	/* That was easy! */
    }

    /* Lock the "unload" lock to prevent further mounts */

    MVFS_LOCK(&mfs_unload_lock);

    /* Check for any active mnodes.  (Note: if there are no
     * mnodes, then there can't be any mounts since a mount needs
     * a root mnode at least! 
     */

    if ((num = mfs_mncount()) > 0) {
	MVFS_UNLOCK(&mfs_unload_lock);
        mvfs_log(MFS_LOG_INFO,
                 "MVFS: mfs_unload: %d mnodes still open\n", num);
	return(EBUSY);		/* Still busy */
    }

    if (mvfs_init_state == MVFS_INIT_COMPLETE) {
        MVFS_UNLOCK(&mfs_unload_lock);
        mvfs_log(MFS_LOG_INFO,
                 "MVFS: mfs_unload: mvfs busy.\n");
	return(EBUSY);		/* Still busy */
    }

    /* Clear out our VFS ops ptr */
    if (vswp != NULL) 
	MFS_VFSSW_CLEAR(vswp, fsnum);

    MDKI_USECDELAY(1000000*2);

    if (mvfs_init_state == MVFS_INIT_PHASE1) {
        MVFS_KMEM_UNLOAD();
    }

    /* Free the memory allocated for proc/thread slab management.
     * Separate from proc/thread hash tables because those come and go
     * with viewroot mounts. 
     */
#ifndef MVFS_SYSTEM_KMEM
    MDB_XLOG((MDB_MEMOP,"proc/thread slab memory being freed: %"KS_FMT_PTR_T" %"KS_FMT_PTR_T"\n", mvfs_proc_slabs, mvfs_thread_slabs));
#endif	/* XXX could put down 3 lines, but macros handle ... */
    MVFS_PROC_SLAB_DESTROY(mvfs_proc_slabs);
    MVFS_PROC_SLAB_DESTROY(mvfs_thread_slabs);
    MVFS_CLR_CRED_SLAB_DESTROY(mvfs_cred_list_slabs);

    mvfs_init_state = MVFS_NOT_INITIALIZED;

    FREELOCK(&mvfs_printf_lock);
    FREELOCK(&mvfs_printstr_lock);
    MVFS_STATLOCK_FREE();
    FREELOCK(&(MDKI_VFS_GET_DATAP()->mvfs_mountlock));
    MVFS_UNLOCK(&mfs_unload_lock);
    FREELOCK(&mfs_unload_lock);

    MVFS_MDEP_UNLOAD();
    return(0);
}

/*
 * This function auto adjusts mvfs_largeinit based on system memory size.
 * Called if user has not adjusted the default value.  Generally, we try
 * to keep the old values of mvfs_largeinit for smaller desktop systems
 * while trying to set reasonable defaults for large Build Servers with
 * multiple-GB of memory.
 *  Algorithm:
 *  mvfs_largeinit is set with a step function with steps of width 
 *  MVFS_LARGEINITBLK.  However, the beginning is special-cased for
 *  compatibility with pre-5.0 settings of 0 and 1.  Then there is a
 *  long step with mvfs_largeinit of 2, to bridge the gap with large systems.
 *  After this, when memory size increases a step -- MVFS_LARGEINITBLK, 
 *  mvfs_largeinit will increase by 1, until it reaches MVFS_LARGEINITMAX.
 *  We never automatically size mvfs_largeinit above MVFS_LARGEINITMAX, 
 *  however, on UNIX, the user is free to set mvfs_largeinit to a larger number
 *  if they wish.
 */
void
mvfs_compute_largeinit(u_long kbytes_available)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    /* keep the old settings for memory < (MVFS_LARGEINITBLK/2) */
    if (kbytes_available < MVFS_MINIMUM_LARGEMEM) {
        mcdp->mvfs_computed_largeinit = 0;
    }
    else if (kbytes_available < MVFS_LARGEINITBLK/2) {
        mcdp->mvfs_computed_largeinit = 1;
    }
    else if (kbytes_available < MVFS_LARGEINITBLK*2) {
        mcdp->mvfs_computed_largeinit = 2;
    }
    else {
        mcdp->mvfs_computed_largeinit = kbytes_available / MVFS_LARGEINITBLK;
        /* Restrict largeinit value to MVFS_LARGEINITMAX */
        if (mcdp->mvfs_computed_largeinit > MVFS_LARGEINITMAX) {
            mcdp->mvfs_computed_largeinit = MVFS_LARGEINITMAX;
        }
    }

}

void
mvfs_physmem_adj_caches(mvfs_cache_sizes_t *mma_sizes)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    u_long kbytes_available; /* Physical memory size in Kilo-Bytes */

    /* Auto scale what mvfs_largeinit would be if not tuned by mma_sizes */
    /* Convert number of memory pages to kbytes. */
    kbytes_available = (MDKI_PAGESIZE() / 1024) * MVFS_PHYSMEM_PAGECNT;

    mvfs_compute_largeinit(kbytes_available);

    if (MVFS_SIZE_VALID(mma_sizes, LARGEINIT))
        MVFS_SIZE_LOAD(mcdp->mvfs_largeinit, mma_sizes, LARGEINIT);
    else {
        mcdp->mvfs_largeinit = mcdp->mvfs_computed_largeinit;
        mvfs_log(MFS_LOG_INFO,
                 "MVFS: Setting mvfs_largeinit to %d based on physical memory"
                 " size of %luKB\n", mcdp->mvfs_largeinit, kbytes_available);
    }
}

/*
 * Complete any deferred initialization, using mount args to set
 * sizes, etc.  (It's put here in the file to avoid having to add
 * prototypes for some functions it calls and are defined above.)
 */
int
mvfs_misc_init(
    mvfs_cache_sizes_t *mma_sizes
)
{
    int error, i;
    mvfs_stats_data_t *sdp;

    /* 
     * Init spinlock for stats before any other inits 
     */

    MDKI_STATS_ALLOC_DATA();
    sdp = MDKI_STATS_GET_DATAP();
    MVFS_STATLOCK_INIT();

    /* Init the statistics structures  
     * Version number for each stats substruct, see sys/mfs_stats.h
     */
    sdp->mfs_clntstat.version = MFS_CLNTSTAT_VERS;
    sdp->mfs_mnstat.version = MFS_MNSTAT_VERS;
    sdp->mfs_clearstat.version = MFS_CLEARSTAT_VERS;
    sdp->mfs_rvcstat.version = MFS_RVCSTAT_VERS;
    sdp->mfs_dncstat.version = MFS_DNCSTAT_VERS;
    sdp->mfs_acstat.version = MFS_ACSTAT_VERS;
    sdp->mfs_rlstat.version = MFS_RLSTAT_VERS;
    sdp->mfs_austat.version = MFS_AUSTAT_VERS;
    /* Lazy way to init the histogram bucket markers, from 
     * static inits in mvfs_rpcutl.c
     */
    sdp->mfs_viewophist = mfs_init_viewophist;  
    /* Zero out all arrays */
    BZERO(sdp->mfs_vnopcnt, mfs_vnopmax*sizeof(sdp->mfs_vnopcnt[0]));
    BZERO(sdp->mfs_vfsopcnt, mfs_vfsopmax*sizeof(sdp->mfs_vfsopcnt[0]));
    BZERO(sdp->mfs_viewopcnt, mfs_viewopmax*sizeof(sdp->mfs_viewopcnt[0]));
    BZERO(&(sdp->mfs_viewophist.histrpc[0]),
          sizeof(sdp->mfs_viewophist.histrpc));
    BZERO(&(sdp->mfs_viewophist.histclr[0]),
          sizeof(sdp->mfs_viewophist.histclr));
    BZERO(&(sdp->mfs_viewophist.histperop[0][0]),
          sizeof(sdp->mfs_viewophist.histperop));
    for (i=0; i < VIEW_NUM_PROCS; i++) {
        sdp->mfs_viewoptime[i].tv_sec = sdp->mfs_viewoptime[i].tv_nsec = 0; 
    }

    /*
     * adjust cache memory sizes
     */
    MVFS_ADJUST_CACHESIZES(mma_sizes);

    error = MVFS_MDEP_MISC_INIT(mma_sizes);
    if (error != 0)
        return error;
    error = mvfs_minormap_init(mma_sizes);
    if (error != 0)
        goto mdeperr;
    error = mvfs_vobmount_init();
    if (error != 0)
        goto maperr;
    error = mvfs_util_init(mma_sizes); /* initialize utility things */
    if (error != 0)
        goto vobmerr;
    error = mvfs_mninit(mma_sizes); /* Init mnodes */
    if (error != 0) {
        goto utilerr;
    }
    error = mvfs_procinit(mma_sizes); /* Init per-process info tables */
    if (error != 0) {
        goto mnerr;
    }
    error = mvfs_auditinit(mma_sizes); /* Init audit structs */
    if (error != 0) {
        /*
         * should have no auditing set up yet, so it's safe to
         * unload without having audit initialized yet
         */
        mvfs_procdata_free();
        goto mnerr;
    }
    error = mvfs_viewinit(mma_sizes); /* Init view structs */
    if (error != 0) {
        goto procerr;
    }
    error = mvfs_dncinit(mma_sizes);   /* Init dir name cache */
    if (error != 0) {
        goto viewerr;
    }
    error = mvfs_clnt_init(mma_sizes);	/* Init rpc client cache */
    if (error != 0) {
        goto dncerr;
    }
    error = MVFS_INIT_CREDLIST(mma_sizes); /* Init cleartext cred cache */
    if (error != 0) {
        /* Unwind in reverse order */
        /* don't need to undo mvfs_clear_init, it failed so we came here */
        mvfs_clnt_free();
      dncerr:
        mvfs_dncfree();
      viewerr:
        mvfs_viewfree();
      procerr:
        /*
         *  We must unload the audit after proc stuff: proc stuff might
         *  release things via audit, and that requires manipulating the
         *  audit locks that would be freed by mvfs_auditfree().
         */
        mvfs_procdata_free();
        mvfs_auditfree();
      mnerr:
        mvfs_mndata_free();
      utilerr:
        mvfs_util_free();
      vobmerr:
	mvfs_vobmount_free();
      maperr:
        mvfs_minormap_free();
      mdeperr:
        MVFS_MDEP_MISC_FREE();
    }
    return error;
}

/*
 * Routines to search for vob mounts
 */

void
mfs_findvfs_lock()
{
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    /* Lock the mount-lock to keep the list of vob-mounts stable */
    MVFS_LOCK(&(mvdp->mvfs_mountlock));
}

void
mfs_findvfs_unlock()
{
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    /* Unlock the mount-lock which protected the vob-mounts list */
    MVFS_UNLOCK(&(mvdp->mvfs_mountlock));
}

VFS_T *
mfs_findvfs_nm(case_insensitive, nm)
tbs_boolean_t case_insensitive;
mfs_pn_char_t *nm;
{
    int i;
    VFS_T *vfsp;
    int myzoneid = MDKI_GETZONEID();
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    ASSERT(ISLOCKED(&(MDKI_VFS_GET_DATAP()->mvfs_mountlock)));

    for (i=0; i <= mvdp->mfs_vobmount_hwm; i++) {
	vfsp = mvdp->mfs_vobmounts[i];
        if (vfsp == NULL) continue;
	ASSERT(myzoneid == VFS_TO_MMI(vfsp)->mmi_zoneid);
        /* FIXME: findvfs_nm should support case-insensitive someday */
	if (PN_STRCMP(case_insensitive, 
            VFS_TO_MMI(vfsp)->mmi_vobtag, nm) == 0) {
	    return(vfsp);
	    }
    }
    return(NULL);	/* Not found */
}

VFS_T *
mfs_findvfs_mntpath(path)
mfs_pn_char_t *path;
{
    int i;
    VFS_T *vfsp;
    int myzoneid = MDKI_GETZONEID();
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    ASSERT(ISLOCKED(&(MDKI_VFS_GET_DATAP()->mvfs_mountlock)));

    for (i=0; i <= mvdp->mfs_vobmount_hwm; i++) {
	vfsp = mvdp->mfs_vobmounts[i];
        if (vfsp == NULL) continue;
        /* FIXME: findvfs_mntpath should support case-insensitive someday */
	if (myzoneid == VFS_TO_MMI(vfsp)->mmi_zoneid &&
            PN_STRCMP(FALSE, VFS_TO_MMI(vfsp)->mmi_mntpath, path) == 0)
            return(vfsp);
    }

    return(NULL);	/* Not found */
}

VFS_T *
mfs_findvfs_oid(oidp, indx, unique)
tbs_oid_t *oidp;
int *indx;
tbs_boolean_t *unique;
{
    int i = 0;
    VFS_T *vfsp;
    VFS_T *saved_vfsp = NULL;
    int myzoneid = MDKI_GETZONEID();
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    *unique = TRUE;

    ASSERT(ISLOCKED(&(MDKI_VFS_GET_DATAP()->mvfs_mountlock)));
   
    if (indx)
	i = *indx;
    for (; i <= mvdp->mfs_vobmount_hwm; i++) {
	vfsp = mvdp->mfs_vobmounts[i];
        if (vfsp == NULL) continue;
	if (MFS_OIDEQ(VFS_TO_MMI(vfsp)->mmi_voboid, *oidp)) {
	    if (indx) {
		*indx = i + 1;
		indx = NULL;
	    }
	    if (saved_vfsp)
		*unique = FALSE;
	    else {
		saved_vfsp = vfsp;
	    ASSERT(myzoneid == VFS_TO_MMI(vfsp)->mmi_zoneid);
	    }
	}
    }
    return(saved_vfsp);
}

VFS_T *
mfs_findvfs_uuid(uuidp)
tbs_uuid_t *uuidp;
{
    VFS_T *vfsp;
    int i;
    int myzoneid = MDKI_GETZONEID();
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    ASSERT(ISLOCKED(&(MDKI_VFS_GET_DATAP()->mvfs_mountlock)));

    for (i=0; i <= mvdp->mfs_vobmount_hwm; i++) {
	vfsp = mvdp->mfs_vobmounts[i];
	if (vfsp == NULL) continue;
	if (MFS_UUIDEQ(VFS_TO_MMI(vfsp)->mmi_vobuuid, *uuidp)) {
	    ASSERT(myzoneid == VFS_TO_MMI(vfsp)->mmi_zoneid);
	    return(vfsp);
	    }
    }

    return(NULL);	/* Not found */
}

VFS_T *
mfs_findvfs_cookie(cookiep)
u_long *cookiep;
{
    VFS_T *vfsp;
    u_long i;
    int myzoneid = MDKI_GETZONEID();
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    ASSERT(ISLOCKED(&(MDKI_VFS_GET_DATAP()->mvfs_mountlock)));

    for (i=*cookiep; i <= mvdp->mfs_vobmount_hwm; i++) {
	vfsp = mvdp->mfs_vobmounts[i];
	if (vfsp == NULL) continue;
	*cookiep = i+1;		/* Leave cookie at next entry */
	ASSERT(myzoneid == VFS_TO_MMI(vfsp)->mmi_zoneid);
	return(vfsp);
    }

    return(NULL);	/* No more vob mounts found */
}

/*
 * Main mount routines
 */

/*
 * MFS register/unregister calls.  Actually, this allocates
 * a bogus vfsp, and then calls mount with the normal
 * mount arguments.  It can be used on either Unix or NT
 * to support vobs under Nt-style view-tags without any
 * real mounting required.
 */

int
mfs_register_mount(mnt_data, mnt_datalen, cred, callinfo)
caddr_t mnt_data;
size_t mnt_datalen;
CRED_T *cred;
MVFS_CALLER_INFO *callinfo;
{
    VFS_T *vfsp;
    int error;

    /* Allocate a vfs struct for the mount */
   
    vfsp = (VFS_T *)KMEM_ALLOC(sizeof(VFS_T), KM_SLEEP);
    if (vfsp == NULL) return(ENOMEM);

    /*
     * Initialize the VFS structure.
     * We use the VFS op ptr for MVFS VFS ops set up in mvfsinit()
     * We do not assign a data ptr yet, the mount subroutine does this later.
     */

    MVFS_INITVFS(vfsp, mfs_vfsopp, NULL);

    /* Now, call the mount subr */

    error = mfs_vmount_subr(vfsp, 0, NULL, 0, mnt_data, mnt_datalen, cred,
                            callinfo);
    if (error != 0) {
	KMEM_FREE(vfsp, sizeof(*vfsp));
    }
    return(error);
}

int
mfs_unregister_mount(vfsp, cred)
VFS_T *vfsp;
CRED_T *cred;
{
    int error;

    /* Try the unmount first */

    error = mfs_vunmount(vfsp, cred);
    if (error) return(error);

    /* Now that we are unmounted, free up the VFS struct */

    MVFS_FINISHVFS(vfsp);		/* clean up any state set up by
					   MVFS_INITVFS, e.g. lock cleanup */
    KMEM_FREE(vfsp, sizeof(*vfsp));
    return(0);
}

int
mfs_unregister_all_vobs(cred)
CRED_T *cred;
{
    int i;
    VFS_T *vfsp;
    int error;
    /* 
     * This var "some_error" is set non-zero if some unmount has
     * an error .  It is the "last" unmount error encountered.
     */
    int some_error = 0;	
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();

    for (i=0; i <= mvdp->mfs_vobmount_hwm; i++) {
	vfsp = mvdp->mfs_vobmounts[i];
        if (vfsp == NULL) continue;
	error = mfs_unregister_mount(vfsp, cred);
	if (error) {
	    some_error = error;
	    mvfs_log(MFS_LOG_WARN, "trouble unregistering mount %d%s\n",
			i, mfs_strerr(error));
	}
	/* Continue after errors */
    }
    return(some_error);
}

/*
 * Mfs mount in a useful internal form.
 * Set up mount info record and attach it to vfs struct.
 */
int
mfs_vmount_subr(vfsp, mvp, path, flags, data, datalen, acred, callinfo)
VFS_T *vfsp;	/* Our vfs ptr (mount tab) */
VNODE_T *mvp;	/* mount point vnode */
char *path;		/* Pathname of special file */
int flags;		/* flags to the mount */
caddr_t data;		/* data args */
size_t datalen;		/* data args length */
CRED_T *acred;		/* acred may be NULL for pre-SVR4 wrappers */
MVFS_CALLER_INFO *callinfo; /* Caller info (irp info)    */
			/* unused since Clearcase NT port */
{
    int error;			/* error number */
    VNODE_T *rtvp;		/* the server's root */
    struct mfs_mnode *mnp;	/* root MFS vinfo */
    struct mfs_mntinfo *mmi;	/* MFS mount info */
    VNODE_T *svp;		/* .specdev vp */
    register int i;
    int num;
    SPL_T s;
    char *pnp;
    int vobminor, minor_num;
    dev_t major_num;
    int nvminor_num;
    dev_t nvmajor_num;
    int device_attached = FALSE;
    tbs_boolean_t saved_largeinit = FALSE;
    tbs_boolean_t mvfs_misc_init_done = FALSE;
    VFS_T *canary;
    u_long cookie = 0;
    /* Declare a type so we can do one allocation to save stack space. */
    struct {
        struct mfs_mntargs mma;
        struct mvfs_splitpool splitpool;
    } *alloc_unitp;
    struct mfs_mntargs *mmap;
    struct mvfs_splitpool *splitpoolp;
    mvfs_common_data_t *mcdp;
    mvfs_vfs_data_t *mvdp;
    mvfs_viewroot_data_t *vrdp;

    /*
     * Check for load/unload in progress.  If so, just bug
     * out of here!
     */

    if (mvfs_init_state == MVFS_NOT_INITIALIZED) {
        return(EIO);
    }

    /* Before we get any locks, do a quick sanity check, did we get
     * any data passed to us? 
     */

    if (data == NULL) {
        return(EINVAL);
    }

    /* Allocate stuff to keep the stack size down. */
    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    mmap = &(alloc_unitp->mma);
    splitpoolp = &(alloc_unitp->splitpool);

    if (!CONDITIONAL_LOCK(&mfs_unload_lock)) return(EIO);
    mcdp = MDKI_COMMON_GET_DATAP();
    mvdp = MDKI_VFS_GET_DATAP();
    MVFS_LOCK(&(mvdp->mvfs_mountlock));
 
    error = 0;
    mmi = NULL;
    rtvp = NULL;

    /*
     * Only check for duplicate mounts after everything is initialized in a
     * given zone.
     */
    if (mvdp->mvfs_mount_count != 0) {
        /* Do this only when we're initialized (may need to allocate
         * minormap)
         */
        while ((canary = mfs_findvfs_cookie(&cookie)) != NULL)
            if (canary == vfsp) {
                mvfs_log(MFS_LOG_ERR, "duplicate vfsp %lx already mounted at ID %lu\n", vfsp, cookie);
                /* Should we mark the existing vfsp mount as broken somehow? */
                ASSERT(canary != vfsp); /* die on a debug kernel */
                error = EBUSY;
                goto errout;            /* fail on non-debug kernel */
            }
    }
    /* Get the mount args */
    error = CopyInMfs_mntargs(data, mmap, callinfo);

    if (error)
	goto errout;

    /* Check the mount args version.  */

    if (mmap->mma_mntvers != MFSMNT_VERSION) {
        mvfs_log(MFS_LOG_ERR,
                 "mount_mvfs version mismatch: expected %d got %d\n",
                 MFSMNT_VERSION, mmap->mma_mntvers);
	error = EINVAL;
	goto errout;
    }
    if (mmap->mma_mntsize != sizeof(*mmap)) {
	mvfs_log(MFS_LOG_ERR,
                 "mount_mvfs parameter size mismatch (got %d wanted %d)\n",
                 mmap->mma_mntsize, sizeof(*mmap));
	error = EINVAL;
	goto errout;
    }

    if (mvdp->mvfs_mount_count == 0) {
        /* complete remaining phases during first mount of this zone */
        if (!(mmap->mma_flags & MFSMNT_VIEWROOT)) {
            /* Must mount viewroot to get cache sizing/etc */
            mvfs_log(MFS_LOG_ERR, "mount_mvfs: viewroot must be mounted before VOBs (mma_flags=%x)\n", mmap->mma_flags);
            error = EIO;
            goto errout;
        }
	if (mmap->mma_sizes.version != MVFS_SETCACHE_VERSION) {
            mvfs_log(MFS_LOG_ERR, "mount_mvfs cache size version mismatch\n");
            error = EINVAL;
            goto errout;
        }

        saved_largeinit = TRUE;
        mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_LARGEINIT] = mcdp->mvfs_largeinit;

        error = mvfs_misc_init(&mmap->mma_sizes);
        if (error)
            goto errout;
        else
            mvfs_misc_init_done = TRUE;

#ifndef MVFS_DEBUG
	/*
	 * done with startup messages, crank down logpri to normal
	 * starting value.
	 */
	mfs_logpri = MFS_LOG_ERR;
#endif
        mvfs_init_state = MVFS_INIT_COMPLETE;
    }
    /*
     * It may be the case that we fail after this point, and leave
     * init state at MVFS_INIT_COMPLETE.  To fully clean up and be
     * able to honor new cache sizes/etc will require a successful
     * unmount of something (perhaps requiring a mount first, if this
     * failure was for /view).
     * XXX Seems like this should be cleaned up.
     */

    /* stats structures are not available till after misc_init */
    BUMPSTAT(mfs_vfsopcnt[MFS_VMOUNT], s);

    /* Allocate room for the mount info struct */

    mmi = (struct mfs_mntinfo *)
            KMEM_ALLOC(sizeof(struct mfs_mntinfo),KM_SLEEP);
    if (mmi == NULL) {
	error = ENOMEM;
	goto errout;
    }
    BZERO(mmi, sizeof(struct mfs_mntinfo));
    mmi->mmi_minor = mmi->mmi_nvminor = -1; /* mark unallocated */
    INITSPLOCK(mmi->mmi_rclock, "mvfs_mmi_refcnt"); /* Initialize refcnt lock */
    mmi->mmi_zoneid = MDKI_GETZONEID();

    /* Set type of mount */

    if ((mmap->mma_flags & MFSMNT_VIEWROOT) != 0) {
	if (mmap->mma_vobminor != 0) {
	    error = EINVAL;
	    goto errout;
	}
	mmi->mmi_mnttype = MFS_VIEWSVRMNT;
    }
    else {
	if ((MDKI_INGLOBALZONE()) && (mmap->mma_vobminor != 0)) {
            /*
	     * User space requests 1,2, ..., MVFS_MAJFIXMAX*MVFS_MINORMAX/2.
	     * we convert this to (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * MVFS_MINORMAX - 2,
	     * (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * MVFS_MINORMAX - 4,
	     * (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * MVFS_MINORMAX - 6,
             * ...,
	     * MVFS_MAJDYNMAX * MVFS_MINORMAX.
             * The ncaexported vobs map to the high-addr part in the mvfs_minormap, 
             * and going downward. But, the non-exported vobs look for empty slots 
             * start from lower-addr part.
             *
	     * Note: mma_vobminor is unsigned so math here is OK.
             */
	    vobminor = (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * MVFS_MINORMAX - 2*mmap->mma_vobminor;
	    if (vobminor > (MVFS_MAJDYNMAX + MVFS_MAJFIXMAX) * MVFS_MINORMAX - 2 || vobminor < MVFS_MAJDYNMAX * MVFS_MINORMAX)
            {
		error = EINVAL;
		goto errout;
	    }
	    if (mvfs_get_minorpair(&mvfs_minormap, MVFS_MINMAPSIZE,
				   vobminor) != 0) {
		error = EBUSY;		/* FIXME: better code for device
					   in use ?*/
		goto errout;
	    }
	    mmi->mmi_minor = vobminor;
	    mmi->mmi_nvminor = vobminor + 1;
	}
	mmi->mmi_mnttype = MFS_VOBMNT;
    }

    /* 
     * Fill in common "server" and "retry" structs.
     *     For Viewroot this is albd server info
     *     For Vobs this is vob svr/retry and view retry info.
     */

    error = mfs_copyin_strbuf(mmap->mma_host, &mmi->mmi_svr.host);
    if (error) goto errout;

    mmi->mmi_retry.soft = ((mmap->mma_flags & MFSMNT_SOFT) != 0);
    mmi->mmi_retry.nointr = ((mmap->mma_flags & MFSMNT_NOINTR) != 0);
    mmi->mmi_retry.timeo = mmap->mma_timeo;
    mmi->mmi_retry.retries = mmap->mma_retries;
    if (mmi->mmi_retry.timeo == 0) mmi->mmi_retry.timeo = MFSMNT_TIMEO_DEFAULT;
    if (mmi->mmi_retry.retries == 0) 
		mmi->mmi_retry.retries = MFSMNT_RETRANS_DEFAULT;

    /* Fill mount dir pathname */

    error = mfs_copyin_strbuf(mmap->mma_mntpath, &mmi->mmi_mntpath);
    if (error) goto errout;

    vrdp = MDKI_VIEWROOT_GET_DATAP();
    if (mmi->mmi_mnttype == MFS_VIEWSVRMNT) {

        /* Check for duplicate mount */

	if (vrdp->mfs_viewroot_vfsp) {
	    error = EEXIST;
	    goto errout;
	}

        /* Fill in viewroot server struct */

	pnp = STRDUP("albd server");
	MFS_INIT_STRBUFPN_PAIR_IN(&mmi->mmi_svr.lpn, pnp, pnp);
	mmi->mmi_svr.rpn = STRDUP("albd server");
	mmi->mmi_svr.uuid = TBS_UUID_NULL;
	mmi->mmi_svr.svrbound = 1;
        mmi->mmi_svr.addr = mmap->mma_addr;
	MVFS_SIN_CVT(&mmi->mmi_svr.addr);
	/* Set up MMI info before any makenodes. */

        mmi->mmi_minor = mvfs_vfsgetnum(&mvfs_minormap, MVFS_MINMAPSIZE);
	mmi->mmi_nvminor = -1;	/* Not used for viewroot mountpoint */
	mmi->mmi_gen   = mfs_mntgen;

	vfsp->vfs_bsize = DEV_BSIZE;

        /* For Linux, the device number is assigned before we are called */
	MVFS_SET_FSTYPE(vfsp);
	vfsp->vfs_data = (MVFS_VFSDATA_T)mmi;
        if ((error = mvfs_find_devnum(mmi->mmi_minor, &major_num, &minor_num)) != 0)
            goto errout;
        MAKEFSID(VFS_FSID(vfsp), major_num, minor_num);
	MAKEVFSDEV(vfsp, major_num, minor_num, major_num, -1);

	/* 
         * Set up the mount point history mode suffix if passed in
         */

	if (mmap->mma_hmsuffix.s != NULL && mmap->mma_hmsuffix.l > 0) {
            error = mfs_copyin_strbuf(mmap->mma_hmsuffix, &mmi->mmi_hmsuffix);
            if (error) goto errout;
	    mmi->mmi_hmsuffixlen = STRLEN(mmi->mmi_hmsuffix);
	} else {
	    mmi->mmi_hmsuffix = NULL;
	    mmi->mmi_hmsuffixlen = 0;
   	}

	/*
	 * Set up the mount point HM version name if there
 	 * is a hm suffix configured.  Normally .@@ will
         * "warp" you to the element level, so you need
         * to say .@@/main/LATEST/file.c/main/LATEST  
         * The hm version name can be used to to warp into hm
         * to the version of the current directory so that you
         * can say: ^@@/file.c/main/LATEST.  
	 */

	if (mmi->mmi_hmsuffix) {
	    /* Allocate suffixlen + 2 for extra char and trailing null */
	    mmi->mmi_hmvers_nm = KMEM_ALLOC(mmi->mmi_hmsuffixlen + 2, KM_SLEEP);
	    if (mmi->mmi_hmvers_nm) {
	        mmi->mmi_hmvers_nm[0] = MVFS_VX_VERS_CHAR; /* Canned ... */
		STRCPY(&mmi->mmi_hmvers_nm[1], mmi->mmi_hmsuffix);
	    } else {
		error = ENOMEM;	
		goto errout;
	    }
	}

	/* 
	 * Set up the kind of view-tags to create under this mount.
	 * FIXME: for now, based on the PORT.  But... should be
	 * a mount-argument...
   	 */

	mmi->mmi_default_vwtag_kind = MFS_VIEWCLAS;

        /* Make the special view root node and fill it in */

        error = mfs_makespecnode(MFS_VIEWDIRCLAS, (VNODE_T *)NULL, 
			(void *)NULL, vfsp, &rtvp);
        if (error) goto errout;
        MVFS_SET_VROOT(rtvp);

        /* Add "." and "..".  Need an extra hold for each dirent */
	 
	VN_HOLD(rtvp);
  	(void) mfs_ramdir_add(rtvp, ".", rtvp, &num);
	VN_HOLD(rtvp);
	(void) mfs_ramdir_add(rtvp, "..", rtvp, &num);
        MVFS_INCREMENT_LINK(rtvp);

	/* Add the ".specdev" object for ioctl's */

	error = mfs_makespecnode(MFS_SDEVCLAS, (VNODE_T *)NULL,
			(void *)NULL, vfsp, &svp);
	if (!error) {
	    /* Add to view root dir.  Use makespecnode hold count for
	       this ptr, no extra one needed. */
	    (void) mfs_ramdir_add(rtvp, MVFS_SPECDEV, svp, &num);
	}

        /* Now set up the flags and root vnode in the mount point. */
   
        mmi->mmi_rootvp = rtvp; 
   
	vrdp->mfs_viewroot_vfsp = vfsp; /* Record vfsp of viewroot */
	vrdp->mfs_viewroot_vp = rtvp; /* Record vp of viewroot (no hold count) */
        vrdp->mfs_viewroot_specvp = svp; /* Record vp of .specdev (no hold count) */

	goto out;	/* Std cleanup and return */
    }
    if (vrdp->mfs_viewroot_vfsp == NULL) {
	/* viewroot not mounted--this will screw up minor number allocation
	   policy for exported views.  We must error out. */
	/* FIXME:  what error code? */
	error = EINVAL;		
	goto errout;
    }

    /*
     * VOB MOUNT - fill in fields only associated with VOB mounts
     */

    /* 
     * Fill in vob-tag name.  
     * NOTE:  this is a pointer in the mntpath name, and so
     * is not independently freed in unmount.
     */

    if (mmi->mmi_mntpath) {
	mmi->mmi_vobtag = MVFS_FIND_LEAF(mmi->mmi_mntpath);
	/*
	 * Don't allow duplicate MVFS mounts at the same "pathname"
 	 * On Unix 
	 *	- While "mount on mount" might theoretically work, I think
	 *	  it is likely to cause trouble, and certainly confusing to have
	 *	  to think about all the time... so its good to be sure it
	 *        isn't happening.  (Think about looking up ".." at a vob root
	 *	  that has 2 or more vob roots mounted at the same point to make
	 *	  your head hurt).
	 */
	if (mfs_findvfs_mntpath(mmi->mmi_mntpath) != NULL) {
	    error = EEXIST;
	    goto errout;
	}
    } else {
	mmi->mmi_vobtag = NULL;
    }

    /* VOB MOUNT - Fill in vob-only portions of mmi_svr struct */

    MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).s = NULL;
    MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).l = 0;
    MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).m = 0;
    error = mfs_copyin_strbufpn(MFS_STRBUFPN_PAIR_GET_KPN(&mmap->mma_spath),
				&MFS_STRBUFPN_PAIR_GET_KPN(&mmi->mmi_svr.lpn).s);
    if (error) goto errout;

    MFS_STRBUFPN_PAIR_GET_KPN(&mmi->mmi_svr.lpn).l =
	STRLEN(MFS_STRBUFPN_PAIR_GET_KPN(&mmi->mmi_svr.lpn).s);
    MFS_STRBUFPN_PAIR_GET_KPN(&mmi->mmi_svr.lpn).m =
	MFS_STRBUFPN_PAIR_GET_KPN(&mmi->mmi_svr.lpn).l + 1;
    if ((MFS_STRBUFPN_PAIR_GET_UPN(&mmap->mma_spath).s != NULL) &&
                (MFS_STRBUFPN_PAIR_GET_UPN(&mmap->mma_spath).s !=
		 MFS_STRBUFPN_PAIR_GET_KPN(&mmap->mma_spath).s)) {
	error = mfs_copyin_strbufpn(MFS_STRBUFPN_PAIR_GET_UPN(&mmap->mma_spath),
				    &MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).s);
	if (error) goto errout;
	MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).l =
	    STRLEN(MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).s);
	MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).m =
	    MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).l + 1;
    }
    error = mfs_copyin_strbuf(mmap->mma_rpath, &mmi->mmi_svr.rpn);
    if (error) goto errout;
    mmi->mmi_svr.uuid = mmap->mma_replica_uuid;

    if (mmap->mma_sptab_cnt) {
	mvfs_log(MFS_LOG_DEBUG, "copyin sptab cnt %d\n", mmap->mma_sptab_cnt);
	/*
	 * copy in, translate, and verify the format of pathname
	 * translation items.
	 */
	mmi->mmi_sptable_len = mmap->mma_sptab_cnt;
	mmi->mmi_sptable = (mvfs_sp_ent_t *)
	    KMEM_ALLOC(mmi->mmi_sptable_len * sizeof(*mmi->mmi_sptable),
		       KM_SLEEP);
	if (mmi->mmi_sptable == NULL) {
            error = ENOMEM;
            goto errout;
        }
	BZERO(mmi->mmi_sptable,
	      mmi->mmi_sptable_len * sizeof(*mmi->mmi_sptable));
	for (i = 0; i < mmap->mma_sptab_cnt; i++) {
	    error = CopyInMvfs_splitpool_index((caddr_t)mmap->mma_sptab_ents, 
	            splitpoolp, i, callinfo);

	    if (error)
		goto errout;
	    error = mfs_copyin_strbuf(splitpoolp->msp_prefix,
				      &mmi->mmi_sptable[i].sp_prefix);
	    if (error)
		goto errout;
	    mmi->mmi_sptable[i].sp_prlen =
		STRLEN(mmi->mmi_sptable[i].sp_prefix);
	    if (mmi->mmi_sptable[i].sp_prefix[mmi->mmi_sptable[i].sp_prlen-1]
		!= MVFS_PN_SEP_CHAR) {
		error = EINVAL;		/* prefix must end in sep char */
		goto errout;
	    }
	    /* Copy in kernel and user versions of the replacement */
	    error = mfs_copyin_strbufpn(MFS_STRBUFPN_PAIR_GET_KPN(&splitpoolp->msp_target),
					 &mmi->mmi_sptable[i].sp_target);
	    if (error)
		goto errout;
	    mmi->mmi_sptable[i].sp_tglen =
		STRLEN(mmi->mmi_sptable[i].sp_target);
	    error = mfs_copyin_strbufpn(MFS_STRBUFPN_PAIR_GET_UPN(&splitpoolp->msp_target),
					 &mmi->mmi_sptable[i].sp_usertarget);
	    if (error)
		goto errout;
	    mmi->mmi_sptable[i].sp_usrlen =
		STRLEN(mmi->mmi_sptable[i].sp_usertarget);
	    if (mmi->mmi_sptable[i].sp_target[mmi->mmi_sptable[i].sp_tglen-1]
		!= MVFS_PN_SEP_CHAR ||
		mmi->mmi_sptable[i].sp_usertarget[mmi->mmi_sptable[i].sp_usrlen-1]
		!= MVFS_PN_SEP_CHAR) {
		error = EINVAL;		/* replacement must end in sep char */
		goto errout;
	    }
	}
    }
    /* Fill in vob-only vob subdir and vob-oid args */

    error = mfs_copyin_strbuf(mmap->mma_vob_subdir, &mmi->mmi_vobdir);
    if (error) goto errout;
    mmi->mmi_voboid = mmap->mma_vob_oid; 
    mmi->mmi_vobuuid = mmap->mma_replica_uuid;

    /* Fill in other vob-mount paramters (e.g. caches) */

    mmi->mmi_noac = ((mmap->mma_flags & MFSMNT_NOAC) != 0);
    mmi->mmi_nodnlc = ((mmap->mma_flags & MFSMNT_NODNLC) != 0);
    mmi->mmi_ac_regmax = mmap->mma_ac_regmax;
    mmi->mmi_ac_regmin = mmap->mma_ac_regmin;
    mmi->mmi_ac_dirmax = mmap->mma_ac_dirmax;
    mmi->mmi_ac_dirmin = mmap->mma_ac_dirmin;

    /* Other defaults if not set in mount args */

    if (mmi->mmi_ac_regmax == 0) mmi->mmi_ac_regmax = 60;
    if (mmi->mmi_ac_regmin == 0) mmi->mmi_ac_regmin = 3;
    if (mmi->mmi_ac_dirmax == 0) mmi->mmi_ac_dirmax = 60;
    if (mmi->mmi_ac_dirmin == 0) mmi->mmi_ac_dirmin = 30;

    /* Initialize dir attribute cache flush time */
    mmi->mmi_ac_dir_ftime = 0;

    /* no need to lock mmi_refcnt here as noone else can be looking at it */
    mmi->mmi_refcnt = 0;
    if (mmi->mmi_minor == -1) {
	mmi->mmi_minor = mvfs_vfsgetnum(&mvfs_minormap, MVFS_MINMAPSIZE);
	if (mmi->mmi_minor < 0) {
	    error = ENFILE;
	    goto errout;
	}
	mmi->mmi_nvminor = mvfs_vfsgetnum(&mvfs_minormap, MVFS_MINMAPSIZE);
	if (mmi->mmi_nvminor < 0) {
	    error = ENFILE;
	    goto errout;
	}
    }
    if ((error = mvfs_find_devnum(mmi->mmi_nvminor, &major_num, &minor_num)) != 0)
        goto errout;
    MAKEFSID(mmi->mmi_nvfsid, major_num, minor_num);
    mmi->mmi_gen = mfs_mntgen;
    mmi->mmi_rdev = 0;

    /* Fill in the vfs struct for the MFS mount */

    vfsp->vfs_bsize = DEV_BSIZE;
    vfsp->vfs_flag |= (vrdp->mfs_viewroot_vfsp->vfs_flag & MVFS_VFS_NOSUID);
    if ((mmap->mma_flags & MFSMNT_RDONLY) != 0)
        vfsp->vfs_flag |= VFS_RDONLY;

    MVFS_SET_FSTYPE(vfsp);
    vfsp->vfs_data = (MVFS_VFSDATA_T)mmi;
    if ((error = mvfs_find_devnum(mmi->mmi_minor, &major_num, &minor_num)) != 0)
        goto errout;
    MAKEFSID(VFS_FSID(vfsp), major_num, minor_num);
    if ((error = mvfs_find_devnum(mmi->mmi_nvminor, &nvmajor_num, &nvminor_num)) != 0)
        goto errout;
    /* nvminor is returned to VOP_GETATTR() for view-less vob root;
       use it for what we put in the vfs_dev slot (but let the mdep
       implementation choose if it needs to put something else there,
       e.g. Linux). */
    MAKEVFSDEV(vfsp, major_num, minor_num, nvmajor_num, nvminor_num);

    /* The root vnode for the MFS mount is made with
     * a canned fid (inumber).  The actual vob object
     * which the mount point represents may be different
     * in different views (a different version of the dir).
     * As a result, the actual binding to the correct vob/view
     * object can not be done until the user references the mount
     * point w/ a view context.
     */
    
    error = mfs_makevobrtnode(NULL, vfsp, &rtvp);
    if (error) goto errout;

    /* We have made an inode.  Nothing must allow us to fail now
       or we will orphan an inode in the inode cache with a garbage
       back pointer to a mount point. */

    /* Now set up the flags and root vnode in the mount point. */
    
    MVFS_SET_VROOT(rtvp);
    mmi->mmi_rootvp = rtvp;	/* MFS root vnode */

    /*
     * Add this VFS to the table of vob mounts.
     */
    mvdp->mfs_vobmounts[mmi->mmi_minor] = vfsp;
    mvdp->mfs_vobmount_hwm = (mmi->mmi_minor > mvdp->mfs_vobmount_hwm) ? 
		mmi->mmi_minor : mvdp->mfs_vobmount_hwm;

    /* Cleanup (if error) and return */

errout:
out:
    MDB_VFSLOG((MFS_VMOUNT,"rtvp=%"KS_FMT_PTR_T" devpath=%"KS_FMT_PTR_T", mntpath=%s, vfsp=%"KS_FMT_PTR_T", flags=%lx, err=%d\n",
                rtvp, path,
                (mmi && mmi->mmi_mntpath) ? mmi->mmi_mntpath : "", vfsp,
                vfsp->vfs_flag, error));

    if (error) {
        if (mmi) {
            FREESPLOCK(mmi->mmi_rclock);
	    mfs_svrdestroy(&mmi->mmi_svr);
	    if (mmi->mmi_mntpath) STRFREE(mmi->mmi_mntpath);
	    if (mmi->mmi_vobdir) STRFREE(mmi->mmi_vobdir);
	    if (mmi->mmi_hmsuffix) STRFREE(mmi->mmi_hmsuffix);
	    if (mmi->mmi_hmvers_nm) STRFREE(mmi->mmi_hmvers_nm);
	    if (mmi->mmi_sptable) {
		for (i = 0; i < mmi->mmi_sptable_len; i++) {
		    if (mmi->mmi_sptable[i].sp_prefix)
			STRFREE(mmi->mmi_sptable[i].sp_prefix);
		    if (mmi->mmi_sptable[i].sp_target)
			STRFREE(mmi->mmi_sptable[i].sp_target);
		    if (mmi->mmi_sptable[i].sp_usertarget)
			STRFREE(mmi->mmi_sptable[i].sp_usertarget);
		}		    
		KMEM_FREE(mmi->mmi_sptable, mmi->mmi_sptable_len * sizeof(*mmi->mmi_sptable));
	    }
	    if (mmi->mmi_minor >= 0)
    		mvfs_vfsputnum(&mvfs_minormap, MVFS_MINMAPSIZE,
			       mmi->mmi_minor);
	    if (mmi->mmi_nvminor >= 0)
    		mvfs_vfsputnum(&mvfs_minormap, MVFS_MINMAPSIZE,
			       mmi->mmi_nvminor);
	    KMEM_FREE(mmi, sizeof(struct mfs_mntinfo));
	}
	/* Must never get here with a rt vnode ptr.  The iput
	   to release it would not destroy it, just leave it cached.
	   I need a call that will really destroy the vnode. */
	ASSERT(rtvp == NULL);

        if (saved_largeinit)
            mcdp->mvfs_largeinit = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_LARGEINIT];
    } else {
        mvdp->mvfs_mount_count++;
    }

    MVFS_UNLOCK(&(mvdp->mvfs_mountlock));
    if (mvdp->mvfs_mount_count == 0) {
        if (mvfs_misc_init_done) mvfs_misc_free();
    }
    MVFS_UNLOCK(&mfs_unload_lock);
    KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
    return (error);
}

/*
 * mfs_vunmount vfsop
 */
int
mfs_vunmount(vfsp, acred)
VFS_T *vfsp;
CRED_T *acred;		/* Cred may be NULL from pre-SVR4 wrappers */
{
    VNODE_T *rtvp;		/* Root vnode ptr */
    struct mfs_mntinfo *mmi;	/* Mount info */
    int error;
    int count;
    int refcnt;
    SPL_T s;
    mvfs_vfs_data_t *mvdp;
    mvfs_viewroot_data_t *vrdp;
    mvfs_common_data_t *mcdp;

    /* Lock vs. load/unload */

    if (mvfs_init_state != MVFS_INIT_COMPLETE) return(EIO);

    MVFS_LOCK(&mfs_unload_lock);
    mvdp = MDKI_VFS_GET_DATAP();
    vrdp = MDKI_VIEWROOT_GET_DATAP();
    mcdp = MDKI_COMMON_GET_DATAP();

    MVFS_LOCK(&(mvdp->mvfs_mountlock));

    BUMPSTAT(mfs_vfsopcnt[MFS_VUMOUNT], s);

    mmi = VFS_TO_MMI(vfsp);

    MDB_VFSLOG((MFS_VUMOUNT,"vfsp=%"KS_FMT_PTR_T" rtvp=%"KS_FMT_PTR_T" mntpath=%s\n",
                vfsp, mmi->mmi_rootvp, mmi->mmi_mntpath));

    /* 
     * The refcnt check on the viewsvr mount is relatively
     * complicated.  We must first uncache all hold counts
     * on views (from any vob, the name cache, or loopback
     * objects), and then check that every view has
     * exactly 1 refcount left on it, and the total 
     * refcnts on the mount is only the contents of the /view dir.
     * If it passes these checks, then the unmount should be
     * able to succeed and so it is OK to destroy the contents 
     * of the /view dir.
     */

    if (MFS_ISVIEWSVRMNT(mmi)) {
	mfs_dncflush();		/* Flush name cache - it HOLDS views */
	mfs_mnflush();		/* Flush all vobs, viewsvr mount */

	/* 
         * Check that each view has only 1 ref, and get total count
         * of objects.
         */
	
	error = mfs_ramdir_refchk(mmi->mmi_rootvp, &count);
        if (error) goto errout;
	SPLOCK(mmi->mmi_rclock, s);
	if (mmi->mmi_refcnt > count+1) {	/* +1 ref on mount point */
            refcnt = mmi->mmi_refcnt;
	    SPUNLOCK(mmi->mmi_rclock, s);
            mvfs_log(MFS_LOG_ERR, "umount: ref check failed: refcnt=%d expected %d\n", refcnt, count+3);
            error = EBUSY;
	    goto errout;
	}
	SPUNLOCK(mmi->mmi_rclock, s);

	if (V_COUNT(mmi->mmi_rootvp) > MVFS_UMOUNT_EXPECTED_REFCHK) {
	    mvfs_log(MFS_LOG_ERR, "umount: ref check on viewrootvp %"KS_FMT_PTR_T" failed: refcnt=%d expected %d\n", mmi->mmi_rootvp, V_COUNT(mmi->mmi_rootvp), MVFS_UMOUNT_EXPECTED_REFCHK);
	    error = EBUSY;
	    goto errout;
   	}

	/* 
         * Passes all refcnt checks - now PURGE the contents
         * of the RAMDIR.
         */
	mfs_ramdir_purge(mmi->mmi_rootvp, RELEASE);
    }
	
    /* 
     * Flush the name cache.  This has VFS pointers in it
     * which may be invalid after this unmount.
     */

    mvfs_dnc_flushvfs(vfsp);

    /* Invalidate cached mnodes */

    mfs_mnflushvfs(vfsp);	/* Uncache all idle vnodes */

    /* Make sure the mount point isn't busy. */

    SPLOCK(mmi->mmi_rclock, s);
    if (mmi->mmi_refcnt > 1) {
        refcnt = mmi->mmi_refcnt;
	SPUNLOCK(mmi->mmi_rclock, s);
        mvfs_log(MFS_LOG_ERR, "umount: mmi ref check on %s failed: refcnt=%d expected 1\n", mmi->mmi_mntpath, refcnt);
        error = EBUSY;
	goto errout;
    } 
    SPUNLOCK(mmi->mmi_rclock, s);

    rtvp = mmi->mmi_rootvp;
    if (V_COUNT(rtvp) != MVFS_UMOUNT_BUSY_REFCHK) {
	mvfs_log(MFS_LOG_ERR, "umount: ref check on %s rootvp %"KS_FMT_PTR_T" failed: refcnt=%d expected %d\n", mmi->mmi_mntpath, rtvp, V_COUNT(rtvp), MVFS_UMOUNT_BUSY_REFCHK);
        error = EBUSY;
	goto errout;
    }

    /* 
     * Free up and release mfs root inode.  Note that since VN_RELE
     * may decide to "cache" the vnode, we must force it out ourselves
     * by flushing the mnode cache again.
     */

    /*
     * If we are a VOB mount, some platforms (e.g. Linux) will need
     * the core code to reduce the root vnode's link count so that the
     * inode is properly freed.  This is taken care of in
     * mfs_ramdir_purge for the view root.
     */
    if (!MFS_ISVIEWSVRMNT(mmi)) {
        MVFS_DECREMENT_LINK(rtvp);
    }

    VN_RELE(rtvp);
    mmi->mmi_rootvp = NULL;
    mfs_mnflushvfs(vfsp);	/* Could be left in freelist */

    /* Release the minor device(s) allocated for this mount
       and increment the mount generation number so caches
       will not be confused if anyone reuses this minor
       dev in an fsid. */

    mvfs_vfsputnum(&mvfs_minormap, MVFS_MINMAPSIZE, mmi->mmi_minor);
    if (mmi->mmi_nvminor >= 0)
	mvfs_vfsputnum(&mvfs_minormap, MVFS_MINMAPSIZE, mmi->mmi_nvminor);

    mfs_mntgen++;                       /* covered by mfs_unload_lock */

    /* Free up and release allocated structs and pointers */

    if (MFS_ISVIEWSVRMNT(mmi)) {
	vrdp->mfs_viewroot_vfsp = NULL;
	vrdp->mfs_viewroot_vp = NULL;
        vrdp->mfs_viewroot_specvp = NULL;
        /*
         * Close the logfile here (when unmounting /view) since we may
         * need a viewroot in order to call some of the log file
         * internal functions (e.g. MVOP_WRITE_KERNEL())
         */
        mvfs_logfile_close();
    } else {
        mvdp->mfs_vobmounts[mmi->mmi_minor] = NULL;	/* Clear vfs table */
    }

    mfs_svrdestroy(&mmi->mmi_svr);
    if (mmi->mmi_mntpath) STRFREE(mmi->mmi_mntpath);
    if (mmi->mmi_vobdir) STRFREE(mmi->mmi_vobdir);
    if (mmi->mmi_hmsuffix) STRFREE(mmi->mmi_hmsuffix);
    if (mmi->mmi_hmvers_nm) STRFREE(mmi->mmi_hmvers_nm);
    if (mmi->mmi_sptable) {
	for (count = 0; count < mmi->mmi_sptable_len; count++) {
	    if (mmi->mmi_sptable[count].sp_prefix)
		STRFREE(mmi->mmi_sptable[count].sp_prefix);
	    if (mmi->mmi_sptable[count].sp_target)
		STRFREE(mmi->mmi_sptable[count].sp_target);
	    if (mmi->mmi_sptable[count].sp_usertarget)
		STRFREE(mmi->mmi_sptable[count].sp_usertarget);
	}		    
	KMEM_FREE(mmi->mmi_sptable, mmi->mmi_sptable_len * sizeof(*mmi->mmi_sptable));
    }

    /* Clean up mount table entry */

    MVFS_REMOVE_VFS_DATA(vfsp, mmi);
    FREESPLOCK(mmi->mmi_rclock);
    KMEM_FREE(mmi, sizeof(struct mfs_mntinfo));
    
    ASSERT(mvfs_init_state == MVFS_INIT_COMPLETE);
    if (--(mvdp->mvfs_mount_count) == 0) {
        mvfs_misc_free();
    }
    MVFS_UNLOCK(&(mvdp->mvfs_mountlock)); 
     
    if (mvdp->mvfs_mount_count == 0) {
        mvfs_init_state = MVFS_INIT_PHASE1;
#ifndef MVFS_DEBUG
        /* return logpri to default value */
        mfs_logpri = MFS_LOG_INFO;
#endif
    }

    MVFS_UNLOCK(&mfs_unload_lock);
    return(0);

    /* FIXME: any cleanup on errors? */
errout:
    MVFS_UNLOCK(&(mvdp->mvfs_mountlock));
    MVFS_UNLOCK(&mfs_unload_lock);
    return(error);
}

/*
 * Get file system statistics.
 * Note that statfs struct is a little different on FSS systems.
 */

int
mfs_vstatfs(vfsp, sbp)
register VFS_T *vfsp;
STATVFS_T *sbp;
{
    int error;
    struct mfs_mntinfo *mmi;
    CLR_VNODE_T *cvp;
    CRED_T *cred;
    SPL_T s;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if (vfsp == NULL)
	return(EINVAL);

    BUMPSTAT(mfs_vfsopcnt[MFS_VSTATFS], s);

    /* Do STATFS  for VOB/View mount. */

    mmi = VFS_TO_MMI(vfsp);

    MVFS_VSTATFS_BZERO(sbp, sizeof(*sbp));	/* Zero first ... */

    if (mmi->mmi_mnttype == MFS_VIEWSVRMNT) {
	if (vfsp == vrdp->mfs_viewroot_vfsp) {
            MDB_VFSLOG((MFS_VSTATFS, "real viewroot, using ROOTDIR\n"));
	    error = MVFS_STATFS(ROOTDIR->v_vfsp, CLR_ROOTDIR, sbp);  
	    STATVFS_FILL(sbp);
	} else {
            /*
             * Just use ROOTDIR, this platform doesn't have cloned VFSes
             * that matter for statfs reasons.
             */
            MDB_VFSLOG((MFS_VSTATFS, "cloned viewroot %"KS_FMT_PTR_T", using ROOTDIR\n",
                        vfsp));
            error = MVFS_STATFS(ROOTDIR->v_vfsp, CLR_ROOTDIR, sbp);   
	    STATVFS_FILL(sbp);
        }

    } else {
        /* Not VIEWSVRMNT */
        cred = MVFS_DUP_DEFAULT_CREDS();
  	error = LOOKUP_STORAGE_FILE(TRUE,
			MFS_STRBUFPN_PAIR_GET_KPN(&mmi->mmi_svr.lpn).s, 
			NULL, &cvp, cred);
	MDKI_CRFREE(cred);
	if (!error) {
	    error = MVFS_STATFS(MVFS_CVP_TO_VP(cvp)->v_vfsp, cvp, sbp);
	    CVN_RELE(cvp);
        }
	STATVFS_FILL(sbp);
    }

    MDB_VFSLOG((MFS_VSTATFS, "vfsp=%"KS_FMT_PTR_T" mtype=%d err=%d\n",
                vfsp, mmi->mmi_mnttype, error));
    return (error);
}

/*
 * MFS_ROOT - return held root for a vfs
 */

int
mfs_root(vfsp, vpp)
VFS_T *vfsp;
VNODE_T **vpp;
{
    struct mfs_mntinfo *mmi;
    int error;
    SPL_T s;

    mmi = VFS_TO_MMI(vfsp);

    if (mmi) {
        *vpp = mmi->mmi_rootvp;
        error = 0;          /* Optimistically assume the normal case. */
        /*
        ** If we're at the view root (/view) then don't check the state of the
        ** view so that, e.g., /view/.specdev will work.  Do the view check for
        ** vob roots (/vobs/foo).
        */
        if (mmi->mmi_mnttype == MFS_VOBMNT) {
            VNODE_T *vw;
            /*
            ** Check if someone ended the view while we are in it.  This is a
            ** lookup of a vob root, so return ESTALE, just like we do for ".."
            ** in mfs_lookup() in this same situation.  Copy some code from
            ** mfs_bindroot() and mvfs_rvclookup() to check our situation.
	    **
            ** mfs_getview() can ignore the cred arg, and we don't have one
            ** anyway, so pass in NULL.
            */
            if ((vw = mfs_getview(*vpp, NULL /* cred */, FALSE /* NO HOLD */)) != NULL) {
                if (VTOM(vw)->mn_view.id == MFS_NULLVID) {
                    *vpp = NULL;
                    error = MVFS_ESTALE; /* View is gone. */
                }
            }
        }
        if (*vpp) {
            VN_HOLD(*vpp);      /* If we're returning a vnode, hold it. */
        }
    } else {
        *vpp = NULL;
	error = ENXIO;          /* No mount info. */
    }
    MDB_VFSLOG((MFS_VROOT, "vfsp=%"KS_FMT_PTR_T" rtvp= %"KS_FMT_PTR_T"\n", vfsp, *vpp));
    BUMPSTAT(mfs_vfsopcnt[MFS_VROOT], s);
    return(error); 
}

/* Some state vars and defn for periodic maint/sync */

int mfs_do_sync = 1;
int mdb_crash = 0;

extern void mfs_prkmem(P_NONE);
/****************************************************************************
 * mfs_periodic_maintenance
 * Perform periodic maintenance tasks for the MVFS
 * No arguments
 * Returns:	no result
 *
 * Description:
 *	This routine performs the standard MVFS periodic maintenance
 *	that is not related to any mountpoint. The tasks this routine
 *	performs are:
 *	- Panics the system if the internal flag 'mdb_crash' goes
 *	  to zero (this is a countdown counter do allow for setting
 *	  it with a time-delay).  Normally, it is just set to
 *	  1 with a kernel debugger, and a system panic happens in the
 *	  next 1 minute.  For some deadlocks, it may not be possible
 *	  to run a kernel debugger after the deadlock has happened,
 *	  so this flag can be set to a large value (i.e. 60) and
 *	  then the test that deadlocks is run, and then you wait 60 minutes
 *	  for it to panic with the deadlock case.
 *	- Optionally does a scan for internal consistency of the mnode
 *	  database i.e. the call to MDB_CHKPOINT(1)
 *	- Displays the heap on the console if KMEM tracking is on and 
 *	  the flag is set to print the heap (used for debug of memory leaks)
 *	- Call mfs_procpurge() to garbage collect mfs proc structs
 *	  for any dead processes.
 *	- Calls mfs_vwdircleanhm() to timeout any history mode 
 *	  view-tags we created dynamically after a suitable time-period.
 */

void
mfs_periodic_maintenance(P_NONE)
{
    VNODE_T *rootvp;
    mvfs_viewroot_data_t *vrdp;
    mvfs_common_data_t *zmcdp;  
    int z;

    /* Nothing to do if not initialized */

    if (mvfs_init_state != MVFS_INIT_COMPLETE) {
        return;
    }

    /* 
     * Exit quickly on panic to avoid hangs that prevent
     * dumps from happening.
     */

    if (MDKI_PANICSTR()) return;

    /* 
     * Check for debug crash requested.
     * The only way to set this flag is for 'root'
     * to patch the kernel with a debugger.
     * mdb_crash can be used for "delayed" crashes
     * by setting it to a value greater than 1;
     * it will be decremented on each sync and
     * panic on the transition to 0.
     */

    if ((mdb_crash != 0) && (--mdb_crash == 0)) MDKI_PANIC("MVFS debug crash");

    MDB_CHKPOINT(1);    /* debug scan in background */

    MFS_PRKMEM();		/* Display heap if flag set && using mvfs kmem */

    mvfs_procpurge(MVFS_PROCPURGE_NOSLEEP); /* Clean up dead processes */
    MVFS_FLUSH_CREDLIST(FALSE);

    vrdp = MDKI_VIEWROOT_GET_DATAP();
    if (vrdp->mfs_viewroot_vfsp) { 		/* Clean up stale HM views */
	rootvp = vrdp->mfs_viewroot_vp;
	VN_HOLD(rootvp);
	/* FIXME: needs nowait option! */
	mfs_viewdircleanhm(rootvp);
	VN_RELE(rootvp);
    }

 
    return;
}

 
/* 
 * MFS_SYNC - vnode version of sync call.
 * This is called every 30 secs or minute.  Use it for background cleanup
 * work.  Note that this is called with vfsp = NULL to indicate all
 * the vfs's should be cleaned up.
 *
 */

int
mfs_vsync(VFS_T *vfsp,
	  short flag,
	  CRED_T *acred)	/* acred may be NULL from pre-SVR4 wrappers */
{

    SPL_T s;
    mvfs_viewroot_data_t *vrdp = NULL;

    if (mvfs_init_state != MVFS_INIT_COMPLETE) {
        return(0);
    }

    /* Exit quickly on panic to avoid hangs that prevent
     * dumps from happening.
     */

    if (MDKI_PANICSTR()) return(0);

    /* Do periodic maintenance functions off the sync
     * operations on Unix.  That way we don't have to
     * worry about creating system threads or deferred 
     * procedure calls in the Unix kernel.
     * (Callouts via timeout() usually happen at an
     * elevated interrupt priority level at which the
     * periodic maintenance operations can not be done,
     * so we can't just use the timeout() mechanism in the
     * kernel).
     */
    vrdp = MDKI_VIEWROOT_GET_DATAP();
    if (vrdp != NULL) {
        if (vfsp == NULL || vrdp->mfs_viewroot_vfsp == NULL || 
                                        vfsp == vrdp->mfs_viewroot_vfsp)
	    /*
	     * Only do this once per sync call.  Some systems call us only once
	     * with a null vfsp.  Other systems call us for each vfs, so only do
	     * this once if the viewroot is mounted (and once in the odd case
	     * that the viewroot isn't mounted).
	     */
	    mfs_periodic_maintenance();
    }

    /* Log first, usually have to debug a hang here ... */

    MDB_VFSLOG((MFS_VSYNC, 
		"vfsp=%"KS_FMT_PTR_T" time= %"MVFS_FMT_CTIME_X"\n", 
		vfsp, MDKI_CTIME()));
    BUMPSTAT(mfs_vfsopcnt[MFS_VSYNC], s);

    /* 
     * Sync any "dirty" mnodes (vob mnodes with cached pages) 
     */
    if (mfs_do_sync)
	mvfs_mnsyncmnodes(vfsp);

    return(0);
}

/*
 * MFS_GETMNODE - find a VOB mnode from a view vnode and internal fid
 *
 * This routine leaves the MNODE REFCOUNT INCREMENTED AND RETURNS
 * with the mnode still locked!.  Callers of
 * this routine are responsible for calling mfs_mnrele when done
 * with the mnode, or when the vnode refcount has been successfully
 * incremented.
 */

int
mfs_getmnode(vfsp, vw, fidp, mnpp, nnp, cred)
VFS_T *vfsp;
VNODE_T *vw;
mfs_fid_t *fidp;
mfs_mnode_t **mnpp;
int *nnp;		/* Newnode flag ptr */
CRED_T *cred;	/* May be NULL */
{
    int error = 0;
    mfs_mnode_t *mnp;
    SPL_T s;

    /* Could we get a vnode's vp->v_vfsp which is a "local" file
     * system VFS type by accident, e.g. on systems which manufacture
     * LOOPCLAS vnodes with vp->v_vfsp pointing at the real vfsp, but
     * vp->v_ops pointing at MVFS?  ASSERT that such a case isn't
     * showing up in this routine.
     */
    ASSERT(MVFS_VFSISMVFS(vfsp, mfs_vfsopp));
    if (MFS_ISVIEWSVRMNT(VFS_TO_MMI(vfsp))) {
        /* Never allow viewroot nodes back to NFS daemon (the only
           caller). */
        error = ESTALE;
        mnp = NULL;
    } else { 
        if (fidp->mf_dbid == MVFS_ROOTDBID) {
	    if (fidp->mf_gen != MVFS_ROOTGEN) {
		error = ESTALE;
		mnp = 0;
	    } else {
		mnp = mfs_mnget(MFS_VOBRTCLAS, vw, fidp, vfsp, nnp);
		if (!mnp) error = ESTALE;
	    }
	} else {
	    if (vw != NULL) {
	        mnp = mfs_mnget(MFS_VOBCLAS, vw, fidp, vfsp, nnp);
		if (!mnp) error = ESTALE;
	
	        /* New VOB node, must get info via RPC to fill it in */

	        if (!error && *nnp) {

		    /* Make partial view file handle */

		    MFS_FID_TO_PARTIAL_VFH(*fidp, vfsp, vw, mnp->mn_vob.vfh);

		    /* 
		     * Get attributes (and full vfh) from view 
		     * For non-atria access, the credentials may
		     * be NULL because the vget() op does not have
		     * credentials as an argument.  In addition, the process
		     * and/or thread credentials may not be set to the
		     * actual user, but be left at sys_cred.
		     * This can create problems if the view ever did an
		     * access check on the creds for a getattr!
                     */

		    if (cred == NULL) {
		        cred = MVFS_DUP_DEFAULT_CREDS();
	    	        error = mfs_clnt_getattr_mnp(mnp, vfsp, cred);
	                MDKI_CRFREE(cred);
		        cred = NULL;
		    } else {
		        error = mfs_clnt_getattr_mnp(mnp, vfsp, cred);
		    }
		    BUMPSTAT(mfs_acstat.ac_misses, s);
		    BUMPSTAT(mfs_acstat.ac_newmiss, s);

		    /* 
		     * If no error, and only have partial fid, promote vfh gen
                     * back up to fid. (VFH was filled in by clnt_getattr) 
		     * If there is an error, then mark this mnode "stale"
                     * and clear the fid so it won't be reused with bad
                     * data in it, and then free it up and return a NULL
                     * mnode ptr to the client.  Don't forget to unlock!
		     */

	 	    if (!error) {
			if (mnp->mn_hdr.fid.mf_gen == 0)
		            mnp->mn_hdr.fid.mf_gen = mnp->mn_vob.vfh.gen;
		    } else {
			mnp->mn_hdr.stale = 1;
			
			mnp->mn_hdr.fid.mf_gen = MFS_NULL_GEN;
			MUNLOCK(mnp);
			mfs_mnrele(mnp, vfsp);
			mnp = NULL;
		    }
		}	/* if (*nnp) ... */
	    } else {	/* vw != NULL... else ...  */
	        mnp = NULL;
		error = ESRCH;
	    }
	}		/* fidp == ROOTDBID... else ... */
    }			/* VIEWSVRMNT ... else ... */

    /* NOTE: returns with mnode locked, and mnode refcount incremented. */
    *mnpp = mnp;
    return(error);
}

int
mfs_getvnode(vfsp, vw, fidp, vpp, cred)
VFS_T *vfsp;
VNODE_T *vw;
mfs_fid_t *fidp;
VNODE_T **vpp;
CRED_T *cred;
{
    int error = 0;
    mfs_mnode_t *mnp;
    VNODE_T *vp = NULL;
    int newnode;
    VTYPE_T vtype;

    /*
     * Get the mnode from the FID.  Note that this returns with
     * mnode locked and mnode refcount incremented.
     */

    /*
     * All internal code is calling this in a VOB context (vfsp is for
     * a VOB mount).  NFS server might call it with a viewroot, which
     * we don't support, but let getmnode bounce that.
     */
    error = mfs_getmnode(vfsp, vw, fidp, &mnp, &newnode, cred);
    if (!error) {
	ASSERT(mnp->mn_hdr.mcount > 0);
    }

    /*
     * Get the vnode.  
     * See mfs_makevobnode in vnodeops for a discussion of mnode and
     * vnode refcounts and how they are used.  This routine
     * decrements mnode refcount (if appropriate) and unlocks the
     * mnode.
     */

    if (!error) {
        error = MVFS_VNGET(vfsp, NULL, mnp, &vp);
    }

    *vpp = vp;
    return(error);
}

/*
 * MFS_UUID_TO_HASH32 - utility routine to hash a uuid
 * Really a copy of tbs_uuid_to_hash32, but for the kernel.
 */

u_long
mfs_uuid_to_hash32(uuid_p)
A_CONST tbs_uuid_t *uuid_p;
{
    ks_uint32_t val;
    /* note: only the high part of the low time varies greatly */
    /* -- and then only when time has past */
    val = (uuid_p->time_low >> 2) + (uuid_p->time_low >> 22) +
            ((uuid_p->time_mid + uuid_p->time_hi_and_version
            + uuid_p->clock_seq_low
            + (uuid_p->clock_seq_hi_and_reserved << 26)
            + (uuid_p->node[5] << 20) + (uuid_p->node[4] << 16)
            + (uuid_p->node[3] << 12) + (uuid_p->node[2] << 8)
            + (uuid_p->node[1] << 4) + uuid_p->node[0])
        * 536870909); /* large prime */
    return (long) (val & 0xFFFFFFFF);		/* mask to 32 bits */
}

/*
 * MFS_VGET - find a vnode from its external (NFS) fid
 */

int
mfs_vget(vfsp, vpp, xfidp)
VFS_T *vfsp;
VNODE_T **vpp;
struct fid *xfidp;
{
    int error = 0;
    mfs_xfid_t *mxfidp;
    mfs_fid_t fid;
    VNODE_T *vw;
    SPL_T s;
    mfs_mnode_t *mnp;
    int exportid;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    /* If not a vob mount return EINVAL */

    if (MFS_ISVIEWSVRMNT(VFS_TO_MMI(vfsp))) {
	return(EINVAL);
    }

    /* Cast to mfs fid structure */

    mxfidp = (mfs_xfid_t *)xfidp;

    /* Get view if a viewroot vfs exists and valid view id */

    if (vrdp->mfs_viewroot_vfsp && mxfidp->mfx_vid != MFS_NULLVID) {
	mnp = VTOM(vrdp->mfs_viewroot_vp);
	MLOCK(mnp);
	if (mxfidp->mfx_vid < (unsigned) mnp->mn_ramdir.export_hwm &&
	    mnp->mn_ramdir.export_ents[mxfidp->mfx_vid] != -1) {
	    exportid = mnp->mn_ramdir.export_ents[mxfidp->mfx_vid];
	    vw = mnp->mn_ramdir.ents[exportid].vp;
            /* Hold vw in case someone else releases it from
               ramdir/export table */
            VN_HOLD(vw);
	} else
	    vw = NULL;
	MUNLOCK(mnp);
    } else {
	vw = NULL;
    }

    /* Build normal fid from rest of export fid */
  
    fid.mf_dbid = mxfidp->mfx_dbid;
    fid.mf_gen  = mxfidp->mfx_gen;

    /* Restore real gen by subtracting the view uuid hash
     * we added in on the fetch of the fid.  If the view-id
     * has accidently hooked us back up to the wrong view,
     * then this will create a bogus generation number and
     * we will get ESTALE.
     */

    if (vw) {
	fid.mf_gen -= 
	    mfs_uuid_to_hash32(&(VTOM(vw)->mn_view.svr.uuid));
    }
    /*
     * also subtract out VOB uuid hash (needed to prevent same view/different
     * VOB confusion at the unbound VOB root)
     */
    fid.mf_gen -= mfs_uuid_to_hash32(&(VFS_TO_MMI(vfsp)->mmi_svr.uuid));

    error = mfs_getvnode(vfsp, vw, &fid, vpp, NULL);
    if (vw != NULL) {
        VN_RELE(vw);
    }
    /* 
     * I would like to fetch the cleartext here for non-atria
     * access, but unfortuneately, this op has no credentials
     * associated with it for the lookup.
     * Thus the getcleartext has to be deferred to read/write/getattr
     * ops (as required depending on system implementation).
     *
     */

    MDB_VFSLOG((MFS_VGET, "vfsp=%"KS_FMT_PTR_T" *vpp=%"KS_FMT_PTR_T", fid=%x/%x/%x/%x err=%d\n", vfsp,
		*vpp, ((int *)mxfidp)[0], ((int *)mxfidp)[1],
		((int *)mxfidp)[2], ((int *)mxfidp)[3], error));
    BUMPSTAT(mfs_vfsopcnt[MFS_VGET], s);
    MDB_CHKPOINT(0);
    return(error);
}

int
mfs_nosys(void)
{
    mvfs_log(MFS_LOG_WARN, "unimplemented vfs or vnode op\n");
    return(ENOSYS);
}

int
mfs_swapvp(void)
{
    mvfs_log(MFS_LOG_WARN, "unimplemented swapvp vfs op\n");
    return(ENOSYS);
}

/* Allocate a minor device number */

int
mvfs_vfsgetnum(map, size)
MVFS_MINMAP_T *map;
int size;
{
    u_char *p;
    u_char *ep;
    unsigned val;
    int i;
    SPL_T s;

    SPLOCK(map->lock, s);
    for (p = &map->vec[0], ep = &map->vec[size]; p < ep; p++) {
	if (*p != (u_char)(~0)) {
	    val = *p;
	    for (i = 0; i < NBBY; i++) {
		if ((val & (1 << i)) == 0) {
		    *p |= (1 << i);
		    SPUNLOCK(map->lock, s);
		    return(int)(((p - (&map->vec[0])) * NBBY) + i);
		}
	    }
	    MDKI_PANIC("mvfs: mfsvfsgetnum: bad map");
	}
    }
    SPUNLOCK(map->lock, s);
    return(-1);
}

/*
 * Allocate a particular minor device number pair, returning an error
 * if not possible.
 */
int
mvfs_get_minorpair(map, size, index)
MVFS_MINMAP_T *map;
int size;
int index;
{
    u_char *p;
    unsigned val;
    int i, bitoffset;
    SPL_T s;

    if (index > size * NBBY)
	return 1;			/* out of range */
    if ((index & 1) != 0)
	return 1;			/* not an even starting index--
					 * this means we can't check both bits
					 * in same byte, yech!
					 */
    ASSERT((NBBY / 2)*2 == NBBY);	/* must be even # bits in a byte */
    SPLOCK(map->lock, s);
    p = &map->vec[index/NBBY];
    bitoffset = index % NBBY;

    if ((*p & (3 << bitoffset)) == 0) {
	/* available. */
	*p |= (3 << bitoffset);
	SPUNLOCK(map->lock, s);
	return 0;
    }
    SPUNLOCK(map->lock, s);
    return 1;				/* no room, sorry */
}

int
mvfs_vfsputnum(map, size, n)
    MVFS_MINMAP_T *map;
    int size;
    int n;
{
    int ix, off;
    SPL_T s;

    if (n >= (size * NBBY))
	MDKI_PANIC("mvfs: mfs_vfsputnum: out of range");
    ix = n / NBBY;
    off = n - (ix * NBBY);
    SPLOCK(map->lock, s);
    if ((map->vec[ix] & (1 << off)) == 0)
	MDKI_PANIC("mvfs: mfs_vfsputnum: not allocated");
    map->vec[ix] &= ~(1 << off);
    SPUNLOCK(map->lock, s);
    return(0);
}

/*
 * MFS_GETVIEWROOT - get a held vnode ptr to the viewroot
 */

VNODE_T *
mfs_getviewroot()
{
    VNODE_T *dvp;
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    MVFS_LOCK(&(mvdp->mvfs_mountlock));  /* Freeze the mount table */
    if ((dvp = vrdp->mfs_viewroot_vp) != NULL) {
        VN_HOLD(dvp);           /* Hold view root */
    }
    MVFS_UNLOCK(&(mvdp->mvfs_mountlock));
    return(dvp);
}

/*
 * MFS_GETSPECDEV - get a held vnode ptr to the specdev vp
 */
VNODE_T *
mfs_getspecdev()
{
    VNODE_T *vp;
    mvfs_vfs_data_t *mvdp = MDKI_VFS_GET_DATAP();
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    MVFS_LOCK(&(mvdp->mvfs_mountlock));	/* Freeze the mount table */
    if ((vp = vrdp->mfs_viewroot_specvp) != NULL) {
	VN_HOLD(vp);
    }
    MVFS_UNLOCK(&(mvdp->mvfs_mountlock));
    return(vp);
}

/*
 * Convert mmi_minor in struct mfs_mntinfo to the major/minor number actually
 * used by OS. mmi_minor may be larger than MVFS_MINORMAX, so we need to split
 * it into a major/minor combination.
 */
STATIC int
mvfs_find_devnum(
    int mmi_minor, 
    dev_t *majorp, 
    int *minorp
)
{
    int major_index;
    major_index = mmi_minor / MVFS_MINORMAX;
    if (mvfs_majortbl[major_index] == -1) {
        if ((mvfs_majortbl[major_index] = MDKI_GET_MAJOR_DEV()) == -1)
            return ENODEV;
    }
    *majorp = mvfs_majortbl[major_index];
    *minorp = mmi_minor % MVFS_MINORMAX;
    return 0;
}
static const char vnode_verid_mvfs_vfsops_c[] = "$Id:  f1e5f3d3.365611dd.8aaa.00:01:83:09:5e:0d $";
