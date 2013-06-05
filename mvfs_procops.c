/* * (C) Copyright IBM Corporation 1991, 2012. */
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
/* mvfs_procops.c */
#include "mvfs_systm.h"
#include "mvfs.h"

STATIC void mvfs_procinherit_copy(P1(mvfs_proc_t *to)
				  PN(mvfs_proc_t *from));
STATIC mvfs_proc_t * mvfs_myproc(P_NONE);
STATIC mvfs_thread_t * mvfs_threadalloc(P1(MVFS_THREADID_T *thrid));
STATIC void mvfs_threadfree(mvfs_thread_t *thr, tbs_boolean_t needs_dequeue);
STATIC void mvfs_threadrele(mvfs_thread_t *thr);
STATIC mvfs_proc_t * mvfs_procalloc(P1(MVFS_PROCID_T *procid)
				    PN(MVFS_PROCTAG_T *proctag));
STATIC void mvfs_procfree(P1(mvfs_proc_t *) PN(mvfs_thread_t *));
STATIC void mvfs_procfree_int(P1(mvfs_proc_t *));
STATIC void mvfs_procrele(P1(mvfs_proc_t *) PN(mvfs_thread_t *));
STATIC void mvfs_snapshot_thread(P1(mvfs_thread_t *thr));
STATIC int mvfs_purgechain(P1(int bucket) PN(int purge)
			   PN(mvfs_thread_t *mythread));
STATIC void mvfs_procpurge_afps(P_NONE);
STATIC unsigned int mvfs_pidhash(MVFS_PROCID_T *pidp);

/*
 * Routine to manipulate the per-process MVFS state (other than current
 * view which is stored as the u_rdir vnode).  
 *
 * Here's the scheme for process and thread state auditing in the MVFS:
 *
 * THREADS:
 *
 * (a) Each thread has a private mvfs_thread_t structure which
 * contains the current MVFS-specific thread state.  Nobody else ever
 * looks at this structure so there is no locking required.  The
 * thread state persists until the process exits.
 * mvfs_thread_t.thr_threadid uniquely identifies the thread to which
 * it belongs.
 *
 * (b) Each time the MVFS is entered from outside code (i.e. every
 * VOP_xxx), the thread's state is found by lookup in a hash table,
 * and refreshed from the process's state.  [see mvfs_enter_fs()] If
 * necessary, a new thread state structure is created and linked into
 * the process's chains.  If necessary, a new process state structure
 * is created, its fields inherited, and it is linked into the process
 * hash table.  New thread allocation is not protected by any lock
 * (there is no potential to race another thread, since the
 * mvfs_thread_t is thread-specific).
 * 
 * (c) When a thread sets any MVFS process state, it first updates the
 * state in its private mvfs_thread_t, then synchronizes it to the
 * process state [see mvfs_sync_procstate()]
 *
 * (d) mvfs_thread_t.thr_proc is always valid.  Process structures never
 * disappear without removing all their threads, so the thr_proc will
 * always be directly usable without validation
 *
 * (e) All mvfs_thread_t's are chained into a hash table by
 * mvfs_thread_t.thr_hashnxt.  Operations on the thread hash table
 * are protected by a pool of spin locks.
 *
 * (f) Some system threads must not block in paging contexts.  In case
 * they ever get into this code, their thread and process structures
 * are allocated from a static table which is configured based on a
 * particular port's mdep.h file.
 *
 * XXX/FIXME:
 *
 * This code used to assume that a thread never leaves its
 * originating process.  This isn't true on some systems.  There is
 * also an implicit assumption that the thread always wants to
 * synchronize its state from a process, which also isn't true if a
 * process is acting as an agent for other processes, with one thread
 * per client process.  
 *
 * We try to handle the first case in the mvfs_mythread() routine.  If
 * we detect a thread has changed processes, we treat it as a miss,
 * discard its current state, and create a new thread and attach it to
 * the new process.
 *
 * The second case is not handled at present---all threads in a given process
 * share the auditing state of the process.
 *
 * PROCESSES:
 *
 * Since the MVFS does not get called on fork/exit under UNIX, these
 * structures are 'loosely' consistent with the process table.
 * With the exception of the view vnode pointer, these structures
 * (or any they points to) should not hold any resources (except
 * memory) due to its loose consistency. Holding resources can result
 * in strange user problems because they are not freed on process exit.
 * Example: if you held a vnode on an NFS mount point, the NFS mount
 * point would unmountable, even though all processes using that
 * mount point had exited.  Very strange for administrators!
 *
 * (a) Each process has an mvfs_proc_t structure which contains the
 * latest MVFS process state.  The mvfs_proc_t contains a spinlock,
 * which must be held during any reading or writing of the structure
 * contents.  (mvfs_proc_t.mp_procid,mvfs_proc_t.mp_proctag) uniquely
 * identifies the process to which it belongs.  mp_procid is immutable
 * once an mvfs_proc_t is created and linked to a hash chain;
 * mp_proctag may be changed (under the mvfs_proc_t's spinlock)
 *
 * (b) The mvfs_proc_t is created the first time a process is
 * discovered to have entered the MVFS, and its state initialized from
 * an ancestor process. [see mvfs_procinherit]
 * 
 * (c) Threads initialize their state from the process's mvfs_proc_t,
 * using the spinlock to protect the reading of the mvfs_proc_t.
 *
 * (d) All thread structures belonging to a process are linked on a
 * singly-linked list, rooted at mvfs_proc_t.mp_threads and connected
 * by mvfs_thread_t.thr_next.
 *
 * (e) All mvfs_proc_t's are chained into a hash table by
 * mvfs_proc_t.mp_hashnxt.  The hash table is only needed to find
 * ancestor process state for inheritance and to clean up dead process
 * state; most lookups of process state come directly from the
 * mvfs_thread_t.thr_proc.  All process hash table operations are
 * protected by mvfs_proclock (a sleep lock).
 *
 * (f) When a process's state is created, the mvfs_proclock is held
 * while inheritance searching continues, both because the lock is
 * required for hash table lookups, and to insure no processes are
 * recycled while inheritance is searching.  If an ancestor is found,
 * it's data are spinlocked while the inheritance copy proceeds.
 *
 * (g) Periodically [either due to a sync() on a filesystem, which calls
 * mvfs_procpurge(), or due to an audit ioctl() operation, which calls
 * mvfs_procpurge()], the hash table is walked in search of stale
 * processes; any such have all their mvfs_thread_t's recycled and then
 * the mvfs_proc_t itself recycled.  This procedure requires the
 * mvfs_proclock to walk the hash table, and dropping the lock while
 * recycling a process.  If the caller cannot wait (for instance,
 * mfs_periodic_maintenace() calls mvfs_procpurge() with the NOSLEEP flag),
 * conditional locking is used and the operation prematurely terminated if the
 * lock cannot be acquired.
 *
 * (h) each thread holds one ref count on the process structure.  The
 * hash table link also holds one ref count.
 */
/*
 * Locking summary:
 *
 *	To look at your own thread state (mvfs_mythread()->*) you
 *	don't need any locks.
 *
 *	To do anything to any process state (proc->*), you must
 *	MVFS_LOCK(&proc->mp_lock) around read or write access.  This
 *	includes inheritance copies.
 *
 *	To search or alter the thread hash table, you must hold the
 *	chain's spin lock. (includes inserting new entries)
 *
 *	To search or alter the process hash table, you must hold the
 *	mvfs_proclock.  (includes inserting new entries--which
 *	unfortunately means that a hash table rescan at creation will
 *	be required, since allocating a new entry may do page I/O and
 *	we must drop the lock for the duration of the allocation operation)
 *
 * Other older notes on process state:
 *	Because the MVFS does not get called on UNIX when a process dies,
 *	you must be careful not to store information in the thread or process
 *	state block which would be a problem if it were retained
 *	after process death.  The call "mvfs_procpurge()" is called
 *      from sync() every minute to garbage collect any dead process
 *	structures.
 *
 *	One potentially risky area is the held vnode ptr for
 *	the audit file.  This has to be done because of the 
 *	restriction below about looking up pnames.  In general
 *	it is not a problem because these files are only active
 *	during a build, and are files in /tmp or /usr/tmp whose
 *	FS's aren't unmounted except at reboot, and the reboot
 *	sync() calls will flush any stale process state and cleanup
 *	any residual vnode ptrs from dead processes.
 *
 * Restriction of calls:
 *	Because these proc ops are called from anywhere in the code
 *	with potentially any MVFS resource held/locked, these routines
 *	must be very careful about what they call.  Forbidden operations
 *	include:
 *	    1) Activating/locking any MVFS vnode/mnode.
 *	    2) Any calls to lookup pathnames (even outside of the MVFS
 *	       i.e. audit file pnames), because lookup can require
 *             (1) above.
 *	
 *	In general, try to keep this code restricted to memory
 *	operations (e.g. allocate/free memory and copying data around). 
 */

/* Max size of mfs proctab */
extern int mvfs_maxnproc;

/*
 *  mvfs_threadid_spl_pool -- splock pool -- protects:
 *    all operations on hash buckets in mvfs_threadid_hashtable
 *
 * If you need both this and the mvfs_proclock, you must get the
 * mvfs_proclock (sleep lock) first.
 */
#define THREADID_SPLOCK(hash_val, _mcdp, lockpp, st) { \
        SPLOCK_SELECT(&(_mcdp->proc_thr.mvfs_threadid_spl_pool), hash_val, HASH_SPLOCK_MAP, lockpp); \
        SPLOCK(**(lockpp), st); }
                            
#define THREADID_SPUNLOCK(hash_val, _mcdp, lockpp, st) { \
        SPLOCK_SELECT(&(_mcdp->proc_thr.mvfs_threadid_spl_pool), hash_val, HASH_SPLOCK_MAP, lockpp); \
        SPUNLOCK(**(lockpp), st); }

#ifndef MVFS_SYSTEM_KMEM
struct mvfs_slab_list *mvfs_thread_slabs;
struct mvfs_slab_list *mvfs_proc_slabs;
#endif

int
mvfs_procinit(mvfs_cache_sizes_t *mma_sizes)
{
    u_long len;
    int sp_poolsize, error = 0;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_THREADHASHTAB_SZ] = mcdp->mvfs_threadhash_sz;
    MVFS_SIZE_DEFLOAD_NONZERO(mcdp->mvfs_threadhash_sz, mma_sizes, THREADHASHTAB_SZ,
                              MVFS_THREADHASH_SZ_DEFAULT);

    len = sizeof(mvfs_thread_t *) * MVFS_THREADHASH_SZ(mcdp);
    mcdp->proc_thr.mvfs_threadid_hashtable = (mvfs_thread_t **)KMEM_ALLOC(len, KM_SLEEP);
    if (mcdp->proc_thr.mvfs_threadid_hashtable == (mvfs_thread_t **)NULL) {
        mvfs_log(MFS_LOG_ERR,"mvfs_procinit: no memory (threadhash)");
        error = ENOMEM;
    }
    if (error == 0) {
        BZERO((caddr_t) mcdp->proc_thr.mvfs_threadid_hashtable, len);

        HASH_SPLOCK_SET_POOLSIZE(sp_poolsize, MVFS_THREADHASH_SZ(mcdp));
        if (mvfs_splock_pool_init(&(mcdp->proc_thr.mvfs_threadid_spl_pool), sp_poolsize, NULL,
                                  "mvfs_thr_hash_spl") != 0)
        {
            mvfs_log(MFS_LOG_ERR,"mvfs_procinit: no memory (threadhash locks)");
            KMEM_FREE(mcdp->proc_thr.mvfs_threadid_hashtable,
                      sizeof(mvfs_thread_t *)*MVFS_THREADHASH_SZ(mcdp));
            error = ENOMEM;
        }
    }
    if (error == 0) {
        len = sizeof(mvfs_proc_t *) * MVFS_PROCHASH_SZ;
        mcdp->proc_thr.mvfs_procid_hashtable = (mvfs_proc_t **)KMEM_ALLOC(len, KM_SLEEP);
        if (mcdp->proc_thr.mvfs_procid_hashtable == (mvfs_proc_t **)NULL) {
            mvfs_log(MFS_LOG_ERR,"mvfs_procinit: no memory (prochash)");
            KMEM_FREE(mcdp->proc_thr.mvfs_threadid_hashtable,
                      sizeof(mvfs_thread_t *)*MVFS_THREADHASH_SZ(mcdp));
            error = ENOMEM;
        }
    }
    if (error == 0) {
        BZERO((caddr_t) mcdp->proc_thr.mvfs_procid_hashtable, len);

        INITLOCK(&(mcdp->proc_thr.mvfs_proclock), "mvfsproc");
        INITSPLOCK(mcdp->proc_thr.mvfs_proc_alloclock, "mvfs_procalloc_spl");

        ASSERT(sizeof(mvfs_thread_t) >= sizeof(void *));
        ASSERT(sizeof(mvfs_proc_t) >= sizeof(void *));
    
        mcdp->proc_thr.mvfs_nproc_alloced = 0;
        mcdp->proc_thr.mvfs_nthr_alloced = 0;
    }

    if (error != 0) {
        mcdp->mvfs_threadhash_sz = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_THREADHASHTAB_SZ];
    }
    return error;
}

/*
 * MVFS_PROCDATA_FREE - dispose of allocated proc state
 */

void
mvfs_procdata_free()
{
    mvfs_thread_t *mythread;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if (mcdp->proc_thr.mvfs_threadid_hashtable == NULL) return;	/* Not inited yet */
    mvfs_procpurge_afps();

    mythread = mvfs_mythread();

    mvfs_procpurge(MVFS_PROCPURGE_FLUSH);

    /*
     * only our process remains now; It has no auditing stuff. Safe to
     * discard it.
     */
    MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
    mvfs_procrele(mythread->thr_proc, mythread); /* unlocks it for us */

    ASSERT(mcdp->proc_thr.mvfs_nproc_alloced == 0 && mcdp->proc_thr.mvfs_nthr_alloced == 0);

    KMEM_FREE(mcdp->proc_thr.mvfs_threadid_hashtable,
	      sizeof(mvfs_thread_t *)*MVFS_THREADHASH_SZ(mcdp));
    KMEM_FREE(mcdp->proc_thr.mvfs_procid_hashtable,
	      sizeof(mvfs_proc_t *)*MVFS_PROCHASH_SZ);

    FREELOCK(&(mcdp->proc_thr.mvfs_proclock));

    mvfs_splock_pool_free(&(mcdp->proc_thr.mvfs_threadid_spl_pool));

    FREESPLOCK(mcdp->proc_thr.mvfs_proc_alloclock);
    mcdp->mvfs_threadhash_sz = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_THREADHASHTAB_SZ];
    return;
}

int
mvfs_proc_getcaches(mvfs_cache_sizes_t *szp)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    szp->size[MVFS_SETCACHE_THREADHASHTAB_SZ] = MVFS_THREADHASH_SZ(mcdp);
    szp->size[MVFS_SETCACHE_PROCHASHTAB_SZ] = MVFS_PROCHASH_SZ;
    return 0;
}

int
mvfs_proc_compute_caches(
    ks_int32_t scale_factor,
    mvfs_cache_sizes_t *szp
)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if ((szp->mask & MVFS_CACHEBIT(THREADHASHTAB_SZ)) == 0) {
        szp->size[MVFS_SETCACHE_THREADHASHTAB_SZ] = MVFS_THREADHASH_SZ_DEFAULT;
        szp->mask |= MVFS_CACHEBIT(THREADHASHTAB_SZ);
    }
    /* not tunable */
    szp->size[MVFS_SETCACHE_PROCHASHTAB_SZ] = MVFS_PROCHASH_SZ;
    szp->mask |= MVFS_CACHEBIT(PROCHASHTAB_SZ);
    return 0;
}

/*
 * Zap a particular process.  Clean up all state (except for the
 * active thread).
 *
 * REQUIRES: caller must insure that any audit file stuff is gone before
 * calling here.
 */
void
mvfs_zap_proc(proc)
mvfs_proc_t *proc;
{
    register mvfs_proc_t *mpchase, **mpp;
    register mvfs_thread_t *others;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    unsigned int bucket;

    ASSERT(ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));
    
    bucket = mvfs_pidhash(&proc->mp_procid);

    mpp = &(mcdp->proc_thr.mvfs_procid_hashtable[bucket]);
    MDB_XLOG((MDB_PROCOPS,"zapping proc %"KS_FMT_PTR_T" bucket %d\n", proc, bucket));
    ASSERT(*mpp != NULL);

    /*
     * mpp points to the cell holding the pointer we should replace
     * to remove an elt from this singly-linked queue.
     * This starts out as the address of the bucket ptr, and
     * is changed to the address of each hashnxt pointer as we walk
     * down the chain.
     */
    for (mpchase = *mpp;
	 mpchase;
	 mpp = &mpchase->mp_hashnxt, mpchase = mpchase->mp_hashnxt) {
	if (mpchase == proc) {
	    /* remove this proc from hash queue: */
	    *mpp = mpchase->mp_hashnxt;
	    mpchase->mp_hashnxt = 0;

	    MDB_XLOG((MDB_PROCOPS,"zapping proc %"KS_FMT_PTR_T"\n", proc));
	    while ((others = proc->mp_threads) != NULL)
		mvfs_threadrele(others);
	    /* all threads on this process are gone, now clean up
	     * process state:
	     */
	    ASSERT(proc->mp_threads == NULL); /* no more threads left */
	    ASSERT(proc->mp_afp == NULL); /* caller must have dumped afp */
	    /* use internal form to avoid afp flushing */
	    mvfs_procfree_int(proc);
	    return;
	}
    }
    /* We'd better have dropped it */
    MDKI_PANIC("zapped proc not on hash bucket");
    return;
}

/*
 * Zap all the invalid processes on a given process hash chain.
 *
 * If it dropped the lock and reaquired it, returns 1.
 * If it dropped the lock and couldn't reacquire it, returns -1.
 * Otherwise returns 0.
 */
STATIC int
mvfs_purgechain(bucket, purge, mythread)
int bucket;
int purge;
mvfs_thread_t *mythread;
{
    register mvfs_proc_t *mpchase, **mpp;
    register mvfs_proc_t *myproc = mythread->thr_proc;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    int unlocked = 0;
#ifdef MVFS_DEBUG
    mvfs_thread_t *thr;
#endif

    ASSERT(ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));
    
repurge:
    mpp = &(mcdp->proc_thr.mvfs_procid_hashtable[bucket]);
    MDB_XLOG((MDB_PROCOPS2,"purging bucket %d, %"KS_FMT_PTR_T"\n", bucket, *mpp));

    /*
     * mpp points to the cell holding the pointer we should replace
     * to remove an elt from this singly-linked queue.
     * This starts out as the address of the bucket ptr, and
     * is changed to the address of each hashnxt pointer as we walk
     * down the chain.
     */
    for (mpchase = *mpp;
	 mpchase;
	 mpp = &mpchase->mp_hashnxt, mpchase = mpchase->mp_hashnxt) {
#ifdef MVFS_DEBUG
        MVFS_LOCK(&mpchase->mp_lock);
	for (thr = mpchase->mp_threads; thr; thr = thr->thr_next)
	    ASSERT(thr->thr_proc == mpchase);
        MVFS_UNLOCK(&mpchase->mp_lock);
#endif
	if (purge == MVFS_PROCPURGE_FLUSH ||
	    !MVFS_PROCVALID(mpchase)) {
	    MDB_XLOG((MDB_PROCOPS,
		     "purge: (pid not found) mp=%"KS_FMT_PTR_T", pid=%"MVFS_FMT_PROCID_T_D",tag=%d\n",
		      mpchase,mpchase->mp_procid, mpchase->mp_proctag)); 
	    /* remove this proc from hash queue: */
	    *mpp = mpchase->mp_hashnxt;
	    mpchase->mp_hashnxt = 0;

	    /* 
	     * mvfs_procrele drops the mvfs_proclock in case it ends
	     * up doing audit file I/O while releasing the process and
	     * its threads.  It needs the lock held to prevent races
	     * to diddle the process's threads; normally it would
	     * hold the spinlock but it calls things which expect to
	     * diddle the spinlock themselves, so it relies on
	     * the table lock to prevent this race.
	     */
	    /*
	     * If we're not purging everything, we better not purge
	     * ourselves.
	     */
	    ASSERT(purge == MVFS_PROCPURGE_FLUSH || mpchase != myproc);
	    if (purge != MVFS_PROCPURGE_FLUSH || mpchase != myproc) {
		/* procrele may take thread hash chain spinlock */
		mvfs_procrele(mpchase, mythread); /* drops lock */
		if (purge == MVFS_PROCPURGE_NOSLEEP) {
		    /* only conditionally reacquire the lock in this case: */
		    if (!CONDITIONAL_LOCK(&(mcdp->proc_thr.mvfs_proclock)))
			return -1;
		} else {
		    MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
		}
		unlocked = 1;
	    }
	    goto repurge;		/* the hash chain may have changed */
	}
    }
    /* must clean up completely if flushing */
    ASSERT((purge != MVFS_PROCPURGE_FLUSH) || (*mpp == NULL));
    return unlocked;
}

/*
 * MVFS_PROCPURGE - call procrele on all proc structs for which the
 * process is no longer running.  This is called periodically under UNIX
 * since the proc stuff doesn't get called at process exit.
 * It is also called when the filesystem is about to be unloaded.
 */
void
mvfs_procpurge(slp)
int slp;
{
    int i;
    int rescan, relocked;
    register mvfs_proc_t *mp;
    SPL_T s;
    mvfs_thread_t *mythread = mvfs_mythread();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    switch (slp) {
    case MVFS_PROCPURGE_SLEEP:
    case MVFS_PROCPURGE_FLUSH:
	MDB_XLOG((MDB_PROCOPS,"mvfs_procpurge\n"));
        MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
	break;
    case MVFS_PROCPURGE_NOSLEEP:
	if (!CONDITIONAL_LOCK(&(mcdp->proc_thr.mvfs_proclock))) return;
	break;
    }

    rescan = 1;
    while (rescan != 0) {
	rescan = 0;
	relocked = 0;
	for (i=0, mp = mcdp->proc_thr.mvfs_procid_hashtable[0];
	     i < MVFS_PROCHASH_SZ;
	     mp = mcdp->proc_thr.mvfs_procid_hashtable[++i]) {
	
	    if (mp == (mvfs_proc_t *)NULL)
		continue;		/* none on this bucket */

	    /* 
	     * we must drop the mvfs_proclock before calling purge,
	     * because when we purge a process we may end up pushing
	     * auditing information to a file, and that requires that
	     * we get our own thread info (in order to inhibit
	     * auditing while we write the auditing information).  The
	     * result may be a new thread/proc which need to be added
	     * to the chains, which requires getting the locks.  Now,
	     * if we're not sleeping, we just go on to the next chain
	     * pointer until we're done (not caring about new
	     * additions); if we are flushing we keep cycling until
	     * we've scanned the entire list without dropping the
	     * lock. */
	    relocked = mvfs_purgechain(i, slp, mythread);
	    if (relocked == -1)
		return;			/* couldn't get lock back. */
	    if (relocked == 1) switch (slp) {
	    case MVFS_PROCPURGE_FLUSH:
		/*
		 * Mark flag so we run through the table again, in
		 * case someone slipped in.
		 */
		rescan = 1;
	    case MVFS_PROCPURGE_SLEEP:
	    case MVFS_PROCPURGE_NOSLEEP:
		/* don't care that it unlocked/relocked */
		break;
	    }
	}
    }
    MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
}

/*
 * MVFS_PROCPURGE_AFPS - run down all the proc and thread structures,
 * flushing all the audit file pointers.
 *
 * We're called by the process which wishes to unload the module.
 * This won't happen if any MVFS file systems are still mounted, so we
 * can safely assume we're the only active process in this filesystem.
 * (I think?)
 */
STATIC void
mvfs_procpurge_afps()
{
    int i;
    int rescan, relocked;
    register mvfs_proc_t *mp, *mpchase;
    register mvfs_thread_t *thrchase;
    SPL_T s;
    mvfs_thread_t *mythread;
    extern LOCK_T mfs_unload_lock;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * we must only be called by the unloading thread.
     */
    ASSERT(ISLOCKEDBYME(&mfs_unload_lock));
    mythread = mvfs_mythread();		/* force an allocate now,
					   in case we need it */

    MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
    for (i=0, mp = mcdp->proc_thr.mvfs_procid_hashtable[0];
	 i < MVFS_PROCHASH_SZ;
	 mp = mcdp->proc_thr.mvfs_procid_hashtable[++i]) {
	
     rescan_chain:
	for (mpchase = mp;
	     mpchase;			/* handles case of empty bucket */
	     mpchase = mpchase->mp_hashnxt)
        {
	    if (mpchase->mp_afp != NULL) {
                /* This call drops the proc lock. */
                mvfs_afprele_proc(mpchase, mythread);
                MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
		goto rescan_chain;
	    }
	    for (thrchase = mpchase->mp_threads;
		 thrchase;
		 thrchase = thrchase->thr_next)
            {
		if (thrchase->thr_afp) {
		    mvfs_log(MFS_LOG_ERR,
                             "thread %"KS_FMT_PTR_T" still has audit ptr when "
                             "files unmounted?", thrchase);

		    MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
		    /* may do some blocking on writes, etc. */
		    /* these two lines are essentially mvfs_afprele_thr(),
		       but for some other thread, not us. */
                    /* We're also depending on the statement above that we're
                    ** the last thread running so it's OK to access somebody
                    ** else's thread without a lock.  Normally, we don't use
                    ** locking to access thread data because the owning thread
                    ** is the only one that is supposed to access it (see the
                    ** big comment above).
                    */
		    mvfs_afprele(thrchase->thr_afp, mythread);
		    thrchase->thr_afp = NULL;
		    MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
		    goto rescan_chain;
		}
	    }
	}
    }
    MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
}

/* MFS_PROCINHERIT - inherit/initialize proc info for current process */

#define MAXPARENT 1024

mvfs_proc_t *
mvfs_procinherit_from(mp)
mvfs_proc_t *mp;
{
    int i = 0;
    MVFS_PROCESS_T *parent, *nextparent;
    mvfs_proc_t *pmp;
    MVFS_PROCID_T procid, pprocid;
    MVFS_PROCTAG_T proctag;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
  
    ASSERT(ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock))); /* caller holds lock while we work */

    /* As long as there is a parent pid (non-zero) then continue
       looking for a parent with state to inherit from.  Limit
       this loop to a max just for extra safety. */
    parent = MDKI_PARENT_PROC(MDKI_CURPROC()); /* Start with parent (locked) */
    MDKI_MYPROCID(&procid);
    MDB_XLOG((MDB_PROCOPS,"inherit for %"MVFS_FMT_PROCID_T_D": parent %"KS_FMT_PTR_T"\n", MDKI_CURPID(), parent));

    while (parent) {
	if (++i >= MAXPARENT)
	    break;			/* recursed too far, give up */
	if (!MDKI_PRISACTIVE(parent)) {
            MDB_XLOG((MDB_PROCOPS,"inherit for %d: parent dead\n",
                      MDKI_CURPID()));
	    break;			/* end of the chain */
        }
	MDKI_PROCID(&pprocid,parent);
	if (MDKI_PROCID_EQ(&procid, &pprocid)) {
	    /* do this early test, to avoid any case where the
	     * MDKI_PARENT_PROC() would try to acquire a lock held by
	     * the previous call to MDKI_PARENT_PROC() that hasn't
	     * been released yet by MDKI_PRUNLOCK().
	     */
	    mvfs_log(MFS_LOG_DEBUG,
		    "procinherit: terminating search at self loop, 0x%"MVFS_FMT_PROCID_T_D"\n",
		     procid);
	    break;			/* parent == current, give up */
	}
	procid = pprocid;
	MDKI_PROCTAG(&proctag,parent);
	pmp = mvfs_findproc(&pprocid, &proctag, NULL);
	if (pmp) {
	    MDKI_PRUNLOCK(parent);
	    MDB_XLOG((MDB_PROCOPS,"procinherit %"KS_FMT_PTR_T" from %"KS_FMT_PTR_T" (%"MVFS_FMT_PROCID_T_D"/%x)\n",
				  mp, pmp, pmp->mp_procid, pmp->mp_proctag));
	    return pmp;
	}
	MDKI_PRUNLOCK(parent);		/* done with parent */
	nextparent = MDKI_PARENT_PROC(parent);
	if (!nextparent) {
	    mvfs_log(MFS_LOG_DEBUG,
		    "procinherit: can't find parent proc for %"MVFS_FMT_PROCID_T_D"\n",
		     MDKI_PRPID(parent));
	    parent = NULL;
	    break;
	}
	if (parent == nextparent) {
	    /* top of chain, leave */
	    parent = NULL;
	    MDKI_PRUNLOCK(nextparent);
	    break;
	}
	parent = nextparent;
        MDB_XLOG((MDB_PROCOPS,"inherit for %"MVFS_FMT_PROCID_T_D": nextparent "KS_FMT_PTR_T" (%"MVFS_FMT_PROCID_T_D", %d)\n",
            MDKI_CURPID(), parent, MDKI_PRPID(parent), MDKI_PRSTATE(parent)));
    }

    if (parent)				/* haven't unlocked yet: */
	MDKI_PRUNLOCK(parent);

    /* Print a message if we messed up and looked at too many parents */

    if (i >= MAXPARENT) {
	mvfs_log(MFS_LOG_DEBUG, "procinherit: pid %"MVFS_FMT_PROCID_T_D": too many parents!\n",
				MDKI_CURPID());
    }

    /* No suitable parent state found, leave it as initialized by caller. */

    mvfs_log(MFS_LOG_DEBUG, "procinherit: no inheritance: pid=%"MVFS_FMT_PROCID_T_D"\n", 
			    MDKI_CURPID());
    MDB_XLOG((MDB_PROCOPS, "procinherit: no inheritance: pid=%"MVFS_FMT_PROCID_T_D"\n", 
			   MDKI_CURPID()));
    return NULL;
}

STATIC void
mvfs_procinherit_copy(to, from)
mvfs_proc_t *to;
mvfs_proc_t *from;
{
    mfs_auditfile_t *afp = NULL;

    /* Found valid state to use */
    MVFS_LOCK(&from->mp_lock);
    to->mp_inherit = from->mp_inherit;
    if (to->mp_afp) {
	mfs_afphold(to->mp_afp);
	afp = to->mp_afp;
    }
    MVFS_UNLOCK(&from->mp_lock);
    if (afp) {
	/* pulled out here 'cuz we can't print with spinlock held */
	MDB_XLOG((MDB_AUDITF, "afphold: afp=%"KS_FMT_PTR_T" refcnt=%d pid=%"MVFS_FMT_PROCID_T_D"\n",
			      afp, afp->refcnt, MDKI_CURPID()));
    }
    MDB_XLOG((MDB_PROCOPS, "inherit: mp=0x%"KS_FMT_PTR_T", pid=%"MVFS_FMT_PROCID_T_D", from=%"KS_FMT_PTR_T"\n",
			   to, MDKI_CURPID(), from));
    return;
}

void
mvfs_procinherit(mp)
mvfs_proc_t *mp;
{
    register mvfs_proc_t *procfrom;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    ASSERT(ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock))); /* caller holds lock while we work */

    procfrom = MVFS_PROCINHERIT_FROM(mp);
    if (procfrom) {
	MVFS_PROCINHERIT_COPY(mp, procfrom);
    }
}

/* MVFS_MYTHREAD - find/create valid thread info for this process. 
 *	This routine always returns a valid thread info state (even if
 *	all reset) for this process.
 *
 *	This routine MUST NOT DISTURB the u-area in any of
 *	its activities, since it is called from page-fault
 *	code (or its equivalent).
 */
mvfs_thread_t *
mvfs_mythread()
{
    register mvfs_thread_t *mth;
    mvfs_proc_t *mproc;
    unsigned int hashindex;
    MVFS_THREADID_T threadid;
    SPL_T s;
    MVFS_PROCTAG_T metag;
    MVFS_PROCID_T mepid;
    SPLOCK_T *lockp;
    tbs_boolean_t foundrealthr;
    MVFS_PROCTAG_T mthproctag;
    MVFS_PROCID_T mthpid;
    mvfs_proc_t *mthproc = NULL;
    register mvfs_proc_t *mpchase, **mpp;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    
    /*
     * We must not hold the mvfs_proclock upon calling this function.
     * If we do, then we block allocating a thread structure; doing so
     * while holding that lock may result in a deadlock if, for example,
     * the current thread is a VM page push operation.
     */
    ASSERT(NOTLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));
    BZERO(&threadid,sizeof(MVFS_THREADID_T));
    MDKI_MYTHREADID(&threadid);
    MDKI_MYPROCID(&mepid);
    MDKI_MYPROCTAG(&metag, &mepid);
    
    hashindex = MDKI_THREADHASH(&threadid, mcdp);

    THREADID_SPLOCK(hashindex, mcdp, &lockp, s);
    for (mth = mcdp->proc_thr.mvfs_threadid_hashtable[hashindex];
	 mth;
	 mth = mth->thr_hashnxt) {
	ASSERT(mth->thr_hashbucket == hashindex);
	if (MDKI_THREADID_EQ(&threadid, &mth->thr_threadid)) {

	    foundrealthr = (MDKI_PROC_EQ(mth->thr_proc, &mepid, &metag));

            mthproctag = mth->thr_proc->mp_proctag;
            mthpid = mth->thr_proc->mp_procid;
            THREADID_SPUNLOCK(hashindex, mcdp, &lockp, s);


            if (foundrealthr)
                return(mth);

            mvfs_log(MFS_LOG_DEBUG,
		    "mvfs_mythread: changing allegiance, mthr %"KS_FMT_PTR_T" procid %"MVFS_FMT_PROCID_T_D" proctag %d\n",
                     mth, mepid, metag);
	    /*
	     * This thread has apparently changed allegiance and is
	     * now in a different process.  Lets grab the 
             * mvfs_proclock and see if its mvfs_proc is on the hash queue.
	     * If it is, release the thread. If it isn't, the procpurge
	     * has cleaned up or is in the process of cleaning up, 
             * so just allocate a new one.
	     */

	    MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));

            if ((mthproc = mvfs_findproc(&mthpid, &mthproctag, NULL)) 
                  != NULL)
            {
                mvfs_threadrele(mth);
                /*
                 * If the process is dead, we also want to release the
                 * proc, but we can't do it until we get our own new
                 * thread created.  Use mthproc != NULL as an
                 * indicator for later.
                 */
                if (MVFS_PROCVALID(mthproc))
                    mthproc = NULL;
            }
	    MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
	    goto realloc;		/* we don't hold spinlock */
	}
    }
    /* none there yet, get a new one */
    /*
     * nobody else could be allocating a thread struct for us (by
     * definition it's thread-specific), so we don't need to worry
     * about racing ourselves for a thread.  Therefore it's safe to
     * drop and reacquire the hash table lock around this call (the
     * lock just keeps the hash table pointers intact, and we don't
     * care where in the hash chain we end up).  There are other
     * things that happen in the process part of allocating a new
     * thread that are easier to not worry about if we don't hold this
     * lock.
     */
    THREADID_SPUNLOCK(hashindex, mcdp, &lockp, s);
realloc:
    mth = mvfs_threadalloc(&threadid);
    MDB_XLOG((MDB_PROCOPS,"pid %"MVFS_FMT_PROCID_T_D": alloc new thread %"KS_FMT_PTR_T"\n",
			  mth->thr_proc->mp_procid, mth));
#ifdef MVFS_DEBUG
    MDKI_MYPROCID(&mepid);
    MDKI_MYPROCTAG(&metag, &mepid);
    if (!(MDKI_PROC_EQ(mth->thr_proc, &mepid, &metag)))
        mvfs_log(MFS_LOG_WARN,
		    "mvfs_mythread: different procinfo  mthr 0x%x procid 0x%x proctag 0x%x mprocid 0x%x mproctag 0x%x\n",
		     mth, mepid, metag, mth->thr_proc->mp_procid, mth->thr_proc->mp_proctag);
#endif

    /* RATLC01308802: short lived mvfs_thread used for inactive during proc
     *  exit does not go onto hash chain.  (Solaris only) 
     */
    if (!MDKI_IS_SOL_EXITPROC(mepid,mcdp)) {
        /* attach new thread to hash chains */
        THREADID_SPLOCK(hashindex, mcdp, &lockp, s);
        mth->thr_hashbucket = hashindex;
        mth->thr_hashnxt = mcdp->proc_thr.mvfs_threadid_hashtable[hashindex];
        mcdp->proc_thr.mvfs_threadid_hashtable[hashindex] = mth;
        THREADID_SPUNLOCK(hashindex, mcdp, &lockp, s);
    }

    if (mthproc != NULL) {
        /* delayed flush of potentially dead process.  Look it up again. */
        MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));

        if ((mthproc = mvfs_findproc(&mthpid, &mthproctag, NULL)) != NULL &&
            !MVFS_PROCVALID(mthproc))
        {
            /*
             * unhash it first, then release it.  We don't use
             * mvfs_purgechain() since we don't want to spend time now
             * looking at other procs--we only care about the one we
             * identified.
             */
            hashindex = mvfs_pidhash(&mthpid);
            mpp = &(mcdp->proc_thr.mvfs_procid_hashtable[hashindex]);

            for (mpchase = *mpp;
                 mpchase;
                 mpp = &mpchase->mp_hashnxt, mpchase = mpchase->mp_hashnxt)
            {
                if (mpchase == mthproc) {
                    /* remove this proc from hash queue: */
                    *mpp = mpchase->mp_hashnxt;
                    mpchase->mp_hashnxt = 0;
                    break;
                }
            }
            ASSERT(mpchase == mthproc);
            mvfs_procrele(mthproc, mth); /* drops proclock */
	    mvfs_log(MFS_LOG_DEBUG,
		    "mvfs_mythread: dumped stale mvfs_proc"
                     " procid %"MVFS_FMT_PROCID_T_D" proctag %d\n",
                     mthpid, mthproctag);
        } else {
            /* we have to drop proclock */
            MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
        }
    }
    return mth;
}

/*
 * Take a snapshot from the thread's related process.
 */
STATIC void
mvfs_snapshot_thread(thr)
mvfs_thread_t *thr;
{
    register mvfs_proc_t *proc = thr->thr_proc;

    ASSERT(proc != NULL);		/* just in case */

    mvfs_afprele_thr(thr);		/* it checks for NULL */
	
    MVFS_LOCK(&proc->mp_lock);
    thr->thr_inherit = proc->mp_inherit;
    if (thr->thr_afp)
	mfs_afphold(thr->thr_afp);
    MVFS_UNLOCK(&proc->mp_lock);
    if (thr->thr_afp) {
	/* pulled out here 'cuz we can't print with spinlock held */
	MDB_XLOG((MDB_AUDITF, "afphold: afp=%"KS_FMT_PTR_T" refcnt=%d pid=%"MVFS_FMT_PROCID_T_D"\n",
			      thr->thr_afp, thr->thr_afp->refcnt,
			      MDKI_CURPID()));
    }
    MDB_XLOG((MDB_PROCOPS2,"snapshot: thr %"KS_FMT_PTR_T" to proc %"KS_FMT_PTR_T"\n",
			   thr, thr->thr_proc));
}

/*
 * Find thread via mfs_mythread().  Increment count and copy state if
 * appropriate.
 */
mvfs_thread_t *
mvfs_enter_fs()
{
    register mvfs_thread_t *rthread;
#if defined(MVFS_DEBUG) || defined(DEBUG_PERF)
    timestruc_t stime;	/* For statistics */
    timestruc_t dtime;

    MDKI_HRTIME(&stime);	/* Fetch start time for stats */
#endif
    rthread = MVFS_MDEP_ENTER_FS();
    if (rthread->thr_activecount++ == 0) {
	/* first activation */
	mvfs_snapshot_thread(rthread);
    }
#if defined(MVFS_DEBUG) || defined(DEBUG_PERF)
    MVFS_BUMPTIME(stime, dtime, mfs_clntstat.mvfsthread_time);
#endif
    return rthread;
}

/*
 * Decrement active count and clean up remaining state if zero.
 */
void
mvfs_exit_fs(thr)
register mvfs_thread_t *thr;
{
    register mvfs_thread_t *rthread;
#if defined(MVFS_DEBUG) || defined(DEBUG_PERF)
    timestruc_t stime;	/* For statistics */
    timestruc_t dtime;

    MDKI_HRTIME(&stime);	/* Fetch start time for stats */
#endif
    
    MVFS_MDEP_EXIT_FS(thr);
        
    if (--thr->thr_activecount == 0) {
	ASSERT(thr->thr_rebindinh == 0);
	ASSERT(thr->thr_auditinh == 0);
	BZERO(thr->thr_errstr, sizeof(thr->thr_errstr));
	if (thr->thr_afp)
	    mvfs_afprele_thr(thr);
	BZERO(&thr->thr_inherit, sizeof(thr->thr_inherit));
        MVFS_DUMMY_RELE(thr);
    }
#if defined(MVFS_DEBUG) || defined(DEBUG_PERF)
    MVFS_BUMPTIME(stime, dtime, mfs_clntstat.mvfsthread_time);
#endif
    return;
}

/*
 * Copy inheritable stuff from thread back to process state.
 */
void
mvfs_sync_procstate(thr)
register mvfs_thread_t *thr;
{
    register mvfs_proc_t *proc = thr->thr_proc;
    mfs_auditfile_t *afp;

    ASSERT(proc != NULL);
    
    if (thr->thr_afp)
	mfs_afphold(thr->thr_afp);		/* for extra ref */
    if (thr->thr_afp) {
	/* pulled out here 'cuz we can't print with spinlock held */
	MDB_XLOG((MDB_AUDITF, "afphold: afp=%"KS_FMT_PTR_T" refcnt=%d pid=%"MVFS_FMT_PROCID_T_D"\n",
			      thr->thr_afp, thr->thr_afp->refcnt,
			      MDKI_CURPID()));
    }
    MVFS_LOCK(&proc->mp_lock);
    afp = proc->mp_afp;
    proc->mp_afp = NULL;
    proc->mp_inherit = thr->thr_inherit;
    MVFS_UNLOCK(&proc->mp_lock);

    /* 
     * drop the afp we zapped, but do it outside the spinlock to avoid
     * locking delays.
     */
    if (afp)
	mvfs_afprele(afp, thr);
}

/*
 * Copy inheritable stuff from thread back to process state.  Caller
 * holds lock on thread's process structure.
 */
void
mvfs_sync_procstate_locked(thr)
register mvfs_thread_t *thr;
{
    register mvfs_proc_t *proc = thr->thr_proc;

    ASSERT(proc != NULL);

    /* lock held by caller:    MVFS_LOCK(&proc->mp_lock); */
    /*
     * we cannot diddle with the audit file pointers because doing so
     * might push audit file pages to disk, and we don't dare do that
     * when we hold a spinlock.
     */
    ASSERT(thr->thr_afp == proc->mp_afp);
    proc->mp_inherit = thr->thr_inherit;
    /* lock held by caller:    MVFS_UNLOCK(&proc->mp_lock); */
}

STATIC mvfs_thread_t *
mvfs_threadalloc(thrid)
MVFS_THREADID_T *thrid;
{
    register mvfs_thread_t *thr;
    register mvfs_proc_t *proc;
    MVFS_PROCID_T mepid;
    SPL_T s;
    int exit_thread = 0;    
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * allocate a thread from the standard place.
     */
    
    /* We must not hold the proc lock while we do allocation stuff
     * that might block.
     */
    ASSERT(!ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));

    /*
     * Warning: In certain contexts (e.g. page push), we must not make
     * a blocking memory allocation request.
     *
     * None of the putpage routines need to get hold of the current
     * thread, for the moment.
     */

	thr = MVFS_THREAD_ALLOC();
    BZERO(thr, sizeof(*thr));

    SPLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);
    mcdp->proc_thr.mvfs_nthr_alloced++;
    SPUNLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);

    /*
     * now get proc, and chain this thread onto it.
     * RATLC01291132: On Solaris, use special proc structure for threads
     * which are exiting, i.e. if pid is p0 or zsched. These follow a
     * different procedure for being cleaned up later.
     */
    proc = mvfs_myproc();

    thr->thr_threadid = *thrid;
    MVFS_LOCK(&proc->mp_lock);
    thr->thr_proc = proc;
    thr->thr_next = proc->mp_threads;
    proc->mp_threads = thr;
    proc->mp_refcnt++;
    MVFS_UNLOCK(&proc->mp_lock);
    /* don't fill in thread inherit stuff--higher levels do that as needed */
    return thr;
}

/* RATLC01291132: needs_dequeue flag indicates whether thread is on 
 * mp_threads list of mvfs_proc struct. TRUE when called beneath mvfs_procrele,
 * FALSE when called by mvfs_purge_exit_threads.  
 */
STATIC void
mvfs_threadfree(thr, needs_dequeue)
mvfs_thread_t *thr;
tbs_boolean_t needs_dequeue; /* Only dequeue thread if on mp_threads list */
{
    SPL_T s;
    register mvfs_thread_t *thrnext, *thrprev;
    mvfs_proc_t *proc;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    int i;

    ASSERT(thr->thr_activecount == 0);
    /* should always hold if activecount == 0: */
    ASSERT(thr->thr_afp == NULL);
    if (needs_dequeue) {
        /*
         * get proc, and remove this thread from chain.
         */
        proc = thr->thr_proc;
        MVFS_LOCK(&proc->mp_lock);
        if (proc->mp_threads == thr) {
	    proc->mp_threads = thr->thr_next;
	    thr->thr_next = NULL;
        } else {
	    for (thrprev = proc->mp_threads, thrnext = thrprev->thr_next;
	         thrnext;
	         thrprev = thrnext, thrnext = thrprev->thr_next) {
	        if (thrnext == thr) {
		    thrprev->thr_next = thr->thr_next;
		    thr->thr_next = NULL;
		    break;
	        }
	    }
	    ASSERT(thrnext != NULL);	/* we'd better have found ourselves */
        }

        proc->mp_refcnt--;   /* drop ref on proc.  Caller
    			        will free if appropriate. */
        MVFS_UNLOCK(&proc->mp_lock);
    }

    /* any audit info in thr_afp was released when activecount went to zero */

    SPLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);
    mcdp->proc_thr.mvfs_nthr_alloced--;
    SPUNLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);

    BZERO(thr, sizeof(*thr));
    MVFS_THREAD_FREE(thr);

    return;
}

STATIC void
mvfs_threadrele(thr)
mvfs_thread_t *thr;
{
    register mvfs_thread_t **mthp, *mtchase;
    SPL_T s;
    u_int bucket;
    SPLOCK_T *lockp;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    
    /* remove thread from hash bucket list, then free it */

    bucket = thr->thr_hashbucket;
    MDB_XLOG((MDB_PROCOPS,"mvfs_threadrele %"KS_FMT_PTR_T" bucket %d\n", thr, bucket));
    THREADID_SPLOCK(bucket, mcdp, &lockp, s);
    mthp = &(mcdp->proc_thr.mvfs_threadid_hashtable[bucket]);
    ASSERT(*mthp != NULL);

    /*
     * see comments above in mvfs_purgechain() about mpp.
     */
    for (mtchase = *mthp;
	 mtchase;
	 mthp = &mtchase->thr_hashnxt, mtchase = mtchase->thr_hashnxt) {
	if (thr == mtchase) {
	    *mthp = thr->thr_hashnxt;
            THREADID_SPUNLOCK(bucket, mcdp, &lockp, s);
            /*flag tells threadfree to dequeue from mp_threads */
	    mvfs_threadfree(thr,TRUE);
	    return;
	}
    }
    MDKI_PANIC("thread not on hash bucket");
}

STATIC mvfs_proc_t *
mvfs_procalloc(procid, proctag)
MVFS_PROCID_T *procid;
MVFS_PROCTAG_T *proctag;
{
    register mvfs_proc_t *proc;
    SPL_T s;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * allocate a proc from the standard place.
     */
    
    /* We must not hold the proc lock while we do allocation stuff
     * that might block.
     */
    ASSERT(!ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));

    /*
     * see Warning in threadalloc code about who can't block.
     */

	proc = MVFS_PROC_ALLOC();
    BZERO(proc, sizeof(*proc));

    SPLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);
    mcdp->proc_thr.mvfs_nproc_alloced++;
    SPUNLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);

    INITLOCK(&proc->mp_lock, "mp_lock");
    proc->mp_procid = *procid;
    proc->mp_proctag = *proctag;
    proc->mp_refcnt++;
    return proc;
}

STATIC void
mvfs_procfree(proc, mythread)
mvfs_proc_t *proc;
mvfs_thread_t *mythread;
{
    /* This call drops the proc lock (assumed to be locked). */
    mvfs_afprele_proc(proc, mythread);		/* it checks for NULL */

    mvfs_procfree_int(proc);
}

STATIC void
mvfs_procfree_int(proc)
mvfs_proc_t *proc;
{
    SPL_T s;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    register int i;

    ASSERT(proc->mp_refcnt == 1);
    ASSERT(proc->mp_hashnxt == NULL);

    FREELOCK(&proc->mp_lock);
    BZERO(proc, sizeof(*proc));

    SPLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);
    mcdp->proc_thr.mvfs_nproc_alloced--;
    SPUNLOCK(mcdp->proc_thr.mvfs_proc_alloclock, s);

    MVFS_PROC_FREE(proc);
    return;
}

STATIC void
mvfs_procrele(proc, mythread)
mvfs_proc_t *proc;
mvfs_thread_t *mythread;
{
    register mvfs_thread_t *thr;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    ASSERT(ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));

    MDB_XLOG((MDB_PROCOPS,"mvfs_procrele %"KS_FMT_PTR_T" pid %"MVFS_FMT_PROCID_T_D"\n",
              proc, proc->mp_procid));
    /*
     * Because we are told this process is dead, there's no danger of
     * another thread trying to diddle with the proc state via a thread
     * pointer.  We hold the mvfs_proclock, so nobody will mess with it
     * by finding it in the process hash table either.  Therefore it's safe
     * to touch proc->mp_* without taking the spinlock (we need to do so
     * if our callees try to lock it, e.g. the thread release code which
     * unlinks the threads from the processes).
     */

    /* garbage collect all the threads */
    while ( (thr = proc->mp_threads) != NULL)
	mvfs_threadrele(thr);

    /* and then release the proc itself */
    mvfs_procfree(proc, mythread);
    return;
}

STATIC unsigned int
mvfs_pidhash(MVFS_PROCID_T *pidp)
{
    register ks_uint32_t rval = 0;
    register int i;

    register unsigned char *cp = (unsigned char *)pidp;
    register unsigned int *ip = (unsigned int *)pidp;

    if ((sizeof(*pidp) % sizeof(unsigned int)) == 0) {
	/* add up as integers */
	for (i = 0; i < sizeof(*pidp); i += sizeof(unsigned int))
	    rval += *ip++;
    } else {
	/* this has the effect of adding up the contents of *pidp as
	   if it were an array of little-endian integers */
	for (i = 0; i < sizeof(*pidp); i++) {
	    rval += *cp++ << (NBBY*(i % 4));
	}
    }
    return (unsigned int)(rval % MVFS_PROCHASH_SZ);
}

mvfs_proc_t *
mvfs_findproc(procidp, tagp, hashp)
MVFS_PROCID_T *procidp;
MVFS_PROCTAG_T *tagp;
register unsigned int *hashp;
{
    register mvfs_proc_t *mp, *newmp;
    register unsigned int hashindex;
    SPL_T s;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    ASSERT(ISLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock)));

    if (hashp != NULL)
	hashindex = *hashp;
    else
	hashindex = mvfs_pidhash(procidp);
	
    for (mp = mcdp->proc_thr.mvfs_procid_hashtable[hashindex];
	 mp;
	 mp = mp->mp_hashnxt)
	if (MDKI_PROCID_EQ(procidp, &mp->mp_procid) &&
            MDKI_PROCTAG_EQ(tagp, &mp->mp_proctag))
        {
	    return mp;
	}
    /* TODO: garbage collect dead processes? */
    return NULL;
}

/* MVFS_MYPROC - find/create valid proc info for this process.  This
 *	routine always returns a valid proc info state (even if all
 *	reset) for this process.  It is used in whenever a new thread
 *	is encountered whose process doesn't already have a proc
 *	struct here.
 *
 *	This routine MUST NOT DISTURB the u-area in any of
 *	its activities, since it is called from page-fault
 *	code (or its equivalent).
 */

STATIC mvfs_proc_t *
mvfs_myproc()
{
    register mvfs_proc_t *mp, *newmp;
    unsigned int hashindex;
    MVFS_PROCID_T procid;
    MVFS_PROCTAG_T proctag;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    MDKI_MYPROCID(&procid);
    MDKI_MYPROCTAG(&proctag, &procid);

    hashindex = mvfs_pidhash(&procid);
    
    MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
    mp = mvfs_findproc(&procid, &proctag, &hashindex);

    if (mp) {
	MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
	return mp;
    }
    
    /* none there yet, get a new one.  We must drop the lock while we allocate,
       then rescan. */
    MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
    newmp = mvfs_procalloc(&procid, &proctag);
    MVFS_LOCK(&(mcdp->proc_thr.mvfs_proclock));
    MDB_XLOG((MDB_PROCOPS,"alloc new proc %"KS_FMT_PTR_T"\n",newmp));

    mp = mvfs_findproc(&procid, &proctag, &hashindex);
    if (mp != NULL) {
	mvfs_procfree(newmp, 0);
	MDB_XLOG((MDB_PROCOPS,"new proc duplicate, discarded %"KS_FMT_PTR_T" for %"KS_FMT_PTR_T"\n",
			      newmp, mp));
	return mp;
    }

    /*
     * OK, we're the unique proc now. Link into hash chain, inherit
     * from parents, and return.
     */

    newmp->mp_hashnxt = mcdp->proc_thr.mvfs_procid_hashtable[hashindex];
    mcdp->proc_thr.mvfs_procid_hashtable[hashindex] = newmp;

    mvfs_procinherit(newmp);
    MVFS_UNLOCK(&(mcdp->proc_thr.mvfs_proclock));
    return newmp;
}

#ifndef MVFS_SYSTEM_KMEM
mvfs_thread_t *
mvfs_thread_cachealloc()
{
    return (mvfs_thread_t *)mvfs_slab_getchunk(mvfs_thread_slabs,
					       sizeof(mvfs_thread_t));
}

void
mvfs_thread_cachefree(thr)
mvfs_thread_t *thr;
{
    mvfs_slab_freechunk(mvfs_thread_slabs, (caddr_t) thr,
			sizeof(mvfs_thread_t));
}

mvfs_proc_t *
mvfs_proc_cachealloc()
{
    return (mvfs_proc_t *)mvfs_slab_getchunk(mvfs_proc_slabs,
					     sizeof(mvfs_proc_t));
}

void
mvfs_proc_cachefree(proc)
mvfs_proc_t *proc;
{
    mvfs_slab_freechunk(mvfs_proc_slabs, (caddr_t) proc,
			sizeof(mvfs_proc_t));
}
#endif

static const char vnode_verid_mvfs_procops_c[] = "$Id:  9cc4f7e5.946011e1.940d.00:01:84:c3:8a:52 $";
