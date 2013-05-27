/* * (C) Copyright IBM Corporation 1990, 2011. */
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
/* mvfs_vnodeops.c */

/*
 * These are the vnode ops routines which implement the vnode interface to
 * the Multi Version file system.  These routines just take their 
 * parameters, make them look networkish by putting the right info into 
 * interface structs, and then calling the appropriate remote routine(s) to 
 * do the work.
 *
 * There are really two different file systems here, implemented as 1
 * FS to save stack space.
 *     1) There is a "viewserver" FS for psuedo-objects which support:
 *		a) View server extended naming
 *	        b) Process context objects (as root dir of process)
 *		c) Loopback (cover vnode) objects for auditing/tool replacement
 *		d) Directory objects which hold view/process objects
 *     2) There is the "view/vob" FS which actually implements access to
 *	  Multi Version objects.  The view/vob FS mostly implements
 *	  directory operations to support configuration/auditing.  Actual FS 
 *	  IO is done by creating "cleartext" files in an underlying FS (nfs or
 *	  the local disk fs), and doing IO to that file.
 *
 * Note on directory name lookup caching:
 *	Is is important that the results on a given client machine come
 *	out the same with or without the cache.  To correctly do this,
 *	I serialize the mfscall() to the server and cache operation
 *      to maintain cache consistency with the Mnode lock
 *	in all directory ops.  This way, I cannot get into races with
 *	myself that would cause invalid information in the cache.
 *	Other clients (or the server) can cause our cached information
 *	to be invalid, the same as with data.  We detect this when the
 *	attributes time out and we notice something has changed in the dir.
 *	Also, if we detect a stale file handle, the directory cache is purged
 *	of that vnode to avoid repeating the error.
 *
 * Note on locking:
 *	The rules for mnode/vnode locking in this layer are described below:
 *		1) MLOCK a parent before a child (dir hierarchy)
 *		2) MLOCK a non-History Mode object before a HM object
 *		   e.g. only warp to history mode not from it!
 *		3) If no hierarchical relationship (e.g. rename), use MLOCK2
 *		4) When calling out to the FS, no MLOCK's should be held
 *		   Note that getting a vnode for a cleartext file falls
 *		   into this category.
 *
 *	Note that a refcnt on the vnode guarantees the validity of the mnode
 *	attached to it, and the stability of these fields.
 *
 *	For looback mnodes, the mn_hdr.realvp is also protected by
 *	the mtable_lock/refcnt on vnode.
 *      For vob mnodes, this field is protected by the mnode lock.
 *
 *	Any other field can change at any time unless the mnode lock is held.
 *
 *	See the notes in mvfs_mnode.c for information on locking the
 *	mnode management structures (mnum_to_mnode table, mnode hash chains
 *	vobfreelist, destroylist, and the mnode header).
 *
 * Note on use of "realvp":
 *	For loopback FS objects, the real vnode pointer is stable
 *	throughout the life of the object, and can be used freely as long
 *	as there is a refcnt on the MFS object.
 *
 *	For the vob FS objects, the cleartext vnode pointer can be
 *	purged by operations that detect it as stale (and then it is typically
 *	refetched).  This is called "cleartext revalidation" and occurs
 *	because the view/vob are allowed to move the cleartext around
 *	from one storage location to another (e.g. from view storage dir
 *	to the vob storage dir).  The pointer in the mnode is the only
 *	reference count on the cleartext vnode ptr.  When it is purged,
 *	typically the cleartext vnode goes onto the freelist and could
 *	be reused and turn into another object during the time you
 *	are waiting for your operation on it to finish.  As a result it
 * 	is not stable and 1 of two protocols must be used:
 *	1) Keep the mnode locked while you use the cleartext vnode ptr.
 *	   This prevents anyone from purging the cleartext vnode ptr so the
 *	   mnode refcnt on it will not go away.
 *	2) Lock the mnode, copy the vnode ptr into a local var, VN_HOLD() the
 *	   cleartext vnode, unlock the mnode.  Copying the vnode ptr keeps
 *	   the code immune from someone changing the contents of the mnode.
 *	   The extra refcnt on the cleartext vnode ptr will keep it off the
 *	   freelist until you are done with it.  Remember to VN_RELE() it
 *	   when you are done.
 */

#include "mvfs_systm.h"
#include "mvfs.h"
#include "mvfs_dnc.h"
#include "tbs_errno.h"
#include "mvfs_transtype.h"

/*
** Forward declarations for static routines.
*/
STATIC int MVFS_NOINLINE
mvfs_change_oid_subr(
    VNODE_T *vp,
    int mode,
    int choidflag,
    int sleepflag,
    CALL_DATA_T *cd
);

STATIC int
mvfs_groupmember(
    u_long vgid, 
    CRED_T *cred
);

int mfs_map_enabled = 1;	/* Turn on mapping by default */

#ifdef MVFS_VATTR_SLAB_ALLOC
struct mvfs_slab_list* mvfs_vattr_slabs;
#endif

/* 
 * For all ports, mvfs_vnodeops are defined in system-dependent
 * file, since the number and order of the ops in the switch varies.
 */

/*
 * MVFS_SYNC_ATTR - routine to synchronize attributes to the
 * 	view server database (if modified).  All operations
 *	are done using the saved credentials in the mnode.
 */
int
mvfs_sync_attr(
    mfs_mnode_t *mnp,
    VATTR_T *vap,
    int bhflag,
    u_int saflag,
    CALL_DATA_T *cd
)
{
    int error;
    int mtime_was_zero = 0;
    CALL_DATA_T *ncd;

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    /* 
     * Sync the view with the cleartext file if we have
     * one and some attributes credentials and the cleartext
     * is not in the VOB (you can't fix VOB attributes).  All the
     * real work is done in mfs_clnt_setattr() since
     * it merges in the right cleartext attributes to
     * the null attributes record we set up. 
     */

    if ( (mnp->mn_hdr.realvp) && (MCRED(mnp)) && (!mnp->mn_vob.cleartext.isvob) &&
         (!mnp->mn_vob.cleartext.delete_on_close) ) { 
	/* Get latest cleartext attributes */
        /*
         * By passing in the vattr (if we have one passed from
         * mfs_getattr()), we may copy only a subset of the attributes
         * from the cleartext file (if the vattr's mask only lists one
         * or a few attributes) into the view's database.
         *
         * A full update to the view of all attributes will wait until
         * a future change (or any time a caller asks for mtime: we
         * should notice it has changed on the cleartext file), or at
         * the very latest, until the file is closed (where we
         * unconditionally synchronize all file attributes).
         *
         * Note, this change delays updating of mtime in the view,
         * which is a weakening of attribute consistency when multiple
         * hosts are accessing the same view, but it's no weaker than
         * NFS will be.
         */

        (void) mvfs_clearattr(MTOV(mnp), vap, MCRED(mnp));
	
	/* Following to debug problems with 'touch' setting
	 * fstat time to 0
	 */
        if (mnp->mn_vob.attr.fstat.mtime.tv_sec == 0) {
	    mtime_was_zero = 1;
	    mvfs_log(MFS_LOG_DEBUG, "sync_attr: vw=%s vob=%s dbid=0x%x "
                     "mtime = 0x%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X" "
                     "clr mtime=0x%"KS_FMT_TV_USEC_T_X"\n",
                     mfs_vp2vw(mnp->mn_hdr.vp),
                     mfs_vp2dev(mnp->mn_hdr.vp),
                     mnp->mn_hdr.fid.mf_dbid,
                     mnp->mn_vob.attr.fstat.mtime.tv_sec, 
                     mnp->mn_vob.attr.fstat.mtime.tv_usec,
                     VATTR_GET_MTIME(&mnp->mn_vob.cleartext.va));
	}

        ncd = MVFS_ALLOC_SUBSTITUTE_CRED(cd, MCRED(mnp));
        if (ncd == NULL) {
            error = ENOMEM;
        } else {
	    error = mvfs_clnt_setattr_locked(MTOV(mnp), NULL, 0, bhflag, 
		                             MWCRED(mnp), ncd, saflag);
        }
        MVFS_FREE_SUBSTITUTE_CRED(ncd);
	if (error) {
	    mvfs_logperr(MFS_LOG_WARN, error, 
		"mvfs_sync_attr: vw=%s vob=%s dbid=0x%x", 
		    mfs_vp2vw(mnp->mn_hdr.vp),
		    mfs_vp2dev(mnp->mn_hdr.vp),
		    mnp->mn_hdr.fid.mf_dbid);
	} else if (mtime_was_zero) {
	    mvfs_log(MFS_LOG_DEBUG, "sync_attr: after: mtime=0x%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X"\n",
		mnp->mn_vob.attr.fstat.mtime.tv_sec,
		mnp->mn_vob.attr.fstat.mtime.tv_usec);
	}
    } else {
	MDB_XLOG((MDB_CTIME, "sync_attr: nothing to do (mcred=%"KS_FMT_PTR_T")\n", MCRED(mnp)));
	error = 0;
    }
    return(error);
}

/*
 * Routine to set the 'audited' bit on an object if not already
 * set.  Used to record that an object has been included in
 * an audit (from read/write/truncate/choid) so that the system knows
 * if a choid is required or not.
 *
 * The only place this is called is from the macro MFS_AUDIT.
 * This routine must handle ANY kind of object.
 */

void
mfs_set_audited(
    VNODE_T *vp,
    CALL_DATA_T *cd
)
{

    mvfs_thread_t *mth;
    mfs_mnode_t *mnp = VTOM(vp);
    int error = 0;
    timestruc_t stime;	/* For statistics */
    timestruc_t dtime;

    MDKI_HRTIME(&stime);

    /* Only really do anything for VOB objects for now */

    if (!MFS_ISVOB(mnp)) return;

    if (!MVFS_ISVTYPE(vp, VREG)) return;	/* Only set audited on reg files */

    /* 
     * If auditing enabled and the audit bit isn't on in
     * the object stats then must set the audited bit in
     * the object stat record.  This is not done for RO objects
     * (winked in DO's) because they are immutable and need a choid
     * anyways (at which time they become a view-private file via
     * copy-on-write, and the choid op will set the "audited" bit as
     * appropriate).
     * 
     * This info is used to indicate whether a choid is needed by others 
     * writing this file.
     */

    mth = MVFS_MYTHREAD(cd);
    if (mth->thr_auditon) {
	MLOCK(mnp);
	if (mnp->mn_hdr.realvp == NULL)	/* probably a failed COW */
	    error = EIO;		/* only for reporting below */
	else if (!MFS_FSTAT_AUDITED(mnp) && !MFS_CLRTEXT_RO(mnp)) {
	    error = mvfs_clnt_setattr_locked(vp, NULL, 
			TBS_FMODE_AUDITED_OBJ, MFS_USE_PROCBH, 0,
                        cd, 0);
	}
	MUNLOCK(mnp);
        /*
         * No one to return the error to, so display a log message
         * for any errors.  This is important evidence to have since it is
         * likely that later, a choid will not be done to the object,
         * and the user will report a bug because clearmake did not
         * rebuild when he expected it to.
         */

	MVFS_BUMPTIME(stime, dtime, mfs_austat.au_settime);
	mvfs_logperr(MFS_LOG_ERR, error, 
		"unable to set audited bit on object vw=%s vob=%s dbid=0x%x", 
			mfs_vp2vw(vp), mfs_vp2dev(vp), mfs_vp2dbid(vp));
    }
}

STATIC int MVFS_NOINLINE
mvfs_change_oid_subr(
    VNODE_T *vp,
    int mode,		/* File open flags (FREAD, FWRITE, etc..) */
    int choidflag,	/* MFS_CHOID_TRUNC, MFS_CHOID_FORCE */
    int sleepflag,
    CALL_DATA_T *cd
)
{
    mvfs_thread_t *mth;
    mfs_mnode_t *mnp = VTOM(vp);
    CLR_VNODE_T *ocvp = NULL;
    CLR_VNODE_T *ncvp = NULL;
    int choid_done = 0;
    VATTR_SIZE_T len;
    view_change_oid_option_t choid_opt;
    int error;
    int copy_on_write;
    char *op_str = "choid";
    char *cl_nm = NULL;
    /* Declare a type so we can do one allocation to save stack space. */
    struct {
        VATTR_T va;
        tbs_oid_str_t oid_str;
        tbs_oid_t prevoid, newoid;
        char *endchar;
    } *alloc_unitp;
    VATTR_T *vap;
    tbs_oid_str_t *oid_strp;

    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));
    ASSERT(mnp->mn_hdr.realvp);

    if (!MVFS_ISVTYPE(vp, VREG)) {
        mvfs_log(MFS_LOG_INFO, "attempt to copy-on-write nonfile object\n");
        return(EISDIR);
    }

    /* Allocate these for future use (to save stack space). */
    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }

    /* This was added to avoid BSOD on windows during the nightly builds using Visual Studio.
     * The reason being that we store the OID in the struct alloc_unitp and we pass it as a
     * string to MFS_AUDIT. Since this oid filed used to lie at the end of struct and whenever
     * this is allocated (it is allocated in non-paged area) at the end of a page, this used to
     * issue page-fault (from a non-paged area), while reading this oid by strlen function. To
     * avoid this we put an extra char at the end of struct to prevent the reading off of this struct.
     */
    alloc_unitp->endchar = NULL;

    /* vap is initialized below before first use and oid_strp is only used as
    ** the return from mvfs_oid_to_str() below.
    */
    vap = &(alloc_unitp->va);
    oid_strp = &(alloc_unitp->oid_str);

    mth = MVFS_GET_THREAD(cd);
    /*
     * For now, both explicit force, and any truncate attempt will
     * force the object to be choided.   Although not strictly necessary,
     * forcing the choid on all truncates leads to a more robust system.
     */

    if ((choidflag & (MFS_CHOID_FORCE|MFS_CHOID_TRUNC)) == 0  && 
						!MFS_CLRTEXT_RO(mnp)) {
        /* 
         * Suppress duplicate choids - they are expensive.
         * This is particularly true for non-atria access where
         * all we see is a continuous stream of Write operations
         * without any open/close.
         */

        /* 
         * Strategy 1 - if OID not audited (potentially in a CR), 
	 * there is no need for a choid
         */

	if (!MFS_FSTAT_AUDITED(mnp)) {
            error = 0;
            goto errout;
	}

  	/* 
	 * Strategy 2 - maintain a cache of the BH that the last
	 * choid was done under.  Additional (non-forced) choids
         * under this same BH will be suppressed.  This optimization
	 * is required because an object may be choided under a build,
	 * which means that the 'audited' bit is set (because the
	 * choid put that OID in the audit file), but the object should
	 * not be repeatedly choided.
	 *
	 * Both the build-handle and an MFS-internal build-handle-sequence
	 * number must be checked.  A choid is required by clearmake for each
	 * different audit in clearmake, even when the two audits have
	 * the same build handle (frozen ref time).  So, the MFS
	 * maintains a sequence number which is incremented every
	 * time there is a 'start audit' done.  If this is a 
	 * different audit (even with the same BH), then a choid must
	 * be done.
         * e.g. if a DO lib.a is written in target foo.o and 
         *      in target bar.o, there should be two choids
         *	because they are different targets (different audits)
         * 	even though the build session is the same for both
	 */

	if (mnp->mn_vob.choid_audited &&
		MFS_BHEQ(mnp->mn_vob.choid_bh, mth->thr_bh) &&
		mnp->mn_vob.choid_bh_seq == mth->thr_aud_seq) {
            error = 0;
	    goto errout;
	}

    }

    /* 
     * We want to do the choid.  Check the sleepflag and
     * return EWOULDBLOCK if the caller does not want to sleep.
     */

    if (sleepflag == MFS_NOSLEEP) {
	error = EWOULDBLOCK;
	goto errout;
    }

    /*
     * Set common choid options for the choid operation
     */
    choid_opt = (mth->thr_auditon) ?
	VIEW_CHANGE_OID_SET_AUDIT : VIEW_CHANGE_OID_CLR_AUDIT;
  retry:
    copy_on_write = MFS_CLRTEXT_RO(mnp);

    /*
     * If the object is already view-private, we only have to
     * do a simple choid to the view.  Otherwise, we must do 
     * a complete copy-on-write operation.
     */

    if (!copy_on_write) {		/* Choid only */
        if ((cl_nm = mnp->mn_vob.cleartext.nm) == NULL) cl_nm = "not cached";
        mvfs_log(MFS_LOG_DEBUG, "clearops: %s: prevoid=%s, prevnm: %s\n", 
                 op_str, mvfs_oid_to_str(&mnp->mn_vob.attr.obj_oid, *oid_strp), cl_nm);
	error = mfs_clnt_choid_locked(vp, choid_opt, &alloc_unitp->prevoid, cd);
	if (error) goto errout;
	choid_done = 1;
    } else {				/* Copy-on-write */
        /* mustn't be writers if it's currently read-only */
        ASSERT(mnp->mn_vob.open_wcount == 0);
	if (mnp->mn_vob.open_count != 0 && mnp->mn_vob.open_wcount == 0) {
	    mvfs_log(MFS_LOG_DEBUG, "choid copy-on-write TXTBSY\n");
            /*
             * We can't allow this to succeed.  Most systems have file locking
             * state attached to the cleartext vnode, and if we retarget
             * the cltxt, the locking state gets incorrectly abandoned.
             */
	    error = ETXTBSY;
	    goto errout;
	}
        error = mfs_getcleartext(vp, &ocvp, cd);
        if (error) goto errout;

        /* Get latest/best length of old cleartext */

	error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
	if (error) goto errout;
    	len = VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va);

	/* Do the choid */
        op_str = "cow";
        if ((cl_nm = mnp->mn_vob.cleartext.nm) == NULL) cl_nm = "not cached";
        mvfs_log(MFS_LOG_DEBUG, "clearops: %s: prevoid=%s, prevnm: %s\n", 
                 op_str, mvfs_oid_to_str(&mnp->mn_vob.attr.obj_oid, *oid_strp), cl_nm);

	error = mfs_clnt_choid_locked(vp, choid_opt, &alloc_unitp->prevoid, cd);
	if (error) goto errout;
	choid_done = 1;

  	/*
	 * Expect cleartext to change... so purge the old
         * cleartext binding.
         */

	mfs_clear_mark_name_purge(vp);	/* Mark for purge */
	mfs_clear_rele(vp, MVFS_CD2CRED(cd));	/* And do the purge now */

        /* 
         * Old cleartext was in the vob.  The new one should be
         * a private file in the view (because of the choid), so
         * we need to create it and (if not truncate op) copy the old
         * file over to the new file.
         */

        VATTR_NULL(vap);
    	VATTR_SET_TYPE(vap, VREG);
    	/*
     	 * This is OK for clear_create mode because choid changed the
     	 * mode in the stats (in the returned RPC response) from read-only 
	 * to the correct write permissions.
     	 */
    	VATTR_SET_MODE_TYPE(vap, S_IFREG);
    	VATTR_SET_MODE_RIGHTS(vap, 
		(mnp->mn_vob.attr.fstat.mode & ~TBS_FMODE_AUDITED_OBJ) & 
			  ~MDKI_GET_U_CMASK());
    	VATTR_SET_SIZE(vap, 0);
    	VATTR_SET_MASK(vap, AT_TYPE|AT_MODE|AT_SIZE);
    	error = mfs_clear_create(vp, vap, &ncvp, cd, mode);  /* no FOFFMAX */
    	switch (error) {
          case 0:                       /* all's well */
            break;
          case EEXIST:
            /*
             * we probably raced some other MVFS client on initiating a COW
             * operation.
             * Since this client might want to get the COW results, retry
             * the whole choid.
             *
             * The cleartext has been dropped as a result of
             * mfs_clear_create()'s errors.
             *
             * FIXME: do we care about this client seeing
             * indeterminate state of the underlying file, if the
             * other MVFS client is in the middle of copying the file
             * bits?  (That can already happen without doing this
             * restart, if there was no race but an MVFS client just
             * happened to start reading the file while it was being
             * copied).
             */
            /*
             * The COW file already exists, so the object should have
             * been converted to a view-private file and the mnode
             * attributes updated to reflect this as of the reply to
             * the mfs_clnt_choid_locked() call above.  (Even if it
             * has since transitioned back to a read-only cleartext,
             * we do want to retry the COW, but the ASSERT would fail.
             * In a debugging kernel, we can tolerate that.)
             */
            ASSERT(!MFS_CLRTEXT_RO(mnp));
            goto retry;
          default:
            goto errout;
        }

    	ASSERT(mnp->mn_hdr.realvp);

        /*
         * MVFS used to re-open the cltxt here, if we had existing
         * opens.  However, we don't permit copy-on-write operations
         * if there are any existing opens (because switching to a new
         * cltxt file meant we would abandon things like file locks
         * which were held on the original cltxt vnode).  So rather
         * than do the re-opening work (which can't be done right on
         * some platforms, e.g. Linux), we just ASSERT the proper open
         * state.
         */
        ASSERT(mnp->mn_vob.open_wcount == 0 && 
               mnp->mn_vob.open_count == 0);

	if (ocvp == ncvp) {
	    error = ENXIO;
	    mvfs_log(MFS_LOG_ERR, "copy-on-write: duplicate cleartext\n");
	    goto errout;
	}

        /* 
         * Finally, copy the original into the destination 
         * (if not truncating anyways)
         */

        if ((choidflag & MFS_CHOID_TRUNC) != MFS_CHOID_TRUNC) {
            error = MVFS_COPYVP(ocvp, ncvp, len, MVFS_CD2CRED(cd));
        }
    }

errout:
    if (ocvp) CVN_RELE(ocvp);
    if (ncvp) CVN_RELE(ncvp);

    /* 
     * Don't print error unless got past choid - choid errors are 
     * common and normal as they are the first time permissions 
     * are checked. 
     */
    if (error && choid_done) {
	mfs_clearperr(vp, "cleartext copy-on-write", error);
    }
    if (choid_done) {		/* Record choid in audit */

	/*
	 * Record choid done under the current audit build handle
	 * for the choid suppression at the beginning
	 */
	MFS_REMEMBER_CHOID_BH(mth, mnp);

        /*
         *
         * Copy out oid before we unlock below to avoid
         * getting a possibly older oid from a lookup that is
         * in progress, but stalled waiting for the mnode lock.
         *
         *    Thread 1              Thread 2
         *                          Create "file" (RPC 1)
         *                          Open "file"
         *    Lookup "file" (RPC 2) MLOCK(mnp)
         *    MLOCK(mnp)            Change OID (RPC 3)
         *         .                MUNLOCK(mnp)
         *    Store Attr            Audit CHOID
         *
         * Audit will possibly pick up older OID from Lookup (returned from RPC 2).  This
         * is more likely on an MP machine.
         *
         * The store of attributes above also will back up the attributes.
         * This is not good, but without some sequencing from the view_server
         * for all view vstats returned, we can't do anything about it (we don't
         * want to rely on ctime since time can be backed up by resetting the
         * hosts clock).  This is why the fix is considered incomplete.
         */

        alloc_unitp->newoid = mnp->mn_vob.attr.obj_oid;

	/* Unlock to avoid deadlocks with the auditing */

	MUNLOCK(mnp);
	
        MFS_AUDIT(MFS_AR_CHOID,NULL,(char *)&alloc_unitp->newoid,NULL,(char *)&alloc_unitp->prevoid,vp,cd);
	MLOCK(mnp);		/* Re-lock for caller */
	/* 
	 * In case someone purged the cleartext while we were auditing,
         * we must restore it here.  Callers expect a cleartext to be
         * valid on non-error return from this routine.
         */
	if (!error) error = mfs_getcleartext(vp, NULL, cd);
    }

    /* Log msg: op type (choid-only/cow), uid, cmd, oid, cleartxtname, vp, mnp, err */
    mvfs_log(MFS_LOG_DEBUG, "clearops: %s: uid=%d, cmd=%s\n", 
             op_str, MDKI_CR_GET_UID(MVFS_CD2CRED(cd)), MDKI_GET_U_COMM_PTR());
    if ((cl_nm = mnp->mn_vob.cleartext.nm) == NULL) cl_nm = "not cached";
    mvfs_log(MFS_LOG_DEBUG, "clearops: %s: newoid=%s, newnm: %s\n", 
             op_str, mvfs_oid_to_str(&mnp->mn_vob.attr.obj_oid, *oid_strp), cl_nm);
    MDB_XLOG((MDB_CLEAROPS,"%s: vp=%"KS_FMT_PTR_T", mnp=%"KS_FMT_PTR_T", err=%d nm=%s\n", 
              op_str, vp, mnp, error, mnp->mn_vob.cleartext.nm));

    MVFS_EXIT_FS(mth);
    KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
    return (error);
}

int
mfs_change_oid(
    VNODE_T *vp,
    int choidflag,		/* MFS_CHOID_TRUNC, MFS_CHOID_FORCE */
    int sleepflag,
    CALL_DATA_T *cd
)
{
    return(mvfs_change_oid_subr(vp, FREAD|FWRITE, choidflag, sleepflag, cd));
}

#ifdef MVFS_MMAP_CVP
/*
 * The following utility routines are compiled and used in some of the
 * Unix VM implementations.
 */
/*
 * This function saves cleartext vp in *cvpp with a refcnt on hold
 * upon successful return. *cvpp is set to NULL, if any error happens.
 *
 * MVFS_MMAP_GETCVP: get and hold cvp for mmap.
 */
int
mvfs_mmap_getcvp(
    VNODE_T *vp,
    CLR_VNODE_T **cvpp,
    u_int mflags,
    u_int prot,
    CALL_DATA_T *cd
)
{
    int error = 0;
    mfs_mnode_t *mnp = VTOM(vp);

    /* Only VOBCLAS mnode can have mmap */
    ASSERT(MFS_ISVOB(mnp));

    MLOCK(mnp);
    *cvpp = NULL;

    if ((prot & PROT_WRITE) && (vp->v_vfsp->vfs_flag & VFS_RDONLY)) {
        error = EROFS;
        goto out;
    }

    *cvpp = MFS_CVP(vp);
    if (*cvpp == NULL) {
        if ((error = mfs_getcleartext(vp, cvpp, cd)) != 0)
            goto out;
    } else {
        CVN_HOLD(*cvpp);
    }
    
    /* 
     * Must not be a cleartext in the VOB if shared
     * write access.  The open() call was supposed to fix.
     */
    if ((mflags & MAP_SHARED) && (prot & PROT_WRITE) &&
        !mfs_clear_writable(vp))
    {
        error = EROFS;
        CVN_RELE(*cvpp);
        *cvpp = NULL;
    }

  out:
    MUNLOCK(mnp);
    return error;
}

/* 
 * MVFS_MMAP_NO_AUDIT - routine to work around inability to audit page
 * read/write if we map the underlying vnode at mmap time.
 * Pre-records a read and write audit if the memory map operation is
 * for shared write, or just a read audit if the map is for read.
 * mvfs_enter/exit_fs should be executed by its caller.
 */

void
mvfs_mmap_no_audit(
    VNODE_T *vp,
    u_int mflags,
    u_int prot,
    CALL_DATA_T *cd
)
{
    mfs_mnode_t *mnp = VTOM(vp);
    int choidflags, error = 0;

    if (prot & PROT_READ) {
	MFS_AUDIT(MFS_AR_READ, NULL, NULL, NULL, NULL, vp, cd);
    }
    if ((mflags & MAP_SHARED) && (prot & PROT_WRITE)) {
	if (MFS_ISVOB(mnp)) {
	    /* Don't try to choid on a loopback node! */
	    /*
	     * We need to do a CHOID here, because it could be an open-less
	     * mmap, e.g. the NFS server.
	     */
	    MLOCK(mnp);
	    error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
	    if (!error) {
		/* Force choid; no open to rely on if NFS */
		choidflags = MFS_CHOID_FORCE;
		if (VATTR_GET_SIZE(&(mnp->mn_vob.cleartext.va)) == 0)
		    choidflags |= MFS_CHOID_TRUNC;
		error = mfs_change_oid(vp, choidflags, MFS_SLEEP, cd);
	    }
	    MUNLOCK(mnp);
	}
	if (!error) {
	    MFS_AUDIT(MFS_AR_WRITE, NULL, NULL, NULL, NULL, vp, cd);
	}
    }
}

#endif /* MVFS_MMAP_CVP */

/*ARGSUSED*/

/*
 * MFS_OPENV - vnode open operation.
 *
 * CWD REBIND NOTE:
 *	The cwd can not be rebound in the open call.  Once the FS has
 *      decided to open the wrong fd, you can not convince it otherwise
 *      and had better not do the getattr etc. on a different vnode
 *      or incorrect results will occur.
 */
int
mfs_openv(
    VNODE_T **vpp,
    int mode,		/* File open flags (FREAD, FWRITE, etc..) */
    CALL_DATA_T *cd
)
{
    return(mvfs_openv_ctx(vpp, mode, cd, TRUE, NULL));
}

int
mfs_openv_subr(
    VNODE_T **vpp,
    int mode,		/* File open flags (FREAD, FWRITE, etc..) */
    CALL_DATA_T *cd,
    int do_vop
)
{
    return mvfs_openv_ctx(vpp, mode, cd, do_vop, NULL);
}

int
mvfs_openv_ctx(
    VNODE_T **vpp,
    int mode,
    CALL_DATA_T *cd,
    int do_vop,
    MVFS_OPEN_CTX_T *ctxp
)
{
    VNODE_T *vp = *vpp;
    VNODE_T *xvp;
    int error, ctxt_retry_done;
    CLR_VNODE_T *cvp;
    mfs_mnode_t *mnp;
    int choidflag;
    MVFS_DECLARE_THREAD(mth)
    /* Declare a type so we can do one allocation to save stack space. */
    struct {
            timestruc_t stime;	/* For statistics */
            timestruc_t dtime;
            timestruc_t stime1;
    } *alloc_unitp;
    int ct_err;
    VATTR_T *cvap = NULL;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

#ifdef NFSV4_SHADOW_VNODE
    tbs_oid_t prev_oid;
#endif

    MVFS_ENTER_FS(mth);
    error = 0;

    /* Allocate these for future use (to save stack space). */
    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }

    MDKI_HRTIME(&(alloc_unitp->stime));	/* Fetch start time for stats */
    error = ctxt_retry_done = 0;
    ASSERT(VTOM(vp)->mn_hdr.vp);
    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    error = 0;
	    break;
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS: 	/* Pass on to real vnode */
	    if (do_vop) {
		cvp = MFS_CLRVP(vp);	/* Keep realvp safe */
		error = MVOP_OPEN(&cvp, mode, cd, ctxp);
	    }
	    break;
	case MFS_NTVWCLAS:
	    error = 0;		/* Allow any open */
	    break;
	case MFS_VIEWDIRCLAS:
	    error = mfs_viewdiropen(vpp, mode, MVFS_CD2CRED(cd));
	    break;
	case MFS_VOBRTCLAS:
	    vp = mfs_bindroot(*vpp, cd, &error);
	    if (error) {
		if (error == ESRCH) error = 0;	/* Null view OK */
		break;				/* Exit */
	    }
	    /* Fall Through */
	case MFS_VOBCLAS: {

	    /* 
	     * Preset error return to OK.
	     */

	    error = 0;

	    /* Rebind the vnode op */
 
	    (void) mfs_rebind_vpp((vp != *vpp), &vp, cd);

	    mnp = VTOM(vp);

            if ((mode & (FWRITE|FTRUNC|FAPPEND))
                && (vp->v_vfsp->vfs_flag & VFS_RDONLY) != 0)
            {
                error = EROFS;
                break;
            }

	    /*
             * For directories, return EISDIR if the open mode
	     * includes write access.
	     */
	    if (MVFS_ISVTYPE(vp, VDIR)) {
		if (mode & (FWRITE|FTRUNC|FAPPEND)) {
		    error = EISDIR;
                    break;
                }
	    }

	    /* 
	     * For regular files, get cleartext and if an open for
	     * write then choid the file
	     */
		
	    if (MVFS_ISVTYPE(vp, VREG)) {
                /* Get latest attributes if close to open consistency */
                /* Do this before getting cleartext, in case a winked-in VOB
                   DO has moved to the view (copy-on-write).  It'll get
                   noticed and the cleartext flushed in that case. */

                if (mcdp->mvfs_ctoenabled && VIEW_ISA_VIEW_OBJ((&mnp->mn_vob.vfh))) {
                    /* No need to get latest attributes if a VOB object.
                     * Strict close to open consistency is less important here
                     * as the object is immutable with regards to length,
                     * dates and data contents.  This does open
                     * a window where the owner/rights can be wrong
                     * do to a cleartool protect on the element, but
                     * the system has already checked these before this
                     * open and done the wrong thing, so being more 
                     * aggressive doesn't really help anything.  This
                     * optimization can save 40 to 50% of the 'getattrs'
                     * that are done to the view_server in a typical
                     * build with lots of little header files included.
                     *
                     * I don't do this for non-VREG objects, because I
                     * want strong consistency for 'dirs'.  e.g. ls
                     * or anyone doing a readdir would validate the name-cache
                     * by refetching the most up-to-date stat for the dir.
                     */

                    /* Force getattr to view to get latest attributes */
                    /* May notice a changed vob DO as a bonus :) */

                    MDKI_HRTIME(&(alloc_unitp->stime1)); /* Fetch start time for stats */
                    mfs_clnt_getattr(vp, cd);
                    MVFS_BUMPTIME((alloc_unitp->stime1), (alloc_unitp->dtime),
                                 mfs_clearstat.cto_getattr_time);

                    BUMPSTAT(mfs_acstat.ac_cto);
                    BUMPVSTAT(vp,acstat.ac_cto);
                    BUMPSTAT(mfs_acstat.ac_misses);
                    BUMPVSTAT(vp,acstat.ac_misses);
                }

    		MLOCK(mnp);

		/* Make sure a cleartext before any choid call */
		error = mfs_getcleartext(vp, NULL, cd);
		if (error) {
                    /* print error if even one other current open
                       (getcltxt logs if open_count > 1) */
                    if (error == ESTALE && mnp->mn_vob.open_count > 0)
                        mvfs_clear_log_stale(vp);
                    mvfs_log(MFS_LOG_DEBUG,
                       "mvfs_openv_ctx: 1st mfs_getcleartext(%p) returned %d\n",
                        vp, error);
		    MUNLOCK(mnp);
		    break;
		}

                /*
                 * Don't choid on FCREAT if it's FREAD|FCREAT.
                 * That's a stupid but valid flag setting.  If the
                 * file didn't exist, VOP_CREATE() already did the
                 * creation for us and it's allocated a new OID.
                 */
		if (mode != (FREAD|FCREAT) &&
                    (mode & (FWRITE|FCREAT|FAPPEND|FTRUNC)))
                {
		    choidflag = MFS_CHOID_FORCE;
		    if (mode & FTRUNC) choidflag |= MFS_CHOID_TRUNC;
		    if (MFS_CLRTEXT_RO(mnp) &&
			mnp->mn_vob.open_count != 0 &&
			mnp->mn_vob.open_wcount == 0)
		    {
			mvfs_log(MFS_LOG_DEBUG, "open copy-on-write TXTBSY\n");
			error = ETXTBSY;
			MUNLOCK(mnp);
			break;
		    }
                    /* mvfs_change_oid_subr() unlocks the MLOCK while it does
                    ** auditing and then reaquires it.  Things could have
                    ** changed while it was unlocked, but analysis seems to show
                    ** that we don't have to redo the mfs_getcleartext() and
                    ** the vob object checks we did above because they couldn't
                    ** have changed that much.  Besides, we redo the
                    ** mfs_getcleartext() call just below.
                    */
		    error = mvfs_change_oid_subr(vp, mode, choidflag, 
                            MFS_SLEEP, cd);
		    if (error) {
			mfs_clear_error(vp, "change oid failed", error);
			MUNLOCK(mnp);
			break;
		    }
		    /* Save write credentials for deferred operations */
        	    MSETCRED(mnp, 1, MVFS_CD2CRED(cd));
		} else {
		    /* Save read credentials for deferred operations */
		    MSETCRED(mnp, 0, MVFS_CD2CRED(cd));
		}

		/* 
		 * Get and hold cleartext after any choids 
		 * Note that choid may have changed the cleartext vnode.
		 */

		error = mfs_getcleartext(vp, &cvp, cd);
		if (error) {
                    /* print error if even one other current open
                       (getcltxt logs if open_count > 1) */
                    if (error == ESTALE && mnp->mn_vob.open_count > 0)
                        mvfs_clear_log_stale(vp);
                    mvfs_log(MFS_LOG_DEBUG,
                       "mvfs_openv_ctx: 2nd mfs_getcleartext(%p) returned %d\n",
                        vp, error);
		    MUNLOCK(mnp);
		    break;
		}

		/* Open the cleartext in tandem with the vnode.
	         * Many system count/match up various opens/closes, so
		 * we do this to the cleartext.  
	         * This also solves problems with file lock semantics 
		 * that depend on correct open/close boundaries for the 
		 * cleartext.
		 * 
		 * Once the cleartext vnode ptr is fetched, it isn't
	         * released until the vnode is inactivated, no matter
		 * what errors occur.
		 * 
		 * The MFS maintains an open_count in the mnode to tell
		 * mfs_getcleartext() whether it is OK to rebind stale
		 * cleartexts or not.  If the vnode is open, then the
		 * cleartext can't be rebound, because we don't know
		 * how many of each 'mode' of opens to perform for the
		 * underlying file system, or what 'modes' of opens it
		 * might care about.  This counter must be bumped
		 * AFTER the getcleartext() above, during which it
		 * is OK to rebind the cleartext if this is the first
		 * open.
		 */
ctxt_open:
#ifdef NFSV4_SHADOW_VNODE
                /* NFSv4 compliance: RATLC01011478: Saving the oid before
                 * MVOP_OPEN. Cleartext vnode can change during MVOP_OPEN
                 * either due to a COW or due to the NFSv4 open symantics.
                 * 'prev_oid' will be used to differentiate between the two
                 * cases.
                 * Refer the CR for more details on NFSv4 open symantics.
                 */
                prev_oid = mnp->mn_vob.attr.obj_oid;
#endif
        	if (do_vop) {
		    MDKI_HRTIME(&(alloc_unitp->stime1)); /* Fetch start time for stats */
		    error = MVOP_OPEN(&cvp, mode, cd, ctxp);
                    if (error) {
                        mvfs_log(MFS_LOG_DEBUG, "openv: mnode=%x error=%d\n", mnp, error);
                    }
		    MVFS_BUMPTIME((alloc_unitp->stime1), (alloc_unitp->dtime),
                                 mfs_clearstat.clearopen_time);
		    BUMPSTAT(mfs_clearstat.clearopen);
                    /* RATLC00699167: NFSv3 caching causes atime to not be
                     * updated until next mod of file.  To prevent scrubber
                     * erroneously removing containers we are using but not
                     * modifying, we periodically force update of atime.
                     * Issue for vob containers only.
                     * Default interval is 5 minutes; undocumented tunable.
                     */
                    if (mnp->mn_vob.cleartext.isvob && !error) {
                        if (mnp->mn_vob.cleartext.atime_pushed == 0) {
                            /* No push, but start tracking interval */
                            mnp->mn_vob.cleartext.atime_pushed = MDKI_CTIME();
                        } else {
                            if (MDKI_CTIME() >
                                 (mnp->mn_vob.cleartext.atime_pushed
                                + mcdp->mvfs_ctxt_atime_refresh)) {

                                if ((cvap = MVFS_VATTR_ALLOC()) == NULL) {
                                    error = ENOMEM;
                                    MUNLOCK(mnp);
                                    mvfs_log(MFS_LOG_DEBUG, "openv: error setting ctxt atime mnode=%x cvp=%x error=ENOMEM\n",mnp, cvp);
                                    break;
                                }
                                VATTR_NULL(cvap);
                                VATTR_SET_MASK(cvap, AT_ATIME);
                                VATTR_SET_ATIME_TS(cvap, &(alloc_unitp->stime1));
                                if ((ct_err = MVOP_SETATTR(cvp, cvap, 0, cd, ctxp)) != 0) {
                                    /* error not truly fatal here, but open
                                     * just succeeded, so certainly unexpected.
                                     */
                                    mvfs_log(MFS_LOG_DEBUG, "openv: error setting ctxt atime mnode=%x cvp=%x error=%d\n",mnp, cvp, ct_err);
                                } else {
                                    mnp->mn_vob.cleartext.atime_pushed = MDKI_CTIME();
                                }
                                MVFS_VATTR_FREE(cvap);
                            }
                        }
                    }
		} else {
                    MDB_VLOG((MFS_VOPEN, "openv: MVOP_OPEN skipped vp=%x\n", cvp));
                }
		if (!error) {
                    mnp->mn_vob.open_count++;
                    /*
                     * We need to tally number of opens
                     * done with write enabled. This is so
                     * mfs_clear_rele can correctly
                     * close the cleartext vnode.
                     */
                    if ((mode & FWRITE) == FWRITE) {
                        ASSERT(!MFS_CLRTEXT_RO(mnp));
                        mnp->mn_vob.open_wcount++;
                    }

                    /* Check if cvp returned by MVOP_OPEN is different from
                     * the cleartext vnode saved in realvp. cvp could be
                     * different because of:
                     *  copy-on-write
                     */
		    if (cvp != mnp->mn_hdr.realvp) {
#ifdef NFSV4_SHADOW_VNODE
                        /* NFSv4 Compliance: RATLC01011478: NFSv4 protocol is
                         * stateful and implements the open and close procedures.
                         * NFSv4 implementation on HP-UX 11.31 and Solaris 10
                         * introduces the concept of shadow vnodes in order to
                         * handle hard links. NFSv4 on Linux does not implement
                         * shadow vnodes. NFSv4 client allocates a vnode for a 
                         * file when it is looked up for the first time (master vnode).
                         * If the same file is accessed via a different pathname,
                         * NFSv4 client allocates a shadow vnode which points to the
                         * same rnode pointed to by the master. In NFSv4 
                         * implementations that involve shadow vnodes, NFSv4 always
                         * returns the master vnode on VOP_OPEN of a shadow vnode,
                         * as only the master vnode holds the file's resources.
                         * The subsequent file operations will use the master vnode.
                         *
                         * The shadow-master association is encountered in MVFS 
                         * only when the view server is local to the MVFS client
                         * and VOB storage is NFSv4 mounted. This is due to the side
                         * effect of cleartext generation by view server. 
                         * In the generation of cleartext for a regular file
                         * and in wink-in of derived objects, the view server uses
                         * a temporary file provided to it by the VOB RPC server.
                         * After the cleartext generation and necessary validations,
                         * view server renames this temporary to the fully qualified
                         * cleartext pathname supplied to it by the VOB RPC server.
                         * When the temporary file was looked up for the first time
                         * by the view server, NFSv4 client retrieves the file handle
                         * corresponding to it from the NFSv4 server and allocates the
                         * first vnode (master vnode). After the rename when MVFS 
                         * accessed the fully qualified pathname, NFSv4 client queries
                         * the NFSv4 server and is provided the same file handle as in
                         * case of the temporary file. As the file handles are
                         * same, NFSv4 client allocates a new vnode (shadow vnode)
                         * and assoicates it with the vnode (master vnode) created
                         * for the temporary file. This shadow-master association
                         * persists until NFSv4 client invalidates the master vnode
                         * (as it is referring to the temporary file that no longer
                         * exists).
                         *
                         * During MVOP_OPEN, the vnode can change due to a COW
                         * as well. If it has changed due to a COW, the OID would
                         * also change. OID does not vary if the vnode has
                         * changed due to NFSv4 client open operation. If
                         * the OID has not changed, but the vnode has, then this
                         * must be the case of NFSv4 implementing shadow vnodes.
                         * Save the new vnode (master vnode) in cleartext cache
                         * as realvp_master for later use. The shadow vnode in
                         * realvp would remain as is.
                         * Refer the CR for details.
                         */
                        if (MFS_OIDEQ(prev_oid, mnp->mn_vob.attr.obj_oid)) {
                            if ((mnp->mn_hdr.realvp_master != NULL) &&
                                   (mnp->mn_hdr.realvp_master != cvp)) {
                                mvfs_log(MFS_LOG_ERR, "open: unexpected multiple NFSv4 master vnodes. master1=%"
                                             KS_FMT_PTR_T", master2=%"KS_FMT_PTR_T"\n",
                                             mnp->mn_hdr.realvp_master, cvp);
                                CVN_RELE(mnp->mn_hdr.realvp_master);
				mnp->mn_hdr.realvp_master = NULL;
                            } 

                            /* Increment the hold count on the master vnode to
                             * accomodate CVN_RELE at the end of the routine
                             */
                            CVN_HOLD(cvp);

			    /* NFSv4 has transferred a reference count on the shadow
                             * to a reference on the master vnode. That is the reference
                             * to be used here.
                             */
                            mnp->mn_hdr.realvp_master = cvp;
                        } else {
                            mvfs_log(MFS_LOG_ERR, "open: cleartext changed!\n");
                            mvfs_new_cltxt(vp, cvp);
                        }
#else
			mvfs_log(MFS_LOG_ERR, "open: cleartext changed!\n");
			/* 
			 * Pass through of cleartext copy-on-write.
			 * Release old cleartext vnode, and 
			 * assign the new cleartext vnode, and take
			 * an extra hold count for the assignment.
                         * We must inhibit any paging that could use
                         * the cleartext (without the mnode lock)
                         * when dropping the cleartext vnode (in those
                         * ports that need this protection)
			 */
                        mvfs_new_cltxt(vp, cvp);
#endif
		    }
		} else {
		    /* If ESTALE, purge the current cleartext binding and
		     * try again (once).  If our timing is bad, the scrubber may
		     * have taken away our cleartext, even though the
		     * revalidate timestamp says it is okay.
		     */
		    if (error == ESTALE && mnp->mn_vob.open_count == 0 && !ctxt_retry_done) {
			mvfs_log(MFS_LOG_ESTALE,"open: retry stale cltxt, cvp %"KS_FMT_PTR_T"\n",cvp);
			/*
			 * mfs_getcleartext takes a reference on the VNODE. We
			 * must release it before purging the cleartext.
			 */

			CVN_RELE(cvp);

	   		mfs_clear_mark_name_purge(vp);
	   		mfs_clear_rele(vp, MVFS_CD2CRED(cd));
			if ((error = mfs_getcleartext(vp, &cvp, cd)) == 0) {
			    ctxt_retry_done = TRUE;
			    goto ctxt_open;
			} else {
			    mvfs_logperr(MFS_LOG_DEBUG,error,"open: retry can't get cltxt");
		            mfs_clear_error(vp, "cleartext open retry failed", error);
                            /* XXX need to zero out cvp to avoid extra release below */
		        } 
		    } /* end ESTALE retry */
		    else {
                        /* print error if even one other current open */
                        if (error == ESTALE && mnp->mn_vob.open_count > 0)
                            mvfs_clear_log_stale(vp);
		        mfs_clear_error(vp, "cleartext open failed", error);
                    }
		}

		/* Done with cleartext vnode from getcleartext for now */

	    	if (cvp)
		    CVN_RELE(cvp);
	
	        MUNLOCK(mnp);
	    }
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    /* Release an allocated bound root vnode */

    if (vp != *vpp) VN_RELE(vp);

    MDB_VLOG((MFS_VOPEN,"vp=%"KS_FMT_PTR_T" mode=%x, err=%d\n",vp,mode,error));
    BUMPSTAT(mfs_vnopcnt[MFS_VOPEN]);

    MVFS_EXIT_FS(mth);
    BUMPSTAT(mfs_clearstat.unclearopen);
    MVFS_BUMPTIME((alloc_unitp->stime), (alloc_unitp->dtime),
                 mfs_clearstat.unclearget_time);
    KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
    return (error);
}

/* MFS_CLOSEV - called on last close of the object */

/*ARGSUSED*/

/* If something needs to change here make sure to check if a corresponding
** change needs to be made in mvfs_closev_ctx below.
*/
int
mfs_pre_closev(
    VNODE_T *vp,
    int flag,
    VNODE_T **bvpp,
    CALL_DATA_T *cd
)
{
    int error = 0;

    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	case MFS_LOOPCLAS:
	case MFS_VIEWCLAS:
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    break;

	case MFS_VOBRTCLAS:
	    vp = mfs_bindroot(vp, cd, &error);
	    if (error) {
		if (error == ESRCH) error = 0;	/* OK if no view */
	    }
	    break;
	case MFS_VOBCLAS:
	    break;
	default:
	    error = ENXIO;
	    break;
	}

    *bvpp = vp;
    return(error);
}

int
mfs_closev(
    VNODE_T *avp,
    int flag,
    MVFS_LASTCLOSE_T count,
    MOFFSET_T o,                   /* SVR4 only -- currently unused */
    CALL_DATA_T *cd
)
{
	return(mvfs_closev_ctx(avp, flag, count, o, cd, NULL));
}

/* If something in the handling of a class changes here, make sure to check if
** a similar change is needed in the mfs_pre_closev or mfs_post_closev routines
** above.
*/
int
mvfs_closev_ctx(
    VNODE_T *avp,
    int flag,
    MVFS_LASTCLOSE_T count,
    MOFFSET_T o,                   /* SVR4 only -- currently unused */
    CALL_DATA_T *cd,
    MVFS_CLOSE_CTX_T *ctxp
)
{
    VNODE_T *vp = avp;
    mfs_mnode_t *mnp;
    int error;
    int ct_stat = 0;
    CLR_VNODE_T *cvp;
    int need_flush = 0;
    MVFS_DECLARE_THREAD(mth)

    MVFS_ENTER_FS(mth);
    error = mfs_pre_closev(avp, flag, &vp, cd);
    if (error) goto errout;

    mnp = VTOM(vp);
    ASSERT(mnp->mn_hdr.vp);

    switch (mnp->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    break;
	case MFS_LOOPCLAS:
	    /* 
	     * Flush out pages on our vnode ptr to the cleartext
	     * cache.  When the cleartext ptr is inactivated
	     * the pages will go to net/disk.
	     * THIS ONLY WORKS BECAUSE THERE IS NO CACHING OF
	     * LOOPBACK CLEARTEXT VNODES.
	     */
	    if (MVFS_ISVTYPE(vp, VREG)) {
	    	(void) PVN_FLUSH(vp, MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
	    }
	    /* Fall through */
	case MFS_VIEWCLAS:
	    error = MVOP_CLOSE(MFS_CLRVP(vp), flag, count, o,
                               MVFS_CD2CRED(cd), ctxp);
	    if (error) goto errout;
	    break;
	case MFS_NTVWCLAS:
	    error = 0;		/* Allow any close */
	    goto errout;
	    break;
	case MFS_VIEWDIRCLAS:
	    error = mfs_viewdirclose(vp, flag, 0, MVFS_CD2CRED(cd));
	    if (error) goto errout;
	    break;
	case MFS_VOBRTCLAS:
	    /* Should not happen since mfs_pre_closev() takes care of it. */

	    /* Fall through */
	case MFS_VOBCLAS: {

	    /* 
             * No need to rebind the dir
	     * Dirs do not have cleartext ... so nothing to do here.
             */

	    if (MVFS_ISVTYPE(vp, VDIR)) break;
	
	    /*
	     * Sync any of our IO pages to cleartext.
	     * (M)VOP_CLOSE is responsible for any "underlying" syncs
	     * required for that FS.  Also flush any pages from the
	     * page cache, since the file has been modified.
	     */	
	    if (MVFS_ISVTYPE(vp, VREG) && mnp->mn_hdr.cached_pages) {
		need_flush = 1;
		(void) PVN_FLUSH(vp, MFS_PVN_FLUSH|MFS_PVN_INVAL,
                                 MVFS_CD2CRED(cd));
	    }
            /* Do all the close manipulations (and counting) while holding our
            ** lock.  This code is similar to that in mfs_post_closev(), which
            ** used to be called below, but that meant we didn't lock around
            ** all the manipulations.
            */
    	    MLOCK(mnp);

#ifdef NFSV4_SHADOW_VNODE
            /* NFSv4 Compliance: RATLC01011478: 'realvp_master' would be set
             * if in mvfs_openv_ctx(), MVOP_OPEN on the vnode (shadow vnode)
             * replaced it with the master vnode. If so, MVOP_CLOSE must 
             * be called on the master. Refer mvfs_openv_ctx() and the CR
             * for details.
             */
            if (mnp->mn_hdr.realvp_master) cvp = mnp->mn_hdr.realvp_master;
            else cvp = MFS_CLRVP(vp);
#else
    	    cvp = MFS_CLRVP(vp);
#endif


    	    /* Close cleartext file. There should be one,
	     * So log an error if not!  Save status of ctxt op, need it
	     * to make decisions during post-processing.
	     */
	    if (!cvp) {
		mvfs_log(MFS_LOG_ERR, "close: no cleartext vnode\n");
	    } else {
		if ((ct_stat = MVOP_CLOSE(cvp, flag, count, o,
                               MVFS_CD2CRED(cd), ctxp)) != 0) { 
		    mfs_clear_error(vp, "cleartext close failed", ct_stat); 
		}
	    }
	    if (ct_stat == 0) {
	        error = mvfs_sync_attr(mnp, NULL, MFS_USE_PROCBH, 
                                       MVFS_SATTR_ATIME_EROFS_OK, cd);
            }
	    if (MVFS_IS_LASTCLOSE(count)) {
	        mnp->mn_vob.open_count--;
                if ((flag & FWRITE) == FWRITE) {
                    ASSERT(!MFS_CLRTEXT_RO(mnp));
                    mnp->mn_vob.open_wcount--;
                }
	    }

#ifdef NFSV4_SHADOW_VNODE
            /* NFSv4 Compliance: RATLC01011478: If the open count is zero
             * we no longer need the master vnode, if any, saved in the
             * mnode header (refer mvfs_openv_ctx()). 
             * Refer the CR for details.
             */
            if ((mnp->mn_vob.open_count == 0) &&
                    (mnp->mn_hdr.realvp_master != NULL)) {
                CVN_RELE(mnp->mn_hdr.realvp_master);
		mnp->mn_hdr.realvp_master = NULL;
            }
#endif

	    MUNLOCK(mnp);
	    break;
	}
	default:
	    error = ENXIO;
	    goto errout;
	    break;
    }

errout:

    /* Release an allocated bound root vnode */

    if (vp != avp) VN_RELE(vp);

    MDB_VLOG((MFS_VCLOSE,"vp=%"KS_FMT_PTR_T" flag=%x, cnt=%x, pid=%d err=%d\n",vp,flag,count,MDKI_CURPID(),error));
    BUMPSTAT(mfs_vnopcnt[MFS_VCLOSE]);
    MVFS_EXIT_FS(mth);
    return(error);
}

int
mfs_read(
    VNODE_T *vp,
    struct uio *uiop,
    int ioflag,
    CALL_DATA_T *cd
)
{
    return(mvfs_rdwr_ctx(vp, uiop, UIO_READ, ioflag, NULL, cd, NULL));
}

int
mfs_write(
    VNODE_T *vp,
    struct uio *uiop,
    int ioflag,
    CALL_DATA_T *cd
)
{
    return(mvfs_rdwr_ctx(vp, uiop, UIO_WRITE, ioflag, NULL, cd, NULL));
}

/*
 * MVFS_PRE_RDWR - perform actions required before a read/write.
 * Caller must call mvfs_post_rdwr() when done.  (They are an inseparable pair).
 * This allows easy implementation of alternative actual I/O algorithms.
 *
 * This routine has two important side effects:
 *	- It acquires a refcount on the cleartext vnode
 *	- It acquire the RWLOCK on the cleartext vnode
 *
 * mvfs_post_rdwr() must be called to undo these (as well as finish rdwr processing)
 */
int
mvfs_pre_rdwr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    CALL_DATA_T *cd,
    CLR_VNODE_T **cvpp,
    ssize_t *ucp,
    MVFS_RDWR_CTX_T *ctxp
)
{
    int error;
    mfs_mnode_t *mnp;
    MOFFSET_T max_offset;

    ASSERT(VTOM(vp)->mn_hdr.vp);
    ASSERT(cvpp);
    ASSERT(ucp);

    /* Disallow IO on dir objects.  */

    if (MVFS_ISVTYPE(vp, VDIR)) {
	return(EISDIR);
    }

    if (rw == UIO_WRITE && (vp->v_vfsp->vfs_flag & VFS_RDONLY) != 0) {
        return(EROFS);
    }

    /* Check for negative offset.
       Note: this may be a problem with some objects,
       but I don't understand why sometimes the error is
       caught in lseek, and for MFS/NFS it always has
       to be trapped here.  If I don't trap it and the cleartext
       is local, no error is reported, but no bytes are
       read either.... */

    if (MVFS_IS_INVALID_OFFSET(MVFS_UIO_OFFSET(uiop))) {
	return(EINVAL);
    }

    /* Save previous 'count' for logging/debugging */

    *ucp = uiop->uio_resid;	/* For logging/debug */

    /* Switch on class of object.
       Note that View, ntvw, viewdir, vobrt are all dir
       objects and so no check is needed below */

    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    error = 0;		/* Nothing to read/write */
	    break;
	case MFS_LOOPCLAS:	/* Return real vnode for IO */
	    error = 0;
            *cvpp = MFS_CLRVP(vp);
            CVN_HOLD(*cvpp);
	    if (rw == UIO_WRITE) {
	      MVOP_RWWRLOCK(*cvpp, ctxp);
	    }
	    else {
	      MVOP_RWRDLOCK(*cvpp, ctxp);
	    }
	    break;
	case MFS_VOBCLAS: {

            MVFS_RDWR_GET_MAXOFF(vp, uiop, max_offset, ctxp);

            /*
             * For writes, if the uio_offset is greater than or equal
             * limit, we return EFBIG. If we ask to write data that
             * extends beyond the max offset, we write only to
             * the max_offset.  The next write will cause an error.
             *
             * For reads, if the offset is greater than the 
             * limit, we return EOVERFLOW. Like writes, we
             * will only read until the limit and an error 
             * is returned on the next read.
             */
            if (MVFS_IS_INVALID_OFFSET(MVFS_UIO_OFFSET(uiop))) {
                error = EINVAL;
            } else if (MVFS_UIO_OFFSET(uiop) >= max_offset) {

	        if (rw == UIO_WRITE) {		
		    error = EFBIG;
                } else {
		    error = EOVERFLOW;
                }
		break;
	    }

    	    mnp = VTOM(vp);

    	    /* NFS and executables access the file without an
       	       open so get the cleartext here if required */

   	    MLOCK(mnp);

	    /* 
	     * Getcleartext will validate/refetch the cleartext
	     * if it is stale.
	     */
	    error = mfs_getcleartext(vp, cvpp, cd);
	    if (error) {
		MUNLOCK(mnp);
		break;
	    }
	    ASSERT(*cvpp != NULL);

	    /*
	     * Choid the object if required on writes.
	     */

	    if (rw == UIO_WRITE) {		
		/* 
		 * Before choid, make sure cleartext is writable.
		 * Any copy-on-write should have been done in open!
		 */
		if (!mfs_clear_writable(vp)) {
		    MUNLOCK(mnp);
		    error = EROFS;
		    break;
		}

		/* 
		 * Choid the cleartext now, we are writing it.
	 	 * The mfs_change_oid subroutine will decide if a 
		 * choid is really needed or not.
                 */

	        error = mfs_change_oid(vp, 0, MFS_SLEEP, cd);
	        if (error) {
		    MUNLOCK(mnp);
		    break;
	        }

		/* 
		 * Since we checked for RO cleartext above, the
		 * cleartext vnode shouldn't have changed in
		 * the choid call.  Verify this!
		 */

		if (*cvpp != mnp->mn_hdr.realvp) {
		    mvfs_log(MFS_LOG_ERR, "rdwr: unexpected copy-on-write\n");
		    error = ENXIO;
		    MUNLOCK(mnp);
		    break;
		}
	    }

	    /* Unlock around the IO */

    	    MUNLOCK(mnp);

	    /* Acquire I/O lock for cleartext file */
	    if (rw == UIO_WRITE) {
	      MVOP_RWWRLOCK(*cvpp, ctxp);
	    }
	    else {
	      MVOP_RWRDLOCK(*cvpp, ctxp);
	    }
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }
    return (error);
}

/*
 * To make life simpler for the caller, this function returns the
 * error code that was passed in if it's nonzero.  Otherwise, it will
 * return any new error code that occurs in cleanup, or zero if there
 * was no error coming in and it encountered no errors.
 */
int
mvfs_post_rdwr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    CALL_DATA_T *cd,
    CLR_VNODE_T *cvp,
    ssize_t uc,
    int error,
    MVFS_RDWR_CTX_T *ctxp 
)
{
    mfs_mnode_t *mnp;

    mnp = VTOM(vp);
    ASSERT(mnp->mn_hdr.vp);

    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    break;
	case MFS_LOOPCLAS:	/* Release RW lock and refcount on vnode */
	    if (rw == UIO_WRITE) {
	      MVOP_RWWRUNLOCK(cvp, ctxp);
	    }
	    else {
	      MVOP_RWRDUNLOCK(cvp, ctxp);
	    }
	    CVN_RELE(cvp);
	    break;
	case MFS_VOBCLAS: {
	    if (rw == UIO_WRITE) {
	      MVOP_RWWRUNLOCK(cvp, ctxp);
	    }
	    else {
	      MVOP_RWRDUNLOCK(cvp, ctxp);
	    }
	    CVN_RELE(cvp);

    	    /* 
	     * On successful IO, save credentials, set dirty bit,
       	     * and update V.3 inode if required. 
	     * 
	     * Log any "incomplete" writes as a "cleartext" error.
	     * This is because POSIX says that when the disk gets
	     * full, but some data can be written, a success is
	     * returned with only some bytes written.  Usually this
	     * results in a bogus file, and we want to warn the
	     * user so he won't blame the MFS.
	     *
	     * Allow EINTR to support partial transfers on EINTR.
  	     */

	    MLOCK(mnp);
    	    if (!error || (error == EINTR)) {
                int err;
	    	if (rw == UIO_WRITE) {
		    BUMPSTAT(mfs_clearstat.clearwrite);
		    if (uiop->uio_resid > 0) {


                        mvfs_log(MFS_LOG_DEBUG, 
                                 "incomplete cleartext write for vp: %"KS_FMT_PTR_T" error:%d \n", 
                                  vp,error);
		    }
	    	    MSETCRED(mnp, 1, MVFS_CD2CRED(cd));
	    	    mnp->mn_hdr.clear_dirty = 1;

		    /* 
		     * Update wrapper idea of size and modification state
   		     * if required by the port 
   		     */

		    err = MVFS_WRAP_SYNC_SIZE(vp, MVFS_UIO_OFFSET(uiop), FALSE);
                    if (err == 0)
                        err = MVFS_WRAP_SET_MODIFIED(vp);
		    if (err == 0)
                        err = MVFS_WRAP_SET_INODE_CHANGED(vp);
	   	} else {
	            BUMPSTAT(mfs_clearstat.clearread);
	    	    MSETCRED(mnp, 0, MVFS_CD2CRED(cd));
		    err = MVFS_WRAP_SET_ACCESSED(vp);
		}
                if (error == 0)
                    error = err;    /* pass error back if it's new */
	    } else {
		mfs_clear_error(vp, rw == UIO_WRITE ? "cleartext write failed" : "cleartext read failed", error);
                /* check use count > 1:  if we're the only reference, don't
                   complain that someone else has it open */
                if (error == ESTALE && mnp->mn_vob.open_count > 1)
                    mvfs_clear_log_stale(vp);
	    }
	    MUNLOCK(mnp);
	    break;
	}
	default:
	    MDKI_PANIC("mvfs_post_rdwr: invalid vnode class");
	    break;	/* Never reached */
    }

    /* Do audit if no error */

    if (!error) {
	MFS_AUDIT((rw == UIO_READ) ? MFS_AR_READ : MFS_AR_WRITE, NULL,NULL,NULL,NULL,vp,cd);
    }

    MDB_VLOG((MFS_VRDWR,"vp=%"KS_FMT_PTR_T
              " off= %"MVFS_FMT_UIO_OFFSET_X
              ", rw=%x, cnt=%"MVFS_FMT_SSIZE_T_X
              "/%"MVFS_FMT_UIO_RESID_X
              ", err=%d\n",
              vp, MVFS_UIO_OFFSET(uiop), rw, uc, uiop->uio_resid, error));
#ifdef MVFS_DEBUG
    if (uiop->uio_resid > uc) {
	MVFS_PRINTF("MFS resid failure, write errno=%d\n", error);
	MVFS_PRINTF("orig resid=0x%"MVFS_FMT_SSIZE_T_X"   new resid = 0x%"MVFS_FMT_UIO_RESID_X"   offset=0x%"MVFS_FMT_UIO_OFFSET_X"\n", 
		    uc, uiop->uio_resid, MVFS_UIO_OFFSET(uiop));
    }
#endif
    ASSERT(uiop->uio_resid <= uc);
    return(error);
}

int
mfs_rdwr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    CALL_DATA_T *cd
)
{
    return(mvfs_rdwr_ctx(vp, uiop, rw, ioflag, NULL, cd, NULL));
}

int
mfs_rdwr_subr(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd
)
{
    return mvfs_rdwr_ctx(vp, uiop, rw, ioflag, vap, cd, NULL);
}

EXTERN int
mvfs_rdwr_ctx(
    VNODE_T *vp,
    struct uio *uiop,
    UIO_RW_T rw,
    int ioflag,
    VATTR_T *vap,
    CALL_DATA_T *cd,
    MVFS_RDWR_CTX_T *ctxp
)
{
    int error;
    ssize_t uc;
    CLR_VNODE_T *cvp;
    timestruc_t stime, dtime;
    timestruc_t stime1;
    MVFS_DECLARE_THREAD(mth)

    MDKI_HRTIME(&stime1);	/* Fetch start time for stats */

    /* Start by entering the FS (snap thread state) */

    MVFS_ENTER_FS(mth);

    /* Do pre-I/O work */

    error = mvfs_pre_rdwr(vp, uiop, rw, cd, &cvp, &uc, ctxp);
    if (error) goto done;

    /* 
     * Switch on class of object.
     * Note that View, ntvw, viewdir, vobrt are all dir
     * objects and so no check is needed below. (Dir objects
     * are prohibited by mvfs_pre_rdwr())
     */

    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    error = 0;		/* Nothing to read/write */
	    break;
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    if (rw == UIO_READ)
		error = MVOP_READ(MVFS_CVP_TO_VP(cvp), uiop, ioflag, vap, cd, ctxp);
	    else 
		error = MVOP_WRITE(MVFS_CVP_TO_VP(cvp), uiop, ioflag, vap, cd, ctxp);
	    break;
	case MFS_VOBCLAS: {
            MOFFSET_T max_offset;
            MOFFSET_T max_resid;
            MOFFSET_T adj_resid;

            /*
             * If the read/write spans the max offset 
             * only r/w to the maximum offset.
             */

            adj_resid = 0;
            MVFS_RDWR_GET_MAXOFF(vp, uiop, max_offset, ctxp);

            max_resid = (max_offset - MVFS_UIO_OFFSET(uiop));

            if (max_resid < uiop->uio_resid) {
                adj_resid = uiop->uio_resid - max_resid;
                uiop->uio_resid = (MVFS_UIO_RESID_T) max_resid;
            }

	    MDKI_HRTIME(&stime);	/* Fetch start time for stats */
	    if (rw == UIO_READ)
		error = MVOP_READ(MVFS_CVP_TO_VP(cvp), uiop, ioflag, vap, cd, ctxp);
	    else
	        error = MVOP_WRITE(MVFS_CVP_TO_VP(cvp), uiop, ioflag, vap, cd, ctxp);
 	    if (!error || error == EINTR) {
	    	if (rw == UIO_WRITE) {
		    MVFS_BUMPTIME(stime, dtime, mfs_clearstat.clearwr_time);
	   	} else {
		    MVFS_BUMPTIME(stime, dtime, mfs_clearstat.clearrd_time);
		}
	    }

            /*
             * Adjust for value not read if crosses max boundary.
             */

	    uiop->uio_resid += (MVFS_UIO_RESID_T) adj_resid;
	    break;
	}
	default:
	    MDKI_PANIC("mfs_rdwr on invalid vnode type");
	    break;	/* Shouldn't reach here */
    }

    /* Do post-I/O processing */

    /*
     * This function returns a non-zero error code, or a new code if
     * error is zero.
     */
    error = mvfs_post_rdwr(vp, uiop, rw, cd, cvp, uc, error, ctxp);

    /* Keep stats and exit the FS */

done:
    BUMPSTAT(mfs_vnopcnt[MFS_VRDWR]);
    MVFS_EXIT_FS(mth);
    if (VTOM(vp)->mn_hdr.mclass == MFS_VOBCLAS && (!error || error == EINTR)) {
	if (rw == UIO_READ) {
	    MVFS_BUMPTIME(stime1, dtime, mfs_clearstat.unclearrd_time);
	} else {
	    MVFS_BUMPTIME(stime1, dtime, mfs_clearstat.unclearwr_time);
	}
    }
    return (error);
}

struct {
    int used;		/* if cmd is used */
    long mininfolen;	/* min infolen reqd */
    long maxinfolen;	/* max infolen reqd */
} mvfs_ioctl_valid_table[] = {
	/* MVFS_CMD_GET_VIEWINFO 1 */
	{TRUE, sizeof(mvfs_viewinfo_t), sizeof(mvfs_viewinfo_t)},
	/* MVFS_CMD_XSTAT 2 */
	{TRUE, sizeof(mvfs_xstat_t), sizeof(mvfs_xstat_t)},
	/* MVFS_CMD_GET_CLRNAME 3 */
	{TRUE, sizeof(mvfs_clrname_info_t), sizeof(mvfs_clrname_info_t)},
	/* MVFS_CMD_GET_VFH 4 */
	{TRUE, sizeof(mvfs_iovfh_t), sizeof(mvfs_iovfh_t)},
	/* MVFS_CMD_IOINVAL 5 */
	{TRUE, sizeof(mvfs_ioinval_t), sizeof(mvfs_ioinval_t)},
	/* MVFS_CMD_REVALIDATE 6 */
	{TRUE, 0, 0},
	/* MVFS_CMD_MKVIEWTAG 7 */
	{TRUE, sizeof(mvfs_mkviewtag_info_t), sizeof(mvfs_mkviewtag_info_t)},
	/* MVFS_CMD_RMVIEWTAG 8 */
	{TRUE, sizeof(mvfs_viewtag_info_t), sizeof(mvfs_viewtag_info_t)},
	/* MVFS_CMD_GET_VIEWADDR 9 */
	{TRUE, sizeof(mvfs_viewaddr_t), sizeof(mvfs_viewaddr_t)},
	/* MVFS_CMD_GET_VOBINFO 10 */
	{TRUE, sizeof(mvfs_vobinfo_t), sizeof(mvfs_vobinfo_t)},
	/* MVFS_CMD_GET_VIEWTAG_DIR 11 */
	{TRUE, sizeof(mfs_strbufpn_pair_t), sizeof(mfs_strbufpn_pair_t)},
	/* MVFS_CMD_CHANGE_MTYPE 12 */
	{TRUE, sizeof(mvfs_iochange_mtype_t), sizeof(mvfs_iochange_mtype_t)},
	/* MVFS_CMD_GET_AFILE 13 */
	{TRUE, sizeof(mfs_strbufpn_t), sizeof(mfs_strbufpn_t)},
	/* MVFS_CMD_SET_AFILE 14 */
	{TRUE, sizeof(mfs_strbufpn_pair_t), sizeof(mfs_strbufpn_pair_t)},
	/* MVFS_CMD_GET_PROCF 15 */
	{TRUE, sizeof(u_long), sizeof(u_long)},
	/* MVFS_CMD_SET_PROCF 16 */
	{TRUE, sizeof(u_long), sizeof(u_long)},
	/* MVFS_CMD_START_AUDIT 17 */
	{TRUE, sizeof(u_long), sizeof(u_long)},
	/* MVFS_CMD_STOP_AUDIT 18 */
	{TRUE, 0, 0},
	/* MVFS_CMD_SYNC_AUDIT 19 */
	{TRUE, 0, 0},
	/* MVFS_CMD_GET_VXSUFFIX 20 */
	{TRUE, sizeof(mfs_strbuf_t), sizeof(mfs_strbuf_t)},
	/* MVFS_CMD_GET_CACHE_ENB 21 */
	{TRUE, sizeof(u_long), sizeof(u_long)},
	/* MVFS_CMD_SET_CACHE_ENB 22 */
	{TRUE, sizeof(u_long), sizeof(u_long)},
	/* MVFS_CMD_FLUSH_CACHE 23 */
	{TRUE, sizeof(u_long), sizeof(u_long)},
	/* MVFS_CMD_READ_DNC 24 */
	{TRUE, sizeof(mfs_ioncent_t), sizeof(mfs_ioncent_t)},
	/* MVFS_CMD_GET_RCSID 25 */
	{TRUE, -1L, -1L},
	/* MVFS_CMD_GET_SCCSID 26 */
	{TRUE, -1L, -1L},
	/* MVFS_CMD_GET_LOGINFO 27 */
	{TRUE, sizeof(mvfs_loginfo_t), sizeof(mvfs_loginfo_t)},
	/* MVFS_CMD_SET_LOGINFO 28 */
	{TRUE, sizeof(mvfs_loginfo_t), sizeof(mvfs_loginfo_t)},
	/* MVFS_CMD_GET_BH 29 */
	{TRUE, sizeof(mvfs_bhinfo_t), sizeof(mvfs_bhinfo_t)},
	/* MVFS_CMD_SET_BH 30 */
	{TRUE, sizeof(mvfs_bhinfo_t), sizeof(mvfs_bhinfo_t)},
	/* MVFS_CMD_GET_STATS 31 */
	{TRUE, sizeof(mvfs_statbufs_t), sizeof(mvfs_statbufs_t)},
	/* MVFS_CMD_SETPROCVIEW 32 */
	{TRUE, sizeof(mvfs_viewtag_info_t), sizeof(mvfs_viewtag_info_t)},
	/* MVFS_CMD_GET_PROCVIEWINFO 33 */
	{TRUE, sizeof(mvfs_viewinfo_t), sizeof(mvfs_viewinfo_t)},
	/* MVFS_CMD_GET_XATTR 34 */
	{TRUE, sizeof(mvfs_io_xattr_t), sizeof(mvfs_io_xattr_t)},
	/* MVFS_CMD_SET_XATTR 35 */
	{TRUE, sizeof(mvfs_io_xattr_t), sizeof(mvfs_io_xattr_t)},
	/* MVFS_CMD_MOUNT 36 */
	{TRUE, sizeof(struct mfs_mntargs), sizeof(struct mfs_mntargs)},
	/* MVFS_CMD_UNMOUNT 37 */
	{TRUE, sizeof(mvfs_unmount_info_t), sizeof(mvfs_unmount_info_t)},
	/* MVFS_CMD_RMALLVIEWTAGS 38 */
	{TRUE, 0, 0},
	/* MVFS_CMD_UMOUNTALL 39 */
	{TRUE, 0, 0},
	/* MVFS_CMD_GET_POOLMAPS 40 */
	{TRUE, sizeof(mvfs_ioget_poolmaps_t), sizeof(mvfs_ioget_poolmaps_t)},
	/* MVFS_CMD_VDM 41 */
	{FALSE,0,0},
	/* MVFS_CMD_ABORT 42 */
	{TRUE, 0, 0},
	/* MVFS_CMD_EXPORTVIEWTAG 43 */
	{TRUE, sizeof(mvfs_export_viewinfo_t), sizeof(mvfs_export_viewinfo_t)},
	/* MVFS_CMD_UNEXPORTVIEWTAG 44 */
	{TRUE, sizeof(mvfs_viewtag_info_t), sizeof(mvfs_viewtag_info_t)},
	/* MVFS_CMD_ZERO_STATS 45 */
	{TRUE, 0, 0},
	/* MVFS_CMD_GET_CACHE_USAGE 46 */
	{TRUE, sizeof(mvfs_cache_usage_t), sizeof(mvfs_cache_usage_t)},
	/* MVFS_CMD_SET_CACHE_SIZES 47 */
	{TRUE, sizeof(mvfs_cache_sizes_t), sizeof(mvfs_cache_sizes_t)},
	/* MVFS_CMD_AUDIT_MARKER 48 */
	{TRUE, sizeof(u_long), sizeof(u_long)},
	/* MVFS_CMD_IOD_NULL 49 */
	{FALSE, 0, 0},
	/* MVFS_CMD_GET_VIEW_STATS 50 */
	{TRUE, sizeof(mvfs_viewstats_t), sizeof(mvfs_viewstats_t)},
	/* MVFS_CMD_ZERO_VIEW_STATS 51 */
	{TRUE, sizeof(mvfs_zero_viewstat_t), sizeof(mvfs_zero_viewstat_t)},
	/* MVFS_CMD_SIDHOST_CREDMAPPING 52 */
	{TRUE, sizeof(mvfs_sidhost_cred_t), sizeof(mvfs_sidhost_cred_t)},
	/* MVFS_CMD_DELETE_SIDHOST_CREDMAPPING 53 */
	{TRUE, sizeof(mvfs_sid_t), sizeof(mvfs_sid_t)},
	/* MVFS_CMD_GET_VIEWTAG_EXPORT 54 */
	{TRUE, sizeof(mvfs_export_viewinfo_t), sizeof(mvfs_export_viewinfo_t)},
	/* MVFS_CMD_GET_GFSINFO 55 */
	{TRUE, sizeof(mvfs_gfsinfo_t), sizeof(mvfs_gfsinfo_t)},
	/* MVFS_CMD_SET_VOBRT_VFSMNT 56 */
	{TRUE, sizeof(mfs_strbufpn_pair_t), sizeof(mfs_strbufpn_pair_t)},
	/* MVFS_CMD_GET_CACHE_SIZES 57 */
	{TRUE, sizeof(mvfs_cache_sizes_t), sizeof(mvfs_cache_sizes_t)},
        /* MVFS_CMD_REG_GRPLIST_ORDER 58 */
        {FALSE, 0, 0},
        /* MVFS_CMD_UNREG_GRPLIST_ORDER 59 */
        {FALSE, 0, 0},
	/* MVFS_CMD_COMPUTE_CACHE_DEFAULTS 60 */
	{TRUE, sizeof(mvfs_cache_sizes_t), sizeof(mvfs_cache_sizes_t)},
        /* MVFS_CMD_GRPLIST_READ 61 */
        {FALSE, 0, 0},
        /* MVFS_CMD_MKVIEWTAG_EX 62 */
        {FALSE, 0, 0},

/* If you add items here, add them as well to the 32/64 bit conversion
   table in mvfs_transtype.c */

};

int
mvfs_ioctl_validate(mcbp)
mvfscmd_block_t *mcbp;
{
    /* valid MVFS_CMD ? */
    if (mcbp->hdr.cmd < MVFS_CMD_MIN)
	return (EINVAL);

    if (mcbp->hdr.cmd > MVFS_CMD_MAX)
	return (EINVAL);

    /* Used ? */
    if (mvfs_ioctl_valid_table[mcbp->hdr.cmd - 1].used != TRUE)
	return(EINVAL);

    /* Do we need to check length ? */
    if (mvfs_ioctl_valid_table[mcbp->hdr.cmd - 1].mininfolen == -1L)
	return (0);

    if (mcbp->infolen < mvfs_ioctl_valid_table[mcbp->hdr.cmd - 1].mininfolen)
	return (EINVAL);

    if (mcbp->infolen > mvfs_ioctl_valid_table[mcbp->hdr.cmd - 1].maxinfolen)
	return (EINVAL);

    return (0);
}

/*ARGSUSED*/
int
mvfs_ioctlv_subr(
    VNODE_T *avp,
    int com,
    caddr_t data,
    int flag,
    CALL_DATA_T *cd,
    MVFS_IOCTL_RVALP_T rvalp,			/* NYI; currently unused */
    VOPBD_T *vopbdp,
    MVFS_CALLER_INFO *callinfo
)
{
    VNODE_T *vp = avp;
    VNODE_T *xvp;
    mvfscmd_block_t *iocbuf;
    caddr_t infop;
    int error;
    CLR_VNODE_T *cvp;
    MVFS_DECLARE_THREAD(mth)
    int ourcmd = 0;
    int ourerr = 0;

    MVFS_ENTER_FS(mth);

    /*
     * Note: only allow the ioctl on the character special device
     * we have created.  While some OS's allow ioctl on any object,
     * others do not, so the char device is the only portable
     * choice.
     */

    ASSERT(VTOM(vp)->mn_hdr.vp);
    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	case MFS_VIEWCLAS:
	    error = MVOP_IOCTL(MFS_REALVP(vp), com, data, flag, cd, 
                               rvalp, vopbdp, callinfo);
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EINVAL;
	    break;
	case MFS_SDEVCLAS:	/* Special ops here */
	    if (MVFS_IS_IT_OUR_IOCTL(com, callinfo) == 0) {
		error = ENOTTY;
		break;
	    }

	    /* Validate passed in length as not too long. */

            if (((com >> 16) & _IOCPARM_MASK) > MVFS_IOCTL_MAXLEN) {
		MVFS_EXIT_FS(mth);
		return (EINVAL);
	    }

	    /* 
             * Allocate block for copyin of data and call copyin routine with it 
	     * For future compatibility (where we may ADD fields and check the length
	     * to tell old apps from new apps with new ioctl cmd), I allocate the
	     * max length buffer we support, not the "current" size of an
  	     * mvfscmd block.
             */

	    iocbuf = (mvfscmd_block_t *)KMEM_ALLOC(sizeof(mvfscmd_block_t), KM_SLEEP);
            if (iocbuf == NULL) {
                error = ENOMEM;
                break;
            }
	    error = mvfs_ioctl_copyin((mvfscmd_block_t *)data, iocbuf,
	                               &infop, callinfo);
	    if (error) {
		KMEM_FREE(iocbuf, sizeof(mvfscmd_block_t));
	        break; /* nothing we can do */
	    }

	    error = MVFS_VALIDATE_IOCTL(iocbuf, callinfo);
	    if (error) {
		KMEM_FREE(iocbuf, sizeof(mvfscmd_block_t));
	        break; /* nothing we can do */
	    }

            /* Lookup ignores commands w/out pnames
             * On error, mvfs_ioctl_lookup returns -1 and sets the 
             * status field in the iocbuf.  The call to copyout below
             * will copy out the status code to the user and reset 
             * error to 0, assuming no errors in copyout.
             */
            error = mvfs_ioctl_lookup(iocbuf, &xvp, &cvp, cd, callinfo);

	    if (!error) {
		error = mvfs_mioctl(xvp, cvp, iocbuf, flag, cd, callinfo);
                ourcmd = MCB_CMD(iocbuf);
                ourerr = iocbuf->status;
                if (cvp)
                    CVN_RELE(cvp);
                if (xvp)
                    VN_RELE(xvp);
	    }

	    error = mvfs_ioctl_copyout((mvfscmd_block_t *)data, iocbuf, 
	                                infop, callinfo);
	    if (iocbuf) {
		KMEM_FREE(iocbuf, sizeof(mvfscmd_block_t));
	    }
	    break;
	case MFS_VOBRTCLAS:
	    vp = mfs_bindroot(vp, cd, &error);
	    if (error == ESRCH) error = EINVAL;  /* Null view - no ioctl */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {

	    /* Pass through the ioctl to the cleartext */

	    error = 0;
	    if (MVFS_ISVTYPE(vp, VREG)) {
	        MLOCK(VTOM(vp));
		error =  mfs_getcleartext(vp, &cvp, cd);
		MUNLOCK(VTOM(vp));
		if (!error) {
                    error = MVOP_IOCTL(MVFS_CVP_TO_VP(cvp), com, data, flag,
                                       cd, rvalp, vopbdp, callinfo);
		    CVN_RELE(cvp);
		} else {
		    if (error == EISDIR) error = ENOTTY;
		}
	    } else {
	        /* Not a file - give unsupported ioctl */
		error = ENOTTY;
	    }
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    if (vp && vp != avp) VN_RELE(vp);   /* Release if allocated a vnode */

    MDB_VLOG((MFS_VIOCTL,"vp=%"KS_FMT_PTR_T" com=%x(%d), data=%"KS_FMT_PTR_T", err=%d(%d)\n",vp,com,ourcmd,data,error,ourerr));
    BUMPSTAT(mfs_vnopcnt[MFS_VIOCTL]);
    MVFS_EXIT_FS(mth);
    return(error);
}

/*ARGSUSED*/
int
mfs_ioctlv(
    VNODE_T *avp,
    int com,
    caddr_t data,
    int flag,
    CALL_DATA_T *cd,
    MVFS_IOCTL_RVALP_T rvalp			/* NYI; currently unused */
)
{
    return(mvfs_ioctlv_subr(avp, com, data, flag, cd, rvalp, 
           (VOPBD_T *) NULL, NULL));
}

/* MFS_CHK_TIMEDOUT - check if the attr cache has timed out. */

int
mfs_ac_timedout(
    mfs_mnode_t *mnp,
    int evmiss_flag,
    CALL_DATA_T *cd
)
{
    time_t curtime;
    VNODE_T *vp = MTOV(mnp);
    struct mfs_mntinfo *mmi;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    ASSERT(vp);

    /* If attribute caching disabled, always return "timed out" */

    if (!mcdp->mvfs_acenabled) return(1);
    mmi = V_TO_MMI(vp);
    if (mmi->mmi_noac) return(1);

    /* Attribute cache is timed out either by:
     * (1) mn_vob.attrtime is earlier than the current time (resolution
     *     of seconds only!), or dir-type mnode attrsettime is earlier
     *     than mmi_ac_dir_ftime (dir attribute cache flush time).
     * (2) Current generation of attributes less than generation
     *     required by 'process state'.  (see comment below on this
     *     in mvfs_set_ac_timeout).
     */
    if (MVFS_MYTHREAD(cd)->thr_attrgen > mnp->mn_vob.attrgen) {
	if (evmiss_flag) {
	    BUMPSTAT(mfs_acstat.ac_evmiss);
	} else {
	    BUMPSTAT(mfs_acstat.ac_genmiss);
	    BUMPVSTATM(mnp,acstat.ac_genmiss);
	}
	return 1;
    }

    if (MVFS_ISVTYPE(vp, VDIR) &&
        mnp->mn_vob.attrsettime.tv_sec <= mmi->mmi_ac_dir_ftime)
    {
	if (evmiss_flag) {
	    BUMPSTAT(mfs_acstat.ac_evmiss);
	} else {
	    BUMPSTAT(mfs_acstat.ac_timo);
	    BUMPVSTATM(mnp, acstat.ac_timo);
	}
        return 1;
    }

    curtime = MDKI_CTIME();
    /* NB: VIEW_ISA_VIEW_OBJ() identifies shared DOs (wink-ins) as view
       objects.  They're an artifact of the view, not a part of the VOB.
       Thus, no worries about COWable files not getting noticed here because
       of LVUT. */
    if (curtime > mnp->mn_vob.attrtime.tv_sec) {
	if (!VIEW_ISA_VIEW_OBJ(&mnp->mn_vob.vfh) &&
	    !MFS_HMVFH(&mnp->mn_vob.vfh) &&
	    mnp->mn_vob.attr.fstat.type != TBS_FTYPE_DIR &&
	    mnp->mn_hdr.viewvp != NULL) {
	    /*
	     * Check whether the last VOB update time indicates that
	     * some other RPC request to the view for this VOB
	     * discovered that nothing significant has changed that
	     * could affect a VOB object (The other call that might
	     * have done that in essence performed a bulk revalidation
	     * on the name cache).
	     *
	     * If the view said this thing was up to date, then reset
	     * the attr time to look like we just refetched this data.
	     *
	     * basic conditions for trusting the attributes and sliding the
	     * timeout:
	     *
	     * the LVUT is not stale [checked inside find_vobstamp()]
	     * -and-
	     * object is a VOB-resident thing
	     * -and-
	     * attribute lvut == current lvut
	     */
	    struct timeval vs;
	    time_t valid_thru;
	    if (mvfs_viewdir_find_vobstamp(mnp->mn_hdr.viewvp,
					   &VFS_TO_MMI(mnp->mn_hdr.vfsp)->mmi_svr.uuid,
					   &vs, &valid_thru)) {
		MLOCK(mnp);
		if (MFS_TVEQ(mnp->mn_vob.lvut, vs)) {
		    /*
		     * OK, we can slide the vob attribute time now, but
		     * make sure it doesn't extend beyond the LVUT's valid
		     * lifetime.
		     */
		    mvfs_set_ac_timeout(mnp, mnp->mn_hdr.vfsp,
					valid_thru, FALSE, FALSE);
		    MUNLOCK(mnp);
		    BUMPSTAT(mfs_acstat.ac_lvuthit);
		    BUMPVSTATM(mnp,acstat.ac_lvuthit);
		    return 0;
		} else {
		    MUNLOCK(mnp);
		    MDB_XLOG((MDB_LVUT,
			      "lvutmiss: vw=%"KS_FMT_PTR_T", vfsp=%"KS_FMT_PTR_T", lvut=%"KS_FMT_TV_SEC_T_D".%"KS_FMT_TV_USEC_T_D", timeout=%"KS_FMT_TIME_T_D"\n",
			      mnp->mn_hdr.viewvp, mnp->mn_hdr.vfsp,
			      vs.tv_sec, vs.tv_usec,
			      valid_thru));
		    BUMPSTAT(mfs_acstat.ac_lvutmiss);
		    BUMPVSTATM(mnp,acstat.ac_lvutmiss);
		}
	    }
	}
	if (evmiss_flag) {
	    BUMPSTAT(mfs_acstat.ac_evmiss);
	} else {
	    BUMPSTAT(mfs_acstat.ac_timo);
	    BUMPVSTATM(mnp,acstat.ac_timo);
	}
	return 1;
    }
    return 0;
}

/* MFS_SET_AC_TIMEOUT - set timeout for attr cache.
*/

void
mvfs_set_ac_timeout(mnp, vfsp, nlt, bumpgen, goodlvut)
mfs_mnode_t *mnp;
VFS_T *vfsp;
time_t nlt;		/* delta must not extend later than this time */
int bumpgen;
int goodlvut;
{
    time_t delta;
    time_t ctime = MDKI_CTIME();
    struct mfs_mntinfo *mmi = VFS_TO_MMI(vfsp);
    mvfs_mnode_data_t *mndp = MDKI_MNODE_GET_DATAP();

    ASSERT(MISLOCKED(mnp));

    /* To be used from makevobnode, this must only depend on
       mn_vob.attr and mmi_xxx fields. */

    /* Default timeout delta is about 1/10 the time since the last
       modified time.  (Actually 1/8 since that is quicker...) */

    delta = (ctime - mnp->mn_vob.attr.fstat.mtime.tv_sec) >> 3;

    /* Clamp to min/max based on dir or not. */

    if (mnp->mn_vob.attr.fstat.type == TBS_FTYPE_DIR) {
	if (delta < mmi->mmi_ac_dirmin) {
	    delta = mmi->mmi_ac_dirmin;
        } else if (delta > mmi->mmi_ac_dirmax) {
	    delta = mmi->mmi_ac_dirmax;
	}
    } else {
	if (delta < mmi->mmi_ac_regmin) {
	    delta = mmi->mmi_ac_regmin;
        } else if (delta > mmi->mmi_ac_regmax) {
	    delta = mmi->mmi_ac_regmax;
	}
	
    }

    /* 
     * Save time attrs set for info purposes only.
     */

    mnp->mn_vob.attrsettime.tv_sec = ctime;
    mnp->mn_vob.attrsettime.tv_nsec = 0;


    if (nlt != 0)
	mnp->mn_vob.attrtime.tv_sec = KS_MIN(ctime + delta, nlt);
    else
	mnp->mn_vob.attrtime.tv_sec = ctime + delta;
    mnp->mn_vob.attrtime.tv_nsec = 0;

   /*
    * Set the "attributes" generation number.
    * This number is used to allow a process to set a 'marker'
    * in time that forces attributes to be refetched on everything
    * it touches past that point.  This is used in distributed
    * builds to make sure the local node sees the effects of
    * builds done on remote nodes by revalidating its caches.
    *
    * There are 3 components to how this works:
    *    (1) The process does an ioctl() which bumps the system
    *        wide 'attribute generation' number and sets its process
    *        state with the new attribute generation number.
    *    (2) In the 'attribute cache' timeout check, if the
    *        object's 'attribute generation' number is less than
    *	     what the process requires, then the object has not
    *	     had its attributes fetched since the marker was
    *	     set in step (1), and so the attribute cache
    *        is declared 'timed out' and new attributes will be refetched.
    *    (3) Whenever attributes are set (e.g. a new timeout is set 
    *        in this routine), the attribute generation number is set
    *        to the current system wide value, thus indicating that
    *        they have been fetched over the wire since step (1)
    *	     was last performed.
    */

    if (bumpgen)
	mnp->mn_vob.attrgen = MFS_MNATTRGEN(mndp);
    if (goodlvut) {
        /* save the freshly-fetched LVUT, and let it be valid for as long
         * as the maximum normal file attribute timeout.
         *
         * This will extend the lifetime of other items cached with the same
         * LVUT by this timeout, which is OK since they're VOB elements
         * and probably not changing quickly, and as long as we fetch/record
         * a new LVUT once each ac_regmax interval, we will notice
         * any changes once a new LVUT is returned; this is an acceptable delay
         */
	mvfs_viewdir_save_vobstamp(mnp->mn_hdr.viewvp,
				   &mmi->mmi_svr.uuid,
				   ctime + mmi->mmi_ac_regmax,
				   &mnp->mn_vob.lvut);
    }
}

#define MFS_DTMMOD	0x1
#define MFS_VOBMOD	0x2
#define MFS_EXPMOD	0x4
#define MFS_DOMOD	0x8

/*
 * MFS_AC_SET_STAT - set the stats in the mnode
 * Return modified flags.
 */

int
mvfs_ac_set_stat(
    mfs_mnode_t *mnp,
    view_vstat_t *vstat,
    int goodlvut,
    CRED_T *cred
)
{
    int flags = 0;

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    BUMPSTAT(mfs_acstat.ac_updates);
    BUMPVSTATM(mnp,acstat.ac_updates);

    /* 
     * Check if dir or file is modified. 
     * Don't worry about cleartext mod time here because:
     *    1) It is handled in mvfs_clearattr()
     *    2) Stack is a problem so we don't want to call mvfs_clearattr()
     *       from this routine.
     *    3) Only vobmod events (which don't need cleartext mod time)
     *       affect the name cache etc.
     */

    if ((mnp->mn_vob.attr.fstat.mtime.tv_sec != 
				vstat->fstat.mtime.tv_sec) ||
    		 (mnp->mn_vob.attr.fstat.mtime.tv_usec != 
				vstat->fstat.mtime.tv_usec))
	flags |= MFS_DTMMOD;

    /* Compare vob modified times for changes in VOB (not view) */

    if ((vstat->event_time.tv_sec != 
			mnp->mn_vob.attr.event_time.tv_sec) ||
		(vstat->event_time.tv_usec != 
			mnp->mn_vob.attr.event_time.tv_usec))
	flags |= MFS_VOBMOD;

    /* Compare object oids (detect COW which requires a cleartext
       dump/reload) */

    /* mtype check is to avoid marking mnodes which can't have changed
       cleartexts (at least for COW reasons)--COWed DOs will change
       from View DO's to view-private files, and perhaps later back to view
       DOs (so check OIDs if mtype either changes or is a DO type, since the
       DO->viewpvt->DO conversion may have happened since last we checked
       this mnode).  If it is promoted later & scrubbed (e.g. by view_scrubber)
       the view's inode will change and it will use a new mnode.

       OIDNULL check is to avoid marking new mnodes which are
       being created */

    if ((mnp->mn_vob.attr.mtype != vstat->mtype ||
         VOB_MTYPE_IS_DO(vstat->mtype)) &&
        !MFS_OIDNULL(mnp->mn_vob.attr.obj_oid) &&
        !MFS_OIDEQ(vstat->obj_oid, mnp->mn_vob.attr.obj_oid))
    {
	flags |= MFS_DOMOD;
#ifdef MVFS_DEBUG
        mvfs_log(MFS_LOG_DEBUG, "oidchg mtype %d to %d\n",
                 mnp->mn_vob.attr.mtype, vstat->mtype);
#endif
    }

    /* Save view attributes. */

    mnp->mn_vob.attr = *vstat;

    MVFS_CREDUTL_SIDS_TO_NATIVE_IDS(mnp, cred); /* Convert to native */

    /* Set timeout on attribute cache. */

    mvfs_set_ac_timeout(mnp, mnp->mn_hdr.vfsp, 0, TRUE, goodlvut);

    /* Return modified flags */

    return(flags);

}

/*
 * MFS_AC_MODEVENTS - process modified events on an object
 */

void
mfs_ac_modevents(
    register VNODE_T *vp,
    int flags,
    CRED_T *cred
)
{
    register mfs_mnode_t *mnp;
    int modified;
    int vobmod;
    int expmod;		/* Indicates caller expects this modification */
    int domod;

    modified = ((flags & MFS_DTMMOD) != 0);
    vobmod   = ((flags & MFS_VOBMOD) != 0);
    expmod   = ((flags & MFS_EXPMOD) != 0);
    domod    = ((flags & MFS_DOMOD) != 0);

    mnp = VTOM(vp);

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));
    ASSERT(mnp->mn_hdr.vp);

    /* 
     * If the object has been "modified"  (either in the view or vob)
     * then flush any caches, dir names etc.  
     */

    if (modified || vobmod || domod) {
	if (modified) {
	    BUMPSTAT(mfs_acstat.ac_mod);
	    BUMPVSTAT(vp,acstat.ac_mod);
	} else {
	    BUMPSTAT(mfs_acstat.ac_vobmod);
	    BUMPVSTAT(vp,acstat.ac_vobmod);
	}
	
	switch (MVFS_GETVTYPE(vp)) {
	    case VREG:
		if (vobmod) {

		    /* 
		     * On a VOB modified event, set a flag to flush
		     * the cleartext binding on the next inactive.
		     * The cleartext may have moved, but we can't just purge 
		     * it here, because there may be activities
		     * in progress (mapped files, open files) that require
		     * the cleartext vnode ptr to remain active.
		     */
		    mfs_dnc_invalvp(vp);          /* Invalidate name cache */
		    mfs_clear_mark_name_purge(vp);
		}
                if (domod && MFS_CLRTEXT_RO(mnp)) {
                    /* MFS_CLRTEXT_RO() check because we only need to
                       dump it if we or some other client might have
                       COWed it.  view-private vnode bindings are
                       stable until after promotion which yields new
                       mnodes */
                    mfs_clear_mark_name_purge(vp);
                    if (mnp->mn_vob.open_count == 0) {
                        MDB_XLOG((MDB_CLEAROPS,
                                  "domod: vp=%"KS_FMT_PTR_T" VOB DO released\n", vp));
                        mfs_clear_rele(vp, cred);
                    } else {
                        MDB_XLOG((MDB_CLEAROPS,
                                  "domod: vp=%"KS_FMT_PTR_T" VOB DO name purge\n", vp));
                    }
                }
		break;
	    case VDIR:
	        mnp->mn_vob.dir_eof = 0;   /* Don't know end of dir anymore */
		/*
		 * If a VOB event, then dir might be an "out of date"
                 * cdir.  Set bit to indicate this.  This can only
                 * be cleared by a successful verify that this is correct
                 * version for this object in this view.  Also note,
                 * that history mode vnodes can NEVER be out of date.
                 */
		if (vobmod && !VTOM(mnp->mn_hdr.viewvp)->mn_view.hm) {
		    MFS_REBINDINVAL(mnp);
		}
	        /* 
	         * If unexpected 3'rd party modification - purge dir cache.
	         * If the caller passed in the "expected mod" flag, then
                 * the caller is responsible for fixing the name cache 
                 * correctly for what he did.
                 */
	        if (!expmod || vobmod) {
		    mfs_dnc_invalvp(vp);

		    mfs_index_cache_flush(vp);
		} else {
		    BUMPSTAT(mfs_acstat.ac_expmod);
		    BUMPVSTAT(vp,acstat.ac_expmod);
		}
                /* flush rddir cache, too hard to clean it up */
                mvfs_rddir_cache_flush(VTOM(vp));

		break;
	    default:
	        /* If any cached data (symlink) - assume not valid now */
	        if (mnp->mn_vob.slinktext) {
	    	    KMEM_FREE(mnp->mn_vob.slinktext, mnp->mn_vob.slinklen);
	    	    mnp->mn_vob.slinktext = NULL;
	    	    mnp->mn_vob.slinklen = 0;
		}
		break;
	}  /* end of switch(MVFS_GETVTYPE(vp)) */
    }	   /* end of modified or vob modified */
}

/* MFS_ATTRCACHE - routine to manage object attribute cache.
   This routine should be called with the MVI struct locked! */

void
mfs_attrcache(
    VNODE_T *vp,
    view_vstat_t *vstat,
    int expmod,     /* Indicates caller expects this modification */
    CRED_T *cred
)
{
    mfs_mnode_t *mnp;
    int modflags;

    mnp = VTOM(vp);

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));
    ASSERT(mnp->mn_hdr.vp);

    /* First stuff the stats */

    modflags = mvfs_ac_set_stat(mnp, vstat, FALSE, cred);

    /* Sync attributes with wrapper (if any) */

    MVFS_WRAP_UPDATE_ATTRS(vp);

    /* Process and modified events */

    if (expmod) modflags |= MFS_EXPMOD;
    mfs_ac_modevents(vp, modflags, cred);
}

/* MFS_EVTIME_VALID - validate event time */

int
mfs_evtime_valid(
    VNODE_T *vp,
    struct timeval *tvp,
    CALL_DATA_T *cd
)
{
    int error;
    int vobmod;

    /* Fetch latest attributes if attr cached timed out */

    ASSERT(VTOM(vp)->mn_hdr.vp);
    vobmod = (tvp->tv_sec != VTOM(vp)->mn_vob.attr.event_time.tv_sec) ||
	   	(tvp->tv_usec != VTOM(vp)->mn_vob.attr.event_time.tv_usec);

    /*
     * Only do getattr if we don't think the VOB node changed--the caller
     * will do a VIEW_LOOKUP anyway which subsumes the getattr.
     * (we assume that if the times don't match before the getattr that they'll
     * still not match after the getattr)  (XXX?)
     */
    if ((vobmod == 0) && mfs_ac_timedout(VTOM(vp), TRUE, cd)) {
	BUMPVSTAT(vp,acstat.ac_evmiss);
	error = mfs_clnt_getattr(vp, cd);
	BUMPSTAT(mfs_acstat.ac_misses);
	BUMPVSTAT(vp,acstat.ac_misses);
        if (error) return(0);	/* Not valid if any errors! */

	/* if we refetched stats, re-check vobmod */
	vobmod = (tvp->tv_sec != VTOM(vp)->mn_vob.attr.event_time.tv_sec) ||
		 (tvp->tv_usec != VTOM(vp)->mn_vob.attr.event_time.tv_usec);
    }

    return((vobmod == 0));
}

void
mvfs_noview_vobrt_getattr(
    VNODE_T *vp,
    VATTR_T *vap
)
{
    u_long dev;
    struct timeval tv;

    VATTR_SET_TYPE(vap, VDIR);
    VATTR_SET_MODE_RIGHTS(vap, 0555);
    VATTR_SET_MODE_TYPE(vap, S_IFDIR);
    VATTR_SET_UID(vap, 0);            /* no view uses unix cred for NT */
    VATTR_SET_GID(vap, 0); 
    /* Give this a different fsid/dev to mtab entry will
       not match any dir returned during pwd (see below) */
    dev = FSID_TO_DEV(V_TO_MMI(vp)->mmi_nvfsid);
    VATTR_ADJUST_DEV(dev, vp);
    VATTR_SET_FSID(vap, &dev);
    VATTR_SET_NODEID(vap, VTOM(vp)->mn_hdr.fid.mf_dbid);
    VATTR_SET_NLINK(vap, 2);
    VATTR_SET_SIZE(vap, 1024);
    VATTR_SET_BLKSIZE(vap, DEV_BSIZE);
    tv.tv_sec = MDKI_CTIME();
    tv.tv_usec = 0;
    VATTR_SET_ATIME_TV(vap, &tv);
    VATTR_SET_MTIME_TV(vap, &tv);
    VATTR_SET_CTIME_TV(vap, &tv);
    VATTR_SET_RDEV(vap, 0);
    VATTR_SET_NBLOCKS(vap, VATTR_BTODB(1024));
    VATTR_FILL(vap);
}

/* MFS_GETATTR - get attributes vnode op */

int
mfs_getattr(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag,				/* NYI; currently unused */
    CALL_DATA_T *cd
)
{
    VNODE_T *vp = avp;
    int error;
    int fromcache = 0;	/* For logging */
    mfs_mnode_t *mnp = VTOM(vp);
    int stalecnt = 0;
    u_long dev;
    struct timeval tv;
    VNODE_T *rdir;
    /* Some platforms (e.g. linux) call this routine with the mnode 
     * lock already held for certain mnode classes . Taking the
     * lock again in here will result in a recursive lock panic.
     * The workaround is to check for the mnode lock already held
     * for these mnode classes and skip locking again.
     *
     * The following flag keeps track of when we need to unlock after
     * checking and locking the mnode for these special cases.
     */
    int munlock = 0; /* Used only for MFS_VIEWCLAS and MFS_NTVWCLAS */
    MVFS_DECLARE_THREAD(mth)
    
    MVFS_ENTER_FS(mth);

    ASSERT(mnp->mn_hdr.vp);
    switch (mnp->mn_hdr.mclass) {
      case MFS_SDEVCLAS:
        /* Fill in canned attributes */
        error = 0;
        VATTR_SET_TYPE(vap, VCHR);
        VATTR_SET_MODE_RIGHTS(vap, 0444);
        VATTR_SET_MODE_TYPE(vap, S_IFCHR);
        VATTR_SET_UID(vap, 0);  /* specdev uses unix cred for NT */
        VATTR_SET_GID(vap, 0);
        dev = FSID_TO_DEV(VFS_FSID(vp->v_vfsp));
        VATTR_ADJUST_DEV(dev, vp);
        VATTR_SET_FSID(vap, &dev);
        VATTR_SET_NODEID(vap, mnp->mn_hdr.fid.mf_dbid);
        VATTR_SET_NLINK(vap, 2);
        VATTR_SET_SIZE(vap, 0);
        VATTR_SET_BLKSIZE(vap, DEV_BSIZE);

        tv.tv_sec = MDKI_CTIME();
        tv.tv_usec = 0;
        VATTR_SET_ATIME_TV(vap, &tv);
        VATTR_SET_MTIME_TV(vap, &tv);
        VATTR_SET_CTIME_TV(vap, &tv);
        VATTR_SET_RDEV(vap, VATTR_GET_FSID(vap) & 0xffff);
        VATTR_SET_NBLOCKS(vap, VATTR_BTODB(VATTR_GET_SIZE(vap)));
        VATTR_FILL(vap);
        break;

      case MFS_VIEWCLAS: 
        MFS_CHKSP(STK_GETATTR);
        error = MVOP_GETATTR(MFS_REALVP(vp), MFS_CLRVP(vp), vap, flag,
                             MVFS_CD2CRED(cd));
        /* 
         * Special hack to fix problems with programs that require
         * "/" to be a real device on the system, not a funky MFS
         * object.  If the view you are stat-ing is your root,
         * DONT fix up the device etc.  Otherwise, we MUST fix
         * up the dev/inumber or pwd will get a non-root view
         * confused with the root view (and give an incorrect
         * pwd result if you have set "/" to view 'xyz' and are cd'd
         * into view 'qrs').  Note that this makes an ls -l
         * of "/view" differ from one view to the next (your current
         * view will stat as a non-MFS device).
         */
	 rdir = MDKI_GET_U_RDIR();
        /*
         * rdir may be NULL if there is no chroot in effect (no
         * setview), or on Linux due to resource shortages.
         */
        if (vap && vp != rdir) {
            dev = FSID_TO_DEV(VFS_FSID(vp->v_vfsp));
            VATTR_ADJUST_DEV(dev, vp);
            VATTR_SET_FSID(vap, &dev);
            if (!ISLOCKEDBYME(MLOCK_ADDR(mnp))) {
                MLOCK(mnp); /* Guard against update in progress */
                munlock = 1; /* unlock mnode when done */
            }
            VATTR_SET_NODEID(vap, mnp->mn_hdr.mnum);
            if (MDKI_COMMON_GET_DATAP()->mvfs_vlinkcnt2 != 0) {
                VATTR_SET_NLINK(vap, 2);
            }
            if (error == 0) {
                MVFS_COPY_UID_TO_VATTR(vap, &mnp->mn_view.cuid, 
                                       mnp, &error); 
            }
            if (error == 0) {
                MVFS_COPY_GID_TO_VATTR(vap, &mnp->mn_view.cgid,
                                       mnp, &error); 
            }
            VATTR_SET_CTIME_TS(vap, &mnp->mn_view.ctime);
            if (munlock) MUNLOCK(mnp);
            VATTR_FILL(vap);
        }
        if (rdir) {
            MDKI_VNRELE_RCDIR(rdir); /* release root if needed */
        }
        rdir = NULL;
        break;
      case MFS_LOOPCLAS:	/* Pass on to real vnode */
        MFS_CHKSP(STK_GETATTR);
	error = MVOP_GETATTR(MFS_REALVP(vp), MFS_CLRVP(vp), vap, flag,
                    MVFS_CD2CRED(cd));
        break;
      case MFS_VIEWDIRCLAS:
        error = mfs_viewdirgetattr(vp, vap, MVFS_CD2CRED(cd));
        if (vap) VATTR_FILL(vap);
        break;
      case MFS_NTVWCLAS:
        if (!ISLOCKEDBYME(MLOCK_ADDR(mnp))) {
            MLOCK(mnp); /* Guard against update in progress */
            munlock = 1; /* unlock when done */
        }
	error = mvfs_ntvw_getattr(vp, vap, MVFS_CD2CRED(cd));
        if (munlock) MUNLOCK(mnp);
	break;
      case MFS_VOBRTCLAS:
        if ((flag & MVFS_GETATTR_NO_BINDROOT) != 0)
            error = ESRCH;
        else
            vp = mfs_bindroot(vp, cd, &error);
        if (error) {
            if (error == ESRCH) {	 /* Dummy up stats for no view */
                error = 0;
                mvfs_noview_vobrt_getattr(vp, vap);
             }
             break;
	}
        /* Fall through */
      case MFS_VOBCLAS: {

        (void) mfs_rebind_vpp((vp != avp), &vp, cd);

        mnp = VTOM(vp);
	MLOCK(mnp);

	/* 
         * Get latest cleartext attrs if a view object.
	 * Don't bother doing this for 'immutable' objects
	 * in a VOB.  In that case, we got the stats at
	 * getcleartext time (if the cleartext is active),
	 * and they shouldn't have changed.  This saves
	 * lots of 'refresh' stats over NFS to the cleartext
	 * pool (about 90% of the traffic we generate to
	 * the cleartext pool).
         */

	if (mnp->mn_hdr.realvp && !mnp->mn_vob.cleartext.isvob) {
            error = mvfs_clearattr(vp, vap, MVFS_CD2CRED(cd));
            /* Mark cleartext for purge on ANY error */
            if (error) {
                mfs_clear_mark_purge(vp);
            }
        }

        if (mnp->mn_vob.sync_ctime && mnp->mn_hdr.realvp) {
#if !defined(ATRIA_WIN32_COMMON) && (defined(MVFS_LOG) || defined(MVFS_DEBUG))
            struct timeval mtime, ctime;
#endif
            /*
             * fetch new cleartext attributes (due to NFS oddity on some
             * platforms) and force updates to the view--this will
             * update the ctime in the view database and we'll cache the
             * new value.
             */
            mvfs_sync_attr(mnp, vap, MFS_USE_NULLBH, 0, cd);
#if !defined(ATRIA_WIN32_COMMON) && (defined(MVFS_LOG) || defined(MVFS_DEBUG))
            VATTR_GET_MTIME_TV(&mnp->mn_vob.cleartext.va, &mtime);
            VATTR_GET_CTIME_TV(&mnp->mn_vob.cleartext.va, &ctime);
            MDB_XLOG((MDB_CTIME,
                     "vp=%"KS_FMT_PTR_T" attr_synced mtime=0x%"KS_FMT_TV_SEC_T_D".%"KS_FMT_TV_USEC_T_D" ctime=0x%"KS_FMT_TV_SEC_T_D".%"KS_FMT_TV_USEC_T_D"\n",
                      vp, mtime.tv_sec, mtime.tv_usec,
                      ctime.tv_sec, ctime.tv_usec));
#else
#endif
        }
        /* drop lock now that we're done with attributes */
        MUNLOCK(mnp);

        /* 
         * Set or reset error to 0.  
         * If clearattr was called and got an error, we want
         * to continue anyways.
         */

        error = 0;

        /* Check for usable cached attributes */

        if (!mfs_ac_timedout(mnp, FALSE, cd)) {
            if (vap) {
                MLOCK(mnp);     /* Guard against update in progress */
                mfs_mn_to_vattr(mnp, vap);
                MUNLOCK(mnp);
                VATTR_FILL(vap);
            }
            BUMPSTAT(mfs_acstat.ac_hits);
            BUMPVSTAT(vp,acstat.ac_hits);
            fromcache = 1;
        } else {
            error = mfs_clnt_getattr(vp, cd);
            if (!error && vap) {
                MLOCK(mnp);     /* Guard against update in progress */
                mfs_mn_to_vattr(mnp, vap);
                MUNLOCK(mnp);
                VATTR_FILL(vap);
            }
            BUMPSTAT(mfs_acstat.ac_misses);
            BUMPVSTAT(vp,acstat.ac_misses);
        }

        /* 
         * Special hack to fix programs that look through the /etc/mtab
         * for doing "pwd".  As an accelerator, some systems scan/stat
         * the entries in /etc/mtab instead of the parent dir when
         * a device boundary is crossed.  To make sure that the
         * mount point pame for the VOB mount point 
         * (without view extended name)
         * is only used for the "setview" case, we force all stats of
         * VOB objects from the "setview" or "null view" to have a
         * different fsid (the null view fsid) from stats of objects
         * from view-extended names (which have the normal and natural
         * mount point fsid).
         * Thus "/vobs/<mount>" is in the mtab, with the null view dev.
         * "/view/<tag>/vobs/<mount>" has the normal dev, and
         * is not in the mtab.
         * This only leaves "df" broken on view-extended names since
         * it can't find an mtab entry for the dev.
         */

        if (vap) {
            rdir = mfs_getview(NULL, MVFS_CD2CRED(cd), TRUE /* HOLD */);
            /* if rdir is non-NULL, it's an MFS node of type view */
            if (rdir && MFS_VIEW(vp) == rdir) {
                dev = FSID_TO_DEV(V_TO_MMI(vp)->mmi_nvfsid);
                /*
                 * don't smear the device number--let it use
                 * the default value which will match the mount
                 * table.  Smearing is only required to disambiguate
                 * /view/<tag1>/vobs/<mount> from
                 * /view/<tag2>/vobs/<mount>.  This is the case of
                 * plain /vobs/<mount> in the currently set view.
                 */
                /*VATTR_ADJUST_DEV(dev, vp);*/
                VATTR_SET_FSID(vap, &dev);
            } else {
                dev = (dev_t) VATTR_GET_FSID(vap);
                VATTR_ADJUST_DEV(dev, vp);
                VATTR_SET_FSID(vap, &dev);
            }
            if (rdir != NULL)
                VN_RELE(rdir);
	    }
	    rdir = NULL;
	    break;
	}
      default:
        error = ENXIO;
        break;
    }
    if (error == 0) {
        MVFS_MDEP_GETATTR_CLEANUP(avp, vp, vap, flag, MVFS_CD2CRED(cd));
    }

    if (vp != avp) VN_RELE(vp);   /* Release if allocated bound root vnode */

#ifdef MVFS_DEBUG
    if (vap) {
        struct timeval mtime, ctime;
        VATTR_GET_MTIME_TV(vap, &mtime);
        VATTR_GET_CTIME_TV(vap, &ctime);
	MDB_VLOG((MFS_VGETATTR,
		  "vp=%"KS_FMT_PTR_T" mask=%x fromcache=%d, mtime=0x%"KS_FMT_TV_SEC_T_D".%"KS_FMT_TV_USEC_T_D" ctime=0x%"KS_FMT_TV_SEC_T_D".%"KS_FMT_TV_USEC_T_D" err=%d\n",
		  vp, VATTR_GET_MASK(vap), fromcache,
		  mtime.tv_sec, mtime.tv_usec,
		  ctime.tv_sec, ctime.tv_usec,
		  error));
    } else
	MDB_VLOG((MFS_VGETATTR,"vp=%"KS_FMT_PTR_T" no mask fromcache=%d, err=%d\n",vp, fromcache, error));
#endif
    BUMPSTAT(mfs_vnopcnt[MFS_VGETATTR]);
    MVFS_EXIT_FS(mth);
    return(error);

}

/*
 * Internal routine to evaluate whether an object should be
 * choided (and which of the view-db and cleartext should be
 * modified)
 *
 * This call requires that the caller:
 *      - Have the mnode locked
 *      - Have an active cleartext vp for VREG files
 */

int mfs_changeattr_eval_choid(P1(VNODE_T *vp)
                              PN(VATTR_T *vap)
                              PN(int flag)
                              PN(CRED_T *cred)
                              PN(tbs_boolean_t *choid_needed_p)
                              PN(u_long *view_db_mask_p)
                              PN(u_long *clear_mask_p));

int
mfs_changeattr_eval_choid(vp, vap, flag, cred, choid_needed_p, 
			  view_db_mask_p, clear_mask_p)
VNODE_T *vp;
VATTR_T *vap;
int flag;
CRED_T *cred;
tbs_boolean_t *choid_needed_p;
u_long *view_db_mask_p;
u_long *clear_mask_p;
{
    u_long mask;
    mfs_mnode_t *mnp = VTOM(vp);
    
    ASSERT(MISLOCKED(mnp));

    /*
     * Initialize return masks to update nothing
     */
    *choid_needed_p = FALSE;
    *view_db_mask_p = 0;
    *clear_mask_p = 0;

    /* Get a copy of the vattr mask */

    mask = VATTR_GET_MASK(vap);

    /*
     * If the object isn't a regular file, then no special
     * checks are needed.  Just update the view-db with 
     * the requested attributes changes.  There is no cleartext
     * to update or "data" requiring a choid.
     */

    if (!MVFS_ISVTYPE(vp, VREG)) {
        *view_db_mask_p |= mask;
        return (0);
    }

    /* Must have cleartext for the following checks */

    ASSERT(mnp->mn_hdr.realvp != NULL);

    /*
     * MODE changes.
     *      Never choid
     *      Update view-db always
     *      Conditionally update the cleartext (see end of this routine)
     */
    if (mask & AT_MODE) {
        *view_db_mask_p |= AT_MODE;
    }

    /*
     * UID/GID/SIZE changes
     *      Always choid
     *      Always update view-db
     *      Always update cleartext (after choid this is view-pvt)
     */
    if (mask & (AT_UID|AT_GID|AT_SIZE)) {
        *choid_needed_p = TRUE;
        *view_db_mask_p |= (mask & (AT_UID|AT_GID|AT_SIZE));
        *clear_mask_p |= (mask & (AT_UID|AT_GID|AT_SIZE));
    }

    /*
     * MTIME updates
     *  To implement "touch" mtime updates need to choid.
     *  Some systems, however, also send down an mtime change
     *  when all they really want to change is the atime.
     *  We don't want to choid for these cases (you would lose
     *  CRs on the derived objects), so we detect and suppress
     *  this case by noticing that the requested mtime change
     *  is the same second as the current mtime.
     */
    if (mask & AT_MTIME) {
        if (VATTR_GET_MTIME(vap) != mnp->mn_vob.attr.fstat.mtime.tv_sec) {
            *choid_needed_p = TRUE;
            *view_db_mask_p |= AT_MTIME;
            *clear_mask_p |= AT_MTIME;
            *clear_mask_p |= (mask & AT_MTIME_SET);
        }
    }

    /*
     * CTIME updates
     *      Never choid
     *      Always update the view-db
     *      Conditionally update the cleartext (see end of routine)
     */

    if (mask & (AT_ATIME|AT_CTIME)) {
        *view_db_mask_p |= (mask & (AT_ATIME|AT_CTIME));
    }

    /*
     * Make sure a CHOID is done for any read-only cleartext
     * that has any cleartext mask bit sets past this point.
     * This is a catch-all to make sure no "bug" in any of
     * the code above can cause a shared DO to get its 
     * attributes whacked... any such bug will cause a specific
     * user/view to get a spurious choid.
     */

    if (MFS_CLRTEXT_RO(mnp) && !*choid_needed_p && *clear_mask_p != 0) {
        mvfs_log(MFS_LOG_ERR, 
            "Unexpected choid of winked-in derived object: vw=%s vob=%s dbid=0x%x",
                mfs_vp2vw(vp), mfs_vp2dev(vp), mfs_vp2dbid(vp));
        *choid_needed_p = TRUE;
    }

    /*
     * Conditionally update the attributes on the cleartext file.
     * We do this only if the cleartext file is in the view-storage
     * or will be in the view-storage as the result of a choid
     * being needed for some other attribute change.  The basic
     * idea is that these attributes must be kept in sync between
     * the view-db and the cleartext for cleartext files in the
     * view, but not for the cleartext file in the VOB associated
     * with a winked-in DO (other VOB objects will have access
     * denied with EROFS, so we don't need to worry about them)
     */

    if ((*view_db_mask_p & (AT_MODE|AT_ATIME|AT_CTIME)) != 0) {
        if (!MFS_CLRTEXT_RO(mnp) || *choid_needed_p) {
            *clear_mask_p |= *view_db_mask_p & (AT_MODE|AT_ATIME|AT_CTIME);
            *clear_mask_p |= mask & AT_ATIME_SET;
        }
    }

    return (0);
                
}

int
mvfs_changeattr(
    VNODE_T *avp,
    VATTR_T *vap,
    int flag,			/* NYI */
    CALL_DATA_T *cd,
    MVFS_CALLER_CONTEXT_T *ctxp
)
{
    VNODE_T *vp = avp;
    tbs_oid_t prevoid;		/* For changeoid audit record */
    int error;
    int sverr = 0;
    mfs_mnode_t *mnp;
    u_long mask;
    VATTR_T *xvap = NULL;
    struct timeval tm,ta;
    int choidflags;
    CLR_VNODE_T *cvp = NULL;
    MOFFSET_T oldsize;
    MVFS_DECLARE_THREAD(mth)
    tbs_boolean_t choid_needed;
    u_long view_db_mask;
    u_long clear_mask;

    if ((vp->v_vfsp->vfs_flag & VFS_RDONLY) != 0)
        return EROFS;

    ASSERT(VTOM(vp)->mn_hdr.vp);

    /* Fill in vattr mask from arg mask */

    mask = VATTR_GET_MASK(vap);
    VATTR_GET_MTIME_TV(vap, &tm);
    VATTR_GET_ATIME_TV(vap, &ta);
    
    /* Check for fields which can not be set.  Some NFS servers
     * allow va_type to be set on a setattr and ignore it, and some
     * (like the SUN NFS servers) do not.  To guarantee consistent
     * working behavoir of all MFS setattr calls to the cleartext 
     * always disallow va_type on setattr calls. 
     */

    if (mask & AT_TYPE) {
	mvfs_log(MFS_LOG_DEBUG, "va_type set on changeattr mask=0x%x\n", mask);
	error = EINVAL;
	return(error);
    }
    if (mask & AT_NOSET) {
	mvfs_log(MFS_LOG_DEBUG, "invalid changeattr mask=0x%x\n", mask);
	error = EINVAL;
	return(error);
    }

    /* Do not allow setting of size for directories */
    if ((mask & AT_SIZE) && MVFS_ISVTYPE(vp, VDIR)) {
        error = EISDIR;
        return(error);
    }

    MVFS_ENTER_FS(mth);
    /* Switch based on class of object */

    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    if (mask & AT_SIZE) error = EINVAL;
	    /* FIXME: allow this someday for rights/time? */
	    else error = EPERM;
	    break;
	case MFS_LOOPCLAS:
	    /* 
	     * Flush our pages to cleartext file.  (M)VOP_SETATTR
	     * for cleartext is reponsible for any flushing it needs.
	     */
	    if (MVFS_ISVTYPE(vp, VREG)) {
		mvfs_log(MFS_LOG_ERR,
			"getattr on loopback file node (shouldn't happen), vp=%"KS_FMT_PTR_T"\n", vp);
		(void) PVN_FLUSH(vp, MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
	    }
	    /* Fall through */
	case MFS_VIEWCLAS:
	    error = MVOP_SETATTR(MFS_REALVP(vp), vap, flag, cd, ctxp);
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EROFS;
	    break;
	case MFS_VOBRTCLAS:
	    vp = mfs_bindroot(vp, cd, &error);
	    if (error == ESRCH) error = EROFS;  /* No view gets read-only */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {
            MOFFSET_T max_offset;

            max_offset = MVFS_GET_MAXOFF(vp);

            /* Get a vattr struct we can mess around with */

	    if ((VATTR_GET_TYPE(vap) == VREG) && (mask & AT_SIZE) && 
	        (VATTR_GET_SIZE(vap) > max_offset))
	    {
		return(EFBIG);
	    }

            xvap = MVFS_VATTR_ALLOC();

            if (xvap == NULL) {
                error = ENOMEM;
                break; /* from switch */
            }

            VATTR_NULL(xvap);
            MVFS_COPY_VATTR(xvap, vap, &error); /* make a copy of original vattr */

            if (error) break;

	    (void) mfs_rebind_vpp((vp != avp), &vp, cd);

	    /*
	     * Sync pages to cleartext.
	     * MVOP_SETATTR is responsible for any sync the cleartext
	     * FS requires.
	     */
            if (MVFS_ISVTYPE(vp, VREG) && VTOM(vp)->mn_hdr.cached_pages) {
		(void) PVN_FLUSH(vp, MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
	    }

	    mnp = VTOM(vp);
    	    MLOCK(mnp);

    	    if (MVFS_ISVTYPE(vp, VREG)) {

	        /* 
	         * Get validated cleartext vnode for clearattr and
		 * RO cleartext checks below.
		 */

	        error = mfs_getcleartext(vp, NULL, cd);
	        if (error) {
		    MUNLOCK(mnp);
		    break;
		}

                error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
                if (error) {
                    MUNLOCK(mnp);
                    break;
                }
                /*
                 * Save "oldsize" in case we truncate the file, so
                 * we can pass the previous size to the PVN_TRUNC
                 * call later on to toss pages beyond the new eof.
                 */
                oldsize = mnp->mn_vob.attr.fstat.size;
            }

            error = mfs_changeattr_eval_choid(vp, vap, flag, MVFS_CD2CRED(cd),
                    &choid_needed, &view_db_mask, &clear_mask);
            if (error) {
                MUNLOCK(mnp);
                break;
            }

            if (choid_needed) {

		/* Must be regular file */
		ASSERT(MVFS_ISVTYPE(vp, VREG));

		choidflags = MFS_CHOID_FORCE;
		/* Set truncate flag if setting length to zero. */
		if ((mask & AT_SIZE) != 0 && VATTR_GET_SIZE(vap) == 0) 
			choidflags |= MFS_CHOID_TRUNC;
		error = mfs_change_oid(vp, choidflags, MFS_SLEEP, cd);
		if (error) {
                    MUNLOCK(mnp);
                    break;
		}
	    }

	    if (MVFS_ISVTYPE(vp, VREG)) {

		/* 
		 * Fetch (and hold a vnode ptr to) the cleartext
		 * after.  We need the cleartext even if we did
		 * not do a choid above for setattr done below.
		 */

		error = mfs_getcleartext(vp, &cvp, cd);
		if (error) {
		    MUNLOCK(mnp);
		    break;
		}

		/* 
		 * Flush any dirty cleartext pages before we
		 * do the setattr call on the cleartext.
		 */
		ASSERT(cvp);
		if (mnp->mn_hdr.clear_dirty) {
		    (void) PVN_FLUSH(MVFS_CVP_TO_VP(cvp), MFS_PVN_FLUSH,
                                     MVFS_CD2CRED(cd));
		    mnp->mn_hdr.clear_dirty = 0;
		}

	        /*
	         * Due to various identity transformations, we have to
	         * be careful that we don't hose the user by allowing
	         * him to do something in the view/vob which can't
	         * be done to the cleartext.  Therefore, for all
	         * ops that require "owner" permissions, make
	         * sure the user is also the "owner" of the cleartext
	         * before we do the op to the view.
	         */

		/* Non truncate ops - probe ownership */

		if (clear_mask & (AT_MODE|AT_UID|AT_GID)) {
		    error = mfs_clearowner(vp, 
					   clear_mask & (AT_UID|AT_GID|AT_MODE), 
					   cd);
		    if (error) {
		        mvfs_logperr(MFS_LOG_DEBUG, error,
			    "setattr: clearowner check"); 
		        mvfs_log(MFS_LOG_DEBUG, 
			    "m/u/g/s= 0%o/%d/%d/%"MVFS_FMT_VATTR_SIZE_T_D" a/mtime= 0x%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X"/0x%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X"\n",
				VATTR_GET_MODE(vap), VATTR_GET_UID(vap),
				VATTR_GET_GID(vap), VATTR_GET_SIZE(vap),
				ta.tv_sec, ta.tv_usec, tm.tv_sec, tm.tv_usec);
			MUNLOCK(mnp);
			break;
		    }
		}
	    }

	    /*
	     * Access check for setting atime/mtime (utimes call).
             * We do this after all the suppression of atime/mtime setting
             * above.  We also do this because not all systems enforce
	     * the access check above the setattr vnode op, so we have to
	     * do it in here.
	     */

	    if (view_db_mask & (AT_MTIME|AT_ATIME)) {
                if ( ((MVFS_COMPARE_MNODE_UID(MVFS_CD2CRED(cd),
                        mnp->mn_vob.user_id)) == FALSE) &&
                     (MVFS_CHKACCESS_MNODE(vp, VWRITE, mnp->mn_vob.user_id,
                        mnp->mn_vob.group_id, mnp->mn_vob.attr.fstat.mode,
				    MVFS_CD2CRED(cd)) != 0) ) 
                {
		        error = EACCES;
		        MUNLOCK(mnp);
		        break;
                }
	    }
		
    	    /* Change the attributes in the view.  
	     * All setattr calls have writer class credentials. 
	     * Lock around both the change to the view and the change to 
	     * the cleartext to prevent race conditions 
	     * For elements with cleartext (i.e VREG), do NOT set the 
	     * mtime/atime directly here.... for these, we must set the 
             * cleartext first, then go back and sync the cleartext a/mtime 
             * to the view.  The times for set atime/mtime may be set 
             * differently at the server for the cleartext due to different 
             * implementations of NFS.
             * For other types, atime/mtime set in the view now
             */

	    if (MVFS_ISVTYPE(vp, VREG))
	        VATTR_SET_MASK(xvap, view_db_mask & ~(AT_MTIME|AT_ATIME));
	    else
	        VATTR_SET_MASK(xvap, view_db_mask);
	    error = mvfs_clnt_setattr_locked(vp, xvap, 0, 
                                             MFS_USE_PROCBH, 1, cd, 0);

	    /* Change the attributes on the cleartext */

	    if (!error && cvp && clear_mask != 0) {
	 	/*
	   	 * Make copy of original vattr, it may have changed above.
		 */
                MVFS_FREE_VATTR_FIELDS(xvap);
                MVFS_COPY_VATTR(xvap, vap, &error);
                if (error) {
                    mvfs_log(MFS_LOG_ERR,
                        "mvfs_changeattr: changed view attr but not cleartext, err %d\n",
                         error);
                    mfs_clear_mark_rwerr(vp);
                    MUNLOCK(mnp);
                    break;
                }
                VATTR_SET_MASK(xvap, clear_mask);
		error = MVOP_SETATTR(MVFS_CVP_TO_VP(cvp), xvap, flag, cd, ctxp);

		/* Gross hack for setuid programs.  If operation
	         * fails with EPERM and we are setuid root, then
	         * try again with our "real id" to handle untrusted
 		 * root over the net.
		 */
		if (error == EPERM && MDKI_CR_IS_SETUID_ROOT(MVFS_CD2CRED(cd)))
                {
		    MDKI_CR_SET_E2RUID(MVFS_CD2CRED(cd));
		    error = MVOP_SETATTR(MFS_REALVP(vp), xvap, flag, cd, ctxp);
		    MDKI_CR_SET_E2ROOTUID(MVFS_CD2CRED(cd));
		    mvfs_logperr(MFS_LOG_DEBUG, error, 
				"setattr: setuid root retry");
	 	}

		if (error) {
                    /*
                     * The view and cleartext attrs may now be inconsistent,
                     * perhaps because cltxt (NFS) permissions are more
                     * stringent than view in some cases.
                     *
                     * Suppress printing of error message if we failed
                     * to set only mtime/atime/size.
                     * These should be re-synced from the
                     * cltxt, which we can do silently by falling
                     * through to mvfs_clearattr() and mtime sync logic
                     * below.  However, we still need to indicate the
                     * cleartext error to our caller, so save it.
                     */
                    MDB_VLOG((MFS_VSETATTR,
                              "vp=%"KS_FMT_PTR_T" cltxt setattr mask=%x flg=%x failed errno=%d\n",
                              vp, clear_mask, flag, error));
                    /* this mask is now defined in mvfs_systm.h
                     * so that it can be made platform specific.
                     */
                    if ((clear_mask & ~MVFS_SIZE_TIME_MASK) != 0) {
                        mfs_clear_error(vp, "cleartext setattr failed", error);
                    } else {
                        /* mark for flushing as mfs_clear_error()
                           would do in most cases. */
                        mfs_clear_mark_rwerr(vp);
                        sverr = error;
                        error = 0;
                        /* ... and fall through to resync code below. */
                    }
                }
                if (error == 0) {
		    /* Fetch out latest/best cleartext attributes. */
	            error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
		    /* 
		     * Make sure size matches what we think we set 
		     * Otherwise, subsequent 'size' merges will with the mnode
                     * stats will be using stale data from the cleartext file
                     * and will cause mistakes in the MVFS.
                     */
		    if (clear_mask & AT_SIZE) {
		        if (VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va) != 
								VATTR_GET_SIZE(vap)) {
			    mvfs_log(MFS_LOG_DEBUG, 
				"setattr: cleartext size %"MVFS_FMT_VATTR_SIZE_T_D" != truncate size %"MVFS_FMT_VATTR_SIZE_T_D"\n", 
					VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va),
					VATTR_GET_SIZE(vap));
			}
		    }

		    /* If updated size or mtime, then must sync the mtime */
		    if ((clear_mask & (AT_SIZE|AT_MTIME)) != 0 || sverr != 0) {
			VTOM(vp)->mn_vob.sync_mtime = 1;
		    }
		    /* If we have set mtime or atime, 
		     * then we must sync the attributes from the
	             * cleartext back to the view.  (These ops
		     * have the cleartext as 'master'
		     */
		    if ((clear_mask & (AT_MTIME|AT_ATIME)) != 0 || sverr != 0)
                    {
		        VATTR_NULL(xvap);	/* Create null xvap for sync */
	                error = mvfs_clnt_setattr_locked(vp, xvap, 0,
						MFS_USE_PROCBH, 1,
                                                cd, 0);
			mvfs_logperr(MFS_LOG_WARN, error,
			  "setattr: sync_mtime failed: vw=%s vob=%s dbid=0x%x",
			    mfs_vp2vw(vp), mfs_vp2dev(vp), mfs_vp2dbid(vp));
			error = 0;	/* Don't fail op for this... */
		    }
		}
            }
            MUNLOCK(VTOM(vp));
            if (sverr)
                error = sverr;

	    /* 
	     * If we successfully updated the size, then
	     * we must notify the VM subsystem to toss any
	     * cached pages past the end of the file.
	     */
	    if (!error && ((mask & AT_SIZE) != 0) && (MVFS_ISVTYPE(vp, VREG))) {
		PVN_TRUNC(vp, flag, VATTR_GET_SIZE(vap), oldsize,
                          MVFS_CD2CRED(cd));
	    }

	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    /* Audit if no error and truncating a file to 0 */
    if (!error && MVFS_ISVTYPE(vp, VREG) && ((mask & AT_SIZE) != 0) &&
	VATTR_GET_SIZE(vap) == 0) {
	MFS_AUDIT(MFS_AR_TRUNCATE,NULL,NULL,NULL,NULL,vp,cd);
    }

    if (xvap != NULL) {
        MVFS_FREE_VATTR_FIELDS(xvap);
        MVFS_VATTR_FREE(xvap);
    }

    if (cvp != NULL) CVN_RELE(cvp); /* Release held cleartext if one */
    if (vp != avp) VN_RELE(vp);   /* Release if allocated bound root vnode */

    MDB_VLOG((MFS_VSETATTR,"vp=%"KS_FMT_PTR_T" msk=%x flg=%x m/u/g/s = 0%o/%d/%d/%"MVFS_FMT_VATTR_SIZE_T_D"\n\ta/mtime=0x%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X"/0x%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X" err=%d\n",vp, 
	    VATTR_GET_MASK(vap), flag,
		VATTR_GET_MODE(vap), VATTR_GET_UID(vap), VATTR_GET_GID(vap),
		VATTR_GET_SIZE(vap), ta.tv_sec, ta.tv_usec, 
		tm.tv_sec, tm.tv_usec, error));
    BUMPSTAT(mfs_vnopcnt[MFS_VSETATTR]);
    MVFS_EXIT_FS(mth);
    return (error);

}

/*
 * mfs_owner - check if current cred is owner of an object and
 * therefore has rights to modify the owner, group, and/or access control.
 */
int
mfs_owner(
    VNODE_T *vp,
    CALL_DATA_T *cd
)
{
    VATTR_T va;
    int error;
    view_fhandle_t *vfhp;
   
    VATTR_NULL(&va); 
    VATTR_SET_MASK(&va, AT_UID);
    error = mfs_getattr(vp, &va, 0, cd);
    if (error) {
        MVFS_FREE_VATTR_FIELDS(&va); /* free nt sids if copied */
        return(error);
    }
    if ((MVFS_IS_OWNER(MVFS_CD2CRED(cd), &va)) == FALSE) {
        MVFS_FREE_VATTR_FIELDS(&va);
        return(EPERM);
    } else {
        MVFS_FREE_VATTR_FIELDS(&va);
        /* 
         * Check for EROFS errors
         * This routine only returns OK if the owner can
         * actually write (chmod/chown/chgrp) the object.
         */
        switch (VTOM(vp)->mn_hdr.mclass) {
            case MFS_SDEVCLAS:      return(EROFS);
            case MFS_LOOPCLAS:      return(0);
            case MFS_VIEWCLAS:      return(0);
            case MFS_VIEWDIRCLAS:   return(EROFS);
            case MFS_VOBRTCLAS:     return(0);
            case MFS_VOBCLAS:
                /* 
                 * Dirs are always OK for view-pvt chmod/chown/chgrp
                 * The view will check the operation later for whether
                 * it is valid.
                 */
                if (!MVFS_ISVTYPE(vp, VREG)) return(0);
                /* 
                 * For files, only OK if not history mode or 
                 * vob object.  (i.e. a view-pvt file or winked-in DO)
                 */
                vfhp = &VTOM(vp)->mn_vob.vfh;
                if (MFS_HMVFH(vfhp) || !VIEW_ISA_VIEW_OBJ(vfhp)) 
                    return(EROFS);
                else
                    return(0);
            case MFS_NTVWCLAS:      return(EROFS);
            default:                return(EINVAL);
        }
    }
}

/*
 * mfs_accessv - renamed since mfs_access reserved for V.3 FSS op.
 */

int
mfs_accessv(
    VNODE_T *vp,
    int mode,
    int flag,				/* NYI; not currently used */
    CALL_DATA_T *cd
)
{
    return(mvfs_accessv_ctx(vp, mode, flag, cd, NULL));
}

int
mvfs_accessv_ctx(
    VNODE_T *vp,
    int mode,
    int flag,				/* NYI; not currently used */
    CALL_DATA_T *cd,
    MVFS_ACCESS_CTX_T *ctxp
)
{

    VATTR_T va;
    int error;
    MVFS_DECLARE_THREAD(mth)

    MVFS_ENTER_FS(mth);
    BUMPSTAT(mfs_vnopcnt[MFS_VACCESS]);

    /*
     * If in loopback, invoke the underlying VOP_ACCESS. This prevents
     * us from overriding the underlying FS's permissions.
     */

    if (MFS_ISLOOP(VTOM(vp))) {
        error = MVOP_ACCESS(MFS_REALVP(vp), mode, 0, cd, ctxp);
        MVFS_EXIT_FS(mth);
        return error;
    }

    /* Request the attributes we will use, for cleanliness (used to be
       the case that loopback calls would come through here and the
       underlying file system could have optimized its return values
       based on the mask) */
    VATTR_NULL(&va);
    VATTR_SET_MASK(&va, AT_UID|AT_GID|AT_MODE);

    error = mfs_getattr(vp, &va, 0, cd);
    if (error) {
        MVFS_FREE_VATTR_FIELDS(&va); /* free nt sids if copied */
        MDB_VLOG((MFS_VACCESS,"vp=%"KS_FMT_PTR_T" err=%d: getattr failed\n",vp, error));
        MVFS_EXIT_FS(mth);
        return error;
    }
    error = MVFS_CHKACCESS(vp, mode, &va, MVFS_CD2CRED(cd));
    MVFS_FREE_VATTR_FIELDS(&va);
    MVFS_EXIT_FS(mth);
    return error;
}

int
mfs_chkaccess(vp, mode, vuid, vgid, vmode, cred)
VNODE_T *vp;
int mode;
u_long vuid;
u_long vgid;
int vmode;
CRED_T *cred;
{

    int error;

    /* Superuser always gets access */

    if (MDKI_CR_IS_ROOT(cred)) {
	MDB_VLOG((MFS_VACCESS,"vp=%"KS_FMT_PTR_T" err=%d: superuser match\n",vp,0));
	return(0);
    }

    /* Access check based on only one of
       owner, group, public (checked in that order) */

    if (MDKI_CR_GET_UID(cred) != vuid) {
	/* Not owner - try group */
	mode >>= 3;
	if (MDKI_CR_GET_GID(cred) != vgid) {
	    /* Not group, try group list */
	    if (!MVFS_GROUPMEMBER(vgid, cred))
		/* No groups, try public */
		mode >>= 3;			
	}
    }

    if ((vmode & mode) == mode) {
	error = 0;
    } else {
	error = EACCES;
    }

    MDB_VLOG((MFS_VACCESS,"vp=%"KS_FMT_PTR_T" err=%d: rqst=%o, mode=%o\n",vp,error,mode,vmode));
    return(error);
}

#ifdef MVFS_GROUPMEMBER_DEFAULT
/*
 * Routine to check if a gid is a member of the group list.
 */
STATIC int
mvfs_groupmember(u_long vgid, CRED_T *cred)
{
	CRED_GID_T *gp;
	CRED_GID_T *gpe;

	gp = MDKI_CR_GET_GRPLIST(cred);
	gpe = MDKI_CR_END_GRPLIST(cred);
#ifdef MFS_NO_GROUPCOUNT_IN_CRED
	/* List terminated by special null value */
	for (; gp < gpe && *gp != CRED_EOGROUPS; gp++)
#else
	for (; gp < gpe; gp++)
#endif
	{
	    if (vgid == *gp) {
		return(TRUE);
	    }
	}
	return(FALSE);
}
#endif  /* MVFS_GROUPMEMBER_DEFAULT */

int
mfs_readlink(
    VNODE_T *avp,
    struct uio *uiop,
    CALL_DATA_T *cd
)
{
    VNODE_T *vp = avp;
    int error;
    ssize_t uc = uiop->uio_resid;
    char *lnkbuf;
    int  lnklen;
    mfs_mnode_t *mnp;
    MVFS_DECLARE_THREAD(mth)
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    ASSERT(VTOM(vp)->mn_hdr.vp);

    /* Only allowed for objects of type "link".
       Standard error is "ENXIO". */

    if (!MVFS_ISVTYPE(vp, VLNK)) {
	return (ENXIO);
    }
    MVFS_ENTER_FS(mth);

    /* Switch based on class.  Note that several kinds
       of objects are not included as they can not be symlinks. */

    switch (VTOM(vp)->mn_hdr.mclass) {
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    error = MVOP_READLINK(MFS_REALVP(vp), uiop, cd);
	    break;
	case MFS_VOBCLAS: {

	    /* 
	     * First, check for a cached version of the link text.
 	     * Stale text is purged by a "lookup" or stat
             * which finds that the vnode is modified. 
	     * (see the code in mfs_attrcache() if it detects a modified
             *  object)
	     * The logic to allow us to do this is based on the fact
             * that to change the contents of a symlink, you must
             * remove and re-create it.  That makes a change to the dir,
             * which flushes the name cache translation to this symlink,
             * which forces a lookup of the symlink object again, which
             * will revalidate/purge the cached symlink text and force
             * us to refetch the new version.
	     */
	    mnp = VTOM(vp);
 	    MLOCK(mnp);
	    if (mcdp->mvfs_rlenabled && mnp->mn_vob.slinktext) {
		BUMPSTAT(mfs_rlstat.rl_hits);
		/* Cached entry is a string, links don't move the NULL */
		error = UIOMOVE(mnp->mn_vob.slinktext, 
				mnp->mn_vob.slinklen-1, UIO_READ, uiop);
		/* Make a copy of the link text, so that audit call is not
		 * affected if a cache update races in and frees the text
  		 * after we unlock.
		 */
		lnkbuf = STRDUP(mnp->mn_vob.slinktext);
		MUNLOCK(mnp);
		MFS_AUDIT(MFS_AR_RDLINK, NULL, lnkbuf, NULL, NULL, vp, cd);
		STRFREE(lnkbuf);
		break;
	    }
		
	    BUMPSTAT(mfs_rlstat.rl_misses);	
	    lnkbuf = (char *)KMEM_ALLOC(MAXPATHLEN, KM_SLEEP);
	    if (lnkbuf == NULL) {
		error = ENOMEM;
		MUNLOCK(mnp);
		break;
	    }
	    error = mfs_clnt_readlink(vp, lnkbuf, &lnklen, cd);
	    if (!error) {
	        error = UIOMOVE(lnkbuf, (int)lnklen, UIO_READ, uiop);

		ASSERT(lnklen < MAXPATHLEN);
		lnkbuf[lnklen] = '\0';	/* Force null to make a str */
		
		/* Cache symlink contents for next time */

		if (mcdp->mvfs_rlenabled) {
		    if (mnp->mn_vob.slinktext) {
		        KMEM_FREE(mnp->mn_vob.slinktext, mnp->mn_vob.slinklen);
		        mnp->mn_vob.slinktext = NULL;
		        mnp->mn_vob.slinklen = 0;
		    }
	
                    /* Cache the symlink text with a NULL appended to make
                       it a string for auditing (see mfs_audit()) */	
		    mnp->mn_vob.slinktext = 
		        (char *)KMEM_ALLOC(lnklen+1, KM_SLEEP);
		    if (mnp->mn_vob.slinktext) {
		        BCOPY(lnkbuf, mnp->mn_vob.slinktext, lnklen+1);
		        mnp->mn_vob.slinklen = lnklen+1;
	 	    }
	  	}

		/* Must unlock before audit */

		MUNLOCK(mnp);

	        /* Do auditing */
		MFS_AUDIT(MFS_AR_RDLINK, NULL, lnkbuf, NULL, NULL, vp, cd);
	    } else {
		MUNLOCK(mnp);
	    }
	    KMEM_FREE(lnkbuf, MAXPATHLEN);
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    if (vp != avp) VN_RELE(vp);   /* Release if allocated bound root vnode */

    MDB_VLOG((MFS_VREADLINK,"vp=%"KS_FMT_PTR_T" err=%d\n",vp,error));
    BUMPSTAT(mfs_vnopcnt[MFS_VREADLINK]);
    MVFS_EXIT_FS(mth);
    return (error);
}

/*
 * MFS_INACTIVE
 * This procedure cleans up a lot of junk we created while manipulating
 * the file when the reference count goes to zero.
 *
 * For DB concurrency it is important that this routine leave no
 * operations for when the vnode/mnode gets recycled, but does them
 * all during the last close (e.g. now).  Otherwise a "random" user
 * may require the DB to purge the mnode while they have the DB
 * locked and get a deadlock.  Doing all the work now guarantees
 * we are doing for a user who knows he is touching an MFS file.
 *
 * (1) If the file was "removed" while it was open it got renamed
 *     by the mfs_remove op to keep it around while it was being used.
 *     The home node doesn't keep the refcnt, so we have to simulate
 *     that ability by not really removing the renamed file until the refcnt
 *     goes to zero (e.g. here!).
 *
 * (2) If the file was written, the VIEW doesn't know the length
 *     of the cleartext file.  Here we tell the VIEW what we
 *     did to the cleartext file while it wasn't watching.
 *
 * An additional problem is that since inactive can be called
 * by the last close of a 'read-only' authorized person, the
 * credentials may not be correct for updating the view.  We
 * save the credentials of the person who made the change
 * we now need to fix up.  These are mn_vob.rmv_cred
 * for the delayed remove, and mn_vob.cred for the delayed "set length".
 *
 * To avoid DB deadlocks, this routine must NOT DO ANY CALLS TO THE
 * VIEW for DIRECTORIES.
 */


/*ARGSUSED*/
int
mfs_inactive_common(
    VNODE_T *vp,
    int sleep_flag,
    CALL_DATA_T *cd	/* Don't trust */
)
{
    mfs_mnode_t *mnp;
    int error;

    mnp = VTOM(vp);
    ASSERT(MISLOCKED(mnp));

    /* Switch based on class of object. */

    error = 0;
    switch (mnp->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	case MFS_VIEWCLAS:
	case MFS_VIEWDIRCLAS:
	case MFS_VOBRTCLAS:
	    break;
	case MFS_NTVWCLAS:
	    break;
	/* Loopback.  Sync IO associated with the loopback vnode,
         * No need to sync the cleartext vnode, it will be sync'd
         * with the VN_RELE() later when we dump the vnode.
         * THIS ONLY WORKS BECAUSE WE DON'T CACHE LOOPBACK CLEARTEXT
	 * VNODE PTRS.
         */
	case MFS_LOOPCLAS:
	    break;
	case MFS_VOBCLAS: {

    	    /* 
	     * No delayed operations on dirs (remove, pages to write etc.)
       	     * Just return OK to make sure no possibility of DB deadlock.
	     * No need to purge name cache, we can't be here
             * unless all cache entries (and their hold counts)
             * have been released! 
             */

    	    if (MVFS_ISVTYPE(vp, VDIR)) {
		break;
	    }

	    /* Sync dirty pages of the cleartext vnode (if any)
	     * to the home node.  This is the necessary hook to try
	     * to make third-party accesses work reasonably.
	     *
	     * Note that this sync must be done with the mnode locked,
	     * to avoid races with other inactivators.  The alternative
	     * would be to grab an extra reference to the realvp, but
	     * just holding the mnode locked is cheaper.
	     *
	     * Note that I don't do this if inactive is being
	     * called with the nosleep flag.  To do so could
	     * cause deadlocks because the scan of the vnode for
	     * dirty pages might wait on a locked page which
	     * can't be paged out (via NFS) because the networking
	     * code needs free memory that only the caller can
 	     * make available (e.g. pageout daemon in some architectures).
	     */
	    if (mnp->mn_hdr.clear_dirty && mnp->mn_hdr.realvp &&
				sleep_flag == MFS_INACTIVE_SLEEP) {
	        (void) PVN_FLUSH(MVFS_CVP_TO_VP(mnp->mn_hdr.realvp),
                                 MFS_PVN_FLUSH, MVFS_CD2CRED(cd));

#ifdef NFSV4_SHADOW_VNODE
                if (mnp->mn_hdr.realvp_master) {
                    (void) PVN_FLUSH(MVFS_CVP_TO_VP(mnp->mn_hdr.realvp_master),
                                 MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
                }
#endif

		mnp->mn_vob.sync_mtime = 1;  /* Sync mtime */
		mnp->mn_hdr.clear_dirty = 0;
	    }
	    mnp->mn_vob.cleartext.used = 0;	/* turn off ref bit */

    	    /* Remove the renamed file if the user did a remove
       	     * while the file was open. 
	     * NOTE: All delayed operations in inactive must be done
             *       with a NULL build handle, as the build handle of
             *       the current process may well be meaningless to
             *       the view this object is in.
	     */

    	    if (mnp->mn_vob.rmv_name) {
		VNODE_T *dvp;
		char *rmv_name;
		CALL_DATA_T *tmp_cd;

		ASSERT(mnp->mn_vob.rmv_dvp);
		ASSERT(mnp->mn_vob.rmv_cred);
		/*
		 * We must drop the lock on this node--the leaf--while
		 * we do the remove, since the remove will lock the parent,
		 * and that is a locking order violation and a potential
		 * deadlock situation.
		 */
		tmp_cd = MVFS_ALLOC_SUBSTITUTE_CRED(cd, mnp->mn_vob.rmv_cred);
		rmv_name = mnp->mn_vob.rmv_name;
		dvp = mnp->mn_vob.rmv_dvp;
		mnp->mn_vob.rmv_dvp = NULL;
		mnp->mn_vob.rmv_name = NULL;
		mnp->mn_vob.rmv_cred = NULL;

		MUNLOCK(mnp);
		error = mfs_clnt_remove(dvp, rmv_name, MFS_USE_NULLBH,
                            sleep_flag == MFS_INACTIVE_SLEEP ? TRUE : FALSE,
                            tmp_cd);
		MLOCK(mnp);

		/* Just removed it, don't need attr cred now. */

		if (!error) {
	    	    MFS_ATTRINVAL(vp);	/* Link count changed */
	    	    /* Mark cleartext for purging of info on inactive */
	    	    mfs_clear_mark_name_purge(vp);
		} else {
		    mvfs_logperr(MFS_LOG_WARN, error, 
			"inactive: can't remove: vw=%s vob=%s dbid=0x%x nm=%s",
				mfs_vp2vw(dvp),
				mfs_vp2dev(dvp),
				mfs_vp2dbid(dvp),
				rmv_name);
		}

		/* Don't propagate attempted remove error after logging
		 * it.
		 */

		error = 0;
	
		/* Release resources for delayed remove.  Note
	   	that I don't care a hoot about whether there was
	   	an error on the remove or not .. I did the best
           	I could. This could leave stranded files in error
	   	conditions (like network failures) */

		VN_RELE(dvp);
		KMEM_FREE(rmv_name, STRLEN(rmv_name)+1);
		MDKI_CRFREE(MVFS_CD2CRED(tmp_cd));
                MVFS_FREE_SUBSTITUTE_CRED(tmp_cd);
    	    }

    	    /* 
	     * Sync the view with the cleartext file. 
             * Do this only if not vhand running.  If the
	     * pagedaemon is running, then do not
	     * attempt this as the cleartext 'getattr' will
	     * first attempt to flush all cleartext pages to the
	     * home node and may cause deadlocks.
             */

	    if (sleep_flag == MFS_INACTIVE_SLEEP) 
			mvfs_sync_attr(mnp, NULL, MFS_USE_NULLBH, 0, cd);

    	    /* Debug check that no delwri pages before we clear
       	       the attributes needed by mfs_strategy or mfs_putpage calls.
               FIXME: not MP safe!  */

    	    MDB_NODELWRI(vp);

    	    /* Clear attribute credentials regardless */

	    MCLRCRED(mnp);

	    /* 
             * Finally, if there was a RW error on the cleartext
	     * purge it now.  NFS saves the last IO error, and will
	     * continue to return it until its vnode is inactivated.
	     * This is bad if we hold on to the vnode, and NFS
	     * returns a bogus saved error on a completely different
	     * user open.
	     */

		(void) mfs_clear_rele(vp, MVFS_CD2CRED(cd));

	    error = 0;	
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    MDB_VLOG((MFS_VINACTIVE,"vp=%"KS_FMT_PTR_T" err=%d mnp=%"KS_FMT_PTR_T" cnt = %d\n",vp,error,mnp,
              V_COUNT(vp)));
    BUMPSTAT(mfs_vnopcnt[MFS_VINACTIVE]);
    return(error);
}

/* ARGSUSED */
int
mfs_lookup(
    VNODE_T *advp,
    char *nm,
    VNODE_T **vpp,
    struct pathname *pnp,
    int flags,
    ROOTDIR_T *rdir,
    CALL_DATA_T *cd
)
{
    return mvfs_lookup_ctx(advp, nm, vpp, pnp, flags, rdir, cd, NULL);
}

int
mvfs_lookup_ctx(
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
    VNODE_T *dvp = advp;
    int error;
    CLR_VNODE_T *cvp;
    VNODE_T *xvp;		/* Extra vnode ptr in some paths */
    CLR_VNODE_T *xcvp;
    tbs_dbid_t rootdbid;
    VNODE_T *ucdir;
    VNODE_T *rebound_cdir;
    VNODE_T *vw;
    mvfs_thread_t *mth;
    char *cc_comp_buf = NULL;
    int fromcache = 0;

    /* Only do lookup in a directory */

    if (!MVFS_ISVTYPE(dvp, VDIR)) return (ENOTDIR);
    if (nm == NULL || nm[0] == '\0') {
#ifdef MVFS_NULL_NAME_ALLOWED
	mth = MVFS_GET_THREAD(cd);
	VN_HOLD(dvp);
	*vpp = dvp;
	error = 0;
	goto do_audit;
#else
	mvfs_log(MFS_LOG_ERR, "mfs_lookup: null name\n");
	return (EINVAL);
#endif
    }

    mth = MVFS_GET_THREAD(cd);
    /* Clean out.  Some error paths don't set vpp and we
       want to be sure it is NULL on any error */

    *vpp = NULL;

    /* Switch based on class of object. */

    ASSERT(VTOM(dvp)->mn_hdr.vp);
    switch (VTOM(dvp)->mn_hdr.mclass) {
	case MFS_NTVWCLAS:
            /* We don't pass rdir since it isn't used */
	    error = mvfs_ntvw_lookup(advp, nm, vpp, pnp, flags, NULL,
                    MVFS_CD2CRED(cd));
	    break;
	case MFS_VIEWCLAS:
	    if (MVFS_THREAD_LOOKUP_ROOT(mth)) {
	        /*
	         * if there has been a setview, any lookup through root
	         * comes here.  If the lookup is trying to go through
	         * the real root, switch it now.
	         */
	    	error = MVFS_ROOT_LOOKUP(MVFS_THREAD_LOOKUP_ROOT(mth), 
	    				 nm, vpp, pnp, flags, MVFS_CD2CRED(cd),
                                         ctxp);
	        goto noauditout;	/* these are never auditted */
	    }
	    if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp),nm, ".") == 0) {    /* Handle "." */
		*vpp = dvp;
		VN_HOLD(*vpp);
		error = 0;
		break;
	    } else if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "..") == 0) {  /* Handle ".." */
                /*
                 * dotdot from a view gets to the viewroot, assuming
                 * the view is not stale.
                 */
                MLOCK(VTOM(dvp));
	        if (VTOM(dvp)->mn_view.id == MFS_NULLVID) {
		    error = ESTALE;	/* Stale view */
                    MUNLOCK(VTOM(dvp));
		    break;
		}
                MUNLOCK(VTOM(dvp));
		*vpp = mfs_getviewroot(); /* includes a HOLD */
                if (*vpp == NULL)
                    error = ESTALE;
                else
                    error = 0;
		break;
	    /* 
             * VERY FUNKY.  Lookup "SETVIEW^" leaves you at "." in
	     * the viewtag, but sets your current proc view
	     */
	    } else if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "SETVIEW^") == 0) {
		VN_HOLD(dvp);		/* A hold for the setview call */
		error = MVFS_SET_PROCVIEW(dvp, NULL);	/* Set the view */
		if (!error) {
		    *vpp = dvp;			/* Return current dir */
		    VN_HOLD(*vpp);		/* One hold for the return vp */
		}
		break;
	    } else if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "VIEWPVT^") == 0) {
		/* 
		 * This name sets the "always cover" bit in the
		 * view-tag.  In views with this bit set, all lookups
		 * make loopback nodes to cover any non-atria dirs.
		 */
		*vpp = dvp;		/* Return view-tag */
		VN_HOLD(*vpp);		/* One hold for the return vp */
		VTOM(dvp)->mn_view.always_cover = 1;	/* Always cover */
		error = 0;
		break;
	    } else if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "VIEWSHR^") == 0) {
		/* Undo view-private set above */
		*vpp = dvp;
		VN_HOLD(*vpp);
		VTOM(dvp)->mn_view.always_cover = 0;
		error = 0;
		break;
	    }
	    /* Fall through */
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    ASSERT(MFS_REALVP(dvp));

	    if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "ROOTDIR^") == 0) {	    /* Escape for real root */
		*vpp = ROOTDIR;
		VN_HOLD(*vpp);
                MVFS_SAVE_ROOTDIR(ctxp);
		error = 0;
		break;
	    }
	    error = LOOKUP_COMPONENT(MFS_CLRVP(dvp), nm, &cvp, pnp, rdir,
                                     MVFS_CD2CRED(cd), ctxp);
            if (!error) {
                error = mfs_makeloopnode(MFS_VIEW(dvp), cvp, vpp,
                                         MVFS_CD2CRED(cd));
                CVN_RELE(cvp);       /* Release lookup refcnt */
            } else {
                *vpp = NULL;
            }
	    break;
	case MFS_VIEWDIRCLAS:
	    /* 
	     * Another funky name.  As a 'view-tag' this name indicates
	     * the view of the current working directory (if there
	     * is one), or else the setview (if there is one) or
	     * else it is ignored (and will return name not found).
	     */
	    if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "CWDVW^") == 0) {
                VNODE_T *cdir = MDKI_GET_U_CDIR();
                /* see comments about MVFS_NULL_U_CDIR_ERR in mvfs_mioctl.c */
                if (cdir == NULL) {
                    error = MVFS_NULL_U_CDIR_ERR;
                    break;
                }
		*vpp = mfs_getview(cdir, MVFS_CD2CRED(cd), TRUE /* HOLD */);
                MDKI_VNRELE_RCDIR(cdir);
		if (*vpp != NULL) {	/* Vnode returned held */
		    error = 0;
		    break;
		}
	    }
	    /* Following is a variant of the above, but only gets the "setview" state */
	    if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "CURVIEW^") == 0) {
		*vpp = mfs_getview(NULL, MVFS_CD2CRED(cd), TRUE /* HOLD */);
		if (*vpp != NULL) {
		    error = 0;
		    break;
		}
	    }
	    if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "CLRVIEW^") == 0) {
		error = MVFS_SET_PROCVIEW((VNODE_T *)NULL, NULL); /* clear view */
		if (!error) {
		    *vpp = dvp;			/* Return current dir */
		    VN_HOLD(*vpp);		/* One hold for the return vp */
		}
		break;
	    }
	    error = mfs_viewdirlookup(dvp, nm, vpp, MVFS_CD2CRED(cd),
                                      pnp, flags);
	    break;
	case MFS_VOBRTCLAS: {

	    /* NOTES on lookup of ".." at a vob root:
	       The algorithm for traversing up a mount point is
	       funny at the VOB root because there is more than
	       the 1 root floating around.  The algorithm for traversing
	       up is to check if the dir object is a ROOT (via VROOT flag),
	       and the name is "..".  If it is, then the lookuppn code
	       will link back to the vnode covered by the v_vfsp of
	       this object and lookup ".." in that object/FS.  To make
	       matters worse, FSS systems make this check AFTER the
	       (M)VOP_LOOKUP call, while real vnode systems make this
	       check before the (M)VOP_LOOKUP call. */

	    /* Handle ".." at the root.  There are three cases:
		1) Synonym for the root in an NT-style view.
		   Just return the view vp as the vnode for ..
		2) Synonym for root in a "loopback"-style view: use recursion to
		   lookup the real ".." (traversing up the mount point)
		   and make an appropriate loopback vnode so we don't
		   lose the view.
		3) The "real unbound root" (e.g. NULL view and pointed
		   to by the mount point.  Just return ourselves.
		   This would happen anyways if we did nothing, but
		   this short cut saves us from possible stack overflow
		   due to the recursion from (2) above.
	     */

	    if (mfs_hmcmp(nm, "..") == 0) {
		if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "..") != 0) {	/* No HM ".." at root */
		    *vpp = NULL;
		    error = ENOENT;
		    break;
		}

		/* Not "mount point" (no view) root. Make view-cover vnode for ".." */

		vw = MFS_VIEW(dvp);	/* dvp holds vw, so no need for local hold */
		if (vw != NULL) {
		    /* 
		     * Check for case (1) - an NT style view, and just
		     * return the view-tag as ".." if so.
		     */
		    if (MFS_ISNTVIEW(VTOM(vw))) {
			*vpp = vw;
			VN_HOLD(*vpp);
			error = 0;
			break;
		    }

		    /* 
		     * Must be case (2) - a loopback style view cover of
		     * a vob root.  Lookup ".." through the mountpoint and
		     * make a loopback vnode over that.
		     */
		    ASSERT(MFS_ISLOOPVIEW(VTOM(vw)));

		    error = MVFS_ROOT(dvp->v_vfsp, &xvp);
                    if (!error) {
                        ASSERT(VTOM(xvp)->mn_hdr.viewvp == NULL);
                        MVFS_VP_TO_CVP(xvp, &xcvp);
                        error = LOOKUP_COMPONENT(xcvp, nm, &cvp, pnp, NULL, 
                                                 MVFS_CD2CRED(cd), ctxp);
                        CVN_RELE(xcvp);
                        VN_RELE(xvp);
                    }
                    
                    if (!error) {
                        error = mfs_makeloopnode(vw, cvp, vpp,
                                                 MVFS_CD2CRED(cd));
                        CVN_RELE(cvp);       /* Release lookup refcnt */
                    } else {
                        *vpp = NULL;
                    }
		    break;
		} else {
		    /* 
		     * Naked VOB root without a view.
		     * If the setview is a LOOPVIEW, then
		     * we just return the vnode itself and the higher
		     * layers of Unix pathname lookup will chase up
		     * this mount point.  If the setview is an
		     * NTVIEW, then we have to return that view-tag
		     * as the result of the ".." lookup
		     */

		    if ((*vpp = mfs_getview(NULL, MVFS_CD2CRED(cd), 
                                            TRUE /* HOLD */)) != NULL) {
		        if (MFS_ISNTVIEW(VTOM(*vpp))) {
			    /* Hold already done by getview call */
			    error = 0;
			    break;
			} else {
			    /* Release getview refcount on loopback view */
			    VN_RELE(*vpp);
			    *vpp = NULL;
			}
		    }

		    *vpp = dvp;
		    VN_HOLD(*vpp);
		    error = 0;
		    break;
		}
	    }

	    /* Normal case - not ".." at a root of a vob. */

	    dvp = mfs_bindroot(dvp, cd, &error);
	    if (error) {
		/*
                 * No view: return same vnode for .  (.. done above), 
	         * or ENOENT for any other name (including .@@)
                 */
		if (error == ESRCH) {
		    if (nm[0] == '.' && nm[1] == '\0') {
			*vpp = dvp;
			VN_HOLD(*vpp);
			error = 0;
		    } else {
			error = ENOENT;
		    }
		}
		break;
	    }
	    /* Fall through */
	}
	case MFS_VOBCLAS: {

	    /* Find out if we have permission to be doing this.  Have
             * to check before going to cache because the dir may
             * have been chmod'ed.
             */
            error = mfs_accessv(dvp, VEXEC, 0, cd);
	    if (error) break;

    	    /* 
	     * Before checking name cache, validate caches. 
	     * Note that detecting stale name cache entries is
             * depends on the attribute cache timeout.
	     */

	    if (mfs_ac_timedout(VTOM(dvp), FALSE, cd)) {
                error = mfs_getattr(dvp, NULL, 0, cd);
		if (error) break;
		BUMPSTAT(mfs_acstat.ac_misses);
		BUMPVSTAT(dvp,acstat.ac_misses);
	    }
	    (void) mfs_rebind_vpp((advp != dvp), &dvp, cd);

	    /* Note: dnclookup may MLOCK dvp in some paths! */
    	    *vpp = mfs_dnclookup(dvp, nm, pnp, cd);
    	    if (*vpp == NULL) {
                if (MVFS_PN_CI_LOOKUP(pnp)) {
                    error = EOPNOTSUPP;
                    goto noauditout;
                }
		error = mfs_clnt_lookup(dvp, nm, vpp, cd);
	    } else {
		fromcache = 1;
	  	if (*vpp == MFS_DNC_ENOENTVP) {
		    *vpp = NULL;
		    error = ENOENT;
		    mvfs_log(MFS_LOG_ENOENT,
		        "lookup cached ENOENT: vw=%s vob=%s dbid=0x%x nm=%s\n",
			mfs_vp2vw(dvp), mfs_vp2dev(dvp), 
			mfs_vp2dbid(dvp), nm);
		}
	    }

	    /* 
             * If got a successful (non-cached) lookup, then the
	     * result must be valid for itself.  Update the rebind
 	     * cache for the target object to indicate this.
	     */

	    if (*vpp && !fromcache) {
		mfs_rebind_self(*vpp, cd);
	    }

            /* 
	     * Check if we looked up ".." and found a vob root synonym.
             * The nm[0] check is a fast way to avoid doing a strcmp on
             * most name lookups.
             *
             * For non-history-mode lookups, root synonyms are as follows:
             * dvp/.. -> root version (edbid = root edbid, vdbid = *) (lookup)
             * root version is a synonym -> return pseudo-root.
             *
	     * For history-mode it is a lot more complex:
             * dvp/.. -> root version (edbid = root edbid, vdbid = *) (lookup)
             * NOT AT THE ROOT YET, return this vnode
             * root version/.. -> root branch (edbid=vdbid= branch dbid)
             * root branch/..  -> root element (edbid=vdbid= root dbid)
             * THIS IS THE ROOT SYNONYM we want to skip to the pseudo-root for
   	     */

	    if (!error &&  (mfs_hmcmp(nm, ".") || mfs_hmcmp(nm, ".."))) {
		rootdbid = V_TO_MMI(dvp)->mmi_root_edbid;

		if (MFS_ISROOTSYNONYM(*vpp, rootdbid)) {
		    xvp = *vpp;		/* Save for audit */

		    /* 
                     * Make loopback node (if needed) to not lose view
		     * extended naming at the VOB root. Use view of 
                     * bound root vnode (could have warped from parent dir!)
		     */

                    MVFS_VP_TO_CVP(VFS_TO_MMI(xvp->v_vfsp)->mmi_rootvp, &cvp);
		    error = mfs_makeloopnode(MFS_VIEW(xvp), cvp, vpp,
                                             MVFS_CD2CRED(cd));
                    CVN_RELE(cvp);

                    /*
                    ** Check if someone ended the view while we are in it.  This
                    ** is a lookup of ".." at the vob root, so return ESTALE,
                    ** just like we do below if we call mfs_bindroot() (which
                    ** calls mvfs_rvclookup(), which returns ESTALE, in this
                    ** case).
                    ** Copy some code from mfs_bindroot()
                    ** and mvfs_rvclookup() to check our situation..  There is
                    ** similar code in mfs_root() to catch people trying to
                    ** sneak into the stale view with an absolute pathname.
                    */
                    if (!error) {
                        VNODE_T *vw;

                        if ((vw = mfs_getview(*vpp, MVFS_CD2CRED(cd),
                                              FALSE /* NO HOLD */)) != NULL) {
                            if (VTOM(vw)->mn_view.id == MFS_NULLVID) {
                                error = ESTALE;
                                VN_RELE(*vpp);	/* mfs_makeloopnode() did a VN_HOLD */
                                *vpp = NULL;
                            }
                        }
                    }
		    /* 
		     * Make sure bound root is in audit for both
		     * parent and looked-up vnode. 
                     */

		    if (!error) {
	                MFS_AUDIT(MFS_AR_LOOKUP,dvp,
                            PN_GET_CASE_CORRECT_COMP(pnp, nm, &cc_comp_buf), 
                            NULL,NULL,xvp,cd);
			if (cc_comp_buf != NULL)
			    STRFREE(cc_comp_buf);
                    }

		    /* Done with the "looked up" bound root.  Release */

		    VN_RELE(xvp);
		    goto noauditout;
		}
	    }
	    /* 
             * Check with (possible) mdep routine for special file 
	     * conditions (such as file in process of being deleted) 
	     * that mean we should skip auditing this file.
             */
	    if ((*vpp) && MVFS_SKIP_AUDIT(*vpp)) { 
    		MDB_VLOG((MFS_VLOOKUP,"mfs_lookup:  skipping audit of vp 0x%"KS_FMT_PTR_T"\n", 
	    		*vpp));
		goto noauditout;
	    }
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    /* Do audit */

do_audit:
    if (!error) {
	MFS_AUDIT(MFS_AR_LOOKUP, dvp, 
                  PN_GET_CASE_CORRECT_COMP(pnp, nm, &cc_comp_buf),
		  NULL, NULL, *vpp, cd);
	if (cc_comp_buf != NULL)
	    STRFREE(cc_comp_buf);
    }

noauditout:

    if (dvp != advp) VN_RELE(dvp);  /* Release if allocated bound root vnode */

    /* 
     * *vpp may not always be a mvfs vnode, see ".." and ROOTDIR cases above 
     * and mfs_makeloopnode(). So check for ISMFS first before doing VTOM 
     * in the following.
     */
    MDB_VLOG((MFS_VLOOKUP,"vp=%"KS_FMT_PTR_T" nm=%s, rvp=%"KS_FMT_PTR_T", rmnp=%"KS_FMT_PTR_T", hit=%d, err=%d\n",dvp,nm,*vpp,(*vpp ? (MFS_VPISMFS(*vpp)?VTOM(*vpp):0) : 0), fromcache, error));

    BUMPSTAT(mfs_vnopcnt[MFS_VLOOKUP]);
    MVFS_EXIT_FS(mth);
    ASSERT(error == 0 || *vpp == NULL);
    return (error);
}

/*
 * MFS_CREATE - create an object
 *
 * For regular files, the MFS must also create the cleartext
 * file so that it will get the correct owner etc.  This way
 * the VIEW server does not have to run as root to set an
 * arbitrary user as owner.
 *
 * This call is implemented to obey POSIX semantics for all
 * open calls with the O_CREAT flag set.  The creat() call to
 * the system looks like an open( , O_CREAT|O_TRUNC|O_WRONLY, mode) call.
 * The argument usage of the initial attributes is unusual and this
 * operation expects the following:
 *    va->va_type is the "type" of object to creat (dir, file etc.)
 *		It is always set for all create calls.
 *    va->va_mode is the "mode" argument from the open(O_CREAT) or creat()
 *		call.  This should be used in preference to the "mode"
 *		argument in the mfs_create() call signature because this
 *		argument is not reliably set.  This value is also set for
 *		all creates.
 *    va->va_size is set to 0 if the O_TRUNC flag is set.
 *    excl      represents the existence or not of O_EXCL flag.
 */

/*ARGSUSED*/
int
mfs_create(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,
    VNODE_T **vpp,
    CALL_DATA_T *cd
)
{
    return(mvfs_create_subr_with_cvp(advp, nm, vap, excl, mode, vpp, cd, 0,
                                     NULL, NULL));
}

int
mvfs_create_subr(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,       /* Don't use, use vap->va_mode instead */
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    int flag
)
{
    return(mvfs_create_subr_with_cvp(advp, nm, vap, excl, mode, vpp, cd, flag,
                                     NULL, NULL));
}

int
mvfs_create_subr_ctx(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,       /* Don't use, use vap->va_mode instead */
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    MVFS_CREATE_CTX_T *ctxp
)
{
    return(mvfs_create_subr_with_cvp(advp, nm, vap, excl, mode, vpp, cd, 0,
                                     NULL, ctxp));
}

/* Subroutine that does the real work.  This routine returns the cleartext
** vnode pointer if it creates the cleartext.  The pointer is also stored
** in the mnode structure (as realvp).
*/
int
mvfs_create_subr_with_cvp(
    VNODE_T *advp,
    char *nm,
    VATTR_T *vap,
    VCEXCL_T excl,
    int mode,                   /* Don't use, use vap->va_mode instead */
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    int flag,
    CLR_VNODE_T **cvpp, 
    MVFS_CREATE_CTX_T *ctxp                           /* context, used by Linux */
)
{
    VNODE_T *dvp = advp;
    CLR_VNODE_T *loopvp = NULL;
    int error;
    mvfs_thread_t *mth;
    int xerr;
    u_long mask;
    int file_created = 0;

    if ((dvp->v_vfsp->vfs_flag & VFS_RDONLY) != 0)
        return EROFS;

    ASSERT(VTOM(dvp)->mn_hdr.vp);

    /* Initialize return args. */
    *vpp = NULL;
    if (cvpp != NULL) {
        *cvpp = NULL;
    }

    if (!MVFS_ISVTYPE(dvp, VDIR)) return(ENOTDIR);

    if (nm == NULL || nm[0] == '\0') {
#ifdef MVFS_NULL_NAME_ALLOWED
        if (VATTR_GET_TYPE(vap) == VDIR) {
#else
	/* Many file systems treat this as a reference to ".", as 
	 * long as the create is not exclusive. */
        if (VATTR_GET_TYPE(vap) == VDIR && !MDKI_ISVCEXCL(excl)) {
#endif
            mth = MVFS_GET_THREAD(cd);
            /* reference this directory & return it. */
            VN_HOLD(dvp);
            *vpp = dvp;
            error = 0;
            goto null_done;
        }
	mvfs_log(MFS_LOG_ERR, "mfs_create: null name\n");
	return(EEXIST);
    }

    mth = MVFS_GET_THREAD(cd);

    mask = VATTR_GET_MASK(vap);

    /* Switch op based on class */
    switch (VTOM(dvp)->mn_hdr.mclass) {
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    MFS_CHKSP(STK_VOPCREATE);
	    error = MVOP_CREATE(MFS_REALVP(dvp), nm, vap, excl, mode, &loopvp,
                                cd, flag, ctxp);
	    if (!error) {
		error = mfs_makeloopnode(MFS_VIEW(dvp), loopvp, vpp,
                                         MVFS_CD2CRED(cd));
	    	CVN_RELE(loopvp);
	    } else *vpp = NULL;
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
            /*
             * Pass in null pathname, rdir, and zero flags, as do other
             * "rogue" (non-VOP) callers of mfs_lookup().
             */
            if ((error = mfs_lookup(dvp, nm, vpp, (struct pathname *)NULL,
                                    0, (ROOTDIR_T *)NULL, cd)) == 0)
            {
                /*
                 * We emulated a lookup instead.
                 * if EXCL, or not a directory, convert to EEXIST, otherwise
                 * it's a "stupid but valid" combination opening for read
                 * and create (but not exclusive create) of an existing
                 * directory.
                 */
                if (MDKI_ISVCEXCL(excl) || VATTR_GET_TYPE(vap) != VDIR) {
                    VN_RELE(*vpp);
                    *vpp = NULL;
                    error = EEXIST;
                }
                /*
                 * fall through with error == 0 and *vpp filled in by
                 * call to mfs_lookup()
                 */
                /* XXX can we get here if VCHR, VBLK, VFIFO? */
            } else
	    	error = EROFS;
	    break;
	case MFS_VOBRTCLAS:
	    dvp = mfs_bindroot(dvp, cd, &error);
	    if (error == ESRCH) error = EROFS;   /* Null view to RO FS */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {
	    if (VATTR_GET_TYPE(vap) == VCHR ||
		 VATTR_GET_TYPE(vap) == VBLK || VATTR_GET_TYPE(vap) == VFIFO) {
	    	error = ENXIO;
	    	*vpp = NULL;
	        break;
	    }

	    (void) mfs_rebind_vpp((advp != dvp), &dvp, cd);


          retry_lookup:
	    error = mfs_lookup(dvp, nm, vpp, (struct pathname *)0, 0,
                              (ROOTDIR_T *)0, cd);
	    if (!error) {
                /* The lookup succeeded so we didn't create our file or the
                ** cleartext, so the file_created and cvp initial values (0)
                ** are correct.
                */
		if (MDKI_ISVCEXCL(excl)) {
		    VN_RELE(*vpp);
		    *vpp = NULL;
		    error = EEXIST;
		    break;
		} else {
		    /* Check access for open mode on existing file */
		    error = mfs_accessv(*vpp, mode, 0, cd);
		    /* Overwrite existing - check for O_TRUNC */
		    if (!error && (mask & AT_SIZE) != 0 && 
			VATTR_GET_SIZE(vap) == 0) {

                        /*
                         * Check to see if this is a 32 bit create call
                         * on an existing large file. 
                         * If it is, we can't truncate, we must set 
                         * an error.
                         */

                        if (!MVFS_LFS_OPENMODE(flag)) { 
                            VATTR_T va;

                            VATTR_NULL(&va);
                            VATTR_SET_MASK(&va, AT_SIZE);

                            error = mfs_getattr(*vpp, &va, 0, cd);
                            if ((!error) && 
                                (VATTR_GET_SIZE(&va) > MVFS_MAXOFF_32_T))
                            {
                                error = EOVERFLOW;
                            }
                            MVFS_FREE_VATTR_FIELDS(&va); 
                        }
     
			/* Trash existing values in va struct since
			   only a truncate is required and we don't
			   want to get errors by having invalid fields
			   set for the setattr.  The type/mode values
                           are no longer needed and must not be set
	                   on the setattr call, and stack space in
			   many implementations is too scarce to have
			   a separate vattr struct in the stack here.
 			 */
                        if (!error) {
			    VATTR_NULL(vap);
			    VATTR_SET_SIZE(vap, 0);
			    VATTR_SET_MASK(vap, AT_SIZE);
		            error = mvfs_changeattr(*vpp, vap, 0, cd, ctxp);
                       }
		    }
		    if (!error)
			/* need to fill in *vap with current stats */
			error = mfs_getattr(*vpp, vap, 0, cd);
		    if (error) {	/* Release vnode on error */
                        MVFS_FREE_VATTR_FIELDS(vap); /* and nt sids if copied */
			VN_RELE(*vpp);
			*vpp = NULL;
		    }
		    break;
	        }
  	    }

            /* Our file doesn't exist, so get the view server to do what's
            ** necessary, including generating a cleartext pathname.
            */
	    error = mfs_clnt_create(dvp, nm, vap, vpp, cd);

            if (error == EEXIST) {
                /*
                 * In a race, another process could create the file after our 
                 * lookup, and causes mfs_clnt_create to return EEXIST. So, we
                 * lookup again, and do access checking.
                 */
                goto retry_lookup;
            }
            
	    if (error == EINTR) {
		/* 
	         * Don't know if view got rqst or not - try to at least
	         * send a remove.  Otherwise the user is left fractured
	         * on quits with a view version of the file, and
	         * no cleartext (open will fail, truncate will fail etc.)
	 	 */
		xerr = mfs_clnt_remove(dvp, nm, MFS_USE_NULLBH, TRUE, cd);
		if (xerr && xerr != ENOENT) {		 
		    mvfs_logperr(MFS_LOG_ERR, xerr, 
		        "create: failed rmv after cleartext error: vw=%s vob=%s dbid=0x%x nm=%s", 
			    mfs_vp2vw(dvp), mfs_vp2dev(dvp), 
			    mfs_vp2dbid(dvp), nm);
		}
		MFS_ATTRINVAL(dvp);
	    }
	    if (!error) {
                /* We created our file and the cleartext path without error. */
                file_created = 1;

	        MLOCK(VTOM(*vpp));
		/* Set sync_mtime to force update of mtime in
		 * view to reflect the mtime of the underlying
	 	 * cleartext.
		 */
		VTOM(*vpp)->mn_vob.sync_mtime = 1;
	        /*
	         * Set object already choided under audit to avoid
	         * spurious choid right after create.  Create assigns
  		 * a unique oid for the object.
		 */
		MFS_REMEMBER_CHOID_BH(mth, VTOM(*vpp));
		/* 
		 * Cleartext pname was returned/set by the clnt_create
	         * so it is guaranteed to be right.
                 * If cvpp is non-NULL
                 * the cleartext vp is returned held (under the mnode lock)
                 * so even if someone else races us and changes the realvp
                 * (cleartext vp) in the mnode, this cleartext vp will be
                 * valid for our caller.
		 */
	        error = mfs_clear_create(*vpp, vap, cvpp, cd, flag); 

	        MUNLOCK(VTOM(*vpp));
		if (error) {
		    /* Cleartext create logged the error - cleanup */
		    xerr = mfs_clnt_remove(dvp, nm, MFS_USE_NULLBH, TRUE, cd);
 		    if (xerr && xerr != ENOENT) {
			mvfs_logperr(MFS_LOG_ERR, xerr, 
			    "create: failed rmv after cleartext error: vw=%s vob=%s dbid=0x%x nm=%s", 
				mfs_vp2vw(dvp), mfs_vp2dev(dvp), 
				mfs_vp2dbid(dvp), nm);
		    }
		    MFS_ATTRINVAL(dvp);
                    if (cvpp != NULL && *cvpp != NULL) {
                        /* Get rid of the cleartext vnode we created above. */
                        CVN_RELE(*cvpp);
                        *cvpp = NULL;
                    }
		    VN_RELE(*vpp);   /* Dump new vnode */
		    *vpp = NULL;     /* Null out worthless vnode */
		}
  	    }
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    /* Do audit if no error and we actually created a file.
     * No create audit record on "re-opened" files that existed
     * previously.
     */
    if (!error && file_created) {
	MFS_AUDIT(MFS_AR_CREATE, dvp, nm, NULL, NULL, *vpp, cd);
    }

    if (dvp != advp) VN_RELE(dvp);   /* Release if alloc bound root vnode */

  null_done:
    /*
     * *vpp may not always be a mvfs vnode (see mfs_makeloopnode()), so
     * check for ISMFS first before making the VTOM call 
     */
    MDB_VLOG((MFS_VCREATE,"vp=%"KS_FMT_PTR_T" nm=%s, rvp=%"KS_FMT_PTR_T", rmnp=%"KS_FMT_PTR_T", err=%d\n",dvp,nm,*vpp,(*vpp ? (MFS_VPISMFS(*vpp)?VTOM(*vpp):0) : 0), error));
    BUMPSTAT(mfs_vnopcnt[MFS_VCREATE]);
    MVFS_EXIT_FS(mth);
    return (error);
}

/*
 * MFS_REMOVE - remove a name.
 *
 * To support the Unix semantic of remove while open, we have
 * to do some funny things here.  If the user attempts to remove
 * the file while it is open (refcnt > 1) then we actually rename
 * it here and save the renamed file info for a delayed
 * remove operation when the file is no longer being referenced
 * (in the mfs_inactive call.)
 *
 * The View will actually remove the cleartext if its link count
 * from the view directories goes to zero.  It can always do
 * this since it has rights to the directory in which view storage
 * is kept.
 */
int
mfs_remove(
    VNODE_T *advp,
    char *nm,
    CALL_DATA_T *cd
)
{
    return(mvfs_remove_ctx(advp, NULL, nm, cd, NULL));
}

/*
 * Rename in-use files that are targets of unlink or rename.
 * When all platforms have been converted to use call_data blocks,
 * the calling sequence of this function can be changed to remove the
 * pointer to the mvfs_thread_t in the calling sequence and getting mth 
 * from the call_data block.
 */
STATIC int
mvfs_silly_rename(
    VNODE_T *dvp,
    VNODE_T *vp,                        /* target */
    char *nm,
    mvfs_thread_t *mth,
    CALL_DATA_T *cd
)
{
    char *tnm;
    int error;
    mfs_mnode_t *mnp = VTOM(vp);

    ASSERT(mnp->mn_vob.rmv_dvp == NULL);
    ASSERT(mnp->mn_vob.rmv_cred == NULL);
    ASSERT(mnp->mn_vob.rmv_name == NULL);
    ASSERT(MISLOCKED(mnp));

    tnm = mfs_uniq_name();
    if (tnm == NULL) {
        return ENOMEM;
    }

    /* 
     * Must unlock around dir ops or can get deadlock
     * from locking child first, then parent.
     */
		 
    MUNLOCK(mnp);
    MFS_INHAUDIT(mth);
    error = mvfs_rename_ctx(dvp, nm, dvp, tnm, cd, NULL);
    mvfs_log(MFS_LOG_DEBUG, "silly-renaming %s to %s: %d\n", nm, tnm, error);
    MFS_ENBAUDIT(mth);
    MLOCK(mnp);
    /* 
     * Since we unlocked, there may be a race.
     * In races, only 1 rename should be "successful".
     * But since it is possible that 2 are successful
     * (if a third party snuck in and recreated the old
     * name after the first rename, but before the second)..
     * check for this possibility to avoid refcounted
     * vnodes and storage leaks.  Only accept
     * the "first" successful rename.
     */
    if (error != 0 || mnp->mn_vob.rmv_name != NULL) {
        KMEM_FREE(tnm, STRLEN(tnm)+1);
        if (error == ENOENT)
            /* suppress ENOENT error in case of a race to rename target */
            error = 0;
    } else {
        mnp->mn_vob.rmv_name = tnm;
        VN_HOLD(dvp);	/* Bump refnct for ptr we are about to save */
        mnp->mn_vob.rmv_dvp = dvp;
        mnp->mn_vob.rmv_cred = MDKI_CRDUP(MVFS_CD2CRED(cd));
    }
    return error;
}

int 
mvfs_remove_ctx(
    VNODE_T *advp,
    VNODE_T *avp,
    char *nm,
    CALL_DATA_T *cd,
    MVFS_REMOVE_CTX_T *ctxp
)
{
    VNODE_T *dvp = advp;
    VNODE_T *vp = avp;
    mvfs_thread_t *mth;
    struct mfs_auditrmstat *rmstatp = NULL;
    int error;
    mfs_mnode_t *mnp;

    if ((dvp->v_vfsp->vfs_flag & VFS_RDONLY) != 0)
        return EROFS;

    ASSERT(VTOM(dvp)->mn_hdr.vp);

    if (!MVFS_ISVTYPE(dvp, VDIR)) return(ENOTDIR);
    if (nm == NULL || nm[0] == '\0') {
	mvfs_log(MFS_LOG_ERR, "mfs_remove: null name\n");
	return (EINVAL);
    }

    mth = MVFS_GET_THREAD(cd);
    /* Switch based on class of object */

    switch (VTOM(dvp)->mn_hdr.mclass) {
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    error = MVOP_REMOVE(MFS_REALVP(dvp), vp, nm, cd, ctxp);
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EROFS;
	    break;
	case MFS_VOBRTCLAS:
	    dvp = mfs_bindroot(dvp, cd, &error);
	    if (error == ESRCH) error = EROFS;   /* Null view to ROFS */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {

	    (void) mfs_rebind_vpp((advp != dvp), &dvp, cd);

            /* Lookup the name to get its vnode so we can
             * check on the refcnt.
             */

	    if (vp == NULL) {
		error = mfs_lookup(dvp, nm, &vp,
				   (struct pathname *)0, 0,
				   (ROOTDIR_T *)0, cd);
		if (error) break;
	    }
	    ASSERT(MFS_ISVOB(VTOM(vp)));

	    /* Don't allow remove to remove directories */
	    if (MVFS_ISVTYPE(vp, VDIR)) {
		error = EPERM;
		if (avp == NULL)
		    VN_RELE(vp);
		break;
	    }

	    /*
	     * Must sync modified pages.  After a real remove there may
             * be nowhere for them to go, and we don't want spurious
             * ESTALE for modified pages that can't be written out.
             * This also shouldn't be done with the MLOCK held which is
             * why it is done out here before we get the mnode lock.
	     * Also, invalidate cached pages from the page cache -- they
	     * may hold vnode references which would force us to rename
	     * instead of removing.
             */

            mnp = VTOM(vp);	 /* Info on object, not dir */
	    (void) PVN_FLUSH(vp, MFS_PVN_FLUSH|MFS_PVN_INVAL, MVFS_CD2CRED(cd));

    	    MLOCK(mnp);	 /* Lock to avoid races on rmv state */

	    /*
	     * Note that this sync must be done with the mnode locked,
	     * to avoid races with other inactivators.  The alternative
	     * would be to grab an extra reference to the realvp, but
	     * just holding the mnode locked is cheaper.
	     */
	    if (mnp->mn_hdr.realvp) {
		(void) PVN_FLUSH(MVFS_CVP_TO_VP(mnp->mn_hdr.realvp),
                                 MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
	    }

#ifdef NFSV4_SHADOW_VNODE
            if (mnp->mn_hdr.realvp_master) {
                (void) PVN_FLUSH(MVFS_CVP_TO_VP(mnp->mn_hdr.realvp_master),
                                 MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
            }
#endif

	    /*
	     * Fetch the audit information and release the
	     * vnode before we actually remove it.  This solves
             * two problems:
             *   (1) Avoids trying to get info (stat) an object that
             *       no longer exists in mfs_audit()
             *   (2) Avoids holding a refcnt on a vnode whose
             *       DBID (inode number in FSS) may get reused and
             *       cause a "stale active vnode" message and associated
             *	     incorrect results.
             */

	    rmstatp = (struct mfs_auditrmstat *)
				KMEM_ALLOC(sizeof(mfs_auditrmstat_t), KM_SLEEP);
	    if (rmstatp == NULL) {
	        error = ENOMEM;
		MUNLOCK(mnp);
		if (avp == NULL)
		    VN_RELE(vp);
	        break;
	    }

            mfs_init_rmstat(vp, rmstatp);

    	    /* If still open references to this, just rename and
       	       save info for delayed remove. Don't worry about
	       race conditions with other processes, remove and
               open simultaneously has undefined order even on a local
               system. 
	       NT:  If delete on close flag is set do the remove now.
	    */

    	    if (MVFS_DEL_RENAME_NEEDED(vp, ctxp) &&
    			(mnp->mn_vob.rmv_name == NULL) &&
        		(!mnp->mn_vob.cleartext.delete_on_close)) {

                error = mvfs_silly_rename(dvp, vp, nm, mth, cd);
                if (error != 0) {
		    MUNLOCK(mnp);
		    if (avp == NULL)
			VN_RELE(vp);
		    break;
		}
    	    } else {			/* Real remove op */
		/*
		 * Now we can remove from the view
		 */
		MUNLOCK(mnp);
        	error = mfs_clnt_remove(dvp, nm, MFS_USE_PROCBH, TRUE, cd);
		MLOCK(mnp);
        	if (!error) {
    	            /* Can't trust attributes or cleartext after remove. */
    	    	    MFS_ATTRINVAL(vp);
		    mfs_clear_mark_name_purge(vp);
		    /* Release cached slink info - may not be valid */
		    if (mnp->mn_vob.slinktext) {
			KMEM_FREE(mnp->mn_vob.slinktext, mnp->mn_vob.slinklen);
			mnp->mn_vob.slinktext = NULL;
			mnp->mn_vob.slinklen = 0;
		    }
        	}
    	    }
    	    MUNLOCK(mnp);
	    /*
	     * Release the vnode we have just removed if it was
	     * not passed to us.
	     *
             * Unless someone has just started using this object
             * by another name, it will be inactived.
	     */
            if (avp == NULL)
		VN_RELE(vp);	    /* Done with vnode of object */
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    /* 
     * Do audit.  Note that the "saved audit info" is passed
     * in a weird place on this call (as the second dir name)
     */

    if (!error) {
        MFS_AUDIT(MFS_AR_UNLINK, dvp, nm, NULL, (char *)rmstatp, NULL, cd);
    }

    if (rmstatp) 		    /* Done with saved info for audit */
	KMEM_FREE(rmstatp, sizeof(*rmstatp));
    if (dvp != advp) VN_RELE(dvp);  /* Release if allocated bound root vnode */

    MDB_VLOG((MFS_VREMOVE,"vp=%"KS_FMT_PTR_T" name=%s, err=%d\n",dvp,nm,error));
    BUMPSTAT(mfs_vnopcnt[MFS_VREMOVE]);
    MVFS_EXIT_FS(mth);
    return (error);
}

/* This function assumes that it is being called only for MVFS objects.  It 
 * should never be called for Linux shadow files or any other file object.
 */
int
mvfs_link_ctx(
    VNODE_T *atdvp,
    VNODE_T *vp,
    char *tnm,
    CALL_DATA_T *cd,
    MVFS_LINK_CTX_T *ctxp
)
{
    VNODE_T *tdvp = atdvp;
    int error;
    MVFS_DECLARE_THREAD(mth)

    /* 
     * Verify same device 
     * This must occur before the ASSERTs because in some systems
     * this op is called solely on the basis of one of the vnodes.
     * There is no guarantee that the other vnode is MFS, unless they
     * are both on the same mountpoint.
     */
    if (!MVFS_IS_SAME_VFS(vp, tdvp)) {
	return(EXDEV);
    }
    if ((tdvp->v_vfsp->vfs_flag & VFS_RDONLY) != 0) {
        return(EROFS);
    }
    ASSERT(VTOM(tdvp)->mn_hdr.vp);
    ASSERT(VTOM(vp)->mn_hdr.vp);

    /* Verify vp is not a dir */

    if (MVFS_ISVTYPE(vp, VDIR)) {
	return(EISDIR);
    }
    if (tnm == NULL || tnm[0] == '\0') {
	mvfs_log(MFS_LOG_ERR, "mfs_link: null name\n");
	return (EINVAL);
    }

    MVFS_ENTER_FS(mth);
    /* Switch based on class of ***DIR*** object */

    switch (VTOM(tdvp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    error = EROFS;	/* Not allowed */
	    break;
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    /* I can't always trust the FS to check for xdev, so I do
	       it here.  The object must have a real vp, and both real
	       vp's must be on the same dev. */

	    if (!MFS_REALVP(vp) ||
		!MVFS_IS_SAME_VFS(MFS_REALVP(vp), MFS_REALVP(tdvp))) {
		error = EXDEV;
		break;
	    }
	    error = MVOP_LINK(MFS_REALVP(tdvp), MFS_REALVP(vp), tnm, cd, ctxp);
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EROFS;
	    break;
	case MFS_VOBRTCLAS:
	    tdvp = mfs_bindroot(tdvp, cd, &error);
	    if (error == ESRCH) error = EROFS;   /* Null view to RO FS */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {
	    /* Verify same view AFTER bindroot */
	    if (MFS_VIEW(vp) != MFS_VIEW(tdvp)) {
		error = EXDEV;
		break;
	    }
	    (void) mfs_rebind_vpp((atdvp != tdvp), &tdvp, cd);
	    error = mfs_clnt_link(vp, tdvp, tnm, cd);
	    break;
        }
	default:
	    error = ENXIO;
	    break;
    }

    /* Do audit if no error */

    if (!error) {
	MFS_AUDIT(MFS_AR_LINK, tdvp, tnm, NULL, NULL, vp, cd);
    }

    if (tdvp != atdvp) VN_RELE(tdvp); /* Release if allocated bnd root vnode */

    MDB_VLOG((MFS_VLINK,"vp=%"KS_FMT_PTR_T" tdvp=%"KS_FMT_PTR_T", nm=%s, err=%d\n",vp,tdvp,tnm, error));
    BUMPSTAT(mfs_vnopcnt[MFS_VLINK]);
    MVFS_EXIT_FS(mth);
    return (error);
}

int
mfs_link(
    VNODE_T *atdvp,
    VNODE_T *vp,
    char *tnm,
    CALL_DATA_T *cd
)
{
    return mvfs_link_ctx(atdvp, vp, tnm, cd, NULL);
}

int
mfs_rename(
    VNODE_T *aodvp,
    char *onm,
    VNODE_T *atdvp,
    char *tnm,
    CALL_DATA_T *cd
)
{
    return mvfs_rename_ctx(aodvp, onm, atdvp, tnm, cd, NULL);
}

int
mvfs_rename_ctx(
    VNODE_T *aodvp,
    char *onm,
    VNODE_T *atdvp,
    char *tnm,
    CALL_DATA_T *cd,
    MVFS_RENAME_CTX_T *ctxp
)
{
    VNODE_T *odvp = aodvp;
    VNODE_T *tdvp = atdvp;
    VNODE_T *vp;
    int error;
    mvfs_thread_t *mth;

    /* 
     * Make sure the same "device" 
     * The device check must occur before the ASSERTs because
     * on some systems, the call to our op is made solely on
     * the basis of the FS type of the "odvp", and there is
     * no guarantee that tdvp is even an MFS object.
     */
    if (!MVFS_IS_SAME_VFS(odvp, tdvp)) {
        return(EXDEV);
    }
    if ((odvp->v_vfsp->vfs_flag & VFS_RDONLY) != 0) {
        return(EROFS);
    }
    ASSERT(VTOM(odvp)->mn_hdr.vp);
    ASSERT(VTOM(tdvp)->mn_hdr.vp);

    mth = MVFS_GET_THREAD(cd);

    /* Disallow hames we know are no good */

    if (mfs_hmcmp(onm, ".") == 0 || mfs_hmcmp(onm, "..") == 0 ||
	mfs_hmcmp(tnm, ".") == 0 || mfs_hmcmp(tnm, "..") == 0) {
	error = EINVAL;
        goto errout;
    }

    /* Switch based on class of object */

    switch (VTOM(odvp)->mn_hdr.mclass) {
	case MFS_SDEVCLAS:
	    error = ENOTDIR;
	    break;
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    error = MVOP_RENAME(MFS_REALVP(odvp),onm,MFS_REALVP(tdvp),tnm,
                                cd,ctxp);
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EROFS;
	    break;
	case MFS_VOBRTCLAS:
	    odvp = mfs_bindroot(odvp, cd, &error);
	    if (error == ESRCH) error = EROFS;   /* Null view -> RO FS */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {

	    /* Must bind to dir if applicable */

	    if (MFS_ISVOBRT(VTOM(tdvp))) {
		tdvp = mfs_bindroot(tdvp, cd, &error);
		if (error == ESRCH) error = EROFS;    /* Null view -> RO FS */
		if (error) break;
	    }

	    /* Must make sure that the two dirs are the same view */
	
	    if (MFS_VIEW(odvp) != MFS_VIEW(tdvp)) {
		error = EXDEV;
		break;
	    }

	    /* Must do out of date cwd checks on both dirs */

	    (void) mfs_rebind_vpp((aodvp != odvp), &odvp, cd);
            (void) mfs_rebind_vpp((atdvp != tdvp), &tdvp, cd);

	    /* 
             * This is a real pain, but we have to SYNC the attributes
             * on the target vnode before the rename, since rename can
             * end up deleting the target object (rename over existing),
             * and we don't want to try to sync the attributes after the
             * rename has deleted the object (causes the view to get
             * unhappy about operations on objects that don't exist!)
             * If there is an error, ignore it.  ENOENT is an expected
             * condition, and other errors are probably OK too.
             */

            error = mfs_lookup(tdvp, tnm, &vp, (struct pathname *)0, 0,
                               (ROOTDIR_T *)0, cd);
            if (error == 0) {
	        ASSERT(MFS_ISVOB(VTOM(vp)));
		MLOCK(VTOM(vp));
		mvfs_sync_attr(VTOM(vp), NULL, MFS_USE_PROCBH, 0, cd);
		mfs_clear_mark_purge(vp);
                /*
                 * By pure happenstance, the tests for link count > 1
                 * (inside the macro definitions of MVFS_RENAME_RENAME_NEEDED())
                 * mean that directory targets don't ever get
                 * silly-renamed since they have link-count of 2 or
                 * more.
                 */
                if (MVFS_RENAME_RENAME_NEEDED(vp, ctxp) &&
                    VTOM(vp)->mn_vob.rmv_name == NULL &&
                    !VTOM(vp)->mn_vob.cleartext.delete_on_close)
                {
                    /* There's one more case to check.  If the caller specified
                    ** the same name in the same dir for the source and target
                    ** (like the one of our tests does) then we don't want to
                    ** do the silly rename because that will cause the
                    ** subsequent rename to fail because we'll have changed the
                    ** source name as well with the silly rename (since it is
                    ** the same as the target name), and therefore, the server
                    ** rename rpc will fail because it won't find the source
                    ** name.  Note, we don't have case-sensitive info for the
                    ** pathname here so we just pass in FALSE for the pn
                    ** compare (similar to what we did in the mfs_lookup
                    ** above).
                    */
                    if (!(tdvp == odvp && PN_STRCMP(FALSE, tnm, onm) == 0)) {
                        /*
                         * It would be nicer to use a hard-link on the target,
                         * but that is subject to race conditions where two
                         * rename-induced links succeed.  We can only record
                         * one of them for cleanup when the vnode is
                         * deactivated.  So we must use rename, which might
                         * recursively call back to this routine on the target.
                         * Since the rename is to a unique target name (at
                         * least per host--what about multi-host cases?), we
                         * will be limited to one level of recursion.
                         */
                        MDB_VLOG((MFS_VRENAME,
                                  "need to silly-rename target %s"
                                  " (lcount %lu vcount %d)\n", tnm,
                                  VTOM(vp)->mn_vob.attr.fstat.nlink,
                                  V_COUNT(vp)));
                        error = mvfs_silly_rename(tdvp, vp, tnm, mth, cd);
                    } else {
                        MDB_VLOG((MFS_VRENAME,
                                  "same name, no need to silly-rename"
                                  " target %s (lcount %lu vcount %d)\n", tnm,
                                  VTOM(vp)->mn_vob.attr.fstat.nlink,
                                  V_COUNT(vp)));
                    }
                } else {
                    MDB_VLOG((MFS_VRENAME,
                              "no need to silly-rename target %s (lcount %lu"
                              " vcount %d)\n", tnm,
                              VTOM(vp)->mn_vob.attr.fstat.nlink, V_COUNT(vp)));
                }
		MUNLOCK(VTOM(vp));
		VN_RELE(vp);
	    } else {
                /* if target doesn't exist, continue with rename */
                if (error == ENOENT) {
                    error = 0;
                }
            }
	    /* Now we can do the rename */

            if (error == 0)
                error = mfs_clnt_rename(odvp, onm, tdvp, tnm, cd);
	    break;
        }
	default:
	    error = ENXIO;
	    break;
    }

    /* Do audit if no error
       FIXME: don't have object, just old dir/name.
       Perhaps 2 recs, 1 for unlink, and 1 for link. */

    if (!error) {
	MFS_AUDIT(MFS_AR_RENAME, odvp, onm, tdvp, tnm, NULL, cd);
    }

    if (odvp != aodvp) VN_RELE(odvp);   /* Release if allocated vnode */
    if (tdvp != atdvp) VN_RELE(tdvp);   /* Release if allocated vnode */

errout:
    MDB_VLOG((MFS_VRENAME,"vp=%"KS_FMT_PTR_T" %s to %"KS_FMT_PTR_T" %s, err=%d\n",odvp,onm,tdvp,tnm,error));
    BUMPSTAT(mfs_vnopcnt[MFS_VRENAME]);
    MVFS_EXIT_FS(mth);
    return (error);
}

int
mvfs_mkdir(
    VNODE_T *advp,
    char *nm,
    VATTR_T *va,
    VNODE_T **vpp,
    CALL_DATA_T *cd
)
{
    return mvfs_mkdir_ctx(advp, nm, va, vpp, cd, NULL);
}

int
mvfs_mkdir_ctx(
    VNODE_T *advp,
    char *nm,
    VATTR_T *va,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    MVFS_MKDIR_CTX_T *ctxp
)
{
    VNODE_T *dvp = advp;
    CLR_VNODE_T *cvp;
    int error;
    MVFS_DECLARE_THREAD(mth)

    if ((dvp->v_vfsp->vfs_flag & VFS_RDONLY) != 0)
        return EROFS;

    ASSERT(VTOM(dvp)->mn_hdr.vp);

    *vpp = NULL;
    if (!MVFS_ISVTYPE(dvp, VDIR)) {
	return(ENOTDIR);
    }
    if (nm == NULL || nm[0] == '\0') {
	mvfs_log(MFS_LOG_ERR, "mvfs_mkdir: null name\n");
	return (EINVAL);
    }

    MVFS_ENTER_FS(mth);
    /* Switch based on class of object */

    switch (VTOM(dvp)->mn_hdr.mclass) {
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    error = MVOP_MKDIR(MFS_REALVP(dvp), nm, va, &cvp, cd, ctxp);
	    if (!error) {
		error = mfs_makeloopnode(MFS_VIEW(dvp), cvp, vpp,
                                         MVFS_CD2CRED(cd));
		CVN_RELE(cvp);
	    } else *vpp = NULL;
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EROFS;
	    break;
	case MFS_VOBRTCLAS:
	    dvp = mfs_bindroot(dvp, cd, &error);
	    if (error == ESRCH) error = EROFS;	/* Null view returns EROFS */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS:
	    (void) mfs_rebind_vpp((advp != dvp), &dvp, cd);
	    /*
	     * Screen out bogus names "." and ".."; these must return EEXIST
	     */
	    if (nm[0] == '.' && (nm[1] == '\0' || 
				 (nm[1] == '.' && nm[2] == '\0'))) {
		error = EEXIST;
		break;
	    }
	    error = mfs_clnt_mkdir(dvp, nm, va, vpp, cd);
	    if (!error) {	
		mfs_rebind_self(*vpp, cd);  /* New dir is rebinds to self */
	    }
	    break;
	default:
	    error = ENXIO;
	    break;
    }

    if (!error) {
	MFS_AUDIT(MFS_AR_CREATE, dvp, nm, NULL, NULL, *vpp, cd);
    }

    if (dvp != advp) VN_RELE(dvp);   /* Release if allocated bound root vnode */

    /*
     * *vpp may not always be a mvfs vnode (see mfs_makeloopnode()), so
     * check for ISMFS before making the VTOM call 
     */
    MDB_VLOG((MFS_VMKDIR,"vp=%"KS_FMT_PTR_T" nm=%s, rvp=%"KS_FMT_PTR_T", rmnp=%"KS_FMT_PTR_T", err=%d\n",dvp,nm,*vpp,(*vpp ? (MFS_VPISMFS(*vpp)?VTOM(*vpp):0) : 0),error));
    BUMPSTAT(mfs_vnopcnt[MFS_VMKDIR]);
    MVFS_EXIT_FS(mth);
    return (error);
}

/* MFS_RMDIR - remove dir operation */

int
mfs_rmdir(
    VNODE_T *advp,
    char *nm,
    VNODE_T *cdir,
    CALL_DATA_T *cd
)
{
    return mvfs_rmdir_ctx(advp, nm, cdir, cd, NULL);
}

EXTERN int
mvfs_rmdir_ctx(
    VNODE_T *advp,
    char *nm,
    VNODE_T *cdir,
    CALL_DATA_T *cd,
    MVFS_RMDIR_CTX_T *ctxp
)
{
    VNODE_T *dvp = advp;
    struct mfs_auditrmstat *rmstatp = NULL;
    VNODE_T *vp;
    int error;
    MVFS_DECLARE_THREAD(mth)

    if ((dvp->v_vfsp->vfs_flag & VFS_RDONLY) != 0)
        return EROFS;

    ASSERT(VTOM(dvp)->mn_hdr.vp);

    if (!MVFS_ISVTYPE(dvp, VDIR)) return(ENOTDIR);
    if (nm == NULL || nm[0] == '\0') {
	mvfs_log(MFS_LOG_ERR, "mfs_rmdir: null name\n");
	return (EINVAL);
    }
    if (nm[0] == '.' && nm[1] == '\0') {
        /* can't remove ., you idiot.
           EINVAL is the error code used by HP-UX & Solaris. */
#ifdef MVFS_DEBUG
        mvfs_log(MFS_LOG_DEBUG, "mfs_rmdir: remove . fails\n");
#endif
        return EINVAL;
    }

    MVFS_ENTER_FS(mth);

    /* Switch based on class of object */

    switch (VTOM(dvp)->mn_hdr.mclass) {
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    error = MVOP_RMDIR(MFS_REALVP(dvp), nm, cdir, cd, ctxp);
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EROFS;
	    break;
	case MFS_VOBRTCLAS:
	    dvp = mfs_bindroot(dvp, cd, &error);
	    if (error == ESRCH) error = EROFS;	/* Null view returns EROFS */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {
	    (void) mfs_rebind_vpp((advp != dvp), &dvp, cd);

            /* 
             * Lookup the name to get its stat information 
             * before we remove it.
             */

            error = mfs_lookup(dvp, nm, &vp, (struct pathname *)0, 0,
                               (ROOTDIR_T *)0, cd);
            if (error) break;
	    ASSERT(MFS_ISVOB(VTOM(vp)));

    	    MLOCK(VTOM(vp));	 /* Lock to avoid races on rmv state */

	    /*
	     * Fetch the audit information and release the
	     * vnode before we actually remove it.  This solves
             * two problems:
             *   (1) Avoids trying to get info (stat) an object that
             *       no longer exists in mfs_audit()
             *   (2) Avoids holding a refcnt on a vnode whose
             *       DBID (inode number in FSS) may get reused and
             *       cause a "stale active vnode" message and associated
             *	     incorrect results.
             */

	    rmstatp = (struct mfs_auditrmstat *)
				KMEM_ALLOC(sizeof(mfs_auditrmstat_t), KM_SLEEP);
	    if (rmstatp == NULL) {
	        error = ENOMEM;
		MUNLOCK(VTOM(vp));
		VN_RELE(vp);
	        break;
	    }

	    mfs_init_rmstat(vp, rmstatp);

	    MUNLOCK(VTOM(vp));
	    VN_RELE(vp);	/* release hold count from lookup */

	    /*
	     * Now we actually remove the dir.
	     */

	    error = mfs_clnt_rmdir(dvp, nm, cd);
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    /* 
     * Do audit.  Note that the "saved audit info" is passed
     * in a weird place on this call (as the second dir name)
     */

    if (!error) {
        MFS_AUDIT(MFS_AR_UNLINK, dvp, nm, NULL, (char *)rmstatp, NULL, cd);
    }

    if (rmstatp) 		    /* Done with saved info for audit */
	KMEM_FREE(rmstatp, sizeof(*rmstatp));
	
    if (dvp != advp) VN_RELE(dvp);   /* Release if allocated bound root vnode */

    MDB_VLOG((MFS_VRMDIR,"vp=%"KS_FMT_PTR_T" name=%s, err=%d\n",dvp,nm,error));
    BUMPSTAT(mfs_vnopcnt[MFS_VRMDIR]);
    MVFS_EXIT_FS(mth);
    return (error);
}

int
mfs_symlink(
    VNODE_T *advp,
    char *lnm,
    VATTR_T *tva,
    char *tnm,
    CALL_DATA_T *cd
)
{
    return mvfs_symlink_ctx(advp, lnm, tva, tnm, NULL, cd, NULL);
}

int
mvfs_symlink_ctx(
    VNODE_T *advp,
    char *lnm,
    VATTR_T *tva,
    char *tnm,
    VNODE_T **vpp,
    CALL_DATA_T *cd,
    MVFS_SYMLINK_CTX_T *ctxp
)
{
    VNODE_T *dvp = advp;
    VNODE_T *vp = NULL;
    int error;
    MVFS_DECLARE_THREAD(mth)

    if ((dvp->v_vfsp->vfs_flag & VFS_RDONLY) != 0)
        return EROFS;

    ASSERT(VTOM(dvp)->mn_hdr.vp);
    if (!MVFS_ISVTYPE(dvp,VDIR)) return(ENOTDIR);
    if (lnm == NULL || lnm[0] == '\0' ||
	tnm == NULL || tnm[0] == '\0') {
	mvfs_log(MFS_LOG_ERR, "mfs_symlink: null name\n");
	return (EINVAL);
    }

    MVFS_ENTER_FS(mth);
    /* Switch based on class of object */

    switch (VTOM(dvp)->mn_hdr.mclass) {
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    error = MVOP_SYMLINK(MFS_REALVP(dvp), lnm, tva, tnm, &vp, cd, ctxp);
	    break;
	case MFS_NTVWCLAS:
	case MFS_VIEWDIRCLAS:
	    error = EROFS;	/* illegal */
	    break;
	case MFS_VOBRTCLAS:
	    dvp = mfs_bindroot(dvp, cd, &error);
	    if (error == ESRCH) error = EROFS;	/* Null view returns EROFS */
	    if (error) break;
	    /* Fall through */
	case MFS_VOBCLAS: {
	    (void) mfs_rebind_vpp((advp != dvp), &dvp, cd);
	    error = mfs_clnt_symlink(dvp, lnm, tva, tnm, &vp, cd);
	    break;
        }
	default:
	    error = ENXIO;
	    break;
    }

    if (!error && vp) {
	MFS_AUDIT(MFS_AR_SYMLINK, dvp, lnm, NULL, NULL, vp, cd);
	MFS_AUDIT(MFS_AR_RDLINK, NULL, tnm, NULL, NULL, vp, cd);
    }

    if (dvp != advp) VN_RELE(dvp);   /* Release if allocated bound root vnode */

    if (vpp != NULL) {
        /* *vpp is granted our reference (if vp not null) */
        *vpp = vp;
    } else if (vp != NULL) {
        VN_RELE(vp);     /* Release symlink vnode if created */
    }

    MDB_VLOG((MFS_VSYMLINK,"vp=%"KS_FMT_PTR_T" symvp=%"KS_FMT_PTR_T" lnm=%s, tnm=%s, err=%d\n",dvp,vp,lnm,tnm,error));
    BUMPSTAT(mfs_vnopcnt[MFS_VSYMLINK]);
    MVFS_EXIT_FS(mth);
    return (error);
}

/*
 * Internal routine to do the dummy readdir if there
 * is no view for the VOB reference.  Just dummies up 
 * two dirents with . and ..
 * Note:
 *     MVFS_UIO_OFFSET(uiop) should not be larger than
 *     31 bits. See check in mfs_readdir. 
 */

int
mfs_noview_readdir(dvp, uiop, cred)
VNODE_T *dvp;
struct uio *uiop;
CRED_T *cred;
{
    KDIRENT_T *dirent;
    MOFFSET_T o_offset = (int)MVFS_UIO_OFFSET(uiop);
    MOFFSET_T offset;
    int reclen;
    int i;
    int error = 0;
    char *nm = 0;                       /* shut up GCC with = 0 */
    size_t nmlen;
    size_t direntlen;

    ASSERT(VTOM(dvp)->mn_hdr.vp);

    if (MVFS_IS_INVALID_OFFSET(MVFS_UIO_OFFSET(uiop)) || uiop->uio_resid <= KDIRENT_RECLEN(0)) {
	return(ENOENT);
    }

    /* 
     * Allocate structure for dirents.  Can't use stack storage because
     * uiomove won't work from the stack on some machines (eg. HP's).
     * Also, allocate dirent with enough room for max length of name
     * we return ("..")
     */
    direntlen = KDIRENT_RECLEN(sizeof(".."));
    dirent = (KDIRENT_T *)KMEM_ALLOC(direntlen, KM_SLEEP);
    if (dirent == NULL) {
	return(ENOMEM);
    }

    /* Loop through simulated "." and ".." dir returning the right
     * entries the user has asked for.
     * For portability to the most brain-damaged Unix systems that
     * can't use cookies in d_off, but require real byte offsets, we
     * do everything by byte-offset for these simulated dirs.
     */

    offset = 0;		/* Offset in dir we are processing */
    for (i = 0; i < 2; i++, offset += reclen) {
	if (i == 0) nm = ".";
	if (i == 1) nm = "..";

        nmlen = STRLEN(nm);
        reclen = KDIRENT_RECLEN(nmlen);
	if (offset < MVFS_UIO_OFFSET(uiop))
	    continue;		/* Skip entry user doesn't want */

	KDIRENT_INIT(dirent, VTOM(dvp)->mn_hdr.fid.mf_dbid, nm, nmlen, offset+reclen);

	/* Make sure room in buffer */

	if (reclen > uiop->uio_resid) {
	    /* Error if no entries returned yet */
	    if (MVFS_UIO_OFFSET(uiop) == o_offset) {
		error = EINVAL;
		break;
	    }
	    break;	/* No room */
	}

	/* Copy to user */

	error = READDIR_UIOMOVE((caddr_t)dirent, &reclen,
				UIO_READ, uiop, offset);
        if (error != 0)
            break;
    }
    if (!READDIR_BUF_FULL(uiop))
        MVFS_UIO_OFFSET(uiop) = offset;
    KMEM_FREE(dirent, direntlen);
    return(error);

}

/*
 * Read directory entries.
 * There are some weird things to look out for here.  The uio_offset
 * field is either 0 or it is the offset returned from a previous
 * readdir.  It is an opaque value used by the server to find the
 * correct directory block to read.  The byte count must be at least
 * 1 dir entry of bytes.  The count field is the number of blocks to
 * read on the server.  This is advisory only, the server may return
 * only one block's worth of entries.  Entries may be compressed on
 * the server.
 *
 * CWD REBINDING:
 *     Since readdir is an operation on an "open" file descriptor, it
 *     can not be rebound.  Once an fd is open'd wrong you are stuck 
 *     with it.  The secret is to catch the stale cwd in all the other
 *     operations and fix it before the open is done in the OS.
 */
int
mfs_readdir(
    VNODE_T *advp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    int *eofp
)
{
    return mvfs_readdir_ctx(advp, uiop, cd, eofp, NULL);
}
EXTERN int 
mvfs_readdir_ctx(
    VNODE_T *advp,
    struct uio *uiop,
    CALL_DATA_T *cd,
    int *eofp,
    MVFS_READDIR_CTX_T *ctxp
)
{
    VNODE_T *dvp = advp;
    int error;
    ssize_t uc;
    MOFFSET_T uoff;
    MVFS_DECLARE_THREAD(mth)
    mfs_mnode_t *mnp;
    tbs_boolean_t fromcache = FALSE;

    ASSERT(VTOM(dvp)->mn_hdr.vp);
    if (!MVFS_ISVTYPE(dvp, VDIR)) return(ENOTDIR);

    /* For logging, save original offset and count  */

    uoff = MVFS_UIO_OFFSET(uiop);
    uc   = uiop->uio_resid;

    /*
     * We need to get our thread before going to errout: (otherwise
     * asserts fail in audit code)
     */
    MVFS_ENTER_FS(mth);

    /*
     * Maximum size of a Large File Directory is 31 bits.
     * If offset is greater than this value, return eofp;
     */

    if (uoff >= MVFS_MAX_DIRSIZE) {
        if (eofp)
            *eofp = 1;
        error = 0;
        goto errout;
    }

    if (eofp != NULL)
	*eofp = 0;

    /* Switch based on class of object */

    switch (VTOM(dvp)->mn_hdr.mclass) {
	case MFS_VIEWCLAS:
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
	    MVOP_RWRDLOCK(MFS_CLRVP(dvp), ctxp);
	    error = MVOP_READDIR(MFS_REALVP(dvp), uiop, cd, eofp, ctxp);
	    MVOP_RWRDUNLOCK(MFS_CLRVP(dvp), ctxp);
	    break;
	case MFS_NTVWCLAS:
	    error = mvfs_ntvw_readdir(dvp, uiop, MVFS_CD2CRED(cd), eofp);
	    break;
	case MFS_VIEWDIRCLAS:
	    error = mvfs_viewdirreaddir(dvp, uiop, MVFS_CD2CRED(cd), eofp);
	    break;
	case MFS_VOBRTCLAS:
	    dvp = mfs_bindroot(dvp, cd, &error);
	    if (error) {
		/*
		 * For a NULL View call routine which will return
                 * dummied up '.' and '..'
		 */
	        if (error == ESRCH) 
		    error = mfs_noview_readdir(dvp, uiop, MVFS_CD2CRED(cd));
		break;
	    }
            ASSERT(dvp);
            ASSERT(VTOM(dvp));
	    ASSERT(MFS_ISVOB(VTOM(dvp)));
	    /* Fall through */
	case MFS_VOBCLAS: {

	    (void) mfs_rebind_vpp((dvp != advp), &dvp, cd);

            mnp = VTOM(dvp);
	    ASSERT(MFS_ISVOB(mnp));
	    /*
	     * To force reasonable name and readdir cache consistency, 
             * validate the AC on the dir and force a refetch
             * if it has timed out.  If the dir was modified
             * then the attr cache code will flush the name cache.
	     * FIXME: really way reply field in clnt_readdir that
             * validates existing name cache...
             *
             * This call is made here instead of in clnt_readdir
             * to avoid deep stacks.
             */

	    if (mfs_ac_timedout(mnp, FALSE, cd )) {
		if (mfs_getattr(dvp, NULL, 0, cd) != 0) {
                    /* getattr will usually indirectly flush the cache for this
                       node if DTM has changed on directory, via
                       mfs_clnt_getattr()->mfs_attrcache()->mfs_ac_modevents() */
                    MLOCK(mnp);
                    mvfs_rddir_cache_flush(mnp);
                    MUNLOCK(mnp);
                }
		BUMPSTAT(mfs_acstat.ac_misses);
		BUMPVSTAT(dvp,acstat.ac_misses);
                BUMPSTAT(mfs_acstat.ac_rddirmiss);
	    } else {
                /*
                 * Look in readdir cache on mnode
                 */
                MLOCK(mnp);
                if (mnp->mn_vob.rddir_cache != NULL) {
                    /*
                     * RDDIRPLUS implementation on some OSes (linux)
                     * may end up accessing this object again via a
                     * bindroot.  Need to accomodate recursive locks
                     * in this case.
                     */
                    MVFS_RDDIR_MNLOCK_SET_RECURSIVE(mnp);
                    fromcache = mvfs_rddir_cache_get(mnp, uiop,
                                                     MVFS_CD2CRED(cd),
                                                     eofp, &error);
                    MVFS_RDDIR_MNLOCK_CLEAR_RECURSIVE(mnp);
                }
                MUNLOCK(mnp);
            }
            if (!fromcache)
                error = mfs_clnt_readdir(dvp, uiop, cd, eofp);
	    break;
        }
	default:
	    error = ENXIO;
	    break;
    }

errout:
    if (!error) {
	MFS_AUDIT(MFS_AR_READ, NULL, NULL, NULL, NULL, dvp, cd);
    }

    if (dvp != advp) {
	ASSERT(!MFS_ISVIEWDIR(VTOM(dvp)));
	VN_RELE(dvp);   /* Release if allocated bound root vnode */
    }
    MDB_VLOG((MFS_VREADDIR,
              "fromcache=%d, vp=%"KS_FMT_PTR_T" err=%d pid=%d, off=%"MVFS_FMT_MOFFSET_T_D"/%"MVFS_FMT_UIO_OFFSET_D", bc=%"MVFS_FMT_MOFFSET_T_D"\n",
              fromcache,
              dvp, error, MDKI_CURPID(), uoff,
              MVFS_UIO_OFFSET(uiop), uc - (MOFFSET_T)uiop->uio_resid));
    BUMPSTAT(mfs_vnopcnt[MFS_VREADDIR]);
    MVFS_EXIT_FS(mth);
    return (error);
}

/*
 * MFS_FSYNC 
 */
int
mfs_fsync(
    VNODE_T *vp,
    int flag,
    CALL_DATA_T *cd
)
{
    return mvfs_fsync_ctx(vp, flag, cd, NULL);
}
int
mvfs_fsync_ctx(
    VNODE_T *vp,
    int flag,
    CALL_DATA_T *cd,
    MVFS_FSYNC_CTX_T *ctxp
)
{
    VNODE_T *cvp;
    int error;
    MVFS_DECLARE_THREAD(mth)

    MVFS_ENTER_FS(mth);
    
    /* Force out our own pages if we have any before locking the mnode */

    if (MVFS_ISVTYPE(vp, VREG) && MFS_ISVOB(VTOM(vp)) && 
					VTOM(vp)->mn_hdr.cached_pages) {
	error = PVN_FLUSH(vp, MFS_PVN_FLUSH, MVFS_CD2CRED(cd));
	if (error) {
	    MVFS_EXIT_FS(mth);
	    return(error);
	}
    }

    /* Fsync cleartext if one present. */

    ASSERT(VTOM(vp)->mn_hdr.vp);
    MLOCK(VTOM(vp));
    if ((cvp = MFS_REALVP(vp)) != NULL) {
	VN_HOLD(cvp);
	MUNLOCK(VTOM(vp));
	error = MVOP_FSYNC(cvp, flag, cd, ctxp);
	VN_RELE(cvp);
    } else {
	MUNLOCK(VTOM(vp));
	error = 0;
    }

    /* Fsync any delayed attributes.  (MUST BE AFTER DATA SYNC!)  */

    if (!error && MFS_ISVOB(VTOM(vp))) {
	MLOCK(VTOM(vp));
	error = mvfs_sync_attr(VTOM(vp), NULL, MFS_USE_PROCBH, 0, cd);
	MUNLOCK(VTOM(vp));
    }

    MDB_VLOG((MFS_VFSYNC, "vp=%"KS_FMT_PTR_T" mcred=%"KS_FMT_PTR_T"\n",vp, MCRED(VTOM(vp))));
    BUMPSTAT(mfs_vnopcnt[MFS_VFSYNC]);
    MVFS_EXIT_FS(mth);
    return(error);
}

/*
 * MFS_LOCKCTL - record locking operation
 * Record-locking requests are passed to the cleartext which does
 * local/remote record locking as required.
 */
int
mfs_lockctl(
    VNODE_T *vp,
    struct flock *ld,
    int cmd,
    CALL_DATA_T *cd,
    int clid
)
{
    return mvfs_lockctl_ctx(vp, ld, cmd, cd, (MVFS_LOCKCTL_CTX_T *) &clid);
}

int
mvfs_lockctl_ctx(
    VNODE_T *vp,
    void *ld,
    int cmd,
    CALL_DATA_T *cd,
    MVFS_LOCKCTL_CTX_T *ctxp
)
{
    int error;
    CLR_VNODE_T *cvp;
    mfs_mnode_t *mnp;
    MVFS_DECLARE_THREAD(mth)

    mnp = VTOM(vp);

    /* Must be a regular file */

    if (!MVFS_ISVTYPE(vp, VREG)) {
	return(EINVAL);		/* by convention */
    }
	
    MVFS_ENTER_FS(mth);
    /* Switch based on class of object.  Note no need to
       check classes that only have dir objects in them. */

    switch (mnp->mn_hdr.mclass) {
	case MFS_LOOPCLAS:	/* Pass on to real vnode */
            error = MVOP_LOCKCTL(MFS_REALVP(vp), ld, cmd, cd, ctxp);
	    break;
	case MFS_VOBCLAS: {
	    error = 0;
	    MLOCK(mnp);
	    error = mfs_getcleartext(vp, &cvp, cd);
	    MUNLOCK(mnp);
	    if (error) break;
	    error = MVOP_LOCKCTL(cvp, ld, cmd, cd, ctxp);
	    CVN_RELE(cvp);
	    break;
	}
	default:
	    error = ENXIO;
	    break;
    }

    MDB_VLOG((MFS_VLOCKCTL,"vp=%"KS_FMT_PTR_T" err= %d\n",vp,error));
    BUMPSTAT(mfs_vnopcnt[MFS_VLOCKCTL]);
    MVFS_EXIT_FS(mth);
    return(error);
}

/*
 * MFS_VFID - get fid for object
 */

int
mfs_vfid(vp, fidpp)
VNODE_T *vp;
struct fid **fidpp;
{
    int error;
    mfs_xfid_t *mxfidp = 0;             /* shut up GCC with = 0 */
    mfs_mnode_t *mnp;
    MVFS_DECLARE_THREAD(mth)

    MVFS_ENTER_FS(mth);
    mnp = VTOM(vp);
    ASSERT(mnp->mn_hdr.vp);
    if (MFS_ISVOB(mnp) || MFS_ISVOBRT(mnp)) {
	if (MFS_VIEW(vp) &&
	    VTOM(MFS_VIEW(vp))->mn_view.exid == (u_int) -1) {
	    /* not marked for export.  bounce it. */
	    *fidpp = NULL;
            MVFS_VFID_SET_EXP_ERROR(error, mnp, vp);
	} else {
#ifdef KMEMDEBUG
	    /* 
	     * The MFS does not return the storage for a fid.  The
	     * code that does can't be made to call mfs_kfree(), so
	     * we can't use the mfs_kalloc() to allocate this storage..
	     * so we go straight the the underlying allocator.
	     */
	    mxfidp = (mfs_xfid_t *)REAL_KMEM_ALLOC(sizeof(mfs_xfid_t), KM_SLEEP);
#else
	    mxfidp = (mfs_xfid_t *)KMEM_ALLOC(sizeof(mfs_xfid_t), KM_SLEEP);
#endif
	    if (mxfidp != NULL) {
		mxfidp->mfx_len = MFS_XFIDDATASZ;
		mxfidp->mfx_vid = 0;
		mxfidp->mfx_dbid = mnp->mn_hdr.fid.mf_dbid;
		mxfidp->mfx_gen  = mnp->mn_hdr.fid.mf_gen;
		if (MFS_VIEW(vp)) {
		    mxfidp->mfx_vid = (u_short)(VTOM(MFS_VIEW(vp))->mn_view.exid);
		    /* Make the generation number be offset by the
		     * hash of the view uuid.  This will detect and
		     * return ESTALE if /view is unmounted (or system reboots)
		     * and the views are registered in a different order
		     * so that the view-id in mfx_vid is no longer the
		     * same view as it used to be.
		     */
		    mxfidp->mfx_gen += 
			mfs_uuid_to_hash32(&(VTOM(MFS_VIEW(vp))->mn_view.svr.uuid));
		}
		/*
		 * also add in VOB uuid hash (needed to prevent same
		 * view/different VOB confusion at the unbound VOB root)
		 */
		mxfidp->mfx_gen +=
		    mfs_uuid_to_hash32(&(VFS_TO_MMI(vp->v_vfsp)->mmi_svr.uuid));
		*fidpp = (struct fid *)mxfidp;
		error = 0;
	    } else {
		*fidpp = NULL;
		error = ENOMEM;
	    }
	}
    } else {
        *fidpp = NULL;
        MVFS_VFID_SET_ERROR(error, mnp, vp);
    }

    if (!error) {
	MDB_VLOG((MFS_VFID, "vp=%"KS_FMT_PTR_T" fid=%x/%x/%x/%x err=%d\n",vp,
				((int *)mxfidp)[0], ((int *)mxfidp)[1],
				((int *)mxfidp)[2], ((int *)mxfidp)[3], error));
    }
    else {
	MDB_VLOG((MFS_VFID, "vp=%"KS_FMT_PTR_T" err=%d\n",vp, error));
    }
    BUMPSTAT(mfs_vnopcnt[MFS_VFID]);
    MVFS_EXIT_FS(mth);
    return(error);
}

/*
 * MFS_CMP - compare two vnodes
 */

int
mfs_cmp(vp1, vp2)
VNODE_T *vp1;
VNODE_T *vp2;
{

    MDB_VLOG((MFS_VCMP, "vp1=%"KS_FMT_PTR_T" vp2 = %"KS_FMT_PTR_T"\n",vp1, vp2));
    BUMPSTAT(mfs_vnopcnt[MFS_VCMP]);
    return (vp1 == vp2);
}

/*
 * MFS_REALVP - get realvp for vnode
 */

int
mfs_realvp(vp, vpp)
VNODE_T *vp;
VNODE_T **vpp;
{

    mvfs_log(MFS_LOG_WARN, "stray realvp\n");
    BUMPSTAT(mfs_vnopcnt[MFS_VREALVP]);
    return(ENOSYS);
}

/*
 * MFS_MAP - map call 
 */

int
mfs_map(vp)
VNODE_T *vp;
{

    /*
     * Only log a stray map if by some chance the vnode is not
     * marked as 'VNOMAP' for the higher levels.
     */
#ifdef VNOMAP
    if ((vp->v_flag & VNOMAP) == 0) {
        mvfs_log(MFS_LOG_WARN, "stray map\n");
    }
#endif
    BUMPSTAT(mfs_vnopcnt[MFS_VMAP]);
    return(ENOSYS);
}

/*
 * MFS_PUTPAGE
 */

int
mfs_putpage(vp)
VNODE_T *vp;
{

    mvfs_log(MFS_LOG_WARN, "stray putpage\n");
    BUMPSTAT(mfs_vnopcnt[MFS_VPUTPAGE]);
    return(ENOSYS);
}

int
mfs_getpage(vp)
VNODE_T *vp;
{

    mvfs_log(MFS_LOG_WARN, "stray getpage");
    BUMPSTAT(mfs_vnopcnt[MFS_VGETPAGE]);
    return(ENOSYS);
}

/*
 * MFS_GETVIEW - get an appropriate view for the current object.
 *	Returns view vnode pointer with a "hold" on it.
 *	Caller must do a VN_RELE on the returned view later.
 * 	Options:
 *	vp == NULL: just get any setview view.
 *
 *	Note also, that "cred" may be passed in NULL.  Further,
 *	note that this routine is called by mfs_root() whose
 *	caller may expect it not to wait (see comment there).
 *	Thus, be careful here that nothing waits.
 */
VNODE_T *
mfs_getview(vp, cred, hold)
VNODE_T *vp;
CRED_T *cred;
int hold;
{

    VNODE_T *vw = NULL, *procvw = NULL;
    int error;

    /* Get "view" from object itself if set.  This is the
       most frequent case.  The check for the view's id is
       to catch "stale" views and not use them. */

    if (vp) {
        ASSERT(VTOM(vp)->mn_hdr.vp);
        if ((vw = MFS_VIEW(vp)) != NULL) {
            if (VTOM(vw)->mn_view.id == MFS_NULLVID) return(NULL);
	    if (VTOM(vw)->mn_view.svr.host == NULL ||
	        MFS_STRBUFPN_PAIR_GET_KPN(&(VTOM(vw)->mn_view.svr.lpn)).s == NULL ||
	        VTOM(vw)->mn_view.svr.rpn == NULL) {
	        return(NULL);
	    }
            if (hold) {
                MFS_HOLDVW(vw);
            }
	    return(vw);
	}
    }

    /* 
     * Get the process's view.  This is the "setview" to use when first
     * encountering a VOB root. We need to assign MVFS_GET_PROCVIEW() to
     * a local variable first, rather than putting MVFS_GET_PROCVIEW() into
     * MFS_VPISMFS() and MFS_VIEW() directly. Otherwise, chroot from other
     * thread can cause race condition.
     */
    procvw = MVFS_GET_PROCVIEW();
    if (MFS_VPISMFS(procvw)) {
        vw = MFS_VIEW(procvw);
        if (vw != NULL) {
            if (hold) {
                MFS_HOLDVW(vw);
            }
	    return(vw);
	}
    }

    /* No view found to use ... */

    return(NULL);
}

/*
 * MFS_BINDROOT - bind a vob root to the vnode for that root
 *	in the user's view.  The procedure signature is a little
 *	strange.  We return the vnode ptr so that the vp can be
 *	allocated in a register in calling routines.
 *	The error is returned via a ptr to the error instead.
 *
 *	If a vnode is returned, then it is returned HELD
 *	and must be released by the caller.
 */
VNODE_T *
mfs_bindroot(
    VNODE_T *vp,
    CALL_DATA_T *cd,
    int *errp
)
{
    VNODE_T *vw;
    int error;
    VNODE_T *rtvp = NULL;
    mfs_fid_t fid;
    int inum;

    ASSERT(MFS_ISVOBRT(VTOM(vp)));
    ASSERT(VTOM(vp)->mn_hdr.vp);

    /* 
     * Get "view" for this process.  If can't find
     * one, just return "ESRCH" to indicate no view. 
     * This is simply a unique error for any op calling this routine.
     * FIXME: put mapping into hdr and rename ENOVIEW.
     * Each vnode op handles the "no view" case differently.
     */

    vw = mfs_getview(vp, MVFS_CD2CRED(cd), TRUE /* HOLD */);
    if (vw == NULL) {
	error = ESRCH;
	goto errout;
    }

    error = mvfs_rvclookup(vw, V_TO_MMI(vp)->mmi_rootvp, &fid, cd);
    if (!error) {
	error = mfs_getvnode(vp->v_vfsp, vw, &fid, &rtvp, cd);
	if (error) goto errout;
    } else if (error != ENOENT) {
	goto errout;
    } else {		/* Find it the hard way */
	error = mfs_clnt_bindroot(1, vw, vp->v_vfsp, "", &rtvp, cd);

        /* Update the bound root cache */

	if (!error) {
	    error = mvfs_rvcenter(vw, rtvp,
				 &(VTOM(rtvp)->mn_hdr.fid), MVFS_CD2CRED(cd));
	}
    }

    /* Make audit record for bound root and view of root */

    if (!error) {
	ASSERT(rtvp);
        MFS_AUDIT(MFS_AR_ROOT, NULL, V_TO_MMI(vp)->mmi_mntpath,
			NULL, NULL, rtvp, cd);
	MFS_AUDIT(MFS_AR_VIEW, NULL, NULL, NULL, NULL, rtvp, cd);
    }

    /* General exit code */

errout:
    MDB_XLOG((MDB_BINDROOT,"vp=%"KS_FMT_PTR_T" err=%d, vw=%"KS_FMT_PTR_T", rtvp=%"KS_FMT_PTR_T" rtmnp=%"KS_FMT_PTR_T"\n", vp, error, vw, rtvp, rtvp ? VTOM(rtvp) : 0));
    if (vw != NULL) VN_RELE(vw);	/* Release allocated view */
    if (errp) *errp = error;		/* Return error if wanted */
    if (!error) return(rtvp);
    else return(vp);			/* Return same vnode on error */
}

/*
 * Utility routine to return vnode type from
 * mnode ptr
 */
VTYPE_T
mfs_mn_vtype(mnp)
mfs_mnode_t *mnp;
{
    /* Set vnode type for init */

    switch (mnp->mn_hdr.mclass) {
    	case MFS_SDEVCLAS: 	
	    return(VCHR);
	case MFS_VOBRTCLAS:
	case MFS_VIEWDIRCLAS:
	case MFS_VIEWCLAS:	
	case MFS_NTVWCLAS:
	    return(VDIR);
	case MFS_VOBCLAS:
	    return(mfs_ftype_to_vtype(mnp->mn_vob.attr.fstat.type)); 
	case MFS_LOOPCLAS:
	    ASSERT(mnp->mn_hdr.realvp);
	    return(MVFS_GETVTYPE(MVFS_CVP_TO_VP(mnp->mn_hdr.realvp)));
   	default: 
	    MDKI_PANIC("mvfs: Unknown mnode type");
    }

    return(VNON);		/* never reached */
}

/* For Linux, which is neither FSS nor strict vnode as described here (it's
 * somewhere in-between), this description might be confusing. */

/*
 * MAKENODE calls - make MFS nodes of the appropriate types
 *
 * MAKENODE PROTOCOL:
 *	To make the makenode protocol MP safe all steps are protected
 *	by various locks.  To make the protocol portable, the model used
 *	is the worst case (for FSS systems), where the vnode and mnode
 *	are in two separate pieces which must be linked up.  This makes
 *      for a 3-phase protocol.  For portability the protocol only uses
 *	simple mutual exclusion locks, and tries to piggy back MP
 *      support on locks that are required for uni-processor support anyways.
 *	Any operations that might not be in someone's MP kernel (e.g.
 *	an atomic unlock spinlock to sleep primitive) are avoided.
 *	The macros are implemented as "recursive locks" that panic
 *	on recursion so that I get dumps instead of hangs.
 *
 *	There are two confusing lock names here due to some history.
 *	The "mnode monitor lock" is a single lock in mfs_mnode.c that
 *	protects the mnode hash tables, freelist, etc.  The "mnode lock"
 *	is a mutual exclusion lock in the mnode itself which
 *	is used to protect access to fields within the mnode itself.
 *
 *	The 3 phases are:
 *	    1) Mnode find/create in the mnode hashes
 *	    2) Mnode update/initialization with current info
 *	    3) Mnode "linkup" with vnode and hold of vnode.
 *
 *      When deactivating vnodes, the phases are reversed (with phase
 *      2 omitted) e.g.
 *          - Unlink vnode (inode) from mnode if no vnode references
 *	    - Free/destroy mnode if no references
 *
 *      The first two phases are handled the same for both vnode
 *      and FSS systems.  The 3'rd phase is fairly different for
 *      the two systems.  As of 01/07/92 the routines are a little
 *	twisted but the basic outline is as follows:
 *          Phase 1 is done in mfs_mnget.  Mnode ptrs
 *		are protected by a "refcount" scheme.
 *	    Phase 2 lock is acquired in mfs_mnget.
 *          Phase 2 is work done in the makexxxnode, iread etc. routine.
 *	    Phase 2 lock is released in MVFS_VNGET
 *	    Phase 3 is done in either mvfs_vnget (for vnodes) or
 *		mfs_fss_vnget (in fsswrap) for FSS systems (via
 *		a macro MVFS_VNGET to pick the right routine).
 *	        Mnode refcount (from phase 1) is dropped after the vnode
 *		(inode) refcount has been incremented (the vnode holds
 *	        an mnode refcount due to vp->v_data mnode ptr).
 *
 *
 *      Phases in more detail:
 *      Phase 1: Activation of mnode
 *	   This phase is protected
 *	   by the mfs_mnlock (mnode monitor lock) which protects
 *	   the mnode hash tables, freelists, mnum_to_mnode table etc.
 *	   This lock also protects the "mnode reference count" in
 *	   the mnode itself.  It also protects all the non-vnode ptrs
 *	   in the mnode hdr.
 *  	    
 *         In phase 1, the mnode hash tables are searched to locate
 *	   the mnode (under the mnode monitor lock).  
 *	   If this succeeds (e.g. mnode found):
 *		- remove from freelist (if on the freelist)
 *	   	- mnode refcount is incremented.  This guarantees that
 *		  the mnode will not be freed/deallocated even when
 *		  the mnode monitor lock is released.
 *		- release the mnode monitor lock
 *		- acquire the mnode lock for phase 2 
 *	          This is done in mnget for consistency, mnget always 
 *		  returns with the mnode lock held.
 *	   If the mnode is not found:
 *		- allocate new storage (holding the mnode monitor lock)
 *		  for the new mnode.
 *		- initialize the hdr, setting mnode refcount = 1
 *		  AND WITH THE MNODE LOCK ACQUIRED.  This later point
 *		  with the protocol above guarantees the consistency
 *		  of phase 2.
 *	        - add the mnode to the hash buckets.
 *		- release the mnode monitor lock.
 *
 *	   At the end of phase 1 we have a mnode which is safe from
 *	   deallocation (due to refcount), and is locked so that we
 *	   can modify mnode-specific fields as necessary safely.  The mnode
 *	   lock prevents anyone from seeing unitialized new mnodes.
 *         (The other strategy of not hashing a new mnode and then going
 *	   back to hash it later is actually more expensive than the
 *	   lock because it requires a second search of the hash chains 
 *	   to detect races on adding the mnode).
 *
 *	Phase 2:  Initialize/update mnode.
 *	   The routine (makenode, iread etc.) fills in any mnode fields
 *	   required.  These are different for different types of mnodes.
 *	   The mnode lock is held throughout.
 *
 *      Phase 3:  Linkup to vnode/increment vnode refcount.
 *	   Phase 3 is different for vnode vs. FSS and so there are two
 *	   descriptions here.  Basically what phase 3 does is
 *	   either:
 *	       mnode attached to vnode: 
 *		    bump v_count, release phase 1 mnode refcount.
 *             	    (the mnode is held by a refcount for the v_data
 *		    ptr to the mnode). Vnode refcount is protected
 *		    by whatever means the MP OS uses in VN_HOLD macro.
 *	       mnode not attached to vnode:
 *		    Attach vnode to mnode by setting v_data, and mnode
 *		    back ptr.  LEAVE the refcount from phase 1 (do not
 *		    release it) to reflect this mnode ptr.
 *		    Initialize the vnode fields, including the setting
 *		    of v_count to 1.  Noone can be trying to VN_HOLD
 *		    at this point, since you can only VN_HOLD a vnode
 *		    you already have, and this vnode ptr is not available
 *		    yet until we finish this protocol.
 *
 *	Phase 3 FSS:
 *	   - The mnode lock is released.  
 *	     The mnode lock can not be held
 *	     when calling iget() to get the inode because iget() locks
 *           the inode, and lock ordering is always inode->mnode.  Any
 *	     reverse ordering would cause deadlocks.  (Mnode is still safe
 *	     due to mnode refcount).
 *
 *	   The FSS routine iget() handles all races in activation/
 *	   deactivation of inodes from the inode table based on
 *	   refcount.  There are two cases:
 *	    	Inode found in the inode hashes:
 *		The inode must be active and associated with an mnode
 *		for this to happen.
 *		   - iget will bump the refcount on the inode and return.
 *		   - fss_vnget calls mfs_mnrele() to release the mnode
 *		     refcount from phase 1.
 *		Inode not found in inode hashes:
 *		   - iget will call mfs_iread() to associate mnode with
 *		     inode.  mfs_iread() goes through phases 1 and 2
 *		     again (2 is usually a NOP here), leaving the extra
 *		     mnode refcount to account for the mnode ptr in the
 *		     inode.
 *		   - iget returns an inode with refcount=1
 *		   - fss_vnget will release the mnode refcount acquired
 *		     in phase 1 leaving 1 refcount for the mnode ptr in
 *		     the vnode.
 *
 *	Phase 3 Vnode:
 *	   This section may seem a bit funny to those who know the
 *	   vnode is really part of the mnode structure (the vnode/mnode
 *	   are allocated at the same time in contiguous memory), but
 *	   for consistency of protocol, they are treated as if they
 *	   were separate.
 *
 *	   - the mnode lock IS RETAINED to protect the linkup/hold
 *	     of the vnode as necessary.  This allows the lock to do
 *	     double duty and avoids extra locking.
 *
 *         There are two cases at this point:
 *	       The mnode to vnode back-ptr is non-NULL.  This indicates
 *	       an active (non-zero refcount) vnode.
 *		   - bump the vnode refcount
 *		   - unlock the mnode lock
 *	           - call mfs_mnrele() to release the mnode refcount 
 *                   acquired in phase 1
 *	       The mnode to vnode back-ptr is NULL.  This indicates that
 *	       the mnode was on the freelist (e.g. vnode refcount was 0).
 *		   - set the mnode back-ptr and vnode fwd ptr (v_data).
 *		   - initialize the vnode as needed with refcount = 1.
 *		   - unlock the mnode lock.
 *		   - LEAVE (do not relase) the mnode refcount. This refcount
 *		     reflects the vnode fwd ptr.
 *
 * DEACTIVATION:
 *
 *    Deactivation is basically the reverse.
 *
 *
 *    Vnode:
 *	mfs_inactive is called whenever the vnode refcount goes to 0.
 *	Since mfs_inactive does operations thay may involve rpc's and
 *	waiting, the mnode is not held locked all the time.  At the
 *	end of mfs_inactive:
 *	   - acquire the mnode lock.
 *	   - check the vnode refcount.  Once the vnode count went to
 *	     0, the only way to get it back to 1 is to go through the
 *	     activation protocol above (a refcount of 0 means no ptrs
 *	     are around to the vnode - you have to get one via makexxxnode).
 *	     Thus the 0->1 activation vs. 0->freelist deactivation is
 *	     always protected by the protocol herein, and that is the only 
 *	     vnode refcount transitions we care about.
 *
 *	     If v_count == 0 the vnode should really be released
 *	     (no one did VN_HOLD's via activations above) so:
 *		- clear the mnode back ptr, vnode fwd ptr. i.e. unlink
 *		  the vnode/mnode from each other under mnode lock.
 *		- unlock the mnode lock.
 *		- call mfs_mnrele() to drop the mnode refcount because
 *		  we destroyed the vnode fwd ptr.  Mnode may get freed
 *	          or destroyed if mnode refcount goes to 0.
 *	    If v_count > 0 somone must have gone through the activation
 *	    protocol to get this same mnode/vnode pair and bump the hold
 *	    count.  
 *		- unlock the mnode lock.
 *		- return.  Vnode was saved while we slept
 */

int
mfs_makevobrtnode(vw, vfsp, vpp)
VNODE_T *vw;
VFS_T *vfsp;
VNODE_T **vpp;
{
    mfs_mnode_t *mnp;
    VNODE_T *vp;
    int newnode;
    mfs_fid_t fid;
    int error;

    MDB_CHKPOINT(0);

    if (vw == NULL) {
	fid.mf_dbid = MVFS_ROOTDBID;
	fid.mf_gen = MVFS_ROOTGEN;
    } else {
	/* Check for stale view passed in */
	if (VTOM(vw)->mn_view.id == 0) {
	    *vpp = NULL;
	    return(ESTALE);
	}
	fid.mf_dbid = MVFS_ROOTDBID;
	fid.mf_gen = MVFS_ROOTGEN;
    }
    mnp = mfs_mnget(MFS_VOBRTCLAS, vw, &fid, vfsp, &newnode);
    if (mnp == NULL) {
	*vpp = NULL;
	return(ENOMEM);
    }

    /* Get the vnode.  This macro must be called with the
       mnode locked, and returns with the mnode unlocked and
       the vnode attached, initialized vnode refcount incremented,
       and mnode refcount decremented. */

    error = MVFS_VNGET(vfsp, NULL, mnp, &vp);

    ASSERT(error || (MFS_VIEW(vp) == vw));

    MDB_XLOG((MDB_MKNOD,"vobrt: vp=%"KS_FMT_PTR_T",vw=%"KS_FMT_PTR_T",vfsp=%"KS_FMT_PTR_T",err=%d\n",vp,vw,vfsp,error));
    MDB_CHKPOINT(0);

    *vpp = vp;
    return (error);
}

/* MFS_MAKEVOBNODE - make a vob vnode */

int
mfs_makevobnode(
    view_vstat_t *vstatp,	/* Object stat record */
    struct timeval *lvut,	/* view's LVUT */
    VNODE_T *vw,	        /* View object is in (may be NULL) */
    view_fhandle_t *vfhp,	/* Optional view file handle ptr */
    VFS_T *vfsp,	        /* VFS ptr */
    CRED_T *cred,		/* callers credentials */
    VNODE_T **vpp	        /* Returned vnode ptr */
)
{
    int error;
    int newnode;
    VNODE_T *vp;
    mfs_mnode_t *mnp;
    mfs_fid_t fid;
    VNODE_T *hmvw = NULL;
    int hmwarp;
    int modflags;

    MDB_CHKPOINT(0);
    ASSERT(vfhp);
    ASSERT(vw);

    /* 
     * Check for warp from non-history mode to history mode
     */

    hmwarp = (MFS_HMVFH(vfhp) && !VTOM(vw)->mn_view.hm) ? 1 : 0;
    if (hmwarp) {
        error = mfs_viewdirhmview(vw, &hmvw, cred);
	if (error) { 
	    vp = NULL;
	    goto out;
	}
	vw = hmvw;	/* Use HM view for rest */
    }
	
    /* Make the fid and then get the mnp struct */

    fid.mf_dbid = vfhp->ver_dbid;
    fid.mf_gen  = vfhp->gen;
    mnp = mfs_mnget(MFS_VOBCLAS, vw, (void *)&fid, vfsp, &newnode);
    if (mnp == NULL) {
	*vpp = NULL;
	return(ENOMEM);
    }

    /* If a "newnode" update the file handle */

    if (newnode) mnp->mn_vob.vfh = *vfhp;   /* Updates generation number too */

    /* Store attributes now if we have them to avoid any
       unneeded getattr calls.  For SVR3 this prevents
       an extra getattr RPC in iget->mfs_iread().  NOTE:
       The mfs_getattr() call in mfs_iread does perform
       the sync of the inode attributes with the mnp attributes
       by calling MFS_UPDATE_IATTR().  This is necessary or
       NFS access will break on getting wrong attributes after
       link/unlink etc. */

    if (vstatp) {
	if (lvut) {
	    mnp->mn_vob.lvut = *lvut;
	    modflags = mvfs_ac_set_stat(mnp, vstatp, TRUE, cred);
	} else {
	    mnp->mn_vob.lvut.tv_sec = mnp->mn_vob.lvut.tv_usec = 0;
	    modflags = mvfs_ac_set_stat(mnp, vstatp, FALSE, cred);
	}
    } else {
	modflags = 0;	/* Can't be modified */
    }

    /* Get the vnode.  This routine takes a locked mnode, and
       attaches to vnode, bumps vnode refcount etc. to return with
       an valid, refcounted vnode hooked up to an unlocked mnode. 
       It also drops the mnode refcount acquired by mnget. */

    error = MVFS_VNGET(vfsp, NULL, mnp, &vp);

    if (!error) {
	ASSERT(vp);
	ASSERT(VTOM(vp) == mnp);
	ASSERT(MTOV(mnp) == vp);
	ASSERT(MVFS_GETVTYPE(vp) == mfs_ftype_to_vtype(mnp->mn_vob.attr.fstat.type));

        /* 
	 * If not a new mnode and modified, process modified events!
	 */

        if (!newnode && (modflags != 0)) {
	    MLOCK(mnp);
	    (void) mfs_ac_modevents(vp, modflags, cred);
            /*
             * If the object changed (usually from cleartext COW),
             * flush the cleartext here (it might have come off the free
             * list).  It's safe to do so, since this is a reactivation from
             * an unopened state.
             * [We don't flush it when we detect it, in case the
             * cleartext is held open by some other process.]
             */
            if (modflags & MFS_DOMOD) {
#ifdef MVFS_DEBUG
                mvfs_log(MFS_LOG_DEBUG, "domod clear_rele vp=%"KS_FMT_PTR_T"\n", vp);
#endif
                mfs_clear_rele(vp, cred);
            }
	    MUNLOCK(mnp);
	}


        if (MFS_VIEW(vp) != vw) {
	    VN_RELE(MFS_VIEW(vp));
	    VTOM(vp)->mn_hdr.viewvp = vw;
	    MFS_HOLDVW(vw);
        }
    }

out:

    if (hmvw) VN_RELE(hmvw);	/* If got a hist mode view, release now */

    MDB_XLOG((MDB_MKNOD,"vob: vp=%"KS_FMT_PTR_T",vfsp=%"KS_FMT_PTR_T",err=%d\n",vp,vfsp,error));
    MDB_CHKPOINT(0);

    *vpp = vp;
    return (error);
}

/*
 * MFS_MAKELOOPNODE - make a loopback node only if required to
 *	not lose the view extended naming state.  This call
 *	must be called with the "bound root" of a VOB, never
 *	the "unbound root" at the VOB root.  (The unbound
 *	root has no curview state to inherit!)
 *
 *      This routine can return a new loopback vnode in vpp or
 *      the "realvp" itself.  In either case, the returned
 *      vnode (if no error) will have its hold count incremented.
 *	This routine will never VN_RELE the realvp or dvp.
 */
int
mfs_makeloopnode(
    VNODE_T *vw,
    CLR_VNODE_T *realcvp,
    VNODE_T **vpp,
    CRED_T *cred
)
{
    int error;
    char *reason = "no reason";
    VNODE_T *realvp = MVFS_CVP_TO_VP(realcvp);
    VNODE_T *setview;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    error = 0;
    *vpp = NULL;

    ASSERT(vw && MFS_ISVIEW(VTOM(vw)));

    /* Only cover dirs.  Never cover files/device/fifo/sockets.  */

    if (!MVFS_ISVTYPE(realvp, VDIR)) {
	reason = "not dir"; goto nocover;
    }

    /* 
     * Handle /view
     * /view:  		This comes in as the 'realvp' when looking up ".."
     *			from /view/tag.  We just want to return /view with
     *			out any view context (/view is view invariant!)
     */

    if (MFS_VPISMFS(realvp) && MFS_ISVIEWDIR(VTOM(realvp))) {
	reason = "/view"; goto nocover;
    }

    /*
     * /view/tag:	This comes in as the 'rootdir' of the system for
     *			the realvp.  We need to return the view-tag itself,
     *			so we don't lose the 'viewness' of an extended
     *			pathname.
     */

    if (realvp == ROOTDIR) {
	MFS_HOLDVW(vw);
	*vpp = vw;
	error = 0;
	goto out;
    }

    /*
     * /view/tag/...:	This case is for objects that are not inside
     *			a VOB.  Realvp is either a non-vob object
     *		        or it may be the VOB root (mount point).
     *		  
     *			We don't cover if the object is in the default view.
     *			When we use the VOB root to do lookups, the
     *			bindroot call will rediscover the setview view
     *			so that absolute pathnames (e.g. /vobs/atria) work.
     *
     */

    /* Get current setview view for decisions to cover or not. */

    setview = mfs_getview(NULL, cred, TRUE /* HOLD */);
    if (vw == setview && !VTOM(vw)->mn_view.always_cover &&
        !MVFS_MDEP_COVER_CVIEW())
    {
	if (setview) VN_RELE(setview);	/* Done with setview */
	reason="setview view";
        goto nocover;
    }
    if (setview) VN_RELE(setview);

    /* 
     * /view/tag/xxx/vobrt:	This case is for vobroots that
     *				are not in the current setview view.
     *				We construct a 'synonym' vob root which
     *				is tagged with the current view.
     *				This vw is then used in lookup for
     *				the bindroot operation.
     *
     */

    if (MFS_VPISMFS(realvp) && MFS_ISVOBRT(VTOM(realvp))) {

        /* We don't actually cover the VOB root, we just get another "VOB root"
        ** vnode with the correct view set in it.  This comment (from /main/2)
        ** used to say that setting the VROOT flag was required so lookuppn()
        ** knows that this is a root inode to handle ".." correctly.  However,
        ** setting the VROOT flag actually confuses the lookup of ".." causing,
        ** e.g. /bin/pwd, to not be able to "track up the chain" to find the
        ** view-extended name since it stops at the VROOT.  Thus, the code has
        ** always been correct (not setting the VROOT flag), but the comment was
        ** wrong.
        */
        error = mfs_makevobrtnode(vw,realvp->v_vfsp, vpp);
	goto out;
    }

    /*
     * make sure no-one is covering an MVFS node underneath.
     */
    error = MVFS_COVER_CHECK(realvp);
    if (error) goto out;

    /*
     * Make a cover inode for a non-atria object 
     * makespecnode takes its own ref on realcvp
     */
    error = mfs_makespecnode(MFS_LOOPCLAS, vw, realcvp,
			     MVFS_LOOPCLAS_VFSP(vrdp, realcvp,
                                                VTOM(vw)->mn_view.always_cover),
                             vpp);

out:
    MDB_XLOG((MDB_MKNOD,"LOOP: realvp=%"KS_FMT_PTR_T", vfsp=%"KS_FMT_PTR_T", rvp=%"KS_FMT_PTR_T", err=%d\n", 
	     realvp, 
	     MVFS_LOOPCLAS_VFSP(vrdp, realvp, VTOM(vw)->mn_view.always_cover), 
	     *vpp,error));
    MDB_CHKPOINT(0);
    return(error);

nocover:		/* Just hold and return the orig vnode. */
    VN_HOLD(realvp);
    *vpp = realvp;
    MDB_XLOG((MDB_MKNOD,"loop: realvp=%"KS_FMT_PTR_T", no cover reason=%s\n", realvp, reason));
    return(0);
}

/*
 * MFS_MAKESPECNODE - make any of the special nodes.
 *	MFS_LOOPCLAS    hand in valid view and realvp
 *	MFS_SDEVCLAS    vw and infop both NULL
 *	MFS_VIEWCLAS	    "
 *	MFS_VIEWDIRCLAS     "
 */
int
mfs_makespecnode(class, vw, infop, vfsp, vpp)
mfs_class_t class;
VNODE_T *vw;
void *infop;
VFS_T *vfsp;
VNODE_T **vpp;
{
    int error;
    VNODE_T *vp;
    mfs_mnode_t *mnp;
    int newnode;
    mfs_fid_t fid;
    mfs_fid_t *fidp;

    MDB_CHKPOINT(0);

    /* Get the existing MVI struct for this cleartext
       vnode, or create a new one.  This will get us
       the "inode number" we want to use.  If the file
       handle is passed in, try to locate via that.  Else
       the cleartext vnode is required, and we will locate
       by that means. */

    if (class == MFS_LOOPCLAS) {
	ASSERT(vw);
	ASSERT(infop);
	fid.mf_realvp = (CLR_VNODE_T *)infop;
        /*
         * Put the real vnode's vfsp in the fid, so that mnfind has
         * enough detail to find a matching extant loopclas mnode on
         * systems which clone the loopback vfsp.
         */
	fid.mf_realvfsp = MVFS_CVP_TO_VP(fid.mf_realvp)->v_vfsp;
	fidp = &fid;
    } else {
	if (infop) {
	    fid.mf_mnum = *(u_long *)infop;
	    fid.mf_gen  = 0;
	    fidp = &fid;
	} else {
	    fidp = NULL;
	}
    }

    mnp = mfs_mnget(class, vw, fidp, vfsp, &newnode);
    if (mnp == NULL) {
	*vpp = NULL;
	return(ENOMEM);
    }

    /* Go from mnode to vnode. */
    error = MVFS_VNGET(mnp->mn_hdr.vfsp, vfsp, mnp, &vp);

    MDB_XLOG((MDB_MKNOD,"spec: vp=%"KS_FMT_PTR_T",vfsp=%"KS_FMT_PTR_T",err=%d\n",vp,vfsp,error));
    MDB_CHKPOINT(0);

    *vpp = vp;
    return(error);
}

/*
 * MFS_REBIND_SELF - make a vnode "rebind" cache point to itself
 * for the current build handle.
 */

void
mfs_rebind_self(
    VNODE_T *vp,
    CALL_DATA_T *cd
)
{
    register mfs_mnode_t *mnp;
   
    mnp = VTOM(vp);
    ASSERT(MFS_ISVOB(mnp));

    /* 
     * Set rebind cache to be valid for self.
     */
    MLOCK(mnp);

    mnp->mn_vob.rebind.valid = 1;
    mnp->mn_vob.rebind.self = 1;
    mnp->mn_vob.rebind.bh = MVFS_MYTHREAD(cd)->thr_bh;

    MUNLOCK(mnp);

    return;
}

/*
 * MFS_REBIND_VPP - routine to rebind a dir vnode if it is stale.
 * Used for rebinding the current working dir.  The most common
 * cause of a "stale cwd" is checkout/checkin of ".".
 *
 * Rebinding of u_cdir can only be done for lookup types of operations.
 * On these operation it is safe to change u_cdir because callers expect
 * a return vnode to use and aren't holding onto an active vnode, so changing 
 * the world is both safe and effective.  It is not effective, and possibly
 * not safe to change u_cdir on lookup type ops because the caller may 
 * be calling in with a saved vnode from an open which you had better not
 * rebind underneath him (e.g. readdir, getattr, etc.)... but we do it
 * anyways because it is required for "globbing" in the shell which
 * looks up the cwd without any lookup op (at the MFS level) because
 * the higher levels shortcut lookups of "." or "".
 *
 * This return returns '1' if it rebound the vnode passed in.
 */

int
mfs_rebind_vpp(
    int release,
    VNODE_T **vpp,
    CALL_DATA_T *cd
)
{
    mfs_mnode_t *mnp;
    mvfs_thread_t *mth;
    VNODE_T *vp;
    VNODE_T *xvp = *vpp;
    mfs_fid_t fid;
    struct timeval evtime;
    int error;
    int rebound = 0;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if (!mcdp->mvfs_rebind_dir_enable) return(0);		/* global enable */

    ASSERT(vpp && *vpp);

    if (!MVFS_ISVTYPE(*vpp, VDIR)) return(0);		/* Dirs only */
    if (!MFS_VPISMFS(*vpp)) return(0);		/* MFS objects only */
    ASSERT(VTOM(*vpp)->mn_hdr.vp);
    /* 
     * Don't rebind if "audit inhibit"  or explicit "rebind inhibit" is on.  
     * For auditing we want to only work on the original object
     * passed in and not a "rebound" one.
     */
    mth = MVFS_MYTHREAD(cd);
    if (mth->thr_rebindinh > 0) return(0);

    mnp = VTOM(*vpp);
    if (!MFS_ISVOB(mnp)) return(0);		/* VOB objects only */

    /* 
     * Get latest attributes to check for vob events and need
     * to rebind.  If get "ESTALE" this may be a "uncheckout ." case
     * where the current dir has been removed... so just go revalidate
     * the dir the hard way.
     */

    if (mfs_ac_timedout(mnp, FALSE, cd)) {
	error = mfs_clnt_getattr(*vpp, cd);
	if (error == ESTALE || error == EIO) {
	    mvfs_logperr(MFS_LOG_DEBUG, error, 
		"rebind: getattr vw=%s vob=%s dbid=0x%x", 
			mfs_vp2vw(*vpp), mfs_vp2dev(*vpp), mfs_vp2dbid(*vpp));
	    MLOCK(mnp);
	    goto hardway;
	}
        if (error) {
	    mvfs_logperr(MFS_LOG_WARN, error, 
		"rebind: getattr: vw=%s vob=%s dbid=0x%x", 
			mfs_vp2vw(*vpp), mfs_vp2dev(*vpp), mfs_vp2dbid(*vpp));
	    return(0);
	}
	BUMPSTAT(mfs_acstat.ac_misses);
	BUMPVSTAT(*vpp,acstat.ac_misses);
    }
    
    /*
     * Use the "rebind version" cache as a fast path for previously 
     * rebound vnode.
     */

    MLOCK(mnp);
    if (mnp->mn_vob.rebind.valid && 
			MFS_BH_SAMECONFIG(mth->thr_bh,
					  mnp->mn_vob.rebind.bh)) {

	/* Check for rebind to same version */

	if (mnp->mn_vob.rebind.self) {
	    MUNLOCK(mnp);
            MDB_XLOG((MDB_REBIND,"pid=%d bh %x.%x vp=%"KS_FMT_PTR_T" rebound self\n", MDKI_CURPID(), mth->thr_bh.build_session, mth->thr_bh.target_id, *vpp));
	    return(0);
	} else {
  	    fid = mnp->mn_vob.rebind.fid;
	    evtime = mnp->mn_vob.rebind.evtime;
	    MUNLOCK(mnp);
	    error = mfs_getvnode(mnp->mn_hdr.vfsp, mnp->mn_hdr.viewvp,
			&fid, &vp, cd);
	    if (!error) {
		ASSERT(MFS_VPISMFS(vp));
		ASSERT(MFS_ISVOB(VTOM(vp)));
		if (!MFS_TVEQ(VTOM(vp)->mn_vob.attr.event_time, 
			evtime)) {
		    VN_RELE(vp);
		    vp = NULL;
		    MLOCK(mnp);
		    goto hardway;	/* Do it the hard way */
		}
	    } else {
		mvfs_logperr(MFS_LOG_DEBUG, error,
			"rebind: getvnode vw=%s vob=%s dbid=0x%x",
			   mfs_vp2vw(*vpp), 
			   mfs_vp2dev(*vpp), fid.mf_dbid);
		MLOCK(mnp);
		goto hardway;
	    }
	}
    } else {		/* Cache miss - do it the hard way */
hardway:
	MFS_REBINDINVAL(mnp);
	MUNLOCK(mnp);
	error = mfs_clnt_rebind_dir(*vpp, &vp, cd);
        if (error) {
	    mvfs_logperr(MFS_LOG_WARN, error, "rebind: vw=%s vob=%s dbid=0x%x",
		mfs_vp2vw(*vpp),
		mfs_vp2dev(*vpp), mfs_vp2dbid(*vpp));
	    goto errout;
	}
	ASSERT(MFS_ISVOB(VTOM(vp)));

	/* Check if rebinding got back same version */

	if (vp == *vpp) {	/* False alarm */
	    VN_RELE(vp);
	    mfs_rebind_self(*vpp, cd);
	    goto errout;	/* No need to rebind */
	} else {
	    MLOCK(mnp);
	    mnp->mn_vob.rebind.valid = 1;
            mnp->mn_vob.rebind.self = 0;
	    mnp->mn_vob.rebind.bh = mth->thr_bh;
	    mnp->mn_vob.rebind.fid = VTOM(vp)->mn_hdr.fid;
	    mnp->mn_vob.rebind.evtime = VTOM(vp)->mn_vob.attr.event_time;
	    MUNLOCK(mnp);
	}
    }

    /* Replace vnode passed in.  Release the old vnode if
       the release flag was passed */

    ASSERT(vp);
    if (release) {
	VN_RELE(*vpp);
    }
    *vpp = vp;
    rebound = 1;

errout:
    MDB_XLOG((MDB_REBIND,"pid=%d, bh %x.%x dvp %"KS_FMT_PTR_T" (%"KS_FMT_PTR_T") -> %"KS_FMT_PTR_T" (%"KS_FMT_PTR_T") error=%d\n", 
		MDKI_CURPID(), mth->thr_bh.build_session, 
		mth->thr_bh.target_id, xvp, VTOM(xvp), *vpp, VTOM(*vpp), error));
    return(rebound);
}

/*
 * MFS_RWLOCK - lock the vnode/mnode from higher level
 * Called from read/write code.
 */
void
mfs_rwlock(vp, w)
VNODE_T *vp;
int w;
{
    MLOCKHI(VTOM(vp));
}

/* MFS_RWUNLOCK  - unlock vnode/mnode */
void
mfs_rwunlock(vp,w)
VNODE_T *vp;
int w;
{
    MUNLOCKHI(VTOM(vp));
}

/* 
 * MFS_SEEK - seek on character device
 */
int
mfs_seek(vp, ooff, noffp)
VNODE_T *vp;
MOFFSET_T ooff;
MOFFSET_T *noffp;
{
    return mvfs_seek_ctx(vp, ooff, noffp, NULL);
}

int
mvfs_seek_ctx(
    VNODE_T *vp,
    MOFFSET_T ooff,
    MOFFSET_T *noffp,
    MVFS_SEEK_CTX_T *ctxp
)
{
    MOFFSET_T max_offset;

/*    BUMPSTAT(mfs_vnopcnt[MFS_VSEEK]);*/
    ASSERT(VTOM(vp)->mn_hdr.vp);
    if (VTOM(vp)->mn_hdr.mclass == MFS_VIEWCLAS ||
        VTOM(vp)->mn_hdr.mclass == MFS_LOOPCLAS)
    {
	/* Pass on to real vnode */
          return MVOP_SEEK(MFS_REALVP(vp), ooff, noffp, ctxp);
    }

    max_offset = MVFS_GET_MAXOFF_SEEK_CTX(vp, ctxp);

    return ((MVFS_IS_INVALID_OFFSET(*noffp) || (*noffp > max_offset)) ? EINVAL : 0);
}

/*
 * MFS_NOERR - no op vnode routine
 */
int
mfs_noerr(vp)
VNODE_T *vp;
{
    return(0);
}

/*
 * MFS_NOVAL - no op vnode routine, returns no value
 */
void
mfs_noval(vp)
VNODE_T *vp;
{
}

dev_t
mvfs_devadjust(dev_t dev, VNODE_T *vp)
{
    register struct mfs_mnode *mnp = VTOM(vp);
    register VNODE_T *vw;

    vw = MFS_VIEW(vp);
    /*
     * If there is a view available for this node, then make the
     * device number reflect that view as best we can, by stuffing the
     * viewid in the remainder of the minor device number.
     *
     * mdep files define MVFS_VIEW_SHIFT_BITS and MVFS_VIEW_MASK_BITS
     * to indicate how many bits to shift and mask the view id with
     * before ORing into the minor number.
     *
     */
    if (vw)
        dev = MDKI_MAKEDEVICE(MDKI_MAJOR(dev),
			      MDKI_MINOR(dev)|((VTOM(vw)->mn_view.id & MVFS_VIEW_MASK_BITS) << MVFS_VIEW_SHIFT_BITS));
    return dev;

}

static const char vnode_verid_mvfs_vnodeops_c[] = "$Id:  94c2efe0.45b111e0.9a57.00:11:25:23:c8:f1 $";
