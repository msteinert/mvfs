/* * (C) Copyright IBM Corporation 1991, 2007. */
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

/*
 */

#include "mvfs_audit.h"
#include "mvfs_base.h"
#include "mvfs_systm.h"
#include "mvfs.h"
#include <tbs_errno.h>
#include "mvfs_copy.h"
#include "mvfs_transtype.h"

mvfs_audit_data_t mvfs_audit_data_var;

STATIC MVFS_NOINLINE mfs_auditfile_t *
mfs_afpnew(
    char *pname,
    char *upname,
    CLR_VNODE_T *cvp
);
STATIC void MVFS_NOINLINE
mfs_afpdestroy(
    mfs_auditfile_t *afp
);
STATIC MVFS_NOINLINE struct mfs_auditfile *
mfs_afpget(
    char *pname,
    char *upname,
    CLR_VNODE_T *cvp
);
STATIC void mfs_afp_obsolete(P1(mvfs_thread_t *mth));
STATIC int mfs_cmpdirrec(P1(mfs_auditrec_t *rp)
			 PN(u_int kind)
			 PN(VNODE_T *dvp)
			 PN(char *nm)
			 PN(size_t nmlen)
			 PN(VNODE_T *vp)
			 PN(VATTR_T *vap));
STATIC int mfs_cmprwrec(P1(mfs_auditrec_t *rp)
			PN(u_int kind)
			PN(VNODE_T *vp)
			PN(VATTR_T *vap));
STATIC int mfs_cmpviewrec(P1(mfs_auditrec_t *rp)
			  PN(VNODE_T *vp));
STATIC int mfs_isdupl(P1(mfs_auditfile_t *afp)
		      PN(u_int kind)
		      PN(VNODE_T *dvp)
		      PN(char *nm)
		      PN(size_t nmlen)
		      PN(VNODE_T *vp)
		      PN(VATTR_T *vap));
STATIC void mvfs_auditwrite_int(P1(mfs_auditfile_t *afp)
			       PN(mvfs_thread_t *mth));

/*
 * Initialize the structures for managing the audit subsystem
 */
int
mvfs_auditinit(mvfs_cache_sizes_t *mma_sizes)
{
    mvfs_audit_data_t *madp;

    MDKI_AUDIT_ALLOC_DATA();
    madp = MDKI_AUDIT_GET_DATAP();

    INITLOCK(&(madp->mfs_aflock), "mfsaflck");
    INITSPLOCK(madp->mvfs_audgenlock,"mvfs_audgenlock");
    INIT_LIST(madp->mfs_aflist, mfs_auditfile_t);
#ifdef MVFS_DEBUG
    madp->mvfs_debug_no_audit_detail = 0;
#endif
    madp->mvfs_audit_gen = 0;

    return 0;
}

void
mvfs_auditfree()
{
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();

    FREELOCK(&(madp->mfs_aflock));
    FREESPLOCK(madp->mvfs_audgenlock);

    /* Free the memory for the mvfs_audit_data_t if it was allocated. */
    MDKI_AUDIT_FREE_DATA();

}
    
/* 
 * Utility routines to manipulate the audit file struct 
 */

/* 
 * MFS_AFPNEW - create an afp struct 
 */

STATIC MVFS_NOINLINE mfs_auditfile_t *
mfs_afpnew(
    char *pname,
    char *upname,
    CLR_VNODE_T *cvp
)
{
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();
    mfs_auditfile_t *afp;

    ASSERT(ISLOCKED(&(madp->mfs_aflock)));	/* Need lock to avoid races */

    afp = (mfs_auditfile_t *)KMEM_ALLOC(sizeof(mfs_auditfile_t), KM_NOSLEEP);
    if (afp != NULL) {
	BZERO(afp, sizeof(*afp));
        afp->buf = (mfs_auditrec_t *)KMEM_ALLOC(mvfs_auditbufsiz, KM_NOSLEEP);
        if (afp->buf == NULL) goto errout;
	afp->buflen = mvfs_auditbufsiz;
	afp->path = STRDUP(pname);
	if (afp->path == NULL) goto errout;
	afp->upath = STRDUP(upname);
	if (afp->upath == NULL) goto errout;

	/* Save credentials of "creating" process.  These are used
	   for all writes to the file.  Prevents trojan horses from
	   using a filename and then executing a setuid program
	   which has greater rights. */

	afp->cred = MVFS_DUP_DEFAULT_CREDS();
	afp->cvp = cvp;
	CVN_HOLD(cvp);	/* Hold for this reference */
	INITLOCK(&afp->lock, "afplock ");
	afp->curpos = afp->buf;
   	afp->refcnt = 1;
	ADD_TO_EOL(&(madp->mfs_aflist), afp);
    }

    return(afp);

errout:
    if (afp) {
	if (afp->buf) KMEM_FREE(afp->buf, afp->buflen);
	if (afp->path) STRFREE(afp->path);
	if (afp->upath) STRFREE(afp->upath);
	if (afp->cvp) CVN_RELE(afp->cvp);
	KMEM_FREE(afp, sizeof(*afp));
    }

    return(NULL);
}

/* 
 * MFS_AFPGET - find and existing or create a new afp struct 
 *	Note that the "key" is only the pathname.  If the user
 *	uses different pathnames to the same file, the audits
 *	will not be synchronized, and the audit output file may
 *	contain trash.
 */

STATIC MVFS_NOINLINE struct mfs_auditfile *
mfs_afpget(
    char *pname,
    char *upname,
    CLR_VNODE_T *cvp
)
{
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();
    register struct mfs_auditfile *afp;

    MVFS_LOCK(&(madp->mfs_aflock));		/* lock AF list */
    for (afp = madp->mfs_aflist.next; 
			afp != (mfs_auditfile_t *)&(madp->mfs_aflist); 
			afp = afp->next) {
	/*
	 * Only reuse audit entry if both the same pathname
	 * and the same vnode (from the lookup of that pathname).
	 * It is possible to have entries that have not been
	 * freed yet because some daemon was started under the build.
	 * e.g. clearmake creates /tmp/auditfile
	 *    long-running daemons starts and keeps running.
	 * clearmake terminates audit, and does audit for /tmp/auditfile
	 * clearmake deleted (unlinks) /tmp/auditfile
	 *    (note that long-runnin daemon still has vnode open).
	 * clearmake creates new scratch auditfile at /tmp/auditfile
	 *    but this is a differenct object (and vnode), even
	 *    though the name is the same.
 	 */
	if ((STRCMP(pname, afp->path) == 0) &&
		CVN_CMP(cvp, afp->cvp) &&
		(STRCMP(upname, afp->upath) == 0)) {
	    afp->refcnt++;
	    afp->obsolete = 0;		/* Can't be obsolete. */
	    MVFS_UNLOCK(&(madp->mfs_aflock));
	    MDB_XLOG((MDB_AUDITF, "afpget: afp=%"KS_FMT_PTR_T" refcnt=%d pid=%d\n",
		afp, afp->refcnt, MDKI_CURPID()));
	    return(afp);
	}
    }

    /* No previous found, allocate a new struct */

    afp = mfs_afpnew(pname, upname, cvp);
    MVFS_UNLOCK(&(madp->mfs_aflock));
    MDB_XLOG((MDB_AUDITF, "afpget: (new) afp=%"KS_FMT_PTR_T" refcnt=%d pid=%d\n",
		afp, afp->refcnt, MDKI_CURPID()));
    return(afp);
}

STATIC void MVFS_NOINLINE
mfs_afpdestroy(
    mfs_auditfile_t *afp
)
{
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();

    ASSERT(ISLOCKED(&(madp->mfs_aflock)));
    ASSERT(afp->refcnt <= 1);	    /* No destroy if more refs */

    MDB_XLOG((MDB_AUDITF, "afpdestroy: afp=%"KS_FMT_PTR_T" refcnt=%d pid=%d\n",
		afp, afp->refcnt, MDKI_CURPID()));

    RM_LIST(afp);
    FREELOCK(&afp->lock);
    if (afp->cvp) CVN_RELE(afp->cvp);
    if (afp->path) STRFREE(afp->path);
    if (afp->upath) STRFREE(afp->upath);
    if (afp->buf)  KMEM_FREE(afp->buf, afp->buflen);
    if (afp->cred) MDKI_CRFREE(afp->cred);
    MVFS_FREE_VATTR_FIELDS(&afp->va);
#ifdef MVFS_DEBUG
    BZERO(afp, sizeof(afp));
#endif
    KMEM_FREE(afp, sizeof(*afp));
}

void
mfs_afphold(afp)
mfs_auditfile_t *afp;
{
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();

    MVFS_LOCK(&(madp->mfs_aflock));
    afp->refcnt++;
    MVFS_UNLOCK(&(madp->mfs_aflock));
    /* we can't debug print inside afphold() because of spin locks
     * held. by callers.  This debug code is now in the callers.
     */
/*    MDB_XLOG((MDB_AUDITF, "afphold: afp=%"KS_FMT_PTR_T" refcnt=%d pid=%d\n",
		afp, afp->refcnt, MDKI_CURPID())); */
}

/*
 * thr must be the caller's thread struct.  It's passed in for
 * efficiency (so we don't need to do a hash lookup to find it
 * ourselves. 
 */
void
mvfs_afprele(afp, thr)
mfs_auditfile_t *afp;
mvfs_thread_t *thr;
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();

    if (afp == NULL) return;

   /*
    * can't hold it over potential I/O or blocking memory allocation *
    * operations
    */
    ASSERT(NOTLOCKEDBYME(&(mcdp->proc_thr.mvfs_proclock))); 

    MDB_XLOG((MDB_AUDITF, "afprele: afp=%"KS_FMT_PTR_T" refcnt=%d pid=%d\n",
		afp, afp->refcnt, MDKI_CURPID()));

afp_race:
    if (afp->refcnt == 1) {   /* Sync contents */
	mvfs_auditwrite_int(afp, thr);
    }

    MVFS_LOCK(&(madp->mfs_aflock));

    if (afp->refcnt == 1) {
        if (afp->curpos != afp->buf) {
           /* Not empty, flush it, come down again and should be empty then */
           MVFS_UNLOCK(&(madp->mfs_aflock));
           goto afp_race;
        }
	mfs_afpdestroy(afp);
    } else {
	afp->refcnt--;
    }
    MVFS_UNLOCK(&(madp->mfs_aflock));
}

/* There are two versions because we need to release from both threads
   and processes */

/* This must be called with the thread's proc structure already locked */
void
mvfs_afprele_proc(proc, mythread)
mvfs_proc_t *proc;
mvfs_thread_t *mythread;
{
    mvfs_afprele(proc->mp_afp, mythread);
    proc->mp_afp = NULL;	/* Kill ptr */
}

void
mvfs_afprele_thr(thr)
mvfs_thread_t *thr;
{
    mvfs_afprele(thr->thr_afp, thr);
    thr->thr_afp = NULL;	/* Kill ptr */
}

STATIC void
mfs_afp_obsolete(mth)
mvfs_thread_t *mth;
{
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();

    if (mth->thr_afp == NULL) return;
    MVFS_LOCK(&(madp->mfs_aflock));
    mth->thr_afp->obsolete = 1;	/* Set obsolete flag */
    MVFS_UNLOCK(&(madp->mfs_aflock));
    return;
}

/*
 * MFS_GETBH - get current build handle
 */

view_bhandle_t
mfs_getbh()
{
    mvfs_thread_t *mth;

    mth = mvfs_mythread();
    return(mth->thr_bh);
}

/* 
 * MFS_AUDITIOCTL - ioctl's related to audits.  Note that the 
 * arg data has already been copied in (except for strbuf's) and
 * that any results will be copied out by mfs_mioctl() in vnodeops.
 */
	
int 
mfs_auditioctl(
    mvfscmd_block_t *kdata,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    mvfs_thread_t *mth;
    tbs_status_t tbsstatus = TBS_ST_OK;
    int error = 0;
    int s;
    timestruc_t stime;	/* For statistics */
    timestruc_t dtime;

    MDKI_HRTIME(&stime);

    mvfs_procpurge(MVFS_PROCPURGE_SLEEP); /* Clean up any dead proc state */

    /* mfs_mioctl() validated the call and copied in cmdblock; we 
     * copy in data here, and convert for LP64 if necessary 
     */

    mth = mvfs_mythread();		/* Get my process state */

    switch (MCB_CMD(kdata)) {
	case MVFS_CMD_GET_BH: {
	    auto mvfs_bhinfo_t bhinfo;

            if ((error = CopyInMvfs_bhinfo(kdata->infop, &bhinfo, callinfo)) != 0)
		break;

	    bhinfo.bh = mth->thr_bh;
	    bhinfo.bh_ref_time = mth->thr_bh_ref_time;
	    bhinfo.flags = 0;
	    if (mth->thr_usereftime) bhinfo.flags |= MVFS_BHF_DNC_REFTIME;
	    if (mth->thr_attrgen != 0) bhinfo.flags |= MVFS_BHF_REVALIDATE;
            error = CopyOutMvfs_bhinfo(&bhinfo, kdata->infop, callinfo);
	    break;
	}

	case MVFS_CMD_SET_BH: {
	    auto mvfs_bhinfo_t bhinfo;

            if ((error = CopyInMvfs_bhinfo(kdata->infop, &bhinfo, callinfo)) != 0)
                break;
            kdata->status = TBS_ST_OK;   

	    tbsstatus = MVFS_ESTABLISH_FORK_HANDLER();
	    if (tbsstatus != TBS_ST_OK) break;
	 

	    mth->thr_bh = bhinfo.bh;
	    mth->thr_bh_ref_time = bhinfo.bh_ref_time;
            mth->thr_usereftime = ((bhinfo.flags & MVFS_BHF_DNC_REFTIME) != 0);
	    /* 
	     * Update attributes generation if requested 
	     * Note: mfs_mn_newattrgen gets the mnode monitor lock (mfs_mnlock)
	     */
	    if ((bhinfo.flags & MVFS_BHF_REVALIDATE) != 0) {
		mth->thr_attrgen = mfs_mn_newattrgen();
	    }
	    mvfs_sync_procstate(mth);
            MDB_XLOG((MDB_AUDITF, "newsetbh: bh=%x.%x ref=%"KS_FMT_TV_SEC_T_X".%"KS_FMT_TV_USEC_T_X" gen=%d pid=%d\n",
			bhinfo.bh.build_session, bhinfo.bh.target_id, 
			bhinfo.bh_ref_time.tv_sec, bhinfo.bh_ref_time.tv_usec,
			mth->thr_attrgen,
			MDKI_CURPID()));
	    break;
        }
	/*
	 * New invalidation based on generation tag.
	 * Forces refetch of all attributes from view for objects
	 * referenced by this process (and children from forks after
	 * this point!)
	 */
	case MVFS_CMD_REVALIDATE:
	    mth->thr_attrgen = mfs_mn_newattrgen();
	    mvfs_sync_procstate(mth);
	    break;

	case MVFS_CMD_GET_AFILE: {
	    auto mfs_strbufpn_t afile;

            if ((error = CopyInMfs_strbufpn(kdata->infop, &afile, callinfo)) != 0)
                break;
            kdata->status = TBS_ST_OK;

	    if (mth->thr_afp && mth->thr_afp->upath)
		error = mfs_copyout_strbufpn(afile, mth->thr_afp->upath);
	    else
		error = mfs_copyout_strbufpn(afile, NULL);
	    if (error != 0)
		break;
            error = CopyOutMfs_strbufpn(&afile, kdata->infop, callinfo);
	    break;
	}

	case MVFS_CMD_SET_AFILE: {
	    auto mfs_strbufpn_pair_t afile_info;
	    auto char *path;
	    auto char *upath;
	    auto CLR_VNODE_T *afvp;
	    auto SPL_T s;
	    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();

            if ((error = CopyInMfs_strbufpn_pair(kdata->infop, &afile_info, callinfo)) != 0)
                break;

            /* We're going to use the pathname strings that we just copied in
            ** pointers to, so we need to copy them in as well.  We could use
            ** the MFS_STRBUFPN_PAIR_COPYIN_STRS macro defined in
            ** sys/mfs_ioctl.h.  However, we really just want the pathnames
            ** (path and upath) and are no longer interested in the
            ** mfs_strbufpn_pair structure, so it makes sense to just copy in
            ** the strings.  A few notes...on Unix/Linux the
            ** MFS_STRBUFPN_PAIR_GET_KPN macro just returns the upn, so we are
            ** actually copying in the same string twice.  Also, at least one
            ** of our callee's (mfs_afpget) may copy the strings we pass in
            ** with STRDUP, so we may "permanently" use the the "duplicated"
            ** space.  However, we're willing to waste the space for coding
            ** simplicity.
            */
	    error = mfs_copyin_strbufpn(MFS_STRBUFPN_PAIR_GET_KPN(&afile_info), &path);
	    if (error) break;
	    error = mfs_copyin_strbufpn(MFS_STRBUFPN_PAIR_GET_UPN(&afile_info), &upath);
	    if (error) {
		STRFREE(path);
		break;
	    }

	    /* 
	     * Verify the file exists and is suitable for auditing 
	     * This is very important or deadlocks/crashes may
	     * result from unexpected recursion in the MFS!
             */

	    MFS_INHAUDIT(mth);
	    error = LOOKUP_AUDIT_FILE(path, &afvp, cred);
	    if (!error) {
		CLR_VNODE_T *rvp = NULL;
		if (MVOP_REALCVP(afvp, &rvp) == 0 && afvp != rvp) {
		    /*
		     * got a realvp through the operation, use that instead.
		     */
                    CVN_HOLD(rvp);
		    CVN_RELE(afvp);
                    afvp = rvp;
		}
		if (MFS_VPISMFS(MVFS_CVP_TO_VP(afvp))) {
		    error = EINVAL;
		} else if (!MVFS_ISVTYPE(MVFS_CVP_TO_VP(afvp), VREG)) {
		    error = EISDIR;
		} else {
		    error = MVOP_ACCESS(MVFS_CVP_TO_VP(afvp), VWRITE, 0, cred, NULL);
		}
	        if (error) CVN_RELE(afvp);
	    }
	    MFS_ENBAUDIT(mth);
	    if (error) {
                MDB_XLOG((MDB_AUDITF, "setafile: path=%s error=%d pid=%d\n",
				path, error, MDKI_CURPID()));
		STRFREE(path);
		STRFREE(upath);
		break;
	    }

	    tbsstatus = MVFS_ESTABLISH_FORK_HANDLER();
	    if (tbsstatus != TBS_ST_OK) break;
	 
	    /* File is suitable, release old file binding and
	       try to attach new file. */
	
	    if (mth->thr_afp != NULL) {
	        mvfs_afprele_thr(mth);
	    }
	    mth->thr_afp = mfs_afpget(path, upath, afvp);
	    if (mth->thr_afp == NULL) error = ENOMEM;

	    /*
	     * Assign a new audit generation number
	     * to force a unique choid for every different
 	     * audit file (on DO's).
	     */

	    SPLOCK(madp->mvfs_audgenlock, s);
	    madp->mvfs_audit_gen++;	      /* Protected by mvfs_audgenlock */
	    mth->thr_aud_seq = madp->mvfs_audit_gen; /* State for this process */
	    SPUNLOCK(madp->mvfs_audgenlock, s);

	    mvfs_sync_procstate(mth);

	    CVN_RELE(afvp);	/* afpget held audit file vnode if needed */
            MDB_XLOG((MDB_AUDITF, "setafile: path=%s afp=%"KS_FMT_PTR_T" err=%d pid=%d\n",
		path, mth->thr_afp, error, MDKI_CURPID()));
	    STRFREE(path);
	    STRFREE(upath);
	    break;
	}

	case MVFS_CMD_GET_PROCF: {
	    auto u_long procflags;  

	    procflags = 0;
	    if (mth->thr_auditon) procflags |= MVFS_PF_AUDITON;
	    if (mth->thr_auditv)  procflags |= MVFS_PF_AUDITVOB;
	    if (mth->thr_auditnv) procflags |= MVFS_PF_AUDITNONVOB;
	    error = CopyOutMvfs_u_long(&procflags, kdata->infop, callinfo);
	    break;
	}

	case MVFS_CMD_SET_PROCF: {
	    auto u_long procflags;

	    if ((error = CopyInMvfs_u_long(kdata->infop, &procflags, callinfo)) != 0)
	            break;

	    tbsstatus = MVFS_ESTABLISH_FORK_HANDLER();
	    if (tbsstatus != TBS_ST_OK) break;
	 
	    mth->thr_auditv = ((procflags & MVFS_PF_AUDITVOB) != 0);
	    mth->thr_auditnv= ((procflags & MVFS_PF_AUDITNONVOB) != 0);
	    mvfs_sync_procstate(mth);

	    break;
	}

	case MVFS_CMD_START_AUDIT: {
	    auto u_long aflags;

	    if ((error = CopyInMvfs_u_long(kdata->infop, &aflags, callinfo)) != 0)
	            break;

	    MDB_XLOG((MDB_AUDITF, "startaudit: pid=%d\n", MDKI_CURPID()));

	    tbsstatus = MVFS_ESTABLISH_FORK_HANDLER();
	    if (tbsstatus != TBS_ST_OK) break;
	 

	    /*
	     * We need to insure no other thread has set the audit
	     * state since we last synchronized, so we take the proc lock
	     * and check it out.
	     */
            MVFS_LOCK(&(mth->thr_proc->mp_lock));

	    if (mth->thr_proc->mp_auditon) {
                MVFS_UNLOCK(&(mth->thr_proc->mp_lock));
		error = EEXIST;
		break;
	    }
	    if (mth->thr_afp != mth->thr_proc->mp_afp) {
		/* build interference of some sort. Some other thread
		 * changed the audit file pointer between when we
		 * entered the FS and now. We can't handle this if we
		 * want to be atomic on audit state enabling per
		 * process.
		 */
                MVFS_UNLOCK(&(mth->thr_proc->mp_lock));
		error = EINVAL; /* XXX ? */
		break;
	    }
	    mth->thr_auditon = 1;
	    mth->thr_auditv = ((aflags & MVFS_PF_AUDITVOB) != 0);
	    mth->thr_auditnv= ((aflags & MVFS_PF_AUDITNONVOB) != 0);
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	    if (MDKI_CALLER_IS_32BIT(callinfo)) 
	        mth->thr_afp->af_transtype = 1;
#endif
	    mvfs_sync_procstate_locked(mth); /* XXX */
	    
            MVFS_UNLOCK(&(mth->thr_proc->mp_lock));
            MVFS_MDEP_PROC_START_AUDIT(); /* update NT's mdep proc shadow */
	    break;
	}
	case MVFS_CMD_STOP_AUDIT: {	/* Stops audit and release resources */
            MVFS_MDEP_PROC_STOP_AUDIT(); /* update NT's mdep proc shadow */
            MDB_XLOG((MDB_AUDITF, "stopaudit: pid=%d\n", MDKI_CURPID()));
	    if (mth->thr_afp) {		/* Sync/release audit file if one */
		mvfs_auditwrite(mth);
		error = mth->thr_afp->auditwerr;	/* Delayed write error */
		mfs_afp_obsolete(mth);	/* Obsolete (shut off) the auditfile */
	        mvfs_afprele_thr(mth);	/* Release struct now */
	    }
	    mth->thr_auditon = 0;
	    mvfs_sync_procstate(mth);	/* it will drop */
	    break;
	}
	case MVFS_CMD_SYNC_AUDIT: {
            MDB_XLOG((MDB_AUDITF, "syncaudit: pid=%d\n", MDKI_CURPID()));
	    if (mth->thr_afp) {	/* Sync the audit file */
		mvfs_auditwrite(mth);
	        error = mth->thr_afp->auditwerr;	/* Return any write error */
	    } else error = ENOENT;	/* No audit file */
	    break;
	}
	default: {
	    error = ENOTTY;
	    break;
	}
	case MVFS_CMD_AUDIT_MARKER: {
	    auto u_long marker_flags;

            if ((error = CopyInMvfs_u_long(kdata->infop, &marker_flags, callinfo)) != 0)
                break;
            MDB_XLOG((MDB_AUDITF, "auditmarker: pid=%d flags=%lx\n",
		      MDKI_CURPID(), marker_flags));

	    if (mth->thr_afp) {
		/* write a marker */
		MFS_AUDIT_EXT(MFS_AR_MARKER, NULL, NULL, NULL, NULL, NULL,
			      marker_flags, cred);
                if (mth->thr_afp == NULL) {
                    /* If mth->thr_afp->obsolete was set, the mfs_audit 
                     * will have set mth->thr_afp to NULL so there is
                     * nothing left for us to do.  This means that someone
                     * else has called MVFS_CMD_STOP_AUDIT.
                     * I do not return EINVAL as we would if thr_afp was
                     * already NULL because we actually did the work that
                     * was needed to finish cleaning up the audit file.
                     */
                    MDB_XLOG((MDB_AUDITF,
                      "auditmarker: Audit stopped. pid=%d flags=%lx\n",
		      MDKI_CURPID(), marker_flags));
                    break;
                }
		/*
		 * As a side-effect, auditwrite also cleans out the
		 * in-core buffer and thus ensures that items accessed
		 * on both sides of the marker get recorded on both
		 * sides.
		 */
		mvfs_auditwrite(mth);
		error = mth->thr_afp->auditwerr;
	    } else
		error = EINVAL;
	}
    }	/* end of switch */

    if (error == 0 && tbsstatus != TBS_ST_OK) {
	error = tbs_status2errno(tbsstatus);
    }
    MFS_BUMPTIME(stime, dtime, mfs_austat.au_ioctltime);
    return(error);
}

int
mfs_v2objtype(VTYPE_T vtype)
{
    switch (vtype) {
	case VDIR: return(MFS_OT_DIR);
	case VREG: return(MFS_OT_REG);
	case VLNK: return(MFS_OT_LNK);
	case VBLK: return(MFS_OT_BLK);
	case VCHR: return(MFS_OT_CHR);
        default:   return(MFS_OT_NONE);
    }
}

STATIC int
mfs_cmpdirrec(rp, kind, dvp, nm, nmlen, vp, vap)
register mfs_auditrec_t *rp;
u_int kind;
VNODE_T *dvp;
char *nm;
size_t nmlen;
VNODE_T *vp;
VATTR_T *vap;
{
    struct timeval tv;

    if (rp->mfs_dirrec.namlen != nmlen) {
	MDB_XLOG((MDB_AUDITOPS2, "mismatch namlen\n"));
	return(0);
    }
    if (rp->mfs_dirrec.objtype != mfs_v2objtype(MVFS_GETVTYPE(vp))) {
	MDB_XLOG((MDB_AUDITOPS2, "mismatch objtype\n"));
	return(0);
    }
    VATTR_GET_MTIME_TV(vap, &tv);
    if (!MFS_TVEQ(tv, rp->mfs_dirrec.objdtm)) { 
	MDB_XLOG((MDB_AUDITOPS2, "mismatch dtm\n"));
	return(0);
    }
    if (dvp && MFS_ISVOB(VTOM(dvp))) {
        if (!MFS_OIDEQ(rp->mfs_dirrec.diroid,VTOM(dvp)->mn_vob.attr.obj_oid)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch diroid\n"));
	    return(0);
	}
    }
    if (MFS_ISVOB(VTOM(vp))) {
        if (!MFS_OIDEQ(rp->mfs_dirrec.objoid,VTOM(vp)->mn_vob.attr.obj_oid)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch objoid\n"));
	    return(0);
	}
	if (!MFS_OIDEQ(rp->mfs_dirrec.voboid,V_TO_MMI(vp)->mmi_voboid)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch voboid\n"));
	    return(0);
  	}
        if (rp->mfs_dirrec.objsn.sn_high != VTOM(vp)->mn_vob.vfh.ver_dbid ||
	    rp->mfs_dirrec.objsn.sn_low != VTOM(vp)->mn_vob.vfh.gen) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch obj serial number\n"));
	    return(0);
        }
    }

    /* Use bcmp - it is faster */
    if (BCMP(rp->mfs_dirrec.name, nm, nmlen)) {
	MDB_XLOG((MDB_AUDITOPS2, "mismatch name\n"));
	return(0);
    }

    /* Everything matches, return a duplicate */
    return(1);
}

STATIC int
mfs_cmpviewrec(rp, vp)
register mfs_auditrec_t *rp;
VNODE_T *vp;
{
    char *nm = mfs_vp2vw(vp);
    size_t nmlen;

    if (nm) nmlen = STRLEN(nm);
    else nmlen = 0;

    if (MFS_VIEW(vp) == NULL) {
	if (!MFS_UUIDNULL(rp->mfs_viewrec.viewuuid)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch uuid\n"));
	    return(0);
	}
    } else {
        if (!MFS_UUIDEQ(rp->mfs_viewrec.viewuuid, 
					VTOM(MFS_VIEW(vp))->mn_view.svr.uuid)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch uuid\n"));
	    return(0);
	}
    }
    if (BCMP(rp->mfs_viewrec.name, nm, nmlen)) {
	MDB_XLOG((MDB_AUDITOPS2, "mismatch name\n"));
	return(0);
    }

    /* All match - return dupl */
    return(1);
}

STATIC int
mfs_cmprwrec(rp, kind, vp, vap)
register mfs_auditrec_t *rp;
u_int kind;
VNODE_T *vp;
VATTR_T *vap;
{
    struct timeval tv;

    if (rp->mfs_rwrec.objtype != mfs_v2objtype(MVFS_GETVTYPE(vp))) {
	MDB_XLOG((MDB_AUDITOPS2, "mismatch objtype\n"));
	return(0);
    }
    /* Ignore DTM changes on writes.  They always change
       with every write. */
    if (kind != MFS_AR_WRITE) {
	VATTR_GET_MTIME_TV(vap, &tv);
        if (!MFS_TVEQ(tv, rp->mfs_rwrec.objdtm)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch dtm\n"));
	    return(0);
        }
    }
    if (MFS_ISVOB(VTOM(vp))) {
        if (!MFS_OIDEQ(rp->mfs_rwrec.objoid,VTOM(vp)->mn_vob.attr.obj_oid)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch objoid\n"));
	    return(0);
	}
	if (!MFS_OIDEQ(rp->mfs_rwrec.voboid,V_TO_MMI(vp)->mmi_voboid)) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch voboid\n"));
	    return(0);
  	}
        if (rp->mfs_dirrec.objsn.sn_high != VTOM(vp)->mn_vob.vfh.ver_dbid ||
	    rp->mfs_dirrec.objsn.sn_low != VTOM(vp)->mn_vob.vfh.gen) {
	    MDB_XLOG((MDB_AUDITOPS2, "mismatch obj serial number\n"));
	    return(0);
        }
    }

    /* Everything matched - return a dupl */

    return(1);
}

STATIC int
mfs_isdupl(afp, kind, dvp, nm, nmlen, vp, vap)
mfs_auditfile_t *afp;
u_int kind;
VNODE_T *dvp;
char *nm;
size_t nmlen;
VNODE_T *vp;
VATTR_T *vap;
{
   int i;
   mfs_auditrec_t *rp;

   ASSERT(ISLOCKED(&afp->lock));

   /* Look back through the current buffer for a duplicate.
      At a max, look back a parameterized number of entries. This should
      be tuned to handle a reasonable depth of LOOKUP/READ
      pairs during hdr file nesting. */

   for (i=0, rp = afp->lastpos; i < mvfs_duplsearchmax && rp >= afp->buf;
	i++, rp = MFS_PREVREC(rp)) {
	if (rp->kind != kind) {
	    continue;
	}
	switch (kind) {
	    case MFS_AR_ROOT:
	    case MFS_AR_LOOKUP:
	    case MFS_AR_RDLINK:
		if (mfs_cmpdirrec(rp, kind, dvp,nm,nmlen, vp, vap)) return(1);
		break;
	    case MFS_AR_READ:
	    case MFS_AR_WRITE:
		if (mfs_cmprwrec(rp, kind, vp, vap)) return(1);
		break;
	    case MFS_AR_LINK:   /* Hard to dupl once name exists */
	    case MFS_AR_UNLINK: /* Hard to dupl once name gone */
	    case MFS_AR_CREATE: /* Hard to dupl once name created */
	    case MFS_AR_RENAME:	/* Never dupls because name/oid changes */
	    case MFS_AR_CHOID:  /* Hard to dupl cause can never get same oid */
 	    case MFS_AR_SYMLINK: /* Hard to dupl symlink create */
		return(0);
	    case MFS_AR_VIEW:
		if (mfs_cmpviewrec(rp, vp)) return (1);
		break;
	    case MFS_AR_MARKER:
	    	break;			/* never considered a duplicate */
	}
    }

    /* No dupl found, return such */

    MDB_XLOG((MDB_AUDITOPS2, "mfs_isdupl: no dupl: lastpos=%x\n", afp->lastpos));
    return(0);
}

/*
 * mfs_init_rmstat - routine to fill in a rmstat structure for a
 * vnode.  Used to save remove information from before the
 * remove operation, because during the audit, we can't do a
 * stat to get that info on a file-handle that may now
 * be stale (which happened when the remove succeeded).
 */

void
mfs_init_rmstat(vp, rmstatp)
VNODE_T *vp;
mfs_auditrmstat_t *rmstatp;
{
    mfs_mnode_t *mnp;

    ASSERT(MFS_VPISMFS(vp));

    mnp = VTOM(vp);

    if (MFS_ISVOB(mnp)) {
        rmstatp->objtype = (u_short)mfs_v2objtype(MVFS_GETVTYPE(vp));
	rmstatp->objdtm.tv_sec  = 0;
	rmstatp->objdtm.tv_usec = 0;
	rmstatp->voboid  = V_TO_MMI(vp)->mmi_voboid;
	rmstatp->objoid  = mnp->mn_vob.attr.obj_oid;
	rmstatp->objsn.sn_high = mnp->mn_vob.vfh.ver_dbid;
	rmstatp->objsn.sn_low  = mnp->mn_vob.vfh.gen;
	rmstatp->elemoid = mnp->mn_vob.attr.elem_oid;
	rmstatp->mtype   = (ks_uint32_t) mnp->mn_vob.attr.mtype;
    } else {
	BZERO(rmstatp, sizeof(*rmstatp));
	rmstatp->objtype = (u_short)mfs_v2objtype(MVFS_GETVTYPE(vp));
    }
}

int
mfs_audit(kind, dvp, nm1, dvp2, nm2, vp, flags, cred)
int kind;
VNODE_T *dvp;
VNODE_T *dvp2;
char *nm1;
char *nm2;
VNODE_T *vp;
u_long flags;
CRED_T *cred;
{
    register mfs_auditfile_t *afp;
    register mfs_auditrec_t *rp = 0;    /* shut up GCC */
    mfs_auditrmstat_t *rmstatp;
    mvfs_thread_t *mth;
    struct timeval mtime;
    mfs_mnode_t *mnp;
    size_t len1, len2;
    SPL_T s;
    timestruc_t stime;	/* For statistics */
    timestruc_t dtime;
    mvfs_audit_data_t *madp = MDKI_AUDIT_GET_DATAP();

    MDKI_HRTIME(&stime);

    MVFS_INIT_TIMEVAL(mtime);	/* Keep the compiler happy */

    /* Get my process state and check for no audit,
       or audit temporarily inhibited. */

    mth = mvfs_mythread();
    if (!mth->thr_auditon || mth->thr_auditinh) {
	MDB_XLOG((MDB_AUDITOPS2, "mfs_audit: skipping audit mth 0x%"KS_FMT_PTR_T" auditon 0x%x auditinh 0x%x\n",
	    mth, mth->thr_auditon, mth->thr_auditinh));
	goto no_audit;
    }

    /* Can't audit if no audit file ptr */

    afp = mth->thr_afp;
    if (afp == NULL) {
	MDB_XLOG((MDB_AUDITOPS2, "mfs_audit: skipping audit NULL afp for mth 0x%"KS_FMT_PTR_T"\n", mth));
        goto no_audit;		/* allow stats leak */
    }

    /* Check for audit file that has been obsoleted by 
     * clearmake doing a STOP_AUDIT.  This happens when
     * the  current process is a long-running daemon that
     * was started under the audit, but proceeded to orphan
     * itself.  The flag in the auditfile struct is left
     * there to tell us to stop auditing and clean up our
     * reference.
     */

    if (afp->obsolete) {
	/* No need to sync writes here ... noone cares about them */
	mvfs_afprele_thr(mth);		/* Release ref on auditfile */
	afp = NULL;
	mth->thr_auditon = 0;
	mvfs_sync_procstate(mth);
	MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
	MDB_XLOG((MDB_AUDITOPS2, "mfs_audit: skipping audit afp 0x%"KS_FMT_PTR_T" OBSOLETE for mth 0x%"KS_FMT_PTR_T"\n", afp, mth));
	goto no_audit;
    }

    /* Check for non-vob objects if only auditing vob obj */

    if (dvp) {
        if (MFS_VPISMFS(dvp) && MFS_ISVOB(VTOM(dvp))) {
	    if (!mth->thr_auditv) {
		MDB_XLOG((MDB_AUDITOPS2, "mfs_audit:  Skipping vob obj directory audit mth 0x%"KS_FMT_PTR_T" dvp 0x%"KS_FMT_PTR_T"\n", mth, dvp));
	        goto no_audit; /* allow stats leak */
	    }
        } else {
	    if (!mth->thr_auditnv) {
		MDB_XLOG((MDB_AUDITOPS2, "mfs_audit:  Skipping non-vob obj directory audit mth 0x%"KS_FMT_PTR_T" dvp 0x%"KS_FMT_PTR_T"\n", mth, dvp));
	        goto no_audit; /* allow stats leak */
	    }
	}
    }

    if (vp) {
        if (MFS_VPISMFS(vp) && MFS_ISVOB(VTOM(vp))) {
	    if (!mth->thr_auditv) {
		MDB_XLOG((MDB_AUDITOPS2, "mfs_audit:  Skipping vob obj audit mth 0x%"KS_FMT_PTR_T" vp 0x%"KS_FMT_PTR_T"\n", mth, vp));
	        goto no_audit;
	    }
        } else {
	    if (!mth->thr_auditnv) {
		MDB_XLOG((MDB_AUDITOPS2, "mfs_audit:  Skipping non-vob obj audit mth 0x%"KS_FMT_PTR_T" vp 0x%"KS_FMT_PTR_T"\n", mth, vp));
	        goto no_audit; /* allow stats leak */
	    }
	}
    }

    /* Count audit calls that pass the above tests */

    BUMPSTAT(mfs_austat.au_calls, s);

    /* Lock the audit file struct before using sub-fields */

    MVFS_LOCK(&afp->lock);

    /* 
     * Do a getattr to get the DTM on the object.  We use
     * a static vattr struct in the audit file structure to
     * conserve stack space.  This has somewhat undesirable 
     * attributes (auditing is single threaded over a call
     * which may take a long time), but is the lesser of many evils.
     * FIXME: make conditional code based on "SMALL_STACK" cpp define.
     */

    if (vp) {

	/* 
         * Do getattr to get mtime.  There are several important
         * points here:
	 * 	1) VOP_GETATTR takes a lot of stack.  To avoid
         *         save some stack, we call mfs_getattr directly for
         *         MFS objects (they may call VOP_GETATTR at a lower level
         *         to get cleartext attributes!)
         *      2) For NFS, VOP_GETATTR() sync's all modified data to
         *         the home node to get an uptodate mod time.  If we
         *         did this for every write call, we might be sync'ing
         *         every 20 bytes or so ... yeeech!  So, we do not
         *         do the VOP_GETATTR() for writes to allow sequential
         *         writes without syncing every one.
         */

        if (kind != MFS_AR_WRITE) {
            VATTR_SET_MASK(&afp->va, AT_MTIME);
	    MFS_INHREBIND(mth);
	    if (MFS_VPISMFS(vp)) {
	        BUMPSTAT(mfs_austat.au_vgetattr, s);
		MFS_CHKSP(STK_MFSGETATTR);
		if (mfs_getattr(vp, &afp->va, 0, cred) == 0) {
		    VATTR_GET_MTIME_TV(&afp->va, &mtime);
	        } else {
                    MVFS_FREE_VATTR_FIELDS(&afp->va); /* and free any NT sids copied */
	        }
	    } else {
                CLR_VNODE_T *cvp;
	        BUMPSTAT(mfs_austat.au_nvgetattr, s);
	        MFS_CHKSP(STK_GETATTR);	
                MVFS_VP_TO_CVP(vp, &cvp); /* XXX non-mvfs: fail? */
	        if (MVOP_GETATTR(vp, cvp, &afp->va, 0, cred) == 0) {
		    VATTR_GET_MTIME_TV(&afp->va, &mtime);
	        } else {
                    MVFS_FREE_VATTR_FIELDS(&afp->va);
	        }
                CVN_RELE(cvp);
	    }
	    MFS_ENBREBIND(mth);
 	}
    } 

    /* Get the length of the char strings if any */

    if (nm1) len1 = STRLEN(nm1);
    else len1 = 0;

    /* Check if a redundant audit record, and discard if so. */

    if (mfs_isdupl(afp, kind, dvp, nm1, len1, vp, &afp->va)) {
	BUMPSTAT(mfs_austat.au_dupl, s);
	goto out;
    }

    /* 
     * Based on type of record, check for room in the audit
     * buffer and fill in the record. 
     */

    switch (kind) {
	case MFS_AR_ROOT:
	case MFS_AR_LOOKUP:
	case MFS_AR_RDLINK:
	case MFS_AR_LINK:
	case MFS_AR_CREATE:
        case MFS_AR_SYMLINK:
	    if ((MFS_AUDITOFF(afp->curpos, afp->buf) +
			 MFS_AUDITDIRSIZ(len1)) > afp->buflen) {
		/* If writing can make room tell caller to flush 
		   and try again.*/
		if (afp->curpos != afp->buf) {
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(1);
		} else {
		    afp->auditwerr=ENOSPC;
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(0);
		}
	    }
            if (afp->auditwerr) goto out;
    	    rp = afp->curpos;
    	    rp->version = MFS_AUDITVERSION;
    	    rp->prevoff = afp->lastsize;
    	    rp->nextoff = (u_short)MFS_AUDITDIRSIZ(len1);
    	    rp->kind = (u_short)kind;
	    rp->mfs_dirrec.namlen = (u_short)len1;
	    rp->mfs_dirrec.objtype = (u_short)mfs_v2objtype(MVFS_GETVTYPE(vp));
	    rp->mfs_dirrec.objdtm  = mtime;
	    if (MFS_VPISMFS(vp) && MFS_ISVOB(VTOM(vp))) {
		mnp = VTOM(vp);
	        rp->mfs_dirrec.viewuuid = VTOM(MFS_VIEW(vp))->mn_view.svr.uuid;
	        rp->mfs_dirrec.voboid  = V_TO_MMI(vp)->mmi_voboid;
	        rp->mfs_dirrec.objoid  = mnp->mn_vob.attr.obj_oid;
		rp->mfs_dirrec.objsn.sn_high = mnp->mn_vob.vfh.ver_dbid;
		rp->mfs_dirrec.objsn.sn_low = mnp->mn_vob.vfh.gen;
		rp->mfs_dirrec.elemoid = mnp->mn_vob.attr.elem_oid;
		rp->mfs_dirrec.mtype   = (ks_uint32_t) mnp->mn_vob.attr.mtype;
	    } else {
		rp->mfs_dirrec.viewuuid = TBS_UUID_NULL;
		rp->mfs_dirrec.voboid  = TBS_OID_NULL;
		rp->mfs_dirrec.objoid  = TBS_OID_NULL;
		rp->mfs_dirrec.objsn.sn_high = 0;
		rp->mfs_dirrec.objsn.sn_low = 0;
		rp->mfs_dirrec.elemoid = TBS_OID_NULL;
		rp->mfs_dirrec.mtype   = 0;
	    }
	    if (vp && MFS_VPISMFS(vp) && MFS_VIEW(vp)) {
	        rp->mfs_dirrec.viewuuid = VTOM(MFS_VIEW(vp))->mn_view.svr.uuid;
	    } else {
		rp->mfs_dirrec.viewuuid = TBS_UUID_NULL;
	    }
	    if (dvp && MFS_VPISMFS(dvp) && MFS_ISVOB(VTOM(dvp))) {
		rp->mfs_dirrec.diroid  = VTOM(dvp)->mn_vob.attr.obj_oid;
	    } else {
		rp->mfs_dirrec.diroid  = TBS_OID_NULL;
	    }
	    
 	    STRCPY(rp->mfs_dirrec.name, nm1);
	    break;
	/*
	 * Unlink is unusual.  nm2 has ptr to a "saved info"
         * record.  This is the info to log, fetched before the UNLINK
         * was done.  The object may not exist after the UNLINK for
         * us to stat in here.
         */
	case MFS_AR_UNLINK:
	    rmstatp = (mfs_auditrmstat_t *) nm2; 

	    if ((MFS_AUDITOFF(afp->curpos, afp->buf) +
			 MFS_AUDITDIRSIZ(len1)) > afp->buflen) {
		/* If writing can make room tell caller to flush 
		   and try again.*/
		if (afp->curpos != afp->buf) {
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(1);
		} else {
		    afp->auditwerr=ENOSPC;
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(0);
		}
	    }
            if (afp->auditwerr) goto out;
    	    rp = afp->curpos;
    	    rp->version = MFS_AUDITVERSION;
    	    rp->prevoff = afp->lastsize;
    	    rp->nextoff = (u_short)MFS_AUDITDIRSIZ(len1);
    	    rp->kind = (u_short)kind;
	    rp->mfs_dirrec.namlen = (u_short)len1;
	    /* May not be remove stats for some unlinks */
	    if (rmstatp) {
	        rp->mfs_dirrec.objtype = rmstatp->objtype;
	        rp->mfs_dirrec.objdtm  = rmstatp->objdtm;
	        rp->mfs_dirrec.voboid  = rmstatp->voboid;
	        rp->mfs_dirrec.objoid  = rmstatp->objoid;
		rp->mfs_dirrec.objsn   = rmstatp->objsn;
	        rp->mfs_dirrec.elemoid = rmstatp->elemoid;
	        rp->mfs_dirrec.mtype   = rmstatp->mtype;
	    } else {
		rp->mfs_dirrec.objtype = 0;
		rp->mfs_dirrec.objdtm.tv_sec  = 0;
		rp->mfs_dirrec.objdtm.tv_usec = 0;
		rp->mfs_dirrec.voboid  = TBS_OID_NULL;
		rp->mfs_dirrec.objoid  = TBS_OID_NULL;
		rp->mfs_dirrec.objsn.sn_high = 0;
		rp->mfs_dirrec.objsn.sn_low = 0;
		rp->mfs_dirrec.elemoid = TBS_OID_NULL;
		rp->mfs_dirrec.mtype   = 0;
	    }

	    /* For remove, must take view from the dir. */
	    if (dvp && MFS_VPISMFS(dvp) && MFS_VIEW(dvp)) {
	        rp->mfs_dirrec.viewuuid = VTOM(MFS_VIEW(dvp))->mn_view.svr.uuid;
	    } else {
		rp->mfs_dirrec.viewuuid = TBS_UUID_NULL;
	    }
	    if (dvp && MFS_VPISMFS(dvp) && MFS_ISVOB(VTOM(dvp))) {
		rp->mfs_dirrec.diroid  = VTOM(dvp)->mn_vob.attr.obj_oid;
	    } else {
		rp->mfs_dirrec.diroid  = TBS_OID_NULL;
	    }
	    
 	    STRCPY(rp->mfs_dirrec.name, nm1);
	    break;
	case MFS_AR_READ:
	case MFS_AR_WRITE:
	case MFS_AR_TRUNCATE:
	    if ((MFS_AUDITOFF(afp->curpos, afp->buf) +
			 MFS_AUDITRWSIZ) > afp->buflen) {
		/* If writing can make room tell caller to flush 
		   and try again.*/
		if (afp->curpos != afp->buf) {
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(1);
		} else {
		    afp->auditwerr=ENOSPC;
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(0);
		}
	    }
            if (afp->auditwerr) goto out;
    	    rp = afp->curpos;
    	    rp->version = MFS_AUDITVERSION;
    	    rp->prevoff = afp->lastsize;
    	    rp->nextoff = MFS_AUDITRWSIZ;
    	    rp->kind = (u_short)kind;
	    rp->mfs_rwrec.objtype = (u_short)mfs_v2objtype(MVFS_GETVTYPE(vp));
	    rp->mfs_rwrec.objdtm  = mtime;
	    if (MFS_VPISMFS(vp) && MFS_ISVOB(VTOM(vp))) {
		mnp = VTOM(vp);
		rp->mfs_rwrec.viewuuid = VTOM(MFS_VIEW(vp))->mn_view.svr.uuid;
	        rp->mfs_rwrec.voboid  = V_TO_MMI(vp)->mmi_voboid;
	        rp->mfs_rwrec.objoid  = mnp->mn_vob.attr.obj_oid;
		rp->mfs_rwrec.objsn.sn_high = mnp->mn_vob.vfh.ver_dbid;
		rp->mfs_rwrec.objsn.sn_low = mnp->mn_vob.vfh.gen;
		rp->mfs_rwrec.elemoid = mnp->mn_vob.attr.elem_oid;
		rp->mfs_rwrec.mtype   = (ks_uint32_t) mnp->mn_vob.attr.mtype;
	    } else {
		rp->mfs_rwrec.viewuuid = TBS_UUID_NULL;
		rp->mfs_rwrec.voboid  = TBS_OID_NULL;
		rp->mfs_rwrec.objoid  = TBS_OID_NULL;
		rp->mfs_rwrec.objsn.sn_high = 0;
		rp->mfs_rwrec.objsn.sn_low = 0;
		rp->mfs_rwrec.elemoid = TBS_OID_NULL;
		rp->mfs_rwrec.mtype   = 0;
	    }
	    break;
	case MFS_AR_RENAME:
    	    if (nm2) len2 = STRLEN(nm2);	/* Length of name */
    	    else len2 = 0;
	    if ((MFS_AUDITOFF(afp->curpos, afp->buf) +
			 MFS_AUDITRNMSIZ(len1, len2)) > afp->buflen) {
		/* If writing can make room tell caller to flush 
		   and try again.*/
		if (afp->curpos != afp->buf) {
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(1);
		} else {
		    afp->auditwerr=ENOSPC;
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(0);
		}
	    }
            if (afp->auditwerr) goto out;
    	    rp = afp->curpos;
    	    rp->version = MFS_AUDITVERSION;
    	    rp->prevoff = afp->lastsize;
    	    rp->nextoff = (u_short)MFS_AUDITRNMSIZ(len1,len2);
    	    rp->kind = (u_short)kind;
	    rp->mfs_rnmrec.o_namlen = (u_short)len1;
	    rp->mfs_rnmrec.t_namlen = (u_short)len2;
	    /* Take view uuid from first dir.  Renames cannot
	     * be between two different views.
	     */
	    if (MFS_VPISMFS(dvp) && MFS_ISVOB(VTOM(dvp))) {
		rp->mfs_rnmrec.viewuuid  = VTOM(MFS_VIEW(dvp))->mn_view.svr.uuid;
		rp->mfs_rnmrec.o_diroid  = VTOM(dvp)->mn_vob.attr.obj_oid;
	    } else {
		rp->mfs_rnmrec.o_diroid  = TBS_OID_NULL;
	    }
	    if (MFS_VPISMFS(dvp2) && MFS_ISVOB(VTOM(dvp2))) {
		rp->mfs_rnmrec.t_diroid  = VTOM(dvp2)->mn_vob.attr.obj_oid;
	    } else {
		rp->mfs_rnmrec.t_diroid  = TBS_OID_NULL;
	    }
	    STRCPY(rp->mfs_rnmrec.name, nm1);
	    rp->mfs_rnmrec.name[len1] = '\0';	/* Make sure end of string */
	    STRCPY(&rp->mfs_rnmrec.name[len1+1], nm2);
	    break;
	case MFS_AR_CHOID:
	    if ((MFS_AUDITOFF(afp->curpos, afp->buf) +
			 MFS_AUDITCHOIDSIZ) > afp->buflen) {
		/* If writing can make room tell caller to flush 
		   and try again.*/
		if (afp->curpos != afp->buf) {
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(1);
		} else {
		    afp->auditwerr=ENOSPC;
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(0);
		}
	    }
            if (afp->auditwerr) goto out;
    	    rp = afp->curpos;
    	    rp->version = MFS_AUDITVERSION;
    	    rp->prevoff = afp->lastsize;
	
    	    rp->nextoff = MFS_AUDITCHOIDSIZ;
    	    rp->kind = (u_short)kind;
	    rp->mfs_choidrec.prevoid = *(tbs_oid_t *)nm2;
	    rp->mfs_choidrec.objtype = (u_short)mfs_v2objtype(MVFS_GETVTYPE(vp));
	    if (MFS_VPISMFS(vp) && MFS_ISVOB(VTOM(vp))) {
		mnp = VTOM(vp);
		rp->mfs_choidrec.viewuuid = 
				VTOM(MFS_VIEW(vp))->mn_view.svr.uuid;
	        rp->mfs_choidrec.objoid  = *(tbs_oid_t *)nm1;
		rp->mfs_choidrec.objsn.sn_high = mnp->mn_vob.vfh.ver_dbid;
		rp->mfs_choidrec.objsn.sn_low  = mnp->mn_vob.vfh.gen;
	        rp->mfs_choidrec.mtype = 
			(ks_uint32_t)mnp->mn_vob.attr.mtype;
	    } else {
		rp->mfs_choidrec.viewuuid = TBS_UUID_NULL;
	        rp->mfs_choidrec.objoid  = TBS_OID_NULL;
		rp->mfs_choidrec.objsn.sn_high = 0;
		rp->mfs_choidrec.objsn.sn_low = 0;
	        rp->mfs_choidrec.mtype = 0;
	    }
	    break;
	case MFS_AR_VIEW:
	    len1 = STRLEN(mfs_vp2vw(vp));
	   
	    if ((MFS_AUDITOFF(afp->curpos, afp->buf) +
			 MFS_AUDITVIEWSIZ(len1)) > afp->buflen) {
		/* If writing can make room tell caller to flush 
		   and try again.*/
		if (afp->curpos != afp->buf) {
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(1);
		} else {
		    afp->auditwerr=ENOSPC;
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(0);
		}
	    }
            if (afp->auditwerr) goto out;
    	    rp = afp->curpos;
    	    rp->version = MFS_AUDITVERSION;
    	    rp->prevoff = afp->lastsize;
	
    	    rp->nextoff = (u_short)MFS_AUDITVIEWSIZ(len1);
    	    rp->kind = (u_short)kind;
	    rp->mfs_viewrec.namlen = (u_short)len1;
	    rp->mfs_viewrec.viewuuid = VTOM(MFS_VIEW(vp))->mn_view.svr.uuid;
 	    STRNCPY(rp->mfs_viewrec.name, mfs_vp2vw(vp), len1+1);
	    break;
	case MFS_AR_MARKER:
	    if ((MFS_AUDITOFF(afp->curpos, afp->buf) +
			 MFS_AUDITMARKERSIZ) > afp->buflen) {
		/* If writing can make room tell caller to flush 
		   and try again.*/
		if (afp->curpos != afp->buf) {
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(1);
		} else {
		    afp->auditwerr=ENOSPC;
		    MVFS_UNLOCK(&afp->lock);
		    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
		    return(0);
		}
	    }
            if (afp->auditwerr) goto out;
    	    rp = afp->curpos;
    	    rp->version = MFS_AUDITVERSION;
    	    rp->prevoff = afp->lastsize;
	
    	    rp->nextoff = MFS_AUDITMARKERSIZ;
    	    rp->kind = (u_short)kind;
	    rp->mfs_markerrec.markerval = flags;
	    break;

	default:
	    mvfs_log(MFS_LOG_WARN, 
		     "audit: unknown audit record type %d\n", kind);
	    rp = 0;
	    goto out;
    }

    ASSERT(rp->nextoff <= sizeof(mfs_auditrec_t)); 
    ASSERT(MFS_AUDITOFF(rp, afp->buf) <= afp->buflen);
    afp->lastsize = rp->nextoff;
    afp->lastpos = rp;
    afp->curpos = MFS_NEXTREC(rp);
    ASSERT(MFS_AUDITOFF(afp->curpos, afp->buf) <= afp->buflen);

out:
    MVFS_UNLOCK(&afp->lock);
    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
    MDB_XLOG((MDB_AUDITOPS, "mfs_audit: err=%x, mth=%"KS_FMT_PTR_T", pid=%x, rp=%"KS_FMT_PTR_T", kind=%d\n", afp->auditwerr, mth, mth->thr_proc->mp_procid, rp, kind));
    return(0);

no_audit:
#ifdef MVFS_DEBUG
    if (madp->mvfs_debug_no_audit_detail) {
        MDB_XLOG((MDB_AUDITOPS, "mfs_audit: skipping audit mth=%"KS_FMT_PTR_T", pid=%x, afp 0x%"KS_FMT_PTR_T", kind=%d\n", mth, mth->thr_proc->mp_procid, mth->thr_afp, kind));
    }
#endif
    return(0);

}

void
mvfs_auditwrite(thr)
mvfs_thread_t *thr;
{
    mvfs_auditwrite_int(thr->thr_afp, thr);
}

STATIC void
mvfs_auditwrite_int(afp, mth)
register mfs_auditfile_t *afp;
register mvfs_thread_t *mth;
{
    struct _ucva {
	CRED_T *cred;
	VATTR_T va;
	struct uio uio;
	IOVEC_T iovec;
    } *uvp;
    CLR_VNODE_T *cvp = NULL;
    struct uio *uiop;
    struct mfs_auditrec_32 *tbufp = NULL;
    int error, xerror;
    int s;
    timestruc_t stime;	/* For statistics */
    timestruc_t dtime;
    void *afp_file;
    u_long bytes_to_write;

    /* If no buffer, nothing to do */

    if (afp == NULL) {
	return;
    }

    MDKI_HRTIME(&stime);

    ASSERT(mth == mvfs_mythread());	/* we must inhibit ourselves */
    /* Lock the auditfile buffer */

    MVFS_LOCK(&afp->lock);

    /* If a delayed write error, just clear the bufptrs and return */

    if (afp->auditwerr) {
	afp->curpos = afp->buf;
	afp->lastpos = NULL;
	goto out;
    }

    /* Check for empty buffer */

    if (afp->curpos == afp->buf)  {
	goto out;
    }

    /* Inhibit audit of ops during audit write */

    MFS_INHAUDIT(mth);

    /* 
     * Allocate cred and vattr structs to save stack space 
     * (On sysV.3 also do this for mfs_uiosave area and do the save).
     */

    uvp = (struct _ucva *) KMEM_ALLOC(sizeof(struct _ucva), KM_SLEEP);
    ASSERT(uvp);
    VATTR_NULL(&uvp->va);
    uvp->uio.uio_iov = &uvp->iovec;
    uiop = &uvp->uio;

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
    /* LP64 kernel must translate some record fields for 32-bit apps */
    if (afp->af_transtype) {
	/* 
         * Using mvfs_auditbufsize is not quite accurate, but safe, since
         * this native LP64 version is larger than the 32-bit version we 
         * are about to convert to. 
	 */
	tbufp = (struct mfs_auditrec_32 *)KMEM_ALLOC(mvfs_auditbufsiz, KM_SLEEP);
        if (tbufp == NULL) {
            error = ENOMEM;
            goto errout;
        }
    }

    if (afp->af_transtype && (tbufp != NULL)) {
	mfs_auditbuf_to_mfs_auditbuf_32(afp->buf, tbufp, afp->curpos);
    }
#endif 

    /* Open the object */

    cvp = afp->cvp;	/* Copy out the vp in case open changes it. */
    if (cvp == NULL) {
	mvfs_log(MFS_LOG_ERR, "no audit file on auditwrite\n");
	error = ESTALE;
	goto errout;
    }
    CVN_HOLD(cvp);
    error = MVOP_OPEN_KERNEL(&cvp, FWRITE, afp->cred, &afp_file);
    if (error) goto errout;

    /* Use getattr to get end of file for write */

    MFS_CHKSP(STK_GETATTR);
    VATTR_SET_MASK(&uvp->va, AT_SIZE);
    error = MVOP_GETATTR(MVFS_CVP_TO_VP(cvp), cvp, &uvp->va, 0, afp->cred);
    if (error) goto closeout;

    /* 
     * Actually write the buffer (mfs_uioset sets uio_resid).
     */
    bytes_to_write = MFS_AUDITOFF(afp->curpos, afp->buf);
    mfs_uioset(uiop, (tbufp != NULL) ? (char *)tbufp : (char *)afp->buf,
		bytes_to_write, uvp->va.va_size, UIO_SYSSPACE);

    MVOP_RWWRLOCK(cvp, NULL);

    /* Some filesystems can return an error of 0 but only do a partial write.
    ** Thus, we will keep trying to write until
    ** we stop making "progress" (i.e. no bytes were written), at which point
    ** we return the error, if there is one, or ENOSPC (as our best guess as
    ** to the problem) if there isn't one.
    */
    do {
        error = MVOP_WRITE_KERNEL(cvp, uiop, 0, NULL, afp->cred, afp_file);
        if (error == 0) {
            if (uiop->uio_resid == bytes_to_write) { /* No progress... */
                MDB_XLOG((MDB_AUDITF,
                          "auditwrite: MVOP_WRITE() returned 0 but wrote "
                          "nothing (%s), returning ENOSPC\n", afp->path));
                error = ENOSPC; /* Our best guess. */
            } else {
#ifdef MVFS_DEBUG               /* Only compile the test if we're debugging */
                if (uiop->uio_resid > 0) {
                    MDB_XLOG((MDB_AUDITF,
                              "auditwrite: MVOP_WRITE() returned 0 but wrote "
                              "%lu bytes, leaving "
                              "%"MVFS_FMT_UIO_RESID_D" bytes (%s)\n",
                              (u_long)(bytes_to_write - uiop->uio_resid),
                              uiop->uio_resid, afp->path));
                }
#endif
                ASSERT((long)(uiop->uio_resid) >= 0);   /* We're assigning to a ulong */
                bytes_to_write = uiop->uio_resid;
            }
        }
    } while ((error == 0) && (bytes_to_write != 0));

    MVOP_RWWRUNLOCK(cvp, NULL);

closeout:
    xerror = MVOP_CLOSE_KERNEL(cvp, FWRITE, /* Close audit file */
                               MVFS_LASTCLOSE_COUNT | MVFS_KEEPHANDLE,
                               (MOFFSET_T)0, afp->cred, afp_file);
    if (!error) error = xerror;		/* Don't overwrite real error */

errout:
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
    if (tbufp != NULL) {
	KMEM_FREE(tbufp,mvfs_auditbufsiz);
    }
#endif

    MVFS_FREE_VATTR_FIELDS(&uvp->va);
    KMEM_FREE(uvp, sizeof(*uvp));
    if (cvp) CVN_RELE(cvp);
    MFS_ENBAUDIT(mth);
    if (error) {
	afp->auditwerr = error;
        mvfs_logperr(MFS_LOG_WARN, error, "audit write to %s", afp->path);
    }
    afp->lastpos = NULL;		/* Reset ptrs */
    afp->curpos  = afp->buf;
out:
    MVFS_UNLOCK(&afp->lock);
    MFS_BUMPTIME(stime, dtime, mfs_austat.au_time);
    return;
}
static const char vnode_verid_mvfs_auditops_c[] = "$Id:  198c0ea1.eafe11dc.947a.00:01:83:09:5e:0d $";
