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
/* mvfs_mioctl.c */

#include "mvfs_systm.h"
#include <tbs_base.h>
#include <tbs_errno.h>
#include <credutl_kernel.h>
#include "mvfs.h"
#include "mvfs_dnc.h"
#include "mvfs_copy.h"

STATIC int MVFS_NOINLINE
mvfs_copyout_vobinfo(
    mvfs_vobinfo_t *vip,
    struct mfs_mntinfo *mmi,
    tbs_boolean_t unique ,
    mvfscmd_block_t *cmdblk,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_copyout_viewinfo(
    mvfs_viewinfo_t *vip,
    VNODE_T *vp
);

STATIC int MVFS_NOINLINE
mvfs_do_inval(
    tbs_oid_t *replica_oid_p,
    mvfs_ioinval_t *invp,
    VNODE_T *cdir,
    VNODE_T *vp,
    CALL_DATA_T *cd
);

STATIC int MVFS_NOINLINE
mvfs_get_vobinfo(
    VNODE_T *vp, 
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_setprocview(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_procviewinfo(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_viewinfo(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_xstat(
    VNODE_T *vp,
    CLR_VNODE_T *cvp,
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_clrname(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_vfh(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_ioinval(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    VNODE_T *cdir,
    MVFS_CALLER_INFO *callinfo
);

STATIC int
mvfs_mkviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_rmviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_exportviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_unexportviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_viewtag_export(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_viewaddr(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_xattr(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_set_xattr(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_viewtag_dir(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_read_dnc(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_loginfo(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_set_loginfo(
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_stats(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_rmallviewtags(
    mvfscmd_block_t *data,
    CRED_T *cred
);

STATIC int MVFS_NOINLINE
mvfs_get_poolmaps(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_zero_stats(CRED_T *cred);

STATIC int MVFS_NOINLINE
mvfs_get_cache_usage(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_set_cache_sizes(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_cache_sizes(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_compute_cache_defaults(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_view_stats(
     mvfscmd_block_t *data,
     CRED_T *cred,
     MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_zero_view_stats(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_sidhost_credmapping(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_delete_sidhost_credmapping(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_set_vobrt_vfsmnt(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
);

STATIC int MVFS_NOINLINE
mvfs_get_gfsinfo(
     mvfscmd_block_t *data,
     MVFS_CALLER_INFO *callinfo
);

STATIC void
mvfs_addup_clntstat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_mnstat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_clearstat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_rvcstat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_dncstat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_acstat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_rlstat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_austat(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_vnopcnt(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_vfsopcnt(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_viewopcnt(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_viewophist(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_addup_viewoptime(
    mvfs_stats_data_t *sdp,
    mvfs_stats_data_t *total_sdp
);

STATIC void
mvfs_add_times(
      timestruc_t *sdp_time,
      timestruc_t *percpu_time
);

/*
 * MVFS_IOCTL_COPYIN - copyin the ioctl command block and lookup pathname
 * returning vnode ptr if command takes a pathname.
 *
 * NOTE: The ioctl handlers are responsible for copying in/out
 *       data specific to the ioctl command. 
 */

int
mvfs_ioctl_copyin(
    mvfscmd_block_t *data,
    mvfscmd_block_t *iocbufp,
    caddr_t *infop,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;

    /* Check for no buffer */

    if (iocbufp == NULL) return(ENOMEM);

    return(CopyInMvfscmd_block((caddr_t)data, iocbufp, callinfo));
}

/*
 * MVFS_IOCTL_LOOKUP - lookup pname in many ioctls
 * Return both vpp and cvpp held, unless *vpp is an MVFS node, in which
 * case *cvpp may be returned as NULL.
 */

int
mvfs_ioctl_lookup(
    mvfscmd_block_t *iocbuf,
    VNODE_T **vpp,
    CLR_VNODE_T **cvpp,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
)
{

    int error = 0;
    CLR_VNODE_T *cvp;
    VNODE_T *vp;
    mvfs_thread_t *mth;
    SYMFOLLOW_T lookflag;
    int extra_lookflag;
    VNODE_T *xvp;
    int mfs_obj_only = 1;	/* MFS objects only for ioctl */
    int bindroot = 1;		/* Bind root if object is vob root */
    CLR_VNODE_T *rcvp = NULL;   /* Avoid a compiler warning by initializing. */
    int s;
    char *pn;

    MFS_CHKSP(STK_LOOKUPNAME);

    /* Clear return pointers in case of errors. */
    *cvpp = NULL;
    *vpp = NULL;
    /* 
     * Most ioctl's take a pname.  Set appropriate flags.
     */

    lookflag = (MCB_PNFLAGS(iocbuf) & MVFS_CMD_PN_NOFOLLOW_SYMLINK) ? NO_FOLLOW : FOLLOW_LINK;
    extra_lookflag = (MCB_PNFLAGS(iocbuf) & MVFS_CMD_PN_CS_LOOKUP) ? 
		MVFS_RO_LOOKUP : MVFS_RO_LOOKUP|MVFS_CI_LOOKUP;
    pn = (MCB_OBJPN(iocbuf)).s;

    switch (MCB_CMD(iocbuf)) {
  	case MVFS_CMD_GET_VOBINFO:  {
            /* For this command, we need to look into the data to check for
             * a pathname, so we must copy it in here. 
             */
	    mvfs_vobinfo_t *vobinfo_p = KMEM_ALLOC(sizeof(*vobinfo_p),
                                                   KM_SLEEP);
	    if (vobinfo_p == NULL) {
                iocbuf->status = tbs_errno2status(ENOMEM);
                return(-1);
            }

	    if ((error = CopyInMvfs_vobinfo(iocbuf->infop, vobinfo_p, callinfo)) != 0) {
                iocbuf->status = tbs_errno2status(error);
                KMEM_FREE(vobinfo_p, sizeof(*vobinfo_p));
                return(-1);
            }

	    pn = NULL;
	    bindroot = 0;
	    /* lookup only if MVFS_VOBINFO_IN_PNAME */
	    if (vobinfo_p->utype != MVFS_VOBINFO_IN_PNAME) {
                KMEM_FREE(vobinfo_p, sizeof(*vobinfo_p));
		return(0);
	    }
	    pn = (MFS_STRBUFPN_PAIR_GET_KPN(&(vobinfo_p->vobid.pn_s.pair))).s;
	    lookflag = (vobinfo_p->vobid.pn_s.pnflags & MVFS_CMD_PN_NOFOLLOW_SYMLINK) ? NO_FOLLOW : FOLLOW_LINK;
	    extra_lookflag = (vobinfo_p->vobid.pn_s.pnflags & 
				MVFS_CMD_PN_CS_LOOKUP) ? 
			          MVFS_RO_LOOKUP|MVFS_NB_LOOKUP :
				  MVFS_RO_LOOKUP|MVFS_CI_LOOKUP|MVFS_NB_LOOKUP;
            KMEM_FREE(vobinfo_p, sizeof(*vobinfo_p));
	    break;
        }
	case MVFS_CMD_CHANGE_MTYPE:
	case MVFS_CMD_GET_VIEWINFO:
	case MVFS_CMD_GET_CLRNAME:
	case MVFS_CMD_GET_VFH:
	case MVFS_CMD_GET_VIEWADDR:
	case MVFS_CMD_GET_XATTR:
	case MVFS_CMD_IOINVAL:
	case MVFS_CMD_SET_XATTR:
	    break;
	case MVFS_CMD_XSTAT:
	    mfs_obj_only = 0;
	    break;
	default:
	    return(0);		/* No lookup for this ioctl */
    }

    /* 
     * Pname ptr is required.
     */
    if (pn == NULL) {
        iocbuf->status = TBS_ST_EINVAL;
	return(-1);
    }

    /*
     * INHIBIT auditing of lookups in ioctl's.  They
     * can cause stack overflows on some systems, and
     * we don't want all this meta-junk in the audit.
     */
 
    mth = MVFS_MYTHREAD(cd); 
    MFS_INHAUDIT(mth);

    /* Do the name lookup */

    vp = NULL;
    cvp = NULL;
    error = LOOKUP_FOR_IOCTL(pn, UIO_USERSPACE, lookflag, extra_lookflag, 
						NULL, &cvp,cd);
    if (error) {
	ASSERT(cvp == NULL);
	iocbuf->status = tbs_errno2status(error);
   	error = -1;		/* Flag error status */
	goto out;
    }
    if (MVOP_REALCVP(cvp, &rcvp) == 0 && rcvp != cvp) {
	/*
	 * got a realvp through the operation, use that instead.
	 */
	CVN_HOLD(rcvp);
	CVN_RELE(cvp);
	cvp = rcvp;
    }
    vp = MVFS_CVP_TO_VP(cvp);
    VN_HOLD(vp);

    /*
     * The new xstat functions work on non-MFS objects too!
     */

    if (mfs_obj_only) {
        if (!MFS_VPISMFS(vp)) {
            VN_RELE(vp);
            CVN_RELE(cvp);
	    vp = NULL;
            cvp = NULL;
	    iocbuf->status = TBS_ST_NOT_AN_OBJ;
	    error = -1;
	    goto out;
	}
    }
    if (MFS_VPISMFS(vp)) {
        /* don't return CLR_VNODE_T when returning MVFS node */
        CVN_RELE(cvp);
        cvp = NULL;
    }

    /* 
     * Bind root... except for getvobinfo calls. 
     * These calls work even without a view on the vob root. 
     */
    if (MFS_VPISMFS(vp) && MFS_ISVOBRT(VTOM(vp)) && bindroot) {
        xvp = mfs_bindroot(vp, MVFS_VIEW_CREDS(vp, cd), &error);

	/* The looked up vnode is no longer needed.
	   If bindroot failed, then we need to release the
	   lookup ref before returning the error.  If bindroot
	   succeeded, then xvp is a different vnode and
	   we want that vnode, not the unbound root. */

	VN_RELE(vp);
	vp = NULL;
	if (error == ESRCH) {
            iocbuf->status = TBS_ST_NOT_AN_OBJ;
            error = -1;
	    goto out;
	} else if (error) {
            iocbuf->status = tbs_errno2status(error);
            error = -1;
	    goto out;
   	} else {
	    vp = xvp;
            ASSERT(MFS_VPISMFS(vp));
	}
    }

    /* Enable auditing */

out:
    MFS_ENBAUDIT(mth);

    /*
     * Rebind (if needed) when the vnode is in the VOB
     * before the ioctl gets any further, but after the
     * audit inhibit is off. (rebind_vpp is inhibited by
     * the audit inhibit bit).  If there is an error,
     * then vp should be null and the VPISMFS macro will
     * return false.
     */

    if (MFS_VPISMFS(vp) && MFS_ISVOB(VTOM(vp))) {
	mfs_rebind_vpp(1, &vp, MVFS_VIEW_CREDS(vp, cd));
        ASSERT(MFS_VPISMFS(vp));
    }
    if (vp != NULL) {
        if (MFS_VPISMFS(vp)) {
            ASSERT(cvp == NULL);
        } else {
            ASSERT(cvp && MVFS_CVP_TO_VP(cvp) == vp);
        }
    } else {
        ASSERT(cvp == NULL);
        ASSERT(error != 0);
    }
    *cvpp = cvp;
    *vpp = vp;
    return(error);
}

/*
 * mvfs_ioctl_copyout
 *   This routine updates the status in the mvfs's cmdblock.
 *   NOTE: The user is now responsible for copying out data
 *         specific to the ioctl cmd.
 */
int
mvfs_ioctl_copyout(
    mvfscmd_block_t *data,
    mvfscmd_block_t *iocbufp,
    caddr_t infop,
    MVFS_CALLER_INFO *callinfo 
)
{
    int error = 0;

    /* now get the status to be returned */

    error = CopyOutMvfscmd_block_status(iocbufp, (caddr_t) data, callinfo);
    return(error);
}

/*
 * Other internal routines used to copyout info
 * for VOBINFO, VIEWINFO type calls.
 */

STATIC int MVFS_NOINLINE
mvfs_copyout_viewinfo(vip, vp)
mvfs_viewinfo_t *vip;
VNODE_T *vp;
{
    VNODE_T *vw;
    int error = 0;
    mfs_mnode_t *mnp;

    vw = MFS_VIEW(vp);
    if (vw == NULL) return(EINVAL);

    mnp = VTOM(vw);
    ASSERT(MFS_ISVIEW(mnp));

    MLOCK(mnp);		/* Lock view object */

    /* Check for view w/out tagname */

    if (mnp->mn_view.viewname == NULL) {
	mvfs_log(MFS_LOG_ESTALE, 
		"ioctl: stale view vw=%s vob=%s dbid=0x%x\n",
		mfs_vp2vw(vp), 
		mfs_vp2dev(vp), mfs_vp2dbid(vp));
	MUNLOCK(mnp);
	return(ESTALE);
    }

    error = mfs_copyout_strbuf(vip->vname, mnp->mn_view.viewname);
    if (error) goto out;
    error = mfs_copyout_strbufpn(vip->spath, MFS_STRBUFPN_PAIR_GET_UPN(&mnp->mn_view.svr.lpn).s);
    if (error) goto out;
    error = mfs_copyout_strbuf(vip->host, mnp->mn_view.svr.host);
    if (error) goto out;
    error = mfs_copyout_strbufpn(vip->rpath, mnp->mn_view.svr.rpn);
    if (error) goto out;
    vip->uuid = mnp->mn_view.svr.uuid;

out:
    MUNLOCK(mnp);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_copyout_vobinfo(
    mvfs_vobinfo_t *vip,
    struct mfs_mntinfo *mmi,
    tbs_boolean_t unique,
    mvfscmd_block_t *cmdblk,
    MVFS_CALLER_INFO *callinfo 
)
{
    int error;

    error = mfs_copyout_strbufpn(vip->mntpath, mmi->mmi_mntpath);
    if (error) return(error);
    error = mfs_copyout_strbufpn(vip->spath, MFS_STRBUFPN_PAIR_GET_UPN(&mmi->mmi_svr.lpn).s);
    if (error) return(error);
    error = mfs_copyout_strbuf(vip->host, mmi->mmi_svr.host);
    if (error) return(error);
    error = mfs_copyout_strbufpn(vip->rpath, mmi->mmi_svr.rpn);
    if (error) return(error);
    vip->oid = mmi->mmi_voboid;
    BCOPY(&mmi->mmi_vobuuid, &vip->uuid, sizeof(tbs_uuid_t));
    vip->unique = unique;
    error = CopyOutMvfs_vobinfo(vip, cmdblk->infop, callinfo);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_do_inval(
    tbs_oid_t *replica_oid_p,
    mvfs_ioinval_t *invp,
    VNODE_T *cdir,
    VNODE_T *vp,
    CALL_DATA_T *cd
)
{
    char *inv_nm = NULL;
    int mnum;
    VNODE_T *xvp;
    tbs_uuid_t vob_uuid;
    mfs_mnode_t *mnp;
    int error = 0;
    tbs_boolean_t safe;
    VFS_T *vfsp;

    /* Now, switch on invalidate type.  Always invalidate
       the view first, then local information.  That way
       there should be no races that re-fetch stale info
       from the view (by another process) after the local
       object is invalidated, but before the view processes
       the invalidate request.  */

    switch (invp->invaltype) {
    case MVFS_INV_NC:
	error = mfs_copyin_strbufpn(invp->nm, &inv_nm);
	if (error) break;
	error = mfs_clnt_inval(vp,VIEW_INVALIDATE_TYPE_NAME,
			       replica_oid_p, &invp->obj_oid, inv_nm,
			       MVFS_VIEW_CREDS(vp, cd));
	mnum = 0;
        vfsp = NULL;
        safe = FALSE;
	while ((mnp = mfs_mngetnextoid(&mnum, vp, 
				       replica_oid_p,
				       &invp->obj_oid)) != NULL) {
	    if ((error = MVFS_VNGET(mnp->mn_hdr.vfsp, NULL, mnp, &xvp)) != 0) {
		continue;
	    }
	    MLOCK(VTOM(xvp));
	    MFS_ATTRINVAL(xvp);  /* Before DNLC so that rddir cache does not lag */
            /* remove name */
	    safe = (mfs_dncremove(xvp, inv_nm,
                                  MVFS_VIEW_CREDS(xvp, MVFS_CD2CRED(cd))) != 0);
            /*
             * If we were able to find the entry in the DNC, then
             * the RVC was purged if appropriate and safe == TRUE.
             */
            vfsp = mnp->mn_hdr.vfsp;    /* take copy of last known vfsp */
	    MUNLOCK(VTOM(xvp));
	    VN_RELE(xvp);
	    xvp = NULL;
	}
	STRFREE(inv_nm);
        /*
         * We need to flush this replica's VOB root for this view,
         * because the RVC entry probably was not known under the name
         * that was just flushed, or the mnode might have been purged
         * from the cache before we got here and thus wasn't found by
         * mngetnextoid().  In either case the DNC RVC entry may still
         * be marked valid yet have the wrong fid.
         */
        if (!safe) {
            if (!vfsp) {
                mfs_findvfs_lock();
                vfsp = mfs_findvfs_uuid(&replica_oid_p->obj_uuid);
                mfs_findvfs_unlock();
                /*
                 * We don't really care if the VFS_T pointed to by vfsp is
                 * GC'ed--we don't reference the structure's contents.  We
                 * only care that any DNC entries which would point to
                 * mnodes in it get flushed.  (That will happen if the VOB
                 * is unmounted, so if the VFS goes and another one is
                 * allocated later, all we suffer is potentially excess
                 * cache flushing, not correctness problems.)
                 */
            }
            mvfs_rvcflush(vp, vfsp);
        }
	error = 0;
	break;

    case MVFS_INV_OBJ:
    case MVFS_INV_ELEM:
	error = mfs_clnt_inval(vp, VIEW_INVALIDATE_TYPE_OBJ,
			       replica_oid_p, &invp->obj_oid, NULL,
			       MVFS_VIEW_CREDS(vp, cd));
	mnum = 0;
        safe = FALSE;
	while ((mnp = mfs_mngetnextoid(&mnum, vp, 
				       replica_oid_p,
				       &invp->obj_oid)) != NULL) {
	    if ((error = MVFS_VNGET(mnp->mn_hdr.vfsp, NULL, mnp, &xvp)) != 0) {
		continue;
	    }
	    MLOCK(VTOM(xvp));
	    MFS_ATTRINVAL(xvp);
	    if (MVFS_ISVTYPE(xvp,VREG)) {
		mfs_clear_mark_purge(xvp);
		/*
		 * Don't leave the cleartext null, as some routines
		 * (especially paging routines) have trouble fetching
		 * it.  If we can't get it...  well, too bad, not much
		 * we can do here.  Leave it null and let the paging
		 * routine get the error.
		 */
	    }

            if (!safe && invp->invaltype == MVFS_INV_ELEM) {
                /* 
                 * Update the dir attribute cache flush time, so all of
                 * the ac and rdc entries before this time will be flushed.
                 * It's needed by "rmelem" operation only.
                 */
                VFS_TO_MMI(mnp->mn_hdr.vfsp)->mmi_ac_dir_ftime = MDKI_CTIME();
            }

            /*
             * If the object being invalidated happens to be the bound VOB
             * root for this view, this call to mfs_dnc_invalvp() will flush
             * an RVC entry from the DNC, so set a flag to avoid doing it
             * again.
             */
            safe = TRUE;
            /* 
             * Invalidate name cache for translations to and from
             * this object.
             */
	    mfs_dnc_invalvp(xvp);
	    MUNLOCK(VTOM(xvp));
	    VN_RELE(xvp);
	    xvp = NULL;
	}
	/* 
	 * Must flush all ENOENTS that might change without any
         * change to the dir (e.g. label change could make object
         * appear).  See the name cache code for more info on this
         * call.
	 */
	mfs_dnc_inval_obj_not_found();
        if (!safe) {
            /* Since we only get here if we didn't find any vnodes,
             * there was no sense in saving the vfsp.
             */
            mfs_findvfs_lock();
            vfsp = mfs_findvfs_uuid(&replica_oid_p->obj_uuid);
            mfs_findvfs_unlock();
            if (vfsp) {
                if (invp->invaltype == MVFS_INV_ELEM)
                    VFS_TO_MMI(vfsp)->mmi_ac_dir_ftime = MDKI_CTIME();
                /*
                 * Must remove this VOB root for this view from the
                 * DNC, because flushing by mnode didn't find one.  If
                 * there isn't a mount for this replica, don't flush
                 * since there's nothing to worry about.
                 */
                mvfs_rvcflush(vp, vfsp);
            }
        }
	error = 0;
	break;

    case MVFS_INV_VIEW:
	error = mfs_clnt_inval(vp, VIEW_INVALIDATE_TYPE_VIEW,
			       &TBS_OID_NULL, &TBS_OID_NULL, NULL,
			       MVFS_VIEW_CREDS(vp, cd));
	/* What about active vnodes?  HACK: Just do cdir! */
	if (MFS_VPISMFS(cdir) && MFS_ISVOB(VTOM(cdir))) {
	    MFS_ATTRINVAL(cdir);  /* Before DNLC so that rddir cache does not lag */
	}
        /* mvfs_dnc_invalvw() effects include an rvcflush */
	mfs_dnc_invalvw(vp);	/* Invalidate name cache for this view */
	mfs_mnflushvw(vp);	/* Flush cached vnodes for view */
	break;

	/* Invalidate VFS invalidates the whole mount point. */
    case MVFS_INV_VFS:
	error = mfs_clnt_inval(vp, VIEW_INVALIDATE_TYPE_VOB,
			       replica_oid_p, &TBS_OID_NULL, NULL,
			       MVFS_VIEW_CREDS(vp, cd));
	/* HACK: invalidate attrs on u_cdir if mfs */
	if (MFS_VPISMFS(cdir) && MFS_ISVOB(VTOM(cdir))) {
	    MFS_ATTRINVAL(cdir);  /* Before DNLC so that rddir cache does not lag */
	}
	mfs_dncflush();		/* Flush whole name cache */
	mfs_mnflush();		/* Flush all vnodes */
        /* RVC was flushed as part of flush of name cache */
	break;

    default:
	error = EINVAL;
	break;
    }
    return error;
}
/* MVFS_MIOCTL - process special MVFS ioctl's */
/*ARGSUSED*/
int
mvfs_mioctl(
    VNODE_T *vp,
    CLR_VNODE_T *cvp,
    mvfscmd_block_t *data,
    int flag,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mfs_strbuf_t *strbufp;
    VNODE_T *cdir;
    VNODE_T *rvp = NULL;
    u_long *ulp;
    mfs_strbuf_t strbuf_data;
    u_long mvfs_data;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    /*
     * Fill in initial status
     */
    data->status = TBS_ST_OK;
    switch (MCB_CMD(data)) {

	/* VOB and VOB object related operations */

	case MVFS_CMD_GET_VOBINFO: {
            error =  mvfs_get_vobinfo(vp, data, callinfo);
	    break;
	}

	/* Set process view */
	case MVFS_CMD_SETPROCVIEW: {
            error = mvfs_setprocview(data, MVFS_CD2CRED(cd), callinfo);
            break;
	}

	/* Get current process view info */

	case MVFS_CMD_GET_PROCVIEWINFO: {
            error = mvfs_get_procviewinfo(data, callinfo);
            break;
	}

	case MVFS_CMD_GET_VIEWINFO: {
            error = mvfs_get_viewinfo(vp, data, MVFS_CD2CRED(cd), callinfo);
	    break;
	}

	/*
	 * MVFS_CMD_XSTAT - xstat call.
	 */
	case MVFS_CMD_XSTAT: {
            error = mvfs_xstat(vp, cvp, data, cd, callinfo);
            break;
	}
	case MVFS_CMD_GET_CLRNAME: {
            error = mvfs_get_clrname(vp, data, cd, callinfo);
            break;
	}
	/*
 	 * Get the view file handle
	 */

	case MVFS_CMD_GET_VFH: {
            error = mvfs_get_vfh(vp, data, callinfo);
	    break;
	}

	/*
	 * Invalidate caches:
	 * 	Invalidate by OID is the preferred way.
	 */
	case MVFS_CMD_IOINVAL: {
            cdir = MDKI_GET_U_CDIR();
            if (cdir == NULL) {
                error = MVFS_NULL_U_CDIR_ERR;
                break;
            }

            if (MVOP_REALVP(cdir, &rvp) == 0 && cdir != rvp) {
                /*
                 * there was an underlying real VP, we'd better use that.
                 */
                MDKI_VNHOLD_RCDIR(rvp);
                MDKI_VNRELE_RCDIR(cdir);
                cdir = rvp;			/* no need to frob ref cnts */
            }
            error = mvfs_ioinval(vp, data, cd, cdir, callinfo);
            if (cdir != NULL) {
                MDKI_VNRELE_RCDIR(cdir);
            }
            break;
        }
	/* Note:
	 * mkviewtag and rmviewtag have no access checking on
         * them.  this matches the previous access checks done
	 * by cleartool_priv command.
	 */

	case MVFS_CMD_MKVIEWTAG: {
            error = mvfs_mkviewtag(data, MVFS_CD2CRED(cd), callinfo);
            break;
        }

	case MVFS_CMD_RMVIEWTAG: {
            error = mvfs_rmviewtag(data, MVFS_CD2CRED(cd), callinfo);
            break;
        }

	case MVFS_CMD_EXPORTVIEWTAG: {
            error = mvfs_exportviewtag(data, MVFS_CD2CRED(cd), callinfo);
	    break;
	}
	case MVFS_CMD_UNEXPORTVIEWTAG: {
            error = mvfs_unexportviewtag(data, MVFS_CD2CRED(cd), callinfo);
	    break;
	}
	case MVFS_CMD_GET_VIEWTAG_EXPORT: {
            error = mvfs_get_viewtag_export(data, MVFS_CD2CRED(cd), callinfo);
	    break;
	}
	case MVFS_CMD_GET_VIEWADDR: {
            error = mvfs_get_viewaddr(vp, data, callinfo);
	    break;
	}

        case MVFS_CMD_CHANGE_MTYPE: {
	    mvfs_iochange_mtype_t change_mtype;

	    if ((error = CopyInMvfs_iochange_mtype(data->infop, &change_mtype, callinfo)) != 0)
	        break;

	    error = mfs_clnt_change_mtype(vp, change_mtype.mtype,
                                     &data->status,  
                                     MVFS_VIEW_CREDS(vp, cd));
	    break;
	}

	case MVFS_CMD_GET_XATTR: {
            error = mvfs_get_xattr(vp, data, callinfo);
	    break;
	}

	case MVFS_CMD_SET_XATTR: {
            error = mvfs_set_xattr(vp, data, cd, callinfo);
	    break;
	}

	case MVFS_CMD_GET_BH:
	case MVFS_CMD_SET_BH:
	case MVFS_CMD_GET_AFILE:
	case MVFS_CMD_SET_AFILE:
	case MVFS_CMD_GET_PROCF:
	case MVFS_CMD_SET_PROCF:
	case MVFS_CMD_START_AUDIT:
	case MVFS_CMD_STOP_AUDIT:
	case MVFS_CMD_SYNC_AUDIT:
	case MVFS_CMD_REVALIDATE:
	case MVFS_CMD_AUDIT_MARKER:
	    error = mfs_auditioctl(data, cd, callinfo);
	    break;
	case MVFS_CMD_GET_VXSUFFIX: {
	    if ((error = CopyInMfs_strbuf(data->infop, 
                                          &strbuf_data, callinfo)) != 0)
	        break;

	    mfs_findvfs_lock();       
	    if (vrdp->mfs_viewroot_vfsp) {
	        error = mfs_copyout_strbuf(strbuf_data, 
			VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_hmsuffix);
	    } else {
		error = ENODEV;
	    }
	    mfs_findvfs_unlock();       
	    error = CopyOutMfs_strbuf(&strbuf_data, data->infop, callinfo);
	    break;

	}

	case MVFS_CMD_GET_VIEWTAG_DIR: {
            error = mvfs_get_viewtag_dir(data, callinfo);
	    break;
	}
	/*
	 *  Cache enable / flush operations
         */

 	case MVFS_CMD_GET_CACHE_ENB: {
	    ulp = &mvfs_data;
	    *ulp = 0;
	    if (mcdp->mvfs_acenabled) *ulp |= MVFS_CE_ATTR;
	    if (mcdp->mvfs_dncenabled) *ulp |= MVFS_CE_NAME;
	    if (mcdp->mvfs_dncnoentenabled) *ulp |= MVFS_CE_NOENT;
	    if (mcdp->mvfs_rvcenabled) *ulp |= MVFS_CE_RVC;
	    if (mcdp->mvfs_rlenabled) *ulp |= MVFS_CE_SLINK;
	    if (mcdp->mvfs_ctoenabled) *ulp |= MVFS_CE_CTO;
	    if (mcdp->mvfs_rebind_dir_enable) *ulp |= MVFS_CE_CWDREBIND;
	    error = CopyOutMvfs_u_long(ulp, data->infop,callinfo);
	    break;
	}

	case MVFS_CMD_SET_CACHE_ENB: {
	    ulp = &mvfs_data;
	    if (!MDKI_SUSER(MVFS_CD2CRED(cd))) {
		error = EPERM;
	 	MDKI_SET_U_ERROR(0);   /* Eliminate suser side effect */
		break;
	    }

	    if ((error = CopyInMvfs_u_long((caddr_t)(data->infop), ulp, callinfo)) != 0)
	        break;

	    mcdp->mvfs_acenabled = (*ulp & MVFS_CE_ATTR) ? 1 : 0;
	    mcdp->mvfs_dncenabled = (*ulp & MVFS_CE_NAME) ? 1 : 0;
	    mcdp->mvfs_dncnoentenabled = (*ulp & MVFS_CE_NOENT) ? 1 : 0;
	    mcdp->mvfs_rvcenabled = (*ulp & MVFS_CE_RVC) ? 1 : 0;
	    mcdp->mvfs_rlenabled = (*ulp & MVFS_CE_SLINK) ? 1 : 0;
	    mcdp->mvfs_ctoenabled = (*ulp & MVFS_CE_CTO) ? 1 : 0;
	    mcdp->mvfs_rebind_dir_enable = (*ulp & MVFS_CE_CWDREBIND) ? 1 : 0;
	    break;
	}

	case MVFS_CMD_FLUSH_CACHE: {
	    ulp = &mvfs_data;
            if ((error = CopyInMvfs_u_long((caddr_t)(data->infop), ulp, callinfo)) != 0)
	        break;

	    /* Allow anyone to flush caches (they can already via
               other invalidate calls */
	    if (*ulp & MVFS_CF_MN) mfs_mnflush();
	    if (*ulp & MVFS_CF_NC) mfs_dncflush();
	    if (*ulp & MVFS_CF_RVC) mfs_viewdirflushrvc();
	    if (*ulp & MVFS_CF_LKUP) MVFS_FLUSH_CREDLIST(TRUE);
	    break;
	}

	case MVFS_CMD_READ_DNC: {
            error = mvfs_read_dnc(data, callinfo);
	    break;
	}

	case MVFS_CMD_GET_RCSID: {
	    if ((error = CopyInMfs_strbuf(data->infop, 
                                          &strbuf_data, callinfo)) != 0)
            {
		break;
            }
	    if ((error = mfs_copyout_strbuf(strbuf_data, 
                                            MVFS_RCSID_STRING_VAR)) != 0)
            {
		break;
            }
	    error = CopyOutMfs_strbuf(&strbuf_data, data->infop, callinfo);
	    break;
	}
	case MVFS_CMD_GET_SCCSID: {
	    if ((error = CopyInMfs_strbuf(data->infop, 
                                          &strbuf_data, callinfo)) != 0)
            {
		break;
            }
	    if ((error = mfs_copyout_strbuf(strbuf_data, 
                                            MVFS_SCCSID_STRING_VAR)) != 0)
            {
		break;
            }
	    error = CopyOutMfs_strbuf(&strbuf_data, data->infop, callinfo);
	    break;
	}

	case MVFS_CMD_GET_LOGINFO: {
            error = mvfs_get_loginfo(data, callinfo);
	    break;
	}

	case MVFS_CMD_SET_LOGINFO: {
            error = mvfs_set_loginfo(data, cd, callinfo);
	    break;
	}

    case MVFS_CMD_GET_STATS: {
            error = mvfs_get_stats(data, callinfo);
	    break;
  	}

#if (defined(MVFS_DEBUG) || defined(MVFS_CRASH_DEBUG))
	case MVFS_CMD_ABORT: {
	    extern int mdb_crash;
	    if (MDKI_INGLOBALZONE() && MDKI_SUSER(MVFS_CD2CRED(cd)))
		mdb_crash = 1;
	    else {
		error = EPERM;
		MDKI_SET_U_ERROR(0);	/* Eliminate suser side effect. */
	    }
	    break;
	}
#endif
	case MVFS_CMD_RMALLVIEWTAGS: {
            error = mvfs_rmallviewtags(data, MVFS_CD2CRED(cd));
	    break;
	}

	case MVFS_CMD_UNMOUNTALL: {
	    error = mfs_unregister_all_vobs(MVFS_CD2CRED(cd));
	    break;
	}

	case MVFS_CMD_GET_POOLMAPS: {
            error = mvfs_get_poolmaps(data, callinfo);
	    break;
	}

        case MVFS_CMD_ZERO_STATS: {
            error = mvfs_zero_stats(MVFS_CD2CRED(cd));
	    break;
	}
	case MVFS_CMD_GET_CACHE_USAGE: {
            error = mvfs_get_cache_usage(data, callinfo);
	    break;
	}
	case MVFS_CMD_SET_CACHE_SIZES: {
            error = mvfs_set_cache_sizes(data, MVFS_CD2CRED(cd), callinfo);
	    break;
	}
	case MVFS_CMD_GET_CACHE_SIZES: {
            error = mvfs_get_cache_sizes(data, callinfo);
	    break;
	}
	case MVFS_CMD_COMPUTE_CACHE_DEFAULTS: {
            error = mvfs_compute_cache_defaults(data, callinfo);
	    break;
	}
	case MVFS_CMD_GET_VIEW_STATS: {
            error = mvfs_get_view_stats(data, MVFS_CD2CRED(cd), callinfo);
            break;
	}
	case MVFS_CMD_ZERO_VIEW_STATS: {
            error = mvfs_zero_view_stats(data, MVFS_CD2CRED(cd), callinfo);
            break;
	}
	case MVFS_CMD_SIDHOST_CREDMAPPING: {
            error = mvfs_sidhost_credmapping(data, callinfo);
	    break;
	}

	case MVFS_CMD_DELETE_SIDHOST_CREDMAPPING: {
            error = mvfs_delete_sidhost_credmapping(data, callinfo);
	    break;
	}

        case MVFS_CMD_GET_GFSINFO: {
             error = mvfs_get_gfsinfo(data, callinfo);
           break;
       }

	case MVFS_CMD_SET_VOBRT_VFSMNT: {
            error = mvfs_set_vobrt_vfsmnt(data, callinfo);
	    break;
	}

 	default:
	    error = ENOTTY;
	    break;
    }

    /* shove the real error into status */
    if ((error != 0) && (error != -1)) {
	if (data->status == TBS_ST_OK)
	    data->status = tbs_errno2status(error);
	error = -1; /* general indication */
    }

    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_vobinfo(
    VNODE_T *vp, 
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    struct mfs_mntinfo *mmi;
    VFS_T *vfsp;
    tbs_boolean_t unique;
    tbs_uuid_t uuid;
    tbs_oid_t oid;
    mvfs_vobinfo_t *vobinfop;

    if ((vobinfop = KMEM_ALLOC(sizeof(*vobinfop), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    if ((error = CopyInMvfs_vobinfo(data->infop, vobinfop, callinfo)) != 0) {
        goto cleanup;
    }
    switch (vobinfop->utype) {

        case MVFS_VOBINFO_IN_PNAME: {

            ASSERT(vp);
            if (!MFS_ISVOB(VTOM(vp)) && !MFS_ISVOBRT(VTOM(vp))) {
                data->status = TBS_ST_NOT_AN_OBJ;
                break;
            }
            mmi = V_TO_MMI(vp);		/* Get mount data */
            error = mvfs_copyout_vobinfo(vobinfop, mmi, TRUE, data, callinfo);
            break;
        }

        case MVFS_VOBINFO_IN_UUID: {
            error = CopyInTbs_uuid_s((caddr_t)vobinfop->vobid.vob_uuid, &uuid,
                                     callinfo);
            if (error)
                break;

            mfs_findvfs_lock();       /* Keep mountinfo stable */

            vfsp = mfs_findvfs_uuid(&uuid);
            if (vfsp == NULL) {
                mfs_findvfs_unlock();          
                data->status = TBS_ST_NOT_FOUND;
                break;
            }
            error = mvfs_copyout_vobinfo(vobinfop, VFS_TO_MMI(vfsp), TRUE, data,
                                         callinfo);
            mfs_findvfs_unlock();       
            break;
        }

        case MVFS_VOBINFO_IN_OID: {
            error = CopyInTbs_oid_s((caddr_t)vobinfop->vobid.vob_oid, (&oid),
                                    callinfo);
            if (error)
                break;

            mfs_findvfs_lock();       /* Keep mountinfo stable */
            vfsp = mfs_findvfs_oid(&oid, NULL, &unique);
            if (vfsp == NULL) {
                mfs_findvfs_unlock();         
                data->status = TBS_ST_NOT_FOUND;
                break;
            }

            error = mvfs_copyout_vobinfo(vobinfop, VFS_TO_MMI(vfsp), unique,
                                         data, callinfo);
            mfs_findvfs_unlock();        
            break;
        }

        case MVFS_VOBINFO_IN_COOKIE: {
            mfs_findvfs_lock();       /* Keep mountinfo stable */
            /* 
             * Find vfs by cookie and update to next 
             * entry to find 
             */
            vfsp = mfs_findvfs_cookie(&(vobinfop->vobid.cookie));
            if (vfsp == NULL) {
                mfs_findvfs_unlock();         
                data->status = TBS_ST_NOT_FOUND;
                break;
            }
            error = mvfs_copyout_vobinfo(vobinfop, VFS_TO_MMI(vfsp), TRUE, data,
                                         callinfo);
            mfs_findvfs_unlock();        
            break;
        }

        default:
            error = ENOTTY;
            break;
    }
  cleanup:
    KMEM_FREE(vobinfop, sizeof(*vobinfop));
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_setprocview(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    char *tagn = NULL;
    mvfs_viewtag_info_t viewtag_info;
    mfs_strbuf_t *strbuf;
    VNODE_T *vw;
    int  error = 0;

    if ((error = CopyInMvfs_viewtag_info(data->infop, 
                 &viewtag_info, callinfo)) != 0)
    {
        return(error);
    }

    strbuf = &viewtag_info.viewtag;

    /* 
     * For this ioctl, allow a NULL string ptr.
     * This is how a user resets to 'no current view'
     */
    if (strbuf->s != NULL || strbuf->m != 0) {
        error = mfs_copyin_strbuf(*strbuf, &tagn);
    }

    /* Look up new view tag to make sure it is OK */
            
    if (error == 0  && tagn) {
        error = mfs_viewtaglookup(tagn, &vw, cred);
        PN_STRFREE(tagn);		/* Done with tagname */
        if (error) { 
            data->status = tbs_errno2status(error);
	    return(0);
        }
    } else {
	vw = NULL;		/* Unset current view */
    }

    /* 
     * All looks OK, now jam the root dir 
     * Note: following call "uses up" a refcount on vw, so we
     * don't VN_RELE it in this level.
     */

    if (error == 0)
        error = MVFS_SET_PROCVIEW(vw, &data->status);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_procviewinfo(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    VNODE_T *cdir;
    VNODE_T *vw;
    mvfs_viewinfo_t viewinfo;
    int  error = 0;

    if ((error = CopyInMvfs_viewinfo(data->infop, &viewinfo, callinfo)) 
               != 0)
    {
        return(error);
    }

    cdir = MVFS_GET_PROCVIEW();	/* Get process view dir */
    if (cdir == NULL || !MFS_VPISMFS(cdir)) {
	error = ENOENT;		/* Not in a view */
    } else {
        vw = MFS_VIEW(cdir);	/* Get view */
        if (vw == NULL) {	/* No view */
	    error = ENOENT;
        } else {
            VN_HOLD(vw);	/* Hold view for safety */
            error = mvfs_copyout_viewinfo(&viewinfo, vw);
            VN_RELE(vw);

            if (error == 0)
                error = CopyOutMvfs_viewinfo(&viewinfo, 
                                             data->infop, callinfo);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_viewinfo(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_viewinfo_t viewinfo;
    VNODE_T *setview;

    if ((error = CopyInMvfs_viewinfo(data->infop, &viewinfo, callinfo)) 
         != 0)
    {
        return(error);
    } 

    setview = mfs_getview(NULL, cred, TRUE /* HOLD */);
    if (MFS_VIEW(vp) == NULL ||
        (MFS_VIEW(vp) == setview && MFS_ISLOOP(VTOM(vp))))
    {
        /* Don't return view info for loopback items in "our" view.
         * Some kernels have to cover every non-MVFS object, even the
         * ones in the current view.  Refusing view info for them
         * allows the auditing checks in clearmake et.al. to pass.
         * LOOKUP_FOR_AUDIT() in the MVFS auditing code will ensure
         * that the caller doesn't accidentally pass in an MVFS path
         * for an auditing file.
         */
        data->status = TBS_ST_NOT_AN_OBJ;
    } else {
        error = mvfs_copyout_viewinfo(&viewinfo, vp);

        if (error == 0)
            error = CopyOutMvfs_viewinfo(&viewinfo, data->infop, callinfo);

    }
    if (setview) VN_RELE(setview);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_xstat(
    VNODE_T *vp,
    CLR_VNODE_T *cvp,
    mvfscmd_block_t *data, 
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
)
{
    mvfs_xstat_t xstat;
    VATTR_T *vap;
    view_vstat_t *vstatp;
    mfs_mnode_t *mnp;
    int  error = 0;

    /*
     * Check for user buffer too small
     */
#if !(defined(ATRIA_LP64) || defined(ATRIA_LLP64))
    if (data->infolen < sizeof(xstat)) {
	return(EINVAL);
    }
#endif
    if ((error = CopyInMvfs_xstat(data->infop, &xstat, callinfo)) != 0)
	return(error);

    vstatp = (view_vstat_t *)KMEM_ALLOC(sizeof(*vstatp), 
                                        KM_SLEEP);
    if (vstatp == NULL) return(ENOMEM);

    vap = MVFS_VATTR_ALLOC();

    if (vap == NULL) {
        KMEM_FREE(vstatp, sizeof(*vstatp));
	return(ENOMEM);
    }

    VATTR_NULL(vap);
    VATTR_SET_MASK(vap, AT_ALL);

    /*
     * Use VOP_GETATTR to get stats for 2 reasons:
     *   - It works on non-MFS objects
     *   - For MFS objects it does all the hokum about
     *     kludging size, devs etc. for the external world.
     */
    if (MFS_VPISMFS(vp))
        error = mfs_getattr(vp, vap, 0, MVFS_VIEW_CREDS(vp, cd));
    else
        error = MVOP_GETATTR(vp, cvp, vap, 0,
                             MVFS_VIEW_CREDS(vp, MVFS_CD2CRED(cd)));
    if (error) {
        MVFS_FREE_VATTR_FIELDS(vap);
        MVFS_VATTR_FREE(vap);
	KMEM_FREE(vstatp, sizeof(*vstatp));
	return(error);
    }

    /*
     * Now, create initial vstatp for copyout.
     * Create this from the VATTR_T record,
     * and add in any oids as needed.
     */

    BZERO(vstatp, sizeof(*vstatp));
    switch(VATTR_GET_TYPE(vap)) {
        case VREG: vstatp->fstat.type = TBS_FTYPE_REG; break;
	case VDIR: vstatp->fstat.type = TBS_FTYPE_DIR; break;
	case VBLK: vstatp->fstat.type = TBS_FTYPE_BLK; break;
	case VCHR: vstatp->fstat.type = TBS_FTYPE_CHR; break;
	case VLNK: vstatp->fstat.type = TBS_FTYPE_LNK; break;
	case VFIFO: vstatp->fstat.type = TBS_FTYPE_FIFO; break;
	default: vstatp->fstat.type = TBS_FTYPE_NULL; break;
    }
    vstatp->fstat.mode = 
	(tbs_fmode_t)(VATTR_GET_MODE(vap) & VIEW_ATTR_FMODES_MASK);
    vstatp->fstat.nlink = VATTR_GET_NLINK(vap);
    MVFS_VATTR_TO_FSTAT_DB_UID(vap, &vstatp->fstat.usid); 
    MVFS_VATTR_TO_FSTAT_DB_GID(vap, &vstatp->fstat.gsid); 
    vstatp->fstat.size = (u_long) VATTR_GET_SIZE(vap);
    vstatp->fstat.nodeid = VATTR_GET_NODEID(vap);
    VATTR_GET_ATIME_TV(vap, &vstatp->fstat.atime);
    VATTR_GET_MTIME_TV(vap, &vstatp->fstat.mtime);
    VATTR_GET_CTIME_TV(vap, &vstatp->fstat.ctime);

    MVFS_FREE_VATTR_FIELDS(vap);
    MVFS_VATTR_FREE(vap);   /* Done with vap */

    /* Set oids/mtype only if MFS object under VOB */

    if (MFS_VPISMFS(vp) && MFS_ISVOB(VTOM(vp))) {
        mnp = VTOM(vp);
	/* Copy over extended stat fields */
	vstatp->mtype = mnp->mn_vob.attr.mtype;
	vstatp->elem_oid = mnp->mn_vob.attr.elem_oid;
	vstatp->obj_oid  = mnp->mn_vob.attr.obj_oid;
	vstatp->event_time = mnp->mn_vob.attr.event_time;
        xstat.vob_oid = V_TO_MMI(vp)->mmi_voboid;
	xstat.replica_uuid = V_TO_MMI(vp)->mmi_vobuuid;
        if (MFS_VIEW(vp)) {
            xstat.view_uuid = VTOM(MFS_VIEW(vp))->mn_view.svr.uuid;
	    xstat.view_hm   = VTOM(MFS_VIEW(vp))->mn_view.hm;
        } else {
	    xstat.view_uuid = TBS_UUID_NULL;
	    xstat.view_hm   = 0;
	}
	xstat.xmode = mnp->mn_vob.attr.fstat.mode & 
					~VIEW_ATTR_FMODES_MASK;
	 /* 
	  * Add in appropriate flags.
	  */

	xstat.flags    = 0;

	if (MFS_ISROOTSYNONYM(vp, V_TO_MMI(vp)->mmi_root_edbid)) 
	    xstat.flags |= MVFS_XISROOT;
	if (VTOM(MFS_VIEW(vp))->mn_view.hm) xstat.flags |= MVFS_XVXOBJ;

	if (MFS_FSTAT_AUDITED(VTOM(vp))) {
	    xstat.flags |= MVFS_XAUDITED;
	}

    }

    data->status = TBS_ST_OK;

    /* Copyout and free the vstat structure */

    if (xstat.vstat != NULL) {
	    error = CopyOutView_vstat(vstatp, 
                                      (caddr_t)xstat.vstat, callinfo);
    }

    if (error == 0) 
    	error = CopyOutMvfs_xstat(&xstat, data->infop, callinfo);

    KMEM_FREE(vstatp, sizeof(*vstatp));
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_clrname(VNODE_T *vp, 
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
)
{
    mvfs_clrname_info_t clrname_info;
    mfs_mnode_t *mnp;
    int  error = 0;

    if ((error = CopyInMvfs_clrname_info(data->infop, 
                                         &clrname_info, callinfo)) != 0)
    {
        return(error);
    }

    mnp = VTOM(vp);
    if (!MFS_ISVOB(mnp)) {	/* Only on vob objects for now */
        return(EINVAL);
    }

    MLOCK(mnp);

    /* 
     * Get cleartext name only if caller has asked for it.
     * This causes extra RPC's and may result in a "fetch"
     * of the cleartext, so it should be looked upon as an
     * "expensive" operation.
     */
    if (clrname_info.clrname.s != NULL && 
        clrname_info.clrname.m != 0) 
    {
        if (MVFS_ISVTYPE(vp, VREG) && 
            mnp->mn_vob.cleartext.nm == NULL) 
        {
            error = mfs_getcleartext(vp, NULL,  
                                     MVFS_VIEW_CREDS(vp, cd));
        } else {
            /* No cleartext expected, or already have it. */
            error = 0;
        }
        if (mnp->mn_vob.cleartext.nm && error == 0) {
            if (mnp->mn_vob.cleartext.isvob) 
                clrname_info.clrpool = MVFS_XCTVOB;
            else 
                clrname_info.clrpool = MVFS_XCTVIEW;

            error = mfs_copyout_strbufpn(clrname_info.clrname,
                                mnp->mn_vob.cleartext.nm);
        } else {
            if (error) {
                clrname_info.clrpool = MVFS_XCTERROR;
            } else {
                clrname_info.clrpool = MVFS_XCTNONE;
            }
            /* 
             * Overwrite error on copy-out, we are done with it 
             */
            error = mfs_copyout_strbufpn(clrname_info.clrname, 
                                         "");
        }
        error = CopyOutMvfs_clrname_info(&clrname_info, 
                                         data->infop, callinfo);
    }

    MUNLOCK(mnp);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_vfh(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mfs_mnode_t *mnp;
    mvfs_iovfh_t iovfh;

    if ((error = CopyInMvfs_iovfh(data->infop, &iovfh, callinfo)) == 0) {
        mnp = VTOM(vp);

        if (!MFS_ISVOB(mnp)) {	/* Only on vob objects for now */
            error = EINVAL;
        } else {
            iovfh.vfh = mnp->mn_vob.vfh; /* set view handle */
            error = CopyOutMvfs_iovfh(&iovfh, data->infop, callinfo);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_ioinval(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    VNODE_T *cdir,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_ioinval_t ioinval;
    VFS_T *vobvfs;
    mfs_mnode_t *mnp;
    tbs_uuid_t vob_uuid;
    tbs_oid_t *vob_oid_p;
    tbs_boolean_t unique;
    int indx, first;

    if (!MFS_ISVIEW(VTOM(vp))) {       /* Pname must be a view */
        return(EINVAL);
    }

    if ((error = CopyInMvfs_ioinval(data->infop, &ioinval, callinfo)) == 0)
    {

        /*
         * (int) cast is necessary for some broken compilers
         * which generate incorrect comparisons otherwise.
         */
        if (ioinval.utype == (int) MVFS_IOINVAL_VOB_OID) 
        {   
            /*
             * Locate vob uuid 
             * We may have multiple replicas of the same VOB
             * family mounted.  Loop through the vobvfs table
             * invalidating all the ones we find.
             *
             * We ignore races of new things added to the 
             * MVFS mountlist while we do the invalidations.
             */
            error = indx = 0;
            first = 1;
            do {
                mfs_findvfs_lock();         
                vobvfs = mfs_findvfs_oid(&ioinval.un.vob_oid, 
                                         &indx, &unique);
                if (vobvfs == NULL) {
                    if (first)
                        error = EINVAL;
                    mfs_findvfs_unlock();        
                    break;
                }
                mfs_findvfs_unlock();        
                vob_uuid = VFS_TO_MMI(vobvfs)->mmi_vobuuid;
                vob_oid_p = (tbs_oid_t *)&vob_uuid;
                error = mvfs_do_inval(vob_oid_p, &ioinval, 
                                      cdir, vp, cd);
                first = 0;
                if (error)
                    break;
            } while (vobvfs != NULL);
        }
        else /* just invalidate on requested replica */
            error = mvfs_do_inval((tbs_oid_t *)
                                  &ioinval.un.replica_uuid,
            		          &ioinval, cdir, vp, cd);
    }
    return(error);
}

/* 
 * UNIX version of mkviewtag 
 */
STATIC int MVFS_NOINLINE
mvfs_mkviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_mkviewtag_info_t *vtp;
    VNODE_T *vw = NULL;
    VNODE_T *dvp = NULL;
    char *rpn = NULL;
    char *host = NULL;
    char *tagn = NULL;
    CRED_T *vwcreds = NULL;
    void *attp = NULL;
    mfs_mnode_t *mnp;
    /* Declare a type so we can do one allocation to save stack space. */
    struct {
        struct mfs_svr svr;
    } *alloc_unitp;
    struct mfs_svr *svrp;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    /* Allocate stuff to keep the stack size down. */
    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    svrp = &(alloc_unitp->svr);

    if ((vtp = KMEM_ALLOC(sizeof(*vtp), KM_SLEEP)) == NULL) {
        error = ENOMEM;
        goto cleanup;
    }
    if ((error = CopyInMvfs_mkviewtag_info(data->infop, vtp, callinfo)) != 0) {
        goto cleanup;
    }
    data->status = TBS_ST_OK;

    /* 
     * Check to see if the new addr that was passed in via
     * the ioctl can actually be contacted, before assuming
     * that it is valid. Use a null RPC to the view 
     * server at that address to do so.
     */
    BZERO(svrp, sizeof(*svrp));
    svrp->addr = vtp->addr;
    svrp->down = 0;
    svrp->svrbound = 1;
    if ((error = mvfs_clnt_ping_server(svrp, cred)) != 0) {
        mvfs_log(MFS_LOG_DEBUG, "mvfs_clnt_ping_server failed with error %d\n",
                 error);
        goto cleanup;
    }

    /* Get parameters */

    if ((error = mfs_copyin_strbuf(vtp->viewtag, &tagn)) != 0) {
        goto cleanup;
    }

    /* We must prevent of history mode sufix being part of the view name */
    if (mfs_hmname(tagn, NULL)){
        error = EINVAL;
        goto cleanup;
    }

    /* We are going to use the pathname strings, so copy them in.  The
    ** CopyInMvfs_mkviewtag_info above already did the equivalent of
    ** CopyInMfs_strbufpn_pair since mkviewtag_info.spath is a
    ** mfs_strbufpn_pair_t.
    */
    MFS_STRBUFPN_PAIR_COPYIN_STRS(&(vtp->spath), error);
    if (error != 0) {
        PNPAIR_STRFREE(&(vtp->spath));
        goto cleanup;
    }
    /* Get the view server hostname and remote view storage pathname. */
    if ((error = mfs_copyin_strbuf(vtp->host, &host)) == 0) {
        error = mfs_copyin_strbufpn(vtp->rpath, &rpn);
    }
    if (error != 0)  {
        PNPAIR_STRFREE(&(vtp->spath));
        if (host != NULL)
            HN_STRFREE(host);
        goto cleanup;
    }

    /* We need to prevent an add/add race, as that will result in user-space
     * code getting EEXIST and believing the directory needs to be
     * removed/recreated, which in a thundering herd problem results in
     * spurious errors.
     */
    MVFS_LOCK(&(vrdp->mvfs_mkviewtag_lock));

    /* See if existing view-tag */
    if ((error = mfs_viewtaglookup(tagn, &vw, cred)) == 0) {
        mnp = VTOM(vw);
        ASSERT(MFS_ISVIEW(mnp));
        MLOCK(mnp);
        /* 
         * See if all setup OK already
         * This compare is always case-sensitive 
         */
        if (PN_STRCMP(FALSE,
                      MFS_STRBUFPN_PAIR_GET_KPN(&mnp->mn_view.svr.lpn).s,
                      MFS_STRBUFPN_PAIR_GET_KPN(&(vtp->spath)).s) == 0 &&
            HN_STRCMP(mnp->mn_view.svr.host, host) == 0 &&
            PN_STRCMP(FALSE, mnp->mn_view.svr.rpn, rpn) == 0 &&
            MFS_UUIDEQ(mnp->mn_view.svr.uuid, vtp->uuid))
        {
            /* Always fix with latest address */
            mnp->mn_view.svr.addr = vtp->addr;
            mnp->mn_view.svr.svrbound = 1;  /* Reset flags */
            mnp->mn_view.svr.dprinted = 0;
            mnp->mn_view.svr.uprinted = 0;
            data->status = TBS_ST_OK;
        } else {
            /* 
             * Exists, but not same info, give EEXIST error
             */
            data->status = TBS_ST_EEXIST;
        }
        MUNLOCK(mnp);
        mvfs_rvcflush(vw, NULL); /* Flush root version cache */
        if (vw) VN_RELE(vw);
    } else {
        /* Look to see if some process might still be attached to
         * it (but the tag was removed by another process). If so,
         * resurrect it.
         */
        error = mvfs_viewuuidrecover(tagn, 
                                     &vtp->uuid, host, &(vtp->spath),
                                     rpn, &vw, cred);
        switch (error) {
          case 0:
            mnp = VTOM(vw);
            MLOCK(mnp);
            mnp->mn_view.svr.addr = vtp->addr;
            mnp->mn_view.svr.svrbound = 1;  /* Reset flags */
            mnp->mn_view.svr.dprinted = 0;
            mnp->mn_view.svr.uprinted = 0;
            data->status = TBS_ST_OK;
            MUNLOCK(mnp);
            mvfs_rvcflush(vw, NULL);	/* Flush root version cache */
            VN_RELE(vw);
            goto out;

          default:
            /* some problem reattaching; return this error */
            data->status = tbs_errno2status(error);
            error = 0;
            goto out;

          case ENOENT:
            /* not found to reattach, let's do the hard work */
            break;
        }
        /* Doesn't exist... try to create it */
        dvp = mfs_getviewroot();   /* View root vnode ptr */
        if (dvp == NULL) {
            data->status = TBS_ST_MFS_ERR;
            error = 0;
            /* Make the view-tag */
        } else if ((error = mfs_viewdirmkdir(dvp, tagn, NULL, &vw, cred, host)) != 0) {
            data->status = tbs_errno2status(error);
            error = 0;
        } else {
            ASSERT(vw != NULL);
            mnp = VTOM(vw);
            ASSERT(MFS_ISVIEW(mnp));
            /* 
             * Move string ptrs into viewtag.  Null out the local ptrs so they
             * are not freed on cleanup below.
             */
            MLOCK(mnp);            /* Lock while updating */

            /* mark whether windows view or not */
            mnp->mn_view.windows_view = vtp->windows_view; 

            mnp->mn_view.svr.lpn = vtp->spath;
            /* Don't use MFS_STRBUFPN_PAIR_GET_{KPN,UPN} macros because we want
            ** to make sure we are setting both fields to NULL.
            */
            (vtp->spath).kpn.s = NULL;
            (vtp->spath).upn.s = NULL;

            /* View server hostname already filled in during mfs_viewdirmkdir call */
            mnp->mn_view.svr.rpn = rpn;   rpn = NULL;
            mnp->mn_view.svr.uuid = vtp->uuid;
            mnp->mn_view.svr.addr = vtp->addr;
            mnp->mn_view.svr.svrbound = 1;  /* reset flags */
            mnp->mn_view.svr.dprinted = 0;
            mnp->mn_view.svr.uprinted = 0;
            /* 
             * Make view handle be UUID too.  This catches cases where I am
             * talking to a different view than I think I am.
             */
            mnp->mn_view.vh.view_uuid = vtp->uuid;
            vwcreds = MDKI_GET_UCRED();
            mnp->mn_view.cuid = MDKI_CR_GET_UID(vwcreds);
            mnp->mn_view.cgid = MDKI_CR_GET_GID(vwcreds);
            MDKI_CRFREE(vwcreds);
            MUNLOCK(mnp);
            mvfs_rvcflush(vw, NULL); /* Flush root version cache */
            if (vw) VN_RELE(vw);
        }
    }
  out:
    MVFS_UNLOCK(&(vrdp->mvfs_mkviewtag_lock));

    if (dvp) VN_RELE(dvp);
    if (tagn) PN_STRFREE(tagn);
    PNPAIR_STRFREE(&(vtp->spath));
    if (rpn) PN_STRFREE(rpn);
    if (host) HN_STRFREE(host);

  cleanup:
    KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
    if (vtp != NULL) {
    KMEM_FREE(vtp, sizeof(*vtp));
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_rmviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_viewtag_info_t viewtag_info;
    VNODE_T *dvp = NULL;
    char *tagn = NULL;

    if ((error = CopyInMvfs_viewtag_info(data->infop, 
          &viewtag_info, callinfo)) == 0)
    {
        error = mfs_copyin_strbuf(viewtag_info.viewtag, &tagn);
        if (error == 0 ) {
            /* Get held vnode ptr to viewroot dir */
            dvp = mfs_getviewroot();
            if (dvp == NULL) {
                error = ENXIO;
            } else {
                /* Now, remove the view-tag */
                error = mfs_viewdirrmdir(dvp, tagn, cred);
                VN_RELE(dvp);
            }
            PN_STRFREE(tagn);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_exportviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_export_viewinfo_t export_viewinfo; 
    VNODE_T *dvp = NULL;
    VNODE_T *vw = NULL;
    char *tagn = NULL;

    if (!MDKI_SUSER(cred) || !MDKI_INGLOBALZONE()) {
        error = EPERM;
        MDKI_SET_U_ERROR(0);  /* Eliminate suser side effect. */
    } else if ((error = CopyInMvfs_export_viewinfo(data->infop, 
                &export_viewinfo, callinfo)) == 0)
    {
        /* Get parameters */
        error = mfs_copyin_strbuf(export_viewinfo.viewtag, 
                                  &tagn);
        if (error == 0 ) {
            if (mfs_hmname(tagn, NULL)) {
                data->status = TBS_ST_EINVAL;
            } else if (!MFS_VIDINRANGE(export_viewinfo.exportid))
            {
                data->status = TBS_ST_EINVAL;
            } else if ((dvp = mfs_getviewroot()) == NULL) {
                error = ENXIO;
            /* See if existing view-tag */
            } else {
                if ((error = mfs_viewtaglookup(tagn, &vw, cred)) 
                       == 0) 
                {
                    /* do view export stuff */
                    error = mvfs_viewdirexport(dvp, vw, 
                                       export_viewinfo.exportid);
                    VN_RELE(vw);
                }
                VN_RELE(dvp);
            }
            PN_STRFREE(tagn);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_unexportviewtag(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_viewtag_info_t viewtag_info;
    VNODE_T *dvp = NULL;
    VNODE_T *vp;
    char *tagn = NULL;

    if (!MDKI_SUSER(cred) || !MDKI_INGLOBALZONE()) {
        error = EPERM;
        MDKI_SET_U_ERROR(0);   /* Eliminate suser side effect */
    } else if ((error = CopyInMvfs_viewtag_info(data->infop, 
                &viewtag_info, callinfo)) == 0)
    {
        error = mfs_copyin_strbuf(viewtag_info.viewtag, &tagn);
        if (error == 0 ) {
            /* Get held vnode ptr to viewroot dir */
            dvp = mfs_getviewroot();
            if (dvp == NULL) {
                error = ENXIO;
            } else {
                if ((error = mfs_viewtaglookup(tagn, &vp, 
                            cred)) == 0) 
                {
                    error = mvfs_viewdirunexport(dvp, vp);
                    VN_RELE(vp);
                }
                VN_RELE(dvp);
            }
            PN_STRFREE(tagn);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_viewtag_export(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error;
    VNODE_T *vp;
    mvfs_export_viewinfo_t export_viewinfo;
    char *tagn = NULL;

    if ((error = CopyInMvfs_export_viewinfo(data->infop,
                      &export_viewinfo, callinfo)) != 0)
    {
        return(error);
    }

    /* Get parameters */
    if ((error = mfs_copyin_strbuf(export_viewinfo.viewtag, 
         &tagn)) == 0) {

        if ((error = mfs_viewtaglookup(tagn, &vp, cred)) == 0) {
            if (VTOM(vp)->mn_view.exid == (u_int) -1) {
                data->status = TBS_ST_NOT_FOUND;
            } else {
                export_viewinfo.exportid = VTOM(vp)->mn_view.exid;
                error = CopyOutMvfs_export_viewinfo(
                                &export_viewinfo, data->infop, callinfo);
            }
	    VN_RELE(vp);
        }
        PN_STRFREE(tagn);
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_viewaddr(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    struct mvfs_viewaddr viewaddr;
    mfs_mnode_t *mnp;

    if (MFS_VIEW(vp) == NULL) {
        error = EINVAL;
    } else if ((error = CopyInMvfs_viewaddr(data->infop, 
                                            &viewaddr, callinfo)) == 0)
    {
        mnp = VTOM(MFS_VIEW(vp));
        ASSERT(MFS_ISVIEW(mnp));
        viewaddr.addr = mnp->mn_view.svr.addr;
        error = CopyOutMvfs_viewaddr(&viewaddr, data->infop, callinfo);
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_xattr(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_io_xattr_t io_xattr;
    mfs_mnode_t *mnp;

    if ((error = CopyInMvfs_io_xattr(data->infop, &io_xattr, callinfo)) 
         == 0) 
    {
        mnp = VTOM(vp);
        data->status = TBS_ST_OK;
        if (!MFS_ISVOB(mnp)) {		/* Vob objects only */
	    data->status = TBS_ST_NOT_AN_OBJ;
        } else {
            MLOCK(mnp);
            switch (io_xattr.xattr_type) {
	        case MVFS_IO_XATTR_NULL:	
		        io_xattr.xvalue = 0;
		        break;
	        case MVFS_IO_XATTR_MTYPE:	
		        io_xattr.xvalue = (u_long)mnp->mn_vob.attr.mtype;
		        break;
	        case MVFS_IO_XATTR_XMODE:
		        io_xattr.xvalue = mnp->mn_vob.attr.fstat.mode &
				        TBS_FMODE_AUDITED_OBJ;
		        break;
	        case MVFS_IO_XATTR_NTFILEATTRS:
		        io_xattr.xvalue = 0;	/* No such attrs yet */
		        break;
	        default:
		        data->status = TBS_ST_EINVAL;
		        break;
            }
            MUNLOCK(mnp);
            if (data->status ==  TBS_ST_OK)
                error = CopyOutMvfs_io_xattr(&io_xattr, 
                                             data->infop, callinfo);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_set_xattr(
    VNODE_T *vp,
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_io_xattr_t io_xattr;
    mfs_mnode_t *mnp;

    if ((error = CopyInMvfs_io_xattr(data->infop, &io_xattr, callinfo)) 
         == 0)
    {
        mnp = VTOM(vp);
        data->status = TBS_ST_OK;
        if (!MFS_ISVOB(mnp)) {		/* Vob objects only */
	    data->status = TBS_ST_NOT_AN_OBJ;
        } else {
            switch (io_xattr.xattr_type) {
	        case MVFS_IO_XATTR_NULL:	
		    io_xattr.xvalue = 0;
		    break;
	        case MVFS_IO_XATTR_MTYPE:	
                    error = mfs_clnt_change_mtype(vp, 
				    (vob_mtype_t)io_xattr.xvalue
                                    , &data->status, 
				    MVFS_VIEW_CREDS(vp, cd));
		    break;
	        case MVFS_IO_XATTR_XMODE:
		    if (io_xattr.xvalue != TBS_FMODE_AUDITED_OBJ)
                    {
		        data->status = TBS_ST_EINVAL;
                        break;
		    } 
		    if (!MVFS_ISVTYPE(vp, VREG)) break;
		    MLOCK(mnp);
		    error = mvfs_clnt_setattr_locked(vp,
				    NULL, io_xattr.xvalue,
				    MFS_USE_PROCBH, 0,  
				    MVFS_VIEW_CREDS(vp, cd), 0);
		    MUNLOCK(mnp);
		    break;
	    case MVFS_IO_XATTR_NTFILEATTRS:
		    break;
	    default:
		    data->status = TBS_ST_EINVAL;
		    break;
            }
            if (data->status ==  TBS_ST_OK)
    	        error = CopyOutMvfs_io_xattr(&io_xattr, 
                                             data->infop, callinfo);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_viewtag_dir(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mfs_strbufpn_pair_t get_viewtag_dir_info;
    mfs_strbufpn_t *strbufpnp;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if ((error = CopyInMfs_strbufpn_pair(data->infop, 
             &get_viewtag_dir_info, callinfo)) == 0)
    {

        strbufpnp = &get_viewtag_dir_info.upn;
        mfs_findvfs_lock();         
        if (vrdp->mfs_viewroot_vfsp) {
	    error = mfs_copyout_strbufpn(*strbufpnp, 
		    VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_mntpath);
        } else {
	    error = ENODEV;
        }
        mfs_findvfs_unlock();        
        if (!error) {
            strbufpnp = &get_viewtag_dir_info.kpn;
	    mfs_findvfs_lock();         
	    if (vrdp->mfs_viewroot_vfsp) {
	        error = mfs_copyout_strbufpn(*strbufpnp, 
		    VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_mntpath);
	    } else {
	        error = ENODEV;
	    }
	    mfs_findvfs_unlock();       
        }

        /*
         * Need to Copy the length information out to the user
         */

        error = CopyOutMfs_strbufpn_pair(&get_viewtag_dir_info, 
                                         data->infop, callinfo);
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_read_dnc(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mfs_ioncent_t *ioncentp;

    if ((ioncentp = KMEM_ALLOC(sizeof(*ioncentp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    if ((error = CopyInMfs_ioncent(data->infop, ioncentp, callinfo)) == 0)
    {
        if ((error = mfs_dnc_getent(ioncentp)) == 0)
            error = CopyOutMfs_ioncent(ioncentp, data->infop, callinfo);
    }
    KMEM_FREE(ioncentp, sizeof(*ioncentp));
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_loginfo(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_loginfo_t loginfo;

    if ((error = CopyInMvfs_loginfo(data->infop, &loginfo, callinfo)) == 0)
    {
        loginfo.priority = mfs_logpri;
        loginfo.vops_mask = mdb_vops;
        loginfo.vfsops_mask = mdb_vfsops;
        loginfo.xops_mask = mdb_xops;
        loginfo.traps_mask = mdb_traps;
        loginfo.kernlog_pn.l = loginfo.kernlog_pn.m;
        loginfo.assert_panic_on = mvfs_panic_assert; 
        if ((error = mvfs_logfile_get(loginfo.kernlog_pn.s,
			     &loginfo.kernlog_pn.l)) == 0)
        {
            error = CopyOutMvfs_loginfo(&loginfo, data->infop, callinfo);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_set_loginfo(
    mvfscmd_block_t *data,
    CALL_DATA_T *cd,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_loginfo_t loginfo;
    char *logfile = NULL;

    if ((error = CopyInMvfs_loginfo(data->infop, &loginfo, callinfo)) == 0)
    {

        if (!MDKI_SUSER(MVFS_CD2CRED(cd)) || !MDKI_INGLOBALZONE()) {
            error = EPERM;
            MDKI_SET_U_ERROR(0);/* Eliminate suser side effect.*/
        } else {
            if (loginfo.mask & MVFS_LOGINFO_KERNLOGFILE) {
                if (loginfo.kernlog_pn.l == 0)
                    mvfs_logfile_close();
                else {
                    error = mfs_copyin_strbufpn(
                                            loginfo.kernlog_pn,
                                            &logfile);
                    if (error == 0) {
                        error = mvfs_logfile_set(logfile, cd);
                    }
                }
            }
            if (error == 0) {
                if (loginfo.mask & MVFS_LOGINFO_PRIORITY) {
                    mfs_logpri = loginfo.priority;
                    mvfs_mnclear_logbits();
                    MVFS_LOGBUFFER_FLUSH();
                }

                if (loginfo.mask & MVFS_LOGINFO_VOPS)
                    mdb_vops = loginfo.vops_mask;
                if (loginfo.mask & MVFS_LOGINFO_VFSOPS)
                    mdb_vfsops = loginfo.vfsops_mask;
                if (loginfo.mask & MVFS_LOGINFO_XOPS) {
                    mdb_xops = loginfo.xops_mask;
                }
                if (loginfo.mask & MVFS_LOGINFO_TRAPS)
                    mdb_traps = loginfo.traps_mask;
                if ((loginfo.mask & MVFS_LOGINFO_PANICONOFF) != 0) {
		    mvfs_panic_assert = loginfo.assert_panic_on;
                }

                error = 0;
            }
            if (logfile != NULL) PN_STRFREE(logfile);
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_stats(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    int  cpuid;
    mvfs_statbufs_t *mvfs_statbufsp;
    mvfs_stats_data_t *percpu_sdp;
    mvfs_stats_data_t *output_sdp;

    /*
     * If we have a CPU id beyond the max cpus, we print an error message before
     * returning the statistics.  We print a different error message for platforms
     * in which max cpu is obtained through an API and the ones which define it as
     * a constant value that is obtained from the header files.  This is because,
     * in the former case, restarting ClearCase might help pick up the right value.
     * MAXCPU_IS_CONSTANT is used to indicate if the max cpu is constant.
     */
    if (mvfs_cpu_beyond_limit) {
        UPRINTF(("mvfs: ERROR: The current number of CPUs is larger than the "
                 "maximum number of CPUs(%d) MVFS could handle. Some "
                 "statistics might have been lost. \n", mvfs_max_cpus));
#ifndef MAXCPU_IS_CONSTANT
        UPRINTF(("Stopping and restarting ClearCase might fix this issue.\n"));
#endif
        UPRINTF(("Continuing with the available statistics....\n \n"));
    }

    if ((output_sdp = KMEM_ALLOC(sizeof(mvfs_stats_data_t), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    MVFS_STAT_ZERO(output_sdp);

    if ((mvfs_statbufsp = KMEM_ALLOC(sizeof(*mvfs_statbufsp), KM_SLEEP)) == NULL) {
        KMEM_FREE(output_sdp, sizeof(mvfs_stats_data_t));
        return(ENOMEM);
    }
    for (cpuid = 0; cpuid < mvfs_max_cpus; cpuid++) {
        percpu_sdp = MDKI_STATS_GET_DATAP(cpuid);
        if ((percpu_sdp != NULL) && !(percpu_sdp->zero_me))  {
           mvfs_addup_clntstat(percpu_sdp, output_sdp);
           mvfs_addup_mnstat(percpu_sdp, output_sdp);
           mvfs_addup_clearstat(percpu_sdp, output_sdp);
           mvfs_addup_rvcstat(percpu_sdp, output_sdp);
           mvfs_addup_dncstat(percpu_sdp, output_sdp);
           mvfs_addup_acstat(percpu_sdp, output_sdp);
           mvfs_addup_rlstat(percpu_sdp, output_sdp);
           mvfs_addup_austat(percpu_sdp, output_sdp);
           mvfs_addup_vnopcnt(percpu_sdp, output_sdp);
           mvfs_addup_vfsopcnt(percpu_sdp, output_sdp);
           mvfs_addup_viewopcnt(percpu_sdp, output_sdp);
           mvfs_addup_viewophist(percpu_sdp, output_sdp);
           mvfs_addup_viewoptime(percpu_sdp, output_sdp);
        }
    }

    if ((error = CopyInMvfs_statbufs(data->infop,
                                     mvfs_statbufsp, callinfo)) == 0)
    {

        if (mvfs_statbufsp->clntstat.s &&
            mvfs_statbufsp->clntstat.m)
        {
            error = CopyOutMfs_clntstat(&(output_sdp->mfs_clntstat),
                        (caddr_t)mvfs_statbufsp->clntstat.s,
                         mvfs_statbufsp->clntstat.m, callinfo);
        }
        if (error == 0 && mvfs_statbufsp->mnstat.s &&
            mvfs_statbufsp->mnstat.m)
        {
            error = COPYOUT((caddr_t)&(output_sdp->mfs_mnstat),
                            (caddr_t)mvfs_statbufsp->mnstat.s,
                            KS_MIN(mvfs_statbufsp->mnstat.m,
                                   sizeof(struct mfs_mnstat)));
        }
        if (error == 0 && mvfs_statbufsp->clearstat.s &&
            mvfs_statbufsp->clearstat.m)
        {
            error = CopyOutMfs_clearstat(&(output_sdp->mfs_clearstat),
                        (caddr_t)mvfs_statbufsp->clearstat.s,
                        mvfs_statbufsp->clearstat.m, callinfo);
        }
        if (error == 0 && mvfs_statbufsp->rvcstat.s &&
            mvfs_statbufsp->rvcstat.m)
       {
            error = COPYOUT((caddr_t)&(output_sdp->mfs_rvcstat),
                            (caddr_t)mvfs_statbufsp->rvcstat.s,
                            KS_MIN(mvfs_statbufsp->rvcstat.m,
                                   sizeof(struct mfs_rvcstat)));
        }
        if (error == 0  && mvfs_statbufsp->dncstat.s &&
            mvfs_statbufsp->dncstat.m)
        {
            error = COPYOUT((caddr_t)&(output_sdp->mfs_dncstat),
                            (caddr_t)mvfs_statbufsp->dncstat.s,
                            KS_MIN(mvfs_statbufsp->dncstat.m,
                                   sizeof(struct mfs_dncstat)));
        }
        if (error == 0 && mvfs_statbufsp->acstat.s &&
            mvfs_statbufsp->acstat.m)
        {
            error = COPYOUT((caddr_t)&(output_sdp->mfs_acstat),
                            (caddr_t)mvfs_statbufsp->acstat.s,
                            KS_MIN(mvfs_statbufsp->acstat.m,
                                   sizeof(struct mfs_acstat)));
        }
        if (error == 0 && mvfs_statbufsp->rlstat.s &&
            mvfs_statbufsp->rlstat.m)
        {
            error = COPYOUT((caddr_t)&(output_sdp->mfs_rlstat),
                            (caddr_t)mvfs_statbufsp->rlstat.s,
                            KS_MIN(mvfs_statbufsp->rlstat.m,
                                   sizeof(struct mfs_rlstat)));
        }
        if (error == 0 && mvfs_statbufsp->austat.s &&
            mvfs_statbufsp->austat.m)
        {
            error = CopyOutMfs_austat(&(output_sdp->mfs_austat),
                        (caddr_t)mvfs_statbufsp->austat.s,
                        mvfs_statbufsp->austat.m, callinfo);
        }

        /* Copy out the opcnt vectors */

        if (error == 0 && mvfs_statbufsp->vnopcnt.s &&
            mvfs_statbufsp->vnopcnt.m)
        {
            error = COPYOUT((caddr_t)output_sdp->mfs_vnopcnt,
                             (caddr_t)mvfs_statbufsp->vnopcnt.s,
                              KS_MIN(mvfs_statbufsp->vnopcnt.m,
                              sizeof(MVFS_STAT_CNT_T)*mfs_vnopmax));
        }
        if (error == 0 && mvfs_statbufsp->vfsopcnt.s &&
            mvfs_statbufsp->vfsopcnt.m)
        {
            error = COPYOUT((caddr_t)output_sdp->mfs_vfsopcnt,
                            (caddr_t)mvfs_statbufsp->vfsopcnt.s,
                             KS_MIN(mvfs_statbufsp->vfsopcnt.m,
                              sizeof(MVFS_STAT_CNT_T)*mfs_vfsopmax));
        }
        if (error == 0 && mvfs_statbufsp->viewopcnt.s &&
            mvfs_statbufsp->viewopcnt.m)
        {
            error = COPYOUT((caddr_t)output_sdp->mfs_viewopcnt,
                            (caddr_t)mvfs_statbufsp->viewopcnt.s,
                            KS_MIN(mvfs_statbufsp->viewopcnt.m,
                             sizeof(MVFS_STAT_CNT_T)*mfs_viewopmax));
        }
        if (error == 0 && mvfs_statbufsp->viewoptime.s &&
                 mvfs_statbufsp->viewoptime.m)
        {
            error = CopyOuttimestruc_array(output_sdp->mfs_viewoptime,
                        (caddr_t)mvfs_statbufsp->viewoptime.s,
                        mfs_viewopmax,
                        KS_MIN(mvfs_statbufsp->viewoptime.m,
                            sizeof(timestruc_t)*mfs_viewopmax), callinfo);
        }
        if (error == 0 && mvfs_statbufsp->viewophist.s &&
            mvfs_statbufsp->viewophist.m)
        {
            error = CopyOutMfs_rpchist(&(output_sdp->mfs_viewophist),
                        (caddr_t)mvfs_statbufsp->viewophist.s,
                         mvfs_statbufsp->viewophist.m, callinfo);
        }
    }
    KMEM_FREE(output_sdp, sizeof(mvfs_stats_data_t));
    KMEM_FREE(mvfs_statbufsp, sizeof(*mvfs_statbufsp));
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_rmallviewtags(
    mvfscmd_block_t *data, 
    CRED_T *cred
)
{
    int  error;
    VNODE_T *dvp = NULL;
    struct uio uio;
    IOVEC_T iov;
    caddr_t dirbufp;
    KDIRENT_T *direntp;
    size_t direntlen = KDIRENT_RECLEN(MAXNAMELEN);
    int eofp;

    dirbufp = KMEM_ALLOC(direntlen, KM_SLEEP);
    if (dirbufp == NULL) {
        return(ENOMEM);
    }
    uio.uio_iov = &iov;
    MVFS_UIO_OFFSET(&uio) = 0;

    /* Get held vnode ptr to viewroot dir */
    dvp = mfs_getviewroot();
    if (dvp == NULL) {
        KMEM_FREE(dirbufp, direntlen);
        error = ENXIO;
    } else {
        mfs_viewdiropen(&dvp, 0, cred);
        error = 0;
        while (!error) {
            mfs_uioset (&uio, dirbufp, direntlen, 
                        MVFS_UIO_OFFSET(&uio), UIO_SYSSPACE);
            error = mvfs_viewdirreaddir(dvp, &uio, cred, &eofp);
            if ((error) || (uio.uio_resid == direntlen))
                break;
            direntp = (KDIRENT_T *) dirbufp;
            while (((caddr_t) direntp) < 
                   (dirbufp + (direntlen - uio.uio_resid))) 
            {
                if (STRCMP(KDIRENT_GET_NAME(direntp), 
                           MVFS_SPECDEV) != 0)
                {
                    mfs_viewdirrmdir(dvp, 
                             KDIRENT_GET_NAME(direntp), cred);
                }
                direntp = (KDIRENT_T *) 
                          (((caddr_t)(direntp)) + 
                             KDIRENT_GET_RECLEN(direntp));
            }
        }
        mfs_viewdirclose(dvp, 0, 0, cred);
        KMEM_FREE(dirbufp, direntlen);

        VN_RELE(dvp);
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_poolmaps(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_ioget_poolmaps_t pmap;
    register int i;
    mfs_strbufpn_t buftmp;
    struct mfs_mntinfo *mmi;
    VFS_T *vfsp;

    if ((error = CopyInMvfs_ioget_poolmaps(data->infop, &pmap, callinfo)) 
         != 0) 
    {
        return(error);
    }
    mfs_findvfs_lock();                       /* Keep mountinfo stable */
    vfsp = mfs_findvfs_uuid(&pmap.replica_uuid);
    if (vfsp == NULL) {
        mfs_findvfs_unlock();        
        data->status = TBS_ST_NOT_FOUND;
        error = 0;
    } else {

        mmi = VFS_TO_MMI(vfsp);
        if (pmap.mapcount == 0) {
            /* query for how many slots needed */
            pmap.mapcount = mmi->mmi_sptable_len;
            mfs_findvfs_unlock();         
            /* 
             * Explicitly copy the number of slots back out here
             */
            error = CopyOutMvfs_ioget_poolmaps(&pmap, 
                                               data->infop, callinfo);
        } else  {
            /* 
             * actually wants the path translations--
             * go do the copyout 
             */
            for (i = 0; i < KS_MIN(pmap.mapcount, 
                                   mmi->mmi_sptable_len); i++) 
            {
                 /* 
                  * only copy out the pattern and 
                  * the user-version of the
                  * pathnames 
                  */
                  error = CopyInMfs_strbufpn_index((caddr_t)
                                             pmap.patterns, 
                                             &buftmp, i, callinfo);
                  /* 
                   * all these break's get out of the "for" 
                   * loop only 
                   */
                  if (error)
                    break;
                  error = mfs_copyout_strbufpn(buftmp,
                                  mmi->mmi_sptable[i].sp_prefix);
                  if (error)
                      break;
                  error = CopyOutMfs_strbufpn_index(&buftmp,
                                   (caddr_t)pmap.patterns, i, callinfo);
                  if (error)
                      break;
                  error = CopyInMfs_strbufpn_index((caddr_t)
                                pmap.replacements, &buftmp, i, callinfo);
        
                  if (error)
                    break;
                  error = mfs_copyout_strbufpn(buftmp,
                             mmi->mmi_sptable[i].sp_usertarget);
                  if (error)
                      break;
                  error = CopyOutMfs_strbufpn_index(&buftmp,
                                (caddr_t)pmap.replacements, i, callinfo);
                  if (error)
                      break;
            }
            mfs_findvfs_unlock();          
        }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_zero_stats(CRED_T *cred)
{
    int  error = 0;
    int  cpuid;
    mvfs_stats_data_t *sdp;

    if (MDKI_SUSER(cred)) {
        for (cpuid = 0; cpuid < mvfs_max_cpus; cpuid++) {
             if ((sdp = MDKI_STATS_GET_DATAP(cpuid)) != NULL) {
                sdp->zero_me = TRUE;
             }
        }
    } else {
        error = EPERM;
        MDKI_SET_U_ERROR(0);/* Eliminate suser side effect. */
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_cache_usage(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_cache_usage_t mvfs_cache_usage ;

    mvfs_mn_count(&mvfs_cache_usage);
    mvfs_dnc_count(&mvfs_cache_usage);
    mvfs_rpc_count(&mvfs_cache_usage);
    error = CopyOutMvfs_cache_usage(&mvfs_cache_usage, 
                                     data->infop, callinfo);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_set_cache_sizes(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_cache_sizes_t mvfs_cache_size;

    if ((error = CopyInMvfs_cache_sizes(data->infop, 
                                        &mvfs_cache_size, callinfo)) != 0)
    {
        return(error);
    }

    if (MDKI_INGLOBALZONE() && MDKI_SUSER(cred)) {
        if (mvfs_cache_size.version >= MVFS_SETCACHE_VERSION) {
            mvfs_cache_size.version = MVFS_SETCACHE_VERSION; /* return it */
            error = mvfs_mn_setcaches(&mvfs_cache_size);
            if (!error)
                error = mvfs_dnc_setcaches(&mvfs_cache_size);
            if (!error)
                error = mvfs_rpc_setcaches(&mvfs_cache_size);
            if (!error)
                error = mvfs_rddir_cache_setcaches(&mvfs_cache_size);
        } else {
            error = EINVAL;
        }
    } else {
        error = EPERM;
        MDKI_SET_U_ERROR(0);   /* Eliminate suser side effect. */
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_cache_sizes(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();
    int  error = 0;
    mvfs_cache_sizes_t mvfs_cache_size;

    if ((error = CopyInMvfs_cache_sizes(data->infop, 
                                        &mvfs_cache_size, callinfo)) != 0)
    {
        return(error);
    }

    if (mvfs_cache_size.version >= MVFS_SETCACHE_VERSION) {
        mvfs_cache_size.version = MVFS_SETCACHE_VERSION; /* return it */
        mvfs_cache_size.size[MVFS_SETCACHE_LARGEINIT] = mcdp->mvfs_largeinit;
        mvfs_cache_size.mask = MVFS_SETCACHE_ALLBITS;
        error = mvfs_mn_getcaches(&mvfs_cache_size);
        if (!error)
            error = mvfs_dnc_getcaches(&mvfs_cache_size);
        if (!error)
            error = mvfs_rpc_getcaches(&mvfs_cache_size);
        if (!error)
            error = mvfs_proc_getcaches(&mvfs_cache_size);
        if (!error)
            error = mvfs_rddir_cache_getcaches(&mvfs_cache_size);
    } else {
        error = EINVAL;
    }
    if (error == 0)
        error = CopyOutMvfs_cache_sizes(&mvfs_cache_size, data->infop,
                                        callinfo);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_compute_cache_defaults(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    ks_int32_t scale_factor;
    mvfs_cache_sizes_t mvfs_cache_size;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if ((error = CopyInMvfs_cache_sizes(data->infop, 
                                        &mvfs_cache_size, callinfo)) != 0)
    {
        return(error);
    }

    if (mvfs_cache_size.version >= MVFS_SETCACHE_VERSION) {
        mvfs_cache_size.version = MVFS_SETCACHE_VERSION; /* return it */
        /* capture requested scale factor */
        scale_factor = mvfs_cache_size.size[MVFS_SETCACHE_LARGEINIT];
        if (scale_factor == MVFS_SETCACHE_DEFAULT_VALUE) {
            mvfs_cache_size.size[MVFS_SETCACHE_LARGEINIT] =
                mcdp->mvfs_computed_largeinit;
            mvfs_cache_size.mask |= MVFS_CACHEBIT(LARGEINIT);
            scale_factor = mcdp->mvfs_computed_largeinit;
        }
        error = mvfs_mn_compute_caches(scale_factor, &mvfs_cache_size);
        if (!error)
            error = mvfs_dnc_compute_caches(scale_factor, &mvfs_cache_size);
        if (!error)
            error = mvfs_rpc_compute_caches(scale_factor, &mvfs_cache_size);
        if (!error)
            error = mvfs_proc_compute_caches(scale_factor, &mvfs_cache_size);
        if (!error)
            error = mvfs_rddir_compute_caches(scale_factor, &mvfs_cache_size);
    } else {
        error = EINVAL;
    }
    if (error == 0)
        error = CopyOutMvfs_cache_sizes(&mvfs_cache_size, data->infop,
                                        callinfo);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_view_stats(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_viewstats_t *mvwsp;
    char *tagn = NULL;
    mvfs_statbufs_t *sp;
    VNODE_T *vw = NULL;

    if ((mvwsp = KMEM_ALLOC(sizeof(*mvwsp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    sp = &(mvwsp->stats);

    if ((error = CopyInMvfs_viewstats(data->infop, mvwsp, callinfo)) != 0) {
        goto cleanup;
    }
    if ((error = mfs_copyin_strbuf(mvwsp->viewtag, &tagn)) == 0) {
        /* See if existing viewtag */
        error = mfs_viewtaglookup(tagn, &vw, cred); 
        PN_STRFREE(tagn);
        if (error == 0) {
            if (sp->clntstat.s && sp->clntstat.m) {
                error = CopyOutMfs_clntstat( 
        		&(VTOM(vw)->mn_view.pvstat->clntstat),
        		(caddr_t)sp->clntstat.s, sp->clntstat.m,
        		callinfo);
            }
            if (error == 0) {
                if (sp->acstat.s && sp->acstat.m) {
                    error = COPYOUT((caddr_t)
                            &(VTOM(vw)->mn_view.pvstat->acstat),
                            (caddr_t)sp->acstat.s,
                            KS_MIN(sp->acstat.m, 
                                    sizeof(struct mfs_acstat)));
                }
                if (error == 0 && sp->dncstat.s && 
                    sp->dncstat.m) 
                {
                    error = COPYOUT((caddr_t)
                            &(VTOM(vw)->mn_view.pvstat->dncstat),
                            (caddr_t)sp->dncstat.s,
                            KS_MIN(sp->dncstat.m,
                                   sizeof(struct mfs_dncstat)));
                }
            }    
            VN_RELE(vw);
        }    
    }
  cleanup:
    KMEM_FREE(mvwsp, sizeof(*mvwsp));
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_zero_view_stats(
    mvfscmd_block_t *data,
    CRED_T *cred,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_zero_viewstat_t mvfs_zero_viewstat;
    char *tagn = NULL;
    VNODE_T *vw = NULL;
            
    if ((error = CopyInMvfs_zero_viewstat(data->infop, 
         &mvfs_zero_viewstat, callinfo)) != 0) 
    {
        return(error);
    }
    if ((error = mfs_copyin_strbuf(mvfs_zero_viewstat.viewtag,
                                   &tagn)) == 0)
    {
            /* See if existing viewtag */
            error = mfs_viewtaglookup(tagn, &vw, cred); 
            PN_STRFREE(tagn);       
            if (error == 0 ) {
                if (MDKI_SUSER(cred)) {
                    MVFS_PVSTAT_ZERO(vw);
                } else {
                    error = EPERM;
                    MDKI_SET_U_ERROR(0); /* Eliminate suser side effect. */ 
                }
               VN_RELE(vw);
            }
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_sidhost_credmapping(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_sidhost_cred_t sidhost_cred;

    if ((error = CopyInMvfs_sidhost_cred(data->infop, 
                                         &sidhost_cred, callinfo)) == 0)
    {
        error = MVFS_REGISTER_SIDHOST_CREDMAPS(&sidhost_cred);
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_delete_sidhost_credmapping(
    mvfscmd_block_t *data, 
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mvfs_sid_t mvfs_sid;

    if ((error = CopyInMvfs_sid(data->infop, &mvfs_sid, callinfo)) == 0)
        error = MVFS_UNREGISTER_SIDHOST_CREDMAPS(&mvfs_sid);
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_set_vobrt_vfsmnt(
    mvfscmd_block_t *data,
    MVFS_CALLER_INFO *callinfo
)
{
    int  error = 0;
    mfs_strbufpn_pair_t vob_path;

    if ((error = CopyInMfs_strbufpn_pair(data->infop, &vob_path, callinfo)) == 0) {
        /* We are going to use the pathname strings, so copy them in. */
        MFS_STRBUFPN_PAIR_COPYIN_STRS(&vob_path, error);
        /* Do the actual work of this ioctl if everything is OK. */
        if (error == 0) {
            error = MVFS_SET_VOBRT_VFSMNT(&vob_path); 
        }
        /* Cleanup after success or failure (this handles NULL pointers). */
        PNPAIR_STRFREE(&vob_path);
    }
    return(error);
}

STATIC int MVFS_NOINLINE
mvfs_get_gfsinfo(
     mvfscmd_block_t *data,
     MVFS_CALLER_INFO *callinfo
)
{
     int error = 0;
     mvfs_gfsinfo_t gfsinfo;

     if ((error = CopyInMvfs_gfsinfo(data->infop, &gfsinfo, callinfo)) == 0) {
         /* Do the actual work of this ioctl if everything is OK. */
         if (error == 0) {
             error = MVFS_GET_GFSINFO(&gfsinfo);
         }
         if (error == 0) {
             error = CopyOutMvfs_gfsinfo(&gfsinfo, data->infop, callinfo);
         }
     }
     return(error);
}

STATIC void
mvfs_addup_clntstat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{

#define ADDUP_FIELD(field) sdp->mfs_clntstat.field +=  \
                                percpu_sdp->mfs_clntstat.field

    ADDUP_FIELD(clntget);
    ADDUP_FIELD(clntfree);
    ADDUP_FIELD(clntcreate);
    ADDUP_FIELD(clntdestroy);
    ADDUP_FIELD(clntcalls);
    ADDUP_FIELD(clntretries);
    ADDUP_FIELD(mfscall);
    ADDUP_FIELD(mfsfail);
    ADDUP_FIELD(mfsintr);
    ADDUP_FIELD(mfsmaxdelay);
    ADDUP_FIELD(mfsmaxdelaytime);

    mvfs_add_times(&(sdp->mfs_clntstat.mvfsthread_time),
                   &(percpu_sdp->mfs_clntstat.mvfsthread_time));
    return;

#undef ADDUP_FIELD
}

STATIC void
mvfs_addup_mnstat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
#define ADDUP_FIELD(field) sdp->mfs_mnstat.field +=  \
                                percpu_sdp->mfs_mnstat.field

    ADDUP_FIELD(mnget);
    ADDUP_FIELD(mnfound);
    ADDUP_FIELD(mnfoundstale);
    ADDUP_FIELD(mnreusefree);
    ADDUP_FIELD(mncreate);
    ADDUP_FIELD(mngetnum);
    ADDUP_FIELD(mnfree);
    ADDUP_FIELD(mndestroy);
    ADDUP_FIELD(mnreclaim);
    ADDUP_FIELD(mnvobhashcnt);
    ADDUP_FIELD(mncvphashcnt);
    ADDUP_FIELD(mnotherhashcnt);
    ADDUP_FIELD(mnflushvfscnt);
    ADDUP_FIELD(mnflushvwcnt);

    return;

#undef ADDUP_FIELD
}

STATIC void
mvfs_addup_clearstat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
#define ADDUP_FIELD(field) sdp->mfs_clearstat.field  += \
                                percpu_sdp->mfs_clearstat.field

#define ADDUP_TIME(t_val) (mvfs_add_times(&(sdp->mfs_clearstat.t_val), \
                                          &(percpu_sdp->mfs_clearstat.t_val)))
    ADDUP_FIELD(clearget);
    ADDUP_FIELD(clearcreate);
    ADDUP_FIELD(clearraces);
    ADDUP_FIELD(clearcreatraces);
    ADDUP_FIELD(clearread);
    ADDUP_FIELD(clearwrite);

    ADDUP_TIME(clearget_time);
    ADDUP_TIME(clearcreat_time);
    ADDUP_TIME(clearrd_time);
    ADDUP_TIME(clearwr_time);
    ADDUP_TIME(clearopen_time);
    ADDUP_TIME(unclearrd_time);
    ADDUP_TIME(unclearwr_time);
    ADDUP_TIME(unclearget_time);
    ADDUP_TIME(cto_getattr_time);

    ADDUP_FIELD(clearopen);
    ADDUP_FIELD(unclearopen);
    ADDUP_FIELD(cleargetmiss);
    ADDUP_FIELD(clearreclaim);
    ADDUP_FIELD(clearreclaimmiss);
    ADDUP_FIELD(cleargetlkup);

    return;

#undef ADDUP_FIELD
#undef ADDUP_TIME
}

STATIC void
mvfs_addup_rvcstat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
#define ADDUP_FIELD(field) sdp->mfs_rvcstat.field  += \
                                percpu_sdp->mfs_rvcstat.field

    ADDUP_FIELD(rvc_hits);
    ADDUP_FIELD(rvc_misses);
    ADDUP_FIELD(rvc_misstimo);
    ADDUP_FIELD(rvc_purge);

    return;

#undef ADDUP_FIELD
}

STATIC void
mvfs_addup_dncstat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
#define ADDUP_FIELD(field) sdp->mfs_dncstat.field  += \
                                percpu_sdp->mfs_dncstat.field

    ADDUP_FIELD(dnc_hits);
    ADDUP_FIELD(dnc_hitdot);
    ADDUP_FIELD(dnc_hitdir);
    ADDUP_FIELD(dnc_hitreg);
    ADDUP_FIELD(dnc_hitnoent);
    ADDUP_FIELD(dnc_hitbhfromnull);
    ADDUP_FIELD(dnc_misses);
    ADDUP_FIELD(dnc_missinvalid);
    ADDUP_FIELD(dnc_missdncgen);
    ADDUP_FIELD(dnc_missevtime);
    ADDUP_FIELD(dnc_missnoenttimedout);
    ADDUP_FIELD(dnc_missbh);
    ADDUP_FIELD(dnc_missnovp);
    ADDUP_FIELD(dnc_add);
    ADDUP_FIELD(dnc_adddir);
    ADDUP_FIELD(dnc_addreg);
    ADDUP_FIELD(dnc_addnoent);
    ADDUP_FIELD(dnc_addbhinvariant);
    ADDUP_FIELD(dnc_addnoop);
    ADDUP_FIELD(dnc_addbh);
    ADDUP_FIELD(dnc_addlong);
    ADDUP_FIELD(dnc_addunlock);
    ADDUP_FIELD(dnc_change);
    ADDUP_FIELD(dnc_remove);
    ADDUP_FIELD(dnc_invalvp);
    ADDUP_FIELD(dnc_flush);
    ADDUP_FIELD(dnc_invalhits);
    ADDUP_FIELD(dnc_flushvw);
    ADDUP_FIELD(dnc_flushvfs);
    ADDUP_FIELD(dnc_invalvw);
    ADDUP_FIELD(dnc_invalnf);
    ADDUP_FIELD(dnc_missdir);
    ADDUP_FIELD(dnc_missreg);
    ADDUP_FIELD(dnc_missnoent);

    return;

#undef ADDUP_FIELD
}

STATIC void
mvfs_addup_acstat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
#define ADDUP_FIELD(field) sdp->mfs_acstat.field  += \
                                percpu_sdp->mfs_acstat.field

    ADDUP_FIELD(ac_hits);
    ADDUP_FIELD(ac_misses);
    ADDUP_FIELD(ac_updates);
    ADDUP_FIELD(ac_mod);
    ADDUP_FIELD(ac_expmod);
    ADDUP_FIELD(ac_vobmod);
    ADDUP_FIELD(ac_evmiss);
    ADDUP_FIELD(ac_cto);
    ADDUP_FIELD(ac_timo);
    ADDUP_FIELD(ac_genmiss);
    ADDUP_FIELD(ac_newmiss);
    ADDUP_FIELD(ac_lvuthit);
    ADDUP_FIELD(ac_lvutmiss);
    ADDUP_FIELD(ac_rddirhit);
    ADDUP_FIELD(ac_rddirmiss);

    return;

#undef ADDUP_FIELD
}

STATIC void
mvfs_addup_rlstat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
#define ADDUP_FIELD(field) sdp->mfs_rlstat.field  += \
                                percpu_sdp->mfs_rlstat.field
    ADDUP_FIELD(rl_hits);
    ADDUP_FIELD(rl_misses);

    return;

#undef ADDUP_FIELD
}

STATIC void
mvfs_addup_austat(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
#define ADDUP_FIELD(field) sdp->mfs_austat.field  += \
                                percpu_sdp->mfs_austat.field

#define ADDUP_TIME(t_val) (mvfs_add_times(&(sdp->mfs_austat.t_val), \
                                          &(percpu_sdp->mfs_austat.t_val)))

    ADDUP_FIELD(au_calls);
    ADDUP_FIELD(au_vgetattr);
    ADDUP_FIELD(au_nvgetattr);
    ADDUP_FIELD(au_dupl);

    ADDUP_TIME(au_time);
    ADDUP_TIME(au_settime);
    ADDUP_TIME(au_ioctltime);

    return;

#undef ADDUP_FIELD
#undef ADDUP_TIME
}

STATIC void
mvfs_addup_vnopcnt(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
    int i;
    for (i = 0; i < MFS_VNOPCNT; i++) {
         sdp->mfs_vnopcnt[i] += percpu_sdp->mfs_vnopcnt[i];
    }

    return;
}

STATIC void
 mvfs_addup_vfsopcnt(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
    int i;
    for (i = 0; i < MFS_VFSOPCNT; i++) {
         sdp->mfs_vfsopcnt[i] += percpu_sdp->mfs_vfsopcnt[i];
    }

    return;
}

STATIC void
mvfs_addup_viewopcnt(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
    int i;
    for (i = 0; i < VIEW_NUM_PROCS; i++) {
         sdp->mfs_viewopcnt[i] += percpu_sdp->mfs_viewopcnt[i];
    }

    return;
}

STATIC void
mvfs_addup_viewophist(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
    int i,j;
    for (i = 0; i < MFS_NUM_HISTX; i++) {
         mvfs_add_times(&(sdp->mfs_viewophist.histval[i]),
                        &(percpu_sdp->mfs_viewophist.histval[i]));
    }
    for (i = 0; i < MFS_NUM_HISTX; i++) {
         sdp->mfs_viewophist.histrpc[i] +=
                percpu_sdp->mfs_viewophist.histrpc[i];
         sdp->mfs_viewophist.histclr[i] +=
                percpu_sdp->mfs_viewophist.histclr[i];
    }
    for (i = 0; i < VIEW_NUM_PROCS; i++) {
         for (j = 0; j < MFS_NUM_HISTX; j++) {
              sdp->mfs_viewophist.histperop[i][j] +=
                 percpu_sdp->mfs_viewophist.histperop[i][j];
         }
    }

    return;
}

STATIC void
mvfs_addup_viewoptime(
    mvfs_stats_data_t *percpu_sdp,
    mvfs_stats_data_t *sdp
)
{
        int i;
        for (i = 0; i < VIEW_NUM_PROCS; i++) {
             mvfs_add_times(&(sdp->mfs_viewoptime[i]),
                            &(percpu_sdp->mfs_viewoptime[i]));
        }

        return;
}

STATIC void
mvfs_add_times(
    timestruc_t *sdp_time,
    timestruc_t *percpu_time
)
{
        timestruc_t tmp;

        tmp.tv_nsec = sdp_time->tv_nsec + percpu_time->tv_nsec;
        tmp.tv_sec = sdp_time->tv_sec + percpu_time->tv_sec;

        if (tmp.tv_nsec  >= 1000000000) {
            tmp.tv_nsec -= 1000000000;
            tmp.tv_sec++;
        }
        sdp_time->tv_nsec = tmp.tv_nsec;
        sdp_time->tv_sec = tmp.tv_sec;

        return;
}

/*
 * Routine to zero out per-view statistics.  We don't zero out the whole
 * per-view stat structure because it has our lock in it.
 */
void
mvfs_pview_stat_zero(struct mvfs_pvstat *pvp)
{
        BZERO(&(pvp->clntstat), sizeof(struct mfs_clntstat));
        BZERO(&(pvp->acstat), sizeof(struct mfs_acstat));
        BZERO(&(pvp->dncstat), sizeof(struct mfs_dncstat));
        pvp->clntstat.version = MFS_CLNTSTAT_VERS;
        pvp->acstat.version = MFS_ACSTAT_VERS;
        pvp->dncstat.version = MFS_DNCSTAT_VERS;
}
static const char vnode_verid_mvfs_mioctl_c[] = "$Id:  66dbe673.dc5411df.9210.00:01:83:0a:3b:75 $";
