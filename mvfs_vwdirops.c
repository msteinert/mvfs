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
/* mvfs_vwdirops.c */
#include "mvfs_systm.h"
#include "mvfs.h"

/* 
 * Static variable for viewroot subsystem. */
mvfs_viewroot_data_t mvfs_viewroot_data_var;

STATIC int
mvfs_ramdir_insert(
    VNODE_T *dvp,
    char *nm,
    VNODE_T *vp
);

/* MFS_RAMDIR_ADD - add a ramdir link to a vnode.
   Assumes the vnode pointer is already held. */

int
mfs_ramdir_add(dvp, nm, vp, num)
VNODE_T *dvp;
char *nm;
VNODE_T *vp;
int *num;		/* returned slot used */
{
    register mfs_mnode_t *mnp;
    register int vnum;
    int freevnum;
    int error;

    ASSERT(MFS_ISVIEWDIR(VTOM(dvp)));

    mnp =  VTOM(dvp);
    MLOCK(mnp);

    *num = 0;

    /* Must scan for duplicates and first hole to put
       element into */

    freevnum = -1;
    for (vnum=0; vnum < mnp->mn_ramdir.hwm; vnum++) {
	if (mnp->mn_ramdir.ents[vnum].nm == NULL) {
	    if (freevnum == -1) freevnum = vnum;
	    continue;
	}
	if (STRCMP(mnp->mn_ramdir.ents[vnum].nm, nm) == 0) {
	    MUNLOCK(mnp);
	    return(EEXIST);
	}
    }

    /* If no free slot found, try pushing the highwater mark */

    if (freevnum == -1 && (mnp->mn_ramdir.hwm < mnp->mn_ramdir.max))
	freevnum = mnp->mn_ramdir.hwm++;

    /* If have a slot, fill it in.  Otherwise return an error */

    if (freevnum != -1) {
	mnp->mn_ramdir.ents[freevnum].nm = STRDUP(nm);
	if (mnp->mn_ramdir.ents[freevnum].nm == NULL) error = ENOMEM;
	else {
	    mnp->mn_ramdir.ents[freevnum].vp = vp;
    	    mnp->mn_ramdir.mtime.tv_sec = MDKI_CTIME();	
	    if (MVFS_ISVTYPE(vp, VDIR))
	        mnp->mn_ramdir.lnk_cnt++;
	    *num = freevnum;	/* Return slot number used */
            error = 0;
            MVFS_WRAP_UPDATE_ATTRS(dvp);
	}
    } else {
	error = ENOSPC;
    }

    MUNLOCK(mnp);
    return(error);
}

/* MFS_RAMDIR_REMOVE - remove a ramdir link to a vnode.
   Returns the vnode "removed" without affecting the hold
   count (the hold count the ramdir used to have is transferred to the
   caller). */

int
mfs_ramdir_remove(dvp, nm, vpp)
VNODE_T *dvp;
char *nm;
VNODE_T **vpp;
{
    register mfs_mnode_t *mnp;
    register int vnum;
    int error;

    ASSERT(MFS_ISVIEWDIR(VTOM(dvp)));

    mnp =  VTOM(dvp);
    MLOCK(mnp);

    error = ENOENT;
    for (vnum=0; vnum < mnp->mn_ramdir.hwm; vnum++) {
	if (mnp->mn_ramdir.ents[vnum].nm == NULL) continue;
	if (STRCMP(mnp->mn_ramdir.ents[vnum].nm, nm) == 0) {
	    if (MVFS_ISVTYPE(mnp->mn_ramdir.ents[vnum].vp, VDIR))
	        mnp->mn_ramdir.lnk_cnt--;
	    *vpp = mnp->mn_ramdir.ents[vnum].vp;  /* return vnode */
 	    mnp->mn_ramdir.ents[vnum].vp = NULL;
	    STRFREE(mnp->mn_ramdir.ents[vnum].nm);

    	    mnp->mn_ramdir.mtime.tv_sec = MDKI_CTIME();	

	    /* Update high water mark if required */
	
	    if (vnum == mnp->mn_ramdir.hwm-1) {
		while (mnp->mn_ramdir.ents[--vnum].nm == NULL) {}
		mnp->mn_ramdir.hwm = vnum+1;
	    }

	    error = 0;
            MVFS_WRAP_UPDATE_ATTRS(dvp);
	    break;
	}
    }
    MUNLOCK(mnp);
    if (error)
        *vpp = NULL;
    else
        MVFS_RM_DCACHE(*vpp);
    return(error);
}

/* 
 * MFS_RAMDIR_PURGEVP - remove all ramdir links to a vnode.
 * 	Assumes vnode ptrs are held and does a release on them
 */

void
mfs_ramdir_purgevp(dvp, vp)
VNODE_T *dvp;
VNODE_T *vp;
{
    register mfs_mnode_t *mnp;
    register int vnum;
    int hwm;

    ASSERT(MFS_ISVIEWDIR(VTOM(dvp)));

    mnp =  VTOM(dvp);
    MLOCK(mnp);

    /* Must continue scan to remove multiple links to same object. */

    for (vnum=0; vnum < mnp->mn_ramdir.hwm; vnum++) {
	if (mnp->mn_ramdir.ents[vnum].vp == vp) {
	    if (MVFS_ISVTYPE(vp, VDIR))
	        mnp->mn_ramdir.lnk_cnt--;
	    VN_RELE(vp);
 	    mnp->mn_ramdir.ents[vnum].vp = NULL;
	    ASSERT(mnp->mn_ramdir.ents[vnum].nm);
	    STRFREE(mnp->mn_ramdir.ents[vnum].nm);

    	    mnp->mn_ramdir.mtime.tv_sec = MDKI_CTIME();	

            /* Update high water mark if releasing the highest slot */

	    if (vnum == mnp->mn_ramdir.hwm-1) {
	        hwm = vnum;
		while (mnp->mn_ramdir.ents[--hwm].nm == NULL) {}
		mnp->mn_ramdir.hwm = hwm+1;
	    }
	}
    }
    MUNLOCK(mnp);
}

/* MFS_RAMDIR_PURGE - purge all entries in a ramdir. */

void
mfs_ramdir_purge(dvp, release)
VNODE_T *dvp;
int release;
{
    register mfs_mnode_t *mnp;
    register int vnum;

    mnp =  VTOM(dvp);
    ASSERT(MFS_ISVIEWDIR(mnp));

    /* No need to lock.  This is only called from cleanup code. */

    if (mnp->mn_ramdir.ents == NULL) return; 

    for (vnum=0; vnum < mnp->mn_ramdir.max; vnum++) {
	if (mnp->mn_ramdir.ents[vnum].vp != NULL) {
	    ASSERT(mnp->mn_ramdir.ents[vnum].nm);
	    STRFREE(mnp->mn_ramdir.ents[vnum].nm);
            MVFS_DECREMENT_LINK(mnp->mn_ramdir.ents[vnum].vp);
            if (MVFS_ISVTYPE(mnp->mn_ramdir.ents[vnum].vp, VDIR))
                mnp->mn_ramdir.lnk_cnt--;
	    if (release) VN_RELE(mnp->mn_ramdir.ents[vnum].vp);
	    mnp->mn_ramdir.ents[vnum].vp = NULL;
	}
    }
}

/* 
 * MFS_RAMDIR_REFCHK - check refcnt on all entries in a ramdir. 
 * Used by unmount code.
 */

int
mfs_ramdir_refchk(dvp, countp)
VNODE_T *dvp;
int *countp;
{
    register mfs_mnode_t *mnp;
    register int vnum;
    *countp = 0;

    mnp =  VTOM(dvp);
    ASSERT(MFS_ISVIEWDIR(mnp));
    MLOCK(mnp);

    if (mnp->mn_ramdir.ents == NULL) return(0); 

    for (vnum=0; vnum < mnp->mn_ramdir.max; vnum++) {
	if (mnp->mn_ramdir.ents[vnum].vp != NULL) {
	    ASSERT(mnp->mn_ramdir.ents[vnum].nm);
	    /* Skip "." and ".." */
	    if (STRCMP(mnp->mn_ramdir.ents[vnum].nm, ".") == 0 ||
		STRCMP(mnp->mn_ramdir.ents[vnum].nm, "..") == 0) continue;
	    (*countp)++;
	    if (V_COUNT(mnp->mn_ramdir.ents[vnum].vp) > 1) {
		mvfs_log(MFS_LOG_INFO, 
		    "umount: refchk on view %s failed: refcnt %d\n", 
			mnp->mn_ramdir.ents[vnum].nm, 
			V_COUNT(mnp->mn_ramdir.ents[vnum].vp));
		MUNLOCK(mnp);
		return(EBUSY);
	    }
	}
    }
    MUNLOCK(mnp);
    return(0);
}

STATIC int
mvfs_ramdir_insert(
    VNODE_T *dvp,
    char *nm,
    VNODE_T *vp
)
{
    int num, error;
    mfs_mnode_t *mnp;
    VNODE_T *xvp;

    /* Get an extra hold count since ramdir_add doesn't 
       In addition, once the ptr is in the ramdir, someone
       could come along and VN_RELE it.  This extra count
       makes sure we don't lose it. */
    VN_HOLD(vp);
    error = mfs_ramdir_add(dvp, nm, vp, &num);
    if (error) {
        VN_RELE(vp);	/* Get rid of extra hold on error */
    }
    /* Check for slot out of range */
    if (!error && !MFS_VIDINRANGE(num)) {	
        (void) mfs_ramdir_remove(dvp, nm, &xvp);
        VN_RELE(vp);	/* Get rid of extra hold when remove */
        error = ENOSPC;
    }
    if (!error) {
        mnp = VTOM(vp);
        MLOCK(mnp);
        mnp->mn_view.id = num;	/* View ID */
        mnp->mn_view.exid = (u_int) -1;	/* not exported (yet) */
        MUNLOCK(mnp);
        MVFS_MAKE_DCACHE(dvp, vp, nm);
    }
    return error;
}

/*********************** VNODE OPS ***************************************/

/* MFS_VIEWDIRMKDIR - create a view inside a viewdir
 * Must be followed by an fcntl() to fill it in with valid info.
 */
    
int
mfs_viewdirmkdir(dvp, nm, vap, vpp, cred, hostnm, is_windows_view)
VNODE_T *dvp;
char *nm;
VATTR_T *vap;	/* May be NULL from mfs_viewdirhmview() */
VNODE_T **vpp;
CRED_T *cred;
char *hostnm;
tbs_boolean_t is_windows_view;
{
    mfs_mnode_t *mnp;
    int error;
    mfs_class_t kind;	/* Kind of view-tag to create */

    ASSERT(MFS_ISVIEWDIR(VTOM(dvp)));
    ASSERT(dvp == MDKI_VIEWROOT_GET_DATAP()->mfs_viewroot_vp);

    /* Get kind of view-tag to create */

    kind = V_TO_MMI(dvp)->mmi_default_vwtag_kind;
    ASSERT(kind == MFS_VIEWCLAS || kind == MFS_NTVWCLAS);

    /* Create a view vnode. No info required. */

    error = mfs_makespecnode(kind, (VNODE_T *)NULL,
	NULL, dvp->v_vfsp, vpp);

    /* Fill in the ramdir if we got our new vnode. */

    if (!error) {
        mnp =  VTOM(*vpp);
        MLOCK(mnp);
        /* The following  initialization is to avoid BSOD due to hostname null
         * This can happen when a thread creating a view node and the halfway
         * filling it, like in this function just after calling 
         * mvfs_ramdir_insert, if another thread tries to use it then it would
         * cause BSOD while verifying credentials due to hostname null
         * or
         * A thread creating the vp and half way through some other thread 
         * creates it and so it fails in mvfs_ramdir_insert, then it would 
         * try to release this vp but since hostname (viewserver) details
         * not filled yet, causes BSOD
         */

        /* mark whether windows view or not */
        mnp->mn_view.windows_view = is_windows_view ? 1 : 0; 
        mnp->mn_view.svr.host = PN_STRDUP(hostnm);  /* Host name copy */
        mnp->mn_view.viewname = PN_STRDUP(nm);      /* View tag name copy */
        mnp->mn_view.usedtime = MDKI_CTIME();
        mnp->mn_view.ctime.tv_sec = MDKI_CTIME();
        MVFS_COPY_UID_TO_VIEW(mnp, cred, &error);
        if (error)  {
            MUNLOCK(mnp);
        } else {
            MVFS_COPY_GID_TO_VIEW(mnp, cred, &error);
            if (error) { 
                MUNLOCK(mnp);
            } else {
                if (MFS_ISLOOPVIEW(mnp)) {
                    CVN_HOLD(CLR_ROOTDIR);
                    /* Cover vnode is root (loopback) */
                    mnp->mn_hdr.realvp = CLR_ROOTDIR;	
                } else {
                    mnp->mn_hdr.realvp = NULL;		/* Cover vnode is NULL */
                }
                MUNLOCK(mnp);

                error = mvfs_ramdir_insert(dvp, nm, *vpp);
            }
        }
        if (error) {
            VN_RELE(*vpp);	/* Release created vnode */
            *vpp = NULL;
        }
    }
	
    return(error);
}

/* MFS_VIEWDIRRMDIR - remove a dir from the view server dir.
 */

int
mfs_viewdirrmdir(dvp, nm, cred)
VNODE_T *dvp;
char *nm;
CRED_T *cred;
{
    mfs_mnode_t *mnp;
    VNODE_T *vp;
    char *hmnm = NULL;
    int error;

    mnp =  VTOM(dvp);
    ASSERT(MFS_ISVIEWDIR(mnp));

    /* Don't remove canned names */

    if (STRCMP(nm, ".") == 0 || STRCMP(nm, "..") == 0 ||
        STRCMP(nm, MVFS_SPECDEV) == 0)
    {
	return(EPERM);
    }

    if (!mfs_hmname(nm, NULL)) {
	/* Allocate the memory before we get too far. */
	hmnm = mfs_hmappend(nm);
	if (hmnm == NULL) 
	    return(ENOMEM);
    }
    /* Now remove the name and get the vnode back */

    error = mfs_ramdir_remove(dvp, nm, &vp);
    if (error) return(error);

    (void) mvfs_viewdirunexport(dvp, vp);

    /* Before releasing vnode, flush cached mnodes for
       this view and invalidate fields pointing back at the 
       /view dir. */

    ASSERT(MFS_ISVIEW(VTOM(vp)));
    mfs_mnflushvw(vp);		/* Flush mnodes with this view */

    MLOCK(VTOM(vp));
    VTOM(vp)->mn_view.id = MFS_NULLVID;	
    MUNLOCK(VTOM(vp));
    VN_RELE(vp);   /* Release vnode */

    /* 
     * If the name was a non-hm name, make sure the HM form of
     * the view goes away too!
     */

    if (hmnm != NULL) {
        error = mfs_ramdir_remove(dvp, hmnm, &vp);
	STRFREE(hmnm);
        if (error) {
	    if (error == ENOENT) error = 0;
	    return(error);
	}

        /* Before releasing vnode, flush cached mnodes for
           this view and invalidate fields pointing back at the 
           /view dir. */

        ASSERT(MFS_ISVIEW(VTOM(vp)));
        mfs_mnflushvw(vp);		/* Flush mnodes with this view */
        MLOCK(VTOM(vp));
        VTOM(vp)->mn_view.id = MFS_NULLVID;	
        MUNLOCK(VTOM(vp));
        VN_RELE(vp);   /* Release vnode */
    }
	
    return(0);
}

/* MFS_VIEWDIRUNEXPORT - remove a view from being exported. */

int
mvfs_viewdirunexport(dvp, vw)
VNODE_T *dvp;
VNODE_T *vw;
{
    mfs_mnode_t *mnp;
    VNODE_T *vp;
    mfs_mnode_t *vwmnp;
    char *hmnm;
    int error = 0;
    void *arg;

    mnp =  VTOM(dvp);
    ASSERT(MFS_ISVIEWDIR(mnp));
    vwmnp = VTOM(vw);
    ASSERT(MFS_ISVIEW(vwmnp));

    MLOCK(mnp);
    MLOCK(vwmnp);
    if (vwmnp->mn_view.exid != (u_int) -1) {
        /* if nobody raced us to deactivate, then
           we deactivate it here. */
        ASSERT(mnp->mn_ramdir.export_hwm >= vwmnp->mn_view.exid);
        ASSERT(mnp->mn_ramdir.export_ents[vwmnp->mn_view.exid] ==
               vwmnp->mn_view.id);
        /* /view mnode lock protects export fields */
        mnp->mn_ramdir.export_ents[vwmnp->mn_view.exid] = -1;
        vwmnp->mn_view.exid = (u_int) -1;
    } else
	error = ENOENT;	/* not marked for export */
    MUNLOCK(vwmnp);
    if (error == 0) {
        /* do this while /view is still locked to avoid races */
        /* Try to flush the dentry and its children, so that we don't get
         * dangling references which prevent VOB unmounts.
         * (They'd get cleaned up if we unmounted /view, but that is
         * clumsier to arrange than doing this.)
         */
        MVFS_START_FLUSH_DCACHE(vw, &arg);
    }
    MUNLOCK(mnp);
    if (error == 0 && arg != NULL)
        MVFS_FINISH_FLUSH_DCACHE(arg);
    return error;
}

int
mvfs_viewdirexport(dvp, vw, exid)
VNODE_T *dvp;
VNODE_T *vw;
register int exid;
{
    mfs_mnode_t *vwmnp;
    mfs_mnode_t *mnp;
    int error = 0, size, len;
    int *newids;
    register int i;

    /* Get held vnode ptr to viewroot dir */
    mnp = VTOM(dvp);
    vwmnp = VTOM(vw);
    MLOCK(mnp);		/* /view mnode lock protects this stuff */
    MLOCK(vwmnp);		/* order is parent->child */
    if (vwmnp->mn_view.exid != (u_int) -1 && vwmnp->mn_view.exid != exid) {
	/* already exported with different ID */
	error = EEXIST;
	goto badexport;
    }
    if (exid < mnp->mn_ramdir.export_hwm) {
	/* insure that we're duplicating an existing export
	   or creating a new one. */
	if (mnp->mn_ramdir.export_ents[exid] == vwmnp->mn_view.id ||
	    mnp->mn_ramdir.export_ents[exid] == -1) {
	    /* redundant export ID setting (OK) or
	       new entry for this slot. (re)Enter it. */
	    mnp->mn_ramdir.export_ents[exid] = vwmnp->mn_view.id;
	    vwmnp->mn_view.exid = exid;
	} else {			
	    /* someone else has this export ID */
	    error = EEXIST;
	    goto badexport;
	}
    } else if (exid < mnp->mn_ramdir.max) {
	/* need to expand table.  Expand size by increments of
	   MVFS_EXPORT_CHUNK to accommodate this export ID.  Don't forget
	   that exid is used as an index, so we need +1 to account for
	   0-based array indexes. */
	size = ((exid + 1 + MVFS_EXPORT_CHUNK-1) / MVFS_EXPORT_CHUNK) * MVFS_EXPORT_CHUNK;
	newids = (int *) KMEM_ALLOC(size*sizeof(int), KM_SLEEP);
	if (newids == NULL) {
            error = ENOMEM;
            goto badexport;
        }
	if (mnp->mn_ramdir.export_hwm) {
	    len = sizeof(int)*mnp->mn_ramdir.export_hwm;
	    BCOPY(mnp->mn_ramdir.export_ents, newids, len);
	    KMEM_FREE(mnp->mn_ramdir.export_ents, len);
	}
	/* fill in new unused entries with marker */
	for (i = mnp->mn_ramdir.export_hwm; i < size; i++)
	    newids[i] = -1;
	mnp->mn_ramdir.export_ents = newids;
	mnp->mn_ramdir.export_hwm = size;
	/* and assign our slot */
	mnp->mn_ramdir.export_ents[exid] = vwmnp->mn_view.id;
	vwmnp->mn_view.exid = exid;
    } else
	error = EINVAL;		/* out of range of what ramdir can handle */
badexport:
    MUNLOCK(vwmnp);
    MUNLOCK(mnp);
    return error;
}

/* MFS_VIEWDIROPEN */

int 
mfs_viewdiropen(vpp, flag, cred)
VNODE_T **vpp;
int flag;
CRED_T *cred;
{
    ASSERT(MFS_ISVIEWDIR(VTOM(*vpp)));

    VTOM(*vpp)->mn_ramdir.atime.tv_sec = MDKI_CTIME();

    return(0);		/* Easy as pie */
}

/* MFS_VIEWDIRCLOSE */

int 
mfs_viewdirclose(vp, flag, count, cred)
VNODE_T *vp;
int flag;
int count;
CRED_T *cred;
{

    ASSERT(MFS_ISVIEWDIR(VTOM(vp)));

    return(0);
}

/* MFS_VIEWDIRGETATTR - getattr for viewdir */

int 
mfs_viewdirgetattr(vp, vap, cred)
VNODE_T *vp;
VATTR_T *vap;
CRED_T *cred;
{
    int error;
    u_long dev;

    ASSERT(MFS_ISVIEWDIR(VTOM(vp)));

    /* This routine is easy if not want any info returned */

    if (vap == NULL) return(0);

    /* Generate an appropriate vattr struct to return */

    VATTR_SET_TYPE(vap, VDIR);
    VATTR_SET_MODE_RIGHTS(vap, (S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH));
    VATTR_SET_MODE_TYPE(vap, S_IFDIR);
    VATTR_SET_UID(vap, MDKI_ROOT_UID); /* viewdir uses unix cred for NT */
    VATTR_SET_GID(vap, MDKI_ROOT_GID); 
    dev = FSID_TO_DEV(VFS_FSID(vp->v_vfsp));
    VATTR_SET_FSID(vap, &dev);
    VATTR_SET_NODEID(vap, VTOM(vp)->mn_hdr.mnum);
    if (MDKI_COMMON_GET_DATAP()->mvfs_vlinkcnt2 == 0)
        VATTR_SET_NLINK(vap, VTOM(vp)->mn_ramdir.lnk_cnt);
    else
        VATTR_SET_NLINK(vap, 2);
    VATTR_SET_SIZE(vap, sizeof(struct mfs_ramdirent)*(VTOM(vp)->mn_ramdir.max));
    VATTR_SET_BLKSIZE(vap, DEV_BSIZE);
    VATTR_SET_ATIME_TS(vap, &VTOM(vp)->mn_ramdir.atime);
    VATTR_SET_MTIME_TS(vap, &VTOM(vp)->mn_ramdir.mtime);
    VATTR_SET_CTIME_TS(vap, &VTOM(vp)->mn_ramdir.ctime);
    /* XXX find appropriate mask */
    VATTR_SET_RDEV(vap, (dev_t)(dev & 0xffff));
    VATTR_SET_NBLOCKS(vap, VATTR_BTODB(VATTR_GET_SIZE(vap)));
    VATTR_FILL(vap);
    return(0);
}

/* MFS_VIEWDIRLOOKUP - lookup a name in a the viewdir */

int 
mfs_viewdirlookup(dvp, nm, vpp, cred, pnp, flag)
VNODE_T *dvp;
char *nm;
VNODE_T **vpp;
CRED_T *cred;
struct pathname *pnp;
int flag;
{
    register mfs_mnode_t *mnp;
    int vnum;
    int hm, hmnum;
    char *snm = NULL;
    VNODE_T *vw;
    int len;
    int error, error2;

    ASSERT(MFS_ISVIEWDIR(VTOM(dvp)));

    mnp =  VTOM(dvp);

    /* Check for history mode "tail" */

    hm = mfs_hmname(nm, &snm);
    if (!hm) snm = nm;

    MLOCK(mnp);
    for (vnum=0; vnum < mnp->mn_ramdir.hwm; vnum++) {
	if (mnp->mn_ramdir.ents[vnum].nm == NULL) continue;
	if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp),
                      mnp->mn_ramdir.ents[vnum].nm, snm) == 0) {
	    /* History mode name, find/create hm view from this view */
	    if (hm) {
    		STRFREE(snm);	/* Don't need string anymore */
	  	vw = mnp->mn_ramdir.ents[vnum].vp;
		/* Must be a view - nothing else */
		if (!MFS_VPISMFS(vw) || !MFS_ISVIEW(VTOM(vw))) {
		    MUNLOCK(mnp);
		    return(ENOENT);
		}
		VN_HOLD(vw);
		MUNLOCK(mnp);
        	error = mfs_viewdirhmview(vw, vpp, cred);
		VN_RELE(vw);		/* Done with view */
                /* 
                 * For case-insensitive lookup must set
                 * case-correct name in pnp from final vnode
                 * because we may have auto-registered a
                 * history mode view-tag and we want the correct
                 * history mode name.
                 */
                if (*vpp != NULL && MVFS_PN_CI_LOOKUP(pnp)) {
                    error2 = PN_SET_CASE_CORRECT_COMP(pnp, VTOM(*vpp)->mn_view.viewname);
		    if (error == 0)
		        error = error2;
                }
		return(error);		/* Return error/hist view */
	    } else {
	        *vpp = mnp->mn_ramdir.ents[vnum].vp;
	        if (MFS_VPISMFS(*vpp) && MFS_ISVIEW(VTOM(*vpp))) {
		    MFS_HOLDVW(*vpp);
	        } else {
	            VN_HOLD(*vpp);
	        }
                /* 
                 * Set case-correct component for case-insensitive lookups
                 * from dir entry (may not be a view-tag) 
                 */
		error = 0;
                if (MVFS_PN_CI_LOOKUP(pnp)) {
                    error = PN_SET_CASE_CORRECT_COMP(pnp, mnp->mn_ramdir.ents[vnum].nm);
                }
	        MUNLOCK(mnp);
	        return(error);
	    }
	}
    }

    /* Free up stripped name if allocated one */

    if (hm) STRFREE(snm);

    /* No match ever found ... return ENOENT */

    MUNLOCK(mnp);
    *vpp = NULL;
    return(ENOENT);
}

/* 
 * MFS_VIEWTAGLOOKUP - lookup a viewtag in the viewroot 
 * Really just a wrapper that knows about the viewroot dir vnode,
 * and doesn't require all the extra args of a full lookup.
 */

int
mfs_viewtaglookup(nm, vpp, cred)
mfs_pn_char_t *nm;
VNODE_T **vpp;
CRED_T *cred;
{
    int error;
    VNODE_T *vp;
    VNODE_T *dvp;

    dvp = mfs_getviewroot();
    if (!dvp) return(ENOENT);	/* Not found */

    error = mfs_viewdirlookup(dvp, nm, &vp, cred, (struct pathname *)NULL, 0);
    VN_RELE(dvp);

    /* Make sure we only return view tags from this routine */

    if (!error) {
	if (!MFS_ISVIEW(VTOM(vp))) {
	    VN_RELE(vp);
	    vp = NULL;
	    error = EINVAL;		/* So not get enoent on valid item */
	} else {
	    if (vpp) {
	        *vpp = vp;		/* Return vnode ptr (held) */
	    } else {
		VN_RELE(vp);		/* Release unwanted vnode ptr */
	    }
	}
    } else {
	if (vpp) *vpp = NULL;		/* If return vnode ptr, make sure NULL */
    }

    return(error);
}

/* MFS_VIEWDIRREADDIR - read the dir contents of the viewdir*/
int 
mvfs_viewdirreaddir(
    VNODE_T *dvp,
    struct uio *uiop,
    CRED_T *cred,
    int *eofp
)
{
    register mfs_mnode_t *mnp;
    size_t direntlen = KDIRENT_RECLEN(MAXNAMELEN);
    KDIRENT_T *dirent;
    size_t nmlen;
    int reclen;
    MOFFSET_T offset;
    ssize_t oresid;
    int error;
    VNODE_T *vp;
    int vnum;

    mnp =  VTOM(dvp);
    ASSERT(MFS_ISVIEWDIR(mnp));

    /* Initial checks */

    if (MVFS_IS_INVALID_OFFSET(MVFS_UIO_OFFSET(uiop)) || uiop->uio_resid <= KDIRENT_RECLEN(0)) {
	return(ENOENT);
    }

    /* Create and internel dirent buffer to use */

    dirent = (KDIRENT_T *)KMEM_ALLOC(direntlen, KM_SLEEP);
    if (dirent == NULL) {
	return(ENOMEM);
    }

    /* 
     * Return as many items as the buffer will hold.  Note
     * that at EOF we must return no entries with an
     * error=0 
     *
     * Because some ports require that file-systems return byte-length
     * offsets in dirs (not cookies), we just do this for all virtual
     * directories in the MVFS by design.  That way, we never have
     * to worry about using the dreaded dir-index cache for these directories.
     *
     * The basic strategy here is to scan the whole directory, computing
     * the offset in bytes that would have been generated and returning
     * those entries that are past the user-passed-in offset.  This is
     * a crummy algorithm, but it is OK because of a couple of assumptions:
     *	1) Applications read the directory in large chunks
     *  2) The list of registered view-tags is relatively small.
     * These two assumptions lead to the conclusion that only 1 or maybe 2
     * readdir operations will be required to get the whole directory
     * contents, so using this crummy algorithm isn't such a big deal.
     * (Why make things complicated if you don't need to!)
     *
     * Speaking of making things complicated... For Linux the check
     * for whether we have filled the user buffer doesn't work because
     * the buffers are opaque to us.  The uio structure contains a
     * pointer to a function that handles the actual movement of data.
     * The only way to know that the buffer is full is that the
     * filldir function will return an error.  When this happens, the
     * Linux function will make READDIR_BUF_FULL() true and set offset
     * so that we exit this loop cleanly and with the proper offset
     * for next time.
     */

    error = 0;
    oresid = uiop->uio_resid;
    offset = 0;

    MLOCK(mnp);
    for (vnum=0; vnum < mnp->mn_ramdir.hwm; vnum++, offset += reclen) {

        /*
         * We don't check to see if offset has grown
         * larger that MVFS_MAX_DIRSIZE because we
         * limit the growth.
         */

        ASSERT(offset <= MVFS_MAX_DIRSIZE);
	reclen = 0;
	if (mnp->mn_ramdir.ents[vnum].nm == NULL) continue;
	vp = mnp->mn_ramdir.ents[vnum].vp;
	ASSERT(vp);
        nmlen = STRLEN(mnp->mn_ramdir.ents[vnum].nm);
	reclen = KDIRENT_RECLEN(nmlen);

	/* Skip entries before requested offset */

	if (offset < (MOFFSET_T)MVFS_UIO_OFFSET(uiop)) continue;

        /* Fill in the dirent struct buffer we have allocated */

	KDIRENT_INIT(dirent, VTOM(vp)->mn_hdr.mnum, mnp->mn_ramdir.ents[vnum].nm,
			nmlen, offset+reclen);

  	/* See if room in user buffer */

	if ((int)reclen > uiop->uio_resid) {
	    /* Error if no entries returned yet */
	    if (oresid == uiop->uio_resid) {
		error = EINVAL;
		break;
	    }
	    break;
	}

	/* Copy to user */

	error = READDIR_UIOMOVE((caddr_t)dirent, &reclen, UIO_READ, uiop, offset);
        if (error != 0 || READDIR_BUF_FULL(uiop))
            break;
    }

    MVFS_UIO_OFFSET(uiop) = offset;
    MUNLOCK(mnp);
   
    KMEM_FREE(dirent, direntlen); 
    return(error);
}

/*
 * Find or create a history mode view from a non-history mode
 * view.
 */

int
mfs_viewdirhmview(vw, vpp, cred)
VNODE_T *vw;
VNODE_T **vpp;
CRED_T *cred;
{
    VNODE_T *vdir;
    register mfs_mnode_t *viewrootmnp, *vwmnp, *hmmnp;
    VNODE_T *hmvw;
    char *hmnm = NULL;
    char *nm;
    int len;
    int vnum;
    int error = 0;

    vwmnp = VTOM(vw);
    ASSERT(MFS_ISVIEW(vwmnp));
    *vpp = NULL;

    /* History mode stays history mode */

    if (vwmnp->mn_view.hm) {
	*vpp = vw;
	VN_HOLD(vw);
	return(0);
    }

    vdir = mfs_getviewroot(); /* includes a hold */
    if (vdir == NULL)
        return(ESTALE);

    viewrootmnp = VTOM(vdir);

    /* 
     * Now look in the viewdir to see if already a history
     * mode view with the same view name.
     */

    /*
     * lock order: we are allowed to hold locks from higher to lower
     * levels in the name tree.  compare to
     * mvfs_utils.c:mfs_copyout_viewtag()
     */
    MLOCK(viewrootmnp);                 /* viewroot */
    MLOCK(vwmnp);                       /* view itself */

    vnum = vwmnp->mn_view.id;

    /* Check for "stale" view */
    /* RATLC01007990: An endview process could have raced us and initiated
     *                the deletion of the view tag directory. vnum will be
     *                set to zero only after the mnodes with the view are
     *                flushed. If the view tag is already deallocated then
     *                the view is stale even though vnum is non-zero. 
     *                Check mfs_viewdirrmdir() or refer the defect
     *                notes for further information.
     */
    if ((vnum == 0) || (viewrootmnp->mn_ramdir.ents[vnum].nm == NULL)) {
        MUNLOCK(vwmnp);
	MUNLOCK(viewrootmnp);
	VN_RELE(vdir);
	mvfs_log(MFS_LOG_ESTALE, "hmview: stale vw=%s\n", mfs_vw2nm(vw));
	return(ESTALE);
    }

    hmnm = mfs_hmappend(VTOM(vdir)->mn_ramdir.ents[vnum].nm);
    MUNLOCK(vwmnp);                     /* drop lock on view */
    if (hmnm == NULL) { /* Could not get memory for new name */
	MUNLOCK(viewrootmnp);
	VN_RELE(vdir);
	mvfs_log(MFS_LOG_ERR, "hmview: no memory for hm viewname\n");
	return(ENOMEM);
    }

    for (vnum=0; vnum < viewrootmnp->mn_ramdir.hwm; vnum++) {
	if (viewrootmnp->mn_ramdir.ents[vnum].nm == NULL) continue;
	if (STRCMP(viewrootmnp->mn_ramdir.ents[vnum].nm, hmnm) == 0) {
	    hmvw = viewrootmnp->mn_ramdir.ents[vnum].vp;
	    ASSERT(MFS_VPISMFS(hmvw) && MFS_ISVIEW(VTOM(hmvw)));

	    /* Release locks, dir, storage */

	    *vpp = hmvw;
	    VN_HOLD(hmvw);
	    MUNLOCK(viewrootmnp);
	    VN_RELE(vdir);
	    STRFREE(hmnm);

	    /* Go update the hmview with the latest svr info
	     * from the "master" view
	     */

	    goto update_hmview;
	}
	    
    }

    /*
     * Couldn't find one, then CREATE one
     */

    MUNLOCK(viewrootmnp);	/* Unlock for mkdir */
    error = mfs_viewdirmkdir(vdir, hmnm, NULL, &hmvw, cred, vwmnp->mn_view.svr.host, vwmnp->mn_view.windows_view);
    VN_RELE(vdir);	/* Done with view root dir */
    STRFREE(hmnm);	/* Done with history mode name */
    if (error) return(error);

    /* 
     * Set history mode bit on view and copy original view info
     * into this view. 
     */

update_hmview:
    hmmnp = VTOM(hmvw);		/* Now work on new history mode view */

    /*
     * lock both regular and new hm view nodes, so we can copy safely
     * (some items can be updated, e.g. svr.addr)
     */
    MLOCK2(hmmnp,vwmnp);
    hmmnp->mn_view.vh  = vwmnp->mn_view.vh;
    hmmnp->mn_view.hm  = 1;
    hmmnp->mn_view.svr.svrbound = vwmnp->mn_view.svr.svrbound;
    hmmnp->mn_view.svr.addr = vwmnp->mn_view.svr.addr;
    PNPAIR_STRFREE(&hmmnp->mn_view.svr.lpn);
    MFS_STRBUFPN_PAIR_GET_KPN(&hmmnp->mn_view.svr.lpn).s =
		STRDUP(MFS_STRBUFPN_PAIR_GET_KPN(&(vwmnp->mn_view.svr.lpn)).s);
    MFS_STRBUFPN_PAIR_GET_KPN(&hmmnp->mn_view.svr.lpn).l = STRLEN(MFS_STRBUFPN_PAIR_GET_KPN(&hmmnp->mn_view.svr.lpn).s);
    MFS_STRBUFPN_PAIR_GET_KPN(&hmmnp->mn_view.svr.lpn).m = MFS_STRBUFPN_PAIR_GET_KPN(&hmmnp->mn_view.svr.lpn).l + 1;

    if (hmmnp->mn_view.svr.host) STRFREE(hmmnp->mn_view.svr.host);
    hmmnp->mn_view.svr.host = STRDUP(vwmnp->mn_view.svr.host);

    if (hmmnp->mn_view.svr.rpn) STRFREE(hmmnp->mn_view.svr.rpn);
    hmmnp->mn_view.svr.rpn = STRDUP(vwmnp->mn_view.svr.rpn);
    hmmnp->mn_view.svr.uuid = vwmnp->mn_view.svr.uuid;
    /* Now that we have complete information about the hmview, update
     * the credentials to UNIX or NT depending upon view type (based
     * on view info we just set up above).
     */
    MVFS_COPY_UID_TO_VIEW(hmmnp, MVFS_VIEW_CREDS(hmvw, cred, TRUE), &error);
    if (error == 0) {
        MVFS_COPY_GID_TO_VIEW(hmmnp, MVFS_VIEW_CREDS(hmvw, cred, TRUE), &error);
        if (error != 0)
            mvfs_log(MFS_LOG_DEBUG, "hmview: error %d updating GID in vw=%s\n",
                                  error, mfs_vw2nm(hmvw));
    } else {
        mvfs_log(MFS_LOG_DEBUG, "hmview: error %d updating UID in vw=%s\n",
                              error, mfs_vw2nm(hmvw));
    }

    MUNLOCK(vwmnp);
    MUNLOCK(hmmnp);
    if (error != 0) {
       VN_RELE(hmvw);
       *vpp = NULL;
       return(error);
    }

    *vpp = hmvw;
    return(0);
}

/*
 * Purge unused history mode views from the view directory
 */

void
mfs_viewdircleanhm(dvp)
VNODE_T *dvp;
{
    register mfs_mnode_t *mnp;
    register int vnum;
    VNODE_T *vw;
    int hwm;

    ASSERT(MFS_ISVIEWDIR(VTOM(dvp)));

    mnp = VTOM(dvp);
    MLOCK(mnp);
    for (vnum=0; vnum < mnp->mn_ramdir.hwm; vnum++) {
	vw = mnp->mn_ramdir.ents[vnum].vp;

	/* XXX Checking VTOM(vw) for NULL to be sure we don't panic
	 * accessing a NULL mnode, this shouldn't happen but has been seen
	 * in practice.  This will help prevent panics until we understand the
	 * root cause of the problem.  See ESC RATLC01024800. */
	if ((vw == NULL) || (VTOM(vw) == NULL)) continue;
	if (MFS_VPISMFS(vw) && 
            		MFS_ISVIEW(VTOM(vw)) &&
	    		VTOM(vw)->mn_view.hm &&
	    		(VTOM(vw)->mn_view.usedtime+mvfs_idleview_timeout
							< MDKI_CTIME())) {
	    mfs_mnflushvw(vw);	    	/* Flushed cached mnodes for refcnt */
	    if (V_COUNT(vw) == 1) {	/* No users, remove */
		ASSERT(mnp->mn_ramdir.ents[vnum].nm);
	        STRFREE(mnp->mn_ramdir.ents[vnum].nm);
		if (MVFS_ISVTYPE(vw, VDIR))
		    mnp->mn_ramdir.lnk_cnt--;
	        mnp->mn_ramdir.ents[vnum].nm = NULL;
	        mnp->mn_ramdir.ents[vnum].vp = NULL;
		VN_RELE(vw);

		/* Update dir mod time */

		mnp->mn_ramdir.mtime.tv_sec = MDKI_CTIME();
		
		/* Update high water mark if releasing highest slot */

		if (vnum == mnp->mn_ramdir.hwm-1) {
		    for (hwm = vnum-1; (mnp->mn_ramdir.ents[hwm].nm == NULL)
				&& (hwm >= 0); hwm--) {}
		    mnp->mn_ramdir.hwm = hwm+1;
		}
	    } else {
		/* Update "used time" so we don't constantly try to
                   get rid of a HM view the user is still using */
		VTOM(vw)->mn_view.usedtime = MDKI_CTIME();
	    }
	}
    }

    MUNLOCK(mnp);
    return;
}

/*
 * MFS_VIEWDIRFLUSHRVC - flush rvc for all active views.  
 * 	FIXME:  really should be an rvc call that scans mnodes,
 *		but the locking is too hard right now.  Revisit this
 *		when global view tags implemented.
 */

void
mfs_viewdirflushrvc()
{
    VNODE_T *dvp;
    register mfs_mnode_t *mnp;
    register int vnum;
    VNODE_T *vw;
    int hwm;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();
     
    if (vrdp->mfs_viewroot_vfsp) {
	dvp = VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_rootvp;
	VN_HOLD(dvp);
    } else {
	return;
    }
	
    ASSERT(MFS_ISVIEWDIR(VTOM(dvp)));

    mnp = VTOM(dvp);
    MLOCK(mnp);
    for (vnum=0; vnum < mnp->mn_ramdir.hwm; vnum++) {
	vw = mnp->mn_ramdir.ents[vnum].vp;
	if (vw == NULL) continue;
	if (MFS_VPISMFS(vw) && MFS_ISVIEW(VTOM(vw))) {
	    mvfs_rvcflush(vw, NULL);    /* Flush RVC for view */
	}
    }

    MUNLOCK(mnp);
    VN_RELE(dvp);
    return;
}

#define NEXTSTAMP_INCR(mnp) \
	    (mnp)->mn_view.vobstamp_next++; \
	    if ((mnp)->mn_view.vobstamp_next == MVFS_NUM_VOB_STAMPS) \
		(mnp)->mn_view.vobstamp_next = 0

STATIC int mvfs_viewdir_find_vobstamp_subr(P1(struct mfs_mnode *mnp)
					   PN(tbs_uuid_t *uuid));

STATIC int
mvfs_viewdir_find_vobstamp_subr(mnp, uuid)
register struct mfs_mnode *mnp;
tbs_uuid_t *uuid;
{
    register int i;

    for (i = 0; i < MVFS_NUM_VOB_STAMPS; i++) {
	if (MFS_UUIDEQ(mnp->mn_view.vobstamps[i].vobuuid, *uuid)) {
	    /* Since we'll use this entry now, make sure it's not the next
	     * ejection candidate so it sticks around for a while.
	     */
	    if (mnp->mn_view.vobstamp_next == i) {
		NEXTSTAMP_INCR(mnp);
	    }
	    return i;
	}
    }
    return -1;
}

/*
 * mvfs_viewdir_find_vobstamp: find the VOB stamp matching the given replica
 * uuid.  If no stamp is cached, or the stamp is out of date,
 * return 0.  If stamp is valid, return 1.
 */
int
mvfs_viewdir_find_vobstamp(vw, vobuuid, outstamp, outlife)
VNODE_T *vw;
tbs_uuid_t *vobuuid;
struct timeval *outstamp;
time_t *outlife;
{
    register int i;
    SPL_T s;
    time_t ctime;
    register struct mfs_mnode *mnp = VTOM(vw);

    ASSERT(MFS_ISVIEW(mnp));
    ctime = MDKI_CTIME();

    MVFS_LOCK(STAMPLOCK_ADDR(mnp));
    i = mvfs_viewdir_find_vobstamp_subr(mnp, vobuuid);
    if (i != -1 && ctime <= mnp->mn_view.vobstamps[i].valid_thru) {
	*outstamp = mnp->mn_view.vobstamps[i].lvut;
	*outlife = mnp->mn_view.vobstamps[i].valid_thru;
	/* Since we just used this entry, make sure it's not the next
	 * ejection candidate
	 */
	if (mnp->mn_view.vobstamp_next == i) {
	    NEXTSTAMP_INCR(mnp);
	}
	MVFS_UNLOCK(STAMPLOCK_ADDR(mnp));
	return 1;
    }
    MVFS_UNLOCK(STAMPLOCK_ADDR(mnp));
    return 0;
}

/*
 * mvfs_viewdir_save_vobstamp: Save the new LVUT in the view for this VOB.
 * Replace the previous entry for this VOB, if possible.
 */
void
mvfs_viewdir_save_vobstamp(vw, vobuuid, timeout, newlvut)
VNODE_T *vw;
tbs_uuid_t *vobuuid;
time_t timeout;
struct timeval *newlvut;
{
    register int i;
    SPL_T s;
    register struct mfs_mnode *mnp = VTOM(vw);

    ASSERT(MFS_ISVIEW(mnp));

    MVFS_LOCK(STAMPLOCK_ADDR(mnp));
    i = mvfs_viewdir_find_vobstamp_subr(mnp, vobuuid);
    if (i == -1) {
	/*
	 * eject some other entry.
	 */
	i = mnp->mn_view.vobstamp_next;
	NEXTSTAMP_INCR(mnp);
	mnp->mn_view.vobstamps[i].vobuuid = *vobuuid;
    }
    mnp->mn_view.vobstamps[i].lvut = *newlvut;
    if (mnp->mn_view.vobstamps[i].valid_thru < timeout) {
        MDB_XLOG((MDB_LVUT,
		  "save_vobstamp: timeout (%"KS_FMT_TIME_T_D") < existing timeout (%ld)\n",
		  timeout, mnp->mn_view.vobstamps[i].valid_thru));
    }
    mnp->mn_view.vobstamps[i].valid_thru = timeout;
    MVFS_UNLOCK(STAMPLOCK_ADDR(mnp));
    return;
}

int
mvfs_viewinit(mvfs_cache_sizes_t *mma_sizes)
{
    mvfs_viewroot_data_t *vrdp;

    MDKI_VIEWROOT_ALLOC_DATA();
    vrdp = MDKI_VIEWROOT_GET_DATAP();

    /* 
     * The VFS and VP of the viewroot mount.
     * Must hold mount lock or increment vnode refcount when using these fields!
     */
    vrdp->mfs_viewroot_vfsp = NULL;
    vrdp->mfs_viewroot_vp = NULL;
    vrdp->mfs_viewroot_specvp = NULL;

    INITLOCK(&(vrdp->mvfs_mkviewtag_lock), "mvfsvwtg");
    return(0);
}

void
mvfs_viewfree()
{
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    FREELOCK(&(vrdp->mvfs_mkviewtag_lock));
    MDKI_VIEWROOT_FREE_DATA();

}

/*
 * mvfs_viewuuidrecover: try to find a view not known to the VIEWDIR
 * by uuid and tag which matches the caller's tag and uuid, reattach
 * it, and return.
 */

int
mvfs_viewuuidrecover(
    mfs_pn_char_t *nm,
    tbs_uuid_t *vwuuid,
    char *host,
    mfs_strbufpn_pair_t *lpn,
    char *rpn,
    VNODE_T **vwpp,
    CRED_T *cred
)
{
    mfs_mnode_t *mnp;
    int mnum, error;
    VNODE_T *dvp, *xvp;

    dvp = mfs_getviewroot();
    if (!dvp) return(ENOENT);	/* Not found */

    /* Look for view node by uuid */
    mnum = 0;
    error = ENOENT;
    while (error != 0 && (mnp = mvfs_mngetnextview(&mnum)) != NULL) {
        /*
         * If we find a detached view node with the same uuid, global path,
         * host, local path, and view tag name, then accept it and reattach it.
         * (Must check view tag name, else two tags for the same view will
         * end up attached to the same view node, which causes problems both
         * with 'cleartool pwv' perhaps finding the other name not used by
         * the user and when removing a view tag (processes set to the other
         * tag get screwed up).
         */
        if (mnp->mn_view.id == MFS_NULLVID &&
            MFS_UUIDEQ(mnp->mn_view.svr.uuid, *vwuuid) &&
            PN_STRCMP(FALSE,
                      MFS_STRBUFPN_PAIR_GET_KPN(&mnp->mn_view.svr.lpn).s,
                      MFS_STRBUFPN_PAIR_GET_KPN(lpn).s) == 0 &&
            HN_STRCMP(mnp->mn_view.svr.host, host) == 0 &&
            PN_STRCMP(FALSE, mnp->mn_view.svr.rpn, rpn) == 0 &&
            PN_STRCMP(FALSE, nm, mnp->mn_view.viewname) == 0)
        {
            ASSERT(mnp->mn_hdr.vfsp != NULL);
            error = MVFS_VNGET(mnp->mn_hdr.vfsp, NULL, mnp, vwpp);
            /*
             * MFS_VNGET gets a ref cnt on vnode (if no error).  It
             * always drops the lock on the mnode and consumes a
             * reference count on mnp (even in error cases), so we
             * should not mfs_mnrele() after this point.
             */
            if (error) {
                /*
                 * when VNGET returns an error, it returns no vnode
                 * either.  Just keep looking.  (We don't just bail
                 * out, in case there is another previous view vnode
                 * which wasn't able to be reattached in a prior
                 * attempt, but might be usable now.)
                 */
                *vwpp = NULL;
                continue;
            } else {
#ifdef MVFS_DEBUG
                mvfs_log(MFS_LOG_DEBUG, "found view %s vp %"KS_FMT_PTR_T" by uuid\n",
                         mnp->mn_view.viewname, *vwpp);
#endif
                /* everything checks out */
                error = mvfs_ramdir_insert(dvp, nm, *vwpp);
                if (error) {
                    /*
                     * Oops, something went wrong.  Give up the
                     * search & return
                     */
                    VN_RELE(*vwpp); /* drop hold from VNGET */
                    *vwpp = NULL;
                } 
                /*
                 * if no error, ramdir_insert() used up the reference
                 * returned by VNGET
                 *
                 * In all cases, this was the right view, so conclude the
                 * search & return (either with or without error)
                 */
                break;
            }
        } else {
            /* wrong view, release mnode and keep searching */
            MUNLOCK(mnp);
            mfs_mnrele(mnp);
        }
    }
    VN_RELE(dvp);
    return error;
}
static const char vnode_verid_mvfs_vwdirops_c[] = "$Id:  c0e0e8d4.737211e1.90e6.00:01:83:0a:3b:75 $";
