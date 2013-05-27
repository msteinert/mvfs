/* * (C) Copyright IBM Corporation 1993, 2005. */
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
/* mvfs_ntvwops.c */
/* Description:
 *	NT view-tags are view-tags that work just like the orignal Unix
 *	view-tags, except that they are not LOOPBACK nodes on a global
 *	root (because NT doesn't have a global root).  Instead... these
 *	view-tags appear to just have the VOB's underneath them.  This gives
 *	a single mount that looks like this:
 *	pathname				kind of MVFS object
 *      ------------------------------------	-------------------
 *	/					MFS_VIEWDIRCLAS
 *	/<view-tag>				MFS_NTVWDIRCLAS
 *	/<view-tag>/<vob-tag>			MFS_VOBRTCLAS (not mounted on anything)
 *
 *	Each NT view-tag gets added to the VIEWDIR object by the
 *	ioctls that register view-tags with the MFS and are called from
 *	startview or setview.
 *
 *	Each NT vob-tag gets added to a list of all VOBs (and therefore will
 *	appear under every registered view-tag) by an ioctl that
 *	registers a VOB with the MVFS (without actually having a Unix-style
 *	mount going on).
 *
 * 	Most all operations on these view-tags return EINVAL or EROFS, since
 *	the directory this tag represents can not be written.
 *
 *	The following read-only operations are supported and implemented
 *	in this module:
 *		getattr()	Gets stats for the NT view-tag.  These are completely
 *				faked.
 *		lookup()	Attempts to lookup a VOBRT object by scanning the list
 *				of all registered VOBs and looking for a vob-tag name
 *				match.
 *		readdir()	Returns a list of all the currently registered vob-tags
 *				in the readdir directory entry format.
 */
#include "mvfs_systm.h"
#include "mvfs.h"

/****************************************************************************
 * mvfs_ntvw_getattr
 * Return canned vnode attributes structure for an NT-style view-tag
 * IN	vp		Vnode pointer to an NT view-tag
 * IN	vap		(OPTIONAL) Ptr to a vnode attributes structure to fill in
 * IN   cred		Ptr to the current user-credentials structure
 * RESULT:              Unix errno
 */
int 
mvfs_ntvw_getattr(vp, vap, cred)
VNODE_T *vp;
VATTR_T *vap;
CRED_T *cred;
{

    mfs_mnode_t *mnp = VTOM(vp);
    dev_t dev;
    struct vfs *vfsp;
    int error;

    ASSERT(MFS_ISNTVIEW(mnp));

    /* This routine is easy if not want any info returned */

    if (vap == NULL) return(0);

    /* 
     * Generate an appropriate vattr struct to return 
     * Here is what I think is appropriate:
     *	Kind of object:	Directory
     *  Unix rights:	r-xr-xr-x
     *	Owner:		Uid of the person who registered the view-tag
     *  Group:		Gid of the person who registered the view-tag
     *  Fsid:		fsid of view-dir it is cataloged in
     *  Inumber:	Mnumber (used for all objects on view-dir mount)
     *  Nlink:		2
     *  Size:		DEV_BSIZE (1 block in size)
     *  Blksize:	DEV_BSIZE
     *  Atime:		Creation time of view-tag 
     *			(FIXME: should be time of last open from open op).
     *  Mtime:		Creation time of view-tag 
     *			(FIXME: should be time of last mount/unmount)
     *  Ctime:		Creation time of view-tag
     *  Rdev:		Variant of fsid returned above (in dev format)
     *  Nblocks:	Calculated from size/blksize above... but
     *			I'll bet you the value is "1".
     */
    VATTR_SET_TYPE(vap, VDIR);
    VATTR_SET_MODE_RIGHTS(vap, (S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH));
    VATTR_SET_MODE_TYPE(vap, S_IFDIR);
    MVFS_COPY_UID_TO_VATTR(vap, &mnp->mn_view.cuid, mnp, &error);
    MVFS_COPY_GID_TO_VATTR(vap, &mnp->mn_view.cgid, mnp, &error);
    dev = FSID_TO_DEV(VFS_FSID(vp->v_vfsp));
    VATTR_SET_FSID(vap, &dev);
    VATTR_SET_NODEID(vap, VTOM(vp)->mn_hdr.mnum);
    VATTR_SET_NLINK(vap, 2);
    VATTR_SET_SIZE(vap, DEV_BSIZE);
    VATTR_SET_BLKSIZE(vap, DEV_BSIZE);
    VATTR_SET_ATIME_TS(vap, &mnp->mn_view.ctime);
    VATTR_SET_MTIME_TS(vap, &mnp->mn_view.ctime);
    VATTR_SET_CTIME_TS(vap, &mnp->mn_view.ctime);
    VATTR_SET_RDEV(vap, (dev_t)(dev & 0xffff));
    VATTR_SET_NBLOCKS(vap, VATTR_BTODB(VATTR_GET_SIZE(vap)));
    VATTR_FILL(vap);
    return(0);
}

/****************************************************************************
 * mvfs_ntvw_lookup
 * Lookup and return a VOBRT vnode from the vob-tag name
 * IN	dvp		Vnode pointer to an NT view-tag
 * IN	nm		Name of the vob-tag to lookup
 * OUT  vpp		Ptr to a var to get the resulting vnode ptr from the lookup
 *			or NULL if the lookup fails
 * IN OUT pnp		(OPTIONAL) Ptr to a pathname struct (not used)
 * IN   flags		flags that control the lookup (not used)
 * IN   rdir		(OPTIONAL) root vnode ptr (not used)
 * IN   cred		Ptr to the current user-credentials structure
 * RESULT:              Unix errno
 */
int 
mvfs_ntvw_lookup(dvp, nm, vpp, pnp, flags, rdir, cred)
VNODE_T *dvp;
char *nm;
VNODE_T **vpp;
struct pathname *pnp;
int flags;
VNODE_T *rdir;
CRED_T *cred;
{
    register mfs_mnode_t *mnp;
    VNODE_T *vp;
    int error, error2;
    VFS_T *vfsp;

    mnp =  VTOM(dvp);
    ASSERT(MFS_ISNTVIEW(mnp));

    /* Handle "." and ".." */

    if (STRCMP(nm, ".") == 0) {
	*vpp = dvp;
	VN_HOLD(*vpp);
	error = 0;
	goto done;
    }

    if (STRCMP(nm, "..") == 0) {
	*vpp = mfs_getviewroot();
	if (*vpp) {
	    /* Getviewroot already did hold */
	    error = 0;
	} else {
	    error = ENOENT;
	}
	goto done;
    }

    /* 
     * Handle SETVIEW^ hack in here too... see mfs_lookup() for description
     * under old-style view-tags.
     */

    if (PN_STRCMP(MVFS_PN_CI_LOOKUP(pnp), nm, "SETVIEW^") == 0) {
        VN_HOLD(dvp);                           /* Hold for setview call */
        error = MVFS_SET_PROCVIEW(dvp, NULL);   /* Set the view-tag */
        if (!error) {
            *vpp = dvp;
            VN_HOLD(*vpp);
        }
        goto done;
    }
        
    /* 
     * Scan the list of registered vobs looking for a name match.
     * Must lock the lists first so the VFS pointer can't go
     * away while we are using it.
     */

    mfs_findvfs_lock();
    vfsp = mfs_findvfs_nm(MVFS_PN_CI_LOOKUP(pnp), nm);
    if (vfsp == NULL) {			/* Not found -> ENOENT */
	mfs_findvfs_unlock();
	*vpp = NULL;
	return(ENOENT);
    }

    /* 
     * Found a matching VOB mount, now get the vob-rt if we can 
     * to hold the mount-point around while the lock is unlocked.
     */

    error = MVFS_ROOT(vfsp, &vp);
    mfs_findvfs_unlock();
    if (error) goto done;	/* Some problem */
    ASSERT(vp != NULL);
    ASSERT(vfsp != NULL);

    /* If case-insensitive lookup, must set case-correct component */

    if (MVFS_PN_CI_LOOKUP(pnp)) {
        error = PN_SET_CASE_CORRECT_COMP(pnp, VFS_TO_MMI(vfsp)->mmi_vobtag);
    }

    /* Make a VOBRT vnode in the desired view */

    error2 = mfs_makevobrtnode(MFS_VIEW(dvp), vfsp, vpp);
    if (error == 0)
        error = error2;

    /* Done with non-view mount-point */

    VN_RELE(vp);

    /* 
     * Either return the VOBRT and no error, or the error returned
     * from the mfs_makevobrtnode call
     */
done:
    return(error);
}

/****************************************************************************
 * mvfs_ntvw_readdir
 * Lookup and return a VOBRT vnode from the vob-tag name
 * IN	dvp		Vnode pointer to an NT view-tag
 * IN	uiop		Ptr to uio struct decribing the user's buffer
 * IN   cred		Ptr to the current user-credentials structure
 * RESULT:              Unix errno
 */
int 
mvfs_ntvw_readdir(dvp, uiop, cred, eofp)
VNODE_T *dvp;
struct uio *uiop;
CRED_T *cred;
int *eofp;
{
    register mfs_mnode_t *mnp;
    size_t direntlen = KDIRENT_RECLEN(MAXNAMELEN);
    KDIRENT_T *dirent;
    size_t nmlen;
    int reclen;
    MOFFSET_T offset;
    MVFS_UIO_RESID_T oresid;
    int error;
    VNODE_T *vp;
    u_long cookie;
    VFS_T *vfsp;
    int i;
    char *nm = 0;                       /* shut up GCC = 0 */

    mnp =  VTOM(dvp);
    ASSERT(MFS_ISNTVIEW(mnp));

    /* Initial checks */

    if (MVFS_IS_INVALID_OFFSET(MVFS_UIO_OFFSET(uiop)) || uiop->uio_resid <= KDIRENT_RECLEN(0)) {
	return(ENOENT);
    }

    /* 
     * Create and internel dirent buffer to use 
     * This dirent buffer can hold a maximum length "name".
     */

    dirent = (KDIRENT_T *)KMEM_ALLOC(direntlen, KM_SLEEP);
    if (dirent == NULL) {
	return(ENOMEM);
    }

    /* 
     * Return as many items as the buffer will hold.  Note
     * that at EOF we must return no entries with an
     * error=0.
     *
     * For portability, all virtual directories in the MVFS
     * are based on byte-offsets, not small integer (or large integer)
     * cookies.  This is because many Unix systems or NFS implementations
     * ASSUME byte-offsets as the coin of the realm when talking about
     * subsequent readdir operations... so it is much easier if we
     * just conform to this silliness to begin with (lest we have to
     * use the dreaded dir-index cache!)
     *
     * Since I expect the list of vob's to be short, and I expect
     * higher level code to get big chunks of data at a time (normally
     * everything in 1 or possibly 2 calls total), I think it
     * is an OK algorithm to restart at the beginning of the list
     * every time and scan it for the offset the user wants.
     */
    error = 0;
    oresid = uiop->uio_resid;		/* Starting resid before moving any data */

    /* 
     * Keep track of byte-offset.  All MVFS virtual dirs are
     * based on real byte-offsets, not cookies in their returns.
     */
    offset = 0;			/* Offset in the virtual vw-tag dir we are at */

    /* First two entries are "." and ".." */

    for (i=0; i < 2; i++, offset +=reclen) {
	if (i == 0) nm = ".";
	if (i == 1) nm = "..";

        /*
         * We don't check to see if offset has grown
         * larger that MVFS_MAX_DIRSIZE because we
         * limit the growth.
         */

        ASSERT(offset <= MVFS_MAX_DIRSIZE);

	nmlen = STRLEN(nm);
	reclen = KDIRENT_RECLEN(nmlen);

	if (offset < (int)MVFS_UIO_OFFSET(uiop)) continue;

	/* FIXME: dbid for .. is wrong - should be for /view, not vwtag */

	KDIRENT_INIT(dirent, VTOM(dvp)->mn_hdr.fid.mf_dbid, nm, nmlen, 
					offset+reclen);

   	/* Verify room in buffer */

	if ((int)reclen > uiop->uio_resid) {
	    if ((int)uiop->uio_resid == oresid) {
		error = EINVAL;
		goto exit_no_lock;
	    } else {
		/* No room in buffer */
		error = 0;
		goto exit_no_lock;
	    }
	}

	/* Copy to user */

 	error = READDIR_UIOMOVE((caddr_t)dirent, &reclen, UIO_READ, uiop, offset);
	if (error != 0 || READDIR_BUF_FULL(uiop))
            goto exit_no_lock;
    }
		
    /* 
     * Start scan at beginning of all the VFS's.  Start returning
     * entries when we have passed the desired user offset
     */

    cookie = 0;				/* Cookie that matches that offset */

    mfs_findvfs_lock();			/* Lock the VFS structures */
    while ((vfsp = mfs_findvfs_cookie(&cookie)) != NULL) {
	reclen = 0;
        nmlen = STRLEN(VFS_TO_MMI(vfsp)->mmi_vobtag);
	reclen = KDIRENT_RECLEN(nmlen);

        /* 
         * Skip any entry for which the name is too long.  This is a bug
	 * from the mount, and we don't want to even see this in the virtual dir,
	 * so we also don't update the offset.
         */
	if (nmlen > MAXNAMELEN) continue;

    	/* 
	 * If the offset in the virtual dir is before the user-rqst, skip
	 * this entry, but update the offset for the next entry.
	 */

	if (offset < (int)MVFS_UIO_OFFSET(uiop)) {
	        offset += reclen;
		continue;
	}

	/* Get the root vnode of this VFS for the dirent data */

  	error = MVFS_ROOT(vfsp, &vp);
	if (error) break;
	ASSERT(MFS_ISVOBRT(VTOM(vp)));

        /* Fill in the dirent struct buffer we have allocated */

	KDIRENT_INIT(dirent, 
			VTOM(vp)->mn_hdr.fid.mf_dbid, 	    /* Inum for the dir */
			VFS_TO_MMI(vfsp)->mmi_vobtag,	    /* Vob tag-name */
			nmlen, 				    /* Length of the name */
			offset+reclen);			    /* Next offset */

	/* Done with the root vnode */

	VN_RELE(vp);

  	/* See if room in user buffer */

	if ((int)reclen > uiop->uio_resid) {
	    /* Error if no entries returned yet */
	    if (oresid == uiop->uio_resid) {
		error = EINVAL;
		break;
	    }
	    break;
	}

	/* 
	 * Unlock the VFS list while UIOMOVE the dirent to the user.
	 * We don't want to have the VFS list locked in case the VM
	 * system has to do something on the uiomove etc. below,
         * that would bump into this lock.
	 */

	mfs_findvfs_unlock();
	error = READDIR_UIOMOVE((caddr_t)dirent, &reclen, UIO_READ, uiop, offset);
	mfs_findvfs_lock();
        if (error != 0 || READDIR_BUF_FULL(uiop))
            break;

        /* Update current offset */

	offset += reclen;
    }
    mfs_findvfs_unlock();		/* Unlock vfs list on exit from loop */
    if (!READDIR_BUF_FULL(uiop))
        MVFS_UIO_OFFSET(uiop) = offset;
  
exit_no_lock: 
    KMEM_FREE(dirent, direntlen); 
    return(error);
}

static const char vnode_verid_mvfs_ntvwops_c[] = "$Id:  e7f2a8f4.637911da.8655.00:01:83:a6:4c:63 $";
