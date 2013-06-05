/* * (C) Copyright IBM Corporation 1992, 2012. */
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
/* tbs_errno.c */

/*LINTLIBRARY*/

#include "mvfs_systm.h"
#include <linux/types.h>
#include <linux/errno.h>
#include <linux/time.h>
#include "tbs_errno.h"

#include <tbs_base.h>

/* This routine accepts either a status code or Unix errno.
 *
 * When called with a status code: convert the status code to a Unix errno, 
 * it should never be called with any of the status codes that are greater
 * than TBS_STLIMIT. If it is, it will be converted to EIO.
 *
 * When called with a Unix errno: if errno is < TBS_STBASE then just pass 
 * it thru, otherwise convert it to EIO.
 */

int
tbs_status2errno(status)
     tbs_status_t status;
{
    switch (status) {
      case TBS_ST_EPERM:
	return (EPERM);		/* Not owner */
      case TBS_ST_VIEW_NO_CFS_SET:	/* No config spec set in view */
      case TBS_ST_VIEW_NO_VER:	/* No version specified in CFS */
      case TBS_ST_ENOENT:
	return (ENOENT);	/* No such file or directory */
      case TBS_ST_VIEW_NEEDS_REFORMAT:
      case TBS_ST_VOB_NEEDS_REFORMAT:
      case TBS_ST_CONFIG_SPEC_ERR:
      case TBS_ST_DB_TIMEOUT:
      case TBS_ST_EIO:
	return (EIO);		/* I/O error */
      case TBS_ST_ENXIO:
	return (ENXIO);		/* No such device or address */
      case TBS_ST_ENOMEM:
      case TBS_ST_SM_ENOMEM:
	return (ENOMEM);	/* Not enough core */
      case TBS_ST_EACCES:
	return (EACCES);	/* Permission denied */
      case TBS_ST_EEXIST:
	return (EEXIST);	/* File exists */
      case TBS_ST_EXDEV:
	return (EXDEV);		/* Cross-device link */
      case TBS_ST_ENODEV:
	return (ENODEV);	/* No such device */
      case TBS_ST_ENOTDIR:
	return (ENOTDIR);	/* Not a directory */
      case TBS_ST_EISDIR:
	return (EISDIR);	/* Is a directory */
      case TBS_ST_EINVAL:
      case TBS_ST_SM_EINVAL:
	return (EINVAL);	/* Invalid argument */
      case TBS_ST_EFBIG:
	return (EFBIG);		/* File too large */
      case TBS_ST_ENOSPC:
      case TBS_ST_SM_DB_ENOSPC:
	return (ENOSPC);	/* No space left on device */
      case TBS_ST_EROFS:
      case TBS_ST_DB_AREA_LOCKED:
      case TBS_ST_VIEW_NEEDS_RECOVERY:
	return (EROFS);		/* Read-only file system */
      case TBS_ST_EMLINK:
	return (EMLINK);	/* Too many links */
      case TBS_ST_ELOOP:
	return (ELOOP);		/* Too many levels of symbolic links */
      case TBS_ST_ENAMETOOLONG:
	return (ENAMETOOLONG);	/* File name too long */
      case TBS_ST_ENOTEMPTY:
	return (ENOTEMPTY);	/* Directory not empty */
      case TBS_ST_EDQUOT:
	return (EDQUOT);	/* Disc quota exceeded */
      case TBS_ST_WRONG_VOB:	/* The vob oid is wrong, vob recreated? */
      case TBS_ST_VIEW_STALE_DIR:/* Directory not select in CFS */
      case TBS_ST_VIEW_UNKNOWN_VOB:
      case TBS_ST_ESTALE:
	return (ESTALE);	/* Stale VIEW file handle */
      case TBS_ST_EBUSY:
	return (EBUSY);		/* Device/MVFS busy */
      case TBS_ST_EPFNOSUPPORT:
        return (EPFNOSUPPORT);  /* Protocol family not supported */
#ifdef ECANCELED /* if not defined, use default below */
      case TBS_ST_ABORT:
        return (ECANCELED);
#endif
      case -1:			/* in case rpc status makes its way here */
	return(EIO);
      case 0:
        return(status);
      default:
        if (status < TBS_STBASE)
            return (int) status;
        return (EIO);
    }
    
}

tbs_status_t
tbs_errno2status(st_errno)
     int st_errno;
{
    switch (st_errno) {
      case EPERM:
	return (TBS_ST_EPERM);		/* Not owner */
      case ENOENT:
	return (TBS_ST_ENOENT);		/* No such file or directory */
      case EIO:
	return (TBS_ST_EIO);		/* I/O error */
      case ENXIO:
	return (TBS_ST_ENXIO);		/* No such device or address */
      case ENOMEM:
	return (TBS_ST_ENOMEM);		/* Not enough core */
      case EACCES:
	return (TBS_ST_EACCES);		/* Permission denied */
      case EEXIST:
	return (TBS_ST_EEXIST);		/* File exists */
      case EXDEV:
	return (TBS_ST_EXDEV);		/* Cross-device link */
      case ENODEV:
	return (TBS_ST_ENODEV);		/* No such device */
      case ENOTDIR:
	return (TBS_ST_ENOTDIR);	/* Not a directory */
      case EISDIR:
	return (TBS_ST_EISDIR);		/* Is a directory */
      case EINVAL:
	return (TBS_ST_EINVAL);		/* Invalid argument */
      case EFBIG:
	return (TBS_ST_EFBIG);		/* File too large */
      case ENOSPC:
	return (TBS_ST_ENOSPC);		/* No space left on device */
      case EROFS:
	return (TBS_ST_EROFS);		/* Read-only file system */
      case EMLINK:
	return (TBS_ST_EMLINK);		/* Too many links */
      case ELOOP:
	return (TBS_ST_ELOOP);		/* Too many levels of symbolic links */
      case ENAMETOOLONG:
	return (TBS_ST_ENAMETOOLONG);	/* File name too long */
#if ENOTEMPTY != EEXIST
      case ENOTEMPTY:
	return (TBS_ST_ENOTEMPTY);	/* Directory not empty */
#endif
      case EDQUOT:
	return (TBS_ST_EDQUOT);		/* Disc quota exceeded */
      case ESTALE:
	return (TBS_ST_ESTALE);		/* Stale VIEW file handle */
      case EBUSY:
	return (TBS_ST_EBUSY);		/* Device/MVFS busy */
      case EPFNOSUPPORT:
        return (TBS_ST_EPFNOSUPPORT);   /* Protocol family not supported */
#ifdef ECANCELED /* if not defined, use default below */
      case ECANCELED:
        return (TBS_ST_ABORT);
#endif
      default:
	return (TBS_ST_ERR);
    }
    
}

static const char vnode_verid_tbs_errno_c[] = "$Id:  99100000.e3b811ca.a343.00:01:55:00:0f:df $";
