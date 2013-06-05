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
#ifndef MFS_MOUNT_H_
#define MFS_MOUNT_H_
/* 
 * Description:
 *	This header file defines the user space structures and constants
 *	for the MFS mount call.
 */
#include "linux/types.h"
#include "linux/param.h"
#include "mfs_ioctl.h"
#define MNTTYPE_MVFS	"mvfs"		/* Atria multi-version file system */

/*
 * FIXME: temporary canned name of view root.  Change when
 * we figure out how to get /etc/mtab right.
 */

#define MFS_VIEWROOTPATH	"/view"

/*
 * Define mount options unique to Atria.  In addition, the MFS supports
 * the common local and nfs mount options: ro, rw, soft, hard, 
 * intr, nointr, noac, timeo, retrans, noac, acregmin, acregmax, 
 * acdirmin, acdirmax
 */

#define MFSOPT_VIEWROOT	"viewroot"  /* Identifies "view root" special mount */
#define MFSOPT_NODNLC	"nodnlc"    /* Do not use dir name lookup cache */
#define MFSOPT_XNSUFFIX "xnsuffix"  /* eXtended Naming suffix */
#define MFSOPT_UUID     "uuid"      /* Vob replica uuid; Don't read from stg path */
#define MFSOPT_EXPORTID	"exportid"  /* Vob export minor device ID */
#define MFSOPT_EXPORTIDEQ "exportid="

#define MVFS_MAX_VIEW_EXPORTID	4096
#define MVFS_MAX_VOB_EXPORTID	512
#define MFSOPT_NOPOOLS	"no_rgy_pools"	/* Don't use registry's pool map */
#define MFSOPT_POOLMAP	"poolmap"	/* Pool map info; string option */
#define MFSOPT_RDONLY   "ro"            /* read only VOB mount */

#define MFSOPT_POOLMAP_SEPARATORS "|"	/* list of separator characters */

/*
 * Define the mount call argument structure
 */

#define MFSMNT_VERSION		12	/* Mount arg structure version */

#define MFSMNT_VOB		0x0001	/* VOB mount flag */
#define MFSMNT_VIEWROOT		0x0002	/* View root mount flag */
#define MFSMNT_SOFT		0x0004	/* Soft mount option */
#define MFSMNT_NOINTR		0x0008	/* No intr option */
#define MFSMNT_NOAC		0x0010	/* No attr cache option */
#define MFSMNT_NODNLC		0x0020	/* No dir lookup cache option */
#define MFSMNT_RDONLY		0x0040	/* Read only VOB mount */

/*
 * Define default mount option values
 */

#define MFSMNT_TIMEO_DEFAULT	50	/* RPC timeout in 1/10'th seconds */
#define MFSMNT_RETRANS_DEFAULT	7	/* Number of RPC retries */

struct mvfs_splitpool {
    struct mfs_strbuf	msp_prefix;	/* prefix to match */
    mfs_strbufpn_pair_t	msp_target;	/* replacement for vobstg+prefix */
};

/*
 * Notes:
 *     VOB mounts:
 *	    mma_hmsuffix, mma_addr are not used.
 *     Viewroot mounts:
 *	    mma_vob_oid is not used.
 *	    mma_spath, mma_rpath, mma_uuid are not used.
 *	    Other server information is for local ALBD
 */

struct mfs_mntargs {
        u_short		 	mma_mntvers;	/* Mount args version */
        u_short                 mma_mntsize;    /* size of structure (needed for 32/64 detection) */
	u_long		 	mma_flags;	/* Mount flags */
	struct mfs_strbuf	mma_mntpath;	/* Mnt dir path */
	struct mfs_strbuf	mma_host;	/* Svr host name */
	u_short			mma_port;	/* ALBD port on the local host (in_port_t) */
	mfs_strbufpn_pair_t	mma_spath;	/* Svr storage path */
	struct mfs_strbuf	mma_rpath;	/* Svr storage remote path */
	tbs_oid_t	 	mma_vob_oid; 	/* VOB oid */
	u_long			mma_timeo;	/* Timeout in .1 secs */
	u_long			mma_retries;	/* Max retries */
	u_long		 	mma_ac_regmax;	/* Attr cache reg file max */
	u_long		 	mma_ac_regmin; 	/* Attr cache reg file min */
	u_long		 	mma_ac_dirmax;	/* Attr cache dir max */
	u_long		 	mma_ac_dirmin; 	/* Attr cache dir min */
	struct mfs_strbuf	mma_hmsuffix;	/* History mode suffix */
	tbs_uuid_t		mma_replica_uuid; /* Svr replica uuid */
	int			mma_sptab_cnt;  /* count of split pool ents */
	struct mvfs_splitpool	*mma_sptab_ents; /* split pool entries */
        u_int		 	mma_vobminor;	/* requested VOB minor devno */
        mvfs_cache_sizes_t      mma_sizes;      /* cache sizes, see mfs_ioctl.h */

};

#endif /* MFS_MOUNT_H */
/* $Id: 21e255a3.0a6b11e1.8d67.00:01:83:0a:3b:75 $ */
