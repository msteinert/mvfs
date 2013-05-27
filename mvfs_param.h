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
#ifndef MVFS_PARAM_H_
#define MVFS_PARAM_H_
/*
 *
 * Description:
 *	Header file for MVFS parameters which can be modified for 
 *	kernel rebuilds.
 *
 * WARNING:
 *	If you add something to this header file you must update ALL
 *	the sysgen descriptors.
 *
 */

/*
 * Version information
 */

EXTERN char *mvfs_version;	/* Version and copyright string */
EXTERN char *mvfs_sccsID;	/* SCCS what string */
EXTERN char *mvfs_rcsID;		/* RCS ident string */

#define MVFS_PARAM_TYPE int
#define PARAM_TYPE MVFS_PARAM_TYPE

/* 
 * Flags which are global enables for caches and functionality in the MVFS
 * Many of these enables can also be controlled on a per-mount point
 * basis with mount options (e.g. name cache, attr cache).
 *
 * mvfs_dncenabled:	enables name caching in the MVFS
 * mvfs_dncnoentenabled: requires that mvfs_dncenabled == 1 to have effect.
 *			enables caching of ENOENT returns in the MVFS name
 *			cache.
 * mvfs_rvcenabled:	enables "root version cache" e.g. a cache of
 *			the version to select for root of the VOB.
 * mvfs_acenabled:	enables caching of attributes in mnodes
 * mvfs_ctoenabled:	enables "close to open" consistency.  Forces
 *			every open to fetch the current attributes over
 *			the wire (i.e. overrides the attribute cache).
 * mvfs_rlenabled:	enables the "readlink text" cache.
 * mvfs_rebind_dir_enable:
 *                      enables "rebinding" (i.e. automatic "cd" command)
 *			of a user's current working dir to a new one which
 *			has become the appropriate one selected by his
 *			configuration spec (e.g. by checkout/checkin of
 *			the cwd by this user or another user).
 * mvfs_vlinkcnt2:      Controls the link count returned by a stat on /view
 *                      or on /view/<viewtag>.  The default is to return the
 *                      actual link count for /view and the root link count
 *                      for /view/<viewtag>.  Set to 1, both cases will return
 *                      a link count of 2 which was the system behaviour 
 *                      before ClearCase v3.2.1.
 */

EXTERN PARAM_TYPE mvfs_dncenabled;
EXTERN PARAM_TYPE mvfs_dncnoentenabled;
EXTERN PARAM_TYPE mvfs_rvcenabled;
EXTERN PARAM_TYPE mvfs_acenabled;
EXTERN PARAM_TYPE mvfs_ctoenabled;
EXTERN PARAM_TYPE mvfs_rlenabled;
EXTERN PARAM_TYPE mvfs_rebind_dir_enable;
EXTERN PARAM_TYPE mvfs_vlinkcnt2;

/*
 * MNODE parameters
 *
 * These parameters normally are set to 0 which allows the system
 * to select a reasonable default.
 *
 * mvfs_largeinit:	when non-zero, selects larger set of defaults
 * mvfs_mnmax:		max number of mnodes to allow system-wide (both
 *			active an on freelist)
 * mvfs_vobfreemax:	max number of mnodes to cache on the freelist
 * mvfs_vobfreemin:	size to reduce vob freelist to when cache has grown 
 *			too large
 * mvfs_cvpfreemax:	max number of "cleartext" vnodes to hold active
 *			in mnodes cached on the freelist
 * mvfs_cvpfreemin:	size to reduce number of active "cleartext" vnodes to
 *			when cache has grown too large
 * mvfs_vobhashsize:    hash size for vob type mnodes.
 * mvfs_cvphashsize:    hash size for cvp type mnodes.
 * mvfs_otherhashsize:  hash size for all other mnodes.
 * mvfs_vobfreehashsize:hash size for vob free list
 *
 * Defaults:		  mvfs_largeinit == 0	 mvfs_largeinit != 0
 * mvfs_mnmax		  4096			 4096*(mvfs_largeinit+1) for
 *						 mvfs_largeinit <= 4, above that
 *						 2048*(mvfs_largeinit+7)
 * mvfs_vobfreemax	  22% of mvfs_mnmax	 22% of mvfs_mnmax
 * mvfs_vobfreemin	  90% of mvfs_vobfreemax 90% of mvfs_vobfreemax
 * mvfs_cvpfreemax (UNIX) 20% of non-mvfs inodes 50% of non-mvfs inodes
 * mvfs_cvpfreemin 	  mvfs_cvpfreemax-(32*(mvfs_largeinit+1))
 * mvfs_vobhashsize	  mvfs_mnmax/4 and must be power of 2
 *                        must be 2^n (512, 1024, 2048, etc.)
 * mvfs_cvphashsize	  mvfs_vobhashsize/4 [typically 128]
 *                        must be 2^n (128, 256, 512, 1024, etc.)
 * mvfs_otherhashsize	  mvfs_vobhashsize/8 [typically 64]
 *                        must be 2^n (64, 128, 256, 512, etc.)
 * mvfs_vobfreehashsize	  mvfs_vobhashsize/16 [typically 32]
 *                        must be 2^n (32, 64, 128, 256, etc.)
 * 
 *
 * Settings for mvfs_vobfreemax consume system memory resources.  Each
 * mode takes about 450 bytes of storage, so 900 is a bit more than 400K bytes,
 * and 1800 is a bit more than 800K.
 * Settings for mvfs_cvpfreemax control how many non-MVFS vnodes can be 
 * can be "held" by the MVFS as cached "cleartext" files.  If this
 * value does not leave enough free inodes, the system will report
 * "inode table overflow" and/or may hang.
 */

EXTERN PARAM_TYPE mvfs_largeinit;
EXTERN PARAM_TYPE mvfs_mnmax;
EXTERN u_long mvfs_cvpfreecnt;

#define MVFS_WATERMARK_TYPE u_long
#define MVFS_DEF_CVPFREEMAX_LARGE(_mcdp)  (NINODE(_mcdp) / 2) /* 50% of non-mvfs inodes */
#define MVFS_DEF_CVPFREEMAX_SMALL(_mcdp)  (NINODE(_mcdp) / 5) /* 20% of non-mvfs inodes */

EXTERN MVFS_WATERMARK_TYPE mvfs_vobfreemax;
EXTERN MVFS_WATERMARK_TYPE mvfs_vobfreemin;
EXTERN MVFS_WATERMARK_TYPE mvfs_cvpfreemax;
EXTERN MVFS_WATERMARK_TYPE mvfs_cvpfreemin;

/*
 * Name Cache parameters
 *
 * The name cache is partitioned into different areas based on the
 * result of the name translation. 
 * The three areas are:
 *	Target of name is a dir
 *	Target of name is a non-dir (file, fifo, char dev etc.)
 *	Target of name is a ENOENT return.
 *
 * This keeps dirs resident for a long time (ls/search of large dirs
 * will not thrash out all the dir cache entries), and makes both
 * dirs and files immune to thrashing from ENOENT cached entries
 * (which would happen all the time due to search rules).
 *
 * Each entry takes about 130 bytes of storage.
 */

EXTERN PARAM_TYPE mvfs_dncdirmax;
EXTERN PARAM_TYPE mvfs_dncregmax;
EXTERN PARAM_TYPE mvfs_dncnoentmax;

/*
 * Miscellaneous parameters
 *
 * mvfs_max_rpcdelay:		time (in seconds) the max successful rpc
 *				should take.  RPC's longer than this are
 *				logged to the console as excessively slow.
 * mvfs_idleview_timeout:	time (in seconds) before scrubbing unused
 *				dynamically created views in '/view' dir.
 * mvfs_view_rebind_timeout:    view idle time (in seconds) after which a view
 *                              server's port number should be re-checked with
 *                              the host's ALBD
 * mvfs_auditbufsiz:		buffer size to use for buffering audit records
 * mvfs_duplsearchmax:		max number of entries to search backwards
 *				in audit buffer to try and eliminate duplicate
 *				audit records (uses CPU time to make audit
 *				file smaller).
 * mvfs_cowbufsiz:		buffer size to use for Copy-on-write operations
 * mvfs_client_cache_size:	number of RPC client handles to cache
 * mvfs_rddir_blocks:           number of return blocks to cache with each
 *                              mnode from readdir requests.
 * mvfs_threadhashsize:         UNIX: Threadid hash size, see system specific 
 *                              file mvfs_param.c for allowed values.
 * mvfs_ctxt_atime_refresh:     UNIX: Time (in seconds) after which an 
 *                              operation which opens a VOB cleartext container 
 *                              will force access time of that file to be 
 *                              updated (keeps scrubber informed of access 
 *                              regardless of cleartext filesystem caching)  
 */

EXTERN PARAM_TYPE mvfs_max_rpcdelay;
EXTERN PARAM_TYPE mvfs_idleview_timeout;
EXTERN PARAM_TYPE mvfs_view_rebind_timeout;
EXTERN PARAM_TYPE mvfs_auditbufsiz;
EXTERN PARAM_TYPE mvfs_duplsearchmax;
EXTERN PARAM_TYPE mvfs_cowbufsiz;
EXTERN PARAM_TYPE mvfs_client_cache_size;
EXTERN PARAM_TYPE mvfs_rddir_blocks;
EXTERN PARAM_TYPE mvfs_threadhash_sz;
EXTERN PARAM_TYPE mvfs_ctxt_atime_refresh;

#undef PARAM_TYPE
/* leave MVFS_PARAM_TYPE visible for other users */

#endif	/* MVFS_PARAM_H_ */
/* $Id: 1ab3a2cf.a23a11df.8bc7.00:01:84:7a:f2:e4 $ */
