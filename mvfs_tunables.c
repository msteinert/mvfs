/* * (C) Copyright IBM Corporation 2001, 2007. */
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
/* mvfs_tunables.c */
#include "mvfs_systm.h"
#include "mvfs.h"
#include "mvfs_when.h"

/* 
 * Flags which are global enables for caches and functionality in the MFS
 * Many of these enables can also be controlled on a per-mount point
 * basis with mount options (e.g. name cache, attr cache).
 *
 * mvfs_dncenabled:	enables name caching in the MFS
 * mvfs_dncnoentenabled: requires that mvfs_dncenabled == 1 to have effect.
 *			enables caching of ENOENT returns in the MFS name
 *			cache.
 * mvfs_rvcenabled:	enables "root version cache" e.g. a cache of
 *			the version to select for root of the VOB.
 * mvfs_acenabled:	enables caching of attributes in mnodes
 * mvfs_ctoenabled:	enables "close to open" consistency.  Forces
 *			every open to fetch the current attributes over
 *			the wire (i.e. overrides the attribute cache).
 * mvfs_rlenabled:	enables the "readlink text" cache.
 * mvfs_rebind_dir_enable: enables "rebinding" (i.e. automatic "cd" command)
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

int mvfs_dncenabled = 1;
int mvfs_dncnoentenabled = 1;
int mvfs_rvcenabled = 1;
int mvfs_acenabled = 1;
int mvfs_ctoenabled = 1;
int mvfs_rlenabled = 1;
int mvfs_rebind_dir_enable = 1;
int mvfs_vlinkcnt2 = 0;

/*
 * MNODE parameters
 *
 * These system tunable parameters normally are set to 0 which allows the 
 * system to select a reasonable default.
 *
 * mvfs_largeinit:	when non-zero, selects larger set of defaults
 *			also affects name cache size (see below)
 * mvfs_mnmax:		max number of mnodes to allow system-wide (both
 *			active an on freelist)
 * mvfs_vobfreemax:	max number of mnodes to cache on the freelist
 * mvfs_vobfreemin:	size to reduce vob freelist to when cache has grown
 *			too large
 * mvfs_cvpfreemax:	max number of "cleartext" vnodes to hold active
 *			in mnodes cached on the freelist
 * mvfs_cvpfreemin:	size to reduce number of active "cleartext" vnodes to
 *			when cache has grown to large
 *
 * Defaults:		mvfs_largeinit == 0	mvfs_largeinit != 0
 * mvfs_mnmax		4096			4096*(mvfs_largeinit+1) for
 *						mvfs_largeinit <= 4, above that
 *						2048*(mvfs_largeinit+7)
 * mvfs_vobfreemax	22% of mvfs_mnmax	22% of mvfs_mnmax
 * mvfs_vobfreemin	90% of mvfs_vobfreemax	90% of mvfs_vobfreemax
 * mvfs_cvpfreemax	20% of non-mfs inodes	50% of non-mfs inodes
 * mvfs_cvpfreemin	mvfs_cvpfreemax-32	mvfs_cvpfreemax-32
 *
 * Settings for mvfs_vobfreemax consume system memory resources.  Each
 * mode takes about 450 bytes of storage, so 400K bytes is about 880,
 * and 800 Kbytes is about 1760.
 * Settings for mvfs_cvpfreemax control how many non-MFS vnodes can be 
 * can be "held" by the MFS as cached "cleartext" files.  If this
 * value does not leave enough free inodes, the system will report
 * "inode table overflow" and/or may hang.
 */

int mvfs_largeinit = 0;
int mvfs_mnmax = 0;
MVFS_WATERMARK_TYPE mvfs_vobfreemax = 0;
MVFS_WATERMARK_TYPE mvfs_vobfreemin = 0;
MVFS_WATERMARK_TYPE mvfs_cvpfreemax = 0;
MVFS_WATERMARK_TYPE mvfs_cvpfreemin = 0;

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
 * Each entry takes about 130 bytes of storage.  Default init
 * takes about 180K, and largeinit takes about 360K
 *
 * If mvfs_largeinit is set, then if the values below are less than
 * the "largeinit" default, then the system will increase the value
 * to the largeinit default.
 * 
 * All parameters are now defaulted to 0 which means they will be 
 * controlled by the largeinit default.  Setting any of these 
 * parameters to non-zero values overrides the largeinit default 
 * value for that parameter.
 * 
 * Largeinit = 0 defaults: (previous initialized values)
 *    mvfs_dncdirmax = 200;
 *    mvfs_dncregmax = 800;
 *    mvfs_dncnoentmax = 400;
 * 
 * Largeinit = 1 defaults:
 *    mvfs_dncdirmax:	 400
 *    mvfs_dncregmax:	1600
 *    mvfs_dncnoentmax:	 800
 * 
 */

int mvfs_dncdirmax = 0;
int mvfs_dncregmax = 0;
int mvfs_dncnoentmax = 0;

/*
 * Other tunables:
 * mvfs_client_cache_size:	number of RPC client handles to cache
 *   for smaller systems with mvfs_largeinit <= 2
 *      default: (mvfs_largeinit + 1) * 5
 *   for larger systems:
 *	default: (mvfs_largeinit + 1) * 10
 *   up to a maximum of 240
 *
 * mvfs_rddir_blocks:           number of return blocks to cache with each
 *                              mnode from readdir requests.
 *      default: 2*(mvfs_largeinit+1); up to a maxium of 6
 *
 */

int mvfs_client_cache_size = 0;
int mvfs_rddir_blocks = 0;

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
 * mvfs_threadhash_sz:          threadid hash size (see mdep headers/hash
 *                              functions for size criteria)
 * mvfs_ctxt_atime_refresh:     Time (in seconds) after which an operation
 *                              which opens a VOB cleartext container will force
 *                              access time of that file to be updated (keeps
 *                              scrubber informed of access regardless of
 *                              cleartext filesystem caching)
 */

int mvfs_max_rpcdelay = 30;
int mvfs_idleview_timeout = 300;
int mvfs_view_rebind_timeout = 60 * 60 * 2; /* 2 hours */
#ifdef ATRIA_LP64
int mvfs_auditbufsiz = 8192;
int mvfs_cowbufsiz = 8192;
#else
int mvfs_auditbufsiz = 4096;
int mvfs_cowbufsiz = 4096;
#endif
int mvfs_duplsearchmax = 20;
int mvfs_threadhash_sz = MVFS_THREADHASH_SZ_DEFAULT;
int mvfs_ctxt_atime_refresh = 3600; 

/*
 * Version string for MFS
 */
char *mvfs_version = MVFS_VERSION_STRING;
char mvfs_sccsIDstr[]  = MVFS_SCCSID_STRING;
char *mvfs_sccsID = mvfs_sccsIDstr;
char *mvfs_rcsID   = MVFS_RCSID_STRING;

mvfs_common_data_t mvfs_common_data_var;

/* 
 * Copy the initial values from global variables.
 */
int mvfs_copy_tunable(mvfs_common_data_t *mcdp)
{
    mcdp->mvfs_largeinit = mvfs_largeinit;

    mcdp->mvfs_mnmax = mvfs_mnmax;
    mcdp->mvfs_vobfreemax = mvfs_vobfreemax;
    mcdp->mvfs_vobfreemin = mvfs_vobfreemin;
    mcdp->mvfs_cvpfreemax = mvfs_cvpfreemax;
    mcdp->mvfs_cvpfreemin = mvfs_cvpfreemin;

    mcdp->mvfs_dncdirmax = mvfs_dncdirmax;
    mcdp->mvfs_dncregmax = mvfs_dncregmax;
    mcdp->mvfs_dncnoentmax = mvfs_dncnoentmax;

    mcdp->mvfs_rddir_blocks = mvfs_rddir_blocks;
    mcdp->mvfs_client_cache_size = mvfs_client_cache_size;
    mcdp->mvfs_ctxt_atime_refresh = mvfs_ctxt_atime_refresh;
    mcdp->mvfs_threadhash_sz = mvfs_threadhash_sz;

    mcdp->mvfs_dncenabled = mvfs_dncenabled;
    mcdp->mvfs_dncnoentenabled = mvfs_dncnoentenabled;
    mcdp->mvfs_rvcenabled = mvfs_rvcenabled;
    mcdp->mvfs_acenabled = mvfs_acenabled;
    mcdp->mvfs_ctoenabled = mvfs_ctoenabled;
    mcdp->mvfs_rlenabled = mvfs_rlenabled;
    mcdp->mvfs_rebind_dir_enable = mvfs_rebind_dir_enable;
    mcdp->mvfs_vlinkcnt2 = mvfs_vlinkcnt2;

    return 0;
}
static const char vnode_verid_mvfs_tunables_c[] = "$Id:  3b655ae4.c20911db.897a.00:01:83:a6:4c:63 $";
