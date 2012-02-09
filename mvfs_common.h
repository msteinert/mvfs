/* * (C) Copyright IBM Corporation 2006, 2007. */
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

#ifndef MVFS_COMMON_H_
#define MVFS_COMMON_H_

#include "mvfs_base.h"

typedef struct mvfs_common_data 
{
/* tunables and relevant data */
    int mvfs_dncenabled;
    int mvfs_dncnoentenabled;
    int mvfs_rvcenabled;
    int mvfs_acenabled;
    int mvfs_ctoenabled;
    int mvfs_rlenabled;
    int mvfs_rebind_dir_enable;
    int mvfs_vlinkcnt2;

    int mvfs_ctxt_atime_refresh;
    int mvfs_dncdirmax;
    int mvfs_dncregmax;
    int mvfs_dncnoentmax;
    int mvfs_mnmax;
    MVFS_WATERMARK_TYPE mvfs_cvpfreemax;
    MVFS_WATERMARK_TYPE mvfs_vobfreemax;
    MVFS_WATERMARK_TYPE mvfs_cvpfreemin;
    MVFS_WATERMARK_TYPE mvfs_vobfreemin;
    int mvfs_threadhash_sz;
    int mvfs_rddir_blocks;
    int mvfs_client_cache_size;
    int mvfs_largeinit;

    ks_int32_t mvfs_computed_largeinit;
    mvfs_cache_sizes_t mvfs_init_sizes; /* Initial sizes before mount */

/* Consolidated data for various MVFS subsystems */
    mvfs_rpc_data_t mvfs_rpc;           /* RPC client handle cache */
    mvfs_proc_thread_data_t proc_thr;   /* MVFS proc/thread state structs */
    mvfs_credlist_data_t cred;          /* System-wide credlist */
} mvfs_common_data_t;

EXTERN int mvfs_copy_tunable(mvfs_common_data_t *mcdp);
EXTERN void mfs_periodic_maintenance(P_NONE);
#endif /* MVFS_COMMON_H_ */
/* $Id: 2b4388bc.66bb11dc.9bbb.00:01:83:09:5e:0d $ */
