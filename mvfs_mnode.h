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
#ifndef MVFS_MNODE_H_
#define MVFS_MNODE_H_

#include "mvfs_base.h"

/* All mnode subsystem data is now contained in this structure. */
typedef struct mvfs_mnode_data 
{

    /*
     * The mnum_to_mnode table has a pointer to every mnode allocated in the 
     * system.  Mnodes are allocated as needed from memory.  The mfs_mnlock
     * protects access to the mnum_to_mnode table and the associated
     * counts and indexes.
     * See mvfs_mnode.c for a full description of mnode locking. 
     */
    LOCK_T mfs_mnlock;	 	 /* Monitor for tables and arrays */
    mfs_mnode_t **mnum_to_mnode; /* Array maps from mnum to mnode */
    int mfs_mncnt;		 /* Current mnode cnt */
    long mvfs_mtmhwm;		 /* Index to the highest mnode allocated in
    				  * the mnum_to_mnode table */
    long mvfs_mtmpfs;		 /* Possible free slot.  Index of first possible
    				  * free slot in the mum_to_mnode table */
    u_long mfs_mngen;		 /* Mnode activation sequencer */
    u_long mfs_attrgen;		 /* Attribute 'generation' for builds */
    u_long mvfs_attgenrollover;	 /* Rollover stat for attribute generation # */
    u_long mfs_growrestart;	 /* Restart stat after growtable in mfs_mnget */
    int mvfs_sync_in_progress;	 /* Flag, 1 indicates a sync is in progress */

    /*
     * Hash lists for mnodes.  All mnodes are found on 1 of 3 hash tables.
     * See mvfs_mnode.c for a full description of the hash tables and locking of
     * these structures.

     * XXX note mvfs_vobhashsize, cvphashsize, and otherhashsize no longer 
     * initialized here to MIN values; Temp done in mninit.  Good?  
     */
    int mvfs_vobhashsize;	 /* VOB hash table size */
    int mvfs_cvphashsize;	 /* CVP hash table size */
    int mvfs_otherhashsize;	 /* "Other" hash table size */

    /* 
     * Pointer to each of the 3 mnode hash tables.  
     *
     * XXX note mvfs_vobhash, cvphash, and otherhash no longer initialized 
     * when declared to NULL; done at mninit time, cover all tuning cases? 
     */
    mfs_mnhash_slot_t *mvfs_vobhash;
    mfs_mnhash_slot_t *mvfs_cvphash;
    mfs_mnhash_slot_t *mvfs_otherhash;

    /* 
     * Hash table locks and macros.  We use the standard MVFS lock pools.
     * Each lock in the mvfs_lock pool protects the hash chain links in the 
     * mnode header.
     */
    mvfs_lock_pool_t	mvfs_vobhash_mlp;	/* MVFS lock pool for vobhash */
    mvfs_lock_pool_t	mvfs_cvphash_mlp;	/* MVFS lock pool for cvphash */
    mvfs_lock_pool_t	mvfs_otherhash_mlp;	/* MVFS lock pool for otherhash */

    /* 
     * VOB freelist.  VOB mnodes are cached on the freelist when they are
     * freed up if they still have the associated view.  See mvfs_mnode.c for a
     * full description of the vob freelist and the cvp freelist.  Note the
     * vob freelist is really a hashed vob freelist; every mnode on the 
     * vob freelist is really on one of n hash lists.
     */

    /* XXX note: no longer initialized to NULL here, done in mninit.OK?  */
    int mvfs_vobfreehashsize;		/* VOB freelist hash size */

    /* 
     * Pointer VOB freelist hash.
     */
    /* XXX note this is no longer initialized to NULL here, in mninit: OK? */
    mvfs_vobfreehash_slot_t *mvfs_vobfreehash;

    /* 
     * Vobfree hash table lock.  Uses the standard MVFS lock pools.
     * Each lock in the mvfs_lock pool protects the hash chain links in the 
     * mnode header.
     */
    mvfs_lock_pool_t	mvfs_vobfreehash_mlp;	/* MVFS lock pool for vobfreehash */

    /*
     * Global freelist lock is used only for counters and on-line changes to the
     * high and low water mark parameters.
     */
    LOCK_T	mvfs_vobfreelock;

    u_long mvfs_vobfreecnt;	   /* Count of mnodes on freelist */
    u_long mvfs_cvpfreecnt;	   /* Count of "cached" held vnodes in freelist */
/* XXX no longer initialized to 0 here -- in mninit, OK? */
    int mvfs_mnfreelist_mgmt_ip;   /* Flag: freelist reduction in progress */

    /* Cleartext aging */
    time_t mvfs_nt_age_cvp_time;   /* Time to age cleartext */

    /* 
     * The mnode destroy list.  See mvfs_mnode.c for full details on this LRU.
     */

    LOCK_T mvfs_mndestroylock;	  /* Control access to mvfs_mndestroy list & cnt*/

    mvfs_mndestroylist_t  mvfs_mndestroylist;
    u_long mvfs_mndestroycnt;	  /* Count of entries on mndestroy list */

    /*
     * Recommended buffer size used for sync'ing mnodes.  This size starts at
     * MVFS_FLS_BUFSZ_FACTOR (10) times the size of the expected average hash 
     * chain length.  It is automatically doubled whenever we detect it is too
     * small, up to a maximum of mvfs_mnmax.
     */
    long mvfs_rec_fls_bufsz;	  /* Size of buffer to use for mnplist in sync */
    				  /* Default buffer for mnplist in sync */
    mfs_mnode_t ** mvfs_default_mnplist;  
    u_long mvfs_mnplist_toosmall; /* # of times needed mnplist was too small for
				   * a hash chain during sync. */

} mvfs_mnode_data_t;

#endif /* MVFS_MNODE_H_ */
/* $Id: c9d14d3e.9c1e11dd.9a62.00:01:83:29:c0:fc $ */
