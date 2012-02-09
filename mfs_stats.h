/* * (C) Copyright IBM Corporation 1994, 2006. */
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

#ifndef MFS_STATS_H_
#define MFS_STATS_H_

#include <view_rpc_kernel.h>            /* needed for VIEW_NUM_PROCS */

/* 
 * Vnode operation statistics.
 * These definitions are also used for enabling "logging"
 */

#define	MFS_VOPEN	 0	/* open		0x00000001 */
#define	MFS_VCLOSE	 1	/* close 	0x00000002 */
#define	MFS_VRDWR	 2	/* rdwr 	0x00000004 */
#define	MFS_VIOCTL	 3	/* ioctl 	0x00000008 */
#define	MFS_VSELECT	 4	/* select 	0x00000010 */
#define	MFS_VGETATTR	 5	/* getattr 	0x00000020 */
#define	MFS_VSETATTR	 6	/* setattr 	0x00000040 */
#define	MFS_VACCESS	 7	/* access 	0x00000080 */
#define	MFS_VLOOKUP	 8	/* lookup	0x00000100 */
#define	MFS_VCREATE	 9	/* create	0x00000200 */
#define	MFS_VREMOVE	10	/* remove	0x00000400 */
#define	MFS_VLINK	11	/* link		0x00000800 */
#define	MFS_VRENAME	12	/* rename	0x00001000 */
#define	MFS_VMKDIR	13	/* mkdir	0x00002000 */
#define	MFS_VRMDIR	14	/* rmdir	0x00004000 */
#define	MFS_VREADDIR	15	/* readdir	0x00008000 */
#define	MFS_VSYMLINK	16	/* symlink	0x00010000 */
#define	MFS_VREADLINK	17	/* readlink	0x00020000 */
#define	MFS_VFSYNC	18	/* fsync	0x00040000 */
#define	MFS_VINACTIVE	19	/* inactive	0x00080000 */
#define	MFS_VLOCKCTL	20	/* lockctl	0x00100000 */
#define	MFS_VFID	21	/* fid		0x00200000 */
#define	MFS_VGETPAGE	22	/* getpage	0x00400000 */
#define	MFS_VPUTPAGE	23	/* putpage	0x00800000 */
#define	MFS_VMAP	24	/* map		0x01000000 */
#define	MFS_VDUMP	25	/* dump		0x02000000 */
#define	MFS_VCMP	26	/* cmp		0x04000000 */
#define MFS_VREALVP	27	/* realvp	0x08000000 */
#define MFS_VCNTL	28	/* cntl		0x10000000 */
#define MFS_VSPACE	29	/* space	0x20000000 */
#define MFS_VPATHCONF	30	/* pathconf	0x40000000 */
#define MFS_VNOPCNT	31

/* VFS Ops */

#define	MFS_VMOUNT	 0	/* mount 	0x00000001 */
#define MFS_VUMOUNT	 1	/* unmount 	0x00000002 */
#define MFS_VROOT	 2	/* root 	0x00000004 */
#define MFS_VSTATFS	 3	/* statfs 	0x00000008 */
#define MFS_VSYNC	 4	/* sync 	0x00000010 */
#define MFS_VGET	 5	/* vget (vn)	0x00000020 */
#define MFS_VMNTROOT	 6	/* mountroot 	0x00000040 */
#define MFS_VSWAPVP	 7	/* swapvp 	0x00000080 */
#define MFS_VFSOPCNT	 8

extern int mfs_vnopmax;			  /* Real size of tables */
extern char *mfs_vnopname[];
extern int mfs_vfsopmax;		  /* Real size of tables */
extern char *mfs_vfsopname[];

/*
 * RPC statistics.  Indexed by view/vob op.
 */

extern int mfs_viewopmax;		  /* Real size of tables */
extern MVFS_ATOMIC_T mfs_viewopcnt[];
extern timestruc_t mfs_viewoptime[];
extern char  *mfs_viewopnames[];

/*
 * Other internal statistics.
 * Most of these stats are protected on an MP machine
 * by 'mfs_statlock'.  The following are exceptions:
 *    mfs_mnstat:    Protected by the mfs_mnlock
 *    mfs_dncstat:   Protected by the mfs_dnclock
 */

#define MFS_CLNTSTAT_VERS	3
struct mfs_clntstat {
	ks_uint32_t    version;
	MVFS_ATOMIC_T  clntget;		/* Clnt statistics */
	MVFS_ATOMIC_T  clntfree;
	MVFS_ATOMIC_T  clntcreate;
	MVFS_ATOMIC_T  clntdestroy;
	MVFS_ATOMIC_T  clntcalls;
	MVFS_ATOMIC_T  clntretries;
	MVFS_ATOMIC_T  mfscall;		/* RPC statistics */
	MVFS_ATOMIC_T  mfsfail;
	MVFS_ATOMIC_T  mfsintr;
	MVFS_ATOMIC_T  mfsmaxdelay;	/* Number of RPC's longer than 30 secs */
	MVFS_ATOMIC_T  mfsmaxdelaytime;	/* Longest delayed RPC */
	timestruc_t  mvfsthread_time;	/* thread/process gunk timing */
};

#define MFS_MNSTAT_VERS		6
struct mfs_mnstat {
	ks_uint32_t    version;
	MVFS_ATOMIC_T  mnget;		/* Mnode statistics */
        MVFS_ATOMIC_T  mnfound;
	MVFS_ATOMIC_T  mnfoundstale;
	MVFS_ATOMIC_T  mnreusefree;
	MVFS_ATOMIC_T  mncreate;
	MVFS_ATOMIC_T  mngetnum;
	MVFS_ATOMIC_T  mnfree;
	MVFS_ATOMIC_T  mndestroy;
	MVFS_ATOMIC_T  mnreclaim;
        MVFS_ATOMIC_T  mnvobhashcnt;
        MVFS_ATOMIC_T  mncvphashcnt;
        MVFS_ATOMIC_T  mnotherhashcnt;
        MVFS_ATOMIC_T  mnflushvfscnt;
        MVFS_ATOMIC_T  mnflushvwcnt;
};

#define MFS_CLEARSTAT_VERS	5
struct mfs_clearstat {
	ks_uint32_t  version;
	MVFS_ATOMIC_T  clearget;	/* Cleartext op statistics */
	MVFS_ATOMIC_T  clearcreate;
	MVFS_ATOMIC_T  clearraces;
	MVFS_ATOMIC_T  clearcreatraces;
	MVFS_ATOMIC_T  clearread;
	MVFS_ATOMIC_T  clearwrite;
	timestruc_t  clearget_time;
	timestruc_t  clearcreat_time;
	timestruc_t  clearrd_time;
	timestruc_t  clearwr_time;
	timestruc_t  clearopen_time;
	timestruc_t  unclearrd_time;
	timestruc_t  unclearwr_time;
	timestruc_t  unclearget_time;
	timestruc_t  cto_getattr_time;	/* clnt_geattr forced by cto */
	MVFS_ATOMIC_T  clearopen;
	MVFS_ATOMIC_T  unclearopen;
	MVFS_ATOMIC_T  cleargetmiss;	/* misses on clearget */
	MVFS_ATOMIC_T  clearreclaim;	/* reclaimed from free list */
	MVFS_ATOMIC_T  clearreclaimmiss;/* had cltxt but we dropped it */
	MVFS_ATOMIC_T  cleargetlkup;	/* lookups for creds */
};

#define MFS_RVCSTAT_VERS	3
struct mfs_rvcstat {
	ks_uint32_t  version;
	MVFS_ATOMIC_T  rvc_hits;	/* Root version cache statistics */
	MVFS_ATOMIC_T  rvc_misses;
	MVFS_ATOMIC_T  rvc_misstimo;
	MVFS_ATOMIC_T  rvc_purge;
}; 

#define MFS_DNCSTAT_VERS	5
struct mfs_dncstat {			/* Protect with DNC spinlock! */
	ks_uint32_t  version;
	MVFS_ATOMIC_T  dnc_hits;	/* Hits (total) */
	MVFS_ATOMIC_T  dnc_hitdot;	/* Hits because looked up "." */
	MVFS_ATOMIC_T  dnc_hitdir;	/* Hits in dir dnc cache */
	MVFS_ATOMIC_T  dnc_hitreg;	/* Hits in reg dnc cache */
	MVFS_ATOMIC_T  dnc_hitnoent;	/* Hits in enoent dnc cache */
	MVFS_ATOMIC_T  dnc_hitbhfromnull;	/* Hits cause bh optimization worked */
	MVFS_ATOMIC_T  dnc_misses;	/* Misses (total) */
	MVFS_ATOMIC_T  dnc_missinvalid;	/* Misses cause entry invalidated */
	MVFS_ATOMIC_T  dnc_missdncgen;	/* Misses cause dncgen mismatch */
	MVFS_ATOMIC_T  dnc_missevtime;	/* Misses cause evtime changed */
	MVFS_ATOMIC_T  dnc_missnoenttimedout;/* Misses cause noent timed out */
	MVFS_ATOMIC_T  dnc_missbh;	/* Misses because bh miss */
	MVFS_ATOMIC_T  dnc_missnovp;	/* Misses on vnode activation */
	MVFS_ATOMIC_T  dnc_add;		/* Adds (total) */
	MVFS_ATOMIC_T  dnc_adddir;	/* Add dir entry */
	MVFS_ATOMIC_T  dnc_addreg;	/* Add reg entry */
	MVFS_ATOMIC_T  dnc_addnoent;	/* Add enoent entries */
	MVFS_ATOMIC_T  dnc_addbhinvariant;	/* Adds with bh invariant */
	MVFS_ATOMIC_T  dnc_addnoop;	/* Adds that found existing OK entry */
	MVFS_ATOMIC_T  dnc_addbh;	/* Adds with bh only */
	MVFS_ATOMIC_T  dnc_addlong;	/* Adds with long names */
	MVFS_ATOMIC_T  dnc_addunlock;	/* Adds failing dur to unlock of dir */
	MVFS_ATOMIC_T  dnc_change;	/* Adds that changed existing entry */
	MVFS_ATOMIC_T  dnc_remove;	/* Explicit removes */
	MVFS_ATOMIC_T  dnc_invalvp;	/* Inval vp call */
	MVFS_ATOMIC_T  dnc_flush;	/* Flush complete dnc calls */
	MVFS_ATOMIC_T  dnc_invalhits;	/* Number of items invalidated */
	MVFS_ATOMIC_T  dnc_flushvw;	/* Flush view from dnc calls */
	MVFS_ATOMIC_T  dnc_flushvfs;	/* Flush vob from dnc calls */
	MVFS_ATOMIC_T  dnc_invalvw;	/* Invalidate view calls */
	MVFS_ATOMIC_T  dnc_invalnf;	/* Invalidate obj not found calls */
	/*
	 * The following are locked with statslock instead of dnc
	 * splock, since these are updated in the code which calls
	 * dnclookup.
	 */
	MVFS_ATOMIC_T  dnc_missdir;	/* missed on dir */
	MVFS_ATOMIC_T  dnc_missreg;	/* missed on regular file */
	MVFS_ATOMIC_T  dnc_missnoent;	/* missed on noent */
};

#define MFS_ACSTAT_VERS		4
struct mfs_acstat {
	ks_uint32_t  version;
	MVFS_ATOMIC_T  ac_hits;		/* Attribute cache statistics */
        MVFS_ATOMIC_T  ac_misses;
        MVFS_ATOMIC_T  ac_updates;
	MVFS_ATOMIC_T  ac_mod;
	MVFS_ATOMIC_T  ac_expmod;
	MVFS_ATOMIC_T  ac_vobmod;	/* Vob event modified */
	MVFS_ATOMIC_T  ac_evmiss;	/* Vob event time ac fetches */
        MVFS_ATOMIC_T  ac_cto;		/* Getattr's due to force getattr on open */
	MVFS_ATOMIC_T  ac_timo;		/* attr cache timeout misses */
	MVFS_ATOMIC_T  ac_genmiss;	/* build generation misses */
	MVFS_ATOMIC_T  ac_newmiss;	/* new mnode (compulsory) misses */
	MVFS_ATOMIC_T  ac_lvuthit;	/* hits on LVUT revalidated an mnode */
	MVFS_ATOMIC_T  ac_lvutmiss;	/* misses on LVUT as part of timo miss */
        MVFS_ATOMIC_T ac_rddirhit;	/* rddir hits */
        MVFS_ATOMIC_T ac_rddirmiss;	/* rddir misses */
};

#define MFS_RLSTAT_VERS		2
struct mfs_rlstat {
	ks_uint32_t  version;
	MVFS_ATOMIC_T  rl_hits;	/* Readlink text cache statistics */
        MVFS_ATOMIC_T  rl_misses;
};

#define MFS_AUSTAT_VERS		3
struct mfs_austat {
	ks_uint32_t  version;
	MVFS_ATOMIC_T  au_calls;	/* Audit stats */
	MVFS_ATOMIC_T  au_vgetattr;	/* VOB getattrs */
	MVFS_ATOMIC_T  au_nvgetattr;	/* Non-VOB getattrs */
	MVFS_ATOMIC_T  au_dupl;		/* Audit dupls eliminated */
	timestruc_t  au_time;		/* time in audits so far */
	timestruc_t  au_settime;	/* time in set_audited calls so far */
	timestruc_t  au_ioctltime;	/* time in ioctl calls so far */
};

/*
 * Histogram of RPC delays.
 */

#define MFS_RPCHIST_VERS	3
#define MFS_NUM_HISTX		16
struct mfs_rpchist {
	ks_uint32_t	version;
	timestruc_t	histval[MFS_NUM_HISTX];	/* Histogram slot values */
	MVFS_ATOMIC_T	histrpc[MFS_NUM_HISTX];	/* Histogram RPC but cltxt */
	MVFS_ATOMIC_T	histclr[MFS_NUM_HISTX]; /* Histogram for cltxt fetch */
	/*
	 * Histogram for each op (in addition to the regular hist).  Since
	 * we put this at the end, it's mostly compatible with older
	 * mvfsstat gathering programs.  New ones are smart enough to zero
	 * out the stats before requesting; old kernels won't copy out
	 * that much data and so it stays zero in process space.
	 * Old stat gatherers don't know anything about the extra length.
	 */
	MVFS_ATOMIC_T	histperop[VIEW_NUM_PROCS][MFS_NUM_HISTX];
};
	

extern struct mfs_clntstat 	mfs_clntstat;
extern struct mfs_mnstat   	mfs_mnstat;
extern struct mfs_clearstat 	mfs_clearstat;
extern struct mfs_rvcstat	mfs_rvcstat;
extern struct mfs_dncstat	mfs_dncstat;
extern struct mfs_acstat	mfs_acstat;
extern struct mfs_rlstat	mfs_rlstat;
extern struct mfs_austat	mfs_austat;
extern struct mfs_rpchist	mfs_viewophist;

#endif	/* MFS_STATS_H_ */
/* $Id: 96d36da4.66b911dc.9bbb.00:01:83:09:5e:0d $ */
