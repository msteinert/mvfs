/* * (C) Copyright IBM Corporation 1990, 2005. */
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

#ifndef MFS_DEBUG_H_
#define MFS_DEBUG_H_

#include "mfs_stats.h"

/*
 * mfs debugging support - patchable flags
 */

extern unsigned long mdb_vops;	/* Mask for vnode ops to print */
extern unsigned long mdb_vfsops;/* Mask for vfs ops to print */
extern unsigned long mdb_iops;	/* Mask for FSS ops to print */
extern unsigned long mdb_xops;  /* Mask for other ops to print */
extern unsigned long mdb_traps; /* Misc debugging "traps" in the code */
extern int mfs_logpri;		/* Console msg print priority level */
extern char *mvfs_sevmsg[];	/* Console msg hdr by severity */

/*
 * Logging macros
 * Determine if a log priority being logger.
 * Display a log message tagged with severity (if log priority being logged)
 * 	mvfs_log_sevpri(severity, priority, "fmt", args...)
 * Display a log message tagged with severity if error != 0 and
 * priority indicates that error should be logged.
 *      mvfs_logperr_sevpri(severity, priority, error, "fmt", args ...)
 *
 * the older versions (mvfs_log, mvfs_logperr) use the priorty as the
 * severity.  It can't be expanded as a macro to the new versions,
 * because they're variadic functions and cpp doesn't like variadic
 * macros.
 *
 * Log priority values are declared in "sys/mfs_ioctl.h"
 *
 * Note: no newline for "fmt" in MFS_LOGERR but should
 *	 have a newline for "fmt" in MFS_LOG
 *
 */

#define MVFS_PRI_LOGGED(pri)  ((pri) <= mfs_logpri)

EXTERN void
mvfs_log(P1(int pri)
	 PN(A_CONST char *fmt)
	 PN(...));

EXTERN void
mvfs_vlog(
    int pri,
    const char *fmt,
    va_list ap
);

EXTERN void
mvfs_logperr(P1(int pri)
	     PN(int err)
	     PN(A_CONST char *fmt)
	     PN(...));

EXTERN void
mvfs_log_sevpri(
    int sev,
    int pri,
    const char *fmt,
    ...
);

EXTERN void
mvfs_logperr_sevpri(
    int sev,
    int pri,
    int err,
    const char *fmt,
    ...
);

/*
 * Flag values for vnode ops/vfs ops come from the statistics
 * indices
 */

/* 
 * Inode ops (for sysV.3).
 * These are used only for logging (no stats kept)
 */

#define MDB_INIT	 0	/* init		0x00000001 */
#define MDB_IPUT	 1	/* iput		0x00000002 */
#define MDB_IREAD	 2	/* iread	0x00000004 */
#define MDB_IIUPDAT	 3	/* iupdat	0x00000008 */
#define MDB_READI	 4	/* readi	0x00000010 */
#define MDB_WRITEI	 5	/* writei	0x00000020 */
#define MDB_ITRUNC	 6	/* itrunc	0x00000040 */
#define MDB_ISTATF	 7	/* statf	0x00000080 */
#define MDB_NAMEI	 8	/* namei	0x00000100 */
#define MDB_IMOUNT	 9	/* mount	0x00000200 */
#define MDB_IUMOUNT	10	/* umount	0x00000400 */
#define MDB_GETINODE	11	/* getinode	0x00000800 */
#define MDB_OPENI	12	/* openi 	0x00001000 */
#define MDB_CLOSEI	13	/* closei	0x00002000 */
#define MDB_IUPDATE	14	/* update 	0x00004000 */
#define MDB_ISTATFS	15	/* statfs	0x00008000 */
#define MDB_IACCESS	16	/* access	0x00010000 */
#define MDB_IGETDENTS	17	/* getdents 	0x00020000 */
#define MDB_IALLOCMAP	18	/* allocmap	0x00040000 */
#define MDB_IFREEMAP	19	/* freemap	0x00080000 */
#define MDB_IREADMAP	20	/* readmap	0x00100000 */
#define MDB_ISETATTR	21	/* setattr	0x00200000 */
#define MDB_INOTIFY	22	/* notify	0x00400000 */
#define MDB_IFCNTL	23	/* fcntl	0x00800000 */
#define MDB_IFSINFO	24	/* fsinfo	0x01000000 */
#define MDB_IIOCTL	25	/* ioctl	0x02000000 */
#define MDB_IUTIL	26	/* utils 	0x04000000 */
#define MDB_IOPCNT	27	/* Opcnt 	*/

/*
 * Other MFS "xops"
 */
#define MDB_PROCOPS	0	/* Procops		0x00000001 */
#define MDB_AUDITOPS	1	/* Audit ops		0x00000002 */
#define MDB_AUDITW	2	/* Audit buffer ops	0x00000004 */
#define MDB_CLEAROPS	3	/* Cleartext ops 	0x00000008 */
#define MDB_RPCOPS	4	/* RPC calls		0x00000010 */
#define MDB_MFSCALL	5	/* mfscall routine	0x00000020 */
#define MDB_MKNOD	6	/* Makenode ops 	0x00000040 */
#define MDB_REBIND	7	/* Cwd rebinding	0x00000080 */
#define MDB_BINDROOT	8	/* Bindroot call	0x00000100 */
#define MDB_MNOPS	9	/* Mnode ops 		0x00000200 */
#define MDB_MAPOPS	10	/* Mapped file ops	0x00000400 */
#define MDB_ALBDOPS	11	/* ALBD ops		0x00000800 */
#define MDB_AUDITF	12	/* Audit file ops	0x00001000 */
#define MDB_DNCBHUSE	13	/* DNC BH optimizations 0x00002000 */
#define MDB_USERLOOKUP  14	/* MFS 'user' lookups   0x00004000 */
#define MDB_GETAPAGE	15	/* MFS getapage calls   0x00008000 */
#define MDB_PUTAPAGE	16	/* MFS putapage calls   0x00010000 */
#define MDB_MEMOP	17	/* memory allocate op   0x00020000 */
#define MDB_PROCOPS2	18	/* more detailed procop 0x00040000 */
#define MDB_AUDITOPS2	19	/* more detailed audops 0x00080000 */
#define MDB_RVC_DNC	20	/* RVC in DNC		0x00100000 */
#define MDB_DNC_REALLOC	21	/* DNC reallocation	0x00200000 */
#define MDB_PROCOPS3	22	/* Process/Thread       0x00400000 */
#define MDB_DIRNOTIFY   23	/* Directory notify	0x00800000 */
#define MDB_CTIME	24	/* CTIME mucking	0x01000000 */
#define MDB_PNLOOKUP 	25	/* Pathname lookup	0x02000000 */
#define MDB_NTLAYER  	26	/* NT Layered FS Dev	0x04000000 */
#define MDB_LVUT	27	/* LVUT			0x08000000 */
#define MDB_RECURLOCKS 	28   	/* Recursive locks   	0x10000000 */
#define MDB_CREDS 	29   	/* NT creds    	        0x20000000 */
#define MDB_VNODES 	30   	/* general vnode stuff  0x40000000 */
#define MDB_XOPCNT	31

/*
 * MFS debug "traps" in the code.
 */
#define MDB_PANICSTK	1	/* Panic on stk chk	0x00000002 */
#define MDB_CHKMNODE1	3	/* Check mnodes 1/min   0x00000008 */
#define MDB_CHKMNODE2	4	/* Check mnodes more    0x00000010 */

#define MDB_IS_XLOG_SET(f) (mdb_xops & 1<<(f))

#if defined(MVFS_LOG) || defined(MVFS_DEBUG)

/* Macro to print vnode op info */

EXTERN void
mdb_vlog(P1(u_int op)
	 PN(A_CONST char *fmt)
	 PN(...));

EXTERN void
mdb_vfslog(P1(u_int op)
	   PN(A_CONST char *fmt)
	   PN(...));

EXTERN void
mdb_xlog(P1(u_int op)
	 PN(A_CONST char *fmt)
	 PN(...));

#define MDB_VLOG(x) mdb_vlog x

/* Macro to print vfs op info */

#define MDB_VFSLOG(x) mdb_vfslog x

/* 
 *Macro to print miscellaneous debug messages and related flags
 */

#define MDB_XLOG(x) mdb_xlog x

#else

#define MDB_VLOG(x) /**/
#define MDB_VFSLOG(x) /**/
#define MDB_XLOG(x)	/**/

#endif  /* MVFS_LOG */

#ifdef MVFS_DEBUG

 /*
 * Macros to run consistency checks inside the kernel
 */

EXTERN void mfs_chkpoint(P1(int));
EXTERN void mfs_chkpages(P1(VNODE_T *) PN(int));
#define MDB_CHKPOINT(bg)  mfs_chkpoint(bg)
#define MDB_NOPAGES(vp)	mfs_chkpages(vp,0);
#define MDB_NODELWRI(vp) mfs_chkpages(vp,1);

#else

#define MDB_CHKPOINT(bg)
#define MDB_NOPAGES(vp)
#define MDB_NODELWRI(vp)

#endif	/* MVFS_DEBUG */
#endif	/* MFS_DEBUG_H_ */
/* $Id: 2b029d54.637911da.8655.00:01:83:a6:4c:63 $ */
