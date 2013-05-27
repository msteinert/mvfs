/* * (C) Copyright IBM Corporation 1990, 2007. */
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
/* mvfs_debug.c */
#include "mvfs_systm.h"
#include "mvfs.h"

/* For sumbols only */
#include "mvfs_dnc.h"

/*
 * Debug information and statistics.
 */

u_long mdb_vops;
u_long mdb_vfsops;
u_long mdb_iops;
u_long mdb_xops;
u_long mdb_traps;
int	mfs_logpri = MFS_LOG_INFO;	/* adjusted in mvfsinit if not debug */

/*
 * These messages are formatted to use the same prefix tags as imsg.h's
 * IMSG_XXX_LABEL constants.
 */
char  *mvfs_sevmsg[] = {
	"mvfs: Ok: ", "mvfs: Ok: ", "mvfs: Ok: ", "mvfs: Error: ", 
	"mvfs: Warning: ", "mvfs: Ok: INFO: ", "mvfs: Ok: ESTALE: ",
	"mvfs: Ok: DEBUG: ", "mvfs: Ok: ENOENT:  ",
};

int mfs_vnopmax = MFS_VNOPCNT;
int mfs_vfsopmax = MFS_VFSOPCNT;

int mvfs_cnprint_delay = 100000;

#ifdef MVFS_DEBUG
void
mfs_chkpoint(bg)
int bg;
{
    /* See if right time to check (1 minute vs. all chkpoints).
       Most chkpoints are called with 0.  1 minute update calls
       with value of 1. */

    if ((mdb_traps & (MDB_CHKMNODE2|MDB_CHKMNODE1)) == 0) return;
    if (!bg && (mdb_traps & MDB_CHKMNODE2) == 0) return;
	
}
#endif

#if defined(MVFS_DEBUG) || defined(MVFS_LOG)
char *mfs_vnopname[MFS_VNOPCNT] = {
	"mvfs: Ok: open:", "mvfs: Ok: close:", "mvfs: Ok: rdwr:",
	"mvfs: Ok: ioctl:", "mvfs: Ok: select:", "mvfs: Ok: getattr:",
	"mvfs: Ok: setattr:", "mvfs: Ok: access:", "mvfs: Ok: lookup:",
	"mvfs: Ok: create:", "mvfs: Ok: remove:", "mvfs: Ok: link:",
	"mvfs: Ok: rename:", "mvfs: Ok: mkdir:", "mvfs: Ok: rmdir:",
	"mvfs: OK: readdir:", "mvfs: Ok: symlink:", "mvfs: Ok: readlink:",
	"mvfs: Ok: fsync:", "mvfs: Ok: inactive:", "mvfs: Ok: lockctl:",
	"mvfs: Ok: fid:", "mvfs: Ok: getpage:", "mvfs: Ok: putpage:",
	"mvfs: Ok: map:", "mvfs: Ok: dump:", "mvfs: Ok: cmp:",
	"mvfs: Ok: realvp:", "mvfs: Ok: cntl:", "mvfs: Ok: space:",
	"mvfs: Ok: pathconf:"
};

char *mfs_vfsopname[MFS_VFSOPCNT] = {
	"mvfs: Ok: mount:", "mvfs: Ok: unmount:", "mvfs: Ok: root:",
	"mvfs: Ok: statfs:", "mvfs: Ok: sync:", "mvfs: Ok: vget:",
	"mvfs: Ok: mountroot:", "mvfs: Ok: swapvp:"
};

char *mdb_xopname[MDB_XOPCNT] = {
	"mvfs: Ok: procop:", "mvfs: Ok: auditop:", "mvfs: Ok: auditw:",
	"mvfs: Ok: clearop:", "mvfs: Ok: rpc:", "mvfs: Ok: mfscall:",
	"mvfs: Ok: mknod:", "mvfs: Ok: rebind:", "mvfs: Ok: bindroot:",
	"mvfs: Ok: mnode:", "mvfs: Ok: mmap:", "mvfs: Ok: albd:",
	"mvfs: Ok: auditf:", "mvfs: Ok: dncbh:", "mvfs: Ok: upath:",
	"mvfs: Ok: getapage:",	"mvfs: Ok: putapage:", "mvfs: Ok: memop:",
	"mvfs: Ok: proc2:", "mvfs: Ok: audit2:", "mvfs: Ok: rvcdnc:",
	"mvfs: Ok: dncrealloc:", "mvfs: Ok: proc3:", "mvfs: Ok: dirnotify:",
        "mvfs: Ok: ctime:", "mvfs: Ok: pnlookup:", "mvfs: Ok: ntlayer:",
        "mvfs: Ok: lvut:", "mvfs: Ok: recurlock:", "mvfs: Ok: creds:",
        "mvfs: Ok: vnodes:"
};

extern CLR_VNODE_T *mvfs_printf_logvp;

void
mdb_vlog(u_int op, A_CONST char *fmt, ...)
{
    va_list ap;

    if ((mdb_vops & (1UL)<<(op)) != 0) {
	va_start(ap, fmt);
	MVFS_VPRINTF_3 (MFS_LOG_DEBUG, mfs_vnopname[op], NULL, fmt, ap);
	if (!mvfs_printf_logvp && mvfs_cnprint_delay)
	    MDKI_USECDELAY(mvfs_cnprint_delay);
	va_end(ap);
    }
}

void
mdb_vfslog(u_int op, A_CONST char *fmt, ...)
{
    va_list ap;
    if ((mdb_vfsops & (1UL)<<(op)) != 0) {
	va_start(ap, fmt);
	MVFS_VPRINTF_3 (MFS_LOG_DEBUG, mfs_vfsopname[(op)], NULL, fmt, ap);
	if (!mvfs_printf_logvp && mvfs_cnprint_delay)
	    MDKI_USECDELAY(mvfs_cnprint_delay);
	va_end(ap);
    }
}

void
mdb_xlog(u_int f, A_CONST char *fmt, ...)
{
    va_list ap;
    if ((mdb_xops & (1UL)<<(f)) != 0) {
	va_start(ap, fmt);
	MVFS_VPRINTF_3 (MFS_LOG_DEBUG, mdb_xopname[f], NULL, fmt, ap);
	if (!mvfs_printf_logvp && mvfs_cnprint_delay)
	    MDKI_USECDELAY(mvfs_cnprint_delay);
	va_end(ap);
    }
}

#endif
static const char vnode_verid_mvfs_debug_c[] = "$Id:  ebc100a6.9c1c11dd.9a62.00:01:83:29:c0:fc $";
