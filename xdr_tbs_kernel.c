/* * (C) Copyright IBM Corporation 1990, 2008. */
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
/* xdr_tbs_kernel.c */
/*LINTLIBRARY*/
/*
 * This file and the other _kernel.c XDR files are compiled both in
 * kernel mode (for MVFS) and non-kernel mode (for the rest of ClearCase).
 */
#include "mvfs_systm.h"
#include "mvfs.h"               /* Need debug prototypes, e.g. mfs_kalloc() */
#include <linux/types.h>
#include <linux/time.h>

#include <xdr_ks.h>
#include <tbs_base.h>
#include "tbs_rpc_kernel.h"

/*
 * Type choices for time struct, size_t, time_t, etc are made in
 * platform-specific header files ks_mdep_<platform>.h, the proper one
 * of which will get included for us by ks_base.h.
 */ 

/* Timeval is not done as an EZ_XDR_ROUTINE, because it is supplied in
 * system libraries on several platforms and it doesn't take a third arg.
 * (We will ignore the fact that EZRPC passes it a third argument.)
 */

bool_t 
xdr_timeval(xdrs, objp)
    XDR *xdrs;
    struct timeval *objp;
{

    if (!xdr_long(xdrs, &objp->tv_sec) ||
	!xdr_long(xdrs, &objp->tv_usec))
	return (FALSE);
    return (TRUE);
}

#undef xdr_size_t
EZ_XDR_ROUTINE(size_t)
{
#if defined(ATRIA_SIZE_T_ULONG)
 return xdr_u_long(xdrs, (u_long *)objp);
#elif defined(ATRIA_SIZE_T_UINT)
 return xdr_u_int(xdrs, (u_int *)objp);
#elif defined(ATRIA_SIZE_T_INT)
 return xdr_int(xdrs, (int *)objp);
#elif defined(ATRIA_SIZE_T_UINT64)
 return atria_xdr_uint64(xdrs, (unsigned __int64 *)objp);
#else
#error "xdr_tbs_kernel.c: no code for xdr_size_t"
#endif
}


#undef xdr_time_t
EZ_XDR_ROUTINE(time_t)
{
#if defined(ATRIA_TIME_T_INT)
    return xdr_int(xdrs, (int *)objp);
#elif defined(ATRIA_TIME_T_LONG)
    return xdr_long(xdrs, (long *)objp);
#elif defined(ATRIA_TIME_T_INT64)
    return atria_xdr_int64(xdrs, (__int64 *)objp EZ_XDR_ARG);
#else
#error "xdr_tbs.c: no code for xdr_time_t"
#endif 
}

#undef xdr_tbs_boolean_t
EZ_XDR_ROUTINE(tbs_boolean_t)
{
    return xdr_int(xdrs, (int *)objp);
}

#undef xdr_tbs_status_t
EZ_XDR_ROUTINE(tbs_status_t)
{
    return xdr_int(xdrs, (int *)objp);
}

EZ_XDR_ROUTINE(tbs_uuid_t)
{
    ks_uint32_t l1,l2,l3;
    int i; 
    ks_uint32_t ulong_time_low;


    if (xdrs->x_op == XDR_FREE)
	return TRUE;
    if (xdrs->x_op == XDR_ENCODE) {
	/*
	 * Pack the UUID.  Do it in steps to avoid sign extensions
	 */
	l1 = objp->time_hi_and_version;
        l1 = (l1 << 16) | objp->time_mid;

	l2 = objp->clock_seq_hi_and_reserved;
        l2 = (l2 << 8) | objp->clock_seq_low;
        l2 = (l2 << 8) | objp->node[0];
        l2 = (l2 << 8) | objp->node[1];

	for (l3 = 0, i = 2; i <= 5; i++) {
	    l3 = l3 << 8 | objp->node[i];
	}
    }
    ulong_time_low = objp->time_low;
    if (!XDR_KS_U_INT32(xdrs, &ulong_time_low) ||
	!XDR_KS_U_INT32(xdrs, &l1) ||
	!XDR_KS_U_INT32(xdrs, &l2) ||
	!XDR_KS_U_INT32(xdrs, &l3))
	return (FALSE);

    if (xdrs->x_op == XDR_DECODE) {
	/*
	 * Unpack the UUID.
	 */
	objp->time_low = ulong_time_low;
	objp->time_mid = (unsigned short) (l1 & 0xffff);
	objp->time_hi_and_version = (unsigned short) (l1 >> 16);
	objp->node[1] = (unsigned char) (l2 & 0xff);
	l2 = l2 >> 8;
	objp->node[0] = (unsigned char) (l2 & 0xff);
	l2 = l2 >> 8;
	objp->clock_seq_low = (unsigned char) (l2 & 0xff);
	l2 = l2 >> 8;
	objp->clock_seq_hi_and_reserved = (unsigned char) (l2 & 0xff);
	for (i = 5; i >= 2; i--) {
	    objp->node[i] = (unsigned char) (l3 & 0xff);
	    l3 = l3 >> 8;
	}
    }
    return (TRUE);
}

#undef xdr_tbs_oid_t
EZ_XDR_ROUTINE(tbs_oid_t)
{
    return xdr_tbs_uuid_t(xdrs, &objp->obj_uuid EZ_XDR_ARG_PASSTHRU);
}

#undef xdr_tbs_dbid_t
EZ_XDR_ROUTINE(tbs_dbid_t)
{
    return xdr_u_long(xdrs, (u_long *)objp);
}

#undef xdr_tbs_ftype_t
EZ_XDR_ROUTINE(tbs_ftype_t)
{
    return xdr_u_long(xdrs, (u_long *)objp);
}

#undef xdr_tbs_fmode_t
EZ_XDR_ROUTINE(tbs_fmode_t)
{
    return xdr_u_long(xdrs, (u_long *)objp);
}

EZ_XDR_ROUTINE(tbs_fstat_db_t)
{
    ks_int32_t size_high; 
    ks_uint32_t size_low;

    /*
     * We need to have the code independent of Big/Little endian.
     */

    if (xdrs->x_op == XDR_ENCODE) {
	KS_LLONG_SPLIT(objp->size, size_high, size_low);
    }

    if (!xdr_tbs_ftype_t(xdrs, &objp->type EZ_XDR_ARG_PASSTHRU) ||
	!xdr_tbs_fmode_t(xdrs, &objp->mode EZ_XDR_ARG_PASSTHRU) ||
	!xdr_u_long(xdrs, &objp->nlink) ||
	!xdr_u_long(xdrs, &objp->flags) ||
	!xdr_credutl_sid_t(xdrs, &objp->usid EZ_XDR_ARG_PASSTHRU) ||
	!xdr_credutl_sid_t(xdrs, &objp->gsid EZ_XDR_ARG_PASSTHRU) ||
	!XDR_KS_U_INT32(xdrs, &size_low) ||
	!XDR_KS_INT32(xdrs, &size_high) ||
	!xdr_u_long(xdrs, &objp->nodeid) ||
	!xdr_timeval(xdrs, &objp->xtime) ||
	!xdr_timeval(xdrs, &objp->atime) ||
	!xdr_timeval(xdrs, &objp->mtime) ||
	!xdr_timeval(xdrs, &objp->ctime)) {
	return (FALSE);
    }

    if (xdrs->x_op == XDR_DECODE) {
        objp->size = KS_LLONG_COMBINE(size_high, size_low);
    }

    return (TRUE);
}

#undef xdr_vob_mtype_t
EZ_XDR_ROUTINE(vob_mtype_t)
{
    return xdr_enum(xdrs, (enum_t *)objp);
}

#if defined(ATRIA_TIME_T_INT64)
bool_t
atria_xdr_int64(
    XDR *xdrs,
    __int64 *objp
    EZ_XDR_ARGDECL
)
{
    int local_int;

    if (xdrs->x_op == XDR_ENCODE) {
        ASSERT(*objp >= INT_MIN && *objp <= INT_MAX);
        if (!(*objp >= INT_MIN && *objp <= INT_MAX)) {
            return(FALSE);              /* out of range; force encoding error */
        }
        local_int = (int)(*objp & 0xffffffff);
        return (xdr_int(xdrs, &local_int));
    }
    else if (xdrs->x_op == XDR_DECODE) {
        if (!xdr_int(xdrs, &local_int)) {
            return(FALSE);
        }
        *objp = (__int64) local_int;
        return(TRUE);
    }
    else if (xdrs->x_op == XDR_FREE) {
        return (TRUE);
    }
    return (FALSE);
}
#endif /* ATRIA_TIME_T_INT64 */

#if defined(ATRIA_SIZE_T_UINT64)
bool_t
atria_xdr_uint64(
    XDR *xdrs,
    unsigned __int64 *objp
    EZ_XDR_ARGDECL
)
{
    u_int local_uint;

    if (xdrs->x_op == XDR_ENCODE) {
        ASSERT(*objp <= UINT_MAX);
        if (!(*objp <= UINT_MAX)) {
            return(FALSE);              /* out of range; force encoding error */
        }
        local_uint = (u_int) *objp;
        return (xdr_u_int(xdrs, &local_uint));
    }
    else if (xdrs->x_op == XDR_DECODE) {
        if (!xdr_u_int(xdrs, &local_uint)) {
            return(FALSE);
        }
        /* Make sure no sign extention */
        *objp = (unsigned __int64) (local_uint & UINT_MAX);
        return(TRUE);
    }
    else if (xdrs->x_op == XDR_FREE) {
        return (TRUE);
    }
    return (FALSE);
}
#endif /* ATRIA_SIZE_T_UINT64 */

static const char vnode_verid_xdr_tbs_kernel_c[] = "$Id:  4fccd0db.169111d7.81b9.00:50:da:ba:19:c8 $";
