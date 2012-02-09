/* * (C) Copyright IBM Corporation 1990, 2006. */
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

#if !defined(_TBS_RPC_KERNEL_H_)
#define _TBS_RPC_KERNEL_H_
#include <ks_rpc.h>
#include <tbs_base.h>
#include <vob_mtype.h>

/* The following macro is used to cast an xdr function for one kind of
   object (say u_long) to that for another kind of object (say tbs_fmode_t) */

#if ((__GNUC__ == 3) && (__GNUC_MINOR__ >= 4)) || __GNUC__ >= 4
/* can't do XDR function name casting */
#else
#define TBS_XDR_FUNC(obj_type) (bool_t (*)(XDR *_xdrs, obj_type *_objp EZ_XDR_ARGDECL))
#endif /* GCC 3.4 or later */

/*
 * Type choices for time struct, size_t, time_t, etc are made in
 * platform-specific header files ks_mdep_<platform>.h, the proper one
 * of which will get included for us by ks_base.h.
 */

/****************************************************************************
 * xdr_size_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(size_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#if defined(ATRIA_SIZE_T_ULONG)
#define xdr_size_t (TBS_XDR_FUNC(size_t) xdr_u_long)
#elif defined(ATRIA_SIZE_T_UINT)
#define xdr_size_t (TBS_XDR_FUNC(size_t) xdr_u_int)
#elif defined(ATRIA_SIZE_T_INT)
#define xdr_size_t (TBS_XDR_FUNC(size_t) xdr_int)
#elif defined(ATRIA_SIZE_T_UINT64)
extern bool_t atria_xdr_uint64(XDR *, size_t * EZ_XDR_ARGDECL);
#define xdr_size_t (TBS_XDR_FUNC(size_t) atria_xdr_uint64)
#else
#error "tbs_rpc_kernel.h: xdr type for size_t unknown"
#endif 
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_time_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */

EZ_XDR_ROUTINE(time_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#if (defined(ATRIA_TIME_T_INT) && !defined(ATRIA_64BIT_LONGS))
#define xdr_time_t (TBS_XDR_FUNC(time_t) xdr_int) 
#elif defined(ATRIA_TIME_T_UINT)
#define xdr_time_t (TBS_XDR_FUNC(time_t) xdr_u_int)
#else
#if defined(ATRIA_TIME_T_LONG)
#define xdr_time_t (TBS_XDR_FUNC(time_t) xdr_long)
#elif defined(ATRIA_TIME_T_UINT64)
#define xdr_time_t (TBS_XDR_FUNC(time_t) xdr_uint64)
#else
#error "tbs_rpc.h: xdr type for time_t unknown"
#endif
#endif
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_timeval (struct timeval)
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
/*
 * Provided by system libraries on some operating systems, and by us
 * on others.
 */
#ifndef WINSOCK
EXTERN bool_t
xdr_timeval(
    XDR *,
    struct timeval *
);
#endif

/****************************************************************************
 * xdr_tbs_boolean_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_boolean_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#define xdr_tbs_boolean_t (TBS_XDR_FUNC(tbs_boolean_t) xdr_int)
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_tbs_status_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_status_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#define xdr_tbs_status_t (TBS_XDR_FUNC(tbs_status_t) xdr_int)
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_tbs_uuid_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_uuid_t);

/****************************************************************************
 * xdr_tbs_oid_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_oid_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#define xdr_tbs_oid_t (TBS_XDR_FUNC(tbs_oid_t) xdr_tbs_uuid_t)
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_tbs_dbid_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_dbid_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#define xdr_tbs_dbid_t (TBS_XDR_FUNC(tbs_dbid_t) xdr_u_long)
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_tbs_ftype_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_ftype_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#define xdr_tbs_ftype_t (TBS_XDR_FUNC(tbs_ftype_t) xdr_u_long)
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_tbs_fmode_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_fmode_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#define xdr_tbs_fmode_t (TBS_XDR_FUNC(tbs_fmode_t) xdr_u_long)
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

/****************************************************************************
 * xdr_tbs_fstat_db_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(tbs_fstat_db_t);

/****************************************************************************
 * xdr_vob_mtype_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(vob_mtype_t);

#if !defined(TBS_RPC_DECLS_ONLY) && defined(TBS_XDR_FUNC)
#define xdr_vob_mtype_t (TBS_XDR_FUNC(vob_mtype_t) xdr_enum)
#endif /* !TBS_RPC_DECLS_ONLY && TBS_XDR_FUNC */

#define xdr_xid_t xdr_u_long

#endif /* !defined(_TBS_RPC_KERNEL_H_) */
/* $Id: 0ba3822c.66bb11dc.9bbb.00:01:83:09:5e:0d $ */
