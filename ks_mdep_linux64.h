/* * (C) Copyright IBM Corporation 1998, 2008. */
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
/* ks_mdep_linux64.h */

#if !defined(_KS_MDEP_LINUX64_H_)
#define _TBS_KS_MDEP_LINUX64_H_

/*
 * Declare types which will select the XDR types for items defined elsewhere.
 * All platforms must define the underlying type for ks_xdr_int32_t and
 * ks_xdr_uint32_t.
 * In addition, each platform must declare what the underlying type is for
 * size_t, time_t, timeval_t, and pid_t.  See xdr_tbs.c for primary usage of
 * these.  
 */
 
/* These types match the pointer types passed and used by the C
   library's implementation of the XDR operation entry points in
   XDR->x_ops.  They should only be used by implementations of XDR
   operations, or by XDR routines which handle inline data, and NEVER
   in any data structure encoded or used by generic ClearCase
   code.  See xdr_stg.c.  */

#define KS_XDR_PUTLONG_ARG_T const long
#define KS_XDR_GETLONG_ARG_T long
#define KS_XDR_PUTBYTE_ARG_T const char *
#define KS_XDR_GETBYTE_ARG_T char *
#define KS_XDR_BYTE_LEN_T unsigned int
#define KS_XDR_INLINE_PTR_TYPE int32_t
#define KS_XDR_INLINE_LEN_TYPE int
#define KS_XDR_GETPOS_CONST const

#define XDR_KS_IN_ADDR XDR_KS_U_INT32

/*
 * Integers large enough to hold a pointer.
 * These types should be redefined in terms of intptr_t and uintptr_t
 * when supported on this platform.
 */ 
typedef long ks_intptr_t;
typedef unsigned long ks_uintptr_t;

/* how to declare/define a native XDR routine (not EZ-RPC!) */
#define KS_XDR_NATIVE_ROUTINE(procname)         \
bool_t                                          \
procname(                                       \
    XDR *xdrs,                                  \
    void *obj_p,                                \
    ...                                         \
)

#define ATRIA_SIZE_T_ULONG
#define ATRIA_TIME_T_LONG
#define KS_TIME_T time_t
#define ATRIA_TIMEVAL_T_INT
#define ATRIA_PID_T_INT
#define ATRIA_TIMEVAL_TV_SEC_TIME_T
#define KS_TIMEVAL_TV_SEC time_t

#if !defined(TRUE)
/* need our own private defs to be the same as Linux headers.  Most
   other UNIX systems don't include parentheses.  Linux <rpc/types.h>
   does, without any #ifndef protection, and cpp complains at
   duplicate definitions if we don't match it.  */
#define TRUE (1)
#define FALSE (0)
#endif

#endif /* !defined(_KS_MDEP_LINUX64_H_) */

/* $Id: 5cb13a36.9c1f11dd.9a62.00:01:83:29:c0:fc $ */
