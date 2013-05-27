/* * (C) Copyright IBM Corporation 1992, 2007. */
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
#if !defined(_XDR_KS_H_)
#define _XDR_KS_H_

#include <ks_rpc.h>

/****************************************************************************
 * xdr_credutl_sid_t
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(credutl_sid_t);

/****************************************************************************
 * xdr_ks_canon_pname_p_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(ks_canon_pname_p_t);

/****************************************************************************
 * xdr_ks_off32_t
 * === SUMMARY LINE ===
 * IN	*xdrs
 * IN	*objp
 */
EZ_XDR_ROUTINE(ks_off32_t);

/*
 * Definitions for memory allocations routines used by xdrs
 */ 
#define XDR_KS_MEM_ALLOC(S) KMEM_ALLOC(S, KM_SLEEP)
#define XDR_KS_MEM_FREE KMEM_FREE
#define XDR_KS_MEM_ZERO BZERO
#endif /* _XDR_KS_H_ */

/* $Id: 51b10f16.9c1e11dd.9a62.00:01:83:29:c0:fc $ */
