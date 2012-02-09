/* * (C) Copyright IBM Corporation 1991, 2005. */
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

#if !defined(_KS_RPC_H_)
#define _KS_RPC_H_

#include <linux/types.h>

#include <linux/socket.h>

#include "linux_krpc.h"                 /* our own custom hacks */

#define EZ_XDR_ROUTINE_TYPE(name,type)          \
EZ_EXTERN bool_t                                \
xdr_##name(                                     \
    XDR *xdrs,                                  \
    type objp                                   \
)
#define EZ_XDR_ARGDECL /**/
#define EZ_XDR_ARG /**/
#define EZ_XDR_ARG_PASSTHRU /**/
#define EZ_XDR_ARG_TIME_T /**/
#define EZ_XDR_ROUTINE(name) EZ_XDR_ROUTINE_TYPE(name,name *)

#define EZ_EXTERN EXTERN

#endif /* _KS_RPC_H */
/* $Id: a8c2a4e4.637911da.8655.00:01:83:a6:4c:63 $ */
