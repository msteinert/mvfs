/* * (C) Copyright IBM Corporation 1992, 2005. */
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
/* xdr_ks_kernel.c */
/*LINTLIBRARY*/

#include "ks_base.h"

#include "mvfs_systm.h"

#include "xdr_ks.h"

/****************************************************************************
 * xdr_ks_char_t
 * const char safe version of xdr_char().
 * On some platforms, xdr_char() may reassign the input char variable
 * while doing XDR_ENCODE.
 * IN	*xdrs
 * IN	*objp
 */
typedef char ks_char_t;

EZ_XDR_ROUTINE(ks_char_t)
{
    int i;

    if (xdrs->x_op == XDR_ENCODE)
        i = (*objp);

    if (!xdr_int(xdrs, &i))
        return (FALSE);

    if (xdrs->x_op == XDR_DECODE)
        *objp = (ks_char_t)i;

    return (TRUE);
}

EZ_XDR_ROUTINE(credutl_sid_t)
{
    u_int length;

    if (xdrs->x_op == XDR_DECODE)
        XDR_KS_MEM_ZERO(objp, sizeof *objp);

    if (!xdr_ks_char_t(xdrs, &objp->length EZ_XDR_ARG_PASSTHRU))
        return(FALSE);

    if (objp->length == 0)
        return(TRUE);

    if (!xdr_ks_char_t(xdrs, &objp->type EZ_XDR_ARG_PASSTHRU))
        return(FALSE);

    length = objp->length - CREDUTL_SID_BASE_SIZE(objp);
    if (length > CREDUTL_MAX_SID_LENGTH)
        return(FALSE);

    if (!xdr_opaque(xdrs, objp->sid, length))
        return(FALSE);
    return(TRUE);
}

EZ_XDR_ROUTINE(ks_off32_t)
{
    return xdr_int(xdrs, (int *)objp);
}

static const char vnode_verid_xdr_ks_kernel_c[] = "$Id:  291e2f2a.146311d7.8dbb.00:01:80:ae:c2:81 $";
