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

/* xdr_albd_kernel.c */

/*LINTLIBRARY*/

#include "mvfs_systm.h"
#include <linux/param.h>
#include "albd_rpc_kernel.h"

EZ_XDR_ROUTINE(albd_protocol_in_t)
{
    return xdr_enum(xdrs, (enum_t *)objp);
}

EZ_XDR_ROUTINE(albd_rpc_trait_t)
{
    if (!xdr_u_long(xdrs, &objp->rpc_prog) ||
	!xdr_u_long(xdrs, &objp->rpc_ver) ||
	!xdr_albd_protocol_in_t(xdrs, &objp->protocol EZ_XDR_ARG_PASSTHRU))
    {
	return (FALSE);
    }
    return (TRUE);
}

EZ_XDR_ROUTINE(albd_sockaddr_in_t)
{
    /*
     * albd_sockaddr_in_t is just a struct sockaddr_in, so call
     * that routine from libks (if not in the kernel)
     */

    /*
     * The only thing meaningful is the port number. The internet
     * parts of the address will be different.
     */

    albd_sockaddr_in_t saddr;

    if (xdrs->x_op == XDR_ENCODE) {
	saddr.sin_port = ntohs(objp->sin_port);
	saddr.sin_addr.s_addr = ntohl(objp->sin_addr.s_addr);
    }

    if (!XDR_KS_IN_ADDR(xdrs, &saddr.sin_addr.s_addr) ||
	!xdr_u_short(xdrs, &saddr.sin_port)) {
	return (FALSE);
    }
    if (xdrs->x_op == XDR_DECODE) {
	objp->sin_family = AF_INET;
	objp->sin_zero[0] = objp->sin_zero[1] = 0;
	objp->sin_zero[2] = objp->sin_zero[3] = 0;
	objp->sin_zero[4] = objp->sin_zero[5] = 0;
	objp->sin_zero[6] = objp->sin_zero[7] = 0;
	objp->sin_port = htons(saddr.sin_port);
	objp->sin_addr.s_addr = htonl(saddr.sin_addr.s_addr);
    }
    return (TRUE);
}

EZ_XDR_ROUTINE(albd_server_port_t)
{
    enum ks_addrfamily af;

    if (xdrs->x_op == XDR_ENCODE) {
        switch (objp->af) {
          case AF_INET:
            af = KS_AF_IPV4;
            break;
          case AF_INET6:
            af = KS_AF_IPV6;
            break;
          default:
            return (FALSE);
        }
    }
    if (!xdr_enum(xdrs, (enum_t *)&af) ||
        !xdr_u_int(xdrs, &objp->port))
    {
        return (FALSE);
    }
    if (xdrs->x_op == XDR_DECODE) {
        switch (af) {
          case KS_AF_IPV4:
            objp->af = AF_INET;
            break;
          case KS_AF_IPV6:
            objp->af = AF_INET6;
            break;
          default:
            return (FALSE);
        }
    }
    return (TRUE);
}

EZ_XDR_ROUTINE(albd_server_port_list_t)
{
    int i;

    if (!xdr_u_int(xdrs, &objp->num_ports))
    {
    	return (FALSE);
    }

#define ARRAY_LENGTH(x) (sizeof(x)/sizeof((x)[0]))
#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

    /*
     * We can't encode them all, because some of them may be
     * uninitialized and have address families.
     */
    for (i = 0; i < MIN(objp->num_ports, ARRAY_LENGTH(objp->ports)); i++) {
        if (!xdr_albd_server_port_t(xdrs, &objp->ports[i] EZ_XDR_ARG_PASSTHRU))
            return (FALSE);
    }
    /* on decode, be nice and zero out the remainder */
    if (xdrs->x_op == XDR_DECODE) {
        while (i < ARRAY_LENGTH(objp->ports)) {
            objp->ports[i].af = 0;
            objp->ports[i].port = 0;
            i++;
        }
    }

    return (TRUE);
}

bool_t
xdr_albd_hdr_req_t(xdrs, objp)
     XDR *xdrs;
     albd_hdr_req_t *objp;
{
    if (!xdr_time_t(xdrs, &objp->boot_time EZ_XDR_ARG_TIME_T) ||
	!xdr_u_long(xdrs, &objp->xid)) {
	return (FALSE);
    }
    return (TRUE);
}

bool_t
xdr_albd_hdr_reply_t(xdrs, objp)
     XDR *xdrs;
     albd_hdr_reply_t *objp;
{
    if (!xdr_u_long(xdrs, &objp->xid) ||
	!xdr_tbs_status_t(xdrs, &objp->status EZ_XDR_ARG)) {
	return (FALSE);
    }
    return (TRUE);
}

EZ_XDR_ROUTINE(ks_canon_pname_p_t)
{
	return xdr_string(xdrs, (char **)objp, TBS_MAX_PNAME_LEN);
}

bool_t
xdr_albd_find_server_req_t(xdrs, objp)
     XDR *xdrs;
     albd_find_server_req_t *objp;
{
    if (!xdr_albd_hdr_req_t(xdrs, &objp->hdr) ||
	!xdr_albd_rpc_trait_t(xdrs, &objp->rpc_trait EZ_XDR_ARG) ||
	!xdr_tbs_uuid_t(xdrs, &objp->uuid EZ_XDR_ARG) ||
	!xdr_ks_canon_pname_p_t(xdrs, &objp->path EZ_XDR_ARG)) {
	return (FALSE);
    }
    return (TRUE);
}

bool_t
xdr_albd_find_server_v70_reply_t(
     XDR *xdrs,
     albd_find_server_v70_reply_t *objp
)
{
    if (!xdr_albd_hdr_reply_t(xdrs, &objp->hdr)) {
        return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
        return (TRUE);
    }
    if (!xdr_time_t(xdrs, &objp->coming_up EZ_XDR_ARG_TIME_T)) {
        return (FALSE);
    }
    if (objp->coming_up != 0) {
        return (TRUE);
    }
    if (!xdr_tbs_uuid_t(xdrs, &objp->uuid EZ_XDR_ARG) ||
        !xdr_ks_canon_pname_p_t(xdrs, &objp->path EZ_XDR_ARG)) {
        return (FALSE);
    }
    if (!xdr_albd_sockaddr_in_t(xdrs, &objp->saddr EZ_XDR_ARG)) {
    	return (FALSE);
    }
    
    return (TRUE);
}

bool_t
xdr_albd_find_server_reply_t(
     XDR *xdrs,
     albd_find_server_reply_t *objp
)
{
    if (!xdr_albd_hdr_reply_t(xdrs, &objp->hdr)) {
        return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
        return (TRUE);
    }
    if (!xdr_time_t(xdrs, &objp->coming_up EZ_XDR_ARG_TIME_T)) {
        return (FALSE);
    }
    if (objp->coming_up != 0) {
        return (TRUE);
    }
    if (!xdr_tbs_uuid_t(xdrs, &objp->uuid EZ_XDR_ARG) ||
        !xdr_ks_canon_pname_p_t(xdrs, &objp->path EZ_XDR_ARG)) {
        return (FALSE);
    }	
    if (!xdr_albd_server_port_list_t(xdrs, &objp->port_list EZ_XDR_ARG)) {
	 return (FALSE);
    }
    
    return (TRUE);
}

static const char vnode_verid_xdr_albd_kernel_c[] = "$Id:  ed3e0c0a.145d11d7.81b6.00:50:da:ba:19:c8 $";
