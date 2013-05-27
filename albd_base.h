/* * (C) Copyright IBM Corporation 1994, 2007. */
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
#ifndef _ALBD_BASE_H_
#define _ALBD_BASE_H_

typedef TBS_HANDLE(albd_rpc_handle_t);


typedef enum albd_protocol_in {
    ALBD_PROTOCOL_UNKNOWN,
    ALBD_PROTOCOL_UDP,
    ALBD_PROTOCOL_TCP,
    ALBD_PROTOCOL_END
} albd_protocol_in_t;
#define ALBD_NUM_PROTOCOL_FAMILIES 2

/*
 * Traits are equally applicable to IPv4 and IPv6; what we really mean
 * by albd_protocol_in_t is the transport type (STREAM vs. DGRAM), not
 * specifically the IPv4 implementations of TCP or UDP.
 */
typedef struct albd_rpc_trait {
    u_long rpc_prog;
    u_long rpc_ver;
    albd_protocol_in_t protocol;
} albd_rpc_trait_t;

/* How many addresses we could use per address family? */
#define ALBD_MAX_ADDRS_PER_AF 1

/*
 * Address family/port pair. 
 */
typedef struct albd_server_port_s {
    int af;
    u_int port;
} albd_server_port_t;

/*
 * I'd prefer to do this as a dynamic array (to make it easier to add
 * future protocols), but that's a bit of a pain with memory
 * allocation, especially for MVFS kernel code.
 */
typedef struct albd_server_port_list_s {
    u_int num_ports;
    albd_server_port_t ports[KS_NUM_ADDRESS_FAMILIES];
} albd_server_port_list_t;

#endif /* _ALBD_BASE_H_ */
/* $Id: 8a01404e.9c1f11dd.9a62.00:01:83:29:c0:fc $ */
