/* * (C) Copyright IBM Corporation 1994, 2005. */
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

typedef struct albd_rpc_trait {
    u_long rpc_prog;
    u_long rpc_ver;
    albd_protocol_in_t protocol;
} albd_rpc_trait_t;
#endif /* _ALBD_BASE_H_ */
/* $Id: ed02bb74.637a11da.8655.00:01:83:a6:4c:63 $ */
