/*
 * Copyright (C) 1999, 2003 IBM Corporation.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA
 *
 * Author: IBM Corporation
 * This module is part of the IBM (R) Rational (R) ClearCase (R)
 * Multi-version file system (MVFS).
 * For support, please visit http://www.ibm.com/software/support
 */
#include "vnode_linux.h"

/* Address space operations for MVFS */

/* We used to have stub mapping operations.  All they would do would be
 * to report an error.  However, even the existence of these functions
 * would make sendfile think that it would work on our files.  By not
 * having the functions declared at all, we cause it to find out sooner.
 */

struct address_space_operations mvfs_addrspace_ops = {
};

static const char vnode_verid_mvfs_linux_asops_c[] = "$Id:  8ed00c54.636c11da.8655.00:01:83:a6:4c:63 $";
