/* * (C) Copyright IBM Corporation 1990, 2009. */
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
#ifndef MVFS_H_
#define MVFS_H_

#include "mvfs_base.h"

#include "mvfs_common.h"
#include "mvfs_vfsops.h"
#include "mvfs_mnode.h"
#include "mvfs_dnc.h"
#include "mvfs_viewroot.h"
#include "mvfs_audit.h"
/* ... Add sub system header files here */

extern mvfs_common_data_t mvfs_common_data_var;
extern mvfs_vfs_data_t mvfs_vfs_data_var;
extern mvfs_mnode_data_t mvfs_mnode_data_var;
extern mvfs_dnlc_data_t mvfs_dnlc_data_var;
extern mvfs_viewroot_data_t mvfs_viewroot_data_var;
extern mvfs_stats_data_t **mvfs_stats_data_ptr_percpu;
extern mvfs_audit_data_t mvfs_audit_data_var;
/* more data to be added */

#endif /* MVFS_H_ */
/* $Id: 656913fd.b44911de.8ddb.00:01:83:29:c0:fc $ */
