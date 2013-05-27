/* * (C) Copyright IBM Corporation 2006, 2008. */
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
#ifndef MVFS_VFSOPS_H_
#define MVFS_VFSOPS_H_

#include "mvfs_base.h"

typedef struct mvfs_vfs_data 
{
    LOCK_T mvfs_mountlock;	/* Serializes mount/unmounts */
    u_long mvfs_mount_count;
    VFS_T **mfs_vobmounts;	/* Table of VOB mounts */
    int mfs_vobmount_hwm;	/* High water mark on table */
} mvfs_vfs_data_t;

#endif /* MVFS_VFSOPS_H_ */
/* $Id: f091542e.9c1e11dd.9a62.00:01:83:29:c0:fc $ */
