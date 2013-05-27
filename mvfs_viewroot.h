/* * (C) Copyright IBM Corporation 2006, 2007. */
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
#ifndef MVFS_VIEWROOT_H_
#define MVFS_VIEWROOT_H_

#include "mvfs_base.h"

/* All viewroot subsystem data is now contained in this structure. */
typedef struct mvfs_viewroot_data 
{

    VFS_T *mfs_viewroot_vfsp;
    VNODE_T *mfs_viewroot_vp;
    VNODE_T *mfs_viewroot_specvp;
    LOCK_T mvfs_mkviewtag_lock;

} mvfs_viewroot_data_t;

#endif /* MVFS_VIEWROOT_H_ */
/* $Id: cb914d6e.9c1e11dd.9a62.00:01:83:29:c0:fc $ */
