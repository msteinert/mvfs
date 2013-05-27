/*
 * Copyright (C) 1999, 2008 IBM Corporation.
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
/*
 * MVFS sysgen parameters
 */

/* #define __NO_VERSION__ */

#include "vnode_linux.h"

/*
 * Device number settings for the MVFS
 * (Users shouldn't adjust them unless directed by a Rational tech support.)
 *
 * mvfs_major: The first major device number used.
 * mvfs_majdynmax: The max number of major devices can be dynamically allocated
 * mvfs_majfixmax: The max number of reserved major devices.
 *
 * By default:
 * The max number of vobs that can be mounted simultaneously:
 * Note: This formula differs from that used by Solaris.  We need to reserve
 * two device numbers, one for /dev/mvfs and one for /view.
 * (mvfs_majdynmax + mvfs_majfixmax) * (number of minor devices) /2 - 2
 * 
 *  Device numbers are 32 bits.  12 for the major, 20 for the minor.
 *  But we limit the number of minor devices we use for mounting vobs
 *  so that we can use the rest of the bits for identifying views.
 *  mvfs_view_shift_bits is the number of bits reserved for vob mounts.
 *  2 * 1024 /2 -2 = 1022
 */

int mvfs_major = 0;   /* Linux will pick one if we specify 0 */

int mvfs_majdynmax = 2;
int mvfs_majfixmax = 0;
u_int mvfs_view_shift_bits = 10;
static const char vnode_verid_mvfs_param_c[] = "$Id:  ad777f26.9c9311dd.9a62.00:01:83:29:c0:fc $";
