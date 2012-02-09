/*
 * Copyright (C) 1999, 2005 IBM Corporation.
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
 * Utility functions available to file system implementations.
 * These are functions which might have builtin implementations that
 * get optimized into function calls with -Os; we need them to be real
 * implementations since the Linux kernel doesn't export some of these
 * on some platforms.
 */

#include "vnode_linux.h"

#ifdef __x86_64__
extern int
mdki_memcmp(
    const void *b1,
    const void *b2,
    size_t size
)
{
    return(memcmp(b1, b2, size));
}
#endif
static const char vnode_verid_mvfs_linux_builtins_c[] = "$Id:  b4f00de4.636c11da.8655.00:01:83:a6:4c:63 $";
