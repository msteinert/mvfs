/* * (C) Copyright IBM Corporation 1991, 2009. */
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
#if !defined(_CREDUTL_KERNEL_H_)
#define _CREDUTL_KERNEL_H_

#include <linux/types.h>
#include <linux/stat.h>

/* FIXME When we look at ks_base.h we may need to fix this.
 * to use #ifdef KERNEL to select the kernel only version.
 */
#include <ks_base.h>

#ifdef __cplusplus
extern "C" {
#endif

/* The credentials structure encapsulates the POSIX notion of an
 * identity: the user ID, group ID, and group list.
 * Unlike POSIX, we maintain the invariants that the group ID
 * is *never* in the group list, and that no group ID ever
 * appears more than once in the group list.
 */
typedef struct credutl_credentials_t {
    credutl_uid_t uid;			/* user ID */
    credutl_gid_t gid;			/* group ID */
    int ngroups;			/* number of groups in list */
    credutl_gid_t group_list[CREDUTL_NGROUPS_MAX];
} credutl_credentials_t;


/******************************************************************
 * credutl_unix_uid_to_sid
 * Converts a 32-bit UNIX UID to a credutl-style SID.
 */

EXTERN void
credutl_unix_uid_to_sid(
    uid_t uid,
    credutl_sid_t *sid_p
);

/******************************************************************
 * credutl_unix_gid_to_sid
 * Converts a 32-bit UNIX GID to a credutl-style SID.
 */

EXTERN void
credutl_unix_gid_to_sid(
    gid_t gid,
    credutl_sid_t *sid_p
);

/******************************************************************
 * credutl_sid_to_unix_uid
 * Converts a credutl-style SID to a 32-bit UNIX GID.
 */

EXTERN credutl_uid_t
credutl_sid_to_unix_uid(const credutl_sid_t *sid_p);

/******************************************************************
 * credutl_sid_to_unix_gid
 * Converts a credutl-style SID to a 32-bit UNIX GID.
 */

EXTERN credutl_gid_t
credutl_sid_to_unix_gid(const credutl_sid_t *sid_p);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /*_CREDUTL_KERNEL_H_*/
/* $Id: 40e3ad97.a23a11df.8bc7.00:01:84:7a:f2:e4 $ */
