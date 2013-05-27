/* * (C) Copyright IBM Corporation 1991, 2008. */
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
/* credutl_kernel.c */

#include "mvfs_systm.h"
#include "mvfs.h"
#include <credutl_kernel.h>

#define	CREDUTL_ASSERT ASSERT
#define CREDUTL_MEM_ZERO BZERO

/*
 * UNIX platforms can only ever handle UNIX style SIDs
 */
#define CREDUTL_UNIX_SID_SUPPORT

/*
 * Internal sanity number for UNIX SIDs to verify uids and gids
 * are of the right type (not to be used for any exported routines).
 */
#define CREDUTL_SID_UNIX_UID 1
#define CREDUTL_SID_UNIX_GID 2

/*
 * Setup "Nobody" and "DontCare" as special SIDs since we can not translate to
 * an NT SID representation of nobody/dontcare.  
 */
EXPORT_DATA
const
credutl_sid_t CREDUTL_SID_NOBODY = {
    2,
    CREDUTL_SID_TYPE_NOBODY
};

EXPORT_DATA
const
credutl_sid_t CREDUTL_SID_DONTCARE = {
    2,
    CREDUTL_SID_TYPE_DONTCARE
};

STATIC ks_uint32_t
get_int32(const char *s)
{
    ks_uint32_t uid;

    uid = (*s++ << 24) & 0xff000000;
    uid |= (*s++ << 16) & 0x00ff0000;
    uid |= (*s++ << 8) & 0x0000ff00;
    uid |= *s++ & 0x000000ff;

    return(uid);
}

/******************************************************************
 * credutl_unix_id_to_sid
 * Converts a 32-bit UNIX UID or GID to a credutl-style SID.
 */

STATIC
void
credutl_unix_id_to_sid(
    uid_t id,
    credutl_sid_t *sid_p,
    char unix_id_type
)
{
    int i = 0;
    char *sp = &sid_p->sid[0];

    sid_p->type = CREDUTL_SID_TYPE_UNIX;
    
    sp[i++] = (char)((id >> 24) & 0xff);
    sp[i++] = (char)((id >> 16) & 0xff);
    sp[i++] = (char)((id >> 8) & 0xff);
    sp[i++] = (char)(id & 0xff);
    sp[i++] = unix_id_type;
 
    sid_p->length = (char)(CREDUTL_SID_BASE_SIZE(sid_p) + i);
}

/******************************************************************
 * credutl_unix_uid_to_sid
 * Converts a 32-bit UNIX UID to a credutl-style SID.
 */

void
credutl_unix_uid_to_sid(
    uid_t uid,
    credutl_sid_t *sid_p
)
{
    credutl_unix_id_to_sid(uid, sid_p, CREDUTL_SID_UNIX_UID);
}

/******************************************************************
 * credutl_unix_gid_to_sid
 * Converts a 32-bit UNIX GID to a credutl-style SID.
 */

void
credutl_unix_gid_to_sid(
    gid_t gid,
    credutl_sid_t *sid_p
)
{
    credutl_unix_id_to_sid(gid, sid_p, CREDUTL_SID_UNIX_GID);
}

/******************************************************************
 * credutl_sid_to_unix_uid
 * Converts a credutl-style SID to a 32-bit UNIX UID.
 */

credutl_uid_t
credutl_sid_to_unix_uid(const credutl_sid_t *sid_p)
{
    int i = 0;
    const char *sp = &sid_p->sid[0];
    uid_t uid;
    char unix_id_type;

    CREDUTL_ASSERT(sid_p->type == CREDUTL_SID_TYPE_UNIX);
    
    uid = get_int32(&sp[i]);
    i += 4;	/* advance past the 4 bytes (32 bits) of the uid */

    unix_id_type = sp[i++];
 
    /* CREDUTL_ASSERT(unix_id_type == CREDUTL_SID_UNIX_UID); */
    if (unix_id_type != CREDUTL_SID_UNIX_UID)
        return KS_UID_NOBODY;

    /* CREDUTL_ASSERT(sid_p->length == i + CREDUTL_SID_BASE_SIZE(sid_p)); */
    if (sid_p->length != i + CREDUTL_SID_BASE_SIZE(sid_p))
        return KS_UID_NOBODY;

    return(uid);
}

/******************************************************************
 * credutl_sid_to_unix_gid
 * Converts a credutl-style SID to a 32-bit UNIX GID.
 */

credutl_gid_t
credutl_sid_to_unix_gid(const credutl_sid_t *sid_p)
{
    int i = 0;
    const char *sp = &sid_p->sid[0];
    gid_t gid;
    char unix_id_type;

    CREDUTL_ASSERT(sid_p->type == CREDUTL_SID_TYPE_UNIX);
    
    gid = (sp[i++] << 24) & 0xff000000;
    gid |= (sp[i++] << 16) & 0x00ff0000;
    gid |= (sp[i++] << 8) & 0x0000ff00;
    gid |= sp[i++] & 0x000000ff;

    unix_id_type = sp[i++];
 
    /* CREDUTL_ASSERT(unix_id_type == CREDUTL_SID_UNIX_GID); */
    if (unix_id_type != CREDUTL_SID_UNIX_GID)
        return KS_GID_NOBODY;

    /* CREDUTL_ASSERT(sid_p->length == i + CREDUTL_SID_BASE_SIZE(sid_p)); */
    if (sid_p->length != i + CREDUTL_SID_BASE_SIZE(sid_p))
        return KS_GID_NOBODY;

    return(gid);
}

static const char vnode_verid_credutl_kernel_c[] = "$Id:  e9ae0bf1.145d11d7.81b6.00:50:da:ba:19:c8 $";
