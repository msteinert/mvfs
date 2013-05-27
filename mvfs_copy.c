/* * (C) Copyright IBM Corporation 1998, 2010. */
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
/* mvfs_copy.c */


#include "mvfs_systm.h"
#include <tbs_base.h>
#include <tbs_errno.h>
#include "mvfs.h"
#include "mvfs_dnc.h"
#include "mvfs_copy.h"

#include "mvfs_transtype.h"

int
CopyInMfs_strbufpn(
    caddr_t uargp,
    struct mfs_strbufpn *kargp,
    MVFS_CALLER_INFO *callinfo
)
{

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mfs_strbufpn_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mfs_strbufpn_32));
		if (!res)
			mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32, kargp);
		return res;
	}
#endif
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mfs_strbufpn)));
}

int
CopyOutMfs_strbufpn(
    struct mfs_strbufpn *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mfs_strbufpn_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mfs_strbufpn_to_mfs_strbufpn_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mfs_strbufpn_32)));
	}
#endif
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mfs_strbufpn)));
}

int
CopyInMfs_strbufpn_index(
    caddr_t uargp,
    struct mfs_strbufpn *kargp,
    int index,
    MVFS_CALLER_INFO *callinfo
)
{
    caddr_t data_ptr;

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
    int res;
    struct mfs_strbufpn_32 vbl_32;

    data_ptr = (caddr_t) (uargp + ((sizeof(struct mfs_strbufpn_32)) * index));

    if (MDKI_CALLER_IS_32BIT(callinfo)) {
        res = COPYIN(data_ptr, (caddr_t)&vbl_32, sizeof(struct mfs_strbufpn_32));
        if (!res)
            mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32, kargp);
        return res;
     }
#endif
     data_ptr = (caddr_t) (uargp + ((sizeof(struct mfs_strbufpn)) * index));
     return(COPYIN(data_ptr, (caddr_t)kargp, sizeof(struct mfs_strbufpn)));
}

int
CopyOutMfs_strbufpn_index(
    struct mfs_strbufpn *kargp,
    caddr_t uargp,
    int index,
    MVFS_CALLER_INFO *callinfo
)
{
    caddr_t data_ptr;

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
    struct mfs_strbufpn_32 vbl_32;

    data_ptr = (caddr_t) (uargp + ((sizeof(struct mfs_strbufpn_32)) * index));

    if (MDKI_CALLER_IS_32BIT(callinfo)) {
        mfs_strbufpn_to_mfs_strbufpn_32(kargp, &vbl_32);
        return (COPYOUT((caddr_t)&vbl_32, data_ptr, sizeof(struct mfs_strbufpn_32)));
    }
#endif
    data_ptr = (caddr_t) (uargp + ((sizeof(struct mfs_strbufpn)) * index));
    return (COPYOUT((caddr_t)kargp, data_ptr, sizeof(struct mfs_strbufpn)));
}

int
CopyInMfs_strbufpn_pair(
    caddr_t uargp,
    struct mfs_strbufpn_pair *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mfs_strbufpn_pair_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mfs_strbufpn_pair_32));
		if (!res)
			mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(&vbl_32, kargp);
		return res;
	}
#endif
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mfs_strbufpn_pair)));
}

int
CopyOutMfs_strbufpn_pair(
    struct mfs_strbufpn_pair *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mfs_strbufpn_pair_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mfs_strbufpn_pair_to_mfs_strbufpn_pair_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mfs_strbufpn_pair_32)));
	}
#endif

	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mfs_strbufpn_pair)));
}

int
CopyInMfs_strbuf(
    caddr_t uargp,
    struct mfs_strbuf *kargp,
    MVFS_CALLER_INFO *callinfo
)
{

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mfs_strbuf_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mfs_strbuf_32));
		if (!res)
			mfs_strbuf_32_to_mfs_strbuf(&vbl_32, kargp);
		return res;
	}
#endif

	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mfs_strbuf)));
}

int
CopyOutMfs_strbuf(
    struct mfs_strbuf *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mfs_strbuf_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mfs_strbuf_to_mfs_strbuf_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mfs_strbuf_32)));
	}
#endif
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mfs_strbuf)));
}

int
CopyInMfs_mntargs(
    caddr_t uargp,
    struct mfs_mntargs *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
            mfs_mntargs_32_to_mfs_mntargs((struct mfs_mntargs_32 *)uargp,
                                          kargp);
            return(0);
        }
#endif /* LP64 */
        BCOPY(uargp, (caddr_t)kargp, sizeof(struct mfs_mntargs));
        return(0);
}

int
CopyInMvfscmd_block(
    caddr_t uargp,
    struct mvfscmd_block *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfscmd_block_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfscmd_block_32));
		if (!res)
			mvfscmd_block_32_to_mvfscmd_block(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfscmd_block)));
}

int
CopyOutMvfscmd_block(
    struct mvfscmd_block *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfscmd_block_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfscmd_block_to_mvfscmd_block_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfscmd_block_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfscmd_block)));
}
int
CopyOutMvfscmd_block_status(
    struct mvfscmd_block *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
	    struct mvfscmd_block_32 *vbl_32;

		/*
         * Note: the status fields are the same size so we can
         * just do a CopyOut.
         */

        vbl_32 = (struct mvfscmd_block_32 *) uargp;  
		return (COPYOUT((caddr_t)&kargp->status, (caddr_t) &vbl_32->status, 
                 sizeof(vbl_32->status)));
	}
#endif /* ATRIA_LP64 */
	{
	struct mvfscmd_block *data = (struct mvfscmd_block *) uargp;

	return(COPYOUT((caddr_t)&kargp->status, (caddr_t)&data->status, 
		  sizeof(kargp->status)));
	}
}

int
CopyInMvfs_loginfo(
    caddr_t uargp,
    struct mvfs_loginfo *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_loginfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_loginfo_32));
		if (!res)
			mvfs_loginfo_32_to_mvfs_loginfo(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_loginfo)));
}

int
CopyOutMvfs_loginfo(
    struct mvfs_loginfo *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_loginfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_loginfo_to_mvfs_loginfo_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_loginfo_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_loginfo)));
}

int
CopyInMvfs_viewtag_info(
    caddr_t uargp,
    struct mvfs_viewtag_info *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_viewtag_info_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_viewtag_info_32));
		if (!res)
			mvfs_viewtag_info_32_to_mvfs_viewtag_info(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_viewtag_info)));
}

int
CopyInMvfs_mkviewtag_info(
    caddr_t uargp,
    struct mvfs_mkviewtag_info *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_mkviewtag_info_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_mkviewtag_info_32));
		if (!res)
			mvfs_mkviewtag_info_32_to_mvfs_mkviewtag_info(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_mkviewtag_info)));
}

int
CopyInMvfs_viewinfo(
    caddr_t uargp,
    struct mvfs_viewinfo *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_viewinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_viewinfo_32));
		if (!res)
			mvfs_viewinfo_32_to_mvfs_viewinfo(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_viewinfo)));
}

int
CopyOutMvfs_viewinfo(
    struct mvfs_viewinfo *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_viewinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_viewinfo_to_mvfs_viewinfo_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_viewinfo_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_viewinfo)));
}

int
CopyInMvfs_vobinfo(
    caddr_t uargp,
    struct mvfs_vobinfo *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_vobinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_vobinfo_32));
		if (!res)
			mvfs_vobinfo_32_to_mvfs_vobinfo(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_vobinfo)));
}

int
CopyOutMvfs_vobinfo(
    struct mvfs_vobinfo *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_vobinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_vobinfo_to_mvfs_vobinfo_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_vobinfo_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_vobinfo)));
}

int
CopyInMvfs_ioget_poolmaps(
    caddr_t uargp,
    struct mvfs_ioget_poolmaps *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	int res;
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_ioget_poolmaps_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_ioget_poolmaps_32));
		if (!res)
			mvfs_ioget_poolmaps_32_to_mvfs_ioget_poolmaps(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_ioget_poolmaps)));
}
int
CopyOutMvfs_ioget_poolmaps(
    struct mvfs_ioget_poolmaps *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
	int res;
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_ioget_poolmaps_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_ioget_poolmaps_to_mvfs_ioget_poolmaps_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_ioget_poolmaps_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_ioget_poolmaps)));
}

int
CopyInMvfs_iovfh(
    caddr_t uargp,
    struct mvfs_iovfh *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_iovfh_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_iovfh_32));
		if (!res)
			mvfs_iovfh_32_to_mvfs_iovfh(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_iovfh)));
}

int
CopyOutMvfs_iovfh(
    struct mvfs_iovfh *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_iovfh_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_iovfh_to_mvfs_iovfh_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_iovfh_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_iovfh)));
}

int
BcopyInMvfscmd_block(
    caddr_t kdatap,
    struct mvfscmd_block *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	int res = 0;
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfscmd_block_32_to_mvfscmd_block((struct mvfscmd_block_32 *)kdatap, 
			kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	BCOPY(kdatap, (caddr_t) kargp, sizeof(struct mvfscmd_block));
	return (res);
}
int
BcopyOutMvfscmd_block(
    struct mvfscmd_block *kdatap,
    caddr_t kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	int res = 0;
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfscmd_block_32 *vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfscmd_block_to_mvfscmd_block_32(kdatap, 
			(struct mvfscmd_block_32 *)kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	BCOPY((caddr_t)kdatap, kargp, sizeof(struct mvfscmd_block));
	return res;
}

int
BcopyOutMvfscmd_block_status(
    struct mvfscmd_block *kdatap,
    caddr_t kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	int res = 0;
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		((struct mvfscmd_block_32 *)kargp)->status = kdatap->status;
		return res;
	}
#endif /* ATRIA_LP64 */
	{
	struct mvfscmd_block *data = (struct mvfscmd_block *) kargp;

	BCOPY((caddr_t)&kdatap->status, (caddr_t)&data->status, 
		  sizeof(kdatap->status));
	}
	return res;
}

int 
CopyInMvfs_splitpool(
    caddr_t uargp,
    struct mvfs_splitpool *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_splitpool_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_splitpool_32));
		if (!res)
			mvfs_splitpool_32_to_mvfs_splitpool(&vbl_32, kargp);
		return res;
	}
#endif
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_splitpool)));
}
int
CopyInMvfs_splitpool_index(
    caddr_t uargp,
    struct mvfs_splitpool *kargp,
    int index,
    MVFS_CALLER_INFO *callinfo
)
{
	caddr_t splitdata;
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_splitpool_32 vbl_32;

	splitdata = (caddr_t) (uargp + ((sizeof(struct mvfs_splitpool_32)) * index));

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(splitdata, (caddr_t)&vbl_32, sizeof(struct mvfs_splitpool_32));
		if (!res)
			mvfs_splitpool_32_to_mvfs_splitpool(&vbl_32, kargp);
		return res;
	}
#endif
	splitdata = uargp + ((sizeof(struct mvfs_splitpool)) * index);
	return(COPYIN(splitdata, (caddr_t)kargp, sizeof(struct mvfs_splitpool)));
}

int
CopyOutView_vstat(
    struct view_vstat *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
        int res;
	struct view_vstat_32 *vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
                /*
                 * structure is large so let's malloc space
                 */
                if ((vbl_32 = KMEM_ALLOC(sizeof(struct view_vstat_32), KM_SLEEP))
                        == NULL)
                {
                        return(ENOMEM);
                }
		view_vstat_to_view_vstat_32(kargp, vbl_32);
		res = (COPYOUT((caddr_t)vbl_32, uargp, sizeof(struct view_vstat_32)));
                KMEM_FREE(vbl_32, sizeof(struct view_vstat_32));
                return res;
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct view_vstat)));
}

int
CopyInMvfs_xstat(
    caddr_t uargp,
    struct mvfs_xstat *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_xstat_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_xstat_32));
		if (!res)
			mvfs_xstat_32_to_mvfs_xstat(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_xstat)));
}

int
CopyOutMvfs_xstat(
    struct mvfs_xstat *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_xstat_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_xstat_to_mvfs_xstat_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_xstat_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_xstat)));
}

int
CopyInMvfs_ioinval(
    caddr_t uargp,
    struct mvfs_ioinval *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_ioinval_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_ioinval_32));
		if (!res)
			mvfs_ioinval_32_to_mvfs_ioinval(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_ioinval)));
}

int
CopyInMvfs_clrname_info(
    caddr_t uargp,
    struct mvfs_clrname_info *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_clrname_info_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_clrname_info_32));
		if (!res)
			mvfs_clrname_info_32_to_mvfs_clrname_info(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_clrname_info)));
}

int
CopyOutMvfs_clrname_info(
    struct mvfs_clrname_info *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_clrname_info_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_clrname_info_to_mvfs_clrname_info_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_clrname_info_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_clrname_info)));
}

int
CopyInMvfs_viewaddr(
    caddr_t uargp,
    struct mvfs_viewaddr *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	/* A struct mvfs_viewaddr is currently a ks_sockaddr_storage_t (a union
	** type), each of whose constituents is the same length on all
	** platforms in both 32-bit and 64-bit environments.  Thus, we don't
	** need to do anything special for it.
	*/
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_viewaddr)));
}

int
CopyOutMvfs_viewaddr(
    struct mvfs_viewaddr *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
	/* See the comment above. */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_viewaddr)));
}

int
CopyInMvfs_iochange_mtype(
    caddr_t uargp,
    struct mvfs_iochange_mtype *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_iochange_mtype_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_iochange_mtype_32));
		if (!res)
			mvfs_iochange_mtype_32_to_mvfs_iochange_mtype(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_iochange_mtype)));
}

int
CopyInMfs_ioncent(
    caddr_t uargp,
    struct mfs_ioncent *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mfs_ioncent_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
            res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mfs_ioncent_32));
            if (!res)
                mfs_ioncent_32_to_mfs_ioncent(&vbl_32, kargp);
            return res;
	} else {
            return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mfs_ioncent)));
        }
#else
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mfs_ioncent)));
#endif /* ATRIA_LP64 */
}

int
CopyOutMfs_ioncent(
    struct mfs_ioncent *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mfs_ioncent_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
            mfs_ioncent_to_mfs_ioncent_32(kargp, &vbl_32);
            return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mfs_ioncent_32)));
	} else {
            return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mfs_ioncent)));
        }
#else
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mfs_ioncent)));
#endif /* ATRIA_LP64 */
}

int
CopyInMvfs_bhinfo(
    caddr_t uargp,
    struct mvfs_bhinfo *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_bhinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_bhinfo_32));
		if (!res)
			mvfs_bhinfo_32_to_mvfs_bhinfo(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_bhinfo)));
}

int
CopyOutMvfs_bhinfo(
    struct mvfs_bhinfo *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_bhinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_bhinfo_to_mvfs_bhinfo_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_bhinfo_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_bhinfo)));
}

int
CopyInMvfs_statbufs(
    caddr_t uargp,
    struct mvfs_statbufs *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_statbufs_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_statbufs_32));
		if (!res)
			mvfs_statbufs_32_to_mvfs_statbufs(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_statbufs)));
}

int
CopyInMvfs_io_xattr(
    caddr_t uargp,
    struct mvfs_io_xattr *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_io_xattr_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_io_xattr_32));
		if (!res)
			mvfs_io_xattr_32_to_mvfs_io_xattr(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_io_xattr)));
}

int
CopyOutMvfs_io_xattr(
    struct mvfs_io_xattr *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_io_xattr_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_io_xattr_to_mvfs_io_xattr_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_io_xattr_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_io_xattr)));
}

int
CopyInMvfs_export_viewinfo(
    caddr_t uargp,
    struct mvfs_export_viewinfo *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_export_viewinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_export_viewinfo_32));
		if (!res)
			mvfs_export_viewinfo_32_to_mvfs_export_viewinfo(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_export_viewinfo)));
}

int
CopyOutMvfs_export_viewinfo(
    struct mvfs_export_viewinfo *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_export_viewinfo_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_export_viewinfo_to_mvfs_export_viewinfo_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(struct mvfs_export_viewinfo_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_export_viewinfo)));
}

int
CopyInMvfs_cache_sizes(
    caddr_t uargp,
    struct mvfs_cache_sizes *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_cache_sizes)));
}

int
CopyOutMvfs_cache_sizes(
    struct mvfs_cache_sizes *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
    return(COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_cache_sizes)));
}

int
CopyOutMvfs_cache_usage(
    struct mvfs_cache_usage *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
	/*
	 * Needs no translation for 64 bit support
	 */

	return (COPYOUT((caddr_t)kargp, uargp, sizeof(struct mvfs_cache_usage)));
}
int
CopyInTbs_uuid_s(
    caddr_t uargp,
    struct tbs_uuid_s *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct tbs_uuid_s_32 vbl_32;

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(vbl_32));
		if (!res)
			tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct tbs_uuid_s)));
}
int
CopyInTbs_oid_s(
    caddr_t uargp,
    struct tbs_oid_s *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct tbs_oid_s_32 vbl_32;

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(vbl_32));
		if (!res)
			tbs_oid_s_32_to_tbs_oid_s(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct tbs_oid_s)));
}

int
CopyOutMfs_clntstat(
    struct mfs_clntstat *kargp,
    caddr_t uargp,
    size_t max_len,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
        struct mfs_clntstat_32 vbl_32;
        if (MDKI_CALLER_IS_32BIT(callinfo)) {
                mfs_clntstat_to_mfs_clntstat_32(kargp, &vbl_32);
                return (COPYOUT((caddr_t)&vbl_32, uargp,
                                KS_MIN(max_len, sizeof(struct mfs_clntstat_32))));
        } else {
                return (COPYOUT((caddr_t)kargp, uargp,
                                KS_MIN(max_len, sizeof(struct mfs_clntstat))));
        }
#else
        return (COPYOUT((caddr_t)kargp, uargp,
                        KS_MIN(max_len, sizeof(struct mfs_clntstat))));
#endif /* ATRIA_LP64 */
}

int
CopyOutMfs_clearstat(
    struct mfs_clearstat *kargp,
    caddr_t uargp,
    size_t max_len,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mfs_clearstat_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mfs_clearstat_to_mfs_clearstat_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, 
				KS_MIN(max_len, sizeof(struct mfs_clearstat_32))));
	} else {
                return (COPYOUT((caddr_t)kargp, uargp,
                                KS_MIN(max_len, sizeof(struct mfs_clearstat))));
        }
#else
        return (COPYOUT((caddr_t)kargp, uargp,
                        KS_MIN(max_len, sizeof(struct mfs_clearstat))));
#endif /* ATRIA_LP64 */
}

int
CopyOutMfs_austat(
    struct mfs_austat *kargp,
    caddr_t uargp,
    size_t max_len,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mfs_austat_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mfs_austat_to_mfs_austat_32(kargp, &vbl_32);
		return (COPYOUT((caddr_t)&vbl_32, uargp, 
				KS_MIN(max_len, sizeof(struct mfs_austat_32))));
	} else {
                return (COPYOUT((caddr_t)kargp, uargp,
                                KS_MIN(max_len, sizeof(struct mfs_austat))));
        }
#else
        return (COPYOUT((caddr_t)kargp, uargp,
                        KS_MIN(max_len, sizeof(struct mfs_austat))));
#endif /* ATRIA_LP64 */
}

int
CopyOutMfs_rpchist(
    struct mfs_rpchist *kargp,
    caddr_t uargp,
    size_t max_len,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		struct mfs_rpchist_32 *vbl_32;
		int res;

		/*
		 * structure is large so let's malloc space
		 */
		if ((vbl_32 = KMEM_ALLOC(sizeof(struct mfs_rpchist_32), KM_SLEEP)) 
			== NULL) 
		{
			return(ENOMEM);
		}
		mfs_rpchist_to_mfs_rpchist_32(kargp, vbl_32);
		res = COPYOUT((caddr_t)vbl_32, uargp, KS_MIN(max_len, 
						sizeof(struct mfs_rpchist_32)));
		KMEM_FREE(vbl_32, sizeof(struct mfs_rpchist_32));
		return(res);
	} else {
                return (COPYOUT((caddr_t)kargp, uargp, KS_MIN(max_len,
                                                sizeof(struct mfs_rpchist))));
        }
#else
        return (COPYOUT((caddr_t)kargp, uargp, KS_MIN(max_len,
                                        sizeof(struct mfs_rpchist))));
#endif /* ATRIA_LP64 */
}

int 
CopyOuttimestruc_array(
    timestruc_t *kargp,
    caddr_t uargp,
    int no_entries,
    size_t max_len,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		struct timestruc_32 vbl_32;
		struct timestruc_32 *uvbl_32;
		timestruc_t *kvbl;
		int i; 
		int res = 0;
		long len = 0;

		uvbl_32 = (struct timestruc_32 *) uargp;
		kvbl = kargp;

		for ( i = 0 ; i < no_entries && len < max_len ; 
			i++, kvbl++, uvbl_32++, len += sizeof(struct timestruc_32)) 
		{
			mfs_timestruc_to_mfs_timestruc_32(kvbl, &vbl_32);
			if ((res = COPYOUT(&vbl_32, (caddr_t)uvbl_32, 
					sizeof(struct timestruc_32))) != 0)
				return (res);
		}
		return res;
	} else {
                return(COPYOUT((caddr_t)kargp, uargp, KS_MIN(max_len, sizeof(timestruc_t) * no_entries)));
        }
#else
        return(COPYOUT((caddr_t)kargp, uargp, KS_MIN(max_len, sizeof(timestruc_t) * no_entries)));
#endif /* ATRIA_LP64 */
}

int
CopyInMvfs_pointer(
    caddr_t uargp,
    char **kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	ptr32_t vbl_32;

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(vbl_32));
		if (!res)
			*kargp = PTR32_TO_PTR(vbl_32);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(char *)));
}

int
CopyInMvfs_u_long(
    caddr_t uargp,
    u_long *kargp,
    MVFS_CALLER_INFO *callinfo
)
{

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		ks_uint32_t vbl_32;
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(vbl_32));
		if (!res)
			*kargp = vbl_32;
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(u_long)));
}
int
CopyOutMvfs_u_long(
    u_long *kargp,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{

#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		ks_uint32_t vbl_32;
		vbl_32 = *kargp;
		return (COPYOUT((caddr_t)&vbl_32, uargp, sizeof(vbl_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kargp, uargp, sizeof(u_long)));
}

int
CopyInMvfs_viewstats(
    caddr_t uargp,
    struct mvfs_viewstats *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_viewstats_32 *vbl_32;

	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		/*
		 * structure is large so let's malloc space
		 */
		if((vbl_32 = KMEM_ALLOC(sizeof(struct mvfs_viewstats_32), KM_SLEEP))
			== NULL)
		{
			return(ENOMEM);
		}

		res = COPYIN(uargp, (caddr_t)vbl_32, sizeof(struct mvfs_viewstats_32));
		if (!res)
			mvfs_viewstats_32_to_mvfs_viewstats(vbl_32, kargp);
		KMEM_FREE(vbl_32, sizeof(struct mvfs_viewstats_32));
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_viewstats)));
}

int
CopyInMvfs_zero_viewstat(
    caddr_t uargp,
    struct mvfs_zero_viewstat *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_zero_viewstat_32 vbl_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&vbl_32, sizeof(struct mvfs_zero_viewstat_32));
		if (!res)
			mvfs_zero_viewstat_32_to_mvfs_zero_viewstat(&vbl_32, kargp);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_zero_viewstat)));
}
/*
 * NT specific call.
 */
int
CopyInMvfs_sidhost_cred(
    caddr_t uargp,
    struct mvfs_sidhost_cred *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_sidhost_cred)));
}
/*
 * NT specific call. 
 */
int
CopyInMvfs_sid(
    caddr_t uargp,
    struct mvfs_sid *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_sid)));
}
/*
 * NT specific call. 
 */
int
CopyInMvfs_unmount_info(
    caddr_t uargp,
    struct mvfs_unmount_info *kargp,
    MVFS_CALLER_INFO *callinfo
)
{
	return(COPYIN(uargp, (caddr_t)kargp, sizeof(struct mvfs_unmount_info)));
}

int
CopyInMvfs_gfsinfo(
    caddr_t uargp,
    struct mvfs_gfsinfo *kgfsinfo,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	int res;
	struct mvfs_gfsinfo_32 gfsinfo_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		res = COPYIN(uargp, (caddr_t)&gfsinfo_32, sizeof(struct mvfs_gfsinfo_32));
		if (!res)
			mvfs_gfsinfo_32_to_mvfs_gfsinfo(&gfsinfo_32, kgfsinfo);
		return res;
	}
#endif /* ATRIA_LP64 */
	return(COPYIN(uargp, (caddr_t)kgfsinfo, sizeof(struct mvfs_gfsinfo)));
}
int
CopyOutMvfs_gfsinfo(
    struct mvfs_gfsinfo *kgfsinfo,
    caddr_t uargp,
    MVFS_CALLER_INFO *callinfo
)
{
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
	struct mvfs_gfsinfo_32 gfsinfo_32;
	if (MDKI_CALLER_IS_32BIT(callinfo)) {
		mvfs_gfsinfo_to_mvfs_gfsinfo_32(kgfsinfo, &gfsinfo_32);
		return (COPYOUT((caddr_t)&gfsinfo_32, uargp, 
                                sizeof(struct mvfs_gfsinfo_32)));
	}
#endif /* ATRIA_LP64 */
	return (COPYOUT((caddr_t)kgfsinfo, uargp, 
                        sizeof(struct mvfs_gfsinfo)));
}
static const char vnode_verid_mvfs_copy_c[] = "$Id:  3033a857.a23a11df.8bc7.00:01:84:7a:f2:e4 $";
