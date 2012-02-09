/* * (C) Copyright IBM Corporation 1998, 2005. */
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

#ifndef MVFS_COPY_H_
#define MVFS_COPY_H_

extern int CopyInMfs_strbufpn(caddr_t uargp, struct mfs_strbufpn *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_strbufpn(struct mfs_strbufpn *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMfs_strbufpn_pair(caddr_t uargp , struct mfs_strbufpn_pair *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_strbufpn_pair(struct mfs_strbufpn_pair *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMfs_strbuf(caddr_t uargp , struct mfs_strbuf *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_strbuf(struct mfs_strbuf *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMfs_mntargs(caddr_t uargp , struct mfs_mntargs *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfscmd_block(caddr_t uargp , struct mvfscmd_block *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfscmd_block(struct mvfscmd_block *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfscmd_block_status(struct mvfscmd_block *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);

extern int CopyInMvfs_loginfo(caddr_t uargp , struct mvfs_loginfo *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_loginfo(struct mvfs_loginfo *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_viewtag_info(caddr_t uargp , struct mvfs_viewtag_info *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_mkviewtag_info(caddr_t uargp , struct mvfs_mkviewtag_info *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_viewinfo(caddr_t uargp , struct mvfs_viewinfo *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_viewinfo(struct mvfs_viewinfo *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_vobinfo(caddr_t uargp , struct mvfs_vobinfo *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_vobinfo(struct mvfs_vobinfo *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int BcopyInMvfscmd_block(caddr_t kdatap, struct mvfscmd_block *kargp, MVFS_CALLER_INFO *callinfo);
extern int BcopyOutMvfscmd_block(struct mvfscmd_block *kdatap, caddr_t kargp, MVFS_CALLER_INFO *callinfo);
extern int BcopyOutMvfscmd_block_status(struct mvfscmd_block *kdatap, caddr_t kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_ioget_poolmaps(caddr_t uargp, struct mvfs_ioget_poolmaps *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_ioget_poolmaps(struct mvfs_ioget_poolmaps *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_iovfh(caddr_t , struct mvfs_iovfh *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_iovfh(struct mvfs_iovfh *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyOutView_vstat(struct view_vstat *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_ioinval(caddr_t uargp, struct mvfs_ioinval *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_clrname_info(caddr_t , struct mvfs_clrname_info *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_clrname_info(struct mvfs_clrname_info *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_viewaddr(caddr_t , struct mvfs_viewaddr *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_viewaddr(struct mvfs_viewaddr *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_iochange_mtype(caddr_t , struct mvfs_iochange_mtype *, MVFS_CALLER_INFO *callinfo);
extern int CopyInMfs_ioncent(caddr_t , struct mfs_ioncent *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_ioncent(struct mfs_ioncent *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_bhinfo(caddr_t , struct mvfs_bhinfo *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_bhinfo(struct mvfs_bhinfo *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_statbufs(caddr_t , struct mvfs_statbufs *, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_io_xattr(caddr_t , struct mvfs_io_xattr *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_io_xattr(struct mvfs_io_xattr *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_export_viewinfo(caddr_t , struct mvfs_export_viewinfo *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_export_viewinfo(struct mvfs_export_viewinfo *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_cache_sizes(caddr_t , struct mvfs_cache_sizes *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_cache_sizes(struct mvfs_cache_sizes *, caddr_t, MVFS_CALLER_INFO *callinfo);
extern int CopyInTbs_uuid_s(caddr_t , struct tbs_uuid_s *, MVFS_CALLER_INFO *callinfo);
extern int CopyInTbs_oid_s(caddr_t , struct tbs_oid_s *, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_clntstat(struct mfs_clntstat *, caddr_t, size_t , MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_clearstat(struct mfs_clearstat *, caddr_t, size_t , MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_austat(struct mfs_austat *, caddr_t, size_t , MVFS_CALLER_INFO *callinfo);
extern int CopyOutMfs_rpchist(struct mfs_rpchist *, caddr_t, size_t , MVFS_CALLER_INFO *callinfo);
extern int CopyOuttimestruc_array(timestruc_t *, caddr_t , int , size_t , MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_pointer(caddr_t , char **, MVFS_CALLER_INFO *callinfo) ;
extern int CopyInMvfs_u_long(caddr_t , u_long * , MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_viewstats(caddr_t , struct mvfs_viewstats *, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_zero_viewstat(caddr_t , struct mvfs_zero_viewstat *, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_xstat(caddr_t uargp, struct mvfs_xstat *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_xstat(struct mvfs_xstat *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_u_long(u_long *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyOutMvfs_cache_usage(struct mvfs_cache_usage *kargp, caddr_t uargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_vdminfo(caddr_t uargp, struct mvfs_vdminfo *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_sidhost_cred( caddr_t uargp, struct mvfs_sidhost_cred *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_sid( caddr_t uargp, struct mvfs_sid *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_splitpool_index(caddr_t uargp, struct mvfs_splitpool *kargp, int index, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_sidhost_cred( caddr_t uargp, struct mvfs_sidhost_cred *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_sid( caddr_t uargp, struct mvfs_sid *kargp, MVFS_CALLER_INFO *callinfo);
extern int CopyInMvfs_unmount_info( caddr_t uargp, struct mvfs_unmount_info *kargp, MVFS_CALLER_INFO *callinfo);
#endif /* MVFS_COPY_H_ */
/* $Id: 2db2ae64.637a11da.8655.00:01:83:a6:4c:63 $ */
