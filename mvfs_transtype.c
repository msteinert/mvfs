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
/* mvfs_transtype.c */

#include "mvfs_systm.h"
#include <tbs_base.h>
#include <tbs_errno.h>
#include "mvfs.h"
#include "mvfs_dnc.h"

#include "mvfs_transtype.h"

#if defined(ATRIA_WIN32_COMMON) || defined(ATRIA_LP64) || defined(ATRIA_LLP64)
void
mfs_strbufpn_to_mfs_strbufpn_32(struct mfs_strbufpn *vbl, struct mfs_strbufpn_32 *vbl_32)
{
	vbl_32->s  = PTR_TO_PTR32(vbl->s);
	vbl_32->l = (mvfs_size_t_32) vbl->l;
	vbl_32->m = (mvfs_size_t_32) vbl->m;
}

void
mfs_strbufpn_32_to_mfs_strbufpn(struct mfs_strbufpn_32 *vbl_32, struct mfs_strbufpn *vbl)
{
	vbl->s = PTR32_TO_PTR(vbl_32->s);
	vbl->l = vbl_32->l;
	vbl->m = vbl_32->m;
}

void
mvfs_iofid_to_mvfs_iofid_32(struct mvfs_iofid *vbl, struct mvfs_iofid_32 *vbl_32)
{
	vbl_32->dbid = vbl->dbid;
	vbl_32->gen = vbl->gen;
}
void
mvfs_iofid_32_to_mvfs_iofid(struct mvfs_iofid_32 *vbl_32, struct mvfs_iofid *vbl)
{
	vbl->dbid = vbl_32->dbid;
	vbl->gen = vbl_32->gen;
}
void
view_bhandle_to_view_bhandle_32(struct view_bhandle *vbl, struct view_bhandle_32 *vbl_32)
{
	vbl_32->build_session = vbl->build_session;
	vbl_32->target_id = vbl->target_id;
}
void
view_bhandle_32_to_view_bhandle(struct view_bhandle_32 *vbl_32, struct view_bhandle *vbl)
{
	vbl->build_session = vbl_32->build_session;
	vbl->target_id = vbl_32->target_id;
}

void
mvfs_timeval_to_mvfs_timeval_32(struct timeval *vbl, struct timeval_32 *vbl_32)
{
	vbl_32->tv_sec = (ks_int32_t)vbl->tv_sec;
	vbl_32->tv_usec = vbl->tv_usec;
}
void
mvfs_timeval_32_to_mvfs_timeval(struct timeval_32 *vbl_32, struct timeval *vbl)
{
	vbl->tv_sec = vbl_32->tv_sec;
	vbl->tv_usec = vbl_32->tv_usec;
}

void
mfs_ioncent_to_mfs_ioncent_32(struct mfs_ioncent *vbl, struct mfs_ioncent_32 *vbl_32)
{
	int i;

	vbl_32->offset = vbl->offset;
	vbl_32->eocache = vbl->eocache;
	vbl_32->flags = vbl->flags;
        /* We just assume here that the time_t addtime will fit in 32 bits even
        ** if it is a 64-bit type in this kernel.  We could avoid this ASSERT
        ** and cast on Windows since we know the user-space addtime will be
        ** 64-bits (see below and mvfs_transtype.h for details), but it hardly
        ** seems worth the #ifdef to check.
        */
        ASSERT(INT_MIN <= vbl->addtime && vbl->addtime <= INT_MAX);
	vbl_32->addtime = (ks_int32_t)vbl->addtime;
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->dvw, &vbl_32->dvw);
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->mp, &vbl_32->mp);
	mvfs_iofid_to_mvfs_iofid_32(&vbl->dfid, &vbl_32->dfid);
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->nm, &vbl_32->nm);
	for ( i = 0; i < MVFS_IONCBHMAX; i++)
		view_bhandle_to_view_bhandle_32(&vbl->bhlist[i], &vbl_32->bhlist[i]);
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->vw, &vbl_32->vw);
	mvfs_iofid_to_mvfs_iofid_32(&vbl->fid, &vbl_32->fid);
	mvfs_timeval_to_mvfs_timeval_32(&vbl->evtime, &vbl_32->evtime);
}
void
mfs_ioncent_32_to_mfs_ioncent(struct mfs_ioncent_32 *vbl_32, struct mfs_ioncent *vbl)
{
	int i;

	vbl->offset = vbl_32->offset;
	vbl->eocache = vbl_32->eocache;
	vbl->flags = vbl_32->flags;
	vbl->addtime = (time_t)vbl_32->addtime;
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->dvw, &vbl->dvw);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->mp, &vbl->mp);
	mvfs_iofid_32_to_mvfs_iofid(&vbl_32->dfid, &vbl->dfid);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->nm, &vbl->nm);
	for ( i = 0; i < MVFS_IONCBHMAX; i++)
		view_bhandle_32_to_view_bhandle(&vbl_32->bhlist[i], &vbl->bhlist[i]);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->vw, &vbl->vw);
	mvfs_iofid_32_to_mvfs_iofid(&vbl_32->fid, &vbl->dfid);
	mvfs_timeval_32_to_mvfs_timeval(&vbl_32->evtime, &vbl->evtime);
}

void
mfs_timestruc_to_mfs_timestruc_32(timestruc_t *vbl, struct timestruc_32 *vbl_32)
{
        vbl_32->tv_sec = (ks_int32_t)vbl->tv_sec;
        vbl_32->tv_nsec = vbl->tv_nsec;
}

void
mfs_clntstat_to_mfs_clntstat_32(struct mfs_clntstat *vbl, struct mfs_clntstat_32 *vbl_32)
{
        vbl_32->version = vbl->version;
        vbl_32->clntget = vbl->clntget;
        vbl_32->clntfree = vbl->clntfree;
        vbl_32->clntcreate = vbl->clntcreate;
        vbl_32->clntdestroy = vbl->clntdestroy;
        vbl_32->clntcalls = vbl->clntcalls;
        vbl_32->clntretries = vbl->clntretries;
        vbl_32->mfscall = vbl->mfscall;
        vbl_32->mfsfail = vbl->mfsfail;
        vbl_32->mfsintr = vbl->mfsintr;
        vbl_32->mfsmaxdelay = vbl->mfsmaxdelay;
        vbl_32->mfsmaxdelaytime = vbl->mfsmaxdelaytime;
        mfs_timestruc_to_mfs_timestruc_32(&vbl->mvfsthread_time,
                &vbl_32->mvfsthread_time);
}

void
mfs_clearstat_to_mfs_clearstat_32(struct mfs_clearstat *vbl, struct mfs_clearstat_32 *vbl_32)
{
        vbl_32->version = vbl->version;
        vbl_32->clearget = vbl->clearget;
        vbl_32->clearcreate = vbl->clearcreate;
        vbl_32->clearraces = vbl->clearraces;
        vbl_32->clearcreatraces = vbl->clearcreatraces;
        vbl_32->clearread = vbl->clearread;
        vbl_32->clearwrite = vbl->clearwrite;
        mfs_timestruc_to_mfs_timestruc_32(&vbl->clearget_time,
                &vbl_32->clearget_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->clearcreat_time,
                &vbl_32->clearcreat_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->clearrd_time,
                &vbl_32->clearrd_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->clearwr_time,
                &vbl_32->clearwr_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->clearopen_time,
                &vbl_32->clearopen_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->unclearrd_time,
                &vbl_32->unclearrd_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->unclearwr_time,
                &vbl_32->unclearwr_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->unclearget_time,
                &vbl_32->unclearget_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->cto_getattr_time,
                &vbl_32->cto_getattr_time);
        vbl_32->clearopen = vbl->clearopen;
        vbl_32->unclearopen = vbl->unclearopen;
        vbl_32->cleargetmiss = vbl->cleargetmiss;
        vbl_32->clearreclaim = vbl->clearreclaim;
        vbl_32->clearreclaimmiss = vbl->clearreclaimmiss;
}

void
mfs_austat_to_mfs_austat_32(struct mfs_austat *vbl, struct mfs_austat_32 *vbl_32)
{
        vbl_32->version = vbl->version;
        vbl_32->au_calls = vbl->au_calls;
        vbl_32->au_vgetattr = vbl->au_vgetattr;
        vbl_32->au_nvgetattr = vbl->au_nvgetattr;
        vbl_32->au_dupl = vbl->au_dupl;
        mfs_timestruc_to_mfs_timestruc_32(&vbl->au_time, &vbl_32->au_time);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->au_settime, &vbl_32->au_settime);
        mfs_timestruc_to_mfs_timestruc_32(&vbl->au_ioctltime, &vbl_32->au_ioctltime);
}

void
mfs_rpchist_to_mfs_rpchist_32(struct mfs_rpchist *vbl, struct mfs_rpchist_32 *vbl_32)
{
        int i, j;

        vbl_32->version = vbl->version;
        for ( i = 0; i < MFS_NUM_HISTX; i++) {
            mfs_timestruc_to_mfs_timestruc_32(&vbl->histval[i], &vbl_32->histval[i]);
            vbl_32->histrpc[i] = vbl->histrpc[i];
            vbl_32->histclr[i] = vbl->histclr[i];
        }

        for (i = 0; i < VIEW_NUM_PROCS; i++) {
                for (j = 0; j < MFS_NUM_HISTX; j++)
                        vbl_32->histperop[i][j] = vbl->histperop[i][j];
        }
}

#endif
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
struct {
    int used;		/* if cmd is used */
    long mininfolen;	/* min infolen reqd */
    long maxinfolen;	/* max infolen reqd */
} mvfs_ioctl_valid_table_32[] = {
	/* MVFS_CMD_GET_VIEWINFO 1 */
	{TRUE, sizeof(struct mvfs_viewinfo_32), sizeof(struct mvfs_viewinfo_32)},
	/* MVFS_CMD_XSTAT 2 */
	{TRUE, sizeof(struct mvfs_xstat_32), sizeof(struct mvfs_xstat_32)},
	/* MVFS_CMD_GET_CLRNAME 3 */
	{TRUE, sizeof(struct mvfs_clrname_info_32), sizeof(struct mvfs_clrname_info_32)},
	/* MVFS_CMD_GET_VFH 4 */
	{TRUE, sizeof(struct mvfs_iovfh_32), sizeof(struct mvfs_iovfh_32)},
	/* MVFS_CMD_IOINVAL 5 */
	{TRUE, sizeof(struct mvfs_ioinval_32), sizeof(struct mvfs_ioinval_32)},
	/* MVFS_CMD_REVALIDATE 6 */
	{TRUE, 0, 0},
	/* MVFS_CMD_MKVIEWTAG 7 */
	{TRUE, sizeof(struct mvfs_mkviewtag_info_32), sizeof(struct mvfs_mkviewtag_info_32)},
	/* MVFS_CMD_RMVIEWTAG 8 */
	{TRUE, sizeof(struct mvfs_viewtag_info_32), sizeof(struct mvfs_viewtag_info_32)},
	/* MVFS_CMD_GET_VIEWADDR 9 */
	{TRUE, sizeof(struct mvfs_viewaddr), sizeof(struct mvfs_viewaddr)},
	/* MVFS_CMD_GET_VOBINFO 10 */
	{TRUE, sizeof(struct mvfs_vobinfo_32), sizeof(struct mvfs_vobinfo_32)},
	/* MVFS_CMD_GET_VIEWTAG_DIR 11 */
	{TRUE, sizeof(struct mfs_strbufpn_pair_32), sizeof(struct mfs_strbufpn_pair_32)},
	/* MVFS_CMD_CHANGE_MTYPE 12 */
	{TRUE, sizeof(struct mvfs_iochange_mtype_32), sizeof(struct mvfs_iochange_mtype_32)},
	/* MVFS_CMD_GET_AFILE 13 */
	{TRUE, sizeof(struct mfs_strbufpn_32), sizeof(struct mfs_strbufpn_32)},
	/* MVFS_CMD_SET_AFILE 14 */
	{TRUE, sizeof(struct mfs_strbufpn_pair_32), sizeof(struct mfs_strbufpn_pair_32)},
	/* MVFS_CMD_GET_PROCF 15 */
	{TRUE, sizeof(ks_uint32_t), sizeof(ks_uint32_t)},
	/* MVFS_CMD_SET_PROCF 16 */
	{TRUE, sizeof(ks_uint32_t), sizeof(ks_uint32_t)},
	/* MVFS_CMD_START_AUDIT 17 */
	{TRUE, sizeof(ks_uint32_t), sizeof(ks_uint32_t)},
	/* MVFS_CMD_STOP_AUDIT 18 */
	{TRUE, 0, 0},
	/* MVFS_CMD_SYNC_AUDIT 19 */
	{TRUE, 0, 0},
	/* MVFS_CMD_GET_VXSUFFIX 20 */
	{TRUE, sizeof(struct mfs_strbuf_32), sizeof(struct mfs_strbuf_32)},
	/* MVFS_CMD_GET_CACHE_ENB 21 */
	{TRUE, sizeof(ks_uint32_t), sizeof(ks_uint32_t)},
	/* MVFS_CMD_SET_CACHE_ENB 22 */
	{TRUE, sizeof(ks_uint32_t), sizeof(ks_uint32_t)},
	/* MVFS_CMD_FLUSH_CACHE 23 */
	{TRUE, sizeof(ks_uint32_t), sizeof(ks_uint32_t)},
	/* MVFS_CMD_READ_DNC 24 */
	{TRUE, sizeof(struct mfs_ioncent_32), sizeof(struct mfs_ioncent_32)},
	/* MVFS_CMD_GET_RCSID 25 */
	{TRUE, -1L, -1L},
	/* MVFS_CMD_GET_SCCSID 26 */
	{TRUE, -1L, -1L},
	/* MVFS_CMD_GET_LOGINFO 27 */
	{TRUE, sizeof(struct mvfs_loginfo_32), sizeof(struct mvfs_loginfo_32)},
	/* MVFS_CMD_SET_LOGINFO 28 */
	{TRUE, sizeof(struct mvfs_loginfo_32), sizeof(struct mvfs_loginfo_32)},
	/* MVFS_CMD_GET_BH 29 */
	{TRUE, sizeof(struct mvfs_bhinfo_32), sizeof(struct mvfs_bhinfo_32)},
	/* MVFS_CMD_SET_BH 30 */
	{TRUE, sizeof(struct mvfs_bhinfo_32), sizeof(struct mvfs_bhinfo_32)},
	/* MVFS_CMD_GET_STATS 31 */
	{TRUE, sizeof(struct mvfs_statbufs_32), sizeof(struct mvfs_statbufs_32)},
	/* MVFS_CMD_SETPROCVIEW 32 */
	{TRUE, sizeof(struct mvfs_viewtag_info_32), sizeof(struct mvfs_viewtag_info_32)},
	/* MVFS_CMD_GET_PROCVIEWINFO 33 */
	{TRUE, sizeof(struct mvfs_viewinfo_32), sizeof(struct mvfs_viewinfo_32)},
	/* MVFS_CMD_GET_XATTR 34 */
	{TRUE, sizeof(struct mvfs_io_xattr_32), sizeof(struct mvfs_io_xattr_32)},
	/* MVFS_CMD_SET_XATTR 35 */
	{TRUE, sizeof(struct mvfs_io_xattr_32), sizeof(struct mvfs_io_xattr_32)},
	/* MVFS_CMD_MOUNT 36 */
	{TRUE, sizeof(struct mfs_mntargs_32), sizeof(struct mfs_mntargs_32)},
	/* MVFS_CMD_UNMOUNT 37 */
	{TRUE, sizeof(struct mvfs_unmount_info_32), sizeof(struct mvfs_unmount_info_32)},
	/* MVFS_CMD_RMALLVIEWTAGS 38 */
	{TRUE, 0, 0},
	/* MVFS_CMD_UMOUNTALL 39 */
	{TRUE, 0, 0},
	/* MVFS_CMD_GET_POOLMAPS 40 */
	{TRUE, sizeof(struct mvfs_ioget_poolmaps_32), sizeof(struct mvfs_ioget_poolmaps_32)},
	/* MVFS_CMD_VDM 41 */
	{FALSE, 0, 0},
	/* MVFS_CMD_ABORT 42 */
	{TRUE, 0, 0},
	/* MVFS_CMD_EXPORTVIEWTAG 43 */
	{TRUE, sizeof(struct mvfs_export_viewinfo_32), sizeof(struct mvfs_export_viewinfo_32)},
	/* MVFS_CMD_UNEXPORTVIEWTAG 44 */
	{TRUE, sizeof(struct mvfs_viewtag_info_32), sizeof(struct mvfs_viewtag_info_32)},
	/* MVFS_CMD_ZERO_STATS 45 */
	{TRUE, 0, 0},
	/* MVFS_CMD_GET_CACHE_USAGE 46 */
	{TRUE, sizeof(mvfs_cache_usage_t), sizeof(mvfs_cache_usage_t)},
	/* MVFS_CMD_SET_CACHE_SIZES 47 */
	{TRUE, sizeof(mvfs_cache_sizes_t), sizeof(mvfs_cache_sizes_t)},
	/* MVFS_CMD_AUDIT_MARKER 48 */
	{TRUE, sizeof(ks_uint32_t), sizeof(ks_uint32_t)},
	/* MVFS_CMD_IOD_NULL 49 */
	{FALSE, 0, 0},
	/* MVFS_CMD_GET_VIEW_STATS 50 */
	{TRUE, sizeof(struct mvfs_viewstats_32), sizeof(struct mvfs_viewstats_32)},
	/* MVFS_CMD_ZERO_VIEW_STATS 51 */
	{TRUE, sizeof(struct mvfs_zero_viewstat_32), sizeof(mvfs_zero_viewstat_t)},
	/* MVFS_CMD_SIDHOST_CREDMAPPING 52 */
        {FALSE, 0, 0},
	/* MVFS_CMD_DELETE_SIDHOST_CREDMAPPING 53 */
        {FALSE, 0, 0},
	/* MVFS_CMD_GET_VIEWTAG_EXPORT 54 */
	{TRUE, sizeof(struct mvfs_export_viewinfo_32), sizeof(struct mvfs_export_viewinfo_32)},
        /* MVFS_CMD_GET_GFSINFO 55 */
        {TRUE, sizeof(struct mvfs_gfsinfo_32), sizeof(struct mvfs_gfsinfo_32)},
        /* MVFS_CMD_SET_VOBRT_VFSMNT 56 */
        {TRUE, sizeof(struct mfs_strbufpn_pair_32), sizeof(struct mfs_strbufpn_pair_32)},
	/* MVFS_CMD_GET_CACHE_SIZES 57 */
	{TRUE, sizeof(mvfs_cache_sizes_t), sizeof(mvfs_cache_sizes_t)},
        /* MVFS_CMD_REG_GRPLIST_ORDER 58 */
        {FALSE, 0, 0},

        /* MVFS_CMD_UNREG_GRPLIST_ORDER 59 */
        {FALSE ,0, 0},
	/* MVFS_CMD_COMPUTE_CACHE_DEFAULTS 60 */
	{TRUE, sizeof(mvfs_cache_sizes_t), sizeof(mvfs_cache_sizes_t)},

       /* MVFS_CMD_GRPLIST_READ 61 */
       {FALSE, 0, 0},
       /* MVFS_CMD_MKVIEWTAG_EX 62 */
       {FALSE, 0, 0},

};

int
mvfs_ioctl_validate_32(
    mvfscmd_block_t *mcbp
)
{

    /* valid MVFS_CMD ? */
    if (mcbp->hdr.cmd < MVFS_CMD_MIN) {
        mvfs_log(MFS_LOG_DEBUG,"mvfs_ioctl_validate_32: EINVAL cmd %d min %d\n",
                 mcbp->hdr.cmd, MVFS_CMD_MIN);
	return (EINVAL);
    }

    if (mcbp->hdr.cmd > MVFS_CMD_MAX) {
        mvfs_log(MFS_LOG_DEBUG,"mvfs_ioctl_validate_32: EINVAL cmd %d max %d\n",
                 mcbp->hdr.cmd, MVFS_CMD_MAX);
	return (EINVAL);
    }

    /* Used ? */
    if (mvfs_ioctl_valid_table_32[mcbp->hdr.cmd - 1].used != TRUE) {
        mvfs_log(MFS_LOG_DEBUG,"mvfs_ioctl_validate_32: EINVAL cmd %d table unused\n",
                 mcbp->hdr.cmd);
	return(EINVAL);
    }

    /* Do we need to check length ? */
    if (mvfs_ioctl_valid_table_32[mcbp->hdr.cmd - 1].mininfolen == -1L) {
        mvfs_log(MFS_LOG_DEBUG,"mvfs_ioctl_validate_32: cmd %d mininfolen -1\n",
                 mcbp->hdr.cmd);
	return (0);
    }

    if (mcbp->infolen < mvfs_ioctl_valid_table_32[mcbp->hdr.cmd - 1].mininfolen) {
        mvfs_log(MFS_LOG_DEBUG,"mvfs_ioctl_validate_32: EINVAL cmd %d mininfolen %d, mcbp->infolen %d\n",
                 mcbp->hdr.cmd, mvfs_ioctl_valid_table_32[mcbp->hdr.cmd - 1].mininfolen, mcbp->infolen);
	return (EINVAL);
    }

    if (mcbp->infolen > mvfs_ioctl_valid_table_32[mcbp->hdr.cmd - 1].maxinfolen) {
        mvfs_log(MFS_LOG_DEBUG,"mvfs_ioctl_validate_32: EINVAL cmd %d maxinfolen %d, mcbp->infolen %d\n",
                 mcbp->hdr.cmd, mvfs_ioctl_valid_table_32[mcbp->hdr.cmd - 1].maxinfolen, mcbp->infolen);
	return (EINVAL);
    }

    return (0);
}

int
mvfs_ioctl_validate_64bitos(
    mvfscmd_block_t *mcbp,
    MVFS_CALLER_INFO *callinfo
)
{
	if (MDKI_CALLER_IS_32BIT(callinfo)) 
		return(mvfs_ioctl_validate_32(mcbp));
	return(mvfs_ioctl_validate(mcbp));
}

tbs_boolean_t
mvfs_ioctl_chk_cmd(
    int cmd,
    MVFS_CALLER_INFO *callinfo
)
{
	if (MDKI_CALLER_IS_32BIT(callinfo)) 
		return(cmd == MVFS_IOCTL_CMD_32);
	return(cmd == MVFS_IOCTL_CMD);
}

void
mfs_strbufpn_pair_to_mfs_strbufpn_pair_32(struct mfs_strbufpn_pair *vbl, struct mfs_strbufpn_pair_32 *vbl_32)
{
	vbl_32->upn.s  = PTR_TO_PTR32(vbl->upn.s);
	vbl_32->upn.l = (mvfs_size_t_32) vbl->upn.l;
	vbl_32->upn.m = (mvfs_size_t_32) vbl->upn.m;
	vbl_32->kpn.s  = PTR_TO_PTR32(vbl->kpn.s);
	vbl_32->kpn.l = (mvfs_size_t_32) vbl->kpn.l;
	vbl_32->kpn.m = (mvfs_size_t_32) vbl->kpn.m;
}

void
mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(struct mfs_strbufpn_pair_32 *vbl_32, struct mfs_strbufpn_pair *vbl)
{
	vbl->upn.s = PTR32_TO_PTR(vbl_32->upn.s);
	vbl->upn.l = vbl_32->upn.l;
	vbl->upn.m = vbl_32->upn.m;
	vbl->kpn.s = PTR32_TO_PTR(vbl_32->kpn.s);
	vbl->kpn.l = vbl_32->kpn.l;
	vbl->kpn.m = vbl_32->kpn.m;
}

void
mfs_strbuf_to_mfs_strbuf_32(struct mfs_strbuf *vbl, struct mfs_strbuf_32 *vbl_32)
{
	vbl_32->s  = PTR_TO_PTR32(vbl->s);
	vbl_32->l = (mvfs_size_t_32) vbl->l;
	vbl_32->m = (mvfs_size_t_32) vbl->m;
}
void
mfs_strbuf_32_to_mfs_strbuf(struct mfs_strbuf_32 *vbl_32, struct mfs_strbuf *vbl)
{
	vbl->s = PTR32_TO_PTR(vbl_32->s);
	vbl->l = vbl_32->l;
	vbl->m = vbl_32->m;
}

void
tbs_uuid_s_to_tbs_uuid_s_32(struct tbs_uuid_s *vbl, struct tbs_uuid_s_32 *vbl_32)
{
	vbl_32->time_low = vbl->time_low;
	vbl_32->time_mid = vbl->time_mid;
	vbl_32->time_hi_and_version = vbl->time_hi_and_version;
	vbl_32->clock_seq_hi_and_reserved = vbl->clock_seq_hi_and_reserved;
	vbl_32->clock_seq_low = vbl->clock_seq_low;
	BCOPY((caddr_t)&vbl->node[0], (caddr_t)&vbl_32->node[0],
		sizeof(vbl->node));
}

void
tbs_uuid_s_32_to_tbs_uuid_s(struct tbs_uuid_s_32 *vbl_32, struct tbs_uuid_s *vbl)
{
	vbl->time_low = vbl_32->time_low;
	vbl->time_mid = vbl_32->time_mid;
	vbl->time_hi_and_version = vbl_32->time_hi_and_version;
	vbl->clock_seq_hi_and_reserved = vbl_32->clock_seq_hi_and_reserved;
	vbl->clock_seq_low = vbl_32->clock_seq_low;
	BCOPY((caddr_t)&vbl_32->node[0], (caddr_t)&vbl->node[0],
		sizeof(vbl_32->node));
}

void
tbs_oid_s_to_tbs_oid_s_32(struct tbs_oid_s *vbl, struct tbs_oid_s_32 *vbl_32)
{
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->obj_uuid, &vbl_32->obj_uuid);
}
void
tbs_oid_s_32_to_tbs_oid_s(struct tbs_oid_s_32 *vbl_32, struct tbs_oid_s *vbl)
{
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->obj_uuid, &vbl->obj_uuid);
}

void
mfs_mntargs_32_to_mfs_mntargs(struct mfs_mntargs_32 *vbl_32, struct mfs_mntargs * vbl)
{

	vbl->mma_mntvers = vbl_32->mma_mntvers;
        /*
         * Some platforms (e.g. Linux x86_64) don't have an indication
         * of the caller's ABI (32 or 64-bit), so we have to infer the
         * ABI from some data in the call.  By the time this procedure
         * is invoked, we have guessed that the caller used a 32-bit
         * ABI.  The mma_mntsize field should match our 32-bit
         * structure size, and if it does, we convert it to the 64-bit
         * size when creating the 64-bit structure.  If it does not
         * match, it's probably the result of a structure revision
         * that didn't get caught by a version number check.  In that
         * case we continue the translation (since we can't indicate
         * an error) and set the mntsize to something that won't pass
         * further checks.
         */
        if (vbl_32->mma_mntsize == sizeof(*vbl_32))
            vbl->mma_mntsize = sizeof(*vbl);
        else
            vbl->mma_mntsize = 0;
	vbl->mma_flags = vbl_32->mma_flags;

	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->mma_mntpath, &vbl->mma_mntpath);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->mma_host, &vbl->mma_host);

	/* in_port_t is the same in 32-bit and 64-bit kernels (u_short). */
	vbl->mma_port = vbl_32->mma_port;

	mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(&vbl_32->mma_spath, 
		&vbl->mma_spath);

	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->mma_rpath, &vbl->mma_rpath);

	tbs_oid_s_32_to_tbs_oid_s(&vbl_32->mma_vob_oid, &vbl->mma_vob_oid);

	vbl->mma_timeo = vbl_32->mma_timeo;
	vbl->mma_retries = vbl_32->mma_retries;
	vbl->mma_ac_regmax = vbl_32->mma_ac_regmax;
	vbl->mma_ac_regmin = vbl_32->mma_ac_regmin;
	vbl->mma_ac_dirmax = vbl_32->mma_ac_dirmax;
	vbl->mma_ac_dirmin = vbl_32->mma_ac_dirmin;

	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->mma_hmsuffix, &vbl->mma_hmsuffix);

	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->mma_replica_uuid, 
			&vbl->mma_replica_uuid);

	vbl->mma_sptab_cnt = vbl_32->mma_sptab_cnt;
	vbl->mma_sptab_ents = PTR32_TO_PTR(vbl_32->mma_sptab_ents);
	vbl->mma_vobminor = vbl_32->mma_vobminor;

        vbl->mma_sizes = vbl_32->mma_sizes;
}

void
mvfscmd_header_to_mvfscmd_header_32(struct mvfscmd_header *vbl, struct mvfscmd_header_32 *vbl_32)
{
	vbl_32->cmd = vbl->cmd;
	vbl_32->pnflags = vbl->pnflags;
	mfs_strbufpn_pair_to_mfs_strbufpn_pair_32(&vbl->objnmpair, 
		&vbl_32->objnmpair);
}

void
mvfscmd_header_32_to_mvfscmd_header(struct mvfscmd_header_32 *vbl_32, struct mvfscmd_header *vbl)
{
	vbl->cmd = vbl_32->cmd;
	vbl->pnflags = vbl_32->pnflags;
	mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(&vbl_32->objnmpair, 
		&vbl->objnmpair);
}

void
mvfscmd_block_to_mvfscmd_block_32(struct mvfscmd_block *vbl, struct mvfscmd_block_32 *vbl_32)
{
	mvfscmd_header_to_mvfscmd_header_32(&vbl->hdr, &vbl_32->hdr);
	vbl_32->infolen = vbl->infolen;
	vbl_32->infop  = PTR_TO_PTR32(vbl->infop);
	vbl_32->status = vbl->status;
}

void
mvfscmd_block_32_to_mvfscmd_block(struct mvfscmd_block_32 *vbl_32, struct mvfscmd_block *vbl)
{
	mvfscmd_header_32_to_mvfscmd_header(&vbl_32->hdr, &vbl->hdr);
	vbl->infolen = vbl_32->infolen;
	vbl->infop  = PTR32_TO_PTR(vbl_32->infop);
	vbl->status = vbl_32->status;
}

void
mvfs_loginfo_to_mvfs_loginfo_32(struct mvfs_loginfo *vbl, struct mvfs_loginfo_32 *vbl_32)
{
	vbl_32->mask = vbl->mask;
	vbl_32->priority = vbl->priority;
	vbl_32->io_ops_mask = vbl->io_ops_mask;
	vbl_32->vops_mask = vbl->vops_mask;
	vbl_32->vfsops_mask = vbl->vfsops_mask;
	vbl_32->xops_mask = vbl->xops_mask;
	vbl_32->traps_mask = vbl->traps_mask;
	vbl_32->assert_panic_on = vbl->assert_panic_on;
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->kernlog_pn,&vbl_32->kernlog_pn);
}

void
mvfs_loginfo_32_to_mvfs_loginfo(struct mvfs_loginfo_32 *vbl_32, struct mvfs_loginfo *vbl)
{
	vbl->mask = vbl_32->mask;
	vbl->priority = vbl_32->priority;
	vbl->io_ops_mask = vbl_32->io_ops_mask;
	vbl->vops_mask = vbl_32->vops_mask;
	vbl->vfsops_mask = vbl_32->vfsops_mask;
	vbl->xops_mask = vbl_32->xops_mask;
	vbl->traps_mask = vbl_32->traps_mask;
	vbl->assert_panic_on = vbl_32->assert_panic_on;
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->kernlog_pn,&vbl->kernlog_pn);
}

void
mvfs_viewtag_info_32_to_mvfs_viewtag_info(struct mvfs_viewtag_info_32 *vbl_32, struct mvfs_viewtag_info *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewtag, &vbl->viewtag);
}

void
mvfs_mkviewtag_info_32_to_mvfs_mkviewtag_info(struct mvfs_mkviewtag_info_32 *vbl_32, struct mvfs_mkviewtag_info *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewtag, &vbl->viewtag);
	mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(&vbl_32->spath, &vbl->spath);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->host, &vbl->host);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->rpath, &vbl->rpath);
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->uuid, &vbl->uuid);
	/* ks_sockaddr_storage_t is the same in 32-bit and 64-bit kernels. */
	vbl->addr = vbl_32->addr;
#if defined(ATRIA_LLP64)
        vbl->windows_view = vbl_32->windows_view;
        vbl->pad = vbl_32->pad;
#endif
}

void
mvfs_viewinfo_to_mvfs_viewinfo_32(struct mvfs_viewinfo *vbl, struct mvfs_viewinfo_32 *vbl_32)
{
	mfs_strbuf_to_mfs_strbuf_32(&vbl->vname, &vbl_32->vname);
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->spath, &vbl_32->spath);
	mfs_strbuf_to_mfs_strbuf_32(&vbl->host, &vbl_32->host);
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->rpath, &vbl_32->rpath);
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->uuid, &vbl_32->uuid);
}
void
mvfs_viewinfo_32_to_mvfs_viewinfo(struct mvfs_viewinfo_32 *vbl_32, struct mvfs_viewinfo *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->vname, &vbl->vname);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->spath, &vbl->spath);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->host, &vbl->host);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->rpath, &vbl->rpath);
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->uuid, &vbl->uuid);

}
void
mvfs_vobinfo_to_mvfs_vobinfo_32(struct mvfs_vobinfo *vbl, struct mvfs_vobinfo_32 *vbl_32)
{
	vbl_32->utype = vbl->utype;

	switch (vbl->utype) {
	  case MVFS_VOBINFO_IN_COOKIE:
		vbl_32->vobid.cookie = vbl->vobid.cookie;
		break;
	  case MVFS_VOBINFO_IN_OID: 
		vbl_32->vobid.vob_oid = PTR_TO_PTR32(vbl->vobid.vob_oid);
		break;
	  case MVFS_VOBINFO_IN_UUID:
		vbl_32->vobid.vob_uuid = PTR_TO_PTR32(vbl->vobid.vob_uuid);
		break;
	  default:
		mfs_strbufpn_pair_to_mfs_strbufpn_pair_32(&(vbl->vobid.pn_s.pair), 
			&(vbl_32->vobid.pn_s.pair));
		vbl_32->vobid.pn_s.pnflags = vbl->vobid.pn_s.pnflags;
	};

	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->mntpath, &vbl_32->mntpath);
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->spath, &vbl_32->spath);
	mfs_strbuf_to_mfs_strbuf_32(&vbl->host, &vbl_32->host);
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->rpath, &vbl_32->rpath);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->oid, &vbl_32->oid);
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->uuid, &vbl_32->uuid);
	vbl_32->unique = vbl->unique;
}
void
mvfs_vobinfo_32_to_mvfs_vobinfo(struct mvfs_vobinfo_32 *vbl_32, struct mvfs_vobinfo *vbl)
{
	vbl->utype = vbl_32->utype;

	switch (vbl_32->utype) {
	  case MVFS_VOBINFO_IN_COOKIE:
		vbl->vobid.cookie = vbl_32->vobid.cookie;
		break;
	  case MVFS_VOBINFO_IN_OID: 
		vbl->vobid.vob_oid = PTR32_TO_PTR(vbl_32->vobid.vob_oid);
		break;
	  case MVFS_VOBINFO_IN_UUID:
		vbl->vobid.vob_uuid = PTR32_TO_PTR(vbl_32->vobid.vob_uuid);
		break;
	  default:
		mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(
			&(vbl_32->vobid.pn_s.pair), &(vbl->vobid.pn_s.pair));
		vbl->vobid.pn_s.pnflags = vbl_32->vobid.pn_s.pnflags;
	};

	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->mntpath, &vbl->mntpath);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->spath, &vbl->spath);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->host, &vbl->host);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->rpath, &vbl->rpath);
	tbs_oid_s_32_to_tbs_oid_s(&vbl_32->oid, &vbl->oid);
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->uuid, &vbl->uuid);
	vbl->unique = vbl_32->unique;
}

void
mvfs_ioget_poolmaps_32_to_mvfs_ioget_poolmaps(struct mvfs_ioget_poolmaps_32 *vbl_32, struct mvfs_ioget_poolmaps *vbl)
{
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->replica_uuid, &vbl->replica_uuid);
	vbl->mapcount = vbl_32->mapcount;
	vbl->patterns = PTR32_TO_PTR(vbl_32->patterns);
	vbl->replacements = PTR32_TO_PTR(vbl_32->replacements);
}

void
mvfs_ioget_poolmaps_to_mvfs_ioget_poolmaps_32(struct mvfs_ioget_poolmaps *vbl, struct mvfs_ioget_poolmaps_32 *vbl_32)
{
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->replica_uuid, &vbl_32->replica_uuid);
	vbl_32->mapcount = vbl->mapcount;
	vbl_32->patterns = PTR_TO_PTR32(vbl->patterns);
	vbl_32->replacements = PTR_TO_PTR32(vbl->replacements);
}

void
view_fhandle_to_view_fhandle_32(struct view_fhandle *vbl, struct view_fhandle_32 *vbl_32)
{
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->vob_uuid, &vbl_32->vob_uuid);
	vbl_32->ver_dbid = vbl->ver_dbid;
	vbl_32->elem_dbid = vbl->elem_dbid;
	vbl_32->gen = vbl->gen;
	vbl_32->flags = vbl->flags;
	vbl_32->pad0 = vbl->pad0;
}
void
view_fhandle_32_to_view_fhandle(struct view_fhandle_32 *vbl_32, struct view_fhandle *vbl)
{
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->vob_uuid, &vbl->vob_uuid);
	vbl->ver_dbid = vbl_32->ver_dbid;
	vbl->elem_dbid = vbl_32->elem_dbid;
	vbl->gen = vbl_32->gen;
	vbl->flags = vbl_32->flags;
	vbl->pad0 = vbl_32->pad0;
}
void
mvfs_iovfh_to_mvfs_iovfh_32(struct mvfs_iovfh *vbl, struct mvfs_iovfh_32 *vbl_32)
{
	view_fhandle_to_view_fhandle_32(&vbl->vfh, &vbl_32->vfh);
}
void
mvfs_iovfh_32_to_mvfs_iovfh(struct mvfs_iovfh_32 *vbl_32, struct mvfs_iovfh *vbl)
{
	view_fhandle_32_to_view_fhandle(&vbl_32->vfh, &vbl->vfh);
}

void
mvfs_splitpool_32_to_mvfs_splitpool(struct mvfs_splitpool_32 *vbl_32, struct mvfs_splitpool *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->msp_prefix, &vbl->msp_prefix);
	mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(&vbl_32->msp_target, 
		&vbl->msp_target);
}

void
mvfs_credutl_sid_s_to_credutl_sid_s_32(struct credutl_sid_s *vbl, struct credutl_sid_s *vbl_32)
{   
    vbl_32->length = vbl->length;
    vbl_32->type = vbl->type;

	BCOPY((caddr_t)&vbl->sid[0], (caddr_t)&vbl_32->sid[0],
		sizeof(vbl->sid));
}

void
tbs_fstat_db_s_to_tbs_fstat_db_s_32(struct tbs_fstat_db_s *vbl, struct tbs_fstat_db_s_32 *vbl_32)
{
	vbl_32->type = vbl->type;
	vbl_32->mode = vbl->mode;
	vbl_32->nlink = vbl->nlink;
	vbl_32->flags = vbl->flags;

        mvfs_credutl_sid_s_to_credutl_sid_s_32(&vbl->usid, &vbl_32->usid);
        mvfs_credutl_sid_s_to_credutl_sid_s_32(&vbl->gsid, &vbl_32->gsid);

	vbl_32->size = vbl->size;
	vbl_32->nodeid = vbl->nodeid;
	mvfs_timeval_to_mvfs_timeval_32(&vbl->xtime, &vbl_32->xtime);
	mvfs_timeval_to_mvfs_timeval_32(&vbl->atime, &vbl_32->atime);
	mvfs_timeval_to_mvfs_timeval_32(&vbl->mtime, &vbl_32->mtime);
	mvfs_timeval_to_mvfs_timeval_32(&vbl->ctime, &vbl_32->ctime);
}

void
view_vstat_to_view_vstat_32(struct view_vstat *vbl, struct view_vstat_32 *vbl_32)
{
	tbs_fstat_db_s_to_tbs_fstat_db_s_32(&vbl->fstat, &vbl_32->fstat);
	vbl_32->mtype = vbl->mtype;
	tbs_oid_s_to_tbs_oid_s_32(&vbl->elem_oid, &vbl_32->elem_oid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->obj_oid, &vbl_32->obj_oid);
	mvfs_timeval_to_mvfs_timeval_32(&vbl->event_time, &vbl_32->event_time);
    
}

void
mvfs_xstat_to_mvfs_xstat_32(struct mvfs_xstat *vbl, struct mvfs_xstat_32 *vbl_32)
{

	vbl_32->vstat  = PTR_TO_PTR32(vbl->vstat);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->vob_oid, &vbl_32->vob_oid);
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->view_uuid, &vbl_32->view_uuid);
	vbl_32->view_hm = vbl->view_hm;
	vbl_32->xmode = vbl->xmode;
	vbl_32->flags = vbl->flags;
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->replica_uuid, &vbl_32->replica_uuid);
	BCOPY((caddr_t)&vbl->spare[0], (caddr_t)&vbl_32->spare[0],
		sizeof(vbl->spare));
}
void
mvfs_xstat_32_to_mvfs_xstat(struct mvfs_xstat_32 *vbl_32, struct mvfs_xstat *vbl)
{
	vbl->vstat = PTR32_TO_PTR(vbl_32->vstat);
	tbs_oid_s_32_to_tbs_oid_s(&vbl_32->vob_oid, &vbl->vob_oid);
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->view_uuid, &vbl->view_uuid);
	vbl->view_hm = vbl_32->view_hm;
	vbl->xmode = vbl_32->xmode;
	vbl->flags = vbl_32->flags;
	tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->replica_uuid, &vbl->replica_uuid);
	BCOPY((caddr_t)&vbl_32->spare[0], (caddr_t)&vbl->spare[0],
		sizeof(vbl_32->spare));
}

void
mvfs_ioinval_32_to_mvfs_ioinval( struct mvfs_ioinval_32 *vbl_32, struct mvfs_ioinval *vbl)
{
	vbl->utype = vbl_32->utype;

	switch (vbl->utype) {
	  case MVFS_IOINVAL_VOB_OID:
		tbs_oid_s_32_to_tbs_oid_s(&vbl_32->un.vob_oid, &vbl->un.vob_oid);
		break;
	  default:
		tbs_uuid_s_32_to_tbs_uuid_s(&vbl_32->un.replica_uuid, 
			&vbl->un.replica_uuid);
	}
	vbl->invaltype = vbl_32->invaltype;
	tbs_oid_s_32_to_tbs_oid_s(&vbl_32->obj_oid, &vbl->obj_oid);
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->nm, &vbl->nm);
}

void
mvfs_clrname_info_to_mvfs_clrname_info_32(struct mvfs_clrname_info *vbl, struct mvfs_clrname_info_32 *vbl_32)
{
	vbl_32->clrpool = vbl->clrpool;
	mfs_strbufpn_to_mfs_strbufpn_32(&vbl->clrname, &vbl_32->clrname);
}
void
mvfs_clrname_info_32_to_mvfs_clrname_info(struct mvfs_clrname_info_32 *vbl_32, struct mvfs_clrname_info *vbl)
{
	vbl->clrpool = vbl_32->clrpool;
	mfs_strbufpn_32_to_mfs_strbufpn(&vbl_32->clrname, &vbl->clrname);
}

void
mvfs_iochange_mtype_32_to_mvfs_iochange_mtype(struct mvfs_iochange_mtype_32 *vbl_32, struct mvfs_iochange_mtype *vbl)
{
	vbl->mtype = vbl_32->mtype;
}

void
mvfs_bhinfo_to_mvfs_bhinfo_32(struct mvfs_bhinfo *vbl, struct mvfs_bhinfo_32 *vbl_32)
{
	view_bhandle_to_view_bhandle_32(&vbl->bh, &vbl_32->bh);
	mvfs_timeval_to_mvfs_timeval_32(&vbl->bh_ref_time, &vbl_32->bh_ref_time);
	vbl_32->flags = vbl->flags;
}

void
mvfs_bhinfo_32_to_mvfs_bhinfo(struct mvfs_bhinfo_32 *vbl_32, struct mvfs_bhinfo *vbl)
{
	view_bhandle_32_to_view_bhandle(&vbl_32->bh, &vbl->bh);
	mvfs_timeval_32_to_mvfs_timeval(&vbl_32->bh_ref_time, &vbl->bh_ref_time);
	vbl->flags = vbl_32->flags;
}

void
mvfs_statbufs_32_to_mvfs_statbufs(struct mvfs_statbufs_32 *vbl_32, struct mvfs_statbufs *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->clntstat, &vbl->clntstat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->mnstat, &vbl->mnstat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->clearstat, &vbl->clearstat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->rvcstat, &vbl->rvcstat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->dncstat, &vbl->dncstat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->acstat, &vbl->acstat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->rlstat, &vbl->rlstat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->austat, &vbl->austat);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->vnopcnt, &vbl->vnopcnt);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->vfsopcnt, &vbl->vfsopcnt);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewopcnt, &vbl->viewopcnt);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewoptime, &vbl->viewoptime);
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewophist, &vbl->viewophist);
}

void
mvfs_io_xattr_to_mvfs_io_xattr_32(struct mvfs_io_xattr *vbl, struct mvfs_io_xattr_32 *vbl_32)
{
	vbl_32->xattr_type = vbl->xattr_type;
	vbl_32->xvalue = vbl->xvalue;
}
void
mvfs_io_xattr_32_to_mvfs_io_xattr(struct mvfs_io_xattr_32 *vbl_32, struct mvfs_io_xattr *vbl)
{
	vbl->xattr_type = vbl_32->xattr_type;
	vbl->xvalue = vbl_32->xvalue;
}

void
mvfs_export_viewinfo_32_to_mvfs_export_viewinfo(struct mvfs_export_viewinfo_32 *vbl_32, struct mvfs_export_viewinfo *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewtag, &vbl->viewtag);
	vbl->exportid = vbl_32->exportid;
}

void
mvfs_export_viewinfo_to_mvfs_export_viewinfo_32(struct mvfs_export_viewinfo *vbl, struct mvfs_export_viewinfo_32 *vbl_32)
{
	mfs_strbuf_to_mfs_strbuf_32(&vbl->viewtag, &vbl_32->viewtag);
	vbl_32->exportid = vbl->exportid;
}

void 
mfs_audit_object_sn_to_mfs_audit_object_sn_32(struct mfs_audit_object_sn *vbl, struct mfs_audit_object_sn_32 *vbl_32)
{
	vbl_32->sn_high = vbl->sn_high;
	vbl_32->sn_low = vbl->sn_low;
}
void 
mfs_auditrw_to_mfs_auditrw_32(struct mfs_auditrw *vbl, struct mfs_auditrw_32 *vbl_32)
{
	vbl_32->objtype = vbl->objtype;
	mvfs_timeval_to_mvfs_timeval_32(&vbl->objdtm, &vbl_32->objdtm);
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->viewuuid, &vbl_32->viewuuid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->voboid, &vbl_32->voboid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->objoid, &vbl_32->objoid);
	mfs_audit_object_sn_to_mfs_audit_object_sn_32(&vbl->objsn,&vbl_32->objsn);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->elemoid, &vbl_32->elemoid);
	vbl_32->mtype = vbl->mtype;
}
void 
mfs_auditdir_to_mfs_auditdir_32(struct mfs_auditdir *vbl, struct mfs_auditdir_32 *vbl_32)
{
	vbl_32->namlen = vbl->namlen;
	vbl_32->objtype = vbl->objtype;
	mvfs_timeval_to_mvfs_timeval_32(&vbl->objdtm, &vbl_32->objdtm);
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->viewuuid, &vbl_32->viewuuid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->voboid, &vbl_32->voboid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->objoid, &vbl_32->objoid);
	mfs_audit_object_sn_to_mfs_audit_object_sn_32(&vbl->objsn,&vbl_32->objsn);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->elemoid, &vbl_32->elemoid);
	vbl_32->mtype = vbl->mtype;
	tbs_oid_s_to_tbs_oid_s_32(&vbl->diroid, &vbl_32->diroid);
	/*
	 * Make sure we include the null terminator
	 */
	BCOPY(&vbl->name[0], &vbl_32->name[0], vbl->namlen + 1);
}
void 
mfs_auditrnm_to_mfs_auditrnm_32(struct mfs_auditrnm *vbl, struct mfs_auditrnm_32 *vbl_32)
{
	vbl_32->o_namlen = vbl->o_namlen;
	vbl_32->t_namlen = vbl->t_namlen;
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->viewuuid, &vbl_32->viewuuid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->o_diroid, &vbl_32->o_diroid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->t_diroid, &vbl_32->t_diroid);
	/*
	 * Make sure we include the null terminator for both strings
	 * String is of format: From_name\0To_name\0
	 */
	BCOPY(&vbl->name[0], &vbl_32->name[0], vbl->o_namlen + vbl->t_namlen + 2 );
}
void 
mfs_auditchoid_to_mfs_auditchoid_32(struct mfs_auditchoid *vbl, struct mfs_auditchoid_32 *vbl_32)
{
	vbl_32->objtype = vbl->objtype;
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->viewuuid, &vbl_32->viewuuid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->prevoid, &vbl_32->prevoid);
	tbs_oid_s_to_tbs_oid_s_32(&vbl->objoid, &vbl_32->objoid);
	mfs_audit_object_sn_to_mfs_audit_object_sn_32(&vbl->objsn,&vbl_32->objsn);
	vbl_32->mtype = vbl->mtype;
}
void 
mfs_auditview_to_mfs_auditview_32(struct mfs_auditview *vbl, struct mfs_auditview_32 *vbl_32)
{
	vbl_32->namlen = vbl->namlen;
	tbs_uuid_s_to_tbs_uuid_s_32(&vbl->viewuuid, &vbl_32->viewuuid);
	BCOPY(&vbl->name[0], &vbl_32->name[0], vbl->namlen+1);
}
void 
mfs_auditmarker_to_mfs_auditmarker_32(struct mfs_auditmarker *vbl, struct mfs_auditmarker_32 *vbl_32)
{
	vbl_32->markerval = vbl->markerval;
}
void 
mfs_auditrec_to_mfs_auditrec_32(struct mfs_auditrec *vbl, struct mfs_auditrec_32 *vbl_32)
{
	vbl_32->version = vbl->version;
	vbl_32->prevoff = vbl->prevoff;
	vbl_32->nextoff = vbl->nextoff;
	vbl_32->kind = vbl->kind;

	switch (vbl->kind) {
		case MFS_AR_ROOT:
		case MFS_AR_LOOKUP:
		case MFS_AR_RDLINK:
		case MFS_AR_LINK:
		case MFS_AR_CREATE:
		case MFS_AR_UNLINK:
		case MFS_AR_SYMLINK:
			mfs_auditdir_to_mfs_auditdir_32(&vbl->mfs_dirrec, 
				&vbl_32->mfs_dirrec_32);
			break;
		case MFS_AR_READ:
		case MFS_AR_WRITE:
		case MFS_AR_TRUNCATE:
			mfs_auditrw_to_mfs_auditrw_32(&vbl->mfs_rwrec, 
				&vbl_32->mfs_rwrec_32);
			break;
		case MFS_AR_RENAME:
			mfs_auditrnm_to_mfs_auditrnm_32(&vbl->mfs_rnmrec, 
				&vbl_32->mfs_rnmrec_32);
			break;
		case MFS_AR_CHOID:
			mfs_auditchoid_to_mfs_auditchoid_32(&vbl->mfs_choidrec, 
				&vbl_32->mfs_choidrec_32);
			break;
		case MFS_AR_VIEW:
			mfs_auditview_to_mfs_auditview_32(&vbl->mfs_viewrec,
				&vbl_32->mfs_viewrec_32);
			break;
		case MFS_AR_MARKER:
			mfs_auditmarker_to_mfs_auditmarker_32(&vbl->mfs_markerrec, 
				&vbl_32->mfs_markerrec_32);
			break;
	}
}
void 
mfs_auditbuf_to_mfs_auditbuf_32(struct mfs_auditrec *vbl, struct mfs_auditrec_32 *vbl_32, struct mfs_auditrec *curpos)
{
	struct mfs_auditrec *ap;
	struct mfs_auditrec_32 *xap;

	for (ap = vbl, xap = vbl_32; ap < curpos; 
			ap = MFS_NEXTREC(ap), xap = MFS_NEXTREC_32(xap)) {
		mfs_auditrec_to_mfs_auditrec_32(ap, xap);
	}
}
void
mvfs_viewstats_32_to_mvfs_viewstats(struct mvfs_viewstats_32 *vbl_32, struct mvfs_viewstats *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewtag, &vbl->viewtag);
	mvfs_statbufs_32_to_mvfs_statbufs(&vbl_32->stats, &vbl->stats);
}
void
mvfs_zero_viewstat_32_to_mvfs_zero_viewstat(struct mvfs_zero_viewstat_32 *vbl_32, struct mvfs_zero_viewstat *vbl)
{
	mfs_strbuf_32_to_mfs_strbuf(&vbl_32->viewtag, &vbl->viewtag);
}

void
mvfs_gfsinfo_to_mvfs_gfsinfo_32(struct mvfs_gfsinfo *gfsinfo, struct mvfs_gfsinfo_32 *gfsinfo_32)
{
	gfsinfo_32->gfsno  = gfsinfo->gfsno;
}

void
mvfs_gfsinfo_32_to_mvfs_gfsinfo(struct mvfs_gfsinfo_32 *gfsinfo_32, struct mvfs_gfsinfo *gfsinfo)
{
	gfsinfo->gfsno  = gfsinfo_32->gfsno;
}

#endif /* ATRIA_LP64 || ATRIA_LLP64 */
static const char vnode_verid_mvfs_transtype_c[] = "$Id:  2f93a83f.a23a11df.8bc7.00:01:84:7a:f2:e4 $";
