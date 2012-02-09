/* * (C) Copyright IBM Corporation 1991, 2005. */
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

/* xdr_view_kernel.c */
/*LINTLIBRARY*/

#include "mvfs_systm.h"
#include "mvfs.h"

#include "view_rpc_kernel.h"
/* Following is for xdr_ks_canon_pname_t prototype */
#include <xdr_ks.h>

EZ_XDR_ROUTINE(view_fhandle_t)
{

    if (xdrs->x_op == XDR_FREE)
	return TRUE;

    if (!xdr_tbs_uuid_t(xdrs, &objp->vob_uuid EZ_XDR_ARG_PASSTHRU)) {
	return (FALSE);
    }
    return (xdr_tbs_dbid_t(xdrs, &objp->ver_dbid EZ_XDR_ARG_PASSTHRU) &&
	    xdr_tbs_dbid_t(xdrs, &objp->elem_dbid EZ_XDR_ARG_PASSTHRU) &&
	    xdr_u_long(xdrs, &objp->gen) &&
	    xdr_u_long(xdrs, &objp->flags) &&
	    xdr_u_long(xdrs, &objp->pad0));
}

EZ_XDR_ROUTINE(view_vstat_t)
{
    if (xdrs->x_op == XDR_FREE)
	return TRUE;

    return (xdr_tbs_fstat_db_t(xdrs, &objp->fstat EZ_XDR_ARG_PASSTHRU) &&
	    xdr_vob_mtype_t(xdrs, &objp->mtype EZ_XDR_ARG_PASSTHRU) &&
	    xdr_tbs_oid_t(xdrs, &objp->elem_oid EZ_XDR_ARG_PASSTHRU) &&
	    xdr_tbs_oid_t(xdrs, &objp->obj_oid EZ_XDR_ARG_PASSTHRU) &&
	    xdr_timeval(xdrs, &objp->event_time));
}

EZ_XDR_ROUTINE(view_set_attr_t)
{
    ks_uint32_t size_high, size_low;
    credutl_sid_t *usidp = &objp->usid;
    credutl_sid_t *gsidp = &objp->gsid;

    if (xdrs->x_op == XDR_FREE)
	return TRUE;

    /* Note: We always send the entire structure regardless of the
     * mask bits. */
    if (!xdr_u_long(xdrs, &objp->mask) ||
        !xdr_u_long(xdrs, &objp->type) ||
        !xdr_u_long(xdrs, &objp->mode))
        return FALSE;

    switch (xdrs->x_op) {
      case XDR_ENCODE:
        if ((objp->mask & VIEW_ATTR_USID) == 0)
            usidp = (credutl_sid_t *)&CREDUTL_SID_DONTCARE;
        if ((objp->mask & VIEW_ATTR_GSID) == 0)
            gsidp = (credutl_sid_t *)&CREDUTL_SID_DONTCARE;
	/* Fall through. */
      case XDR_DECODE:
        if (!xdr_credutl_sid_t(xdrs, usidp EZ_XDR_ARG_PASSTHRU) ||
            !xdr_credutl_sid_t(xdrs, gsidp EZ_XDR_ARG_PASSTHRU))
            return FALSE;
        break;
      default:
        break;
    }

    if (xdrs->x_op == XDR_ENCODE) {
        KS_ULLONG_SPLIT(objp->size, size_high, size_low);
    }

    if (!XDR_KS_U_INT32(xdrs, &size_low) ||
        !XDR_KS_U_INT32(xdrs, &size_high) ||
        !xdr_timeval(xdrs, &objp->atime) ||
        !xdr_timeval(xdrs, &objp->mtime))
    {
        return (FALSE);
    }

    if (xdrs->x_op == XDR_DECODE) {
        objp->size = KS_ULLONG_COMBINE(size_high, size_low);
    }

    return (TRUE);
}

EZ_XDR_ROUTINE(view_build_session_t)
{
    return xdr_u_long(xdrs, objp);
}

EZ_XDR_ROUTINE(view_name_state_t)
{
    return xdr_u_long(xdrs, objp);
}

bool_t
xdr_view_hdr_req_t(
     XDR *xdrs,
     view_hdr_req_t *objp)
{

    if (xdrs->x_op == XDR_FREE)
	return TRUE;
    return (xdr_time_t(xdrs, &objp->boot_time EZ_XDR_ARG_TIME_T) &&
	    xdr_xid_t(xdrs, &objp->xid EZ_XDR_ARG) &&
	    xdr_view_handle_t(xdrs, &objp->view EZ_XDR_ARG) &&
	    xdr_view_bhandle_t(xdrs, &objp->build_handle EZ_XDR_ARG));
}

bool_t
xdr_view_hdr_reply_t(
     XDR *xdrs,
     view_hdr_reply_t *objp)
{

    if (xdrs->x_op == XDR_FREE)
	return TRUE;
    return (xdr_xid_t(xdrs, &objp->xid EZ_XDR_ARG) &&
	    xdr_tbs_status_t(xdrs, &objp->status EZ_XDR_ARG));
}

EZ_XDR_ROUTINE(view_dir_arg_t)
{
    return (xdr_view_fhandle_t(xdrs, &objp->d_fhandle EZ_XDR_ARG_PASSTHRU) &&
	    xdr_string(xdrs, &objp->name, TBS_MAX_NAME_LEN));
}

bool_t
xdr_view_null_req_t(
     XDR *xdrs,
     view_null_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr));
}

bool_t
xdr_view_null_reply_t(
     XDR *xdrs,
     view_null_reply_t *objp)
{
    return (xdr_view_hdr_reply_t(xdrs, &objp->hdr));
}

bool_t
xdr_view_dir_req_t(
     XDR *xdrs,
     view_dir_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_dir_arg_t(xdrs, &objp->file EZ_XDR_ARG) &&
	    xdr_timeval(xdrs, &objp->dir_dtm));
}

bool_t
xdr_view_dir_reply_t(
     XDR *xdrs,
     view_dir_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_view_vstat_t(xdrs, &objp->vstat EZ_XDR_ARG));
}

EZ_XDR_ROUTINE(view_dir_mod_t)
{
    return (xdr_view_vstat_t(xdrs, &objp->dvstat EZ_XDR_ARG_PASSTHRU) &&
	    xdr_bool(xdrs, &objp->dir_dtm_valid));
}

bool_t
xdr_view_dir_mod_reply_t(
     XDR *xdrs,
     view_dir_mod_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_view_dir_mod_t(xdrs, &objp->dir_mod EZ_XDR_ARG));
}

bool_t
xdr_view_replica_root_req_t(
     XDR *xdrs,
     view_replica_root_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_tbs_oid_t(xdrs, &objp->vob_root EZ_XDR_ARG) &&
	    xdr_tbs_uuid_t(xdrs, &objp->replica_root EZ_XDR_ARG) &&
	    xdr_string(xdrs, &objp->host_name, TBS_MAX_NAME_LEN) &&
	    xdr_string(xdrs, &objp->host_pathname, TBS_MAX_PNAME_LEN) &&
	    xdr_view_hm_warp_opt_t(xdrs, &objp->hm_warp_opt) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->pname EZ_XDR_ARG));
}

bool_t
xdr_view_root_reply_t(
     XDR *xdrs,
     view_root_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_view_vstat_t(xdrs, &objp->vstat EZ_XDR_ARG) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_bool(xdrs, &objp->bh_invariant));
}

bool_t
xdr_view_lookup_req_t(
     XDR *xdrs,
     view_lookup_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->d_fhandle EZ_XDR_ARG) &&
	    xdr_string(xdrs, &objp->name, TBS_MAX_NAME_LEN) &&
	    xdr_view_hm_warp_opt_t(xdrs, &objp->hm_warp_opt) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->residual_pname EZ_XDR_ARG));
}

bool_t
xdr_view_lookup_reply_t(
     XDR *xdrs,
     view_lookup_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr) ||
	!xdr_view_name_state_t(xdrs, &objp->name_state EZ_XDR_ARG)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_view_vstat_t(xdrs, &objp->vstat EZ_XDR_ARG) &&
	    xdr_size_t(xdrs, &objp->residual_resolved EZ_XDR_ARG) &&
	    xdr_bool(xdrs, &objp->bh_invariant) &&
	    xdr_timeval(xdrs, &objp->lvut));
}

bool_t
xdr_view_getattr_req_t(
     XDR *xdrs,
     view_getattr_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG));
}

bool_t
xdr_view_getattr_reply_t(
     XDR *xdrs,
     view_getattr_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_view_vstat_t(xdrs, &objp->vstat EZ_XDR_ARG) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_timeval(xdrs, &objp->lvut) &&
	    xdr_bool(xdrs, &objp->bh_invariant));
}

bool_t
xdr_view_setattr_req_t(
     XDR *xdrs,
     view_setattr_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_view_set_attr_t(xdrs, &objp->sattr EZ_XDR_ARG));
}

bool_t
xdr_view_readlink_req_t(
     XDR *xdrs,
     view_readlink_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_size_t(xdrs, &objp->max_text_size EZ_XDR_ARG));
}

bool_t
xdr_view_readlink_reply_t(
     XDR *xdrs,
     view_readlink_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_size_t(xdrs, &objp->text_size EZ_XDR_ARG) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->text EZ_XDR_ARG));
}

bool_t
xdr_view_create_req_t(
     XDR *xdrs,
     view_create_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_dir_arg_t(xdrs, &objp->create EZ_XDR_ARG) &&
	    xdr_view_set_attr_t(xdrs, &objp->iattr EZ_XDR_ARG) &&
	    xdr_size_t(xdrs, &objp->max_text_size EZ_XDR_ARG) &&
	    xdr_timeval(xdrs, &objp->dir_dtm));
}

bool_t
xdr_view_create_reply_t(
     XDR *xdrs,
     view_create_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_view_vstat_t(xdrs, &objp->vstat EZ_XDR_ARG) &&
	    xdr_view_dir_mod_t(xdrs, &objp->dir_mod EZ_XDR_ARG) &&
	    xdr_size_t(xdrs, &objp->text_size EZ_XDR_ARG) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->text EZ_XDR_ARG));
}

bool_t
xdr_view_link_req_t(
     XDR *xdrs,
     view_link_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_view_dir_arg_t(xdrs, &objp->to EZ_XDR_ARG) &&
	    xdr_timeval(xdrs, &objp->dir_dtm));
}

bool_t
xdr_view_rename_req_t(
     XDR *xdrs,
     view_rename_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_dir_arg_t(xdrs, &objp->from EZ_XDR_ARG) &&
	    xdr_view_dir_arg_t(xdrs, &objp->to EZ_XDR_ARG) &&
	    xdr_timeval(xdrs, &objp->dir_dtm));
}

bool_t
xdr_view_symlink_req_t(
     XDR *xdrs,
     view_symlink_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_dir_arg_t(xdrs, &objp->create EZ_XDR_ARG) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->text EZ_XDR_ARG) &&
	    xdr_view_set_attr_t(xdrs, &objp->iattr EZ_XDR_ARG) &&
	    xdr_timeval(xdrs, &objp->dir_dtm));
}

bool_t
xdr_view_symlink_reply_t(
     XDR *xdrs,
     view_symlink_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_view_vstat_t(xdrs, &objp->vstat EZ_XDR_ARG) &&
	    xdr_view_dir_mod_t(xdrs, &objp->dir_mod EZ_XDR_ARG));
}

bool_t
xdr_view_mkdir_req_t(
     XDR *xdrs,
     view_mkdir_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_dir_arg_t(xdrs, &objp->create EZ_XDR_ARG) &&
	    xdr_view_set_attr_t(xdrs, &objp->iattr EZ_XDR_ARG) &&
	    xdr_timeval(xdrs, &objp->dir_dtm));
}

bool_t
xdr_view_readdir_req_t(
     XDR *xdrs,
     view_readdir_req_t *objp)
{
    int size_high = 0; /* Reserving a high field for the sizes to support 64 bit
                          sizes in the future */
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->d_fhandle EZ_XDR_ARG) &&
	    xdr_u_long(xdrs, &objp->offset) &&
            xdr_int(xdrs, &size_high) &&
	    xdr_size_t(xdrs, &objp->max_dirent_size EZ_XDR_ARG));
}

bool_t
xdr_view_readdir_reply_t(
     XDR *xdrs,
     view_readdir_reply_t *objp)
{

#define STRUCT_DIRENT		KDIRENT_T
#define STRUCT_DIRENT_SIZE(namelen) \
				KDIRENT_RECLEN(namelen)
#define DIRENT_GET_NAME(dp) 	      KDIRENT_GET_NAME(dp)
#define DIRENT_SET_DELIM(dp,nmlen)    KDIRENT_GET_NAME(dp)[nmlen] = '\0';
#define DIRENT_SET_INO(dp,ino)        KDIRENT_SET_INO(dp,ino)
#define DIRENT_SET_OFF(dp,off)        KDIRENT_SET_OFF(dp,off)
#define DIRENT_SET_NAMLEN(dp, nmlen)  KDIRENT_SET_NAMLEN(dp, nmlen)
#define DIRENT_SET_RECLEN(dp, nmlen)  KDIRENT_SET_RECLEN(dp, nmlen)

    size_t total_size;
    u_int reclen;
    view_dir_off_t offset;
    u_long ino;
    char *name;
    u_int namelen;
    STRUCT_DIRENT *dp;
    int size_high = 0; /* Reserving a high field for the size to support 64 bit
                          sizes in the future */

    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    switch (xdrs->x_op) {
      case XDR_FREE:	
	if (objp->ents != NULL) {
	    KMEM_FREE(objp->ents, (u_int)VIEW_READDIR_MAX_SIZE);
	    objp->ents = NULL;
	}
	break;
      case XDR_ENCODE:
	return FALSE;                   /* encode not used in kernel */
      case XDR_DECODE:
	total_size = objp->max_dirent_size;
	if ((dp = (STRUCT_DIRENT *)objp->ents) == NULL) {
	    dp = (STRUCT_DIRENT *)KMEM_ALLOC((u_int) total_size, KM_SLEEP);
	    if ((objp->ents = (view_dirent_t *) dp) == NULL) {
		return (FALSE);
	    }
	}

	for (;; dp = (STRUCT_DIRENT *)((char *)dp + reclen)) {
	
	    if (!xdr_u_int(xdrs, &namelen)) {
		return (FALSE);
	    }
	    if (namelen == 0) {
		/*
		 * End of the directory entries being sent.
		 */
		break;
	    }
	    /*
	     * See if there is room in the returned record.
	     */
	    if ((reclen = STRUCT_DIRENT_SIZE(namelen)) > total_size) {
		objp->eof = FALSE;
		goto too_much;
	    }

	    if (!xdr_opaque(xdrs, DIRENT_GET_NAME(dp), namelen) ||
		!xdr_u_long(xdrs, (u_long *)&ino) ||
		!xdr_view_dir_off_t(xdrs, &offset EZ_XDR_ARG) ||
                !xdr_int(xdrs, &size_high)) {
		return (FALSE);
	    }
	    DIRENT_SET_DELIM(dp,namelen);
	    DIRENT_SET_OFF(dp,offset);
	    DIRENT_SET_NAMLEN(dp,namelen);
	    DIRENT_SET_INO(dp,ino);
	    DIRENT_SET_RECLEN(dp,namelen);
	    total_size -= reclen;
	}
	   
	if (!xdr_bool(xdrs, &objp->eof)) {
	    return (FALSE);
	}
      too_much:
	objp->offset = (u_long)offset;	/* Last offset that we got */
	objp->size = (char *)dp - (char *)(objp->ents);
	break;
    }
    return (TRUE);
}

bool_t
xdr_view_cltxt_reply_t(
     XDR *xdrs,
     view_cltxt_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_size_t(xdrs, &objp->text_size EZ_XDR_ARG) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->text EZ_XDR_ARG) &&
	    xdr_bool(xdrs, &objp->vob));
}

bool_t
xdr_view_invalidate_req_t(
     XDR *xdrs,
     view_invalidate_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_enum(xdrs, (enum_t *)&objp->type) &&
	    xdr_tbs_oid_t(xdrs, &objp->vob_oid EZ_XDR_ARG) &&
	    xdr_tbs_oid_t(xdrs, &objp->obj_oid EZ_XDR_ARG) &&
	    xdr_string(xdrs, &objp->name, TBS_MAX_NAME_LEN));
}

bool_t
xdr_view_change_mtype_req_t(
     XDR *xdrs,
     view_change_mtype_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_vob_mtype_t(xdrs, &objp->mtype EZ_XDR_ARG));
}

bool_t
xdr_view_change_oid_req_t(
     XDR *xdrs,
     view_change_oid_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_enum(xdrs, (enum_t *)&objp->option));
}

bool_t
xdr_view_gpath_req_t(
     XDR *xdrs,
     view_gpath_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_view_fhandle_t(xdrs, &objp->fhandle EZ_XDR_ARG) &&
	    xdr_tbs_boolean_t(xdrs, &objp->hm_force EZ_XDR_ARG) &&
	    xdr_size_t(xdrs, &objp->max_path_size EZ_XDR_ARG) &&
	    xdr_size_t(xdrs, &objp->max_ext_path_size EZ_XDR_ARG));
}

bool_t
xdr_view_gpath_reply_t(
     XDR *xdrs,
     view_gpath_reply_t *objp)
{
    if (!xdr_view_hdr_reply_t(xdrs, &objp->hdr)) {
	return (FALSE);
    }
    if (objp->hdr.status != TBS_ST_OK) {
	return (TRUE);
    }
    return (xdr_size_t(xdrs, &objp->path_size EZ_XDR_ARG) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->path EZ_XDR_ARG) &&
	    xdr_size_t(xdrs, &objp->ext_path_size EZ_XDR_ARG) &&
	    xdr_ks_canon_pname_p_t(xdrs, &objp->ext_path EZ_XDR_ARG));
}

EZ_XDR_ROUTINE(view_handle_t)
{
    return xdr_tbs_uuid_t((xdrs), &(objp)->view_uuid EZ_XDR_ARG_PASSTHRU);
}

EZ_XDR_ROUTINE(view_bhandle_t)
{

    if (xdrs->x_op == XDR_FREE)
	return TRUE;
    return (xdr_view_build_session_t(xdrs, &objp->build_session EZ_XDR_ARG_PASSTHRU) &&
	    xdr_u_long(xdrs, &objp->target_id));
}

bool_t
xdr_view_getprop_req_t(
     XDR *xdrs,
     view_getprop_req_t *objp)
{
    return (xdr_view_hdr_req_t(xdrs, &objp->hdr) &&
	    xdr_enum(xdrs, (enum_t *)&objp->vprop));
}

bool_t
xdr_view_getprop_reply_t(
     XDR *xdrs,
     view_getprop_reply_t *objp)
{
    return (xdr_view_hdr_reply_t(xdrs, &objp->hdr) &&
	    xdr_u_long(xdrs, &objp->pvalue));
}
static const char vnode_verid_xdr_view_kernel_c[] = "$Id:  f02d4dd8.16a311d7.939c.00:01:80:ae:c2:81 $";
