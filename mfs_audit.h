/* * (C) Copyright IBM Corporation 1990, 2008. */
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
#ifndef MFS_AUDIT_H_
#define MFS_AUDIT_H_

#include <tbs_base.h>
#include "linux/types.h"
#include "linux/param.h"

#define MFS_AUDITVERSION	11

/*
 * MFS structures for audit records
 */
typedef struct mfs_audit_object_sn {
	ks_uint32_t	sn_high;
	ks_uint32_t	sn_low;
} mfs_audit_object_sn_t;

struct mfs_auditrw {
	u_short	    		objtype;	/* Object type */
	struct timeval		objdtm;		/* Object DTM */
	tbs_uuid_t		viewuuid;	/* View UUID */
	tbs_oid_t   		voboid;		/* VOB OID */
	tbs_oid_t   		objoid;		/* Object OID */
	mfs_audit_object_sn_t	objsn;		/* Object serial number */
	tbs_oid_t		elemoid;	/* Element OID */
	ks_uint32_t		mtype;		/* Object meta-type */
};
typedef struct mfs_auditrw mfs_auditrw_t;

struct mfs_auditdir {
	u_short	    		namlen;		/* Length of name  */
	u_short	    		objtype;	/* Object type */
	struct timeval		objdtm;		/* Object DTM */
	tbs_uuid_t		viewuuid;	/* View UUID */
	tbs_oid_t   		voboid;		/* VOB OID */
	tbs_oid_t   		objoid;		/* Object OID */ 
	mfs_audit_object_sn_t	objsn;		/* Object serial number */
	tbs_oid_t		elemoid;	/* Element oid or viepvt OID */
	ks_uint32_t		mtype;		/* Object meta-type */
	tbs_oid_t   		diroid;		/* Dir OID (may be null) */
	char	    		name[AUDIT_MAXPATH];  /* Name */
};
typedef struct mfs_auditdir mfs_auditdir_t;

struct mfs_auditrnm {
	u_short			o_namlen;	/* From dir name len */
	u_short			t_namlen;	/* To dir name len */
	tbs_uuid_t		viewuuid;	/* View UUID */
	tbs_oid_t   		o_diroid;	/* From dir oid */
	tbs_oid_t		t_diroid;	/* To dir oid */
	char			name[AUDIT_MAXPATH];  /* From name\0To name */
};
typedef struct mfs_auditrnm mfs_auditrnm_t;

struct mfs_auditchoid {
	tbs_uuid_t		viewuuid;	/* View UUID */
	tbs_oid_t		prevoid;	/* Previous version oid */
	tbs_oid_t		objoid;		/* Current verion oid */
	mfs_audit_object_sn_t	objsn;		/* Object serial number */
	ks_uint32_t		mtype;		/* Object meta-type */
	u_short			objtype;	/* Object type */
};
typedef struct mfs_auditchoid mfs_auditchoid_t;

struct mfs_auditview {
	u_short			namlen;		/* Name length */
	tbs_uuid_t		viewuuid;	/* View uuid */
	char			name[AUDIT_MAXPATH];	/* View tag name */
};
typedef struct mfs_auditview mfs_auditview_t;

struct mfs_auditmarker {
	ks_uint32_t		markerval;	/* value bits */
};
typedef struct mfs_auditmarker mfs_auditmarker_t;

struct mfs_auditrec {
	u_short			version;	/* Version of record */
	u_short			prevoff;	/* Offset to previous record */
	u_short			nextoff;	/* Offset to next record */
	u_short	    		kind;		/* Kind of audit record */
	union mfs_ar_u {
	    struct mfs_auditrw  rw;		/* Form for read/write */
	    struct mfs_auditdir dir;		/* Form for dir ops */
	    struct mfs_auditrnm rnm;		/* Form for rename */
	    struct mfs_auditchoid choid;	/* Form for choid */
	    struct mfs_auditview view;		/* Form for view */
	    struct mfs_auditmarker marker;	/* Form for marker */
	} mfs_ar_un;
};
typedef struct mfs_auditrec mfs_auditrec_t;

#define mfs_rwrec	mfs_ar_un.rw
#define mfs_dirrec	mfs_ar_un.dir
#define mfs_rnmrec	mfs_ar_un.rnm
#define mfs_choidrec	mfs_ar_un.choid
#define mfs_viewrec	mfs_ar_un.view
#define mfs_markerrec	mfs_ar_un.marker

/*
 * Define auditrec kinds and type of union.
 */

#define MFS_AR_ROOT	1	/* dirrec, name=mntpath */
#define MFS_AR_LOOKUP	2	/* dirrec */
#define MFS_AR_READ	3	/* rwrec */
#define MFS_AR_WRITE	4	/* rwrec */
#define MFS_AR_RDLINK	5	/* dirrec, diroid nil, name=link text */
#define MFS_AR_LINK	6	/* dirrec */
#define MFS_AR_CREATE	7	/* dirrec */
#define MFS_AR_RENAME   8	/* rnmrec */
#define MFS_AR_UNLINK	9	/* dirrec */
#define MFS_AR_TRUNCATE 10	/* rwrec */
#define MFS_AR_CHOID	11	/* choidrec */
#define MFS_AR_VIEW	12	/* view rec, name=viewtag */
#define MFS_AR_SYMLINK	13	/* dirrec */
#define MFS_AR_MARKER	14	/* marker */

/*
 * Define object types
 */

#define MFS_OT_NONE	0
#define MFS_OT_REG	1
#define MFS_OT_DIR	2
#define MFS_OT_BLK	3
#define MFS_OT_CHR	4
#define MFS_OT_LNK	5

/* 
 * Macro MFS_AUDITSIZ(ap) gives the space (in bytes) used by an audit
 * record entry read from the audit output file.
 */

#define MFS_AUDITSIZ(ap)  ((mfs_auditrec_t *)(ap))->nextoff

/*
 * Following macros are used to determine the size of
 * an audit record when writing them based on its kind.
 */
#define MFS_AUDITHDRSIZ   (sizeof(mfs_auditrec_t)-sizeof(union mfs_ar_u))
#ifndef MFS_AUDIT_ALIGN_BDRY
#ifndef ATRIA_LP64
#define MFS_AUDIT_ALIGN_BDRY 3
#else
#define MFS_AUDIT_ALIGN_BDRY 7
#endif
#endif
#define MFS_AUDITRWSIZ  \
	((MFS_AUDITHDRSIZ  + sizeof(mfs_auditrw_t) + MFS_AUDIT_ALIGN_BDRY) & ~MFS_AUDIT_ALIGN_BDRY)
#define MFS_AUDITDIRSIZ(len) \
	((MFS_AUDITHDRSIZ + (sizeof(mfs_auditdir_t) - AUDIT_MAXPATH) + \
		((len)+1) + MFS_AUDIT_ALIGN_BDRY) & ~MFS_AUDIT_ALIGN_BDRY)
#define MFS_AUDITRNMSIZ(len1, len2) \
	((MFS_AUDITHDRSIZ + (sizeof(mfs_auditrnm_t) - AUDIT_MAXPATH) + \
		((len1)+1+(len2)+1) + MFS_AUDIT_ALIGN_BDRY) & ~MFS_AUDIT_ALIGN_BDRY)
#define MFS_AUDITCHOIDSIZ \
	((MFS_AUDITHDRSIZ+ sizeof(mfs_auditchoid_t) +MFS_AUDIT_ALIGN_BDRY ) & ~MFS_AUDIT_ALIGN_BDRY)
#define MFS_AUDITVIEWSIZ(len) \
	((MFS_AUDITHDRSIZ + (sizeof(mfs_auditview_t) - AUDIT_MAXPATH) + \
		((len)+1) + MFS_AUDIT_ALIGN_BDRY) & ~MFS_AUDIT_ALIGN_BDRY)
#define MFS_AUDITMARKERSIZ ((MFS_AUDITHDRSIZ + sizeof(mfs_auditmarker_t) + \
			     MFS_AUDIT_ALIGN_BDRY) & ~MFS_AUDIT_ALIGN_BDRY)

/*
 * MFS_PREVREC and MFS_NEXTREC return a pointer to the previous 
 * or next record in a buffer.  No handling for buffer wrap/oveflow
 * is provided.
 */

#define MFS_PREVREC(ap) 	((mfs_auditrec_t *)((char *)ap-ap->prevoff))
#define MFS_NEXTREC(ap) 	((mfs_auditrec_t *)((char *)ap+ap->nextoff))

/*
 * MFS_AUDITOFF returns the byte offset of the audit record "ap"
 * in the buffer "bp".
 */
#define MFS_AUDITOFF(ap, bp) ((char *)ap - (char *)bp)

/*
 * Define kernel constants
 */

/*
 * Structure passed to internal kernel mfs_audit() routine with saved
 * stat information on the removed object
 */

struct mfs_auditrmstat {
	u_short	    		objtype;	/* Object type */
	struct timeval		objdtm;		/* Object DTM */
	tbs_oid_t   		voboid;		/* VOB OID */
	tbs_oid_t   		objoid;		/* Object OID */ 
	mfs_audit_object_sn_t	objsn;		/* Object serial number */
	tbs_oid_t		elemoid;	/* Element OID */
	ks_uint32_t		mtype;		/* Object meta-type */
};
typedef struct mfs_auditrmstat mfs_auditrmstat_t;

#endif /* MFS_AUDIT_H_ */
/* $Id: f681020e.9c1c11dd.9a62.00:01:83:29:c0:fc $ */
