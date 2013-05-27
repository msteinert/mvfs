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
#ifndef MVFS_TRANSTYPE_H_
#define MVFS_TRANSTYPE_H_


#if defined(ATRIA_WIN32_COMMON) || defined(ATRIA_LP64) || defined(ATRIA_LLP64)
typedef ks_uint32_t mvfs_size_t_32;
typedef ks_uint32_t mvfs_boolean_t_32;
typedef ks_uint32_t mvfs_dbid_t_32;

struct mfs_strbufpn_32 {
    ptr32_t s;
    mvfs_size_t_32 l;
    mvfs_size_t_32 m;
};
typedef struct mfs_strbufpn_32 mfs_strbufpn_t_32;

struct mvfs_iofid_32 {
    ks_uint32_t dbid;
    ks_uint32_t gen;
};

struct view_bhandle_32 {
    ks_uint32_t build_session;
    ks_uint32_t target_id;
};

struct timeval_32 {
    ks_int32_t tv_sec;
    ks_int32_t tv_usec;
};

struct mfs_ioncent_32 {
    ks_uint32_t offset;
    ks_uint16_t eocache;
    ks_uint16_t flags;
    ks_int32_t addtime;
    struct mfs_strbufpn_32 dvw;
    struct mfs_strbufpn_32 mp;
    struct mvfs_iofid_32 dfid;
    struct mfs_strbufpn_32 nm;
    struct view_bhandle_32 bhlist[MVFS_IONCBHMAX];
    struct mfs_strbufpn_32 vw;
    struct mvfs_iofid_32 fid;
    struct timeval_32 evtime;
};

/* The first field in a timestruc_t is a time_t on most platforms. */
struct timestruc_32 {
    ks_int32_t tv_sec;
    ks_int32_t tv_nsec;
};

/*
 * The version field used to be the first field in the stats structures before
 * changing the statistics fields to 64-bit ints.  However, some compilers
 * (e.g. in 64-bit kernels on x86_64 machines) would put a word of padding in
 * the structure to align the statistics field accesses in that case.  Since
 * user space is 32-bit, the compiler did not put in this padding when building
 * user space, e.g. the mvfsstat command.  We considered these alternatives to
 * solve this problem:
 *
 * - Declaring an explicit padding field.  That would waste space for the
 *   architectures that didn't need it.
 *
 * - Using compiler options to pack the structures, e.g. #pragma pack(4) on
 *   Solaris or __attribute__ ((packed)) on GCC.  However, that is fragile and
 *   could lead to errors if the architecture requires that 64-bit fields be
 *   aligned to be accessed correctly (e.g. Sparc).
 *
 * - Changing the version to a 64-bit field.  That would waste space, as above,
 *   and it could also cause the version number check done by, e.g. mvfsstat,
 *   to fail if user space expected an older version in a 32-bit field (since
 *   the version would now be in the 2nd word of the 2 word version field).
 *
 * - Moving the version field to the end of the structure.  That does not use
 *   any extra space, but it suffers from the same version checking problem as
 *   above (since now mvfsstat would be looking at the first word of the first
 *   statistics field in the structure).
 *
 * The final decision was to move the version field to the end of the
 * structure.  This does mean that we won't be able to run later MVFS kernels
 * with this change against an older user space installation.  This should not
 * be a problem for customers since we ship user space and the kernel together.
 */

struct mfs_clntstat_32 {
    MVFS_STAT_CNT_T clntget;
    MVFS_STAT_CNT_T clntfree;
    MVFS_STAT_CNT_T clntcreate;
    MVFS_STAT_CNT_T clntdestroy;
    MVFS_STAT_CNT_T clntcalls;
    MVFS_STAT_CNT_T clntretries;
    MVFS_STAT_CNT_T mfscall;
    MVFS_STAT_CNT_T mfsfail;
    MVFS_STAT_CNT_T mfsintr;
    MVFS_STAT_CNT_T mfsmaxdelay;
    MVFS_STAT_CNT_T mfsmaxdelaytime;
    struct timestruc_32  mvfsthread_time;
    ks_uint32_t version;
};

struct mfs_clearstat_32 {
    MVFS_STAT_CNT_T clearget;
    MVFS_STAT_CNT_T clearcreate;
    MVFS_STAT_CNT_T clearraces;
    MVFS_STAT_CNT_T clearcreatraces;
    MVFS_STAT_CNT_T clearread;
    MVFS_STAT_CNT_T clearwrite;
    struct timestruc_32  clearget_time;
    struct timestruc_32  clearcreat_time;
    struct timestruc_32  clearrd_time;
    struct timestruc_32  clearwr_time;
    struct timestruc_32  clearopen_time;
    struct timestruc_32  unclearrd_time;
    struct timestruc_32  unclearwr_time;
    struct timestruc_32  unclearget_time;
    struct timestruc_32  cto_getattr_time;
    MVFS_STAT_CNT_T clearopen;
    MVFS_STAT_CNT_T unclearopen;
    MVFS_STAT_CNT_T cleargetmiss;
    MVFS_STAT_CNT_T clearreclaim;
    MVFS_STAT_CNT_T clearreclaimmiss;
    MVFS_STAT_CNT_T cleargetlkup;
    ks_uint32_t version;
};

struct mfs_austat_32 {
    MVFS_STAT_CNT_T au_calls;
    MVFS_STAT_CNT_T au_vgetattr;
    MVFS_STAT_CNT_T au_nvgetattr;
    MVFS_STAT_CNT_T au_dupl;
    struct timestruc_32  au_time;
    struct timestruc_32  au_settime;
    struct timestruc_32  au_ioctltime;
    ks_uint32_t version;
};

struct mfs_rpchist_32 {
    struct timestruc_32  histval[MFS_NUM_HISTX];
    MVFS_STAT_CNT_T histrpc[MFS_NUM_HISTX];
    MVFS_STAT_CNT_T histclr[MFS_NUM_HISTX];
    MVFS_STAT_CNT_T histperop[VIEW_NUM_PROCS][MFS_NUM_HISTX];
    ks_uint32_t version;
};

extern void mfs_strbufpn_to_mfs_strbufpn_32(struct mfs_strbufpn *, struct mfs_strbufpn_32 *);
extern void mfs_strbufpn_32_to_mfs_strbufpn(struct mfs_strbufpn_32 *, struct mfs_strbufpn *);
extern void timeval_to_timeval_32(struct timeval *vbl, struct timeval_32 *vbl_32);
extern void timeval_32_to_timeval(struct timeval_32 *vbl_32, struct timeval *vbl);
extern void mvfs_iofid_to_mvfs_iofid_32(struct mvfs_iofid *, struct mvfs_iofid_32 *);
extern void mvfs_iofid_32_to_mvfs_iofid(struct mvfs_iofid_32 *, struct mvfs_iofid *);
extern void view_bhandle_to_view_bhandle_32(struct view_bhandle *, struct view_bhandle_32 *);
extern void view_bhandle_32_to_view_bhandle(struct view_bhandle_32 *, struct view_bhandle *);
extern void mfs_ioncent_to_mfs_ioncent_32(struct mfs_ioncent *, struct mfs_ioncent_32 *);
extern void mfs_ioncent_32_to_mfs_ioncent(struct mfs_ioncent_32 *, struct mfs_ioncent *);
extern void mfs_timestruc_to_mfs_timestruc_32(timestruc_t *, struct timestruc_32 *);
extern void mfs_clntstat_to_mfs_clntstat_32(struct mfs_clntstat *, struct mfs_clntstat_32 *);
extern void mfs_clearstat_to_mfs_clearstat_32(struct mfs_clearstat *, struct mfs_clearstat_32 *);
extern void mfs_austat_to_mfs_austat_32(struct mfs_austat *, struct mfs_austat_32 *);
extern void mfs_rpchist_to_mfs_rpchist_32(struct mfs_rpchist *, struct mfs_rpchist_32 *);
#endif
#if defined(ATRIA_LP64) || defined(ATRIA_LLP64)
struct mfs_strbuf_32 {
    ptr32_t s;
    mvfs_size_t_32 l;
    mvfs_size_t_32 m;
};

typedef struct mfs_strbuf_32 mfs_strbuf_t_32;

struct mfs_strbufpn_pair_32 {
    mfs_strbufpn_t_32 upn;
    mfs_strbufpn_t_32 kpn;
};

typedef struct mfs_strbufpn_pair_32 mfs_strbufpn_pair_t_32;

struct tbs_uuid_s_32 {
    ks_uint32_t time_low;
    ks_uint16_t time_mid;
    ks_uint16_t time_hi_and_version;
    ks_byte_t clock_seq_hi_and_reserved;
    ks_byte_t clock_seq_low;
    ks_byte_t node[6];
};

struct mvfs_gfsinfo_32 {
    ks_int32_t  gfsno;
};
struct tbs_oid_s_32 {
    struct tbs_uuid_s_32 obj_uuid;
};

struct mfs_mntargs_32 {
    ks_uint16_t mma_mntvers;
    ks_uint16_t mma_mntsize;
    ks_uint32_t mma_flags;
    struct mfs_strbuf_32 mma_mntpath;
    struct mfs_strbuf_32  mma_host;

    u_short mma_port;         /* in_port_t (same in 32-bit, 64-bit kernels). */
    struct mfs_strbufpn_pair_32  mma_spath;
    struct mfs_strbuf_32 mma_rpath;
    struct tbs_oid_s_32  mma_vob_oid;
    ks_uint32_t mma_timeo;
    ks_uint32_t mma_retries;
    ks_uint32_t mma_ac_regmax;
    ks_uint32_t mma_ac_regmin;
    ks_uint32_t mma_ac_dirmax;
    ks_uint32_t mma_ac_dirmin;
    struct mfs_strbuf_32 mma_hmsuffix;
    struct tbs_uuid_s_32 mma_replica_uuid;
    ks_int32_t mma_sptab_cnt;
    ptr32_t mma_sptab_ents;
    ks_uint32_t mma_vobminor;
    struct mvfs_cache_sizes mma_sizes;

};

struct mvfs_ioinval_32 {
    ks_int32_t utype;
    union {
        struct tbs_oid_s_32 vob_oid;
        struct tbs_uuid_s_32 replica_uuid;
    } un;
    ks_uint32_t invaltype;
    struct tbs_oid_s_32 obj_oid;
    struct mfs_strbufpn_32 nm;
};

typedef ks_uint32_t mvfs_cmd_pn_flags_t_32;

struct mvfscmd_header_32 {
    ks_uint32_t cmd;
    mvfs_cmd_pn_flags_t_32 pnflags;
    struct mfs_strbufpn_pair_32 objnmpair;
};

struct mvfscmd_block_32 {
    struct mvfscmd_header_32 hdr;
    ks_uint32_t infolen;
    ptr32_t infop;
    tbs_status_t status;
};

struct mvfs_loginfo_32 {
    ks_uint32_t mask;
    ks_uint32_t priority;
    ks_uint32_t io_ops_mask;
    ks_uint32_t vops_mask;
    ks_uint32_t vfsops_mask;
    ks_uint32_t xops_mask;
    ks_uint32_t traps_mask;
    mvfs_boolean_t_32 assert_panic_on;
    struct mfs_strbufpn_32 kernlog_pn;
};

struct mvfs_viewtag_info_32 {
    struct  mfs_strbuf_32 viewtag;
};

struct mvfs_mkviewtag_info_32 {
    struct mfs_strbuf_32 viewtag;
    struct mfs_strbufpn_pair_32 spath;
    struct mfs_strbuf_32 host;
    struct mfs_strbufpn_32 rpath;
    struct tbs_uuid_s_32 uuid;
    ks_sockaddr_storage_t addr; /* Same size in 32-bit and 64-bit kernels. */
#if defined(ATRIA_LLP64)
    ks_uint32_t windows_view : 1;
    ks_uint32_t pad : 31;
#else
    ks_uint32_t winflag_and_pad;
#endif
};

struct mvfs_viewinfo_32 {
    struct mfs_strbuf_32 vname;
    struct mfs_strbufpn_32 spath;
    struct mfs_strbuf_32 host;
    struct mfs_strbufpn_32 rpath;
    struct tbs_uuid_s_32 uuid;
};

struct mvfs_vobinfo_32 {
    ks_int32_t utype;
    union {
        struct {
            struct mfs_strbufpn_pair_32 pair;
            mvfs_cmd_pn_flags_t_32 pnflags;
        } pn_s;
        ptr32_t vob_uuid;
        ptr32_t vob_oid;
        ks_uint32_t cookie;
    } vobid;
    struct mfs_strbufpn_32 mntpath;
    struct mfs_strbufpn_32 spath;
    struct mfs_strbuf_32 host;
    struct mfs_strbufpn_32 rpath;
    struct tbs_oid_s_32 oid;
    struct tbs_uuid_s_32 uuid;
    mvfs_boolean_t_32 unique;
};

struct mvfs_ioget_poolmaps_32 {
    struct tbs_uuid_s_32 replica_uuid;
    ks_int32_t mapcount;
    ptr32_t patterns;
    ptr32_t replacements;
};

struct view_fhandle_32 {
    struct tbs_uuid_s_32 vob_uuid;
    mvfs_dbid_t_32 ver_dbid;
    mvfs_dbid_t_32 elem_dbid;
    ks_uint32_t gen;
    ks_uint32_t flags;
    ks_uint32_t pad0;
};

struct mvfs_iovfh_32 {
    struct view_fhandle_32 vfh;
};
struct mvfs_splitpool_32 {
    struct mfs_strbuf_32 msp_prefix;
    struct mfs_strbufpn_pair_32 msp_target;
};
struct mvfs_aix_device_info_32 {
    ks_int32_t mvfs_specdev_major; /* Major number for specdev */
};

typedef int vob_mtype_t_32;

struct tbs_fstat_db_s_32 {
    ks_uint32_t type;
    ks_uint32_t mode;
    ks_uint32_t nlink;
    ks_uint32_t flags;
    credutl_sid_t   usid;
    credutl_sid_t   gsid;
    ks_off64_t size;
    ks_uint32_t nodeid;
    struct timeval_32 xtime;
    struct timeval_32 atime;
    struct timeval_32 mtime;
    struct timeval_32 ctime;
}
#if defined(ATRIA_LINUX) && defined(__x86_64__)
/*
 * AMD64 is unusual in that ks_off64_t, a 64-bit "long" when compiled
 * for the 64-bit kernel, has a minimum alignment of 8 bytes, but the
 * counterpart for 32-bit user-space, a 64-bit "long long" (since we
 * have a common user-space for x86 and x86_64), has a 4-byte
 * alignment.  Since we need our structures to have the same size and
 * layout in the kernel and user-space, we have to pack the AMD64
 * version to a 4-byte alignment.  (usid/gsid are character-based
 * types and align to 2 byte boundaries, leaving size aligned to a
 * 4-byte boundary in 32-bit user-space.)
 */
__attribute__((packed))
#endif
;

struct view_vstat_32 {
    struct tbs_fstat_db_s_32 fstat;
    vob_mtype_t_32 mtype;
    struct tbs_oid_s_32 elem_oid;
    struct tbs_oid_s_32 obj_oid;
    struct timeval_32 event_time;
};

struct mvfs_xstat_32 {
    ptr32_t vstat;
    struct tbs_oid_s_32  vob_oid;
    struct tbs_uuid_s_32  view_uuid;
    mvfs_boolean_t_32 view_hm;
    ks_uint32_t xmode;
    ks_uint32_t flags;
    struct tbs_uuid_s_32  replica_uuid;
    ks_uint32_t spare[6];
};

struct mvfs_clrname_info_32 {
    ks_uint16_t clrpool;
    struct mfs_strbufpn_32 clrname;
};

struct mvfs_iochange_mtype_32 {
    ks_int32_t mtype;
};

struct mvfs_bhinfo_32 {
    struct view_bhandle_32 bh;
    struct timeval_32 bh_ref_time;
    ks_uint32_t flags;
};

struct mvfs_statbufs_32 {
    struct mfs_strbuf_32 clntstat;
    struct mfs_strbuf_32 mnstat;
    struct mfs_strbuf_32 clearstat;
    struct mfs_strbuf_32 rvcstat;
    struct mfs_strbuf_32 dncstat;
    struct mfs_strbuf_32 acstat;
    struct mfs_strbuf_32 rlstat;
    struct mfs_strbuf_32 austat;
    struct mfs_strbuf_32 vnopcnt;
    struct mfs_strbuf_32 vfsopcnt;
    struct mfs_strbuf_32 viewopcnt;
    struct mfs_strbuf_32 viewoptime;
    struct mfs_strbuf_32 viewophist;
};

struct mvfs_io_xattr_32 {
    ks_int32_t xattr_type;
    ks_uint32_t xvalue;
};

struct mvfs_unmount_info_32 {
    struct mfs_strbufpn_32 tag;
    struct tbs_uuid_s_32 replica_uuid;
};

struct mvfs_export_viewinfo_32 {
    struct mfs_strbuf_32 viewtag;
    ks_int32_t exportid;
};

typedef struct mfs_audit_object_sn_32 {
    ks_uint32_t sn_high;
    ks_uint32_t sn_low;
} mfs_audit_object_sn_t_32;

struct mfs_auditrw_32 {
    ks_uint16_t objtype;                /* Object type */
    struct timeval_32 objdtm;           /* Object DTM */
    struct tbs_uuid_s_32 viewuuid;      /* View UUID */
    struct tbs_oid_s_32 voboid;         /* VOB OID */
    struct tbs_oid_s_32 objoid;         /* Object OID */
    mfs_audit_object_sn_t_32 objsn;     /* Object serial number */
    struct tbs_oid_s_32 elemoid;        /* Element OID */
    ks_uint32_t mtype;                  /* Object meta-type */
};

struct mfs_auditdir_32 {
    ks_uint16_t namlen;                 /* Length of name  */
    ks_uint16_t objtype;                /* Object type */
    struct timeval_32 objdtm;           /* Object DTM */
    struct tbs_uuid_s_32 viewuuid;      /* View UUID */
    struct tbs_oid_s_32 voboid;         /* VOB OID */
    struct tbs_oid_s_32 objoid;         /* Object OID */ 
    mfs_audit_object_sn_t_32 objsn;     /* Object serial number */
    struct tbs_oid_s_32 elemoid;        /* Element oid or viepvt OID */
    ks_uint32_t mtype;                  /* Object meta-type */
    struct tbs_oid_s_32 diroid;         /* Dir OID (may be null) */
    char name[AUDIT_MAXPATH];           /* Name */
};

struct mfs_auditrnm_32 {
    ks_uint16_t o_namlen;               /* From dir name len */
    ks_uint16_t t_namlen;               /* To dir name len */
    struct tbs_uuid_s_32 viewuuid;      /* View UUID */
    struct tbs_oid_s_32 o_diroid;       /* From dir oid */
    struct tbs_oid_s_32 t_diroid;       /* To dir oid */
    char name[AUDIT_MAXPATH];           /* From name\0To name */
};

struct mfs_auditchoid_32 {
    struct tbs_uuid_s_32 viewuuid;      /* View UUID */
    struct tbs_oid_s_32 prevoid;        /* Previous version oid */
    struct tbs_oid_s_32 objoid;         /* Current verion oid */
    mfs_audit_object_sn_t_32 objsn;     /* Object serial number */
    ks_uint32_t mtype;                  /* Object meta-type */
    ks_uint16_t objtype;                /* Object type */
};

struct mfs_auditview_32 {
    ks_uint16_t namlen;                 /* Name length */
    struct tbs_uuid_s_32 viewuuid;      /* View uuid */
    char name[AUDIT_MAXPATH];           /* View tag name */
};

struct mfs_auditmarker_32 {
    ks_uint32_t markerval;              /* value bits */
};
        
struct mfs_auditrec_32 {
    ks_uint16_t version;
    ks_uint16_t prevoff;
    ks_uint16_t nextoff;
    ks_uint16_t kind;
    union mfs_ar_u_32 {
        struct mfs_auditrw_32 rw;
        struct mfs_auditdir_32 dir;
        struct mfs_auditrnm_32 rnm;
        struct mfs_auditchoid_32 choid;
        struct mfs_auditview_32 view;
        struct mfs_auditmarker_32 marker;
    } mfs_ar_un_32;
};

#define mfs_rwrec_32   mfs_ar_un_32.rw
#define mfs_dirrec_32  mfs_ar_un_32.dir
#define mfs_rnmrec_32  mfs_ar_un_32.rnm
#define mfs_choidrec_32    mfs_ar_un_32.choid
#define mfs_viewrec_32 mfs_ar_un_32.view
#define mfs_markerrec_32   mfs_ar_un_32.marker

#define MFS_NEXTREC_32(ap) ((struct mfs_auditrec_32 *)((u_long)ap+ap->nextoff))

struct mvfs_viewstats_32 {
    struct mfs_strbuf_32 viewtag;
    struct mvfs_statbufs_32 stats;
};

struct mvfs_zero_viewstat_32 {
    struct mfs_strbuf_32 viewtag;
};

extern void mvfs_statbufs_32_to_mvfs_statbufs(struct mvfs_statbufs_32 *, struct mvfs_statbufs *);
extern void mvfs_io_xattr_to_mvfs_io_xattr_32(struct mvfs_io_xattr *, struct mvfs_io_xattr_32 *);
extern void mvfs_io_xattr_32_to_mvfs_io_xattr(struct mvfs_io_xattr_32 *, struct mvfs_io_xattr *);
extern void mvfs_export_viewinfo_32_to_mvfs_export_viewinfo(struct mvfs_export_viewinfo_32 *, struct mvfs_export_viewinfo *);
extern void mvfs_export_viewinfo_to_mvfs_export_viewinfo_32(struct mvfs_export_viewinfo *, struct mvfs_export_viewinfo_32 *);
extern void mvfs_clrname_info_to_mvfs_clrname_info_32(struct mvfs_clrname_info *, struct mvfs_clrname_info_32 *);
extern void mvfs_clrname_info_32_to_mvfs_clrname_info(struct mvfs_clrname_info_32 *, struct mvfs_clrname_info *);
extern void mvfs_iochange_mtype_32_to_mvfs_iochange_mtype(struct mvfs_iochange_mtype_32 *, struct mvfs_iochange_mtype *);
extern void mvfs_bhinfo_to_mvfs_bhinfo_32(struct mvfs_bhinfo *, struct mvfs_bhinfo_32 *);
extern void mvfs_bhinfo_32_to_mvfs_bhinfo(struct mvfs_bhinfo_32 *, struct mvfs_bhinfo *);
extern void view_vstat_to_view_vstat_32(struct view_vstat *, struct view_vstat_32 *);
extern void mvfs_xstat_to_mvfs_xstat_32(struct mvfs_xstat *, struct mvfs_xstat_32 *);
extern void mvfs_xstat_32_to_mvfs_xstat(struct mvfs_xstat_32 *, struct mvfs_xstat *);
#define MVFS_IOCTL_CMD_32       MIOWR('M', 101, struct mvfscmd_block_32)

extern void mvfs_gfsinfo_to_mvfs_gfsinfo_32(struct mvfs_gfsinfo *, struct mvfs_gfsinfo_32 *);
extern void mvfs_gfsinfo_32_to_mvfs_gfsinfo(struct mvfs_gfsinfo_32 *, struct mvfs_gfsinfo *);
extern void mfs_strbufpn_pair_to_mfs_strbufpn_pair_32(struct mfs_strbufpn_pair *, struct mfs_strbufpn_pair_32 *);
extern void mfs_strbufpn_pair_32_to_mfs_strbufpn_pair(struct mfs_strbufpn_pair_32 *, struct mfs_strbufpn_pair *);
extern void mfs_strbuf_to_mfs_strbuf_32(struct mfs_strbuf *, struct mfs_strbuf_32 *);
extern void mfs_strbuf_32_to_mfs_strbuf(struct mfs_strbuf_32 *, struct mfs_strbuf *);
extern void mfs_mntargs_32_to_mfs_mntargs(struct mfs_mntargs_32 *, struct mfs_mntargs *);
extern void tbs_oid_s_to_tbs_oid_s_32(struct tbs_oid_s *, struct tbs_oid_s_32 *);
extern void tbs_oid_s_32_to_tbs_oid_s(struct tbs_oid_s_32 *, struct tbs_oid_s *);
extern void tbs_uuid_s_to_tbs_uuid_s_32(struct tbs_uuid_s *, struct tbs_uuid_s_32 *);
extern void tbs_uuid_s_32_to_tbs_uuid_s(struct tbs_uuid_s_32 *, struct tbs_uuid_s *);
extern void mvfscmd_header_to_mvfscmd_header_32(struct mvfscmd_header *, struct mvfscmd_header_32 *);
extern void mvfscmd_header_32_to_mvfscmd_header(struct mvfscmd_header_32 *, struct mvfscmd_header *);
extern void mvfscmd_block_to_mvfscmd_block_32(struct mvfscmd_block *, struct mvfscmd_block_32 *);
extern void mvfscmd_block_32_to_mvfscmd_block(struct mvfscmd_block_32 *, struct mvfscmd_block *);
extern void mvfs_loginfo_to_mvfs_loginfo_32(struct mvfs_loginfo *, struct mvfs_loginfo_32 *);
extern void mvfs_loginfo_32_to_mvfs_loginfo(struct mvfs_loginfo_32 *, struct mvfs_loginfo *);
extern void mvfs_viewtag_info_32_to_mvfs_viewtag_info(struct mvfs_viewtag_info_32 *, struct mvfs_viewtag_info *);
extern void mvfs_mkviewtag_info_32_to_mvfs_mkviewtag_info(struct mvfs_mkviewtag_info_32 *, struct mvfs_mkviewtag_info *);
extern void mvfs_viewinfo_to_mvfs_viewinfo_32(struct mvfs_viewinfo *, struct mvfs_viewinfo_32 *);
extern void mvfs_viewinfo_32_to_mvfs_viewinfo(struct mvfs_viewinfo_32 *, struct mvfs_viewinfo *);
extern void mvfs_ioget_poolmaps_to_mvfs_ioget_poolmaps_32(struct mvfs_ioget_poolmaps *, struct mvfs_ioget_poolmaps_32 *);
extern void mvfs_ioget_poolmaps_32_to_mvfs_ioget_poolmaps(struct mvfs_ioget_poolmaps_32 *, struct mvfs_ioget_poolmaps *);
extern void view_fhandle_to_view_fhandle_32(struct view_fhandle *, struct view_fhandle_32 *);
extern void view_fhandle_32_to_view_fhandle(struct view_fhandle_32 *, struct view_fhandle *);
extern void mvfs_iovfh_to_mvfs_iovfh_32(struct mvfs_iovfh *, struct mvfs_iovfh_32 *);
extern void mvfs_iovfh_32_to_mvfs_iovfh(struct mvfs_iovfh_32 *, struct mvfs_iovfh *);

extern void mvfs_splitpool_32_to_mvfs_splitpool(struct mvfs_splitpool_32 *vbl_32, struct mvfs_splitpool *vbl);

extern void tbs_fstat_db_s_to_tbs_fstat_db_s_32(struct tbs_fstat_db_s *vbl, struct tbs_fstat_db_s_32 *vbl_32);

extern void view_vstat_to_view_vstat_32(struct view_vstat *vbl, struct view_vstat_32 *vbl_32); 
extern void view_vstat_32_to_view_vstat(struct view_vstat_32 *vbl_32, struct view_vstat *vbl);
extern void mvfs_xstat_to_mvfs_xstat_32(struct mvfs_xstat *vbl, struct mvfs_xstat_32 *vbl_32);
extern void mvfs_xstat_32_to_mvfs_xstat(struct mvfs_xstat_32 *vbl_32, struct mvfs_xstat *vbl);

extern void mvfs_ioinval_32_to_mvfs_ioinval( struct mvfs_ioinval_32 *vbl_32, struct mvfs_ioinval *vbl);
extern int mvfs_ioctl_validate_64bitos(mvfscmd_block_t *, MVFS_CALLER_INFO *callinfo );
extern tbs_boolean_t mvfs_ioctl_chk_cmd(int cmd, MVFS_CALLER_INFO *callinfo);

extern void mfs_auditrec_to_mfs_auditrec_32(struct mfs_auditrec *, struct mfs_auditrec_32 *);
extern void mfs_auditbuf_to_mfs_auditbuf_32(struct mfs_auditrec *, struct mfs_auditrec_32 *, struct mfs_auditrec *);
extern void mvfs_viewstats_32_to_mvfs_viewstats(struct mvfs_viewstats_32 *, struct mvfs_viewstats *);
extern void mvfs_zero_viewstat_32_to_mvfs_zero_viewstat(struct mvfs_zero_viewstat_32 *, struct mvfs_zero_viewstat *);

extern void mvfs_vobinfo_32_to_mvfs_vobinfo(struct mvfs_vobinfo_32 *vbl_32, struct mvfs_vobinfo *vbl);
extern void mvfs_vobinfo_to_mvfs_vobinfo_32(struct mvfs_vobinfo *vbl, struct mvfs_vobinfo_32 *vbl_32);

#endif /* ATRIA_LP64 || ATRIA_LLP64 */

#endif /* MVFS_TRANSTYPE_H_ */
/* $Id: 2f93a827.a23a11df.8bc7.00:01:84:7a:f2:e4 $ */
