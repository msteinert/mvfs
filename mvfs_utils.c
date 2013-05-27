/* * (C) Copyright IBM Corporation 1990, 2010. */
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
/* mvfs_utils.c */
#include "mvfs_systm.h"
#include "mvfs.h"
#include "mvfs_dnc.h"
#include <credutl_kernel.h>

/*
 * TBS constants here for lack of a better place ...
 */

A_CONST tbs_oid_t TBS_OID_NULL = { {0, 0, 0, 0, 0, {0}} };
A_CONST tbs_uuid_t TBS_UUID_NULL = { 0, 0, 0, 0, 0, {0} };

view_bhandle_t mfs_null_bh;

/*
 * This module is a grabbag of misc routines.
 * In general the routines are in the following groups (in order):
 *    1) copyin/copyout utility routines
 *    2) string/pathname manipulation utilities
 *    3) vstat/fstat conversion routines
 *    4) UIO utility routines
 *    5) other misc routines
 */

/******************* COPYIN / COPYOUT ROUTINES *************************/

/* 
 * MFS_COPYIN_STRBUF - copyin an mfs "strbuf".  
 * This routine allocates memory which can be
 * freed with STRFREE().  It also may sleep waiting
 * for memory.
 */

int
mfs_copyin_strbuf(strbuf, strp)
struct mfs_strbuf strbuf;
char **strp;
{
    char *str;
    int error = 0;

    if (strbuf.l < 0 || strbuf.l > MAXPATHLEN) {
	*strp = NULL;
	return(EINVAL);
    }

    if (strbuf.s == NULL || strbuf.l == 0) {	/* No string to copy */
	*strp = NULL;
	return(EINVAL);		/* Null strings not allowed */
    }

    str = KMEM_ALLOC(strbuf.l+1, KM_SLEEP);
    if (str == NULL) return(ENOMEM);

    if ((error = COPYIN((caddr_t)strbuf.s, (caddr_t)str, strbuf.l)) != 0) 
    {
	KMEM_FREE(str, strbuf.l+1);
	*strp = NULL;
	return(error);
    }

    str[strbuf.l] = '\0';	/* Force null at end */
    *strp = str;		/* Return allocated string */
    return(0);
}

/* 
 * MFS_COPYIN_STRBUFPN - copyin an mfs "strbufpn".  
 * This routine allocates memory which can be
 * freed with STRFREE().  It also may sleep waiting
 * for memory.
 */

int
mfs_copyin_strbufpn(strbufpn, strp)
struct mfs_strbufpn strbufpn;
char **strp;
{
    char *str;
    int error;

    /* strbufpn.s and strp could be the same string, i.e. our caller wants us
    ** to overwrite a user-space pointer with a kernel pointer.  Therefore,
    ** only set *strp (to NULL or str) just before we return.
    */
    if (strbufpn.l < 0 || strbufpn.l > MAXPATHLEN) {
	*strp = NULL;
	return(EINVAL);
    }

    if (strbufpn.s == NULL || strbufpn.l == 0) {	/* No string to copy */
	*strp = NULL;
	return(EINVAL);		/* Null strings not allowed */
    }

    if ((str = KMEM_ALLOC(strbufpn.l+1, KM_SLEEP)) == NULL) {
        *strp = NULL;
        return(ENOMEM);
    }

    if ((error = COPYIN((caddr_t)strbufpn.s, (caddr_t)str, strbufpn.l)) != 0) {
	KMEM_FREE(str, strbufpn.l+1);
	*strp = NULL;
	return(error);
    }

    str[strbufpn.l] = '\0';	/* Force null at end */
    *strp = str;		/* Return allocated string */
    return(0);
}

/*
 * MFS_COPYOUT_STRBUF - routine to copyout a string to
 * a strbuf.
 */

int
mfs_copyout_strbuf(strbuf, s)
struct mfs_strbuf strbuf;
char *s;
{
    size_t len;
    int error = 0;

    /* The user can ignore return strings by setting the ptr
       to NULL or the length to 0 */

    if (strbuf.s == NULL || strbuf.m == 0) return(0);

    /* 
     * Really copy the string out.  If the string to
     * copy out is a NULL ptr, then simply copy out 
     * a null string.
     */

    if (s != NULL) {
        len = STRLEN(s);
        if (len+1 > strbuf.m) {	/* Won't fit, return error */
	    return(EINVAL);
        }
        error = COPYOUT((caddr_t)s, (caddr_t)strbuf.s, len+1);
    } else {
	len = 0;	/* Hold a 0 */
	error = COPYOUT((caddr_t)&len, (caddr_t)strbuf.s, 1);
    }
    if (!error)
	strbuf.l = len;
    return(error);
}

int
mfs_copyout_strbufpn(strbufpn, s)
struct mfs_strbufpn strbufpn;
char *s;
{
    size_t len;
    int error = 0;

    /* The user can ignore return strings by setting the ptr
       to NULL or the length to 0 */

    if (strbufpn.s == NULL || strbufpn.m == 0) return(0);

    /* 
     * Really copy the string out.  If the string to
     * copy out is a NULL ptr, then simply copy out 
     * a null string.
     */

    if (s != NULL) {
        len = STRLEN(s);
        if (len+1 > strbufpn.m) {	/* Won't fit, return error */
	    return(EINVAL);
        }

        if ((error = COPYOUT((caddr_t)s, (caddr_t)strbufpn.s, len+1)) != 0) 
        {
	    return(error);
        }
    } else {
	len = 0;	/* Hold a 0 */
	if ((error = COPYOUT((caddr_t)&len, (caddr_t)strbufpn.s, 1)) != 0) 
        {
	    return(error);
	}
    }
    strbufpn.l = len;

    return(0);
}

/* MFS_COPYOUT_VIEWTAG - routine to copyout a view tag name for ioctls */

int
mfs_copyout_viewtag(err_if_stale, str, vw)
int err_if_stale;
mfs_strbufpn_t str;
VNODE_T *vw;
{
    mfs_mnode_t *mnp;
    int id;
    int error;
    VNODE_T *vwroot;

    if (vw == NULL) {
	error = mfs_copyout_strbufpn(str, NULL);
	return(error);
    }

    mnp = VTOM(vw);
    ASSERT(MFS_ISVIEW(mnp));

    vwroot = mfs_getviewroot();
    if (vwroot == NULL)
        return(ESTALE);

    /*
     * Lock the ramdir and the view mnode to keep the entries stable
     * while we copy out the name.
     */
    MLOCK(VTOM(vwroot));
    MLOCK(mnp);

    id = mnp->mn_view.id;
    if (id == MFS_NULLVID) {
	if (err_if_stale) {
	    error = ESTALE;
	} else {
	    error = mfs_copyout_strbufpn(str, NULL);
	}
    } else {
        error = mfs_copyout_strbufpn(str,
                                     VTOM(vwroot)->mn_ramdir.ents[id].nm);
    }
    MUNLOCK(mnp);
    MUNLOCK(VTOM(vwroot));
    VN_RELE(vwroot);
    return(error);
}

/******************* STRING / PATHNAME ROUTINES *************************/

/* MFS_STRERR - get error string for common errors */

char *
mfs_strerr(error)
int error;
{
    mvfs_thread_t *mth;

    switch (error) {
	case EPERM:	return(" - No permission match");
	case ENOENT:	return(" - No such file or directory");
	case EINTR:	return(" - Interrupted system call");
	case EIO:	return(" - I/O error");
	case ENOMEM:	return(" - Not enough kernel memory");
	case EACCES:	return(" - Permission denied");
	case EEXIST:	return(" - File exists");
	case ENOTDIR:	return(" - Not a directory");
	case EISDIR:	return(" - Is a directory");
	case EINVAL:	return(" - Invalid argument");
	case ENOSPC:	return(" - No space left on device");
	case EROFS:	return(" - Read-only file system");
#if ENOTEMPTY != EEXIST
        case ENOTEMPTY: return(" - Directory not empty");
#endif
	case ETIMEDOUT: return(" - Connection timed out");
	case EAGAIN: return(" - Potentially transient error, retry required");
	case EOPNOTSUPP: return(" - Operation not supported");
	case ENOSYS: return(" - System operation not available");
	case ELOOP:	return(" - Too many levels of symbolic links");
	case ENAMETOOLONG: return(" - File name too long");
	case ESTALE:	return(" - Stale NFS file handle");
	case EBADF:	return(" - Bad file descriptor");
	/*
	 * ClearCase errors that can't be converted to Unix errors
	 */
	case TBS_ST_ERR: return(" - Unspecified ClearCase error");
	case TBS_ST_NOT_FOUND: return(" - ClearCase object has no data");
	case TBS_ST_NOT_AN_OBJ: return(" - Not a ClearCase object");
	case TBS_ST_TIMEOUT: return(" - Communications timeout in ClearCase server");
	case TBS_ST_DBID_NOT_FOUND: return(" - ClearCase dbid not found");
	case TBS_ST_NOT_LICENSED: return(" - No license available from ClearCase license manager; use clearlicense to display license usage");
	case TBS_ST_XREV_COMPAT: return(" - ClearCase cross-rev compatibility problem");
	case TBS_ST_VIEW_ERR:	return(" - ClearCase view error");
	case TBS_ST_VOB_ERR:	return(" - ClearCase vob error");
	case TBS_ST_DB_ERR:	return(" - ClearCase database error");
	case TBS_ST_MFS_ERR:	return(" - ClearCase MVFS error");
	case TBS_ST_VIEW_NO_CFS_SET: return(" - No configuration spec set");
	case TBS_ST_VIEW_STALE_DIR: return(" - stale ClearCase directory");
	case TBS_ST_VIEW_NO_VER: return(" - No version selected by configuration spec");
	case TBS_ST_VIEW_CLTXT_ERR: return(" - Problem with cleartext fetch");
	case TBS_ST_VIEW_UNKNOWN_VOB: return(" - VOB not registered with view"); 
	case TBS_ST_WRONG_VOB: return(" - VOB mount is stale");
	case TBS_ST_WRONG_VIEW: return(" - View tag is stale");
	case TBS_ST_VOB_NEEDS_RECOVERY: return(" - VOB needs recovery");
	case TBS_ST_DB_AREA_LOCKED: return(" - Lock on VOB database prevents write transactions");
	case TBS_ST_LICENSE_BUSY: return(" - All licenses in use; use clearlicense to display license usage");
	case TBS_ST_VIEW_NEEDS_RECOVERY: return(" - View needs recovery");
	case TBS_ST_VIEW_NEEDS_REFORMAT: return(" - View needs reformat");
	case TBS_ST_VOB_NEEDS_REFORMAT:	 return(" - VOB needs reformat");
	case TBS_ST_CONFIG_SPEC_ERR: return(" - Trouble with config spec");
	case TBS_ST_RGY_DTM_MISMATCH: return(" - Registry changed between operations");
	case TBS_ST_REPLAY_ALREADY_DONE: return(" - Operation has already been replayed");
	case TBS_ST_REPLAY_MISSING_INPUT: return(" - Replay is missing required input");
	case TBS_ST_NOT_A_REGISTRY_SVR: return(" - Specified host is not a registry server");
	case TBS_ST_NOT_A_LICENSE_SVR: return(" - Specified host is not a license server");
	case TBS_ST_DB_TIMEOUT:	return(" - Database timed out");
	case TBS_ST_VIEW_STG_UNAVAIL: return(" - View storage directory or control files unavailable");
	case TBS_ST_VIEW_CONTACT_ERR: return(" - Unable to contact the view");
        case TBS_ST_HAS_CHKOUTS: return(" - checkouts prevent the operation");
        case TBS_ST_DENIED: return(" - the requested operation is denied");
        case TBS_ST_OBJ_LOCKED: return(" - object locks prevent the operation completing");
        case TBS_ST_NOT_MASTER: return(" - not the master of the object");

	/* Other errors */
	default:
	    mth = mvfs_mythread();
	    MVFS_SNPRINTF(mth->thr_errstr, sizeof(mth->thr_errstr),
			  " - error %d", error);
	    return(mth->thr_errstr);
    }
}

/* MFS_VP2VW - get viewtag name for a vnode for printing */

static char *mfs_stringnull = "(null)";

char *
mfs_vw2nm(vw)
VNODE_T *vw;
{
    char *s;
    if (vw == NULL) return(mfs_stringnull);
    if (!MFS_ISVIEW(VTOM(vw))) return(mfs_stringnull);
    return(VTOM(vw)->mn_view.viewname);
}

char *
mfs_vp2vw(vp)
VNODE_T *vp;
{
    VNODE_T *vw;
    char *s;

    if (vp == NULL) return(mfs_stringnull);
    if (!MFS_VPISMFS(vp)) return(mfs_stringnull);
    vw = MFS_VIEW(vp);
    return(mfs_vw2nm(vw));
}

/* MFS_VP2DEV - get mnt pathname for a vnode for printing */

char *
mfs_vp2dev(vp)
VNODE_T *vp;
{
    char *s;

    if (vp == NULL) return(mfs_stringnull);
    if (!MFS_VPISMFS(vp)) return(mfs_stringnull);

    if ((s = V_TO_MMI(vp)->mmi_mntpath) == NULL) {
	s = mfs_stringnull;
    }
    return(s);
}

u_long
mfs_vp2dbid(vp)
VNODE_T *vp;
{
    if (!MFS_VPISMFS(vp)) return (0);
    /* Really only useful for VOB objects */
    if (MFS_ISVOB(VTOM(vp)) || MFS_ISVOBRT(VTOM(vp))) {
        return ((u_long)VTOM(vp)->mn_hdr.fid.mf_dbid);
    } else {
	return ((u_long)VTOM(vp)->mn_hdr.mnum);
    }
}

/* MFS_STRDUP - dup a string (allocating storage) */

char *
mfs_strdup(str)
char *str;
{
    char *rp;
    rp = KMEM_ALLOC(STRLEN(str)+1, KM_SLEEP);
    if (rp != NULL) STRCPY(rp, str);
    return(rp);
}

/* MVFS_ANSI_STRCASECMP - compare two strings case-insensitive */

int
mvfs_ansi_strcasecmp(s1, s2)
char *s1;
char *s2;
{
    int c1,c2;
    tbs_boolean_t mb_char = FALSE;

    do {
        c1 = *s1++;
        c2 = *s2++;
        if ('A' <= c1 && c1 <= 'Z' && !mb_char) c1 += 'a'-'A';
        if ('A' <= c2 && c2 <= 'Z' && !mb_char) c2 += 'a'-'A';
        /* 
         * Poor man's handling of multibyte chars without locale info.
         * I don't downcase a char after a non-ANSI 0x8n char assuming
         * it is part of a 2-byte multibyte sequence.
         * This sets a flag for the next char conversion in
         * the loop.  Note that we only need one flag, because
         * if c2 doesn't match c1 we won't execute the next
         * iteration of the loop.
         */
        mb_char = ((c1 & 0x80) != 0);
    } while (c1 && c1 == c2);

    return(c1-c2);
}

/* MVFS_ANSI_STRNCASECMP - compare two strings case-insensitive n-bytes maximum */

int
mvfs_ansi_strncasecmp(s1, s2, n)
char *s1;
char *s2;
size_t n;
{
    int c1,c2;
    tbs_boolean_t mb_char = FALSE;

    if (n != 0) {
	do {
	    c1 = *s1++;
	    c2 = *s2++;
	    if ('A' <= c1 && c1 <= 'Z' && !mb_char) c1 += 'a'-'A';
	    if ('A' <= c2 && c2 <= 'Z' && !mb_char) c2 += 'a'-'A';
	    /* 
	     * Poor man's handling of multibyte chars without locale info.
	     * I don't downcase a char after a non-ANSI 0x8n char assuming
	     * it is part of a 2-byte multibyte sequence.
	     * This sets a flag for the next char conversion in
	     * the loop.  Note that we only need one flag, because
	     * if c2 doesn't match c1 we won't execute the next
	     * iteration of the loop.
	     */
	    mb_char = ((c1 & 0x80) != 0);
	    if (c1 != c2)
		return(c1 - c2);
	    if (c1 == 0)
		break;
	} while (--n != 0);
    }
    return(0);
}

/* MVFS_ANSI_STR_ISLOWER - routine to see if string already downcased */

tbs_boolean_t
mvfs_ansi_str_islower(s)
char *s;
{
    char *cp;

    for (cp = s; *cp; cp++) {
        /* 
         * Poor man's handling of multi-byte characters...
         * just ignore.
         */
        if ((*cp & 0x80) != 0) {
            cp++;                   /* Skip this char and next */
            if (*cp) continue;      /* If next isn't end of string */
        } 
        if ('A' <= *cp && *cp <= 'Z') return(FALSE);
    }

    return(TRUE);
}

/* MVFS_ANSI_TRAILING_SEPS - routine to count the number of trailing */
/*                           separators in a pathname */

int
mvfs_ansi_trailing_seps(pn, pn_len)
char *pn;
size_t pn_len;
{
    char *p = pn + pn_len - 1;
    int num_seps = 0;

    while ((pn_len != 0) && PN_IS_SEPCHAR(*p)) {
        num_seps++;
	pn_len --;
	p--;
    }

    return num_seps;
}

/* MFS_UNIQ_NAME - generate a unique name */

u_long mfs_uid = 0;
char *mfs_tohex = "0123456789abcdef";
SPLOCK_T mvfs_uniqlock;
LOCK_T mvfs_printf_lock;
LOCK_T mvfs_printstr_lock;

char *
mfs_uniq_name()
{
    char *newstr;
    char *s;
    u_long uid = 0;
    int i;
    SPL_T sp;

    s = newstr = (char *)KMEM_ALLOC(15, KM_NOSLEEP);
    if (s == NULL) return(s);	/* No memory */


    if (mfs_uid == 0) uid = MDKI_CTIME();
    SPLOCK(mvfs_uniqlock, sp);
    if (mfs_uid == 0) mfs_uid = uid;	/* Set to a uniq value on boot */
    else mfs_uid++;			/* Sequential UID's after that */
    uid = mfs_uid;
    SPUNLOCK(mvfs_uniqlock, sp);

    *s++ = '.';		/* Fill in ".mvfs_" prefix */
    *s++ = 'm';
    *s++ = 'v';
    *s++ = 'f';
    *s++ = 's';
    *s++ = '_';
    for (i=0; i < 8; i++, uid = uid << 4) {
	*s++ = mfs_tohex[(uid >> 28) & 0xf];
    }
    *s = '\0';

    return(newstr);
}

/*
 * MFS_HMSUFFIX_LEN - return history mode suffix length
 */

int
mfs_hmsuffix_len()
{
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if (vrdp->mfs_viewroot_vfsp == NULL) return(0);
    return((int)VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_hmsuffixlen);
}

/*
 * MFS_HMNAME - check for HM name.  If a stripped name ptr
 *	is provided, then return the stripped name if the original
 *	was a history mode name, otherwise return NULL.
 */

int
mfs_hmname(nm, stripped_nm_p)
char *nm;
char **stripped_nm_p;	/* May be NULL if stripped nm not desired */
{
    struct mfs_mntinfo *mmi;
    size_t len;
    size_t strplen;
    char *strpnm;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();
  
    if (!(vrdp->mfs_viewroot_vfsp)) goto nothm;
    mmi = VFS_TO_MMI(vrdp->mfs_viewroot_vfsp);
    if (mmi->mmi_hmsuffixlen == 0 || mmi->mmi_hmsuffix == NULL) goto nothm;

    len = STRLEN(nm);
    if (len < mmi->mmi_hmsuffixlen)
        goto nothm;
    strplen = len - mmi->mmi_hmsuffixlen;

    if (STRCMP(&nm[strplen], mmi->mmi_hmsuffix) == 0) {
	if (stripped_nm_p) {
	    /* Must turn "@@" into ".@@" */
	    if (strplen == 0) {
		strpnm = KMEM_ALLOC(2, KM_SLEEP);
		ASSERT(strpnm);
		BCOPY(".", strpnm, 2);
	    } else {
	        strpnm = KMEM_ALLOC(strplen+1, KM_SLEEP);
	        ASSERT(strpnm);
	        BCOPY(nm, strpnm, strplen);
	        strpnm[strplen] = '\0';
	    }
	    *stripped_nm_p = strpnm;
	}
	return(1);
    }

nothm:    
    if (stripped_nm_p) *stripped_nm_p = NULL;
    return(0);
}

/*
 * MFS_HMCMP - compare for history mode version of a non-
 * history mode string (e.g. used to look for "." or ".." with
 * trailing HM suffix)... returns 0 if a match, -1 otherwise.
 * For example:  Both "." and ".@@" would compare with "." etc.
 *
 * One special hack is to allow "@@" to match ".", because the
 * higher levels of SW often accumulate a naked @@ after a "/" on dirs.
 *
 * Another special hack is to allow the hmvers_nm (e.g. ^@@) to
 * match "." because if you are in HM already, it is a 
 * synonym for ".@@".
 */

int
mfs_hmcmp(hmnm, nm)
char *hmnm;
char *nm;
{
    char *hmsuffix = NULL;
    char *hmvers = NULL;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if (vrdp->mfs_viewroot_vfsp &&
        VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_hmsuffixlen) {
	hmsuffix = VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_hmsuffix;
	hmvers = VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_hmvers_nm;
    }
  
    /* Special check for just "@@" same as "." */ 
    /* 06/04/93 - DEJ - added check for "^@@" same as "." */
    if (nm[0] == '.' && nm[1] == '\0') {
	if (hmsuffix && (STRCMP(hmnm, hmsuffix) == 0)) {   /* Just hm suffix */
	    return(0);
	}
	if (hmvers && (STRCMP(hmnm, hmvers) == 0)) {	/* Just "^@@" */
	    return(0);
	}
    }

    /* Actually compare name and hm suffix */

    while (*nm) {
	if (*nm++ != *hmnm++) return(-1);	/* Not a match */
    }
    if (*hmnm == '\0') return(0);       /* Exact match */
    if (hmsuffix && (STRCMP(hmnm, hmsuffix) == 0)) {	/* HM form of nm */
	return(0);
    }
    return(-1);
}

/*
 * Simply cat the HM suffix onto the end of a buffer
 * Returns length added to buffer.
 */

int
mfs_hmstrcat(s)
char *s;
{
    struct mfs_mntinfo *mmi;
    size_t len;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if (!(vrdp->mfs_viewroot_vfsp)) return(0);
    mmi = VFS_TO_MMI(vrdp->mfs_viewroot_vfsp);
    if (mmi->mmi_hmsuffixlen == 0 || mmi->mmi_hmsuffix == NULL) return(0);
    len = STRLEN(s);
    BCOPY(mmi->mmi_hmsuffix, &s[len], 
			mmi->mmi_hmsuffixlen+1);  /* Incl '\0' */
    return((int)mmi->mmi_hmsuffixlen);
}
	    
/*
 * Return a string with the history mode suffix appended to
 * the end.  Returned  string is an allocated copy.
 */

char *
mfs_hmappend(nm)
char *nm;
{
    struct mfs_mntinfo *mmi;
    char *hmnm;
    size_t len;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();

    if (!(vrdp->mfs_viewroot_vfsp)) return(STRDUP(nm));
    mmi = VFS_TO_MMI(vrdp->mfs_viewroot_vfsp);
    if (mmi->mmi_hmsuffixlen == 0 || mmi->mmi_hmsuffix == NULL) {
	hmnm = STRDUP(nm);	/* No HM suffix, return orig nm duplicate */
	return(hmnm);
    }

    len = STRLEN(nm);
    hmnm = KMEM_ALLOC(len+mmi->mmi_hmsuffixlen+1, KM_SLEEP);
    if (hmnm != NULL) {
        BCOPY(nm, hmnm, len);
        BCOPY(mmi->mmi_hmsuffix, &hmnm[len], 
			mmi->mmi_hmsuffixlen+1);  /* Incl '\0' */
    }
    return(hmnm);
}
/******************* VSTAT / FSTAT ROUTINES *************************/

/* MFS_FTYPE_TO_VTYPE - translate an over-th-wire file type into a v_type */

VTYPE_T
mfs_ftype_to_vtype(ftype)
tbs_ftype_t ftype;
{
    switch (ftype) {
      case TBS_FTYPE_REG: return(VREG);
      case TBS_FTYPE_DIR: return(VDIR);
      case TBS_FTYPE_BLK: return(VBLK);
      case TBS_FTYPE_CHR: return(VCHR);
      case TBS_FTYPE_LNK: return(VLNK);
      case TBS_FTYPE_FIFO: return(VFIFO);
      default: return (VNON);
    }
}

/* 
 * MFS_MN_TO_VATTR - translate attributes in an mnp struct to vattr 
 * and merge in cleartext information if appropriate.
 */

int mfs_fsregblksize = MFS_BLOCKSIZE;
int mfs_fsdirblksize = MFS_BLOCKSIZE;

void
mfs_mn_to_vattr(mnp, vap)
struct mfs_mnode *mnp;
VATTR_T *vap;
{
    u_long dev;
    int error;

    ASSERT(MFS_ISVOB(mnp));

    VATTR_SET_TYPE(vap, mfs_ftype_to_vtype(mnp->mn_vob.attr.fstat.type));
    VATTR_SET_MODE_RIGHTS(vap, mnp->mn_vob.attr.fstat.mode);
    switch (VATTR_GET_TYPE(vap)) {
        case VREG: VATTR_SET_MODE_TYPE(vap, S_IFREG); break;
	case VDIR: VATTR_SET_MODE_TYPE(vap, S_IFDIR); break;
	case VBLK: VATTR_SET_MODE_TYPE(vap, S_IFBLK); break;
	case VCHR: VATTR_SET_MODE_TYPE(vap, S_IFCHR); break;
	case VLNK: VATTR_SET_MODE_TYPE(vap, S_IFLNK); break;
	case VFIFO: VATTR_SET_MODE_TYPE(vap, S_IFIFO); break;
	default:
	    mvfs_log(MFS_LOG_WARN, "unknown type %d, vw=%s vob=%s dbid=0x%x\n", 
		VATTR_GET_TYPE(vap),
		mfs_vw2nm(mnp->mn_hdr.viewvp),
		VFS_TO_MMI(mnp->mn_hdr.vfsp)->mmi_mntpath,
		mnp->mn_hdr.fid.mf_dbid);
	    VATTR_SET_MODE_TYPE(vap, S_IFREG); 
	    break;
    }
    MVFS_COPY_UID_TO_VATTR(vap, &mnp->mn_vob.user_id, mnp, &error);
    if (error != 0) {
        mvfs_log(MFS_LOG_ERR, "mfs_mn_to_vattr: Failed to copy uid, error %d\n", error);
    }
    MVFS_COPY_GID_TO_VATTR(vap, &mnp->mn_vob.group_id, mnp, &error);
    if (error != 0) {
        mvfs_log(MFS_LOG_ERR, "mfs_mn_to_vattr: Failed to copy gid, error %d\n", error);
    }
    dev = FSID_TO_DEV(VFS_FSID(mnp->mn_hdr.vfsp));
    VATTR_SET_FSID(vap, &dev);
    VATTR_SET_NODEID(vap, mnp->mn_vob.attr.fstat.nodeid); /* really inumber */
    VATTR_SET_NLINK(vap, mnp->mn_vob.attr.fstat.nlink);
    VATTR_SET_ATIME_TV(vap, &mnp->mn_vob.attr.fstat.atime);
    /*
     * Only override with attributes if no errors on the cleartext.
     * If there are cleartext errors, then ignore the cleartext attrs.
     */
    if (mnp->mn_hdr.realvp && !mnp->mn_vob.cleartext.rwerr) {
	VATTR_SET_SIZE(vap, VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va)); 
	/* 
	 * Use mtime from cleartext ONLY if the file is dirty!
         * (e.g. we wrote the file)
	 */
	if (mnp->mn_hdr.clear_dirty) {
	    VATTR_SET_MTIME_VATTR(vap, &mnp->mn_vob.cleartext.va);
        } else { 
	    VATTR_SET_MTIME_TV(vap, &mnp->mn_vob.attr.fstat.mtime);
	}
    } else {
  	VATTR_SET_SIZE(vap, mnp->mn_vob.attr.fstat.size); 
	VATTR_SET_ATIME_TV(vap, &mnp->mn_vob.attr.fstat.atime);
	VATTR_SET_MTIME_TV(vap, &mnp->mn_vob.attr.fstat.mtime);
    }
   
    VATTR_SET_CTIME_TV(vap, &mnp->mn_vob.attr.fstat.ctime);
    VATTR_SET_RDEV(vap,	0); /* fstat.rdev doesn't exist anymore */
    if (VATTR_GET_TYPE(vap) == VDIR) {
	VATTR_SET_BLKSIZE(vap, mfs_fsdirblksize);
    } else {
        VATTR_SET_BLKSIZE(vap, mfs_fsregblksize);
    }
    VATTR_SET_NBLOCKS(vap, VATTR_BTODB(VATTR_GET_SIZE(vap)));
}

/* MFS_SATTR_IS_NULL - check for a null sattr recored */

int
mfs_sattr_is_null(sattr)
view_set_attr_t *sattr;
{
    return (sattr->mask == 0);
}

/* MFS_SATTR_NULL - fill in sattr struct with null values */

void
mfs_sattr_null(sattr)
view_set_attr_t *sattr;
{
    sattr->mask = 0;
    sattr->type = TBS_FTYPE_NULL;
    sattr->mode = (u_short) -1;
    sattr->usid.length = 0;
    sattr->usid.type = CREDUTL_SID_TYPE_NOBODY;
    sattr->gsid.length = 0; 
    sattr->gsid.type = CREDUTL_SID_TYPE_NOBODY;
    sattr->size = 0;
    sattr->atime.tv_sec = sattr->atime.tv_usec = -1;
    sattr->mtime.tv_sec = sattr->mtime.tv_usec = -1;
}

/* MFS_VATTR_TO_SATTR - map vattr struct into view sattr */

void
mfs_vattr_to_sattr(vap, sattr)
VATTR_T *vap;
view_set_attr_t *sattr;
{
    u_long mask;
    mask = VATTR_GET_MASK(vap);
    switch (VATTR_GET_TYPE(vap)) {
      case VREG: sattr->type = TBS_FTYPE_REG; break;
      case VDIR: sattr->type = TBS_FTYPE_DIR; break;
      case VBLK: sattr->type = TBS_FTYPE_BLK; break;
      case VCHR: sattr->type = TBS_FTYPE_CHR; break;
      case VLNK: sattr->type = TBS_FTYPE_LNK; break;
      case VFIFO: sattr->type = TBS_FTYPE_FIFO; break;
      default: sattr->type = TBS_FTYPE_NULL;
    }

    /* Use the mask to set null values for those items we
     * aren't going to set (just to be neat an obvious) 
     */
    sattr->mode = (mask & AT_MODE) ? 
	(u_short)(VATTR_GET_MODE(vap) & VIEW_ATTR_FMODES_MASK) : (u_short) -1;
    MVFS_VATTR_TO_SATTR_UID(mask, vap, sattr);
    MVFS_VATTR_TO_SATTR_GID(mask, vap, sattr);

    sattr->size = (mask & AT_SIZE) ? VATTR_GET_SIZE(vap) : 0;

    if (mask & AT_ATIME) {
	VATTR_GET_ATIME_TV(vap, &sattr->atime);
    } else {
        sattr->atime.tv_sec = sattr->atime.tv_usec = -1;
    }
    if (mask & AT_MTIME) {
	VATTR_GET_MTIME_TV(vap, &sattr->mtime);
    } else {
	sattr->mtime.tv_sec = sattr->mtime.tv_usec = -1;
    }

    /* 
     * Finally, set up the mask bits.
     * Our bit defns actually match the V.4 bit definitions,
     * but for maximum portibility, do it the hard way instead
     * of just copying them over.
     */

    sattr->mask = 0;
    if (mask & AT_TYPE)  sattr->mask |= VIEW_ATTR_TYPE;
    if (mask & AT_MODE)  sattr->mask |= VIEW_ATTR_MODE;
    if (mask & AT_UID)   sattr->mask |= VIEW_ATTR_USID;
    if (mask & AT_GID)   sattr->mask |= VIEW_ATTR_GSID;
    if (mask & AT_SIZE)  sattr->mask |= VIEW_ATTR_SIZE;
    if (mask & AT_ATIME) sattr->mask |= VIEW_ATTR_ATIME;
    if (mask & AT_MTIME) sattr->mask |= VIEW_ATTR_MTIME;
    
}

/******************* UIO ROUTINES *************************/

/********************** MISC ROUTINES *********************************/

/* MFS_FIX_STALE - fixup caches etc. on ESTALE from view */

void
mfs_fix_stale(vp)
VNODE_T *vp;
{

    ASSERT(MFS_VPISMFS(vp));	/* MFS vnodes only */
    if (!MFS_ISVOB(VTOM(vp))) return; /* Vob nodes only */

    /* 
     * Invalidate name cache - names to this version are obviously
     * no good.  (Includes any RVC entries.)
     */

    mfs_dnc_invalvp(vp);

    /* Can't trust attributes either */

    MFS_ATTRINVAL(vp);

    /* Log ESTALE fixing */

    mvfs_log(MFS_LOG_ESTALE, 
		"stale vnode vw=%s vob=%s dbid.gen = 0x%x.0x%x\n", 
		mfs_vp2vw(vp), mfs_vp2dev(vp), mfs_vp2dbid(vp),
		VTOM(vp)->mn_hdr.fid.mf_gen);
}

/* MFS_SVRDESTROY - free up resources in a svr struct */

void
mfs_svrdestroy(svrp)
struct mfs_svr *svrp;
{
    PNPAIR_STRFREE(&svrp->lpn);
    if (svrp->host) STRFREE(svrp->host);
    if (svrp->rpn) STRFREE(svrp->rpn);
}

int
mvfs_util_init(mvfs_cache_sizes_t *mma_sizes)
{
    INITSPLOCK(mvfs_uniqlock,"mvfs_util_spl");
    return 0;
}

void
mvfs_util_free()
{

    FREESPLOCK(mvfs_uniqlock);
}

CLR_VNODE_T *mvfs_printf_logvp = NULL;
STATIC char *printf_logfilename = NULL;
STATIC size_t printf_loglen = 0;
STATIC CRED_T *printf_log_cred = NULL;
STATIC void *printf_filp;
extern int mvfs_cnprint_delay;

int
mvfs_logfile_set(
    char *logfile,
    CALL_DATA_T *cd
)
{
    int error;
    CLR_VNODE_T *newvp;
    char *lfname;
    void *tmp_printf_filp = NULL;

    /* Get copy early */
    lfname = STRDUP(logfile);
    if (lfname == NULL)
        return ENOMEM;

    MVFS_LOCK(&mvfs_printf_lock);
    error = LOOKUP_FOR_IOCTL(lfname, UIO_SYSSPACE, FOLLOW_LINK,
			     0, NULL, &newvp, cd);
    if (error == 0) {
	if (MFS_VPISMFS(MVFS_CVP_TO_VP(newvp))) {
	    error = EINVAL;
	} else if (!MVFS_ISVTYPE(MVFS_CVP_TO_VP(newvp), VREG)) {
	    error = EISDIR;
	} else
	    error = MVOP_OPEN_KERNEL(&newvp, FWRITE, MVFS_CD2CRED(cd),
                                     &tmp_printf_filp);

	if (error == 0) {
	    if (mvfs_printf_logvp != 0) {
		MVOP_FSYNC_KERNEL(MVFS_CVP_TO_VP(mvfs_printf_logvp),
                                  FLAG_DATASYNC, MVFS_CD2CRED(cd), printf_filp);
		(void) MVOP_CLOSE_KERNEL(mvfs_printf_logvp, FWRITE,
                                         MVFS_LASTCLOSE_COUNT,
                                         (MOFFSET_T)0, printf_log_cred,
                                         printf_filp);
		CVN_RELE(mvfs_printf_logvp);
		MDKI_CRFREE(printf_log_cred);
	    }
	    mvfs_printf_logvp = newvp;
            printf_filp = tmp_printf_filp;
	    if (printf_logfilename != NULL) {
		PN_STRFREE(printf_logfilename); /* dump old one, if present */
	    }
	    /* Use our copy */
	    printf_logfilename = lfname;
	    /* and don't free it... */
	    lfname = NULL;
	    printf_loglen = STRLEN(logfile)+1;
	    printf_log_cred = MDKI_CRDUP(MVFS_CD2CRED(cd)); /* take private copy */
	} else {
	    CVN_RELE(newvp);
	}
    }

    MVFS_UNLOCK(&mvfs_printf_lock);
    if (lfname != NULL) STRFREE(lfname);

    return error;
}

void
mvfs_logfile_close()
{
    MVFS_LOCK(&mvfs_printf_lock);
    if (mvfs_printf_logvp) {
	MVOP_FSYNC_KERNEL(MVFS_CVP_TO_VP(mvfs_printf_logvp),
                          FLAG_DATASYNC, printf_log_cred, printf_filp);
	(void) MVOP_CLOSE_KERNEL(mvfs_printf_logvp, FWRITE, MVFS_LASTCLOSE_COUNT,
                                 (MOFFSET_T)0, printf_log_cred, printf_filp);
	REAL_CVN_RELE(mvfs_printf_logvp); /* don't do logging VN_RELE()! */
	mvfs_printf_logvp = NULL;
        printf_filp = NULL;
	if (printf_logfilename != NULL)
	  PN_STRFREE(printf_logfilename);
	printf_logfilename = NULL;
	printf_loglen = 0;
	MDKI_CRFREE(printf_log_cred);
	printf_log_cred = NULL;
    }
    MVFS_UNLOCK(&mvfs_printf_lock);
    return;
}

int
mvfs_logfile_get(pn, len)
char *pn;
register size_t *len;				/* in/out */
{
    int error;

    MVFS_LOCK(&mvfs_printf_lock);
    *len = KS_MIN(*len,printf_loglen);
    if (*len == 0)
	error = 0;
    else
	error = COPYOUT(printf_logfilename, pn, *len);
    MVFS_UNLOCK(&mvfs_printf_lock);
    return error;
}

void
mvfs_logfile_putstr(str, len, nofileoffset)
A_CONST char *str;
u_int len;
int nofileoffset;
{
    struct uio uios;
    IOVEC_T iov;
    struct uio *uiop;
    VATTR_T *vap = NULL;
    int error = 0;

    if (mvfs_printf_logvp == NULL) {
	/* If not going to file, skip first "nofileoffset" bytes */
        /*
         * also we don't need our own printf lock if going to console
         * rather than to log file
         */
        MVFS_REAL_PRINTF("%s", str + nofileoffset);
	return;
    }

    if ((vap = MVFS_VATTR_ALLOC()) == NULL) {
        MVFS_REAL_PRINTF("mvfs_logfile_putstr: failed to allocate a struct "
                         "vattr for use by subsequent code.");
	return;
    }

    MVFS_LOCK(&mvfs_printf_lock);
    uiop = &uios;
    uios.uio_iov = &iov;

    VATTR_NULL(vap);
    VATTR_SET_MASK(vap, AT_SIZE);
    error = MVOP_GETATTR(MVFS_CVP_TO_VP(mvfs_printf_logvp),
                         mvfs_printf_logvp, vap, 0, printf_log_cred);
    if (error) {
        goto cleanup;
    }
    mfs_uioset(&uios, (caddr_t) str, len, vap->va_size, UIO_SYSSPACE);

    MVOP_RWWRLOCK(mvfs_printf_logvp, NULL);
    (void) MVOP_WRITE_KERNEL(mvfs_printf_logvp, &uios, 0, NULL, printf_log_cred,
                             printf_filp);
    MVOP_RWWRUNLOCK(mvfs_printf_logvp, NULL);

  cleanup:
    MVFS_UNLOCK(&mvfs_printf_lock);
    MVFS_FREE_VATTR_FIELDS(vap); /* free sids if copied in getattr */
    MVFS_VATTR_FREE(vap);
    return;
}

#if defined(ATRIA_WIN32_COMMON) && defined(ATRIA_NT_60)
/* Export for Windows to share with minifilter */
__declspec(dllexport)
#endif
void
mvfs_log(
    int pri,
    const char *fmt,
    ...
)
{
    va_list ap;
    va_start(ap, fmt);
    mvfs_vlog(pri, fmt, ap);
    va_end(ap);
}

EXTERN void
mvfs_vlog(
    int pri,
    const char *fmt,
    va_list ap
)
{
    if (MVFS_PRI_LOGGED(pri)) {
	MVFS_VPRINTF_3(pri, mvfs_sevmsg[pri], NULL, fmt, ap);
	if (!mvfs_printf_logvp && pri >= MFS_LOG_DEBUG) {
	    MDKI_USECDELAY(mvfs_cnprint_delay);
	}
    }
}

void
mvfs_logperr(
    int pri,
    int err,
    const char *fmt,
    ...
)
{
    va_list ap;
    if ((err) != 0 && MVFS_PRI_LOGGED(pri) &&
	(((err) != EINTR && (err) != ESTALE) ||
         MVFS_PRI_LOGGED(MFS_LOG_ESTALE)))
    {
	va_start(ap, fmt);
	MVFS_VPRINTF_3(pri, mvfs_sevmsg[pri], mfs_strerr(err), fmt, ap);
	va_end(ap);
	if (!mvfs_printf_logvp && pri >= MFS_LOG_DEBUG) {
	    MDKI_USECDELAY(mvfs_cnprint_delay);
	}
    }
}

void
mvfs_log_sevpri(
    int sev,
    int pri,
    const char *fmt,
    ...
)
{
    va_list ap;
    if (MVFS_PRI_LOGGED(pri)) {
	va_start(ap, fmt);
	MVFS_VPRINTF_3(pri, mvfs_sevmsg[sev], NULL, fmt, ap);
	va_end(ap);
	if (!mvfs_printf_logvp && pri >= MFS_LOG_DEBUG) {
	    MDKI_USECDELAY(mvfs_cnprint_delay);
	}
    }
}

void
mvfs_logperr_sevpri(
    int sev,
    int pri,
    int err,
    const char *fmt,
    ...
)
{
    va_list ap;
    if ((err) != 0 && MVFS_PRI_LOGGED(pri) &&
	((err) != EINTR && ((err) != ESTALE || MVFS_PRI_LOGGED(MFS_LOG_ESTALE)))) {
	va_start(ap, fmt);
	MVFS_VPRINTF_3(pri, mvfs_sevmsg[sev], mfs_strerr(err), fmt, ap);
	va_end(ap);
	if (!mvfs_printf_logvp && pri >= MFS_LOG_DEBUG) {
	    MDKI_USECDELAY(mvfs_cnprint_delay);
	}
    }
}

static char printstr[512];
#define STARTOFFSET 9			/* 8 hex digits + space */
static char digit[] = "0123456789abcdef";

void
mvfs_logfile_printf(A_CONST char *fmt, ...)
{
    va_list ap;
    time_t now;

    va_start(ap, fmt);
    MVFS_VPRINTF_3(MFS_LOG_ERR, mvfs_sevmsg[MFS_LOG_ERR], NULL, fmt, ap);
    va_end(ap);
}

void
mvfs_logfile_vprintf_3(pri, msg1, msg2, fmt, ap)
    int pri;
    A_CONST char *msg1;
    A_CONST char *msg2;
    A_CONST char *fmt;
    va_list ap;
{
    int len;
    time_t now;

    MVFS_LOCK(&mvfs_printstr_lock);

    now = MDKI_CTIME();
    printstr[0] = digit[(now >> 28) & 0xf];
    printstr[1] = digit[(now >> 24) & 0xf];
    printstr[2] = digit[(now >> 20) & 0xf];
    printstr[3] = digit[(now >> 16) & 0xf];
    printstr[4] = digit[(now >> 12) & 0xf];
    printstr[5] = digit[(now >> 8) & 0xf];
    printstr[6] = digit[(now >> 4) & 0xf];
    printstr[7] = digit[(now >> 0) & 0xf];
    printstr[8] = ' ';
    printstr[sizeof(printstr)-1] = '\0';
    len = STARTOFFSET;

    if (msg1) {
	STRNCPY(printstr+len, msg1, sizeof(printstr)-len-1);
	len = STRLEN(printstr);
    }
    if (len+1 < sizeof(printstr)) {
	MVFS_VSNPRINTF(printstr+len, sizeof(printstr)-len-1, fmt, ap);
	if (msg2 != NULL) {
	    len += STRLEN(printstr+len);
	    if (len+1 < sizeof(printstr))
		STRNCPY(printstr+len, msg2, sizeof(printstr)-len-1);
	    len += STRLEN(printstr+len);
	    if (len+1 < sizeof(printstr)) /* XXX */
		STRCPY(printstr+len, "\n");
	}
    }
    mvfs_logfile_putstr(printstr, STRLEN(printstr), STARTOFFSET);
    MVFS_UNLOCK(&mvfs_printstr_lock);
}

/****************************************************************************
 * mvfs_oid_to_str (adapted from tbs_oid_to_str)
 * Return a string representing a given object identifier.
 * IN   oid_p           object id
 * IN   str             string buffer for formatted OID string
 * RESULT:              pointer to string buffer
 */

char *
mvfs_oid_to_str(
    A_CONST tbs_oid_t *oid_p,
    tbs_oid_str_t str
)
{
    return mvfs_uuid_to_str(&(oid_p->obj_uuid), str);
}

/****************************************************************************
 * mvfs_uuid_to_str  (adapted from tbs_uuid_to_str)
 * Return a string representing a unique identifier.
 * IN   uuid_p          UUID
 * IN   str             string buffer for formatted UUID string
 * RESULT:              pointer to string buffer
 */

char *
mvfs_uuid_to_str(
    A_CONST tbs_uuid_t *uuid_p,
    tbs_uuid_str_t str
)
{
    if (uuid_p == NULL) {
        STRCPY(str, "(NULL)");
        return str;
    }

    MVFS_SNPRINTF(str, sizeof(tbs_uuid_str_t),
                  "%08x.%04hx%04hx.%02x%02x.%02x:%02x:%02x:%02x:%02x:%02x",
                  uuid_p->time_low, uuid_p->time_mid, uuid_p->time_hi_and_version,
                  uuid_p->clock_seq_hi_and_reserved, uuid_p->clock_seq_low,
                  uuid_p->node[0], uuid_p->node[1], uuid_p->node[2],
                  uuid_p->node[3], uuid_p->node[4], uuid_p->node[5]);

    return str;
}  /* mvfs_uuid_to_str */

/* Initialize a spinlock pool header.
 * IN: pool   -  ptr to the struct anchoring the pool
 * IN: size   -  number of locks in the pool
 * IN: func   -  NULL if calling code uses a macro, else a func ptr
 * IN: locknm -  spin lock name
 * Return value: ENOMEM if error on kmem_alloc, else 0 for success
 */
int 
mvfs_splock_pool_init(
    splock_pool_t *pool,
    int size,
    SPLOCK_T *(*func)(splock_pool_t *, unsigned int),
    char *locknm
)
{
    int          len, i;
    caddr_t      chunk;

    len = sizeof(SPLOCK_T) * size;
    chunk = KMEM_ALLOC(len, KM_SLEEP);
    if (chunk == NULL) {
        /* Calling layer must decide how to handle error */
        mvfs_log(MFS_LOG_ERR, "Failed to allocate memory for spinlock table, pool %"KS_FMT_PTR_T"\n", pool);
        return(ENOMEM);  
    }
    BZERO(chunk, len);
    pool->spl_table = (SPLOCK_T *) chunk;

    for (i = 0; i < size; i++) {
        INITSPLOCK(pool->spl_table[i], locknm);
    }

    pool->spl_count = size;
    pool->spl_func = func;
    return (0);

}

void
mvfs_splock_pool_free(splock_pool_t *pool)
{
    int i;
    for (i=0; i < pool->spl_count; i++) {
        FREESPLOCK(pool->spl_table[i]);
    }
    KMEM_FREE(pool->spl_table, sizeof(SPLOCK_T) * pool->spl_count);
    return;
}

/* Initialize a mvfs_lock pool.
 * IN: pool   -  ptr to the struct anchoring the pool
 * IN: size   -  number of locks in the pool
 * IN: func   -  NULL if calling code uses a macro, else a func ptr
 * IN: locknm -  mvfs lock name
 * Return value: ENOMEM if error on kmem_alloc, else 0 for success
 */
int 
mvfs_lock_pool_init(
    mvfs_lock_pool_t *pool,
    int size,
    LOCK_T *(*func)(mvfs_lock_pool_t *, unsigned int),
    char *locknm
)
{
    int          len, i;
    caddr_t      chunk;

    len = sizeof(LOCK_T) * size;
    chunk = KMEM_ALLOC(len, KM_SLEEP);
    if (chunk == NULL) {
        /* Calling layer must decide how to handle error */
        mvfs_log(MFS_LOG_ERR, "Failed to allocate memory for mvfs_lock table, pool %"KS_FMT_PTR_T"\n", pool);
        return(ENOMEM);  
    }
    BZERO(chunk, len);
    pool->mlp_table = (LOCK_T *) chunk;

    for (i = 0; i < size; i++) {
        INITLOCK(&(pool->mlp_table[i]), locknm);
    }

    pool->mlp_count = size;
    pool->mlp_func = func;
    return (0);

}

void
mvfs_lock_pool_free(mvfs_lock_pool_t *pool)
{
    int i;
    for (i=0; i < pool->mlp_count; i++) {
        FREELOCK(&(pool->mlp_table[i]));
    }
    KMEM_FREE(pool->mlp_table, sizeof(LOCK_T) * pool->mlp_count);
    return;
}

void
mvfs_credutl_unix_uid_to_sid(
    CRED_UID_T uid,
    credutl_sid_t *csidp
)
{
    /* If we are nobody, then copy the nobody credutl_sid */
    if (uid == TBS_UID_NOBODY)
       *csidp = CREDUTL_SID_NOBODY;
    else credutl_unix_uid_to_sid(uid, csidp);
    
}

void
mvfs_credutl_unix_gid_to_sid(
    CRED_GID_T gid,
    credutl_sid_t *csidp
)
{
    /* If we are nobody, then copy the nobody credutl_sid */
    if (gid == TBS_GID_NOBODY)
       *csidp = CREDUTL_SID_NOBODY;
    else credutl_unix_gid_to_sid(gid, csidp);
}

/*
 * MVFS_ENSURE_POWER2 - Internal routine to return a power of 2 close to
 * 			the seed number.
 */
int
mvfs_ensure_power2(int n)
{
    int size;

    size = 1;
    while ((n = n/2) != 0)
	size *= 2;
    return size;
}

/*
 * MVFS_BUMPTIME - keep statistics on real time.  This routine takes the
 * start time which is passed in as the first arg.  It reads in the current
 * time into the second arg and computes the delta between those.  This delta
 * is added to the total time which is the third arg to this.  This routine
 * can be used to keep a running total of delta times as well as returning
 * the current delta, or it can just be used to compute the current delta
 * by setting the third arg to 0.  This routine assumes the caller is taking
 * care of syncronizing access to the total time by locking or disabling
 * preemption.  This routine assumes that tvp->tv_nsec is canonical.
 */

void
mvfs_bumptime(
    timestruc_t *stp,
    timestruc_t *dtp,
    timestruc_t *tvp
)
{
    MDKI_HRTIME(dtp);

    /*
     * Calculate time operation took.
     */

    if ((dtp->tv_nsec -= stp->tv_nsec) < 0) {
        dtp->tv_nsec += 1000000000;
        dtp->tv_sec--;
    }
    dtp->tv_sec -= stp->tv_sec;

    if ((tvp->tv_nsec += dtp->tv_nsec) >= 1000000000) {
        tvp->tv_nsec -= 1000000000;
        tvp->tv_sec++;
    }
    tvp->tv_sec += dtp->tv_sec;

    return;
}
static const char vnode_verid_mvfs_utils_c[] = "$Id:  63dbe5cb.dc5411df.9210.00:01:83:0a:3b:75 $";
