/* * (C) Copyright IBM Corporation 1990, 2012. */
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
/* mvfs_clearops.c */
/*
 * Description:
 *
 * 	A word about locking protocols is important here.  The cleartext
 *	operations all require that the mnode be locked before calling
 *	the cleartext op. 
 */
#include "mvfs_systm.h"
#include "mvfs.h"

/*
 * Prototypes for internal routines
 */
EXTERN int 
mvfs_translate_path(
    char *text,
    size_t size,
    mvfs_sp_ent_t *table,
    int tlen,
    char **replacement_p
);

STATIC void
mfs_clear_closevp(
    VNODE_T *vp,
    CRED_T *cred
);

#ifdef MVFS_DEBUG
int mvfs_cltxt_creds_enabled = 1;       /* not STATIC, so it's patchable */
#endif

STATIC CRED_T *
mvfs_find_cred(
    mvfs_clr_creds_t *aclist,
    CRED_T *cred
);
STATIC void
mvfs_clear_release_credlist(mvfs_clr_creds_t *a_clist);

/* MFS_CLEARPERR - print an error on a cleartext file */

void
mfs_clearperr(vp, s, error)
VNODE_T *vp;
char *s;
int error;
{
    char *pn;
    int pri;

    ASSERT(MFS_ISVOB(VTOM(vp)));

    pn = VTOM(vp)->mn_vob.cleartext.nm;
    pri = MFS_LOG_ERR;
    /* 
     * Lower priority msg for ESTALE/EINTR - they are expected! 
     * Warn if error is 0, this is used for "incomplete writes".
     */
    if (error == ESTALE) pri = MFS_LOG_ESTALE;
    if (error == 0) 	 pri = MFS_LOG_WARN;
    if (error == EINTR || error == ENOSPC)  pri = MFS_LOG_DEBUG;

    /*
     * Put more post-mortem info into the system console log.
     */
    mvfs_log(pri, "%s view=%s vob=%s dbid=0x%x%s\n", s, mfs_vp2vw(vp),
	     mfs_vp2dev(vp), mfs_vp2dbid(vp), mfs_strerr(error));
    if (pn) {
	mvfs_log(pri, "cleartext pname= %s\n", pn);
    }

    /* Ignore blurt to user on ESTALE/EINTR */

    if (pri > MFS_LOG_ERR) return;

    /* Blurt to user with just cleartext pname.
     * Often the most useful name to give.
     * FIXME: someday, use "name" of object for user instead of
     * a name from underlying implementation.
     */

    /* FIXME: need an abstraction for cleartext uprintf here. */
    UPRINTF(("%s view=%s vob=%s dbid=0x%x%s\n", s, mfs_vp2vw(vp),
			mfs_vp2dev(vp), mfs_vp2dbid(vp), mfs_strerr(error)));
    if (pn) {
	UPRINTF(("cleartext pname= %s\n", pn));
    }
}

/* MFS_CLEAR_ERROR - common cleartext error handling from vnode ops */

void
mfs_clear_error(vp, s, error)
VNODE_T *vp;
char *s;
int error;
{
    mfs_mnode_t *mnp;

    if (error == 0) return;

    mnp = VTOM(vp);
    ASSERT(MFS_ISVOB(mnp));

    switch (error) {
	case 0:	return;
        /*
         * Don't mark the vnode rwerr just because the user passed
         * in an invalid address.
         */
	case EFAULT: return;

	/* Expected errors */
	case EINVAL:                    /* FIXME: reconsider for plain return */
	case EAGAIN:
	case EDEADLK:
	case EFBIG:
	case ETIMEDOUT:
	case EINTR:
	    /*
	     * Record error.  This will cause the MFS to release
	     * (purge) the cleartext on inactive, so we won't get
	     * burned by a sticky NFS error on a completely different
	     * open of the file.
	     */
	    mfs_clear_mark_rwerr(vp);
	    break;
	case ESTALE:
	    mfs_clear_mark_rwerr(vp);
	    break;
	default:
	    mfs_clearperr(vp, s, error);
	    mfs_clear_mark_rwerr(vp);
	    break;
    }
    return;
}

void
mvfs_clear_log_stale(
    VNODE_T *vp
)
{
    mfs_mnode_t *mnp = VTOM(vp);
    char *pn;

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    if (mnp->mn_vob.cleartext.ostale_logged) {
        MDB_XLOG((MDB_CLEAROPS, "mnp %lx already stale logged\n", mnp));
        return;
    }

    pn = mnp->mn_vob.cleartext.nm;
    mvfs_log(MFS_LOG_INFO,
             /* FIXME: more than 80 characters of text on one line.
                Poor form, but three lines of error message seem
                excessive. */
             "cannot access file with stale cleartext "
             "(still open by another process or file descriptor)\n");
    mvfs_log(MFS_LOG_INFO,
             "open file: view=%s vob=%s dbid=0x%x%s%s\n",
             mfs_vp2vw(vp),
             mfs_vp2dev(vp), mfs_vp2dbid(vp),
             (pn != NULL) ? " cltxt pname=" : "",
             (pn != NULL) ? pn : "");
    mnp->mn_vob.cleartext.ostale_logged = 1;
}

EXTERN int 
mvfs_translate_path(
    char *text,
    size_t size,
    mvfs_sp_ent_t *table,
    int tlen,
    char **replacement_p
)
{
    register size_t remainder;
    register size_t substlen;
    register int i;

    for (i = 0; i < tlen; i++) {
	if (size < table[i].sp_prlen) /* table prefix too long--no match */
	    continue;
	if (STRNCMP(table[i].sp_prefix, text, table[i].sp_prlen) == 0) {
	    /* The prefix matches--do the substitution. We assume that
	     * sp_target includes a pathname separator on the
	     * end---the mount code insures this for us.  Allocate
	     * space for the replacement prefix, the remainder of the path
	     * component, plus one for NUL terminator, and shuffle the bytes.
	     */
	    remainder = size - table[i].sp_prlen;
	    substlen = table[i].sp_tglen;
	    *replacement_p = KMEM_ALLOC(substlen + remainder + 1,
				     KM_SLEEP|KM_PAGED);
	    if (*replacement_p == NULL)
		    return ENOMEM;
	    BCOPY(table[i].sp_target, *replacement_p, substlen);
	    BCOPY(text + table[i].sp_prlen,
		  (*replacement_p) + substlen,
		  remainder);
	    (*replacement_p)[substlen + remainder] = '\0';
	    break;
	} else {
            *replacement_p = NULL;
        }
    }
    return 0;				/* no error */
}

/* MFS_SET_CPNAME - set the cleartext pname in the mnode */

int
mfs_set_cpname(mnp, text, text_size)
register mfs_mnode_t *mnp;
char *text;
size_t text_size;
{
    char *cp;
    size_t cplen;
    struct mfs_mntinfo *mmi;
    VNODE_T *vp;
    char *trans_text;
    int err;
    int sep_len = 0;  /* length (in characters) of a separator */

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    if (mnp->mn_vob.cleartext.nm) {
	PN_STRFREE(mnp->mn_vob.cleartext.nm);
    }

    vp = MTOV(mnp);
    mmi = V_TO_MMI(vp);

    /* 
     * Allocate enough room for pname and trailing null. 
     * I make a copy here to save kernel memory space since the
     * size of the text buffer is usually MAXPATHLEN
     */

    if (mnp->mn_vob.cleartext.isvob) {
	if (mmi->mmi_sptable) {
	    /* We have a pathname translation prefix table for this VOB.
	       Apply it to the text name. */
	    err = mvfs_translate_path(text, text_size,
					     mmi->mmi_sptable,
					     mmi->mmi_sptable_len,
					     &trans_text);
	    if (err != 0)
		return err;
	    if (trans_text) {
		mnp->mn_vob.cleartext.nm = trans_text;
		return 0;
	    }
	}
	cp = (char *) MFS_STRBUFPN_PAIR_GET_KPN(&(mmi->mmi_svr.lpn)).s;
    } else {
#ifdef ATRIA_NT_60
        /* Windows only, Vista and above platforms
         *
         * If we attached to the view storage via a mup enabled name, use that
         * instead of the local pathname to the view storage.
         */
        if (((cp = VTOM(mnp->mn_hdr.viewvp)->mn_view.svr.net_pn) == NULL))
            cp = (char *) MFS_STRBUFPN_PAIR_GET_KPN(
                 &(VTOM(mnp->mn_hdr.viewvp)->mn_view.svr.lpn)).s;
#else
        cp = (char *) MFS_STRBUFPN_PAIR_GET_KPN(
             &(VTOM(mnp->mn_hdr.viewvp)->mn_view.svr.lpn)).s;
#endif 
    }
    cplen = STRLEN(cp);
    ASSERT(cp);
    ASSERT(cplen);

    /* Check to see if we will need a separator or not (changes length) */
    if ((cplen == 0) || (MVFS_TRAILING_SEPS(cp, cplen) == 0)) {
	sep_len = 1;
    }

    mnp->mn_vob.cleartext.nm = KMEM_ALLOC(cplen+sep_len+text_size+1,
					  KM_SLEEP|KM_PAGED);
    if (mnp->mn_vob.cleartext.nm) {
        BCOPY(cp, mnp->mn_vob.cleartext.nm, cplen);
	/* Add a separator if not already there */
	if (sep_len == 1) {
            mnp->mn_vob.cleartext.nm[cplen++] = MVFS_PN_SEP_CHAR;
	}
        BCOPY(text, &mnp->mn_vob.cleartext.nm[cplen], text_size);
        mnp->mn_vob.cleartext.nm[cplen+text_size] = '\0';
	return(0);
    } else {
	return(ENOMEM);
    }
}

/* 
 * MFS_CLEAR_MARK_DOC - mark cleartext vnode as delete on close.
 */

void
mfs_clear_mark_doc(vp)
VNODE_T *vp;
{
    mfs_mnode_t *mnp = VTOM(vp);

    if (!MFS_ISVOB(mnp)) return;	/* Vob objects only */
    if (mnp->mn_hdr.realvp) {
        mnp->mn_vob.cleartext.delete_on_close = 1;
    }
}

/* 
 * MFS_CLEAR_MARK_PURGE - mark cleartext vnode for purge when inactivated 
 * This is used when the vnode is invalid (needs to be re-looked up),
 * but we think the pathname is probably still valid.
 */

void
mfs_clear_mark_purge(vp)
VNODE_T *vp;
{
    mfs_mnode_t *mnp = VTOM(vp);

    if (!MFS_ISVOB(mnp)) return;	/* Vob objects only */

#ifdef NFSV4_SHADOW_VNODE
    if (mnp->mn_hdr.realvp || mnp->mn_hdr.realvp_master) {
#else
    if (mnp->mn_hdr.realvp) {
#endif
        mnp->mn_vob.cleartext.purge_cvp = 1;
    }
}

/* 
 * MFS_CLEAR_MARK_NAME_PURGE - mark cleartext for purge when inactivated, and
 * be sure to flush any name 
 * This is used when both the pathname and any cleartext vnode are suspected
 * of being wrong.
 */

void
mfs_clear_mark_name_purge(vp)
VNODE_T *vp;
{
    mfs_mnode_t *mnp = VTOM(vp);

    if (!MFS_ISVOB(mnp)) return;	/* Vob objects only */

#ifdef NFSV4_SHADOW_VNODE
    if (mnp->mn_hdr.realvp || mnp->mn_hdr.realvp_master) {
#else
    if (mnp->mn_hdr.realvp) {
#endif
        mnp->mn_vob.cleartext.purge_cvp = 1;
    }
    mnp->mn_vob.cleartext.purge_nm = 1;
}

/* 
 * MFS_CLEAR_MARK_RWERR - mark cleartext for 'read-write' error
 * Will eventually cause pathname and cleartext vnode ptr (if set)
 * to be purged on inactive().  In addition, it causes the MVFS
 * not to continue I/O, or trust the attributes of the cleartext
 * after the read/write error.
 */

void
mfs_clear_mark_rwerr(vp)
VNODE_T *vp;
{
    mfs_mnode_t *mnp = VTOM(vp);

    if (!MFS_ISVOB(mnp)) return;	/* Vob objects only */

#ifdef NFSV4_SHADOW_VNODE
    if (mnp->mn_hdr.realvp || mnp->mn_hdr.realvp_master) {
#else
    if (mnp->mn_hdr.realvp) {
#endif
        mnp->mn_vob.cleartext.purge_cvp = 1;
    }
    mnp->mn_vob.cleartext.purge_nm = 1;
    mnp->mn_vob.cleartext.rwerr = 1;
}

/* MFS_CLEAR_RELE - release cleartext vnode ptr */

void
mfs_clear_rele(
    VNODE_T *vp,
    CRED_T *cred
)
{
    register struct mfs_mnode *mnp;
    VNODE_T *cvp;

    mnp = VTOM(vp);

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    /* Nothing to do if no purges set */

    if (!(mnp->mn_vob.cleartext.rwerr ||
	  mnp->mn_vob.cleartext.purge_nm ||
	  mnp->mn_vob.cleartext.purge_cvp)) return;

    /* Mistake to purge while open_count > 0 */

    if (mnp->mn_vob.open_count > 0) {
        if (mdb_traps & 0x100)
	    MDKI_PANIC("purging cleartext while vnode open");
	else {
	    mvfs_log(MFS_LOG_ERR, ("clear_rele: purging cleartext while vnode open\n"));
	    /*
	     * We need to preserve the OPEN/CLOSE set
	     * for the underlying cleartext vnode, so
	     * let closes it.
	     */
	    if (mnp->mn_hdr.realvp)
	        mfs_clear_closevp(vp,cred);
	}
    }
    MDB_XLOG((MDB_CLEAROPS,"rele: mnp=%"KS_FMT_PTR_T", cvp=%"KS_FMT_PTR_T", purge=%d\n",
			   mnp, mnp->mn_hdr.realvp,
			   mnp->mn_vob.cleartext.purge_cvp));

#ifdef NFSV4_SHADOW_VNODE
    if ((mnp->mn_hdr.realvp || mnp->mn_hdr.realvp_master) && mnp->mn_vob.cleartext.purge_cvp) {
#else
    if (mnp->mn_hdr.realvp && mnp->mn_vob.cleartext.purge_cvp) {
#endif
        /* 
         * Drop the cleartext vnode.
         */
        mvfs_new_cltxt(vp, NULL);
    }

    /* 06/07/93 - name is cached if no error on vnode */
    if (mnp->mn_vob.cleartext.nm && mnp->mn_vob.cleartext.purge_nm) {
        /* purge creds if not dropped above by mvfs_new_cltxt() */
        if (!mnp->mn_vob.cleartext.purge_cvp)
            MVFS_RELEASE_CREDLIST(mnp);
	PN_STRFREE(mnp->mn_vob.cleartext.nm);
    }

    /* Clear some "sticky bits" */

    mnp->mn_vob.cleartext.rwerr = 0;
    mnp->mn_vob.cleartext.purge_nm = 0;
    mnp->mn_vob.cleartext.purge_cvp = 0;
    mnp->mn_vob.cleartext.ostale_logged = 0;

    /* pages will get flushed when underlying filesystem inactivate is called */
    mnp->mn_hdr.clear_dirty = 0;
}

/* MFS_CLEAR_WRITABLE - check if cleartext writable and display error if not */

int
mfs_clear_writable(vp)
VNODE_T *vp;
{
    mfs_mnode_t *mnp = VTOM(vp);

    ASSERT(MFS_ISVOB(mnp));
    ASSERT(MISLOCKED(mnp));

    if (MFS_CLRTEXT_RO(mnp)) {
	mfs_clearperr(vp, "writing shared cleartext files not allowed", EROFS);
	return(0);
    }
    return(1);
}

/* MVFS_CLEARATTR - update cleartext attributes */

int
mvfs_clearattr(
    VNODE_T *vp,
    VATTR_T *vap, /* look only at mask to determine behavior */
    CRED_T *cred
)
{
    int error = 0;
    register struct mfs_mnode *mnp;
    CLR_VNODE_T *cvp;
    struct timeval o_mtime;
    struct timeval n_mtime;
    VATTR_SIZE_T sz;
    int modified = 0;
    u_long mask;
    VATTR_T va;

    mnp = VTOM(vp);

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));
	
    cvp = mnp->mn_hdr.realvp;
    if (cvp != NULL) {
	MFS_CHKSP(STK_GETATTR);
	ASSERT(!MFS_VPISMFS(MVFS_CVP_TO_VP(cvp)));
	VATTR_GET_MTIME_TV(&mnp->mn_vob.cleartext.va, &o_mtime);
	sz = VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va);
        /*
         * NFS performance hack: Some platforms force a synchronous
         * write of any delayed/buffered data before returning full
         * file attributes via MVOP_GETATTR() (in order to get mtime,
         * and perhaps nblocks, updated).  If the caller of
         * nfs_getattr() only asks for the file size, the underlying
         * NFS will not do a flush.  So, we see what our caller asked
         * for, treating AT_SIZE specially.  A request for any other
         * attributes, or for unknown attributes (vap == NULL) and we
         * do a regular MVOP_GETATTR().
         */
        if (vap) {
            mask = VATTR_GET_MASK(vap);
            if (mask == AT_SIZE) {
                VATTR_NULL(&va);
                VATTR_SET_MASK(&va, mask);
                error = MVOP_GETATTR(MVFS_CVP_TO_VP(cvp), cvp, &va, 0, cred);
                if (!error)
                    VATTR_SET_SIZE(&mnp->mn_vob.cleartext.va,
                                   VATTR_GET_SIZE(&va));
                MVFS_FREE_VATTR_FIELDS(&va);
            } else {
                /*
                 * don't bother with VATTR_NULL, the MVOP_GETATTR() will
                 * fill in everything (AT_ALL).
                 *
                 * NOTE: mfs_mnclean must free the cleartext vattr data.
                 */
                VATTR_SET_MASK(&mnp->mn_vob.cleartext.va, AT_ALL);
                error = MVOP_GETATTR(MVFS_CVP_TO_VP(cvp), cvp, 
                                     &mnp->mn_vob.cleartext.va, 0, cred);
            }
        } else {
            /*
             * don't bother with VATTR_NULL, the MVOP_GETATTR() will
             * fill in everything (AT_ALL).
             *
             * NOTE: mfs_mnclean must free the cleartext vattr data.
             */
            VATTR_SET_MASK(&mnp->mn_vob.cleartext.va, AT_ALL);
            error = MVOP_GETATTR(MVFS_CVP_TO_VP(cvp), cvp,
                                 &mnp->mn_vob.cleartext.va, 0, cred);
        }
	if (!error) {
	    /* Sync with wrapper (if required) */
	    MVFS_WRAP_SYNC_SIZE(vp,
                                VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va),
                                TRUE);

	    /* See if file appears modified */

	    VATTR_GET_MTIME_TV(&mnp->mn_vob.cleartext.va, &n_mtime);
	    modified = (!MFS_TVEQ(n_mtime, o_mtime) ||
			sz != VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va));

	    /* Data modified .. don't care unless this is procedure
               text in which case we should kill the processes using it. */

	    if (modified) {
	        PVN_FLUSHTEXT(vp);
		 mnp->mn_vob.sync_ctime = 1;	/* known out of date */
	    }
	} else {
	    mfs_clearperr(vp, "cleartext getattr", error);
	    if (error == ESTALE) {
		mfs_clear_mark_purge(vp);	/* Mark for purge */
	    }
	}
#ifdef MVFS_DEBUG
	VATTR_GET_ATIME_TV(&mnp->mn_vob.cleartext.va, &o_mtime);
	VATTR_GET_MTIME_TV(&mnp->mn_vob.cleartext.va, &n_mtime);
	MDB_XLOG((MDB_CLEAROPS, "attr: mnp=%"KS_FMT_PTR_T", a/m= %"KS_FMT_TV_SEC_T_D".%"KS_FMT_TV_USEC_T_D"/%"KS_FMT_TV_SEC_T_D".%"KS_FMT_TV_USEC_T_D", mod=%d size= %"MVFS_FMT_VATTR_SIZE_T_D", err=%d\n",
		mnp, o_mtime.tv_sec, o_mtime.tv_usec, n_mtime.tv_sec,
		  n_mtime.tv_usec, modified,
		VATTR_GET_SIZE(&mnp->mn_vob.cleartext.va),error));
#endif
    }
    return(error);
}

/* 
 * MFS_CLEAROWNER - probe over the wire for cleartext owner 
 * FIXME: for NT this should become a general "cleartext" access
 * check for ability to do 'setattr' ops.
 */

int
mfs_clearowner(
    VNODE_T *vp,
    u_long mask,		/* AT_xxx mask for attr's to probe */
    CALL_DATA_T *cd
)
{
    int error;
    VATTR_T *xvap;
    mfs_mnode_t *mnp;

    mnp = VTOM(vp);
    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    if (MFS_REALVP(vp) == NULL) {
	return(ENXIO);
    }
    ASSERT(!MFS_VPISMFS(MFS_REALVP(vp)));

    /*
     * This routine is fundamentally a risky hack!  What we want
     * to do is probe over the wire (if a network FS) to the cleartext
     * for the rights we will have on operations to that cleartext.
     * This can't just be done by looking at the rights because many
     * network FS's transform your identity (esp for root) to another
     * identity at the server.
     *
     * Since there is no call to check these access over the wire
     * what I do is a "SETATTR" of the fields in question (uid, gid, and/or
     * mode) their current values (owner check).
     * This has the inherent risk of racing
     * with a third party node actually changing that field and
     * causing the change by some other node to get lost!  What's
     * a fella to do...
     */

    /* Get latest attributes to narrow race window... */
    /*
     * don't bother with VATTR_NULL, the MVOP_GETATTR() will
     * fill in everything (AT_ALL).
     *
     * NOTE: mfs_mnclean must free the cleartext vattr data.
     */
    VATTR_SET_MASK(&mnp->mn_vob.cleartext.va, AT_ALL);
    error = MVOP_GETATTR(MFS_REALVP(vp), MFS_CLRVP(vp), 
                         &mnp->mn_vob.cleartext.va, 0, MVFS_CD2CRED(cd));
    if (!error) {
        xvap = MVFS_VATTR_ALLOC();

	if (xvap == NULL) return(ENOMEM);

	VATTR_NULL(xvap);
	/* For now, we only support uid/gid/mode */
	if (mask & AT_UID) {
            MVFS_COPY_UID(xvap, &mnp->mn_vob.cleartext.va, mnp, &error);
	}
	if (mask & AT_GID && !error) 
        {
            MVFS_COPY_GID(xvap, &mnp->mn_vob.cleartext.va, mnp, &error);
	}
	if ((mask & AT_MODE) && !error) 
        {
	    VATTR_SET_MODE_TYPE(xvap, 
				VATTR_GET_MODE(&mnp->mn_vob.cleartext.va));
	    VATTR_SET_MODE_RIGHTS(xvap, 
				  VATTR_GET_MODE(&mnp->mn_vob.cleartext.va));
	}
        if (!error) {
            VATTR_SET_MASK(xvap, mask & (AT_UID|AT_GID|AT_MODE));
            error = MVOP_SETATTR(MFS_CVP(vp), xvap, 0, cd, NULL);
            /* Normal setuid hack! */
            if (error == EPERM && MDKI_CR_IS_SETUID_ROOT(MVFS_CD2CRED(cd))) 
            { 
                MDKI_CR_SET_E2RUID(MVFS_CD2CRED(cd));
                error = MVOP_SETATTR(MFS_CVP(vp), xvap, 0, cd, NULL);
                MDKI_CR_SET_E2ROOTUID(MVFS_CD2CRED(cd));
            }
        }
        MVFS_FREE_VATTR_FIELDS(xvap);
        MVFS_VATTR_FREE(xvap);
    }

    return(error);
}

/* 
 * MFS_GETCLEARTEXT_NM - fetch cleartext pathname.
 * On success, returns with cleartext name valid.
 * On failure, cleartext name is NULL.
 */

int
mfs_getcleartext_nm(
    VNODE_T *vp,
    CALL_DATA_T *cd
)
{
    int error;

    error = mfs_clnt_cltxt_locked(vp, cd);
    if (!error) {
	ASSERT(VTOM(vp)->mn_vob.cleartext.nm);
    }
    return(error);
}

EXTERN void
mvfs_new_cltxt(
    VNODE_T *vp,
    CLR_VNODE_T *new_cvp
)
{
    register struct mfs_mnode *mnp = VTOM(vp);

    /* Must prevent paging while dropping realvp */
    MVFS_PVN_INH_PAGING(vp);

#ifdef NFSV4_SHADOW_VNODE
    /* NFSv4 Compliance: RATLC01011478: NFSv4 introduces the concept of shadow
     * vnodes. Refer mvfs_openv_ctx() and the CR for the considerations and
     * changes in MVFS to handle shadow vnodes. All the callers of
     * mvfs_new_cltxt() have ensured that mn_vob.open_count is 0 before the 
     * call. If so, realvp_master must have been unset when open count became zero
     * (refer mvfs_closev_ctx()). Therefore realvp_master is not expected
     * to be set.
     */
    if (mnp->mn_hdr.realvp_master) {
	mvfs_log(MFS_LOG_ERR, 
	   "new cltxt: unexpected NFSv4 master vnode in cleartext cache. master="
            KS_FMT_PTR_T", realvp="KS_FMT_PTR_T"\n", 
	    mnp->mn_hdr.realvp_master, mnp->mn_hdr.realvp);
        CVN_RELE(mnp->mn_hdr.realvp_master);
	mnp->mn_hdr.realvp_master = NULL;
    }
#endif

    CVN_RELE(mnp->mn_hdr.realvp);       /* drop reference to old cltxt vnode */
    mnp->mn_hdr.realvp = new_cvp;
    if (mnp->mn_hdr.realvp != NULL) {
        /* new_cvp is often NULL, but if it's not we better hold what we're
        ** saving away. */
        CVN_HOLD(mnp->mn_hdr.realvp);
    } 
    MVFS_PVN_ENB_PAGING(vp);

    /* drop cache of creds used for lookups */
    MVFS_RELEASE_CREDLIST(mnp);
}

/*
 * Credentials caching for lookups:
 *
 * The MVFS caches cleartext vnodes (realvp) in the mnode.  To properly
 * enforce access controls to the file, it is not sufficient to just
 * check access permissions on the realvp, because while the vnode
 * itself may have permissive mode bits, there may be components in the
 * pathname used to look up the realvp which are not accessible to
 * another user.  We must look up the cleartext via the name for each
 * distinct CRED_T to verify that it has permission to get to the
 * cleartext.
 *
 * In order to avoid looking up the name on every cleartext activation,
 * we cache the CRED_Ts which have already been used to activate the
 * cleartext on a particular mnode.  (The cache is flushed whenever the
 * cleartext is deactivated.)  If no equivalent CRED_T has looked up
 * the name, the caller looks it up and (if successful) adds itself to
 * the linked list of validated CRED_Ts for that mnode.  Error handling
 * is a bit troublesome--if the caller has trouble finding the realvp
 * or finds one but it doesn't match the already-known realvp, we have
 * to be careful to respond appropriately.
 *
 * When a CRED_T is being added to an mnode, it is first looked up in a
 * system-wide hash table of known CRED_Ts.  There may be an equivalent
 * CRED_T already known to MVFS, and if so that CRED_T is referenced
 * and linked to the mnode.  This is particularly desirable for non-CC
 * access: the NFS daemon generates new distinct CRED_Ts on each RPC,
 * and if we didn't use a common CRED_T for a user, we could be
 * referencing a different CRED_T on each mnode touched by the user.
 * By sharing as much as possible with already-known CRED_Ts, we hope
 * to reduce the memory impact of the cache.
 */

/* 
 * MFS_GETCLEARTEXT - activate an existing cleartext file for rdwr 
 * On success, returns with realvp, cleartext name, bits, and attrs
 *	valid.
 * On failure, all of the above are NULL.
 */

int
mfs_getcleartext(
    VNODE_T *vp,
    CLR_VNODE_T **cvpp,
    CALL_DATA_T *cd
)
{
    int error = 0;
    register struct mfs_mnode *mnp;
    CLR_VNODE_T *cvp = NULL;	/* Set to known value for Asserts */
    timestruc_t stime;	/* For statistics */
    timestruc_t dtime;
    int clookup_retries = 0;	/* Clookup retries */
    tbs_boolean_t record_creds = TRUE;
    CRED_T *fcred;

    mnp = VTOM(vp);

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    /* For now, regular files only please... */

    if (!MVFS_ISVTYPE(vp, VREG)) {
	error = EISDIR;
	goto out;
    }
	 
    /*
     * This routine returns a validated (non-stale), held, cleartext
     * vnode to the caller.  It uses multiple levels of caching
     * to speed up the operation of going from the
     * MFS vnode object to the real vnode object
     * The levels are:
     *	   (1) Active held cleartext vnode in mnp->mn_hdr.realvp
     *	       FIXME: add validation to this vnode after a timeout
     *	           by doing an MVOP_OPEN/CLOSE pair or a getattr_otw.
     *     (2) NYI:  for local files, cache a FID (from VOP_FID).
     *     (3) Just cache the pname.  If the lookup fails,
     *	       they dump the pname and get the object again.
     *     (4) Everything missed - go to the view.
     */


    if (mnp->mn_hdr.realvp != NULL) {
        /*
         * Verify that this caller has permissions to look up the cleartext
         * name (alas, requiring a lookup if we don't find their creds
         * in our list).  See comments above for a full explanation of what's
         * going on here.
         */
        if (DO_CLTXT_CREDS()) {
	    MCILOCK(mnp);
            fcred = mvfs_find_cred(mnp->mn_vob.cleartext.ok_creds,
                                   MVFS_CD2CRED(cd));
	    MCIUNLOCK(mnp);
            if (fcred == NULL) {
                /* needs to run a lookup */
                BUMPSTAT(mfs_clearstat.cleargetlkup);
                goto findit;
            } else {
                MDB_XLOG((MDB_CLEAROPS, "mnp %"KS_FMT_PTR_T" found cred %"KS_FMT_PTR_T" as %"KS_FMT_PTR_T"\n",
                          mnp, MVFS_CD2CRED(cd), fcred));
                record_creds = FALSE;       /* found them */
            }
        }

	/* 
	 * If still in revalidation window, then just used
	 * the cached cleartext and update the time validated.
	 * If the caller of this routine gets an error, he will mark
	 * the cleartext for purging on inactive, and the revalidate_time
	 * won't matter.  It he doesn't get an error, then the op succeeded
	 * and so it was right to update the revalidate time.
	 */
	if (MDKI_CTIME() < mnp->mn_vob.cleartext.revalidate_time) {
	    /* 
	     * Randomize time a bit with mnode number low-bits. 
	     * To avoid revalidating a pile of cleartexts all at once.
             */
	    mnp->mn_vob.cleartext.revalidate_time = 
			MDKI_CTIME() + 3600 + (mnp->mn_hdr.mnum & 0xff);
	    error = 0;
	    if (mnp->mn_vob.cleartext.used == 0) {
		BUMPSTAT(mfs_clearstat.clearreclaim);
	    }
	    goto out;
	} else {
	    /*
	     * Time expired.  Revalidate the cleartext ptr by doing an
	     * a getattr on the cleartext.   You can't do an open/close
	     * pair (which would force a getattr on the file over NFS),
	     * because that could mess up file-locks on the cleartext.
	     * Since this only happens after an egregiously long timeout
	     * ( ~ 1 hr), I'm willing to bet the getattr will go over the wire
	     * for NFS.
	     */
            /*
             * don't bother with VATTR_NULL, the MVOP_GETATTR() will
             * fill in everything (AT_ALL).
             *
             * NOTE: mfs_mnclean must free the cleartext vattr data.
             */
            VATTR_SET_MASK(&mnp->mn_vob.cleartext.va, AT_ALL);
	    error = MVOP_GETATTR(MVFS_CVP_TO_VP(mnp->mn_hdr.realvp),
                                 mnp->mn_hdr.realvp, 
			         &mnp->mn_vob.cleartext.va, 0, 
                                 MVFS_CD2CRED(cd));
	    if (!error) {
		if (mnp->mn_vob.cleartext.used == 0) {
		    BUMPSTAT(mfs_clearstat.clearreclaim);
		}
		goto out;	/* Cleartext validated OK */
	    }

	    /* 
	     * Release cleartext here iff no active opens.
	     * If there are opens, then we can't rebind
	     * the cleartext because we don't know how many
	     * of what kinds of opens to do.
	     */

	    if (mnp->mn_vob.open_count == 0) {
                mvfs_new_cltxt(vp, NULL); /* drop old cltxt */
	    } else {
		/* Can't rebind, but mark for purging on inactive */
		mfs_clear_mark_purge(vp);
		error = ESTALE;
		goto out;
	    }
	}
    }

    /*
     * If we once had a cleartext but didn't find it attached now,
     * count that as a miss.
     */
    if (mnp->mn_vob.cleartext.hadonce) {
	BUMPSTAT(mfs_clearstat.clearreclaim);
	BUMPSTAT(mfs_clearstat.clearreclaimmiss);
    }

  findit:
    /* 
     * Keep stats counter on number of getcleartexts
     * that required any lookups
     */
	
    BUMPSTAT(mfs_clearstat.clearget);

    /*
     * If there is a cleartext pname still cached, then try to
     * open that pname.  This pname may be stale (or not found), 
     * in which case we must flush this pname and fall through 
     * to refetching the cleartext pname from the view.
     *
     * The important requirement for this to work
     * is that no "writable" cleartext is ever scrubbed from
     * underneath the MFS.  This will lose data which can not be
     * reconstructed.
     */

    if (mnp->mn_vob.cleartext.nm) {
	/* Keep time stats over lookup ops only */
        MDKI_HRTIME(&stime);
	error = LOOKUP_STORAGE_FILE(MFS_CLRTEXT_RO(mnp),
			mnp->mn_vob.cleartext.nm, NULL, &cvp, MVFS_CD2CRED(cd));
	MVFS_BUMPTIME(stime, dtime, mfs_clearstat.clearget_time);
        if (mnp->mn_hdr.realvp != NULL) {
            /* we were looking up to check this caller's permissions */
            if (!error) {
                /* looked up OK */
                if (!CVN_CMP(mnp->mn_hdr.realvp, cvp)) {
                    /* whoops, lookup intended for permission checking only,
                     * yet returned a different (effective) vnode!
                     * If no active opens, we can drop the old
                     * cleartext vp, as is done in other cases here, and
                     * use the new. Case like this can happen if old ctxt is 
                     * scrubbed and ctxt gets re-created by a different client. 
                     */
                    MDB_XLOG((MDB_CLEAROPS, "getcleartext: mnp %"
                              KS_FMT_PTR_T" cvp mismatch, old %"
                              KS_FMT_PTR_T" new %"KS_FMT_PTR_T"\n",
                              mnp, mnp->mn_hdr.realvp, cvp));
                    if (mnp->mn_vob.open_count == 0) {
                        /* drop old ctxt & creds, take extra hold on the new */
                        mvfs_new_cltxt(vp, cvp);
                        mnp->mn_vob.cleartext.rwerr = 0;
                        mnp->mn_vob.cleartext.purge_nm = 0;   
                        mnp->mn_vob.cleartext.purge_cvp = 0; 
                        mnp->mn_vob.cleartext.ostale_logged = 0; 
                        mnp->mn_vob.cleartext.atime_pushed = 0;
                        /* Make sure we have valid stats */
                        error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
                        MDB_XLOG((MDB_CLEAROPS, "idle ctxt rebind %s\n", mnp->mn_vob.cleartext.nm));
                    } else {
                        mfs_clear_mark_purge(vp);
                        error = ESTALE;
                        MDB_XLOG((MDB_CLEAROPS, "ctxt marked stale %s\n", mnp->mn_vob.cleartext.nm));
                    }
                } else {  
                    /* cvp matched, just make sure we have valid stats */
                    if (mnp->mn_vob.cleartext.used == 0) {
                        BUMPSTAT(mfs_clearstat.clearreclaim);
                    }
                    error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
                }
                CVN_RELE(cvp); /* release the hold from the lookup */
                goto out;
            }
            /* Error on lookup.  If mnode isn't open, flush its
               cleartext and continue below to lookup from scratch.
               FIXME: (?) Don't bother differentiating errors (permission vs.
               missing cltxt). */
	    if (mnp->mn_vob.open_count == 0) {
                MDB_XLOG((MDB_CLEAROPS,
                          "mnp %"KS_FMT_PTR_T" idle cltxt %s lookup failure %d, dumped\n",
                          mnp, mnp->mn_vob.cleartext.nm, error));
                /* Dump (probable) bogus cleartext pathname we have, along
                   with creds used with it... */
                PN_STRFREE(mnp->mn_vob.cleartext.nm);
                mvfs_new_cltxt(vp, NULL); /* drop old cltxt & creds */
                /* ...and continue with relookup below */
            } else switch (error) {
              case ENOENT:
                /* Someone has it open, but we can't find the cltxt?
                   probably a scrubbed cleartext.  Mark for purge, return
                   ESTALE. */
              case ENOTDIR:
              case EISDIR:
                /* something bogus about the cleartext pathname; Also mark
                   for purge/return ESTALE */
                error = ESTALE;
                mfs_clear_mark_purge(vp);
                MDB_XLOG((MDB_CLEAROPS, "mnp %"KS_FMT_PTR_T" in use, stale cltxt\n", mnp));
                goto out;
                /* FIXME: what other errors should we ignore? */
              default:
                /* Just return the error code as-is */
                goto out;
            }
        } else if (!error) {
            /* lookup succeeded, didn't have realvp attached yet */
            MVFS_CTXT_VN_DUP(vp, cvp);
	    mnp->mn_hdr.realvp = cvp;
#ifdef NFSV4_SHADOW_VNODE
	    mnp->mn_hdr.realvp_master = NULL;
#endif
	    mnp->mn_vob.cleartext.rwerr = 0;	   /* No RW error yet */
	    mnp->mn_vob.cleartext.purge_nm = 0;	   /* No name error either */
	    mnp->mn_vob.cleartext.purge_cvp = 0;   /* No need to purge cvp yet */
	    mnp->mn_vob.cleartext.ostale_logged = 0; /* log again if needed */
	    mnp->mn_vob.cleartext.atime_pushed = 0;   
	    /* Make sure we have valid stats */
	    error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
	    goto out;
	} else {
	    /* Dump bogus cleartext pathname we have.  (No creds around since
               realvp vnode is NULL.) */
	    PN_STRFREE(mnp->mn_vob.cleartext.nm);
	}
    }

    /* Fetch the correct cleartext pathname (possibly constructing it
       as a side effect) */

    error = mfs_getcleartext_nm(vp, cd);
    if (!error) {
        /* Keep stats over lookup operations only */

	/* count misses here.  Any cleartext evicted from the
	 * cleartext free list (which is just a subset of mnodes on the
	 * mnode free list with attached cleartexts) has its name freed,
	 * so we will arrive here either if the name was present but
	 * didn't work or because the name wasn't known.
	 */

	BUMPSTAT(mfs_clearstat.cleargetmiss);

        MDKI_HRTIME(&stime);

	/* 
         * LOOKUP_STORAGE_FILE should never go down the MFS file system so
         * there is no need to unlock the mnode before this lookup
         * to avoid lock order violation (e.g. file to dir), and deadlocks.
         * Keeping the mnode locked avoids nasty races.
	 *
	 * Since the automounter may give ENOENT because it is in
	 * the middle of something, and all will be OK in a second,
	 * we retry failed lookups (with ENOENT) a few times.
         */

	for (clookup_retries=0; clookup_retries < 3; clookup_retries++) {
	    error = LOOKUP_STORAGE_FILE(MFS_CLRTEXT_RO(mnp),
					mnp->mn_vob.cleartext.nm, 
					NULL, &cvp, MVFS_CD2CRED(cd));
	    if (!error) {
                MVFS_CTXT_VN_DUP(vp, cvp);
	        mnp->mn_hdr.realvp = cvp;
#ifdef NFSV4_SHADOW_VNODE
                mnp->mn_hdr.realvp_master = NULL;
#endif
	        mnp->mn_vob.cleartext.rwerr = 0;	/* No RW error yet */
	        mnp->mn_vob.cleartext.purge_nm = 0;	/* No name error */
		mnp->mn_vob.cleartext.purge_cvp = 0;
		mnp->mn_vob.cleartext.ostale_logged = 0;
		mnp->mn_vob.cleartext.atime_pushed = 0;
		break;					/* Success!! */
	    } else {
	        ASSERT(cvp == NULL);
                if (error != ENOENT) {
                    /* give a chance of retrying to EINVAL as well - only once */
                    if(error != EINVAL || clookup_retries)
                        break;
                }
		mvfs_log(MFS_LOG_DEBUG, 
		    "Retrying cleartext lookup vw=%s vob=%s dbid=0x%x%s\n",
			 mfs_vp2vw(vp), mfs_vp2dev(vp), mfs_vp2dbid(vp),
			 mfs_strerr(error));
		mvfs_log(MFS_LOG_DEBUG, "cleartext pname= %s\n", 
			 mnp->mn_vob.cleartext.nm);
                /*
                 * if we got ENOENT, try poking the NFS cache to find
                 * the entry which is supposed to be there.
                 */
                if (MVFS_PROD_PARENT_DIR_CACHE(mnp, MVFS_CD2CRED(cd)) != 0) {
                    MDKI_USECDELAY(400000);	/* Sleep for .4 sec */
                }
	    }
	}

	/* Log error if lookup still a failure */
	if (error) {
	    mfs_clearperr(vp, "cleartext lookup", error);
	    /* 
  	     * On errors, must purge all state after print pname 
	     * Never return error with a bad pname still cached in the mnode.
             */
	    mfs_clear_mark_name_purge(vp);
	    mfs_clear_rele(vp, MVFS_CD2CRED(cd));

	    /* 
	     * FIXME: want some code here to correct automatically
	     * when the view/vob is fractured.  If the object is
	     * a DO, then we don't want to bomb right away, we would
	     * like to let the compile etc. proceed to overwrite this
	     * object.  We can't allow this for non-DO objects, however,
	     * because if we don't have real cleartext, and the user
	     * does a "checkin", then cleartool will blindly checkin
	     * an empty file (the user may not even know) and loose
	     * data silently.
	     */
	    /* 
	     * The view has the file, but can't find the cleartext.
	     * Change ENOENT to ENXIO to:
	     *    a) Tell the user something is fractured
	     *    b) Keep programs from trying to create on this fractured
	     *       file and getting a spurious EEXIST (From the view).
	     */
	    if (error == ENOENT) error = ENXIO;
	} else {
	    /* Keep stats only for good lookups (even if took retries!) */
	    MVFS_BUMPTIME(stime, dtime, mfs_clearstat.clearget_time);
	}
    } else {
	/*
	 * Log error.  Note that some TBS_ST_xxx errors
	 * can come back from the clnt_cltxt_locked call.
	 * Slam the error to EIO which is the correct
	 * user error when we can't get the data after
	 * printing the right message.
	 */
	mfs_clearperr(vp, "fetch cleartext", error);
	if (error != EINTR)
	    error = ENXIO;  /* Error for IO/cleartext probs */
    }

    /* 
     * If we just looked up the object, then get 
     * the initial cleartext attributes 
     */

    if (!error) {
	ASSERT(mnp->mn_hdr.realvp);
	error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd));
    }

out:
    if (!error) {
	mnp->mn_vob.cleartext.used = 1;	/* used the cltxt this time around */
	mnp->mn_vob.cleartext.hadonce = 1; /* Had cltxt at least once */
	ASSERT(mnp->mn_hdr.realvp);
	if (cvpp) {
	    *cvpp = mnp->mn_hdr.realvp;
	    CVN_HOLD(*cvpp);
	}

        MVFS_RECORD_CREDLIST(mnp, record_creds, MVFS_CD2CRED(cd));

	/*
	 * Update next revalidate time to 1 hr from now.
	 * Add in the low bits of the mnode number so we don't try
	 * to revalidate all mnodes in the same second.  If the
	 * caller gets an error on this cleartext, he will
	 * mark the cleartext for purge on inactive, and the revalidate
	 * time won't matter anymore.  Otherwise, the op must have
	 * succeeded, so the cleartext must still be OK.
	 */
	mnp->mn_vob.cleartext.revalidate_time = 
		MDKI_CTIME() + 3600 + (mnp->mn_hdr.mnum & 0xff);
    } else {
	if (cvpp) *cvpp = NULL;
    }
    if (error == ESTALE) {
        if (mnp->mn_vob.open_count > 1)
            mvfs_clear_log_stale(vp);
        else {
            MDB_XLOG((MDB_CLEAROPS,"getcleartext: mnp %"KS_FMT_PTR_T" not logging, open count %d\n", mnp, mnp->mn_vob.open_count));
        }
    }
    MDB_XLOG((MDB_CLEAROPS, "getcleartext: vp=%"KS_FMT_PTR_T", cvp=%"KS_FMT_PTR_T", err=%d\n",vp,mnp->mn_hdr.realvp,error));
    return (error);
}

/* MFS_GETLEAF_ANSI - utility routine to return pointer to the last component
   of the passed in (ansi) pathname. */

mfs_pn_char_t *
mfs_getleaf_ansi(pname)
mfs_pn_char_t *pname;
{
    mfs_pn_char_t *s = pname;
    mfs_pn_char_t *leafp = pname;

    for ( ; *s != '\0'; s++) {
	if (PN_IS_SEPCHAR(*s) && 	/* Pathname separator */
	    s[1] != '\0' && 		/* Not trailing separator after leaf */
	    !PN_IS_SEPCHAR(s[1])) {	/* Not multiple separators slashes */
	    	leafp = ++s;		/* Set leaf ptr to next character */
	}
    }	

    /* Return final leaf-name found */
    return(leafp);
}

/* MFS_CLEAR_CREATE - create a cleartext file */

int
mfs_clear_create(
    VNODE_T *vp,
    VATTR_T *vap,
    CLR_VNODE_T **cvpp,
    CALL_DATA_T  *cd,
    int flag
)
{

    int error = 0;
    register struct mfs_mnode *mnp;
    VNODE_T *dvp;
    CLR_VNODE_T *cvp = NULL;   /* Must be set to known value for Asserts */
    timestruc_t stime;
    timestruc_t dtime;
    VNODE_T *urdir = NULL;

    mnp = VTOM(vp);

    ASSERT(MISLOCKED(mnp));
    ASSERT(MFS_ISVOB(mnp));

    MFS_CHKSP(STK_VOPCREATE);

    ASSERT(mnp->mn_hdr.realvp == NULL);

    /*
     * Be neat and init return arg to null in case of errors.
     */
    if (cvpp) *cvpp = NULL;

    /* 
     * If no pathname yet, go get it.  This check is
     * done because the pathname may be cached from the
     * actual create RPC to the view.
     */

    if (mnp->mn_vob.cleartext.nm == NULL) { 
    	error = mfs_clnt_cltxt_locked(vp, cd);
    } else {
	error = 0;
    }

    /* If we got a pathname, resolve it to a the directory vnode. */

    if (!error) {
	if (mnp->mn_vob.cleartext.isvob) {	/* Don't create in VOB */
	    mvfs_log(MFS_LOG_WARN, 
		     "attempted vob cleartext create vw=%s vob=%s dbid=0x%x\n",
		     mfs_vp2vw(vp), mfs_vp2dev(vp), mfs_vp2dbid(vp));
	    error = EROFS;
            goto errout;
        }

        /* Time around cleartext create */

        BUMPSTAT(mfs_clearstat.clearcreate);
        MDKI_HRTIME(&stime);

   	error = MVFS_CREATEVP(mnp->mn_vob.cleartext.nm, vap, &cvp,
                              MVFS_CD2CRED(cd), flag);

	/* Handle errors in cleartext creations */
        if (error) {
            if (error != EEXIST) {
                mfs_clearperr(vp, "cleartext create", error);
            }
	    /*
	     * Purge bad cleartext nm.  Never allow
	     * bad cleartext name to be cached.
	     */
	    mfs_clear_mark_name_purge(vp);
	    mfs_clear_rele(vp, MVFS_CD2CRED(cd));
	} else {
	    if (mnp->mn_hdr.realvp == NULL) {
                MVFS_CTXT_VN_DUP(vp, cvp);
		mnp->mn_hdr.realvp = cvp; /* cvp is held by the create above. */
#ifdef NFSV4_SHADOW_VNODE
                mnp->mn_hdr.realvp_master = NULL;
#endif
		mnp->mn_vob.cleartext.rwerr = 0;
		mnp->mn_vob.cleartext.purge_nm = 0;
  		mnp->mn_vob.cleartext.purge_cvp = 0;
  		mnp->mn_vob.cleartext.ostale_logged = 0;
  		mnp->mn_vob.cleartext.atime_pushed = 0;
                MVFS_RECORD_CREDLIST(mnp, TRUE, MVFS_CD2CRED(cd));
	    } else {
		CVN_RELE(cvp);
                cvp = NULL;
		BUMPSTAT(mfs_clearstat.clearcreatraces);
	    }
	    error = mvfs_clearattr(vp, NULL, MVFS_CD2CRED(cd)); /* Get clear attrs */
	    MVFS_BUMPTIME(stime, dtime, mfs_clearstat.clearcreat_time);

            /* Successful create (cvp is stored in realvp), return held vnode
            ** to caller if he wants it (and we didn't release it above) and we
            ** didn't get an error from mvfs_clearattr above.  Otherwise, we've
            ** already set *cvpp to NULL above (as part of our setup).
            */
            if ((cvpp != NULL) && (cvp != NULL) && (error == 0)) {
                CVN_HOLD(cvp);
                *cvpp = cvp;
            }
	}
    } else {
	/*
	 * Note: clnt_cltxt can return some TBS_ST_xxx errors.
	 * Slam user error to EIO so these don't get out to the
	 * user.
	 */
	mfs_clearperr(vp, "fetch cleartext create pathname", error);
	error = EIO;
    }

errout:
    MDB_XLOG((MDB_CLEAROPS,"create: vp=%"KS_FMT_PTR_T", cvp=%"KS_FMT_PTR_T", err=%d\n",vp,cvp,error));
    return (error);
}

/*
 * MFS_COPYVP - copy from one vnode to another
 */

int
mfs_copyvp(ovp, nvp, len, cred)
CLR_VNODE_T *ovp;
CLR_VNODE_T *nvp;
VATTR_SIZE_T len;
CRED_T *cred;
{
    int error;
    char *copybuf = NULL;
    MOFFSET_T off;
    MVFS_UIO_RESID_T nb;
    int ovp_open = 0;
    int nvp_open = 0;
    VATTR_T va;
    void *rdfp, *wrfp;
    struct uio uio;
    IOVEC_T iov;
    struct uio *uiop = &uio;

    uio.uio_iov = &iov;

    if ((copybuf = KMEM_ALLOC(mvfs_cowbufsiz, KM_SLEEP)) == NULL) {
	error = ENOMEM;
	goto errout;
    }
        
#ifdef NFSV4_SHADOW_VNODE
    /* NFSv4 Compliance: RATLC01011478: NFSv4 introduces the concept of shadow
     * vnodes. Refer mvfs_openv_ctx() and the CR for the considerations and 
     * changes in MVFS to handle shadow vnodes. 
     * During VOP_OPEN if NFSv4 replaces the shadow vnode with the master,
     * then there is a decrement in the reference count on the shadow vnode by 
     * one. This is an unexpected behaviour. Due to this decrement, the shadow 
     * vnode's reference count drops to zero by the end of choid operation.
     * In the subsequent call to mfs_copyvp() from mvfs_change_oid_subr(),
     * VOP_OPEN below will cause a panic in NFSv4. This is because an internal
     * data structure of NFSv4 (svnode in rnode4_t) corresponding to the shadow
     * vnode was deallocated when the v_count dropped to zero but was not
     * reinitialized during the subsequent lookup. This extra reference
     * on the shadow vnode will prevent the v_count from dropping to zero.
     */
    CVN_HOLD(ovp);
#endif

    error = MVOP_OPEN_KERNEL(&ovp, FREAD, cred, &rdfp);
    mvfs_logperr(MFS_LOG_ERR, error, "Copy-on-Write src open");
    if (error) goto errout;
    ovp_open = 1;

    error = MVOP_OPEN_KERNEL(&nvp, FWRITE|FTRUNC, cred, &wrfp);
    mvfs_logperr(MFS_LOG_ERR, error, "Copy-on-Write dest open");
    if (error) goto errout;
    nvp_open = 1;

    off = 0;
    while (len) {
	mfs_uioset(uiop, copybuf, mvfs_cowbufsiz, off, UIO_SYSSPACE);

	MVOP_RWRDLOCK(ovp, NULL);
	error = MVOP_READ_KERNEL(ovp, uiop, 0, NULL, cred, rdfp);
	MVOP_RWRDUNLOCK(ovp, NULL);

        mvfs_logperr(MFS_LOG_ERR, error, "Copy-on-Write src read");
	if (error) goto errout;
	nb = mvfs_cowbufsiz - uiop->uio_resid;
	ASSERT(nb <= mvfs_cowbufsiz);
	if (nb == 0) break;
	mfs_uioset(uiop, copybuf, nb, off, UIO_SYSSPACE);

	MVOP_RWWRLOCK(nvp, NULL);
	error = MVOP_WRITE_KERNEL(nvp, uiop, 0, NULL, cred, wrfp);
	MVOP_RWWRUNLOCK(nvp, NULL);

        mvfs_logperr(MFS_LOG_ERR, error, "Copy-on-Write dest write");
	if (error) goto errout;
        len -= nb;
        off += nb;
    }

errout:
    if (ovp_open) (void) MVOP_CLOSE_KERNEL(ovp, FREAD, MVFS_LASTCLOSE_COUNT,
				    (MOFFSET_T)0, cred, rdfp);
    if (nvp_open) (void) MVOP_CLOSE_KERNEL(nvp, FWRITE|FTRUNC, MVFS_LASTCLOSE_COUNT,
				    (MOFFSET_T)0, cred, wrfp);

#ifdef NFSV4_SHADOW_VNODE
    /* NFSv4 Compliance: RATLC01011478: As explained above if NFSv4 replaces
     * the shadow vnode with the master in VOP_OPEN then it releases one
     * reference on the shadow. After VOP_OPEN 'ovp' would be the master vnode.
     * CVN_RELE would release the reference on the master that was passed in. In 
     * the cases where the vnode was not replaced, this CVN_RELE takes care
     * of the extra reference above.
     */ 
    CVN_RELE(ovp);
#endif    

    if (copybuf)
	KMEM_FREE(copybuf, mvfs_cowbufsiz);
    return(error);
}

/* MFS_CLEAR_CLOSEVP - close cleartext vnode ptr */

STATIC void
mfs_clear_closevp(
    VNODE_T *vp,
    CRED_T *cred
)
{
	register struct mfs_mnode *mnp;
	CLR_VNODE_T *cvp;
	int	count;
	int	wcount;

	mnp = VTOM(vp);

	ASSERT(MISLOCKED(mnp));
	ASSERT(MFS_ISVOB(mnp));

        MDB_XLOG((MDB_CLEAROPS, "mfs_clear_closevp: vp=%lx called\n", vp));

	cvp = mnp->mn_hdr.realvp;
	wcount= mnp->mn_vob.open_wcount; 
	count = (mnp->mn_vob.open_count - wcount);


	while ( wcount--  > 0 ) {
		MVOP_CLOSE(cvp, FREAD|FWRITE, MVFS_LASTCLOSE_COUNT, 0, cred, NULL);
	}
	while ( count--  > 0 ) {
		MVOP_CLOSE(cvp, FREAD, MVFS_LASTCLOSE_COUNT, 0, cred, NULL );
	}
    return;
}

#ifndef MVFS_SYSTEM_KMEM
struct mvfs_slab_list *mvfs_cred_list_slabs;
#endif

ks_uint32_t
mvfs_hash_cred(CRED_T *cred)
{
    register ks_uint32_t rval;

    /* FIXME: this hash function needs testing under a realistic
       distribution of credentials to determine whether its
       distribution is suitable. */
    rval = (((ks_uint32_t) MDKI_CR_GET_UID(cred)) << 8) +
        (ks_uint32_t) MDKI_CR_GET_GID(cred);

    return rval % MVFS_CRED_HASHSZ;
}

void
mvfs_clear_release_mnode_credlist(struct mfs_mnode *mnp)
{
    ASSERT(MFS_ISVOB(mnp));

    MCILOCK(mnp);
    mvfs_clear_release_credlist(mnp->mn_vob.cleartext.ok_creds);
    mnp->mn_vob.cleartext.ok_creds = NULL;
    MCIUNLOCK(mnp);
    MDB_XLOG((MDB_CLEAROPS, "mnode %"KS_FMT_PTR_T" drop credlist\n", mnp));
}

STATIC void
mvfs_clear_release_credlist(mvfs_clr_creds_t *a_clist)
{
    mvfs_clr_creds_t *clist, *nclist;

    /* release the list of lookup creds */
    for (clist = a_clist; clist != NULL; clist = nclist)
    {
        if (clist->cred != NULL) {
            MDKI_CRFREE(clist->cred);
        }
        nclist = clist->next;
        MVFS_CLR_CRED_FREE((caddr_t) clist, sizeof(mvfs_clr_creds_t));
    }
}

STATIC CRED_T *
mvfs_find_cred(
    mvfs_clr_creds_t *aclist,
    CRED_T *cred
)
{
    mvfs_clr_creds_t *clist;
    for (clist = aclist; clist != NULL; clist = clist->next) {
        if (clist->cred == cred)
            return clist->cred;
        if (MDKI_CR_EQUAL(cred, clist->cred))
            /* FIXME: should we replace the cred on the list with this
               copy?  might make future checks quicker (since the
               pointers would be equal and the test would be quicker),
               but under some loads we might constantly be swapping
               pointers and references to no good effect (e.g. non-CC
               access, which generates new CRED_Ts on each RPC) */
            return clist->cred;
    }
    return NULL;
}

void
mvfs_record_cred(
    struct mfs_mnode *mnp,
    CRED_T *cred
)
{
    mvfs_clr_creds_t *clist, *sys_clist;
    CRED_T *fcred;
    ks_uint32_t crhash;
    SPL_T s;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if ((clist = (mvfs_clr_creds_t *)MVFS_CLR_CRED_ALLOC()) == NULL) {
        /* System resources are low so don't record this cred.  This
        ** should be OK.
        */
        MDB_XLOG((MDB_CLEAROPS,
                  "MVFS_CLR_CRED_ALLOC() failed for %"KS_FMT_PTR_T"\n",
                  cred));
        return;
    }
    crhash = MVFS_HASH_CRED(cred);

    SPLOCK(mcdp->cred.mvfs_sys_credlist_lock, s);
    /* Make sure we have a free cred list handy.  If not, get one and
        leave it ready in case of need.  This burns maybe a few cred
        lists (as many as can get added during a race in this code) to
        avoid a potential add/add race if we drop/reacquire the lock
        below to do an allocation. */

    if (mcdp->cred.mvfs_free_creds == NULL) {
        SPUNLOCK(mcdp->cred.mvfs_sys_credlist_lock, s);

        if ((sys_clist = (mvfs_clr_creds_t *)MVFS_CLR_CRED_ALLOC()) == NULL) {
            MDB_XLOG((MDB_CLEAROPS,
                      "MVFS_CLR_CRED_ALLOC() failed for sys "
                      "credlist cred=%"KS_FMT_PTR_T"\n",
                      cred));

            /* Clean up the previous memory allocation
            */
            MVFS_CLR_CRED_FREE((caddr_t)clist, sizeof(mvfs_clr_creds_t));
            return;
        }
        SPLOCK(mcdp->cred.mvfs_sys_credlist_lock, s);
        sys_clist->next = mcdp->cred.mvfs_free_creds;
        sys_clist->cred = NULL;
        mcdp->cred.mvfs_free_creds = sys_clist;
    }
    fcred = mvfs_find_cred(mcdp->cred.mvfs_sys_credlist[crhash], cred);
    if (fcred == NULL) {
        /* not found in system hash table, add it there as well as to the
           mnode chain */
        MDKI_CRHOLD(cred);              /* ref for clist, goes on mnp chain */
        clist->cred = cred;
        /* pull a clist from the free_creds list */
        sys_clist = mcdp->cred.mvfs_free_creds;
        mcdp->cred.mvfs_free_creds = sys_clist->next;

        MDKI_CRHOLD(cred);              /* ref for sys_clist, goes in bucket */
        sys_clist->cred = cred;
        sys_clist->next = mcdp->cred.mvfs_sys_credlist[crhash];
        mcdp->cred.mvfs_sys_credlist[crhash] = sys_clist;
        SPUNLOCK(mcdp->cred.mvfs_sys_credlist_lock, s);

#ifdef MVFS_DEBUG
        mcdp->cred.mvfs_sys_crlen[crhash]++;
#endif
    } else {
        /* found on system list, just hold & reference it in the mnode chain */
        MDKI_CRHOLD(fcred);
        SPUNLOCK(mcdp->cred.mvfs_sys_credlist_lock, s);
        clist->cred = fcred;
        MDB_XLOG((MDB_CLEAROPS, "found cred %"KS_FMT_PTR_T" on sys list as %"KS_FMT_PTR_T"\n",
                  cred, fcred));
    }
    MCILOCK(mnp);
    clist->next = mnp->mn_vob.cleartext.ok_creds;
    mnp->mn_vob.cleartext.ok_creds = clist;
    MCIUNLOCK(mnp);

    MDB_XLOG((MDB_CLEAROPS, "mnode %"KS_FMT_PTR_T" adding cred %"KS_FMT_PTR_T" as %"KS_FMT_PTR_T", %d/%d\n",
              mnp, cred, clist->cred,
              MDKI_CR_GET_UID(cred), MDKI_CR_GET_GID(cred)));
}

extern int
mvfs_clear_init(mvfs_cache_sizes_t *mma_sizes)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    INITSPLOCK(mcdp->cred.mvfs_sys_credlist_lock, "mvfs_sys_credlist_spl");
    if (mcdp->mvfs_ctxt_atime_refresh == 0)
        mcdp->mvfs_ctxt_atime_refresh = MVFS_CTXT_ATIME_REFRESH_DEF;
    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_CTXT_ATIME_REFRESH] = mcdp->mvfs_ctxt_atime_refresh;
    return 0;
}

extern void
mvfs_clear_free(void)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    mvfs_flush_credlists(TRUE);
    mvfs_clear_release_credlist(mcdp->cred.mvfs_free_creds);
    mcdp->cred.mvfs_free_creds = NULL;
    FREESPLOCK(mcdp->cred.mvfs_sys_credlist_lock);
}

/* FIXME: This is called periodically.  It's possible we would clean
   the system lists and later need to add a cred to a new mnode (when
   an equivalent cred is already on another mnode).  This means we end
   up with several cred references for the same creds, each attached
   to a different mnode with a "long" active lifetime.  Should we find
   a way to scan mnode cred lists and reap them too?  */

void
mvfs_flush_credlists(
    tbs_boolean_t force
)
{
    register int i;
    SPL_T s;
    static int periodic_threshold = MVFS_CREDLIST_FLUSH_INTERVAL;
    mvfs_clr_creds_t *clist;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if (!force && --periodic_threshold > 0)
        return;

    periodic_threshold = MVFS_CREDLIST_FLUSH_INTERVAL;

    for (i = 0; i < MVFS_CRED_HASHSZ; i++) {
        SPLOCK(mcdp->cred.mvfs_sys_credlist_lock, s);
        clist = mcdp->cred.mvfs_sys_credlist[i];
        mcdp->cred.mvfs_sys_credlist[i] = NULL;
#ifdef MVFS_DEBUG
        mcdp->cred.mvfs_sys_crlen[i] = 0;
#endif
        SPUNLOCK(mcdp->cred.mvfs_sys_credlist_lock, s);
        mvfs_clear_release_credlist(clist);
    }
}
static const char vnode_verid_mvfs_clearops_c[] = "$Id:  ede371a4.1def11e2.8579.00:01:84:c3:8a:52 $";
