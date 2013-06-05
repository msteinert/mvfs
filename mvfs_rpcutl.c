/* * (C) Copyright IBM Corporation 1991, 2012. */
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
/* mvfs_rpcutl.c */
#include "mvfs_systm.h"
#include "tbs_errno.h"
#include "mvfs.h"
#include <albd_rpc_kernel.h>

STATIC int
mvfs_find_ccs(int largeinit);

STATIC int MVFS_NOINLINE
mfscall_int(
    struct mfs_callinfo *trait,
    int op,
    XID_T *xidp,
    struct mfs_svr *svr,
    struct mfs_retryinfo *rinfo,
    xdrproc_t xdrargs,
    void *argsp,
    xdrproc_t xdrres,
    void *resp,
    CRED_T *cred,
    CLIENT *client,
    VNODE_T *view
);

STATIC void
mfs_clnt_free_int(
    CLIENT *,
    VNODE_T *
);

EXTERN int mfs_view_getstatus(P1(void *resp));
EXTERN XID_T mfs_view_getxid(P1(void *resp));
EXTERN void mfs_view_setxid(P1(void *req) PN(time_t bt) PN(XID_T xid));
EXTERN int mfs_albd_getstatus(P1(void *resp));
EXTERN XID_T mfs_albd_getxid(P1(void *resp));
EXTERN void mfs_albd_setxid(P1(void *req) PN(time_t bt) PN(XID_T xid));
STATIC ks_uint32_t mvfs_get_boottime(void);

#if defined(MVFS_COMMON_ALLOC_XID)
EXTERN ks_uint32_t mvfs_alloc_xid(void);
#endif

/* MVFS_CLNT_INIT - init client cache */
/* Note that while this function has code that will try to reduce the cache
 * size if memory is not available, the KM_ALLOC call is made with KM_SLEEP
 * set.  This means that we are more likely to just hang waiting for memory
 * if we are trying to oversubscribe memory.  The setcache code does use the
 * KM_NOSLEEP so that if someone is trying to tune their system, they will
 * get an error and not a hang.
 */

int
mvfs_clnt_init(mvfs_cache_sizes_t *mma_sizes)
{
    int old_size;
    int system_def_ccs;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    INITLOCK(&(mcdp->mvfs_rpc.mfs_client_lock), "mfscllk");

    /*
     * mvfs_client_cache_size must be large enough to prevent allocating
     * new rpchandles on the fly in order to have good performance.
     */
    mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_RPCHANDLES] = mcdp->mvfs_client_cache_size;
    system_def_ccs = mvfs_find_ccs(mcdp->mvfs_largeinit);

    MVFS_SIZE_DEFLOAD_NONZERO(mcdp->mvfs_client_cache_size, mma_sizes, RPCHANDLES,
                              system_def_ccs);

    while (TRUE) {
        old_size = mcdp->mvfs_client_cache_size * sizeof(client_cache_t);
        mcdp->mvfs_rpc.mvfs_client_cache = (client_cache_t *)
                KMEM_ALLOC(mcdp->mvfs_client_cache_size * sizeof(client_cache_t),
                KM_SLEEP);
        if (mcdp->mvfs_rpc.mvfs_client_cache != NULL) {
            /*
             * Success
             */
            BZERO(mcdp->mvfs_rpc.mvfs_client_cache, 
                  mcdp->mvfs_client_cache_size * sizeof(client_cache_t));
            mcdp->mvfs_rpc.mvfs_client_cache_family = AF_UNSPEC;
            break;

	/* 
	 * Size was too large, let's try a smaller value.
	 */
        } else if (mcdp->mvfs_client_cache_size > system_def_ccs) {
	    /* Try the system calculated default */
            mcdp->mvfs_client_cache_size = system_def_ccs;
        } else if (mcdp->mvfs_client_cache_size > CLIENT_CACHE_SIZE_SMALL) {
            /* Try minium */
            mcdp->mvfs_client_cache_size = CLIENT_CACHE_SIZE_SMALL;
        } else {
	    mvfs_log(MFS_LOG_WARN, "Failed to allocate %d bytes for caching of client handles\n", 
                     mcdp->mvfs_client_cache_size * sizeof(client_cache_t)); 
            mcdp->mvfs_client_cache_size = 0;
            return ENOMEM;
        }
	mvfs_log(MFS_LOG_WARN, "Failed to allocate %d bytes for Client handles lowering size to %d\n", 
                 old_size, mcdp->mvfs_client_cache_size * sizeof(client_cache_t));
    }
    return 0;
}

/* MVFS_CLNT_DESTROY - free up client cache */
void
mvfs_clnt_destroy()
{
    register int i;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    MVFS_LOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
    /* clean up the client handles. */
    for (i = 0; i < mcdp->mvfs_client_cache_size; i++) {
	ASSERT(!mcdp->mvfs_rpc.mvfs_client_cache[i].inuse);
	if (mcdp->mvfs_rpc.mvfs_client_cache[i].client)
	    mfs_clnt_free_int(mcdp->mvfs_rpc.mvfs_client_cache[i].client, NULL);
    }
    KMEM_FREE(mcdp->mvfs_rpc.mvfs_client_cache,
	      mcdp->mvfs_client_cache_size*sizeof(client_cache_t));
    mcdp->mvfs_rpc.mvfs_client_cache = NULL;	/* clean up any traces. */
    mcdp->mvfs_client_cache_size = 0;
    MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));

    FREELOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
    mcdp->mvfs_client_cache_size = mcdp->mvfs_init_sizes.size[MVFS_SETCACHE_RPCHANDLES];
}

/*
 * Allocate a different size for the RPC cache, if needed.
 * Note that this code will not sleep waiting for memory to prevent the
 * process from hanging if this request would oversubscribe memory.  The
 * initialization code does use KM_SLEEP.  The assumption is that the
 * value used at startup has been tuned and it is reasonable to wait for
 * the needed memory.
 * Any in-use client handles will get destroyed/freed when the using thread
 * returns the CLIENT * via a call to mfs_clnt_free().  [The client ptr will
 * not appear in any client_cache_t in the new array, so the handle will be
 * freed.] Any handles not in use must be freed.
 */

int
mvfs_rpc_setcaches(szp)
mvfs_cache_sizes_t *szp;
{
    register int i;
    client_cache_t *newcache;
    u_long newsize;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if (MVFS_SIZE_VALID(szp, RPCHANDLES) &&
         szp->size[MVFS_SETCACHE_RPCHANDLES] != mcdp->mvfs_client_cache_size)
    {
	newsize = szp->size[MVFS_SETCACHE_RPCHANDLES];
	newcache = (client_cache_t *) KMEM_ALLOC(newsize * sizeof(*newcache),
						 KM_NOSLEEP);
	if (newcache == NULL)
		return(ENOMEM);
	BZERO(newcache, newsize * sizeof(*newcache));
	MVFS_LOCK(&(mcdp->mvfs_rpc.mfs_client_lock));

        /*
         * We need to free any client handles not in use.
         */

	for (i = 0; i < mcdp->mvfs_client_cache_size; i++) {
	    if (mcdp->mvfs_rpc.mvfs_client_cache[i].client &&
	        !mcdp->mvfs_rpc.mvfs_client_cache[i].inuse)
	        mfs_clnt_free_int(mcdp->mvfs_rpc.mvfs_client_cache[i].client, NULL);
	}

	KMEM_FREE(mcdp->mvfs_rpc.mvfs_client_cache,
		  mcdp->mvfs_client_cache_size*sizeof(client_cache_t));
	mcdp->mvfs_client_cache_size = newsize;
	mcdp->mvfs_rpc.mvfs_client_cache_family = AF_UNSPEC;
	mcdp->mvfs_rpc.mvfs_client_cache = newcache;
	MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
    }
    return 0;
}

int
mvfs_rpc_getcaches(
    mvfs_cache_sizes_t *szp
)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    szp->size[MVFS_SETCACHE_RPCHANDLES] = mcdp->mvfs_client_cache_size;
    return 0;
}

int
mvfs_rpc_compute_caches(
    ks_int32_t scale_factor,
    mvfs_cache_sizes_t *szp
)
{
    if ((szp->mask & MVFS_CACHEBIT(RPCHANDLES)) == 0) {
        szp->size[MVFS_SETCACHE_RPCHANDLES] = mvfs_find_ccs(scale_factor);
        szp->mask |= MVFS_CACHEBIT(RPCHANDLES);
    }
    return 0;
}

int
mvfs_rpc_count(usage)
mvfs_cache_usage_t *usage;
{
    register int i, rval;
    client_cache_t *clientp;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    MVFS_LOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
    for (i=0, rval=0, clientp = mcdp->mvfs_rpc.mvfs_client_cache;
	 i < mcdp->mvfs_client_cache_size;
	 i++, clientp++) {
	if ((clientp->client != NULL))
	    rval++;
    }
    MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
    usage->cache_usage[MVFS_CACHE_INUSE][MVFS_CACHE_RPCHANDLES] = rval;
    usage->cache_usage[MVFS_CACHE_MAX][MVFS_CACHE_RPCHANDLES] = i;
    return 0;
}

/* MVFS_CLNT_GET - get a client handle for an MFS remote call */

int
mvfs_clnt_get(
    struct mfs_callinfo *trait,
    struct mfs_svr *svr,
    struct mfs_retryinfo *rinfo,
    CRED_T *cred,
    VNODE_T *view,
    CLIENT **client_p
)
{
    CLIENT *client = NULL;
    ks_uint32_t lboottime = 0;  /* Initialize to avoid a compiler warning. */
    int retrans;
    int error = 0;
    SPL_T s;
    int i;
    int found;
    int waited = 0;
    client_cache_t *clientp;
    client_cache_t *entryp = NULL;
    MDKI_CLNTKUDP_ADDR_T addr;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    BUMPSTAT(mfs_clntstat.clntget);
    if (view)
	BUMPVSTATV(view, clntstat.clntget);

    retrans = (svr->down) ? 1 : rinfo->retries;

    /* Allocate a handle. */

    if ((mcdp->mvfs_rpc.mvfs_client_cache_family == svr->addr.ks_ss_s.sa_family)) 
    {
        MVFS_LOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
        clientp = mcdp->mvfs_rpc.mvfs_client_cache;
        for (i=0; i<mcdp->mvfs_client_cache_size; i++) {
	    if ((clientp->client != NULL) &&
	        (clientp->proto == trait->proto) &&
	        (clientp->version == trait->version) &&
	        (!clientp->inuse)) {
	         clientp->inuse = 1;
	         clientp->used++;
	         client = clientp->client;

                 clientp->boottime = mvfs_get_boottime();

                 /* We should find a way to ASSERT the client handle's proto/version
                    match the cached records and the requested traits */
	         error = MDKI_CLNTKUDP_INIT(client, &svr->addr.ks_ss_s, retrans, cred,
                                            (!rinfo->nointr), &addr, trait->proto,
                                            trait->version, &svr->knc);
	         if (error != 0) {
	             MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
	             return error;
	         }
	         MDKI_CLNTKUDP_INTR(client, !rinfo->nointr);
	         break;
	    }
	    clientp++;
        }
        MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
    }
    if (!client) {
getclient:
	error = MDKI_CLNTKUDP_CREATE(&svr->knc, &svr->addr.ks_ss_s, trait,
                                     retrans, (!rinfo->nointr), cred, &client);
        /*
         * Compute the boottime to go with this CLIENT handle.
         */
        lboottime = mvfs_get_boottime();
	switch (error) {
          case 0:
            break;

          case EAFNOSUPPORT:
          case EPFNOSUPPORT:
            /* don't log those errors, and don't retry--just fail immediately */
            *client_p = NULL;
            return error;

          default:
	    if (waited == 0) {
		mvfs_log(MFS_LOG_WARN,
			 "cannot allocate RPC client handle, retrying (err %d)\n", error);
		MDKI_USECDELAY(1000000*5);	/* wait 5 sec */
		waited++;
		goto getclient;
	    }
            /* Fall through */
            /* If normal errors have retried and failed again, they will 
             * fall through to this case.  If the error is ERESTART 
             * (or ERESTARTSYS on Linux) then it means that we have 
             * received a signal.  We will not retry, just log the event
             * and get out.
             * It turns out that HP-UX does not have ERESTART, so be
             * careful with this code, the cases below may not exist
             * so this would be a bad place for a break statement.
             */
#ifdef ERESTART
          case ERESTART:
#endif
#ifdef ERESTARTSYS
          case ERESTARTSYS:
#endif
	    mvfs_log(MFS_LOG_ERR,
		     "cannot allocate RPC client handle, giving up (err %d)\n", error);
            *client_p = NULL;		/* This is bad! */
            return error;
	}
	BUMPSTAT(mfs_clntstat.clntcreate);
	if (view)
	    BUMPVSTATV(view, clntstat.clntcreate);
	found = 0;
        if ((mcdp->mvfs_rpc.mvfs_client_cache_family == 
              svr->addr.ks_ss_s.sa_family) ||
            (mcdp->mvfs_rpc.mvfs_client_cache_family ==  
              AF_UNSPEC))
        { 
	    MVFS_LOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
	    clientp = mcdp->mvfs_rpc.mvfs_client_cache;
	    for (i=0; i<mcdp->mvfs_client_cache_size; i++) {
	         if (!clientp->client) {
                     if (mcdp->mvfs_rpc.mvfs_client_cache_family == AF_UNSPEC) 
                     { 
                         mcdp->mvfs_rpc.mvfs_client_cache_family = 
                           svr->addr.ks_ss_s.sa_family;
                     }
		     clientp->inuse = 1;
		     clientp->client = client;
                     clientp->boottime = lboottime;
		     clientp->proto = trait->proto;
		     clientp->version = trait->version;
		     clientp->used++;
		     found = 1;
		     break;
	         }
	         if (!entryp && !clientp->inuse)
		     entryp = clientp;
	         clientp++;
	     }
	     if (!found) {
	         if (entryp) {
		     entryp->inuse = 1;
		     mfs_clnt_free_int(entryp->client, view);
		     entryp->client = client;
                     entryp->boottime = lboottime;
		     entryp->proto = trait->proto;
		     entryp->version = trait->version;
		     entryp->used++;
	         }
	     }
	     MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
        }
    }    

    if (!MDKI_CLNT_AUTH_VALID(client))
	MDKI_PANIC("mfs_clnt_alloc: null auth");

    *client_p = client;
    return 0;
}

/* MVFS_CLNT_FREE - free up an allocated client handle */

void
mvfs_clnt_free(client, error, view)
CLIENT *client;
int error;
VNODE_T *view;
{
    SPL_T s;
    int i;
    client_cache_t *clientp;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    BUMPSTAT(mfs_clntstat.clntfree);
    if (view)
	BUMPVSTATV(view, clntstat.clntfree);

    /*
     * Always free the client resources
     * because if we don't find it in the table
     * we will destroy it.
     */
    MDKI_CLNTKUDP_FREE(client);

    MVFS_LOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
    clientp = mcdp->mvfs_rpc.mvfs_client_cache;
    for (i=0; i<mcdp->mvfs_client_cache_size; i++) {
	if (clientp->client == client) {
            ASSERT(clientp->inuse);
	    clientp->inuse = 0;
	    if (!error) {
                ASSERT(MDKI_CLNT_AUTH_VALID(client));
		MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));
		return;
	    }
	    clientp->client = NULL;
	    break;
	}
	clientp++;
    }
    MVFS_UNLOCK(&(mcdp->mvfs_rpc.mfs_client_lock));

    mfs_clnt_free_int(client, view);
}

/* MFS_CLNT_FREE_INT - free up an allocated client handle */

STATIC void
mfs_clnt_free_int(client, view)
CLIENT *client;
VNODE_T *view;
{

    MDKI_CLNTKUDP_DESTROY(client);
    BUMPSTAT(mfs_clntstat.clntdestroy);
    if (view)
        BUMPVSTATV(view, clntstat.clntdestroy);
}

/*
 * Find the appropriate default setting for mvfs_client_cache_size based on 
 * mvfs_largeinit
 */
STATIC int
mvfs_find_ccs(int largeinit)
{
    int ccs;

    /*
     * For large-memory MP systems mvfs_client_cache_size needs to grow rapidly.
     * But we don't want to change the number allocated by default on smaller 
     * systems (defined by mvfs_largeinit <= 2).
     */
    if ((largeinit >= 0) && (largeinit <= 2)) {
	ccs = (largeinit + 1 ) * CLIENT_CACHE_SIZE_SMALL;
    } else {
	/* For larger systems, increase it by default */
	ccs = (largeinit + 1) * CLIENT_CACHE_SIZE_LARGE;
	/* But don't automatically set it too large */
	if (ccs >  CLIENT_CACHE_SIZE_AUTOMAX)
	     ccs = CLIENT_CACHE_SIZE_AUTOMAX;
    }
    return (ccs);
}

/*
 * Routines to fetch/set pkt parameters
 */

int
mfs_view_getstatus(resp)
void *resp;
{
    return (((view_hdr_reply_t *)resp)->status);
}

XID_T
mfs_view_getxid(resp)
void *resp;
{
    return(((view_hdr_reply_t *)resp)->xid);
}

void
mfs_view_setxid(req, bt, xid)
void *req;
time_t bt;
XID_T xid;
{
    ((view_hdr_req_t *)req)->boot_time = bt;
    ((view_hdr_req_t *)req)->xid = xid;
}

int
mfs_albd_getstatus(resp)
void *resp;
{
    return (((albd_hdr_reply_t *)resp)->status);
}

XID_T
mfs_albd_getxid(resp)
void *resp;
{
    return(((albd_hdr_reply_t *)resp)->xid);
}

void
mfs_albd_setxid(req, bt, xid)
void *req;
time_t bt;
XID_T xid;
{
    ((albd_hdr_req_t *)req)->boot_time = bt;
    ((albd_hdr_req_t *)req)->xid = xid;
}

/* Routine to initialize or get current boottime */
STATIC ks_uint32_t
mvfs_get_boottime(void)
{
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

    if (mcdp->mvfs_boottime == 0) {
        mcdp->mvfs_boottime = MDKI_CTIME();
    }

    return(mcdp->mvfs_boottime);
}

#if defined(MVFS_COMMON_ALLOC_XID)
/* Routine to allocate unique XID for RPCs between MVFS and CC servers */

ks_uint32_t 
mvfs_alloc_xid(void)
{
    ks_uint32_t xid = 0;
    mvfs_common_data_t *mcdp = MDKI_COMMON_GET_DATAP();

retry:
    if (MDKI_ATOMIC_CAS_UINT32(&(mcdp->mvfs_xid), MVFS_XID_ULIMIT, 0)) {
        mcdp->mvfs_boottime = MDKI_CTIME();
        return((ks_uint32_t)MDKI_ATOMIC_INCR_UINT32_NV(&(mcdp->mvfs_xid)));
    } else {
        xid = MDKI_ATOMIC_READ_UINT32(&(mcdp->mvfs_xid));
        if ((xid != MVFS_XID_ULIMIT) &&
            MDKI_ATOMIC_CAS_UINT32(&(mcdp->mvfs_xid), xid, xid + 1)) {
            return(xid + 1);
        } else {
            goto retry;
        }
    }
}
#endif

/*
 * Maximum timeout in 10'ths of seconds.
 * BACKOFF backs off by powers of two but is clamped to the max
 * Issues:
 *	Max should be large enough so longest transaction not
 *	a problem.
 *	Max should not be so long that the user gets impatient waiting
 *	for a "quit" to take effect.
 *
 * Protocol Notes:
 *	The protocol for detecting duplicates at the server is to
 *	compare the XID with the latest XID serviced for that same
 *	socket number.  The server only keeps a copy of the last
 *	request serviced to reply on a duplicate pkt.  Therefore,
 *	It is imperative that for a given socket
 *	(e.g. client handle), that XID's always monotonically
 *	increase and that old transactions are completed before a new
 *	one is started. This is done by always grabbing the socket first
 *	before allocating the XID (XID's are monotonically increasing
 *	per machine in the order they are grabbed).  The same socket
 *	must be held around until the call either succeeds or
 * 	failure is assumed.  Grabbing the socket first prevents
 *	any scheduling/locking anomalies from causing two processes to get
 *	XID's in one order, and then use them in a different order
 *	to the server.
 */
#define MVFS_MAXTIME	300
#define BACKOFF(tim) ((((tim) << 1) > MVFS_MAXTIME) ? MVFS_MAXTIME : ((tim) << 1))

/* VIEW SERVER TABLES */

/* Base time shift for each call.  This allows the user
   to specify one timeout value for the server, and adjusts
   for those calls we know are more timeconsuming.  Too short
   a timeout for "long" calls would result in unnecessary retries. 
   A '*' next to the opname in the comment field indicates that
   this op is not currently used by the MFS */

int mfs_view_timeshft[VIEW_NUM_PROCS] = {
    0,	/* NULL */
    0,	/* 001: VIEW_CONTACT* */
    0,	/* 002: VIEW_SERVER_EXIT* */
    0,	/* 003: VIEW_SETATTR */
    0,	/* 004: VIEW_CREATE */
    0,	/* 005: VIEW_REMOVE */
    0,	/* 006: VIEW_RENAME */
    0,	/* 007: VIEW_SYMLINK */
    0,	/* 008: VIEW_MKDIR */
    0,	/* 009: VIEW_RMDIR */
    0,	/* 010: VIEW_READDIR */
    0,	/* 011: VIEW_STATFS */
    2,	/* 012: VIEW_CLTXT */
    1,	/* 013: VIEW_CHANGE_OID */
    0,	/* 014: VIEW_READDIR_EXT */
    1,	/* 015: VIEW_GPATH */
    0,	/* 016: VIEW_REVALIDATE */
    0,	/* 017: VIEW_CLTXT_PNAME* */
    1,	/* 018: VIEW_CHANGE_MTYPE */
    0,	/* 019: VIEW_INVALIDATE_UUID */
    0,	/* 020: VIEW_LINK */
    0,	/* 021: VIEW_LOOKUP */
    0,	/* 022: VIEW_GETATTR */
    1,	/* 023: VIEW_REPLICA_ROOT */
    0,	/* 024: VIEW_LOOKUP_EXT* */
    0,	/* 025: VIEW_CREATE_CONTAINER* */
    0,	/* 026: VIEW_REMOVE_CONTAINER* */
    0,	/* 027: VIEW_RENAME_CONTAINER* */
    0,	/* 028: VIEW_WINK* */
    0,	/* 029: VIEW_READLINK */
    0	/* 030- : Others* */
};

char *mfs_viewopnames[VIEW_NUM_PROCS] = {
    "nil",
    "contact",		/* 001: */
    "srv_exit",		/* 002: */
    "setattr",		/* 003: MVFS */
    "create",		/* 004: MVFS */
    "remove",		/* 005: MVFS */
    "rename",		/* 006: MVFS */
    "symlink",		/* 007: MVFS */
    "mkdir",		/* 008: MVFS */
    "rmdir",		/* 009: MVFS */
    "readdir",		/* 010: MVFS */
    "null",		/* 011: */
    "getcleartext",	/* 012: MVFS */
    "choid",		/* 013: MVFS */
    "rddir_ext",	/* 014: MVFS */
    "gpath",		/* 015: MVFS */
    "revalidate",	/* 016: MVFS */
    "cltxt_pname",	/* 017: */
    "change_mtype",	/* 018: MVFS */
    "invalidate",	/* 019: MVFS */
    "link",		/* 020: MVFS */
    "lookup_v6",	/* 021: MVFS */
    "getattr",		/* 022: MVFS */
    "replica_root",	/* 023: MVFS */
    "lookup_ext_v6",	/* 024: */
    "create_cntr",	/* 025: */
    "remove_cntr",	/* 026: */
    "rename_cntr",	/* 027: */
    "wink",		/* 028: */
    "readlink",		/* 029: MVFS */
    "view_reload_spec", /* 030: */
    "view_bld_session_free",        /* 031: */
    "view_events_get_vob",          /* 032: */
    "view_vob_create",              /* 033: */
    "view_find_oid",                /* 034: */
    "view_sec_rule",                /* 035: */
    "view_white_out",               /* 036: */
    "view_unwhite_out",             /* 037: */
    "view_bld_get_ref_tim",         /* 038: */
    "view_recover",                 /* 039: */
    "view_dump",                    /* 040: */
    "view_load",                    /* 041: */
    "view_cr_get",                  /* 042: */
    "view_do_promote",              /* 043: */
    "view_do_purge_cltxt",          /* 044: */
    "view_vob_save",                /* 045: */
    "view_bld_session_flags",       /* 046: */
    "view_get_text_mode",           /* 047: */
    "view_cr_add_vob_ref_uuid",     /* 048: */
    "view_cr_rm_vob_ref_uuid",      /* 049: */
    "view_vob_get_path_uuid",       /* 050: */
    "view_vob_rm_path_uuid",        /* 051: */
    "view_vob_recover_obj_uuid",    /* 052: */
    "view_cr_save",                 /* 053: */
    "view_getprop",                 /* 054: */
    "view_setprop",                 /* 055: */
    "view_getlic",                  /* 056: */
    "view_inventory_uuids",         /* 057: */
    "view_setwork_get_vobs",        /* 058: */
    "view_server_exit_rmtag",       /* 059: */
    "view_statistics",              /* 060: */
    "view_set_values",              /* 061: */
    "view_inventory_vobs_uuids",    /* 062: */
    "view_setwork_set_vobs",        /* 063: */
    "view_setwork_cleanup",         /* 064: */
    "view_hlink_set_vobs",          /* 065: */
    "view_hlink_cleanup",           /* 066: */
    "view_hlink_get_vobs",          /* 067: */
    "view_ws_create_db",            /* 068: */
    "view_frz_get_scopes",          /* 069: */
    "view_frz_get_freeze_state",    /* 070: */
    "view_frz_get_reload_info",     /* 071: */
    "view_ws_begin_load_sess",      /* 072: */
    "view_ws_end_load_sess",        /* 073: */
    "view_frx_ok_to_set_spec",      /* 074: */
    "view_ws_upd_wso_attrs",        /* 075: */
    "view_ws_get_scope_aliases",    /* 076: */
    "view_ws_get_unvisited_wsos",   /* 077: */
    "view_ws_invalidate_obj",       /* 078: */
    "view_ws_unload_one_obj",       /* 079: */
    "view_frz_get_num_frozen",      /* 080: */
    "view_ws_get_obj_scopes",       /* 081: */
    "view_ws_rename_obj",           /* 082: */
    "view_ws_is_frz_obj",           /* 083: */
    "view_reload_spec_ext",         /* 084: */
    "view_ws_is_mod_wso_ext",       /* 085: */
    "view_protect_stg_as_clnt",     /* 086: */
    "view_protect_stg_check",       /* 087: */
    "view_unprotect_stg_as_clnt",   /* 088: */
    "view_unprotect_stg_check",     /* 089: */
    "view_reparent_vob",            /* 090: */
    "view_getown_sid",              /* 091: */
    "view_ws_load_one_obj",         /* 092: */
    "view_ws_load_one_slink",       /* 093: */
    "view_get_config_spec",         /* 094: */
    "view_set_config_spec",         /* 095: */
    "view_replace_container",       /* 096: */
    "view_protect_container",       /* 097: */
    "view_fstat_container",         /* 098: */
    "view_ws_unload_one_obj_ext",   /* 099: */
    "view_lookup",                  /* 100: MVFS */
    "view_lookup_ext",              /* 101: */
    "view_dos_to_unsharable",       /* 102: */
    "view_get_dos",                 /* 103: */
    "view_dos_found"                /* 104: */
};

int mfs_viewopmax = VIEW_NUM_PROCS;

/* 
 * Keep histogram of RPC delays for following.
 * Note that separate histogram is kept for
 * cleartext fetch RPC which we expect might take a long time.
 * This statically initialized structure is used only as template
 * for quick initialization of corresponding structure in mvfs_statistics_data.
 */

struct mfs_rpchist mvfs_init_viewophist = {
    { 
	{ 0,  50000000 },		/* .05 secs */
	{ 0, 100000000 },		/* .1 secs */
	{ 0, 250000000 },		/* .25 secs */
	{ 0, 500000000 },		/* .5 secs */
	{ 1, 0 },			/* 1 sec */
	{ 2, 0 },			/* 2 sec */
	{ 3, 0 },			/* 3 sec */
	{ 4, 0 },			/* 4 sec */
	{ 8, 0 }, 			/* 8 sec */
	{ 16, 0 },			/* 16 sec */
	{ 24, 0 },			/* 24 sec */
	{ 32, 0 },			/* 32 sec */
	{ 40, 0 },			/* 40 sec */
	{ 48, 0 },			/* 48 sec */
	{ 64, 0 },			/* 64 sec */
	{ 0x7fffffff, 0 },		/* Many moons */
    },
    {0},
    {0},
    { {0} },
    MFS_RPCHIST_VERS
};

/* ALBD tables */

int mfs_albd_timeshft[ALBD_NUM_PROCS] = {
	0,		/* null */
	0, 0, 0, 0,	/* contact,  register,  find,      idle */
	0, 0, 0, 0, 	/* busy,     hostaddr,	localpath, licensechk */
	0, 0, 0, 0, 	/* stats,    revoke,	schedinfo, rembuild */
	0, 0, 0, 0, 	/* rgy_getid, rgy_findbystr, rgy_findbyuuid, rgy_get */
	0, 0, 0, 0, 	/* rgy_add,  rgy_rem,	svr_alt_uuid, rgy_chk_access */
	0, 	 	/* rgy_get_dtm */
};

char *mfs_albdopnames[ALBD_NUM_PROCS] = {
	"nil",
	"contact", "register", "find_svr", "svridle",
	"svrbusy", "host2addr", "localpath", "licensechk",
	"licstats", "licrevoke", "schedinfo", "rembuild",
	"rgy_getid", "rgy_findbystr", "rgy_findbyuuid", "rgy_get",
	"rgy_add", "rgy_remove", "svr_alt_uuid", "rgy_chk_access",
	"rgy_get_dtm",
};

/* Trait definitions */
struct mfs_callinfo mfs_vwcallstruct = {
	VIEW_SERVER, VIEW_SERVER_VERS,
	"View", 
	mfs_viewopnames, 
	mfs_view_timeshft,
	mfs_view_getstatus, 
	mfs_view_getxid,
	mfs_view_setxid,
};

struct mfs_callinfo mfs_albdcallstruct = {
	ALBD_SERVER, ALBD_SERVER_VERS,
	"Albd",
	mfs_albdopnames,
	mfs_albd_timeshft,
	mfs_albd_getstatus,
	mfs_albd_getxid,
	mfs_albd_setxid,
};

struct mfs_callinfo *mfs_viewcall = &mfs_vwcallstruct;
struct mfs_callinfo *mfs_albdcall = &mfs_albdcallstruct;

/*
 * MVFS_REBINDSVR_PORT - rebind a server's port number
 */
int
mvfs_bindsvr_port(
    struct mfs_svr *svr,
    VFS_T *vfsp,
    CRED_T *cred,
    VNODE_T *vw
)
{
    /* Declare a type so we can do one allocation to save stack space. */
    struct {
        struct mfs_svr albd_svr;
        struct mfs_retryinfo albd_retry;
        albd_find_server_req_t ra;
        albd_find_server_v70_reply_t rr_v70;
        albd_find_server_reply_t rr;
    } *alloc_unitp;
    struct mfs_svr *albd_svrp;
    struct mfs_retryinfo *albd_retryp;
    albd_find_server_req_t *rap;
    albd_find_server_v70_reply_t *rrp_v70;
    albd_find_server_reply_t *rrp;
    mvfs_viewroot_data_t *vrdp = MDKI_VIEWROOT_GET_DATAP();
    u_int num_ports;
    short real_family;
    int error;

    /* Must have viewroot vfs for ALBD port number */
    if ((vrdp->mfs_viewroot_vfsp) == NULL) return(ECONNREFUSED);

    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    albd_svrp = &(alloc_unitp->albd_svr);
    albd_retryp = &(alloc_unitp->albd_retry);
    rap = &(alloc_unitp->ra);
    rrp_v70 = &(alloc_unitp->rr_v70);
    rrp = &(alloc_unitp->rr);

    albd_svrp->down = 0;
    albd_svrp->dprinted = albd_svrp->uprinted = 0;
    albd_svrp->svrbound = 1;
    albd_svrp->addr = svr->addr;		    /* View's host addr */
    /* We just copied the whole address above, including the family.  Now we're
    ** going to set the port for the ALBD server from the mount info and assume
    ** the rest of the address is OK.  Remember, in mfs_vmount_subr() we
    ** "misused" the viewroot mmi_svr.addr to save the ALBD port in the IPv4
    ** port field (and we assume the port is the same for IPv4 and IPv6).
    */
    switch (albd_svrp->addr.ks_ss_s.sa_family) {
      case AF_INET:
        albd_svrp->addr.ks_ss_sin4.sin_port = 
            VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_svr.addr.ks_ss_sin4.sin_port;
        break;

      case AF_INET6:
        albd_svrp->addr.ks_ss_sin6.sin6_port = 
            VFS_TO_MMI(vrdp->mfs_viewroot_vfsp)->mmi_svr.addr.ks_ss_sin4.sin_port;
        break;

      default:
        error = EPFNOSUPPORT;
        goto cleanup;
    }
    MFS_INIT_STRBUFPN_PAIR_IN(&(albd_svrp->lpn), &("albd"[0]), &("albd"[0]));
    albd_svrp->host = svr->host;
    albd_svrp->rpn = "albd";
    albd_svrp->uuid = TBS_UUID_NULL;

    if (vfsp != NULL) {
        *albd_retryp = VFS_TO_MMI(vfsp)->mmi_retry;/* Retry params from VOB */
    } else {
        /* try just once; this branch is for mfs_clnt_inval() */
        albd_retryp->soft = 1;
        albd_retryp->timeo = MFSMNT_TIMEO_DEFAULT;
        albd_retryp->retries = MFSMNT_RETRANS_DEFAULT;
        albd_retryp->nointr = 0;
        albd_retryp->mbz = 0;
    }
    albd_retryp->rebind = 0;			/* But no rebind */

    /* Call the albd to ask for the server's port */
    rap->hdr.xid = (u_long)MDKI_ALLOC_XID();
    rap->rpc_trait.rpc_prog = VIEW_SERVER;
    rap->rpc_trait.rpc_ver = VIEW_SERVER_VERS;
    rap->rpc_trait.protocol = ALBD_PROTOCOL_UDP;
    rap->uuid = svr->uuid;
    rap->path = svr->rpn;
    if ((rrp->path = KMEM_ALLOC(MAXPATHLEN, KM_SLEEP)) == NULL) {
        error = ENOMEM;
        goto cleanup;
    }
    if ((rrp_v70->path = KMEM_ALLOC(MAXPATHLEN, KM_SLEEP)) == NULL) {
        KMEM_FREE(rrp->path, MAXPATHLEN);
        error = ENOMEM;
        goto cleanup;
    }

    /*  Try the new ALBD_FIND_SERVER RPC call first.
     *  If it works, then great...that means the server is up-to-date with IPv6
     *  and it will return an albd_server_port_list_t with multiple IPv4 and/or IPv6
     *  ports.
     *  If it does not work, then we must fall back to the old ALBD_FIND_SERVER_V70
     *  call that returns only an IPv4 address.
     */
    error = mfscall(mfs_albdcall, ALBD_FIND_SERVER, 0, 
                    albd_svrp, albd_retryp,
                    (xdrproc_t) xdr_albd_find_server_req_t, (caddr_t)rap,
                    (xdrproc_t) xdr_albd_find_server_reply_t, (caddr_t)rrp,
                    cred, NULL);
    if (!error)
        error = mfs_geterrno(rrp->hdr.status); 
    if (!error) {
        /* 
         * We do this check to handle IPv4-mapped IPv6 addresses.
         * If we need the port for an IPv4-mapped IPv6 address, then 
         * we grab the IPv4 port returned from the RPC.
         */
        if ((svr->addr.ks_ss_s.sa_family == AF_INET6) &&
            (IN6_IS_ADDR_V4MAPPED(&svr->addr.ks_ss_sin6.sin6_addr))) {
            real_family = AF_INET;
        } else {
            real_family = svr->addr.ks_ss_s.sa_family;
        }
        for (num_ports = 0; num_ports < rrp->port_list.num_ports; num_ports++) {
             if (rrp->port_list.ports[num_ports].af ==
                 real_family) {
                 switch (svr->addr.ks_ss_s.sa_family) {
		   case AF_INET:
		         svr->addr.ks_ss_sin4.sin_port =
		         htons((u_short)rrp->port_list.ports[num_ports].port);
		         break;
		   case AF_INET6:
		         svr->addr.ks_ss_sin6.sin6_port =
		         htons((u_short)rrp->port_list.ports[num_ports].port);
		         break;
		   default:
		         error = EPFNOSUPPORT;
			 goto cleanup;
                 }
                 svr->svrbound = 1;
             }
        }
        if (!svr->svrbound)
            error = EPFNOSUPPORT; 

        MDB_XLOG((MDB_ALBDOPS, "rebindsvr_port: %s:%s -> port %d, error=%d\n", 
	         svr->host, svr->rpn, rrp->port_list.ports[num_ports], error));
    }
    else {
        /* 
         * We must check for AF_INET because the old RPC does not support IPv6. 
         * We do allow IPv4-mapped IPv6 addresses with the old RPC though,
         * since we would then need the IPv4 port returned from the RPC.
         */
        if ((svr->addr.ks_ss_s.sa_family != AF_INET) &&
            (!((svr->addr.ks_ss_s.sa_family == AF_INET6) &&
               (IN6_IS_ADDR_V4MAPPED(&svr->addr.ks_ss_sin6.sin6_addr)))))  {
            error = EPFNOSUPPORT;
            goto cleanup;
        }
        error = mfscall(mfs_albdcall, ALBD_FIND_SERVER_V70, 0, 
                        albd_svrp, albd_retryp,
                        (xdrproc_t) xdr_albd_find_server_req_t, (caddr_t)rap,
                        (xdrproc_t) xdr_albd_find_server_v70_reply_t, (caddr_t)rrp_v70,
                        cred, NULL);
        if (!error)
	    error = mfs_geterrno(rrp_v70->hdr.status);
        if (!error) {
            ASSERT(((struct sockaddr *)&rrp_v70->saddr)->sa_family == AF_INET);
            switch (svr->addr.ks_ss_s.sa_family) {
              case AF_INET:
                    svr->addr.ks_ss_sin4.sin_port = rrp_v70->saddr.sin_port;
                    break;
              case AF_INET6:
                    svr->addr.ks_ss_sin6.sin6_port = rrp_v70->saddr.sin_port;
                    break;
              default:
                    error = EPFNOSUPPORT;
                    goto cleanup;
            }
            svr->svrbound = 1;
        }
        MDB_XLOG((MDB_ALBDOPS, "rebindsvr_port: %s:%s -> port %d, error=%d\n", 
	         svr->host, svr->rpn, rrp_v70->saddr.sin_port, error));
    }

  cleanup:
    if (rrp->path) KMEM_FREE(rrp->path, MAXPATHLEN);
    if (rrp_v70->path) KMEM_FREE(rrp_v70->path, MAXPATHLEN);
    if (alloc_unitp) KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
    return(error);
}

/*
 * MFS_VWCALL - make an rpc call to the view
 */
int
mfs_vwcall(
    VNODE_T *vw,
    VFS_T *vfsp,
    int op,
    xdrproc_t xdrargs,
    void *argsp,
    xdrproc_t xdrres,
    void *resp,
    CRED_T *cred
)
{
    int error, user_error, callerr;
    XID_T xid;
    struct mfs_retryinfo *rinfop;
    int pri;
    int status;
    int retrans;
    int suppress_console_msg;
    MDKI_CLNTKUDP_ADDR_T addr;
    static MVFS_PROCID_T suppress_last_pid = 0;

    /* Declare a type so we can do one allocation to save stack space. */
    struct {
        mfs_mnode_t *mnp;
        CLIENT *client;
    } *alloc_unitp;

    /* Copy over retry info */

    if ((rinfop = KMEM_ALLOC(sizeof(*rinfop), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }
    *rinfop = VFS_TO_MMI(vfsp)->mmi_retry;
    rinfop->rebind = 1;		/* Always support rebinding for view calls */

    /* Allocate the vars we need to save stack space.  Don't return without
    ** freeing after this (i.e. return through the cleanup: label).
    */
    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        KMEM_FREE(rinfop, sizeof(*rinfop));
        return(ENOMEM);
    }
    alloc_unitp->mnp = VTOM(vw);

    if (alloc_unitp->mnp->mn_view.rpctime + mvfs_view_rebind_timeout < MDKI_CTIME()) {
        /* probe ALBD first */
        error = mvfs_bindsvr_port(&alloc_unitp->mnp->mn_view.svr, vfsp, cred, vw);
        if (error) {
            goto cleanup;
        }
    }

    error = mvfs_clnt_get(mfs_viewcall, &alloc_unitp->mnp->mn_view.svr,
                          rinfop, cred, vw, &alloc_unitp->client);
    if (error != 0) {
	/* oh boy, this really bites... */
        goto cleanup;
    }
    xid = (XID_T)MDKI_ALLOC_XID();  /* Allocate an XID we can keep */

    while ((callerr = error = mfscall_int(mfs_viewcall, op, &xid,
			&alloc_unitp->mnp->mn_view.svr, rinfop,  xdrargs, argsp,
			xdrres, resp, cred, alloc_unitp->client, vw)) == EAGAIN) {

	error = mvfs_bindsvr_port(&VTOM(vw)->mn_view.svr, vfsp, cred, vw);

	if (error) {
            if (rinfop->soft) {
	        mvfs_log(MFS_LOG_ERR,
			 "View op %s failed rebind for %s:%s%s\n",
			    mfs_viewopnames[op] ? mfs_viewopnames[op] : "unknown operation",
			    alloc_unitp->mnp->mn_view.svr.host,
			    alloc_unitp->mnp->mn_view.svr.rpn,
			    mfs_strerr(error));
	        UPRINTF(("\nView op %s failed rebind for %s:%s%s \n",
			mfs_viewopnames[op] ? mfs_viewopnames[op] : "unknown operation",
			alloc_unitp->mnp->mn_view.svr.host,
			alloc_unitp->mnp->mn_view.svr.rpn,
			mfs_strerr(error)));
	        break;
	    }
	}
	/* Only 1 rebind attempt for soft mounts and restore orig timeout */
	if (rinfop->soft) {
	    rinfop->rebind = 0;
	    rinfop->timeo = VFS_TO_MMI(vfsp)->mmi_retry.timeo;
	}

        retrans = (alloc_unitp->mnp->mn_view.svr.down) ? 1 : rinfop->retries;

        /* Free client creds */
        MDKI_CLNTKUDP_FREE(alloc_unitp->client);
        /* Re-initialize the client handle */
        error = MDKI_CLNTKUDP_INIT(alloc_unitp->client, 
                                   &alloc_unitp->mnp->mn_view.svr.addr.ks_ss_s,
                                   retrans, cred, (!rinfop->nointr), &addr, 
                                   mfs_viewcall->proto, mfs_viewcall->version,
                                   &alloc_unitp->mnp->mn_view.svr.knc);

        mvfs_log(MFS_LOG_DEBUG, 
                 "mvfs vw call: retrying RPC after server rebind and client "
                 "handle re-init: xid=%lu, view=%s, host=%s, status=%d, "
                 "vob=%s, re-init error code=%u, boottime=%"KS_FMT_TIME_T_D"\n",
                 xid, mfs_vw2nm(vw), alloc_unitp->mnp->mn_view.svr.host, 
                 ((view_hdr_reply_t *)resp)->status,
                 VFS_TO_MMI(vfsp)->mmi_mntpath, error, mvfs_get_boottime());

	if (error != 0) {
            goto cleanup;
        }
    }

    mvfs_clnt_free(alloc_unitp->client, error, vw);

    /* get fresh time after "successful" (got a reply) RPC */
    alloc_unitp->mnp->mn_view.rpctime = MDKI_CTIME(); /* ignore locking */

    /*
     * LOG interesting view-specific errors
     */

    if (callerr == 0)
        status = ((view_hdr_reply_t *)resp)->status;
    else
        status = 0;
    MDB_XLOG((MDB_RPCOPS, "view rpc %s callerr = %d, rebinderr = %d, status= %d user_error=%d\n",
              mfs_viewopnames[op] ? mfs_viewopnames[op] : "<unknown>",
              callerr, error, status, mfs_geterrno(status)));
    if (!error && status != 0) {
	/* Select error priority based on error user will get */
	user_error = mfs_geterrno(status);
	switch (user_error) {
	    case ESTALE:
		pri = MFS_LOG_ESTALE; 
		break;
	    case EIO:
		pri = MFS_LOG_ERR;
		break;
	    default:     
		pri = MFS_LOG_ERR; 
		break;
	}
	suppress_console_msg = 0;	/* By default, print to console */
	if (MVFS_PRI_LOGGED(pri)) {
	    switch(status) {
	      /* VIEW errors */
	      case TBS_ST_VIEW_NO_CFS_SET:
		suppress_console_msg = alloc_unitp->mnp->mn_view.nocfg;
		alloc_unitp->mnp->mn_view.nocfg = 1;
	        goto print_error;
	      case TBS_ST_VIEW_NEEDS_RECOVERY:
		suppress_console_msg = alloc_unitp->mnp->mn_view.needs_recovery;
		alloc_unitp->mnp->mn_view.needs_recovery = 1;
		goto print_error;
	      case TBS_ST_VIEW_NEEDS_REFORMAT:
		suppress_console_msg = alloc_unitp->mnp->mn_view.needs_reformat;
		alloc_unitp->mnp->mn_view.needs_reformat = 1;
		goto print_error;
	      case TBS_ST_WRONG_VOB:
		suppress_console_msg = VFS_TO_MMI(vfsp)->mmi_vobstale_err;
		VFS_TO_MMI(vfsp)->mmi_vobstale_err = 1;
		goto print_error;
              case TBS_ST_VIEW_STG_UNAVAIL:
                suppress_console_msg = alloc_unitp->mnp->mn_view.zombie_view;
                alloc_unitp->mnp->mn_view.zombie_view = 1;
                goto print_error;

	      case TBS_ST_DB_AREA_LOCKED:
	      case TBS_ST_VIEW_CLTXT_ERR:
	      case TBS_ST_VIEW_STALE_DIR:
	      case TBS_ST_NOT_LICENSED:
	      case TBS_ST_LICENSE_BUSY:
print_error:
                if (!suppress_console_msg
                    || suppress_last_pid != MDKI_CURPID()) {

                    suppress_last_pid = MDKI_CURPID();
		    mvfs_log(MFS_LOG_ERR, "view=%s vob=%s%s\n", 
				mfs_vw2nm(vw), 
				VFS_TO_MMI(vfsp)->mmi_mntpath, 
				mfs_strerr(status));
                    UPRINTF(("mvfs: ERROR: view=%s vob=%s%s\n",
                             mfs_vw2nm(vw), 
                             VFS_TO_MMI(vfsp)->mmi_mntpath,
                             mfs_strerr(status)));
		}
		break;

	      /* Special msg for VOB needs recovery 
	       * with suggested command to run
	       */
	      case TBS_ST_VOB_NEEDS_RECOVERY:
		if (!VFS_TO_MMI(vfsp)->mmi_needs_recovery) {
		    mvfs_log(MFS_LOG_ERR, "vob mount %s needs recovery: RUN\n",
		    		VFS_TO_MMI(vfsp)->mmi_mntpath);
		    mvfs_log(MFS_LOG_ERR, "cleartool recovervob %s\n", 
				MFS_STRBUFPN_PAIR_GET_UPN(&(VFS_TO_MMI(vfsp)->mmi_svr.lpn)).s);
		    VFS_TO_MMI(vfsp)->mmi_needs_recovery = 1;
		}
		UPRINTF(("mvfs: ERROR: vob mount %s needs recovery: RUN\ncleartool recovervob %s\n",
			VFS_TO_MMI(vfsp)->mmi_mntpath,
			MFS_STRBUFPN_PAIR_GET_UPN(&(VFS_TO_MMI(vfsp)->mmi_svr.lpn)).s));
		break;

	      case TBS_ST_VOB_NEEDS_REFORMAT:
		if (!VFS_TO_MMI(vfsp)->mmi_needs_reformat) {
		    mvfs_log(MFS_LOG_ERR, "vob mount %s needs reformatting: RUN\n",
				VFS_TO_MMI(vfsp)->mmi_mntpath);
		    mvfs_log(MFS_LOG_ERR, "cleartool reformatvob %s\n", 
				MFS_STRBUFPN_PAIR_GET_UPN(&(VFS_TO_MMI(vfsp)->mmi_svr.lpn)).s);
		    VFS_TO_MMI(vfsp)->mmi_needs_reformat = 1;
		}
		UPRINTF(("mvfs: ERROR: vob mount %s needs reformatting\n",
				VFS_TO_MMI(vfsp)->mmi_mntpath));
		break;
	      case TBS_ST_NOT_FOUND:
	      case TBS_ST_ENOSPC:
		/* These errors are handled at the clnt/clearop level 
	         * when fetching cleartext, so don't print anything here.
		 */
		break;
	      case TBS_ST_CONFIG_SPEC_ERR:
		mvfs_log(MFS_LOG_ERR, "error in the config specification for view %s\n",
			    mfs_vw2nm(vw));
		mvfs_log(MFS_LOG_ERR, "see the vobrpc_server_log on host %s for details\n",
			    VFS_TO_MMI(vfsp)->mmi_svr.host);
		UPRINTF(("mvfs: error in the configuration specification for view %s\nsee the vobrpc_server_log on host %s for details\n",
			mfs_vw2nm(vw),
			VFS_TO_MMI(vfsp)->mmi_svr.host));
		break;

              case TBS_ST_RPC_STALE:
                mvfs_log(MFS_LOG_ERR, 
                         "view %s on host %s found RPC(xid=%lu) to be stale. vob=%s\n",
                         mfs_vw2nm(vw), alloc_unitp->mnp->mn_view.svr.host, 
                         xid, VFS_TO_MMI(vfsp)->mmi_mntpath);
                break;

	      default:

		/* If view gave EIO and no interpretation here, then tell user
	         * to look at the view_log
	         */
		if (user_error == EIO) {
	            mvfs_log(MFS_LOG_ERR, "view=%s vob=%s%s\n",
				mfs_vw2nm(vw),
				VFS_TO_MMI(vfsp)->mmi_mntpath,
				mfs_strerr(status));
		    mvfs_log(MFS_LOG_ERR, "see view log on host %s for more info\n", 
				alloc_unitp->mnp->mn_view.svr.host);
	            UPRINTF(("mvfs: ERROR: view=%s vob=%s%s\nsee view_log on host %s for more info\n",
			mfs_vw2nm(vw),
			VFS_TO_MMI(vfsp)->mmi_mntpath,
			mfs_strerr(status),
			alloc_unitp->mnp->mn_view.svr.host));
		}
		break;
	    } /* switch */
        }     /* if MVFS_PRI_LOGGED ... */
    }	/* !error && remote_error */

    /* If no error at all, then clear all error msg printed bits
     * for both VOB and View
     */

    if (!error && status == 0) {
	VFS_TO_MMI(vfsp)->mmi_needs_recovery = 0;
	VFS_TO_MMI(vfsp)->mmi_needs_reformat = 0;
	VFS_TO_MMI(vfsp)->mmi_vobstale_err = 0;
	VTOM(vw)->mn_view.nocfg = 0;
	VTOM(vw)->mn_view.needs_recovery = 0;
	VTOM(vw)->mn_view.needs_reformat = 0;
	VTOM(vw)->mn_view.zombie_view = 0;
    }
  cleanup:
    KMEM_FREE(rinfop, sizeof(*rinfop));
    KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
    return(error);
}

/*
 * MFSCALL - make an MFS rpc call 
 */
int
mfscall(
    struct mfs_callinfo *trait,
    int op,
    XID_T ixid,	/* Transaction ID allocated if == 0 */
    struct mfs_svr *svr,
    struct mfs_retryinfo *rinfo,
    xdrproc_t xdrargs,
    void *argsp,
    xdrproc_t xdrres,
    void *resp,
    CRED_T *cred,
    VNODE_T *view
)
{
    CLIENT *client;
    int error;
    XID_T xid;

    ASSERT(ixid == 0); /* must allow us to allocate the xid */
    
    error = mvfs_clnt_get(trait, svr, rinfo, cred, view, &client);
    if (error != 0)
	return error;
    xid = (XID_T)MDKI_ALLOC_XID();

    error = mfscall_int(trait, op, &xid, svr, rinfo, xdrargs, argsp,
			xdrres, resp, cred, client, view);
    mvfs_clnt_free(client, error, view);
    return(error);

}

/*
 * MFSCALL_INT - make an MFS rpc call (internal version)
 */
STATIC int MVFS_NOINLINE
mfscall_int(
    struct mfs_callinfo *trait,
    int op,
    XID_T *xidp,                        /* Transaction ID must be supplied */
    struct mfs_svr *svr,
    struct mfs_retryinfo *rinfo,
    xdrproc_t xdrargs,
    void *argsp,
    xdrproc_t xdrres,
    void *resp,
    CRED_T *cred,
    CLIENT *client,
    VNODE_T *view
)
{
    enum clnt_stat status;
    struct timeval wait;
    int rpctimeout;
    int error, remote_error;
    XID_T xid = 0;
    int retrans;
    MDKI_SIGMASK_T saved_holdmask;
    ks_uint32_t lboottime;

    /* Declare a type so we can do one allocation to save stack space. */
    struct {
        timestruc_t start_time;	/* For stats/debug */
        timestruc_t dtime;
        struct rpc_err rpcerr;
        CRED_T *ruid_cred;
        struct mvfs_pvstat *pvp;        /* Pointer to per-view stats */
        MDKI_CLNTKUDP_ADDR_T addr;
    } *alloc_unitp;

    MFS_CHKSP(STK_CLNTCALL);

    /* If the trait or svr is NULL (no server) 
	just croak and return an error */

    if (trait == NULL || svr == NULL) return(ECONNREFUSED);

    /* If the server address isn't bound tell caller to rebind
       (if appropriate). */

    if (!svr->svrbound) {
	if (rinfo->rebind) return(EAGAIN);	/* Tell caller to rebind */
	else return(ECONNREFUSED);
    }

#ifdef MVFS_DEBUG
    /* DEBUG: verify that opnames for trait are up to date */

    if (trait->opnames[op] == NULL) {
	mvfs_log(MFS_LOG_DEBUG, "mfscall: no name for operation %d\n", op);
    }
#endif

    /* Keep stats on calls */
    /* XXX Should get stats lock once around these calls, also move clntstat
     * up from a few lines below here. Could change 5 lock calls to one!
     */

    BUMPSTAT(mfs_clntstat.mfscall);
    if (view) {
	BUMPVSTATV(view, clntstat.mfscall);
    }
    /* Count ops to view, no counts kept for albd */
    if (trait == &mfs_vwcallstruct) {
        BUMPSTAT(mfs_viewopcnt[op]);
    }

    /*
     * MAIN retry loop.  Do to the fact that someone
     * may have rebound the address in the view, we must 
     * free/re-acquire the client handle on each retry attempt
     * for hard mounts.
     */
    ASSERT(*xidp);
    xid = *xidp; /* start out with input xid */

    /* Set initial timeout based on operation type */
    rpctimeout = KS_MIN((rinfo->timeo << (trait->optimeoshft)[op]), MVFS_MAXTIME);

    /* Allocate the vars we need to save stack space.  Don't return without
    ** freeing after this (i.e. return through the errout: label).
    */
    if ((alloc_unitp = KMEM_ALLOC(sizeof(*alloc_unitp), KM_SLEEP)) == NULL) {
        return(ENOMEM);
    }

    alloc_unitp->ruid_cred = NULL;
    alloc_unitp->pvp = 0;
retry_call:
    lboottime = mvfs_get_boottime();

    /* Set return error struct to none */

    alloc_unitp->rpcerr.re_errno = 0;
    error = 0;

    /*
     * Fill in boot time and xid.  Servers use the tag
     * ipaddr,port,boot_time,xid to detect duplicates.  xids lower
     * than the latest xid seen at the server are deemed to be
     * duplicates (until a timeout expires and state is forgotten).
     * boottime is included so that the server can detect when a
     * node has been rebooted, and not discard new requests.
     * boottime is also useful for some mutant OSes that use one UDP
     * port for all CLIENT handles since it can serve as a differentiator 
     * between the handles to avoid unnecessary view_server RPC rejections 
     * (as long as those OSes provide a mechanism to generate unique numbers
     * rather than a fixed time).  We want the view server to be able to 
     * detect real network errors (packet delay/duplication/reordering) 
     * within the stream generated by a single CLIENT handle, so we use the
     * same boottime value for all uses of a particular CLIENT.  
     *
     * The boottime value returned by mvfs_get_boottime() is the same until 
     * XID wraps around.  Hence, all CLIENTs have the same boottime, but 
     * different UDP source ports.  Once XID wraps around, mvfs_boottime is 
     * updated and mvfs_get_boottime() returns the updated value.  Mutant
     * OSes have a single UDP source port and distinct boottimes per
     * CLIENT.
     *
     * Since this routine is using a single CLIENT, it uses the same boottime
     * value for all its retries.
     */
    if (argsp)
        (*trait->set_xid)(argsp, lboottime, xid);

    /* Also set the RPC xid, so we'll discard incorrect responses. */
    MDKI_SET_XID(client,xid);

    /* Get start time for real-time stats */
    MDKI_HRTIME(&(alloc_unitp->start_time));

    /* 
     * Do the call.  CLNT_CALL will do one set of retries according
     * to the values in the client handle.
     */

    MDKI_CLNTKUDP_INTR(client, !rinfo->nointr);

    ASSERT(cred);
    MDKI_CLNT_CALL(client, op, xdrargs, argsp, xdrres, resp, wait, rpctimeout,
			    &saved_holdmask, !(rinfo->nointr), cred, status);
    BUMPSTAT(mfs_clntstat.clntcalls);
    if (view) {
        BUMPVSTATV(view, clntstat.clntcalls);
    }
    /* Parse RPC return status */

    switch (status) {

    case RPC_SUCCESS: 
        /* Print a message if reply XID not match call XID */
        if (resp) {
            if ((*trait->get_xid)(resp) != xid) {
    	    mvfs_log(MFS_LOG_INFO, 
    		     "%s op %s xid mismatch: resp %lx, rqst %lx\n",
    		     trait->svrname,
    		     trait->opnames[(int)op] ? trait->opnames[(int)op] : "unknown operation", 
    		     (*trait->get_xid)(resp), xid);
            }
        }

        /* 
	 * Compute time used on calls to a view_server.  
         * Accumulate statistics and print
         * a message if too much time for this call. 
         */

        if (trait == &mfs_vwcallstruct) {
            /*
             * dtime is a time_t struct, for some platforms this is
             * 64bits but we truncate it to 32bits. This should be ok
             * since we don't really expect rpc to take longer than 32bit
             * worth of seconds.
             */
            MVFS_BUMPTIME(alloc_unitp->start_time, alloc_unitp->dtime,
                         mfs_viewoptime[op]);
	    if (alloc_unitp->dtime.tv_sec > mvfs_max_rpcdelay) {
		mvfs_log(MFS_LOG_INFO,
		    "View op %s delayed %d seconds!\n",
		    (trait->opnames)[(int)op] ? (trait->opnames)[(int)op]
                         : "unknown operation", 
		     (int)(alloc_unitp->dtime.tv_sec));
            /* Update max delay stats per-cpu and per-view. */
                SETSTAT_MAX_DELAY((MVFS_STAT_CNT_T)alloc_unitp->dtime.tv_sec);
                if (view) {
                     SETPVSTAT_MAX_DELAY(view, (MVFS_STAT_CNT_T)alloc_unitp->dtime.tv_sec);
                }
	    }
	    if (trait == &mfs_vwcallstruct) {
		/* Separate table for cleartext fetch op - expected slowness */
		if (op == VIEW_CLTXT) {
                    SET_MAXDELAY(alloc_unitp->dtime.tv_sec,
                                 alloc_unitp->dtime.tv_nsec, histclr);
                } else {
                    SET_MAXDELAY(alloc_unitp->dtime.tv_sec,
                                 alloc_unitp->dtime.tv_nsec, histrpc);
		}
	    }
        }
   	
        break;

    /*
     * These errors indicate that we sent the RPC
     * to the wrong server, or to a view_server for a view
     * that is different than the view_uuid passed in the
     * RPC pkt view handle field.  In either case, assume
     * the problem is a stale socket number in the view
     * address, and jump out to rebind view if rebinding
     * is enabled.  If rebinding is not enabled, just
     * fall through to the normal case for total failure.
     */
    case RPC_PROGUNAVAIL:
	if (rinfo->rebind) {
	    error = EAGAIN;
	    goto errout;
	}
	/* Fall through */
	
    /*
     * Unrecoverable errors: give up immediately
     */
    case RPC_PROCUNAVAIL:
    case RPC_AUTHERROR:
    case RPC_CANTENCODEARGS:
    case RPC_CANTDECODERES:
    case RPC_VERSMISMATCH:
    case RPC_PROGVERSMISMATCH:
    case RPC_CANTDECODEARGS:
        BUMPSTAT(mfs_clntstat.mfsfail);
	if (view) {
            BUMPVSTATV(view, clntstat.mfsfail);
	}
        error = 0; /* We fall thru and cause extraction of rpcerr.re_errno */
	           /* ECOMM is not defined for the hp400. */
        break;

    case RPC_INTR:	
        BUMPSTAT(mfs_clntstat.mfsintr);
        if (view) {
            BUMPVSTATV(view, clntstat.mfsintr);
        }
        error = EINTR;
        break;

    case RPC_TIMEDOUT: 
        error = ETIMEDOUT;
        /* Fall through */

    default:
        if (!rinfo->soft || (rinfo->rebind && status == RPC_TIMEDOUT)) {
            if (!svr->dprinted) {
    	        svr->dprinted = 1;
                mvfs_log(MFS_LOG_WARN,
                         "%s server for %s:%s not responding still trying "
                         "(st=%d er=%d soft=%d rebind=%d)\n",
                         trait->svrname, svr->host,
                         MFS_STRBUFPN_PAIR_GET_UPN(&svr->lpn).s,
                         status, error, rinfo->soft, rinfo->rebind);
                UPRINTF(("%s server for %s:%s not responding still trying (st=%d er=%d)\n",
    			 trait->svrname, svr->host, MFS_STRBUFPN_PAIR_GET_UPN(&svr->lpn).s,
                         status, error));
            }
            BUMPSTAT(mfs_clntstat.clntretries);
            if (view) {
                BUMPVSTATV(view, clntstat.clntretries);
            }
    	    /*
             * Return to higher level if it supports rebinding
    	     * on a timeout.  Use a funky unique error e.g. EAGAIN!
     	     * Update the retry structure with timeout backoff!
    	     */
    	    if (status == RPC_TIMEDOUT && rinfo->rebind) {
    		rinfo->timeo = BACKOFF(rpctimeout);
    		error = EAGAIN;
    		goto errout;
    	    } else {			/* Hard mount, retry call */
                rpctimeout = BACKOFF(rpctimeout);
		retrans = (svr->down) ? 1 : rinfo->retries;

		/* Free client creds */
		MDKI_CLNTKUDP_FREE(client);
		/* Re-init */
                /* We should find a way to ASSERT the client handle's
                   proto/version match the requested traits */
	        error = MDKI_CLNTKUDP_INIT(client, &svr->addr.ks_ss_s, retrans,
                                           cred, (!rinfo->nointr), &alloc_unitp->addr, 
                                           trait->proto, trait->version,
                                           &svr->knc);
	        if (error != 0) {
	            goto errout;
	        }
		xid = (XID_T)MDKI_ALLOC_XID();
		/* retry it */
		goto retry_call;
	    }
        } /* end of if (!soft or rebind) */
    } /* end of switch */

    /* On errors, print a message about the problem */

    if (status != RPC_SUCCESS) {
	MDKI_CLNT_GETERR(client, &alloc_unitp->rpcerr);
	if (!error) error = alloc_unitp->rpcerr.re_errno;	/* Use remote error */
	/* Don't print a message if interrupted. */
        if (status != RPC_INTR) {
	    if (status == RPC_PROCUNAVAIL) {
		mvfs_log(MFS_LOG_DEBUG, "Incompatible %s server on %s?\n",
			    trait->svrname, svr->host);
	    } else {
		svr->down = 1;
		mvfs_log(MFS_LOG_ERR, "%s op %s failed for %s:%s -  %s\n", 
			    trait->svrname,
			    (trait->opnames)[(int)op] ? (trait->opnames)[(int)op] : "unknown operation", 
			    svr->host, MFS_STRBUFPN_PAIR_GET_UPN(&svr->lpn).s, MDKI_CLNT_SPERRNO(status));
		UPRINTF(("%s op %s failed for %s:%s - %s\n", 
			    trait->svrname,
			    (trait->opnames)[(int)op] ? (trait->opnames)[(int)op] : "unknown operation",
			    svr->host, MFS_STRBUFPN_PAIR_GET_UPN(&svr->lpn).s, MDKI_CLNT_SPERRNO(status)));
	    }
	}

    /* RPC_SUCCESS, if previously declared server down, now it is up */

    } else {
	if (svr->dprinted) {
	    mvfs_log(MFS_LOG_WARN, "%s server for %s:%s ok\n", 
			trait->svrname, svr->host, MFS_STRBUFPN_PAIR_GET_UPN(&svr->lpn).s);
	    UPRINTF(("%s server for %s:%s ok\n", 
			trait->svrname, svr->host, MFS_STRBUFPN_PAIR_GET_UPN(&svr->lpn).s));
	    svr->dprinted = 0;
	}
	svr->down = 0;

	/* 
         * I think this is the same kludge as NFS has for setuid-root
	 * programs.  Since "root" is turned into "nobody" across
 	 * the net (untrusted root), then someone running a setuid root
	 * program can lose all rights, and the program will fail.
	 * So... we look for this case, and if the real uid is not root
	 * then we retry the RPC request with the real uid instead of
	 * the effective uid (of root-> nobody).  This would be a lot
	 * easier if sunrpc just passed both real and effective and let
 	 * the server decide ....
	 */
	if (resp && MDKI_CR_IS_SETUID_ROOT(cred) && alloc_unitp->ruid_cred == NULL && 
		((remote_error = mfs_geterrno((*(trait->get_status))(resp))) 
							== EACCES ||
		  remote_error == EPERM))
        {
	    mvfs_log(MFS_LOG_DEBUG, "mfscall: retrying setuid root: cmd %s op %s\n", 
                     MDKI_GET_U_COMM_PTR(),
                     (trait->opnames)[(int)op]? (trait->opnames)[(int)op] : "unknown operation");
	    alloc_unitp->ruid_cred = MDKI_CRDUP(cred);
	    MDKI_CR_SET_E2RUID(alloc_unitp->ruid_cred); 
	    /* Overwrite local var to make retry easier.  Ruid_cred flags
             * if there is a credential we must free 
             */
            /* POSIX creds in this case, check MDKI_CR_IS_SETUID_ROOT in mdep */
	    cred = alloc_unitp->ruid_cred; /* POSIX creds only */
	    retrans = (svr->down) ? 1 : rinfo->retries;

	    /* Free client creds */
	    MDKI_CLNTKUDP_FREE(client);
            /* We should find a way to ASSERT the client handle's proto/version
               match the requested traits */
	    error = MDKI_CLNTKUDP_INIT(client, &svr->addr.ks_ss_s, retrans, cred,
                                       (!rinfo->nointr), &alloc_unitp->addr, trait->proto,
                                       trait->version, &svr->knc);
	    if (error != 0) {
	        goto errout;
	    }
	    xid = (XID_T)MDKI_ALLOC_XID();
	    goto retry_call;		/* Go retry from the start */
	}
    }

    /* Destroy the client handle */

errout:

    /* If we allocated a cred struct to try ruid creds, free it now! */

    if (alloc_unitp->ruid_cred) {
	MDKI_CRFREE(alloc_unitp->ruid_cred);
    }
    *xidp = xid;
    KMEM_FREE(alloc_unitp, sizeof(*alloc_unitp));
    return (error);
}
static const char vnode_verid_mvfs_rpcutl_c[] = "$Id:  fefbc03f.236411e2.8951.00:01:84:c3:8a:52 $";
