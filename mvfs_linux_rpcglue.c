/*
 * Copyright (C) 1999, 2007 IBM Corporation.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA
 *
 * Author: IBM Corporation
 * This module is part of the IBM (R) Rational (R) ClearCase (R)
 * Multi-version file system (MVFS).
 * For support, please visit http://www.ibm.com/software/support
 */

#include "vnode_linux.h"

#include "linux_krpc.h"

/*
 * The Linux kernel RPC structure is different from standard ONC RPC
 * kernel structure in many ways.  This is the conversion between the
 * two.
 */

/*
 * XXX There do not appear to be any locking protocols needed for RPC
 * client stuff.  RPC code handles internal structure locking itself
 */
CLIENT *
mdki_linux_clntkudp_create(
    struct sockaddr_in *addr,
    const int version,
    struct rpc_program *prog,
    const int retrans_count,
    const bool_t intr
)
{
    struct rpc_clnt *rpc_cl;
    struct rpc_xprt *xprt;

    xprt = xprt_create_proto(IPPROTO_UDP, addr, NULL);
    if (IS_ERR(xprt))
        return NULL;

    rpc_cl = rpc_create_client(xprt, prog->name /*XXX not really host name! */,
                               prog, version, RPC_AUTH_UNIX);
    if (IS_ERR(rpc_cl)) {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
        (void) xprt_destroy(xprt);
#endif
        return NULL;
    }
    /*
     * Ignore creds for now (not passed in).  At call time, we'll set
     * up current->fsuid/fsgid which will get picked up by the RPC
     * runtime routines.
     */
    rpc_cl->cl_softrtry = 1;         /* we want control after timeouts */
    rpc_cl->cl_intr = intr;
    return rpc_cl;
}

void
mdki_linux_clnt_set_intr(
    CLIENT *cl,
    bool_t intr
)
{
    struct rpc_clnt *rpc_cl = (struct rpc_clnt *)cl;
    rpc_cl->cl_intr = intr;
}

/*
 * Take an existing handle and redirect it to work to a different
 * destination.
 */
void
mdki_linux_clntkudp_init(
    CLIENT *cl,
    struct sockaddr_in *addr,
    int retries,
    bool_t intr
)
{
    struct rpc_clnt *rpc_cl = (struct rpc_clnt *)cl;

    *RPC_PEERADDR(rpc_cl) = *addr;
    rpc_cl->cl_timeout.to_retries = retries;

    /* credentials will be handled in clnt_call */
    rpc_cl->cl_intr = intr;
    MDKI_TRACE(TRACE_RPC, "clntkudp_init %p ntries %d intr %d\n",
               rpc_cl, retries, intr);
}

void
mdki_linux_clnt_call(
    CLIENT *cl,
    int procnum,
    void *args,
    void *results,
    int rpctimeout,
    bool_t intr,
    CRED_T *cred,
    enum clnt_stat *status
)
{
    struct rpc_clnt *rpc_cl = (struct rpc_clnt *)cl;
    int res;
    vnlayer_fsuid_save_t oldfsuid;
    bool_t swap_ids;
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18)
    /*  as they don't export rpc_call we have
     *  at least 2 choices: reimplement rpc_call or use
     *  rpc_call_sync. It requires a pointer to a rpc_message struct.
     */
    struct rpc_message rpc_msg;
#endif
    STACK_CHECK_DECL()

    MDKI_TRACE(TRACE_RPC,
               "clnt_call %p proc %d timeo %d intr %d cred %p\n",
               rpc_cl, procnum, rpctimeout, intr, cred);
    rpc_cl->cl_intr = intr;
    /*
     * set timeouts &c. They're measured in ticks. rpctimeout is in
     * tenths of seconds.
     *
     * We used to call rpc_set_timeout() or xprt_set_timeout(), but
     * those only work right for linear backoff, not exponential
     * backoff.  They also set up the maximum value to be (retries *
     * increment), which is a lot larger than the max value used by
     * other UNIX platforms.
     *
     * So, we need to set the max and initial values ourselves,
     * and set the exponential flag.  (NFS likewise has to set things
     * up itself.)
     */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,9)
    rpc_cl->cl_timeout.to_current = 
#endif
    rpc_cl->cl_timeout.to_initval = rpctimeout * HZ / 10;
    rpc_cl->cl_timeout.to_maxval = 20*HZ; /* like Solaris kudp */
    rpc_cl->cl_timeout.to_exponential = 1;
    /* to_retries is set in mdki_linux_clntkudp_init() */

    ASSERT(rpc_cl->cl_procinfo[procnum].p_encode != NULL);

    /*
     * Set up identity for the duration of the call.
     */
    /* XXX locking? */

    swap_ids = vnlayer_fsuid_save(&oldfsuid, cred);
    STACK_CHECK();
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18)
    res = rpc_call(rpc_cl, procnum, args, results, 0);
#else
    /* the new way: rpc_call_sync and an all-inside struct */
    rpc_msg.rpc_proc = &rpc_cl->cl_procinfo[procnum];
    rpc_msg.rpc_argp = args;
    rpc_msg.rpc_resp = results;
    rpc_msg.rpc_cred = NULL;
    res = rpc_call_sync(rpc_cl, &rpc_msg, 0);
#endif
    STACK_CHECK();
    if (swap_ids)
        vnlayer_fsuid_restore(&oldfsuid);

    /* interpret returns for *status */
    if (res < 0) {
        MDKI_TRACE(TRACE_RPC, "rpc_call proc %d error %d\n", procnum, -res);
        /* It's an errno */
        /*
         * Linux doesn't provide enough detail on the reply.  Sun ONC
         * RPC does much better.  Blargh!
         */
        switch (-res) {
          case EIO:
            /*
             * Bloody RPC code returns EIO in lots of places,
             * obliterating the lower-level transport error code.  The
             * most important code of which is ETIMEDOUT.  So we have
             * to call every random error a timeout.  !@!#$!#
             */
          case ETIMEDOUT:
          case ECONNREFUSED:
            *status = RPC_TIMEDOUT;
            break;
          case EOVERFLOW:               /* from our XDR routine */
            *status = RPC_CANTENCODEARGS;
            break;
          case ERANGE:                  /* from our XDR routine */
            *status = RPC_CANTDECODERES;
            break;
          case ERESTARTSYS:             /* signalled while pending */
            *status = RPC_INTR;
            break;
          case EACCES:
            /* see clnt.c:call_verify().  Probably bad server (try rebind) */
            *status = RPC_PROGUNAVAIL;
            break;
          default:
            MDKI_TRACE(TRACE_RPC,"rpc_call status %d?\n", -res);
            *status = RPC_PROGVERSMISMATCH; /* XXX */
            break;
        }
    } else {
        /* All the RPC layer gunk returns negative errors.  Our encode/decode
           functions will return positive clnt_stat errors. */
        MDKI_TRACE(TRACE_RPC, "rpc_call proc %d okstatus %d\n", procnum, res);
        *status = res;
    }
    return;
}

extern int
mdki_linux_destroy_client(CLIENT *cl)
{
    struct rpc_clnt *rpc_cl = (struct rpc_clnt *)cl;
    return rpc_destroy_client(rpc_cl);
}

static const char vnode_verid_mvfs_linux_rpcglue_c[] = "$Id:  867601ec.66c511dc.9bbb.00:01:83:09:5e:0d $";
