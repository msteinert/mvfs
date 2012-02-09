/* * (C) Copyright IBM Corporation 1990, 2006. */
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

#if !defined(_KS_BASE_H_)
#define _KS_BASE_H_

/****************************************************************************
 * Basic macro values
 *
 * ATRIA_ANSI_C		Indicates that the compiler for this platform provides
 *			(a superset of) the facilities defined by ANSI C.
 *
 * ATRIA_PROTOTYPES	Indicates that the compiler for this platform supports
 *			function prototypes, but not necessarily full ANSI C.
 */

/* ANSI C implies function prototypes (but not necessarily vice versa) */

/****************************************************************************
 * System includes
 */

/* Read in all types (POSIX and BSD), not just the POSIX set */

#include <linux/param.h>
#include <linux/types.h>
#include <linux/stat.h>

/* Mount table definitions */

/* Convert SVR4 tags to pre-SVR4 tags */
#define mnt_special     mnt_fsname
#define mnt_mountp      mnt_dir
#define mnt_mntopts     mnt_opts
#define mnt_fstype      mnt_type
#define MNTTAB_T struct mntent
#define MNT_LINE_MAX 1024

/****************************************************************************
 * Sockets are file descriptors on Unix and something else on Windows.  Use
 * "ks_socket_t" instead of int for sockets.  
 */

#define KS_INVALID_SOCKET (-1)
typedef int ks_socket_t;

/****************************************************************************
 * Some newer operating systems have changed the Berkeley "int" arguments
 * to socket functions to size_t.  This is arguably correct but certainly
 * incompatible.
 * Use "ks_sock_param_t" instead of an explicit type.
 */

typedef int ks_sock_param_t;

/****************************************************************************
 * Basic c directives
 */

#if !defined(STATIC)
#define STATIC static
#endif

/* EXTERN moved to ks_extern.h */

#if !defined(REGISTER)
#define REGISTER register
#endif

#if !defined(VOLATILE)
#define VOLATILE volatile
#endif

#define ATRIA_CONST const
#define A_CONST ATRIA_CONST  /* abbrev. for those who just can't take it */

/****************************************************************************
 * Basic c types
 */

#define KS_INT16_T_MIN  ((ks_int16_t)0x8000)
#define KS_INT16_T_MAX  ((ks_int16_t)0x7fff)
#define KS_INT32_T_MIN  ((ks_int32_t)0x80000000l)
#define KS_INT32_T_MAX  ((ks_int32_t)0x7fffffffl)

#define KS_UINT8_T_MIN  ((ks_uint8_t)0x00)
#define KS_UINT8_T_MAX  ((ks_uint8_t)0xff)
#define KS_UINT16_T_MIN ((ks_uint16_t)0x0000)
#define KS_UINT16_T_MAX ((ks_uint16_t)0xffff)
#define KS_UINT32_T_MAX ((ks_uint32_t)0xffffffff)  /* SKR 21/09/95 */

typedef unsigned char ks_byte_t;

typedef int ks_boolean_t;

/*
 * Machine dependent data types
 *
 */

 

 

#if defined (__i386__)
#define KNOWN_ARCH
#include "ks_mdep_linux.h"
#endif /* __i386__ */
#if defined (__ia64__) || defined(__x86_64__)
#define KNOWN_ARCH
#include "ks_mdep_linux64.h"
#endif /* __ia64__ || __x86_64__ */
#if defined (__s390__) || defined(__powerpc__)
#define KNOWN_ARCH
#if defined(ATRIA_LP64) || defined(__s390x__) || defined(__powerpc64__)
#include "ks_mdep_linux64.h"
#else
#include "ks_mdep_linux.h"
#endif /* ATRIA_LP64 */
#endif /* __s390__ || __powerpc__ */

#ifndef KNOWN_ARCH
#error "ks_base.h: Missing machine dependent type info for this architecture."
#else
#undef KNOWN_ARCH
#endif

/* Explicitly sized integer types */
#if   defined(ATRIA_64BIT_LONGS) || defined(_LP64) || defined(__LP64__)
#define KS_FMT_INT64_T_D "ld"
#define KS_FMT_INT64_T_X "lx"
#define KS_FMT_PTR_T "lx"
typedef long ks_int64_t;
typedef unsigned long ks_uint64_t;
#else
#define KS_FMT_INT64_T_D "lld"
#define KS_FMT_INT64_T_X "llx"
#define KS_FMT_PTR_T "lx"
typedef long long ks_int64_t;
typedef unsigned long long ks_uint64_t;
#endif

typedef int ks_int32_t;
typedef unsigned int ks_uint32_t;
typedef short ks_int16_t;
typedef unsigned short ks_uint16_t;
typedef unsigned char  ks_uint8_t;

typedef ks_int32_t ks_off32_t;
typedef ks_int64_t ks_off64_t;

#define KS_FMT_OFF64_T_D KS_FMT_INT64_T_D
#define KS_FMT_OFF64_T_X KS_FMT_INT64_T_X

/* Efficient 64-bit field composition/decomposition macros
 * for handling as separate 32 bit fields
 */
#define KS_ULLONG_HIGH(llong)    ((ks_uint32_t) ((ks_uint64_t)(llong) >> 32))
#define KS_ULLONG_LOW(llong)     ((ks_uint32_t) (llong))
#define KS_ULLONG_SPLIT(llong, ulhigh, ullow) \
        (void) ((ulhigh) = KS_ULLONG_HIGH(llong), \
                (ullow) = KS_ULLONG_LOW(llong))
#define KS_ULLONG_COMBINE(ulhigh, ullow) \
        ((ks_uint64_t)(ulhigh) << 32 | (ks_uint32_t)(ullow))

#define KS_LLONG_HIGH     KS_ULLONG_HIGH
#define KS_LLONG_LOW      KS_ULLONG_LOW
#define KS_LLONG_SPLIT(llong, lhigh, llow) \
        (void) ((lhigh) = KS_LLONG_HIGH(llong), \
                (llow) = KS_LLONG_LOW(llong))
#define KS_LLONG_COMBINE KS_ULLONG_COMBINE

/*
 * Since we define ks_int32_t and ks_uint32_t to be a single type on all
 * platforms, define XDR routines to encode/decode them here.
 */
#define XDR_KS_U_INT32 xdr_u_int
#define XDR_KS_INT32 xdr_int

#if defined(ATRIA_TIME_T_LONG)
#define KS_LONG_OR_INT_TIME_T(alt_if_long,alt_if_int) alt_if_long
#define KS_FMT_TIME_T_D "ld"
#define KS_FMT_TIME_T_X "lx"
#elif defined(ATRIA_TIME_T_INT) || defined(ATRIA_TIME_T_UINT)
#define KS_LONG_OR_INT_TIME_T(alt_if_long,alt_if_int) alt_if_int
#define KS_FMT_TIME_T_D "d"
#define KS_FMT_TIME_T_X "x"
#elif defined(ATRIA_TIME_T_INT64) || defined(ATRIA_TIME_T_UINT64)
/*
 * KS_LONG_OR_INT_TIME_T needs to be removed and the appropriate
 * format string needs to be used.
 */
#define KS_LONG_OR_INT_TIME_T(alt_if_long,alt_if_int) alt_if_int
#define KS_FMT_TIME_T_D "%I64u"
#define KS_FMT_TIME_T_X "%I64x"
#else
#error "ks_base.h: size of time_t unknown"
#endif /* ATRIA_TIME_T_LONG */

#if defined(ATRIA_TIMEVAL_TV_SEC_TIME_T)
#define KS_FMT_TV_SEC_T_D KS_FMT_TIME_T_D
#define KS_FMT_TV_SEC_T_X KS_FMT_TIME_T_X
#elif defined(ATRIA_TIMEVAL_TV_SEC_LONG)
#define KS_FMT_TV_SEC_T_D "ld"
#define KS_FMT_TV_SEC_T_X "lx"
#elif defined(ATRIA_TIMEVAL_TV_SEC_ULONG)
#define KS_FMT_TV_SEC_T_D "lu"
#define KS_FMT_TV_SEC_T_X "lx"
#elif defined(ATRIA_TIMEVAL_TV_SEC_INT)
#define KS_FMT_TV_SEC_T_D "d"
#define KS_FMT_TV_SEC_T_X "x"
#elif defined(ATRIA_TIMEVAL_TV_SEC_UINT)
#define KS_FMT_TV_SEC_T_D "u"
#define KS_FMT_TV_SEC_T_X "x"
#endif
#if defined(ATRIA_TIMEVAL_T_LONG)
#define KS_LONG_OR_INT_TIMEVAL_T(alt_if_long,alt_if_int) alt_if_long
#define KS_FMT_TV_USEC_T_D "ld"
#define KS_FMT_TV_USEC_T_X "lx"
#elif defined(ATRIA_TIMEVAL_T_INT)
#define KS_LONG_OR_INT_TIMEVAL_T(alt_if_long,alt_if_int) alt_if_int
#define KS_FMT_TV_USEC_T_D "d"
#define KS_FMT_TV_USEC_T_X "x"
#else
#error "ks_base.h: size of timeval unknown"
#endif  /* ATRIA_TIMEVAL_T_LONG */

/*
 * These formats currently the same on all platforms.  
 */
#define KS_LONG_OR_INT_INT32_T(alt_if_long,alt_if_int) alt_if_int
#define KS_LONG_OR_INT_OFF_T(alt_if_long,alt_if_int) alt_if_long

#define KS_FMT_INT32_T_D "d"
#define KS_FMT_INT32_T_X "x"

#if defined(ATRIA_SIZE_T_UINT64)
/*
 * NEEDS to be changed. We probably need to change all references
 * of KS_LONG_OR_INT_SIZE_T to the appropriate format string.
 */

#define KS_LONG_OR_INT_SIZE_T(alt_if_long,alt_if_int) alt_if_long
#define KS_FMT_SIZE_T_D "I64u"
#define KS_FMT_SIZE_T_X "I64x"
#else
#define KS_LONG_OR_INT_SIZE_T(alt_if_long,alt_if_int) alt_if_long
#define KS_FMT_SIZE_T_D "ld"
#define KS_FMT_SIZE_T_X "lx"
#endif

#define XID_T	u_long

#if !defined(TRUE)
#define TRUE 1
#define FALSE 0
#endif

#define VOID_PTR void *

#if !defined(NULL)
#define NULL 0
#endif

#if !defined(NULLC)
#define NULLC '\0'
#endif

#if defined(ATRIA_64BIT_LONGS) || defined(_LP64) || defined(__LP64__)
#define KS_FMT_FSBLKCNT_T_U "lu"
#define KS_FMT_FSBLKCNT_T_X "lx"
#define KS_FMT_FSBLKCNT_T_D "ld"
#define KS_FMT_OFF_T_X "lx"
#define KS_FMT_OFF_T_D "ld"
#define KS_FMT_KS_INT64_T "l"
#define KS_FMT_KS_INT64_T_X "lx"
#define KS_FMT_KS_INT64_T_D "ld"
#define KS_FMT_KS_INT64_T_U "lu"
#else
#define KS_FMT_FSBLKCNT_T_U "llu"
#define KS_FMT_FSBLKCNT_T_X "llx"
#define KS_FMT_FSBLKCNT_T_D "lld"
#define KS_FMT_OFF_T_X "llx"
#define KS_FMT_OFF_T_D "lld"
#define KS_FMT_KS_INT64_T "ll"
#define KS_FMT_KS_INT64_T_X "llx"
#define KS_FMT_KS_INT64_T_D "lld"
#define KS_FMT_KS_INT64_T_U "llu"
#endif
#ifdef ATRIA_64BIT_LONGS
#define KS_MAX_FILE_SIZE 0x7fffffffffffffff /* LSEEK max file length */
#else
#define KS_MAX_FILE_SIZE 0x7fffffffffffffffLL /* LSEEK max file length */
#endif
#define KS_FLOCK_T struct flock
#define KS_GETLK F_GETLK
#define KS_SETLK F_SETLK

#include "ks_extern.h"

#define KS_CHAR char
#define KS_DYNARR(sz,t,x) 	t x
#define KS_UNIONIS(x)
#define KS_CASEIS(x)

/****************************************************************************
 * Data Alignment
 * (for storage allocation)
 */

#define KS_ALIGNMENT_TYPE double /* 8 byte alignment on all platforms */
#define KS_ALIGNMENT_BYTES sizeof(KS_ALIGNMENT_TYPE) /* Must be a power of 2 */
#define KS_ALIGN_SIZE(_bytes) ((_bytes + KS_ALIGNMENT_BYTES-1) & ~(KS_ALIGNMENT_BYTES-1))

/****************************************************************************
 * Handles
 *
 * Use a struct pointer to declare a private type handle to make compiler
 * perform type checking between different kinds of handles
 */

#if defined(lint)
#define KS_HANDLE(name) VOID_PTR name
#else
#define KS_HANDLE(name) struct name ## _struct *name
#endif

/****************************************************************************
 * Function prototypes
 */

/* For RPC interface preprocessor, include special directives */
#define P_NONE void
#ifndef VISTA_H
#define P1(x) x
#endif
#define PN(x) , x
#define	PIN(x)	,x
#define	POUT(x)	,x
#define	PINOUT(x)	,x
#define PINARR(sz,x)	,x
#define	POUTSTR(msz,x)	,x
#define	POUTARR(msz,sz,x)	,x
#define POUTDYNARR(sz,t,x) 	,t x
#define PHIDDEN(x)
#define P1_EZ(x) x
#define	PIN_EZ(x)	,x
#define	POUT_EZ(x)	,x
#define	PINOUT_EZ(x)	,x
#define PINARR_EZ(sz,x)	,x
#define	POUTSTR_EZ(msz,x)	,x
#define	POUTARR_EZ(msz,sz,x)	,x
#define POUTDYNARR_EZ(sz,t,x) 	,t x
#define PHIDDEN_EZ(x)
#define P_NOPROTO
#define P1_VAR ...
#define PN_VAR ,...
#define	RPCPROG(pg)
#define RPCPROGATTR(pg, attr)

/* Prototypes not supported */

/****************************************************************************
 * GNU C extensions for better warning filtering.
 */
#if __GNUC__ >= 2 && __GNUC_MINOR__ >= 5
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN /**/
#endif

/****************************************************************************
 * Component and path name types and maximums
 *
 * Constants: 
 * KS_MAX_NAME_LEN	Maximum length of one component of a pathname.
 * KS_MAX_PNAME_LEN	Maximum length of a pathname.
 * 
 * Types:
 * ks_name_t		String buffer for one component of a pathname.
 * ks_pname_t		String buffer for a pathname.
 * ks_canon_pname_t	Same as ks_pname_t, except for RPC considerations.
 * ks_canon_pname_p_t	Same as "char *", except for RPC considerations.
 * ks_canon_pname_c_p_t	Same as ks_canon_pname_p_t except is a "const char *".
 *
 * Canonical pathnames are pathnames that separate components with forward
 * slashes (/).  The ks_canon_pname_... data types need to be used in EZ-RPC
 * interface definitions that pass pathnames since this type is the "hook" by
 * which pathnames are converted (XDR'd) into and out of canonical form on
 * Windows NT systems.  We supply our own XDR routines for them.
 *  
 * Note that by "hiding" ks_pname_t from EZ-RPC, we make sure that no RPC
 * interfaces use it.  (I.e., we _don't_ define an XDR routine for this type
 * and so any uses of it will result in "undefined global" errors.)
 *
 * Note well that all of this was constructed with a knowledge of the
 * idiosyncrasies of EZ-RPC.  Tamper with it at your own risk.
 */

#define KS_MAX_NAME_LEN 255
typedef KS_CHAR ks_name_t[KS_MAX_NAME_LEN+1];

#define KS_MAX_PNAME_LEN 1023
#if !defined(RPC) || defined(RPC_HDR)
typedef KS_CHAR ks_pname_t[KS_MAX_PNAME_LEN+1];
typedef KS_CHAR ks_canon_pname_t[KS_MAX_PNAME_LEN+1];
typedef KS_CHAR *ks_canon_pname_p_t;
typedef A_CONST KS_CHAR *ks_canon_pname_c_p_t;
#endif

/****************************************************************************
 * The null device name
 *
 * Both Unix and NT variants are simultaneously defined so we can accept
 * either in command line syntax, no matter what the platform.
 */

#define KS_DEVNULL_UNIX	"/dev/null"
#define KS_DEVNULL_NT	"NUL"

#define KS_DEVNULL KS_DEVNULL_UNIX

/****************************************************************************
 * Characters
 *
 * KS_ESC_CHAR		General purpose character for escaping special
 *			characters.
 * KS_LIST_SEP_CHAR	General purpose character for use in a list of strings
 *			(e.g., PATH).
 */

#define KS_ESC_CHAR		'\\'
#define KS_LIST_SEP_CHAR	':'	/* colon */
#define KS_LIST_SEP_STRING	":"	/* colon */ 

/****************************************************************************
 * Status codes
 *
 * KS_ST_BUF_OVFLW      Buffer overflow
 * KS_ST_ERR		Unspecified error.
 * KS_ST_NOT_FOUND	Requested item was not found; no error has been
 *			reported.
 * KS_ST_OK		No error.
 *
 * ks_status_t		Status code.  If a function returns a status code other
 *			than KS_ST_OK, all out parameters are undefined, unless
 *			otherwise indicated in function description.
 */

typedef int ks_status_t;

#define KS_ST_OK		0
#define KS_ST_ERR	  	1
#define KS_ST_NOT_FOUND  	2
#define KS_ST_BUF_OVFLW         3

/****************************************************************************
 * Useful macros
 */

#define KS_MIN(a,b)	((a) < (b) ? (a) : (b))
#define KS_MAX(a,b)	((a) < (b) ? (b) : (a))

#define KS_NULL_ARG

/****************************************************************************
 * Definitions to smooth the way for NT text/binary mode I/O
 */

#define ATRIA_WIN32_BINARY_MODE 
#define O_BINARY 0
#define O_TEXT   0

/****************************************************************************
 * Useful definitions for use with lex
 */
#define KS_YYTEXT_T char *

/****************************************************************************
 * Standardized types for user and group ID's.  These are needed
 * because different UNIX systems define different sizes for uid_t
 * and gid_t types.
 */

/*
 * Since we may pass credentials across the network in the future,
 * they need to be large enough to hold any reasonable number of
 * simultaneous groups.
 */
#define	CREDUTL_NGROUPS_MAX	32

typedef	u_long credutl_uid_t;
typedef u_long credutl_gid_t;
/* taken fron TBS */
#define KS_UID_DONTCARE	(0xffffffff)
#define KS_GID_DONTCARE	(0xffffffff)
#define KS_UID_NOBODY	(0xfffffffe)
#define KS_GID_NOBODY	(0xfffffffe)

#define KS_NOBODY_NAME "nobody"

/****************************************************************************
 * credutl_sid_t A representation of user and group identity that encapsulates
 *               multiple OS identity schemes.  The actual encapsulation scheme
 *               is private to credutl; users of credutl_sid_t's must use
 *               accessor functions defined here.
 * .length       Length in bytes of the entire credutl_sid_t, including the length 
 *               field itself.
 * .type         Type of identity held in the .sid field
 * .sid          Encapsulated representation of a particular type of identity.
 *               Integer values are store in a canonical byte ordering so that a
 *               credutl_sid_t can be safely transmitted and stored without
 *               regard to endian-ness.  
 *
 * credutl_sid_type_t     Enumeration of the possible of user identity types.
 *  CREDUTL_SID_TYPE_UNIX UNIX UID/GIDs.  
 *  CREDUTL_SID_TYPE_NT   Windows NT SIDs
 *  CREDUTL_SID_TYPE_SID_NOBOBY   Nobody SID
 *  CREDUTL_SID_TYPE_SID_DONTCARE Don't care SID
 *
 * CREDUTL_MAX_SID_LENGTH The maximum number of bytes that can be stored in a
 *                        credutl_sid_t's .sid field.  
 *
 * credutl_sid_str_t      String to hold display form of a SID.
 */

#define CREDUTL_MAX_SID_LENGTH 32

typedef enum credutl_sid_type_t {
    CREDUTL_SID_TYPE_NOBODY,
    CREDUTL_SID_TYPE_DONTCARE,
    CREDUTL_SID_TYPE_UNIX,
    CREDUTL_SID_TYPE_NT
} credutl_sid_type_t;

typedef struct credutl_sid_s {
    char length;
    char type;
    char sid[CREDUTL_MAX_SID_LENGTH];
} credutl_sid_t, *credutl_sid_p_t;

/*
 * For use in calculating length field of credutl_sid_t need to add
 * CREDUTL_SID_BASE_SIZE() to the length of the sid portion.
 */
#define CREDUTL_SID_BASE_SIZE(sid_p) (sizeof(sid_p->length) + \
                                      sizeof(sid_p->type))

#define CREDUTL_MAX_SID_STR 100

typedef char credutl_sid_str_t[CREDUTL_MAX_SID_STR + 1];

#define CREDUTL_SID_IS_NOBODY(sid)   ((sid)->type == CREDUTL_SID_TYPE_NOBODY)
#define CREDUTL_SID_IS_DONTCARE(sid) ((sid)->type == CREDUTL_SID_TYPE_DONTCARE)

IMPORT_DATA
const
credutl_sid_t CREDUTL_SID_NOBODY, CREDUTL_SID_DONTCARE;

#ifndef KS_MODE_MASK
#define KS_MODE_MASK 0x00000fff
#endif

#endif /* _KS_BASE_H_ */
/* $Id: 5bd36534.66ba11dc.9bbb.00:01:83:09:5e:0d $ */
