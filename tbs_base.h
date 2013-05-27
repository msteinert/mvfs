/* * (C) Copyright IBM Corporation 1990, 2011. */
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
#if !defined(_TBS_BASE_H_)
#define _TBS_BASE_H_

/****************************************************************************
 * Include ks_base.h first to get the necessary macros defined
 */

#include <ks_base.h>

/****************************************************************************
 * System includes
 */

/* Required for definitions below */
#include <linux/time.h>

#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************************
 * Basic c definitions
 */

typedef u_char tbs_byte_t;
typedef int tbs_boolean_t;

/* Use a struct pointer to declare a private type handle to make compiler
 * perform type checking between different kinds of handles */
#define TBS_HANDLE KS_HANDLE

/****************************************************************************
 * Product-specific definitions                                             *
 ****************************************************************************/

/****************************************************************************
 * String types and maximums
 *
 * Constants: 
 * TBS_MAX_HOSTNAME_LEN	Maximum length of hostname.
 * TBS_MAX_NAME_LEN	Maximum length of one component of a pathname.
 * TBS_MAX_PNAME_LEN	Maximum length of a pathname.
 * TBS_MAX_SHELL_CMD_LEN
 *			Maximum length of a shell command line.
 * 
 * Types:
 * tbs_hostname_t	String buffer for hostname.
 * tbs_name_t		String buffer for one component of a pathname.
 * tbs_pname_t		String buffer for a pathname.
 * tbs_shell_cmd_t	String buffer for a shell command line.
 *
 * Note that the existence of TBS path and component name types and
 * constants that duplicate the KS equivalents is a historical artifact
 * New code should just use the KS versions.
 */

#define TBS_MAX_HOSTNAME_LEN   	63
typedef char tbs_hostname_t[TBS_MAX_HOSTNAME_LEN+1];
#define TBS_MAX_SHELL_CMD_LEN 	8191
typedef char tbs_shell_cmd_t[TBS_MAX_SHELL_CMD_LEN+1];

#define TBS_MAX_NAME_LEN   	KS_MAX_NAME_LEN
typedef ks_name_t tbs_name_t;
#define TBS_MAX_PNAME_LEN	KS_MAX_PNAME_LEN
typedef ks_pname_t tbs_pname_t;

/****************************************************************************
 * Status codes
 *
 * Conventions:
 * A routine that originates a status code of TBS_ST_OK or TBS_ST_NOT_FOUND
 * does not print any error message.  A routine that originates TBS_ST_ERR
 * must print an error message or log error information with a message
 * indicating that the user should check the log (i.e. TBS_LOG_INT_ERR*).
 * A routine that originates any other status code may or may not report an
 * error or log information.
 * When a function returns an error status, all other out parameters are
 * invalid.
 *
 * Constants:
 * TBS_ST_DBID_NOT_FOUND
 *			A database identifier (dbid) passed into a vob library
 *			call does not exist.  This can happen unintentionally
 *			when the caller passes an invalid dbid.  It can also
 *			happen during correct operation when a dbid that was
 *			once valid has been saved by the caller across
 *			transaction boundaries and the entity to which it
 *			referred has since been deleted by a concurrent user.
 * TBS_ST_ERR		Unspecified error; error has been reported.
 * TBS_ST_NOT_FOUND	Requested item was not found; no error has been
 *			reported.
 * TBS_ST_OK		No error.
 * TBS_ST_REPLAY_ALREADY_DONE
 *			VOB operation was not replayed because it had already
 *			been done; no error has been reported.
 * TBS_ST_REPLAY_MISSING_INPUT
 *			VOB operation was not replayed because a required
 *			input value was not available; no error has been
 *			reported.
 * TBS_ST_E*		Status codes approximately equivalent to unix errno
 *			values.
 * TBS_ST*		See below.
 *
 * Types:
 * tbs_status_t		Status code.
 */

typedef int tbs_status_t;

#define TBS_ST_OK		0		/* no error */

/* product-specific error values */
#define TBS_STBASE (tbs_status_t) 1000		/* non-unix value base */
#define TBS_ST_ERR	  	TBS_STBASE+1	/* Unspecified error Atria subsystem */
#define TBS_ST_NOT_FOUND  	TBS_STBASE+2	/* Not found error (no error reported) */
#define TBS_ST_NOT_AN_OBJ 	TBS_STBASE+3	/* Not an Atria filesystem object */
#define TBS_ST_TIMEOUT    	TBS_STBASE+4	/* Unable to communicate with remote server */
#define TBS_ST_DBID_NOT_FOUND	TBS_STBASE+5	/* Vob dbid not found */
#define TBS_ST_NOT_LICENSED	TBS_STBASE+6	/* Not licensed to run ClearCase */
#define TBS_ST_XREV_COMPAT	TBS_STBASE+7	/* Xrev compatibility problem */
#define TBS_ST_VIEW_ERR		TBS_STBASE+8	/* Trouble from VIEW */
#define TBS_ST_VOB_ERR		TBS_STBASE+9	/* Trouble from VOB */
#define TBS_ST_DB_ERR		TBS_STBASE+10	/* Trouble from DB */
#define TBS_ST_MFS_ERR		TBS_STBASE+11	/* Trouble from MFS */
#define TBS_ST_VIEW_NO_CFS_SET	TBS_STBASE+12	/* No config spec set in view */
#define TBS_ST_VIEW_STALE_DIR	TBS_STBASE+13	/* Directory not select in CFS */
#define TBS_ST_VIEW_NO_VER	TBS_STBASE+14	/* No version specified in CFS */
#define TBS_ST_VIEW_CLTXT_ERR	TBS_STBASE+15	/* Problem with cleartext */
#define TBS_ST_VIEW_UNKNOWN_VOB	TBS_STBASE+16	/* View unfamiliar with vob */
#define TBS_ST_WRONG_VOB	TBS_STBASE+17	/* Wrong vob oid found */
#define TBS_ST_WRONG_VIEW	TBS_STBASE+18	/* Wrong view oid found */
#define TBS_ST_VOB_NEEDS_RECOVERY \
				TBS_STBASE+19	/* VOB database needs recovery */
#define TBS_ST_DB_AREA_LOCKED	TBS_STBASE+20	/* VOB database area is locked */
#define TBS_ST_LICENSE_BUSY	TBS_STBASE+21	/* All licenses are allocated */
#define TBS_ST_VIEW_NEEDS_RECOVERY \
				TBS_STBASE+22   /* View DB needs recovering */
#define TBS_ST_VIEW_NEEDS_REFORMAT \
				TBS_STBASE+23   /* View DB needs reformatting */
#define TBS_ST_VOB_NEEDS_REFORMAT \
				TBS_STBASE+24	/* VOB database needs recovery */
#define TBS_ST_CONFIG_SPEC_ERR	TBS_STBASE+25	/* Trouble with config spec */
#define TBS_ST_RGY_DTM_MISMATCH TBS_STBASE+26	/* Registry changed between operations */
#define TBS_ST_REPLAY_ALREADY_DONE \
				TBS_STBASE+27	/* Operation has already been replayed */
#define TBS_ST_REPLAY_MISSING_INPUT \
				TBS_STBASE+28	/* Replay is missing required input */
#define TBS_ST_NOT_A_REGISTRY_SVR \
    	    	    	    	TBS_STBASE+29	/* Specified host is not a registry server */

#define TBS_ST_NOT_A_LICENSE_SVR \
    	    	    	    	TBS_STBASE+30	/* Specified host is not a license server */
#define TBS_ST_DB_TIMEOUT    	TBS_STBASE+31	/* Database timed out */
#define TBS_ST_NO_VIEW          TBS_STBASE+32   /* Operation requires a view */
#define TBS_ST_ALT_OK           TBS_STBASE+33   /* Alternate OK, success with 
						   non-terminal exception */
#define TBS_ST_CONTAINER_CHANGED \
				TBS_STBASE+34	/* Container name changed during operation.
                                                   Worth retrying the operation if replaying */
#define TBS_ST_ERR_RETRYABLE \
				TBS_STBASE+35	/* An error occurred that might be transient.
                                                   Worth retrying the operation if replaying */
#define TBS_ST_VIEW_STG_UNAVAIL \
                                TBS_STBASE+36   /* View storage directory or control files
                                                   unavailable (e.g. rmview of active view),
                                                   so don't retry.  Used specifically for
                                                   this case by MVFS, so don't overload. */
#define TBS_ST_VIEW_CONTACT_ERR \
				TBS_STBASE+37	/* Problem with view contact, communication
						   with view suspended, used for suppressing
						   error messages in cm/clearmake layers */
#define TBS_ST_HAS_CHKOUTS      TBS_STBASE+38   /* An operation, usually a multisite operation, 
						   is prevented from completing because there is a
						   checkout. */
#define TBS_ST_DENIED           TBS_STBASE+39   /* The execution of an operation 
						   is denied. Use TBS_ST_EACCESS or TBS_ST_EPERM 
                                                   where appropriate. TBS_ST_DENIED is used to
						   indicate a deny state is set denying request
						   mastership operations. Be careful about 
						   overloading. */
#define TBS_ST_OBJ_LOCKED       TBS_STBASE+40   /* An object lock prevents an operation. */
#define TBS_ST_NOT_MASTER       TBS_STBASE+41   /* The replica is not the master of an object. It was
						 * presumably expected to by the caller. */
#define TBS_ST_FEATURE_NOT_LICENSED \
                                TBS_STBASE+42   /* A feature not available with
                                                 * the current license / config.
                                                 * E.g., ELCC clients can't
                                                 * create DOs.
                                                 */
#define TBS_ST_FAM_FL_TOO_LOW   TBS_STBASE+43   /*
                                                 * The vob family feature 
                                                 * level is too low to perform 
                                                 * the requested operation.
                                                 */
#define TBS_ST_SIBREP_NOT_CONNECTED TBS_STBASE+44 /* The sibling replica that
                                                   * you are trying to connect
                                                   * to has been marked as not
                                                   * having IP connectivity.
                                                  */
#define TBS_ST_UCM_OBJECT       TBS_STBASE+45     /* The object is a UCM object.
                                                   * The operation is not permitted
                                                   * on UCM objects */
#define TBS_ST_ABORT		TBS_STBASE+46	  /* The operation was cancelled
                                                   * midway
                                                   */
#define TBS_ST_MASTER_XFERRED   TBS_STBASE+47     /* The mastership has xferred
                                                   * to the requesting replica
                                                   */
#define TBS_ST_NOT_LAST_VER     TBS_STBASE+48     /* not the last version on
                                                     the branch */
/* WARNING: When adding an error status you must also update TWO other files:
 *	tbs_cmsg.m
 *	tbs_errno.c
 */

/* unix error values -- correspond to unix errno values */
#define TBS_UNIXBASE		TBS_STBASE+50 	/* unix value base */
#define TBS_ST_EPERM		TBS_UNIXBASE+1	/* Not owner */
#define TBS_ST_ENOENT		TBS_UNIXBASE+2	/* No such file or directory */
#define TBS_ST_EIO		TBS_UNIXBASE+5	/* I/O error */
#define TBS_ST_ENXIO		TBS_UNIXBASE+6	/* No such device or address */
#define	TBS_ST_ENOMEM		TBS_UNIXBASE+12	/* Not enough memory */
#define TBS_ST_EACCES		TBS_UNIXBASE+13	/* Permission denied */
#define TBS_ST_EEXIST		TBS_UNIXBASE+17	/* File exists */
#define	TBS_ST_EXDEV		TBS_UNIXBASE+18	/* Cross-device link */
#define TBS_ST_ENODEV		TBS_UNIXBASE+19	/* No such device */
#define TBS_ST_ENOTDIR		TBS_UNIXBASE+20	/* Not a directory */
#define TBS_ST_EISDIR		TBS_UNIXBASE+21	/* Is a directory */
#define TBS_ST_EINVAL		TBS_UNIXBASE+22	/* Invalid argument */
#define TBS_ST_EFBIG		TBS_UNIXBASE+27	/* File too large */
#define TBS_ST_ENOSPC		TBS_UNIXBASE+28	/* No space left on device */
#define TBS_ST_EROFS		TBS_UNIXBASE+30	/* Read-only file system */
#define	TBS_ST_EMLINK		TBS_UNIXBASE+31	/* Too many links */
#define	TBS_ST_ELOOP		TBS_UNIXBASE+33	/* Too many levels of symbolic links */
#define TBS_ST_ENAMETOOLONG	TBS_UNIXBASE+34	/* File name too long */
#define TBS_ST_ENOTEMPTY	TBS_UNIXBASE+35	/* Directory not empty */
#define TBS_ST_EDQUOT		TBS_UNIXBASE+36	/* Disc quota exceeded */
#define TBS_ST_ESTALE		TBS_UNIXBASE+37	/* Stale VIEW file handle */
#define TBS_ST_EBUSY		TBS_UNIXBASE+38	/* Device/MVFS busy */
#define TBS_ST_EPFNOSUPPORT	TBS_UNIXBASE+39	/* Protocol family not supported */

#define TBS_STLIMIT		TBS_UNIXBASE+40 /* Highest status code */

/* WARNING: the SyncMgr values are greater than TBS_STLIMIT
 * This is OK, because there is no meaningful mapping to errno
 */
 /* SyncMgr base value */
#define TBS_ST_SMBASE                   TBS_STBASE+100 

/* Unspecified SyncMgr error */
#define TBS_ST_SM_ERR                   TBS_ST_SMBASE+1

 /* SyncMgr ran out of memory */ 
#define TBS_ST_SM_ENOMEM                TBS_ST_SMBASE+2

/* SyncMgr detected an internal error */
#define TBS_ST_SM_INT_ERR               TBS_ST_SMBASE+3

 /* SyncMgr detected error due to an invalid function argument */
#define TBS_ST_SM_EINVAL                TBS_ST_SMBASE+4

/* Unspecified SyncMgr db-subsystem error */
#define TBS_ST_SM_DB_ERR                TBS_ST_SMBASE+5

/* Something not found in SyncMgr db */
#define TBS_ST_SM_DB_NOT_FOUND          TBS_ST_SMBASE+6 

/* SyncMgr db ran out of space in the filesystem of its syncmgrdb directory */
#define TBS_ST_SM_DB_ENOSPC             TBS_ST_SMBASE+7

/* SyncMgr is not in the active state */
#define TBS_ST_SM_NOT_ACTIVE            TBS_ST_SMBASE+8

/* SyncMgr is shutting down */
#define TBS_ST_SM_SHUTDOWN              TBS_ST_SMBASE+9

/* SyncMgr reached the limit of the number of export jobs */
#define TBS_ST_SM_MAX_EXPORTS           TBS_ST_SMBASE+10

/* SyncMgr highest status code */
#define TBS_ST_SMLIMIT                  TBS_ST_SMBASE+11

/* WARNING: When adding an error status you must also update TWO other files:
 *	tbs_cmsg.m
 *	tbs_errno.c
 */

/* CCRC Status Codes
 * WARNING: There is no meaningful mapping to errno
 */

/* CCRC base value */
#define TBS_ST_CCRC_BASE                100000

/* Login to the CCRC server failed. */
#define TBS_ST_CCRC_LOGIN_FAILED        TBS_ST_CCRC_BASE + 1

/* CCRC Server session expire. */
#define TBS_ST_CCRC_SESSION_EXPIRED     TBS_ST_CCRC_BASE + 2

/* The view needs to be sync'd with the stream. */
#define TBS_ST_CCRC_VIEW_NEEDS_SYNC_FROM_STREAM TBS_ST_CCRC_BASE + 3

/* An activity with that name already exists. */
#define TBS_ST_CCRC_DUPLICATE_ACTIVITY_NAME TBS_ST_CCRC_BASE + 4

/* A stream with that name already exists. */
#define TBS_ST_CCRC_DUPLICATE_STREAM_NAME TBS_ST_CCRC_BASE + 5

/* The file needs to be merged with the latest on branch. */
#define TBS_ST_CCRC_NEEDS_MERGE_FROM_LATEST TBS_ST_CCRC_BASE + 6

/* CCRC Client is too old for this server. */
#define TBS_ST_CCRC_CLIENT_TOO_OLD      TBS_ST_CCRC_BASE + 7

/* CCRC Client is too new for this server. */
#define TBS_ST_CCRC_CLIENT_TOO_NEW      TBS_ST_CCRC_BASE + 8

/* The element being checked out is not the latest on the branch. */
#define TBS_ST_CCRC_CHECKOUT_NOT_LATEST TBS_ST_CCRC_BASE + 9

/* The version on the client does not match the version on the server. */
#define TBS_ST_CCRC_DISCORDANCE_VERSION TBS_ST_CCRC_BASE + 10

/* The VOB has been reformated. */
#define TBS_ST_CCRC_VOB_WAS_REFORMATTED TBS_ST_CCRC_BASE + 11

/* The activities selected for delivery are invalid. */
#define TBS_ST_CCRC_DELIVER_INVALID_ACTIVITIES TBS_ST_CCRC_BASE + 12

/* The parent stream's baselines are invalid for the current stream. */
#define TBS_ST_CCRC_INVALID_BASELINES   TBS_ST_CCRC_BASE + 13

/* A deliver operation is in progress on the stream in context with a
   target view that was not created by the current user or was not
   created by the current ClearCase web server. */
#define TBS_ST_CCRC_DELIVER_IN_PROGRESS TBS_ST_CCRC_BASE + 14

/* A Custom error code to prevent deliver if the source stream is 
   project Integration stream.*/
#define TBS_ST_CTRC_DELIVER_PROJINTSTREAM_ERR TBS_ST_CCRC_BASE + 15

#define TBS_ST_CTRC_SYNC_CANCELLED TBS_ST_CCRC_BASE + 16

#define TBS_ST_CTRC_SYNC_CANCEL_FAILED TBS_ST_CCRC_BASE + 17

#define TBS_ST_CTRC_SYNC_CONNECTION_FAILED TBS_ST_CCRC_BASE + 18

/* A custom error code to indicate that a checkout failed because
   the checkout branch was not mastered locally. */
#define TBS_ST_CCRC_CHECKOUT_BRANCH_NOT_MASTERED_LOCALLY TBS_ST_CCRC_BASE + 19

/* A custom error code to indicate that a checkout failed because
   the checkout branch type was not mastered locally. */
#define TBS_ST_CCRC_CHECKOUT_BRTYPE_NOT_MASTERED_LOCALLY TBS_ST_CCRC_BASE + 20

/* A custom error code to indicate that setConfigSpec failed because
   of some bad load rules */
#define TBS_ST_CCRC_BAD_LOAD_RULES TBS_ST_CCRC_BASE + 21

/* CCRC highest status code */
#define TBS_ST_CCRC_LIMIT                  TBS_ST_CCRC_BASE + 22

/* WARNING: When adding an error status you must also update TWO other files:
 *	tbs_cmsg.m
 *	tbs_errno.c
 */

/****************************************************************************
 * Object identifier
 *
 * Constants:
 *
 * TBS_DBID_NULL	Null value for type tbs_dbid_t.
 * TBS_OID_NULL		Null value for type tbs_oid_t.
 * TBS_UUID_NULL	Null value for type tbs_uuid_t.
 * TBS_MAX_UUID_STR 	Max string size for UUID encoded as a string.
 * TBS_MAX_OID_STR 	Max string size for OID encoded as a string.
 *
 * Types:
 *
 * tbs_dbid_t		Database identifier for an object, which is unique
 *			only within a vob database and for a limited amount of
 *			time.  A copy of an object in a vob replica has a
 *			different dbid from the original.  Dbid's may be
 *			reassigned when a database is reformatted, and are thus
 *			suitable for runtime use only.
 *
 * tbs_oid_str_t	String buffer big enough to hold object id encoded as a
 *			string.
 *
 * tbs_oid_t		Object identifier, which is unique across space and
 *			time, and is valid unless the corresponding object is
 *			deleted.  A copy of an object in a vob replica has the
 *			same object identifier as the original.
 *			Contents are private and subject to change; use
 *			tbs_oid calls to access.
 *			(See tbs_oid.h for details.)
 *
 * tbs_uuid_str_t	String buffer big enough to hold uuid encoded as a
 *			string.
 *
 * tbs_uuid_t	    	Universal unique identifier across space and time.
 *			Contents are private and subject to change; use
 *			tbs_uuid calls to access.
 *			(See tbs_uuid.h for details.)
 */

typedef struct tbs_uuid_s {
    /* Contents are PRIVATE; use tbs_uuid calls to access */
    ks_uint32_t     time_low;
    ks_uint16_t     time_mid;
    ks_uint16_t     time_hi_and_version;
    ks_byte_t       clock_seq_hi_and_reserved;
    ks_byte_t       clock_seq_low;
    ks_byte_t       node[6];
} tbs_uuid_t;

IMPORT_DATA
A_CONST
tbs_uuid_t TBS_UUID_NULL;

#define TBS_MAX_UUID_STR 50
typedef char tbs_uuid_str_t[TBS_MAX_UUID_STR+1];

typedef struct tbs_oid_s {
    /* Contents are PRIVATE; use tbs_oid calls to access */
    tbs_uuid_t obj_uuid;
} tbs_oid_t;

IMPORT_DATA
A_CONST
tbs_oid_t TBS_OID_NULL;

#define TBS_MAX_OID_STR 50
typedef char tbs_oid_str_t[TBS_MAX_OID_STR+1];

#define TBS_DBID_NULL ((tbs_dbid_t) 0)
typedef u_long tbs_dbid_t;

/****************************************************************************
 * File information
 *
 * Constants:
 * 
 * TBS_DEFAULT_BLOCKSIZE
 *			Default block size in fstat record.
 * TBS_GID_DONTCARE	Special value of group id indicating any group is ok.
 * TBS_GID_NOBODY	Null value for group id.
 * TBS_UID_DONTCARE	Special value of user id indicating any user is ok.
 * TBS_UID_NOBODY	Null value for user id.
 *
 * Types:
 * 
 * tbs_dirent_t		Directory entry.
 * .dbid		Dbid of entry.
 * .name		Name entry is catalogued under.
 * .offset		Offset of entry in directory.
 * 
 * tbs_fmode_t		File mode.
 *  TBS_FMODE_AUDITED_OBJ
 *			Special value indicating an audit object, whose object
 *			id should be changed if it is modified.
 *  TBS_FMODE_DONTCARE	Special value indicating any mode is suitable.
 *  TBS_FMODE_IRGRP	Group read permission.
 *  TBS_FMODE_IROTH	Other read permission.
 *  TBS_FMODE_IRUSR	Owner read permission.
 *  TBS_FMODE_IRWXG	Group read/write/execute permission.
 *  TBS_FMODE_IRWXO	Other read/write/execute permission.
 *  TBS_FMODE_IRWXU	Owner read/write/execute permission.
 *  TBS_FMODE_ISGID	Set group id on execution.
 *  TBS_FMODE_ISUID	Set user id on execution.
 *  TBS_FMODE_ISVTX	Sticky bit indicating a protected dir.
 *  TBS_FMODE_IWGRP	Group write permission.
 *  TBS_FMODE_IWOTH	Other write permission.
 *  TBS_FMODE_IWUSR	Owner write permission.
 *  TBS_FMODE_IXGRP	Group execute/search permission.
 *  TBS_FMODE_IXOTH	Other execute/search permission.
 *  TBS_FMODE_IXUSR	Owner execute/search permission.
 *  TBS_FMODE_NULL	Null value for type tbs_fmode_t.
 * 
 * tbs_fstat_t		File status (see tbs_*_fstat() routines).
 * .atime		Time of last access.
 * .blocks		Kb of disk used by file.
 * .blocksize		Preferred block size.
 * .ctime		Time of last status change.
 * .fsid		Device number.
 * .gid			Owner group id.
 * .mode		Protection mode bits.
 * .mtime		Time of last data modification.
 * .nlink		Number of hard links to this object.
 * .nodeid		Inode number.
 * .rdev		Special device number.
 * .size		File size in bytes.
 * .type		File type.
 * .uid			Owner user id.
 * 
 * tbs_ftype_t		File type.
 *  TBS_FTYPE_BLK	Block special.
 *  TBS_FTYPE_CHR	Character special.
 *  TBS_FTYPE_DIR	Directory.
 *  TBS_FTYPE_FIFO	Named pipe.
 *  TBS_FTYPE_LNK	Symbolic link.
 *  TBS_FTYPE_NULL	Null value for type tbs_ftype_t.
 *  TBS_FTYPE_REG	Regular file.
 *
 * tbs_fstat_db_t	File status as stored in the database
 * .ctime		Time of last status change.
 * .flags		Flags (including Windows attribute bits)
 * .gsid		Owner group id.
 * .mode		Protection mode bits.
 * .mtime		Time of last data modification.
 * .nlink		Number of hard links to this object.
 * .nodeid		Inode number.
 * .size		File size in bytes.
 * .type		File type.
 * .usid		Owner user id.
 * .xtime		Time of file creation.
 * 
 * Note: the nlink & mode fields are used to maintain versioned file line counts
 * (see the VER_LINE_COUNT* accessor macros)
 */

#define TBS_UID_NOBODY		(0xfffffffe)
#define TBS_GID_NOBODY		(0xfffffffe)
#define TBS_UID_DONTCARE	(0xffffffff)
#define TBS_GID_DONTCARE	(0xffffffff)

#define TBS_DEFAULT_BLOCKSIZE 1024

typedef u_long tbs_ftype_t;
#define TBS_FTYPE_NULL	(0L)
#define TBS_FTYPE_REG	(1L)
#define TBS_FTYPE_DIR	(2L)
#define TBS_FTYPE_BLK	(3L)
#define TBS_FTYPE_CHR	(4L)
#define TBS_FTYPE_LNK	(5L)
#define TBS_FTYPE_FIFO	(6L)

/* Note: we abuse VOB_MOD_MASTER_MODE_FLAG in vob_base_pvt.h as a value
 * that doesn't exist below until an oplog change allows for better 
 * implementation.
 */

typedef u_long tbs_fmode_t;
#define	TBS_FMODE_NULL		(0L)
#define	TBS_FMODE_DONTCARE	(0xffffffff)
#define	TBS_FMODE_AUDITED_OBJ	0x80000000
#define	TBS_FMODE_ISUID		0004000
#define	TBS_FMODE_ISGID		0002000
#define	TBS_FMODE_ISVTX		0001000
#define	TBS_FMODE_IRWXU		0000700
#define	  TBS_FMODE_IRUSR	0000400
#define	  TBS_FMODE_IWUSR	0000200
#define	  TBS_FMODE_IXUSR	0000100
#define	TBS_FMODE_IRWXG		0000070
#define	  TBS_FMODE_IRGRP	0000040
#define	  TBS_FMODE_IWGRP	0000020
#define	  TBS_FMODE_IXGRP	0000010
#define	TBS_FMODE_IRWXO		0000007
#define	  TBS_FMODE_IROTH	0000004
#define	  TBS_FMODE_IWOTH	0000002
#define   TBS_FMODE_IXOTH	0000001

#define TBS_FMODE_LEGAL_FS_BITS \
    (TBS_FMODE_ISUID | TBS_FMODE_ISGID | TBS_FMODE_ISVTX | \
     TBS_FMODE_IRWXU | TBS_FMODE_IRWXG | TBS_FMODE_IRWXO)

typedef struct tbs_fstat_s {
    tbs_ftype_t		type;
    tbs_fmode_t		mode;
    u_long		nlink;
    u_long		uid;
    u_long		gid;
    u_long		size;
    u_long		blocksize;
    u_long		rdev;
    u_long		blocks;
    u_long		fsid;
    u_long		nodeid;
    struct timeval	atime;
    struct timeval	mtime;
    struct timeval	ctime;
} tbs_fstat_t;

typedef struct tbs_fstat_db_s {
    tbs_ftype_t		type;
    tbs_fmode_t		mode;
    u_long		nlink;
    u_long		flags;
    credutl_sid_t	usid;
    credutl_sid_t	gsid;
    ks_off64_t		size;
    u_long		nodeid;
    struct timeval	xtime;
    struct timeval	atime;
    struct timeval	mtime;
    struct timeval	ctime;
} tbs_fstat_db_t;

typedef struct tbs_dirent_s {
    u_long		offset;
    tbs_dbid_t		dbid;
    char		*name;
} tbs_dirent_t;

/****************************************************************************
 * TBS_FSTAT_INIT
 * Initialize fstat structure.
 * IN	fstat_p		pointer to fstat structure
 */



#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
/* $Id: 59bf05d3.cb5d4b9a.9a5b.a5:a2:33:78:88:83 $ */
