/* * (C) Copyright IBM Corporation 1999, 2005 */
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

#if !defined(_TBS_ERRNO_H_)
#define _TBS_ERRNO_H_

#include <linux/types.h>
#include "tbs_base.h"

/* The tbs_errno set of routines are those tbs error reporting
 * routines that are also compiled for the kernel.
 *
 * THIS HEADER FILE MUST BE ABLE TO BE INCLUDED IN THE KERNEL
 * 
 *   error reporting:
 *	tbs_status2errno	convert status code to unix errno
 *	tbs_errno2status	convert unix errno to status code
 */

/****************************************************************************
 * tbs_status2errno
 * Convert a tbs_status_t status code to a unix errno value (if possible).
 * IN	status		TBS_ST_xxx status code
 * RESULT:		unix errno value
 */

EXTERN int
tbs_status2errno (P1(tbs_status_t status));

/****************************************************************************
 * tbs_errno2status
 * Convert a unix errno value to a tbs_status_t status code (if possible).
 * IN	st_errno	an errno value
 * RESULT:		status code
 */

EXTERN tbs_status_t
tbs_errno2status (P1(int st_errno));

#endif /* _TBS_ERRNO_H_ */
/* $Id: dfa2a894.637911da.8655.00:01:83:a6:4c:63 $ */
