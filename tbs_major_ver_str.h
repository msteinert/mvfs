/* * (C) Copyright IBM Corporation 1991, 2005 */
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

#if !defined(_TBS_MAJOR_VER_STR_H_)
#define _TBS_MAJOR_VER_STR_H_

/* When included by an Imakefile, only define release number macros */

/* ClearCase Feature Version IDs */
/* 
 * TBS_VER_MAJOR_ID	General product version number, printed by tools when
 *			"-version" requested.  This string has no leading "V".
 *			It has four decimal places; the fourth is 0 for GA
 *			releases.  Between external releases the 4th digit 
 *                      gets incremented to distinguish tools released as 
 *                      patches.
 *			Examples:
 *			    1.0.0.0
 *			    1.0.0.1
 *			    1.1.0.0
 *			    1.1.1.0
 */
#define TBS_RATIONAL_RELEASE_GROUP      "7"
#define TBS_RATIONAL_RELEASE_MAJOR      "0"
#define TBS_RATIONAL_RELEASE_MINOR      "0"
/* Increment RELEASE_BUILD for patches.  Should be '0' for GA release */
#define TBS_RATIONAL_RELEASE_BUILD      "5" 
#define TBS_RATIONAL_RELEASE_BU         ""

/*
 * Individual product IDs:
 */

#define TBS_RATIONAL_FCC_PRODUCT_ID     TBS_RATIONAL_RELEASE_GROUP \
                                        "." TBS_RATIONAL_RELEASE_MAJOR \
                                        "." TBS_RATIONAL_RELEASE_MINOR \
                                        "." TBS_RATIONAL_RELEASE_BUILD

/* For now, the CCLT product ID is the same as the FCC product ID */
#define TBS_RATIONAL_ELCC_PRODUCT_ID    TBS_RATIONAL_FCC_PRODUCT_ID

/* For now, the TBS_*_ID strings will be the same as TBS_RATIONAL_FCC_PRODUCT_ID */
#define TBS_VER_MAJOR_ID  TBS_RATIONAL_FCC_PRODUCT_ID

#endif /* _TBS_MAJOR_VER_STR_H_ */
/* $Id: c63bb870.752f11dd.83a6.00:01:83:20:15:36 $ */
