/* * (C) Copyright IBM Corporation 1995, 2005. */
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

#if !defined(_KS_EXTERN_H_)
#define _KS_EXTERN_H_

/*
 * No DLL declarations for kernel or non-Win32 code
 */
#if !defined(EXPORT_FUNCTION)
#define EXPORT_FUNCTION
#endif
#if !defined(EXPORT_FUNCTION_CXX)
#define EXPORT_FUNCTION_CXX
#endif
#if !defined(EXPORT_DATA)
#define EXPORT_DATA
#endif
#if !defined(EXPORT_DATA_CXX)
#define EXPORT_DATA_CXX
#endif
#if !defined(EXPORT_CLASS)
#define EXPORT_CLASS
#endif

#if !defined(IMPORT_FUNCTION)
#define IMPORT_FUNCTION extern
#endif
#if !defined(IMPORT_FUNCTION_CXX)
#define IMPORT_FUNCTION_CXX
#endif
#if !defined(IMPORT_DATA)
#define IMPORT_DATA extern
#endif
#if !defined(IMPORT_DATA_CXX)
#define IMPORT_DATA_CXX
#endif
#if !defined(IMPORT_CLASS)
#define IMPORT_CLASS 
#endif

#if !defined(EXTERN)

#define EXTERN extern

/*
 * Definition to use when you know that the adorned symbol is defined in a
 * static library.  This regardless of operating system (i.e. on Windows the
 * declspec(import/export) need not apply).
 */
#define EXTERN_S extern

#endif /* EXTERN */
#endif /* _KS_EXTERN_H_ */
/* $Id: 1242ac14.637a11da.8655.00:01:83:a6:4c:63 $ */
