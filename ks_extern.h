/* * (C) Copyright IBM Corporation 1995, 2007. */
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

/*
 * Mechanism for toggling export or import declarations on client library
 * imported or implementation exported data. The implementation should
 * mark these symbols as "export", clients should see these symbols as
 * import. KS_EXPORT_AS_IMPLEMENTATION is defined in the {I,X}makefile
 * for this subsystem.
 */

#ifdef KS_EXPORT_AS_IMPLEMENTATION
#define KS_EXPORT_CLASS           EXPORT_CLASS
#define KS_EXPORT_FUNCTION        EXPORT_FUNCTION
#define KS_EXPORT_DATA            EXPORT_DATA
#else
#define KS_EXPORT_CLASS           IMPORT_CLASS
#define KS_EXPORT_FUNCTION        IMPORT_FUNCTION
#define KS_EXPORT_DATA            IMPORT_DATA
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
/* $Id: ce4120ce.9c1d11dd.9a62.00:01:83:29:c0:fc $ */
