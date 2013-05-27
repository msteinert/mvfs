/* * (C) Copyright IBM Corporation 2006, 2007. */
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
#ifndef MVFS_AUDITDATA_H_
#define MVFS_AUDITDATA_H_
/*
 */

#include "mvfs_systm.h"
#include "mvfs_base.h"

/* All data for managing the audit subsystem is now contained in this 
 * mvfs_audit_data structure. */
typedef struct mvfs_audit_data
{

    LOCK_T mfs_aflock;		/* mfs_aflock protects the mfs_aflist */
    LISTHDR(mfs_aflist, mfs_auditfile_t);

    /*
     * Audit sequence number.  Used in choid logic to detect 'different audits'
     * which require a choid even if the build handle is identical with a 
     * previous translation (to a different audit).
     * This sequence number is incremented under the "mvfs_audgenlock".
     */

    SPLOCK_T mvfs_audgenlock;
    u_long mvfs_audit_gen;	/* Audit sequencer */

#ifdef MVFS_DEBUG
    /* Enable for extra debugging of noaudit */
    int mvfs_debug_no_audit_detail; 
#endif

} mvfs_audit_data_t;

#endif /* MVFS_AUDITDATA_H_ */

/* $Id: f1215446.9c1e11dd.9a62.00:01:83:29:c0:fc $ */
