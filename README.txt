/*
 * Copyright (C) 1999, 2008 IBM Corporation.
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
This file contains instructions on how to rebuild the mvfs
module for use with ClearCase 7.0.

If you are running a Linux kernel that is compatible with one of the
pre-built sets of modules, you should not need to rebuild the modules.
Use the binary versions shipped in this release instead (this is the
default option during the install).

If you try to load a mvfs module that should match your kernel
revision, but see errors like the following, you need to rebuild your
mvfs module:

    /lib/modules/fs/mvfs.o: unresolved symbol d_rehash_0680a5aa
    /lib/modules/fs/mvfs.o: unresolved symbol d_alloc_054621f7
    /lib/modules/fs/mvfs.o: unresolved symbol __mntput_def2fc0a
    /lib/modules/fs/mvfs.o: unresolved symbol inode_setattr_e5f5e28a

Instructions for rebuilding the kernel module can be found in the "IBM
Rational ClearCase and IBM Rational ClearCase Multisite: Installation
Guide" manual (/opt/rational/clearcase/doc/books/cpf_install.pdf).  What
follows summarizes those instructions and provides some additional
troubleshooting information.

REBUILD INSTRUCTIONS for ClearCase 7.0 on 2.4 series Linux kernels
(SLES8, RHEL3)

After you install ClearCase, boot the kernel you want to use and become
the superuser.  Then build the mvfs module for your 2.4
series Linux kernel as follows (if you're using a 2.6 series Linux
kernel, see below)

1) Stop ClearCase

    # /opt/rational/clearcase/etc/clearcase stop

2) Verify that neither the mvfs.o nor vnode.o files exist in the
   directory /lib/modules/`uname -r`/fs.  If you find either file,
   delete or rename it. Also delete /lib/modules/fs/vnode.o if present.
   If these object files remain from a previous
   version of ClearCase, they will interfere with loading of the new
   version.  Warning: Do NOT remove /lib/modules/fs/mvfs.o.

3) Verify that you have the required package installed to allow building
   kernel modules:

   The standard distribution-built kernels come with a package named
   like kernel-source-*.  This command should show that package name:

     # rpm -q -f /lib/modules/`uname -r`/build/Makefile

   If you have rebuilt a custom kernel yourself, the standard kernel
   build and install tools should have setup the correct environment so
   that /lib/modules/`uname -r`/build is a symlink to the proper source
   directory to be used when rebuilding the mvfs module.

4) Change directory to the MVFS build directory:

     # cd /var/adm/rational/clearcase/mvfs/mvfs_src

5) Build and install the mvfs module by doing the following:

5a) Clean up from previous builds:

     # make clean

5b) Set options for the current running kernel, choose the linux source
    directory (the default should be correct):

     # make mvfs_param.mk.config

5c) If the previous step fails, create the mvfs_param.mk.config file
    manually.  Set the following two values in the file:

    RATL_EXTRAFLAGS := <appropriate value>
    LINUX_KERNEL_DIR=<root of kernel source tree>

    RATL_EXTRAFLAGS is needed to distinguish between certain variants 
    of kernel sources that cannot be differentiated based solely on
    the 3-part kernel version (for example, 2.4.21).

    To see which flags might be needed for your kernel, look at the
    RATL_EXTRAFLAGS array in /opt/rational/clearcase/install/Kernel.pl.
    Set your RATL_EXTRAFLAGS variable appropriately.

5d) If you built your kernel with a compiler other than the "cc" defined
    in your search path, you must specify the same compiler to use for
    building the mvfs module.  Add a line to
    mvfs_param.mk.config specifying the compiler to be used. The
    following example specifies the gcc compiler:

    CC=/usr/local/bin/gcc

5e) Build and install the modules:

     # make
     # make install

    You should not see any errors like the following: 

/usr/include/linux/string.h:8:2: #warning Using kernel header in userland!
/usr/include/linux/autoconf.h:1:2: #error Invalid kernel header included in userspace

    Such errors mean that the LINUX_KERNEL_DIR argument you specified
    was not a properly configured kernel source directory.  You must
    either have the correct packages installed for the kernel you are
    running, or you must use the standard kernel build and install tools
    to rebuild the kernel.

    If a module compilation fails with errors about undefined functions
    or structure members, the script may have guessed the wrong kernel
    variant.  Look for code that is conditional on RATL_REDHAT,
    RATL_SUSE, RATL_VENDOR_VER, or RATL_EXTRA_VER, and check to see
    which settings turn on code that is compatible with your kernel.
    Then edit mvfs_param.mk.config to set these flags in
    RATL_EXTRAFLAGS, and rebuild the modules as follows:

     # make cleano
     # make
     # make install

6) Restart ClearCase:

     # /opt/rational/clearcase/etc/clearcase start

Possible warnings:

  If you get a warning when loading the mvfs module about
  mismatched kernel versions, you can safely ignore it if the module
  loads successfully.  If you get a warning about loading GCC-2 compiled
  modules in a GCC-3 kernel, you must rebuild the mvfs module
  with a compiler version that matches your kernel's compiler.

REBUILD INSTRUCTIONS for ClearCase 7.0 on 2.6 series Linux kernels
(SLES9, SLES10, RHEL4 or RHEL5)

After you install ClearCase, boot the kernel you want to use and become
the superuser. Then build the mvfs module for your 2.6 series
Linux kernel as follows:

1) Stop ClearCase

    # /opt/rational/clearcase/etc/clearcase stop

2) Verify that you have the required files installed to allow building
   kernel modules:

   For distribution-supplied kernels, check that you have loaded the
   proper packages:

     # rpm -q -f /lib/modules/`uname -r`/build/Makefile
     # rpm -q -f /lib/modules/`uname -r`/build/Module.symvers

   The standard kernels provide packages with names of the form
   kernel-devel-* on RedHat and kernel-source-* and kernel-syms-*,
   respectively, on SUSE.

   If you have rebuilt the kernel yourself, the standard kernel build
   and install tools should have set up the correct environment
   (including the symbolic link) for rebuilding the mvfs module.

3) Change directory to the MVFS build directory:

     # cd /var/adm/rational/clearcase/mvfs/mvfs_src

4) Build and install the mvfs module by doing the following:

4a) Clean up from previous builds:

     # make clean

4b) Set options for the current running kernel, choose the linux source
    directory (the default should be correct):

     # make mvfs_param.mk.config

4c) If the previous step fails, create the mvfs_param.mk.config file
    manually.  Set the following values in the file:

    CONFIG_MVFS=m
    RATL_EXTRAFLAGS := <appropriate value>
    LINUX_KERNEL_DIR=<root of kernel source tree>

    RATL_EXTRAFLAGS is needed to distinguish between certain variants of
    kernel sources that cannot be differentiated based solely on the
    3-part kernel version (for example, 2.6.9).

    To see which flags might be needed for your kernel, look at the
    RATL_EXTRAFLAGS array in /opt/rational/clearcase/install/Kernel.pl.
    Set your RATL_EXTRAFLAGS variable appropriately.

4d) If you are running RHEL4 or and have upgraded to errata kernels to
    resolve certain problems in flock calls over NFS, uncomment the 
    line in the Makefile that says:
    #RATL_EXTRAFLAGS += -DMVFS_REMOVE_FLOCK_DEFENSIVE_CODE

4e) Build and install the modules:

     # make
     # make install

    You should not see any errors like the following: 

Makefile:438: .config: No such file or directory

    Such errors mean that the LINUX_KERNEL_DIR argument you specified
    was not a properly configured kernel build directory.  You must
    either have the correct packages installed for the kernel you are
    running, or you must use the standard kernel build and install tools
    to rebuild the kernel.

    If a module compilation fails with errors about undefined functions
    or structure members, the script may have guessed the wrong kernel
    variant.  Look for code that is conditional on RATL_REDHAT,
    RATL_SUSE, RATL_VENDOR_VER, or RATL_EXTRA_VER, and check to see
    which settings turn on code that is compatible with your kernel.
    Then edit mvfs_param.mk.config to set these flags in
    RATL_EXTRAFLAGS, and rebuild the modules as follows:

     # make cleano
     # make
     # make install

5) Restart ClearCase:

     # /opt/rational/clearcase/etc/clearcase start

Possible errors:

  If you get a failure when loading a module about mismatched version
  magic strings, you may need to rebuild the module with a compiler
  version that matches your kernel's compiler.

Possible warnings:

  If you get warnings when loading the modules about mismatched kernel
  versions or about loading GCC-2 compiled modules in a GCC-3 kernel,
  you can safely ignore them if the modules load successfully.
