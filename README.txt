/*
 * Copyright (C) 1999, 2011 IBM Corporation.
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
module for use with ClearCase 7.1.

By default the Installation Manager will attempt to rebuild the mvfs module.
If that does not work you can retry the install and select the option to 
install the pre-built mvfs module for you operating system version. 

If you try to load a mvfs module that should match your kernel
revision, but see errors like the following, you need to rebuild your
mvfs module:

FATAL: Error inserting mvfs
(/lib/modules/2.6.9-22.0.2.EL/kernel/fs/mvfs/mvfs.ko):
Invalid module format
Loading the MVFS was unsuccessful(8:1)
/opt/rational/clearcase/etc/clearcase: Loading MVFS failed
---- Please review /var/log/messages for information on the problem.
---- You may need to rebuild your kernel, rebuild your mvfs module,
---- and reboot your system. Information on how to do this can be found in
---- your Release Notes and file /var/adm/rational/clearcase/mvfs/mvfs_src/README.txt.

Instructions for rebuilding the kernel module can be found in the 
installation and configuration help.  What follows summarizes those 
instructions and provides some additional troubleshooting information.

REBUILD INSTRUCTIONS for ClearCase 7.1 on 2.6 series Linux kernels
(SLES9, SLES10, SLES11, RHEL4, RHEL5, RHEL6, MRG 1.3, Ubuntu 8.04.4,
Ubuntu 10.04)

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
     This command removes all of the target files including
     mvfs_param.mk.config

     or

     # make cleano
     which removes all the target files except mvfs_param.mk.config.

4b) Set options for the current running kernel:
    If you do not need to do any specific customizations, you can skip
    down to step 4f.

     # make mvfs_param.mk.config

4c) Beginning in CC 7.1, we use a java program to determine the current
    Linux kernel version and the corresponding build parameters.  If the
    make mvfs_param.mk.config step fails because it could not find the
    correct java executable, change the line in the Makefile that defines
    the value JAVA.  We use by default the version in
    /opt/ibm/RationalSDLC/common/java/jre/bin.  If this is not available,
    you may try the java used by the installer which can be found in 
    /opt/IBM/InstallationManager/eclipse/jre<version string>/jre/bin. You
    may also try any other version of java installed on your system.

4d) If the previous step fails, or if you want to use nonstandard values, 
    edit the mvfs_param.mk.config file manually.  Set the following values 
    in the file:

    CONFIG_MVFS=m
        This value is required for the build system to work correctly.

    RATL_EXTRAFLAGS := <appropriate value>
        This value is needed to distinguish between certain variants of
        kernel sources that cannot be differentiated based solely on the
        3-part kernel version (for example, 2.6.9).  It should contain the
        following entries:

        -DRATL_REDHAT or -DRATL_SUSE or -DRATL_UBUNTU as appropriate
        -DRATL_VENDOR_VER=<Version times 100 plus the update number>
            For example RHEL4 Update 5 -DRATL_VENDOR_VER=405
            For SLES10 SP2  -DRATL_VENDOR_VER=1002
            For RH MRG 1.3  -DRATL_VENDOR_VER=500
        -DRATL_EXTRA_VER=0
        -DRATL_COMPAT32
            Only include this for 64-bit systems.
    
    LINUX_KERNEL_DIR=<root of kernel source tree>
        The default directory is /lib/modules/`uname -r`/build.  This is
        a symlink to the actual build directory.  If you wish to build an
        mvfs module for a different kernel you may edit the
        LINUX_KERNEL_DIR value to point to the correct location.

4e) If you are running RHEL4.6 and have upgraded to errata kernel
    2.6.9-67.0.10.EL or later, uncomment the line in the Makefile that says:
    #RATL_EXTRAFLAGS += -DMVFS_REMOVE_FLOCK_DEFENSIVE_CODE

4f) Build and install the modules:

     # make
     # make install

    If the make phase fails because it could not find a java interpreter
    please see the instructions in 4c above.

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
