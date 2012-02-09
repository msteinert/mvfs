#
# Copyright (C) 2003, 2008 IBM Corporation.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA
#
# Author: IBM Corporation
# This module is part of the IBM (R) Rational (R) ClearCase (R)
# Multi-version file system (MVFS).
# For support, please visit http://www.ibm.com/software/support
#

# Set LINUX_KERNEL_DIR to point at the build tree for your kernel
# (commonly available via a symlink from /lib/modules/`uname -r`/build,
# or you can override it in mvfs_param.mk.config)
#
# If you wish to build for a system other than the one you are currently
# running, provide proper definitions for the following variables in
# mvfs_param.mk.config:
# LINUX_KERNEL_DIR= path to the kernel build tree
# INSTALL_DIR= path to the directory to install the mvfs module for insmod.
# If you are building for a 2.6 kernel with standard source and installation
# directories, you may just define RELEASE to match the target system.
# The command "man modules.conf" will provide information about the 
# default search rules used by modutils.
# As needed you may also redefine KERNEL_REV, RELEASE and MACH.

KERNEL_REV := $(shell uname -r |cut -d . -f 1-2)
RELEASE := $(shell uname -r)
MACH := $(shell uname -m)
MVFS_ARCH=($shell echo $(MACH) |sed -e s:i.86:i386:)
LINUX_KERNEL_DIR=/lib/modules/$(RELEASE)/build
ifeq ($(KERNEL_REV), 2.4)
INSTALL_DIR=/lib/modules/fs
else
INSTALL_DIR=/lib/modules/$(RELEASE)/kernel/fs/mvfs
endif
SRCDIR=.
MVFSSRCDIR=.

# The following is supposed to detect kernel ABI changes, at least on Red Hat.
KABI=$(shell rpm -q --provides kernel-$(RELEASE) | grep kABI )

DEBUG=
NM=nm
DEPMOD=/sbin/depmod
ifdef CLEARCASEHOME
CCHOME=${CLEARCASEHOME}
else
CCHOME=/opt/rational/clearcase
endif
CCINSTALL=${CCHOME}/install

ifndef KBUILD_EXTMOD
# The 2.6 kernel Makefile framework (which sets the KBUILD_EXTMOD variable)
# sets the "obj" variable).
obj=.
else
KERNEL_REV=2.6
endif

# All values that can be redefined in mvfs_param.mk.config must be defined
# before this point.
include $(obj)/mvfs_param.mk.config

KINC=$(LINUX_KERNEL_DIR)/include
ADAPTER_OBJECTS= \
	mvfs_param.o \
	mvfs_linux_asops.o \
	mvfs_linux_builtins.o \
	mvfs_linux_dops.o \
	mvfs_linux_fops.o \
	mvfs_linux_glue.o \
	mvfs_linux_iops.o \
	mvfs_linux_mvops.o \
	mvfs_linux_rpcglue.o \
	mvfs_linux_shadow.o \
	mvfs_linux_sops.o \
	mvfs_linux_utils.o \
	mvfs_linux_mdki.o

MVFS_OBJECTS= \
	mvfs_vfsops.o \
	mvfs_vnodeops.o \
	mvfs_clearops.o \
	mvfs_vwdirops.o \
	mvfs_procops.o \
	mvfs_utils.o \
	mvfs_mnode.o \
	mvfs_debug.o \
	mvfs_rdc.o \
	mvfs_auditops.o \
	mvfs_dncops.o \
	mvfs_clnt.o \
	mvfs_ntvwops.o \
	mvfs_rpcutl.o \
	mvfs_mioctl.o \
	mvfs_kmem.o \
	mvfs_copy.o \
	mvfs_transtype.o \
	xdr_tbs_kernel.o \
	xdr_view_kernel.o \
	tbs_errno.o \
	xdr_albd_kernel.o \
	xdr_ks_kernel.o \
	credutl_kernel.o \
	mvfs_mdep_linux.o \
	mvfs_tunables.o

MVFS_ARCH_64BIT=-DATRIA_LP64 -DRATL_COMPAT32
MVFS_ARCH_DEF.i386=-DATRIA_LINUX_IA32
MVFS_ARCH_DEF.s390=-DATRIA_LINUX_390
MVFS_ARCH_DEF.ppc64=-DATRIA_LINUX_PPC $(MVFS_ARCH_64BIT)
MVFS_ARCH_DEF.s390x=$(MVFS_ARCH_DEF.s390) $(MVFS_ARCH_64BIT)
MVFS_ARCH_DEF.x86_64=$(MVFS_ARCH_DEF.ia32) $(MVFS_ARCH_64BIT)
MVFS_ARCH_DEF=$(MVFS_ARCH_DEF.$(MVFS_ARCH))

MVFS_DEFS= \
	-DATRIA_ANSI_C \
	-DATRIA_LARGEFILE64_SOURCE \
	-DATRIA_LINUX \
	$(MVFS_ARCH_DEF) \
	-DATRIA_UNIX \
	-DI18N_MSGS \
	-DLINUX \
	-DXREV_CLIENTS_SUPPORT_V6_SERVERS \
	-DXREV_SERVERS_SUPPORT_V5_CLIENTS \
	-DXREV_SERVERS_SUPPORT_V6_CLIENTS \
	-D_LARGEFILE64_SOURCE


ifeq ($(KERNEL_REV), 2.6)
# ************************************************************
# 2.6 kernel builds
# ************************************************************


# This little bit of checking is to allow us to build properly on a
# 2.6.9-5.0.5.EL errata kernel for RHEL4.  
UPDATE_VER=$(shell echo $(RELEASE) |cut -d - -f 2 | cut -d . -f 1-2)
ifeq ($(UPDATE_VER), 5.0)
WARNING_FLAGS = -Wall -Wstrict-prototypes -Wunused  
else
WARNING_FLAGS = -Wall -Wstrict-prototypes -Wunused -Werror 
endif

all: mvfs_param.mk.config
	$(MAKE) -C $(LINUX_KERNEL_DIR) SUBDIRS=`pwd`

# If your system is running RHEL4 and has had certain errata installed to
# fix problems with flock on NFS, uncomment the following line.
#RATL_EXTRAFLAGS += -DMVFS_REMOVE_FLOCK_DEFENSIVE_CODE

OPT_SPACE= -Os
EXTRA_CFLAGS += $(WARNING_FLAGS) $(RATL_EXTRAFLAGS) -I$(obj) -D_KERNEL $(OPT_SPACE) $(MVFS_MOD_FLAGS)

ifeq ($(ARCH), x86_64)
# don't optimize for space in this file--GCC will generate a function
# call to memcmp which is unavailable in some kernel environments.
$(obj)/mvfs_linux_builtins.o : OPT_SPACE=
endif

VNODE_CONSTRUCTED_OBJS=timestamp.o
VNODE_BUILT_OBJECTS=$(addprefix $(obj)/,$(ADAPTER_OBJECTS))
VNODE_GEN_OBJECTS=$(addprefix $(obj)/,$(VNODE_CONSTRUCTED_OBJS))
$(VNODE_BUILT_OBJECTS) $(VNODE_GEN_OBJECTS) : MVFS_MOD_FLAGS=-Werror -Wunused


MVFS_BUILT_OBJECTS=$(addprefix $(obj)/,$(MVFS_OBJECTS))
$(MVFS_BUILT_OBJECTS) : MVFS_MOD_FLAGS=${MVFS_DEFS} -Wno-unused

obj-${CONFIG_MVFS} += mvfs.o
mvfs-objs := $(MVFS_OBJECTS) $(ADAPTER_OBJECTS) $(VNODE_CONSTRUCTED_OBJS)
$(obj)/mvfs_tunables.o: $(obj)/mvfs_when.h

$(obj)/mvfs_param.mk.config:
	cd $(obj) && $(CCINSTALL)/kernel_guess

$(obj)/timestamp.c: $(VNODE_BUILT_OBJECTS) $(MVFS_BUILT_OBJECTS)
	@echo GENERATE $@
	@TZ=GMT date '+const char mdki_vnode_build_time[] = "$$Date: %Y-%m-%d.%T (UTC) $$";' >$(obj)/timestamp.c
	@echo 'static const char mdki_vnode_build_host[] = "$$BuiltOn: '`uname -a`' $$";' >>$(obj)/timestamp.c

install: mvfs.ko
	mkdir -p $(INSTALL_DIR)
	install --backup --suffix=.save mvfs.ko $(INSTALL_DIR)/mvfs.ko
	$(DEPMOD)

clean: cleano
	-rm -f mvfs_param.mk.config

cleano:
	-rm -rf *.o *.kobj *.obj *.ko *.mod.? .*.cmd .tmp_versions

else
# ************************************************************
# 2.4 kernel builds
# ************************************************************



all: mvfs_param.mk.config mvfs.o

clean: cleano
	-rm -f mvfs_param.mk.config

cleano:
	-rm -f *.o

WARNING_FLAGS = -Wall -Wstrict-prototypes -Werror
CFLAGS=-D_KERNEL -D__KERNEL__ -DKERNEL -DMODULE $(RATL_EXTRAFLAGS) $(WARNING_FLAGS) $(MOD_FLAGS) -I$(KINC) -I. -Os $(DEBUG)

$(ADAPTER_OBJECTS) : MOD_FLAGS=-Werror -Wno-unused
$(MVFS_OBJECTS) : MOD_FLAGS=${MVFS_DEFS} -I$(SRCDIR) -I$(MVFSSRCDIR) -Wno-unused

ALLOBJS=$(MVFS_OBJECTS) $(ADAPTER_OBJECTS) timestamp.o

mvfs.o: $(ALLOBJS)
	$(CC) -Wl,-r -nostdlib -o $@ $(ALLOBJS) -lgcc

mvfs_tunables.o: mvfs_when.h

mvfs_param.o: mvfs_param.c

install: mvfs.o
	mkdir -p $(INSTALL_DIR)
	install --backup --suffix=.save mvfs.o $(INSTALL_DIR)/mvfs.o

mvfs_param.mk.config:
	 $(CCINSTALL)/kernel_guess

timestamp.c: $(ADAPTER_OBJECTS) $(MVFS_OBJECTS)
	TZ=GMT date '+const char mdki_vnode_build_time[] = "$$Date: %Y-%m-%d.%T (UTC) $$";' >timestamp.c
	echo 'static const char mdki_vnode_build_host[] = "$$BuiltOn: '`uname -a`' $$";' >>timestamp.c

.c.o:
	rm -f $@
	$(CC) $(CFLAGS) -DKBUILD_BASENAME=${@:.o=} -c -o $@ $<

endif
# $Id: 965bdc83.efb011dc.8d14.00:01:83:09:5e:0d $ 
