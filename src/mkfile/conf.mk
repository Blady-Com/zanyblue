#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#  Copyright (C) 2009  Michael Rohan <michael@zanyblue.com>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

#
# Configuation macros ...
# The choices are defined by the types in zanyblue_common.gpr, repeated here
# to simplify direct configuration (i.e., don't want to read another file).

# The choices for OS are "unix" and "Windows_NT".  This only changes the name
# returned by the ZanyBlue.OS.OS_Name function and, if ZanyBlue is built
# as a shared library, the package ZanyBlue.OS.Ld_Run_Path will add the
# ld.so runtime path "$ORIGIN/../lib", i.e., load shared libraries from
# the "lib" directory one from from the directory containing the executable.
# The value is defined to be "unix" if not already defined, i.e., Windows
# defines the environment variable OS with value "Windows_NT", if this is
# defined, use it.
OS ?= unix

# The choices for TYPE are "static", for a static library, and "relocatable",
# for a shared/dynamic library.
#TYPE=relocatable
TYPE=static

# The choices for BUILD are "Debug", for a debug build, "Production" for an
# optimized production build, and  "Coverage" for a coverage enable build via
# "gcov".
#BUILD=Production
BUILD=Debug

# The choices are "gnat" for the GNAT compiler system and "template" for the
# generic compiler definitions.
COMPILER=gnat

# ---------------------------------------------------------------------------
# Macros derived from the configuration macros.
GPRFLAGS+=-XOS=$(OS)
GPRFLAGS+=-XTYPE=$(TYPE)
GPRFLAGS+=-XBUILD=$(BUILD)
GPRFLAGS+=-aP$(TOP)/lib
GPRFLAGS+=-aP$(TOP)/src

include $(TOP)/src/mkfile/$(OS).mk

#
# Makefile definitions for the ZanyBlue version macros
#

V_MAJOR=0
V_MINOR=1
V_PATCH=1
V_STATUS=Beta
PREPFLAGS+=-DV_MAJOR=$(V_MAJOR)
PREPFLAGS+=-DV_MINOR=$(V_MINOR)
PREPFLAGS+=-DV_PATCH=$(V_PATCH)
PREPFLAGS+=-DV_STATUS=$(V_STATUS)
PREPFLAGS+=-DVERSION=$(VERSION)
PREPFLAGS+=-DSVN_VERSION='"$(SVN_VERSION)"'
PREPFLAGS+=-DCOPYRIGHT_YEAR=$(COPYRIGHT_YEAR)

-include $(TOP)/src/mkfile/defs.mk

# If defs.mk doesn't exist, i.e., this is not a source tar ball
# snapshot, query the environment for the svn version and copyright info.
ifndef SVN_VERSION
VERSION=$(V_MAJOR).$(V_MINOR).$(V_PATCH)
SVN_VERSION=$(shell svnversion $(TOP))
COPYRIGHT_YEAR=$(CURRENT_YEAR)
endif

# Add coverage generated files to the clean list
CLEAN_FILES+=$(wildcard obj/*.gcno)
CLEAN_FILES+=$(wildcard obj/*.gcda)
CLEAN_FILES+=$(wildcard obj/*.gcov)
CLEAN_FILES+=$(wildcard *.gcno)
CLEAN_FILES+=$(wildcard *.gcda)
CLEAN_FILES+=$(wildcard *.gcov)
CLEAN_FILES+=$(wildcard *~)

#
# Remove editor backup files
CLEAN_FILES+=$(wildcard *~)

#
# Generated files should be removed ...
CLEAN_FILES+=$(foreach i,$(GENERATED),$(wildcard $i))

#
# General cleaning rules
CLEAN_TARGS=$(patsubst %,%.rmfile,$(CLEAN_FILES))
