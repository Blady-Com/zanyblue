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
# Makefile for the ZanyBlue library and apps
#

TOP=..

GPRFLAGS+=-p
ZANYBLUEADB=$(TOP)/src/root/zanyblue.adb
SLOCDIRS=root os compiler text utils zbmcompile
INCDIR=$(TOP)/include
INCLUDE_SRC_DIRS=root compiler compiler/$(COMPILER) os os/$(OS) text utils
INCLUDE_SRCS=$(foreach d,$(INCLUDE_SRC_DIRS),\
                 $(patsubst $(TOP)/src/$d/%,%,\
                     $(wildcard $(TOP)/src/$d/*.ads) \
                     $(wildcard $(TOP)/src/$d/*generic*.adb)))
INCLUDE_FILES=$(patsubst %,$(TOP)/include/%,$(INCLUDE_SRCS))
GENERATED=$(INCLUDE_FILES)
APPDIRS=zbmcompile
APPTARGETS=$(patsubst %,%.app,$(APPDIRS))
CLEAN_DEPS=$(patsubst %,%.clean,$(APPDIRS) root test)
CLEAN_FILES+=$(wildcard bin/pylib/*.pyc)
CLEAN_FILES+=$(wildcard bin/pylib/zb/*.pyc)

# Include configuration definitions
include $(TOP)/src/mkfile/conf.mk

all: setup library applications

setup: $(ZANYBLUEADB) $(INCDIR) $(INCLUDE_FILES)

library:
	gprbuild $(GPRFLAGS) -P zanyblue_library.gpr

applications:	$(APPTARGETS)

%.app:
	$(MAKE) -C $*

clean::
	gprclean -P zanyblue_library.gpr

clean::	$(CLEAN_DEPS)

%.clean:
	$(MAKE) -C $* clean

check:
	$(MAKE) -C test check

xcheck:
	$(MAKE) -C test xcheck

force:

$(ZANYBLUEADB):
	$(MAKE) -C $(TOP)/src/root

$(INCDIR):
	$(call MKDIR,$(INCDIR))

$(TOP)/include/%:	$(TOP)/src/root/%
	$(call COPY,$(TOP)/src/root/$*,$(TOP)/include/$*)

$(TOP)/include/%:	$(TOP)/src/compiler/%
	$(call COPY,$(TOP)/src/compiler/$*,$(TOP)/include/$*)

$(TOP)/include/%:	$(TOP)/src/compiler/$(COMPILER)/%
	$(call COPY,$(TOP)/src/compiler/$(COMPILER)/$*,$(TOP)/include/$*)

$(TOP)/include/%:	$(TOP)/src/os/%
	$(call COPY,$(TOP)/src/os/$*,$(TOP)/include/$*)

$(TOP)/include/%:	$(TOP)/src/os/$(OS)/%
	$(call COPY,$(TOP)/src/os/$(OS)/$*,$(TOP)/include/$*)

$(TOP)/include/%:	$(TOP)/src/text/%
	$(call COPY,$(TOP)/src/text/$*,$(TOP)/include/$*)

$(TOP)/include/%:	$(TOP)/src/utils/%
	$(call COPY,$(TOP)/src/utils/$*,$(TOP)/include/$*)

# Include general rules.
include $(TOP)/src/mkfile/rules.mk

# Generation of "source lines of code" data for Hudson trending
include $(TOP)/src/mkfile/sloccount.mk

# Include the "gcov" target to build for coverage runs
include $(TOP)/src/mkfile/gcov.mk

# Include the "dist-bundles" target and support macros.
include $(TOP)/src/mkfile/bundles.mk
