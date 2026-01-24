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

GPRFLAGS+=-aP$(TOP)/src/libs3rd/AUnit/BUILD/lib/gnat
TEST_ARGS=-T $(TOP)
TEST_NAME=$(basename $(firstword $(wildcard *.gpr)))
CLEAN_DEPS=$(patsubst %,%.clean,$(TEST_DIRS))
CLEAN_FILES+=$(wildcard *.gcno)
CLEAN_FILES+=$(wildcard *.gcda)
CLEAN_FILES+=$(wildcard *.gcov)
CLEAN_FILES+=$(wildcard GNAT-TEMP-*.TMP)
CLEAN_TARGS+=$(patsubst %,%.clean,$(TEST_DIRS))
SALL_TARGS=$(patsubst %,%.sall,$(TEST_DIRS))

include $(TOP)/src/mkfile/conf.mk

all::
	gprbuild -p $(GPRFLAGS) -P $(TEST_NAME).gpr

gcov:
	$(MAKE) BUILD=Coverage all

check::	all
	$(TOP)/bin/$(TEST_NAME) $(TEST_ARGS)

sall::	all $(SALL_TARGS)

clean:: $(CLEAN_DEPS)
	gprclean $(GPRFLAGS) $(TEST_NAME).gpr

%.clean:
	$(MAKE) -C $* clean

%.sall:
	$(MAKE) -C $* sall

include $(TOP)/src/mkfile/rules.mk
