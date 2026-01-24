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

TOP=../..

APPNAME=zbmcompile
ZBMCOMPILE=$(TOP)/bin/$(APPNAME)
RUNV=-v
MESGDIR=mesg
FACILTIES=zbmcompile zbmcode
PACKAGE=ZBMCompile.Messages
RUNARGS=$(RUNV) -i $(PACKAGE) -d $(MESGDIR) $(FACILTIES)
CLEAN_FILES+=$(wildcard *.gcno)
CLEAN_FILES+=$(wildcard *.gcda)
CLEAN_FILES+=$(wildcard *.gcov)
XGENERATED=zbmcompile-messages.ads zbmcompile-messages.adb

# Include configuration definitions
include $(TOP)/src/mkfile/conf.mk

all:	$(XGENERATED)
	gprbuild -p $(GPRFLAGS) -P $(APPNAME)

messages:	$(XGENERATED)

$(XGENERATED):	$(wildcard $(MESGDIR)/*)
	$(warning "******************************************************")
	$(warning "****                  INFORMATION                 ****")
	$(warning "****                                              ****")
	$(warning "**** Need to regenerate the messages source files ****")
	$(warning "**** Expect two builds of zbmcompile              ****")
	$(warning "****                                              ****")
	$(warning "******************************************************")
	gprbuild $(GPRFLAGS) -P $(APPNAME)
	$(ZBMCOMPILE) $(RUNARGS)

clean::
	gprclean $(GPRFLAGS) -P $(APPNAME)

vclean:
	$(MAKE) GENERATED="$(XGENERATED)" clean

# Include general rules.
include $(TOP)/src/mkfile/rules.mk
