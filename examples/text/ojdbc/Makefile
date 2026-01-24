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
# Makefile for the example app to print OJDBC message to the standard output
# stream.  NOTE: This generally only works for English, the Windows command
# window does not support UTF-8 ouptut.
#

TOP=../../..
GPRFLAGS=-aP$(TOP)/lib
ZBMCOMPILE=$(TOP)/bin/zbmcompile
APPNAME=x_ojdbc
APPLICATION=$(TOP)/bin/$(APPNAME)
PACKAGE=Oracle.Messages
GENERATED=oracle-messages.ads oracle-messages.adb

include $(TOP)/src/mkfile/conf.mk

all:	$(GENERATED)
	gprbuild -p $(GPRFLAGS) -P $(APPNAME).gpr

$(GENERATED):	$(wildcard mesg/ojdbc*.properties)
	$(ZBMCOMPILE) -v $(PACKAGE) -d mesg ojdbc

run:	all
	$(APPLICATION)

run_%:	all
	$(APPLICATION) -l$*

clean::
	gprclean $(GPRFLAGS) -P $(APPNAME).gpr

messages:	$(GENERATED)

include $(TOP)/src/mkfile/rules.mk
