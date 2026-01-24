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
# Makefile for the simple date/time printing application.
#

TOP=../../..
GPRFLAGS=-aP$(TOP)/lib
ZBMCOMPILE=$(TOP)/bin/zbmcompile
APPNAME=x_curtime
APPLICATION=$(TOP)/bin/$(APPNAME)
PACKAGE=Curtime_Messages
GENERATED=curtime_messages.ads curtime_messages.adb

include $(TOP)/src/mkfile/conf.mk

all:	$(GENERATED)
	gprbuild -p $(GPRFLAGS) -P $(APPNAME).gpr

$(GENERATED):	$(wildcard curtime*.properties)
	$(ZBMCOMPILE) -i -v $(PACKAGE) curtime

run:	all
	$(APPLICATION)

run_%:	all
	$(APPLICATION) -l$*

clean::
	gprclean $(GPRFLAGS) -P $(APPNAME).gpr

messages:	$(GENERATED)

include $(TOP)/src/mkfile/rules.mk
