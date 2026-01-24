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
# Makefile for the example application using the messages defined for Sun's
# ORB Java package.
#

TOP=../../..
GPRFLAGS=-aP$(TOP)/lib
ZBMCOMPILE=$(TOP)/bin/zbmcompile
APPNAME=x_sunorb
APPLICATION=$(TOP)/bin/$(APPNAME)
PACKAGE=Sun.ORB_Messages
GENERATED=sun-orb_messages.ads sun-orb_messages.adb

include $(TOP)/src/mkfile/conf.mk

all:	$(GENERATED)
	gprbuild -p $(GPRFLAGS) -P $(APPNAME).gpr

$(GENERATED):	$(wildcard mesg/sunorb*.properties)
	$(ZBMCOMPILE) -i -v $(PACKAGE) -d mesg sunorb

run:	all
	$(APPLICATION)

run_%:	all
	$(APPLICATION) -l$*

clean::
	gprclean $(GPRFLAGS) -P $(APPNAME).gpr

messages:	$(GENERATED)

include $(TOP)/src/mkfile/rules.mk
