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
# Makefile for the simple formatting example app using the ZanyBlue.Text
# package without a properties file defining the messages.
#

TOP=../../..
GPRFLAGS=-aP$(TOP)/lib
APPNAME=x_load_facility
APPLICATION=$(TOP)/bin/$(APPNAME)

include $(TOP)/src/mkfile/conf.mk

all:
	gprbuild -p $(GPRFLAGS) -P $(APPNAME).gpr

run:	all
	$(APPLICATION)

run_%:	all
	ZB_LANG=$* $(APPLICATION)

clean::
	gprclean $(GPRFLAGS) -P $(APPNAME).gpr

include $(TOP)/src/mkfile/rules.mk
