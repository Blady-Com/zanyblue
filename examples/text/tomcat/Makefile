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
# Makefile for the application based on the messages defined for the Apache
# Tomcat project.  This project has several facilities and the compilation
# to Ada optimizes the messages for locale based access.
#

TOP=../../..
GPRFLAGS=-aP$(TOP)/lib
ZBMCOMPILE=$(TOP)/bin/zbmcompile
ZBM_FLAGS=-O -i -v
APPNAME=x_tomcat
APPLICATION=$(TOP)/bin/$(APPNAME)
FACILITIES=ajp authenticator connector core ha ha-session ha-tcp host http11 \
	http jasper jsse loader manager mbeans membership naming naming-res \
	realm security servlet servlets session startup transport tribes users \
	util-buf util-http util-net util util-threads valves webapps
PACKAGE=Apache.Tomcat.Messages
GENERATED=apache-tomcat-messages.ads apache-tomcat-messages.adb

include $(TOP)/src/mkfile/conf.mk

all:	$(GENERATED)
	gprbuild -p $(GPRFLAGS) -P $(APPNAME).gpr

generated:	$(GENERATED)
$(GENERATED):	$(wildcard mesg/*.properties)
	$(ZBMCOMPILE) $(ZBM_FLAGS) $(PACKAGE) -d mesg $(FACILITIES)

run:	all
	$(APPLICATION)

run_%:	all
	$(APPLICATION) -l$*

clean::
	gprclean $(GPRFLAGS) -P $(APPNAME).gpr

messages:	$(GENERATED)

include $(TOP)/src/mkfile/rules.mk
