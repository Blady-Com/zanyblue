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

.SUFFIXES: .adb-in

default:	all

#
# Clean should remove copied/generated files.
clean::	$(CLEAN_TARGS)

#
# Utility make target to print a Makefile define macro, e.g.,
#
#    make print_VERSION
#
print_%:
	@echo $($*)

#
# Rule to remove a file: workaround for Windows NT's "del" erroring out
# if given no arguments
%.rmfile:
	$(call DELETE,$*)

#
# Rule to apply the version definitions to a template file
%.adb:	%.adb-in
	gnatprep $(PREPFLAGS) $< $@
