#
# Copyright (C) 2008, Michael Rohan
#
# ZanyBlue is free software;  you can  redistribute it and/or modify it
# under terms of the  GNU General Public License as published  by the Free
# Software  Foundation;  either version 2,  or (at your option) any later
# version.  ZanyBlue is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for  more details.  You should have  received  a copy of the GNU General
# Public License  distributed with ZanyBlue;  see file COPYING.  If not, write
# to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor,
# Boston, MA 02110-1301, USA.
#
# Makefile for the ZanyBlue documentation
#

TOP=../..
DOCDIR=$(TOP)/doc
HTMLDIR=$(DOCDIR)/html
CLEAN_FILES+=$(wildcard *.aux)
CLEAN_FILES+=$(wildcard *.log)
CLEAN_FILES+=$(wildcard *.dvi)
CLEAN_FILES+=$(wildcard *.toc)
CLEAN_FILES+=zbdefs.tex

all::	dvifile

dvifile:	zanyblue.dvi

zanyblue.dvi:	zbdefs.tex force
	latex zanyblue.tex

install:	zbdefs.tex pdffile html

pdffile:
	pdflatex zanyblue.tex
	makeindex zanyblue.idx
	pdflatex zanyblue.tex
	makeindex zanyblue.idx
	pdflatex zanyblue.tex
	makeindex zanyblue.idx
	mv zanyblue.pdf $(DOCDIR)/zanyblue.pdf

html:
	latex2html -local_icons -dir $(HTMLDIR) zanyblue.tex 

clean::
	rm -f $(CLEAN_FILES)

force:

include $(TOP)/src/zanyblue-version.mk

zbdefs.tex:
	sed -e s/VERSION/$(VERSION)/ -e s/V_STATUS/$(V_STATUS)/ -e s/SVN_VERSION/$(SVN_VERSION)/ zbdefs.tex-in > zbdefs.tex
