#!/bin/sh
#
# Simple script used to build ZanyBlue under Jenkins control: distribution
# build with included Google analytics code.
#

Here=$(dirname $0)

export PATH=.:/usr/gnat/2015/bin:/usr/local/bin:$PATH

# SVN_REVISION set by Jenkins
M_ARGS="SVN_VERSION=$SVN_REVISION"
# Enable Google Analytics via mkwebbundle
M_ARGS="$M_ARGS MKWEBBUNDLE_OPTIONS=-p"
# Copyright year is current year
M_ARGS="$M_ARGS COPYRIGHT_YEAR=`date +%Y`"

cd $Here/..
make distribution $M_ARGS
