#!/usr/bin/env python
#  -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#
#    * Neither the name of ZanyBlue nor the names of its contributors may
#      be used to endorse or promote products derived from this software
#      without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

"""
docsubst.py VAR1=VALUE1 VAR2=VALUE2 src dst

Copy src to dst replacing any occurences of the command line VARs with their
values.
"""

import sys


def error(fmt, *args):
    msg = fmt % args
    sys.stderr.write("%s\n" % msg)
    return 1


def main():
    variables = {}
    srcname = None
    dstname = None
    for arg in sys.argv[1:]:
        if '=' in arg:
            var, value = arg.split('=')
            variables[var] = value
        elif srcname is None:
            srcname = arg
        elif dstname is None:
            dstname = arg
        else:
            return error("Error: Unexpected command line argument \"%s\"", arg)
    if srcname is None:
        return error("Error: no source file defined")
    if dstname is None:
        return error("Error: no destination file defined")
    src = open(srcname, "r")
    dst = open(dstname, "w")
    for line in src:
        for key, value in variables.iteritems():
            line = line.replace(key, value)
        dst.write(line)
    dst.close()
    src.close()
    return 0

if __name__ == "__main__":
    sys.exit(main())
