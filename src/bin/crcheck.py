#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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
Check that the copyright year has been updated for modified files.
"""

import codecs
import os
import re
import subprocess
import time

_TOP = os.path.abspath(__file__)
for i in range(3):
   _TOP = os.path.dirname(_TOP)
_COPYRIGHT1_RC = re.compile(r'.*Copyright .c. (?P<year>\d+), '
                            r'Michael Rohan .*')
_COPYRIGHT2_RC = re.compile(r'.*Copyright .c. (?P<year1>\d+), '
                            r'(?P<year2>\d+), Michael Rohan .*')
_COPYRIGHT3_RC = re.compile(r'.*Copyright \(c\) (?P<malformed>.+) '
                            r'Michael Rohan .*')
_COPYRIGHT_YEAR = str(time.localtime().tm_year)

def crcheck_file(tld, filename, new_p):
    if not os.path.isfile(filename):
       return
    try:
       with codecs.open(os.path.join(tld, filename), "r", "utf-8") as f:
           for line in f.readlines():
               line = line.strip()
               m = _COPYRIGHT1_RC.match(line)
               if m is not None:
                   year = m.group('year')
                   if year != _COPYRIGHT_YEAR:
                       print filename
                   return
               m = _COPYRIGHT2_RC.match(line)
               if m is not None:
                   year1 = m.group('year1')
                   year2 = m.group('year2')
                   if new_p or year2 != _COPYRIGHT_YEAR or year1 == year2:
                       print filename
                   return
               if len(line) == 0:
                   print filename
                   return
    except Exception, e:
        print "Error failed to inspect \"{0}\": {1}".format(
            filename,
            str(e),
        )

def crcheck(tld):
    process = subprocess.Popen('svn status',
                               cwd=tld,
                               shell=True,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    result = process.wait()
    local_files = []
    for line in stdout.split("\n"):
        if len(line) > 0 and line[0] in ('M', 'A'):
            crcheck_file(tld, line[8:], line[0] == 'A')
        if len(line) > 0 and line[0] == '?':
            local_files.append(line[8:])
    if local_files:
        print("Following files are not under source control:")
        for name in local_files:
            print("    {0}".format(name))

if __name__ == '__main__':
    crcheck(_TOP)
