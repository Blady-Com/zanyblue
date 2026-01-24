# -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2016, 2018, Michael Rohan <mrohan@zanyblue.com>
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
See Impl doc string.
"""

import codecs
import os
import re
import subprocess
import time
import zb

_COPYRIGHT1_RC = re.compile(r'.*Copyright .c. (?P<year>\d+), '
                            r'Michael Rohan .*')
_COPYRIGHT2_RC = re.compile(r'.*Copyright .c. (?P<year1>\d+), '
                            r'(?P<year2>\d+), Michael Rohan .*')
_COPYRIGHT3_RC = re.compile(r'.*Copyright \(c\) (?P<malformed>.+) '
                            r'Michael Rohan .*')
_COPYRIGHT_YEAR = str(time.localtime().tm_year)


class Impl(zb.Handler):
    """
    Command to verify copyright dates in various new/modified ZB sources are
    updated prior to committing.
    """

    def handle(self, command, options):
        tld = self.get_param("top_dir")
        svn = self.get_param("svn")
        process = subprocess.Popen(
            '{0} status'.format(svn),
            cwd=tld,
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        stdout, _ = process.communicate()
        process.wait()
        local_files = []
        self.crcheck_file("LICENSE.txt", False)
        for line in stdout.decode("utf-8").split("\n"):
            if len(line) > 0 and line[0] in ('M', 'A'):
                self.crcheck_file(line[8:], line[0] == 'A')
            if len(line) > 0 and line[0] == '?':
                local_files.append(line[8:])
        if local_files:
            self.info("Following files are not under source control:")
            for name in local_files:
                self.info("    {0}", name)

    def crcheck_file(self, filename, new_p):
        """
        Scan a file checking for the copyright notices to verify dates.
        """
        fullpath = os.path.join(self.get_param('top_dir'), filename)
        if not os.path.isfile(fullpath):
            return
        try:
            with codecs.open(fullpath, "r", "utf-8") as src:
                for line in src.readlines():
                    line = line.strip()
                    matches = _COPYRIGHT1_RC.match(line)
                    if matches is not None:
                        year = matches.group('year')
                        if year != _COPYRIGHT_YEAR:
                            self.info("{0}", filename)
                        return
                    matches = _COPYRIGHT2_RC.match(line)
                    if matches is not None:
                        year1 = matches.group('year1')
                        year2 = matches.group('year2')
                        if new_p or year2 != _COPYRIGHT_YEAR or year1 == year2:
                            self.info("{0}", filename)
                        return
                    if len(line) == 0:
                        self.info("{0}", filename)
                        return
        except IOError as exc:
            self.info(
                "Error failed to inspect \"{0}\": {1}", filename, str(exc)
            )
