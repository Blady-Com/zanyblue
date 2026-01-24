#  -*- coding: utf-8 -*-
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

import glob
import os
import shutil
import subprocess
import zb


class Impl(zb.Handler):
    """
    Simple script used to build ZanyBlue under Jenkins control: distribution
    build with Google analytics code (-G option).
    """

    def copy_test_xml_files(self, top_dir):
        """
        Copy the XML Jenkins test files from the system test-area to the
        src test directory.
        """
        xmlfiles = glob.glob("{0}/src/test/system/test-area/*.xml".format(
           top_dir
        ))
        destdir = "{0}/src/test".format(top_dir)
        for xmlfile in xmlfiles:
            shutil.copy(
                xmlfile,
                os.path.join(destdir, os.path.basename(xmlfile))
            )

    def handle(self, command, options):
        """
        The main command implementation.
        """
        top_dir = self.get_param('top_dir')
        self.info("Top dir is \"{0}\"", top_dir)
        self.make(["distribution"])
        self.copy_test_xml_files(top_dir)
        self.info("Build of the ZanyBlue system complete")
        return 0

    def make(self, targets):
        """
        Execute "make" against the list of targets.
        """
        cmd = self.get_param('make')
        cmd.extend(self.get_param('jenkins_makedefs'))
        srcdir = os.path.join(self.get_param('top_dir'), "src")
        for target in targets:
            subprocess.check_call(cmd + [target], cwd=srcdir)
