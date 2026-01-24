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
import tarfile
import zb


class Impl(zb.Handler):
    """
    Verify a ZanyBlue distribution file (.tar.gz) by extracting it, checking
    the MD5 checksums contained in the MANIFEST.txt file, building the
    distribution and running the tests.
    """

    def __init__(self, zbdev):
        """
        Constructor
        """
        super(Impl, self).__init__(zbdev)
        self.set_param("tld", "zanyblue-1.4.0")

    def add_options(self, name, parser):
        """
        Add command line options specific to the "verify" command.
        """
        parser.add_argument(
            "-W", "--work-directory",
            dest='work_dir',
            default=None,
            help='Work directory to use when verifying'
        )
        parser.add_argument(
            "-R", "--reports-directory",
            dest="reports_dir",
            default='reports',
            help="Directory under WORK_DIR to store the XML test results"
        )
        parser.add_argument(
            "distribuion_dir",
            default=None,
            nargs="?",
            help="Directory containing the distribubtion"
        )

    def extract(self):
        """
        Extract the gzipped distribution tar file.
        """
        tld = self.get_param("tld")
        distribution = self.get_param("distribution")
        if tld and os.path.exists(tld):
            self.info("Removing old \"{0}\" directory ...", tld)
            shutil.rmtree(tld)
        self.info("Extracting the distribution \"{0}\" ...", distribution)
        tgz = tarfile.open(distribution, "r:*")
        work_dir = self.get_param("work_dir")
        tgz.extractall(path=work_dir)
        if not os.path.isdir(os.path.join(work_dir, tld)):
            raise Exception("\"{0}\" did not contain \"{1}\"".format(
                distribution,
                tld
            ))

    def handle(self, command, options):
        distribuion_dir = options.distribuion_dir or self.get_param("top_dir")
        distributions = glob.glob(
            "{0}/zanyblue-*.tar.gz".format(distribuion_dir)
        )
        if len(distributions) == 0:
            self.fatal("No distributions found in \"{0}\"", options.destdir)
        elif len(distributions) > 1:
            self.fatal(
                "Multiple distributions found in \"{0}\": {1}",
                options.destdir,
                ", ".join(distributions)
            )
        else:
            distribution = distributions[0]
        work_dir = options.work_dir or self.get_param("verify_workdir")
        self.info("Distribution is \"{0}\"", distribution)
        self.set_param("distribution", distribution)
        self.set_param("work_dir", work_dir)
        self.set_param("tld", os.path.splitext(
            os.path.splitext(os.path.basename(distribution))[0]
        )[0])
        self.set_param("reports_dir", options.reports_dir)
        if os.path.exists(work_dir):
            self.info("Removing existing work directory \"{0}\"", work_dir)
            shutil.rmtree(work_dir)
        os.mkdir(work_dir)
        self.extract()
        self.verify_checksums()
        self.build()
        self.test()
        self.info("Verification \"{0}\" complete", distribution)
        return 0

    def build(self):
        """
        Build the distribution.
        """
        self.info("Building the distribution ...")
        subprocess.check_call(
            self.get_param("make"),
            cwd=os.path.join(
                self.get_param("work_dir"),
                self.get_param("tld"),
                "src"
            )
        )

    def test(self):
        """
        Run the ZanyBlue tests.  The Jenkins compatible XML summary files
        are collected in the "reports" directory.
        """
        self.info("Running the ZanyBlue tests ...")
        work_dir = self.get_param("work_dir")
        tld = self.get_param("tld")
        cmd = self.get_param("make")
        cmd.append(self.get_param('test_target'))
        subprocess.check_call(
            cmd,
            cwd=os.path.join(work_dir, tld, "src")
        )
        self.info("Creating the reports directory for test results ...")
        reports_dir = os.path.join(work_dir, self.get_param("reports_dir"))
        if os.path.exists(reports_dir):
            shutil.rmtree(reports_dir)
        os.mkdir(reports_dir)
        test_dir = os.path.join(work_dir, tld, "src/test")
        xmlfiles = glob.glob("{0}/*.xml".format(test_dir))
        xmlfiles.extend(
            glob.glob("{0}/system/test-area/*.xml".format(test_dir))
        )
        for xmlfile in xmlfiles:
            shutil.copy(
                xmlfile,
                os.path.join(reports_dir, os.path.basename(xmlfile))
            )

    def verify_checksums(self):
        """
        Verify the MD5 checksums included in the distribution's MANIFEST.txt
        file.  Not all platforms have the "md5sum" utility used to verify so
        this verification is optional.
        """
        if not self.get_param("md5sum_verify"):
            self.info("MD5 verification disabled")
            return
        self.info("Verifying MD5 checksums for distributed files")
        subprocess.check_call([
            self.get_param("md5sum"),
            "--check",
            "{0}/MANIFEST.txt".format(self.get_param("tld"))
        ], cwd=self.get_param("work_dir"))
