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

import os
import subprocess
import zb


class Impl(zb.Handler):
    """
    Create a ZanyBlue workspace.  The WS-NAME argument is required.  By
    default, the workspace will use the zbdev defined paths.  To prefix an
    extra path (or paths), use the -P option (can be specified multiple
    times, e.g., "$ zbdev setws -P /usr/gcc/7.1.0/bin mywork" with prefix
    the PATH in effect in the workspace with the GCC path, e.g., to test a
    new compiler.  Branch/workspace specific environment definitions can
    also be defined in a "zbdevrc" script in the root directory of the
    workspace which is sourced on entering the workspace.
    """

    def add_options(self, name, parser):
        parser.add_argument(
            'name',
            metavar='WS-NAME',
            help="Name of the workspace"
        )
        parser.add_argument(
            '-P',
            dest='path',
            metavar='PATH',
            action='append',
            default=[],
            help="Extra PATHs values to use within the workspace for commands",
        )

    def handle(self, command, options):
        ws_name = options.name
        ws_location = self.find_ws(ws_name)
        if ws_location is None:
            self.error("Error: Could not locate the workspace \"{0}\"".format(
                ws_name
            ))
            return
        self.info("Setting ZanyBlue workspace to \"{0}\" at \"{1}\"".format(
            ws_name,
            ws_location
        ))
        os.environ['ZBDEV_WS'] = ws_name
        os.environ['ZBDEV_HOME'] = os.environ['HOME']
        os.environ["ZBDEV_WSDIR"] = ws_location
        os.environ['HOME'] = self.get_param("setws_home")
        for path in options.path:
            if not os.path.isdir(path):
                self.warning(
                    "Warning: -P path \"{0}\" is not a directory".format(
                        path
                    )
                )
            self.info("Prepending the PATH \"{0}\"".format(path))
            self.prepend_path(path)
        subprocess.call("/bin/bash", cwd=ws_location)
