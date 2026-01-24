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
import zb
from zb.scm import get_scm
from zb.scm import SCMError


class Impl(zb.Handler):
    """
    Command to create a ZB workspace.
    """

    def __init__(self, zbdev):
        super(Impl, self).__init__(zbdev)
        self.ws_name = None
        self.ws_path = None

    def add_options(self, name, parser):
        """
        Add "mkws" specific command line options
        """
        parser.add_argument(
            '-R',
            dest="readonly",
            default=False,
            action='store_true',
            help="Create a read-only workspace"
        )
        parser.add_argument(
            '-b',
            dest='branch',
            default="master",
            help='Branch to checkout'
        )
        parser.add_argument(
            '-d',
            dest='directory',
            default=".",
            help='Directory to create the workspace in',
        )
        parser.add_argument(
            'name',
            metavar='WS-NAME',
            help="Name of the workspace"
        )

    def handle(self, command, options):
        """
        Handle the "mkws" command: create a new workspace
        """
        self.ws_name = options.name
        self.ws_path = os.path.join(options.directory, self.ws_name)
        if os.path.exists(self.ws_path):
            self.error("Error: The workspace directory \"{0}\" exists".format(
                self.ws_path
            ))
            return
        self.info("Create the workspace \"{0}\" with path \"{1}\"".format(
            self.ws_name,
            self.ws_path
        ))
        scm_root_attr = "scm_root_ro" if options.readonly else 'scm_root_rw'
        branch_name = options.branch
        branch_info = self.get_param("scm_branches")
        branch_path = branch_info.get(branch_name, None)
        if branch_path is None:
            self.error("Error: No branch data defined for \"{0}\"".format(
                branch_name
            ))
            self.error("Error: Known branches are:")
            for name in list(branch_info.keys()):
                self.error("Error:      \"{0}\"".format(
                    name
                ))
            return
        try:
            scm = get_scm(
                self,
                self.get_param("scm_implementation"),
                self.get_param(scm_root_attr),
                branch_path,
                self.ws_path
            )
            scm.checkout()
        except SCMError as ex:
            self.fatal("{0}".format(str(ex)))
