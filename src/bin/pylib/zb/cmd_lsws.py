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


class Impl(zb.Handler):
    """
    Command to list available workspaces.  Workspaces are defined as SVN
    checked out directory in the list of directories defined by the parameter
    "ws_dirs".
    """

    def add_options(self, name, parser):
        parser.add_argument(
            "-v", "--verbose",
            dest="verbose",
            default=False,
            action="store_true",
            help="Verbose information on workspaces"
        )

    def handle(self, command, options):
        """
        Handler, print the located directories.
        """
        self.verbose = options.verbose
        self.print_workspaces(self.locate_workspaces())

    def get_wsinfo(self, ws_name, location):
        version = None
        if self.verbose:
            version = self.get_ws_version(ws_name, location)
        return {'location': location, 'scm_info': version}

    def locate_workspaces(self):
        """
        Locate the workspaces by scanning the "ws_dirs" directory list.
        """
        result = {}
        for dirname in self.get_param("ws_dirs"):
            if not os.path.isdir(dirname):
                continue
            for ws_name in os.listdir(dirname):
                location = os.path.join(dirname, ws_name)
                if self.is_ws(location):
                    cur_list = result.get(ws_name, [])
                    cur_list.append(self.get_wsinfo(ws_name, location))
                    result[ws_name] = cur_list
        return result

    def print_workspaces(self, workspaces):
        """
        Print the workspaces found, if any.
        """
        ws_names = sorted(workspaces.keys())
        if len(ws_names) == 0:
            print("No ZB workspaces found")
            return
        max_len = 0
        for ws_name in ws_names:
            max_len = max(max_len, len(ws_name))
        for ws_name in ws_names:
            self.print_wsinfo(ws_name, max_len, workspaces[ws_name])

    def print_wsinfo(self, ws_name, max_len, wsinfo):
        """
        Print name, location and eclipsed locations, if any, for a workspace.
        """
        if self.verbose:
            fmt = "{0:<{1}}   {2:<10}   {3}"
        else:
            fmt = "{0:<{1}}   {3}"
        if len(wsinfo) > 1:
            fmt += " eclipsing {4}"
        print(fmt.format(
            ws_name,
            max_len,
            wsinfo[0]['scm_info'],
            wsinfo[0]['location'],
            ", ".join([ws['location'] for ws in wsinfo[1:]])
        ))
