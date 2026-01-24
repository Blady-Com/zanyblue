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

import shutil
import zb


class Impl(zb.Handler):
    """
    Command to delete a ZB workspace.
    """

    def add_options(self, name, parser):
        parser.add_argument(
            "-f", "--force",
            dest="force",
            default=False,
            action="store_true",
            help="Force deletion of workspace, even if modified"
        )
        parser.add_argument(
            'name',
            metavar='WS-NAME',
            help="Name of the workspace"
        )

    def handle(self, command, options):
        ws_name = options.name
        ws_location = self.find_ws(ws_name)
        if ws_location is None:
            self.error("Error: Could not locate the workspace \"{0}\"".format(
                ws_name
            ))
            return
        ws_version = self.get_ws_version(ws_name, ws_location)
        if ws_version == 'ERR' or 'M' in ws_version:
            fmt = 'Modified workspace "{0}" @ {1} ("{2}")'
            if options.force:
                self.error('Error: ' + fmt.format(
                    ws_name,
                    ws_version,
                    ws_location
                ))
            else:
                self.warning('Warning: ' + fmt.format(
                    ws_name,
                    ws_version,
                    ws_location
                ))
                return
        self.info("Deleting the workspace \"{0}\" located at \"{1}\"".format(
            ws_name,
            ws_location
        ))
        shutil.rmtree(ws_location)
