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

import argparse
import zb


class Impl(zb.Handler):
    """
    Command to substitute parameter references in general files.  The source
    file is copied to the destination replacing any occurences of the command
    line VARs with their values.
    """

    def add_options(self, name, parser):
        parser.add_argument(
            '-d',
            metavar='DEFINITION',
            dest='definitions',
            default=[],
            action='append',
            help='Attribute value definition'
        )
        parser.add_argument(
            'src',
            metavar='INPUT',
            type=argparse.FileType("r"),
            help="Source to expand"
        )
        parser.add_argument(
            'dst',
            metavar='OUTPUT',
            type=argparse.FileType("w"),
            help="Destination to write"
        )

    def handle(self, command, options):
        variables = {}
        for arg in options.definitions:
            var, value = arg.split('=')
            variables[var] = value
        for line in options.src:
            for key, value in list(variables.items()):
                line = line.replace(key, value)
            options.dst.write(line)
        options.src.close()
        options.dst.close()
