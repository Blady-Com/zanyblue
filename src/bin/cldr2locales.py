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
Compile CLDR locale data into the ZanyBlue locale structures.
"""

import codecs
import os
import sys

from optparse import OptionParser

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "pylib"))

from zb.localelist import LocaleList


def add_root_locale(locales):
    index = locales.add_locale("")
    locales.set_root_values(index)


def unicode_setup():
    sys.stdout = codecs.lookup('utf-8')[-1](sys.stdout)
    sys.stderr = codecs.lookup('utf-8')[-1](sys.stderr)


def main():
    unicode_setup()
    parser = OptionParser(usage=__doc__)
    parser.add_option(
        "-v", "--verbose",
        action="store_true",
        dest="verbose",
        default=False,
        help="Increase the amount of status output"
    )
    parser.add_option(
        "-d", "--cldr_dir",
        dest="cldr_dir",
        default="cldr",
        help="Location of the CLDR XML data files"
    )
    parser.add_option(
        "-e", "--embed",
        dest="embed",
        default=None,
        help="Name of the file to embed the locale data"
    )
    parser.add_option(
        "-p", "--props_dir",
        dest="props_dir",
        default=None,
        help="Location for the generated .properties files"
    )
    parser.add_option(
        "-B", "--base",
        dest="base",
        default="en",
        help="Base locale for properties files"
    )
    parser.add_option(
        "-A", "--ascii",
        dest="ascii_only",
        default=False,
        action='store_true',
        help="Generate ASCII only output"
    )
    (options, args) = parser.parse_args()
    if options.embed is None or options.props_dir is None:
        print("Error: require both '-e' and '-p' be specified")
        return 1
    verbose = options.verbose
    locales = LocaleList(options.cldr_dir, options.ascii_only)
    add_root_locale(locales)
    for name in args:
        locales.load_locale(name, verbose)
    locales.resolve_locale_aliases(verbose)
    locales.resolve_locales(verbose)
    locales.write_properties(options.props_dir, options.base)
    locales.write_code()

if __name__ == "__main__":
    sys.exit(main())
