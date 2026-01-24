#!/usr/bin/env python
# -*- encoding: utf8 -*-

"""
Compile CLDR locale data into the ZanyBlue locale structures.
"""

import codecs
import os
import sys

from optparse import OptionParser

sys.path.insert(0, os.path.join(os.path.dirname(__file__), u"pylib"))

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
    (options, args) = parser.parse_args()
    if options.embed is None or options.props_dir is None:
        print "Error: require both '-e' and '-p' be specified"
        return 1
    verbose = options.verbose
    locales = LocaleList(options.cldr_dir)
    add_root_locale(locales)
    for name in args:
        locales.load_locale(name, verbose)
    locales.resolve_locale_aliases(verbose)
    locales.resolve_locales(verbose)
    locales.write_properties(options.props_dir, options.base)
    locales.write_code()

if __name__ == "__main__":
    sys.exit(main())
