#!/usr/bin/python -tt
# -*- coding: utf-8 -*-

"""
Compile CLDR locale data into the ZanyBlue locale structures.
"""

import codecs
import json
import os
import sys

from optparse import OptionParser
from xml.dom import minidom

sys.path.insert(0, os.path.join(os.path.dirname(__file__), u"pylib"))

from zb.localelist import LocaleList

def add_root_locale(locales):
    index = locales.add_locale("")
    locales.set_root_values(index)

def main():
    parser = OptionParser(usage = __doc__)
    parser.add_option("-v", "--verbose",
                      action="store_true",
                      dest="verbose",
                      default=False,
                      help="Increase the amount of status output")
    parser.add_option("-d", "--cldr_dir",
                      dest="cldr_dir",
                      default="cldr",
                      help="Location of the CLDR XML data files")
    parser.add_option("-e", "--embed",
                      dest="embed",
                      default=None,
                      help="Name of the file to embed the locale data")
    parser.add_option("-p", "--props_dir",
                      dest="props_dir",
                      default=None,
                      help="Location for the generated .properties files")
    parser.add_option("-B", "--base",
                      dest="base",
                      default="en",
                      help="Base locale for properties files")
    (options, args) = parser.parse_args()
    if options.embed is None and options.props_dir is None:
        print "Error: require either '-e' or '-p' be specified"
        return 1
    if options.embed is not None and options.props_dir is not None:
        print "Error: cannot use both '-e' or '-p' options"
        return 1
    verbose = options.verbose
    locales = LocaleList(options.cldr_dir, options.embed is not None)
    if options.embed is not None:
        add_root_locale(locales)
    for name in args:
        index = locales.load_locale(name, verbose)
    if options.embed is not None:
        locales.resolve_locale_aliases(verbose)
        locales.resolve_locales(verbose)
        locales.sort_strings()
        locales.embed(options.embed)
    else:
        locales.resolve_properties_aliases(verbose)
        locales.write_properties(options.props_dir, options.base)

if __name__ == "__main__":
    sys.exit(main())
