#!/usr/bin/env python
# -*- coding: utf-8 -*-
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
mktarballs: Generate the distribution tar balls for ZanyBlue
"""

import os
import sys
import time
from optparse import OptionParser

_ROOT_DIR = sys.path[0]
sys.path.insert(0, os.path.join(_ROOT_DIR, "pylib"))

from bundles import TarDestination, ZipDestination

_0001 = "This is MKBUNDLES, V{0} - {1} on {2}."
_0002 = "Copyright © {0}, Michael Rohan.  All rights reserved."
_0003 = "Scanning the ZanyBlue directory \"{0}\" ..."
_0004 = "Generating bundles for V{0}, r{2} ({1})"
_0005 = "Generating the bundle \"{0}\" ..."
_0006 = "No output file types specified via the -t option"
_0007 = "The output file type \"{0}\" is not known"
_0008 = "Created the clean inventory file \"{0}\": {1} entries"
_0009 = "Loaded the clean inventory file \"{0}\": {1} entries"
_0010 = "A file type (-t) option must be specified (\"tar.gz\" or \"zip\")"
_0011 = "The {0} must be defined on the command line (\"{1}\" option)"

_DEFAULT_TYPE = ("zip" if os.sys.platform.startswith("win") else "tar.gz")
_DEFS_FILE = "src/mkfile/defs.mk"
_DEFS_CONTENTS = '''#
#  Note, this file was generated to capture the copyright year and SVN version
#  number at the time of tar ball packaging.
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright © %(COPYRIGHT_YEAR)s, Michael Rohan <mrohan@zanyblue.com>
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

#
#  Makefile definitions for the ZanyBlue version macros
#

BUILD=Production
VERSION=%(VERSION)s
STATUS=%(STATUS)s
SVN_VERSION=%(REVISION)sX
COPYRIGHT_YEAR=%(COPYRIGHT_YEAR)s
AHVEN_ARGS=-s .External
ZBTESTNAME=external-zanyblue
'''

_FILE_TYPES = set(['tar', 'tar.bz2', 'tar.gz', 'zip'])


def locate_top(args):
    """
    Determine the top of the source tree: either given on the command line
    or it's two directories above this "bin" directory.
    """
    if len(args) == 1:
        top = args[0]
    else:
        top = os.path.abspath(__file__)
        for i in range(3):
            top = os.path.dirname(top)
    return top


def inventory_name(top):
    return os.path.join(top, "src", "admin", "inventory.lis")


def load_inventory(top):
    filename = inventory_name(top)
    result = set()
    with open(filename, "r") as f:
        for line in f.readlines():
            result.add(line.strip())
    print _0009.format(filename, len(result))
    return result

def mk_defs_file(name, top, version, revision, status, copyright_year):
    """
    Create the defs Makefile used to avoid querying the environment in created
    from tar bundles.
    """
    params = {
       'VERSION': version,
       'REVISION': revision,
       'STATUS': status,
       'COPYRIGHT_YEAR': copyright_year
    }
    return  _DEFS_CONTENTS % params


def inventory_tld(top, subdir=None):
    tld = top
    if subdir:
        tld = os.path.join(top, subdir)
    pathnames = set()
    for path, subdirs, files in os.walk(tld):
        for name in subdirs + files:
            fullname = os.path.join(path, name)
            if '.svn' in fullname:
                continue
            if 'libs3rd' in fullname:
                continue
            pathnames.add(fullname[len(top) + 1:])
    return pathnames

def mk_inventory(top):
    pathnames = set()
    for path, subdirs, files in os.walk(top):
        for name in subdirs + files:
            fullname = os.path.join(path, name)
            if '.svn' in fullname:
                continue
            if 'libs3rd' in fullname:
                continue
            pathnames.add(fullname[len(top) + 1:])
    filename = inventory_name(top)
    with open(filename, "w") as f:
        for pathname in sorted(pathnames):
            f.write("{0}\n".format(pathname))
    print _0008.format(filename, len(pathnames))

def mk_bundle_filenames(top, version, revision, status):
    """
    Return the prefix (top level directory in the genreated bundle)
    and the two bundle file names (src and third party dependencies).
    """
    args = [version, status[0].lower(), revision.lower()]
    prefix = "zanyblue-{0}{1}".format(*args)
    src_bundle = "zanyblue-{0}{1}-r{2}.".format(*args)
    return prefix, src_bundle


def open_bundle(top, prefix, name, filetype, verbose, manifest="MANIFEST.txt"):
    """
    Create the bundle for writing.  Simply dispatch on the file type.
    """
    filename = os.path.join(top, name) + filetype
    if filetype == "zip":
        result = ZipDestination(not verbose, prefix, filename, manifest)
    else:
        result = TarDestination(not verbose, prefix, filename, manifest)
    print _0005.format(filename)
    return result


def main():
    """
    Main driver function.  Setup the command line option parser,
    parse and dispatch.
    """
    parser = OptionParser(usage=__doc__)
    parser.add_option(
        "-v", "--verbose",
        action="store_true",
        dest="verbose",
        default=False,
        help="Increase the amount of generated status output"
    )
    parser.add_option(
        "-I", "--inventory",
        dest="inventory",
        action="store_true",
        default=False,
        help="Generate the initial, clean, inventory of files"
    )
    parser.add_option(
        "-t", "--type",
        dest="filetypes",
        action="append",
        help="Package file type, e.g., tar, tar.bz2, zip, etc"
    )
    parser.add_option(
        "-V", "--version",
        dest="version",
        help="Version number"
    )
    parser.add_option(
        "-R", "--revision",
        dest="revision",
        help="SVN version number"
    )
    parser.add_option(
        "-S", "--status",
        dest="status",
        help="Release status (ALPHA, BETA, ...)"
    )
    parser.add_option(
        "-Y", "--copyright-year",
        dest="copyright_year",
        help="Copyright year"
    )
    parser.add_option(
        "-m", "--make",
        dest="make",
        default="make",
        help="Make command to use"
    )
    (options, args) = parser.parse_args()
    if len(args) > 1:
        parser.print_help()
        sys.exit(2)
    top = os.path.abspath(locate_top(args))
    if options.inventory:
        mk_inventory(top)
        return 0
    fileset = load_inventory(top)
    make = options.make
    verbose = options.verbose
    filetypes = options.filetypes
    if filetypes is None or len(filetypes) == 0:
        print _0006
        return 1
    unknown_types = set(filetypes) - _FILE_TYPES
    if len(unknown_types) > 0:
        for filetype in unknown_types:
            print _0007.format(filetype)
        return 1
    version = options.version
    if not version:
        print _0011.format("version", "-V")
        return 1
    revision = options.revision
    if not revision:
        print _0011.format("revision", "-R")
        return 1
    status = options.status
    if not status:
        print _0011.format("status", "-S")
        return 1
    # Normalize status and revision values
    revision = revision.replace(":", "-")
    status = status
    copyright_year = options.copyright_year or time.localtime().tm_year
    print _0001.format(version, status, time.ctime())
    print _0002.format(copyright_year)
    if verbose:
        print _0004.format(version, status, revision)
    defs_data = mk_defs_file(
        _DEFS_FILE,
        top,
        version,
        revision,
        status,
        copyright_year
    )
    fileset.add("NOTICES.txt")
    fileset = fileset.union(inventory_tld(top, "doc"))
    print _0003.format(top)
    prefix, src_bundle = mk_bundle_filenames(top, version, revision, status)
    for filetype in filetypes:
        dest = open_bundle(top, prefix, src_bundle, filetype, verbose)
        for name in sorted(fileset):
            pathname = os.path.join(top, name)
            if os.path.exists(pathname):
                dest.add(pathname, name)
        dest.add_data(_DEFS_FILE, defs_data)
        dest.close()
    return 0

if __name__ == '__main__':
    sys.exit(main())
