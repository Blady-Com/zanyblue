#!/usr/bin/python -tt

"""
mkwebbundle: Generate the website tar ball for ZanyBlue
"""

import os
import re
import sys
import time
from optparse import OptionParser

_ROOT_DIR = sys.path[0]
sys.path.insert(0, os.path.join(_ROOT_DIR, "pylib"))

from bundles import TarDestination, ZipDestination

_V_MAJOR = 1
_V_MINOR = 0
_V_PATCH = 0

_0001 = "This is MKWEBBUNDLE, V{0}.{1}.{2} on {3}."
_0002 = "Copyright (c) {0}, Michael Rohan.  All rights reserved."
_0003 = "Scanning the ZanyBlue directory \"{0}\" ..."
_0004 = "Generating bundles for V{0}, r{2} ({1})"
_0005 = "Generating the bundle \"{0}\" ({1} entries) ..."
_0006 = "No output file types specified via the -t option"
_0007 = "The output file type \"{0}\" is not known"

_DEFAULT_TYPE = ("zip" if os.sys.platform.startswith("win") else "tar.gz")
_PIWIK_FILE = "src/admin/piwik.html"
_PIWIK_NULL = "<!-- Piwik: Development copy, Piwik tags not included -->\n"

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

def query_make(top, make, variable):
    """
    Return the value of a Makefile variable by querying the top level source
    Makefile.
    """
    cmd = "{0} -s -C \"{1}/src\" print_{2}".format(make, top, variable)
    result = os.popen(cmd).read().strip()
    return result

def load_file(top, path):
    """
    Read and return the contents of a file.
    """
    pathname = os.path.join(top, path)
    f = open(pathname)
    result = f.read()
    f.close()
    return result

def mk_bundle_filename(top, version, revision, status):
    """
    Return the prefix (top level directory in the genreated bundle)
    and the two bundle file names (src and third party dependencies).
    """
    return "zanyblue-{0}{1}-r{2}-website.".format(version,
                                                  status[0].lower(),
                                                  revision.lower())

def open_bundle(top, piwik, name, filetype, verbose, files,
                manifest="MANIFEST.txt"):
    """
    Create the bundle for writing.  Simply dispatch on the file type.
    """
    filename = os.path.join(top, name) + filetype
    if filetype == "zip":
        result = ZipDestination(not verbose, None, filename, manifest)
    else:
        result = TarDestination(not verbose, None, filename, manifest)
    print _0005.format(filename, len(files))
    for name in sorted(files):
        srcfile = os.path.join(top, "doc", name)
        if name.endswith(".html"):
            infile = open(srcfile, "r")
            tmpfile = "html-fixed.tmp"
            outfile = open(tmpfile, "w")
            for line in infile.readlines():
                if "</body>" in line or "</BODY>" in line:
                    outfile.write(piwik)
                outfile.write(line)
            outfile.close()
            infile.close()
            result.add(tmpfile, name)
            os.unlink(tmpfile)
        else:
            result.add(srcfile, name)
    return result

def main():
    """
    Main driver function.  Setup the command line option parser,
    parse and dispatch.
    """
    parser = OptionParser(usage=__doc__)
    parser.add_option("-v", "--verbose",
                      action="store_true",
                      dest="verbose",
                      default=False,
                      help="Increase the amount of generated status output")
    parser.add_option("-t", "--type",
                      dest="filetypes",
                      action="append",
                      help="Package file type, e.g., tar, tar.bz2, zip, etc")
    parser.add_option("-V", "--version",
                      dest="version",
                      help="Version number")
    parser.add_option("-R", "--revision",
                      dest="revision",
                      help="SVN version number")
    parser.add_option("-S", "--status",
                      dest="status",
                      help="Release status (ALPHA, BETA, ...)")
    parser.add_option("-m", "--make",
                      dest="make",
		      default="make",
                      help="Make command to use")
    (options, args) = parser.parse_args()
    if len(args) > 1:
        parser.print_help()
        sys.exit(2)
    top = os.path.abspath(locate_top(args))
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
    version = options.version or query_make(top, make, "VERSION")
    revision = options.revision or query_make(top, make, "SVN_VERSION")
    status = options.status or query_make(top, make, "V_STATUS")
    # Normalize status and revision values
    revision = revision.replace(":", "-")
    print _0001.format(_V_MAJOR, _V_MINOR, _V_PATCH, time.ctime())
    if verbose:
        print _0004.format(version, status, revision)
    if 'M' in revision:
        piwik = _PIWIK_NULL
    else:
        piwik = load_file(top, _PIWIK_FILE)
    print _0003.format(top)
    filelist = []
    docdir = os.path.join(top, "doc")
    for path, subdirs, files in os.walk(docdir):
        for name in files:
            fullname = os.path.join(path, name)
            if '.svn' in fullname:
                continue
            pathname = fullname[len(docdir) + 1:]
            filelist.append(pathname)
    website_bundle = mk_bundle_filename(top, version, revision,status)
    for filetype in filetypes:
        dest = open_bundle(top, piwik, website_bundle, filetype, verbose, filelist)
        dest.close()
    return 0

if __name__ == '__main__':
    sys.exit(main())
