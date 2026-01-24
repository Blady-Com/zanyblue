#!/usr/bin/python -tt

"""
mknotices: Generate the NOTICES.txt and notices.htp files containing
           acknowledgements for non-ZB code included in ZB.
"""

import codecs
import os
import re
import sys
import time
from optparse import OptionParser
from lxml import etree

_NOTICES_TXT_FILE = "{0}/NOTICES.txt"
_NOTICES_HTP_FILE = "{0}/src/doc/website/notices.htp"

_V_MAJOR = 1
_V_MINOR = 0
_V_PATCH = 0

_0001 = "This is MKNOTICES, V{0}.{1}.{2} on {3}."
_0002 = "Copyright (c) {0}, Michael Rohan.  All rights reserved."
_0003 = "Scanning the ZanyBlue directory \"{0}\" ..."
_0004 = "Loading the notices file \"{0}\" ..."
_0005 = "Error, no name tag defined in the notice file \"{0}\"\n"
_0006 = "Creating the NOTICES.txt file \"{0}\""
_0007 = "Creating the notices.htp file \"{0}\""

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

def load_notice_file(notices, fullname):
    """
    Load a ZB notices file giving the XML description of a third party
    component.
    """
    print _0004.format(fullname)
    doc = etree.parse(fullname)
    elements = doc.findall("notice")
    if len(elements) == 0:
        print _0005.format(fullname)
        sys.exit(1)
    for element in elements:
        name = element.find("name").text.strip()
        current_list = notices.get(name, [])
        current_list.append(element)
        notices[name] = current_list

def write_htp_file(top, notices):
    """
    Write the web site notices.htp file giving the notices as an HTML file.
    """
    filename = _NOTICES_HTP_FILE.format(top)
    print _0007.format(filename)
    fh = codecs.open(filename, "w", "utf-8")
    fh.write("<file include=\"defs.hti\" />\n")
    fh.write("<file include=\"header.hti\" name=\"Notices\" />\n\n")
    fh.write("<file include=\"title.hti\" />\n\n")
    fh.write("<div id=\"contents\">\n\n")
    fh.write("<h1>Notices</h1>\n\n")
    fh.write("<p>\n")
    fh.write("ZanyBlue software includes code and data from the following ")
    fh.write("copyright holders:\n")
    fh.write("<ul>\n")
    for name in sorted(notices.keys()):
        fh.write("   <li>\n")
        fh.write("    <a href=\"#{0}\">{0}</a>\n".format(name))
        fh.write("   </li>\n")
    fh.write("\n")
    fh.write("</ul>\n")
    fh.write("The conditions for the use is detailed below.\n\n")
    fh.write("</p>\n")
    for name in sorted(notices.keys()):
        fh.write("<h2><a name=\"{0}\">{0}</a></h2>\n\n".format(name))
        for notice in notices[name]:
            description = notice.find("description").text
            url = notice.find("url").text
            fh.write("<p>\n")
            fh.write("{0}\n".format(description))
            fh.write("</p>\n")
            fh.write("<p>\n")
            fh.write("See <a href=\"{0}\">{0}</a>\n".format(url))
            fh.write("</p>\n")
            for license in notice.findall("license"):
                fh.write("<h3>License")
                if 'title' in license.attrib:
                    title = license.attrib['title']
                    fh.write(": {0}".format(title))
                fh.write("</h3>\n\n")
                fh.write("<pre class=\"license\">\n")
                for line in license.text.split("\n"):
                    fh.write("{0}\n".format(line))
                fh.write("</pre>\n")
            fh.write("\n")
    fh.close()

def write_txt_file(top, notices):
    """
    Write the top level NOTICES.txt file giving the notices as a plain text
    file.
    """
    filename = _NOTICES_TXT_FILE.format(top)
    print _0006.format(filename)
    fh = codecs.open(filename, "w", "utf-8")
    fh.write("This software includes code and data from the ")
    fh.write("following copyright holders:\n\n")
    for name in sorted(notices.keys()):
        fh.write("    {0}\n".format(name))
    fh.write("\n")
    fh.write("The conditions for the use is detailed below.\n\n")
    for name in sorted(notices.keys()):
        fh.write(" *  {0}\n".format(name))
        for notice in notices[name]:
            fh.write("{0}\n".format(notice.find("description").text))
            fh.write("    See {0}\n\n".format(notice.find("url").text))
            for license in notice.findall("license"):
                if 'title' in license.attrib:
                    title = license.attrib['title']
                    fh.write(" ** License: {0}\n\n".format(title))
                else:
                    fh.write(" ** License\n\n")
                for line in license.text.split("\n"):
                    fh.write("{0}\n".format(line))
            fh.write("\n")
    fh.close()

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
    (options, args) = parser.parse_args()
    if len(args) > 1:
        parser.print_help()
        sys.exit(2)
    top = os.path.abspath(locate_top(args))
    verbose = options.verbose
    print _0003.format(top)
    notices = {}
    for path, subdirs, files in os.walk(top):
        for name in subdirs + files:
            fullname = os.path.join(path, name)
            if '.svn' in fullname:
                continue
            if name == "zbnotice.xml":
                load_notice_file(notices, fullname)
    write_txt_file(top, notices)
    write_htp_file(top, notices)
    return 0

if __name__ == '__main__':
    sys.exit(main())
