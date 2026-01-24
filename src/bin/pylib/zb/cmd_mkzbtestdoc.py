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

import codecs
import os
import re
try:
    import jinja2
    HAVE_JINJA2 = True
except ImportError:
    HAVE_JINJA2 = False

import zb


_CMD_RC = re.compile(r'^zbtest-commands-(?P<name>.+)_command.adb$')
_FUNC_RC = re.compile(r'^zbtest-functions-(?P<name>.*)_function.adb$')


class Impl(zb.Handler):
    """
    For the ZBTest commands and functions, generate

    * The properties files describing them used by the "help" command
    * The RST files containing the documentation for the website/documentation
    """

    def __init__(self, zbdev):
        """
        Constructor
        """
        super(Impl, self).__init__(zbdev)
        # RST character for various document levels (section, subsection, etc)
        self.env = None
        self.property_format = "{0}: {1}\n\n    Usage: {2}\n\n{3}"
        self.destdir = os.path.join(self.get_param("src_dir"), "doc/source")
        self.cmddir = os.path.join(
            self.get_param("src_dir"),
            "zbtest/commands"
        )
        self.funcdir = os.path.join(
            self.get_param("src_dir"),
            "zbtest/functions"
        )
        self.params = {}

    def add_options(self, name, parser):
        """
        Add command line options specific to the "mkzbtestdoc" command.
        """
        parser.add_argument(
            "-P", "--properties",
            dest='properties',
            default=False,
            action='store_true',
            help='Generate the commands and functions properties files'
        )
        parser.add_argument(
            "-D", "--documents",
            dest='documents',
            default=False,
            action='store_true',
            help='Generate the RST document commands and functions files'
        )

    def handle(self, command, options):
        if not HAVE_JINJA2:
            self.info("The Python module 'jinja2' are not available")
            self.info("Please pip install the src/admin/requirements.txt file")
            return
        if not options.properties and not options.documents:
            self.fatal("mkzbtestdoc requires either the '-P' or '-D' option")
            return
        self.env = jinja2.Environment(
            loader=jinja2.FileSystemLoader(self.get_param("templates_dir"))
        )
        self.process_files(options, self.cmddir, _CMD_RC, "command")
        self.process_files(options, self.funcdir, _FUNC_RC, "function")
        return 0

    def load_impl_data(self, name, implfile, typename):
        """
        Scan the Ada source file pull out the documentation data marked
        by the strings:

        @usage - remainder of the line gives the command/function usage
        @summary - remainder of the line gives the command/function summary
        @start-doc - the following Ada comment lines give the documentation
                     on the command/function (RST format)
        """
        title = "The ``{0}`` {1}".format(name, typename.capitalize())
        title += "\n" + ("=" * len(title))
        docstring = ""
        usage = ""
        summary = ""
        accumulate = False
        with codecs.open(implfile, "r", "utf-8") as src:
            for line in src.readlines():
                if accumulate:
                    docstring += line[4 if line.startswith("-- ") else 2:]
                    accumulate = not line.isspace()
                elif '@usage' in line:
                    usage = line[line.index("@usage") + 7:].strip()
                elif '@summary' in line:
                    summary = line[line.index("@summary") + 9:].strip()
                elif '@start-doc' in line:
                    accumulate = True
        qdocstring = self.properties_quote(name, summary, usage, docstring)
        return {
            'name': name,
            'docstring': docstring,
            'qdocstring': qdocstring,
            'usage': usage,
            'summary': summary,
            'title': title,
        }

    def load_files(self, dirname, filerc, title):
        """
        Scan the given directory for files matching the regex "filerc"
        returning a sorted list of implementation descriptions.
        """
        result = []
        for impl in os.listdir(dirname):
            match = filerc.match(impl)
            if match is None:
                continue
            implname = match.group('name')
            if implname == "unknown":
                continue
            result.append(
                self.load_impl_data(
                    implname,
                    os.path.join(dirname, impl),
                    title
                )
            )
        self.params[title] = sorted(result, key=lambda x: x['name'])

    def process_files(self, options, impldir, implrc, typename):
        """
        Scan for command/function implementation files and generate either
        the properties or documentation files based on the embedded
        documentation strings.
        """
        self.load_files(impldir, implrc, typename)
        if options.documents:
            self.save_file(
                "{0}.rst".format(typename),
                self.params,
                "{0}/index.rst".format(typename),
                "src/doc/source/zbtest"
            )
            self.save_impl_doc(typename)
        if options.properties:
            self.save_file(
                "{0}.properties".format(typename),
                self.params,
                "{0}s.properties".format(typename.capitalize()),
                "src/zbtest/mesg",
            )

    def properties_quote(self, name, summary, usage, docstring):
        """
        Quote the documentation string to embed in a properties file, i.e.,
        use the string "\n\" at the end of lines, etc.
        """
        result = self.property_format.format(
            name,
            summary,
            usage,
            docstring,
        )
        # Reduce trailing spaces and newlines to a single newline
        result = result.rstrip()
        # Quote result for properties file usage
        result = result.replace("\n ", "\n\\ ")
        result = result.replace("\n", "\\n\\\n")
        return result

    def save_file(self, tmpl, params, destname, reldir):
        """
        Save a template file by having Jinja2 interpret it and write the
        interpreted contents to the given destination.
        """
        destdir = os.path.join(self.get_param('top_dir'), reldir)
        if not os.path.isdir(destdir):
            os.mkdir(destdir)
        dest = os.path.join(destdir, destname)
        if not os.path.isdir(os.path.dirname(dest)):
            os.mkdir(os.path.dirname(dest))
        self.info("Creating the ZBTest documentation file \"{0}\"", dest)
        template = self.env.get_template(os.path.join("zbtest", tmpl))
        with codecs.open(dest, "w", encoding="utf-8") as dst:
            dst.write(template.render(params))

    def save_impl_doc(self, name):
        """
        Save the documentation on the command/function implementations loaded.
        """
        for impl in self.params[name]:
            self.save_file(
                name + "-impl.rst",
                impl,
                "{0}/{1}.rst".format(name, impl['name']),
                "src/doc/source/zbtest"
            )
