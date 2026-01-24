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
try:
    import jinja2
    HAVE_JINJA2 = True
except ImportError:
    HAVE_JINJA2 = False
try:
    import yaml
    HAVE_YAML = True
except ImportError:
    HAVE_YAML = False

import zb


class Impl(zb.Handler):
    """
    Generate the acknowledgement files for usage of 3rd party packages
    (the NOTICES.txt and notices.rst files) and contributions to ZanyBlue
    (the CONTRIBUTIONS.txt and contributions.rst files).
    """

    def __init__(self, zbdev):
        """
        Constructor
        """
        super(Impl, self).__init__(zbdev)
        # RST character for various document levels (section, subsection, etc)
        self.level_chars = "-^"
        self.env = None

    def handle(self, command, options):
        if not HAVE_JINJA2 or not HAVE_YAML:
            self.info("The Python module 'jinja2' or 'yaml' are not available")
            self.info("Please pip install the src/admin/requirements.txt file")
            return
        self.env = jinja2.Environment(
            loader=jinja2.FileSystemLoader(self.get_param("templates_dir"))
        )
        params = {
            'notices': self.load_notices(),
            'contributors': self.load_contribs(),
        }
        for template, dest in list(self.get_param('notice_output').items()):
            self.save_file(template, dest, params)
        return 0

    def normalize_notice(self, notice, level):
        """
        For titles, it's difficult to get the Jinja2 system to add
        the needed "-" characters under the name/version, this routine
        adds a title key with the correct number of "-"/"^" characters
        under it.
        """
        title = notice['name']
        if 'version' in notice:
            title = "{0}, {1}".format(
                notice['name'],
                notice['version'],
            )
        title = "{0}\n{3:{2}<{1}}".format(
            title,
            len(title),
            self.level_chars[level],
            ""
        )
        notice['title'] = title
        if 'notice' in notice:
            notice['notice'] = notice['notice'].replace("\n", "\n    ")

    def load_notice_file(self, filename):
        """
        Load an individual zbnotice.yaml file and update the loaded
        contents to include a "title" key useable by the rst template.
        """
        self.info("Loading the notice file \"{0}\"", filename)
        with codecs.open(filename, "r", encoding="utf-8") as src:
            result = yaml.load(src)
        for notice in result:
            self.normalize_notice(notice, 0)
            for subnotice in notice.get('notices', []):
                self.normalize_notice(subnotice, 1)
        return result

    def load_contribs(self):
        """
        Load the yaml file containing the list of contributions to the ZB
        library, etc.   This list is used by the templates to generate the
        text and rst contributions files.
        """
        contribs_yaml = self.get_param('contribs_yaml')
        self.info("Loading the contributions file \"{0}\"", contribs_yaml)
        with codecs.open(contribs_yaml, "r", encoding="utf-8") as src:
            result = yaml.load(src)
        for contributor in result:
            name = contributor['name']
            contributor['title'] = "{0}\n{2:-<{1}}".format(name, len(name), "")
            contributor['key'] = name.replace(" ", "").lower()
            for contrib in contributor['contributions']:
                contrib['description'] = contrib['description'].replace(
                    "\n",
                    "\n    "
                )
        return result

    def load_notices(self):
        """
        Scan the ZB source tree for zbnotice.yaml files loading each one
        found into a list of notices of third party dependencies.  This
        list is used by the notices template files to generate the text
        and rst notice files.
        """
        notice_name = self.get_param('notice_yaml')
        top_dir = self.get_param("top_dir")
        self.info(
            "Scanning the directory \"{0}\" for \"{1}\" files",
            top_dir,
            notice_name
        )
        result = []
        for subdir, _, files in os.walk(top_dir):
            for filename in files:
                if filename == notice_name and '.svn' not in subdir:
                    result.extend(
                        self.load_notice_file(
                            os.path.join(subdir, filename)
                        )
                    )
        result.sort(key=lambda x: (x['name'], x['version']))
        return result

    def save_file(self, tmpl, dest, params):
        """
        Save a template file by having Jinja2 interpret it and write the
        interpreted contents to the given destination.
        """
        self.info("Creating the acknowledgment file \"{0}\"", dest)
        template = self.env.get_template(tmpl)
        with codecs.open(dest, "w", encoding="utf-8") as dst:
            dst.write(template.render(params))
