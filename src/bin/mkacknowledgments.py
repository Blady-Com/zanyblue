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
from __future__ import unicode_literals

"""
mkacks: Generate the acknowledgement files for usage of 3rd party packages
(the NOTICES.txt and notices.rst files) and contributions to ZanyBlue (the
CONTRIBUTIONS.txt and contributions.rst files).
"""

import airspeed
import codecs
import os
import sys
import yaml

_NOTICE_YAML = "zbnotice.yaml"
_TEMPLATE_OUTPUT = {
    "notices.txt": "NOTICES.txt",
    "notices.rst": "src/doc/source/notices.rst",
    "contribs.txt": "CONTRIBUTIONS.txt",
    "contribs.rst": "src/doc/source/contribs.rst",
}


def normalize_notice(notice, level):
    title = notice['name']
    if 'version' in notice:
        title = "{0}, {1}".format(
            notice['name'],
            notice['version'],
        )
    title = "{0}\n{3:{2}<{1}}".format(title, len(title), level, "")
    notice['title'] = title
    if 'notice' in notice:
        notice['notice'] = notice['notice'].replace("\n", "\n    ")


def load_notice_file(filename):
    print("Loading the notice file \"{0}\"".format(filename))
    with codecs.open(filename, "r", encoding="utf-8") as f:
        result = yaml.load(f)
    for notice in result:
        normalize_notice(notice, "-")
        for subnotice in notice.get('notices', []):
            normalize_notice(subnotice, "^")
    return result


def load_contribs(topdir):
    contribs_yaml = os.path.join(topdir, "src/admin/contributors.yaml")
    print("Loading the contributions file \"{0}\"".format(contribs_yaml))
    with codecs.open(contribs_yaml, "r", encoding="utf-8") as f:
        result = yaml.load(f)
    for contributor in result:
        name = contributor['name']
        contributor['title'] = "{0}\n{2:-<{1}}".format(name, len(name), "")
        contributor['key'] = name.replace(" ", "").lower()
        for contribution in contributor['contributions']:
            contribution['description'] = contribution['description'].replace(
                "\n",
                "\n    "
            )
    return result


def load_notices(topdir, notice_name):
    print("Scanning the directory \"{0}\" for \"{1}\" files".format(
        topdir,
        notice_name
    ))
    result = []
    for subdir, dirs, files in os.walk(topdir):
        for filename in files:
            if filename == notice_name and '.svn' not in subdir:
                result.extend(load_notice_file(os.path.join(subdir, filename)))
    result.sort(key=lambda x: (x['name'], x['version']))
    return result


def save_file(topdir, tmpl, dest, params):
    filename = os.path.join(topdir, dest)
    print("Creating the acknowledgment file \"{0}\"".format(filename))
    tmpl_path = os.path.join(topdir, "src/bin/templates", tmpl)
    with codecs.open(tmpl_path, "r", encoding="utf-8") as f:
        template = airspeed.Template(f.read())
    with codecs.open(filename, "w", encoding="utf-8") as f:
        f.write(template.merge(params))

def main():
    topdir = os.path.abspath(
        os.path.join(
            os.path.dirname(__file__),
            "..",
            "..",
        )
    )
    params = {
        'notices': load_notices(topdir, _NOTICE_YAML),
        'contributors': load_contribs(topdir),
    }
    for template, dest in _TEMPLATE_OUTPUT.iteritems():
        save_file(topdir, template, dest, params)
    return 0


if __name__ == "__main__":
    sys.exit(main())
