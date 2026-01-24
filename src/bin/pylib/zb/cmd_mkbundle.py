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

import hashlib
import os
import subprocess
import tarfile
import tempfile
import zb
try:
    import xml.etree.cElementTree as ET
except ImportError:
    import xml.etree.ElementTree as ET
try:
    import jinja2
    HAVE_JINJA2 = True
except ImportError:
    HAVE_JINJA2 = False


class Impl(zb.Handler):
    """
    Create the source distribution for a release and the files used to
    update the web site.  These are the files that can be uploaded to
    sourceforge.net for each ZanyBlue release.
    """

    def __init__(self, zbdev):
        """
        Constructor
        """
        super(Impl, self).__init__(zbdev)
        self.env = None
        self.manifest = []

    def _get_gacode(self):
        if self.get_param("include_google_analytics"):
            return self.read_file(self.get_param("ganalytics_file"))
        else:
            return self.get_param("ganalytics_null")

    def add_options(self, name, parser):
        parser.add_argument(
            "-G", "--include-google-analytics",
            dest="include_google",
            default=False,
            action='store_true',
            help="Perform a production build (include Google Analytics code)"
        )

    def add_defs_file(self, distribution):
        """
        Add the defs.mk file to the distribution
        """
        template = self.env.get_template(self.get_param('defs_template'))
        tmpfile, tmpname = tempfile.mkstemp(".zb")
        contents = template.render(self.zbdev.params).encode("utf-8")
        os.write(tmpfile, contents)
        os.close(tmpfile)
        self.add_file(
            distribution,
            tmpname,
            arcname=os.path.join(
                self.get_param("versioned_name"),
                self.get_param('defs_name'),
            )
        )
        os.remove(tmpname)

    def add_doc_file(self, website, filename):
        """
        Add a documentation file to the website tar.gz
        """
        top_dir = self.get_param("top_dir")
        fullpath = os.path.join(top_dir, filename)
        arcname = filename.replace("doc/", "")
        self.info("Adding documentation file \"{0}\"", arcname)
        gacode = self._get_gacode()
        if fullpath.endswith("html"):
            contents = self.read_file(filename)
            contents = contents.replace(
                "</body>",
                "\n{0}\n</body>".format(gacode)
            )
            tmpfile, tmpname = tempfile.mkstemp(".zb")
            os.write(tmpfile, contents.encode("utf-8"))
            os.close(tmpfile)
            website.add(tmpname, arcname=arcname)
            os.remove(tmpname)
        else:
            website.add(fullpath, arcname=arcname)

    def add_file(self, distribution, filename, arcname=None):
        """
        Add a source file to the distribution .tar.gz
        """
        top_dir = self.get_param("top_dir")
        fullpath = os.path.join(top_dir, filename)
        arcname = arcname or os.path.join(
            self.get_param("versioned_name"),
            filename
        )
        self.info("Adding distribution file \"{0}\"", arcname)
        self.manifest.append({
            'filename': arcname,
            'md5sum': self.md5sum(filename),
        })
        distribution.add(fullpath, arcname=arcname)

    def add_manifest(self, distribution):
        """
        Add the defs.mk file to the distribution
        """
        template = self.env.get_template(self.get_param('manifest_template'))
        self.set_param("manifest", self.manifest)
        tmpfile, tmpname = tempfile.mkstemp(".zb")
        contents = template.render(self.zbdev.params).encode("utf-8")
        os.write(tmpfile, contents)
        os.close(tmpfile)
        self.add_file(
            distribution,
            tmpname,
            arcname=os.path.join(
                self.get_param("versioned_name"),
                "MANIFEST.txt"
            )
        )
        os.remove(tmpname)

    def bundle_code(self, doc_files):
        """
        Create the bundle containing the files for the ZB code.
        """
        source_files = self.inventory_scm()
        source_files.extend(doc_files)
        source_files.extend(self.get_param("unversioned_files"))
        source_files = sorted(source_files)
        source_tarfile = self.get_param("versioned_tarfile")
        self.info("Creating the distribution tar file \"{0}\"", source_tarfile)
        exclude_files = self.get_param("distribution_exclusions")
        distribution = tarfile.open(source_tarfile, "w:gz")
        for filename in sorted(source_files):
            if filename not in exclude_files:
                self.add_file(distribution, filename)
            else:
                self.info(
                    "Excluding the file \"{0}\" from the distribution",
                    filename
                )
        self.add_defs_file(distribution)
        self.add_manifest(distribution)
        distribution.close()
        self.info(
            "Created the distribution tar file \"{0}\" ({1} entries)",
            source_tarfile,
            len(source_files)
        )
        return source_files

    def bundle_website(self):
        """
        Create the bundle containing the files for the ZB website.
        """
        doc_files = self.inventory_doc()
        website_tarfile = self.get_param("website_tarfile")
        self.info("Creating the website tar file \"{0}\"", website_tarfile)
        website = tarfile.open(website_tarfile, "w:gz")
        for filename in sorted(doc_files):
            self.add_doc_file(website, filename)
        website.close()
        self.info(
            "Created the website tar file \"{0}\" ({1} entries)",
            website_tarfile,
            len(doc_files)
        )
        return doc_files

    def handle(self, command, options):
        """
        The "main" command implementation.
        """
        if not HAVE_JINJA2:
            self.info("The Python module 'jinja2' is not available")
            self.info("Please pip install the src/admin/requirements.txt file")
            return
        self.set_param("include_google_analytics", options.include_google)
        self.env = jinja2.Environment(
            loader=jinja2.FileSystemLoader(self.get_param("templates_dir"))
        )
        doc_files = self.bundle_website()
        self.bundle_code(doc_files)
        self.info("Created the ZanyBlue distribution bundles")
        return 0

    def inventory_doc(self):
        """
        Generate an inventory of the files to include in for documentation.
        """
        result = []
        top_dir = self.get_param("top_dir")
        docdir = os.path.join(top_dir, "doc")
        for root, _, files in os.walk(docdir):
            for name in files:
                fullname = os.path.join(root, name)
                fullname = fullname.replace(top_dir + "/", "")
                result.append(fullname)
        return result

    def inventory_scm(self):
        """
        Generate an inventory of the source controlled files.
        """
        top_dir = self.get_param("top_dir")
        tmpfile, tmpname = tempfile.mkstemp(".zb")
        subprocess.check_call(
            [self.get_param("svn"), "status", "-v", "--xml", top_dir],
            stdout=os.fdopen(tmpfile, "w")
        )
        target = ET.ElementTree(file=tmpname).find("target")
        os.remove(tmpname)
        result = []
        for element in target.findall("entry"):   # pylint: disable=no-member
            wc_status = element.find("wc-status")
            item = wc_status.attrib['item']
            if item in ["normal", "modified"]:
                path = element.attrib['path']
                if not os.path.isdir(path):
                    result.append(path.replace(top_dir + '/', ""))
        return result

    def md5sum(self, filename):
        """
        Return the MD5 checksum for a file.
        """
        result = hashlib.md5()
        result.update(self.read_file(filename, binary=True))
        return result.hexdigest()
