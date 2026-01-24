"""
Classes to support the generation of the tar ball types: .zip, .tar,
.tar.gz and .tar.bz2.  This is basically two implementation classes
derived from a common base class.
"""

import hashlib
import os
import sys
import tarfile
import tempfile
import zipfile

MANIFEST_HEADER = '''#
# This is a simple MD5 manifest file for the contents of a ZanyBlue
# distribution.  The contents of the files in the distribution can
# be verified using the command (in the directory where the distribution
# was extracted):
#
#    $ md5sum --check {0}/{1}
#

'''

class Destination(object):
    """Capture a destination, a tar or zip file."""
    def __init__(self, quiet, prefix, name, manifest="MANIFEST"):
        """Constructor."""
        self.quiet = quiet
        self.prefix = prefix
        self.name = name
        self.manifest = manifest
        self.checksums = {}

    def add(self, pathname, renamed=None):
        """Add a file to the output destination, possibly renamed."""
        destname = os.path.join(self.prefix, pathname)
        if renamed:
            destname = os.path.join(self.prefix, renamed)
        if not self.quiet:
            print 'Adding the file "%s" ("%s")' % (pathname, destname)
        if not os.path.isdir(pathname):
            self.add_file(pathname, destname)
            self.checksums[destname] = self.md5sum(pathname)
        else:
            self.add_dir(pathname, destname)

    def add_dir(self, dirname, renamed):
        # pylint: disable-msg = W0613
        """Add a directory to the destination."""
        self._v_call('add_dir')

    def add_file(self, pathname, destname):
        # pylint: disable-msg = W0613
        """Add a file to the destination."""
        self._v_call('add_file')

    def add_data(self, destname, destdata):
        """Add a file "destname" to the bundle containing "destdata"."""
        tmpname = self.create_temp_file(destdata)
        self.add(tmpname, destname)
        os.remove(tmpname)

    def close(self):
        """Close the destination."""
        MANIFEST = MANIFEST_HEADER.format(self.prefix, self.manifest)
        for path in sorted(self.checksums.keys ()):
            checksum = self.checksums[path]
            MANIFEST += "{0}  {1}\n".format(checksum, path)
        self.add_data(self.manifest, MANIFEST)

    def create_temp_file(self, filedata):
        # pylint: disable-msg = R0201
        """Return the name of a temporary file containing the file data."""
        (tmpfile, tmpname) = tempfile.mkstemp(".zbtmp")
        os.write(tmpfile, filedata)
        os.close (tmpfile)
        return tmpname

    def _v_call(self, method):
        # pylint: disable-msg = R0201
        """Pure virtual call: assert a failure."""
        assert False, "Call to pure virtual function '%s'" % (method,)

    def md5sum(self, pathname):
        # pylint: disable-msg = R0201
        """Generate the MD5 checksum for file."""
        result = hashlib.md5()
        try:
            infile = open(pathname, "rb")
            while True:
                data = infile.read(1024)
                if not data:
                    break
                result.update(data)
            infile.close()
        except IOError, error:
            print "Error: %s" % (str(error),)
        return result.hexdigest()

class TarDestination(Destination):
    """A .tar file destination."""
    def __init__(self, quiet, prefix, name, manifest="MANIFEST"):
        """Constructor"""
        Destination.__init__(self, quiet, prefix, name, manifest)
        self.tar_handle = None
        self.filename = name
        self.mode = "w"
        if self.name.endswith(".gz"):
            self.mode = "w:gz"
            self.type = "gzipped-tar"
        elif self.name.endswith(".bz2"):
            self.mode = "w:bz2"
            self.type = "bzipped-tar"
        try:
            self.tar_handle = tarfile.open(self.filename, self.mode)
            self.is_opened = True
        except (IOError, ValueError), error:
            # Can be raised if :gz, etc. is not recognized in the mode
            print "Error: %s" % (str(error),)
            sys.exit(1)

    def add_dir(self, dirname, renamed):
        # pylint: disable-msg = W0613
        """Add a directory to the destination."""
        self.tar_handle.add(dirname, arcname=renamed, recursive=False)

    def add_file(self, pathname, destname):
        """Add a file to the tar file."""
        self.tar_handle.add(pathname, destname)

    def close(self):
        """Close the tar file."""
        Destination.close(self)
        self.tar_handle.close()

class ZipDestination(Destination):
    """A .tar file destination."""
    def __init__(self, quiet, prefix, name, manifest="MANIFEST"):
        """Constructor"""
        Destination.__init__(self, quiet, prefix, name, manifest)
        self.zip_handle = None
        self.filename = name
        try:
            self.zip_handle = zipfile.ZipFile(self.filename, "w", zipfile.ZIP_DEFLATED)
            self.is_opened = True
        except (IOError, ValueError), error:
            # Can be raised if :gz, etc. is not recognized in the mode
            print "Error: %s" % (str(error),)
            sys.exit(1)

    def add_dir(self, dirname, renamed):
        """Add a directory to a zip, this is a no-op as zip files require
           directories have files, the directory will be implicitly added
           with files."""
        pass

    def add_file(self, pathname, destname):
        """Add a file to the zip file."""
        self.zip_handle.write(pathname, destname)

    def close(self):
        """Close the zip file."""
        Destination.close(self)
        self.zip_handle.close()
