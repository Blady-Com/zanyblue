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
ZBDEV implementation modules: base command class and the driver class to
manage execution parameters.
"""

import argparse
import codecs
import glob
import os
import platform
import re
import subprocess
import sys
import time
import yaml
from six import string_types


_HERE = os.path.dirname(__file__)
_VERSION_RC = re.compile(r'(?P<name>V_[A-Z]+)\s*=\s*(?P<value>\d+)')


class CommandError(Exception):
    """
    Generate exception used to indicate an error during the execution
    of a command.
    """
    pass


class ZBDev(object):
    """
    Main class used as the "engine" to handle parameters, etc., for
    the commands used to implement the "zbdev" commands.
    """

    def __init__(self, zbdev_dir):
        """
        Constructor.
        """
        self.params = {}
        self.impl_cache = {}
        self.parser = None
        self.subparser = None
        self._init_params(zbdev_dir)
        self._set_version()
        self._set_path()
        self._set_envdefs()
        self._init_argparser()
        self._load_commands()

    def _dirtree_find(self, filename, start_dir):
        """
        Iterator generating the instances of the given file name from the
        start_dir upto the root directory of the file system, e.g., the
        sequence of files generated might be

            /u/mrohan/clients/curwork/lib/python/osstpmgt.json
            /u/mrohan/clients/osstpmgt.json
            /u/mrohan/osstpmgt.json
        """
        curdir = start_dir or os.getcwd()
        done = False
        while not done:
            test_file = os.path.join(curdir, filename)
            if os.access(test_file, os.R_OK):
                yield test_file
            newdir = os.path.dirname(curdir)
            done = (newdir == curdir)
            curdir = newdir

    def _init_params(self, zbdev_dir):
        """
        Initial, built-in parameters for the zbdev engine and load the platform
        parameters.
        """
        os_name, os_version, os_id = platform.dist()
        data_dir = os.path.join(zbdev_dir, "data")
        top_dir = os.path.abspath(
            os.path.join(zbdev_dir, os.pardir, os.pardir)
        )
        if not os_name:
            # Fallback on uname if dist didn't work, e.g., FreeBSD
            os_name = platform.uname()[0]
            os_version = platform.uname()[2]
        for name in ['zbdev', os_name.lower()]:
            self._load_yaml(os.path.join(data_dir, "{0}.yaml".format(name)))
        for name in self._dirtree_find("zbdev.yaml", zbdev_dir):
            self._load_yaml(name)
        self.params['home'] = os.environ["HOME"]
        self.params['zbdev_dir'] = zbdev_dir
        self.params['data_dir'] = data_dir
        self.params['os_name'] = os_name
        self.params['os_version'] = os_version
        self.params['os_id'] = os_id
        self.params['top_dir'] = top_dir
        self.params['copyright_year'] = time.localtime().tm_year
        # Finally environment definitions get pulled in
        for name in os.environ:
            if not name.startswith("ZBDEV_"):
                continue
            self.params[name.replace("ZBDEV_", "").lower()] = os.environ[name]

    def _init_argparser(self):
        self.parser = argparse.ArgumentParser(
            prog='zbdev'
        )
        self.parser.add_argument(
            '-P',
            metavar='PARAMETER=VALUE',
            dest='parameters',
            default=[],
            action='append',
            help='Set a parameter value'
        )
        self.subparsers = self.parser.add_subparsers(
            dest='subcommand',
            help='sub-command help'
        )

    def _load_commands(self):
        wildcard = "{0}{1}cmd_*.py".format(
            os.path.join(os.path.dirname(__file__)),
            os.path.sep,
        )
        for cmd in sorted(glob.glob(wildcard)):
            cmdname = cmd[cmd.rfind("cmd_")+4:-3]
            impl = self.get_impl(cmdname)
            impl.add_options(
                cmdname,
                self.subparsers.add_parser(
                    cmdname,
                    description=impl.__doc__
                )
            )
            #print("HELP: {0}\n".format(impl.__doc__))

    def _load_yaml(self, pathname):
        try:
            with codecs.open(pathname, "r", "utf-8") as src:
                defs = yaml.load(src)
            for name, value in list(defs.items()):
                self.params[name] = value
        except IOError:
            pass

    def _param_expand(self, value):
        if isinstance(value, dict):
            for key in list(value.keys()):
                value[key] = self._param_expand(value[key])
        elif isinstance(value, list):
            for i, val in enumerate(value):
                value[i] = self._param_expand(val)
        elif isinstance(value, string_types):
            count = 0
            while count < 10 and '%(' in value:
                value = value % self.params
                count += 1
        return value

    def _set_path(self):
        for extra_path in self.get_param('extra_paths'):
            self.prepend_path(extra_path)

    def _set_envdefs(self):
        for envdef in self.get_param('envdefs'):
            try:
                name, value = envdef.split("=", 1)
                self.info("Adjusting environment to include \"{0}\"", envdef)
                os.environ[name] = value
            except ValueError:
                self.error("Invalid envdef: \"{0}\"".format(envdef))

    def _set_version(self):
        if 'SVN_REVISION' in os.environ:
            self.params['revision'] = os.environ['SVN_REVISION']
        else:
            self.params['revision'] = os.popen("{0} -n {1}".format(
                self.get_param("svnversion"),
                self.get_param("top_dir")
            )).read()
        version_mk = os.path.join(
            self.get_param("top_dir"),
            self.get_param("version_mk"),
        )
        self.set_param("version", "0.0.0")
        if not os.path.isfile(version_mk):
            # Not in a full ZB source tree, the version doesn't
            # apply, just leave it at 0.0.0
            return
        with codecs.open(version_mk, "r", "utf-8") as src:
            for line in src.readlines():
                matches = _VERSION_RC.match(line)
                if matches:
                    self.set_param(
                        matches.group('name').lower(),
                        matches.group('value')
                    )
        version_str ="{0}.{1}.{2}".format(
            self.get_param("v_major"),
            self.get_param("v_minor"),
            self.get_param("v_patch"),
        )
        self.set_param("version", version_str)
        os.environ['ZB_VERSION'] = version_str

    def format_msg(self, fmt, *args):   # pylint: disable=no-self-use
        """
        Format a message given a format string and the list of arguments.
        """
        return fmt.format(*args)

    def info(self, fmt, *args):
        """
        Informational message.
        """
        sys.stderr.write("{0}\n".format(self.format_msg(fmt, *args)))

    def warning(self, fmt, *args):
        """
        Warning message.
        """
        self.info(fmt, *args)
        return 1

    def error(self, fmt, *args):
        """
        Error message.
        """
        self.info(fmt, *args)
        return 1

    def fatal(self, fmt, *args):
        """
        Fatal error encountered, raise a CommandError on the formatted
        message.
        """
        raise CommandError(self.format_msg(fmt, args))

    def commands(self):
        """
        Return, as a list, the set of known commands.
        """
        return sorted(self.impl_cache.keys())

    def get_impl(self, command):
        """
        Return the implementation object for a command.  If the command has
        not already been loaded, load it using the find_impl
        """
        if command not in self.impl_cache:
            self.impl_cache[command] = self.find_impl(command)
        return self.impl_cache[command]

    def find_impl(self, command):
        """
        Load and return the implementation class for a command.  This is
        and instance of the Handler class in the "cmd_NAME.py" module named
        "Impl".
        """
        try:
            klass = getattr(
                __import__(
                    "zb.cmd_{}".format(command),
                    fromlist=['zb']
                ),
                'Impl'
            )
            return klass(self)
        except Exception as ex:
            print(("Exception: {0}".format(ex)))
            raise

    def get_param(self, name, expand=True):
        """
        Get a parameter value.  If expand is True (the default), references to
        other parameters in the value are replaced.
        """
        if expand:
            result = self._param_expand(self.params.get(name, None))
        else:
            result = self.params.get(name, None)
        if isinstance(result, list):
            # Force a copy to prevent accidental updates to the params
            result = result[:]
        return result

    def main(self):
        """
        Main driver routine.
        """
        options = self.parser.parse_args()
        command = options.subcommand
        for paramdef in options.parameters:
            param, value = paramdef.split('=')
            self.set_param(param, value)
        impl = self.get_impl(command)
        try:
            return impl.handle(command, options)
        except CommandError as ex:
            print(("Exception: {0}".format(ex)))
            return 1

    def prepend_path(self, extra_path):
        """
        Prepend a path to the system PATH env list.
        """
        self.info("Adjusting PATH to include \"{0}\"", extra_path)
        paths = os.environ['PATH'].split(os.pathsep)
        paths.insert(0, extra_path)
        os.environ['PATH'] = os.pathsep.join(paths)

    def read_file(self, filename, binary=False):
        """
        Read a file relative to the "top_dir" the entire file contents are
        returned.
        """
        topdir = self.get_param("top_dir")
        fullpath = os.path.join(topdir, filename)
        if binary:
            with open(fullpath, "rb") as src:
                return src.read()
        else:
            with codecs.open(fullpath, "r", "utf-8") as src:
                return src.read()

    def set_param(self, name, value):
        """
        Set a parameter value: simply update/add it to the parameter set.
        """
        self.params[name] = value


class Handler(object):
    """
    Base class used for commands.
    """

    def __init__(self, zbdev):
        """
        Constructor.
        """
        self.zbdev = zbdev

    def format_msg(self, fmt, *args):
        """
        Format a message given a format string and the list of arguments.
        """
        return self.zbdev.format_msg(fmt, *args)

    def info(self, fmt, *args):
        """
        Informational message.
        """
        self.zbdev.info(fmt, *args)

    def warning(self, fmt, *args):
        """
        Warning message.
        """
        return self.zbdev.warning(fmt, *args)

    def error(self, fmt, *args):
        """
        Error message.
        """
        return self.zbdev.error(fmt, *args)

    def fatal(self, fmt, *args):
        """
        Fatal error encountered, raise a CommandError on the formatted
        message.
        """
        self.zbdev.fatal(fmt, *args)

    def add_options(self, name, subparsers):   # pylint: disable=no-self-use
        """
        Base "add_options" routine.  Individual commands should over-ride this
        to add options specific to their implementation.
        """
        pass

    def execute_command(self, *cmdargs):
        self.info("Executing the command: \"{0}\"".format(" ".join(cmdargs)))
        subprocess.check_call(cmdargs)

    def find_ws(self, ws_name):
        for dirname in self.get_param("ws_dirs"):
            location = os.path.join(dirname, ws_name)
            if self.is_ws(location):
                return location
        return None

    def get_param(self, name, expand=True):
        """
        Return the value of a parameter.  Dispatch to the ZBDev method.
        """
        return self.zbdev.get_param(name, expand)

    def get_ws_version(self, ws_name, location):
        status, version = subprocess.getstatusoutput("{0} {1}".format(
            self.get_param("svnversion"), location
        ))
        if status != 0:
            version = "ERR"
        return version.strip()

    def handle(self, command, options):   # pylint: disable=unused-argument
        """
        Base "handle" routine: implementation of the command.  Individual
        commands must over-ride this.
        """
        self.fatal("The command \"{0}\" is not implemented", command)

    def is_ws(self, location):
        svn_dir = os.path.join(location, ".svn")
        return os.path.isdir(location) and os.path.isdir(svn_dir)

    def prepend_path(self, extra_path):
        """
        Prepend a path to the system PATH env list.
        """
        self.zbdev.prepend_path(extra_path)

    def read_file(self, filename, binary=False):
        """
        Return the entire contents of a given file.  Dispatch the to the
        ZBdev method.
        """
        return self.zbdev.read_file(filename, binary)

    def set_param(self, name, value):
        """
        Set a parameter value, just add it to the set known by ZBDev.
        """
        return self.zbdev.set_param(name, value)


def main(zbdev_dir):
    """
    Main driver routine for the zbdev command.  Create a ZBDev object
    and invoke it's main routine.
    """
    try:
        return ZBDev(zbdev_dir).main()
    except CommandError as ex:
        print(("Exception: {0}".format(ex)))
