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
ZBDEV SCM implementation modules: base SCM class.
"""

import traceback


class SCMError(Exception):
    """
    General SCM related exceptions
    """
    pass


class SCM(object):
    """
    Base (abstract) class for SCM implementation, core functionality and
    abstract methods for SCM operations.
    """

    def __init__(self, params, addr, branch, path="."):
        """
        Constructor
        """
        self._params = params
        self._addr = addr
        self._branch = branch
        self._path = path

    def addr(self, value=None):
        """
        Get (or set) the address of the SCM system, e.g., "git://git..."
        """
        result = self._addr
        if value:
            self._addr = value
        return result

    def branch(self, value=None):
        """
        Get (or set) the branch for the workspace
        """
        result = self._branch
        if value:
            self._branch = value
        return result

    def checkout(self):
        """
        Checkout a copy of the source from the SCM system (abstract)
        """
        self._abstract()

    def execute_command(self, *cmdargs):
        """
        Execute a command, normally the SCM command to perform checkouts, etc.
        """
        return self._params.execute_command(*cmdargs)

    def get_param(self, name):
        """
        Get a ZBDev parameter (zbdev.yaml, etc).
        """
        return self._params.get_param(name)

    def path(self, value=None):
        """
        Get (or set) the path for the workspace
        """
        result = self._path
        if value:
            self._path = value
        return result

    def status(self):
        """
        SCM status of a workspace (abstract), modified, etc.
        """
        self._abstract()

    def _abstract(self):
        """
        Raise an exception for an unimplemented abstract method
        """
        raise SCMError("Not implemented: \"{0}:{1}\"".format(
            self.__class__.__name__,
            traceback.extract_stack(limit=2)[0][2]
        ))


def get_scm(params, implname, addr, branch, path):
    """
    Return an SCM implementation for the named SCM
    """
    if implname == 'svn':
        from zb.scm.svn import SVN
        return SVN(params, addr, branch, path)
    else:
        raise SCMError("No SCM implementation for \"{0}\"".format(implname))
