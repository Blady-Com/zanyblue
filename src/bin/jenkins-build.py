#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
# All rights reserved.
#
"""
Simple script used to build ZanyBlue under Jenkins control: distribution
build with included Google analytics code.
"""
from __future__ import unicode_literals

import argparse
import os
import shutil
import subprocess
import sys
from datetime import datetime

_VIRTUAL_ENV_DIR = "venv"
_GNAT_PATH = "/usr/gnat"
_BUILD_TARGETS = ["clean", "all", "xcheck"]
_DISTRIB_TARGET = ["inventory", "distribution", "verify-distribution"]


def make(topdir, make_cmd, makedefs, targets):
    cmd = [make_cmd]
    cmd.extend(makedefs)
    srcdir = os.path.join(topdir, "src")
    for target in targets:
        subprocess.check_call(cmd + [target], cwd=srcdir)


def prepend_path(newpath):
    os.environ['PATH'] = os.pathsep.join(
        [os.path.join(newpath, "bin")] + os.environ['PATH'].split(os.pathsep)
    )


def setup_virtual_env(topdir, new_venv, venvdir):
    """
    Setup the virtual environment, if not already in place.
    """
    cur = os.environ.get("VIRTUAL_ENV", None)
    if cur is not None:
        print("Already in virtual environment located at \"{0}\"".format(
            cur
        ))
        return
    if new_venv and os.path.isdir(venvdir):
        print("Removing existing virtual environment \"{0}\"".format(
            venvdir
        ))
        shutil.rmtree(venvdir)
    print("Setting up the virtual environment \"{0}\"".format(venvdir))
    if not os.path.isdir(venvdir):
        print("Creating a new virtual environment in \"{0}\"".format(
            venvdir
        ))
        subprocess.check_call(["virtualenv", venvdir])
    # Add virtualenv path to PATH ". venv/bin/activate"
    prepend_path(venvdir)
    # Install the ZB requirements
    subprocess.check_call([
        "pip",
        "install",
        "-r",
        os.path.join(topdir, "src/admin/requirements.txt"),
    ])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--gnatdir",
        dest="gnatdir",
        default=None,
        help="Location of the GNAT compiler installation",
    )
    parser.add_argument(
        "--make",
        dest="make",
        default="make",
        help="Define the system command for make, e.g., 'gmake'",
    )
    parser.add_argument(
        "--build-only",
        dest="build_only",
        default=False,
        action='store_true',
        help="Perform a build only, no packaging or verification",
    )
    parser.add_argument(
        "--virtualenv",
        dest="venvdir",
        default=None,
        required=True,
        help="Location of the Python virtual environment for the build",
    )
    parser.add_argument(
        "--new-venv",
        dest="new_venv",
        default=False,
        action="store_true",
        help="Create a new virtual environment for the build",
    )
    parser.add_argument(
        "--makedef",
        dest="makedefs",
        default=[],
        action="append",
        help="Make definitions, e.g., --make-def GNATDOC=:",
    )
    args = parser.parse_args()
    venvdir = os.path.abspath(args.venvdir)
    topdir = os.path.abspath(os.path.join(
        os.path.dirname(__file__),
        "..", ".."
    ))
    print("Top dir is \"{0}\"".format(topdir))
    print("Virtual env dir is \"{0}\"".format(venvdir))
    setup_virtual_env(topdir, args.new_venv, venvdir)
    if args.gnatdir:
        prepend_path(args.gnatdir)
    makedefs = [
        # SVN_REVISION set by Jenkins
        "SVN_VERSION={0}".format(os.environ.get("SVN_REVISION", "") or "EXT"),
        # Enable Google Analytics via mkwebbundle
        "MKWEBBUNDLE_OPTIONS=-p",
        # Copyright year is current year
        "COPYRIGHT_YEAR={0}".format(str(datetime.now().year))
    ]
    makedefs.extend(args.makedefs)
    if args.build_only:
        targets = _BUILD_TARGETS
    else:
        targets = _DISTRIB_TARGET
    make(topdir, args.make, makedefs, targets)
    return 0


if __name__ == "__main__":
    sys.exit(main())
