#!/usr/bin/env python
# -*- encoding: utf-8 -*-
"""
docsubst.py VAR1=VALUE1 VAR2=VALUE2 src dst

Copy src to dst replacing any occurences of the command line VARs with their
values.
"""

import sys


def error(fmt, *args):
    msg = fmt % args
    sys.stderr.write("%s\n" % msg)
    return 1


def main():
    variables = {}
    srcname = None
    dstname = None
    for arg in sys.argv[1:]:
        if '=' in arg:
            var, value = arg.split('=')
            variables[var] = value
        elif srcname is None:
            srcname = arg
        elif dstname is None:
            dstname = arg
        else:
            return error("Error: Unexpected command line argument \"%s\"", arg)
    if srcname is None:
        return error("Error: no source file defined")
    if dstname is None:
        return error("Error: no destination file defined")
    src = open(srcname, "r")
    dst = open(dstname, "w")
    for line in src:
        for key, value in variables.iteritems():
            line = line.replace(key, value)
        dst.write(line)
    dst.close()
    src.close()
    return 0

if __name__ == "__main__":
    sys.exit(main())
