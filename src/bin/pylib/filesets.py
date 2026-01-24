#!/usr/bin/env python
# -*- encoding: utf-8 -*-

"""
filesets: Class to collect files based on their Subversion status and
the path name.
"""

import os
import re


class Filesets(object):
    """
    Collect file names into various categories based on the Subversion status
    (matched as a regular expression, and the path name, also matched as a
    regular expression.
    """

    _UNCATEGORIZED = "**uncategorized**"

    def __init__(self):
        """
        Constructor: initialize the cateogies to handle the uncategorized
        files, i.e., files that didn't match any Subversion status and
        path name rules.

        The rule set is iniitally empty, sending all files to the uncategoried
        category.
        """
        self.category_files = {Filesets._UNCATEGORIZED: []}
        self.rules = []
        self.private_dirs = []

    def add_category_rule(self, category, status, path):
        """
        Add a Subversion status regular expression and path name regular
        expression rule to the named category.  The category is created
        if it doesn't already exist, i.e., initialized to the empty set
        of files.
        """
        self.rules.append({
            'category': category,
            'status_rc': re.compile(status),
            'path_rc': re.compile(path)
        })
        if category not in self.category_files:
            self.category_files[category] = []

    def add_path(self, status, path):
        """
        Add a path with Subversion status value to the set of files.  The
        first rule that matches both the status and path name applies.  If
        no rule applies, the file is sent to the uncategoried set.
        """
        for rule in self.rules:
            if rule['status_rc'].match(status) and rule['path_rc'].match(path):
                self.category_files[rule['category']].append(path)
                return
        self.category_files[Filesets._UNCATEGORIZED].append(path)

    def get_category_files(self, category):
        """
        Get the set of files categoried, via the rules added, to the named
        category.
        """
        return self.category_files[category]

    def get_uncategorized_files(self):
        """
        Get the set of uncategorized files.
        """
        return self.category_files[Filesets._UNCATEGORIZED]

    def svn_add(self, fullname, path):
        """
        Add a file where the status must be determined by querying Subversion.
        """
        for pdir in self.private_dirs:
            if path.startswith(pdir):
                self.add_path("?", path)
                return
        cmd = "svn status --depth=empty \"{0}\" 2>/dev/null".format(fullname)
        status = os.popen(cmd).read()[:8]
        if os.path.isdir(fullname) and len(status) > 0 and status[0] == "?":
            self.private_dirs.append(path)
        self.add_path(status, path)
