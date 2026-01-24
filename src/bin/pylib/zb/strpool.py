# -*- coding: utf-8 -*-

"""
Simple class to accumulate a set of strings mapping them to an index value.
Same string gets mapped to the same index.  The corresponding Ada code
generated create a Wide_String containing all the strings added and a mapping
from string index value to start and end of the sub-string with the accumulated
string corresponding to the index value.
"""

from .zbmsg import *

class StringPool(object):
    """
    Class accumulating strings with index mapping.
    """

    empty_string = 1
    """
    Index of the empty string (constant 1).
    """

    def __init__(self):
        """
        Constructor, initialize the string to index mapping and index to
        sub-string list.  The accumulated string is stored in the "pool"
        attribute.

        The object is initialized with the empty string (string index 1)
        """
        self.string_mapping = {}
        self.index_addresses = []
        self.n_saved = 0
        self.string_set = set()
        self.pool = u""
        self.index(u"")

    def index(self, val):
        """
        Return the index associated with a string.  If the string is not
        already present, add it.
        """
        if not self.string_mapping.has_key(val):
            index = len(self.index_addresses) + 1
            start = self.pool.find(val)
            if start == -1:
                start = len(self.pool) + 1
                self.pool += val
            else:
                self.n_saved += len(val)
                start = start + 1
            end = start + len(val) - 1
            self.string_mapping[val] = index
            self.index_addresses.append((start, end, index))
        result = self.string_mapping[val]
        return result

    def accumulate(self, val):
        self.string_set.add(val)
        return val

    def resolve_strings(self):
        for val in sorted(self.string_set, reverse=True):
            self.index(val)

    def value(self, index):
        start, end, index = self.index_addresses[index - 1]
        return self.pool[start - 1:end]

    def _splitCount(self, s, n):
        """
        Split a string into block of n characters.
        From http://code.activestate.com/recipes/496784/ (r1)
        """
        result = [''.join(x) for x in zip(*[list(s[z::n]) for z in range(n)])]
        n_consumed = n * len(result)
        if len(s) > n_consumed:
           result.append(s[n_consumed:])
        return result

    def sort(self):
        self.index_addresses.sort(key=lambda v: self.pool[v[0] - 1:v[1]])
        mapping = [ 0 for i in range(len(self.index_addresses)) ]
        for address, value in enumerate(self.index_addresses):
            mapping[value[2] - 1] = address + 1
        return mapping

    def stats(self):
        return len(self.pool), self.n_saved

    def write(self, fp):
        """
        Generate the Ada code corresponding the the accumulated string data.
        """
        zbm_write(fp, ZBMSG0001)
        for block in self._splitCount(self.pool, 60):
            zbm_write(fp, ZBMSG0002, block)
        zbm_write(fp, ZBMSG0003)
        zbm_write(fp, ZBMSG0007)
        zbm_write(fp, ZBMSG0004)
        last_index = len(self.index_addresses) - 1
        for index, address in enumerate(self.index_addresses):
            code = ZBMSG0005 if index < last_index else ZBMSG0006
            text = self.pool[address[0]-1:address[1]]
            zbm_write(fp, ZBMSG0054, index + 1, text)
            zbm_write(fp, code, index + 1, *address)
        zbm_new_line(fp)
