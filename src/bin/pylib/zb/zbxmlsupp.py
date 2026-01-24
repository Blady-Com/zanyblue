#  -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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
XML parsing support routines.
"""

import zb.locale

def add_value(args, index, value):
    """
    Add a value to a list at a specific index.  If the list does
    extend to the index, simply add None values for the missing
    entries.
    """
    args.extend([None for _ in range(index + 1 - len(args))])
    args[index] = value


def qualifiers_match(dest_name, element, args, qualifiers, debug):
    """
    Determine if a qualifier matches an XML element.
    """
    result = True
    if element.getAttribute("alt") != '':
        return False
    for qualifier in qualifiers:
        for attribute in list(qualifier.keys()):
            match_value = qualifier[attribute]
            if attribute == "name":
                value = element.nodeName
            else:
                value = element.getAttribute(attribute)
            if match_value[0] == '#':
                arg_num = int(match_value[1:])
                if debug:
                    print(("  ", dest_name, "arg", arg_num, "=>", value))
                if value == '':
                    return False
                add_value(args, arg_num, value)
            elif match_value[0] == "*":
                if debug:
                    print(("    value", dest_name, "=>", value))
                if value == '':
                    return False
                for char in value:
                    # Exclude any Wide_Wide_String values!
                    if ord(char) > 65536:
                        return False
                result = value
            elif match_value[0] == "~":
                if value != '':
                    return False
            elif match_value != value:
                return False
        element = element.parentNode
    return result


def element_value(element):
    """
    Return the accumulated text over child nodes for an element.
    """
    result = ""
    for node in element.childNodes:
        if node.nodeType == node.TEXT_NODE:
            result += node.data
    return result


def parse_element(obj, dom, dest_name, qualifiers, debug=False):
    """
    Parse a CLDR element.
    """
    for element in dom.getElementsByTagName(qualifiers[0]["name"]):
        args = []
        match = qualifiers_match(
            dest_name,
            element,
            args,
            qualifiers,
            debug
        )
        if match:
            parameter = dest_name.format(*args)
            if type(match).__name__ == 'bool':
                value = element_value(element)
            else:
                value = match
            try:
                obj[parameter] = value
                if debug:
                    print((parameter, "=>", value))
            except zb.locale.InvalidPathError:
                if debug:
                    print(("FAIL:", parameter, "=>", value))
        elif debug:
            print((dest_name, "not found"))
