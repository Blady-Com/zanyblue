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
Representation of a list of locales
"""

import codecs

from copy import deepcopy
from xml.dom import minidom

from zb.zbmsg import *
from zb.locale import Locale
from zb.strpool import StringPool
from zb.zbxmlsupp import parse_element

_END_CLDR_DATA = "END-CLDR-DATA"
_BEGIN_CLDR_DATA = "BEGIN-CLDR-DATA"


def info(fmt, *args):
    """
    Print an informational message.
    """
    print((fmt.format(*args)))


class LocaleList(object):

    def __init__(self, cldr_dir, ascii_only):
        self.cldr_dir = cldr_dir
        self.ascii_only = ascii_only
        self.loaded_locales = {}
        self.locales = []
        self.numbering_systems = {}
        self.load_numbering_systems()

    def add_locale(self, name):
        index = len(self.locales)
        locale = Locale(name)
        self.locales.append(locale)
        return index

    def load_numbering_systems(self):
        self.numbering_systems = {}
        numberingfile = "common/supplemental/numberingSystems.xml"
        xmlfile = "{0}/{1}".format(self.cldr_dir, numberingfile)
        info("Loading the numbering systems definitions from \"{0}\"", xmlfile)
        dom = self.load_dom(xmlfile)
        parse_element(self.numbering_systems, dom, "{0}", [
            {"name": "numberingSystem", "type": "numeric",
             "id": "#0", "digits": "*"},
            {"name": "numberingSystems"},
            {"name": "supplementalData"}
        ])
        for name in list(self.numbering_systems.keys()):
            info("Loaded numbering system \"{0}\"", name)

    def set_root_values(self, index):
        day_period_file = "common/supplemental/dayPeriods.xml"
        xmlfile = "{0}/{1}".format(self.cldr_dir, day_period_file)
        dom = self.load_dom(xmlfile)
        self.locales[index].set_root_values(dom)

    def set_value(self, index, path, value):
        return self.locales[index].set_value(path, value)

    def get_value(self, index, path):
        return self.locales[index].get_value(path)

    def load_locale(self, name, verbose):
        xmlfile = "{0}/common/main/{1}.xml".format(self.cldr_dir, name)
        dom = self.load_dom(xmlfile)
        if dom is None:
            info("Skipping missing locale definition file \"{0}\"", xmlfile)
            return
        index = self.add_locale(name)
        locale = self.locales[index]
        n_defs = locale.parse(dom)
        if verbose:
            info("Loaded {0} definitions from \"{1}\"", n_defs, xmlfile)
        self.loaded_locales[name] = index
        return index

    def load_dom(self, path):
        try:
            fp = codecs.open(path, "rb", 'UTF-8')
        except IOError:
            return None
        xmldata = fp.read()
        fp.close()
        return minidom.parseString(xmldata.encode("utf-8"))

    def find(self, level, l, s, t):
        name = "{0:<3}{1:<4}{2:<3}".format(l, s, t)
        for locale in self.locales:
            if locale.tag == name and locale.level < level:
                return locale
        return None

    def find_parent(self, name, level):
        l, s, t = name[0:3], name[3:7], name[7:10]
        return (self.find(level, l, s, "")
               or self.find(level, l, "", t)
               or self.find(level, l, "", "")
               or self.find(level, "", "", ""))

    def resolve_locale_aliases(self, verbose):
        info("Loading alias support locales")
        while True:
            alias_locales = []
            for locale in self.locales:
                alias = locale.get_value("naming/alias")
                if alias is not None:
                    if alias not in self.loaded_locales:
                        alias_locales.append(alias)
            if len(alias_locales) == 0:
                break
            for alias in alias_locales:
                self.load_locale(alias, verbose)
        for locale in self.locales:
            alias = locale.get_value("naming/alias")
            if alias is not None:
                parent = self.locales[self.loaded_locales[alias]]
                info("Transferring aliased locale from \"{0}\" to \"{1}\"",
                    parent.tag, locale.tag
                )
                locale.locale_data = deepcopy(parent.locale_data)

    def resolve_locales(self, verbose):
        self.apply_numbering_systems()
        self.apply_day_periods()
        n_resolutions = 0
        for i in range(1, 5):
            for locale in self.locales:
                if locale.level == i:
                    parent = self.find_parent(locale.tag, locale.level)
                    if parent is not None:
                        n_resolutions += locale.resolve_locale(parent, verbose)
        info("Resolved total of {0} items", n_resolutions)

    def apply_numbering_systems(self):
        for locale in self.locales:
            system = locale.get_value('*numberingSystem')
            if system is not None:
                value = self.numbering_systems[system]
                info("Applying the numbering system \"{0}\" to locale \"{1}\"",
                    system, locale.name
                )
                locale.set_value("naming/numeric/numericDigits", value)
                locale.set_value("naming/numeric/nativeZeroDigit", value[0])

    def apply_day_periods(self):
        root = self.locales[0]
        day_period_info = root.get_value('*dayperiod')
        for locale in self.locales[1:]:
            name = locale.name
            rules = day_period_info.get(name, None)
            if rules is not None:
                locale.apply_day_period_rules(rules)

    def write_code(self):
        strpool = StringPool(self.ascii_only)
        for ns in list(self.numbering_systems.keys()):
            info("Adding digits for \"{0}\" numbering system: {1}",
                  ns, self.numbering_systems[ns]
            )
            strpool.index(self.numbering_systems[ns])
        for locale in self.locales:
            locale.apply_locale_fn(strpool.accumulate)
        strpool.resolve_strings()
        for locale in self.locales:
            locale.apply_locale_fn(strpool.index)
        self.locales.sort(key=lambda l: l.tag)
        self.write_locale_data(strpool)
        self.write_pool(strpool)
        self.write_string_addresses(strpool)
        stored, saved = strpool.stats()
        info("Stored {0} char, saved {1} char, {2:.2f}% savings",
            stored, saved, 100.0 * saved / (stored + saved)
        )

    def write_locale_data(self, strpool):
        fp = codecs.open("zanyblue-text-locales-locale_data.adb", "w", "utf-8")
        zbm_write(fp, ZBMSG1001, len(self.locales))
        last_index = len(self.locales) - 1
        for index, locale in enumerate(self.locales):
            locale.write(fp, index + 1, strpool, index == last_index)
        zbm_new_line(fp)
        zbm_write(fp, ZBMSG1002, "Locale_Data")
        fp.close()

    def write_pool(self, strpool):
        fp = codecs.open("zanyblue-text-locales-pool.adb", "w", "utf-8")
        zbm_write(fp, ZBMSG1003)
        strpool.write_pool(fp)
        zbm_new_line(fp)
        zbm_write(fp, ZBMSG1002, "Pool")
        fp.close()

    def write_string_addresses(self, strpool):
        fp = codecs.open(
            "zanyblue-text-locales-string_addresses.adb",
            "w",
            "utf-8"
        )
        zbm_write(fp, ZBMSG1004)
        strpool. write_string_addresses(fp)
        zbm_write(fp, ZBMSG1002, "String_Addresses")
        fp.close()

    def write(self, fp, package):
        zbm_write(fp, ZBMSG0040, package)
        zbm_new_line(fp)
        zbm_write(fp, ZBMSG0046)
        zbm_write(fp, ZBMSG0047)
        zbm_write(fp, ZBMSG0048)
        zbm_write(fp, ZBMSG0049)
        zbm_write(fp, ZBMSG0050)
        zbm_write(fp, ZBMSG0051)
        zbm_write(fp, ZBMSG0052)
        self.embed(fp)
        zbm_write(fp, ZBMSG0043)
        zbm_write(fp, ZBMSG0044)
        zbm_new_line(fp)
        zbm_write(fp, ZBMSG0045, package)

    def write_properties(self, props_dir, base):
        # Skip the root locale when writing properties
        for index, locale in enumerate(self.locales[1:]):
            locale.write_properties(index, props_dir, base)
