#!/usr/bin/python -tt
# -*- coding: utf-8 -*-

"""
Representation of a list of locales: 
"""

import codecs
import os
import sys

from copy import deepcopy
from xml.dom import minidom

from .zbmsg import *
from .locale import Locale
from .strpool import StringPool
from .zbxmlsupp import parse_element

_END_CLDR_DATA = "END-CLDR-DATA"
_BEGIN_CLDR_DATA = "BEGIN-CLDR-DATA"

class LocaleList(object):

    def __init__(self, cldr_dir):
        self.cldr_dir = cldr_dir
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
        print "Loading the numbering systems definitions from \"{0}\"".format(xmlfile)
        dom = self.load_dom(xmlfile)
        parse_element(self.numbering_systems, dom,
                           "{0}",
                           [ { "name": "numberingSystem",
                               "type": "numeric",
                               "id": "#0",
                               "digits": "*" },
                             { "name": "numberingSystems", },
                             { "name": "supplementalData", } ])
        for name in self.numbering_systems.keys():
            print "Loaded numbering system \"{0}\"".format(name)

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
            print "Skipping missing locale definition file \"{0}\"".format(xmlfile)
            return
        index = self.add_locale(name)
        locale = self.locales[index]
        n_defs = locale.parse(dom)
        if verbose:
            print "Loaded {0} definitions from \"{1}\"".format(n_defs,
                                                               xmlfile)
        self.loaded_locales[name] = index
        return index

    def load_dom(self, path):
        try:
            fp = codecs.open(path, "rb", 'UTF-8')
        except IOError:
            return None
        xmldata = unicode(fp.read())
        fp.close()
        return minidom.parseString(xmldata.encode("utf-8"))

    def find (self, level, l, s, t):
        name = "{0:<3}{1:<4}{2:<3}".format(l, s, t)
        for locale in self.locales:
            if locale.tag == name and locale.level < level:
                return locale
        return None

    def find_parent (self, name, level):
        l, s, t = name[0:3], name[3:7], name[7:10]
        return (self.find (level, l, s, "")
               or self.find (level, l, "", t)
               or self.find (level, l, "", "")
               or self.find (level, "", "", ""))

    def resolve_locale_aliases(self, verbose):
        print "Loading alias support locales"
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
                print "Transferring aliased locale from \"{0}\" to \"{1}\"".format(parent.tag, locale.tag)
                locale.locale_data = deepcopy(parent.locale_data)

    def resolve_locales(self, verbose):
        self.apply_numbering_systems()
        self.apply_day_periods()
        n_resolutions = 0
        for i in range(1, 5):
            for locale in self.locales:
                if locale.level == i:
                    parent = self.find_parent (locale.tag, locale.level)
                    if parent is not None:
                        n_resolutions += locale.resolve_locale(parent, verbose)
        print "Resolved total of {0} items".format(n_resolutions)

    def apply_numbering_systems(self):
        for locale in self.locales:
            system = locale.get_value('*numberingSystem')
            if system is not None:
                value = self.numbering_systems[system]
                print "Applying the numbering system \"{0}\" to locale \"{1}\"".format(system,
                                                               locale.name)
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

    def embed(self, embed_name):
        strpool = StringPool()
        for ns in self.numbering_systems.keys():
            print "Adding digits for \"{0}\" numbering system:".format(
                  ns), self.numbering_systems[ns]
            strpool.index(self.numbering_systems[ns])
        tmp_name = "{0}.zbtmp".format(embed_name)
        wfp = codecs.open(tmp_name, "w", "utf-8")
        rfp = codecs.open(embed_name, "r", "utf-8")
        for locale in self.locales:
            locale.apply_locale_fn(strpool.accumulate)
        strpool.resolve_strings()
        for locale in self.locales:
            locale.apply_locale_fn(strpool.index)
        in_data_block = False
        for line in rfp.readlines():
            if _END_CLDR_DATA in line:
                in_data_block = False
            if not in_data_block:
                wfp.write(line)
            if _BEGIN_CLDR_DATA in line:
                zbm_new_line(wfp)
                zbm_write(wfp, ZBMSG0041, len(self.locales))
                zbm_new_line(wfp)
                last_index = len(self.locales) - 1
                self.locales.sort(key=lambda l: l.tag)
                for index, locale in enumerate(self.locales):
                    locale.write(wfp, index + 1, strpool, index == last_index)
                zbm_new_line(wfp)
                strpool.write(wfp)
                in_data_block = True;
        wfp.close()
        rfp.close()
        os.remove(embed_name)
        os.rename(tmp_name, embed_name)
        print "Embedded locale data into the file \"{0}\"".format(embed_name)
        stored, saved = strpool.stats()
        print "Stored {0} char, saved {1} char, {2:.2f}% savings".format(
            stored, saved, 100.0*saved/(stored + saved))

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
