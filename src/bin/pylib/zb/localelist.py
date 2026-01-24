#!/usr/bin/python -tt
# -*- coding: utf-8 -*-

"""
Representation of a list of locales: 
"""

import codecs
import os

from xml.dom import minidom

from .zbmsg import *
from .locale import Locale
from .strpool import StringPool

_END_CLDR_DATA = "END-CLDR-DATA"
_BEGIN_CLDR_DATA = "BEGIN-CLDR-DATA"

class LocaleList(object):

    def __init__(self, cldr_dir, mklocales):
        self.strpool = StringPool()
        self.cldr_dir = cldr_dir
        self.mklocales = mklocales
        self.loaded_locales = {}
        self.locales = []

    def add_locale(self, name):
        index = len(self.locales)
        self.locales.append(Locale(name, self.strpool, self.mklocales))
        return index

    def set_root_values(self, index):
        self.locales[index].set_root_values()

    def set_value(self, index, path, value):
        return self.locales[index].set_value(path, value)

    def get_value(self, index, path):
        return self.locales[index].get_value(path)

    def load_locale(self, name, verbose):
        xmlfile = os.path.join(self.cldr_dir, "{0}.xml".format(name))
        try:
            fp = codecs.open(xmlfile, "rb", 'UTF-8')
        except IOError:
            print "Skipping missing locale definition file \"{0}\"".format(xmlfile)
            return
        xmldata = unicode(fp.read())
        fp.close()
        dom = minidom.parseString(xmldata.encode("utf-8"))
        index = self.add_locale(name)
        locale = self.locales[index]
        n_defs = locale.parse(dom)
        if verbose:
            print "Loaded {0} definitions from \"{1}\"".format(n_defs,
                                                               xmlfile)
        self.loaded_locales[name] = index
        return index

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

    def resolve_properties_aliases(self, verbose):
        print "Loading alias support locales"
        while True:
            alias_locales = []
            for locale in self.locales:
                alias = locale.get_value("alias")
                if alias is not None:
                    alias_name = self.strpool.value(alias)
                    if alias_name not in self.loaded_locales:
                        alias_locales.append(alias_name)
            if len(alias_locales) == 0:
                break
            for alias in alias_locales:
                self.load_locale(alias, verbose)
        for locale in self.locales:
            name = self.strpool.value(locale.name)
            alias = locale.get_value("alias")
            if alias is not None:
                alias_name = self.strpool.value(alias)
                source = self.locales[self.loaded_locales[alias_name]]
                dest = self.locales[self.loaded_locales[name]]
                dest.props_data = source.props_data
                print "Copied aliased locale from \"{0}\" to \"{1}\"".format(alias_name, name)

    def resolve_locale_aliases(self, verbose):
        print "Loading alias support locales"
        while True:
            alias_locales = []
            for locale in self.locales:
                alias = locale.get_value("naming/alias")
                if alias is not None:
                    alias_name = self.strpool.value(alias)
                    if alias_name not in self.loaded_locales:
                        alias_locales.append(alias_name)
            if len(alias_locales) == 0:
                break
            for alias in alias_locales:
                self.load_locale(alias, verbose)
        for locale in self.locales:
            alias = locale.get_value("naming/alias")
            if alias is not None:
                alias_name = self.strpool.value(alias)
                parent = self.locales[self.loaded_locales[alias_name]]
                print "Transferring aliased locale from \"{0}\" to \"{1}\"".format(parent.tag, locale.tag)
                locale.resolve_locale(parent, verbose)

    def resolve_locales(self, verbose):
        n_resolutions = 0
        for i in range(1, 5):
            for locale in self.locales:
                if locale.level == i:
                    parent = self.find_parent (locale.tag, locale.level)
                    if parent is not None:
                        n_resolutions += locale.resolve_locale(parent, verbose)
        print "Resolved total of {0} items".format(n_resolutions)

    def sort_strings(self):
        mapping = self.strpool.sort()
        for locale in self.locales:
            locale.apply_renumbering(mapping)

    def embed(self, embed_name):
        tmp_name = "%s.zbtmp" % embed_name
        wfp = codecs.open(tmp_name, "w", "utf-8")
        rfp = codecs.open(embed_name, "r", "utf-8")
        in_data_block = False
        for line in rfp.readlines():
            if _END_CLDR_DATA in line:
                in_data_block = False
            if not in_data_block:
                wfp.write(line)
            if _BEGIN_CLDR_DATA in line:
                zbm_new_line(wfp)
                self.strpool.write(wfp)
                zbm_write(wfp, ZBMSG0041)
                zbm_write(wfp, ZBMSG0042)
                zbm_new_line(wfp)
                last_index = len(self.locales) - 1
                self.locales.sort(key=lambda l: l.tag)
                for index, locale in enumerate(self.locales):
                    locale.write(wfp, index == last_index)
                zbm_new_line(wfp)
                in_data_block = True;
        wfp.close()
        rfp.close()
        os.remove(embed_name)
        os.rename(tmp_name, embed_name)

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
        for index, locale in enumerate(self.locales):
            locale.write_properties(index, props_dir, base)
