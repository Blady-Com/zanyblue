#!/usr/bin/python -tt
# -*- coding: utf-8 -*-

"""
Representation of a locale: references to the various string values that
are needed for a locale, e.g., the text for a the full name of the day
"Sunday", etc.

The strings are referenced via a string pool object, i.e., index values for
strings managed by the string pool object passed as a constructor argument.

Simple class to accumulate a set of strings mapping them to an index value.

The locale values are stored in nested dictionaries, e.g., the full name of
the day "Sunday" is stored as, using '/' to denote pseudo nesting of dicts

    naming/day/wide/sun => Sunday

The final keys correspond to the keys used by the underlying CLDR data from
Unicode.org:

    Days  Months  Day Periods  Eras          Numeric
     sun       1           am     0          decimal
     mon       2         noon     1            group
     tue       3           pm                   list
     wed       4                     nativezerodigit
     thu       5                            plussign
     fri       6                           minussign
     sat       7                         exponential
               8                         percentsign
               9                            permille
              10                            infinity
              11                                 nan
              12                        patterndigit

Intermediate dictionaries for days and months support "wide" and "abbreviated".

Formatting information is stored in the "formats" dictionary with keys

      Date       Time  Date_Time     Numeric
      full       full       full     decimal
      long       long       long  scientific
    medium     medium     medium     percent
     short      short      short    currency
"""

import os

from .zbmsg import *

class InvalidPathError(Exception):
    """
    """
    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Invalid path {0}".format(self.path)


class Locale(object):
    def __init__(self, name, strpool, mklocales):
        self.strpool = strpool
        self.mklocales = mklocales
        self.name = self.strpool.index(name)
        self.set_tag_and_level(name)
        self.n_set = 0
        self.props_data = { "alias": None,
                            "languages": {},
                            "scripts": {},
                            "territories": {} }
        self.locale_data = { 'naming': {
                        'alias': None,
                        'day': {
                          'wide': {
                            'sun': None, 'mon': None, 'tue': None, 'wed': None,
                            'thu': None, 'fri': None, 'sat': None },
                          'abbreviated': {
                            'sun': None, 'mon': None, 'tue': None, 'wed': None,
                            'thu': None, 'fri': None, 'sat': None } },
                        'month': {
                          'wide': {
                             '1': None, '2': None, '3': None, '4': None,
                             '5': None, '6': None, '7': None, '8': None,
                             '9': None, '10': None, '11': None, '12': None },
                           'abbreviated': {
                             '1': None, '2': None, '3': None, '4': None,
                             '5': None, '6': None, '7': None, '8': None,
                             '9': None, '10': None, '11': None, '12': None } },
                        'day_period': {
                           'am': None, 'noon': None, 'pm': None },
                        'era': {
                           '0': None, '1': None },
                        'numeric': {
                           'decimal': None, 'group': None,
                           'list': None, 'infinity': None,
                           'plussign': None, 'minussign': None,
                           'exponential': None, 'percentsign': None,
                           'permille': None, 'nativezerodigit': None,
                           'nan': None, 'patterndigit': None } },
                      'formats': {
                        'date': {
                          'full': None, 'long': None,
                          'medium': None, 'short': None },
                        'time': {
                          'full': None, 'long': None,
                          'medium': None, 'short': None },
                        'date_time': {
                          'full': None, 'long': None,
                          'medium': None, 'short': None },
                        'numeric': {
                          'decimal': None, 'scientific': None,
                          'percent': None, 'currency': None } } }

    def apply_renumbering(self, mapping):
        self.name = mapping[self.name - 1]
        self.renumber_data(self.locale_data, mapping)

    def renumber_data(self, dataset, mapping):
        for key in dataset.keys():
            value = dataset[key]
            if type(value).__name__ == 'dict':
                self.renumber_data(value, mapping)
            elif value is not None:
                dataset[key] = mapping[value - 1]
   
    def set_tag_and_level(self, name):
        components = name.split("_")
        self.level = 0
        if len(components) == 1:
            language = components[0][:3]
            script = ""
            territory = ""
            if len(language) > 0:
                self.level += 1
        elif len(components) == 2:
            language = components[0][:3]
            if len(components[1]) > 3:
                script = components[1][:4]
                territory = ""
                self.level = 3
            else:
                script = ""
                territory = components[1][:3]
                self.level = 2
        else:
            language = components[0][:3]
            script = components[1][:4]
            territory = components[2][:3]
            self.level = 4
        self.tag = "{0:<3}{1:<4}{2:<3}".format(language.upper(),
                                               script.upper(),
                                               territory.upper())

    def set_root_values(self):
        self.set_value ("naming/day/wide/sun", "Sunday")
        self.set_value ("naming/day/wide/mon", "Monday")
        self.set_value ("naming/day/wide/tue", "Tuesday")
        self.set_value ("naming/day/wide/wed", "Wednesday")
        self.set_value ("naming/day/wide/thu", "Thursday")
        self.set_value ("naming/day/wide/fri", "Friday")
        self.set_value ("naming/day/wide/sat", "Saturday")

        self.set_value ("naming/day/abbreviated/sun", "Sun")
        self.set_value ("naming/day/abbreviated/mon", "Mon")
        self.set_value ("naming/day/abbreviated/tue", "Tue")
        self.set_value ("naming/day/abbreviated/wed", "Wed")
        self.set_value ("naming/day/abbreviated/thu", "Thu")
        self.set_value ("naming/day/abbreviated/fri", "Fri")
        self.set_value ("naming/day/abbreviated/sat", "Sat")

        self.set_value ("naming/month/wide/1", "January")
        self.set_value ("naming/month/wide/2", "February")
        self.set_value ("naming/month/wide/3", "March")
        self.set_value ("naming/month/wide/4", "April")
        self.set_value ("naming/month/wide/5", "May")
        self.set_value ("naming/month/wide/6", "June")
        self.set_value ("naming/month/wide/7", "July")
        self.set_value ("naming/month/wide/8", "August")
        self.set_value ("naming/month/wide/9", "September")
        self.set_value ("naming/month/wide/10", "October")
        self.set_value ("naming/month/wide/11", "November")
        self.set_value ("naming/month/wide/12", "December")

        self.set_value("naming/month/abbreviated/1", "Jan")
        self.set_value("naming/month/abbreviated/2", "Feb")
        self.set_value("naming/month/abbreviated/3", "Mar")
        self.set_value("naming/month/abbreviated/4", "Apr")
        self.set_value("naming/month/abbreviated/5", "May")
        self.set_value("naming/month/abbreviated/6", "Jun")
        self.set_value("naming/month/abbreviated/7", "Jul")
        self.set_value("naming/month/abbreviated/8", "Aug")
        self.set_value("naming/month/abbreviated/9", "Sep")
        self.set_value("naming/month/abbreviated/10", "Oct")
        self.set_value("naming/month/abbreviated/11", "Nov")
        self.set_value("naming/month/abbreviated/12", "Dec")

        self.set_value("naming/day_period/am", "AM")
        self.set_value("naming/day_period/noon", "noon")
        self.set_value("naming/day_period/pm", "PM")

        self.set_value("naming/era/0", "BCE")
        self.set_value("naming/era/1", "CE")

        self.set_value("naming/numeric/decimal", ".")
        self.set_value("naming/numeric/group", ",")
        self.set_value("naming/numeric/list", ";")
        self.set_value("naming/numeric/nativezerodigit", "0")
        self.set_value("naming/numeric/plussign", "+")
        self.set_value("naming/numeric/minussign", "-")
        self.set_value("naming/numeric/exponential", "E")
        self.set_value("naming/numeric/percentsign", "%")
        self.set_value("naming/numeric/permille", u"‰")
        self.set_value("naming/numeric/infinity", u"∞")
        self.set_value("naming/numeric/nan", "NaN")
        self.set_value("naming/numeric/patterndigit", "#")

        self.set_value("formats/date/full", "EEEE, MMMM d, y")
        self.set_value("formats/date/long", "MMMM d, y")
        self.set_value("formats/date/medium", "MMM d, y")
        self.set_value("formats/date/short", "M/d/yy")

        self.set_value("formats/time/full", "h:mm:ss a zzzz")
        self.set_value("formats/time/long", "h:mm:ss a z")
        self.set_value("formats/time/medium", "h:mm:ss a")
        self.set_value("formats/time/short", "h:mm a")

        self.set_value("formats/date_time/full", "{1} {0}")
        self.set_value("formats/date_time/long", "{1} {0}")
        self.set_value("formats/date_time/medium", "{1} {0}")
        self.set_value("formats/date_time/short", "{1} {0}")

        self.set_value("formats/numeric/decimal", u"#,##0.###")
        self.set_value("formats/numeric/scientific", u"#E0")
        self.set_value("formats/numeric/percent", u"#,##0%")
        self.set_value("formats/numeric/currency", u"¤#,##0.00;(¤#,##0.00)")

    def _get_container_key(self, path):
        components = path.split("/")
        key = components[-1]
        if self.mklocales:
            container = self.locale_data
        else:
            container = self.props_data
        for component in components[:-1]:
            if container.has_key(component):
                container = container[component]
            else:
                raise InvalidPathError(path)
        if self.mklocales and not container.has_key(key):
            raise InvalidPathError(path)
        return container, key

    def set_value(self, path, value):
        if self.mklocales:
            path = path.lower()
        container, key = self._get_container_key(path)
        container[key] = self.strpool.index(value)
        self.n_set += 1
        return container[key]

    def get_value(self, path):
        if self.mklocales:
            path = path.lower()
        container, key = self._get_container_key(path)
        return container[key]

    def resolve_params(self, data, parent, path):
        result = 0
        for key in data.keys():
            if type(data[key]) is dict:
                result += self.resolve_params(data[key], parent, path + [key])
            elif data[key] is None:
                parameter = "/".join(path + [key])
                data[key] = parent.get_value(parameter)
                result += 1
        return result

    def resolve_locale(self, parent, verbose):
        n_resolutions = self.resolve_params(self.locale_data, parent, [])
        if verbose and n_resolutions > 0:
           print "Resolved locale \"{1}\" via locale \"{2}\", {0} items".format(
                     n_resolutions,
                     self.tag,
                     parent.tag)
        return n_resolutions

    def add_value(self, args, index, value):
        args.extend([ None for i in range(index + 1 - len(args))])
        args[index] = value

    def qualifiers_match(self, element, args, qualifiers):
        result = True
        if element.getAttribute("alt") != '':
            return False
        for qualifier in qualifiers:
            for attribute in qualifier.keys():
                match_value = qualifier[attribute]
                if attribute == "name":
                    value = element.nodeName
                else:
                    value = element.getAttribute(attribute)
                if match_value[0] == '#':
                    self.add_value(args, int(match_value[1:]), value)
                elif match_value[0] == "*":
                    result = value
                elif match_value != value:
                    return False
            element = element.parentNode
        return result

    def element_value(self, element):
        result = ""
        for node in element.childNodes:
            if node.nodeType == node.TEXT_NODE:
                result += node.data
        return result

    def parse_element(self, dom, dest_name, qualifiers):
        for element in dom.getElementsByTagName(qualifiers[0]["name"]):
            args = []
            match = self.qualifiers_match(element, args, qualifiers)
            if match:
                parameter = dest_name.format(*args)
                if type(match).__name__ == 'bool':
                    value = self.element_value(element)
                else:
                    value = match
                try:
                    self.set_value(parameter, value)
                except InvalidPathError:
                    pass

    def parse_locales(self, dom):
        n_set = self.n_set
        self.parse_element(dom,
                           "naming/day/{1}/{0}",
                           [ { "name": "day", "type": "#0" },
                             { "name": "dayWidth", "type": "#1" },
                             { "name": "dayContext", "type": "format" },
                             { "name": "days" },
                             { "name": "calendar", "type": "gregorian" } ])
        self.parse_element(dom,
                           "naming/month/{1}/{0}",
                           [ { "name": "month", "type": "#0" },
                             { "name": "monthWidth", "type": "#1" },
                             { "name": "monthContext", "type": "format" },
                             { "name": "months" },
                             { "name": "calendar", "type": "gregorian" } ])
        self.parse_element(dom,
                           "naming/day_period/{0}",
                           [ { "name": "dayPeriod", "type": "#0" },
                             { "name": "dayPeriodWidth", "type": "wide" },
                             { "name": "dayPeriodContext", "type": "format" },
                             { "name": "dayPeriods" },
                             { "name": "calendar", "type": "gregorian" } ])
        self.parse_element(dom,
                           "naming/era/{0}",
                           [ { "name": "era", "type": "#0" },
                             { "name": "eraAbbr" },
                             { "name": "eras" },
                             { "name": "calendar", "type": "gregorian" } ])
        for symbol in [ "decimal", "group", "list", "percentSign",
                        "nativeZeroDigit", "patternDigit", "plusSign",
			"minusSign", "exponential", "perMille", "infinity",
			"nan" ]:
            self.parse_element(dom,
                               "naming/numeric/{0}".format(symbol),
                               [ { "name": symbol, },
                                 { "name": "symbols" },
                                 { "name": "numbers" } ])
        for formatType in [ "decimal", "scientific", "percent", "currency" ]:
            self.parse_element(dom,
                               "formats/numeric/{0}".format(formatType),
                               [ { "name": "pattern", },
                                 { "name": "{0}Format".format(formatType), },
                                 { "name": "{0}FormatLength".format(formatType), },
                                 { "name": "{0}Formats".format(formatType), } ])
        self.parse_element(dom,
                           "formats/date/{0}",
                           [ { "name": "pattern" },
                             { "name": "dateFormat" },
                             { "name": "dateFormatLength", "type": "#0" },
                             { "name": "dateFormats" },
                             { "name": "calendar", "type": "gregorian" } ])
        self.parse_element(dom,
                           "formats/time/{0}",
                           [ { "name": "pattern" },
                             { "name": "timeFormat" },
                             { "name": "timeFormatLength", "type": "#0" },
                             { "name": "timeFormats" },
                             { "name": "calendar", "type": "gregorian" } ])
        self.parse_element(dom,
                           "formats/date_time/{0}",
                           [ { "name": "pattern" },
                             { "name": "dateTimeFormat" },
                             { "name": "dateTimeFormatLength", "type": "#0" },
                             { "name": "dateTimeFormats" },
                             { "name": "calendar", "type": "gregorian" } ])
        self.parse_element(dom,
                           "naming/alias",
                           [ { "name": "alias", "source": "*" },
                             { "name": "ldml" } ])
        return self.n_set - n_set

    def parse_properties(self, dom):
        n_set = self.n_set
        self.parse_element(dom,
                           "languages/{0}",
                           [ { "name": "language", "type": "#0" },
			     { "name": "languages" } ])
        self.parse_element(dom,
                           "scripts/{0}",
                           [ { "name": "script", "type": "#0" },
			     { "name": "scripts" } ])
        self.parse_element(dom,
                           "territories/{0}",
                           [ { "name": "territory", "type": "#0" },
			     { "name": "territories" } ])
        self.parse_element(dom,
                           "alias",
                           [ { "name": "alias", "source": "*" },
                             { "name": "ldml" } ])
        return self.n_set - n_set

    def parse(self, dom):
        if self.mklocales:
            return self.parse_locales(dom)
        else:
            return self.parse_properties(dom)

    def write_properties(self, index, props_dir, base):
        name = self.strpool.value(self.name)
        for category in ["languages", "scripts", "territories"]:
            facility = category[0]
            filename = os.path.join(props_dir, "{0}_{1}.properties".format(facility, name))
            self.write_property_file(filename, name, self.props_data[category])
            if name == base:
                filename = os.path.join(props_dir, "{0}.properties".format(facility))
                self.write_property_file(filename, name, self.props_data[category])

    def write_property_file(self, filename, name, values):
        print "Creating properties file \"{0}\" for locale \"{1}\"".format(
                  filename,
                  name
        )
        fp = open(filename, "w")
        fp.write("# This is generated ZanyBlue CLDR data based on Unicode.org's CLDR data\n")
        fp.write("# DO NOT EDIT\n\n")
        for code in sorted(values.keys()):
            value = self.java_string(self.strpool.value(values[code]))
            fp.write("{0}={1}\n".format(code, value))
        fp.close()

    def java_string(self, value):
        result = ""
        for c in value:
            ic = ord(c)
            if ic >= 32 and ic < 127:
                result += c
            else:
                result += "\\u{0:04x}".format(ic)
        return result

    def write(self, fp, lastp):
        zbm_write(fp, ZBMSG0008, self.tag)
        zbm_write(fp, ZBMSG0009, self.level)
        zbm_write(fp, ZBMSG0053, self.name)
        self.write_days(fp, "Short_Day_Names", "abbreviated")
        self.write_days(fp, "Full_Day_Names", "wide")
        self.write_months(fp, "Short_Month_Names", "abbreviated")
        self.write_months(fp, "Full_Month_Names", "wide")
        self.write_day_periods(fp)
        self.write_eras(fp)
        self.write_date_time_formats(fp, "Date")
        self.write_date_time_formats(fp, "Time")
        self.write_date_time_formats(fp, "Date_Time")
        self.write_numeric_names(fp)
        self.write_numeric_formats(fp, lastp)

    def write_days(self, fp, section, width):
        zbm_write(fp, ZBMSG0010, section)
        zbm_write(fp, ZBMSG0015,
                      self.get_value("naming/day/%s/sun" % width),
                      self.get_value("naming/day/%s/mon" % width),
                      self.get_value("naming/day/%s/tue" % width))
        zbm_write(fp, ZBMSG0016,
                      self.get_value("naming/day/%s/wed" % width),
                      self.get_value("naming/day/%s/thu" % width),
                      self.get_value("naming/day/%s/fri" % width))
        zbm_write(fp, ZBMSG0017,
                      self.get_value("naming/day/%s/sat" % width))

    def write_months(self, fp, section, width):
        zbm_write(fp, ZBMSG0010, section)
        zbm_write(fp, ZBMSG0011,
                      self.get_value("naming/month/%s/1" % width),
                      self.get_value("naming/month/%s/2" % width),
                      self.get_value("naming/month/%s/3" % width))
        zbm_write(fp, ZBMSG0012,
                      self.get_value("naming/month/%s/4" % width),
                      self.get_value("naming/month/%s/5" % width),
                      self.get_value("naming/month/%s/6" % width))
        zbm_write(fp, ZBMSG0013,
                      self.get_value("naming/month/%s/7" % width),
                      self.get_value("naming/month/%s/8" % width),
                      self.get_value("naming/month/%s/9" % width))
        zbm_write(fp, ZBMSG0014,
                      self.get_value("naming/month/%s/10" % width),
                      self.get_value("naming/month/%s/11" % width),
                      self.get_value("naming/month/%s/12" % width))

    def write_day_periods(self, fp):
        zbm_write(fp, ZBMSG0010, "Day_Period_Names")
        zbm_write(fp, ZBMSG0018,
                      self.get_value("naming/day_period/am"),
                      self.get_value("naming/day_period/noon"),
                      self.get_value("naming/day_period/pm"))

    def write_eras(self, fp):
        zbm_write(fp, ZBMSG0010, "Era_Names")
        zbm_write(fp, ZBMSG0019,
                      self.get_value("naming/era/0"),
                      self.get_value("naming/era/1"))

    def write_date_time_formats(self, fp, name):
        zbm_write(fp, ZBMSG0010, "%s_Formats" % name)
        zbm_write(fp, ZBMSG0030,
                      self.get_value("formats/%s/full" % name.lower()),
                      self.get_value("formats/%s/long" % name.lower()),
                      self.get_value("formats/%s/medium" % name.lower()),
                      self.get_value("formats/%s/short" % name.lower()))

    def write_numeric_names(self, fp):
        zbm_write(fp, ZBMSG0010, "Numeric_Items")
        zbm_write(fp, ZBMSG0031,
                      self.get_value("naming/numeric/decimal"),
                      self.get_value("naming/numeric/group"))
        zbm_write(fp, ZBMSG0032,
                      self.get_value("naming/numeric/list"),
                      self.get_value("naming/numeric/nativezerodigit"))
        zbm_write(fp, ZBMSG0033,
                      self.get_value("naming/numeric/plussign"),
                      self.get_value("naming/numeric/minussign"))
        zbm_write(fp, ZBMSG0034,
                      self.get_value("naming/numeric/exponential"),
                      self.get_value("naming/numeric/percentsign"))
        zbm_write(fp, ZBMSG0035,
                      self.get_value("naming/numeric/permille"),
                      self.get_value("naming/numeric/infinity"))
        zbm_write(fp, ZBMSG0036,
                      self.get_value("naming/numeric/nan"),
                      self.get_value("naming/numeric/patterndigit"))

    def write_numeric_formats(self, fp, lastp):
        zbm_write(fp, ZBMSG0010, "Numeric_Formats")
        zbm_write(fp, ZBMSG0037,
                      self.get_value("formats/numeric/decimal"),
                      self.get_value("formats/numeric/scientific"))
        if not lastp:
            zbm_write(fp, ZBMSG0038,
                          self.get_value("formats/numeric/percent"),
                          self.get_value("formats/numeric/currency"))
            zbm_new_line(fp)
        else:
            zbm_write(fp, ZBMSG0039,
                          self.get_value("formats/numeric/percent"),
                          self.get_value("formats/numeric/currency"))
