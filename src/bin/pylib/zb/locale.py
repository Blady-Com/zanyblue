#!/usr/bin/env python
# -*- encoding: utf-8 -*-

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
     wed       4                     nativeZeroDigit
     thu       5                            plusSign
     fri       6                           minusSign
     sat       7                         exponential
               8                         percentSign
               9                            perMille
              10                            infinity
              11                                 nan
              12                        patternDigit

Intermediate dictionaries for days and months support "wide" and "abbreviated".

Formatting information is stored in the "formats" dictionary with keys

      Date       Time  Date_Time     Numeric
      full       full       full     decimal
      long       long       long  scientific
    medium     medium     medium     percent
     short      short      short    currency
"""

import os
import sys

from copy import deepcopy

from .zbmsg import *
from .zbxmlsupp import parse_element

#
# Data structure used to store the locale data parsed from the CLDR XML
# files.  Each new locale copies this empty data structure on construction.
_EMPTY_LOCALE_DATA = {
    'naming': {
        'alias': None,
        'day': {
            'wide': {
                'sun': None,
                'mon': None,
                'tue': None,
                'wed': None,
                'thu': None,
                'fri': None,
                'sat': None
            },
            'abbreviated': {
                'sun': None,
                'mon': None,
                'tue': None,
                'wed': None,
                'thu': None,
                'fri': None,
                'sat': None
            }
         },
        'month': {
            'wide': {
                '1':  None,
                '2':  None,
                '3':  None,
                '4':  None,
                '5':  None,
                '6':  None,
                '7':  None,
                '8':  None,
                '9':  None,
                '10': None,
                '11': None,
                '12': None
            },
            'narrow': {        # Not used
                '1':  None,
                '2':  None,
                '3':  None,
                '4':  None,
                '5':  None,
                '6':  None,
                '7':  None,
                '8':  None,
                '9':  None,
                '10': None,
                '11': None,
                '12': None
            },
            'abbreviated': {
                '1':  None,
                '2':  None,
                '3':  None,
                '4':  None,
                '5':  None,
                '6':  None,
                '7':  None,
                '8':  None,
                '9':  None,
                '10': None,
                '11': None,
                '12': None
            }
        },
        'day_period': {
            'values': {
                'am':           None,
                'weeHours':     None,
                'earlyMorning': None,
                'morning':      None,
                'lateMorning':  None,
                'noon':         None,
                'midDay':       None,
                'afternoon':    None,
                'evening':      None,
                'lateEvening':  None,
                'night':        None,
                'pm':           None
            },
            'rules': {
                'exact': {
                    '0':  None,
                    '1':  None,
                    '2':  None,
                    '3':  None,
                    '4':  None,
                    '5':  None,
                    '6':  None,
                    '7':  None,
                    '8':  None,
                    '9':  None,
                    '10': None,
                    '11': None,
                    '12': None,
                    '13': None,
                    '14': None,
                    '15': None,
                    '16': None,
                    '17': None,
                    '18': None,
                    '19': None,
                    '20': None,
                    '21': None,
                    '22': None,
                    '23': None
                },
                'within': {
                    '0':  None,
                    '1':  None,
                    '2':  None,
                    '3':  None,
                    '4':  None,
                    '5':  None,
                    '6':  None,
                    '7':  None,
                    '8':  None,
                    '9':  None,
                    '10': None,
                    '11': None,
                    '12': None,
                    '13': None,
                    '14': None,
                    '15': None,
                    '16': None,
                    '17': None,
                    '18': None,
                    '19': None,
                    '20': None,
                    '21': None,
                    '22': None,
                    '23': None
                }
            }
        },
        'era': {
            '0': None,
            '1': None
        },
        'numeric': {
            'decimal':         None,
            'group':           None,
            'list':            None,
            'infinity':        None,
            'plusSign':        None,
            'minusSign':       None,
            'exponential':     None,
            'percentSign':     None,
            'perMille':        None,
            'nativeZeroDigit': None,
            'nan':             None,
            'patternDigit':    None,
            'numericDigits':   None
        }
    },
    'formats': {
        'date': {
            'full':   None,
            'long':   None,
            'medium': None,
            'short':  None
        },
        'time': {
            'full':   None,
            'long':   None,
            'medium': None,
            'short':  None
        },
        'date_time': {
            'full':   None,
            'long':   None,
            'medium': None,
            'short':  None
        },
        'numeric': {
            'decimal':    None,
            'scientific': None,
            'percent':    None,
            'currency':   None
        }
    },
    '*languages': {},
    '*scripts': {},
    '*territories': {},
    '*alias': None,
    '*numberingSystem': None,
    '*layout': 'left-to-right',
    '*dayperiod': {
        'exact': {},
        'within': {}
    }
}

#
# Version of the locale data structure initialized for the base (root) locale.

_ROOT_LOCALE_DATA = {
    'naming': {
        'alias': None,
        'day': {
            'wide': {
                'sun': u"Sunday",
                'mon': u"Monday",
                'tue': u"Tuesday",
                'wed': u"Wednesday",
                'thu': u"Thursday",
                'fri': u"Friday",
                'sat': u"Saturday"
            },
            'abbreviated': {
                'sun': u"Sun",
                'mon': u"Mon",
                'tue': u"Tue",
                'wed': u"Wed",
                'thu': u"Thu",
                'fri': u"Fri",
                'sat': u"Sat"
            }
        },
        'month': {
            'wide': {
                '1':  u"January",
                '2':  u"February",
                '3':  u"March",
                '4':  u"April",
                '5':  u"May",
                '6':  u"June",
                '7':  u"July",
                '8':  u"August",
                '9':  u"September",
                '10': u"October",
                '11': u"November",
                '12': u"December"
            },
            'narrow': {        # Not used
                '1':  u"",
                '2':  u"",
                '3':  u"",
                '4':  u"",
                '5':  u"",
                '6':  u"",
                '7':  u"",
                '8':  u"",
                '9':  u"",
                '10': u"",
                '11': u"",
                '12': u""
            },
            'abbreviated': {
                '1':  u"Jan",
                '2':  u"Feb",
                '3':  u"Mar",
                '4':  u"Apr",
                '5':  u"May",
                '6':  u"Jun",
                '7':  u"Jul",
                '8':  u"Aug",
                '9':  u"Sep",
                '10': u"Oct",
                '11': u"Nov",
                '12': u"Dec"
            }
        },
        'day_period': {
            'values': {
                'am':           u"AM",
                'weeHours':     u"",
                'earlyMorning': u"",
                'morning':      u"",
                'lateMorning':  u"",
                'noon':         u"noon",
                'midDay':       u"",
                'afternoon':    u"",
                'evening':      u"",
                'lateEvening':  u"",
                'night':        u"",
                'pm':           u"PM"
            },
            'rules': {
                'exact': {
                    '0':  u'am',
                    '1':  u'am',
                    '2':  u'am',
                    '3':  u'am',
                    '4':  u'am',
                    '5':  u'am',
                    '6':  u'am',
                    '7':  u'am',
                    '8':  u'am',
                    '9':  u'am',
                    '10': u'am',
                    '11': u'am',
                    '12': u'pm',
                    '13': u'pm',
                    '14': u'pm',
                    '15': u'pm',
                    '16': u'pm',
                    '17': u'pm',
                    '18': u'pm',
                    '19': u'pm',
                    '20': u'pm',
                    '21': u'pm',
                    '22': u'pm',
                    '23': u'pm'
                },
                'within': {
                    '0':  u'am',
                    '1':  u'am',
                    '2':  u'am',
                    '3':  u'am',
                    '4':  u'am',
                    '5':  u'am',
                    '6':  u'am',
                    '7':  u'am',
                    '8':  u'am',
                    '9':  u'am',
                    '10': u'am',
                    '11': u'am',
                    '12': u'pm',
                    '13': u'pm',
                    '14': u'pm',
                    '15': u'pm',
                    '16': u'pm',
                    '17': u'pm',
                    '18': u'pm',
                    '19': u'pm',
                    '20': u'pm',
                    '21': u'pm',
                    '22': u'pm',
                    '23': u'pm'
                }
            }
        },
        'era': {
            '0': u"BCE",
            '1': u"CE"
        },
        'numeric': {
            'decimal':         u".",
            'group':           u",",
            'list':            u";",
            'infinity':        u"Inf",
            'plusSign':        u"+",
            'minusSign':       u"-",
            'exponential':     u"E",
            'percentSign':     u"%",
            'perMille':        u"‰",
            'nativeZeroDigit': u"0",
            'nan':             u"NaN",
            'patternDigit':    u"#",
            'numericDigits':   u"0123456789"
        }
    },
    'formats': {
        'date': {
            'full':   u"EEEE, MMMM d, y",
            'long':   u"MMMM d, y",
            'medium': u"MMM d, y",
            'short':  u"M/d/yy"
        },
        'time': {
            'full':   u"h:mm:ss a zzzz",
            'long':   u"h:mm:ss a z",
            'medium': u"h:mm:ss a",
            'short':  u"h:mm a"
        },
        'date_time': {
            'full':   u"{1} {0}",
            'long':   u"{1} {0}",
            'medium': u"{1} {0}",
            'short':  u"{1} {0}"
        },
        'numeric': {
            'decimal':    u"#,##0.###",
            'scientific': u"#E0",
            'percent':    u"#,##0%",
            'currency':   u"¤#,##0.00;(¤#,##0.00)"
        }
    },
    '*languages': {},
    '*scripts': {},
    '*territories': {},
    '*alias': None,
    '*numberingSystem': 'latn',
    '*layout': 'left-to-right',
    '*dayperiod': {
        'exact': {},
        'within': {}
    }
}


def info(fmt, *args):
    """
    Print an informational message.
    """
    print fmt.format(*args)


class InvalidPathError(Exception):
    """
    Exception raised with accessing a parameter not already defined.  The
    Locale initialization routine predefines the expected parameters.  This
    exception is not raised for parameters with paths starting with '*'.
    """
    def __init__(self, path):
        """
        Constructor, just take note of the invalid path.
        """
        self.path = path

    def __str__(self):
        """
        Exception message.
        """
        return "Invalid path \"{0}\"".format(self.path)


class Locale(object):
    """
    Representation of the data defined by the CLDR data for an individual
    locale, day names, numeric formats, etc.
    """

    def __init__(self, name):
        """
        Constructor, take note of the name and assign the 10 character tag
        value.  The set of parameters is also initailized to the empty values
        (CLDR parsing will fill them out later).
        """
        self.name = name
        self.set_tag_and_level(name)
        self.n_set = 0
        self.locale_data = deepcopy(_EMPTY_LOCALE_DATA)

    def accumulate_strings(self, strpool):
        """
        Accumulate strings from the locale into the string pool object.
        Indexes are not assigned in this pass as the string storage is
        optimized then assigned via the apply_string_indexes routine.
        """

    def apply_locale_fn(self, function):
        """
        Apply string transformation function to the main (non-language,
        ternitory, script names data), i.e., to index via a string pool object.
        The core locale data is stored as a long Wide_String with locale
        objects referring to the strings by an index into a table giving the
        start and end of of the string in the long Wide_String pool value.
        """
        self.name = function(self.name)
        self._apply_locale_fn(function, self.locale_data)

    def _apply_locale_fn(self, function, data_set):
        """
        Helper routine to recursively traverse the locale_data structure.
        """
        for key in data_set.keys():
            value = data_set[key]
            if type(value).__name__ == 'dict':
                if key[0] != '*':
                    self._apply_locale_fn(function, value)
            elif value is not None:
                data_set[key] = function(value)

    def set_tag_and_level(self, name):
        """
        Set the tag value associated with a locale name.  This is the 10
        character string giving the language, script and territory, e.g.,
        "en_Latn_US" => "EN LATNUS ".
        """
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
        self.tag = "{0:<3}{1:<4}{2:<3}".format(
            language,
            script,
            territory
        ).upper()

    def set_root_values(self, dayperiod_dom):
        """
        Initialize the root (base) locale value.  In a sense this corresponds
        to the pseudo locale "Ada".  It should, however, really be parsed from
        the root.xml file, however.  Maybe later.
        """
        self.locale_data = deepcopy(_ROOT_LOCALE_DATA)
        # The root locale stores the day period info, load it.
        parse_element(self, dayperiod_dom, "*dayperiod/{0}/exact/{1}", [
            {"name": "dayPeriodRule", "type": "*", "at": "#1"},
            {"name": "dayPeriodRules", "locales": "#0"},
            {"name": "dayPeriodRuleSet"},
            {"name": "supplementalData"}
        ])
        parse_element(self, dayperiod_dom, "*dayperiod/{0}/within/{1}", [
            {"name": "dayPeriodRule", "type": "*", "before": "#1"},
            {"name": "dayPeriodRules", "locales": "#0"},
            {"name": "dayPeriodRuleSet"},
            {"name": "supplementalData"}
        ])
        locale_sets = self.locale_data['*dayperiod']
        for locales in locale_sets.keys():
            if not ' ' in locales:
                continue
            for locale in locales.split(' '):
                info("Copying day period information for \"{0}\" from \"{1}\"",
                    locale, locales)
                locale_sets[locale] = deepcopy(locale_sets[locales])
            del locale_sets[locales]
        for locale in locale_sets.keys():
            for hour_rule in locale_sets[locale].keys():
                for rule in locale_sets[locale][hour_rule].keys():
                    if not rule.endswith(":00"):
                        info("Non-hour rule \"{0}\" for locale \"{1}\"",
                            rule, locale)
                        sys.exit(1)
                    plain_rule = int(rule.replace(":00", ""))
                    cur_rules = locale_sets[locale][hour_rule][rule]
                    locale_sets[locale][hour_rule][plain_rule] = cur_rules
                    del locale_sets[locale][hour_rule][rule]

    def apply_day_period_rules(self, rules):
        info("Applying day period definitions for \"{0}\"", self.name)
        # Apply the day period ranges
        hour = 0
        for switch_hour in sorted(rules['within'].keys()):
            while hour < switch_hour:
                param = "naming/day_period/rules/within/{0}".format(hour)
                value = rules['within'][switch_hour]
                self.set_value(param, value)
                hour += 1
        # Apply the day period exact values
        if 'exact' in rules:
            for hour in sorted(rules['exact'].keys()):
                param = "naming/day_period/rules/exact/{0}".format(hour)
                value = rules['exact'][hour]
                self.set_value(param, value)
        # Apply range values to exact if not already defined
        for hour in range(24):
            wparam = "naming/day_period/rules/within/{0}".format(hour)
            eparam = "naming/day_period/rules/exact/{0}".format(hour)
            if self.get_value(eparam) is None:
                self.set_value(eparam, self.get_value(wparam))

    def _get_container_key(self, path):
        components = path.split("/")
        key = components[-1]
        createp = path[0] == '*'
        container = self.locale_data
        for component in components[:-1]:
            if component not in container:
                if createp:
                    container[component] = {}
                else:
                    raise InvalidPathError(path)
            container = container[component]
        if key not in container and not createp:
            raise InvalidPathError(path)
        return container, key

    def __setitem__(self, name, value):
        return self.set_value(name, value)

    def set_value(self, path, value):
        container, key = self._get_container_key(path)
        container[key] = value
        self.n_set += 1
        return container[key]

    def get_value(self, path):
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
            info("Resolved locale \"{1}\" via locale \"{2}\", {0} items",
                n_resolutions,
                self.tag,
                parent.tag
            )
        return n_resolutions

    def parse_locales(self, dom):
        n_set = self.n_set
        parse_element(self, dom, "naming/day/{1}/{0}", [
            {"name": "day", "type": "#0"},
            {"name": "dayWidth", "type": "#1"},
            {"name": "dayContext", "type": "format"},
            {"name": "days"},
            {"name": "calendar", "type": "gregorian"}
        ])
        parse_element(self, dom, "naming/month/{1}/{0}", [
            {"name": "month", "type": "#0"},
            {"name": "monthWidth", "type": "#1"},
            {"name": "monthContext", "type": "format"},
            {"name": "months"},
            {"name": "calendar", "type": "gregorian"}
        ])
        parse_element(self, dom, "naming/day_period/values/{0}", [
            {"name": "dayPeriod", "type": "#0"},
            {"name": "dayPeriodWidth", "type": "wide"},
            {"name": "dayPeriodContext", "type": "format"},
            {"name": "dayPeriods"},
            {"name": "calendar", "type": "gregorian"}
        ])
        parse_element(self, dom, "naming/era/{0}", [
            {"name": "era", "type": "#0"},
            {"name": "eraAbbr"},
            {"name": "eras"},
            {"name": "calendar", "type": "gregorian"}
        ])
        for symbol in ["decimal", "group", "list", "percentSign",
                       "nativeZeroDigit", "patternDigit", "plusSign",
                       "minusSign", "exponential", "perMille", "infinity",
                       "nan"]:
            parse_element(self, dom, "naming/numeric/{0}".format(symbol), [
                {"name": symbol},
                {"name": "symbols"},
                {"name": "numbers"}
            ])
        for fmtType in ["decimal", "scientific", "percent", "currency"]:
            parse_element(self, dom, "formats/numeric/{0}".format(fmtType), [
                {"name": "pattern"},
                {"name": "{0}Format".format(fmtType)},
                {"name": "{0}FormatLength".format(fmtType), "type": "~"},
                {"name": "{0}Formats".format(fmtType)}
            ])
        parse_element(self, dom, "formats/date/{0}", [
            {"name": "pattern"},
            {"name": "dateFormat"},
            {"name": "dateFormatLength", "type": "#0"},
            {"name": "dateFormats"},
            {"name": "calendar", "type": "gregorian"}
        ])
        parse_element(self, dom, "formats/time/{0}", [
            {"name": "pattern"},
            {"name": "timeFormat"},
            {"name": "timeFormatLength", "type": "#0"},
            {"name": "timeFormats"},
            {"name": "calendar", "type": "gregorian"}
        ])
        parse_element(self, dom, "formats/date_time/{0}", [
            {"name": "pattern"},
            {"name": "dateTimeFormat"},
            {"name": "dateTimeFormatLength", "type": "#0"},
            {"name": "dateTimeFormats"},
            {"name": "calendar", "type": "gregorian"}
        ])
        parse_element(self, dom, "naming/alias", [
            {"name": "alias", "source": "*"},
            {"name": "ldml"}
        ])
        parse_element(self, dom, "*numberingSystem", [
            {"name": "defaultNumberingSystem"},
            {"name": "numbers"},
            {"name": "ldml"}
        ])
        return self.n_set - n_set

    def parse_properties(self, dom):
        n_set = self.n_set
        parse_element(self, dom, "*languages/{0}", [
            {"name": "language", "type": "#0"},
            {"name": "languages"}
        ])
        parse_element(self, dom, "*scripts/{0}", [
            {"name": "script", "type": "#0"},
            {"name": "scripts"}
        ])
        parse_element(self, dom, "*territories/{0}", [
            {"name": "territory", "type": "#0"},
            {"name": "territories"}
        ])
        parse_element(self, dom, "*alias", [
            {"name": "alias", "source": "*"},
            {"name": "ldml"}
        ])
        parse_element(self, dom, "*layout", [
            {"name": "characterOrder"},
            {"name": "orientation"},
            {"name": "layout"},
            {"name": "ldml"}
        ])
        return self.n_set - n_set

    def parse(self, dom):
        result = self.parse_locales(dom)
        result += self.parse_properties(dom)
        return result

    def write_properties(self, index, props_dir, base):
        name = self.name
        for category in ["*languages", "*scripts", "*territories"]:
            facility = category[1]
            filename = os.path.join(
                props_dir,
                "{0}_{1}.properties".format(facility, name)
            )
            self.write_property_file(
                filename,
                name,
                self.locale_data[category]
            )
            if name == base:
                filename = os.path.join(
                    props_dir,
                    "{0}.properties".format(facility)
                )
                self.write_property_file(
                    filename,
                    name,
                    self.locale_data[category]
                )

    def write_property_file(self, filename, name, values):
        info("Creating properties file \"{0}\" for locale \"{1}\"",
            filename,
            name
        )
        fp = open(filename, "w")
        fp.write("# This is generated ZanyBlue CLDR data based on ")
        fp.write("Unicode.org's CLDR data\n")
        fp.write("# DO NOT EDIT\n\n")
        for code in sorted(values.keys()):
            value = self.java_string(values[code])
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

    def write(self, fp, index, strpool, lastp):
        zbm_write(fp, ZBMSG0008, index, self.tag)
        zbm_write(fp, ZBMSG0009, self.level)
        zbm_write(fp, ZBMSG0053, self.name)
        if strpool.value(self.get_value("*layout")) == "left-to-right":
            zbm_write(fp, ZBMSG0042, "Left_To_Right")
        else:
            zbm_write(fp, ZBMSG0042, "Right_To_Left")
        self.write_days(fp, "Short_Day_Names", "abbreviated")
        self.write_days(fp, "Full_Day_Names", "wide")
        self.write_months(fp, "Short_Month_Names", "abbreviated")
        self.write_months(fp, "Full_Month_Names", "wide")
        self.write_day_periods(fp)
        self.write_day_periods_rules(fp, strpool)
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
                      self.get_value("naming/day_period/values/am"),
                      self.get_value("naming/day_period/values/weeHours"),
                      self.get_value("naming/day_period/values/earlyMorning"))
        zbm_write(fp, ZBMSG0055,
                      self.get_value("naming/day_period/values/morning"),
                      self.get_value("naming/day_period/values/lateMorning"),
                      self.get_value("naming/day_period/values/noon"))
        zbm_write(fp, ZBMSG0056,
                      self.get_value("naming/day_period/values/midDay"),
                      self.get_value("naming/day_period/values/afternoon"),
                      self.get_value("naming/day_period/values/evening"))
        zbm_write(fp, ZBMSG0057,
                      self.get_value("naming/day_period/values/lateEvening"),
                      self.get_value("naming/day_period/values/night"),
                      self.get_value("naming/day_period/values/pm"))

    def get_day_period(self, strpool, param):
        cdlr2zb = {
            "am": "AM",
            "weeHours": "Wee_Hours",
            "earlyMorning": "Early_Morning",
            "morning": "Morning",
            "lateMorning": "Late_Morning",
            "noon": "Noon",
            "midDay": "Midday",
            "afternoon": "Afternoon",
            "evening": "Evening",
            "lateEvening": "Late_Evening",
            "night": "Night",
            "pm": "PM"
        }
        index = self.get_value("naming/day_period/rules/" + param)
        return cdlr2zb[strpool.value(index)]

    def write_day_periods_rules(self, fp, strpool):
        self.write_day_period_rule_set(fp, strpool, "exact")
        self.write_day_period_rule_set(fp, strpool, "within")

    def write_day_period_rule_set(self, fp, strpool, name):
        zbm_write(fp, ZBMSG0010, "{0}_Day_Periods".format(name.capitalize()))
        zbm_write(fp, ZBMSG0059,
            self.get_day_period(strpool, name + "/0"),
            self.get_day_period(strpool, name + "/1"),
            self.get_day_period(strpool, name + "/2"))
        for n in [3, 6, 9, 12, 15, 18]:
            zbm_write(
                fp,
                ZBMSG0060,
                n,
                self.get_day_period(strpool, "{0}/{1}".format(name, n)),
                n + 1,
                self.get_day_period(strpool, "{0}/{1}".format(name, n + 1)),
                n + 2,
                self.get_day_period(strpool, "{0}/{1}".format(name, n + 2))
            )
        zbm_write(fp, ZBMSG0061,
            self.get_day_period(strpool, name + "/21"),
            self.get_day_period(strpool, name + "/22"),
            self.get_day_period(strpool, name + "/23"))

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
            self.get_value("naming/numeric/nativeZeroDigit"))
        zbm_write(fp, ZBMSG0033,
            self.get_value("naming/numeric/plusSign"),
            self.get_value("naming/numeric/minusSign"))
        zbm_write(fp, ZBMSG0034,
            self.get_value("naming/numeric/exponential"),
            self.get_value("naming/numeric/percentSign"))
        zbm_write(fp, ZBMSG0035,
            self.get_value("naming/numeric/perMille"),
            self.get_value("naming/numeric/infinity"))
        zbm_write(fp, ZBMSG0036,
            self.get_value("naming/numeric/nan"),
            self.get_value("naming/numeric/patternDigit"))
        zbm_write(fp, ZBMSG0058,
            self.get_value("naming/numeric/numericDigits"))

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
