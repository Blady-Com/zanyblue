#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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
Download the mapping data for encodings from unicode.org and update the
"zanyblue-text-codecs.adb" with this data "compiled" into a format (tables)
for use by the ZanyBlue library.
"""

import argparse
import os
import re
import sys
import urllib2
from pprint import pprint

SRCDIR = os.path.abspath(os.path.join(
    os.path.dirname(__file__),
    "..",
))
ADMINDIR = os.path.join(SRCDIR, "admin")
ZBCODECS = os.path.join(SRCDIR, "text", "zanyblue-text-codecs.adb")

MATCH_RC = re.compile(r'^0x(?P<enc>[0-9A-F]*)\s+0x(?P<uchar>[0-9A-F]*)')
ROOT_URL = 'http://www.unicode.org/Public/MAPPINGS'
MAPPINGS = {
    "ISO8859-2": "ISO8859/8859-2.TXT",
    "ISO8859-3": "ISO8859/8859-3.TXT",
    "ISO8859-4": "ISO8859/8859-4.TXT",
    "ISO8859-5": "ISO8859/8859-5.TXT",
    "ISO8859-6": "ISO8859/8859-6.TXT",
    "ISO8859-7": "ISO8859/8859-7.TXT",
    "ISO8859-8": "ISO8859/8859-8.TXT",
    "ISO8859-9": "ISO8859/8859-9.TXT",
    "ISO8859-10": "ISO8859/8859-10.TXT",
    "ISO8859-11": "ISO8859/8859-11.TXT",
    "ISO8859-13": "ISO8859/8859-13.TXT",
    "ISO8859-14": "ISO8859/8859-14.TXT",
    "ISO8859-15": "ISO8859/8859-15.TXT",
    "ISO8859-16": "ISO8859/8859-16.TXT",
    "CP874": "VENDORS/MICSFT/WINDOWS/CP874.TXT",
    "CP932": "VENDORS/MICSFT/WINDOWS/CP932.TXT",
    "CP936": "VENDORS/MICSFT/WINDOWS/CP936.TXT",
    "CP949": "VENDORS/MICSFT/WINDOWS/CP949.TXT",
    "CP950": "VENDORS/MICSFT/WINDOWS/CP950.TXT",
    "CP1250": "VENDORS/MICSFT/WINDOWS/CP1250.TXT",
    "CP1251": "VENDORS/MICSFT/WINDOWS/CP1251.TXT",
    "CP1252": "VENDORS/MICSFT/WINDOWS/CP1252.TXT",
    "CP1253": "VENDORS/MICSFT/WINDOWS/CP1253.TXT",
    "CP1254": "VENDORS/MICSFT/WINDOWS/CP1254.TXT",
    "CP1255": "VENDORS/MICSFT/WINDOWS/CP1255.TXT",
    "CP1256": "VENDORS/MICSFT/WINDOWS/CP1256.TXT",
    "CP1257": "VENDORS/MICSFT/WINDOWS/CP1257.TXT",
    "CP1258": "VENDORS/MICSFT/WINDOWS/CP1258.TXT",
}
MISC_MAPPINGS = {
    "CP856": "VENDORS/MISC/CP856.TXT",
    "CP1006": "VENDORS/MISC/CP1006.TXT",
    "KOI8-R": "VENDORS/MISC/KOI8-R.TXT",
    "KOI8-U": "VENDORS/MISC/KOI8-U.TXT",
    "KPS9566": "VENDORS/MISC/KPS9566.TXT",
    "KZ1048": "VENDORS/MISC/KZ1048.TXT",
}
EBCDIC_MAPPINGS = {
    "CP037": "VENDORS/MICSFT/EBCDIC/CP037.TXT",
    "CP500": "VENDORS/MICSFT/EBCDIC/CP500.TXT",
    "CP875": "VENDORS/MICSFT/EBCDIC/CP875.TXT",
    "CP1026": "VENDORS/MICSFT/EBCDIC/CP1026.TXT",
}
_01 = ("Encoding_Data : constant array (Positive range <>) of "
       "Encoding_Map_Type := (")
_02 = "--  Data for \"{}\" encoding"
_03 = "(16#{}#, CV (16#{}#), CV (16#{}#), {}){}"
_04 = ");"
_05 = "Handlers : constant array (Positive range <>) of Handler_Type := ("
_06 = "({}, new Wide_String'(\"{}\"), {}, {}, {}, {}){}"
_07 = "--  End of embedded codecs data"

_ERR_001 = 'Error: Alias "{0}" already defined'
_ERR_002 = 'Error: Alias invalid target ("{0}") for alias "{1}"'
_ERR_003 = 'Error: No mapping defined for "{0}"'
_ERR_004 = 'Error: {0} (> 2) byte encoding defined for "{1}"'
_ERR_005 = 'Error: Decoding table error for {}/{}'
_ERR_006 = 'Error: Invalid decoding type "{0}"'

_INFO_001 = 'Added alias definition "{0}" for "{1}"'
_INFO_002 = 'Loading data for "{0}" from "{1}"'
_INFO_003 = 'Downloading data for "{0}" ("{1}")'
_INFO_004 = 'Updating the source "{0}"'

_CODE_001 = "   --  Start of embedded codecs data: Generated, do not edit"
_CODE_002 = "   --  End of embedded codecs data"
_CODE_003 = ("   Encoding_Data : constant array (Positive range <>) of "
             "Encoding_Map_Type := (")
_CODE_004 = ("      (16#{}#, Character'Val (16#{}#), "
             "Character'Val (16#{}#), {}){}")
_CODE_005 = "   );"
_CODE_006 = ("   Handlers : constant array (Positive range <>) of "
             "Handler_Type := (")
_CODE_007 = "      ({}, new Wide_String'(\"{}\"), {}, {}, {}, {}, {}, {}){}"
_CODE_008 = "      --  Data for \"{}\" encoding"
_CODE_009 = ("   Decoding_Data : constant array (Positive range <>) of "
             "Decoding_Map_Type := (")
_CODE_010 = ("      (Character'Val (16#{0}#), Finished, "
             "Wide_Character'Val (16#{1}#), 0, 0){2}")
_CODE_011 = ("      (Character'Val (16#{0}#), Next, "
             "Wide_Character'First, {1}, {2}){3}")


class Codecs(object):

    def __init__(self):
        self.handlers = {
            self.normalize_enc("ASCII"): {"enc": "EA", "dec": "DA"},
            self.normalize_enc("ISO8859-1"): {"enc": "EI", "dec": "DI"},
            self.normalize_enc("UTF-8"): {"enc": "EU", "dec": "DU"},
        }
        self.encoding_table = []
        self.decoding_table = []

    def info(self, code, *args):
        print(code.format(*args))

    def error(self, code, *args):
        raise Exception(code.format(*args))

    def add_alias(self, alias, target):
        alias = self.normalize_enc(alias)
        target = self.normalize_enc(target)
        if target not in self.handlers:
            self.error(_ERR_002, target, alias)
        if alias in self.handlers:
            self.error(_ERR_001, alias)
        self.info(_INFO_001, alias, target)
        self.handlers[alias] = self.handlers[target]

    def add_encoding(self, name, keep_files, use_local):
        mapping = self.load_mapping(name, keep_files, use_local)
        name = self.normalize_enc(name)
        self.handlers[name] = {
            "name": name,
            "enc": "EL",
            "dec": "DL",
        }
        self.update_encodings(name, mapping)
        self.update_decodings(name, mapping)

    def add_decodings(self, namelist, decoding_map):
        cur_table = []
        for char in decoding_map.keys():
            char_map = {
                "char": char,
                "name": " >> ".join(namelist),
            }
            target = decoding_map[char]
            if isinstance(target, dict):
                dstart, dend = self.add_decodings(
                    namelist + [char],
                    target
                )
                char_map["type"] = "recurse"
                char_map["dstart"] = dstart
                char_map["dend"] = dend
            else:
                char_map["type"] = "direct"
                char_map["target"] = target
            cur_table.append(char_map)
        dstart = len(self.decoding_table) + 1
        self.decoding_table.extend(sorted(cur_table, key=lambda x: x["char"]))
        return dstart, len(self.decoding_table)

    def create_decoding_map(self, name, mapping):
        result = {}
        for uchar in sorted(mapping.keys()):
            for encoded in mapping[uchar]:
                cur_dict = result
                for char in encoded[:-1]:
                    if char not in cur_dict:
                        cur_dict[char] = {}
                    cur_dict = cur_dict[char]
                if encoded[-1] in cur_dict:
                    self.error(_ERR_005, name, uchar)
                cur_dict[encoded[-1]] = uchar
        return result

    def load_mapping(self, name, keep_files, use_local):
        if use_local:
            local_file = os.path.join(ADMINDIR, "{}.TXT".format(name))
            self.info(_INFO_002, name, local_file)
            with open(local_file, "r") as f:
                return self.parse_mapping(name, f.read())
        if name not in MAPPINGS:
            self.error(_ERR_003, name)
        url = "{0}/{1}".format(ROOT_URL, MAPPINGS[name])
        self.info(_INFO_003, name, url)
        conn = urllib2.urlopen(url)
        data = conn.read()
        result = self.parse_mapping(name, data)
        conn.close()
        if keep_files:
            with open(os.path.join(ADMINDIR, "{}.TXT".format(name)), "w") as f:
                f.write(data)
        return result

    def normalize_enc(self, name):
        return name.translate(None, "-_.")

    def parse_mapping(self, name, data):
        result = {}
        for line in data.split("\n"):
            matches = MATCH_RC.match(line)
            if matches:
                val = matches.group('enc')
                encoded = [val[i:i + 2] for i in range(0, len(val), 2)]
                if len(encoded) > 2:
                    self.error(_ERR_004, len(encoded), name)
                uchar = matches.group('uchar')
                if uchar not in result:
                    result[uchar] = []
                result[uchar].append(encoded)
        return result

    def save(self, filename):
        self.info(_INFO_004, filename)
        copy_lines = True
        tmp_file = "{}.tmp".format(filename)
        with open(filename, "r") as in_f:
            with open(tmp_file, "w") as out_f:
                for line in in_f.readlines():
                    if _CODE_001 in line:
                        copy_lines = False
                        self.save_encodings(out_f)
                        out_f.write("\n")
                        self.save_decodings(out_f)
                        out_f.write("\n")
                        self.save_handlers(out_f)
                        out_f.write("\n")
                        self.write_code(out_f, _CODE_002)
                    elif _CODE_002 in line:
                        copy_lines = True
                    elif copy_lines:
                        out_f.write(line)
        os.unlink(filename)
        os.rename(tmp_file, filename)

    def save_encodings(self, f):
        self.write_code(f, _CODE_001)
        self.write_code(f, _CODE_003)
        size = len(self.encoding_table) - 1
        cur_mapname = None
        for idx, (uchar, encodings, mapname) in enumerate(self.encoding_table):
            encoding = encodings[0]
            e1 = encoding[0]
            e2 = '00'
            twomap = False
            if len(encoding) == 2:
                e2 = encoding[1]
                twomap = True
            comma = "," if idx < size else ""
            if cur_mapname != mapname:
                self.write_code(f, _CODE_008, mapname)
                cur_mapname = mapname
            self.write_code(f, _CODE_004, uchar, e1, e2, twomap, comma)
        self.write_code(f, _CODE_005)

    def save_decodings(self, f):
        self.write_code(f, _CODE_009)
        size = len(self.decoding_table) - 1
        cur_name = None
        for idx, decode_entry in enumerate(self.decoding_table):
            char = decode_entry["char"]
            name = decode_entry["name"]
            target = decode_entry.get("target", "0000")
            start = decode_entry.get("dstart", 0)
            end = decode_entry.get("dend", 0)
            comma = "," if idx < size else ""
            if cur_name != name:
                self.write_code(f, _CODE_008, name)
                cur_name = name
            if decode_entry["type"] == "direct":
                self.write_code(f, _CODE_010, char, target, comma)
            elif decode_entry["type"] == "recurse":
                self.write_code(f, _CODE_011, char, start, end, comma)
            else:
                self.error(_ERR_006, decode_entry["type"])
        self.write_code(f, _CODE_005)

    def save_handlers(self, f):
        self.write_code(f, _CODE_006)
        size = len(self.handlers) - 1
        for idx, name in enumerate(sorted(self.handlers.keys())):
            handler = self.handlers[name]
            comma = "," if idx < size else ""
            self.write_code(
                f,
                _CODE_007,
                idx + 1,
                name,
                handler['enc'],
                handler['dec'],
                handler.get('estart', 0),
                handler.get('eend', 0),
                handler.get('dstart', 0),
                handler.get('dend', 0),
                comma
            )
        self.write_code(f, _CODE_005)

    def update_decodings(self, name, mapping):
        decoding_map = self.create_decoding_map(name, mapping)
        dstart, dend = self.add_decodings([name], decoding_map)
        self.handlers[name]["dstart"] = dstart
        self.handlers[name]["dend"] = dend

    def update_encodings(self, name, mapping):
        self.handlers[name]["estart"] = len(self.encoding_table) + 1
        for uchar in sorted(mapping.keys()):
            self.encoding_table.append((uchar, mapping[uchar], name))
        self.handlers[name]["eend"] = len(self.encoding_table)

    def write_code(self, f, msg, *args):
        f.write(msg.format(*args))
        f.write("\n")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-E",
        action="append",
        dest="encodings",
        metavar="ENCODING",
        type=str,
        help="List an encoding to include"
    )
    parser.add_argument(
        "-K",
        action="store_true",
        dest="keep_files",
        default=False,
        help="Keep copies of downloaded encoding files"
    )
    parser.add_argument(
        "-L",
        action="store_true",
        dest="use_local",
        default=False,
        help="Use local copies of encoding data files (via previous -K run)"
    )
    parser.add_argument(
        "-S",
        action="store",
        dest="source",
        default=ZBCODECS,
        type=str,
        help="Source to update with encoding data"
    )
    parser.add_argument(
        "-A",
        action="append",
        dest="aliases",
        default=[],
        type=str,
        help="Define aliases for encodings, e.g., BIG5=CP950"
    )
    parser.add_argument(
        "--ebcdic",
        action="store_true",
        dest="include_ebcdic",
        default=False,
        help="Include EBCDIC encodings (un-tested)"
    )
    parser.add_argument(
        "--misc",
        action="store_true",
        dest="include_misc",
        default=False,
        help="Include MISC encodings (un-tested)"
    )
    codecs = Codecs()
    options = parser.parse_args()
    if options.include_ebcdic:
        for encoding in EBCDIC_MAPPINGS:
            MAPPINGS[encoding] = EBCDIC_MAPPINGS[encoding]
    if options.include_misc:
        for encoding in MISC_MAPPINGS:
            MAPPINGS[encoding] = MISC_MAPPINGS[encoding]
    for name in options.encodings or sorted(MAPPINGS.keys()):
        codecs.add_encoding(name, options.keep_files, options.use_local)
    for value in options.aliases:
        try:
            alias, target = value.split("=", 1)
            codecs.add_alias(alias, target)
        except ValueError:
            print "Invalid alias definition \"{}\"".format(value)
    codecs.save(options.source)
    return 0


if __name__ == '__main__':
    sys.exit(main())
