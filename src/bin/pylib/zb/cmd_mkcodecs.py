#  -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2016, 2018, Michael Rohan <mrohan@zanyblue.com>
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
See Impl doc string.
"""

import codecs
import os
import re
import urllib.request, urllib.error, urllib.parse
import zb

MATCH_RC = re.compile(r'^0x(?P<enc>[0-9A-F]*)\s+0x(?P<uchar>[0-9A-F]*)')
CODECS_START = "   --  Start of embedded codecs data: Generated, do not edit"
CODECS_END = "   --  End of embedded codecs data"


def normalize_enc(name):
    """
    Download the mapping data for encodings from unicode.org and update the
    "zanyblue-text-codecs.adb" with this data "compiled" into a format (tables)
    for use by the ZanyBlue library.
    """
    return re.sub('[-_.]', '', name)


class Impl(zb.Handler):
    """
    Implementation of the command to "compile" the encoding mapping tables
    from Unicode.org into the ZanyBlue codecs module (static Ada lookup
    tables).
    """

    def __init__(self, zbdev):
        """
        Constructor
        """
        super(Impl, self).__init__(zbdev)
        self.handlers = {
            normalize_enc("ASCII"): {"enc": "EA", "dec": "DA"},
            normalize_enc("ISO8859-1"): {"enc": "EI", "dec": "DI"},
            normalize_enc("UTF-8"): {"enc": "EU", "dec": "DU"},
        }
        self.encoding_table = []
        self.decoding_table = []
        self.mappings = self.get_param('unicode_std_mappings')
        self.codestream = None

    def add_alias(self, alias, target):
        """
        Extend the encodings table with an alias for an existing value.
        """
        alias = normalize_enc(alias)
        target = normalize_enc(target)
        if target not in self.handlers:
            self.fatal(
                'Error: Alias invalid target ("{0}") for alias "{1}"',
                target,
                alias
            )
        if alias in self.handlers:
            self.fatal('Error: Alias "{0}" already defined', alias)
        self.info('Added alias definition "{0}" for "{1}"', alias, target)
        self.handlers[alias] = self.handlers[target]

    def add_encoding(self, name):
        """
        Add the data collected for an encoding to the encodings table.
        """
        mapping = self.load_mapping(name)
        name = normalize_enc(name)
        self.handlers[name] = {
            "name": name,
            "enc": "EL",
            "dec": "DL",
        }
        self.update_encodings(name, mapping)
        self.update_decodings(name, mapping)

    def add_decodings(self, namelist, decoding_map):
        """
        Add the data collected for an encoding to the decodings table.
        """
        cur_table = []
        for char in list(decoding_map.keys()):
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

    def add_options(self, name, parser):
        """
        Add the command specific options.
        """
        parser.add_argument(
            "-E",
            action="append",
            dest="encodings",
            metavar="ENCODING",
            type=str,
            help="List an encoding to include"
        )
        parser.add_argument(
            "-S",
            action="store",
            dest="source",
            default=self.get_param('codecs_src'),
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

    def create_decoding_map(self, name, mapping):
        """
        Create the nested dictionaries to represent the decoding of a byte
        sequence from the mapping table.
        """
        result = {}
        for uchar in sorted(mapping):
            for encoded in mapping[uchar]:
                cur_dict = result
                for char in encoded[:-1]:
                    if char not in cur_dict:
                        cur_dict[char] = {}
                    cur_dict = cur_dict[char]
                if encoded[-1] in cur_dict:
                    self.fatal(
                        'Error: Decoding table error for {}/{}',
                        name,
                        uchar
                    )
                cur_dict[encoded[-1]] = uchar
        return result

    def handle(self, command, options):
        """
        The "main" routine for the command implementation.
        """
        if options.include_ebcdic:
            ebcdic = self.get_param('unicode_ebcdic_mappings')
            for encoding, mapping in list(ebcdic.items()):
                self.mappings[encoding] = mapping
        if options.include_misc:
            misc = self.get_param('unicode_misc_mappings')
            for encoding, mapping in misc:
                self.mappings[encoding] = mapping
        for name in options.encodings or sorted(self.mappings):
            self.add_encoding(name)
        for value in options.aliases:
            try:
                alias, target = value.split("=", 1)
                self.add_alias(alias, target)
            except ValueError:
                self.info("Invalid alias definition \"{0}\"", value)
        self.save(options.source)
        return 0

    def load_mapping(self, name):
        """
        Load an encoding mapping file from Unicode.org.
        """
        if name not in self.mappings:
            self.fatal('Error: No mapping defined for "{0}"', name)
        local_file = os.path.join(
            self.get_param('admindir'),
            "{}.TXT".format(name)
        )
        if not os.path.isfile(local_file):
            url = "{0}/{1}/{2}.TXT".format(
                self.get_param('unicode_url'),
                self.mappings[name],
                name.replace("ISO", "")
            )
            self.info('Downloading data for "{0}" ("{1}")', name, url)
            conn = urllib.request.urlopen(url)
            data = conn.read()
            conn.close()
            outname = os.path.join(
                self.get_param('admindir'),
                "{}.TXT".format(name)
            )
            with open(outname, "w") as dst:
                dst.write(data)
        self.info('Loading data for "{0}" from "{1}"', name, local_file)
        with codecs.open(local_file, "r", "utf-8") as src:
            return self.parse_mapping(name, src.read())

    def parse_mapping(self, name, data):
        """
        Parse the contents of an Unicode mapping file.  This basically maps
        encoded byte sequences to Unicode characters.
        """
        result = {}
        for line in data.split("\n"):
            matches = MATCH_RC.match(line)
            if matches:
                val = matches.group('enc')
                encoded = [val[i:i + 2] for i in range(0, len(val), 2)]
                if len(encoded) > 2:
                    self.fatal(
                        'Error: {0} (> 2) byte encoding defined for "{1}"',
                        len(encoded),
                        name
                    )
                uchar = matches.group('uchar')
                if uchar not in result:
                    result[uchar] = []
                result[uchar].append(encoded)
        return result

    def save(self, filename):
        """
        Save the collected data into the static Ada tables of the ZanyBlue
        codecs source file.
        """
        self.info('Updating the source "{0}"', filename)
        copy_lines = True
        tmp_file = "{}.tmp".format(filename)
        with open(filename, "r") as in_f:
            with open(tmp_file, "w") as out_f:
                self.codestream = out_f
                for line in in_f.readlines():
                    if CODECS_START in line:
                        copy_lines = False
                        self.save_encodings()
                        out_f.write("\n")
                        self.save_decodings()
                        out_f.write("\n")
                        self.save_handlers()
                        out_f.write("\n")
                        self.write_code(CODECS_END)
                    elif CODECS_END in line:
                        copy_lines = True
                    elif copy_lines:
                        out_f.write(line)
        os.unlink(filename)
        os.rename(tmp_file, filename)

    def save_encodings(self):
        """
        Save the table of encoding mappings table to the Ada codecs source.
        """
        self.write_code(CODECS_START)
        self.write_code(
            "   Encoding_Data : constant array (Positive range <>) of "
            "Encoding_Map_Type := ("
        )
        size = len(self.encoding_table) - 1
        cur_mapname = None
        for idx, (uchar, encodings, mapname) in enumerate(self.encoding_table):
            encoding = encodings[0]
            echar1 = encoding[0]
            echar2 = '00'
            twomap = False
            if len(encoding) == 2:
                echar2 = encoding[1]
                twomap = True
            comma = "," if idx < size else ""
            if cur_mapname != mapname:
                self.write_code(
                    "      --  Data for \"{}\" encoding",
                    mapname
                )
                cur_mapname = mapname
            self.write_code(
                "      (16#{}#, Character'Val (16#{}#), "
                "Character'Val (16#{}#), {}){}",
                uchar, echar1, echar2, twomap, comma
            )
        self.write_code("   );")

    def save_decodings(self):
        """
        Save the table of decoding mappings table to the Ada codecs source.
        """
        self.write_code(
            "   Decoding_Data : constant array (Positive range <>) of "
            "Decoding_Map_Type := ("
        )
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
                self.write_code(
                    "      --  Data for \"{}\" encoding",
                    name
                )
                cur_name = name
            if decode_entry["type"] == "direct":
                self.write_code(
                    "      (Character'Val (16#{0}#), Finished, "
                    "Wide_Character'Val (16#{1}#), 0, 0){2}",
                    char, target, comma
                )
            elif decode_entry["type"] == "recurse":
                self.write_code(
                    "      (Character'Val (16#{0}#), Next, "
                    "Wide_Character'First, {1}, {2}){3}",
                    char, start, end, comma
                )
            else:
                self.fatal(
                    'Error: Invalid decoding type "{0}"',
                    decode_entry["type"]
                )
        self.write_code("   );")

    def save_handlers(self):
        """
        Write the table of supported encodings to the Ada codecs source.
        """
        self.write_code(
            "   Handlers : constant array (Positive range <>) of "
            "Handler_Type := ("
        )
        size = len(self.handlers) - 1
        for idx, name in enumerate(sorted(self.handlers)):
            handler = self.handlers[name]
            comma = "," if idx < size else ""
            self.write_code(
                '      ({}, new Wide_String\'("{}"), {}, {}, {}, '
                '{}, {}, {}){}',
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
        self.write_code("   );")

    def update_decodings(self, name, mapping):
        """
        Update the decodings table for a new encoding.
        """
        decoding_map = self.create_decoding_map(name, mapping)
        dstart, dend = self.add_decodings([name], decoding_map)
        self.handlers[name]["dstart"] = dstart
        self.handlers[name]["dend"] = dend

    def update_encodings(self, name, mapping):
        """
        Update the encodings table for a new encoding.
        """
        self.handlers[name]["estart"] = len(self.encoding_table) + 1
        for uchar in sorted(mapping.keys()):
            self.encoding_table.append((uchar, mapping[uchar], name))
        self.handlers[name]["eend"] = len(self.encoding_table)

    def write_code(self, msg, *args):
        """
        Utility to write some code to the Ada codecs module (basically this
        just writes a formatted string and appends a new line).
        """
        self.codestream.write(msg.format(*args))
        self.codestream.write("\n")
