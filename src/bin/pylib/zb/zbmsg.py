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
Messages for zbcldrcomp.py script.
"""


def zbm_write(fp, code, *args):
    fp.write(code.format(*args) + "\n")


def zbm_new_line(fp):
    fp.write("\n")


ZBMSG0001 = "   Result : constant Wide_String := \"\""
ZBMSG0002 = "             & \"{0}\""
ZBMSG0003 = "             & \"\";"
ZBMSG0004 = "   Result : constant String_Addresses_Type := ("
ZBMSG0005 = "      {0} => (First => {1}, Last => {2}),"
ZBMSG0006 = "      {0} => (First => {1}, Last => {2}));"
ZBMSG0007 = ""
ZBMSG0008 = "      {0:3} => (Tag => \"{1:<10}\","
ZBMSG0009 = "              Level => {0},"
ZBMSG0010 = "              {0} =>"
ZBMSG0011 = "                 (Jan => {0}, Feb => {1}, Mar => {2},"
ZBMSG0012 = "                  Apr => {0}, May => {1}, Jun => {2},"
ZBMSG0013 = "                  Jul => {0}, Aug => {1}, Sep => {2},"
ZBMSG0014 = "                  Oct => {0}, Nov => {1}, Dec => {2}),"
ZBMSG0015 = "                 (Sun => {0}, Mon => {1}, Tue => {2},"
ZBMSG0016 = "                  Wed => {0}, Thu => {1}, Fri => {2},"
ZBMSG0017 = "                  Sat => {0}),"
ZBMSG0018 = ("                  (AM => {0}, " +
             "Wee_Hours => {1}, Early_Morning => {2},")
ZBMSG0019 = "                  (BCE => {0}, CE => {1}),"
ZBMSG0030 = ("                  (Full => {0}, Long => {1}, " +
             "Medium => {2}, Short => {3}),")
ZBMSG0031 = ("                  (Decimal_Point_Character => {0}, " +
             "Group_Character => {1},")
ZBMSG0032 = ("                   List_Character => {0}, " +
             "Zero_Character => {1},")
ZBMSG0033 = ("                   Plus_Character => {0}, " +
             "Minus_Character => {1},")
ZBMSG0034 = ("                   Exponent_Character => {0}, " +
             "Percent_Character => {1},")
ZBMSG0035 = ("                   Permille_Character => {0}, " +
             "Infinity_Character => {1},")
ZBMSG0036 = ("                   Nan_Character => {0}, " +
             "Digit_Pattern_Character => {1},")
ZBMSG0037 = "                  (Decimal => {0}, Scientific => {1},"
ZBMSG0038 = "                   Percent => {0}, Currency => {1})),"
ZBMSG0039 = "                   Percent => {0}, Currency => {1})));"
ZBMSG0040 = "package body {0} is"
ZBMSG0041 = "   Locale_Data : constant Trait_Array_Type (1 .. {0}) := ("
ZBMSG0042 = "              Text_Layout => {0},"
ZBMSG0043 = "   package body Data_Access is"
ZBMSG0044 = "      separate;"
ZBMSG0045 = "end {0};"
ZBMSG0046 = "   type String_Address_Type is"
ZBMSG0047 = "      record"
ZBMSG0048 = "         First : Positive;"
ZBMSG0049 = "         Last  : Natural;"
ZBMSG0050 = "      end record;"
ZBMSG0051 = "   type String_Addresses_Type is"
ZBMSG0052 = ("       array (String_Index_Type range <>) of " +
             "String_Address_Type;")
ZBMSG0053 = "              Name => {0},"
ZBMSG0054 = "      --  {0}: \"{1}\""
ZBMSG0055 = ("                   Morning => {0}, Late_Morning => {1}, " +
             "Noon => {2},")
ZBMSG0056 = ("                   Midday => {0}, Afternoon => {1}, " +
             "Evening => {2},")
ZBMSG0057 = ("                   Late_Evening => {0}, Night => {1}, " +
             "PM => {2}),")
ZBMSG0058 = "                   Decimal_Digits_String => {0}),"
ZBMSG0059 = "                  (0 => {0}, 1 => {1}, 2 => {2},"
ZBMSG0060 = "                   {0} => {1}, {2} => {3}, {4} => {5},"
ZBMSG0061 = "                   21 => {0}, 22 => {1}, 23 => {2}),"
ZBMSG0062 = "             & Wide_Character'Val ({0})"

ZBMSG1001 = ("--\n" +
             "--  This is a generated file.\n" +
             "--  DO NOT EDIT.\n" +
             "--\n\n" +
             "separate (ZanyBlue.Text.Locales)\n" +
             "function Locale_Data return Trait_Array_Type is\n\n" +
             "   Result : constant Trait_Array_Type (1 .. {0}) := (\n")
ZBMSG1002 = ("begin\n" +
             "   return Result;\n" +
             "end {0};")
ZBMSG1003 = ("--\n" +
             "--  This is a generated file.\n" +
             "--  DO NOT EDIT.\n" +
             "--\n\n" +
             "separate (ZanyBlue.Text.Locales)\n" +
             "function Pool return Wide_String is\n")
ZBMSG1004 = ("--\n" +
             "--  This is a generated file.\n" +
             "--  DO NOT EDIT.\n" +
             "--\n\n" +
             "separate (ZanyBlue.Text.Locales)\n" +
             "function String_Addresses return String_Addresses_Type is\n")
