"""
Messages for zbcldrcomp.py script.
"""


def zbm_write(fp, code, *args):
    fp.write(code.format(*args) + "\n")


def zbm_new_line(fp):
    fp.write("\n")


ZBMSG0001 = u"   Result : constant Wide_String := \"\""
ZBMSG0002 = u"             & \"{0}\""
ZBMSG0003 = u"             & \"\";"
ZBMSG0004 = u"   Result : constant String_Addresses_Type := ("
ZBMSG0005 = u"      {0} => (First => {1}, Last => {2}),"
ZBMSG0006 = u"      {0} => (First => {1}, Last => {2}));"
ZBMSG0007 = u""
ZBMSG0008 = u"      {0:3} => (Tag => \"{1:<10}\","
ZBMSG0009 = u"              Level => {0},"
ZBMSG0010 = u"              {0} =>"
ZBMSG0011 = u"                 (Jan => {0}, Feb => {1}, Mar => {2},"
ZBMSG0012 = u"                  Apr => {0}, May => {1}, Jun => {2},"
ZBMSG0013 = u"                  Jul => {0}, Aug => {1}, Sep => {2},"
ZBMSG0014 = u"                  Oct => {0}, Nov => {1}, Dec => {2}),"
ZBMSG0015 = u"                 (Sun => {0}, Mon => {1}, Tue => {2},"
ZBMSG0016 = u"                  Wed => {0}, Thu => {1}, Fri => {2},"
ZBMSG0017 = u"                  Sat => {0}),"
ZBMSG0018 = (u"                  (AM => {0}, " +
             u"Wee_Hours => {1}, Early_Morning => {2},")
ZBMSG0019 = u"                  (BCE => {0}, CE => {1}),"
ZBMSG0030 = (u"                  (Full => {0}, Long => {1}, " +
             u"Medium => {2}, Short => {3}),")
ZBMSG0031 = (u"                  (Decimal_Point_Character => {0}, " +
             u"Group_Character => {1},")
ZBMSG0032 = (u"                   List_Character => {0}, " +
             u"Zero_Character => {1},")
ZBMSG0033 = (u"                   Plus_Character => {0}, " +
             u"Minus_Character => {1},")
ZBMSG0034 = (u"                   Exponent_Character => {0}, " +
             u"Percent_Character => {1},")
ZBMSG0035 = (u"                   Permille_Character => {0}, " +
             u"Infinity_Character => {1},")
ZBMSG0036 = (u"                   Nan_Character => {0}, " +
             u"Digit_Pattern_Character => {1},")
ZBMSG0037 = u"                  (Decimal => {0}, Scientific => {1},"
ZBMSG0038 = u"                   Percent => {0}, Currency => {1})),"
ZBMSG0039 = u"                   Percent => {0}, Currency => {1})));"
ZBMSG0040 = u"package body {0} is"
ZBMSG0041 = u"   Locale_Data : constant Trait_Array_Type (1 .. {0}) := ("
ZBMSG0042 = u"              Text_Layout => {0},"
ZBMSG0043 = u"   package body Data_Access is"
ZBMSG0044 = u"      separate;"
ZBMSG0045 = u"end {0};"
ZBMSG0046 = u"   type String_Address_Type is"
ZBMSG0047 = u"      record"
ZBMSG0048 = u"         First : Positive;"
ZBMSG0049 = u"         Last  : Natural;"
ZBMSG0050 = u"      end record;"
ZBMSG0051 = u"   type String_Addresses_Type is"
ZBMSG0052 = (u"       array (String_Index_Type range <>) of " +
             u"String_Address_Type;")
ZBMSG0053 = u"              Name => {0},"
ZBMSG0054 = u"      --  {0}: \"{1}\""
ZBMSG0055 = (u"                   Morning => {0}, Late_Morning => {1}, " +
             u"Noon => {2},")
ZBMSG0056 = (u"                   Midday => {0}, Afternoon => {1}, " +
             u"Evening => {2},")
ZBMSG0057 = (u"                   Late_Evening => {0}, Night => {1}, " +
             u"PM => {2}),")
ZBMSG0058 = u"                   Decimal_Digits_String => {0}),"
ZBMSG0059 = u"                  (0 => {0}, 1 => {1}, 2 => {2},"
ZBMSG0060 = u"                   {0} => {1}, {2} => {3}, {4} => {5},"
ZBMSG0061 = u"                   21 => {0}, 22 => {1}, 23 => {2}),"

ZBMSG1001 = (u"--\n" +
             u"--  This is a generated file.\n" +
             u"--  DO NOT EDIT.\n" +
             u"--\n\n" +
             u"separate (ZanyBlue.Text.Locales)\n" +
             u"function Locale_Data return Trait_Array_Type is\n\n" +
             u"   Result : constant Trait_Array_Type (1 .. {0}) := (\n")
ZBMSG1002 = (u"begin\n" +
             u"   return Result;\n" +
             u"end {0};")
ZBMSG1003 = (u"--\n" +
             u"--  This is a generated file.\n" +
             u"--  DO NOT EDIT.\n" +
             u"--\n\n" +
             u"separate (ZanyBlue.Text.Locales)\n" +
             u"function Pool return Wide_String is\n")
ZBMSG1004 = (u"--\n" +
             u"--  This is a generated file.\n" +
             u"--  DO NOT EDIT.\n" +
             u"--\n\n" +
             u"separate (ZanyBlue.Text.Locales)\n" +
             u"function String_Addresses return String_Addresses_Type is\n")
