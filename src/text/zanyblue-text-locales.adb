--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Strings.Wide_Fixed;
with Ada.Environment_Variables;
with ZanyBlue.OS;
with ZanyBlue.Text.Utils;
pragma Elaborate_All (Zanyblue.OS);
pragma Elaborate_All (Zanyblue.Text.Utils);

package body ZanyBlue.Text.Locales is

   use Ada.Strings.Wide_Fixed;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Utils;

   Max_Tag_Length : constant := Max_Language_Length
                              + Max_Script_Length
                              + Max_Territory_Length;
   --  Maximum length of the internal locale name, it's simply a concatenation
   --  of the language, script and territory names, uppercased.

   subtype Tag_Type is Wide_String (1 .. Max_Tag_Length);
   --  Internal locale identification: Lang + Script + Terr padded with
   --  spaces, e.g., "EN LATNUS " for "en_Latn_US".

   type String_Index_Type is new Positive range 1 .. 12_000;
   --  Strings are accessed via a simple table giving start and ending indexes
   --  of the string within the "global" string pool.  Each string is stored
   --  within locale structures as a index into this table.  Loading all
   --  available CLDR locales generates a pool with ~6500 strings.  This
   --  range should be sufficient for definitions for quite some time.

   type String_Address_Type is
      record
         First : Positive;
         Last  : Natural;
      end record;
   --  Start and end of a string with the pool

   type String_Addresses_Type is
       array (String_Index_Type range <>) of String_Address_Type;
   --  The collection of strings defined for all locales accessed by index.

   type Month_Names_Type is array (Month_Type) of String_Index_Type;
   --  List of strings (by index) for month names in a locale.

   type Day_Names_Type is array (Day_Type) of String_Index_Type;
   --  List of strings (by index) for day names in a locale.

   type Day_Period_Names_Type is array (Day_Period_Type) of String_Index_Type;
   --  List of strings (by index) for day period names in a locale.

   type Day_Period_Map_Type is array (Hour_Type) of Day_Period_Type;
   --  Mapping from hour number to day period name.

   type Era_Names_Type is array (Era_Type) of String_Index_Type;
   --  List of strings (by index) for era names in a locale.

   type Date_Time_Styles_Type is
      array (Date_Time_Style_Type) of String_Index_Type;
   --  List of strings (by index) for various time/date formats in a locale.

   type Numeric_Items_Type is array (Numeric_Item_Type) of String_Index_Type;
   --  List of strings (by index) for various numeric items in a locale.

   type Numeric_Format_Type is array (Numeric_Style_Type) of String_Index_Type;
   --  List of strings (by index) for various numeric formats in a locale.

   type Locale_Traits_Type is
      record
         Tag                : Tag_Type;
         Level              : Level_Type;
         Name               : String_Index_Type;
         Text_Layout        : Text_Layout_Type;
         Short_Month_Names  : Month_Names_Type;
         Full_Month_Names   : Month_Names_Type;
         Short_Day_Names    : Day_Names_Type;
         Full_Day_Names     : Day_Names_Type;
         Day_Period_Names   : Day_Period_Names_Type;
         Exact_Day_Periods  : Day_Period_Map_Type;
         Within_Day_Periods : Day_Period_Map_Type;
         Era_Names          : Era_Names_Type;
         Date_Formats       : Date_Time_Styles_Type;
         Time_Formats       : Date_Time_Styles_Type;
         Date_Time_Formats  : Date_Time_Styles_Type;
         Numeric_Items      : Numeric_Items_Type;
         Numeric_Formats    : Numeric_Format_Type;
      end record;
   --  Collection of strings used for each locale.

   type Trait_Array_Type is
      array (Trait_Index_Type range <>) of Locale_Traits_Type;
   --  List of predefined traits

   Current_Locale_Value : Locale_Type;
   --  Current locale initialized by the environment (variable or underlying
   --  OS defintion).

   --------------------------------------------------------------------------
   --  The following data structures are generated from the Unicode.org    --
   --  CLDR data and inserted into this file.  Please do not edit this     --
   --  data.  The markers on the below is used by the edit process and     --
   --  should not be removed.                                              --
   --                                                                      --
   --  BEGIN-CLDR-DATA                                                     --

   Locale_Data : constant Trait_Array_Type (1 .. 34) := (

        1 => (Tag => "          ",
              Level => 0,
              Name => 1,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1066, CE => 1062),
              Date_Formats =>
                  (Full => 1027, Long => 948, Medium => 949, Short => 951),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 976,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

        2 => (Tag => "AR        ",
              Level => 1,
              Name => 838,
              Text_Layout => Right_To_Left,
              Short_Day_Names =>
                 (Sun => 170, Mon => 168, Tue => 167,
                  Wed => 169, Thu => 165, Fri => 166,
                  Sat => 164),
              Full_Day_Names =>
                 (Sun => 170, Mon => 168, Tue => 167,
                  Wed => 169, Thu => 165, Fri => 166,
                  Sat => 164),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 154, Feb => 160, Mar => 157,
                  Apr => 173, May => 156, Jun => 152,
                  Jul => 153, Aug => 172, Sep => 162,
                  Oct => 171, Nov => 155, Dec => 163),
              Day_Period_Names =>
                  (AM => 161, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 158),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 159, CE => 158),
              Date_Formats =>
                  (Full => 1020, Long => 811, Medium => 788, Short => 757),
              Time_Formats =>
                  (Full => 352, Long => 361, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 151,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 23),
              Numeric_Formats =>
                  (Decimal => 1131, Scientific => 1128,
                   Percent => 1133, Currency => 339)),

        3 => (Tag => "CS        ",
              Level => 1,
              Name => 823,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 544, Mon => 496, Tue => 328,
                  Wed => 444, Thu => 322, Fri => 484,
                  Sat => 449),
              Full_Day_Names =>
                 (Sun => 542, Mon => 492, Tue => 327,
                  Wed => 440, Thu => 321, Fri => 483,
                  Sat => 447),
              Short_Month_Names =>
                 (Jan => 958, Feb => 333, Mar => 1063,
                  Apr => 1042, May => 960, Jun => 326,
                  Jul => 325, Aug => 883, Sep => 865,
                  Oct => 320, Nov => 957, Dec => 900),
              Full_Month_Names =>
                 (Jan => 632, Feb => 329, Mar => 826,
                  Apr => 761, May => 640, Jun => 323,
                  Jul => 324, Aug => 445, Sep => 349,
                  Oct => 319, Nov => 626, Dec => 488),
              Day_Period_Names =>
                  (AM => 764, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 522),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 481, CE => 552),
              Date_Formats =>
                  (Full => 1021, Long => 806, Medium => 808, Short => 793),
              Time_Formats =>
                  (Full => 991, Long => 992, Medium => 993, Short => 994),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

        4 => (Tag => "DA        ",
              Level => 1,
              Name => 797,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 426, Mon => 596, Tue => 416,
                  Wed => 516, Thu => 410, Fri => 719,
                  Sat => 612),
              Full_Day_Names =>
                 (Sun => 424, Mon => 594, Tue => 414,
                  Wed => 514, Thu => 407, Fri => 717,
                  Sat => 610),
              Short_Month_Names =>
                 (Jan => 681, Feb => 733, Mar => 591,
                  Apr => 844, May => 598, Jun => 658,
                  Jul => 663, Aug => 835, Sep => 464,
                  Oct => 520, Nov => 533, Dec => 784),
              Full_Month_Names =>
                 (Jan => 679, Feb => 729, Mar => 583,
                  Apr => 843, May => 598, Jun => 656,
                  Jul => 661, Aug => 834, Sep => 462,
                  Oct => 519, Nov => 532, Dec => 783),
              Day_Period_Names =>
                  (AM => 735, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 569,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 754),
              Exact_Day_Periods =>
                  (0 => Early_Morning, 1 => Early_Morning, 2 => Early_Morning,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Early_Morning, 7 => Early_Morning, 8 => Early_Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Noon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Night, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Early_Morning, 1 => Early_Morning, 2 => Early_Morning,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Early_Morning, 7 => Early_Morning, 8 => Early_Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Afternoon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Night, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 736, CE => 755),
              Date_Formats =>
                  (Full => 1032, Long => 807, Medium => 790, Short => 791),
              Time_Formats =>
                  (Full => 985, Long => 986, Medium => 987, Short => 988),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1123, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

        5 => (Tag => "DE        ",
              Level => 1,
              Name => 787,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 885, Mon => 932, Tue => 1048,
                  Wed => 935, Thu => 1046, Fri => 1006,
                  Sat => 896),
              Full_Day_Names =>
                 (Sun => 884, Mon => 929, Tue => 1047,
                  Wed => 933, Thu => 1044, Fri => 1005,
                  Sat => 891),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 925,
                  Apr => 1075, May => 945, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 909, Nov => 916, Dec => 1050),
              Full_Month_Names =>
                 (Jan => 972, Feb => 1008, Mar => 924,
                  Apr => 1074, May => 945, Jun => 964,
                  Jul => 968, Aug => 1070, Sep => 886,
                  Oct => 908, Nov => 915, Dec => 1049),
              Day_Period_Names =>
                  (AM => 392, Wee_Hours => 1, Early_Morning => 564,
                   Morning => 391, Late_Morning => 1, Noon => 934,
                   Midday => 1, Afternoon => 548, Evening => 857,
                   Late_Evening => 1, Night => 547, PM => 549),
              Exact_Day_Periods =>
                  (0 => Early_Morning, 1 => Early_Morning, 2 => Early_Morning,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Early_Morning, 7 => Early_Morning, 8 => Early_Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Noon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Night, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Early_Morning, 1 => Early_Morning, 2 => Early_Morning,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Early_Morning, 7 => Early_Morning, 8 => Early_Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Afternoon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Night, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 401, CE => 553),
              Date_Formats =>
                  (Full => 1021, Long => 806, Medium => 792, Short => 793),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

        6 => (Tag => "EL        ",
              Level => 1,
              Name => 751,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 294, Mon => 303, Tue => 269,
                  Wed => 272, Thu => 278, Fri => 280,
                  Sat => 275),
              Full_Day_Names =>
                 (Sun => 293, Mon => 302, Tue => 270,
                  Wed => 271, Thu => 281, Fri => 279,
                  Sat => 276),
              Short_Month_Names =>
                 (Jan => 300, Feb => 267, Mar => 290,
                  Apr => 310, May => 288, Jun => 296,
                  Jul => 298, Aug => 308, Sep => 274,
                  Oct => 283, Nov => 286, Dec => 305),
              Full_Month_Names =>
                 (Jan => 299, Feb => 266, Mar => 289,
                  Apr => 309, May => 291, Jun => 295,
                  Jul => 297, Aug => 307, Sep => 273,
                  Oct => 282, Nov => 285, Dec => 304),
              Day_Period_Names =>
                  (AM => 262, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 264),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 263, CE => 265),
              Date_Formats =>
                  (Full => 1025, Long => 815, Medium => 816, Short => 802),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1123, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 756, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 1130)),

        7 => (Tag => "EN        ",
              Level => 1,
              Name => 748,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => Noon, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 1027, Long => 948, Medium => 949, Short => 951),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

        8 => (Tag => "EN     AU ",
              Level => 2,
              Name => 747,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => Noon, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 1025, Long => 815, Medium => 790, Short => 800),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

        9 => (Tag => "EN     CA ",
              Level => 2,
              Name => 746,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => Noon, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 1023, Long => 812, Medium => 370, Short => 373),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

       10 => (Tag => "EN     GB ",
              Level => 2,
              Name => 745,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => Noon, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 1025, Long => 815, Medium => 816, Short => 790),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 343)),

       11 => (Tag => "EN     IE ",
              Level => 2,
              Name => 744,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 859, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 507),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => Noon, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 1031, Long => 815, Medium => 816, Short => 790),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

       12 => (Tag => "EN     NZ ",
              Level => 2,
              Name => 743,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => Noon, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 1025, Long => 815, Medium => 799, Short => 800),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

       13 => (Tag => "EN     ZA ",
              Level => 2,
              Name => 742,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 882, Mon => 931, Tue => 871,
                  Wed => 867, Thu => 873, Fri => 1004,
                  Sat => 890),
              Full_Day_Names =>
                 (Sun => 881, Mon => 930, Tue => 870,
                  Wed => 866, Thu => 872, Fri => 1003,
                  Sat => 888),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 971, Feb => 1007, Mar => 943,
                  Apr => 1074, May => 941, Jun => 965,
                  Jul => 967, Aug => 1070, Sep => 886,
                  Oct => 910, Nov => 915, Dec => 1052),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => Noon, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 1029, Long => 795, Medium => 796, Short => 366),
              Time_Formats =>
                  (Full => 701, Long => 702, Medium => 703, Short => 704),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

       14 => (Tag => "ES        ",
              Level => 1,
              Name => 738,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 768, Mon => 622, Tue => 592,
                  Wed => 566, Thu => 669, Fri => 395,
                  Sat => 431),
              Full_Day_Names =>
                 (Sun => 766, Mon => 618, Tue => 585,
                  Wed => 565, Thu => 668, Fri => 394,
                  Sat => 430),
              Short_Month_Names =>
                 (Jan => 741, Feb => 734, Mar => 592,
                  Apr => 856, May => 578, Jun => 659,
                  Jul => 664, Aug => 853, Sep => 465,
                  Oct => 527, Nov => 534, Dec => 775),
              Full_Month_Names =>
                 (Jan => 740, Feb => 730, Mar => 582,
                  Apr => 855, May => 577, Jun => 655,
                  Jul => 660, Aug => 852, Sep => 458,
                  Oct => 523, Nov => 528, Dec => 773),
              Day_Period_Names =>
                  (AM => 859, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 507),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 860, CE => 805),
              Date_Formats =>
                  (Full => 1026, Long => 817, Medium => 790, Short => 791),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 1130)),

       15 => (Tag => "FI        ",
              Level => 1,
              Name => 722,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 439, Mon => 607, Tue => 418,
                  Wed => 645, Thu => 411, Fri => 503,
                  Sat => 636),
              Full_Day_Names =>
                 (Sun => 438, Mon => 605, Tue => 417,
                  Wed => 643, Thu => 406, Fri => 502,
                  Sat => 633),
              Short_Month_Names =>
                 (Jan => 423, Feb => 698, Mar => 606,
                  Apr => 696, May => 405, Jun => 642,
                  Jul => 699, Aug => 750, Sep => 436,
                  Oct => 625, Nov => 588, Dec => 670),
              Full_Month_Names =>
                 (Jan => 423, Feb => 698, Mar => 606,
                  Apr => 696, May => 405, Jun => 642,
                  Jul => 699, Aug => 750, Sep => 436,
                  Oct => 625, Nov => 588, Dec => 670),
              Day_Period_Names =>
                  (AM => 847, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 690),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 753, CE => 684),
              Date_Formats =>
                  (Full => 824, Long => 806, Medium => 803, Short => 803),
              Time_Formats =>
                  (Full => 995, Long => 996, Medium => 997, Short => 998),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 739, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

       16 => (Tag => "FR        ",
              Level => 1,
              Name => 720,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 772, Mon => 621, Tue => 591,
                  Wed => 573, Thu => 673, Fri => 398,
                  Sat => 469),
              Full_Day_Names =>
                 (Sun => 771, Mon => 620, Tue => 589,
                  Wed => 571, Thu => 672, Fri => 397,
                  Sat => 468),
              Short_Month_Names =>
                 (Jan => 675, Feb => 716, Mar => 587,
                  Apr => 828, May => 600, Jun => 665,
                  Jul => 667, Aug => 848, Sep => 463,
                  Oct => 526, Nov => 533, Dec => 759),
              Full_Month_Names =>
                 (Jan => 674, Feb => 715, Mar => 587,
                  Apr => 827, May => 600, Jun => 665,
                  Jul => 666, Aug => 848, Sep => 460,
                  Oct => 525, Nov => 530, Dec => 758),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 579, Late_Morning => 1, Noon => 568,
                   Midday => 1, Afternoon => 840, Evening => 1,
                   Late_Evening => 1, Night => 446, PM => 906),
              Exact_Day_Periods =>
                  (0 => Morning, 1 => Morning, 2 => Morning,
                   3 => Morning, 4 => Morning, 5 => Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Noon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Afternoon, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Morning, 1 => Morning, 2 => Morning,
                   3 => Morning, 4 => Morning, 5 => Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Afternoon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Afternoon, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 829, CE => 846),
              Date_Formats =>
                  (Full => 1031, Long => 815, Medium => 816, Short => 791),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

       17 => (Tag => "GA        ",
              Level => 1,
              Name => 713,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 1045, Mon => 955, Tue => 927,
                  Wed => 1056, Thu => 1035, Fri => 1076,
                  Sat => 889),
              Full_Day_Names =>
                 (Sun => 1040, Mon => 1039, Tue => 1038,
                  Wed => 1041, Thu => 1034, Fri => 1036,
                  Sat => 1037),
              Short_Month_Names =>
                 (Jan => 1018, Feb => 1011, Mar => 926,
                  Apr => 1078, May => 1065, Jun => 939,
                  Jul => 975, Aug => 954, Sep => 950,
                  Oct => 1054, Nov => 893, Dec => 918),
              Full_Month_Names =>
                 (Jan => 1017, Feb => 1010, Mar => 926,
                  Apr => 1077, May => 1064, Jun => 938,
                  Jul => 975, Aug => 953, Sep => 937,
                  Oct => 1051, Nov => 892, Dec => 917),
              Day_Period_Names =>
                  (AM => 859, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 507),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 898, CE => 1080),
              Date_Formats =>
                  (Full => 1031, Long => 815, Medium => 816, Short => 790),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 976,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 343)),

       18 => (Tag => "HE        ",
              Level => 1,
              Name => 700,
              Text_Layout => Right_To_Left,
              Short_Day_Names =>
                 (Sun => 202, Mon => 201, Tue => 200,
                  Wed => 199, Thu => 198, Fri => 197,
                  Sat => 174),
              Full_Day_Names =>
                 (Sun => 195, Mon => 190, Tue => 191,
                  Wed => 194, Thu => 196, Fri => 192,
                  Sat => 193),
              Short_Month_Names =>
                 (Jan => 187, Feb => 176, Mar => 181,
                  Apr => 208, May => 182, Jun => 189,
                  Jul => 204, Aug => 213, Sep => 178,
                  Oct => 211, Nov => 180, Dec => 206),
              Full_Month_Names =>
                 (Jan => 186, Feb => 175, Mar => 181,
                  Apr => 207, May => 182, Jun => 188,
                  Jul => 203, Aug => 212, Sep => 177,
                  Oct => 210, Nov => 179, Dec => 205),
              Day_Period_Names =>
                  (AM => 183, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 209),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 184, CE => 185),
              Date_Formats =>
                  (Full => 1022, Long => 809, Medium => 810, Short => 791),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 1130)),

       19 => (Tag => "HU        ",
              Level => 1,
              Name => 697,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 869, Mon => 1001, Tue => 963,
                  Wed => 879, Thu => 1060, Fri => 907,
                  Sat => 878),
              Full_Day_Names =>
                 (Sun => 400, Mon => 695, Tue => 644,
                  Wed => 433, Thu => 822, Fri => 482,
                  Sat => 432),
              Short_Month_Names =>
                 (Jan => 681, Feb => 731, Mar => 557,
                  Apr => 332, May => 560, Jun => 649,
                  Jul => 653, Aug => 835, Sep => 435,
                  Oct => 520, Nov => 533, Dec => 784),
              Full_Month_Names =>
                 (Jan => 677, Feb => 726, Mar => 556,
                  Apr => 331, May => 558, Jun => 647,
                  Jul => 651, Aug => 830, Sep => 434,
                  Oct => 518, Nov => 532, Dec => 783),
              Day_Period_Names =>
                  (AM => 786, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 762),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 694, CE => 693),
              Date_Formats =>
                  (Full => 376, Long => 377, Medium => 368, Short => 368),
              Time_Formats =>
                  (Full => 991, Long => 992, Medium => 993, Short => 994),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 1130)),

       20 => (Tag => "IT        ",
              Level => 1,
              Name => 689,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 768, Mon => 622, Tue => 592,
                  Wed => 574, Thu => 710, Fri => 399,
                  Sat => 471),
              Full_Day_Names =>
                 (Sun => 767, Mon => 619, Tue => 586,
                  Wed => 572, Thu => 709, Fri => 396,
                  Sat => 470),
              Short_Month_Names =>
                 (Jan => 712, Feb => 734, Mar => 592,
                  Apr => 845, May => 602, Jun => 708,
                  Jul => 624, Aug => 853, Sep => 457,
                  Oct => 513, Nov => 534, Dec => 775),
              Full_Month_Names =>
                 (Jan => 711, Feb => 732, Mar => 582,
                  Apr => 842, May => 601, Jun => 707,
                  Jul => 623, Aug => 852, Sep => 455,
                  Oct => 512, Nov => 530, Dec => 774),
              Day_Period_Names =>
                  (AM => 608, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 508),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 858, CE => 798),
              Date_Formats =>
                  (Full => 1031, Long => 795, Medium => 789, Short => 791),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 340)),

       21 => (Tag => "JA        ",
              Level => 1,
              Name => 683,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 87, Mon => 77, Tue => 69,
                  Wed => 72, Thu => 75, Fri => 58,
                  Sat => 89),
              Full_Day_Names =>
                 (Sun => 86, Mon => 76, Tue => 68,
                  Wed => 71, Thu => 74, Fri => 57,
                  Sat => 88),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 1108, Feb => 1105, Mar => 1102,
                  Apr => 1099, May => 1096, Jun => 1093,
                  Jul => 1090, Aug => 1087, Sep => 1084,
                  Oct => 1117, Nov => 1114, Dec => 1110),
              Day_Period_Names =>
                  (AM => 98, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 73,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 97),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 1067, CE => 1080),
              Date_Formats =>
                  (Full => 364, Long => 365, Medium => 366, Short => 366),
              Time_Formats =>
                  (Full => 978, Long => 992, Medium => 993, Short => 994),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 976,
                   Nan_Character => 921, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 343)),

       22 => (Tag => "KO        ",
              Level => 1,
              Name => 641,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 43, Mon => 45, Tue => 39,
                  Wed => 49, Thu => 52, Fri => 55,
                  Sat => 41),
              Full_Day_Names =>
                 (Sun => 42, Mon => 44, Tue => 38,
                  Wed => 48, Thu => 51, Fri => 54,
                  Sat => 40),
              Short_Month_Names =>
                 (Jan => 973, Feb => 1009, Mar => 944,
                  Apr => 1075, May => 941, Jun => 966,
                  Jul => 969, Aug => 1071, Sep => 887,
                  Oct => 911, Nov => 916, Dec => 1053),
              Full_Month_Names =>
                 (Jan => 1107, Feb => 1104, Mar => 1101,
                  Apr => 1098, May => 1095, Jun => 1092,
                  Jul => 1089, Aug => 1086, Sep => 1083,
                  Oct => 1116, Nov => 1113, Dec => 1109),
              Day_Period_Names =>
                  (AM => 47, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 46),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 53, CE => 50),
              Date_Formats =>
                  (Full => 362, Long => 363, Medium => 369, Short => 372),
              Time_Formats =>
                  (Full => 861, Long => 862, Medium => 863, Short => 864),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 343)),

       23 => (Tag => "NB        ",
              Level => 1,
              Name => 545,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 425, Mon => 595, Tue => 415,
                  Wed => 515, Thu => 409, Fri => 718,
                  Sat => 611),
              Full_Day_Names =>
                 (Sun => 424, Mon => 594, Tue => 414,
                  Wed => 514, Thu => 407, Fri => 717,
                  Sat => 610),
              Short_Month_Names =>
                 (Jan => 681, Feb => 733, Mar => 587,
                  Apr => 844, May => 600, Jun => 656,
                  Jul => 661, Aug => 835, Sep => 464,
                  Oct => 520, Nov => 533, Dec => 780),
              Full_Month_Names =>
                 (Jan => 679, Feb => 729, Mar => 587,
                  Apr => 843, May => 600, Jun => 656,
                  Jul => 661, Aug => 834, Sep => 462,
                  Oct => 519, Nov => 532, Dec => 779),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 736, CE => 755),
              Date_Formats =>
                  (Full => 1030, Long => 806, Medium => 807, Short => 793),
              Time_Formats =>
                  (Full => 1125, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 348, Long => 348, Medium => 348, Short => 348),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 340)),

       24 => (Tag => "NL        ",
              Level => 1,
              Name => 538,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 354, Mon => 607, Tue => 776,
                  Wed => 383, Thu => 769, Fri => 390,
                  Sat => 360),
              Full_Day_Names =>
                 (Sun => 353, Mon => 604, Tue => 770,
                  Wed => 382, Thu => 765, Fri => 389,
                  Sat => 357),
              Short_Month_Names =>
                 (Jan => 681, Feb => 733, Mar => 562,
                  Apr => 844, May => 576, Jun => 658,
                  Jul => 663, Aug => 835, Sep => 464,
                  Oct => 520, Nov => 533, Dec => 784),
              Full_Month_Names =>
                 (Jan => 678, Feb => 728, Mar => 603,
                  Apr => 843, May => 576, Jun => 656,
                  Jul => 661, Aug => 831, Sep => 462,
                  Oct => 519, Nov => 532, Dec => 783),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 1111,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => Early_Morning, 1 => Early_Morning, 2 => Early_Morning,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Early_Morning, 7 => Early_Morning, 8 => Early_Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Noon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Night, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Early_Morning, 1 => Early_Morning, 2 => Early_Morning,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Early_Morning, 7 => Early_Morning, 8 => Early_Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Afternoon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Night, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 401, CE => 553),
              Date_Formats =>
                  (Full => 1031, Long => 815, Medium => 816, Short => 794),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 339)),

       25 => (Tag => "PL        ",
              Level => 1,
              Name => 498,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 541, Mon => 494, Tue => 379,
                  Wed => 318, Thu => 819, Fri => 485,
                  Sat => 448),
              Full_Day_Names =>
                 (Sun => 540, Mon => 491, Tue => 378,
                  Wed => 317, Thu => 818, Fri => 499,
                  Sat => 447),
              Short_Month_Names =>
                 (Jan => 442, Feb => 616, Mar => 592,
                  Apr => 639, May => 598, Jun => 821,
                  Jul => 630, Aug => 452, Sep => 381,
                  Oct => 505, Nov => 628, Dec => 706),
              Full_Month_Names =>
                 (Jan => 441, Feb => 615, Mar => 590,
                  Apr => 638, May => 597, Jun => 820,
                  Jul => 629, Aug => 451, Sep => 380,
                  Oct => 504, Nov => 627, Dec => 705),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 546,
                   Morning => 476, Late_Morning => 487, Noon => 386,
                   Midday => 1, Afternoon => 495, Evening => 384,
                   Late_Evening => 1, Night => 387, PM => 906),
              Exact_Day_Periods =>
                  (0 => Night, 1 => Night, 2 => Night,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Late_Morning, 11 => Late_Morning,
                   12 => Noon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Evening, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Evening, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Night, 1 => Night, 2 => Night,
                   3 => Early_Morning, 4 => Early_Morning, 5 => Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Late_Morning, 11 => Late_Morning,
                   12 => Afternoon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Evening, 17 => Evening,
                   18 => Evening, 19 => Evening, 20 => Evening,
                   21 => Evening, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 506, CE => 551),
              Date_Formats =>
                  (Full => 1025, Long => 815, Medium => 816, Short => 792),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 1130)),

       26 => (Tag => "PT        ",
              Level => 1,
              Name => 486,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 768, Mon => 467, Tue => 421,
                  Wed => 480, Thu => 478, Fri => 454,
                  Sat => 431),
              Full_Day_Names =>
                 (Sun => 766, Mon => 466, Tue => 420,
                  Wed => 479, Thu => 477, Fri => 453,
                  Sat => 430),
              Short_Month_Names =>
                 (Jan => 682, Feb => 724, Mar => 592,
                  Apr => 856, May => 600, Jun => 659,
                  Jul => 664, Aug => 853, Sep => 457,
                  Oct => 511, Nov => 534, Dec => 778),
              Full_Month_Names =>
                 (Jan => 680, Feb => 723, Mar => 581,
                  Apr => 855, May => 599, Jun => 657,
                  Jul => 662, Aug => 852, Sep => 456,
                  Oct => 510, Nov => 529, Dec => 777),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 593, Late_Morning => 1, Noon => 575,
                   Midday => 1, Afternoon => 422, Evening => 1,
                   Late_Evening => 1, Night => 536, PM => 906),
              Exact_Day_Periods =>
                  (0 => Morning, 1 => Morning, 2 => Morning,
                   3 => Morning, 4 => Morning, 5 => Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Noon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Afternoon, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Morning, 1 => Morning, 2 => Morning,
                   3 => Morning, 4 => Morning, 5 => Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Afternoon, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Afternoon, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 860, CE => 805),
              Date_Formats =>
                  (Full => 1026, Long => 817, Medium => 790, Short => 791),
              Time_Formats =>
                  (Full => 989, Long => 990, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 342)),

       27 => (Tag => "RO        ",
              Level => 1,
              Name => 474,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 1043, Mon => 956, Tue => 946,
                  Wed => 936, Thu => 970, Fri => 868,
                  Sat => 877),
              Full_Day_Names =>
                 (Sun => 760, Mon => 617, Tue => 580,
                  Wed => 567, Thu => 671, Fri => 393,
                  Sat => 429),
              Short_Month_Names =>
                 (Jan => 692, Feb => 733, Mar => 591,
                  Apr => 844, May => 600, Jun => 686,
                  Jul => 688, Aug => 835, Sep => 463,
                  Oct => 526, Nov => 533, Dec => 784),
              Full_Month_Names =>
                 (Jan => 691, Feb => 727, Mar => 584,
                  Apr => 841, May => 600, Jun => 685,
                  Jul => 687, Aug => 834, Sep => 459,
                  Oct => 524, Nov => 537, Dec => 781),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 330, CE => 804),
              Date_Formats =>
                  (Full => 1025, Long => 815, Medium => 792, Short => 792),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 346, Long => 346, Medium => 346, Short => 346),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 1130)),

       28 => (Tag => "RU        ",
              Level => 1,
              Name => 473,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 247, Mon => 230, Tue => 246,
                  Wed => 222, Thu => 216, Fri => 227,
                  Sat => 225),
              Full_Day_Names =>
                 (Sun => 248, Mon => 229, Tue => 245,
                  Wed => 221, Thu => 217, Fri => 226,
                  Sat => 220),
              Short_Month_Names =>
                 (Jan => 215, Feb => 219, Mar => 238,
                  Apr => 250, May => 237, Jun => 239,
                  Jul => 240, Aug => 252, Sep => 224,
                  Oct => 232, Nov => 234, Dec => 244),
              Full_Month_Names =>
                 (Jan => 214, Feb => 218, Mar => 238,
                  Apr => 249, May => 237, Jun => 239,
                  Jul => 240, Aug => 251, Sep => 223,
                  Oct => 231, Nov => 233, Dec => 243),
              Day_Period_Names =>
                  (AM => 241, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 228),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 242, CE => 236),
              Date_Formats =>
                  (Full => 1024, Long => 813, Medium => 792, Short => 793),
              Time_Formats =>
                  (Full => 991, Long => 992, Medium => 993, Short => 994),
              Date_Time_Formats =>
                  (Full => 346, Long => 346, Medium => 346, Short => 346),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 235, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

       29 => (Tag => "SK        ",
              Level => 1,
              Name => 450,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 544, Mon => 496, Tue => 403,
                  Wed => 444, Thu => 313, Fri => 501,
                  Sat => 449),
              Full_Day_Names =>
                 (Sun => 543, Mon => 493, Tue => 402,
                  Wed => 443, Thu => 312, Fri => 500,
                  Sat => 447),
              Short_Month_Names =>
                 (Jan => 682, Feb => 734, Mar => 592,
                  Apr => 845, May => 561, Jun => 650,
                  Jul => 654, Aug => 836, Sep => 465,
                  Oct => 521, Nov => 534, Dec => 785),
              Full_Month_Names =>
                 (Jan => 676, Feb => 725, Mar => 590,
                  Apr => 839, May => 559, Jun => 648,
                  Jul => 652, Aug => 833, Sep => 461,
                  Oct => 517, Nov => 531, Dec => 782),
              Day_Period_Names =>
                  (AM => 763, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 490),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 489, CE => 550),
              Date_Formats =>
                  (Full => 1021, Long => 806, Medium => 803, Short => 803),
              Time_Formats =>
                  (Full => 991, Long => 992, Medium => 993, Short => 994),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1123, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 976,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

       30 => (Tag => "SV        ",
              Level => 1,
              Name => 437,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 428, Mon => 555, Tue => 413,
                  Wed => 516, Thu => 408, Fri => 719,
                  Sat => 614),
              Full_Day_Names =>
                 (Sun => 427, Mon => 554, Tue => 412,
                  Wed => 514, Thu => 407, Fri => 717,
                  Sat => 613),
              Short_Month_Names =>
                 (Jan => 682, Feb => 734, Mar => 592,
                  Apr => 845, May => 598, Jun => 659,
                  Jul => 664, Aug => 836, Sep => 465,
                  Oct => 521, Nov => 534, Dec => 785),
              Full_Month_Names =>
                 (Jan => 678, Feb => 728, Mar => 587,
                  Apr => 843, May => 598, Jun => 656,
                  Jul => 661, Aug => 832, Sep => 462,
                  Oct => 519, Nov => 532, Dec => 783),
              Day_Period_Names =>
                  (AM => 721, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 749),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 736, CE => 755),
              Date_Formats =>
                  (Full => 1028, Long => 815, Medium => 816, Short => 370),
              Time_Formats =>
                  (Full => 1125, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 344,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 106,
                   Exponent_Character => 334, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 338, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1129, Currency => 1130)),

       31 => (Tag => "TH        ",
              Level => 1,
              Name => 419,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 110, Mon => 142, Tue => 111,
                  Wed => 136, Thu => 133, Fri => 116,
                  Sat => 115),
              Full_Day_Names =>
                 (Sun => 118, Mon => 123, Tue => 119,
                  Wed => 121, Thu => 122, Fri => 120,
                  Sat => 117),
              Short_Month_Names =>
                 (Jan => 130, Feb => 149, Mar => 125,
                  Apr => 109, May => 135, Jun => 127,
                  Jul => 150, Aug => 114, Sep => 148,
                  Oct => 141, Nov => 134, Dec => 139),
              Full_Month_Names =>
                 (Jan => 129, Feb => 145, Mar => 124,
                  Apr => 108, May => 131, Jun => 126,
                  Jul => 147, Aug => 113, Sep => 146,
                  Oct => 140, Nov => 132, Dec => 138),
              Day_Period_Names =>
                  (AM => 144, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 112),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 137, CE => 143),
              Date_Formats =>
                  (Full => 1019, Long => 815, Medium => 816, Short => 801),
              Time_Formats =>
                  (Full => 999, Long => 1000, Medium => 993, Short => 994),
              Date_Time_Formats =>
                  (Full => 346, Long => 346, Medium => 346, Short => 346),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 341)),

       32 => (Tag => "TR        ",
              Level => 1,
              Name => 404,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 905, Mon => 899, Tue => 895,
                  Wed => 336, Thu => 902, Fri => 1059,
                  Sat => 1061),
              Full_Day_Names =>
                 (Sun => 904, Mon => 903, Tue => 894,
                  Wed => 335, Thu => 901, Fri => 1058,
                  Sat => 1057),
              Short_Month_Names =>
                 (Jan => 913, Feb => 315, Mar => 944,
                  Apr => 920, May => 941, Jun => 980,
                  Jul => 875, Aug => 1069, Sep => 1014,
                  Oct => 1016, Nov => 962, Dec => 1073),
              Full_Month_Names =>
                 (Jan => 912, Feb => 314, Mar => 942,
                  Apr => 919, May => 940, Jun => 979,
                  Jul => 874, Aug => 1068, Sep => 1013,
                  Oct => 1015, Nov => 961, Dec => 1072),
              Day_Period_Names =>
                  (AM => 1079, Wee_Hours => 1, Early_Morning => 1,
                   Morning => 1, Late_Morning => 1, Noon => 535,
                   Midday => 1, Afternoon => 1, Evening => 1,
                   Late_Evening => 1, Night => 1, PM => 906),
              Exact_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Within_Day_Periods =>
                  (0 => AM, 1 => AM, 2 => AM,
                   3 => AM, 4 => AM, 5 => AM,
                   6 => AM, 7 => AM, 8 => AM,
                   9 => AM, 10 => AM, 11 => AM,
                   12 => PM, 13 => PM, 14 => PM,
                   15 => PM, 16 => PM, 17 => PM,
                   18 => PM, 19 => PM, 20 => PM,
                   21 => PM, 22 => PM, 23 => PM),
              Era_Names =>
                  (BCE => 928, CE => 947),
              Date_Formats =>
                  (Full => 814, Long => 815, Medium => 816, Short => 792),
              Time_Formats =>
                  (Full => 981, Long => 982, Medium => 983, Short => 984),
              Date_Time_Formats =>
                  (Full => 347, Long => 347, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1123, Group_Character => 1121,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 105,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1126, Currency => 1130)),

       33 => (Tag => "ZH        ",
              Level => 1,
              Name => 356,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 90, Mon => 96, Tue => 94,
                  Wed => 95, Thu => 91, Fri => 93,
                  Sat => 92),
              Full_Day_Names =>
                 (Sun => 79, Mon => 85, Tue => 83,
                  Wed => 84, Thu => 80, Fri => 82,
                  Sat => 81),
              Short_Month_Names =>
                 (Jan => 1108, Feb => 1105, Mar => 1102,
                  Apr => 1099, May => 1096, Jun => 1093,
                  Jul => 1090, Aug => 1087, Sep => 1084,
                  Oct => 1117, Nov => 1114, Dec => 1110),
              Full_Month_Names =>
                 (Jan => 1108, Feb => 1105, Mar => 1102,
                  Apr => 1099, May => 1096, Jun => 1093,
                  Jul => 1090, Aug => 1087, Sep => 1084,
                  Oct => 1117, Nov => 1114, Dec => 1110),
              Day_Period_Names =>
                  (AM => 104, Wee_Hours => 99, Early_Morning => 70,
                   Morning => 104, Late_Morning => 1, Noon => 102,
                   Midday => 102, Afternoon => 103, Evening => 1,
                   Late_Evening => 1, Night => 78, PM => 103),
              Exact_Day_Periods =>
                  (0 => Wee_Hours, 1 => Wee_Hours, 2 => Wee_Hours,
                   3 => Wee_Hours, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Midday, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Night, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Wee_Hours, 1 => Wee_Hours, 2 => Wee_Hours,
                   3 => Wee_Hours, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Midday, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Night, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 100, CE => 101),
              Date_Formats =>
                  (Full => 364, Long => 365, Medium => 371, Short => 374),
              Time_Formats =>
                  (Full => 351, Long => 359, Medium => 850, Short => 851),
              Date_Time_Formats =>
                  (Full => 345, Long => 345, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 976,
                   Nan_Character => 922, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 343)),

       34 => (Tag => "ZH HANT   ",
              Level => 3,
              Name => 355,
              Text_Layout => Left_To_Right,
              Short_Day_Names =>
                 (Sun => 59, Mon => 65, Tue => 63,
                  Wed => 64, Thu => 60, Fri => 62,
                  Sat => 61),
              Full_Day_Names =>
                 (Sun => 79, Mon => 85, Tue => 83,
                  Wed => 84, Thu => 80, Fri => 82,
                  Sat => 81),
              Short_Month_Names =>
                 (Jan => 1108, Feb => 1105, Mar => 1102,
                  Apr => 1099, May => 1096, Jun => 1093,
                  Jul => 1090, Aug => 1087, Sep => 1084,
                  Oct => 1117, Nov => 1114, Dec => 1110),
              Full_Month_Names =>
                 (Jan => 1108, Feb => 1105, Mar => 1102,
                  Apr => 1099, May => 1096, Jun => 1093,
                  Jul => 1090, Aug => 1087, Sep => 1084,
                  Oct => 1117, Nov => 1114, Dec => 1110),
              Day_Period_Names =>
                  (AM => 104, Wee_Hours => 99, Early_Morning => 70,
                   Morning => 104, Late_Morning => 1, Noon => 102,
                   Midday => 102, Afternoon => 103, Evening => 1,
                   Late_Evening => 1, Night => 78, PM => 103),
              Exact_Day_Periods =>
                  (0 => Wee_Hours, 1 => Wee_Hours, 2 => Wee_Hours,
                   3 => Wee_Hours, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Midday, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Night, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Within_Day_Periods =>
                  (0 => Wee_Hours, 1 => Wee_Hours, 2 => Wee_Hours,
                   3 => Wee_Hours, 4 => Early_Morning, 5 => Early_Morning,
                   6 => Morning, 7 => Morning, 8 => Morning,
                   9 => Morning, 10 => Morning, 11 => Morning,
                   12 => Midday, 13 => Afternoon, 14 => Afternoon,
                   15 => Afternoon, 16 => Afternoon, 17 => Afternoon,
                   18 => Night, 19 => Night, 20 => Night,
                   21 => Night, 22 => Night, 23 => Night),
              Era_Names =>
                  (BCE => 66, CE => 67),
              Date_Formats =>
                  (Full => 364, Long => 365, Medium => 367, Short => 375),
              Time_Formats =>
                  (Full => 350, Long => 358, Medium => 850, Short => 851),
              Date_Time_Formats =>
                  (Full => 345, Long => 345, Medium => 347, Short => 347),
              Numeric_Items =>
                  (Decimal_Point_Character => 1121, Group_Character => 1123,
                   List_Character => 1082, Zero_Character => 1120,
                   Plus_Character => 1124, Minus_Character => 1122,
                   Exponent_Character => 1033, Percent_Character => 1127,
                   Permille_Character => 107, Infinity_Character => 976,
                   Nan_Character => 56, Digit_Pattern_Character => 1134,
                   Decimal_Digits_String => 18),
              Numeric_Formats =>
                  (Decimal => 1132, Scientific => 1128,
                   Percent => 1133, Currency => 343)));

   Pool : constant Wide_String := ""
             & "൦൧൨൩൪൫൬൭൮൯༠༡༢༣༤༥༦༧༨༩꤀꤁꤂꤃꤄꤅꤆꤇꤈꤉௦௧௨௩௪௫௬௭௮௯꩐꩑꩒꩓꩔꩕꩖꩗꩘꩙᪐᪑᪒᪓᪔᪕᪖᪗᪘᪙"
             & "০১২৩৪৫৬৭৮৯᥆᥇᥈᥉᥊᥋᥌᥍᥎᥏꯰꯱꯲꯳꯴꯵꯶꯷꯸꯹೦೧೨೩೪೫೬೭೮೯꧐꧑꧒꧓꧔꧕꧖꧗꧘꧙੦੧੨੩੪੫੬੭੮੯"
             & "୦୧୨୩୪୫୬୭୮୯໐໑໒໓໔໕໖໗໘໙០១២៣៤៥៦៧៨៩᮰᮱᮲᮳᮴᮵᮶᮷᮸᮹0123456789᭐᭑᭒᭓᭔᭕᭖᭗᭘᭙"
             & "꣐꣑꣒꣓꣔꣕꣖꣗꣘꣙૦૧૨૩૪૫૬૭૮૯꘠꘡꘢꘣꘤꘥꘦꘧꘨꘩٠١٢٣٤٥٦٧٨٩౦౧౨౩౪౫౬౭౮౯๐๑๒๓๔๕๖๗๘๙"
             & "߉߈߇߆߅߄߃߂߁߀᱀᱁᱂᱃᱄᱅᱆᱇᱈᱉᧐᧑᧒᧓᧔᧕᧖᧗᧘᧙᠐᠑᠒᠓᠔᠕᠖᠗᠘᠙०१२३४५६७८९႐႑႒႓႔႕႖႗႘႙"
             & "᪀᪁᪂᪃᪄᪅᪆᪇᪈᪉۰۱۲۳۴۵۶۷۸۹᱐᱑᱒᱓᱔᱕᱖᱗᱘᱙〇一二三四五六七八九၀၁၂၃၄၅၆၇၈၉０１２３４５６７８９"
             & "화요일토요일일요일월요일오후오전수요일서기목요일기원전금요일非數值金曜日週日週四週六週五週二週三週一西元前火曜日清晨水曜"
             & "日正午木曜日月曜日晚上星期日星期四星期六星期五星期二星期三星期一日曜日土曜日周日周四周六周五周二周三周一午後午前凌晨公元"
             & "前中午下午上午∞−‰เมษายนเม.ย.อา.อ.หลังเที่ยงสิงหาคมส.ค.ศ.วันเสาร์วัน"
             & "อาทิตย์วันอังคารวันศุกร์วันพุธวันพฤหัสบดีวันจันทร์มีนาคมมี.ค"
             & ".มิถุนายนมิ.ย.มกราคมม.ค.พฤษภาคมพฤศจิกายนพฤ.พ.ย.พ.ค.ปีก่อน ค."
             & "ศ.ธันวาคมธ.ค.ตุลาคมต.ค.จ.ก่อนเที่ยงกุมภาพันธ์กันยายนกรกฎาคมก"
             & ".ย.ก.พ.ก.ค.يونيويوليوينايرنوفمبرمايومارسق.مفبرايرصسبتمبرديسم"
             & "برالسبتالخميسالجمعةالثلاثاءالاثنينالأربعاءالأحدأكتوبرأغسطسأب"
             & "ريلשבתפברוארספטמברנובמברמרץמאילפנה״צלפנה״סלסה״נינואריונייום "
             & "שנייום שלישייום שישייום שבתיום רביעייום ראשוןיום חמישייום ו׳"
             & "יום ה׳יום ד׳יום ג׳יום ב׳יום א׳יולידצמבראפרילאחה״צאוקטובראוגו"
             & "סטянваряянв.чтчетвергфевраляфевр.субботасредасентябрясент.сб"
             & "пятницаптпосле полудняпонедельникпноктябряокт.ноябрянояб.не "
             & "числон.э.маямартаиюняиюлядо полуднядо н.э.декабрядек.вторник"
             & "всвоскресеньеапреляапр.августаавг.ЯФСОНМИДАπ.μ.π.Χ.μ.μ.μ.Χ.Φ"
             & "εβρουαρίουΤριΤρίτηΤετάρτηΣεπτεμβρίουΣαβΣάββατοΠεμΠαρασκευήΠέ"
             & "μπτηΟκτωβρίουΝοεμβρίουΜαϊΜαρτίουΜαΐουΚυριακήΙουνίουΙουλίουΙα"
             & "νουαρίουΔευτέραΔεκεμβρίουΑυγούστουΑπριλίουštvrtokŞubatśrodaś"
             & "r.říjnaŘíjčtvrtekčervnačervenceČvcČerúterýúnoraî.Hr.áprilisá"
             & "pr.Úno×10^ÇarşambaÁ¤¤¤¤ #,##0.00;¤ #,##0.00-¤#,##0.00;¤-#,##"
             & "0.00¤#,##0.00;(¤#,##0.00){1}{0}{1}, {0}{1} {0}{0} {1}zářízzz"
             & "zah時mm分ss秒zzzzah时mm分ss秒zzzz h:mm:ss azondagzh_Hantzaterdagy년"
             & " M월 d일 EEEEy年M月d日EEEEyyyy/MM/ddyyyy/M/dyyyy.MM.dd.yyyy. M. d"
             & ".yyyy-MM-ddyyyy-M-dy. MMMM d., EEEEwtorekwt.wrześniawoensdag"
             & "wieczoremweeHoursw południew nocyvrijdagvormittagsvorm.viner"
             & "iviernesvenerdìvendrediven.vasárnapv. Chr.utoroktrtoukokuuta"
             & "torstainatorsdagtor.tisdagtirsdagtir.tiistainathterça-feirat"
             & "ardetammikuutasøndagsøn.söndagsâmbătăsábadoszombatszerdaszep"
             & "temberszept.syyskuutasunnuntainastředastyczniastredasrpnasoi"
             & "rsobotasob.sierpniasexta-feirasettembresetembroseptiembresep"
             & "tembrieseptembreseptembraseptembersept.sep.segunda-feirasame"
             & "disam.sabatoruright-to-leftranoquinta-feiraquarta-feirapř. n"
             & ". l.péntekpátekprzed południemprosincepred n.l.popoludníponi"
             & "edziałekpondělípondelokpon.po południupmplpiątekpiatokperjan"
             & "tainapaździernikap.n.e.p.m.outubroottobreonsdagons.októbraok"
             & "tóberoktoberokt.odp.octubreoctombrieoctobreoct.noviembrenove"
             & "mbronovembrenovembranovembernov.noonnoitenoiembrienlnightnie"
             & "dzielaniedz.nedělenedeľanbnad ranemnachtsnachmittagsnachm.n."
             & " Chr.måndagmárciusmárc.májusmájamáj.mrt.morningmorgensmiérco"
             & "lesmiercurimidimiddagmidDaymercredimercoledìmer.meio-diamayo"
             & "matinmarțimarçomarzomartsmartiemartesmartedìmarsmarraskuutam"
             & "ardimarcamar.manhãmandagman.majamaiomaggiomaartmaandagmaanan"
             & "tainamaaliskuutalørdaglør.lördaglutegoluniluneslunedìlundilu"
             & "n.lugliolokakuutalistopadulistopadalipcaleft-to-rightlednala"
             & "uantainalatnlateMorningkwietniakvětnakesäkuutakeskiviikkonak"
             & "eddjúniusjúnajún.júliusjúlajúl.juniojunhojun.juliojulhojul.j"
             & "uinjuilletjuil.juevesjoulukuutajoijeudijeu.janvierjanv.januá"
             & "rajanuarijaneirojan.jKr.iunieiun.iulieiul.ip.ianuarieian.i. "
             & "sz.i. e.hétfőhuhtikuutahelmikuutaheinäkuutah:mm:ss a zzzzh:m"
             & "m agrudniagiugnogiovedìgennaiogafévrierfévr.fredagfre.fmfife"
             & "vereirofebruárafebruariefebrerofebr.febbraiofeb.f.m.f.Kr.eve"
             & "ningepälukueneroen_ZAen_NZen_IEen_GBen_CAen_AUelokuutaearlyM"
             & "orningeKr.e.m.e.Kr.d‏/M‏/yyyydécembredéc.duminicădubnadu.dop"
             & "oludniadop.donderdagdomingodomenicadinsdagdimanchedim.diciem"
             & "bredicembredezembrodesemberdes.decembriedecembradecemberdec."
             & "de.dd‏/MM‏/yyyydd/MMM/ydd/MM/yyyydd.MM.yyyydd-MM-yydd MMMM y"
             & "dd MMM ydCd/M/yyyyd.M.yyyyd.Hr.d.C.d. MMMM yd. MMM yd. M. yy"
             & "yyd בMMMM yd בMMM yyyyd MMMM، yd MMMM, yd MMMM y 'г'.d MMMM "
             & "y EEEEd 'de' MMMM 'de' yczwartekczw.czerwcacsütörtökcccc, d."
             & " MMMM ybřeznaavrilavr.av. J.-C.augusztusaugustusaugustiaugus"
             & "taaug.arabaprílaaprès-midiaprilieaprileapr.ap. J.-C.aoûtagos"
             & "toafternoonabrilabendsaCa.m.a.C.a h시 m분 s초 zzzza h:mm:ssZářW"
             & "ednesdayViTuesdayThursdayTemmuzSâSzoSzeSundaySrpSonntagSo.Se"
             & "ptemberSaturdaySathSamstagSamhainSalıSa.RCPztProPerşembePaza"
             & "rtesiPMOktoberOctoberOcakNovemberNollaigNisanNaN（非数）MärzMárt"
             & "aMáirtMÖMontagMondayMo.MittwochMittagMi.Meán FómhairMeitheam"
             & "hMayısMartMarchMaiMSMMMM d, yMFómhM/d/yyLúnasaLuanLisLedKvěK"
             & "asımJuniJuneJulyJuliJoJanuaryIúilInfH時mm分ss秒 zzzzHaziranHH:m"
             & "m:ss zzzzHH.mm.ss zzzzHH'h'mm'min'ss's' zzzzH นาฬิกา m นาที "
             & "ss วินาที zzzzFridayFreitagFr.FebruaryFeabhraEylülEkimEanáir"
             & "EEEEที่ d MMMM G yEEEE، d MMMM، yEEEE, d. MMMM yEEEE, d בMMM"
             & "M yEEEE, d MMMM, yEEEE, d MMMM y 'г'.EEEE, d 'de' MMMM 'de' "
             & "yEEEE, MMMM d, yEEEE'en' 'den' d:'e' MMMM yEEEE dd MMMM yEEE"
             & "E d. MMMM yEEEE d MMMM yEEEE 'den' d. MMMM yDéardaoinDé hAoi"
             & "neDé SathairnDé MáirtDé LuainDé DomhnaighDé CéadaoinDubDonne"
             & "rstagDo.DienstagDi.DezemberDeireadh FómhairDecemberDFómhCuma"
             & "rtesiCsCmtCEBřeBealtaineBCEAğustosAugustAralıkAprilAibreánAM"
             & "AD9월9月8월8月7월7月6월6月5월5月4월4月3월3月2월2月1월1月12월12月12 uur 's middag"
             & "s11월11月10월10月+'kl'. HH:mm:ss zzzz%#,##0#E0#,##0 %#,##0.00 ¤#"
             & ",##0.###;#,##0.###-#,##0%"
             & "";

   String_Addresses : constant String_Addresses_Type := (
      --  1: ""
      1 => (First => 1, Last => 0),
      --  2: "൦൧൨൩൪൫൬൭൮൯"
      2 => (First => 1, Last => 10),
      --  3: "༠༡༢༣༤༥༦༧༨༩"
      3 => (First => 11, Last => 20),
      --  4: "꤀꤁꤂꤃꤄꤅꤆꤇꤈꤉"
      4 => (First => 21, Last => 30),
      --  5: "௦௧௨௩௪௫௬௭௮௯"
      5 => (First => 31, Last => 40),
      --  6: "꩐꩑꩒꩓꩔꩕꩖꩗꩘꩙"
      6 => (First => 41, Last => 50),
      --  7: "᪐᪑᪒᪓᪔᪕᪖᪗᪘᪙"
      7 => (First => 51, Last => 60),
      --  8: "০১২৩৪৫৬৭৮৯"
      8 => (First => 61, Last => 70),
      --  9: "᥆᥇᥈᥉᥊᥋᥌᥍᥎᥏"
      9 => (First => 71, Last => 80),
      --  10: "꯰꯱꯲꯳꯴꯵꯶꯷꯸꯹"
      10 => (First => 81, Last => 90),
      --  11: "೦೧೨೩೪೫೬೭೮೯"
      11 => (First => 91, Last => 100),
      --  12: "꧐꧑꧒꧓꧔꧕꧖꧗꧘꧙"
      12 => (First => 101, Last => 110),
      --  13: "੦੧੨੩੪੫੬੭੮੯"
      13 => (First => 111, Last => 120),
      --  14: "୦୧୨୩୪୫୬୭୮୯"
      14 => (First => 121, Last => 130),
      --  15: "໐໑໒໓໔໕໖໗໘໙"
      15 => (First => 131, Last => 140),
      --  16: "០១២៣៤៥៦៧៨៩"
      16 => (First => 141, Last => 150),
      --  17: "᮰᮱᮲᮳᮴᮵᮶᮷᮸᮹"
      17 => (First => 151, Last => 160),
      --  18: "0123456789"
      18 => (First => 161, Last => 170),
      --  19: "᭐᭑᭒᭓᭔᭕᭖᭗᭘᭙"
      19 => (First => 171, Last => 180),
      --  20: "꣐꣑꣒꣓꣔꣕꣖꣗꣘꣙"
      20 => (First => 181, Last => 190),
      --  21: "૦૧૨૩૪૫૬૭૮૯"
      21 => (First => 191, Last => 200),
      --  22: "꘠꘡꘢꘣꘤꘥꘦꘧꘨꘩"
      22 => (First => 201, Last => 210),
      --  23: "٠١٢٣٤٥٦٧٨٩"
      23 => (First => 211, Last => 220),
      --  24: "౦౧౨౩౪౫౬౭౮౯"
      24 => (First => 221, Last => 230),
      --  25: "๐๑๒๓๔๕๖๗๘๙"
      25 => (First => 231, Last => 240),
      --  26: "߉߈߇߆߅߄߃߂߁߀"
      26 => (First => 241, Last => 250),
      --  27: "᱀᱁᱂᱃᱄᱅᱆᱇᱈᱉"
      27 => (First => 251, Last => 260),
      --  28: "᧐᧑᧒᧓᧔᧕᧖᧗᧘᧙"
      28 => (First => 261, Last => 270),
      --  29: "᠐᠑᠒᠓᠔᠕᠖᠗᠘᠙"
      29 => (First => 271, Last => 280),
      --  30: "०१२३४५६७८९"
      30 => (First => 281, Last => 290),
      --  31: "႐႑႒႓႔႕႖႗႘႙"
      31 => (First => 291, Last => 300),
      --  32: "᪀᪁᪂᪃᪄᪅᪆᪇᪈᪉"
      32 => (First => 301, Last => 310),
      --  33: "۰۱۲۳۴۵۶۷۸۹"
      33 => (First => 311, Last => 320),
      --  34: "᱐᱑᱒᱓᱔᱕᱖᱗᱘᱙"
      34 => (First => 321, Last => 330),
      --  35: "〇一二三四五六七八九"
      35 => (First => 331, Last => 340),
      --  36: "၀၁၂၃၄၅၆၇၈၉"
      36 => (First => 341, Last => 350),
      --  37: "０１２３４５６７８９"
      37 => (First => 351, Last => 360),
      --  38: "화요일"
      38 => (First => 361, Last => 363),
      --  39: "화"
      39 => (First => 361, Last => 361),
      --  40: "토요일"
      40 => (First => 364, Last => 366),
      --  41: "토"
      41 => (First => 364, Last => 364),
      --  42: "일요일"
      42 => (First => 367, Last => 369),
      --  43: "일"
      43 => (First => 363, Last => 363),
      --  44: "월요일"
      44 => (First => 370, Last => 372),
      --  45: "월"
      45 => (First => 370, Last => 370),
      --  46: "오후"
      46 => (First => 373, Last => 374),
      --  47: "오전"
      47 => (First => 375, Last => 376),
      --  48: "수요일"
      48 => (First => 377, Last => 379),
      --  49: "수"
      49 => (First => 377, Last => 377),
      --  50: "서기"
      50 => (First => 380, Last => 381),
      --  51: "목요일"
      51 => (First => 382, Last => 384),
      --  52: "목"
      52 => (First => 382, Last => 382),
      --  53: "기원전"
      53 => (First => 385, Last => 387),
      --  54: "금요일"
      54 => (First => 388, Last => 390),
      --  55: "금"
      55 => (First => 388, Last => 388),
      --  56: "非數值"
      56 => (First => 391, Last => 393),
      --  57: "金曜日"
      57 => (First => 394, Last => 396),
      --  58: "金"
      58 => (First => 394, Last => 394),
      --  59: "週日"
      59 => (First => 397, Last => 398),
      --  60: "週四"
      60 => (First => 399, Last => 400),
      --  61: "週六"
      61 => (First => 401, Last => 402),
      --  62: "週五"
      62 => (First => 403, Last => 404),
      --  63: "週二"
      63 => (First => 405, Last => 406),
      --  64: "週三"
      64 => (First => 407, Last => 408),
      --  65: "週一"
      65 => (First => 409, Last => 410),
      --  66: "西元前"
      66 => (First => 411, Last => 413),
      --  67: "西元"
      67 => (First => 411, Last => 412),
      --  68: "火曜日"
      68 => (First => 414, Last => 416),
      --  69: "火"
      69 => (First => 414, Last => 414),
      --  70: "清晨"
      70 => (First => 417, Last => 418),
      --  71: "水曜日"
      71 => (First => 419, Last => 421),
      --  72: "水"
      72 => (First => 419, Last => 419),
      --  73: "正午"
      73 => (First => 422, Last => 423),
      --  74: "木曜日"
      74 => (First => 424, Last => 426),
      --  75: "木"
      75 => (First => 424, Last => 424),
      --  76: "月曜日"
      76 => (First => 427, Last => 429),
      --  77: "月"
      77 => (First => 427, Last => 427),
      --  78: "晚上"
      78 => (First => 430, Last => 431),
      --  79: "星期日"
      79 => (First => 432, Last => 434),
      --  80: "星期四"
      80 => (First => 435, Last => 437),
      --  81: "星期六"
      81 => (First => 438, Last => 440),
      --  82: "星期五"
      82 => (First => 441, Last => 443),
      --  83: "星期二"
      83 => (First => 444, Last => 446),
      --  84: "星期三"
      84 => (First => 447, Last => 449),
      --  85: "星期一"
      85 => (First => 450, Last => 452),
      --  86: "日曜日"
      86 => (First => 453, Last => 455),
      --  87: "日"
      87 => (First => 396, Last => 396),
      --  88: "土曜日"
      88 => (First => 456, Last => 458),
      --  89: "土"
      89 => (First => 456, Last => 456),
      --  90: "周日"
      90 => (First => 459, Last => 460),
      --  91: "周四"
      91 => (First => 461, Last => 462),
      --  92: "周六"
      92 => (First => 463, Last => 464),
      --  93: "周五"
      93 => (First => 465, Last => 466),
      --  94: "周二"
      94 => (First => 467, Last => 468),
      --  95: "周三"
      95 => (First => 469, Last => 470),
      --  96: "周一"
      96 => (First => 471, Last => 472),
      --  97: "午後"
      97 => (First => 473, Last => 474),
      --  98: "午前"
      98 => (First => 475, Last => 476),
      --  99: "凌晨"
      99 => (First => 477, Last => 478),
      --  100: "公元前"
      100 => (First => 479, Last => 481),
      --  101: "公元"
      101 => (First => 479, Last => 480),
      --  102: "中午"
      102 => (First => 482, Last => 483),
      --  103: "下午"
      103 => (First => 484, Last => 485),
      --  104: "上午"
      104 => (First => 486, Last => 487),
      --  105: "∞"
      105 => (First => 488, Last => 488),
      --  106: "−"
      106 => (First => 489, Last => 489),
      --  107: "‰"
      107 => (First => 490, Last => 490),
      --  108: "เมษายน"
      108 => (First => 491, Last => 496),
      --  109: "เม.ย."
      109 => (First => 497, Last => 501),
      --  110: "อา."
      110 => (First => 502, Last => 504),
      --  111: "อ."
      111 => (First => 505, Last => 506),
      --  112: "หลังเที่ยง"
      112 => (First => 507, Last => 516),
      --  113: "สิงหาคม"
      113 => (First => 517, Last => 523),
      --  114: "ส.ค."
      114 => (First => 524, Last => 527),
      --  115: "ส."
      115 => (First => 524, Last => 525),
      --  116: "ศ."
      116 => (First => 528, Last => 529),
      --  117: "วันเสาร์"
      117 => (First => 530, Last => 537),
      --  118: "วันอาทิตย์"
      118 => (First => 538, Last => 547),
      --  119: "วันอังคาร"
      119 => (First => 548, Last => 556),
      --  120: "วันศุกร์"
      120 => (First => 557, Last => 564),
      --  121: "วันพุธ"
      121 => (First => 565, Last => 570),
      --  122: "วันพฤหัสบดี"
      122 => (First => 571, Last => 581),
      --  123: "วันจันทร์"
      123 => (First => 582, Last => 590),
      --  124: "มีนาคม"
      124 => (First => 591, Last => 596),
      --  125: "มี.ค."
      125 => (First => 597, Last => 601),
      --  126: "มิถุนายน"
      126 => (First => 602, Last => 609),
      --  127: "มิ.ย."
      127 => (First => 610, Last => 614),
      --  128: "มิ.ย"
      128 => (First => 610, Last => 613),
      --  129: "มกราคม"
      129 => (First => 615, Last => 620),
      --  130: "ม.ค."
      130 => (First => 621, Last => 624),
      --  131: "พฤษภาคม"
      131 => (First => 625, Last => 631),
      --  132: "พฤศจิกายน"
      132 => (First => 632, Last => 640),
      --  133: "พฤ."
      133 => (First => 641, Last => 643),
      --  134: "พ.ย."
      134 => (First => 644, Last => 647),
      --  135: "พ.ค."
      135 => (First => 648, Last => 651),
      --  136: "พ."
      136 => (First => 644, Last => 645),
      --  137: "ปีก่อน ค.ศ."
      137 => (First => 652, Last => 662),
      --  138: "ธันวาคม"
      138 => (First => 663, Last => 669),
      --  139: "ธ.ค."
      139 => (First => 670, Last => 673),
      --  140: "ตุลาคม"
      140 => (First => 674, Last => 679),
      --  141: "ต.ค."
      141 => (First => 680, Last => 683),
      --  142: "จ."
      142 => (First => 684, Last => 685),
      --  143: "ค.ศ."
      143 => (First => 526, Last => 529),
      --  144: "ก่อนเที่ยง"
      144 => (First => 686, Last => 695),
      --  145: "กุมภาพันธ์"
      145 => (First => 696, Last => 705),
      --  146: "กันยายน"
      146 => (First => 706, Last => 712),
      --  147: "กรกฎาคม"
      147 => (First => 713, Last => 719),
      --  148: "ก.ย."
      148 => (First => 720, Last => 723),
      --  149: "ก.พ."
      149 => (First => 724, Last => 727),
      --  150: "ก.ค."
      150 => (First => 728, Last => 731),
      --  151: "٠"
      151 => (First => 211, Last => 211),
      --  152: "يونيو"
      152 => (First => 732, Last => 736),
      --  153: "يوليو"
      153 => (First => 737, Last => 741),
      --  154: "يناير"
      154 => (First => 742, Last => 746),
      --  155: "نوفمبر"
      155 => (First => 747, Last => 752),
      --  156: "مايو"
      156 => (First => 753, Last => 756),
      --  157: "مارس"
      157 => (First => 757, Last => 760),
      --  158: "م"
      158 => (First => 750, Last => 750),
      --  159: "ق.م"
      159 => (First => 761, Last => 763),
      --  160: "فبراير"
      160 => (First => 764, Last => 769),
      --  161: "ص"
      161 => (First => 770, Last => 770),
      --  162: "سبتمبر"
      162 => (First => 771, Last => 776),
      --  163: "ديسمبر"
      163 => (First => 777, Last => 782),
      --  164: "السبت"
      164 => (First => 783, Last => 787),
      --  165: "الخميس"
      165 => (First => 788, Last => 793),
      --  166: "الجمعة"
      166 => (First => 794, Last => 799),
      --  167: "الثلاثاء"
      167 => (First => 800, Last => 807),
      --  168: "الاثنين"
      168 => (First => 808, Last => 814),
      --  169: "الأربعاء"
      169 => (First => 815, Last => 822),
      --  170: "الأحد"
      170 => (First => 823, Last => 827),
      --  171: "أكتوبر"
      171 => (First => 828, Last => 833),
      --  172: "أغسطس"
      172 => (First => 834, Last => 838),
      --  173: "أبريل"
      173 => (First => 839, Last => 843),
      --  174: "שבת"
      174 => (First => 844, Last => 846),
      --  175: "פברואר"
      175 => (First => 847, Last => 852),
      --  176: "פבר"
      176 => (First => 847, Last => 849),
      --  177: "ספטמבר"
      177 => (First => 853, Last => 858),
      --  178: "ספט"
      178 => (First => 853, Last => 855),
      --  179: "נובמבר"
      179 => (First => 859, Last => 864),
      --  180: "נוב"
      180 => (First => 859, Last => 861),
      --  181: "מרץ"
      181 => (First => 865, Last => 867),
      --  182: "מאי"
      182 => (First => 868, Last => 870),
      --  183: "לפנה״צ"
      183 => (First => 871, Last => 876),
      --  184: "לפנה״ס"
      184 => (First => 877, Last => 882),
      --  185: "לסה״נ"
      185 => (First => 883, Last => 887),
      --  186: "ינואר"
      186 => (First => 888, Last => 892),
      --  187: "ינו"
      187 => (First => 888, Last => 890),
      --  188: "יוני"
      188 => (First => 893, Last => 896),
      --  189: "יונ"
      189 => (First => 893, Last => 895),
      --  190: "יום שני"
      190 => (First => 897, Last => 903),
      --  191: "יום שלישי"
      191 => (First => 904, Last => 912),
      --  192: "יום שישי"
      192 => (First => 913, Last => 920),
      --  193: "יום שבת"
      193 => (First => 921, Last => 927),
      --  194: "יום רביעי"
      194 => (First => 928, Last => 936),
      --  195: "יום ראשון"
      195 => (First => 937, Last => 945),
      --  196: "יום חמישי"
      196 => (First => 946, Last => 954),
      --  197: "יום ו׳"
      197 => (First => 955, Last => 960),
      --  198: "יום ה׳"
      198 => (First => 961, Last => 966),
      --  199: "יום ד׳"
      199 => (First => 967, Last => 972),
      --  200: "יום ג׳"
      200 => (First => 973, Last => 978),
      --  201: "יום ב׳"
      201 => (First => 979, Last => 984),
      --  202: "יום א׳"
      202 => (First => 985, Last => 990),
      --  203: "יולי"
      203 => (First => 991, Last => 994),
      --  204: "יול"
      204 => (First => 991, Last => 993),
      --  205: "דצמבר"
      205 => (First => 995, Last => 999),
      --  206: "דצמ"
      206 => (First => 995, Last => 997),
      --  207: "אפריל"
      207 => (First => 1000, Last => 1004),
      --  208: "אפר"
      208 => (First => 1000, Last => 1002),
      --  209: "אחה״צ"
      209 => (First => 1005, Last => 1009),
      --  210: "אוקטובר"
      210 => (First => 1010, Last => 1016),
      --  211: "אוק"
      211 => (First => 1010, Last => 1012),
      --  212: "אוגוסט"
      212 => (First => 1017, Last => 1022),
      --  213: "אוג"
      213 => (First => 1017, Last => 1019),
      --  214: "января"
      214 => (First => 1023, Last => 1028),
      --  215: "янв."
      215 => (First => 1029, Last => 1032),
      --  216: "чт"
      216 => (First => 1033, Last => 1034),
      --  217: "четверг"
      217 => (First => 1035, Last => 1041),
      --  218: "февраля"
      218 => (First => 1042, Last => 1048),
      --  219: "февр."
      219 => (First => 1049, Last => 1053),
      --  220: "суббота"
      220 => (First => 1054, Last => 1060),
      --  221: "среда"
      221 => (First => 1061, Last => 1065),
      --  222: "ср"
      222 => (First => 1061, Last => 1062),
      --  223: "сентября"
      223 => (First => 1066, Last => 1073),
      --  224: "сент."
      224 => (First => 1074, Last => 1078),
      --  225: "сб"
      225 => (First => 1079, Last => 1080),
      --  226: "пятница"
      226 => (First => 1081, Last => 1087),
      --  227: "пт"
      227 => (First => 1088, Last => 1089),
      --  228: "после полудня"
      228 => (First => 1090, Last => 1102),
      --  229: "понедельник"
      229 => (First => 1103, Last => 1113),
      --  230: "пн"
      230 => (First => 1114, Last => 1115),
      --  231: "октября"
      231 => (First => 1116, Last => 1122),
      --  232: "окт."
      232 => (First => 1123, Last => 1126),
      --  233: "ноября"
      233 => (First => 1127, Last => 1132),
      --  234: "нояб."
      234 => (First => 1133, Last => 1137),
      --  235: "не число"
      235 => (First => 1138, Last => 1145),
      --  236: "н.э."
      236 => (First => 1146, Last => 1149),
      --  237: "мая"
      237 => (First => 1150, Last => 1152),
      --  238: "марта"
      238 => (First => 1153, Last => 1157),
      --  239: "июня"
      239 => (First => 1158, Last => 1161),
      --  240: "июля"
      240 => (First => 1162, Last => 1165),
      --  241: "до полудня"
      241 => (First => 1166, Last => 1175),
      --  242: "до н.э."
      242 => (First => 1176, Last => 1182),
      --  243: "декабря"
      243 => (First => 1183, Last => 1189),
      --  244: "дек."
      244 => (First => 1190, Last => 1193),
      --  245: "вторник"
      245 => (First => 1194, Last => 1200),
      --  246: "вт"
      246 => (First => 1194, Last => 1195),
      --  247: "вс"
      247 => (First => 1201, Last => 1202),
      --  248: "воскресенье"
      248 => (First => 1203, Last => 1213),
      --  249: "апреля"
      249 => (First => 1214, Last => 1219),
      --  250: "апр."
      250 => (First => 1220, Last => 1223),
      --  251: "августа"
      251 => (First => 1224, Last => 1230),
      --  252: "авг."
      252 => (First => 1231, Last => 1234),
      --  253: "Я"
      253 => (First => 1235, Last => 1235),
      --  254: "Ф"
      254 => (First => 1236, Last => 1236),
      --  255: "С"
      255 => (First => 1237, Last => 1237),
      --  256: "О"
      256 => (First => 1238, Last => 1238),
      --  257: "Н"
      257 => (First => 1239, Last => 1239),
      --  258: "М"
      258 => (First => 1240, Last => 1240),
      --  259: "И"
      259 => (First => 1241, Last => 1241),
      --  260: "Д"
      260 => (First => 1242, Last => 1242),
      --  261: "А"
      261 => (First => 1243, Last => 1243),
      --  262: "π.μ."
      262 => (First => 1244, Last => 1247),
      --  263: "π.Χ."
      263 => (First => 1248, Last => 1251),
      --  264: "μ.μ."
      264 => (First => 1252, Last => 1255),
      --  265: "μ.Χ."
      265 => (First => 1256, Last => 1259),
      --  266: "Φεβρουαρίου"
      266 => (First => 1260, Last => 1270),
      --  267: "Φεβ"
      267 => (First => 1260, Last => 1262),
      --  268: "Φ"
      268 => (First => 1260, Last => 1260),
      --  269: "Τρι"
      269 => (First => 1271, Last => 1273),
      --  270: "Τρίτη"
      270 => (First => 1274, Last => 1278),
      --  271: "Τετάρτη"
      271 => (First => 1279, Last => 1285),
      --  272: "Τετ"
      272 => (First => 1279, Last => 1281),
      --  273: "Σεπτεμβρίου"
      273 => (First => 1286, Last => 1296),
      --  274: "Σεπ"
      274 => (First => 1286, Last => 1288),
      --  275: "Σαβ"
      275 => (First => 1297, Last => 1299),
      --  276: "Σάββατο"
      276 => (First => 1300, Last => 1306),
      --  277: "Σ"
      277 => (First => 1286, Last => 1286),
      --  278: "Πεμ"
      278 => (First => 1307, Last => 1309),
      --  279: "Παρασκευή"
      279 => (First => 1310, Last => 1318),
      --  280: "Παρ"
      280 => (First => 1310, Last => 1312),
      --  281: "Πέμπτη"
      281 => (First => 1319, Last => 1324),
      --  282: "Οκτωβρίου"
      282 => (First => 1325, Last => 1333),
      --  283: "Οκτ"
      283 => (First => 1325, Last => 1327),
      --  284: "Ο"
      284 => (First => 1325, Last => 1325),
      --  285: "Νοεμβρίου"
      285 => (First => 1334, Last => 1342),
      --  286: "Νοε"
      286 => (First => 1334, Last => 1336),
      --  287: "Ν"
      287 => (First => 1334, Last => 1334),
      --  288: "Μαϊ"
      288 => (First => 1343, Last => 1345),
      --  289: "Μαρτίου"
      289 => (First => 1346, Last => 1352),
      --  290: "Μαρ"
      290 => (First => 1346, Last => 1348),
      --  291: "Μαΐου"
      291 => (First => 1353, Last => 1357),
      --  292: "Μ"
      292 => (First => 1343, Last => 1343),
      --  293: "Κυριακή"
      293 => (First => 1358, Last => 1364),
      --  294: "Κυρ"
      294 => (First => 1358, Last => 1360),
      --  295: "Ιουνίου"
      295 => (First => 1365, Last => 1371),
      --  296: "Ιουν"
      296 => (First => 1365, Last => 1368),
      --  297: "Ιουλίου"
      297 => (First => 1372, Last => 1378),
      --  298: "Ιουλ"
      298 => (First => 1372, Last => 1375),
      --  299: "Ιανουαρίου"
      299 => (First => 1379, Last => 1388),
      --  300: "Ιαν"
      300 => (First => 1379, Last => 1381),
      --  301: "Ι"
      301 => (First => 1365, Last => 1365),
      --  302: "Δευτέρα"
      302 => (First => 1389, Last => 1395),
      --  303: "Δευ"
      303 => (First => 1389, Last => 1391),
      --  304: "Δεκεμβρίου"
      304 => (First => 1396, Last => 1405),
      --  305: "Δεκ"
      305 => (First => 1396, Last => 1398),
      --  306: "Δ"
      306 => (First => 1389, Last => 1389),
      --  307: "Αυγούστου"
      307 => (First => 1406, Last => 1414),
      --  308: "Αυγ"
      308 => (First => 1406, Last => 1408),
      --  309: "Απριλίου"
      309 => (First => 1415, Last => 1422),
      --  310: "Απρ"
      310 => (First => 1415, Last => 1417),
      --  311: "Α"
      311 => (First => 1406, Last => 1406),
      --  312: "štvrtok"
      312 => (First => 1423, Last => 1429),
      --  313: "št"
      313 => (First => 1423, Last => 1424),
      --  314: "Şubat"
      314 => (First => 1430, Last => 1434),
      --  315: "Şub"
      315 => (First => 1430, Last => 1432),
      --  316: "Ş"
      316 => (First => 1430, Last => 1430),
      --  317: "środa"
      317 => (First => 1435, Last => 1439),
      --  318: "śr."
      318 => (First => 1440, Last => 1442),
      --  319: "října"
      319 => (First => 1443, Last => 1447),
      --  320: "Říj"
      320 => (First => 1448, Last => 1450),
      --  321: "čtvrtek"
      321 => (First => 1451, Last => 1457),
      --  322: "čt"
      322 => (First => 1451, Last => 1452),
      --  323: "června"
      323 => (First => 1458, Last => 1463),
      --  324: "července"
      324 => (First => 1464, Last => 1471),
      --  325: "Čvc"
      325 => (First => 1472, Last => 1474),
      --  326: "Čer"
      326 => (First => 1475, Last => 1477),
      --  327: "úterý"
      327 => (First => 1478, Last => 1482),
      --  328: "út"
      328 => (First => 1478, Last => 1479),
      --  329: "února"
      329 => (First => 1483, Last => 1487),
      --  330: "î.Hr."
      330 => (First => 1488, Last => 1492),
      --  331: "április"
      331 => (First => 1493, Last => 1499),
      --  332: "ápr."
      332 => (First => 1500, Last => 1503),
      --  333: "Úno"
      333 => (First => 1504, Last => 1506),
      --  334: "×10^"
      334 => (First => 1507, Last => 1510),
      --  335: "Çarşamba"
      335 => (First => 1511, Last => 1518),
      --  336: "Çar"
      336 => (First => 1511, Last => 1513),
      --  337: "Á"
      337 => (First => 1519, Last => 1519),
      --  338: "¤¤¤"
      338 => (First => 1520, Last => 1522),
      --  339: "¤ #,##0.00;¤ #,##0.00-"
      339 => (First => 1523, Last => 1544),
      --  340: "¤ #,##0.00"
      340 => (First => 1523, Last => 1532),
      --  341: "¤#,##0.00;¤-#,##0.00"
      341 => (First => 1545, Last => 1564),
      --  342: "¤#,##0.00;(¤#,##0.00)"
      342 => (First => 1565, Last => 1585),
      --  343: "¤#,##0.00"
      343 => (First => 1545, Last => 1553),
      --  344: " "
      344 => (First => 1524, Last => 1524),
      --  345: "{1}{0}"
      345 => (First => 1586, Last => 1591),
      --  346: "{1}, {0}"
      346 => (First => 1592, Last => 1599),
      --  347: "{1} {0}"
      347 => (First => 1600, Last => 1606),
      --  348: "{0} {1}"
      348 => (First => 1607, Last => 1613),
      --  349: "září"
      349 => (First => 1614, Last => 1617),
      --  350: "zzzzah時mm分ss秒"
      350 => (First => 1618, Last => 1630),
      --  351: "zzzzah时mm分ss秒"
      351 => (First => 1631, Last => 1643),
      --  352: "zzzz h:mm:ss a"
      352 => (First => 1644, Last => 1657),
      --  353: "zondag"
      353 => (First => 1658, Last => 1663),
      --  354: "zo"
      354 => (First => 1658, Last => 1659),
      --  355: "zh_Hant"
      355 => (First => 1664, Last => 1670),
      --  356: "zh"
      356 => (First => 1664, Last => 1665),
      --  357: "zaterdag"
      357 => (First => 1671, Last => 1678),
      --  358: "zah時mm分ss秒"
      358 => (First => 1621, Last => 1630),
      --  359: "zah时mm分ss秒"
      359 => (First => 1634, Last => 1643),
      --  360: "za"
      360 => (First => 1621, Last => 1622),
      --  361: "z h:mm:ss a"
      361 => (First => 1647, Last => 1657),
      --  362: "y년 M월 d일 EEEE"
      362 => (First => 1679, Last => 1691),
      --  363: "y년 M월 d일"
      363 => (First => 1679, Last => 1686),
      --  364: "y年M月d日EEEE"
      364 => (First => 1692, Last => 1701),
      --  365: "y年M月d日"
      365 => (First => 1692, Last => 1697),
      --  366: "yyyy/MM/dd"
      366 => (First => 1702, Last => 1711),
      --  367: "yyyy/M/d"
      367 => (First => 1712, Last => 1719),
      --  368: "yyyy.MM.dd."
      368 => (First => 1720, Last => 1730),
      --  369: "yyyy. M. d."
      369 => (First => 1731, Last => 1741),
      --  370: "yyyy-MM-dd"
      370 => (First => 1742, Last => 1751),
      --  371: "yyyy-M-d"
      371 => (First => 1752, Last => 1759),
      --  372: "yy. M. d."
      372 => (First => 1733, Last => 1741),
      --  373: "yy-MM-dd"
      373 => (First => 1744, Last => 1751),
      --  374: "yy-M-d"
      374 => (First => 1754, Last => 1759),
      --  375: "y/M/d"
      375 => (First => 1715, Last => 1719),
      --  376: "y. MMMM d., EEEE"
      376 => (First => 1760, Last => 1775),
      --  377: "y. MMMM d."
      377 => (First => 1760, Last => 1769),
      --  378: "wtorek"
      378 => (First => 1776, Last => 1781),
      --  379: "wt."
      379 => (First => 1782, Last => 1784),
      --  380: "września"
      380 => (First => 1785, Last => 1792),
      --  381: "wrz"
      381 => (First => 1785, Last => 1787),
      --  382: "woensdag"
      382 => (First => 1793, Last => 1800),
      --  383: "wo"
      383 => (First => 1793, Last => 1794),
      --  384: "wieczorem"
      384 => (First => 1801, Last => 1809),
      --  385: "weeHours"
      385 => (First => 1810, Last => 1817),
      --  386: "w południe"
      386 => (First => 1818, Last => 1827),
      --  387: "w nocy"
      387 => (First => 1828, Last => 1833),
      --  388: "w"
      388 => (First => 1776, Last => 1776),
      --  389: "vrijdag"
      389 => (First => 1834, Last => 1840),
      --  390: "vr"
      390 => (First => 1425, Last => 1426),
      --  391: "vormittags"
      391 => (First => 1841, Last => 1850),
      --  392: "vorm."
      392 => (First => 1851, Last => 1855),
      --  393: "vineri"
      393 => (First => 1856, Last => 1861),
      --  394: "viernes"
      394 => (First => 1862, Last => 1868),
      --  395: "vie"
      395 => (First => 1862, Last => 1864),
      --  396: "venerdì"
      396 => (First => 1869, Last => 1875),
      --  397: "vendredi"
      397 => (First => 1876, Last => 1883),
      --  398: "ven."
      398 => (First => 1884, Last => 1887),
      --  399: "ven"
      399 => (First => 1467, Last => 1469),
      --  400: "vasárnap"
      400 => (First => 1888, Last => 1895),
      --  401: "v. Chr."
      401 => (First => 1896, Last => 1902),
      --  402: "utorok"
      402 => (First => 1903, Last => 1908),
      --  403: "ut"
      403 => (First => 1903, Last => 1904),
      --  404: "tr"
      404 => (First => 1909, Last => 1910),
      --  405: "toukokuuta"
      405 => (First => 1911, Last => 1920),
      --  406: "torstaina"
      406 => (First => 1921, Last => 1929),
      --  407: "torsdag"
      407 => (First => 1930, Last => 1936),
      --  408: "tors"
      408 => (First => 1921, Last => 1924),
      --  409: "tor."
      409 => (First => 1937, Last => 1940),
      --  410: "tor"
      410 => (First => 1777, Last => 1779),
      --  411: "to"
      411 => (First => 1427, Last => 1428),
      --  412: "tisdag"
      412 => (First => 1941, Last => 1946),
      --  413: "tis"
      413 => (First => 1941, Last => 1943),
      --  414: "tirsdag"
      414 => (First => 1947, Last => 1953),
      --  415: "tir."
      415 => (First => 1954, Last => 1957),
      --  416: "tir"
      416 => (First => 1947, Last => 1949),
      --  417: "tiistaina"
      417 => (First => 1958, Last => 1966),
      --  418: "ti"
      418 => (First => 1941, Last => 1942),
      --  419: "th"
      419 => (First => 1967, Last => 1968),
      --  420: "terça-feira"
      420 => (First => 1969, Last => 1979),
      --  421: "ter"
      421 => (First => 1479, Last => 1481),
      --  422: "tarde"
      422 => (First => 1980, Last => 1984),
      --  423: "tammikuuta"
      423 => (First => 1985, Last => 1994),
      --  424: "søndag"
      424 => (First => 1995, Last => 2000),
      --  425: "søn."
      425 => (First => 2001, Last => 2004),
      --  426: "søn"
      426 => (First => 1995, Last => 1997),
      --  427: "söndag"
      427 => (First => 2005, Last => 2010),
      --  428: "sön"
      428 => (First => 2005, Last => 2007),
      --  429: "sâmbătă"
      429 => (First => 2011, Last => 2017),
      --  430: "sábado"
      430 => (First => 2018, Last => 2023),
      --  431: "sáb"
      431 => (First => 2018, Last => 2020),
      --  432: "szombat"
      432 => (First => 2024, Last => 2030),
      --  433: "szerda"
      433 => (First => 2031, Last => 2036),
      --  434: "szeptember"
      434 => (First => 2037, Last => 2046),
      --  435: "szept."
      435 => (First => 2047, Last => 2052),
      --  436: "syyskuuta"
      436 => (First => 2053, Last => 2061),
      --  437: "sv"
      437 => (First => 1850, Last => 1851),
      --  438: "sunnuntaina"
      438 => (First => 2062, Last => 2072),
      --  439: "su"
      439 => (First => 2062, Last => 2063),
      --  440: "středa"
      440 => (First => 2073, Last => 2078),
      --  441: "stycznia"
      441 => (First => 2079, Last => 2086),
      --  442: "sty"
      442 => (First => 2079, Last => 2081),
      --  443: "streda"
      443 => (First => 2087, Last => 2092),
      --  444: "st"
      444 => (First => 1924, Last => 1925),
      --  445: "srpna"
      445 => (First => 2093, Last => 2097),
      --  446: "soir"
      446 => (First => 2098, Last => 2101),
      --  447: "sobota"
      447 => (First => 2102, Last => 2107),
      --  448: "sob."
      448 => (First => 2108, Last => 2111),
      --  449: "so"
      449 => (First => 2098, Last => 2099),
      --  450: "sk"
      450 => (First => 2056, Last => 2057),
      --  451: "sierpnia"
      451 => (First => 2112, Last => 2119),
      --  452: "sie"
      452 => (First => 2112, Last => 2114),
      --  453: "sexta-feira"
      453 => (First => 2120, Last => 2130),
      --  454: "sex"
      454 => (First => 2120, Last => 2122),
      --  455: "settembre"
      455 => (First => 2131, Last => 2139),
      --  456: "setembro"
      456 => (First => 2140, Last => 2147),
      --  457: "set"
      457 => (First => 2131, Last => 2133),
      --  458: "septiembre"
      458 => (First => 2148, Last => 2157),
      --  459: "septembrie"
      459 => (First => 2158, Last => 2167),
      --  460: "septembre"
      460 => (First => 2168, Last => 2176),
      --  461: "septembra"
      461 => (First => 2177, Last => 2185),
      --  462: "september"
      462 => (First => 2186, Last => 2194),
      --  463: "sept."
      463 => (First => 2195, Last => 2199),
      --  464: "sep."
      464 => (First => 2200, Last => 2203),
      --  465: "sep"
      465 => (First => 2148, Last => 2150),
      --  466: "segunda-feira"
      466 => (First => 2204, Last => 2216),
      --  467: "seg"
      467 => (First => 2204, Last => 2206),
      --  468: "samedi"
      468 => (First => 2217, Last => 2222),
      --  469: "sam."
      469 => (First => 2223, Last => 2226),
      --  470: "sabato"
      470 => (First => 2227, Last => 2232),
      --  471: "sab"
      471 => (First => 2227, Last => 2229),
      --  472: "s"
      472 => (First => 1499, Last => 1499),
      --  473: "ru"
      473 => (First => 2233, Last => 2234),
      --  474: "ro"
      474 => (First => 1436, Last => 1437),
      --  475: "right-to-left"
      475 => (First => 2235, Last => 2247),
      --  476: "rano"
      476 => (First => 2248, Last => 2251),
      --  477: "quinta-feira"
      477 => (First => 2252, Last => 2263),
      --  478: "qui"
      478 => (First => 2252, Last => 2254),
      --  479: "quarta-feira"
      479 => (First => 2264, Last => 2275),
      --  480: "qua"
      480 => (First => 2264, Last => 2266),
      --  481: "př. n. l."
      481 => (First => 2276, Last => 2284),
      --  482: "péntek"
      482 => (First => 2285, Last => 2290),
      --  483: "pátek"
      483 => (First => 2291, Last => 2295),
      --  484: "pá"
      484 => (First => 2291, Last => 2292),
      --  485: "pt."
      485 => (First => 2050, Last => 2052),
      --  486: "pt"
      486 => (First => 2040, Last => 2041),
      --  487: "przed południem"
      487 => (First => 2296, Last => 2310),
      --  488: "prosince"
      488 => (First => 2311, Last => 2318),
      --  489: "pred n.l."
      489 => (First => 2319, Last => 2327),
      --  490: "popoludní"
      490 => (First => 2328, Last => 2336),
      --  491: "poniedziałek"
      491 => (First => 2337, Last => 2348),
      --  492: "pondělí"
      492 => (First => 2349, Last => 2355),
      --  493: "pondelok"
      493 => (First => 2356, Last => 2363),
      --  494: "pon."
      494 => (First => 2364, Last => 2367),
      --  495: "po południu"
      495 => (First => 2368, Last => 2378),
      --  496: "po"
      496 => (First => 1820, Last => 1821),
      --  497: "pm"
      497 => (First => 2379, Last => 2380),
      --  498: "pl"
      498 => (First => 2381, Last => 2382),
      --  499: "piątek"
      499 => (First => 2383, Last => 2388),
      --  500: "piatok"
      500 => (First => 2389, Last => 2394),
      --  501: "pi"
      501 => (First => 2383, Last => 2384),
      --  502: "perjantaina"
      502 => (First => 2395, Last => 2405),
      --  503: "pe"
      503 => (First => 2395, Last => 2396),
      --  504: "października"
      504 => (First => 2406, Last => 2417),
      --  505: "paź"
      505 => (First => 2406, Last => 2408),
      --  506: "p.n.e."
      506 => (First => 2418, Last => 2423),
      --  507: "p.m."
      507 => (First => 2424, Last => 2427),
      --  508: "p."
      508 => (First => 2202, Last => 2203),
      --  509: "p"
      509 => (First => 1494, Last => 1494),
      --  510: "outubro"
      510 => (First => 2428, Last => 2434),
      --  511: "out"
      511 => (First => 2428, Last => 2430),
      --  512: "ottobre"
      512 => (First => 2435, Last => 2441),
      --  513: "ott"
      513 => (First => 2435, Last => 2437),
      --  514: "onsdag"
      514 => (First => 2442, Last => 2447),
      --  515: "ons."
      515 => (First => 2448, Last => 2451),
      --  516: "ons"
      516 => (First => 2442, Last => 2444),
      --  517: "októbra"
      517 => (First => 2452, Last => 2458),
      --  518: "október"
      518 => (First => 2459, Last => 2465),
      --  519: "oktober"
      519 => (First => 2466, Last => 2472),
      --  520: "okt."
      520 => (First => 2473, Last => 2476),
      --  521: "okt"
      521 => (First => 1907, Last => 1909),
      --  522: "odp."
      522 => (First => 2477, Last => 2480),
      --  523: "octubre"
      523 => (First => 2481, Last => 2487),
      --  524: "octombrie"
      524 => (First => 2488, Last => 2496),
      --  525: "octobre"
      525 => (First => 2497, Last => 2503),
      --  526: "oct."
      526 => (First => 2504, Last => 2507),
      --  527: "oct"
      527 => (First => 2481, Last => 2483),
      --  528: "noviembre"
      528 => (First => 2508, Last => 2516),
      --  529: "novembro"
      529 => (First => 2517, Last => 2524),
      --  530: "novembre"
      530 => (First => 2525, Last => 2532),
      --  531: "novembra"
      531 => (First => 2533, Last => 2540),
      --  532: "november"
      532 => (First => 2541, Last => 2548),
      --  533: "nov."
      533 => (First => 2549, Last => 2552),
      --  534: "nov"
      534 => (First => 2508, Last => 2510),
      --  535: "noon"
      535 => (First => 2553, Last => 2556),
      --  536: "noite"
      536 => (First => 2557, Last => 2561),
      --  537: "noiembrie"
      537 => (First => 2562, Last => 2570),
      --  538: "nl"
      538 => (First => 2571, Last => 2572),
      --  539: "night"
      539 => (First => 2573, Last => 2577),
      --  540: "niedziela"
      540 => (First => 2578, Last => 2586),
      --  541: "niedz."
      541 => (First => 2587, Last => 2592),
      --  542: "neděle"
      542 => (First => 2593, Last => 2598),
      --  543: "nedeľa"
      543 => (First => 2599, Last => 2604),
      --  544: "ne"
      544 => (First => 1858, Last => 1859),
      --  545: "nb"
      545 => (First => 2605, Last => 2606),
      --  546: "nad ranem"
      546 => (First => 2607, Last => 2615),
      --  547: "nachts"
      547 => (First => 2616, Last => 2621),
      --  548: "nachmittags"
      548 => (First => 2622, Last => 2632),
      --  549: "nachm."
      549 => (First => 2633, Last => 2638),
      --  550: "n.l."
      550 => (First => 2324, Last => 2327),
      --  551: "n.e."
      551 => (First => 2420, Last => 2423),
      --  552: "n. l."
      552 => (First => 2280, Last => 2284),
      --  553: "n. Chr."
      553 => (First => 2639, Last => 2645),
      --  554: "måndag"
      554 => (First => 2646, Last => 2651),
      --  555: "mån"
      555 => (First => 2646, Last => 2648),
      --  556: "március"
      556 => (First => 2652, Last => 2658),
      --  557: "márc."
      557 => (First => 2659, Last => 2663),
      --  558: "május"
      558 => (First => 2664, Last => 2668),
      --  559: "mája"
      559 => (First => 2669, Last => 2672),
      --  560: "máj."
      560 => (First => 2673, Last => 2676),
      --  561: "máj"
      561 => (First => 2664, Last => 2666),
      --  562: "mrt."
      562 => (First => 2677, Last => 2680),
      --  563: "morning"
      563 => (First => 2681, Last => 2687),
      --  564: "morgens"
      564 => (First => 2688, Last => 2694),
      --  565: "miércoles"
      565 => (First => 2695, Last => 2703),
      --  566: "mié"
      566 => (First => 2695, Last => 2697),
      --  567: "miercuri"
      567 => (First => 2704, Last => 2711),
      --  568: "midi"
      568 => (First => 2712, Last => 2715),
      --  569: "middag"
      569 => (First => 2716, Last => 2721),
      --  570: "midDay"
      570 => (First => 2722, Last => 2727),
      --  571: "mercredi"
      571 => (First => 2728, Last => 2735),
      --  572: "mercoledì"
      572 => (First => 2736, Last => 2744),
      --  573: "mer."
      573 => (First => 2745, Last => 2748),
      --  574: "mer"
      574 => (First => 2728, Last => 2730),
      --  575: "meio-dia"
      575 => (First => 2749, Last => 2756),
      --  576: "mei"
      576 => (First => 2749, Last => 2751),
      --  577: "mayo"
      577 => (First => 2757, Last => 2760),
      --  578: "may"
      578 => (First => 2757, Last => 2759),
      --  579: "matin"
      579 => (First => 2761, Last => 2765),
      --  580: "marți"
      580 => (First => 2766, Last => 2770),
      --  581: "março"
      581 => (First => 2771, Last => 2775),
      --  582: "marzo"
      582 => (First => 2776, Last => 2780),
      --  583: "marts"
      583 => (First => 2781, Last => 2785),
      --  584: "martie"
      584 => (First => 2786, Last => 2791),
      --  585: "martes"
      585 => (First => 2792, Last => 2797),
      --  586: "martedì"
      586 => (First => 2798, Last => 2804),
      --  587: "mars"
      587 => (First => 2805, Last => 2808),
      --  588: "marraskuuta"
      588 => (First => 2809, Last => 2819),
      --  589: "mardi"
      589 => (First => 2820, Last => 2824),
      --  590: "marca"
      590 => (First => 2825, Last => 2829),
      --  591: "mar."
      591 => (First => 2830, Last => 2833),
      --  592: "mar"
      592 => (First => 2766, Last => 2768),
      --  593: "manhã"
      593 => (First => 2834, Last => 2838),
      --  594: "mandag"
      594 => (First => 2839, Last => 2844),
      --  595: "man."
      595 => (First => 2845, Last => 2848),
      --  596: "man"
      596 => (First => 2834, Last => 2836),
      --  597: "maja"
      597 => (First => 2849, Last => 2852),
      --  598: "maj"
      598 => (First => 2849, Last => 2851),
      --  599: "maio"
      599 => (First => 2853, Last => 2856),
      --  600: "mai"
      600 => (First => 2853, Last => 2855),
      --  601: "maggio"
      601 => (First => 2857, Last => 2862),
      --  602: "mag"
      602 => (First => 2857, Last => 2859),
      --  603: "maart"
      603 => (First => 2863, Last => 2867),
      --  604: "maandag"
      604 => (First => 2868, Last => 2874),
      --  605: "maanantaina"
      605 => (First => 2875, Last => 2885),
      --  606: "maaliskuuta"
      606 => (First => 2886, Last => 2896),
      --  607: "ma"
      607 => (First => 2757, Last => 2758),
      --  608: "m."
      608 => (First => 1854, Last => 1855),
      --  609: "m"
      609 => (First => 1516, Last => 1516),
      --  610: "lørdag"
      610 => (First => 2897, Last => 2902),
      --  611: "lør."
      611 => (First => 2903, Last => 2906),
      --  612: "lør"
      612 => (First => 2897, Last => 2899),
      --  613: "lördag"
      613 => (First => 2907, Last => 2912),
      --  614: "lör"
      614 => (First => 2907, Last => 2909),
      --  615: "lutego"
      615 => (First => 2913, Last => 2918),
      --  616: "lut"
      616 => (First => 2913, Last => 2915),
      --  617: "luni"
      617 => (First => 2919, Last => 2922),
      --  618: "lunes"
      618 => (First => 2923, Last => 2927),
      --  619: "lunedì"
      619 => (First => 2928, Last => 2933),
      --  620: "lundi"
      620 => (First => 2934, Last => 2938),
      --  621: "lun."
      621 => (First => 2939, Last => 2942),
      --  622: "lun"
      622 => (First => 2919, Last => 2921),
      --  623: "luglio"
      623 => (First => 2943, Last => 2948),
      --  624: "lug"
      624 => (First => 2943, Last => 2945),
      --  625: "lokakuuta"
      625 => (First => 2949, Last => 2957),
      --  626: "listopadu"
      626 => (First => 2958, Last => 2966),
      --  627: "listopada"
      627 => (First => 2967, Last => 2975),
      --  628: "lis"
      628 => (First => 1497, Last => 1499),
      --  629: "lipca"
      629 => (First => 2976, Last => 2980),
      --  630: "lip"
      630 => (First => 2976, Last => 2978),
      --  631: "left-to-right"
      631 => (First => 2981, Last => 2993),
      --  632: "ledna"
      632 => (First => 2994, Last => 2998),
      --  633: "lauantaina"
      633 => (First => 2999, Last => 3008),
      --  634: "latn"
      634 => (First => 3009, Last => 3012),
      --  635: "lateMorning"
      635 => (First => 3013, Last => 3023),
      --  636: "la"
      636 => (First => 2585, Last => 2586),
      --  637: "l"
      637 => (First => 1497, Last => 1497),
      --  638: "kwietnia"
      638 => (First => 3024, Last => 3031),
      --  639: "kwi"
      639 => (First => 3024, Last => 3026),
      --  640: "května"
      640 => (First => 3032, Last => 3037),
      --  641: "ko"
      641 => (First => 1914, Last => 1915),
      --  642: "kesäkuuta"
      642 => (First => 3038, Last => 3046),
      --  643: "keskiviikkona"
      643 => (First => 3047, Last => 3059),
      --  644: "kedd"
      644 => (First => 3060, Last => 3063),
      --  645: "ke"
      645 => (First => 3038, Last => 3039),
      --  646: "k"
      646 => (First => 1429, Last => 1429),
      --  647: "június"
      647 => (First => 3064, Last => 3069),
      --  648: "júna"
      648 => (First => 3070, Last => 3073),
      --  649: "jún."
      649 => (First => 3074, Last => 3077),
      --  650: "jún"
      650 => (First => 3064, Last => 3066),
      --  651: "július"
      651 => (First => 3078, Last => 3083),
      --  652: "júla"
      652 => (First => 3084, Last => 3087),
      --  653: "júl."
      653 => (First => 3088, Last => 3091),
      --  654: "júl"
      654 => (First => 3078, Last => 3080),
      --  655: "junio"
      655 => (First => 3092, Last => 3096),
      --  656: "juni"
      656 => (First => 3092, Last => 3095),
      --  657: "junho"
      657 => (First => 3097, Last => 3101),
      --  658: "jun."
      658 => (First => 3102, Last => 3105),
      --  659: "jun"
      659 => (First => 3092, Last => 3094),
      --  660: "julio"
      660 => (First => 3106, Last => 3110),
      --  661: "juli"
      661 => (First => 3106, Last => 3109),
      --  662: "julho"
      662 => (First => 3111, Last => 3115),
      --  663: "jul."
      663 => (First => 3116, Last => 3119),
      --  664: "jul"
      664 => (First => 3106, Last => 3108),
      --  665: "juin"
      665 => (First => 3120, Last => 3123),
      --  666: "juillet"
      666 => (First => 3124, Last => 3130),
      --  667: "juil."
      667 => (First => 3131, Last => 3135),
      --  668: "jueves"
      668 => (First => 3136, Last => 3141),
      --  669: "jue"
      669 => (First => 3136, Last => 3138),
      --  670: "joulukuuta"
      670 => (First => 3142, Last => 3151),
      --  671: "joi"
      671 => (First => 3152, Last => 3154),
      --  672: "jeudi"
      672 => (First => 3155, Last => 3159),
      --  673: "jeu."
      673 => (First => 3160, Last => 3163),
      --  674: "janvier"
      674 => (First => 3164, Last => 3170),
      --  675: "janv."
      675 => (First => 3171, Last => 3175),
      --  676: "januára"
      676 => (First => 3176, Last => 3182),
      --  677: "január"
      677 => (First => 3176, Last => 3181),
      --  678: "januari"
      678 => (First => 3183, Last => 3189),
      --  679: "januar"
      679 => (First => 3183, Last => 3188),
      --  680: "janeiro"
      680 => (First => 3190, Last => 3196),
      --  681: "jan."
      681 => (First => 3197, Last => 3200),
      --  682: "jan"
      682 => (First => 2398, Last => 2400),
      --  683: "ja"
      683 => (First => 2398, Last => 2399),
      --  684: "jKr."
      684 => (First => 3201, Last => 3204),
      --  685: "iunie"
      685 => (First => 3205, Last => 3209),
      --  686: "iun."
      686 => (First => 3210, Last => 3213),
      --  687: "iulie"
      687 => (First => 3214, Last => 3218),
      --  688: "iul."
      688 => (First => 3219, Last => 3222),
      --  689: "it"
      689 => (First => 1845, Last => 1846),
      --  690: "ip."
      690 => (First => 3223, Last => 3225),
      --  691: "ianuarie"
      691 => (First => 3226, Last => 3233),
      --  692: "ian."
      692 => (First => 3234, Last => 3237),
      --  693: "i. sz."
      693 => (First => 3238, Last => 3243),
      --  694: "i. e."
      694 => (First => 3244, Last => 3248),
      --  695: "hétfő"
      695 => (First => 3249, Last => 3253),
      --  696: "huhtikuuta"
      696 => (First => 3254, Last => 3263),
      --  697: "hu"
      697 => (First => 3254, Last => 3255),
      --  698: "helmikuuta"
      698 => (First => 3264, Last => 3273),
      --  699: "heinäkuuta"
      699 => (First => 3274, Last => 3283),
      --  700: "he"
      700 => (First => 3264, Last => 3265),
      --  701: "h:mm:ss a zzzz"
      701 => (First => 3284, Last => 3297),
      --  702: "h:mm:ss a z"
      702 => (First => 3284, Last => 3294),
      --  703: "h:mm:ss a"
      703 => (First => 1649, Last => 1657),
      --  704: "h:mm a"
      704 => (First => 3298, Last => 3303),
      --  705: "grudnia"
      705 => (First => 3304, Last => 3310),
      --  706: "gru"
      706 => (First => 3304, Last => 3306),
      --  707: "giugno"
      707 => (First => 3311, Last => 3316),
      --  708: "giu"
      708 => (First => 3311, Last => 3313),
      --  709: "giovedì"
      709 => (First => 3317, Last => 3323),
      --  710: "gio"
      710 => (First => 2860, Last => 2862),
      --  711: "gennaio"
      711 => (First => 3324, Last => 3330),
      --  712: "gen"
      712 => (First => 2691, Last => 2693),
      --  713: "ga"
      713 => (First => 3331, Last => 3332),
      --  714: "g"
      714 => (First => 1663, Last => 1663),
      --  715: "février"
      715 => (First => 3333, Last => 3339),
      --  716: "févr."
      716 => (First => 3340, Last => 3344),
      --  717: "fredag"
      717 => (First => 3345, Last => 3350),
      --  718: "fre."
      718 => (First => 3351, Last => 3354),
      --  719: "fre"
      719 => (First => 3345, Last => 3347),
      --  720: "fr"
      720 => (First => 3345, Last => 3346),
      --  721: "fm"
      721 => (First => 3355, Last => 3356),
      --  722: "fi"
      722 => (First => 3357, Last => 3358),
      --  723: "fevereiro"
      723 => (First => 3359, Last => 3367),
      --  724: "fev"
      724 => (First => 3359, Last => 3361),
      --  725: "februára"
      725 => (First => 3368, Last => 3375),
      --  726: "február"
      726 => (First => 3368, Last => 3374),
      --  727: "februarie"
      727 => (First => 3376, Last => 3384),
      --  728: "februari"
      728 => (First => 3376, Last => 3383),
      --  729: "februar"
      729 => (First => 3376, Last => 3382),
      --  730: "febrero"
      730 => (First => 3385, Last => 3391),
      --  731: "febr."
      731 => (First => 3392, Last => 3396),
      --  732: "febbraio"
      732 => (First => 3397, Last => 3404),
      --  733: "feb."
      733 => (First => 3405, Last => 3408),
      --  734: "feb"
      734 => (First => 3368, Last => 3370),
      --  735: "f.m."
      735 => (First => 3409, Last => 3412),
      --  736: "f.Kr."
      736 => (First => 3413, Last => 3417),
      --  737: "evening"
      737 => (First => 3418, Last => 3424),
      --  738: "es"
      738 => (First => 1867, Last => 1868),
      --  739: "epäluku"
      739 => (First => 3425, Last => 3431),
      --  740: "enero"
      740 => (First => 3432, Last => 3436),
      --  741: "ene"
      741 => (First => 1870, Last => 1872),
      --  742: "en_ZA"
      742 => (First => 3437, Last => 3441),
      --  743: "en_NZ"
      743 => (First => 3442, Last => 3446),
      --  744: "en_IE"
      744 => (First => 3447, Last => 3451),
      --  745: "en_GB"
      745 => (First => 3452, Last => 3456),
      --  746: "en_CA"
      746 => (First => 3457, Last => 3461),
      --  747: "en_AU"
      747 => (First => 3462, Last => 3466),
      --  748: "en"
      748 => (First => 1468, Last => 1469),
      --  749: "em"
      749 => (First => 1808, Last => 1809),
      --  750: "elokuuta"
      750 => (First => 3467, Last => 3474),
      --  751: "el"
      751 => (First => 2360, Last => 2361),
      --  752: "earlyMorning"
      752 => (First => 3475, Last => 3486),
      --  753: "eKr."
      753 => (First => 3487, Last => 3490),
      --  754: "e.m."
      754 => (First => 3491, Last => 3494),
      --  755: "e.Kr."
      755 => (First => 3495, Last => 3499),
      --  756: "e"
      756 => (First => 1456, Last => 1456),
      --  757: "d‏/M‏/yyyy"
      757 => (First => 3500, Last => 3509),
      --  758: "décembre"
      758 => (First => 3510, Last => 3517),
      --  759: "déc."
      759 => (First => 3518, Last => 3521),
      --  760: "duminică"
      760 => (First => 3522, Last => 3529),
      --  761: "dubna"
      761 => (First => 3530, Last => 3534),
      --  762: "du."
      762 => (First => 3535, Last => 3537),
      --  763: "dopoludnia"
      763 => (First => 3538, Last => 3547),
      --  764: "dop."
      764 => (First => 3548, Last => 3551),
      --  765: "donderdag"
      765 => (First => 3552, Last => 3560),
      --  766: "domingo"
      766 => (First => 3561, Last => 3567),
      --  767: "domenica"
      767 => (First => 3568, Last => 3575),
      --  768: "dom"
      768 => (First => 3561, Last => 3563),
      --  769: "do"
      769 => (First => 2022, Last => 2023),
      --  770: "dinsdag"
      770 => (First => 3576, Last => 3582),
      --  771: "dimanche"
      771 => (First => 3583, Last => 3590),
      --  772: "dim."
      772 => (First => 3591, Last => 3594),
      --  773: "diciembre"
      773 => (First => 3595, Last => 3603),
      --  774: "dicembre"
      774 => (First => 3604, Last => 3611),
      --  775: "dic"
      775 => (First => 3595, Last => 3597),
      --  776: "di"
      776 => (First => 1882, Last => 1883),
      --  777: "dezembro"
      777 => (First => 3612, Last => 3619),
      --  778: "dez"
      778 => (First => 3612, Last => 3614),
      --  779: "desember"
      779 => (First => 3620, Last => 3627),
      --  780: "des."
      780 => (First => 3628, Last => 3631),
      --  781: "decembrie"
      781 => (First => 3632, Last => 3640),
      --  782: "decembra"
      782 => (First => 3641, Last => 3648),
      --  783: "december"
      783 => (First => 3649, Last => 3656),
      --  784: "dec."
      784 => (First => 3657, Last => 3660),
      --  785: "dec"
      785 => (First => 3632, Last => 3634),
      --  786: "de."
      786 => (First => 3661, Last => 3663),
      --  787: "de"
      787 => (First => 1983, Last => 1984),
      --  788: "dd‏/MM‏/yyyy"
      788 => (First => 3664, Last => 3675),
      --  789: "dd/MMM/y"
      789 => (First => 3676, Last => 3683),
      --  790: "dd/MM/yyyy"
      790 => (First => 3684, Last => 3693),
      --  791: "dd/MM/yy"
      791 => (First => 3684, Last => 3691),
      --  792: "dd.MM.yyyy"
      792 => (First => 3694, Last => 3703),
      --  793: "dd.MM.yy"
      793 => (First => 3694, Last => 3701),
      --  794: "dd-MM-yy"
      794 => (First => 3704, Last => 3711),
      --  795: "dd MMMM y"
      795 => (First => 3712, Last => 3720),
      --  796: "dd MMM y"
      796 => (First => 3721, Last => 3728),
      --  797: "da"
      797 => (First => 1438, Last => 1439),
      --  798: "dC"
      798 => (First => 3729, Last => 3730),
      --  799: "d/MM/yyyy"
      799 => (First => 3685, Last => 3693),
      --  800: "d/MM/yy"
      800 => (First => 3685, Last => 3691),
      --  801: "d/M/yyyy"
      801 => (First => 3731, Last => 3738),
      --  802: "d/M/yy"
      802 => (First => 3731, Last => 3736),
      --  803: "d.M.yyyy"
      803 => (First => 3739, Last => 3746),
      --  804: "d.Hr."
      804 => (First => 3747, Last => 3751),
      --  805: "d.C."
      805 => (First => 3752, Last => 3755),
      --  806: "d. MMMM y"
      806 => (First => 3756, Last => 3764),
      --  807: "d. MMM y"
      807 => (First => 3765, Last => 3772),
      --  808: "d. M. yyyy"
      808 => (First => 3773, Last => 3782),
      --  809: "d בMMMM y"
      809 => (First => 3783, Last => 3791),
      --  810: "d בMMM yyyy"
      810 => (First => 3792, Last => 3802),
      --  811: "d MMMM، y"
      811 => (First => 3803, Last => 3811),
      --  812: "d MMMM, y"
      812 => (First => 3812, Last => 3820),
      --  813: "d MMMM y 'г'."
      813 => (First => 3821, Last => 3833),
      --  814: "d MMMM y EEEE"
      814 => (First => 3834, Last => 3846),
      --  815: "d MMMM y"
      815 => (First => 3713, Last => 3720),
      --  816: "d MMM y"
      816 => (First => 3722, Last => 3728),
      --  817: "d 'de' MMMM 'de' y"
      817 => (First => 3847, Last => 3864),
      --  818: "czwartek"
      818 => (First => 3865, Last => 3872),
      --  819: "czw."
      819 => (First => 3873, Last => 3876),
      --  820: "czerwca"
      820 => (First => 3877, Last => 3883),
      --  821: "cze"
      821 => (First => 3877, Last => 3879),
      --  822: "csütörtök"
      822 => (First => 3884, Last => 3892),
      --  823: "cs"
      823 => (First => 3884, Last => 3885),
      --  824: "cccc, d. MMMM y"
      824 => (First => 3893, Last => 3907),
      --  825: "c"
      825 => (First => 1470, Last => 1470),
      --  826: "března"
      826 => (First => 3908, Last => 3913),
      --  827: "avril"
      827 => (First => 3914, Last => 3918),
      --  828: "avr."
      828 => (First => 3919, Last => 3922),
      --  829: "av. J.-C."
      829 => (First => 3923, Last => 3931),
      --  830: "augusztus"
      830 => (First => 3932, Last => 3940),
      --  831: "augustus"
      831 => (First => 3941, Last => 3948),
      --  832: "augusti"
      832 => (First => 3949, Last => 3955),
      --  833: "augusta"
      833 => (First => 3956, Last => 3962),
      --  834: "august"
      834 => (First => 3941, Last => 3946),
      --  835: "aug."
      835 => (First => 3963, Last => 3966),
      --  836: "aug"
      836 => (First => 3932, Last => 3934),
      --  837: "arab"
      837 => (First => 3967, Last => 3970),
      --  838: "ar"
      838 => (First => 1512, Last => 1513),
      --  839: "apríla"
      839 => (First => 3971, Last => 3976),
      --  840: "après-midi"
      840 => (First => 3977, Last => 3986),
      --  841: "aprilie"
      841 => (First => 3987, Last => 3993),
      --  842: "aprile"
      842 => (First => 3994, Last => 3999),
      --  843: "april"
      843 => (First => 3987, Last => 3991),
      --  844: "apr."
      844 => (First => 4000, Last => 4003),
      --  845: "apr"
      845 => (First => 3971, Last => 3973),
      --  846: "ap. J.-C."
      846 => (First => 4004, Last => 4012),
      --  847: "ap."
      847 => (First => 2417, Last => 2419),
      --  848: "août"
      848 => (First => 4013, Last => 4016),
      --  849: "am"
      849 => (First => 1515, Last => 1516),
      --  850: "ah:mm:ss"
      850 => (First => 3283, Last => 3290),
      --  851: "ah:mm"
      851 => (First => 3283, Last => 3287),
      --  852: "agosto"
      852 => (First => 4017, Last => 4022),
      --  853: "ago"
      853 => (First => 2446, Last => 2448),
      --  854: "afternoon"
      854 => (First => 4023, Last => 4031),
      --  855: "abril"
      855 => (First => 4032, Last => 4036),
      --  856: "abr"
      856 => (First => 4032, Last => 4034),
      --  857: "abends"
      857 => (First => 4037, Last => 4042),
      --  858: "aC"
      858 => (First => 4043, Last => 4044),
      --  859: "a.m."
      859 => (First => 4045, Last => 4048),
      --  860: "a.C."
      860 => (First => 4049, Last => 4052),
      --  861: "a h시 m분 s초 zzzz"
      861 => (First => 4053, Last => 4067),
      --  862: "a h시 m분 s초 z"
      862 => (First => 4053, Last => 4064),
      --  863: "a h:mm:ss"
      863 => (First => 4068, Last => 4076),
      --  864: "a h:mm"
      864 => (First => 4068, Last => 4073),
      --  865: "Zář"
      865 => (First => 4077, Last => 4079),
      --  866: "Wednesday"
      866 => (First => 4080, Last => 4088),
      --  867: "Wed"
      867 => (First => 4080, Last => 4082),
      --  868: "Vi"
      868 => (First => 4089, Last => 4090),
      --  869: "V"
      869 => (First => 4089, Last => 4089),
      --  870: "Tuesday"
      870 => (First => 4091, Last => 4097),
      --  871: "Tue"
      871 => (First => 4091, Last => 4093),
      --  872: "Thursday"
      872 => (First => 4098, Last => 4105),
      --  873: "Thu"
      873 => (First => 4098, Last => 4100),
      --  874: "Temmuz"
      874 => (First => 4106, Last => 4111),
      --  875: "Tem"
      875 => (First => 4106, Last => 4108),
      --  876: "T"
      876 => (First => 4091, Last => 4091),
      --  877: "Sâ"
      877 => (First => 4112, Last => 4113),
      --  878: "Szo"
      878 => (First => 4114, Last => 4116),
      --  879: "Sze"
      879 => (First => 4117, Last => 4119),
      --  880: "Sz"
      880 => (First => 4114, Last => 4115),
      --  881: "Sunday"
      881 => (First => 4120, Last => 4125),
      --  882: "Sun"
      882 => (First => 4120, Last => 4122),
      --  883: "Srp"
      883 => (First => 4126, Last => 4128),
      --  884: "Sonntag"
      884 => (First => 4129, Last => 4135),
      --  885: "So."
      885 => (First => 4136, Last => 4138),
      --  886: "September"
      886 => (First => 4139, Last => 4147),
      --  887: "Sep"
      887 => (First => 4139, Last => 4141),
      --  888: "Saturday"
      888 => (First => 4148, Last => 4155),
      --  889: "Sath"
      889 => (First => 4156, Last => 4159),
      --  890: "Sat"
      890 => (First => 4148, Last => 4150),
      --  891: "Samstag"
      891 => (First => 4160, Last => 4166),
      --  892: "Samhain"
      892 => (First => 4167, Last => 4173),
      --  893: "Samh"
      893 => (First => 4167, Last => 4170),
      --  894: "Salı"
      894 => (First => 4174, Last => 4177),
      --  895: "Sal"
      895 => (First => 4174, Last => 4176),
      --  896: "Sa."
      896 => (First => 4178, Last => 4180),
      --  897: "S"
      897 => (First => 4112, Last => 4112),
      --  898: "RC"
      898 => (First => 4181, Last => 4182),
      --  899: "Pzt"
      899 => (First => 4183, Last => 4185),
      --  900: "Pro"
      900 => (First => 4186, Last => 4188),
      --  901: "Perşembe"
      901 => (First => 4189, Last => 4196),
      --  902: "Per"
      902 => (First => 4189, Last => 4191),
      --  903: "Pazartesi"
      903 => (First => 4197, Last => 4205),
      --  904: "Pazar"
      904 => (First => 4197, Last => 4201),
      --  905: "Paz"
      905 => (First => 4197, Last => 4199),
      --  906: "PM"
      906 => (First => 4206, Last => 4207),
      --  907: "P"
      907 => (First => 4183, Last => 4183),
      --  908: "Oktober"
      908 => (First => 4208, Last => 4214),
      --  909: "Okt"
      909 => (First => 4208, Last => 4210),
      --  910: "October"
      910 => (First => 4215, Last => 4221),
      --  911: "Oct"
      911 => (First => 4215, Last => 4217),
      --  912: "Ocak"
      912 => (First => 4222, Last => 4225),
      --  913: "Oca"
      913 => (First => 4222, Last => 4224),
      --  914: "O"
      914 => (First => 4208, Last => 4208),
      --  915: "November"
      915 => (First => 4226, Last => 4233),
      --  916: "Nov"
      916 => (First => 4226, Last => 4228),
      --  917: "Nollaig"
      917 => (First => 4234, Last => 4240),
      --  918: "Noll"
      918 => (First => 4234, Last => 4237),
      --  919: "Nisan"
      919 => (First => 4241, Last => 4245),
      --  920: "Nis"
      920 => (First => 4241, Last => 4243),
      --  921: "NaN（非数）"
      921 => (First => 4246, Last => 4252),
      --  922: "NaN"
      922 => (First => 4246, Last => 4248),
      --  923: "N"
      923 => (First => 3445, Last => 3445),
      --  924: "März"
      924 => (First => 4253, Last => 4256),
      --  925: "Mär"
      925 => (First => 4253, Last => 4255),
      --  926: "Márta"
      926 => (First => 4257, Last => 4261),
      --  927: "Máirt"
      927 => (First => 4262, Last => 4266),
      --  928: "MÖ"
      928 => (First => 4267, Last => 4268),
      --  929: "Montag"
      929 => (First => 4269, Last => 4274),
      --  930: "Monday"
      930 => (First => 4275, Last => 4280),
      --  931: "Mon"
      931 => (First => 4269, Last => 4271),
      --  932: "Mo."
      932 => (First => 4281, Last => 4283),
      --  933: "Mittwoch"
      933 => (First => 4284, Last => 4291),
      --  934: "Mittag"
      934 => (First => 4292, Last => 4297),
      --  935: "Mi."
      935 => (First => 4298, Last => 4300),
      --  936: "Mi"
      936 => (First => 4284, Last => 4285),
      --  937: "Meán Fómhair"
      937 => (First => 4301, Last => 4312),
      --  938: "Meitheamh"
      938 => (First => 4313, Last => 4321),
      --  939: "Meith"
      939 => (First => 4313, Last => 4317),
      --  940: "Mayıs"
      940 => (First => 4322, Last => 4326),
      --  941: "May"
      941 => (First => 4322, Last => 4324),
      --  942: "Mart"
      942 => (First => 4327, Last => 4330),
      --  943: "March"
      943 => (First => 4331, Last => 4335),
      --  944: "Mar"
      944 => (First => 4327, Last => 4329),
      --  945: "Mai"
      945 => (First => 4336, Last => 4338),
      --  946: "Ma"
      946 => (First => 4322, Last => 4323),
      --  947: "MS"
      947 => (First => 4339, Last => 4340),
      --  948: "MMMM d, y"
      948 => (First => 4341, Last => 4349),
      --  949: "MMM d, y"
      949 => (First => 4342, Last => 4349),
      --  950: "MFómh"
      950 => (First => 4350, Last => 4354),
      --  951: "M/d/yy"
      951 => (First => 4355, Last => 4360),
      --  952: "M"
      952 => (First => 1682, Last => 1682),
      --  953: "Lúnasa"
      953 => (First => 4361, Last => 4366),
      --  954: "Lún"
      954 => (First => 4361, Last => 4363),
      --  955: "Luan"
      955 => (First => 4367, Last => 4370),
      --  956: "Lu"
      956 => (First => 4367, Last => 4368),
      --  957: "Lis"
      957 => (First => 4371, Last => 4373),
      --  958: "Led"
      958 => (First => 4374, Last => 4376),
      --  959: "L"
      959 => (First => 4361, Last => 4361),
      --  960: "Kvě"
      960 => (First => 4377, Last => 4379),
      --  961: "Kasım"
      961 => (First => 4380, Last => 4384),
      --  962: "Kas"
      962 => (First => 4380, Last => 4382),
      --  963: "K"
      963 => (First => 3202, Last => 3202),
      --  964: "Juni"
      964 => (First => 4385, Last => 4388),
      --  965: "June"
      965 => (First => 4389, Last => 4392),
      --  966: "Jun"
      966 => (First => 4385, Last => 4387),
      --  967: "July"
      967 => (First => 4393, Last => 4396),
      --  968: "Juli"
      968 => (First => 4397, Last => 4400),
      --  969: "Jul"
      969 => (First => 4393, Last => 4395),
      --  970: "Jo"
      970 => (First => 4401, Last => 4402),
      --  971: "January"
      971 => (First => 4403, Last => 4409),
      --  972: "Januar"
      972 => (First => 4403, Last => 4408),
      --  973: "Jan"
      973 => (First => 4403, Last => 4405),
      --  974: "J"
      974 => (First => 3927, Last => 3927),
      --  975: "Iúil"
      975 => (First => 4410, Last => 4413),
      --  976: "Inf"
      976 => (First => 4414, Last => 4416),
      --  977: "I"
      977 => (First => 3450, Last => 3450),
      --  978: "H時mm分ss秒 zzzz"
      978 => (First => 4417, Last => 4429),
      --  979: "Haziran"
      979 => (First => 4430, Last => 4436),
      --  980: "Haz"
      980 => (First => 4430, Last => 4432),
      --  981: "HH:mm:ss zzzz"
      981 => (First => 4437, Last => 4449),
      --  982: "HH:mm:ss z"
      982 => (First => 4437, Last => 4446),
      --  983: "HH:mm:ss"
      983 => (First => 4437, Last => 4444),
      --  984: "HH:mm"
      984 => (First => 4437, Last => 4441),
      --  985: "HH.mm.ss zzzz"
      985 => (First => 4450, Last => 4462),
      --  986: "HH.mm.ss z"
      986 => (First => 4450, Last => 4459),
      --  987: "HH.mm.ss"
      987 => (First => 4450, Last => 4457),
      --  988: "HH.mm"
      988 => (First => 4450, Last => 4454),
      --  989: "HH'h'mm'min'ss's' zzzz"
      989 => (First => 4463, Last => 4484),
      --  990: "HH'h'mm'min'ss's' z"
      990 => (First => 4463, Last => 4481),
      --  991: "H:mm:ss zzzz"
      991 => (First => 4438, Last => 4449),
      --  992: "H:mm:ss z"
      992 => (First => 4438, Last => 4446),
      --  993: "H:mm:ss"
      993 => (First => 4438, Last => 4444),
      --  994: "H:mm"
      994 => (First => 4438, Last => 4441),
      --  995: "H.mm.ss zzzz"
      995 => (First => 4451, Last => 4462),
      --  996: "H.mm.ss z"
      996 => (First => 4451, Last => 4459),
      --  997: "H.mm.ss"
      997 => (First => 4451, Last => 4457),
      --  998: "H.mm"
      998 => (First => 4451, Last => 4454),
      --  999: "H นาฬิกา m นาที ss วินาที zzzz"
      999 => (First => 4485, Last => 4514),
      --  1000: "H นาฬิกา m นาที ss วินาที z"
      1000 => (First => 4485, Last => 4511),
      --  1001: "H"
      1001 => (First => 1490, Last => 1490),
      --  1002: "G"
      1002 => (First => 3455, Last => 3455),
      --  1003: "Friday"
      1003 => (First => 4515, Last => 4520),
      --  1004: "Fri"
      1004 => (First => 4515, Last => 4517),
      --  1005: "Freitag"
      1005 => (First => 4521, Last => 4527),
      --  1006: "Fr."
      1006 => (First => 4528, Last => 4530),
      --  1007: "February"
      1007 => (First => 4531, Last => 4538),
      --  1008: "Februar"
      1008 => (First => 4531, Last => 4537),
      --  1009: "Feb"
      1009 => (First => 4531, Last => 4533),
      --  1010: "Feabhra"
      1010 => (First => 4539, Last => 4545),
      --  1011: "Feabh"
      1011 => (First => 4539, Last => 4543),
      --  1012: "F"
      1012 => (First => 4306, Last => 4306),
      --  1013: "Eylül"
      1013 => (First => 4546, Last => 4550),
      --  1014: "Eyl"
      1014 => (First => 4546, Last => 4548),
      --  1015: "Ekim"
      1015 => (First => 4551, Last => 4554),
      --  1016: "Eki"
      1016 => (First => 4551, Last => 4553),
      --  1017: "Eanáir"
      1017 => (First => 4555, Last => 4560),
      --  1018: "Ean"
      1018 => (First => 4555, Last => 4557),
      --  1019: "EEEEที่ d MMMM G y"
      1019 => (First => 4561, Last => 4578),
      --  1020: "EEEE، d MMMM، y"
      1020 => (First => 4579, Last => 4593),
      --  1021: "EEEE, d. MMMM y"
      1021 => (First => 4594, Last => 4608),
      --  1022: "EEEE, d בMMMM y"
      1022 => (First => 4609, Last => 4623),
      --  1023: "EEEE, d MMMM, y"
      1023 => (First => 4624, Last => 4638),
      --  1024: "EEEE, d MMMM y 'г'."
      1024 => (First => 4639, Last => 4657),
      --  1025: "EEEE, d MMMM y"
      1025 => (First => 4639, Last => 4652),
      --  1026: "EEEE, d 'de' MMMM 'de' y"
      1026 => (First => 4658, Last => 4681),
      --  1027: "EEEE, MMMM d, y"
      1027 => (First => 4682, Last => 4696),
      --  1028: "EEEE'en' 'den' d:'e' MMMM y"
      1028 => (First => 4697, Last => 4723),
      --  1029: "EEEE dd MMMM y"
      1029 => (First => 4724, Last => 4737),
      --  1030: "EEEE d. MMMM y"
      1030 => (First => 4738, Last => 4751),
      --  1031: "EEEE d MMMM y"
      1031 => (First => 4752, Last => 4764),
      --  1032: "EEEE 'den' d. MMMM y"
      1032 => (First => 4765, Last => 4784),
      --  1033: "E"
      1033 => (First => 1688, Last => 1688),
      --  1034: "Déardaoin"
      1034 => (First => 4785, Last => 4793),
      --  1035: "Déar"
      1035 => (First => 4785, Last => 4788),
      --  1036: "Dé hAoine"
      1036 => (First => 4794, Last => 4802),
      --  1037: "Dé Sathairn"
      1037 => (First => 4803, Last => 4813),
      --  1038: "Dé Máirt"
      1038 => (First => 4814, Last => 4821),
      --  1039: "Dé Luain"
      1039 => (First => 4822, Last => 4829),
      --  1040: "Dé Domhnaigh"
      1040 => (First => 4830, Last => 4841),
      --  1041: "Dé Céadaoin"
      1041 => (First => 4842, Last => 4852),
      --  1042: "Dub"
      1042 => (First => 4853, Last => 4855),
      --  1043: "Du"
      1043 => (First => 4853, Last => 4854),
      --  1044: "Donnerstag"
      1044 => (First => 4856, Last => 4865),
      --  1045: "Domh"
      1045 => (First => 4833, Last => 4836),
      --  1046: "Do."
      1046 => (First => 4866, Last => 4868),
      --  1047: "Dienstag"
      1047 => (First => 4869, Last => 4876),
      --  1048: "Di."
      1048 => (First => 4877, Last => 4879),
      --  1049: "Dezember"
      1049 => (First => 4880, Last => 4887),
      --  1050: "Dez"
      1050 => (First => 4880, Last => 4882),
      --  1051: "Deireadh Fómhair"
      1051 => (First => 4888, Last => 4903),
      --  1052: "December"
      1052 => (First => 4904, Last => 4911),
      --  1053: "Dec"
      1053 => (First => 4904, Last => 4906),
      --  1054: "DFómh"
      1054 => (First => 4912, Last => 4916),
      --  1055: "D"
      1055 => (First => 2725, Last => 2725),
      --  1056: "Céad"
      1056 => (First => 4845, Last => 4848),
      --  1057: "Cumartesi"
      1057 => (First => 4917, Last => 4925),
      --  1058: "Cuma"
      1058 => (First => 4917, Last => 4920),
      --  1059: "Cum"
      1059 => (First => 4917, Last => 4919),
      --  1060: "Cs"
      1060 => (First => 4926, Last => 4927),
      --  1061: "Cmt"
      1061 => (First => 4928, Last => 4930),
      --  1062: "CE"
      1062 => (First => 4931, Last => 4932),
      --  1063: "Bře"
      1063 => (First => 4933, Last => 4935),
      --  1064: "Bealtaine"
      1064 => (First => 4936, Last => 4944),
      --  1065: "Beal"
      1065 => (First => 4936, Last => 4939),
      --  1066: "BCE"
      1066 => (First => 4945, Last => 4947),
      --  1067: "BC"
      1067 => (First => 4945, Last => 4946),
      --  1068: "Ağustos"
      1068 => (First => 4948, Last => 4954),
      --  1069: "Ağu"
      1069 => (First => 4948, Last => 4950),
      --  1070: "August"
      1070 => (First => 4955, Last => 4960),
      --  1071: "Aug"
      1071 => (First => 4955, Last => 4957),
      --  1072: "Aralık"
      1072 => (First => 4961, Last => 4966),
      --  1073: "Ara"
      1073 => (First => 4961, Last => 4963),
      --  1074: "April"
      1074 => (First => 4967, Last => 4971),
      --  1075: "Apr"
      1075 => (First => 4967, Last => 4969),
      --  1076: "Aoine"
      1076 => (First => 4798, Last => 4802),
      --  1077: "Aibreán"
      1077 => (First => 4972, Last => 4978),
      --  1078: "Aib"
      1078 => (First => 4972, Last => 4974),
      --  1079: "AM"
      1079 => (First => 4979, Last => 4980),
      --  1080: "AD"
      1080 => (First => 4981, Last => 4982),
      --  1081: "A"
      1081 => (First => 3441, Last => 3441),
      --  1082: ";"
      1082 => (First => 1533, Last => 1533),
      --  1083: "9월"
      1083 => (First => 4983, Last => 4984),
      --  1084: "9月"
      1084 => (First => 4985, Last => 4986),
      --  1085: "9"
      1085 => (First => 170, Last => 170),
      --  1086: "8월"
      1086 => (First => 4987, Last => 4988),
      --  1087: "8月"
      1087 => (First => 4989, Last => 4990),
      --  1088: "8"
      1088 => (First => 169, Last => 169),
      --  1089: "7월"
      1089 => (First => 4991, Last => 4992),
      --  1090: "7月"
      1090 => (First => 4993, Last => 4994),
      --  1091: "7"
      1091 => (First => 168, Last => 168),
      --  1092: "6월"
      1092 => (First => 4995, Last => 4996),
      --  1093: "6月"
      1093 => (First => 4997, Last => 4998),
      --  1094: "6"
      1094 => (First => 167, Last => 167),
      --  1095: "5월"
      1095 => (First => 4999, Last => 5000),
      --  1096: "5月"
      1096 => (First => 5001, Last => 5002),
      --  1097: "5"
      1097 => (First => 166, Last => 166),
      --  1098: "4월"
      1098 => (First => 5003, Last => 5004),
      --  1099: "4月"
      1099 => (First => 5005, Last => 5006),
      --  1100: "4"
      1100 => (First => 165, Last => 165),
      --  1101: "3월"
      1101 => (First => 5007, Last => 5008),
      --  1102: "3月"
      1102 => (First => 5009, Last => 5010),
      --  1103: "3"
      1103 => (First => 164, Last => 164),
      --  1104: "2월"
      1104 => (First => 5011, Last => 5012),
      --  1105: "2月"
      1105 => (First => 5013, Last => 5014),
      --  1106: "2"
      1106 => (First => 163, Last => 163),
      --  1107: "1월"
      1107 => (First => 5015, Last => 5016),
      --  1108: "1月"
      1108 => (First => 5017, Last => 5018),
      --  1109: "12월"
      1109 => (First => 5019, Last => 5021),
      --  1110: "12月"
      1110 => (First => 5022, Last => 5024),
      --  1111: "12 uur 's middags"
      1111 => (First => 5025, Last => 5041),
      --  1112: "12"
      1112 => (First => 162, Last => 163),
      --  1113: "11월"
      1113 => (First => 5042, Last => 5044),
      --  1114: "11月"
      1114 => (First => 5045, Last => 5047),
      --  1115: "11"
      1115 => (First => 5042, Last => 5043),
      --  1116: "10월"
      1116 => (First => 5048, Last => 5050),
      --  1117: "10月"
      1117 => (First => 5051, Last => 5053),
      --  1118: "10"
      1118 => (First => 1508, Last => 1509),
      --  1119: "1"
      1119 => (First => 162, Last => 162),
      --  1120: "0"
      1120 => (First => 161, Last => 161),
      --  1121: "."
      1121 => (First => 499, Last => 499),
      --  1122: "-"
      1122 => (First => 1544, Last => 1544),
      --  1123: ","
      1123 => (First => 1526, Last => 1526),
      --  1124: "+"
      1124 => (First => 5054, Last => 5054),
      --  1125: "'kl'. HH:mm:ss zzzz"
      1125 => (First => 5055, Last => 5073),
      --  1126: "%#,##0"
      1126 => (First => 5074, Last => 5079),
      --  1127: "%"
      1127 => (First => 5074, Last => 5074),
      --  1128: "#E0"
      1128 => (First => 5080, Last => 5082),
      --  1129: "#,##0 %"
      1129 => (First => 5083, Last => 5089),
      --  1130: "#,##0.00 ¤"
      1130 => (First => 5090, Last => 5099),
      --  1131: "#,##0.###;#,##0.###-"
      1131 => (First => 5100, Last => 5119),
      --  1132: "#,##0.###"
      1132 => (First => 5100, Last => 5108),
      --  1133: "#,##0%"
      1133 => (First => 5120, Last => 5125),
      --  1134: "#"
      1134 => (First => 1525, Last => 1525));

   --  END-CLDR-DATA                                                       --
   --  End of the generated data inserted into this file from the          --
   --  Unicode.org CDLR data.                                              --
   --------------------------------------------------------------------------

   procedure Environment_Initialize;
   --  Initialize the current locale based on the ZB_LANG/LANG environment
   --  variables.

   procedure Decompose_Name (Name      : in Wide_String;
                             Language  : out Language_Type;
                             Script    : out Script_Type;
                             Territory : out Territory_Type);
   --  Decompose a locale name, e.g., "en", "en_Latn_US", etc. into it's
   --  component language, script and territory values.

   function Find_Traits (Language  : in Wide_String;
                         Script    : in Wide_String;
                         Territory : in Wide_String) return Trait_Index_Type;
   --  Locate the traits entry "matching" the given locale data (matching
   --  attempts locale resolution, i.e., "fr_FR" => "fr", etc.

   function Latin_Digit (Ch   : in Wide_Character;
                         Zero : in Wide_Character) return Wide_Character;
   --  Convert a localized digit character to the corresponding Latin (ASCII)
   --  digit.  The Zero character gives the zero character for the localized
   --  digits.  E.g., for Arabic Latin_Digit ('٣', '٠') => '3'

   procedure Lookup_Traits (Language  : in Wide_String;
                            Script    : in Wide_String;
                            Territory : in Wide_String;
                            Index     : out Trait_Index_Type;
                            Found     : out Boolean);
   --  Binary search lookup of a traits by tag value.

   function To_String (Index : in String_Index_Type) return Wide_String;
   --  Convert a string index of a pooled string to a string value.

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Locale_Type) return Boolean
   is
   begin
      return      Left.Language_Code  = Right.Language_Code
         and then Left.Script_Code    = Right.Script_Code
         and then Left.Territory_Code = Right.Territory_Code;
   end "=";

   --------------------
   -- Current_Locale --
   --------------------

   function Current_Locale return Locale_Type is
   begin
      return Current_Locale_Value;
   end Current_Locale;

   -----------------
   -- Date_Format --
   -----------------

   function Date_Format (Locale : in Locale_Type;
                         Style  : in Date_Time_Style_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Date_Formats (Style));
   end Date_Format;

   ----------------------
   -- Date_Time_Format --
   ----------------------

   function Date_Time_Format (Locale : in Locale_Type;
                              Style  : in Date_Time_Style_Type)
      return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Date_Time_Formats (Style));
   end Date_Time_Format;

   -------------------------
   -- Day_Period_For_Time --
   -------------------------

   function Day_Period_For_Time (Locale : in Locale_Type;
                                 Hour   : in Hour_Type;
                                 Minute : in Minute_Type;
                                 Second : in Second_Type)
      return Day_Period_Type
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      if Minute = 0 and then Second = 0 then
         return Locale_Data (Index).Exact_Day_Periods (Hour);
      else
         return Locale_Data (Index).Within_Day_Periods (Hour);
      end if;
   end Day_Period_For_Time;

   ---------------------
   -- Day_Period_Name --
   ---------------------

   function Day_Period_Name (Locale     : in Locale_Type;
                             Day_Period : in Day_Period_Type)
      return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Day_Period_Names (Day_Period));
   end Day_Period_Name;

   --------------------
   -- Decompose_Name --
   --------------------

   procedure Decompose_Name (Name      : in Wide_String;
                             Language  : out Language_Type;
                             Script    : out Script_Type;
                             Territory : out Territory_Type)
   is

      Separator : Wide_Character := '_';
      First     : Positive := Name'First;
      Last      : Natural := 0;

      procedure Get_Separated_Item (Result : out Wide_String;
                                    From   : in out Positive;
                                    Last   : in Natural);

      procedure Get_Separated_Item (Result : out Wide_String;
                                    From   : in out Positive;
                                    Last   : in Natural)
      is

         First    : constant Positive := From;
         Position : Natural := First;

      begin
         Find_Separator : loop
            Position := Position + 1;
            exit Find_Separator when Position >= Last
                             or else Name (Position) = Separator;
         end loop Find_Separator;
         From := Position + 1;
         if Position >= Last then
            Position := Last;
         elsif Name (Position) = Separator then
            Position := Position - 1;
         end if;
         Result := Head (Name (First .. Position), Result'Length);
      end Get_Separated_Item;

   begin
      --  If strings contains dashes, assume it's the separator, e.g., "en-us"
      if Index (Name, "-", First) /= 0 then
         Separator := '-';
      end if;
      --  Ignore any encoding info, e.g., "en_US.utf8"
      Last := Index (Name, ".", First);
      if Last = 0 then
         Last := Name'Last;
      else
         Last := Last - 1;
      end if;
      Get_Separated_Item (Language, First, Last);
      Get_Separated_Item (Script, First, Last);
      Get_Separated_Item (Territory, First, Last);
      if Script (Script'Last) = ' ' then
         --  Fix up, the script is really the territory
         Territory := Script (Territory'Range);
         Script := Empty_Script;
      end if;
   end Decompose_Name;

   -----------------------
   -- Delocalize_Digits --
   -----------------------

   function Delocalize_Digits (Locale : in Locale_Type;
                               Value  : in Wide_String) return Wide_String
   is
      Digit_Str : constant Wide_String := Numeric_Item (
                                             Locale,
                                             Decimal_Digits_String);
      Zero      : constant Wide_Character := Digit_Str (Digit_Str'First);
      Result    : Wide_String (Value'Range) := Value;
      Offset    : Integer;
   begin
      for I in Value'Range loop
         Offset := Wide_Character'Pos (Value (I)) - Wide_Character'Pos (Zero);
         if Offset >= 0 and then Offset <= 9 then
            Result (I) := Latin_Digit (Result (I), Zero);
         end if;
      end loop;
      return Result;
   end Delocalize_Digits;

   ----------------------------
   -- Environment_Initialize --
   ----------------------------

   procedure Environment_Initialize is

      use Ada.Environment_Variables;

      ZBLang : constant String := "ZB_LANG";

   begin
      if Exists (ZBLang) then
         Set_Locale (To_Wide_String (Value (ZBLang)));
      else
         Set_Locale (ZanyBlue.OS.OS_Locale_Name);
      end if;
   end Environment_Initialize;

   --------------
   -- Era_Name --
   --------------

   function Era_Name (Locale : in Locale_Type;
                      Era    : in Era_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Era_Names (Era));
   end Era_Name;

   -----------------
   -- Find_Traits --
   -----------------

   function Find_Traits (Language  : in Wide_String;
                         Script    : in Wide_String;
                         Territory : in Wide_String) return Trait_Index_Type
   is
      Result : Trait_Index_Type := 1;
      Found  : Boolean := False;
   begin
      Lookup_Traits (Language, Script, Territory, Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, Script, "", Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, "", Territory, Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, "", "", Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits ("", "", "", Result, Found);
      return Result;
   end Find_Traits;

   -------------------
   -- Full_Day_Name --
   -------------------

   function Full_Day_Name (Locale : in Locale_Type;
                           Day    : in Day_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Full_Day_Names (Day));
   end Full_Day_Name;

   ---------------------
   -- Full_Month_Name --
   ---------------------

   function Full_Month_Name (Locale : in Locale_Type;
                             Month  : in Month_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Full_Month_Names (Month));
   end Full_Month_Name;

   ----------------------
   -- Get_Locale_Codes --
   ----------------------

   procedure Get_Locale_Codes (Locale    : in Locale_Type;
                               Language  : in out Language_Type;
                               Script    : in out Script_Type;
                               Territory : in out Territory_Type)
   is
   begin
      Language := Locale.Language_Code;
      Script := Locale.Script_Code;
      Territory := Locale.Territory_Code;
   end Get_Locale_Codes;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Locale_Type) return Ada.Containers.Hash_Type
   is
   begin
      return Wide_Hash (Locale_Name (Key));
   end Hash;

   -----------------------
   -- Is_Locale_Defined --
   -----------------------

   function Is_Locale_Defined (Language  : in Wide_String;
                               Script    : in Wide_String;
                               Territory : in Wide_String) return Boolean
   is
      Index : Trait_Index_Type;
      Found : Boolean;
   begin
      Lookup_Traits (Language, Script, Territory, Index, Found);
      return Found;
   end Is_Locale_Defined;

   --------------------
   -- Is_Root_Locale --
   --------------------

   function Is_Root_Locale (Locale : in Locale_Type) return Boolean
   is
   begin
      return      Locale.Language_Code  = Empty_Language
         and then Locale.Script_Code    = Empty_Script
         and then Locale.Territory_Code = Empty_Territory;
   end Is_Root_Locale;

   --------------
   -- Language --
   --------------

   function Language (Locale : in Locale_Type) return Wide_String
   is
   begin
      return Non_Blank_Prefix (Locale.Language_Code);
   end Language;

   -----------------
   -- Latin_Digit --
   -----------------

   function Latin_Digit (Ch   : in Wide_Character;
                         Zero : in Wide_Character) return Wide_Character
   is
      Result : Wide_Character := Ch;
   begin
      --  Quick check, might already a Latin digit
      if Zero /= '0' then
         Result := Wide_Character'Val (Wide_Character'Pos ('0')
                     + (Wide_Character'Pos (Ch) - Wide_Character'Pos (Zero)));
      end if;
      return Result;
   end Latin_Digit;

   -------------------
   -- Locale_Digits --
   -------------------

   function Locale_Digits (Locale    : in Locale_Type;
                           Lowercase : in Boolean) return Wide_String
   is
      Result      : Wide_String (1 .. 16);
   begin
      Result (1 .. 10) := Head (Numeric_Item (Locale,
                                              Decimal_Digits_String),
                                10);
      if Lowercase then
         Result (11 .. 16) := "abcdef";
      else
         Result (11 .. 16) := "ABCDEF";
      end if;
      return Result;
   end Locale_Digits;

   ------------------
   -- Locale_Level --
   ------------------

   function Locale_Level (Locale : in Locale_Type) return Level_Type
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Level;
   end Locale_Level;

   -----------------
   -- Locale_Name --
   -----------------

   function Locale_Name (Language       : in Language_Type;
                         Script         : in Script_Type;
                         Territory      : in Territory_Type) return Wide_String
   is

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : in Wide_Character);

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : in Wide_String;
                        Include_UC : in Boolean := True);

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : in Wide_Character)
      is
      begin
         Position := Position + 1;
         Result (Position) := Value;
      end Append;

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : in Wide_String;
                        Include_UC : in Boolean := True)
      is
      begin
         if Value (Value'First) = ' ' then
            return;
         end if;
         if Include_UC then
            Append (Result, Position, '_');
         end if;
         for I in Value'Range loop
            if Value (I) /= ' ' then
               Append (Result, Position, Value (I));
            end if;
         end loop;
      end Append;

      Result   : Wide_String (1 .. 12);
      Position : Natural := 0;

   begin
      if Language (Language'First) /= ' ' then
         Append (Result, Position, Language,
                 Include_UC => False);
         Append (Result, Position, Script);
         Append (Result, Position, Territory);
      end if;
      return Result (1 .. Position);
   end Locale_Name;

   -----------------
   -- Locale_Name --
   -----------------

   function Locale_Name (Locale : in Locale_Type) return Wide_String
   is
   begin
      return Locale_Name (Locale.Language_Code,
                          Locale.Script_Code,
                          Locale.Territory_Code);
   end Locale_Name;

   -------------------
   -- Lookup_Traits --
   -------------------

   procedure Lookup_Traits (Language  : in Wide_String;
                            Script    : in Wide_String;
                            Territory : in Wide_String;
                            Index     : out Trait_Index_Type;
                            Found     : out Boolean)
   is

      Key : Tag_Type := Head (Language, Max_Language_Length)
                      & Head (Script, Max_Script_Length)
                      & Head (Territory, Max_Territory_Length);

      Left      : Trait_Index_Type := Locale_Data'First;
      Right     : Trait_Index_Type := Locale_Data'Last + 1;
      Center    : Trait_Index_Type;
      Candidate : Tag_Type;

   begin
      ASCII_Uppercase (Key);
      Found := False;
      if Key < Locale_Data (Left).Tag then
         return;
      end if;
      loop
         Center := Left + (Right - Left) / 2;
         Candidate := Locale_Data (Center).Tag;
         if Key = Candidate then
            Index := Center;
            Found := True;
            return;
         end if;

         if Right - Left <= 1 then
            return;
         elsif Key < Candidate then
            Right := Center;
         else
            Left := Center;
         end if;
      end loop;
   end Lookup_Traits;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale (Locale_String  : in Wide_String) return Locale_Type
   is

      Language  : Language_Type;
      Script    : Script_Type;
      Territory : Territory_Type;

   begin
      Decompose_Name (Locale_String, Language, Script, Territory);
      return Make_Locale (Language, Script, Territory);
   end Make_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale (Language  : in Wide_String;
                         Territory : in Wide_String) return Locale_Type
   is
   begin
      return Make_Locale (Language, "", Territory);
   end Make_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale (Language  : in Wide_String;
                         Script    : in Wide_String;
                         Territory : in Wide_String) return Locale_Type
   is
   begin
      return Result : Locale_Type do
         Result.Language_Code := Head (Language, Max_Language_Length);
         Result.Script_Code := Head (Script, Max_Script_Length);
         Result.Territory_Code := Head (Territory, Max_Territory_Length);
         ASCII_Lowercase (Result.Language_Code);
         ASCII_Capitalize (Result.Script_Code);
         ASCII_Uppercase (Result.Territory_Code);
         Result.Traits_Index := Find_Traits (Result.Language_Code,
                                             Result.Script_Code,
                                             Result.Territory_Code);
      end return;
   end Make_Locale;

   -------------------------------
   -- Number_Of_Defined_Locales --
   -------------------------------

   function Number_Of_Defined_Locales return Positive is
   begin
      return Locale_Data'Length;
   end Number_Of_Defined_Locales;

   --------------------
   -- Numeric_Format --
   --------------------

   function Numeric_Format (Locale : in Locale_Type;
                            Style  : in Numeric_Style_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Numeric_Formats (Style));
   end Numeric_Format;

   ------------------
   -- Numeric_Item --
   ------------------

   function Numeric_Item (Locale : in Locale_Type;
                          Item   : in Numeric_Item_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Numeric_Items (Item));
   end Numeric_Item;

   ------------------
   -- Parent_Codes --
   ------------------

   procedure Parent_Codes (
      Language       : in out Language_Type;
      Script         : in out Script_Type;
      Territory      : in out Territory_Type;
      Base_Territory : in Territory_Type := Empty_Territory)
   is

      Language_P     : constant Boolean := Language (1) /= ' ';
      Script_P       : constant Boolean := Script (1) /= ' ';
      Territory_P    : constant Boolean := Territory (1) /= ' ';
      B_Territory_P  : constant Boolean := Base_Territory (1) /= ' ';

   begin
      if Language_P
           and then Script_P
           and then Territory_P then
         Territory := Empty_Territory;
         return;
      end if;

      if Language_P
           and then Script_P
           and then not Territory_P
           and then B_Territory_P then
         Script := Empty_Script;
         Territory := Base_Territory;
         return;
      end if;

      if Language_P
           and then Script_P
           and then not Territory_P
           and then not B_Territory_P then
         Script := Empty_Script;
         Territory := Empty_Territory;
         return;
      end if;

      if Language_P
           and then not Script_P
           and then Territory_P then
         Script := Empty_Script;
         Territory := Empty_Territory;
         return;
      end if;

      Language := Empty_Language;
      Script := Empty_Script;
      Territory := Empty_Territory;
   end Parent_Codes;

   ------------
   -- Script --
   ------------

   function Script (Locale : in Locale_Type) return Wide_String
   is
   begin
      return Non_Blank_Prefix (Locale.Script_Code);
   end Script;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Locale : in Locale_Type)
   is
   begin
      Current_Locale_Value := Locale;
   end Set_Locale;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Name : in String) is
   begin
      Set_Locale (To_Wide_String (Name));
   end Set_Locale;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Wide_Name : in Wide_String)
   is
   begin
      Set_Locale (Make_Locale (Wide_Name));
   end Set_Locale;

   ----------------
   -- Set_Traits --
   ----------------

   procedure Set_Traits (Locale : in out Locale_Type;
                         Name   : in String)
   is
   begin
      Set_Traits (Locale, To_Wide_String (Name));
   end Set_Traits;

   ----------------
   -- Set_Traits --
   ----------------

   procedure Set_Traits (Locale    : in out Locale_Type;
                         Wide_Name : in Wide_String)
   is
      Language  : Language_Type;
      Script    : Script_Type;
      Territory : Territory_Type;
   begin
      Decompose_Name (Wide_Name, Language, Script, Territory);
      Locale.Traits_Index := Find_Traits (Language, Script, Territory);
   end Set_Traits;

   --------------------
   -- Short_Day_Name --
   --------------------

   function Short_Day_Name (Locale : in Locale_Type;
                            Day    : in Day_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Short_Day_Names (Day));
   end Short_Day_Name;

   ----------------------
   -- Short_Month_Name --
   ----------------------

   function Short_Month_Name (Locale : in Locale_Type;
                              Month  : in Month_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Short_Month_Names (Month));
   end Short_Month_Name;

   ---------------
   -- Territory --
   ---------------

   function Territory (Locale : in Locale_Type) return Wide_String
   is
   begin
      return Non_Blank_Prefix (Locale.Territory_Code);
   end Territory;

   -----------------
   -- Text_Layout --
   -----------------

   function Text_Layout (Locale : in Locale_Type) return Text_Layout_Type
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Text_Layout;
   end Text_Layout;

   -----------------
   -- Time_Format --
   -----------------

   function Time_Format (Locale : in Locale_Type;
                         Style  : in Date_Time_Style_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Time_Formats (Style));
   end Time_Format;

   ---------------
   -- To_String --
   ---------------

   function To_String (Index : in String_Index_Type) return Wide_String
   is
      Address : constant String_Address_Type := String_Addresses (Index);
   begin
      return Pool (Address.First .. Address.Last);
   end To_String;

   -----------------
   -- Traits_Name --
   -----------------

   function Traits_Name (Locale : in Locale_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Name);
   end Traits_Name;

   ----------------
   -- Traits_Tag --
   ----------------

   function Traits_Tag (Locale : in Locale_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Tag;
   end Traits_Tag;

   --------------------------
   -- Transfer_Locale_Data --
   --------------------------

   function Transfer_Locale_Data (Source_Locale : in Locale_Type;
                                  Extra_Data    : in Locale_Type)
      return Locale_Type
   is
      S_L, E_L : Language_Type;
      S_S, E_S : Script_Type;
      S_T, E_T : Territory_Type;
   begin
      Get_Locale_Codes (Source_Locale, S_L, S_S, S_T);
      Get_Locale_Codes (Extra_Data, E_L, E_S, E_T);
      if S_L = Empty_Language then
         S_L := E_L;
      end if;
      if S_L /= Empty_Language and then S_S = Empty_Script then
         S_S := E_S;
      end if;
      if S_L /= Empty_Language and then S_T = Empty_Territory then
         S_T := E_T;
      end if;
      return Make_Locale (S_L, S_S, S_T);
   end Transfer_Locale_Data;

begin  --  ZanyBlue.Text.Locales
   Environment_Initialize;
end ZanyBlue.Text.Locales;
