--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Wide_Characters.Handling;
with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Text.Buffer;
with ZanyBlue.Utils.DateTimes;

package body ZanyBlue.Text.Times is

   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Utils.DateTimes;

   function Format_Value (Format_String : Wide_String;
                          Data          : Time;
                          TZ_Offset     : Time_Offset;
                          Locale        : Locale_Type) return Wide_String;
   --  Apply the date/time format string which includes the Unicode.org
   --  value strings, e.g., EEEE for the full day name.

   function Compose_Values (Format_String : Wide_String;
                            Date_String   : Wide_String;
                            Time_String   : Wide_String) return Wide_String;
   --  Compose the date and time values into a single date-time string
   --  based on the formatting information from Unicode.org.

   function To_Month (Number : Month_Number) return Month_Type;
   --  Convert a Month_Number to the Month_Type enumeration.

   --------------------
   -- Compose_Values --
   --------------------
   --
   --  Since the values composed are already formatted, the quote characters
   --  in the format string can simply be ignored.
   --

   function Compose_Values (Format_String : Wide_String;
                            Date_String   : Wide_String;
                            Time_String   : Wide_String) return Wide_String
   is

      Last   : constant Positive := Format_String'Last;
      Result : Unbounded_Wide_String;
      I      : Positive := Format_String'First;

   begin
      while I <= Last loop
         if I <= Last - 2
           and then Format_String (I .. I + 2) = "{0}"
         then
            Append (Result, Time_String);
            I := I + 2;
         elsif I <= Last - 2
           and then Format_String (I .. I + 2) = "{1}"
         then
            Append (Result, Date_String);
            I := I + 2;
         elsif Format_String (I) /= ''' then
            Append (Result, Format_String (I));
         end if;
         I := I + 1;
      end loop;
      return To_Wide_String (Result);
   end Compose_Values;

   ------------
   -- Create --
   ------------

   function Create (Time_Value : Time) return Time_Argument_Type is
   begin
      return Create (Time_Value, UTC_Time_Offset (Date => Time_Value));
   exception
   when Unknown_Zone_Error =>
      return Create (Time_Value, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Time_Value : Time;
                    TZ_Offset  : Time_Offset) return Time_Argument_Type is
   begin
      return Time_Argument_Type'(Data => Time_Value,
                                 TZ_Offset => TZ_Offset);
   end Create;

   ------------
   -- Format --
   ------------

   overriding
   function Format (Value     : Time_Argument_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is

      T : constant Time := Value.Data;
      Z : constant Time_Offset := Value.TZ_Offset;

   begin
      if Type_Name = "time" then
         if Template = "full" then
            return Format_Value (Time_Format (Locale, Full), T, Z, Locale);
         elsif Template = "long" then
            return Format_Value (Time_Format (Locale, Long), T, Z, Locale);
         elsif Template = "medium" then
            return Format_Value (Time_Format (Locale, Medium), T, Z, Locale);
         elsif Template = "short" or else Template'Length = 0 then
            return Format_Value (Time_Format (Locale, Short), T, Z, Locale);
         end if;
      elsif Type_Name = "date" then
         if Template = "full" then
            return Format_Value (Date_Format (Locale, Full), T, Z, Locale);
         elsif Template = "long" then
            return Format_Value (Date_Format (Locale, Long), T, Z, Locale);
         elsif Template = "medium" then
            return Format_Value (Date_Format (Locale, Medium), T, Z, Locale);
         elsif Template = "short" or else Template'Length = 0 then
            return Format_Value (Date_Format (Locale, Short), T, Z, Locale);
         end if;
      elsif Type_Name = "datetime" or Type_Name = "" then
         if Template = "full" then
            return Compose_Values (Date_Time_Format (Locale, Full),
                                   Format_Value (Date_Format (Locale, Full),
                                                       T, Z, Locale),
                                   Format_Value (Time_Format (Locale, Full),
                                                       T, Z, Locale));
         elsif Template = "long" then
            return Compose_Values (Date_Time_Format (Locale, Long),
                                   Format_Value (Date_Format (Locale, Long),
                                                 T, Z, Locale),
                                   Format_Value (Time_Format (Locale, Long),
                                                 T, Z, Locale));
         elsif Template = "medium" then
            return Compose_Values (Date_Time_Format (Locale, Medium),
                                   Format_Value (Date_Format (Locale, Medium),
                                                 T, Z, Locale),
                                   Format_Value (Time_Format (Locale, Medium),
                                                 T, Z, Locale));
         elsif Template = "short" or else Template'Length = 0 then
            return Compose_Values (Date_Time_Format (Locale, Short),
                                   Format_Value (Date_Format (Locale, Short),
                                                 T, Z, Locale),
                                   Format_Value (Time_Format (Locale, Short),
                                                 T, Z, Locale));
         end if;
      end if;
      return Format_Value (Template, T, Z, Locale);
   end Format;

   ------------------
   -- Format_Value --
   ------------------

   function Format_Value (Format_String : Wide_String;
                          Data          : Time;
                          TZ_Offset     : Time_Offset;
                          Locale        : Locale_Type) return Wide_String is

      use Ada.Wide_Characters.Handling;

      End_Of_Fmt : constant Positive := Format_String'Last;
      Quote_Ch   : constant Wide_Character := ''';
      DateTime   : DateTime_Type := Make_DateTime (Data);
      Position   : Natural := Format_String'First;
      Buffer     : Buffer_Type;
      Quoted     : Boolean := False;
      Seq_Length : Positive;
      Ch         : Wide_Character;

      procedure Lower_A (Seq_Length : in out Positive);
      procedure Lower_B (Seq_Length : in out Positive);
      procedure Lower_C (Seq_Length : in out Positive);
      procedure Lower_D (Seq_Length : in out Positive);
      procedure Lower_E (Seq_Length : in out Positive);
      procedure Lower_G (Seq_Length : Positive);
      procedure Lower_H (Seq_Length : in out Positive);
      procedure Lower_L (Seq_Length : Positive);
      procedure Lower_M (Seq_Length : in out Positive);
      procedure Lower_Q (Seq_Length : in out Positive);
      procedure Lower_R (Seq_Length : Positive);
      procedure Lower_S (Seq_Length : in out Positive);
      procedure Lower_U (Seq_Length : Positive);
      procedure Lower_W (Seq_Length : in out Positive);
      procedure Lower_Y (Seq_Length : Positive);
      procedure Lower_Z (Seq_Length : in out Positive);

      procedure Upper_A (Seq_Length : Positive);
      procedure Upper_B (Seq_Length : in out Positive);
      procedure Upper_D (Seq_Length : in out Positive);
      procedure Upper_E (Seq_Length : in out Positive);
      procedure Upper_F (Seq_Length : Positive);
      procedure Upper_G (Seq_Length : in out Positive);
      procedure Upper_H (Seq_Length : in out Positive);
      procedure Upper_M (Seq_Length : in out Positive);
      procedure Upper_L (Seq_Length : in out Positive);
      procedure Upper_O (Seq_Length : in out Positive);
      procedure Upper_Q (Seq_Length : in out Positive);
      procedure Upper_S (Seq_Length : Positive);
      procedure Upper_Y (Seq_Length : Positive);
      procedure Upper_W (Seq_Length : Positive);
      procedure Upper_Z (Seq_Length : in out Positive);

      -------------
      -- Lower_A --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-period
      --
      --  period
      --  a..aaa        am. [e.g. 12 am.]       Abbreviated      AM, PM
      --     May be upper or lowercase depending on the locale and other
      --     options. The wide form may be the same as the short form if the
      --     "real" long form (eg ante meridiem) is not customarily used. The
      --     narrow form must be unique, unlike some other fields. See also
      --     Section 9 Parsing Dates and Times.
      --  aaaa          am. [e.g. 12 am.]       Wide
      --  aaaaa         a [e.g. 12a]            Narrow
      --

      procedure Lower_A (Seq_Length : in out Positive) is
         Period : constant Day_Period_Type :=
            (if DateTime.Hour < 12 then AM else PM);
      begin
         Seq_Length := Positive'Min (Seq_Length, 5);
         case Seq_Length is
         when 1 | 2 | 3 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Abbreviated));
         when 4 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Wide));
         when 5 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Narrow));
         when others =>
            null;
         end case;
      end Lower_A;

      -------------
      -- Lower_B --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-period
      --
      --  period
      --
      --  b..bbb   mid. [e.g. 12 mid.]           Abbreviated
      --  bbbb     midnight [e.g. 12 midnight]   Wide
      --  bbbbb    md [e.g. 12 md]               Narrow
      --
      --  am, pm, noon, midnight
      --  May be upper or lowercase depending on the locale and other
      --  options. If the locale doesn't the notion of a unique "noon"
      --  = 12:00, then the PM form may be substituted. Similarly for
      --  "midnight" = 00:00 and the AM form. The narrow form must be
      --  unique, unlike some other fields.
      --
      --  TODO: No midnight for the CLDR version currently in use?
      --

      procedure Lower_B (Seq_Length : in out Positive) is
         Period : Day_Period_Type;
      begin
         Seq_Length := Positive'Min (Seq_Length, 5);
         if DateTime.Is_Noon then
            Period := Noon;
         elsif DateTime.Hour = 0 or else DateTime.Hour > 12 then
            Period := PM;
         else
            Period := AM;
         end if;
         case Seq_Length is
         when 1 | 2 | 3 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Abbreviated));
         when 4 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Wide));
         when 5 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Narrow));
         when others =>
            null;
         end case;
      end Lower_B;

      -------------
      -- Lower_C --
      -------------
      --
      --  From
      --    https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-weekday
      --
      --  day of the week
      --
      --  c        2         Numeric: 1 digit
      --  cc       2         Numeric: 1 digit
      --  ccc      Tue       Abbreviated
      --  cccc     Tuesday   Wide
      --  ccccc    T         Narrow
      --  cccccc   Tu        Short
      --
      --  Local day of week number/name, format style. Same as E except
      --  adds a numeric value that will depend on the local starting
      --  day of the week. For this example, Monday is the first day of
      --  the week.
      --

      procedure Lower_C (Seq_Length : in out Positive) is
         DIW : constant Day_Type := DateTime.Day_In_Week;
      begin
         Seq_Length := Positive'Min (Seq_Length, 6);
         case Seq_Length is
         when 1 | 2 =>
            --  TODO: Determine day number based on locale start of week
            Accumulate (Buffer, Day_Type'Pos (DIW), Locale);
         when 3 =>
            Add (Buffer, Day_Name (Locale, DIW, Abbreviated));
         when 4 =>
            Add (Buffer, Day_Name (Locale, DIW, Wide));
         when 5 =>
            Add (Buffer, Day_Name (Locale, DIW, Narrow));
         when 6 =>
            Add (Buffer, Day_Name (Locale, DIW, Short));
         when others =>
            null;
         end case;
      end Lower_C;

      -------------
      -- Lower_D --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-day
      --
      --  day
      --  d      1       Numeric: minimum digits       Day of month (numeric).
      --  dd     01      Numeric: 2 digits, zero pad if needed
      --

      procedure Lower_D (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 2);
         Accumulate (Buffer, Integer (DateTime.Day), Locale,
                     Width => Seq_Length);
      end Lower_D;

      -------------
      -- Lower_E --
      -------------
      --
      --  From
      --    https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-weekday
      --
      --  day of the week
      --
      --  e        2         Numeric: 1 digit
      --  ee       02        Numeric: 2 digits + zero pad
      --  eee      Tue       Abbreviated
      --  eeee     Tuesday   Wide
      --  eeeee    T         Narrow
      --  eeeeee   Tu        Short
      --
      --  Local day of week number/name, format style. Same as E except
      --  adds a numeric value that will depend on the local starting
      --  day of the week. For this example, Monday is the first day of
      --  the week.
      --

      procedure Lower_E (Seq_Length : in out Positive) is
         DIW : constant Day_Type := DateTime.Day_In_Week;
      begin
         Seq_Length := Positive'Min (Seq_Length, 6);
         case Seq_Length is
         when 1 | 2 =>
            --  TODO: Determine day number based on locale start of week
            Accumulate (Buffer, Day_Type'Pos (DIW), Locale,
                        Width => Seq_Length);
         when 3 =>
            Add (Buffer, Short_Day_Name (Locale, DIW));
         when 4 =>
            Add (Buffer, Full_Day_Name (Locale, DIW));
         when 5 =>
            --  TODO: Narrow names
            Add (Buffer, Short_Day_Name (Locale, DIW));
         when 6 =>
            --  TODO: Short names
            Add (Buffer, Short_Day_Name (Locale, DIW));
         when others =>
            null;
         end case;
      end Lower_E;

      -------------
      -- Lower_G --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-era
      --
      --  day
      --  g+      2451334   Modified Julian day (numeric).
      --
      --  This is different from the conventional Julian day number in
      --  two regards. First, it demarcates days at local zone midnight,
      --  rather than noon GMT. Second, it is a local number; that is,
      --  it depends on the local time zone. It can be thought of as a
      --  single number that encompasses all the date-related fields.The
      --  field length specifies the minimum number of digits, with
      --  zero-padding as necessary.
      --

      procedure Lower_G (Seq_Length : Positive) is
      begin
         Accumulate (Buffer, DateTime.Modified_Julian_Day, Locale,
                     Width => Seq_Length);
      end Lower_G;

      -------------
      -- Lower_H --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-hour
      --
      --  hour
      --  h      1, 12   Numeric: minimum digits
      --  hh    01, 12   Numeric: 2 digits, zero pad if needed
      --
      --  Hour [1-12]. When used in skeleton data or in a skeleton passed in
      --  an API for flexible date pattern generation, it should match the
      --  12-hour-cycle format preferred by the locale (h or K); it should
      --  not match a 24-hour-cycle format (H or k).
      --

      procedure Lower_H (Seq_Length : in out Positive) is
         Value : Hour_Type := DateTime.Hour rem 12;
      begin
         Seq_Length := Positive'Min (Seq_Length, 2);
         if Value = 0 then
            Value := 12;
         end if;
         Accumulate (Buffer, Value, Locale, Width => Seq_Length);
      end Lower_H;

      -------------
      -- Lower_L --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-month
      --
      --  month
      --  l      [nothing]
      --
      --  This pattern character is deprecated, and should be ignored in
      --  patterns. It was originally intended to be used in combination
      --  with M to indicate placement of the symbol for leap month in
      --  the Chinese calendar. Placement of that marker is now specified
      --  using locale-specific <monthPatterns> data, and formatting and
      --  parsing of that marker should be handled as part of supporting
      --  the regular M and L pattern characters.
      --

      procedure Lower_L (Seq_Length : Positive) is
      begin
         null;
      end Lower_L;

      -------------
      -- Lower_M --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-minute
      --
      --  minute
      --  m      8, 59   Numeric: minimum digits
      --  mm    08, 59   Numeric: 2 digits, zero pad if needed
      --
      --  Minute (numeric). Truncated, not rounded.
      --

      procedure Lower_M (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 2);
         Accumulate (Buffer, DateTime.Minute, Locale, Width => Seq_Length);
      end Lower_M;

      -------------
      -- Lower_Q --
      -------------
      --
      --  From
      --    https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-quarter
      --
      --  quarter
      --  q      2            Numeric: 1 digit
      --  qq     02           Numeric: 2 digits + zero pad
      --  qqq    Q2           Abbreviated
      --  qqqq   2nd quarter  Wide
      --  qqqqq  2            Narrow
      --
      --  Quarter number/name.
      --

      procedure Lower_Q (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 5);
         case Seq_Length is
         when 1 =>
            Accumulate (Buffer, DateTime.Quarter, Locale);
         when 2 =>
            Accumulate (Buffer, DateTime.Quarter, Locale, Width => 2);
         when 3 =>
            Add (Buffer, Quarter_Name (Locale, DateTime.Quarter, Abbreviated));
         when 4 =>
            Add (Buffer, Quarter_Name (Locale, DateTime.Quarter, Wide));
         when 5 =>
            Add (Buffer, Quarter_Name (Locale, DateTime.Quarter, Narrow));
         when others =>
            null;
         end case;
      end Lower_Q;

      -------------
      -- Lower_R --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-year
      --
      --  year
      --  r+      2017      Related Gregorian year (numeric).
      --
      --  For non-Gregorian calendars, this corresponds to the
      --  extended Gregorian year in which the calendar’s year
      --  begins. Related Gregorian years are often displayed, for
      --  example, when formatting dates in the Japanese calendar —
      --  e.g. "2012(平成24)年1月15日" — or in the Chinese calendar
      --  — e.g. "2012壬辰年腊月初四". The related Gregorian
      --  year is usually displayed using the "latn" numbering system,
      --  regardless of what numbering systems may be used for other
      --  parts of the formatted date. If the calendar’s year is linked
      --  to the solar year (perhaps using leap months), then for that
      --  calendar the 'r' year will always be at a fixed offset from the
      --  'u' year. For the Gregorian calendar, the 'r' year is the same
      --  as the 'u' year. For 'r', all field lengths specify a minimum
      --  number of digits; there is no special interpretation for "rr".
      --

      procedure Lower_R (Seq_Length : Positive) is
      begin
         Accumulate (Buffer, DateTime.Year, Root_Locale, Width => Seq_Length);
      end Lower_R;

      -------------
      -- Lower_S --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-second
      --
      --  second
      --  s      8, 12   Numeric: minimum digits
      --  ss    08, 12   Numeric: 2 digits, zero pad if needed
      --
      --  Second (numeric). Truncated, not rounded.
      --

      procedure Lower_S (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 2);
         Accumulate (Buffer, DateTime.Second, Locale, Width => Seq_Length);
      end Lower_S;

      -------------
      -- Lower_U --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-year
      --
      --  year
      --  u+      4601  Extended year (numeric).
      --
      --  This is a single number designating the year of this calendar
      --  system, encompassing all supra-year fields. For example,
      --  for the Julian calendar system, year numbers are positive,
      --  with an era of BCE or CE. An extended year value for the
      --  Julian calendar system assigns positive values to CE years and
      --  negative values to BCE years, with 1 BCE being year 0. For
      --  "u", all field lengths specify a minimum number of digits;
      --  there is no special interpretation for “uu”.
      --

      procedure Lower_U (Seq_Length : Positive) is
      begin
         Accumulate (Buffer, DateTime.Year, Locale, Width => Seq_Length);
      end Lower_U;

      -------------
      -- Lower_W --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-week
      --
      --  week
      --  w       8, 27     Numeric: minimum digits
      --  ww      08, 27    Numeric: 2 digits, zero pad if needed
      --
      --  Week of Year (numeric). When used in a pattern with year, use 'Y'
      --  for the year field instead of 'y'.
      --

      procedure Lower_W (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 2);
         Accumulate (Buffer, DateTime.Week_In_Year, Locale,
                     Width => Seq_Length);
      end Lower_W;

      -------------
      -- Lower_Y --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-year
      --
      --  year
      --  y      2, 20, 201, 2017, 20173
      --  yy     02, 20, 01, 17, 73
      --  yyy    002, 020, 201, 2017, 20173
      --  yyyy   0002, 0020, 0201, 2017, 20173
      --  yyyyy+ ...
      --
      --  Calendar year (numeric). In most cases the length of the
      --  y field specifies the minimum number of digits to display,
      --  zero-padded as necessary; more digits will be displayed if
      --  needed to show the full year. However, "yy" requests just the
      --  two low-order digits of the year, zero-padded as necessary. For
      --  most use cases, "y" or "yy" should be adequate.
      --

      procedure Lower_Y (Seq_Length : Positive) is
         Value : Integer := DateTime.Year;
      begin
         if Seq_Length = 2 then
            Value := Value rem 100;
         end if;
         Accumulate (Buffer, Value, Locale, Width => Seq_Length);
      end Lower_Y;

      -------------
      -- Lower_Z --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-zone
      --
      --  zone
      --  z..zzz   PDT                    The short specific non-location
      --                                  format. Where that is unavailable,
      --                                  falls back to the short localized
      --                                  GMT format ("O").
      --  zzzz     Pacific Daylight Time  The long specific non-location
      --                                  format. Where that is unavailable,
      --                                  falls back to the long localized GMT
      --                                  format ("OOOO").
      --

      procedure Lower_Z (Seq_Length : in out Positive) is
         O_Length : Positive := 1;
      begin
         Seq_Length := Positive'Min (Seq_Length, 4);
         case Seq_Length is
         when 1 | 2 | 3 =>
            O_Length := 1;
         when 4 =>
            O_Length := 4;
         when others =>
            null;
         end case;
         Upper_O (O_Length);
      end Lower_Z;

      -------------
      -- Upper_A --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-second
      --
      --  seconds
      --
      --  A+       69540000   Milliseconds in day (numeric).
      --
      --  This field behaves exactly like a composite of all time-related
      --  fields, not including the zone fields. As such, it also reflects
      --  discontinuities of those fields on DST transition days. On a day
      --  of DST onset, it will jump forward. On a day of DST cessation,
      --  it will jump backward. This reflects the fact that is must be
      --  combined with the offset field to obtain a unique local time
      --  value. The field length specifies the minimum number of digits,
      --  with zero-padding as necessary.
      --

      procedure Upper_A (Seq_Length : Positive) is
         Value : constant Integer := Integer (DateTime.Seconds * 1000.0);
      begin
         Accumulate (Buffer, Value, Locale, Width => Seq_Length);
      end Upper_A;

      -------------
      -- Upper_B --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-period
      --
      --  period
      --
      --  B..BBB   at night [e.g. 3:00 at night]   Abbreviated
      --  BBBB     at night [e.g. 3:00 at night]   Wide
      --  BBBBB    at night [e.g. 3:00 at night]   Narrow
      --
      --  flexible day periods
      --
      --  May be upper or lowercase depending on the locale and other
      --  options. Often there is only one width that is customarily used.
      --

      procedure Upper_B (Seq_Length : in out Positive) is
         Period : constant Day_Period_Type := Day_Period_For_Time (
                                                 Locale,
                                                 DateTime.Hour,
                                                 DateTime.Minute,
                                                 DateTime.Second);
      begin
         Seq_Length := Positive'Min (Seq_Length, 5);
         case Seq_Length is
         when 1 | 2 | 3 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Abbreviated));
         when 4 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Wide));
         when 5 =>
            Add (Buffer, Day_Period_Name (Locale, Period, Narrow));
         when others =>
            null;
         end case;
      end Upper_B;

      -------------
      -- Upper_D --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-day
      --
      --  day
      --  D...DDD    345   Day of year (numeric).
      --
      --  The field length specifies the minimum number of digits,
      --  with zero-padding as necessary.
      --

      procedure Upper_D (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 3);
         Accumulate (Buffer, DateTime.Day_Of_Year, Locale,
                     Width => Seq_Length);
      end Upper_D;

      -------------
      -- Upper_E --
      -------------
      --
      --  From
      --    https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-weekday
      --
      --  day of the week
      --    E..EEE   Tue       Abbreviated
      --    EEEE     Tuesday   Wide
      --    EEEEE    T         Narrow
      --    EEEEEE   Tu        Short
      --
      --  Day of week name, format style.
      --

      procedure Upper_E (Seq_Length : in out Positive) is
         DIW : constant Day_Type := DateTime.Day_In_Week;
      begin
         Seq_Length := Positive'Min (Seq_Length, 6);
         case Seq_Length is
         when 1 | 2 | 3 =>
            Add (Buffer, Short_Day_Name (Locale, DIW));
         when 4 =>
            Add (Buffer, Full_Day_Name (Locale, DIW));
         when 5 =>
            --  TODO: Narrow days
            Add (Buffer, Full_Day_Name (Locale, DIW));
         when 6 =>
            --  TODO: Short days
            Add (Buffer, Full_Day_Name (Locale, DIW));
         when others =>
            null;
         end case;
      end Upper_E;

      -------------
      -- Upper_F --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-day
      --
      --  day
      --  F     2   Day of Week in Month (numeric).
      --
      --  The example is for the 2nd Wed in July
      --

      procedure Upper_F (Seq_Length : Positive) is
      begin
         for I in 1 .. Seq_Length loop
            Accumulate (Buffer, DateTime.Day_Of_Week_In_Month,
                        Locale, Width => 2);
         end loop;
      end Upper_F;

      -------------
      -- Upper_G --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-era
      --
      --  era
      --  G..GGG     AD [variant: CE]                   Abbreviated
      --  GGGG       Anno Domini [variant: Common Era]  Wide
      --  GGGGG      A                                  Narrow
      --
      --  Era name. Era string for the current date.
      --  Ada date time only supports CE
      --

      procedure Upper_G (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 5);
         case Seq_Length is
         when 1 | 2 | 3 =>
            Add (Buffer, Era_Name (Locale, CE, Abbreviated));
         when 4 =>
            Add (Buffer, Era_Name (Locale, CE, Wide));
         when 5 =>
            Add (Buffer, Era_Name (Locale, CE, Narrow));
         when others =>
            null;
         end case;
      end Upper_G;

      -------------
      -- Upper_H --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-hour
      --
      --  hour
      --
      --  H    0, 23    Numeric: minimum digits   Hour [0-23].
      --  HH   00, 23   Numeric: 2 digits, zero pad if needed
      --
      --  When used in skeleton data or in a skeleton passed in an
      --  API for flexible date pattern generation, it should match
      --  the 24-hour-cycle format preferred by the locale (H or k);
      --  it should not match a 12-hour-cycle format (h or K).
      --

      procedure Upper_H (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 2);
         Accumulate (Buffer, DateTime.Hour, Locale, Width => Seq_Length);
      end Upper_H;

      -------------
      -- Upper_L --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-month
      --
      --  month
      --
      --  See Upper_M
      --

      procedure Upper_L (Seq_Length : in out Positive) is
      begin
         Upper_M (Seq_Length);
      end Upper_L;

      -------------
      -- Upper_M --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-month
      --
      --  month
      --  M       9, 12       Numeric: minimum digits
      --  MM      09, 12      Numeric: 2 digits, zero pad if needed
      --  MMM     Sep         Abbreviated
      --  MMMM    September   Wide
      --  MMMMM   S           Narrow
      --
      --  Month number/name, format style (intended to be used in
      --  conjunction with 'd' for day number).
      --

      procedure Upper_M (Seq_Length : in out Positive) is
      begin
         Seq_Length := Positive'Min (Seq_Length, 5);
         case Seq_Length is
         when 1 =>
            Accumulate (Buffer, Integer (DateTime.Month), Locale);
         when 2 =>
            Accumulate (Buffer, Integer (DateTime.Month), Locale, Width => 2);
         when 3 =>
            Add (Buffer,
                 Month_Name (Locale, To_Month (DateTime.Month), Abbreviated));
         when 4 =>
            Add (Buffer,
                 Month_Name (Locale, To_Month (DateTime.Month), Wide));
         when 5 =>
            Add (Buffer,
                 Month_Name (Locale, To_Month (DateTime.Month), Narrow));
         when others =>
            null;
         end case;
      end Upper_M;

      -------------
      -- Upper_O --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-zone
      --
      --  zone
      --  O       GMT-8      The short localized GMT format.
      --  OOOO    GMT-08:00  The long localized GMT format.
      --

      procedure Upper_O (Seq_Length : in out Positive) is
         S : constant Wide_Character := (if TZ_Offset < 0 then '-' else '+');
         Abs_TZ_Offset : constant Integer := abs Integer (TZ_Offset);
         H_Offset : constant Integer := Abs_TZ_Offset / 60;
         M_Offset : constant Integer := Abs_TZ_Offset mod 60;
      begin
         Seq_Length := Positive'Min (Seq_Length, 4);
         if Seq_Length < 4 then
            Seq_Length := 1;
         end if;
         Add (Buffer, "GMT");
         Add (Buffer, S);
         case Seq_Length is
         when 1 =>
            Accumulate (Buffer, H_Offset, Locale);
            if M_Offset > 0 then
               Add (Buffer, ':');
               Accumulate (Buffer, M_Offset, Locale, Width => 2);
            end if;
         when 4 =>
            Accumulate (Buffer, H_Offset, Locale, Width => 2);
            Add (Buffer, ':');
            Accumulate (Buffer, M_Offset, Locale, Width => 2);
         when others =>
            null;
         end case;
      end Upper_O;

      -------------
      -- Upper_Q --
      -------------
      --
      --  From
      --     https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-quarter
      --
      --  quarter
      --
      --  See Lower_Q
      --

      procedure Upper_Q (Seq_Length : in out Positive) is
      begin
         Lower_Q (Seq_Length);
      end Upper_Q;

      -------------
      -- Upper_S --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-second
      --
      --  S+     3456     Fractional Second (numeric).
      --
      --  Truncates, like other numeric time fields, but in this case to
      --  the number of digits specified by the field length. (Example
      --  shows display using pattern SSSS for seconds value 12.34567)
      --

      procedure Upper_S (Seq_Length : Positive) is
         Value : Float := DateTime.Fraction_Seconds;
      begin
         for I in 1 .. Seq_Length loop
            Value := Value * 10.0;
         end loop;
         Value := Float'Floor (Value);
         Accumulate (Buffer, Integer (Value), Locale, Width => Seq_Length);
      end Upper_S;

      -------------
      -- Upper_W --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-week
      --
      --  week
      --  W       3        Numeric: minimum digits
      --
      --  Week of month (numeric).
      --

      procedure Upper_W (Seq_Length : Positive) is
      begin
         for I in 1 .. Seq_Length loop
            Accumulate (Buffer, DateTime.Week_In_Month, Locale);
         end loop;
      end Upper_W;

      -------------
      -- Upper_Y --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-year
      --
      --  year
      --
      --  See Lower_Y
      --

      procedure Upper_Y (Seq_Length : Positive) is
      begin
         Lower_Y (Seq_Length);
      end Upper_Y;

      -------------
      -- Upper_Z --
      -------------
      --
      --  From https://www.unicode.org/reports/tr35/tr35-dates.html#dfst-zone
      --
      --  zone
      --  Z..ZZZ   -0800    The ISO8601 basic format with hours, minutes
      --                    and optional seconds fields. The format
      --                    is equivalent to RFC 822 zone format (when
      --                    optional seconds field is absent). This is
      --                    equivalent to the "xxxx" specifier.
      --  ZZZZ     GMT-8:00 the long localized GMT format. This is
      --                    equivalent to the "OOOO" specifier.
      --  ZZZZZ    -08:00   The ISO8601 extended format with hours,
      --                    minutes and optional seconds fields. The
      --                    ISO8601 UTC indicator "Z" is used when local
      --                    time offset is 0. This is equivalent to the
      --                    "XXXXX" specifier.
      --

      procedure Upper_Z (Seq_Length : in out Positive) is
         S : constant Wide_Character := (if TZ_Offset < 0 then '-' else '+');
         Abs_TZ_Offset : constant Integer := abs Integer (TZ_Offset);
         H_Offset : constant Integer := Abs_TZ_Offset / 60;
         M_Offset : constant Integer := Abs_TZ_Offset mod 60;
      begin
         Seq_Length := Positive'Min (Seq_Length, 5);
         case Seq_Length is
         when 1 | 2 | 3 =>
            Add (Buffer, S);
            Accumulate (Buffer, H_Offset, Locale, Width => 2);
            Accumulate (Buffer, M_Offset, Locale, Width => 2);
         when 4 =>
            Add (Buffer, "GMT");
            Add (Buffer, S);
            Accumulate (Buffer, H_Offset, Locale, Width => 2);
            Add (Buffer, ":");
            Accumulate (Buffer, M_Offset, Locale, Width => 2);
         when 5 =>
            if H_Offset = 0 and M_Offset = 0 then
               Add (Buffer, 'Z');
            else
               Add (Buffer, S);
               Accumulate (Buffer, H_Offset, Locale, Width => 2);
               Add (Buffer, ":");
               Accumulate (Buffer, M_Offset, Locale, Width => 2);
            end if;
         when others =>
            null;
         end case;
      end Upper_Z;

   begin
      Interpret_Format : loop
         exit Interpret_Format when Position > End_Of_Fmt;
         Ch := Format_String (Position);
         if Ch = Quote_Ch then
            Quoted := not Quoted;
         elsif Is_Letter (Ch) then
            Seq_Length := 1;
            while Position + Seq_Length <= End_Of_Fmt
              and then Format_String (Position + Seq_Length) = Ch loop
               Seq_Length := Seq_Length + 1;
            end loop;

            case Ch is

            when 'a' => Lower_A (Seq_Length);
            when 'b' => Lower_B (Seq_Length);
            when 'c' => Lower_C (Seq_Length);
            when 'd' => Lower_D (Seq_Length);
            when 'e' => Lower_E (Seq_Length);
            when 'g' => Lower_G (Seq_Length);
            when 'h' => Lower_H (Seq_Length);
            when 'l' => Lower_L (Seq_Length);
            when 'm' => Lower_M (Seq_Length);
            when 'q' => Lower_Q (Seq_Length);
            when 'r' => Lower_R (Seq_Length);
            when 's' => Lower_S (Seq_Length);
            when 'u' => Lower_U (Seq_Length);
            when 'w' => Lower_W (Seq_Length);
            when 'y' => Lower_Y (Seq_Length);
            when 'z' => Lower_Z (Seq_Length);

            when 'A' => Upper_A (Seq_Length);
            when 'B' => Upper_B (Seq_Length);
            when 'D' => Upper_D (Seq_Length);
            when 'E' => Upper_E (Seq_Length);
            when 'F' => Upper_F (Seq_Length);
            when 'G' => Upper_G (Seq_Length);
            when 'H' => Upper_H (Seq_Length);
            when 'L' => Upper_L (Seq_Length);
            when 'M' => Upper_M (Seq_Length);
            when 'O' => Upper_O (Seq_Length);
            when 'Q' => Upper_Q (Seq_Length);
            when 'S' => Upper_S (Seq_Length);
            when 'W' => Upper_W (Seq_Length);
            when 'Y' => Upper_Y (Seq_Length);
            when 'Z' => Upper_Z (Seq_Length);

            when others =>
               Add (Buffer, Ch, Seq_Length);
            end case;

            Position := Position + Seq_Length - 1;
         else
            Add (Buffer, Ch);
         end if;
         Position := Position + 1;
      end loop Interpret_Format;
      return To_String (Buffer);
   end Format_Value;

   --------------
   -- To_Month --
   --------------

   function To_Month (Number : Month_Number) return Month_Type is
   begin
      return Month_Type'Val (Month_Type'Pos (Jan) + Number - 1);
   end To_Month;

end ZanyBlue.Text.Times;
