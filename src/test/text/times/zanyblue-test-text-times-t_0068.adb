--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--  Copyright (C) 2009  Michael Rohan <michael@zanyblue.com>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--

with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with ZanyBlue.Text.Locales;

separate (ZanyBlue.Test.Text.Times)
procedure T_0068 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Calendar;
   use Ada.Calendar.Time_Zones;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Times;

   procedure Check (Format : Wide_String;
                    Value  : Wide_String);

   Locale    : constant Locale_Type := Make_Locale ("zh_Hans");
   V1        : constant Time := Time_Of (1904, 6, 16, Duration (60483));
   Arg1      : constant Time_Argument := Create (V1, 8 * 60);

   procedure Check (Format : Wide_String;
                    Value  : Wide_String) is

   begin
      Check_Value (R, Arg1.Format (Format, Locale), Value,
                   "Format """ & Format & """ style");
   end Check;

begin
   Check ("",       "下午4:48 04-6-16");
   Check ("full",   "+0800下午4时48分03秒1904年6月16日星期四");
   Check ("long",   "+0800下午4时48分03秒1904年6月16日");
   Check ("medium", "下午4:48:03 1904-6-16");
   Check ("short",  "下午4:48 04-6-16");
end T_0068;
