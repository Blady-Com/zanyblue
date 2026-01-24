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
with ZanyBlue.Text.Locales;

separate (ZanyBlue.Test.Text.Times)
procedure T_0041 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Calendar;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Times;

   Locale    : constant Locale_Type := Make_Locale ("fr_FR");
   V1        : constant Time := Time_Of (1904, 6, 16, Duration (60483));
   V2        : constant Time := Time_Of (1904, 11, 16, Duration (306));
   Arg1      : constant Time_Argument := Create (V1, 60);
   Arg2      : constant Time_Argument := Create (V2, 60);

begin
   Check_Value (R, Arg1.Format ("mm", Locale), "48");
   Check_Value (R, Arg2.Format ("mm", Locale), "05");
end T_0041;
