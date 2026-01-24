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

with ZanyBlue.Text.Locales;

separate (ZanyBlue.Test.Text.Durations)
procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Durations;

   Locale    : constant Locale_Type := Make_Locale ("");
   V1        : constant Duration := Duration (0);
   V2        : constant Duration := Duration (60 + 1);
   V3        : constant Duration := Duration (3600 + 60 + 1);
   V4        : constant Duration := Duration (3600*25 + 60 + 1);
   Arg1      : constant Duration_Argument := +V1;
   Arg2      : constant Duration_Argument := +V2;
   Arg3      : constant Duration_Argument := +V3;
   Arg4      : constant Duration_Argument := +V4;

begin
   Check_Value (R, Arg1.Format ("", Locale), "0:00:00.000");
   Check_Value (R, Arg2.Format ("", Locale), "0:01:01.000");
   Check_Value (R, Arg3.Format ("", Locale), "1:01:01.000");
   Check_Value (R, Arg4.Format ("", Locale), "1 1:01:01.000");
end T_0002;
