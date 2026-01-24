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

separate (ZanyBlue.Test.Text.Wide_Strings)
procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;

   Locale    : constant Locale_Type := Make_Locale ("");
   V1        : constant Wide_String := "";
   V2        : constant Wide_String := "a";
   V3        : constant Wide_String := "James Joyce";
   V4        : constant Wide_String := "Ulysses";
   Arg1      : constant Wide_String_Argument := Create (V1);
   Arg2      : constant Wide_String_Argument := Create (V2);
   Arg3      : constant Wide_String_Argument := Create (V3);
   Arg4      : constant Wide_String_Argument := Create (V4);

begin
   Check_Value (R, Arg1.Format ("", Locale), "");
   Check_Value (R, Arg2.Format ("", Locale), "a");
   Check_Value (R, Arg3.Format ("", Locale), "James Joyce");
   Check_Value (R, Arg4.Format ("", Locale), "Ulysses");
end T_0001;
