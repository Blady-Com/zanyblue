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
with ZanyBlue.Text.Arguments;

separate (ZanyBlue.Test.Text.Integers)
procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Integers;
   use ZanyBlue.Text.Arguments;

   Locale   : constant Locale_Type := Make_Locale ("");
   Arg0     : constant Integer_Argument := Create (0);
   Arg1     : constant Integer_Argument := Create (2009);
   Arg2     : constant Integer_Argument := Create (-2009);
   Arg3     : constant Integer_Argument := Create (2 ** 31 - 1);
   Arg4     : constant Integer_Argument := Create (-(2 ** 31 - 1));

begin
   Check_Value (R, Arg0.Format ("X", Locale), "0");
   Check_Value (R, Arg1.Format ("X", Locale), "7D9");
   Check_Value (R, Arg2.Format ("X", Locale), "-7D9");
   Check_Value (R, Arg3.Format ("X", Locale), "7FFFFFFF");
   Check_Value (R, Arg4.Format ("X", Locale), "-7FFFFFFF");
end T_0014;
