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

separate (ZanyBlue.Test.Text.Generic_Floats)
procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;

   Locale   : constant Locale_Type := Make_Locale ("");
   V1       : constant My_Float := 55.0;
   V2       : constant My_Float := 66.1;
   V3       : constant My_Float := -55.0;
   V4       : constant My_Float := -66.1;
   Arg1     : constant Float_Argument := +V1;
   Arg2     : constant Float_Argument := +V2;
   Arg3     : constant Float_Argument := Create (V3);
   Arg4     : constant Float_Argument := Create (V4);

begin
   Check_Value (R, Arg1.Format ("", Locale), "5.50000E+01");
   Check_Value (R, Arg2.Format ("", Locale), "6.61000E+01");
   Check_Value (R, Arg3.Format ("", Locale), "-5.50000E+01");
   Check_Value (R, Arg4.Format ("", Locale), "-6.61000E+01");
end T_0002;
