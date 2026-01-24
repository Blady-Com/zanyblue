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

separate (ZanyBlue.Test.Text.Generic_Enumerations)
procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;

   type Lights is (Red, Orange, Green);
   package Lights_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Lights);
   use Lights_Arguments;

   Locale     : constant Locale_Type := Make_Locale ("");
   Arg_Red    : constant Enumeration_Argument := Create (Red);
   Arg_Orange : constant Enumeration_Argument := Create (Orange);
   Arg_Green  : constant Enumeration_Argument := Create (Green);

begin
   Check_Value (R, Arg_Red.Format ("*^16", Locale), "*******RED******");
   Check_Value (R, Arg_Orange.Format ("*^16", Locale), "*****ORANGE*****");
   Check_Value (R, Arg_Green.Format ("*^16", Locale), "******GREEN*****");
end T_0003;
