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

separate (ZanyBlue.Test.Text.Locales)
procedure T_0053 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("fr");

begin
   Check_Value (R, Full_Day_Name (L, Mon), "lundi",
                "fr full day name, Mon");
   Check_Value (R, Full_Day_Name (L, Tue), "mardi",
                "fr full day name, Tue");
   Check_Value (R, Full_Day_Name (L, Wed), "mercredi",
                "fr full day name, Wed");
   Check_Value (R, Full_Day_Name (L, Thu), "jeudi",
                "fr full day name, Thu");
   Check_Value (R, Full_Day_Name (L, Fri), "vendredi",
                "fr full day name, Fri");
   Check_Value (R, Full_Day_Name (L, Sat), "samedi",
                "fr full day name, Sat");
   Check_Value (R, Full_Day_Name (L, Sun), "dimanche",
                "fr full day name, Sun");
end T_0053;
