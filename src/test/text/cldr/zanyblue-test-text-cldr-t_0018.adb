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

separate (ZanyBlue.Test.Text.CLDR)
procedure T_0018 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_Name : constant Wide_String := "ja";
   L : constant Locale_Type := Make_Locale (L_Name);

begin
   Check_Value (R, Full_Locale_Name (Make_Locale (""), L),
                "ルート",
                L_Name & " full locale name for base");
   Check_Value (R, Full_Locale_Name (Make_Locale ("en"), L),
                "英語",
                L_Name & " full locale name for en");
   Check_Value (R, Full_Locale_Name (Make_Locale ("en_US"), L),
                "英語 (アメリカ合衆国)",
                L_Name & " full locale name for en_US");
   Check_Value (R, Full_Locale_Name (Make_Locale ("fr"), L),
                "フランス語",
                L_Name & " full locale name for fr");
   Check_Value (R, Full_Locale_Name (Make_Locale ("fr_CA"), L),
                "フランス語 (カナダ)",
                L_Name & " full locale name for fr_CA");
end T_0018;
