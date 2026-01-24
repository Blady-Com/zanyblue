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
procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   procedure Check_Locale (Value              : Wide_String;
                           Expected_Language  : Wide_String;
                           Expected_Territory : Wide_String);

   procedure Check_Locale (Value              : Wide_String;
                           Expected_Language  : Wide_String;
                           Expected_Territory : Wide_String) is

      Locale : constant Locale_Type := Make_Locale (Value);

   begin
      Check_Value (R, Language (Locale), Expected_Language, "Language");
      Check_Value (R, Territory (Locale), Expected_Territory, "Territory");
   end Check_Locale;

begin
   Check_Locale ("",           "", "");
   Check_Locale ("fr",         "fr", "");
   Check_Locale ("frc",        "frc", "");
   Check_Locale ("fr_FR",      "fr", "FR");
   Check_Locale ("fr_FR.UTF8", "fr", "FR");
   Check_Locale ("fr_FRxUTF8", "fr", "");
   Check_Locale ("frxFR.UTF8", "frx", "");
   Check_Locale ("frxFR",      "frx", "");
   Check_Locale ("ab",         "ab", "");
   Check_Locale ("ab_XY",      "ab", "XY");
   Check_Locale ("AB",         "ab", "");
   Check_Locale ("AB_xy",      "ab", "XY");
end T_0002;
