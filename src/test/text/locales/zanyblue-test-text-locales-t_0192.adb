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
procedure T_0192 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   procedure Check_Locale (L      : Wide_String;
                           T      : Wide_String;
                           Name   : Wide_String;
                           LTag   : Wide_String;
                           LName  : Wide_String);

   procedure Check_Locale (L      : Wide_String;
                           T      : Wide_String;
                           Name   : Wide_String;
                           LTag   : Wide_String;
                           LName  : Wide_String) is
      Locale : constant Locale_Type := Make_Locale (L, T);
   begin
      Check_Value (R, Locale_Name (Locale), Name,
                      "Unexpected name value");
      Check_Value (R, Traits_Tag (Locale), LTag,
                      "Unexpected traits tag value");
      Check_Value (R, Traits_Name (Locale), LName,
                      "Unexpected traits name value");
   end Check_Locale;

begin
   Check_Locale ("en", "",        "en",         "EN        ", "en");
   Check_Locale ("fr", "FR",      "fr_FR",      "FR        ", "fr");
   Check_Locale ("en", "us",      "en_US",      "EN        ", "en");
   Check_Locale ("EN", "",        "en",         "EN        ", "en");
   Check_Locale ("FR", "FR",      "fr_FR",      "FR        ", "fr");
   Check_Locale ("EN", "US",      "en_US",      "EN        ", "en");
end T_0192;
