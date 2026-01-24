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
procedure T_0194 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Locale : Locale_Type := Make_Locale ("en");

begin
   Check_Value (R, Locale_Name (Locale), "en",
                   "Expected locale name 'en'");
   Check_Value (R, Traits_Tag (Locale), "EN        ",
                   "Unexpected traits tag 'EN'");
   Check_Value (R, Traits_Name (Locale), "en",
                   "Unexpected traits name 'en'");
   Check_Value (R, Short_Day_Name (Locale, Mon), "Mon",
                "en short day name, Mon");
   Check_Value (R, Short_Day_Name (Locale, Tue), "Tue",
                "en short day name, Tue");
   Check_Value (R, Short_Day_Name (Locale, Wed), "Wed",
                "en short day name, Wed");
   Check_Value (R, Short_Day_Name (Locale, Thu), "Thu",
                "en short day name, Thu");
   Check_Value (R, Short_Day_Name (Locale, Fri), "Fri",
                "en short day name, Fri");
   Check_Value (R, Short_Day_Name (Locale, Sat), "Sat",
                "en short day name, Sat");
   Check_Value (R, Short_Day_Name (Locale, Sun), "Sun",
                "en short day name, Sun");
   Set_Traits (Locale, Wide_Name => "fr");
   Check_Value (R, Locale_Name (Locale), "en",
                   "Expected locale name 'en'");
   Check_Value (R, Traits_Tag (Locale), "FR        ",
                   "Unexpected traits tag 'FR'");
   Check_Value (R, Traits_Name (Locale), "fr",
                   "Unexpected traits name 'fr'");
   Check_Value (R, Short_Day_Name (Locale, Mon), "lun.",
                "fr short day name, Mon");
   Check_Value (R, Short_Day_Name (Locale, Tue), "mar.",
                "fr short day name, Tue");
   Check_Value (R, Short_Day_Name (Locale, Wed), "mer.",
                "fr short day name, Wed");
   Check_Value (R, Short_Day_Name (Locale, Thu), "jeu.",
                "fr short day name, Thu");
   Check_Value (R, Short_Day_Name (Locale, Fri), "ven.",
                "fr short day name, Fri");
   Check_Value (R, Short_Day_Name (Locale, Sat), "sam.",
                "fr short day name, Sat");
   Check_Value (R, Short_Day_Name (Locale, Sun), "dim.",
                "fr short day name, Sun");
end T_0194;
