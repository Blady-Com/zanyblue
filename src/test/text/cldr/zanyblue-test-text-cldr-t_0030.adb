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
procedure T_0030 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   ar : constant Locale_Type := Make_Locale ("ar");

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String);

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String) is
   begin
      Check_Value (R, Language_Name (Abbreviation, Locale => ar), Value,
                      "Expected: " & Value);
   end Check_Language;

begin
   if not Is_Locale_Defined ("ar", "", "") then
      R.Assert (True, "AR localization not included");
      return;
   end if;
   Check_Language ("aa", "الأفارية");
   Check_Language ("ba", "الباشكيرية");
   Check_Language ("ca", "الكاتالوينية");
   Check_Language ("da", "الدانماركية");
   Check_Language ("ee", "الايوي");
   Check_Language ("fa", "الفارسية");
   Check_Language ("ga", "الأيرلندية");
   Check_Language ("ha", "الهوسا");
   Check_Language ("ia", "اللّغة الوسيطة");
   Check_Language ("ja", "اليابانية");
   Check_Language ("ka", "الجورجية");
   Check_Language ("la", "اللاتينية");
   Check_Language ("mad", "المادريز");
   Check_Language ("na", "النورو");
   Check_Language ("oc", "الأوكيتانية");
   Check_Language ("pa", "البنجابية");
   Check_Language ("qu", "الكويتشوا");
   Check_Language ("raj", "الراجاسثانية");
   Check_Language ("sa", "السنسكريتية");
   Check_Language ("ta", "التاميلية");
   Check_Language ("udm", "الأدمرت");
   Check_Language ("vai", "الفاى");
   Check_Language ("wa", "الولونية");
   Check_Language ("xal", "الكالميك");
   Check_Language ("yao", "الياو");
   Check_Language ("za", "الزهيونج");
end T_0030;
