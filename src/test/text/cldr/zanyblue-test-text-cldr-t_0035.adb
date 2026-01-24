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
procedure T_0035 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   ar : constant Locale_Type := Make_Locale ("ar");

   procedure Check_Script (Abbreviation : Wide_String;
                             Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                             Value        : Wide_String) is
   begin
      Check_Value (R, Script_Name (Abbreviation, Locale => ar), Value,
                      "Expected: " & Value);
   end Check_Script;

begin
   if not Is_Locale_Defined ("ar", "", "") then
      R.Assert (True, "AR localization not included");
      return;
   end if;
   Check_Script ("Arab", "العربية");
   Check_Script ("Bali", "البالية");
   Check_Script ("Cans", "مقطعيات أصلية كندية موحدة");
   Check_Script ("Deva", "الديفاناجارى");
   Check_Script ("Egyd", "الديموطيقية");
   Check_Script ("Geok", "الأبجدية الجورجية - أسومتافرلى و نسخرى");
   Check_Script ("Hang", "الهانجل");
   Check_Script ("Inds", "اندس - هارابان");
   Check_Script ("Java", "الجاوية");
   Check_Script ("Kali", "الكياه لى");
   Check_Script ("Lana", "الانا");
   Check_Script ("Mand", "المانداينية");
   Check_Script ("Nkoo", "انكو");
   Check_Script ("Ogam", "الأوجهام");
   Check_Script ("Perm", "البيرميكية القديمة");
   Check_Script ("Roro", "رنجورنجو");
   Check_Script ("Sara", "الساراتي");
   Check_Script ("Tagb", "التاجبانوا");
   Check_Script ("Ugar", "الأجاريتيكية");
   Check_Script ("Vaii", "الفاى");
   Check_Script ("Xpeo", "الفارسية القديمة");
   Check_Script ("Yiii", "اليى");
   Check_Script ("Zxxx", "شفرة للغات الغير مكتوبة");
end T_0035;
