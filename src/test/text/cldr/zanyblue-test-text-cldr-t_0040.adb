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
procedure T_0040 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   ar : constant Locale_Type := Make_Locale ("ar");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (R, Territory_Name (Abbreviation, Locale => ar), Value,
                      "Expected: " & Value);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("ar", "", "") then
      R.Assert (True, "AR localization not included");
      return;
   end if;
   Check_Territory ("001", "العالم");
   Check_Territory ("AC", "جزيرة أسينشيون");
   Check_Territory ("BA", "البوسنة والهرسك");
   Check_Territory ("CA", "كندا");
   Check_Territory ("DE", "ألمانيا");
   Check_Territory ("EA", "سيوتا وميليلا");
   Check_Territory ("FI", "فنلندا");
   Check_Territory ("GA", "الجابون");
   Check_Territory ("HK", "هونج كونج الصينية");
   Check_Territory ("IC", "جزر الكناري");
   Check_Territory ("JE", "جيرسي");
   Check_Territory ("KE", "كينيا");
   Check_Territory ("LA", "لاوس");
   Check_Territory ("MA", "المغرب");
   Check_Territory ("NA", "ناميبيا");
   Check_Territory ("OM", "عُمان");
   Check_Territory ("PA", "بنما");
   Check_Territory ("QA", "قطر");
   Check_Territory ("RE", "روينيون");
   Check_Territory ("SA", "المملكة العربية السعودية");
   Check_Territory ("TA", "تريستان دي كونها");
   Check_Territory ("UA", "أوكرانيا");
   Check_Territory ("VA", "الفاتيكان");
   Check_Territory ("WF", "جزر والس وفوتونا");
   Check_Territory ("YE", "اليمن");
   Check_Territory ("ZA", "جمهورية جنوب افريقيا");
end T_0040;
