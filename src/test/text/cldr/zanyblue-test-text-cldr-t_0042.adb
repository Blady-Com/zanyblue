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
procedure T_0042 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   ja : constant Locale_Type := Make_Locale ("ja");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (R, Territory_Name (Abbreviation, Locale => ja), Value,
                      "Expected: " & Value);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("ja", "", "") then
      R.Assert (True, "JA localization not included");
      return;
   end if;
   Check_Territory ("001", "世界");
   Check_Territory ("AC", "アセンション島");
   Check_Territory ("BA", "ボスニア・ヘルツェゴビナ");
   Check_Territory ("CA", "カナダ");
   Check_Territory ("DE", "ドイツ");
   Check_Territory ("EA", "セウタ・メリリャ");
   Check_Territory ("FI", "フィンランド");
   Check_Territory ("GA", "ガボン");
   Check_Territory ("HK", "中華人民共和国香港特別行政区");
   Check_Territory ("IC", "カナリア諸島");
   Check_Territory ("JE", "ジャージー");
   Check_Territory ("KE", "ケニア");
   Check_Territory ("LA", "ラオス");
   Check_Territory ("MA", "モロッコ");
   Check_Territory ("NA", "ナミビア");
   Check_Territory ("OM", "オマーン");
   Check_Territory ("PA", "パナマ");
   Check_Territory ("QA", "カタール");
   Check_Territory ("RE", "レユニオン島");
   Check_Territory ("SA", "サウジアラビア");
   Check_Territory ("TA", "トリスタン・ダ・クーニャ");
   Check_Territory ("UA", "ウクライナ");
   Check_Territory ("VA", "バチカン市国");
   Check_Territory ("WF", "ウォリス・フツナ");
   Check_Territory ("YE", "イエメン");
   Check_Territory ("ZA", "南アフリカ");
end T_0042;
