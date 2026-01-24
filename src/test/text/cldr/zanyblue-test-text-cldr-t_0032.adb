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
procedure T_0032 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   ja : constant Locale_Type := Make_Locale ("ja");

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String);

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String) is
   begin
      Check_Value (R, Language_Name (Abbreviation, Locale => ja), Value,
                      "Expected: " & Value);
   end Check_Language;

begin
   if not Is_Locale_Defined ("ja", "", "") then
      R.Assert (True, "JA localization not included");
      return;
   end if;
   Check_Language ("aa", "アファル語");
   Check_Language ("ba", "バシキール語");
   Check_Language ("ca", "カタロニア語");
   Check_Language ("da", "デンマーク語");
   Check_Language ("ee", "エウェ語");
   Check_Language ("fa", "ペルシア語");
   Check_Language ("ga", "アイルランド語");
   Check_Language ("ha", "ハウサ語");
   Check_Language ("ia", "インターリングア語");
   Check_Language ("ja", "日本語");
   Check_Language ("ka", "グルジア語");
   Check_Language ("la", "ラテン語");
   Check_Language ("mad", "マドゥラ語");
   Check_Language ("na", "ナウル語");
   Check_Language ("oc", "オック語");
   Check_Language ("pa", "パンジャブ語");
   Check_Language ("qu", "ケチュア語");
   Check_Language ("raj", "ラージャスターン語");
   Check_Language ("sa", "サンスクリット語");
   Check_Language ("ta", "タミール語");
   Check_Language ("udm", "ウドムルト語");
   Check_Language ("vai", "ヴァイ語");
   Check_Language ("wa", "ワロン語");
   Check_Language ("xal", "カルムイク語");
   Check_Language ("yao", "ヤオ語");
   Check_Language ("za", "チワン語");
end T_0032;
