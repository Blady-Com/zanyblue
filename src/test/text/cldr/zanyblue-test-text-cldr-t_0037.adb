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
procedure T_0037 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   ja : constant Locale_Type := Make_Locale ("ja");

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String) is
   begin
      Check_Value (R, Script_Name (Abbreviation, Locale => ja), Value,
                      "Expected: " & Value);
   end Check_Script;

begin
   if not Is_Locale_Defined ("ja", "", "") then
      R.Assert (True, "JA localization not included");
      return;
   end if;
   Check_Script ("Arab", "アラビア文字");
   Check_Script ("Bali", "バリ文字");
   Check_Script ("Cans", "統合カナダ先住民記号");
   Check_Script ("Deva", "デーバナーガリー文字");
   Check_Script ("Egyd", "エジプト民衆文字");
   Check_Script ("Geok", "グルジア文字（フツリ）");
   Check_Script ("Hang", "ハングル");
   Check_Script ("Inds", "インダス文字 (ハラッパ文字)");
   Check_Script ("Java", "ジャワ文字");
   Check_Script ("Kali", "カヤー文字");
   Check_Script ("Laoo", "ラオ文字");
   Check_Script ("Mand", "マンダ文字");
   Check_Script ("Nkoo", "ンコ文字");
   Check_Script ("Ogam", "オガム文字");
   Check_Script ("Perm", "古ペルミック文字");
   Check_Script ("Roro", "ロンゴロンゴ文字");
   Check_Script ("Sara", "サラティ文字");
   Check_Script ("Tagb", "タグバンワ文字");
   Check_Script ("Ugar", "ウガリト文字");
   Check_Script ("Vaii", "ヴァイ文字");
   Check_Script ("Xpeo", "古代ペルシア文字");
   Check_Script ("Yiii", "イ文字");
   Check_Script ("Zxxx", "口承言語のコード");
end T_0037;
