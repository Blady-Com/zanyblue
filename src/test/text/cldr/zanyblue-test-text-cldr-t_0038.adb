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
procedure T_0038 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   zh : constant Locale_Type := Make_Locale ("zh");

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String) is
   begin
      Check_Value (R, Script_Name (Abbreviation, Locale => zh), Value,
                      "Expected: " & Value);
   end Check_Script;

begin
   if not Is_Locale_Defined ("zh", "", "") then
      R.Assert (True, "ZH localization not included");
      return;
   end if;
   Check_Script ("Arab", "阿拉伯语");
   Check_Script ("Bali", "巴厘语");
   Check_Script ("Cans", "加拿大土著统一符号语");
   Check_Script ("Deva", "梵文");
   Check_Script ("Egyd", "后期埃及语");
   Check_Script ("Geok", "格鲁吉亚语文字 (Asomtavruli and Nuskhuri)");
   Check_Script ("Hang", "韩文");
   Check_Script ("Inds", "古希腊哈拉潘");
   Check_Script ("Java", "爪哇语");
   Check_Script ("Kali", "克耶李文字");
   Check_Script ("Lana", "兰拿语");
   Check_Script ("Mand", "阿拉米语");
   Check_Script ("Nkoo", "N’Ko（西非书面语言）");
   Check_Script ("Ogam", "欧甘语");
   Check_Script ("Perm", "古彼尔姆诸语");
   Check_Script ("Rjng", "拉让语");
   Check_Script ("Sara", "沙拉堤文");
   Check_Script ("Tagb", "塔格班瓦语");
   Check_Script ("Ugar", "乌加里特语");
   Check_Script ("Vaii", "瓦依语");
   Check_Script ("Xpeo", "古波斯语");
   Check_Script ("Yiii", "彝语");
   Check_Script ("Zxxx", "撤销写入");
end T_0038;
