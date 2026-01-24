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
procedure T_0033 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   zh : constant Locale_Type := Make_Locale ("zh");

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String);

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String) is
   begin
      Check_Value (R, Language_Name (Abbreviation, Locale => zh), Value,
                      "Expected: " & Value);
   end Check_Language;

begin
   if not Is_Locale_Defined ("zh", "", "") then
      R.Assert (True, "ZH localization not included");
      return;
   end if;
   Check_Language ("aa", "阿法文");
   Check_Language ("ba", "巴什客尔文");
   Check_Language ("ca", "加泰罗尼亚文");
   Check_Language ("da", "丹麦文");
   Check_Language ("el", "希腊文");
   Check_Language ("fa", "波斯文");
   Check_Language ("ga", "爱尔兰文");
   Check_Language ("ha", "豪撒文");
   Check_Language ("ia", "国际语");
   Check_Language ("ja", "日文");
   Check_Language ("ka", "格鲁吉亚文");
   Check_Language ("la", "拉丁文");
   Check_Language ("mad", "马都拉文");
   Check_Language ("na", "瑙鲁文");
   Check_Language ("oc", "奥克西唐语");
   Check_Language ("pa", "旁遮普文");
   Check_Language ("qu", "盖丘亚文");
   Check_Language ("raj", "拉贾斯坦文");
   Check_Language ("sa", "梵文");
   Check_Language ("ta", "泰米尔文");
   Check_Language ("udm", "乌德穆尔特文");
   Check_Language ("vai", "瓦伊文");
   Check_Language ("wa", "瓦隆文");
   Check_Language ("xal", "卡尔梅克文");
   Check_Language ("yao", "瑶族文");
   Check_Language ("za", "壮语");
end T_0033;
