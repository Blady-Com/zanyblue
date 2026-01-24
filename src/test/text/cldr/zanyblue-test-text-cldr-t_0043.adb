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
procedure T_0043 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   zh : constant Locale_Type := Make_Locale ("zh");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (R, Territory_Name (Abbreviation, Locale => zh), Value,
                      "Expected: " & Value);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("zh", "", "") then
      R.Assert (True, "ZH localization not included");
      return;
   end if;
   Check_Territory ("001", "世界");
   Check_Territory ("AC", "阿森松岛");
   Check_Territory ("BA", "波斯尼亚和黑塞哥维那");
   Check_Territory ("CA", "加拿大");
   Check_Territory ("DE", "德国");
   Check_Territory ("EA", "休达及梅利利亚");
   Check_Territory ("FI", "芬兰");
   Check_Territory ("GA", "加蓬");
   Check_Territory ("HK", "中国香港特别行政区");
   Check_Territory ("IC", "加纳利群岛");
   Check_Territory ("JE", "泽西岛");
   Check_Territory ("KE", "肯尼亚");
   Check_Territory ("LA", "老挝人民民主共和国");
   Check_Territory ("MA", "摩洛哥");
   Check_Territory ("NA", "纳米比亚");
   Check_Territory ("OM", "阿曼");
   Check_Territory ("PA", "巴拿马");
   Check_Territory ("QA", "卡塔尔");
   Check_Territory ("RE", "留尼汪");
   Check_Territory ("SA", "沙特阿拉伯");
   Check_Territory ("TA", "特里斯坦-达库尼亚群岛");
   Check_Territory ("UA", "乌克兰");
   Check_Territory ("VA", "梵蒂冈");
   Check_Territory ("WF", "瓦利斯和富图纳");
   Check_Territory ("YE", "也门");
   Check_Territory ("ZA", "南非");
end T_0043;
