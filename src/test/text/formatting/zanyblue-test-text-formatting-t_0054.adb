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

with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Text.Formatting)
procedure T_0054 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Catalogs;

   Locale    : constant Locale_Type := Make_Locale ("en_US");
   Facility  : constant Wide_String := "fac1";
   Key       : constant Wide_String := "key1";
   Message   : constant Wide_String := "Message: #1={0}";
   Expect    : constant Wide_String := "⬖Ｍｅｓｓａｇｅ： ＃１＝«10»⬗";
   Catalog   : constant Catalog_Type := Create;

begin
   Enable_Pseudo_Translations (Catalog, Halfwidth_Forms_Map);
   Add (Catalog, Facility, Key, Message, Locale);
   Check_Value (R, Format (Facility, Key, +10,
                           Locale => Locale,
                           Catalog => Catalog),
                Expect,
           "Format pseudo mis-matched");
end T_0054;
