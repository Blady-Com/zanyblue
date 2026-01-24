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

separate (ZanyBlue.Test.Text.Catalogs)
procedure T_0035 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Facility     : constant Wide_String := "xmpl";
   File_Name    : constant Wide_String := Test_Src_Directory (Test_Area)
                                   & "/strings.properties";
   L_en_US      : constant Locale_Type := Make_Locale ("en_US");
   Catalog      : Catalog_Type;
   Count        : Natural;

   procedure Check (Key : Wide_String; Message : Wide_String);

   procedure Check (Key : Wide_String; Message : Wide_String) is
   begin
      Check_Value (R, Get_Text (Catalog, Facility, Key, L_en_US), Message);
   end Check;

begin
   Catalog := Create;
   Load_File (Catalog, File_Name, Facility, L_en_US, Count);
   R.Assert (Count = 23, "Expected 23 messages");
   Check ("reboot.msg",
          "Before you can use this program you must restart Windows");
   Check ("reboot.title",
          "Restart");
   Check ("reboot.button.now",
          "Restart now");
   Check ("reboot.button.later",
          "Restart later");
   Check ("license.title",
          "License Agreement");
   Check ("license.question",
          "Do you accept the license agreement:");
   Check ("license.button.accept",
          "Accept");
   Check ("license.button.exit",
          "Exit");
end T_0035;
