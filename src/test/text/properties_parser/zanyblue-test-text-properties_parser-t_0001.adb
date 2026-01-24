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

with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Text.Properties_Parser)
procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;

   Test_Name : constant Wide_String := "t_0001";
   File_Name : constant Wide_String := Test_In_Name (Test_Area, Test_Name);
   Handler   : Catalog_Handler_Type;

begin
   Handler.Set_Catalog (Create);
   Parse (File_Name, "myfac", Make_Locale ("en_US"), Handler);
   R.Assert (Handler.Get_N_Messages = 0,
             "Expected no messages in an empty file");
end T_0001;
