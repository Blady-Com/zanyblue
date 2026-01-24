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
procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;

   Test_Name : constant Wide_String := "t_0014";
   File_Name : constant Wide_String := Test_In_Name (Test_Area, Test_Name);
   Facility  : constant Wide_String := "myfac";
   Locale    : constant Locale_Type := Make_Locale ("en_US");
   Catalog   : constant Catalog_Type := Create;
   Handler   : Catalog_Handler_Type;

begin
   Handler.Set_Catalog (Catalog);
   Parse (File_Name, Facility, Locale, Handler);
   Check_Value (R, Get_Text (Catalog, Facility, "0014", Locale),
                   "ab c",
                "Expected value 'ab c' for key 0014");
   R.Assert (Handler.Get_N_Messages = 1, "No message parsed");
end T_0014;
