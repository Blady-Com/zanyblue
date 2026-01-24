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
procedure T_0012 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;

   Test_Name : constant Wide_String := "t_0012";
   File_Name : constant Wide_String := Test_In_Name (Test_Area, Test_Name);
   Facility  : constant Wide_String := "myfac";
   Locale    : constant Locale_Type := Make_Locale ("en_US");
   Catalog   : constant Catalog_Type := Create;
   Handler   : Catalog_Handler_Type;

begin
   Handler.Set_Catalog (Catalog);
   Parse (File_Name, Facility, Locale, Handler);
   R.Assert (False, "Expected exception not raised");
exception
when Duplicate_Key_Error =>
   Check_Value (R, Get_Text (Catalog, Facility, "0012", Locale),
                   "abc",
                "Expected value 'abc' for key 0012");
   R.Assert (Handler.Get_N_Messages = 1, "Expected 1 message parsed");
end T_0012;
