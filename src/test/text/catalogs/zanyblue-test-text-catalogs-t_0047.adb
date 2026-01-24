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
procedure T_0047 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_en_Latn_US   : constant Locale_Type := Make_Locale ("en", "Latn", "US");
   L_en_US        : constant Locale_Type := Make_Locale ("en", "US");
   L_en           : constant Locale_Type := Make_Locale ("en", "");
   L_Root         : constant Locale_Type := Make_Locale ("");
   Catalog        : Catalog_Type;

begin
   Catalog := Create;
   Add (Catalog, "myfac1", "mykey1", "msg: en_Latn_US", L_en_Latn_US);
   Check_Value (R, Get_Locale_Name (Catalog, 1), "en_Latn_US");
   Add (Catalog, "myfac1", "mykey1", "msg: en_US", L_en_US);
   Check_Value (R, Get_Locale_Name (Catalog, 2), "en_US");
   Add (Catalog, "myfac1", "mykey1", "msg: en", L_en);
   Check_Value (R, Get_Locale_Name (Catalog, 3), "en");
   Add (Catalog, "myfac1", "mykey1", "msg: Root", L_Root);
   Check_Value (R, Get_Locale_Name (Catalog, 4), "");
end T_0047;
