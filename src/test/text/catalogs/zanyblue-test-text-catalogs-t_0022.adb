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

with ZanyBlue.Test.Text.Catalogs.Xmpl_Data1;

separate (ZanyBlue.Test.Text.Catalogs)
procedure T_0022 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Test.Text.Catalogs.Xmpl_Data1;

   L_en_US      : constant Locale_Type := Make_Locale ("en_US");
   Catalog      : Catalog_Type;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   Check_Value (R, Get_Pool (Catalog), "", "Expected an empty pool");
   Add (Catalog, "myfac1", "mykey1", Pool1, 1, 30, L_en_US);
   R.Assert (False, "Invalid_Static_Message_Error exception not raised");
exception
when Invalid_Static_Message_Error =>
   R.Assert (True, "Expected Invalid_Static_Message_Error exception raised");
end T_0022;
