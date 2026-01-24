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
procedure T_0034 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Test.Text.Catalogs.Xmpl_Data1;

   L_en_US      : constant Locale_Type := Make_Locale ("en_US");
   Catalog      : Catalog_Type;
   First, Last  : Positive;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   Check_Value (R, Get_Pool (Catalog), "", "Expected an empty pool");
   Add (Catalog, "myfac1", "mykey1", "abcd", L_en_US);
   Check_Value (R, Get_Text (Catalog, "myfac1", "mykey1", L_en_US), "abcd",
           "Expected myfac1/mykey1 = abcde");
   Add (Catalog, "myfac1", "mykey2", "efgh", L_en_US);
   Check_Value (R, Get_Text (Catalog, "myfac1", "mykey2", L_en_US), "efgh",
           "Expected myfac1/mykey2 = efgh");
   Add (Catalog, "myfac2", "mykey1", "xyz", L_en_US);
   Check_Value (R, Get_Text (Catalog, "myfac2", "mykey1", L_en_US), "xyz",
           "Expected myfac2/mykey1 = xyz");
   Query_Message (Catalog, 2, 2, 1, First, Last);
   R.Assert (False, "Query_Message should have raised an exception");
exception
when No_Such_Message_Error =>
   R.Assert (True, "Query_Message raised an exception");
end T_0034;
