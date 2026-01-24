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
procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Locale      : constant Locale_Type := Make_Locale ("en_US");
   Catalog     : Catalog_Type;

   procedure Check_Extra_Indexes;
   procedure Check_Extra_Indexes is
   begin
      R.Assert (Get_Facility (Catalog, 3) = "nosuchfac",
                "Exception not raised!");
   exception
   when No_Such_Facility_Error =>
      R.Assert (True, "Exception raised");
   end Check_Extra_Indexes;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   R.Assert (Number_Of_Facilities (Catalog) = 0, "Expected 0 facility");
   Add (Catalog, "myfac1", "mykey1", "My Message", Locale);
   R.Assert (Get_Facility (Catalog, 1) = "myfac1",
           "Expected facility name myfac1");
   Add (Catalog, "myfac1", "mykey2", "My Message", Locale);
   R.Assert (Get_Facility (Catalog, 1) = "myfac1",
           "Expected facility name myfac1");
   Add (Catalog, "myfac2", "mykey1", "My Message", Locale);
   R.Assert (Get_Facility (Catalog, 2) = "myfac2",
           "Expected facility name myfac1");
   Check_Extra_Indexes;
end T_0006;
