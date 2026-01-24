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
procedure T_0039 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Catalog : constant Catalog_Type := Create;

begin
   R.Assert (Number_Of_Facilities (Catalog) = 0, "Expected 0 facilities");
   Add (Catalog, "fac1");
   R.Assert (Number_Of_Facilities (Catalog) = 1, "Expected 1 facility");
   Check_Value (R, Get_Facility (Catalog, 1), "fac1", "Expected fac1");
   Add (Catalog, "fac1");
   R.Assert (Number_Of_Facilities (Catalog) = 1, "Expected 1 facility");
   Check_Value (R, Get_Facility (Catalog, 1), "fac1", "Expected fac1");
   Add (Catalog, "fac2");
   R.Assert (Number_Of_Facilities (Catalog) = 2, "Expected 2 facilities");
   Check_Value (R, Get_Facility (Catalog, 1), "fac1", "Expected fac1");
   Check_Value (R, Get_Facility (Catalog, 2), "fac2", "Expected fac2");
end T_0039;
