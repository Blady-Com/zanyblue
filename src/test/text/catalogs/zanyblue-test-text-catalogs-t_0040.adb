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

with ZanyBlue.Test.Text.Catalogs.Xmpl_Data3;

separate (ZanyBlue.Test.Text.Catalogs)
procedure T_0040 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Test.Text.Catalogs.Xmpl_Data3;

   Catalog : constant Catalog_Type := Create;
   base : constant Locale_Type := Make_Locale ("");
   fr   : constant Locale_Type := Make_Locale ("fr");
   he   : constant Locale_Type := Make_Locale ("he");

begin
   R.Assert (Number_Of_Facilities (Catalog) = 0, "Expected 0 facilities");
   begin
      Initialize (Catalog);
   exception
   when others =>
      R.Assert (False, "Unexpected exception for Initialize");
   end;
   R.Assert (Number_Of_Facilities (Catalog) = 2, "Expected 2 facilities");
   Check_Value (R, Get_Facility (Catalog, 1), "f1", "Expected 'f1'");
   Check_Value (R, Get_Facility (Catalog, 2), "f2", "Expected 'f2'");
   R.Assert (Number_Of_Keys (Catalog) = 2, "Expected 2 keys");
   Check_Value (R, Get_Key (Catalog, 1), "01", "Expected '01'");
   Check_Value (R, Get_Key (Catalog, 2), "02", "Expected '02'");

   Check_Value (R, Get_Text (Catalog, "f1", "01", base),
                "This fac1 message 0001",
                "Expected 'f1'/'01'");
   Check_Value (R, Get_Text (Catalog, "f1", "02", base),
                "This fac1 message 0002",
                "Expected 'f1'/'02'");
   Check_Value (R, Get_Text (Catalog, "f1", "01", fr),
                "This fac1 message 0001 (fr)",
                "Expected 'f1'/'01'/fr");
   Check_Value (R, Get_Text (Catalog, "f1", "02", fr),
                "This fac1 message 0002 (fr)",
                "Expected 'f1'/'02'/fr");
   Check_Value (R, Get_Text (Catalog, "f1", "01", he),
                "This fac1 message 0001",
                "Expected 'f1'/'01'/he");
   Check_Value (R, Get_Text (Catalog, "f1", "02", he),
                "This fac1 message 0002",
                "Expected 'f1'/'02'/he");

   Check_Value (R, Get_Text (Catalog, "f2", "01", base),
                "This fac2 message 0001",
                "Expected 'f2'/'01'");
   Check_Value (R, Get_Text (Catalog, "f2", "02", base),
                "This fac2 message 0002",
                "Expected 'f2'/'02'");
   Check_Value (R, Get_Text (Catalog, "f2", "01", fr),
                "This fac2 message 0001",
                "Expected 'f2'/'01'/fr");
   Check_Value (R, Get_Text (Catalog, "f2", "02", fr),
                "This fac2 message 0002",
                "Expected 'f2'/'02'/fr");
   Check_Value (R, Get_Text (Catalog, "f2", "01", he),
                "This fac2 message 0001 (he)",
                "Expected 'f2'/'01'/he");
   Check_Value (R, Get_Text (Catalog, "f2", "02", he),
                "This fac2 message 0002 (he)",
                "Expected 'f2'/'02'/he");

end T_0040;
