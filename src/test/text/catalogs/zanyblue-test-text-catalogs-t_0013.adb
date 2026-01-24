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
procedure T_0013 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_en_US      : constant Locale_Type := Make_Locale ("en_US");
   L_fr_FR      : constant Locale_Type := Make_Locale ("fr_FR");
   L_en         : constant Locale_Type := Make_Locale ("en");
   L_fr         : constant Locale_Type := Make_Locale ("fr");
   L            : constant Locale_Type := Make_Locale ("");
   Catalog     : Catalog_Type;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   R.Assert (Number_Of_Locales (Catalog) = 0, "Expected 0 locales");
   Add (Catalog, "myfac1", "mykey1", "myfac1/mykey1/en_US", L_en_US);
   R.Assert (Number_Of_Locales (Catalog) = 1, "Expected 1 locale");
   Add (Catalog, "myfac2", "mykey2", "myfac2/mykey2/L_fr_FR", L_fr_FR);
   R.Assert (Number_Of_Locales (Catalog) = 2, "Expected 2 locales");
   Add (Catalog, "myfac3", "mykey3", "myfac3/mykey3/L_en", L_en);
   R.Assert (Number_Of_Locales (Catalog) = 3, "Expected 3 locales");
   Add (Catalog, "myfac4", "mykey4", "myfac4/mykey4/L_fr", L_fr);
   R.Assert (Number_Of_Locales (Catalog) = 4, "Expected 4 locales");
   Add (Catalog, "myfac5", "mykey5", "myfac5/mykey5", L);
   R.Assert (Number_Of_Locales (Catalog) = 5, "Expected 5 locales");
end T_0013;
