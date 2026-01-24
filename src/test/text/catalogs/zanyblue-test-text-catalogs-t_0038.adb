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
procedure T_0038 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Catalog : Catalog_Type;
   L1      : constant Locale_Type := Make_Locale ("en_US");
   L2      : constant Locale_Type := Make_Locale ("fr_FR");

begin
   Catalog := Create;
   R.Assert (Number_Of_Messages (Catalog) = 0, "Expected 0 messages");
   Add (Catalog, "fac1", "key1", "This is fac1/key1", L1);
   R.Assert (Number_Of_Messages (Catalog) = 1, "Expected 1 message");
   Add (Catalog, "fac1", "key1", "This is different fac1/key1", L1);
   R.Assert (Number_Of_Messages (Catalog) = 1, "Expected 1 message");
   Add (Catalog, "fac1", "key2", "This is fac1/key2", L1);
   R.Assert (Number_Of_Messages (Catalog) = 2, "Expected 2 messages");
   Add (Catalog, "fac1", "key3", "This is fac1/key3", L1);
   R.Assert (Number_Of_Messages (Catalog) = 3, "Expected 3 messages");
   Add (Catalog, "fac2", "key3", "This is fac2/key3", L1);
   R.Assert (Number_Of_Messages (Catalog) = 4, "Expected 4 messages");
   Add (Catalog, "fac2", "key2", "This is fac2/key2", L1);
   R.Assert (Number_Of_Messages (Catalog) = 5, "Expected 5 messages");
   Add (Catalog, "fac1", "key3", "This is fac1/key3", L2);
   R.Assert (Number_Of_Messages (Catalog) = 6, "Expected 6 messages");
   Add (Catalog, "fac2", "key1", "This is fac2/key1", L2);
   R.Assert (Number_Of_Messages (Catalog) = 7, "Expected 7 messages");
end T_0038;
