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
procedure T_0049 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_Root         : constant Locale_Type := Make_Locale ("");
   Catalog        : Catalog_Type;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   Add (Catalog, "myfac1", "mykey1", "msg1", L_Root);
   R.Assert (Pool_Size (Catalog) = 4, "Expected Pool Size of 4");
   Add (Catalog, "myfac1", "mykey2", "msg2", L_Root);
   R.Assert (Pool_Size (Catalog) = 8, "Expected Pool Size of 8");
   Add (Catalog, "myfac1", "mykey3", "msg3", L_Root);
   R.Assert (Pool_Size (Catalog) = 12, "Expected Pool Size of 12");
   Add (Catalog, "myfac1", "mykey4", "msg4", L_Root);
   R.Assert (Pool_Size (Catalog) = 16, "Expected Pool Size of 16");
end T_0049;
