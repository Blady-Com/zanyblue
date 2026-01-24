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
procedure T_0020 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_en_US      : constant Locale_Type := Make_Locale ("en_US");
   L_fr_FR      : constant Locale_Type := Make_Locale ("fr_FR");
   Catalog      : Catalog_Type;
   First        : Positive;
   Last         : Positive;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   Check_Value (R, Get_Pool (Catalog), "", "Expected an empty pool");
   Add (Catalog, "xyz", "mkr", "12345", L_en_US);
   Add (Catalog, "xyz", "mkr", "54321", L_fr_FR);
   Query_Message (Catalog, 1, 1, 1, First, Last);
   R.Assert (First = 1, "Expected first static pool index to be 1");
   R.Assert (Last = 5, "Expected last static pool index to be 5");
   Query_Message (Catalog, 1, 1, 2, First, Last);
   R.Assert (First = 6, "Expected first static pool index to be 1");
   R.Assert (Last = 10, "Expected last static pool index to be 5");
end T_0020;
