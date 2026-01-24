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
procedure T_0018 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_en_US      : constant Locale_Type := Make_Locale ("en_US");
   Catalog      : Catalog_Type;
   First        : Positive;
   Last         : Positive;

begin
   Catalog := Create;
   Add (Catalog, "myfac1", "mykey1", "A message", L_en_US);
   Query_Message (Catalog, 1, 1, 1, First, Last);
   R.Assert (False, "Expected multiple pools exception not raised");
exception
when Multiple_Pools_Error =>
   R.Assert (True, "Expected muliple pools exception raised");
end T_0018;
