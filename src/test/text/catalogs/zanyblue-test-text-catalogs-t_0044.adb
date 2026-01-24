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
procedure T_0044 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Test_Name : constant Wide_String := "t_0044";
   Catalog   : constant Catalog_Type := Create;

begin
   Add (Catalog, "f1", "k1", "f1-k1 message", Make_Locale (""));
   Add (Catalog, "f2", "k2", "f2-k2 message", Make_Locale ("en"));
   Add (Catalog, "f3", "k3", "f3-k3 message", Make_Locale ("fr"));
   Dump (Catalog, Test_Log_Name (Test_Area, Test_Name));
   Check_Log_File (R, Test_Area, Test_Name,
        "Dump of catalog with multiple messages");
end T_0044;
