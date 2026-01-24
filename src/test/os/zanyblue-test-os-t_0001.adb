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

with Ada.Environment_Variables;

separate (ZanyBlue.Test.OS)
procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Environment_Variables;

   procedure Verify_OS (Expected : Wide_String);

   procedure Verify_OS (Expected : Wide_String) is
   begin
      Check_Value (R, OS_Name, Expected, "OS_Name function value?");
   end Verify_OS;

begin
   if Exists ("OS") then
      Verify_OS ("Windows_NT");
   else
      Verify_OS ("unix");
   end if;
end T_0001;
