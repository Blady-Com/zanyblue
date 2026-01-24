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

pragma License (Modified_GPL);

with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Fixed;

package body ZanyBlue.Compiler.GNAT is

   use Ada.Strings.Wide_Maps;
   use Ada.Strings.Wide_Fixed;

   Mapping : constant Wide_Character_Mapping := To_Mapping (
                                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ.",
                                           "abcdefghijklmnopqrstuvwxyz-");

   ----------------------
   -- Gnat_Source_Name --
   ----------------------

   function Gnat_Source_Name (Package_Name : Wide_String) return Wide_String is
   begin
      --  Lowercase the package name and replace periods with dashes.
      return Translate (Package_Name, Mapping);
   end Gnat_Source_Name;

end ZanyBlue.Compiler.GNAT;
