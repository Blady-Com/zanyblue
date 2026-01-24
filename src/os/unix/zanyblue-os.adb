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

with Ada.Environment_Variables;
with Ada.Characters.Conversions;

package body ZanyBlue.OS is

   use Ada.Environment_Variables;
   use Ada.Characters.Conversions;

   Lang_Environment_Name : constant String := "LANG";
   --  Name of the environment variable defining the locale.

   --------------------
   -- OS_Locale_Name --
   --------------------

   function OS_Locale_Name return Wide_String is
   begin
      if Exists (Lang_Environment_Name) then
         return To_Wide_String (Value (Lang_Environment_Name));
      else
         return "";
      end if;
   end OS_Locale_Name;

   -------------
   -- OS_Name --
   -------------

   function OS_Name return Wide_String is
   begin
      return "unix";
   end OS_Name;

end ZanyBlue.OS;
