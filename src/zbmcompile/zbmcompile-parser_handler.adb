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

with ZanyBlue.Text.Formatting;

package body ZBMCompile.Parser_Handler is

   use ZanyBlue.Text.Formatting;

   -------------------
   -- Duplicate_Key --
   -------------------

   procedure Duplicate_Key (Handler       : in out ZBMC_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Locale        : Locale_Type;
                            File_Name     : Wide_String;
                            Current_Line  : Natural;
                            Previous_Line : Natural) is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);
   begin
      Print_Line ("zbmcompile", "00014",
                  +File_Name, +Current_Line, +Key, +Previous_Line);
   end Duplicate_Key;

   ------------------------
   -- Invalid_Definition --
   ------------------------

   procedure Invalid_Definition (Handler         : in out ZBMC_Handler_Type;
                                 Facility        : Wide_String;
                                 Locale          : Locale_Type;
                                 File_Name       : Wide_String;
                                 Current_Line    : Natural;
                                 Additional_Info : String) is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);
   begin
      Print_Line ("zbmcompile", "00013",
                  +File_Name, +Current_Line, +Additional_Info);
   end Invalid_Definition;

end ZBMCompile.Parser_Handler;
