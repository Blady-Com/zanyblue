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

package body ZanyBlue.Text.Null_Object is

   ------------
   -- Format --
   ------------

   function Format (Value    : Null_Argument_Type;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is

      pragma Unreferenced (Value);
      pragma Unreferenced (Template);
      pragma Unreferenced (Locale);

   begin
      return "";
   end Format;

end ZanyBlue.Text.Null_Object;
