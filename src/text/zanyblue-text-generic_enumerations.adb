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

with ZanyBlue.Text.Format_Parser;

package body ZanyBlue.Text.Generic_Enumerations is

   use ZanyBlue.Text.Format_Parser;

   ------------
   -- Create --
   ------------

   function Create (Enumeration_Value : Enumeration_Type)
      return Enumeration_Argument is
   begin
      return Enumeration_Argument'(Data => Enumeration_Value);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Value    : Enumeration_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is
      Formatting : constant Format_Type := Parse (Template, Locale);
   begin
      return Align (Enumeration_Type'Wide_Image (Value.Data),
                    Formatting.Fill, Formatting.Width, Formatting.Align);
   end Format;

end ZanyBlue.Text.Generic_Enumerations;
