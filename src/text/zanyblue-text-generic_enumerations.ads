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

with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;

generic
   type Enumeration_Type is (<>);
package ZanyBlue.Text.Generic_Enumerations is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   type Enumeration_Argument is new Argument_Type with private;

   function Create (Enumeration_Value : Enumeration_Type)
      return Enumeration_Argument;
   --  Create a "boxed" instance of an enumeration value.

   function "+" (Enumeration_Value : Enumeration_Type)
      return Enumeration_Argument
      renames Create;
   --  Utility renaming of the "Create" function.

   overriding
   function Format (Value    : Enumeration_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String;
   --  Format an enumeration value for printing.

private

   type Enumeration_Argument is new Argument_Type with record
      Data : Enumeration_Type;
   end record;

end ZanyBlue.Text.Generic_Enumerations;
