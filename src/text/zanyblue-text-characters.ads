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

package ZanyBlue.Text.Characters is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   type Character_Argument is new Argument_Type with private;

   function Create (Character_Value : Character) return Character_Argument;
   --  Create a "boxed" instance of a character value.

   function "+" (Character_Value : Character) return Character_Argument
      renames Create;
   --  Utility renaming of the "Create" function.

   overriding
   function Format (Value    : Character_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String;
   --  Format a character value for printing.

private

   type Character_Argument is new Argument_Type with record
      Data : Wide_Character;
   end record;

end ZanyBlue.Text.Characters;
