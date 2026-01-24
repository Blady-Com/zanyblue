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

with Ada.Containers.Indefinite_Vectors;
with ZanyBlue.Text.Locales;

package ZanyBlue.Text.Arguments is

   use ZanyBlue.Text.Locales;

   type Argument_List is tagged private;
   --  The List represent formatting arguments to a message, e.g.,
   --  an integer, a string, etc.  The list is initially empty with arguments
   --  added using the the Append methods below.

   function Format (List         : Argument_List;
                    Position     : Natural;
                    Template     : Wide_String;
                    Locale       : Locale_Type;
                    Raise_Errors : Boolean) return Wide_String;
   --  Format an individual argument using the @Template@ to direct the
   --  conversion.

   function Length (List : Argument_List) return Natural;
   --  Return the length of an argument list, i.e., the number of arguments
   --  current on the list.

   procedure Clear (List : in out Argument_List);
   --  Remove any elements on the list restoring it to an empty list.

   type Argument_Type is abstract tagged null record;
   --  Base type for all arguments types.  Used to define the methods
   --  supported by all arguments: @To_String@, @Format@ and @Append@.

   function Format (Value    : Argument_Type;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is
      abstract;
   --  Format an argument value using the @Template@ to direct the convesion.

   procedure Append (List      : in out Argument_List;
                     Argument  : Argument_Type'Class);
   --  Append an argument to a list of arguments.

private

   use Ada.Containers;

   package Argument_Vectors is
      new Indefinite_Vectors (Index_Type => Natural,
                              Element_Type => Argument_Type'Class);
   type Argument_List is tagged record
      Contents : Argument_Vectors.Vector;
   end record;

end ZanyBlue.Text.Arguments;
