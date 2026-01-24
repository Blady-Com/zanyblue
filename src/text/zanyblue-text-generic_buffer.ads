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

with ZanyBlue.Text.Locales;

generic
   type Integer_Type is range <>;
package ZanyBlue.Text.Generic_Buffer is

   use ZanyBlue.Text.Locales;

   type Buffer_Type (Size : Positive) is
      private;

   Null_Character : constant Wide_Character := Wide_Character'Val (0);
   --  Utility character value used to indicate no character.

   Overflow : exception;
   --  Buffer overflow.

   procedure Add_Left (Buffer : in out Buffer_Type;
                       Data   : Wide_Character);
   --  Append a character to left side of the buffer.

   procedure Add_Left (Buffer : in out Buffer_Type;
                       Data   : Wide_String);
   --  Append a string to left side of the buffer.

   procedure Add_Right (Buffer : in out Buffer_Type;
                        Data   : Wide_Character);
   --  Prepend a character to right side of the buffer.

   procedure Add_Right (Buffer : in out Buffer_Type;
                        Data   : Wide_String);
   --  Prepend a string to right side of the buffer.

   procedure Accumulate_Right (Buffer     : in out Buffer_Type;
                               Value      : in out Integer_Type;
                               Locale     : Locale_Type;
                               Width      : Natural := 0;
                               Scale      : Natural := 0;
                               Prefix     : Wide_Character := Null_Character;
                               Base       : Positive := 10;
                               Lowercase  : Boolean := True);
   --  Add (accumulate) a numeric value to the right side of the buffer.

   procedure Accumulate_Left (Buffer     : in out Buffer_Type;
                              Value      : Integer_Type;
                              Locale     : Locale_Type;
                              Width      : Natural := 1);
   --  Add (accumulate a numeric value to the left side of the buffer.

   function Left (Buffer : Buffer_Type) return Wide_String;
   --  Return the value generated on the left.

   function Right (Buffer : Buffer_Type) return Wide_String;
   --  Return the value generated on the right.

private

   type Buffer_Type (Size : Positive) is
      record
          Data         : Wide_String (1 .. Size);
          Left_Index   : Positive := 1;
          Right_Index  : Natural := Size;
      end record;

end ZanyBlue.Text.Generic_Buffer;
