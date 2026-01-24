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

with ZanyBlue.Text.Buffer;
with ZanyBlue.Text.Format_Parser;

package body ZanyBlue.Text.Generic_Modulars is

   use Ada.Characters.Conversions;
   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Text.Format_Parser;

   ------------
   -- Create --
   ------------

   function Create (Value : Modular_Type) return Modular_Argument is
   begin
      return Modular_Argument'(Data => Value);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Value    : Modular_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is

      Formatting : constant Format_Type := Parse (Template, Locale);
      Buffer     : Buffer_Type (4 * Modular_Type'Width + 5);
      Lowercase  : Boolean := True;
      Digit_Map  : Wide_String (1 .. 16);
      Base       : Modular_Type'Base range 2 .. 16;
      X          : Modular_Type'Base := Value.Data;

   begin
      --  Use the data type to determine the base to use
      case Formatting.Data is
         when 'b' =>       Base := 2;
         when 'o' =>       Base := 8;
         when 'x' | 'X' => Base := 16;
                           Lowercase := Formatting.Data = 'x';
         when others =>    Base := 10;
      end case;
      Digit_Map := Locale_Digits (Locale, Lowercase);
      --  Positive add '+' or ' ', if user requested
      case Formatting.Sign is
         when None | Minus => null;
         when Plus =>         Add_Left (Buffer, '+');
         when Space =>        Add_Left (Buffer, ' ');
      end case;
      if Base /= 10 and Formatting.Include_Base then
         --  Decorator with base information if base /= 10 and user requested
         case Base is
            when 2 =>      Add_Left (Buffer, "2#");
            when 8 =>      Add_Left (Buffer, "8#");
            when 16 =>     Add_Left (Buffer, "16#");
            when others => null;
         end case;
         --  Last character in the number buffer (right) should be '#'
         Add_Right (Buffer, '#');
      end if;
      loop
         --  Add digits to the right buffer until exhausted
         Add_Right (Buffer, Digit_Map (Integer (X rem Base) + 1));
         X := X / Base;
         exit when X = 0;
      end loop;
      --  Apply alignment and return
      return Align (Right (Buffer),
                    Formatting.Fill, Formatting.Width, Formatting.Align,
                    Left (Buffer));
   end Format;

end ZanyBlue.Text.Generic_Modulars;
