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

with Ada.Text_IO;
with Ada.Strings.Fixed;
with ZanyBlue.Text.Format_Parser;

package body ZanyBlue.Text.Generic_Floats is

   use Ada.Strings.Fixed;
   use ZanyBlue.Text.Format_Parser;

   package Float_IO is
      new Ada.Text_IO.Float_IO (Float_Type);

   ------------
   -- Create --
   ------------

   function Create (Float_Value : Float_Type) return Float_Argument is
   begin
      return Float_Argument'(Data => Float_Value);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Value    : Float_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is

      Formatting : constant Format_Type := Parse (Template, Locale);
      Buffer     : String (1 .. 128);
      Ch         : Wide_Character := '+';
      Prepend_Ch : Boolean := False;
      First      : Positive;

   begin
      Float_IO.Put (Buffer, Value.Data, Aft => Formatting.Precision + 5);
      First := Index (Buffer, " ", Buffer'Last, Ada.Strings.Backward) + 1;
      case Formatting.Sign is
      when None | Minus =>
         null;
      when Plus =>
         if Value.Data > 0.0 then
            Ch := '+';
            Prepend_Ch := True;
         end if;
      when Space =>
         if Value.Data > 0.0 then
            Ch := ' ';
            Prepend_Ch := True;
         end if;
      end case;
      if Prepend_Ch then
         return Align (Ch & To_Wide_String (Buffer (First .. Buffer'Last)),
                       Formatting.Fill, Formatting.Width, Formatting.Align);
      else
         return Align (To_Wide_String (Buffer (First .. Buffer'Last)),
                       Formatting.Fill, Formatting.Width, Formatting.Align);
      end if;
   end Format;

end ZanyBlue.Text.Generic_Floats;
