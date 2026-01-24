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

package body ZanyBlue.Text.Durations is

   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Text.Format_Parser;

   ------------
   -- Create --
   ------------

   function Create (Duration_Value : Duration) return Duration_Argument is
   begin
      return Duration_Argument'(Data => Duration_Value);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Value    : Duration_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is

      Formatting : constant Format_Type := Parse (Template, Locale);
      Buffer     : Buffer_Type (Natural'Width + 10);
      X          : Natural := Natural (Value.Data * 1000.0);

   begin
      Accumulate_Right (Buffer, X, Locale, 3, 1000);          --  Milliseconds
      Accumulate_Right (Buffer, X, Locale, 2, 60, '.');       --  Seconds
      Accumulate_Right (Buffer, X, Locale, 2, 60, ':');       --  Minutes
      Accumulate_Right (Buffer, X, Locale, 0, 24, ':');       --  Hours
      if X > 0 then
         Accumulate_Right (Buffer, X, Locale, 0, 0, ' ');     --  Days
      end if;
      return Align (Right (Buffer),
                    Formatting.Fill, Formatting.Width, Formatting.Align);
   end Format;

end ZanyBlue.Text.Durations;
