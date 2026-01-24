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
with ZanyBlue.Text.Formatting;

package body ZanyBlue.Text.Arguments is

   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Text.Formatting;

   ------------
   -- Append --
   ------------

   procedure Append (List      : in out Argument_List;
                     Argument  : Argument_Type'Class) is
   begin
      List.Contents.Append (Argument);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out Argument_List) is
   begin
      List.Contents.Clear;
   end Clear;

   ------------
   -- Format --
   ------------

   function Format (List         : Argument_List;
                    Position     : Natural;
                    Template     : Wide_String;
                    Locale       : Locale_Type;
                    Raise_Errors : Boolean) return Wide_String is
      Buffer : Buffer_Type (Integer'Width + Template'Length + 3);
      X : Integer := Position;
   begin
      if Position >= Natural (List.Contents.Length) then
         if Raise_Errors then
            Raise_Exception (No_Such_Argument_Error'Identity,
                             "{0}", +Position);
         end if;
         Add_Right (Buffer, '|');
         if Template'Length > 0 then
            Add_Right (Buffer, Template);
            Add_Right (Buffer, ',');
         end if;
         Accumulate_Right (Buffer, X, Locale);
         Add_Right (Buffer, '|');
         return Right (Buffer);
      end if;
      return List.Contents.Element (Position).Format (Template, Locale);
   end Format;

   ------------
   -- Length --
   ------------

   function Length (List : Argument_List) return Natural is
   begin
      return Natural (List.Contents.Length);
   end Length;

end ZanyBlue.Text.Arguments;
