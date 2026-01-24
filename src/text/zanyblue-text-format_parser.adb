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

--
--  Implementation of the format parsing function.
--

with Ada.Strings.Wide_Maps;
with ZanyBlue.Text.Formatting;

package body ZanyBlue.Text.Format_Parser is

   use Ada.Strings.Wide_Maps;
   use ZanyBlue.Text.Formatting;

   Zero_Offset : constant := Wide_Character'Pos ('0');
   Align_Set   : constant Wide_Character_Set := To_Set ("><=^");
   Sign_Set    : constant Wide_Character_Set := To_Set ("+- ");
   Type_Set    : constant Wide_Character_Set := To_Set ("bcdeEfFgGnoxX%");
   Number_Set  : constant Wide_Character_Set := To_Set ("0123456789");

   Current_Maximum_Field_Width : Positive := 100;

   function Make_Filler (Fill   : Wide_Character;
                         Length : Positive) return Wide_String;
   --  Construct a string of the given length composed of the given character.

   function In_Set (Format   : Wide_String;
                    Position : Positive;
                    Set      : Wide_Character_Set) return Boolean;
   --  Check if the Format character at the given position is with in
   --  the given set.  The Position might be beyond the end of the string
   --  in which case False is returned.

   procedure Parse_Base (Format   : Wide_String;
                         Position : in out Positive;
                         Result   : in out Format_Type);
   --  Parse the optional numeric base specifier, the '#' character.

   procedure Parse_Data_Type (Format   : Wide_String;
                              Position : in out Positive;
                              Result   : in out Format_Type);
   --  Parse the optional data type specifier, one of 'b', 'c', 'd', ...

   procedure Parse_Fill_Align (Format   : Wide_String;
                               Position : in out Positive;
                               Result   : in out Format_Type);
   --  Parse the optional alignment character ('<', '>', ...) and optional
   --  fill character.

   procedure Parse_Sign (Format   : Wide_String;
                         Position : in out Positive;
                         Result   : in out Format_Type);
   --  Parse the optional sign specifier, '+', '-', ' '

   procedure Parse_Zero_Fill (Format   : Wide_String;
                              Position : in out Positive;
                              Result   : in out Format_Type);
   --  Parse the optional numeric "fill with zero" character '0'

   procedure Parse_Width (Format   : Wide_String;
                          Position : in out Positive;
                          Result   : in out Format_Type);
   --  Parse the optional field width value (number).

   procedure Parse_Precision (Format   : Wide_String;
                              Position : in out Positive;
                              Result   : in out Format_Type);
   --  Parse the optional precision value (number).

   procedure Parse_Number (Format   : Wide_String;
                           Position : in out Positive;
                           Result   : in out Natural);
   --  General numeric parsing routine.

   -----------
   -- Align --
   -----------

   function Align (Value     : Wide_String;
                   Fill      : Wide_Character;
                   Width     : Natural;
                   Alignment : Align_Type;
                   Prefix    : Wide_String := "") return Wide_String is

      Extra : constant Integer := Width - (Prefix'Length + Value'Length);

   begin
      if Extra <= 0 then
         --  No padding needed, just return the value
         return Prefix & Value;
      end if;
      case Alignment is
         when None | Left =>
            return Prefix & Value & Make_Filler (Fill, Extra);
         when Right =>
            return Make_Filler (Fill, Extra) & Prefix & Value;
         when Numeric =>
            return Prefix & Make_Filler (Fill, Extra) & Value;
         when Center =>
            return Make_Filler (Fill, Extra / 2 + Extra rem 2)
                 & Prefix & Value
                 & Make_Filler (Fill, Extra / 2);
      end case;
   end Align;

   ------------
   -- In_Set --
   ------------

   function In_Set (Format   : Wide_String;
                    Position : Positive;
                    Set      : Wide_Character_Set) return Boolean is
   begin
      return Position <= Format'Last and then Is_In (Format (Position), Set);
   end In_Set;

   -----------------
   -- Make_Filler --
   -----------------

   function Make_Filler (Fill : Wide_Character; Length : Positive)
      return Wide_String
   is
      Result : constant Wide_String (1 .. Length) := (others => Fill);
   begin
      return Result;
   end Make_Filler;

   -------------------------
   -- Maximum_Field_Width --
   -------------------------

   function Maximum_Field_Width return Positive is
   begin
      return Current_Maximum_Field_Width;
   end Maximum_Field_Width;

   -------------------------
   -- Maximum_Field_Width --
   -------------------------

   procedure Maximum_Field_Width (Value : Positive) is
   begin
      Current_Maximum_Field_Width := Value;
   end Maximum_Field_Width;

   -----------
   -- Parse --
   -----------

   function Parse (Format : Wide_String;
                   Locale : Locale_Type) return Format_Type is
      pragma Unreferenced (Locale);
      Result : Format_Type;
      Position : Positive := Format'First;
   begin
      Parse_Fill_Align (Format, Position, Result);
      Parse_Sign (Format, Position, Result);
      Parse_Base (Format, Position, Result);
      Parse_Zero_Fill (Format, Position, Result);
      Parse_Width (Format, Position, Result);
      Parse_Precision (Format, Position, Result);
      Parse_Data_Type (Format, Position, Result);
      if Position <= Format'Last then
         Raise_Exception (Invalid_Format'Identity,
                          "{0}", +Format (Position .. Format'Last));
      end if;
      if Result.Width > Current_Maximum_Field_Width then
         Raise_Exception (Field_Too_Wide_Error'Identity,
                          "{0}", +Result.Width);
      end if;
      return Result;
   end Parse;

   ----------------
   -- Parse_Base --
   ----------------

   procedure Parse_Base (Format   : Wide_String;
                         Position : in out Positive;
                         Result   : in out Format_Type) is
   begin
      if Position <= Format'Last and then Format (Position) = '#' then
         Result.Include_Base := True;
         Position := Position + 1;
      end if;
   end Parse_Base;

   ---------------------
   -- Parse_Data_Type --
   ---------------------

   procedure Parse_Data_Type (Format   : Wide_String;
                              Position : in out Positive;
                              Result   : in out Format_Type) is
   begin
      if In_Set (Format, Position, Type_Set) then
         case Format (Position) is
            when 'b' => Result.Data := 'b';
            when 'c' => Result.Data := 'c';
            when 'd' => Result.Data := 'd';
            when 'e' => Result.Data := 'e';
            when 'E' => Result.Data := 'E';
            when 'f' => Result.Data := 'f';
            when 'F' => Result.Data := 'F';
            when 'g' => Result.Data := 'g';
            when 'G' => Result.Data := 'G';
            when 'n' => Result.Data := 'n';
            when 'o' => Result.Data := 'o';
            when 'x' => Result.Data := 'x';
            when 'X' => Result.Data := 'X';
            when '%' => Result.Data := '%';
            when others => raise Internal_Error;
         end case;
         Position := Position + 1;
      end if;
   end Parse_Data_Type;

   ----------------------
   -- Parse_Fill_Align --
   ----------------------

   procedure Parse_Fill_Align (Format   : Wide_String;
                               Position : in out Positive;
                               Result   : in out Format_Type) is
      Have_Align_Character : Boolean := False;
      Align_Character      : Wide_Character;
   begin
      if In_Set (Format, Position + 1, Align_Set) then
         Result.Fill := Format (Position);
         Align_Character := Format (Position + 1);
         Have_Align_Character := True;
         Position := Position + 2;
      elsif In_Set (Format, Position, Align_Set) then
         Align_Character := Format (Position);
         Have_Align_Character := True;
         Position := Position + 1;
      end if;
      if Have_Align_Character then
         case Align_Character is
            when '<' => Result.Align := Left;
            when '>' => Result.Align := Right;
            when '=' => Result.Align := Numeric;
            when '^' => Result.Align := Center;
            when others => raise Internal_Error;
         end case;
      end if;
   end Parse_Fill_Align;

   ------------------
   -- Parse_Number --
   ------------------

   procedure Parse_Number (Format   : Wide_String;
                           Position : in out Positive;
                           Result   : in out Natural) is
      Digit : Natural;
   begin
      while In_Set (Format, Position, Number_Set) loop
         Digit := Wide_Character'Pos (Format (Position)) - Zero_Offset;
         Result := 10 * Result + Digit;
         Position := Position + 1;
      end loop;
   end Parse_Number;

   ---------------------
   -- Parse_Precision --
   ---------------------

   procedure Parse_Precision (Format   : Wide_String;
                              Position : in out Positive;
                              Result   : in out Format_Type) is
   begin
      if Position <= Format'Last and then Format (Position) = '.' then
         Position := Position + 1;
         Parse_Number (Format, Position, Result.Precision);
      end if;
   end Parse_Precision;

   ----------------
   -- Parse_Sign --
   ----------------

   procedure Parse_Sign (Format   : Wide_String;
                         Position : in out Positive;
                         Result   : in out Format_Type) is
   begin
      if In_Set (Format, Position, Sign_Set) then
         case Format (Position) is
            when '+' => Result.Sign := Plus;
            when '-' => Result.Sign := Minus;
            when ' ' => Result.Sign := Space;
            when others => raise Internal_Error;
         end case;
         Position := Position + 1;
      end if;
   end Parse_Sign;

   -----------------
   -- Parse_Width --
   -----------------

   procedure Parse_Width (Format   : Wide_String;
                          Position : in out Positive;
                          Result   : in out Format_Type) is
   begin
      Parse_Number (Format, Position, Result.Width);
   end Parse_Width;

   ---------------------
   -- Parse_Zero_Fill --
   ---------------------

   procedure Parse_Zero_Fill (Format   : Wide_String;
                              Position : in out Positive;
                              Result   : in out Format_Type) is
   begin
      if Position <= Format'Last and then Format (Position) = '0' then
         Result.Fill := '0';
         Position := Position + 1;
      end if;
   end Parse_Zero_Fill;

   ---------------
   -- To_String --
   ---------------

   function To_String (Format : Format_Type) return Wide_String is
   begin
      return "['" & Format.Fill & "', "
           & To_Wide_String (
                 Align_Type'Image (Format.Align) & ", " &
                 Sign_Type'Image (Format.Sign) & ", " &
                 Boolean'Image (Format.Include_Base) & ", " &
                 Natural'Image (Format.Width) & ", " &
                 Natural'Image (Format.Precision) & ", " &
                 Data_Type'Image (Format.Data) & "]");
   end To_String;

end ZanyBlue.Text.Format_Parser;
