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
--  Language 2-letter codes and names defined by Library of Congress:
--
--  http://www.loc.gov/standards/iso639-2
--
--  Territory codes defined by ISO 3166
--
--  http://www.iso.org/iso/list-en1-semic-3.txt
--

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;

package body ZanyBlue.Text.Utils is

   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Unbounded;

   function Escape_Character (C : Wide_Character) return String;
   --  Return the String value for a Wide Character, either the character
   --  itself if within Character range or the Unicode_Escape string for
   --  the character.

   function Requires_Unicode_Escape (C : Wide_Character) return Boolean;
   --  Determine if a Wide character requires Unicode escaping for strings.

   function Unicode_Escape (C : Wide_Character) return String;
   --  Return Unicode escape sequence for a wide character, e.g., "\u009e".

   -----------------------
   --  ASCII_Capitalize --
   -----------------------

   procedure ASCII_Capitalize (S : in out Wide_String) is
   begin
      for I in S'Range loop
         S (I) := ASCII_Lowercase (S (I));
      end loop;
      S (S'First) := ASCII_Uppercase (S (S'First));
   end ASCII_Capitalize;

   ---------------------
   -- ASCII_Lowercase --
   ---------------------

   procedure ASCII_Lowercase (S : in out Wide_String) is
   begin
      for I in S'Range loop
         S (I) := ASCII_Lowercase (S (I));
      end loop;
   end ASCII_Lowercase;

   ---------------------
   -- ASCII_Lowercase --
   ---------------------

   function ASCII_Lowercase (C : Wide_Character) return Wide_Character is

      use Ada.Characters.Handling;

      N_C : constant Character := To_Character (C, ' ');

   begin
      if Is_Letter (N_C) and then Is_Upper (N_C) then
         return To_Wide_Character (To_Lower (N_C));
      else
         return C;
      end if;
   end ASCII_Lowercase;

   ---------------------
   -- ASCII_Uppercase --
   ---------------------

   procedure ASCII_Uppercase (S : in out Wide_String) is
   begin
      for I in S'Range loop
         S (I) := ASCII_Uppercase (S (I));
      end loop;
   end ASCII_Uppercase;

   ---------------------
   -- ASCII_Uppercase --
   ---------------------

   function ASCII_Uppercase (C : Wide_Character) return Wide_Character is

      use Ada.Characters.Handling;

      N_C : constant Character := To_Character (C, ' ');

   begin
      if Is_Letter (N_C) and then Is_Lower (N_C) then
         return To_Wide_Character (To_Upper (N_C));
      else
         return C;
      end if;
   end ASCII_Uppercase;

   -----------------
   -- Day_In_Week --
   -----------------

   function Day_In_Week (Day   : Day_Number;
                         Month : Month_Number;
                         Year  : Year_Number) return Day_Type is

      subtype Month_Length_Range is Integer range 28 .. 31;

      function Is_Leap (Year : Year_Number) return Boolean;
      --  Determine if a Year is a leap year.

      function Month_Length (Month : Month_Number;
                             Year  : Year_Number) return Month_Length_Range;
      --  Return the number of days in a month for particular year.

      function New_Years_Day (Year : Year_Number) return Day_Type;
      --  Return the day of the week new year's falls on

      -------------
      -- Is_Leap --
      -------------

      function Is_Leap (Year : Year_Number) return Boolean is
      begin
         return (Year rem 4 = 0 and Year rem 100 /= 0) or Year rem 400 = 0;
      end Is_Leap;

      ------------------
      -- Month_Length --
      ------------------

      function Month_Length (Month : Month_Number;
                             Year  : Year_Number) return Month_Length_Range is
      begin
         case Month is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
            return 31;
         when 4 | 6 | 9 | 11 =>
            return 30;
         when 2 =>
            if Is_Leap (Year) then
               return 29;
            else
               return 28;
            end if;
         end case;
      end Month_Length;

      -------------------
      -- New_Years_Day --
      -------------------

      function New_Years_Day (Year : Year_Number) return Day_Type is
         Interval  : Integer;
         NumLeaps  : Integer;
         PosInWeek : Integer;
      begin
         Interval := Integer (Year) - 1601;
         NumLeaps := Interval / 4 - Interval / 100 + Interval / 400;
         PosInWeek := Day_Type'Pos (Mon) + Interval + NumLeaps;
         return Day_Type'Val (PosInWeek rem 7);
      end New_Years_Day;

      DayNum : Integer := Day_Type'Pos (New_Years_Day (Year));

   begin
      for M in 1 .. Month - 1 loop
         DayNum := DayNum + Month_Length (M, Year);
      end loop;
      DayNum := DayNum + Day - 1;
      return Day_Type'Val (DayNum rem 7);
   end Day_In_Week;

   ----------------------
   -- Escape_Character --
   ----------------------

   function Escape_Character (C : Wide_Character) return String is
   begin
      if Requires_Unicode_Escape (C) then
         return Unicode_Escape (C);
      else
         return "" & Character'Val (Wide_Character'Pos (C));
      end if;
   end Escape_Character;

   -------------------
   -- Escape_String --
   -------------------

   function Escape_String (Source : Wide_String) return String is
      Result : Unbounded_String;
   begin
      for I in Source'Range loop
         Result := Result & Escape_Character (Source (I));
      end loop;
      return To_String (Result);
   end Escape_String;

   ----------------------
   -- Non_Blank_Prefix --
   ----------------------

   function Non_Blank_Prefix (S : Wide_String) return Wide_String is
   begin
      for I in S'Range loop
         if S (I) = ' ' then
            return S (S'First .. I - 1);
         end if;
      end loop;
      return S;
   end Non_Blank_Prefix;

   -----------------------------
   -- Requires_Unicode_Escape --
   -----------------------------

   function Requires_Unicode_Escape (C : Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Character'Pos (C);
      Ch : Character;
   begin
      if Pos >= 127 then
         return True;
      end if;
      Ch := Character'Val (Pos);
      return Ch = '"' or not Ada.Characters.Handling.Is_Graphic (Ch);
   end Requires_Unicode_Escape;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (S      : Wide_String;
                         Start  : Positive;
                         Prefix : Wide_String) return Boolean is
      use Ada.Strings.Wide_Fixed;
   begin
      return Head (S (Start .. S'Last), Prefix'Length) = Prefix;
   end Starts_With;

   ---------------------
   -- Unescape_String --
   ---------------------

   function Unescape_String (Source : String) return Wide_String is

      use Ada.Characters.Conversions;

      Wide_BS : constant Wide_Character := To_Wide_Character (ASCII.BS);
      Wide_HT : constant Wide_Character := To_Wide_Character (ASCII.HT);
      Wide_LF : constant Wide_Character := To_Wide_Character (ASCII.LF);
      Wide_FF : constant Wide_Character := To_Wide_Character (ASCII.FF);
      Wide_CR : constant Wide_Character := To_Wide_Character (ASCII.CR);

      Buffer : Unbounded_Wide_String;
      WCh    : Wide_Character;
      Ch     : Character;
      I      : Positive := Source'First;
      Done   : Boolean;

      procedure Get
        (Ch : out Character; Done : out Boolean);
      procedure Get
        (X : out Natural; Done : out Boolean);

      procedure Get
        (Ch : out Character; Done : out Boolean) is
      begin
         Ch := 'x';
         Done := I > Source'Last;
         if not Done then
            Ch := Source (I);
            I := I + 1;
         end if;
      end Get;

      procedure Get
        (X : out Natural; Done : out Boolean) is
         Offset : Natural;
         Base   : Natural;
         Ch : Character;
      begin
         Get (Ch, Done);
         case Ch is
         when '0' .. '9' =>
            Offset := Character'Pos ('0');
            Base := 0;
         when 'a' .. 'f' =>
            Offset := Character'Pos ('a');
            Base := 10;
         when 'A' .. 'F' =>
            Offset := Character'Pos ('A');
            Base := 10;
         when others =>
            raise Unicode_Format_Error with Source & ':' & Ch;
         end case;
         X := Base + Character'Pos (Ch) - Offset;
      end Get;

   begin
      loop
         Get (Ch, Done);
         exit when Done;
         if Ch = '\' then
            Get (Ch, Done);
            exit when Done;
            case Ch is

            when 'b' =>               -- \b, backspace
               WCh := Wide_BS;

            when 'f' =>               -- \f, form feed
               WCh := Wide_FF;

            when 'n' =>               -- \n, newline
               WCh := Wide_LF;

            when 'r' =>               -- \r, carriage return
               WCh := Wide_CR;

            when  't' =>              -- \t, horizontal tab
               WCh := Wide_HT;

            when 'u' =>               -- Unicode escape
               declare
                  Digit : Natural;
                  Value : Natural := 0;
               begin
                  for I in 1 .. 4 loop
                     Get (Digit, Done);
                     Value := Value * 16 + Digit;
                  end loop;
                  WCh := Wide_Character'Val (Value);
               end;

            when others =>
               WCh := Wide_Character'Val (Character'Pos (Ch));

            end case;
         else
            WCh := Wide_Character'Val (Character'Pos (Ch));
         end if;
         Append (Buffer, WCh);
      end loop;
      return To_Wide_String (Buffer);
   end Unescape_String;

   --------------------
   -- Unicode_Escape --
   --------------------

   function Unicode_Escape (C : Wide_Character) return String is
      Hex_Map : constant String (1 .. 16) := "0123456789abcdef";
      Result : String (1 .. 4);
      Pos : Natural := Wide_Character'Pos (C);
   begin
      for I in 1 .. 4 loop
         Result (5 - I) := Hex_Map ((Pos rem 16) + 1);
         Pos := Pos / 16;
      end loop;
      return "\u" & Result;
   end Unicode_Escape;

end ZanyBlue.Text.Utils;
