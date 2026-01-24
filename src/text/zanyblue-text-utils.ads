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

with Ada.Calendar;

with ZanyBlue.Text.Locales;

package ZanyBlue.Text.Utils is

   use Ada.Calendar;
   use ZanyBlue.Text.Locales;

   function Escape_String (Source : Wide_String) return String;
   --  Return the String value associated with a Wide_String containing
   --  Java style \u escape sequences, e.g.,
   --
   --  "This is π" => "This is \u03c0"

   procedure ASCII_Capitalize (S : in out Wide_String);
   --  Convert a string to capitalize (in place).

   procedure ASCII_Lowercase (S : in out Wide_String);
   --  Convert a wide string to lowercase (in place).

   procedure ASCII_Uppercase (S : in out Wide_String);
   --  Convert a wide string to uppercase (in place).

   function ASCII_Uppercase (C : Wide_Character) return Wide_Character;
   --  Convert a wide character to uppercase.

   function ASCII_Lowercase (C : Wide_Character) return Wide_Character;
   --  Convert a wide character to lowercase.

   function Starts_With (S      : Wide_String;
                         Start  : Positive;
                         Prefix : Wide_String) return Boolean;
   --  Determine if a string begins with the Prefix starting at Start.

   function Unescape_String (Source : String) return Wide_String;
   --  Return the Wide_String value associated with a simple String containing
   --  Java style \u escape sequences, e.g.,
   --
   --  "This is \u03c0" => "This is π"

   function Non_Blank_Prefix (S : Wide_String) return Wide_String;
   --  Return a non-blank prefix of a string.  E.g., "ar " returns "ar".
   --  This is a helper function for the codes for languages, scripts and
   --  territories.

   function Day_In_Week (Day   : Day_Number;
                         Month : Month_Number;
                         Year  : Year_Number) return Day_Type;
   --  Return the day of the week given a particular date.

end ZanyBlue.Text.Utils;
