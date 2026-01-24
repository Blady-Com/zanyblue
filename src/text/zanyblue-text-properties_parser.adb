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

with Ada.Exceptions;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Wide_Text_IO.Text_Streams;
with ZanyBlue.Text.Utils;

package body ZanyBlue.Text.Properties_Parser is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Ada.Characters.Latin_1;
   use Ada.Wide_Text_IO.Text_Streams;
   use ZanyBlue.Text.Utils;

   ------------------
   -- Get_N_Errors --
   ------------------

   function Get_N_Errors (Handler : Parser_Handler_Type) return Natural is
   begin
      return Handler.N_Errors;
   end Get_N_Errors;

   --------------------
   -- Get_N_Messages --
   --------------------

   function Get_N_Messages (Handler : Parser_Handler_Type) return Natural is
   begin
      return Handler.N_Messages;
   end Get_N_Messages;

   ----------------------
   -- Increment_Errors --
   ----------------------

   procedure Increment_Errors (Handler : in out Parser_Handler_Type) is
   begin
      Handler.N_Errors := Handler.N_Errors + 1;
   end Increment_Errors;

   ------------------------
   -- Increment_Messages --
   ------------------------

   procedure Increment_Messages (Handler : in out Parser_Handler_Type) is
   begin
      Handler.N_Messages := Handler.N_Messages + 1;
   end Increment_Messages;

   -----------
   -- Parse --
   -----------

   procedure Parse (File_Name   : Wide_String;
                    Facility    : Wide_String;
                    Locale      : Locale_Type;
                    Handler     : in out Parser_Handler_Type'Class) is
      Source_File   : File_Type;
      Opened        : Boolean := False;
   begin
      Wide_Open (Source_File, In_File, File_Name);
      Opened := True;
      Parse (Source_File, File_Name, Facility, Locale, Handler);
      Close (Source_File);
   exception
   when others =>
      if Opened then
         Close (Source_File);
      end if;
      raise;
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse (Source_File : in out File_Type;
                    File_Name   : Wide_String;
                    Facility    : Wide_String;
                    Locale      : Locale_Type;
                    Handler     : in out Parser_Handler_Type'Class) is

      package Key_To_Line_Maps is
         new Ada.Containers.Indefinite_Hashed_Maps
            (Key_Type        => String,
             Element_Type    => Natural,
             Equivalent_Keys => "=",
             Hash            => Ada.Strings.Hash);


      Key_Definitions : Key_To_Line_Maps.Map;
      Source_Stream   : Stream_Access;
      Cur_Character   : Character;
      Cur_Key         : Unbounded_String;
      Cur_Value       : Unbounded_String;
      Cur_Line        : Natural := 0;

      procedure Kill_Line;
      --  Kill, ignore, a line when an error has been encountered.

      procedure New_Message (Key : String; Value : String);
      --  Perform the call back to define a new key/value pair.

      function Next return Character;
      --  Return the next character from the input stream.

      ---------------
      -- Kill_Line --
      ---------------

      procedure Kill_Line is
      begin
         loop
            case Next is
            when LF | CR | EOT =>
               exit;
            when others =>
               null;
            end case;
         end loop;
      end Kill_Line;

      -----------------
      -- New_Message --
      -----------------

      procedure New_Message (Key : String; Value : String) is
         use Key_To_Line_Maps;

         Position : constant Cursor := Find (Key_Definitions, Key);
         WKey : constant Wide_String := To_Wide_String (Key);
      begin
         if Position /= No_Element then
            Handler.Increment_Errors;
            Handler.Duplicate_Key (Facility, WKey, Locale, File_Name,
                                   Cur_Line, Element (Position));
         end if;
         Handler.Increment_Messages;
         Handler.Add_Key_Value (Facility, WKey, Unescape_String (Value),
                                Locale, File_Name, Cur_Line);
         Include (Key_Definitions, Key, Cur_Line);
      end New_Message;

      ----------
      -- Next --
      ----------

      function Next return Character is
      begin
         if not End_Of_File (Source_File) then
            Character'Read (Source_Stream, Cur_Character);
         else
            Cur_Character := EOT;
         end if;
         if Cur_Character = LF then
            Cur_Line := Cur_Line + 1;
         end if;
         return Cur_Character;
      end Next;

   begin
      Source_Stream := Stream (Source_File);

   <<Start>>
      case Next is
      when EOT =>
         goto EOF;
      when Space | HT | VT | FF =>
         goto Start;
      when LF | CR =>
         goto Start;
      when '#' =>
         goto Comment;
      when others =>
         Cur_Key := To_Unbounded_String ("" & Cur_Character);
         Cur_Value := To_Unbounded_String ("");
         goto Key;
      end case;

   <<Key>>
      case Next is
      when '\' =>
         goto Key_Escape;
      when CR | LF | EOT =>
         goto Finish;
      when Space | HT | VT | FF =>
         goto Key_End_1;
      when ':' | '=' =>
         goto Key_End_2;
      when others =>
         Append (Cur_Key, Cur_Character);
         goto Key;
      end case;

   <<Key_Escape>>
      case Next is
      when 'r' =>
         Append (Cur_Key, CR);
         goto Key;
      when 'n' =>
         Append (Cur_Key, LF);
         goto Key;
      when others =>
         Append (Cur_Key, Cur_Character);
         goto Key;
      end case;

   <<Key_End_1>>
      case Next is
      when LF | CR | EOT =>
         goto Finish;
      when Space | HT | VT | FF =>
         goto Key_End_1;
      when '=' | ':' =>
         goto Key_End_2;
      when others =>
         goto Value;
      end case;

   <<Key_End_2>>
      case Next is
      when LF | CR | EOT =>
         goto Finish;
      when Space | HT | VT | FF =>
         goto Key_End_2;
      when others =>
         goto Value;
      end case;

   <<Value>>
      Append (Cur_Value, Cur_Character);
      case Next is
      when CR | LF | EOT =>
         goto Finish;
      when '\' =>
         goto Value_Escape;
      when others =>
         goto Value;
      end case;

   <<Value_Escape>>
      case Next is
      when CR =>
         goto Value_Escape_CR;
      when LF =>
         goto Value_Escape_LF;
      when others =>
         Append (Cur_Value, '\');
         goto Value;
      end case;

   <<Value_Escape_CR>>
      case Next is
      when LF | Space | HT =>
         goto Value_Escape_Space;
      when others =>
         goto Value;
      end case;

   <<Value_Escape_LF>>
      case Next is
      when CR | Space | HT =>
         goto Value_Escape_Space;
      when others =>
         goto Value;
      end case;

   <<Value_Escape_Space>>
      case Next is
      when Space | HT =>
         goto Value_Escape_Space;
      when LF | CR =>
         goto Finish;
      when others =>
         goto Value;
      end case;

   <<Comment>>
      Kill_Line;
      goto Start;

   <<Finish>>
      New_Message (To_String (Cur_Key), To_String (Cur_Value));
      goto Start;

   <<EOF>>
      return;

   exception
   when Error : Unicode_Format_Error =>
      --  Augment the exception with the file name ane line number
      Handler.Increment_Errors;
      Handler.Invalid_Definition (Facility, Locale, File_Name, Cur_Line,
                                  Exception_Message (Error));
   end Parse;

   ----------------------
   -- Reset_N_Messages --
   ----------------------

   procedure Reset_N_Messages (Handler : in out Parser_Handler_Type) is
   begin
      Handler.N_Messages := 0;
   end Reset_N_Messages;

end ZanyBlue.Text.Properties_Parser;
