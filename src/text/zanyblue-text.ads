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

with Ada.Containers;
with Ada.Wide_Text_IO;
with Ada.Characters.Conversions;

package ZanyBlue.Text is

   No_Such_Facility_Error : exception;
   --  Exception raised when accessing a message associated with an undefined
   --  facility.  The name of the unknown facility (or index, if accessing the
   --  message data by indexes) is included as an exception message.

   No_Such_Key_Error : exception;
   --  The exception raised when attempting to query for an unknown key value.
   --  The facility and key values separated by a '/' character are included
   --  in the exception message.  These are index values if accessing the
   --  message data by index.

   No_Such_Locale_Error : exception;
   --  The locale supplied is not referenced in the catalog.  This exception
   --  does not indiciate the locale is an invalid locale name.

   No_Such_Message_Error : exception;
   --  The exception raised when a message cannot be found for a particular
   --  facility, key and local triple, e.g., the key exists in some locale
   --  for the facility but not in the locale required.  The facility and
   --  key values separated by a '/' character are included in the exception
   --  message.  Again, these are index values if accessing the message data
   --  by index.

   No_Such_Argument_Error : exception;
   --  This exception is raised when formatting a message that includes a
   --  reference to an argument beyond the set supplied by the user, e.g.,
   --  referencing argument number 5 when only 2 arguments were supplied.
   --  If formatting exceptions are disabled, then the a reference to the
   --  argument number is included in the formatted string.

   Invalid_Format_Error : exception;
   --  This exception is raised if the format string includes syntax errors
   --  wrt the micro-grammar for references, e.g., an argument reference
   --  does not have a corresponding closing '}'.

   Internal_Error : exception;
   --  The exception raised when an inconsistency is detected and represents
   --  a bug in the implementation.

   Unicode_Escape_Error : exception;
   --  The exception raised when processing Unicode escape sequences in a
   --  string that do not contain hexadecimal values, e.g., "\u00xy" would
   --  raise this error.  The exception is raised with the character that
   --  is not a hexadecimal character.  It can also be raised in the context
   --  of loading a properties file, in which case the exception is raised
   --  with the file name, line number and offending character as colon
   --  separated arguments.

   Unicode_Format_Error : exception;
   --  The exception raised when a Unicode escaped string is converted to
   --  a Wide_String.

   Duplicate_Key_Error : exception;
   --  This exception is raised when a duplicate key definition is encountered
   --  when processing .properties file.  The exception is raised with the
   --  file name, the duplicate key value, the line number for the duplicate
   --  definition and the line number for the original definition as colon
   --  separated arguments, e.g., "myfile.properties:0001: 10: 5".

   Internal_Container_Error : exception;

   Invalid_Static_Message_Error : exception;
   Multiple_Pools_Error : exception;

   type Constant_String_Access is
      access constant Wide_String;

   type Constant_String_List is
      array (Positive range <>) of Constant_String_Access;

   type Static_Message_Pool_Type is access constant Wide_String;

   function Wide_Hash (Key : Wide_String) return Ada.Containers.Hash_Type;
   --  Return the container hash value for a wide string.  Simply use the
   --  standard String hash function on the UTF8 encoded value.

   function To_Wide_String (S : String) return Wide_String
      renames Ada.Characters.Conversions.To_Wide_String;
   --  Utility name to convert a String to Wide_String

   procedure Wide_Create (File : in out Ada.Wide_Text_IO.File_Type;
                          Name : Wide_String);
   --  Create a text with UTF8 encoding files.

   procedure Wide_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Mode : Ada.Wide_Text_IO.File_Mode;
                        Name : Wide_String);
   --  Create a text with UTF8 encoding files.

   function To_UTF8 (Value : Wide_String) return String;
   --  Wrappers around the UTF-8 encode functions

   function From_UTF8 (Value : String) return Wide_String;
   --  Wrappers around the UTF-8 decode functions

end ZanyBlue.Text;
