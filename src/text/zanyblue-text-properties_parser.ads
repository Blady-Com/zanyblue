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

with Ada.Wide_Text_IO;
with ZanyBlue.Text.Locales;

package ZanyBlue.Text.Properties_Parser is

   use Ada.Wide_Text_IO;
   use ZanyBlue.Text.Locales;

   type Parser_Handler_Type is abstract tagged private;

   procedure Add_Key_Value (Handler   : in out Parser_Handler_Type;
                            Facility  : Wide_String;
                            Key       : Wide_String;
                            Value     : Wide_String;
                            Locale    : Locale_Type;
                            File_Name : Wide_String;
                            Line      : Natural)
      is abstract;
   --  Call back to handle the definition of a key/value pair.

   procedure Duplicate_Key (Handler       : in out Parser_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Locale        : Locale_Type;
                            File_Name     : Wide_String;
                            Current_Line  : Natural;
                            Previous_Line : Natural)
      is abstract;
   --  Call back used to report a duplicate key error.

   procedure Invalid_Definition (Handler         : in out Parser_Handler_Type;
                                 Facility        : Wide_String;
                                 Locale          : Locale_Type;
                                 File_Name       : Wide_String;
                                 Current_Line    : Natural;
                                 Additional_Info : String)
      is abstract;
   --  Call back used to report an invalid definition.

   function Get_N_Messages (Handler : Parser_Handler_Type) return Natural;
   --  Return the number of messages parsed.

   procedure Reset_N_Messages (Handler : in out Parser_Handler_Type);
   --  Reset the number of messages parse (used when re-using a parser).

   function Get_N_Errors (Handler : Parser_Handler_Type) return Natural;
   --  Get the number of error encountered during the parsing of a file.

   procedure Parse (File_Name   : Wide_String;
                    Facility    : Wide_String;
                    Locale      : Locale_Type;
                    Handler     : in out Parser_Handler_Type'Class);
   --  Parse a properties file using the calling back via the Handler to
   --  process message definitions and errors (see the abstract methods
   --  above).

   procedure Parse (Source_File : in out File_Type;
                    File_Name   : Wide_String;
                    Facility    : Wide_String;
                    Locale      : Locale_Type;
                    Handler     : in out Parser_Handler_Type'Class);
   --  Same as Parse but on an already open file handle.

private

   type Parser_Handler_Type is abstract tagged
      record
         N_Messages : Natural := 0;
         N_Errors   : Natural := 0;
      end record;

   procedure Increment_Messages (Handler : in out Parser_Handler_Type);
   --  Increment the number of messages parsed.

   procedure Increment_Errors (Handler : in out Parser_Handler_Type);
   --  Increment the number of errors encountered.

end ZanyBlue.Text.Properties_Parser;
