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

with ZanyBlue.Text.Format_Message;

package body ZanyBlue.Text.Formatting is

   use Ada.Exceptions;
   use Ada.Wide_Text_IO;


   procedure Make_Arguments (Arguments : in out Argument_List;
                             Argument1 : Argument_Type'Class;
                             Argument2 : Argument_Type'Class;
                             Argument3 : Argument_Type'Class;
                             Argument4 : Argument_Type'Class;
                             Argument5 : Argument_Type'Class);
   --  Construct an Argument_List given a set of Argument_Type values.  To
   --  catch No_Such_Argument references, the list generated includes those
   --  arguments upto the first argument that is a Null_Argument.  It's assumed
   --  this null argument starts the argument defaulting in the original call.


   My_Catalog : constant Catalog_Type := Create;
   --  Catalog used by applications that don't manage their own catalogs.

   --------------------
   -- Format_Message --
   --------------------

   function Format_Message (Message      : Wide_String;
                            Arguments    : Argument_List;
                            Mapping      : Pseudo_Map_Access;
                            Locale       : Locale_Type;
                            Raise_Errors : Boolean) return Wide_String
      renames ZanyBlue.Text.Format_Message;
   --  Utility renaming to simplify calls.

   ------------------------
   -- Disable_Exceptions --
   ------------------------

   procedure Disable_Exceptions is
   begin
      Disable_Exceptions (Standard_Catalog);
   end Disable_Exceptions;

   -----------------------
   -- Enable_Exceptions --
   -----------------------

   procedure Enable_Exceptions is
   begin
      Enable_Exceptions (Standard_Catalog);
   end Enable_Exceptions;

   ------------------------
   -- Exceptions_Enabled --
   ------------------------

   function Exceptions_Enabled return Boolean is
   begin
      return Exceptions_Enabled (Standard_Catalog);
   end Exceptions_Enabled;

   ------------
   -- Format --
   ------------

   function Format (Facility  : Wide_String;
                    Key       : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog)
      return Wide_String is
   begin
      return Format_Message (Get_Text (Catalog, Facility, Key, Locale),
                             Arguments, Get_Pseudo_Map (Catalog), Locale,
                             Exceptions_Enabled (Catalog));
   end Format;

   ------------
   -- Format --
   ------------

   function Format (Facility  : Wide_String;
                    Key       : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog)
      return Wide_String is
      Arguments : Argument_List;
   begin
      Make_Arguments (Arguments, Argument1, Argument2, Argument3,
                                 Argument4, Argument5);
      return Format (Facility, Key, Arguments, Locale, Catalog);
   end Format;

   ------------
   -- Format --
   ------------

   function Format (Text      : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale)
      return Wide_String is
   begin
      return Format_Message (Text, Arguments, null, Locale, True);
   end Format;

   ------------
   -- Format --
   ------------

   function Format (Text      : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale)
      return Wide_String is
      Arguments : Argument_List;
   begin
      Make_Arguments (Arguments, Argument1, Argument2, Argument3,
                                 Argument4, Argument5);
      return Format (Text, Arguments, Locale);
   end Format;

   --------------------
   -- Make_Arguments --
   --------------------

   procedure Make_Arguments (Arguments : in out Argument_List;
                             Argument1 : Argument_Type'Class;
                             Argument2 : Argument_Type'Class;
                             Argument3 : Argument_Type'Class;
                             Argument4 : Argument_Type'Class;
                             Argument5 : Argument_Type'Class) is
   begin
      if Argument1 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument1);
      if Argument2 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument2);
      if Argument3 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument3);
      if Argument4 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument4);
      if Argument5 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument5);
   end Make_Arguments;

   -----------
   -- Print --
   -----------

   procedure Print (Facility  : Wide_String;
                    Key       : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog) is
   begin
      Print (Ada.Wide_Text_IO.Current_Output,
             Facility, Key, Arguments, Locale, Catalog);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Facility  : Wide_String;
                    Key       : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog) is
   begin
      Print (Ada.Wide_Text_IO.Current_Output, Facility, Key,
             Argument1, Argument2, Argument3, Argument4, Argument5,
             Locale, Catalog);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Arguments   : Argument_List;
                    Locale      : Locale_Type := Current_Locale;
                    Catalog     : Catalog_Type := Standard_Catalog) is
      Message : constant Wide_String := Format (Facility,
                                                Key,
                                                Arguments,
                                                Locale,
                                                Catalog);
   begin
      Ada.Wide_Text_IO.Put (Destination, Message);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Argument1   : Argument_Type'Class := Null_Argument;
                    Argument2   : Argument_Type'Class := Null_Argument;
                    Argument3   : Argument_Type'Class := Null_Argument;
                    Argument4   : Argument_Type'Class := Null_Argument;
                    Argument5   : Argument_Type'Class := Null_Argument;
                    Locale      : Locale_Type := Current_Locale;
                    Catalog     : Catalog_Type := Standard_Catalog) is
      Arguments : Argument_List;
   begin
      Make_Arguments (Arguments, Argument1, Argument2, Argument3,
                                 Argument4, Argument5);
      Print (Destination, Facility, Key, Arguments, Locale, Catalog);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Text      : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale) is
   begin
      Print (Ada.Wide_Text_IO.Current_Output, Text, Arguments, Locale);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Text      : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale) is
   begin
      Print (Ada.Wide_Text_IO.Current_Output,
             Text, Argument1, Argument2, Argument3, Argument4, Argument5,
             Locale);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Destination  : Ada.Wide_Text_IO.File_Type;
                    Text         : Wide_String;
                    Arguments    : Argument_List;
                    Locale       : Locale_Type := Current_Locale) is
   begin
      Ada.Wide_Text_IO.Put (Destination,
                            Format_Message (Text, Arguments,
                                            null, Locale, True));
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Destination  : Ada.Wide_Text_IO.File_Type;
                    Text         : Wide_String;
                    Argument1    : Argument_Type'Class := Null_Argument;
                    Argument2    : Argument_Type'Class := Null_Argument;
                    Argument3    : Argument_Type'Class := Null_Argument;
                    Argument4    : Argument_Type'Class := Null_Argument;
                    Argument5    : Argument_Type'Class := Null_Argument;
                    Locale       : Locale_Type := Current_Locale) is
      Arguments : Argument_List;
   begin
      Make_Arguments (Arguments, Argument1, Argument2, Argument3,
                                 Argument4, Argument5);
      Print (Destination, Text, Arguments, Locale);
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Facility  : Wide_String;
                         Key       : Wide_String;
                         Arguments : Argument_List;
                         Locale    : Locale_Type := Current_Locale;
                         Catalog   : Catalog_Type := Standard_Catalog) is
   begin
      Print (Facility, Key, Arguments, Locale, Catalog);
      New_Line;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Facility  : Wide_String;
                         Key       : Wide_String;
                         Argument1 : Argument_Type'Class := Null_Argument;
                         Argument2 : Argument_Type'Class := Null_Argument;
                         Argument3 : Argument_Type'Class := Null_Argument;
                         Argument4 : Argument_Type'Class := Null_Argument;
                         Argument5 : Argument_Type'Class := Null_Argument;
                         Locale    : Locale_Type := Current_Locale;
                         Catalog   : Catalog_Type := Standard_Catalog) is
   begin
      Print (Facility, Key,
             Argument1, Argument2, Argument3, Argument4, Argument5,
             Locale, Catalog);
      New_Line;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Destination : Ada.Wide_Text_IO.File_Type;
                         Facility    : Wide_String;
                         Key         : Wide_String;
                         Arguments   : Argument_List;
                         Locale      : Locale_Type := Current_Locale;
                         Catalog     : Catalog_Type := Standard_Catalog) is
   begin
      Print (Destination, Facility, Key, Arguments, Locale, Catalog);
      New_Line (Destination);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Destination : Ada.Wide_Text_IO.File_Type;
                         Facility    : Wide_String;
                         Key         : Wide_String;
                         Argument1   : Argument_Type'Class := Null_Argument;
                         Argument2   : Argument_Type'Class := Null_Argument;
                         Argument3   : Argument_Type'Class := Null_Argument;
                         Argument4   : Argument_Type'Class := Null_Argument;
                         Argument5   : Argument_Type'Class := Null_Argument;
                         Locale      : Locale_Type := Current_Locale;
                         Catalog     : Catalog_Type := Standard_Catalog) is
   begin
      Print (Destination, Facility, Key,
             Argument1, Argument2, Argument3, Argument4, Argument5,
             Locale, Catalog);
      New_Line (Destination);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Text      : Wide_String;
                         Arguments : Argument_List;
                         Locale    : Locale_Type := Current_Locale) is
   begin
      Print (Text, Arguments, Locale);
      New_Line;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Text      : Wide_String;
                         Argument1 : Argument_Type'Class := Null_Argument;
                         Argument2 : Argument_Type'Class := Null_Argument;
                         Argument3 : Argument_Type'Class := Null_Argument;
                         Argument4 : Argument_Type'Class := Null_Argument;
                         Argument5 : Argument_Type'Class := Null_Argument;
                         Locale    : Locale_Type := Current_Locale) is
   begin
      Print (Text,
             Argument1, Argument2, Argument3, Argument4, Argument5,
             Locale);
      New_Line;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Destination  : Ada.Wide_Text_IO.File_Type;
                         Text         : Wide_String;
                         Arguments    : Argument_List;
                         Locale       : Locale_Type := Current_Locale) is
   begin
      Print (Destination, Text, Arguments, Locale);
      New_Line (Destination);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Destination  : Ada.Wide_Text_IO.File_Type;
                         Text         : Wide_String;
                         Argument1    : Argument_Type'Class := Null_Argument;
                         Argument2    : Argument_Type'Class := Null_Argument;
                         Argument3    : Argument_Type'Class := Null_Argument;
                         Argument4    : Argument_Type'Class := Null_Argument;
                         Argument5    : Argument_Type'Class := Null_Argument;
                         Locale       : Locale_Type := Current_Locale) is
   begin
      Print (Destination, Text,
             Argument1, Argument2, Argument3, Argument4, Argument5,
             Locale);
      New_Line (Destination);
   end Print_Line;

   ----------------------
   -- Pseudo_Translate --
   ----------------------

   procedure Pseudo_Translate (Mapping : Pseudo_Map_Vector;
                               Catalog : Catalog_Type := Standard_Catalog) is
   begin
      Enable_Pseudo_Translations (Catalog, Mapping);
   end Pseudo_Translate;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (E       : Ada.Exceptions.Exception_Id;
                              Message : Wide_String) is
   begin
      Raise_Exception (E, To_UTF8 (Message));
   end Raise_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (E        : Ada.Exceptions.Exception_Id;
                         Facility     : Wide_String;
                         Key          : Wide_String;
                         Argument1    : Argument_Type'Class := Null_Argument;
                         Argument2    : Argument_Type'Class := Null_Argument;
                         Argument3    : Argument_Type'Class := Null_Argument;
                         Argument4    : Argument_Type'Class := Null_Argument;
                         Argument5    : Argument_Type'Class := Null_Argument;
                         Locale       : Locale_Type := Current_Locale;
                         Catalog      : Catalog_Type := Standard_Catalog) is
   begin
      Raise_Exception (E, Format (Facility, Key,
                                  Argument1, Argument2, Argument3,
                                  Argument4, Argument5,
                                  Locale, Catalog));
   end Raise_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (E        : Ada.Exceptions.Exception_Id;
                         Facility     : Wide_String;
                         Key          : Wide_String;
                         Arguments    : Argument_List;
                         Locale       : Locale_Type := Current_Locale;
                         Catalog      : Catalog_Type := Standard_Catalog) is
   begin
      Raise_Exception (E, Format (Facility, Key,
                                  Arguments,
                                  Locale, Catalog));
   end Raise_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (E        : Ada.Exceptions.Exception_Id;
                         Text         : Wide_String;
                         Argument1    : Argument_Type'Class;
                         Argument2    : Argument_Type'Class := Null_Argument;
                         Argument3    : Argument_Type'Class := Null_Argument;
                         Argument4    : Argument_Type'Class := Null_Argument;
                         Argument5    : Argument_Type'Class := Null_Argument;
                         Locale       : Locale_Type := Current_Locale) is
      Arguments : Argument_List;
   begin
      Make_Arguments (Arguments, Argument1, Argument2, Argument3,
                                 Argument4, Argument5);
      Raise_Exception (E, Format (Text, Arguments, Locale));
   end Raise_Exception;

   ----------------------
   -- Standard_Catalog --
   ----------------------

   function Standard_Catalog return Catalog_Type is
   begin
      return My_Catalog;
   end Standard_Catalog;

end ZanyBlue.Text.Formatting;
