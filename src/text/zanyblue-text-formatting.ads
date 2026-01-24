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
with Ada.Exceptions;
with Ada.Wide_Text_IO;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Times;
with ZanyBlue.Text.Floats;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Strings;
with ZanyBlue.Text.Integers;
with ZanyBlue.Text.Booleans;
with ZanyBlue.Text.Durations;
with ZanyBlue.Text.Characters;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Null_Object;
with ZanyBlue.Text.Wide_Strings;
with ZanyBlue.Text.Wide_Characters;

package ZanyBlue.Text.Formatting is

   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Null_Object;

   subtype Argument_List is ZanyBlue.Text.Arguments.Argument_List;
   subtype Argument_Type is ZanyBlue.Text.Arguments.Argument_Type;
   --  Convenience renaming to introduce the type name

   procedure Append (List         : in out Argument_List;
                     Argument     : Argument_Type'Class)
      renames ZanyBlue.Text.Arguments.Append;
   --  Convenience renaming of the Append method.

   --  The following "+" definitons simply make the "+" method visible
   --  to client using this package, simplifying the overal usage of the
   --  ZanyBlue.Text library.
   function "+" (String_Value : String)
      return ZanyBlue.Text.Strings.String_Argument
      renames ZanyBlue.Text.Strings.Create;

   function "+" (Wide_String_Value : Wide_String)
      return ZanyBlue.Text.Wide_Strings.Wide_String_Argument
      renames ZanyBlue.Text.Wide_Strings.Create;

   function "+" (Float_Value : Float)
      return ZanyBlue.Text.Floats.Float_Argument
      renames ZanyBlue.Text.Floats.Create;

   function "+" (Integer_Value : Integer)
      return ZanyBlue.Text.Integers.Integer_Argument
      renames ZanyBlue.Text.Integers.Create;

   function "+" (Time_Value : Ada.Calendar.Time)
      return ZanyBlue.Text.Times.Time_Argument
      renames ZanyBlue.Text.Times.Create;

   function "+" (Duration_Value : Duration)
      return ZanyBlue.Text.Durations.Duration_Argument
      renames ZanyBlue.Text.Durations.Create;

   function "+" (Character_Value : Character)
      return ZanyBlue.Text.Characters.Character_Argument
      renames ZanyBlue.Text.Characters.Create;

   function "+" (Wide_Character_Value : Wide_Character)
      return ZanyBlue.Text.Wide_Characters.Wide_Character_Argument
      renames ZanyBlue.Text.Wide_Characters.Create;

   function "+" (Boolean_Value : Boolean)
      return ZanyBlue.Text.Booleans.Enumeration_Argument
      renames ZanyBlue.Text.Booleans.Create;

   function Standard_Catalog return Catalog_Type;
   --  Return the standard catalog managed by the ZanyBlue library to hold
   --  normal application messages.  Explicit reference to a catalog is not
   --  normally used, in this context, this standard catalog is used.

   procedure Enable_Exceptions;
   --  Enable exceptions for missing arguments, invalid formats, etc.

   procedure Disable_Exceptions;
   --  Disable exceptions for missing arguments, invalid formats, etc.

   function Exceptions_Enabled return Boolean;
   --  Are exceptions enabled for the catalog

   function Format (Facility  : Wide_String;
                    Key       : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog)
      return Wide_String;
   --  Format a message with arguments to a Wide_String for display or
   --  printing.

   function Format (Facility  : Wide_String;
                    Key       : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog)
      return Wide_String;
   --  Format a message with 5 possible arguments to a Wide_String for
   --  display or printing.

   function Format (Text      : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale)
      return Wide_String;
   --  Format a message with arguments to a Wide_String for display or
   --  printing.

   function Format (Text      : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale)
      return Wide_String;
   --  Format a message with 5 possible arguments to a Wide_String for
   --  display or printing.

   procedure Print (Facility  : Wide_String;
                    Key       : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print_Line (Facility  : Wide_String;
                         Key       : Wide_String;
                         Arguments : Argument_List;
                         Locale    : Locale_Type := Current_Locale;
                         Catalog   : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print (Facility  : Wide_String;
                    Key       : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print_Line (Facility  : Wide_String;
                         Key       : Wide_String;
                         Argument1 : Argument_Type'Class := Null_Argument;
                         Argument2 : Argument_Type'Class := Null_Argument;
                         Argument3 : Argument_Type'Class := Null_Argument;
                         Argument4 : Argument_Type'Class := Null_Argument;
                         Argument5 : Argument_Type'Class := Null_Argument;
                         Locale    : Locale_Type := Current_Locale;
                         Catalog   : Catalog_Type := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print (Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Arguments   : Argument_List;
                    Locale      : Locale_Type := Current_Locale;
                    Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print_Line (Destination : Ada.Wide_Text_IO.File_Type;
                         Facility    : Wide_String;
                         Key         : Wide_String;
                         Arguments   : Argument_List;
                         Locale      : Locale_Type := Current_Locale;
                         Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print (Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Argument1   : Argument_Type'Class := Null_Argument;
                    Argument2   : Argument_Type'Class := Null_Argument;
                    Argument3   : Argument_Type'Class := Null_Argument;
                    Argument4   : Argument_Type'Class := Null_Argument;
                    Argument5   : Argument_Type'Class := Null_Argument;
                    Locale      : Locale_Type := Current_Locale;
                    Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print_Line (Destination : Ada.Wide_Text_IO.File_Type;
                         Facility    : Wide_String;
                         Key         : Wide_String;
                         Argument1   : Argument_Type'Class := Null_Argument;
                         Argument2   : Argument_Type'Class := Null_Argument;
                         Argument3   : Argument_Type'Class := Null_Argument;
                         Argument4   : Argument_Type'Class := Null_Argument;
                         Argument5   : Argument_Type'Class := Null_Argument;
                         Locale      : Locale_Type := Current_Locale;
                         Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print (Text      : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print_Line (Text      : Wide_String;
                         Arguments : Argument_List;
                         Locale    : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print (Text      : Wide_String;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Argument5 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Print_Line (Text      : Wide_String;
                         Argument1 : Argument_Type'Class := Null_Argument;
                         Argument2 : Argument_Type'Class := Null_Argument;
                         Argument3 : Argument_Type'Class := Null_Argument;
                         Argument4 : Argument_Type'Class := Null_Argument;
                         Argument5 : Argument_Type'Class := Null_Argument;
                         Locale    : Locale_Type := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Print (Destination  : Ada.Wide_Text_IO.File_Type;
                    Text         : Wide_String;
                    Arguments    : Argument_List;
                    Locale       : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print_Line (Destination  : Ada.Wide_Text_IO.File_Type;
                         Text         : Wide_String;
                         Arguments    : Argument_List;
                         Locale       : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print (Destination  : Ada.Wide_Text_IO.File_Type;
                    Text         : Wide_String;
                    Argument1    : Argument_Type'Class := Null_Argument;
                    Argument2    : Argument_Type'Class := Null_Argument;
                    Argument3    : Argument_Type'Class := Null_Argument;
                    Argument4    : Argument_Type'Class := Null_Argument;
                    Argument5    : Argument_Type'Class := Null_Argument;
                    Locale       : Locale_Type := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Print_Line (Destination  : Ada.Wide_Text_IO.File_Type;
                         Text         : Wide_String;
                         Argument1    : Argument_Type'Class := Null_Argument;
                         Argument2    : Argument_Type'Class := Null_Argument;
                         Argument3    : Argument_Type'Class := Null_Argument;
                         Argument4    : Argument_Type'Class := Null_Argument;
                         Argument5    : Argument_Type'Class := Null_Argument;
                         Locale       : Locale_Type := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Raise_Exception (E       : Ada.Exceptions.Exception_Id;
                              Message : Wide_String);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Raise_Exception (E        : Ada.Exceptions.Exception_Id;
                         Facility     : Wide_String;
                         Key          : Wide_String;
                         Argument1    : Argument_Type'Class := Null_Argument;
                         Argument2    : Argument_Type'Class := Null_Argument;
                         Argument3    : Argument_Type'Class := Null_Argument;
                         Argument4    : Argument_Type'Class := Null_Argument;
                         Argument5    : Argument_Type'Class := Null_Argument;
                         Locale       : Locale_Type := Current_Locale;
                         Catalog      : Catalog_Type := Standard_Catalog);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Raise_Exception (E        : Ada.Exceptions.Exception_Id;
                         Facility     : Wide_String;
                         Key          : Wide_String;
                         Arguments    : Argument_List;
                         Locale       : Locale_Type := Current_Locale;
                         Catalog      : Catalog_Type := Standard_Catalog);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Raise_Exception (E        : Ada.Exceptions.Exception_Id;
                         Text         : Wide_String;
                         Argument1    : Argument_Type'Class;
                         Argument2    : Argument_Type'Class := Null_Argument;
                         Argument3    : Argument_Type'Class := Null_Argument;
                         Argument4    : Argument_Type'Class := Null_Argument;
                         Argument5    : Argument_Type'Class := Null_Argument;
                         Locale       : Locale_Type := Current_Locale);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Pseudo_Translate (Mapping : Pseudo_Map_Vector;
                               Catalog : Catalog_Type := Standard_Catalog);
   --  Enable pseudo translations for a catalog.

end ZanyBlue.Text.Formatting;
