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

with Ada.Containers.Hashed_Sets;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Properties_Parser;

package ZanyBlue.Text.Catalogs is

   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Properties_Parser;

   Default_Extension : constant Wide_String := "properties";
   --  Default file extension to use for properties file when loading a
   --  set of localized files for a facility.

   type Catalog_Type is private;
   --  Catalog of messages for an application.  A catalog can contain multiple
   --  Facilities with each Facility containing a set of localized messages
   --  indexed by string keys, e.g.,
   --
   --  Catalog:
   --    Facility => "ojdbc"
   --      Locale => "" (base, English in this case, locale)
   --        ORA-17001 => Internal Error
   --        ...
   --      Locale => "zh_CN" (Simplified Chinese)
   --        ORA-17001 => 内部错误
   --        ...
   --    Facility => "sunorb"
   --      Locale => "" (base, again English in this case, locale)
   --        servertool.serverup => server is already up.
   --        ...
   --      Locale => "fr" (French)
   --        servertool.serverup => le serveur fonctionne déjà.
   --        ...
   --
   --  The Catalog type allows adding localized messges for a Facility and the
   --  lookup of the message text for a (Facility, Key, Locale) triple.  The
   --  message return follows the standard search of locales until a value
   --  associated with a key is found.  E.g., searching ("ojdbc", "ORA-17001",
   --  "fr_CA") will try the locales "fr_CA", "fr" and "" (the base locale)
   --  the message.  The first locale with a message definition is returned.

   type Catalog_Handler_Type is new Parser_Handler_Type with private;

   procedure Add_Key_Value (Handler   : in out Catalog_Handler_Type;
                            Facility  : Wide_String;
                            Key       : Wide_String;
                            Value     : Wide_String;
                            Locale    : Locale_Type;
                            File_Name : Wide_String;
                            Line      : Natural);
   --  Callback to add a key/value pair when parsing a .properties file.

   procedure Duplicate_Key (Handler       : in out Catalog_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Locale        : Locale_Type;
                            File_Name     : Wide_String;
                            Current_Line  : Natural;
                            Previous_Line : Natural);
   --  Callback to generate an error on a duplicate key in a .properties files.

   procedure Invalid_Definition (Handler         : in out Catalog_Handler_Type;
                                 Facility        : Wide_String;
                                 Locale          : Locale_Type;
                                 File_Name       : Wide_String;
                                 Current_Line    : Natural;
                                 Additional_Info : String);
   --  Callback to generate an error on an invalid .properties file definition.

   procedure Set_Catalog (Handler   : in out Catalog_Handler_Type;
                          Catalog   : Catalog_Type);
   --  Set the catalog associated with a parser handler, callback target.

   function Get_Catalog (Handler : Catalog_Handler_Type) return Catalog_Type;
   --  Get the catalog associated with a parser handler.

   package Locale_Sets is
      new Ada.Containers.Hashed_Sets (Element_Type => Locale_Type,
                                      Hash => Hash,
                                      Equivalent_Elements => "=");
   subtype Locale_Set is Locale_Sets.Set;

   function Create return Catalog_Type;
   --  The Catalog type is a simple access type with the internal details
   --  hidden within the package body.  The Create function simply returns
   --  a new Catalog.

   function Is_Valid (Catalog : Catalog_Type) return Boolean;
   --  Since the Catalog_Type is private, package users cannot simply test
   --  catalog values against null.  This method checks that a catalog has
   --  created, i.e., is not null.

   function Get_Text (Catalog  : Catalog_Type;
                      Facility : Wide_String;
                      Key      : Wide_String;
                      Locale   : Locale_Type) return Wide_String;
   --  Return the text associated with a (Facility, Key, Locale) triple.
   --  The message returned uses the locale parenting searching to find
   --  a message in set of locales rooted in the argument locale.

   procedure Add (Catalog  : Catalog_Type;
                  Facility : Wide_String);
   --  Add a facility name to the set of known facilities

   procedure Add (Catalog  : Catalog_Type;
                  Facility : Wide_String;
                  Key      : Wide_String;
                  Message  : Wide_String;
                  Locale   : Locale_Type);
   --  Add a message for a (Facility, Key, Locale) triple to a Catalog.  The
   --  message text is copied to an internal catatlog buffer.

   procedure Load_File (Catalog   : Catalog_Type;
                        File_Name : Wide_String;
                        Facility  : Wide_String;
                        Locale    : Locale_Type;
                        Count     : out Natural);
   --  Load the messages defined in the properties file File_Name into the
   --  catalog for the given facility and locale.

   procedure Load_File (File_Name : Wide_String;
                        Facility  : Wide_String;
                        Locale    : Locale_Type;
                        Handler   : in out Catalog_Handler_Type'Class);
   --  Load the messages defined in the properties file File_Name into the
   --  catalog for the given facility and locale.

   procedure Load_Facility (Catalog     : Catalog_Type;
                            Facility    : Wide_String;
                            Source_Name : Wide_String;
                            N_Locales   : out Natural;
                            N_Messages  : out Natural;
                            Directory   : Wide_String := ".";
                            Extension   : Wide_String := Default_Extension;
                            Locales     : Locale_Set := Locale_Sets.Empty_Set);
   --  Load a set of localized files for a facility.  All files matching
   --  The base file name and extension with interleaved locale names are
   --  loaded.

   procedure Load_Facility (Facility    : Wide_String;
                            Source_Name : Wide_String;
                            N_Locales   : out Natural;
                            N_Messages  : out Natural;
                            Handler     : in out Catalog_Handler_Type'Class;
                            Directory   : Wide_String := ".";
                            Extension   : Wide_String := Default_Extension;
                            Locales     : Locale_Set := Locale_Sets.Empty_Set);
   --  Load a set of localized files for a facility.  All files matching
   --  The base file name and extension with interleaved locale names are
   --  loaded.

   procedure Load_Facility (Catalog     : Catalog_Type;
                            Facility    : Wide_String;
                            N_Locales   : out Natural;
                            N_Messages  : out Natural;
                            Directory   : Wide_String := ".";
                            Extension   : Wide_String := Default_Extension);
   --  Load a set of localized files for a facility.  All files matching
   --  The base file name and extension with interleaved locale names are
   --  loaded.  The facility name and the base name file are the same for
   --  this overloaded call.
   --  If Verbose is enabled, each file loaded generates a message for the
   --  facility V_Facility with the key V_Key.

   procedure Enable_Pseudo_Translations (Catalog : Catalog_Type;
                                         Mapping : Pseudo_Map_Vector);
   --  Enable pseudo translations for a catalog.

   function Get_Pseudo_Map (Catalog : Catalog_Type) return Pseudo_Map_Access;
   --  Return the pseudo translation mapping associated with a catalog.

   procedure Enable_Exceptions (Catalog : Catalog_Type);
   --  Enable exceptions for missing arguments, invalid formats, etc.

   procedure Disable_Exceptions (Catalog : Catalog_Type);
   --  Disable exceptions for missing arguments, invalid formats, etc.

   function Exceptions_Enabled (Catalog : Catalog_Type) return Boolean;
   --  Are exceptions enabled for the catalog

   --------------------------------------------------------------------------
   --  INTERNAL API'S TO SUPPORT GENERATION OF ADA SOURCE VIA ZBMCOMPILE
   --------------------------------------------------------------------------

   procedure Add (Catalog  : Catalog_Type;
                  Facility : Wide_String;
                  Key      : Wide_String;
                  Pool     : Static_Message_Pool_Type;
                  First    : Positive;
                  Last     : Natural;
                  Locale   : Locale_Type);
   --  Add a message for a (Facility, Key, Locale) triple to a Catalog.  The
   --  message text a substring (First .. Last) within a static buffer passed
   --  by access (Pool) which is stored in the catalog.  All messages added
   --  via this Add procedure all need to reference the same Pool.  The
   --  exception Static_Pool_Redefinition if different Pool's are used.  This
   --  method is normally used for initializations generated by the zbmcompile
   --  command.

   procedure Use_Single_Pool (Catalog : Catalog_Type);
   --  Enable indexes to allow access to the various name by index, e.g.,
   --  Facility (I).  The indexes are only maintained if require and must
   --  be explicitly enabled on a per catalog basis via this procedure.

   function Number_Of_Facilities (Catalog : Catalog_Type) return Natural;
   --  Return the number of facilities defined in a catalog.

   function Get_Facility (Catalog : Catalog_Type;
                          Index   : Positive) return Wide_String;
   --  Return the name of facility Index, normally used in an iteration from
   --  1 to Number_Of_Facilities.

   function Number_Of_Keys (Catalog : Catalog_Type) return Natural;
   --  Return the total number of keys used in a catalog across all
   --  facilities.  This is not specific to a particular set of message
   --  for a locale or facility.  Any key added to any facility within
   --  the catalog is registered as a key in the list enumerated by this
   --  function.

   function Get_Key (Catalog : Catalog_Type;
                     Index   : Positive) return Wide_String;
   --  Return the name of key Index, again normally used for an iteration
   --  over 1 .. Number_Of_Keys.

   function Number_Of_Locales (Catalog : Catalog_Type) return Natural;
   --  Return the total number of locales used in a catalog across all
   --  facilities.  Again, this is not specific to a particular facility.
   --  Any locale added to any facility within the catalog is registered
   --  as a locale in the list enumerated by this function.

   function Get_Locale (Catalog : Catalog_Type;
                        Index   : Positive) return Locale_Type;
   --  Return the locale numbered Index, again normally used for an iteration
   --  over 1 .. Number_Of_Locales.

   function Get_Locale_Name (Catalog : Catalog_Type;
                             Index   : Positive) return Wide_String;
   --  Return name for the locale numbered Index, again normally used for
   --  an iteration over 1 .. Number_Of_Locales.

   function Number_Of_Messages (Catalog : Catalog_Type) return Natural;
   --  Return the total number of messages defined in a catalog across
   --  all facilities, keys and locales.

   function Get_Pool (Catalog : Catalog_Type) return Wide_String;
   --  Return the pool string data associated with a catalog.  The data
   --  returned includes the contents of both the static and dynamic pools.
   --  The data returned can be used as the static pool data for a new
   --  catalog with messages defined via the Message_Indexes procedure
   --  (this functionality is only used in the zbmcompile application).

   function Pool_Size (Catalog : Catalog_Type) return Natural;
   --  Return the length of the dynamic pool associated with a catalog.

   procedure Query_Message (Catalog        : Catalog_Type;
                            Facility_Index : Positive;
                            Key_Index      : Positive;
                            Locale_Index   : Positive;
                            First          : out Positive;
                            Last           : out Natural);
   --  Get the First and List indexes for a message with the pool returned
   --  by the Get_Pool function.  The text of associated message is simply
   --
   --     Pool := Get_Pool (Catalog);
   --     Query_Message_Indexes (Catalog, 1, 10, 3,
   --                            First, Last);
   --     Msg := Pool (First .. Last);
   --
   --  All the messages within a catalog can be queried by iterating over
   --  the facilities, keys and locales using the Number_Of_* functions.
   --  Note, this procedure will raise exceptions for non-existant data,
   --  e.g., "myfac1" might not contain data for the locale "fr_FR" while
   --  facility "myfac2" might.

   procedure Iterate (Catalog : Catalog_Type;
                      Handler : not null access
                                   procedure (Facility : Positive;
                                              Key      : Positive;
                                              Locale   : Positive;
                                              First    : Positive;
                                              Last     : Natural));
   --  Iterate over the messages defined in a catalog calling the argument
   --  procedure.

   procedure Iterate (Catalog : Catalog_Type;
                      Handler : not null access
                                   procedure (Facility : Positive;
                                              Key      : Positive;
                                              Locale   : Positive;
                                              Message  : Wide_String));
   --  Iterate over the messages defined in a catalog calling the argument
   --  procedure.

   function Get_Text (Catalog        : Catalog_Type;
                      Facility_Index : Positive;
                      Key_Index      : Positive;
                      Locale_Index   : Positive) return Wide_String;
   --  Return the text of a message given the three index values.  This
   --  will raise one of the No_Such_*_Error exceptions if the triple
   --  does not map to a valid message.

   procedure Reserve (Catalog    : Catalog_Type;
                      Pool_Size  : Natural := 0;
                      Messages   : Natural := 0);
   --  Reserve additional space in a catalog for new messages.  If the
   --  messages are dynamic, the internal string pool used to store text
   --  strings can be increased by giving a Pool_Size.  The Messages
   --  argument is the number of additional messages expected.

   procedure Dump (Catalog   : Catalog_Type;
                   File_Name : Wide_String := "");
   --  Debugging utiltiy to dump the contents of a catalog to a named file.
   --  If the file name is the empty string, dump the contents to the standard
   --  output file.

   type ZBMCompile_Definition is
      record
         Facility_Index : Positive;
         Key_Index : Positive;
         Locale_Index : Positive;
         First : Positive;
         Last : Natural;
      end record;
   type ZBMCompile_List is array (Positive range <>) of ZBMCompile_Definition;
   --  Record and list data types to represent messages defined by the
   --  zbmcompile command.

   procedure Initialize (Catalog    : Catalog_Type;
                         Messages   : ZBMCompile_List;
                         Pool       : Static_Message_Pool_Type;
                         Facilities : Constant_String_List;
                         Keys       : Constant_String_List;
                         Locales    : Constant_String_List);
   --  This supporting procedure Initialize uses this Message_List and
   --  the list of facilities, keys and locales to initialize the messages
   --  associated with a set of facilities.

private

   type Catalog_ADT;
   type Catalog_Type is access Catalog_ADT;

   type Catalog_Handler_Type is new Parser_Handler_Type with
      record
         Catalog  : Catalog_Type;
      end record;

end ZanyBlue.Text.Catalogs;
