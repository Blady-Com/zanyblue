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

with Ada.Exceptions;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Unbounded;

package body ZanyBlue.Text.Catalogs is

   use Ada.Containers;
   use Ada.Exceptions;
   use Ada.Strings.Wide_Unbounded;

   type Message_Definition is
      record
         Pool     : Static_Message_Pool_Type;
         First    : Positive;
         Last     : Natural;
      end record;

   type Message_Triple is
      record
         Facility_Index : Positive;
         Key_Index      : Positive;
         Locale_Index   : Positive;
      end record;

   function Message_Triple_Hash (Value : Message_Triple) return Hash_Type;
   --  Ada.Containers hash function for the Message_Triple type.

   package Message_Maps is
      new Hashed_Maps (Key_Type        => Message_Triple,
                       Element_Type    => Message_Definition,
                       Hash            => Message_Triple_Hash,
                       Equivalent_Keys => "=");

   package Id_To_Name_Vectors is
      new Indefinite_Vectors (Index_Type      => Positive,
                              Element_Type    => Wide_String);

   package Name_To_Id_Maps is
      new Indefinite_Hashed_Maps (Key_Type        => Wide_String,
                                  Element_Type    => Positive,
                                  Hash            => ZanyBlue.Text.Wide_Hash,
                                  Equivalent_Keys => "=");

   No_Such_Item : exception;

   --
   --  Indexed_Strings
   --
   --  Protected type to store strings with a sequence number (index)
   --  allowing querying of the value by index.  This type is used to
   --  store facility, key and locale names.
   --
   protected type Indexed_Strings is

      function Length return Natural;
      --  Return the number of items currently stored in the indexed map.

      function Get (Index : Positive) return Wide_String;
      --  Get the name associated with a particular index.  Raises the
      --  exception No_Such_Item if the index does not exist.

      function Get (Name : Wide_String; Id : Exception_Id) return Positive;
      --  Find index associated with a particular name.  If not present,
      --  the argument exception is raised.

      procedure Add (Name : Wide_String);
      --  Add a new name to the set.  The name is associated with the next
      --  index value, i.e., Length + 1.  This is a noop if the name already
      --  exists in the set.

      procedure Add (Name : Wide_String; Index : out Positive);
      --  Add a new name to the set.  The name is associated with the next
      --  index value, i.e., Length + 1.  This is a noop if the name already
      --  exists in the set.

   private

      Name_To_Id : Name_To_Id_Maps.Map;
      Id_To_Name : Id_To_Name_Vectors.Vector;

   end Indexed_Strings;

   --
   --  Message_Definition_Map
   --
   --  Protected type to store mappings from (Facility, Key, Locale)
   --  index triples to message strings.
   --
   protected type Message_Map is

      function Length return Natural;
      --  Return the number of messages currently stored in the map.

      function Get (Triple : Message_Triple) return Message_Definition;
      --  Get the message associated with a particular index triple.  Raises
      --  the exception No_Such_Item if it does not exist.

      function Text (Triple : Message_Triple;
                     Pool   : Unbounded_Wide_String) return Wide_String;
      --  Get the message associated with a particular index triple.  Raises
      --  the exception No_Such_Item if it does not exist.

      procedure Add (Triple : Message_Triple; Message : Message_Definition);
      --  Add a new message to the set.

      procedure Adjust_Size (Extra_Messages : Natural);
      --  When bulk loading (zbmcompile Initialize) adjust the size of the
      --  set to accommodate the new messages.

      procedure Iterate (Handler : not null access
                                      procedure (Facility : Positive;
                                                 Key      : Positive;
                                                 Locale   : Positive;
                                                 First    : Positive;
                                                 Last     : Natural));

      procedure Iterate (Pool    : Unbounded_Wide_String;
                         Handler : not null access
                                      procedure (Facility : Positive;
                                                 Key      : Positive;
                                                 Locale   : Positive;
                                                 Message  : Wide_String));

   private

      Messages     : Message_Maps.Map;

   end Message_Map;

   type Catalog_ADT is
      record
         Pool         : Unbounded_Wide_String;
         Facilities   : Indexed_Strings;
         Keys         : Indexed_Strings;
         Locales      : Indexed_Strings;
         Messages     : Message_Map;
         Pseudo_Map   : Pseudo_Map_Access;
         Single_Pool  : Boolean := False;
         Raise_Errors : Boolean := True;
      end record;

   procedure Map_To_Triple (Catalog  : Catalog_Type;
                            Facility : Wide_String;
                            Key      : Wide_String;
                            Locale   : Wide_String;
                            Triple   : out Message_Triple;
                            Create   : Boolean := False);

   ---------
   -- Add --
   ---------

   procedure Add (Catalog  : Catalog_Type;
                  Facility : Wide_String) is
   begin
      Catalog.Facilities.Add (Facility);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Catalog  : Catalog_Type;
                  Facility : Wide_String;
                  Key      : Wide_String;
                  Message  : Wide_String;
                  Locale   : Locale_Type) is

      New_Message : Message_Definition;
      Triple      : Message_Triple;
      First       : constant Positive := Length (Catalog.Pool) + 1;
      Last        : Natural;

   begin
      Map_To_Triple (Catalog, Facility, Key, Locale_Name (Locale), Triple,
                     Create => True);
      Append (Catalog.Pool, Message);
      Last := Length (Catalog.Pool);
      New_Message.Pool := null;
      New_Message.First := First;
      New_Message.Last := Last;
      Catalog.Messages.Add (Triple, New_Message);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Catalog  : Catalog_Type;
                  Facility : Wide_String;
                  Key      : Wide_String;
                  Pool     : Static_Message_Pool_Type;
                  First    : Positive;
                  Last     : Natural;
                  Locale   : Locale_Type) is

      New_Message : Message_Definition;
      Triple      : Message_Triple;

   begin
      --  Validate the message is actually within the argument pool
      if First < Pool.all'First or Last > Pool.all'Last then
         raise Invalid_Static_Message_Error;
      end if;
      if Catalog.Single_Pool then
         --  Application requested all message be stored in a single
         --  pool: simply add static message to the dynamic pool area.
         --  This is normally used by "zbmcompile" when generating code.
         Add (Catalog, Facility, Key, Pool.all (First .. Last), Locale);
         return;
      end if;
      Map_To_Triple (Catalog, Facility, Key, Locale_Name (Locale), Triple,
                     Create => True);
      New_Message.Pool := Pool;
      New_Message.First := First;
      New_Message.Last := Last;
      Catalog.Messages.Add (Triple, New_Message);
   end Add;

   -------------------
   -- Add_Key_Value --
   -------------------

   procedure Add_Key_Value (Handler   : in out Catalog_Handler_Type;
                            Facility  : Wide_String;
                            Key       : Wide_String;
                            Value     : Wide_String;
                            Locale    : Locale_Type;
                            File_Name : Wide_String;
                            Line      : Natural) is
      pragma Unreferenced (File_Name);
      pragma Unreferenced (Line);
   begin
      Add (Handler.Catalog, Facility, Key, Value, Locale);
   end Add_Key_Value;

   ------------
   -- Create --
   ------------

   function Create return Catalog_Type is
   begin
      return new Catalog_ADT;
   end Create;

   ------------------------
   -- Disable_Exceptions --
   ------------------------

   procedure Disable_Exceptions (Catalog : Catalog_Type) is
   begin
      Catalog.Raise_Errors := False;
   end Disable_Exceptions;

   ----------
   -- Dump --
   ----------

   procedure Dump (Catalog   : Catalog_Type;
                   File_Name : Wide_String := "") is
      separate;

   -------------------
   -- Duplicate_Key --
   -------------------

   procedure Duplicate_Key (Handler       : in out Catalog_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Locale        : Locale_Type;
                            File_Name     : Wide_String;
                            Current_Line  : Natural;
                            Previous_Line : Natural) is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);
   begin
      raise Duplicate_Key_Error with
                       To_UTF8 (File_Name & ":" & Key) & ":"
                     & Natural'Image (Current_Line) & ":"
                     & Natural'Image (Previous_Line);
   end Duplicate_Key;

   -----------------------
   -- Enable_Exceptions --
   -----------------------

   procedure Enable_Exceptions (Catalog : Catalog_Type) is
   begin
      Catalog.Raise_Errors := True;
   end Enable_Exceptions;

   --------------------------------
   -- Enable_Pseudo_Translations --
   --------------------------------

   procedure Enable_Pseudo_Translations (Catalog : Catalog_Type;
                                         Mapping : Pseudo_Map_Vector) is
   begin
      Catalog.Pseudo_Map := new Pseudo_Map_Type;
      Catalog.Pseudo_Map.Add_Mapping (Mapping);
   end Enable_Pseudo_Translations;

   ------------------------
   -- Exceptions_Enabled --
   ------------------------

   function Exceptions_Enabled (Catalog : Catalog_Type) return Boolean is
   begin
      return Catalog.Raise_Errors;
   end Exceptions_Enabled;

   -----------------
   -- Get_Catalog --
   -----------------

   function Get_Catalog (Handler : Catalog_Handler_Type) return Catalog_Type is
   begin
      return Handler.Catalog;
   end Get_Catalog;

   ------------------
   -- Get_Facility --
   ------------------

   function Get_Facility (Catalog : Catalog_Type;
                          Index   : Positive) return Wide_String is
   begin
      return Catalog.Facilities.Get (Index);
   exception
   when No_Such_Item =>
      raise No_Such_Facility_Error with Positive'Image (Index);
   end Get_Facility;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Catalog : Catalog_Type;
                     Index   : Positive) return Wide_String is
   begin
      return Catalog.Keys.Get (Index);
   exception
   when No_Such_Item =>
      raise No_Such_Key_Error with Positive'Image (Index);
   end Get_Key;

   ----------------
   -- Get_Locale --
   ----------------

   function Get_Locale (Catalog : Catalog_Type;
                        Index   : Positive) return Locale_Type is
   begin
      return Make_Locale (Catalog.Locales.Get (Index));
   exception
   when No_Such_Item =>
      raise No_Such_Locale_Error with Positive'Image (Index);
   end Get_Locale;

   ---------------------
   -- Get_Locale_Name --
   ---------------------

   function Get_Locale_Name (Catalog : Catalog_Type;
                             Index   : Positive) return Wide_String is
   begin
      return Locale_Name (Get_Locale (Catalog, Index));
   end Get_Locale_Name;

   --------------
   -- Get_Pool --
   --------------

   function Get_Pool (Catalog : Catalog_Type) return Wide_String is
   begin
      if not Catalog.Single_Pool then
         raise Multiple_Pools_Error;
      end if;
      return To_Wide_String (Catalog.Pool);
   end Get_Pool;

   --------------------
   -- Get_Pseudo_Map --
   --------------------

   function Get_Pseudo_Map (Catalog : Catalog_Type) return Pseudo_Map_Access is
   begin
      return Catalog.Pseudo_Map;
   end Get_Pseudo_Map;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Catalog  : Catalog_Type;
                      Facility : Wide_String;
                      Key      : Wide_String;
                      Locale   : Locale_Type) return Wide_String is

      Language       : Language_Type;
      Script         : Script_Type;
      Territory      : Territory_Type;
      Base_Territory : Territory_Type;
      Triple         : Message_Triple;

   begin
      Get_Locale_Codes (Locale, Language, Script, Territory);
      Base_Territory := Territory;
      for I in 1 .. Maximum_Locale_Parents loop
         begin
            Map_To_Triple (Catalog, Facility, Key,
                           Locale_Name (Language, Script, Territory),
                           Triple);
            return Catalog.Messages.Text (Triple, Catalog.Pool);
         exception
         when No_Such_Locale_Error | Constraint_Error =>
            exit when Language = Empty_Language;
            Parent_Codes (Language, Script, Territory, Base_Territory);
         end;
      end loop;
      raise No_Such_Message_Error with To_UTF8 (Facility & "/" & Key);
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Catalog        : Catalog_Type;
                      Facility_Index : Positive;
                      Key_Index      : Positive;
                      Locale_Index   : Positive) return Wide_String is

      Triple  : constant Message_Triple := (
                            Facility_Index => Facility_Index,
                            Key_Index => Key_Index,
                            Locale_Index => Locale_Index);

   begin
      return Catalog.Messages.Text (Triple, Catalog.Pool);
   exception
   when Constraint_Error =>
      raise No_Such_Message_Error with Positive'Image (Facility_Index)
                               & "/" & Positive'Image (Key_Index);
   end Get_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Catalog    : Catalog_Type;
                         Messages   : ZBMCompile_List;
                         Pool       : Static_Message_Pool_Type;
                         Facilities : Constant_String_List;
                         Keys       : Constant_String_List;
                         Locales    : Constant_String_List) is
   begin
      Reserve (Catalog, Messages => Messages'Length);
      for I in Messages'Range loop
         Add (Catalog, Facilities (Messages (I).Facility_Index).all,
                       Keys (Messages (I).Key_Index).all,
                       Pool,
                       Messages (I).First,
                       Messages (I).Last,
                       Make_Locale (Locales (Messages (I).Locale_Index).all));
      end loop;
   end Initialize;

   ------------------------
   -- Invalid_Definition --
   ------------------------

   procedure Invalid_Definition (Handler         : in out Catalog_Handler_Type;
                                 Facility        : Wide_String;
                                 Locale          : Locale_Type;
                                 File_Name       : Wide_String;
                                 Current_Line    : Natural;
                                 Additional_Info : String) is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);
   begin
      raise Unicode_Escape_Error with
                       To_UTF8 (File_Name) & ":"
                     & Natural'Image (Current_Line) & ":"
                     & Additional_Info;
   end Invalid_Definition;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Catalog : Catalog_Type) return Boolean is
   begin
      return Catalog /= null;
   end Is_Valid;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Catalog : Catalog_Type;
                      Handler : not null access
                                   procedure (Facility : Positive;
                                              Key      : Positive;
                                              Locale   : Positive;
                                              First    : Positive;
                                              Last     : Natural)) is
   begin
      Catalog.Messages.Iterate (Handler);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Catalog : Catalog_Type;
                      Handler : not null access
                                   procedure (Facility : Positive;
                                              Key      : Positive;
                                              Locale   : Positive;
                                              Message  : Wide_String)) is
   begin
      Catalog.Messages.Iterate (Catalog.Pool, Handler);
   end Iterate;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility (Facility    : Wide_String;
                            Source_Name : Wide_String;
                            N_Locales   : out Natural;
                            N_Messages  : out Natural;
                            Handler     : in out Catalog_Handler_Type'Class;
                            Directory   : Wide_String := ".";
                            Extension   : Wide_String := Default_Extension;
                            Locales     : Locale_Set := Locale_Sets.Empty_Set)
      is separate;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility (
                Catalog     : Catalog_Type;
                Facility    : Wide_String;
                Source_Name : Wide_String;
                N_Locales   : out Natural;
                N_Messages  : out Natural;
                Directory   : Wide_String := ".";
                Extension   : Wide_String := Default_Extension;
                Locales     : Locale_Set := Locale_Sets.Empty_Set) is

      Handler : Catalog_Handler_Type;

   begin
      Handler.Catalog := Catalog;
      Load_Facility (Facility, Source_Name, N_Locales, N_Messages,
                     Handler, Directory, Extension, Locales);
   end Load_Facility;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility (Catalog     : Catalog_Type;
                            Facility    : Wide_String;
                            N_Locales   : out Natural;
                            N_Messages  : out Natural;
                            Directory   : Wide_String := ".";
                            Extension   : Wide_String := Default_Extension) is
   begin
      Load_Facility (Catalog, Facility, Facility,
                     N_Locales, N_Messages,
                     Directory, Extension);
   end Load_Facility;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (Catalog   : Catalog_Type;
                        File_Name : Wide_String;
                        Facility  : Wide_String;
                        Locale    : Locale_Type;
                        Count     : out Natural) is

      Handler : Catalog_Handler_Type;

   begin
      Handler.Set_Catalog (Catalog);
      Parse (File_Name, Facility, Locale, Handler);
      Count := Handler.Get_N_Messages;
   end Load_File;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (File_Name : Wide_String;
                        Facility  : Wide_String;
                        Locale    : Locale_Type;
                        Handler   : in out Catalog_Handler_Type'Class) is
   begin
      Parse (File_Name, Facility, Locale, Handler);
   end Load_File;

   -------------------
   -- Map_To_Triple --
   -------------------

   procedure Map_To_Triple (Catalog  : Catalog_Type;
                            Facility : Wide_String;
                            Key      : Wide_String;
                            Locale   : Wide_String;
                            Triple   : out Message_Triple;
                            Create   : Boolean := False) is
   begin
      if Create then
         Catalog.Facilities.Add (Facility, Triple.Facility_Index);
         Catalog.Keys.Add (Key, Triple.Key_Index);
         Catalog.Locales.Add (Locale, Triple.Locale_Index);
      else
         Triple.Locale_Index := Catalog.Locales.Get (Locale,
                                   No_Such_Locale_Error'Identity);
         Triple.Key_Index := Catalog.Keys.Get (Key,
                                No_Such_Key_Error'Identity);
         Triple.Facility_Index := Catalog.Facilities.Get (Facility,
                                     No_Such_Facility_Error'Identity);
      end if;
   end Map_To_Triple;

   -------------------------
   -- Message_Triple_Hash --
   -------------------------

   function Message_Triple_Hash (Value : Message_Triple) return Hash_Type is
      type M is mod 2**31;
      Result : M := 0;
   begin
      Result := Result xor M'Mod (Value.Facility_Index);
      Result := Result * 10019;
      Result := Result xor M'Mod (Value.Key_Index);
      Result := Result * 10019;
      Result := Result xor M'Mod (Value.Locale_Index);
      Result := Result * 10019;
      return Hash_Type (Result);
   end Message_Triple_Hash;

   --------------------------
   -- Number_Of_Facilities --
   --------------------------

   function Number_Of_Facilities (Catalog : Catalog_Type) return Natural is
   begin
      return Catalog.Facilities.Length;
   end Number_Of_Facilities;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys (Catalog : Catalog_Type) return Natural is
   begin
      return Catalog.Keys.Length;
   end Number_Of_Keys;

   -----------------------
   -- Number_Of_Locales --
   -----------------------

   function Number_Of_Locales (Catalog : Catalog_Type) return Natural is
   begin
      return Catalog.Locales.Length;
   end Number_Of_Locales;

   ------------------------
   -- Number_Of_Messages --
   ------------------------

   function Number_Of_Messages (Catalog : Catalog_Type) return Natural is
   begin
      return Catalog.Messages.Length;
   end Number_Of_Messages;

   ---------------
   -- Pool_Size --
   ---------------

   function Pool_Size (Catalog : Catalog_Type) return Natural is
   begin
      if not Catalog.Single_Pool then
         raise Multiple_Pools_Error;
      end if;
      return Length (Catalog.Pool);
   end Pool_Size;

   -------------------
   -- Query_Message --
   -------------------

   procedure Query_Message (Catalog        : Catalog_Type;
                            Facility_Index : Positive;
                            Key_Index      : Positive;
                            Locale_Index   : Positive;
                            First          : out Positive;
                            Last           : out Natural) is

      Triple  : constant Message_Triple := (
                            Facility_Index => Facility_Index,
                            Key_Index => Key_Index,
                            Locale_Index => Locale_Index);
      Message : Message_Definition;

   begin
      if not Catalog.Single_Pool then
         raise Multiple_Pools_Error;
      end if;
      Message := Catalog.Messages.Get (Triple);
      First := Message.First;
      Last := Message.Last;
   exception
   when Constraint_Error =>
      raise No_Such_Message_Error with Positive'Image (Facility_Index)
                               & "/" & Positive'Image (Key_Index);
   end Query_Message;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Catalog    : Catalog_Type;
                      Pool_Size  : Natural := 0;
                      Messages   : Natural := 0) is
      pragma Unreferenced (Pool_Size);
   begin
      --  There doesn't appear to be an API to reserve space for an
      --  Unbounded String?  Skipping the Pool_Size adjustment.
      if Messages > 0 then
         Catalog.Messages.Adjust_Size (Messages);
      end if;
   end Reserve;

   -----------------
   -- Set_Catalog --
   -----------------

   procedure Set_Catalog (Handler : in out Catalog_Handler_Type;
                          Catalog : Catalog_Type) is
   begin
      Handler.Catalog := Catalog;
   end Set_Catalog;

   ---------------------
   -- Use_Single_Pool --
   ---------------------

   procedure Use_Single_Pool (Catalog : Catalog_Type) is
   begin
      Catalog.Single_Pool := True;
   end Use_Single_Pool;

   ---------------------
   -- Indexed_Strings --
   ---------------------

   protected body Indexed_Strings is

      ---------
      -- Add --
      ---------

      procedure Add (Name : Wide_String) is
         Index : Positive;
         pragma Warnings (Off, Index);
      begin
         Add (Name, Index);
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add (Name : Wide_String; Index : out Positive) is
         use type Name_To_Id_Maps.Cursor;
         Position : constant Name_To_Id_Maps.Cursor := Name_To_Id.Find (Name);
      begin
         if Position = Name_To_Id_Maps.No_Element then
            Id_To_Name.Append (Name);
            Index := Positive (Id_To_Name.Length);
            Name_To_Id.Insert (Name, Index);
         else
            Index := Name_To_Id_Maps.Element (Position);
         end if;
      end Add;

      ---------
      -- Get --
      ---------

      function Get (Index : Positive) return Wide_String is
      begin
         if Index <= Length then
            return Id_To_Name.Element (Index);
         else
            raise No_Such_Item;
         end if;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Name : Wide_String; Id : Exception_Id) return Positive is
         use type Name_To_Id_Maps.Cursor;
         Position : constant Name_To_Id_Maps.Cursor := Name_To_Id.Find (Name);
      begin
         if Position /= Name_To_Id_Maps.No_Element then
            return Name_To_Id_Maps.Element (Position);
         else
            Raise_Exception (Id, To_UTF8 (Name));
         end if;
      end Get;

      ------------
      -- Length --
      ------------

      function Length return Natural is
      begin
         --  ASSERT: Id_To_Name.Length = Name_To_Id.Length
         return Natural (Id_To_Name.Length);
      end Length;

   end Indexed_Strings;

   -----------------
   -- Message_Map --
   -----------------

   protected body Message_Map is

      function Text (Message : Message_Definition;
                     Pool    : Unbounded_Wide_String) return Wide_String;
      --  Return the text associated with a message definition, i.e., a
      --  sub-string pulled from the pool.

      ---------
      -- Add --
      ---------

      procedure Add (Triple  : Message_Triple;
                     Message : Message_Definition) is
         use Message_Maps;
         Position : constant Cursor := Find (Messages, Triple);
      begin
         if Position = No_Element then
            Messages.Insert (Triple, Message);
         else
            Messages.Replace_Element (Position, Message);
         end if;
      end Add;

      -----------------
      -- Adjust_Size --
      -----------------

      procedure Adjust_Size (Extra_Messages : Natural) is
         use Message_Maps;
         Capacity : constant Natural := Natural (Messages.Capacity);
         Size     : constant Natural := Natural (Messages.Length);
         New_Size : constant Natural := Size + Extra_Messages;
      begin
         if New_Size > Capacity then
            --  Extend the size of the messages container, if necessary
            Messages.Reserve_Capacity (Count_Type (New_Size));
         end if;
      end Adjust_Size;

      ---------
      -- Get --
      ---------

      function Get (Triple : Message_Triple) return Message_Definition is
      begin
         return Messages.Element (Triple);
      end Get;

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Handler : not null access
                                      procedure (Facility : Positive;
                                                 Key      : Positive;
                                                 Locale   : Positive;
                                                 First    : Positive;
                                                 Last     : Natural)) is
         use Message_Maps;

         procedure Callback (Position : Cursor);
         --  Ada.Containers callback used to reformat arguments to pass off to
         --  the supplied handler.

         --------------
         -- Callback --
         --------------

         procedure Callback (Position : Cursor) is
            M : constant Message_Definition := Element (Position);
            T : constant Message_Triple := Key (Position);
         begin
            Handler (T.Facility_Index, T.Key_Index, T.Locale_Index,
                     M.First, M.Last);
         end Callback;

      begin
         Messages.Iterate (Callback'Access);
      end Iterate;

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Pool    : Unbounded_Wide_String;
                         Handler : not null access
                                      procedure (Facility : Positive;
                                                 Key      : Positive;
                                                 Locale   : Positive;
                                                 Message  : Wide_String)) is
         use Message_Maps;

         procedure Callback (Position : Cursor);
         --  Ada.Containers callback used to reformat arguments to pass off to
         --  the supplied handler.

         --------------
         -- Callback --
         --------------

         procedure Callback (Position : Cursor) is
            T : constant Message_Triple := Key (Position);
         begin
            Handler (T.Facility_Index, T.Key_Index, T.Locale_Index,
                     Text (Element (Position), Pool));
         end Callback;

      begin
         Messages.Iterate (Callback'Access);
      end Iterate;

      ------------
      -- Length --
      ------------

      function Length return Natural is
      begin
         return Natural (Messages.Length);
      end Length;

      ----------
      -- Text --
      ----------

      function Text (Message : Message_Definition;
                     Pool    : Unbounded_Wide_String) return Wide_String is
      begin
         if Message.Pool /= null then
            return Message.Pool (Message.First .. Message.Last);
         else
            return Slice (Pool, Message.First, Message.Last);
         end if;
      end Text;

      ----------
      -- Text --
      ----------

      function Text (Triple : Message_Triple;
                     Pool   : Unbounded_Wide_String) return Wide_String is
         Message : constant Message_Definition := Get (Triple);
      begin
         if Message.Pool /= null then
            return Message.Pool (Message.First .. Message.Last);
         else
            return Slice (Pool, Message.First, Message.Last);
         end if;
      end Text;

   end Message_Map;

end ZanyBlue.Text.Catalogs;
