--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Exceptions;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Unbounded;
with ZanyBlue.OS;

package body ZanyBlue.Text.Catalogs is

   use Ada.Containers;
   use Ada.Exceptions;
   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.OS;

   type Message_Definition is
      record
         Pool         : Static_Message_Pool_Type;
         First        : Positive;
         Last         : Natural;
         Locale_Index : Locale_Index_Type := 1;
         Count        : Natural := 0;
      end record;

   type Message_Triple is
      record
         Facility_Index : Facility_Index_Type;
         Key_Index      : Key_Index_Type;
         Locale_Index   : Locale_Index_Type;
      end record;

   function Message_Triple_Hash (Value : in Message_Triple) return Hash_Type;
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

      function Get (Index : in Positive) return Wide_String;
      --  Get the name associated with a particular index.  Raises the
      --  exception No_Such_Item if the index does not exist.

      function Get (Name : in Wide_String;
                    Id   : in Exception_Id) return Positive;
      --  Find index associated with a particular name.  If not present,
      --  the argument exception is raised.

      procedure Add (Name : in Wide_String; Index : out Positive);
      --  Add a new name to the set.  The name is associated with the next
      --  index value, i.e., Length + 1.  This is a noop if the name already
      --  exists in the set.

   private

      Name_To_Id : Name_To_Id_Maps.Map;
      Id_To_Name : Id_To_Name_Vectors.Vector;

   end Indexed_Strings;

   --
   --  Message_Map
   --
   --  Protected type to store mappings from (Facility, Key, Locale)
   --  index triples to message strings.
   --
   protected type Message_Map is

      function Length return Natural;
      --  Return the number of messages currently stored in the map.

      procedure Get (Triple : in Message_Triple;
                     Result : out Message_Definition);
      --  Get the message associated with a particular index triple.  Raises
      --  the exception No_Such_Item if it does not exist.

      function Get_Pool return Wide_String;
      --  Return a copy of a the current pool data.  This is used primarily
      --  by the zbmcompile utility to dump a compiled set of .properties
      --  files.

      function Text (Message : in Message_Definition) return Wide_String;
      --  Get the message associated with a particular message.  Raises
      --  the exception No_Such_Item if it does not exist.

      function Pool_Size return Natural;
      --  Return the size of the current string pool.

      procedure Add (Triple  : in Message_Triple;
                     Message : in Message_Definition);
      --  Add a new message to the set.

      procedure Add (Triple        : in Message_Triple;
                     Message       : in Wide_String;
                     Source_Locale : in Locale_Index_Type);
      --  Add a new message to the set.

      procedure Adjust_Size (Extra_Messages : in Natural);
      --  When bulk loading (zbmcompile Initialize) adjust the size of the
      --  set to accommodate the new messages.

      procedure Iterate (
         Handler : not null
                      access
                         procedure (Facility      : in Facility_Index_Type;
                                    Key           : in Key_Index_Type;
                                    Locale        : in Locale_Index_Type;
                                    Source_Locale : in Locale_Index_Type;
                                    First         : in Positive;
                                    Last          : in Natural;
                                    Count         : in Natural));

      procedure Iterate (
         Handler : not null
                      access
                         procedure (Facility      : in Facility_Index_Type;
                                    Key           : in Key_Index_Type;
                                    Locale        : in Locale_Index_Type;
                                    Source_Locale : in Locale_Index_Type;
                                    Message       : in Wide_String;
                                    Count         : in Natural));

   private

      Messages     : Message_Maps.Map;
      Pool         : Unbounded_Wide_String;

   end Message_Map;

   type Catalog_Value is
      record
         Facilities     : Indexed_Strings;
         Keys           : Indexed_Strings;
         Locales        : Indexed_Strings;
         Messages       : Message_Map;
         Logical_Size   : Natural := 0;
         Pseudo_Map     : Pseudo_Map_Access;
         Mark_Messages  : Boolean := True;
         Mark_Arguments : Boolean := True;
         Single_Pool    : Boolean := False;
         Raise_Errors   : Boolean := True;
         Source_Locales : Boolean := True;
      end record;

   function Map_To_Triple (Catalog  : in Catalog_Type;
                           Facility : in Wide_String;
                           Key      : in Wide_String;
                           Locale   : in Locale_Type;
                           Create   : in Boolean) return Message_Triple;

   ---------
   -- Add --
   ---------

   procedure Add (Catalog          : in Catalog_Type;
                  Facility         : in Wide_String;
                  Key              : in Wide_String;
                  Message          : in Wide_String;
                  Locale           : in Locale_Type) is
   begin
      Add (Catalog, Facility, Key, Message, Locale, Locale);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Catalog       : in Catalog_Type;
                  Facility      : in Wide_String;
                  Key           : in Wide_String;
                  Message       : in Wide_String;
                  Locale        : in Locale_Type;
                  Source_Locale : in Locale_Type) is

      Triple : constant Message_Triple := Map_To_Triple (Catalog,
                                                         Facility,
                                                         Key,
                                                         Locale,
                                                         True);

   begin
      Add_Locale (Catalog, Source_Locale);
      Catalog.C.Logical_Size := Catalog.C.Logical_Size + Message'Length;
      Catalog.C.Messages.Add (Triple, Message,
                              Get_Locale_Index (Catalog, Source_Locale));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Catalog          : in Catalog_Type;
                  Facility         : in Wide_String;
                  Key              : in Wide_String;
                  Pool             : in Static_Message_Pool_Type;
                  First            : in Positive;
                  Last             : in Natural;
                  Locale           : in Locale_Type) is
   begin
      Add (Catalog, Facility, Key, Pool, First, Last, Locale, Locale);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Catalog       : in Catalog_Type;
                  Facility      : in Wide_String;
                  Key           : in Wide_String;
                  Pool          : in Static_Message_Pool_Type;
                  First         : in Positive;
                  Last          : in Natural;
                  Locale        : in Locale_Type;
                  Source_Locale : in Locale_Type) is

      New_Message : Message_Definition;
      Triple      : Message_Triple;

   begin
      Add_Locale (Catalog, Source_Locale);
      --  Validate the message is actually within the argument pool
      if First < Pool.all'First or else Last > Pool.all'Last then
         raise Invalid_Static_Message_Error;
      end if;
      if Catalog.C.Single_Pool then
         --  Application requested all message be stored in a single
         --  pool: simply add static message to the dynamic pool area.
         --  This is normally used by "zbmcompile" when generating code.
         Add (Catalog, Facility, Key, Pool.all (First .. Last), Locale,
              Source_Locale);
         return;
      end if;
      Triple := Map_To_Triple (Catalog, Facility, Key, Locale, True);
      New_Message.Pool := Pool;
      New_Message.First := First;
      New_Message.Last := Last;
      New_Message.Locale_Index := Get_Locale_Index (Catalog, Source_Locale);
      Catalog.C.Messages.Add (Triple, New_Message);
   end Add;

   ------------------
   -- Add_Facility --
   ------------------

   procedure Add_Facility (Catalog  : in Catalog_Type;
                           Facility : in Wide_String) is
      Index : Facility_Index_Type;             --  Throwaway value
      pragma Warnings (Off, Index);
   begin
      Add_Facility (Catalog, Facility, Index);
   end Add_Facility;

   ------------------
   -- Add_Facility --
   ------------------

   procedure Add_Facility (Catalog  : in Catalog_Type;
                           Facility : in Wide_String;
                           Index    : out Facility_Index_Type) is
   begin
      Catalog.C.Facilities.Add (Facility, Index);
   end Add_Facility;

   -------------
   -- Add_Key --
   -------------

   procedure Add_Key (Catalog  : in Catalog_Type;
                      Key      : in Wide_String) is
      Index : Key_Index_Type;             --  Throwaway value
      pragma Warnings (Off, Index);
   begin
      Add_Key (Catalog, Key, Index);
   end Add_Key;

   -------------
   -- Add_Key --
   -------------

   procedure Add_Key (Catalog  : in Catalog_Type;
                      Key      : in Wide_String;
                      Index    : out Key_Index_Type) is
   begin
      Catalog.C.Keys.Add (Key, Index);
   end Add_Key;

   -------------------
   -- Add_Key_Value --
   -------------------

   procedure Add_Key_Value (Handler       : in out Catalog_Handler_Type;
                            Facility      : in Wide_String;
                            Key           : in Wide_String;
                            Value         : in Wide_String;
                            Locale        : in Locale_Type;
                            Source_Locale : in Locale_Type;
                            File_Name     : in Wide_String;
                            Line          : in Natural) is
      pragma Unreferenced (File_Name);
      pragma Unreferenced (Line);
   begin
      Add (Handler.Catalog, Facility, Key, Value, Locale, Source_Locale);
   end Add_Key_Value;

   ----------------
   -- Add_Locale --
   ----------------

   procedure Add_Locale (Catalog  : in Catalog_Type;
                         Locale   : in Locale_Type) is
      Index : Locale_Index_Type;             --  Throwaway value
      pragma Warnings (Off, Index);
   begin
      Add_Locale (Catalog, Locale, Index);
   end Add_Locale;

   ----------------
   -- Add_Locale --
   ----------------

   procedure Add_Locale (Catalog  : in Catalog_Type;
                         Locale   : in Locale_Type;
                         Index    : out Locale_Index_Type) is
   begin
      Catalog.C.Locales.Add (Locale_Name (Locale), Index);
   end Add_Locale;

   ------------
   -- Create --
   ------------

   function Create return Catalog_Type is
      Result : constant Catalog_Type := (C => new Catalog_Value);
   begin
      return Result;
   end Create;

   ------------------------
   -- Disable_Exceptions --
   ------------------------

   procedure Disable_Exceptions (Catalog : in Catalog_Type) is
   begin
      Catalog.C.Raise_Errors := False;
   end Disable_Exceptions;

   ----------------------------
   -- Disable_Source_Locales --
   ----------------------------

   procedure Disable_Source_Locales (Catalog : in Catalog_Type) is
   begin
      Catalog.C.Source_Locales := False;
   end Disable_Source_Locales;

   ----------
   -- Dump --
   ----------

   procedure Dump (Catalog   : in Catalog_Type;
                   File_Name : in Wide_String := "") is
      separate;

   -------------------
   -- Duplicate_Key --
   -------------------

   procedure Duplicate_Key (Handler       : in out Catalog_Handler_Type;
                            Facility      : in Wide_String;
                            Key           : in Wide_String;
                            Locale        : in Locale_Type;
                            File_Name     : in Wide_String;
                            Current_Line  : in Natural;
                            Previous_Line : in Natural) is
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

   procedure Enable_Exceptions (Catalog : in Catalog_Type) is
   begin
      Catalog.C.Raise_Errors := True;
   end Enable_Exceptions;

   --------------------------------
   -- Enable_Pseudo_Translations --
   --------------------------------

   procedure Enable_Pseudo_Translations (
      Catalog        : in Catalog_Type;
      Mapping        : in Pseudo_Map_Vector;
      Mark_Messages  : in Boolean := True;
      Mark_Arguments : in Boolean := True)
   is
   begin
      Catalog.C.Pseudo_Map := new Pseudo_Map_Type;
      Catalog.C.Pseudo_Map.Add_Mapping (Mapping);
      Catalog.C.Mark_Messages  := Mark_Messages;
      Catalog.C.Mark_Arguments := Mark_Arguments;
   end Enable_Pseudo_Translations;

   ---------------------------
   -- Enable_Source_Locales --
   ---------------------------

   procedure Enable_Source_Locales (Catalog : in Catalog_Type) is
   begin
      Catalog.C.Source_Locales := True;
   end Enable_Source_Locales;

   ------------------------
   -- Exceptions_Enabled --
   ------------------------

   function Exceptions_Enabled (Catalog : in Catalog_Type) return Boolean is
   begin
      return Catalog.C.Raise_Errors;
   end Exceptions_Enabled;

   -----------------
   -- Get_Catalog --
   -----------------

   function Get_Catalog (Handler : in Catalog_Handler_Type)
      return Catalog_Type
   is
   begin
      return Handler.Catalog;
   end Get_Catalog;

   ------------------
   -- Get_Facility --
   ------------------

   function Get_Facility (Catalog : in Catalog_Type;
                          Index   : in Facility_Index_Type)
      return Wide_String
   is
   begin
      return Catalog.C.Facilities.Get (Positive (Index));
   exception
   when No_Such_Item =>
      raise No_Such_Facility_Error with Facility_Index_Type'Image (Index);
   end Get_Facility;

   ------------------------
   -- Get_Facility_Index --
   ------------------------

   function Get_Facility_Index (Catalog : in Catalog_Type;
                                Name    : in Wide_String)
      return Facility_Index_Type
   is
      Index : Positive;
   begin
      Index := Catalog.C.Facilities.Get (Name,
                                         No_Such_Facility_Error'Identity);
      return Facility_Index_Type (Index);
   end Get_Facility_Index;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Catalog : in Catalog_Type;
                     Index   : in Key_Index_Type) return Wide_String is
   begin
      return Catalog.C.Keys.Get (Positive (Index));
   exception
   when No_Such_Item =>
      raise No_Such_Key_Error with Key_Index_Type'Image (Index);
   end Get_Key;

   -------------------
   -- Get_Key_Index --
   -------------------

   function Get_Key_Index (Catalog : in Catalog_Type;
                           Name    : in Wide_String) return Key_Index_Type is
      Index : Positive;
   begin
      Index := Catalog.C.Keys.Get (Name, No_Such_Key_Error'Identity);
      return Key_Index_Type (Index);
   end Get_Key_Index;

   ----------------
   -- Get_Locale --
   ----------------

   function Get_Locale (Catalog : in Catalog_Type;
                        Index   : in Locale_Index_Type) return Locale_Type is
   begin
      return Make_Locale (Catalog.C.Locales.Get (Positive (Index)));
   exception
   when No_Such_Item =>
      raise No_Such_Locale_Error with Locale_Index_Type'Image (Index);
   end Get_Locale;

   ----------------------
   -- Get_Locale_Index --
   ----------------------

   function Get_Locale_Index (Catalog : in Catalog_Type;
                              Name    : in Wide_String)
      return Locale_Index_Type
   is
      Index : Positive;
   begin
      Index := Catalog.C.Locales.Get (Name, No_Such_Locale_Error'Identity);
      return Locale_Index_Type (Index);
   end Get_Locale_Index;

   ----------------------
   -- Get_Locale_Index --
   ----------------------

   function Get_Locale_Index (Catalog : in Catalog_Type;
                              Locale  : in Locale_Type)
      return Locale_Index_Type
   is
   begin
      return Get_Locale_Index (Catalog, Locale_Name (Locale));
   end Get_Locale_Index;

   ---------------------
   -- Get_Locale_Name --
   ---------------------

   function Get_Locale_Name (Catalog : in Catalog_Type;
                             Index   : in Locale_Index_Type)
      return Wide_String
   is
   begin
      return Locale_Name (Get_Locale (Catalog, Index));
   end Get_Locale_Name;

   ------------------------
   -- Get_Mark_Arguments --
   ------------------------

   function Get_Mark_Arguments (Catalog : in Catalog_Type) return Boolean is
   begin
      return Catalog.C.Mark_Arguments;
   end Get_Mark_Arguments;

   -----------------------
   -- Get_Mark_Messages --
   -----------------------

   function Get_Mark_Messages (Catalog : in Catalog_Type) return Boolean is
   begin
      return Catalog.C.Mark_Messages;
   end Get_Mark_Messages;

   --------------
   -- Get_Pool --
   --------------

   function Get_Pool (Catalog : in Catalog_Type) return Wide_String is
   begin
      if not Catalog.C.Single_Pool then
         raise Multiple_Pools_Error;
      end if;
      return Catalog.C.Messages.Get_Pool;
   end Get_Pool;

   --------------------
   -- Get_Pseudo_Map --
   --------------------

   function Get_Pseudo_Map (Catalog : in Catalog_Type)
      return Pseudo_Map_Access
   is
   begin
      return Catalog.C.Pseudo_Map;
   end Get_Pseudo_Map;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Catalog         : in Catalog_Type;
                      Facility        : in Wide_String;
                      Key             : in Wide_String;
                      Locale          : in Locale_Type;
                      Message_Locale  : access Locale_Type := null)
      return Wide_String
   is

      Message        : Message_Definition;
      Language       : Language_Type;
      Script         : Script_Type;
      Territory      : Territory_Type;
      Base_Territory : Territory_Type;
      Triple         : Message_Triple;

   begin
      Get_Locale_Codes (Locale, Language, Script, Territory);
      Base_Territory := Territory;
      Locale_Loop : for I in 1 .. Maximum_Locale_Parents loop
         Map_Locale_Triple : begin
            Triple := Map_To_Triple (Catalog, Facility, Key,
                                     Make_Locale (Language, Script, Territory),
                                     False);
            Catalog.C.Messages.Get (Triple, Message);
            if Message_Locale /= null then
               Message_Locale.all := Get_Locale (Catalog,
                                                 Message.Locale_Index);
            end if;
            return Catalog.C.Messages.Text (Message);
         exception
         when No_Such_Locale_Error | Constraint_Error =>
            exit Locale_Loop when Language = Empty_Language;
            Parent_Codes (Language, Script, Territory,
                          Base_Territory => Base_Territory);
         end Map_Locale_Triple;
      end loop Locale_Loop;
      raise No_Such_Message_Error with To_UTF8 (Facility & "/" & Key);
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Catalog        : in Catalog_Type;
                      Facility_Index : in Facility_Index_Type;
                      Key_Index      : in Key_Index_Type;
                      Locale_Index   : in Locale_Index_Type) return Wide_String
   is

      Message : Message_Definition;
      Triple  : constant Message_Triple := (
                            Facility_Index => Facility_Index,
                            Key_Index      => Key_Index,
                            Locale_Index   => Locale_Index);

   begin
      Catalog.C.Messages.Get (Triple, Message);
      return Catalog.C.Messages.Text (Message);
   exception
   when Constraint_Error =>
      raise No_Such_Message_Error
         with Facility_Index_Type'Image (Facility_Index)
            & "/" & Key_Index_Type'Image (Key_Index);
   end Get_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Catalog         : in Catalog_Type;
                         Messages        : in ZBMCompile_List;
                         Pool            : in Static_Message_Pool_Type;
                         Facilities      : in Constant_String_List;
                         Keys            : in Constant_String_List;
                         Locales         : in Constant_String_List;
                         Package_Name    : in Wide_String := "";
                         Pool_Length     : in Natural := 0;
                         Expected_Length : in Natural := 0) is
      F, K, L, EL : Positive;
   begin
      if Pool_Length /= Expected_Length then
         raise Pool_Size_Mismatch_Error with
            To_UTF8 (Package_Name) &
               Natural'Image (Pool_Length) & " /=" &
               Natural'Image (Expected_Length);
      end if;
      Reserve (Catalog, Messages => Messages'Length);
      for I in Messages'Range loop
         F := Positive (Messages (I).Facility_Index);
         K := Positive (Messages (I).Key_Index);
         L := Positive (Messages (I).Locale_Index);
         EL := Positive (Messages (I).Source_Locale_Index);
         Add (Catalog, Facilities (F).all,
                       Keys (K).all,
                       Pool,
                       Messages (I).First,
                       Messages (I).Last,
                       Make_Locale (Locales (L).all),
                       Make_Locale (Locales (EL).all));
      end loop;
   end Initialize;

   -----------------------
   -- Invalid_Character --
   -----------------------

   procedure Invalid_Character (Handler         : in out Catalog_Handler_Type;
                                Facility        : in Wide_String;
                                File_Name       : in Wide_String;
                                Current_Line    : in Natural;
                                Ch              : in Character) is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
   begin
      raise Unicode_Escape_Error with
                       To_UTF8 (File_Name) & ":"
                     & Natural'Image (Current_Line) & ":"
                     & "Invalid character: " & Ch;
   end Invalid_Character;

   ------------------------
   -- Invalid_Definition --
   ------------------------

   procedure Invalid_Definition (Handler         : in out Catalog_Handler_Type;
                                 Facility        : in Wide_String;
                                 Locale          : in Locale_Type;
                                 File_Name       : in Wide_String;
                                 Current_Line    : in Natural;
                                 Additional_Info : in String) is
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

   function Is_Valid (Catalog : in Catalog_Type) return Boolean is
   begin
      return Catalog.C /= null;
   end Is_Valid;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (
      Catalog : in Catalog_Type;
      Handler : not null access
                   procedure (Facility      : in Facility_Index_Type;
                              Key           : in Key_Index_Type;
                              Locale        : in Locale_Index_Type;
                              Source_Locale : in Locale_Index_Type;
                              First         : in Positive;
                              Last          : in Natural;
                              Count         : in Natural)) is
   begin
      Catalog.C.Messages.Iterate (Handler);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (
      Catalog : in Catalog_Type;
      Handler : not null access
                   procedure (Facility      : in Facility_Index_Type;
                              Key           : in Key_Index_Type;
                              Locale        : in Locale_Index_Type;
                              Source_Locale : in Locale_Index_Type;
                              Message       : in Wide_String;
                              Count         : in Natural)) is
   begin
      Catalog.C.Messages.Iterate (Handler);
   end Iterate;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility (
      Facility           : in Wide_String;
      Source_Name        : in Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Handler            : in out Catalog_Handler_Type'Class;
      Directory          : in Wide_String := ".";
      Extension          : in Wide_String := Default_Extension;
      Base_Locale_Only   : in Boolean := False;
      Locale_Prefix      : in Wide_String := "";
      Source_Root_Locale : in Locale_Type := Root_Locale)
   is separate;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility (
      Catalog            : in Catalog_Type;
      Facility           : in Wide_String;
      Source_Name        : in Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Directory          : in Wide_String := ".";
      Extension          : in Wide_String := Default_Extension;
      Base_Locale_Only   : in Boolean := False;
      Locale_Prefix      : in Wide_String := "";
      Source_Root_Locale : in Locale_Type := Root_Locale)
   is

      Handler : Catalog_Handler_Type;

   begin
      Handler.Catalog := Catalog;
      Load_Facility (Facility, Source_Name, N_Locales, N_Messages, Handler,
                     Directory => Directory,
                     Extension => Extension,
                     Base_Locale_Only => Base_Locale_Only,
                     Locale_Prefix => Locale_Prefix,
                     Source_Root_Locale => Source_Root_Locale);
   end Load_Facility;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility (
      Catalog            : in Catalog_Type;
      Facility           : in Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Directory          : in Wide_String := ".";
      Extension          : in Wide_String := Default_Extension;
      Source_Root_Locale : in Locale_Type := Root_Locale)
   is
   begin
      Load_Facility (Catalog, Facility, Facility,
                     N_Locales, N_Messages,
                     Directory => Directory,
                     Extension => Extension,
                     Source_Root_Locale => Source_Root_Locale);
   end Load_Facility;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (Catalog       : in Catalog_Type;
                       File_Name     : in Wide_String;
                       Facility      : in Wide_String;
                       Locale        : in Locale_Type;
                       Source_Locale : in Locale_Type := Root_Locale)
      return Natural
   is

      Handler : Catalog_Handler_Type;

   begin
      Add_Locale (Catalog, Source_Locale);
      Handler.Set_Catalog (Catalog);
      Parse (Handler, File_Name, Facility, Locale, Source_Locale);
      return Handler.Get_N_Messages;
   end Load_File;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (File_Name     : in Wide_String;
                        Facility      : in Wide_String;
                        Locale        : in Locale_Type;
                        Handler       : in out Catalog_Handler_Type'Class;
                        Source_Locale : in Locale_Type := Root_Locale) is
   begin
      Parse (Handler, File_Name, Facility, Locale, Source_Locale);
   end Load_File;

   -----------------------
   -- Logical_Pool_Size --
   -----------------------

   function Logical_Pool_Size (Catalog : in Catalog_Type) return Natural is
   begin
      return Catalog.C.Logical_Size;
   end Logical_Pool_Size;

   -------------------
   -- Map_To_Triple --
   -------------------

   function Map_To_Triple (Catalog  : in Catalog_Type;
                           Facility : in Wide_String;
                           Key      : in Wide_String;
                           Locale   : in Locale_Type;
                           Create   : in Boolean) return Message_Triple is
      Result : Message_Triple;
   begin
      if Create then
         Add_Facility (Catalog, Facility, Result.Facility_Index);
         Add_Key (Catalog, Key, Result.Key_Index);
         Add_Locale (Catalog, Locale, Result.Locale_Index);
      else
         Result.Facility_Index := Get_Facility_Index (Catalog, Facility);
         Result.Key_Index      := Get_Key_Index (Catalog, Key);
         Result.Locale_Index   := Get_Locale_Index (Catalog, Locale);
      end if;
      return Result;
   end Map_To_Triple;

   -------------------------
   -- Message_Triple_Hash --
   -------------------------

   function Message_Triple_Hash (Value : in Message_Triple) return Hash_Type is
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

   function Number_Of_Facilities (Catalog : in Catalog_Type) return Natural is
   begin
      return Catalog.C.Facilities.Length;
   end Number_Of_Facilities;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys (Catalog : in Catalog_Type) return Natural is
   begin
      return Catalog.C.Keys.Length;
   end Number_Of_Keys;

   -----------------------
   -- Number_Of_Locales --
   -----------------------

   function Number_Of_Locales (Catalog : in Catalog_Type) return Natural is
   begin
      return Catalog.C.Locales.Length;
   end Number_Of_Locales;

   ------------------------
   -- Number_Of_Messages --
   ------------------------

   function Number_Of_Messages (Catalog : in Catalog_Type) return Natural is
   begin
      return Catalog.C.Messages.Length;
   end Number_Of_Messages;

   ---------------
   -- Pool_Size --
   ---------------

   function Pool_Size (Catalog : in Catalog_Type) return Natural is
   begin
      return Catalog.C.Messages.Pool_Size;
   end Pool_Size;

   -------------------
   -- Query_Message --
   -------------------

   procedure Query_Message (Catalog        : in Catalog_Type;
                            Facility_Index : in Facility_Index_Type;
                            Key_Index      : in Key_Index_Type;
                            Locale_Index   : in Locale_Index_Type;
                            First          : out Positive;
                            Last           : out Natural) is

      Triple  : constant Message_Triple := (
                            Facility_Index => Facility_Index,
                            Key_Index => Key_Index,
                            Locale_Index => Locale_Index);
      Message : Message_Definition;

   begin
      if not Catalog.C.Single_Pool then
         raise Multiple_Pools_Error;
      end if;
      Catalog.C.Messages.Get (Triple, Message);
      First := Message.First;
      Last := Message.Last;
   exception
   when Constraint_Error =>
      raise No_Such_Message_Error
         with Facility_Index_Type'Image (Facility_Index)
            & "/" & Key_Index_Type'Image (Key_Index);
   end Query_Message;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Catalog    : in Catalog_Type;
                      Pool_Size  : in Natural := 0;
                      Messages   : in Natural := 0) is
      pragma Unreferenced (Pool_Size);
   begin
      --  There doesn't appear to be an API to reserve space for an
      --  Unbounded String?  Skipping the Pool_Size adjustment.
      if Messages > 0 then
         Catalog.C.Messages.Adjust_Size (Messages);
      end if;
   end Reserve;

   -----------------
   -- Set_Catalog --
   -----------------

   procedure Set_Catalog (Handler : in out Catalog_Handler_Type;
                          Catalog : in Catalog_Type) is
   begin
      Handler.Catalog := Catalog;
   end Set_Catalog;

   ----------------------------
   -- Source_Locales_Enabled --
   ----------------------------

   function Source_Locales_Enabled (Catalog : in Catalog_Type)
      return Boolean
   is
   begin
      return Catalog.C.Source_Locales;
   end Source_Locales_Enabled;

   ---------------------
   -- Use_Single_Pool --
   ---------------------

   procedure Use_Single_Pool (Catalog : in Catalog_Type) is
   begin
      Catalog.C.Single_Pool := True;
   end Use_Single_Pool;

   ---------------------
   -- Indexed_Strings --
   ---------------------

   protected body Indexed_Strings is

      ---------
      -- Add --
      ---------

      procedure Add (Name  : in Wide_String;
                     Index : out Positive) is
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

      function Get (Index : in Positive) return Wide_String is
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

      function Get (Name : in Wide_String;
                    Id   : in Exception_Id) return Positive is
         use type Name_To_Id_Maps.Cursor;
         Position : constant Name_To_Id_Maps.Cursor := Name_To_Id.Find (Name);
      begin
         if Position /= Name_To_Id_Maps.No_Element then
            return Name_To_Id_Maps.Element (Position);
         else
            Raise_Exception (Id, Message => To_UTF8 (Name));
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

      ---------
      -- Add --
      ---------

      procedure Add (Triple  : in Message_Triple;
                     Message : in Message_Definition) is
         use Message_Maps;
         Position : constant Cursor := Find (Messages, Triple);
      begin
         if Position = No_Element then
            Messages.Insert (Triple, Message);
         else
            Messages.Replace_Element (Position, Message);
         end if;
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add (Triple        : in Message_Triple;
                     Message       : in Wide_String;
                     Source_Locale : in Locale_Index_Type) is
         New_Message : Message_Definition;
         First       : Natural := 0;
         Last        : Natural;
      begin
         if Message'Length > 0 then
            --  Attempt to locate the message in the existing pool
            First := Index (Pool, Message);
            Last  := First + Message'Length - 1;
         end if;
         if First = 0 then
            --  Failed to find it in the existing pool, add it
            First := Length (Pool) + 1;
            Append (Pool, Message);
            Last := Length (Pool);
         end if;
         New_Message.Pool := null;
         New_Message.First := First;
         New_Message.Last := Last;
         New_Message.Locale_Index := Source_Locale;
         Add (Triple, New_Message);
      end Add;

      -----------------
      -- Adjust_Size --
      -----------------

      procedure Adjust_Size (Extra_Messages : in Natural) is
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

      procedure Get (Triple : in Message_Triple;
                     Result : out Message_Definition) is

         use Message_Maps;

         procedure Increment_Count (Key     : in Message_Triple;
                                    Element : in out Message_Definition);

         procedure Increment_Count (Key     : in Message_Triple;
                                    Element : in out Message_Definition) is
            pragma Unreferenced (Key);
         begin
            if Element.Count < Natural'Last then
               Element.Count := Element.Count + 1;
            end if;
         end Increment_Count;

         Position : constant Cursor := Messages.Find (Triple);

      begin
         if Position /= No_Element then
            Update_Element (Messages, Position, Increment_Count'Access);
         end if;
         Result := Messages.Element (Triple);
      end Get;

      --------------
      -- Get_Pool --
      --------------

      function Get_Pool return Wide_String is
      begin
         return To_Wide_String (Pool);
      end Get_Pool;

      -------------
      -- Iterate --
      -------------

      procedure Iterate (
         Handler : not null
                      access
                         procedure (Facility      : in Facility_Index_Type;
                                    Key           : in Key_Index_Type;
                                    Locale        : in Locale_Index_Type;
                                    Source_Locale : in Locale_Index_Type;
                                    First         : in Positive;
                                    Last          : in Natural;
                                    Count         : in Natural)) is
         use Message_Maps;

         procedure Callback (Position : in Cursor);
         --  Ada.Containers callback used to reformat arguments to pass off to
         --  the supplied handler.

         --------------
         -- Callback --
         --------------

         procedure Callback (Position : in Cursor) is
            M : constant Message_Definition := Element (Position);
            T : constant Message_Triple := Key (Position);
         begin
            Handler (T.Facility_Index, T.Key_Index, T.Locale_Index,
                     M.Locale_Index, M.First, M.Last, M.Count);
         end Callback;

      begin
         Messages.Iterate (Callback'Access);
      end Iterate;

      -------------
      -- Iterate --
      -------------

      procedure Iterate (
         Handler : not null
                      access
                         procedure (Facility      : in Facility_Index_Type;
                                    Key           : in Key_Index_Type;
                                    Locale        : in Locale_Index_Type;
                                    Source_Locale : in Locale_Index_Type;
                                    Message       : in Wide_String;
                                    Count         : in Natural))
      is
         use Message_Maps;

         procedure Callback (Position : in Cursor);
         --  Ada.Containers callback used to reformat arguments to pass off to
         --  the supplied handler.

         --------------
         -- Callback --
         --------------

         procedure Callback (Position : in Cursor) is
            M : constant Message_Definition := Element (Position);
            T : constant Message_Triple := Key (Position);
         begin
            Handler (T.Facility_Index, T.Key_Index, T.Locale_Index,
                     M.Locale_Index, Text (Element (Position)), M.Count);
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

      ---------------
      -- Pool_Size --
      ---------------

      function Pool_Size return Natural is
      begin
         return Length (Pool);
      end Pool_Size;

      ----------
      -- Text --
      ----------

      function Text (Message : in Message_Definition) return Wide_String is
      begin
         if Message.Pool /= null then
            return Message.Pool (Message.First .. Message.Last);
         else
            return Slice (Pool, Message.First, Message.Last);
         end if;
      end Text;

   end Message_Map;

end ZanyBlue.Text.Catalogs;
