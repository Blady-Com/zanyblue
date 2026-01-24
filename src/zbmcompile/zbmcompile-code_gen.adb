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
with Ada.Wide_Characters.Unicode;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;

package body ZBMCompile.Code_Gen is

   use Ada.Wide_Text_IO;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Formatting;

   en_US              : constant Locale_Type := Make_Locale ("en_US");
   Output_Buffer_Size : constant := 60;

   procedure Optimize_Locale (Catalog      : Catalog_Type;
                              Result       : Catalog_Type;
                              Locale_Index : Positive);
   --  Optimize a locale my copying all strings for the locale from the
   --  source Catalog to the destination (Result).

   procedure Write_Name_List (File     : in out File_Type;
                              Catalog  : Catalog_Type;
                              Name_Def : Wide_String;
                              List_Def : Wide_String;
                              Unitary  : Wide_String;
                              List_N   : Wide_String;
                              Last     : Wide_String;
                              N        : Natural;
                              Namer    : access
                                             function (Catalog : Catalog_Type;
                                                       I       : Positive)
                                                return Wide_String);
   --  Write a list of names to the generated source file, Keys, Locales, etc.

   procedure Generate_Spec (Catalog         : Catalog_Type;
                            Package_Name    : Wide_String;
                            Spec_Name       : Wide_String;
                            Body_Initialize : Boolean;
                            Export_Name     : Wide_String;
                            Verbose         : Boolean);
   --  Generate the specification file for the generated package.

   procedure Generate_Body (Catalog         : Catalog_Type;
                            Package_Name    : Wide_String;
                            Body_Name       : Wide_String;
                            Body_Initialize : Boolean;
                            Verbose         : Boolean);
   --  Generate the body file for the generated package.

   procedure Generate_Pool (File : in out File_Type;
                            Pool : Wide_String);
   --  Generate the definition of the package variables defining the string
   --  pool used for the messages.  The full pool is written to the file
   --  in a series of sub-strings concatenated together (the output line
   --  length should not exceed 80 for Ada style reasons).  The nested
   --  procedures and functions are defined to allow this sub-stringing
   --  and handle the case of control characters in the pool data which
   --  are written on their own lines.

   procedure Generate_Message_List (File    : in out File_Type;
                                    Catalog : Catalog_Type);
   --  Generate the code to add the messages using indexes to vectors already
   --  generated.

   procedure Generate_Initialize (File : in out File_Type;
                                  Catalog : Catalog_Type);
   --  Generate the initialization routine to define the strings for a
   --  catalog.

   procedure Write_String (File       : in out File_Type;
                           Value      : Wide_String;
                           Decl_Key   : Wide_String;
                           Decl_Index : Positive := 1);
   --  Write a string allowing for line over-runs.


   ------------------
   -- Generate_Ada --
   ------------------

   procedure Generate_Ada (Catalog         : Catalog_Type;
                           Package_Name    : Wide_String;
                           Spec_Name       : Wide_String;
                           Body_Name       : Wide_String;
                           Verbose         : Boolean;
                           Body_Initialize : Boolean;
                           Export_Name     : Wide_String) is
   begin
      Generate_Spec (Catalog, Package_Name, Spec_Name,
                     Body_Initialize, Export_Name, Verbose);
      Generate_Body (Catalog, Package_Name, Body_Name,
                     Body_Initialize, Verbose);
   end Generate_Ada;

   -------------------
   -- Generate_Body --
   -------------------

   procedure Generate_Body (Catalog         : Catalog_Type;
                            Package_Name    : Wide_String;
                            Body_Name       : Wide_String;
                            Body_Initialize : Boolean;
                            Verbose         : Boolean) is
      File : File_Type;
   begin
      Wide_Create (File, Body_Name);
      Print_Line (File, "zbmcode", "20000",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "20001",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "20002",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "20003",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "20004",
                        Locale => en_US);
      New_Line (File);
      Print_Line (File, "zbmcode", "20005", +Package_Name,
                        Locale => en_US);
      New_Line (File);
      Print_Line (File, "zbmcode", "20006",
                        Locale => en_US);
      New_Line (File);
      Write_Name_List (File, Catalog,
                       "20007", "20008", "20010", "20009", "20011",
                       Number_Of_Keys (Catalog), Get_Key'Access);
      Write_Name_List (File, Catalog,
                       "20012", "20013", "20015", "20014", "20016",
                       Number_Of_Locales (Catalog), Get_Locale_Name'Access);
      Generate_Pool (File, Get_Pool (Catalog));
      Generate_Message_List (File, Catalog);
      Generate_Initialize (File, Catalog);
      New_Line (File);
      if Body_Initialize then
         Print_Line (File, "zbmcode", "20031",
                           Locale => en_US);
         Print_Line (File, "zbmcode", "20032",
                           Locale => en_US);
      end if;
      Print_Line (File, "zbmcode", "20033", +Package_Name,
                        Locale => en_US);
      Close (File);
      if Verbose then
         Print_Line ("zbmcompile", "00022", +Package_Name, +Body_Name);
      end if;
   end Generate_Body;

   -------------------------
   -- Generate_Initialize --
   -------------------------

   procedure Generate_Initialize (File : in out File_Type;
                                  Catalog : Catalog_Type) is
   begin
      Print_Line (File, "zbmcode", "20026",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "20027",
                        Locale => en_US);
      if Number_Of_Keys (Catalog) > 0 then
         Print_Line (File, "zbmcode", "20028",
                           Locale => en_US);
      else
         Print_Line (File, "zbmcode", "20029",
                           Locale => en_US);
      end if;
      Print_Line (File, "zbmcode", "20030",
                        Locale => en_US);
   end Generate_Initialize;

   ---------------------------
   -- Generate_Message_List --
   ---------------------------

   procedure Generate_Message_List (File    : in out File_Type;
                                    Catalog : Catalog_Type) is

      procedure Add_Message (F        : Positive;
                             K        : Positive;
                             L        : Positive;
                             First    : Positive;
                             Last     : Natural);
      --  Add an individual message definition, handling the case where
      --  the message is the last message in the list, i.e., no comma.

      N_Messages : constant Natural := Number_Of_Messages (Catalog);
      Last_Key   : Wide_String (1 .. 5) := "20025";
      Current    : Positive := 1;

      -----------------
      -- Add_Message --
      -----------------

      procedure Add_Message (F        : Positive;
                             K        : Positive;
                             L        : Positive;
                             First    : Positive;
                             Last     : Natural) is
         Arguments : Argument_List;
      begin
         Arguments.Append (+F);
         Arguments.Append (+K);
         Arguments.Append (+L);
         Arguments.Append (+First);
         Arguments.Append (+Last);
         if Current = N_Messages then
            Print_Line (File, "zbmcode", Last_Key, Arguments,
                              Locale => en_US);
         else
            Print_Line (File, "zbmcode", "20024", Arguments,
                              Locale => en_US);
         end if;
         Current := Current + 1;
      end Add_Message;

   begin
      if N_Messages = 1 then
         Last_Key := "20023";
      end if;
      Print_Line (File, "zbmcode", "20022", +N_Messages,
                        Locale => en_US);
      Iterate (Catalog, Add_Message'Access);
      New_Line (File);
   end Generate_Message_List;

   -------------------
   -- Generate_Pool --
   -------------------

   procedure Generate_Pool (File : in out File_Type; Pool : Wide_String) is

   begin
      Write_String (File, Pool, "20017");
      Print_Line (File, "zbmcode", "20021",
                        Locale => en_US);
      New_Line (File);
   end Generate_Pool;

   -------------------
   -- Generate_Spec --
   -------------------

   procedure Generate_Spec (Catalog         : Catalog_Type;
                            Package_Name    : Wide_String;
                            Spec_Name       : Wide_String;
                            Body_Initialize : Boolean;
                            Export_Name     : Wide_String;
                            Verbose         : Boolean) is
      File : File_Type;

   begin
      Wide_Create (File, Spec_Name);
      Print_Line (File, "zbmcode", "10000",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "10001",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "10002",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "10003",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "10004",
                        Locale => en_US);
      New_Line (File);
      Print_Line (File, "zbmcode", "10005",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "10006",
                        Locale => en_US);
      New_Line (File);
      Print_Line (File, "zbmcode", "10007", +Package_Name,
                        Locale => en_US);
      New_Line (File);
      if Body_Initialize then
         Print_Line (File, "zbmcode", "10008", +Package_Name,
                           Locale => en_US);
         Print_Line (File, "zbmcode", "10009", +Package_Name,
                           Locale => en_US);
         Print_Line (File, "zbmcode", "10010", +Package_Name,
                           Locale => en_US);
         Print_Line (File, "zbmcode", "10011", +Package_Name,
                           Locale => en_US);
         New_Line (File);
      end if;
      Print_Line (File, "zbmcode", "10012",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "10013",
                        Locale => en_US);
      Print_Line (File, "zbmcode", "10014",
                        Locale => en_US);
      New_Line (File);
      Write_Name_List (File, Catalog,
                       "10015", "10016", "10018", "10017", "10019",
                       Number_Of_Facilities (Catalog), Get_Facility'Access);
      Print_Line (File, "zbmcode", "10020",
                        Locale => en_US);
      if Export_Name'Length > 0 then
         Print_Line (File, "zbmcode", "10021", +Export_Name,
                           Locale => en_US);
      end if;
      New_Line (File);
      Print_Line (File, "zbmcode", "10022", +Package_Name,
                        Locale => en_US);
      Close (File);
      if Verbose then
         Print_Line ("zbmcompile", "00021", +Package_Name, +Spec_Name);
      end if;
   end Generate_Spec;

   --------------
   -- Optimize --
   --------------

   function Optimize (Catalog : Catalog_Type;
                      Verbose : Boolean) return Catalog_Type is

      Result : constant Catalog_Type := Create;
      Current_Messages  : Natural := 0;
      Current_Pool_Size : Natural := 0;
      Messages_Delta    : Natural;
      Pool_Delta        : Natural;

   begin
      if Verbose then
         Print_Line ("zbmcompile", "00023");
      end if;
      Use_Single_Pool (Result);
      Reserve (Result, Pool_Size => Pool_Size (Catalog),
                       Messages  => Number_Of_Messages (Catalog));
      for L in 1 .. Number_Of_Locales (Catalog) loop
         Optimize_Locale (Catalog, Result, L);
         if Verbose then
            Messages_Delta := Number_Of_Messages (Result) - Current_Messages;
            Pool_Delta := Pool_Size (Result) - Current_Pool_Size;
            Print_Line ("zbmcompile", "00024",
                           +Locale_Name (Get_Locale (Catalog, L)),
                           +Messages_Delta,
                           +Pool_Delta);
            Current_Messages := Number_Of_Messages (Result);
            Current_Pool_Size := Pool_Size (Result);
         end if;
      end loop;
      return Result;
   end Optimize;

   ---------------------
   -- Optimize_Locale --
   ---------------------

   procedure Optimize_Locale (Catalog      : Catalog_Type;
                              Result       : Catalog_Type;
                              Locale_Index : Positive) is

      procedure Add_Message (F        : Positive;
                             K        : Positive;
                             L        : Positive;
                             Message  : Wide_String);
      --  Add a message to the optimized catalog.

      Locale : constant Locale_Type := Get_Locale (Catalog, Locale_Index);

      -----------------
      -- Add_Message --
      -----------------

      procedure Add_Message (F        : Positive;
                             K        : Positive;
                             L        : Positive;
                             Message  : Wide_String) is
      begin
         if L = Locale_Index then
            Add (Result, Get_Facility (Catalog, F), Get_Key (Catalog, K),
                 Message, Locale);
         end if;
      end Add_Message;

   begin
      Iterate (Catalog, Add_Message'Access);
   end Optimize_Locale;

   ---------------------
   -- Write_Name_List --
   ---------------------

   procedure Write_Name_List (File     : in out File_Type;
                              Catalog  : Catalog_Type;
                              Name_Def : Wide_String;
                              List_Def : Wide_String;
                              Unitary  : Wide_String;
                              List_N   : Wide_String;
                              Last     : Wide_String;
                              N        : Natural;
                              Namer    : access
                                             function (Catalog : Catalog_Type;
                                                       I       : Positive)
                                                return Wide_String) is
   begin
      for I in 1 .. N loop
         Write_String (File, Namer (Catalog, I), Name_Def, I);
      end loop;
      Print_Line (File, "zbmcode", List_Def, +N,
                        Locale => en_US);
      if N = 1 then
         Print_Line (File, "zbmcode", Unitary,
                           Locale => en_US);
         return;
      else
         for I in 1 ..  N loop
            if I < N then
               Print_Line (File, "zbmcode", List_N, +I,
                                 Locale => en_US);
            else
               Print_Line (File, "zbmcode", Last, +I,
                                 Locale => en_US);
            end if;
         end loop;
      end if;
      New_Line (File);
   end Write_Name_List;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (File       : in out File_Type;
                           Value      : Wide_String;
                           Decl_Key   : Wide_String;
                           Decl_Index : Positive := 1) is

      use Ada.Wide_Characters.Unicode;

      Semicolon        : constant Wide_Character := ';';
      Empty_String     : constant Wide_String := "";
      Buffer           : Wide_String (1 .. Output_Buffer_Size);
      Current_Position : Natural := Value'First;

      function Current_Character return Wide_Character;
      function Current_Character_Pos return Natural;
      procedure Advance;
      function Buffered_Data return Wide_String;
      function Finished return Boolean;

      procedure Advance is
      begin
         Current_Position := Current_Position + 1;
      end Advance;

      function Buffered_Data return Wide_String is
         I : Positive := Buffer'First;
      begin
         while I < Buffer'Last and not Finished loop
            exit when Is_Non_Graphic (Current_Character);
            Buffer (I) := Current_Character;
            Advance;
            if Buffer (I) = '"' then
               I := I + 1;
               Buffer (I) := '"';
            end if;
            I := I + 1;
         end loop;
         return Buffer (Buffer'First .. I - 1);
      end Buffered_Data;

      function Current_Character return Wide_Character is
      begin
         if Finished then
            return Wide_Character'Val (0);
         end if;
         return Value (Current_Position);
      end Current_Character;

      function Current_Character_Pos return Natural is
      begin
         return Wide_Character'Pos (Current_Character);
      end Current_Character_Pos;

      function Finished return Boolean is
      begin
         return Current_Position > Value'Length;
      end Finished;

   begin
      if Value'Length < 30 then
         --  Small string, just write a single liner
         Print_Line (File, "zbmcode", Decl_Key, +Decl_Index,
                                      +Value, +Semicolon,
                           Locale => en_US);
         return;
      else
         Print_Line (File, "zbmcode", Decl_Key, +Decl_Index,
                                      +Empty_String, +Empty_String,
                           Locale => en_US);
      end if;
      while not Finished loop
         if Is_Non_Graphic (Current_Character) then
            Print_Line (File, "zbmcode", "20018", +Current_Character_Pos,
                              Locale => en_US);
            Advance;
         else
            Print_Line (File, "zbmcode", "20019", +Buffered_Data,
                              Locale => en_US);
         end if;
      end loop;
      Print_Line (File, "zbmcode", "20020",
                        Locale => en_US);
   end Write_String;

end ZBMCompile.Code_Gen;
