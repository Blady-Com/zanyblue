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

with Ada.Wide_Text_IO;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;

with ZBTest.Commands.Append_Command;
with ZBTest.Commands.Begin_Command;
with ZBTest.Commands.Compare_Command;
with ZBTest.Commands.Copy_Command;
with ZBTest.Commands.Delenv_Command;
with ZBTest.Commands.Delete_Command;
with ZBTest.Commands.Desc_Command;
with ZBTest.Commands.Dump_Command;
with ZBTest.Commands.Echo_Command;
with ZBTest.Commands.End_Command;
with ZBTest.Commands.Execute_Command;
with ZBTest.Commands.Exit_Command;
with ZBTest.Commands.Filestat_Command;
with ZBTest.Commands.Getenv_Command;
with ZBTest.Commands.Help_Command;
with ZBTest.Commands.Incr_Command;
with ZBTest.Commands.Mkdir_Command;
with ZBTest.Commands.Noop_Command;
with ZBTest.Commands.Prepend_Command;
with ZBTest.Commands.Print_Command;
with ZBTest.Commands.Rename_Command;
with ZBTest.Commands.Run_Command;
with ZBTest.Commands.Set_Command;
with ZBTest.Commands.Setenv_Command;
with ZBTest.Commands.Unknown_Command;
with ZBTest.Commands.Which_Command;
with ZanyBlue.OS;
pragma Elaborate_All (ZanyBlue.OS);

with ZBTest_Messages.ZBTest_Wide_Prints;

package body ZBTest.Commands is

   use Ada.Wide_Text_IO;
   use ZanyBlue.OS;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;
   use ZBTest_Messages.ZBTest_Wide_Prints;

   type Simple_Message_Printer is
      access procedure (Destination : in File_Type    := Current_Output;
                        With_NL     : in Boolean      := True;
                        Locale      : in Locale_Type  := Current_Locale;
                        Catalog     : in Catalog_Type := Standard_Catalog);

   type Argument_Message_Printer is
      access procedure (Argument0   : in Any_Category_Type'Class;
                        Destination : in File_Type    := Current_Output;
                        With_NL     : in Boolean      := True;
                        Locale      : in Locale_Type  := Current_Locale;
                        Catalog     : in Catalog_Type := Standard_Catalog);

   type Command_Definition is
      record
         Name           : Wide_String_Access;
         Implementation : Command_Type;
         Usage          : Simple_Message_Printer;
         Summary        : Argument_Message_Printer;
         Help           : Simple_Message_Printer;
      end record;
   --  Information on a command.

   type Command_List is array (Natural range <>) of Command_Definition;
   --  List of known commands

   Command_Table : constant Command_List := (
                 (Name           => new Wide_String'("?"),
                  Implementation => Unknown_Command.Implementation'Access,
                  Usage          => Print_20000'Access,
                  Summary        => Print_30000'Access,
                  Help           => Print_40000'Access),

                 (Name           => new Wide_String'("append"),
                  Implementation => Append_Command.Implementation'Access,
                  Usage          => Print_20001'Access,
                  Summary        => Print_30001'Access,
                  Help           => Print_40001'Access),

                 (Name           => new Wide_String'("begin"),
                  Implementation => Begin_Command.Implementation'Access,
                  Usage          => Print_20002'Access,
                  Summary        => Print_30002'Access,
                  Help           => Print_40002'Access),

                 (Name           => new Wide_String'("compare"),
                  Implementation => Compare_Command.Implementation'Access,
                  Usage          => Print_20019'Access,
                  Summary        => Print_30019'Access,
                  Help           => Print_40019'Access),

                 (Name           => new Wide_String'("copy"),
                  Implementation => Copy_Command.Implementation'Access,
                  Usage          => Print_20013'Access,
                  Summary        => Print_30013'Access,
                  Help           => Print_40013'Access),

                 (Name           => new Wide_String'("delenv"),
                  Implementation => Delenv_Command.Implementation'Access,
                  Usage          => Print_20026'Access,
                  Summary        => Print_30026'Access,
                  Help           => Print_40026'Access),

                 (Name           => new Wide_String'("delete"),
                  Implementation => Delete_Command.Implementation'Access,
                  Usage          => Print_20020'Access,
                  Summary        => Print_30020'Access,
                  Help           => Print_40020'Access),

                 (Name           => new Wide_String'("desc"),
                  Implementation => Desc_Command.Implementation'Access,
                  Usage          => Print_20017'Access,
                  Summary        => Print_30017'Access,
                  Help           => Print_40017'Access),

                 (Name           => new Wide_String'("dump"),
                  Implementation => Dump_Command.Implementation'Access,
                  Usage          => Print_20003'Access,
                  Summary        => Print_30003'Access,
                  Help           => Print_40003'Access),

                 (Name           => new Wide_String'("echo"),
                  Implementation => Echo_Command.Implementation'Access,
                  Usage          => Print_20023'Access,
                  Summary        => Print_30023'Access,
                  Help           => Print_40023'Access),

                 (Name           => new Wide_String'("end"),
                  Implementation => End_Command.Implementation'Access,
                  Usage          => Print_20004'Access,
                  Summary        => Print_30004'Access,
                  Help           => Print_40004'Access),

                 (Name           => new Wide_String'("execute"),
                  Implementation => Execute_Command.Implementation'Access,
                  Usage          => Print_20014'Access,
                  Summary        => Print_30014'Access,
                  Help           => Print_40014'Access),

                 (Name           => new Wide_String'("exit"),
                  Implementation => Exit_Command.Implementation'Access,
                  Usage          => Print_20005'Access,
                  Summary        => Print_30005'Access,
                  Help           => Print_40005'Access),

                 (Name           => new Wide_String'("filestat"),
                  Implementation => Filestat_Command.Implementation'Access,
                  Usage          => Print_20022'Access,
                  Summary        => Print_30022'Access,
                  Help           => Print_40022'Access),

                 (Name           => new Wide_String'("getenv"),
                  Implementation => Getenv_Command.Implementation'Access,
                  Usage          => Print_20006'Access,
                  Summary        => Print_30006'Access,
                  Help           => Print_40006'Access),

                 (Name           => new Wide_String'("help"),
                  Implementation => Help_Command.Implementation'Access,
                  Usage          => Print_20007'Access,
                  Summary        => Print_30007'Access,
                  Help           => Print_40007'Access),

                 (Name           => new Wide_String'("incr"),
                  Implementation => Incr_Command.Implementation'Access,
                  Usage          => Print_20024'Access,
                  Summary        => Print_30024'Access,
                  Help           => Print_40024'Access),

                 (Name           => new Wide_String'("mkdir"),
                  Implementation => Mkdir_Command.Implementation'Access,
                  Usage          => Print_20021'Access,
                  Summary        => Print_30021'Access,
                  Help           => Print_40021'Access),

                 (Name           => new Wide_String'("noop"),
                  Implementation => Noop_Command.Implementation'Access,
                  Usage          => Print_20008'Access,
                  Summary        => Print_30008'Access,
                  Help           => Print_40008'Access),

                 (Name           => new Wide_String'("prepend"),
                  Implementation => Prepend_Command.Implementation'Access,
                  Usage          => Print_20009'Access,
                  Summary        => Print_30009'Access,
                  Help           => Print_40009'Access),

                 (Name           => new Wide_String'("print"),
                  Implementation => Print_Command.Implementation'Access,
                  Usage          => Print_20010'Access,
                  Summary        => Print_30010'Access,
                  Help           => Print_40010'Access),

                 (Name           => new Wide_String'("rename"),
                  Implementation => Rename_Command.Implementation'Access,
                  Usage          => Print_20015'Access,
                  Summary        => Print_30015'Access,
                  Help           => Print_40015'Access),

                 (Name           => new Wide_String'("run"),
                  Implementation => Run_Command.Implementation'Access,
                  Usage          => Print_20016'Access,
                  Summary        => Print_30016'Access,
                  Help           => Print_40016'Access),

                 (Name           => new Wide_String'("set"),
                  Implementation => Set_Command.Implementation'Access,
                  Usage          => Print_20011'Access,
                  Summary        => Print_30011'Access,
                  Help           => Print_40011'Access),

                 (Name           => new Wide_String'("setenv"),
                  Implementation => Setenv_Command.Implementation'Access,
                  Usage          => Print_20025'Access,
                  Summary        => Print_30025'Access,
                  Help           => Print_40025'Access),

                 (Name           => new Wide_String'("which"),
                  Implementation => Which_Command.Implementation'Access,
                  Usage          => Print_20012'Access,
                  Summary        => Print_30012'Access,
                  Help           => Print_40012'Access));

   function Find_Index (Name : in Wide_String) return Natural;
   --  Return the index in the command table for the named command.  If the
   --  command is not found, 0 is returned.

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name (Index : Natural) return Wide_String is
   begin
      return Command_Table (Index).Name.all;
   end Command_Name;

   ----------
   -- Find --
   ----------

   function Find (Name : in Wide_String) return Command_Type is
   begin
      return Command_Table (Find_Index (Name)).Implementation;
   end Find;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index (Name : in Wide_String) return Natural is
   begin
      for I in 1 .. Command_Table'Last loop
         if Name = Command_Table (I).Name.all then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Index;

   ------------------------
   -- Number_Of_Commands --
   ------------------------

   function Number_Of_Commands return Positive is
   begin
      return Command_Table'Last;
   end Number_Of_Commands;

   ------------------------
   -- Print_Command_Help --
   ------------------------

   procedure Print_Command_Help (Name : in Wide_String) is
   begin
      Command_Table (Find_Index (Name)).Help.all;
   end Print_Command_Help;

   ---------------------------
   -- Print_Command_Summary --
   ---------------------------

   procedure Print_Command_Summary (Name  : in Wide_String;
                                    Index : in Positive) is
   begin
      Command_Table (Find_Index (Name)).Summary.all (+Index);
   end Print_Command_Summary;

   -------------------------
   -- Print_Command_Usage --
   -------------------------

   procedure Print_Command_Usage (Name : Wide_String) is
   begin
      Command_Table (Find_Index (Name)).Usage.all;
   end Print_Command_Usage;

begin   -- ZBTest.Commands
   for I in Command_Table'First .. Command_Table'Last - 1 loop
      if Command_Table (I).Name.all > Command_Table (I + 1).Name.all then
         raise Command_Table_Not_Sorted
            with To_UTF8 (Command_Table (I + 1).Name.all);
      end if;
   end loop;
end ZBTest.Commands;
