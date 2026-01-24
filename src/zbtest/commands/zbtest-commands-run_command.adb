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

with Ada.Calendar;
with ZanyBlue.Wide_Directories;
with ZBTest.XML_Data;

separate (ZBTest.Commands)
procedure Run_Command (State : in out State_Type;
                       Args  : in List_Type) is

   use Ada.Calendar;
   use ZanyBlue.Wide_Directories;
   use ZBTest.XML_Data;

   procedure Execute_Script (State  : in out State_Type;
                             Script : in Wide_String);
   --  Execute the script: open the script file and execute each line.

   function Locate_Script (Path : in Wide_String;
                           Name : in Wide_String) return Wide_String;
   --  Locate the script to execute.

   procedure Register_Run_Failure (State          : in out State_Type;
                                   Script_Name    : in Wide_String);
   --  Register the failure to run a test script as a test failure.

   procedure Run (State : in out State_Type;
                  Name  : in Wide_String);
   --  Run the script.

   procedure Wrap_Up (State     : in out State_Type;
                      Test_Name : in Wide_String);
   --  Wrap-up the test, close any open scopes and print summary

   --------------------
   -- Execute_Script --
   --------------------

   procedure Execute_Script (State  : in out State_Type;
                             Script : in Wide_String) is
      File       : File_Type;
   begin
      Wide_Open (File, In_File, Script);
      State.Read_Eval_Loop (File, False);
      Close (File);
   exception
   when E : others =>
      if Is_Open (File) then
         Close (File);
      end if;
      Print_10040 (+E, +Script);
   end Execute_Script;

   -------------------
   -- Locate_Script --
   -------------------

   function Locate_Script (Path : in Wide_String;
                           Name : in Wide_String) return Wide_String is

      Test_1 : constant Wide_String := Wide_Compose ("", Name, "zbt");
      Test_2 : constant Wide_String := Wide_Compose (Name, Name, "zbt");
      Test_3 : constant Wide_String := Wide_Compose (Path, Name, "zbt");
      Test_4 : constant Wide_String := Wide_Compose (Wide_Compose (Path, Name),
                                                     Name, "zbt");
   begin
      if Wide_Is_File (Name) then
         return Name;
      elsif Wide_Is_File (Test_1) then
         return Test_1;
      elsif Wide_Is_File (Test_2) then
         return Test_2;
      elsif Wide_Is_File (Test_3) then
         return Test_3;
      elsif Wide_Is_File (Test_4) then
         return Test_4;
      else
         raise File_Not_Found;
      end if;
   end Locate_Script;

   --------------------------
   -- Register_Run_Failure --
   --------------------------

   procedure Register_Run_Failure (State          : in out State_Type;
                                   Script_Name    : in Wide_String) is
      Test_Name : constant Wide_String := Format ("{0}.{1}-run",
                                                  +State.Full_Test_Name,
                                                  +Script_Name);
      File      : File_Type;
   begin
      Wide_Create (File, Test_Name);
      Print_10037 (+Script_Name, File);
      Close (File);
      State.Register_Failure (Test_Name);
   end Register_Run_Failure;

   ----------
   -- Run --
   ----------

   procedure Run (State : in out State_Type;
                  Name  : in Wide_String) is
      Cur_Path   : constant Wide_String := State.Get_String ("_curpath");
      Script     : constant Wide_String := Locate_Script (Cur_Path, Name);
      Script_Dir : constant Wide_String := Wide_Containing_Directory (Script);
   begin
      Print_00013 (+Script);
      State.New_Scope;
      State.Initialize_Scope (Script_Dir, Script, Implicit_Scope => True);
      State.Set ("_xmlnode", Create_XML_Node (State.Get ("_doc"),
                                              State.Get ("_xmlnode"),
                                              "testsuite"));
      Set_Attribute (State.Get ("_xmlnode"), "name", State.Full_Test_Name);
      Execute_Script (State, Script);
      Wrap_Up (State, Full_Test_Name (State));
   end Run;

   -------------
   -- Wrap_Up --
   -------------

   procedure Wrap_Up (State     : in out State_Type;
                      Test_Name : in Wide_String) is
      XML_Node : XML_Node_Type;
      Start    : Time;
      Elapsed  : Float;
      N_Fail   : Natural;
      N_OK     : Natural;
   begin
      XML_Node := XML_Node_Type (State.Get ("_xmlnode"));
      Start := State.Get_Time ("_start_time");
      --  Close any user created scopes ...
      while not State.Is_Defined ("_implicit_scope", False) loop
         State.End_Scope;
      end loop;
      State.End_Scope (N_OK, N_Fail);
      Print_00018 (+Test_Name, +N_OK, +N_Fail);
      --  Set the time it took to execute in the XML doc
      Elapsed := Float (Clock - Start);
      Set_Attribute (XML_Node, "time", Format ("{0:f}", +Elapsed));
   end Wrap_Up;

begin
   if Length (Args) = 2 then
      Run (State, Value (Args, 2));
   else
      raise Command_Usage_Error;
   end if;
exception
when File_Not_Found =>
   Register_Run_Failure (State, Value (Args, 2));
   Print_10020 (+Value (Args, 2));
end Run_Command;
