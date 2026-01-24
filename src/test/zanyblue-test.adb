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

with Ada.Directories;
with Ada.Command_Line;
with AUnit.Reporter.Text;
with GNAT.Regexp;
with ZanyBlue.Text;
with ZanyBlue.Test.Reporter_XML;

package body ZanyBlue.Test is

   use AUnit;
   use ZanyBlue.Text;

   procedure Load_Command_Line;

   type String_Access is access String;
   CL_Initialized : Boolean := False;
   CL_Use_XML : Boolean := False;
   CL_Top_Directory : String_Access := null;
   Text_Reporter : AUnit.Reporter.Text.Text_Reporter;
   XML_Reporter : ZanyBlue.Test.Reporter_XML.XML_Reporter;

   function Base_Log_Name (Test_Area : Wide_String;
                           Test_Name : Wide_String) return Wide_String;
   function Matched (Left : Wide_String; Right : Wide_String) return Boolean;

   procedure Add_Routine (Test    : in out Test_Case'Class;
                          Routine : Test_Routine;
                          Name    : String) is
   begin
      Test.Add_Routine (Routine_Spec'(Routine, Format (Name)));
   end Add_Routine;

   function Base_Log_Name (Test_Area : Wide_String;
                           Test_Name : Wide_String) return Wide_String is
   begin
      return Test_Src_Directory (Test_Area) & "/" & Test_Name;
   end Base_Log_Name;

   procedure Check_Log_File (Test    : in out AUnit.Test_Cases.Test_Case'Class;
                             Test_Area : Wide_String;
                             Test_Name : Wide_String;
                             Message   : Wide_String) is
   begin
      WAssert (Test, Compare_Log_File (Test_Area, Test_Name), Message);
   end Check_Log_File;

   procedure Check_Value (Test      : in out AUnit.Test_Cases.Test_Case'Class;
                          Generated : Wide_String;
                          Expected  : Wide_String;
                          Message   : Wide_String := "Failure") is
   begin
      WAssert (Test, Generated = Expected,
               Message & ": """ & Generated & """ /= """ & Expected & """");
   end Check_Value;

   function Compare_Files (Name_A, Name_B : Wide_String) return Boolean is

      use Ada.Wide_Text_IO;

      File_A : File_Type;
      File_B : File_Type;
      Result : Boolean := True;

   begin
      Wide_Open (File_A, In_File, Name_A);
      Wide_Open (File_B, In_File, Name_B);
      while not (End_Of_File (File_A) or End_Of_File (File_B)) loop
         if not Matched (Get_Line (File_A), Get_Line (File_B)) then
            Close (File_A);
            Close (File_B);
            return False;
         end if;
      end loop;
      Result := not (End_Of_File (File_A) xor End_Of_File (File_B));
      Close (File_A);
      Close (File_B);
      return Result;
   end Compare_Files;

   function Compare_Log_File (Test_Area : Wide_String;
                              Test_Name : Wide_String) return Boolean is

      use Ada.Directories;

      RefLog : constant Wide_String := Test_RefLog_Name (Test_Area, Test_Name);
      GenLog : constant Wide_String := Test_Log_Name (Test_Area, Test_Name);
      Result : Boolean;

   begin
      Result := Compare_Files (RefLog, GenLog);
      if Result then
         --  Log file matched, delete the generated file
         Delete_File (To_UTF8 (GenLog));
      end if;
      return Result;
   end Compare_Log_File;

   procedure Create_Log_File (File      : in out Ada.Wide_Text_IO.File_Type;
                              Test_Area : Wide_String;
                              Test_Name : Wide_String) is
      use Ada.Wide_Text_IO;
      Base_Name : constant Wide_String := Base_Log_Name (Test_Area, Test_Name);
   begin
      Wide_Create (File, Base_Name & ".out");
   end Create_Log_File;

   procedure Discard (Value : String) is
      pragma Unreferenced (Value);
   begin
      null;
   end Discard;

   procedure Discard (Value : Wide_String) is
      pragma Warnings (Off, Value);
   begin
      null;
   end Discard;

   procedure Discard (Value : Integer) is
      pragma Warnings (Off, Value);
   begin
      null;
   end Discard;

   procedure Load_Command_Line is
      use Ada.Command_Line;
      Index : Positive := 1;
   begin
      if not CL_Initialized then
         while Index <= Argument_Count loop
            if Argument (Index) = "-X" then
               CL_Use_XML := True;
            elsif Argument (Index) = "-T" then
               Index := Index + 1;
               if Index > Argument_Count then
                  raise Usage_Error;
               end if;
               CL_Top_Directory := new String'(Argument (Index));
            else
               raise Usage_Error;
            end if;
            Index := Index + 1;
         end loop;
         CL_Initialized := True;
      end if;
   end Load_Command_Line;

   function Matched (Left : Wide_String; Right : Wide_String) return Boolean is
      use GNAT.Regexp;
      Left_S : constant String := To_UTF8 (Left);
      Right_S : constant String := To_UTF8 (Right);
   begin
      return Left = Right or else Match (Right_S, Compile (Left_S));
   end Matched;

   function Reporter_Implementation return AUnit.Reporter.Reporter'Class is
   begin
      if Use_XML then
         return XML_Reporter;
      else
         return Text_Reporter;
      end if;
   end Reporter_Implementation;

   procedure Restore_Output (Output : in out Ada.Wide_Text_IO.File_Type) is
   begin
      Ada.Wide_Text_IO.Close (Output);
      Restore_Output;
   end Restore_Output;

   procedure Restore_Output is
   begin
      Set_Output (Ada.Wide_Text_IO.Standard_Output);
   end Restore_Output;

   procedure Set_Output (Output    : in out Ada.Wide_Text_IO.File_Type;
                         Test_Area : Wide_String;
                         Test_Name : Wide_String) is
   begin
      Create_Log_File (Output, Test_Area, Test_Name);
      Set_Output (Output);
   end Set_Output;

   function Test_In_Name (Test_Area : Wide_String;
                          Test_Name : Wide_String) return Wide_String is
   begin
      return Base_Log_Name (Test_Area, Test_Name) & ".in";
   end Test_In_Name;

   function Test_Log_Name (Test_Area : Wide_String;
                           Test_Name : Wide_String) return Wide_String is
   begin
      return Base_Log_Name (Test_Area, Test_Name) & ".out";
   end Test_Log_Name;

   function Test_RefLog_Name (Test_Area : Wide_String;
                              Test_Name : Wide_String) return Wide_String is
   begin
      return Base_Log_Name (Test_Area, Test_Name) & ".log";
   end Test_RefLog_Name;

   function Test_Src_Directory (Test_Area : Wide_String) return Wide_String is
   begin
      return ZanyBlue.Test.Top_Directory & "/src/test/"
                                         & Test_Area;
   end Test_Src_Directory;

   function Top_Directory return Wide_String is
   begin
      Load_Command_Line;
      if CL_Top_Directory = null then
         raise Usage_Error;
      end if;
      return To_Wide_String (CL_Top_Directory.all);
   end Top_Directory;

   function Use_XML return Boolean is
   begin
      Load_Command_Line;
      return CL_Use_XML;
   end Use_XML;

   procedure WAssert (Test      : in out AUnit.Test_Cases.Test_Case'Class;
                      Condition : Boolean;
                      Message : Wide_String) is
   begin
      Test.Assert (Condition, To_UTF8 (Message));
   end WAssert;

end ZanyBlue.Test;
