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

pragma License (GPL);

with Ada.Wide_Text_IO;
with AUnit.Test_Cases;
with AUnit.Reporter;

package ZanyBlue.Test is

   use AUnit.Test_Cases;

   procedure Add_Routine (Test    : in out Test_Case'Class;
                          Routine : Test_Routine;
                          Name    : String);

   --  Following are useful during testing when we are not interested
   --  in a string value but need to generate it, e.g., when we expect
   --  an exception should be raise.
   procedure Discard (Value : String);
   procedure Discard (Value : Wide_String);
   procedure Discard (Value : Integer);

   procedure Check_Value (Test      : in out AUnit.Test_Cases.Test_Case'Class;
                          Generated : Wide_String;
                          Expected  : Wide_String;
                          Message   : Wide_String := "Failure");

   procedure WAssert (Test      : in out AUnit.Test_Cases.Test_Case'Class;
                      Condition : Boolean;
                      Message   : Wide_String);

   procedure Check_Log_File (Test    : in out AUnit.Test_Cases.Test_Case'Class;
                             Test_Area : Wide_String;
                             Test_Name : Wide_String;
                             Message   : Wide_String);
   function Compare_Files (Name_A, Name_B : Wide_String) return Boolean;
   function Compare_Log_File (Test_Area : Wide_String;
                              Test_Name : Wide_String) return Boolean;
   procedure Create_Log_File (File      : in out Ada.Wide_Text_IO.File_Type;
                              Test_Area : Wide_String;
                              Test_Name : Wide_String);
   function Test_Src_Directory (Test_Area : Wide_String) return Wide_String;
   function Test_RefLog_Name (Test_Area : Wide_String;
                              Test_Name : Wide_String) return Wide_String;
   function Test_In_Name (Test_Area : Wide_String;
                          Test_Name : Wide_String) return Wide_String;
   function Test_Log_Name (Test_Area : Wide_String;
                           Test_Name : Wide_String) return Wide_String;
   procedure Set_Output (File : Ada.Wide_Text_IO.File_Type)
      renames Ada.Wide_Text_IO.Set_Output;
   procedure Set_Output (Output    : in out Ada.Wide_Text_IO.File_Type;
                         Test_Area : Wide_String;
                         Test_Name : Wide_String);
   procedure Restore_Output (Output : in out Ada.Wide_Text_IO.File_Type);
   procedure Restore_Output;

   Usage_Error : exception;

   function Use_XML return Boolean;
   function Top_Directory return Wide_String;
   function Reporter_Implementation return AUnit.Reporter.Reporter'Class;

end ZanyBlue.Test;
