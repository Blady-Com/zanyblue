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

with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Text.Formatting)
procedure T_0051 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Catalogs;

   Test_Name : constant Wide_String := "t_0051";
   Locale    : constant Locale_Type := Make_Locale ("");
   Facility  : constant Wide_String := "fac1";
   Key       : constant Wide_String := "key1";
   Message   : constant Wide_String := "Message: #1={0}, #2={1}";
   Catalog   : constant Catalog_Type := Create;

   procedure With_Exceptions (Name : Wide_String);
   procedure Without_Exception (Name : Wide_String);

   procedure With_Exceptions (Name : Wide_String) is
      Output    : File_Type;
   begin
      Enable_Exceptions (Catalog);
      Create_Log_File (Output, Test_Area, Name);
      Print_Line (Output, Facility, Key, +10,
                  Locale => Locale, Catalog => Catalog);
      Close (Output);
      R.Assert (False, "Exception not raised");
   exception
   when No_Such_Argument_Error =>
      Close (Output);
      R.Assert (True, "Expected exception raised");
   end With_Exceptions;

   procedure Without_Exception (Name : Wide_String) is
      Output    : File_Type;
   begin
      Disable_Exceptions (Catalog);
      Create_Log_File (Output, Test_Area, Name);
      Print_Line (Output, Facility, Key, +10,
                  Locale => Locale, Catalog => Catalog);
      Close (Output);
   end Without_Exception;

begin
   Add (Catalog, Facility, Key, Message, Locale);
   With_Exceptions (Test_Name & "a");
   Check_Log_File (R, Test_Area, Test_Name & "a",
           "Should be empty from exception");
   Without_Exception (Test_Name & "b");
   Check_Log_File (R, Test_Area, Test_Name & "b",
           "Exception output failure");
end T_0051;
