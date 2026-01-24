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

separate (ZanyBlue.Test.Text.Formatting)
procedure T_0042 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Test_Name : constant Wide_String := "t_0042";
   Locale    : constant Locale_Type := Make_Locale ("");
   Arguments : Argument_List;
   Output    : File_Type;

begin
   Append (Arguments, +10);
   Append (Arguments, +20);
   Create_Log_File (Output, Test_Area, Test_Name);
   Print_Line (Output, "From {0} to {1} is a long way", Arguments, Locale);
   New_Line (Output);
   Print_Line (Output, "From {0} to {1} is a long way", Arguments, Locale);
   Close (Output);
   Check_Log_File (R, Test_Area, Test_Name,
           "Print_Line with arguments failed");
end T_0042;
