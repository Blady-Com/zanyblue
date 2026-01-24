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
procedure T_0075 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Test_Name : constant Wide_String := "t_0075";
   Locale    : constant Locale_Type := Make_Locale ("");
   Output    : File_Type;

begin
   Set_Output (Output, Test_Area, Test_Name);
   New_Line;
   Print ("A message with no arguments", Locale => Locale);
   Print ("A message with no arguments", Locale => Locale);
   New_Line;
   Restore_Output (Output);
   Check_Log_File (R, Test_Area, Test_Name,
           "Print with no arguments failed");
end T_0075;
