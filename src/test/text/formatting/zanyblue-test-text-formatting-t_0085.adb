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
procedure T_0085 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Test_Name : constant Wide_String := "t_0085";
   Locale    : constant Locale_Type := Make_Locale ("");
   Output    : File_Type;

begin
   Create_Log_File (Output, Test_Area, Test_Name);
   New_Line (Output);
   Print (Output, "Message: #1={0}, #2={1}, #3={2}", +10, +20, +30,
          Locale => Locale);
   Print (Output, "Message: #1={0}, #2={1}, #3={2}", +10, +20, +30,
          Locale => Locale);
   New_Line (Output);
   Close (Output);
   Check_Log_File (R, Test_Area, Test_Name,
           "Print with 3 argument failed");
end T_0085;
