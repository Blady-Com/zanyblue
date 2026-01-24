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
procedure T_0027 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Catalogs;

   Test_Name : constant Wide_String := "t_0027";
   Locale    : constant Locale_Type := Make_Locale ("");
   Facility  : constant Wide_String := "fac1";
   Key       : constant Wide_String := "key1";
   Message   : constant Wide_String := "Message: #1={0}, #2={1}, "
                                     & "#3={2}, #4={3}, #5={4}";
   Catalog   : constant Catalog_Type := Create;
   Output    : File_Type;

begin
   Add (Catalog, Facility, Key, Message, Locale);
   Set_Output (Output, Test_Area, Test_Name);
   Print_Line (Facility, Key, +10, +20, +30, +40, +50,
               Locale => Locale, Catalog => Catalog);
   New_Line;
   Print_Line (Facility, Key, +10, +20, +30, +40, +50,
               Locale => Locale, Catalog => Catalog);
   Restore_Output (Output);
   Check_Log_File (R, Test_Area, Test_Name,
           "Print_Line with 5 argument failed");
end T_0027;
