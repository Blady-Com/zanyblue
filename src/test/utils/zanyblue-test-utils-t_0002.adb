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

with Ada.Calendar;
with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Utils)
procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Catalogs;

   Test_Name  : constant Wide_String := "t_0002";
   Locale     : constant Locale_Type := Make_Locale ("");
   Facility   : constant Wide_String := "fac1";
   Catalog    : constant Catalog_Type := Create;
   Start_Time : Ada.Calendar.Time;
   Output     : File_Type;

   pragma Warnings (Off, Start_Time);

begin
   Add (Catalog, Facility, "00001",
        "App, V{0}.{1}.{2} {3} (r{4}) at {5}",
        Locale);
   Add (Catalog, Facility, "00002",
        "Copyright notice: {0}",
        Locale);
   Add (Catalog, Facility, "00003",
        "Goodbye {0}, {1}",
        Locale);
   Set_Output (Output, Test_Area, Test_Name);
   Banner (Facility, Start_Time, Catalog => Catalog);
   Restore_Output (Output);
   Check_Log_File (R, Test_Area, Test_Name,
                   "Banner should have been printed");
end T_0002;
