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
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Utils;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;
with ZBMCompile.Messages;
with ZBMCompile.Code_Gen;
with ZBMCompile.Command_Line;
with ZBMCompile.Parser_Handler;

procedure Main is

   use Ada.Command_Line;
   use Ada.Exceptions;
   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Utils;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;
   use ZBMCompile;
   use ZBMCompile.Code_Gen;
   use ZBMCompile.Command_Line;
   use ZBMCompile.Parser_Handler;

   Catalog      : Catalog_Type := Create;
   Start_Time   : Ada.Calendar.Time;
   Next_File    : Facility_Source_Type;
   Files_Loaded : Boolean := False;
   N_Locales    : Natural := 0;
   N_Messages   : Natural := 0;
   Handler      : ZBMC_Handler_Type;

begin
   Process_Command_Line;
   Handler.Set_Catalog (Catalog);
   if not Quiet then
      Banner ("zbmcompile", Start_Time);
   end if;
   Use_Single_Pool (Catalog);
   for I in 1 ..  N_Facilities loop
      Next_File := Facility (I);
      Add (Catalog, To_Wide_String (Next_File.Facility));
      Load_Facility (Facility    => To_Wide_String (Next_File.Facility),
                     Source_Name => To_Wide_String (Next_File.Facility),
                     N_Locales   => N_Locales,
                     N_Messages  => N_Messages,
                     Handler     => Handler,
                     Directory   => To_Wide_String (Next_File.Directory),
                     Extension   => Extension,
                     Locales     => Selected_Locales);
      Files_Loaded := Files_Loaded or N_Locales > 0 or N_Messages > 0;
      if Verbose then
            Print_Line ("zbmcompile", "00012",
                        +N_Messages, +To_Wide_String (Next_File.Facility),
                        +N_Locales);
      end if;
   end loop;
   if Handler.Get_N_Errors > 0 and not Force then
      Print_Line ("zbmcompile", "00025", +Handler.Get_N_Errors);
      Set_Exit_Status (Failure);
   elsif not Files_Loaded then
      Print_Line ("zbmcompile", "00015");
      Set_Exit_Status (Failure);
   elsif Debug then
      Dump (Catalog);
   else
      if Handler.Get_N_Errors > 0 then
         Print_Line ("zbmcompile", "00026", +Handler.Get_N_Errors);
      end if;
      if Verbose then
         Print_Line ("zbmcompile", "00020",
                     +Number_Of_Facilities (Catalog),
                     +Number_Of_Keys (Catalog),
                     +Number_Of_Locales (Catalog),
                     +Number_Of_Messages (Catalog));
      end if;
      if Optimize then
         Catalog := Optimize (Catalog, Verbose);
      end if;
      Generate_Ada (Catalog, Package_Name,
                    Spec_Name (Package_Name),
                    Body_Name (Package_Name),
                    Verbose, Body_Initialize, Export_Name);
      Set_Exit_Status (Success);
   end if;
   if not Quiet then
      Trailer ("zbmcompile", Start_Time);
   end if;
exception
when E : Usage =>
   Print_Line ("zbmcompile", "00019", +Exception_Message (E));
   Print_Line ("zbmcompile", "00004");
   Set_Exit_Status (Failure);
end Main;
