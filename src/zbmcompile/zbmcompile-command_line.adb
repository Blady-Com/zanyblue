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

with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with ZanyBlue.Compiler;
with ZanyBlue.Text.Formatting;

package body ZBMCompile.Command_Line is

   package Facility_Source_Vectors is
      new Ada.Containers.Indefinite_Vectors (
             Index_Type   => Positive,
             Element_Type => Facility_Source_Type);

   use Ada.Containers;
   use Ada.Command_Line;
   use ZanyBlue.Text;
   use Facility_Source_Vectors;
   use ZanyBlue.Text.Formatting;

   Verbose_Argument          : Boolean := False;
   Quiet_Argument            : Boolean := False;
   Optimize_Argument         : Boolean := False;
   Force_Argument            : Boolean := False;
   Debug_Argument            : Boolean := False;
   Body_Initialize_Argument  : Boolean := False;
   Selected_Locales_Argument : Locale_Set;
   Extension_Argument        : Unbounded_Wide_String
                                  := To_Unbounded_Wide_String ("properties");
   Directory_Argument        : Unbounded_Wide_String
                                  := To_Unbounded_Wide_String (".");
   Package_Name_Argument     : Unbounded_Wide_String;
   Package_Spec_Argument     : Unbounded_Wide_String;
   Package_Body_Argument     : Unbounded_Wide_String;
   Facilities                : Vector;
   Export_Name_P_Argument    : Boolean := False;
   Export_Name_Argument      : Unbounded_Wide_String;

   procedure Add_Facility (Name : Wide_String);
   --  Add a facility to the list to process.  This involves
   --  "remembering" the directory in "effect" at this point.

   procedure Select_Locale (Locale_Name : Wide_String);
   --  Add a locale to the set of selected locales.

   ------------------
   -- Add_Facility --
   ------------------

   procedure Add_Facility (Name : Wide_String) is

      Next_Entry       : Facility_Source_Type;
   begin
      Next_Entry.Directory := Directory_Argument;
      Next_Entry.Facility := To_Unbounded_Wide_String (Name);
      Append (Facilities, Next_Entry);
   end Add_Facility;

   ---------------------
   -- Body_Initialize --
   ---------------------

   function Body_Initialize return Boolean is
   begin
      return Body_Initialize_Argument;
   end Body_Initialize;

   ---------------
   -- Body_Name --
   ---------------

   function Body_Name (Package_Name : Wide_String) return Wide_String is
   begin
      if Package_Body'Length > 0 then
         return Package_Body;
      else
         return ZanyBlue.Compiler.Body_File_Name (Package_Name);
      end if;
   end Body_Name;

   -----------
   -- Debug --
   -----------

   function Debug return Boolean is
   begin
      return Debug_Argument;
   end Debug;

   -----------------
   -- Export_Name --
   -----------------

   function Export_Name return Wide_String is
   begin
      return To_Wide_String (Export_Name_Argument);
   end Export_Name;

   -------------------
   -- Export_Name_P --
   -------------------

   function Export_Name_P return Boolean is
   begin
      return Export_Name_P_Argument;
   end Export_Name_P;

   ---------------
   -- Extension --
   ---------------

   function Extension return Wide_String is
   begin
      return To_Wide_String (Extension_Argument);
   end Extension;

   --------------
   -- Facility --
   --------------

   function Facility (I : Positive) return Facility_Source_Type is
   begin
      return Element (Facilities, I);
   end Facility;

   -----------
   -- Force --
   -----------

   function Force return Boolean is
   begin
      return Force_Argument;
   end Force;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected (Locale : Locale_Type) return Boolean is
   begin
      return Selected_Locales_Argument.Length = 0
          or Selected_Locales_Argument.Contains (Locale);
   end Is_Selected;

   ------------------
   -- N_Facilities --
   ------------------

   function N_Facilities return Natural is
   begin
      return Natural (Length (Facilities));
   end N_Facilities;

   --------------
   -- Optimize --
   --------------

   function Optimize return Boolean is
   begin
      return Optimize_Argument;
   end Optimize;

   ------------------
   -- Package_Body --
   ------------------

   function Package_Body return Wide_String is
   begin
      return To_Wide_String (Package_Body_Argument);
   end Package_Body;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name return Wide_String is
   begin
      return To_Wide_String (Package_Name_Argument);
   end Package_Name;

   ------------------
   -- Package_Spec --
   ------------------

   function Package_Spec return Wide_String is
   begin
      return To_Wide_String (Package_Spec_Argument);
   end Package_Spec;

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line is

      Index             : Positive := 1;

      function Get_Option_Value (Ch : Character) return Wide_String;
      function Get_Option_Value (Ch : Character) return Unbounded_Wide_String;

      function Get_Option_Value (Ch : Character) return Wide_String is
      begin
         Index := Index + 1;
         if Index > Argument_Count then
            Raise_Exception (Usage'Identity,
                             "zbmcompile", "00016", +Ch);
         end if;
         return From_UTF8 (Argument (Index));
      end Get_Option_Value;

      function Get_Option_Value (Ch : Character)
         return Unbounded_Wide_String is
      begin
         return To_Unbounded_Wide_String (Get_Option_Value (Ch));
      end Get_Option_Value;

   begin
      while Index <= Argument_Count loop
         declare
            Current_Argument : constant String := Argument (Index);
         begin
            if Current_Argument = "-v" then
               Verbose_Argument := True;
            elsif Current_Argument = "-F" then
               Force_Argument := True;
            elsif Current_Argument = "-D" then
               Debug_Argument := True;
            elsif Current_Argument = "-O" then
               Optimize_Argument := True;
            elsif Current_Argument = "-g" then
               Optimize_Argument := False;
            elsif Current_Argument = "-q" then
               Quiet_Argument := True;
            elsif Current_Argument = "-i" then
               Body_Initialize_Argument := True;
            elsif Current_Argument = "-B" then
               Select_Locale ("");
            elsif Current_Argument = "-x" then
               Export_Name_P_Argument := True;
               Export_Name_Argument := Get_Option_Value ('x');
            elsif Current_Argument = "-L" then
               Select_Locale (Get_Option_Value ('L'));
            elsif Current_Argument = "-e" then
               Extension_Argument := Get_Option_Value ('e');
            elsif Current_Argument = "-d" then
               Directory_Argument := Get_Option_Value ('d');
            elsif Current_Argument = "-s" then
               Package_Spec_Argument := Get_Option_Value ('s');
            elsif Current_Argument = "-b" then
               Package_Body_Argument := Get_Option_Value ('b');
            elsif Length (Package_Name_Argument) = 0 then
               Package_Name_Argument := To_Unbounded_Wide_String (
                                           From_UTF8 (Current_Argument));
            else
               Add_Facility (From_UTF8 (Current_Argument));
            end if;
         end;
         Index := Index + 1;
      end loop;
      if Length (Package_Name_Argument) = 0 then
         Raise_Exception (Usage'Identity,
                          "zbmcompile", "00017");
      end if;
      if Facilities.Length = 0 then
         Raise_Exception (Usage'Identity,
                          "zbmcompile", "00018");
      end if;
   end Process_Command_Line;

   -----------
   -- Quiet --
   -----------

   function Quiet return Boolean is
   begin
      return Quiet_Argument;
   end Quiet;

   -------------------
   -- Select_Locale --
   -------------------

   procedure Select_Locale (Locale_Name : Wide_String) is
   begin
      Selected_Locales_Argument.Insert (Make_Locale (Locale_Name));
   end Select_Locale;

   ----------------------
   -- Selected_Locales --
   ----------------------

   function Selected_Locales return Locale_Set is
   begin
      return Selected_Locales_Argument;
   end Selected_Locales;

   ---------------
   -- Spec_Name --
   ---------------

   function Spec_Name (Package_Name : Wide_String) return Wide_String is
   begin
      if Package_Spec'Length > 0 then
         return Package_Spec;
      else
         return ZanyBlue.Compiler.Spec_File_Name (Package_Name);
      end if;
   end Spec_Name;

   -------------
   -- Verbose --
   -------------

   function Verbose return Boolean is
   begin
      return Verbose_Argument;
   end Verbose;

end ZBMCompile.Command_Line;
