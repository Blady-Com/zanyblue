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

with AUnit;
with ZanyBlue.Text.Utils;
with ZanyBlue.Text.Pseudo;

package body ZanyBlue.Test.Text.Pseudo is

   use AUnit;
   use ZanyBlue.Text.Utils;
   use ZanyBlue.Text.Pseudo;

   procedure Check_Mapping (R : in out AUnit.Test_Cases.Test_Case'Class;
                            Mapping : Pseudo_Map_Type;
                            Source  : Wide_String;
                            Target  : Wide_String);

   procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0004 (R : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Mapping (R : in out AUnit.Test_Cases.Test_Case'Class;
                            Mapping : Pseudo_Map_Type;
                            Source  : Wide_String;
                            Target  : Wide_String) is
      function Expected (Ch : Wide_Character) return Wide_Character;

      function Expected (Ch : Wide_Character) return Wide_Character is
      begin
         for I in Source'Range loop
            if Source (I) = Ch then
               return Target (I);
            end if;
         end loop;
         return Ch;
      end Expected;

   begin
      for Ch in Wide_Character'Range loop
         R.Assert (Map (Mapping, Ch) = Expected (Ch),
                 "Failed to Map ("
               & Escape_String ("" & Ch) & ") to "
               & Escape_String ("" & Expected (Ch)));
      end loop;
   end Check_Mapping;

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Pseudo");
   end Name;
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Uppercase mappings");
      Add_Routine (T, T_0002'Access, "T_0002, Lowercase mappings");
      Add_Routine (T, T_0003'Access, "T_0003, Halfwidth_Forms mappings");
      Add_Routine (T, T_0004'Access, "T_0004, Enclosed_Alphanumeric mappings");
   end Register_Tests;


   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Test_Case);
      return Result;
   end Suite;

   procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0004 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Pseudo;
