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
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Format_Message;

package body ZanyBlue.Test.Text.Format_Message is

   use AUnit;
   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   function Format_Message (Message      : Wide_String;
                            Arguments    : Argument_List;
                            Mapping      : Pseudo_Map_Access;
                            Locale       : Locale_Type;
                            Raise_Errors : Boolean) return Wide_String
      renames ZanyBlue.Text.Format_Message;

   procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0004 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0007 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0008 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0009 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0010 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0011 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0012 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0013 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0015 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0016 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0017 (R : in out AUnit.Test_Cases.Test_Case'Class);

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Format_Message");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Simple integer format");
      Add_Routine (T, T_0002'Access, "T_0002, Format on nested selection");
      Add_Routine (T, T_0003'Access, "T_0003, Nested width definition");
      Add_Routine (T, T_0004'Access, "T_0004, Invalid_Format exception");
      Add_Routine (T, T_0005'Access, "T_0005, Invalid_Format exception");
      Add_Routine (T, T_0006'Access, "T_0006, Invalid_Format exception");
      Add_Routine (T, T_0007'Access, "T_0007, Argument selects base (x)");
      Add_Routine (T, T_0008'Access, "T_0008, Argument selects base (o)");
      Add_Routine (T, T_0009'Access, "T_0009, Argument selects base (b)");
      Add_Routine (T, T_0010'Access, "T_0010, Invalid_Format suppression");
      Add_Routine (T, T_0011'Access, "T_0011, Quoted format");
      Add_Routine (T, T_0012'Access, "T_0012, Degenerate format exception");
      Add_Routine (T, T_0013'Access, "T_0013, Degenerate format no exception");
      Add_Routine (T, T_0014'Access, "T_0014, Format with wide characters");
      Add_Routine (T, T_0015'Access, "T_0015, Format with quoting");
      Add_Routine (T, T_0016'Access, "T_0016, Format with quoting embedded");
      Add_Routine (T, T_0017'Access, "T_0017, Format with pseudo translation");
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
   procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0007 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0008 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0009 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0010 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0011 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0012 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0013 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0015 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0016 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0017 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Format_Message;
