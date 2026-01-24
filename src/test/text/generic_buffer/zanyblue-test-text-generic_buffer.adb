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
with ZanyBlue.Text.Generic_Buffer;

package body ZanyBlue.Test.Text.Generic_Buffer is

   type My_Integer is new Integer range 1964 .. 296400;
   package My_Buffer is
      new ZanyBlue.Text.Generic_Buffer (My_Integer);

   use AUnit;
   use My_Buffer;

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

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Generic_Buffer");
   end Name;

   --  Register test routines to call:
   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Empty buffer");
      Add_Routine (T, T_0002'Access, "T_0002, Add left");
      Add_Routine (T, T_0003'Access, "T_0003, Add right");
      Add_Routine (T, T_0004'Access, "T_0004, Add left and right");
      Add_Routine (T, T_0005'Access, "T_0005, Overflow on Add_Left");
      Add_Routine (T, T_0006'Access, "T_0006, Overflow on Add_Right");
      Add_Routine (T, T_0007'Access, "T_0007, Accumulate_Right");
      Add_Routine (T, T_0008'Access, "T_0008, Accumulate_Right w/ Scale");
      Add_Routine (T, T_0009'Access, "T_0009, Accumulate_Right w/ Prefix");
      Add_Routine (T, T_0010'Access, "T_0010, Accumulate_Right w/ Width");
      Add_Routine (T, T_0011'Access, "T_0011, Accumulate_Right w/ Base");
      Add_Routine (T, T_0012'Access, "T_0012, Accumulate_Right w/ Lower Base");
      Add_Routine (T, T_0013'Access, "T_0013, Accumulate_Right w/ Upper Base");
      Add_Routine (T, T_0014'Access, "T_0014, Accumulate_Right w/ mixed");
      Add_Routine (T, T_0015'Access, "T_0015, Accumulate_Left");
      Add_Routine (T, T_0016'Access, "T_0016, Accumulate_Left w/ Width");
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

end ZanyBlue.Test.Text.Generic_Buffer;
