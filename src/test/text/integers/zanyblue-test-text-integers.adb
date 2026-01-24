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

with ZanyBlue.Text.Integers;

package body ZanyBlue.Test.Text.Integers is

   use AUnit;

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
   procedure T_0018 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0019 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0020 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0021 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0022 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0023 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0024 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0025 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0026 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0027 (R : in out AUnit.Test_Cases.Test_Case'Class);

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Integers");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Negative Format");
      Add_Routine (T, T_0002'Access,
                     "T_0002, Numeric hexadecimal formatting");
      Add_Routine (T, T_0003'Access, "T_0003, Positive Format");
      Add_Routine (T, T_0004'Access,
                     "T_0004, Numeric decimal formatting");
      Add_Routine (T, T_0005'Access, "T_0005, Zero Format");
      Add_Routine (T, T_0006'Access,
                     "T_0006, Right hexadecimal formatting");
      Add_Routine (T, T_0007'Access, "T_0007, Negative List Argument");
      Add_Routine (T, T_0008'Access, "T_0008, Positive List Argument");
      Add_Routine (T, T_0009'Access, "T_0009, Zero List Argument");
      Add_Routine (T, T_0010'Access, "T_0010, Decimal formatting");
      Add_Routine (T, T_0011'Access, "T_0011, Binary formatting");
      Add_Routine (T, T_0012'Access, "T_0012, Octal formatting");
      Add_Routine (T, T_0013'Access,
                     "T_0013, Hexadecimal (lowercase) formatting");
      Add_Routine (T, T_0014'Access,
                     "T_0014, Hexadecimal (uppercase) formatting");
      Add_Routine (T, T_0015'Access,
                     "T_0015, Handling of sign specification formatting");
      Add_Routine (T, T_0016'Access,
                     "T_0016, Decimal based formatting");
      Add_Routine (T, T_0017'Access,
                     "T_0017, Binary based formatting");
      Add_Routine (T, T_0018'Access,
                     "T_0018, Octal based formatting");
      Add_Routine (T, T_0019'Access,
                     "T_0019, Hexadecimal (lowercase) based formatting");
      Add_Routine (T, T_0020'Access,
                     "T_0020, Hexadecimal (uppercase) based formatting");
      Add_Routine (T, T_0021'Access,
                     "T_0021, Centered decimal formatting");
      Add_Routine (T, T_0022'Access,
                     "T_0022, Centered hexadecimal formatting");
      Add_Routine (T, T_0023'Access,
                     "T_0023, Left decimal formatting");
      Add_Routine (T, T_0024'Access,
                     "T_0024, Left hexadecimal formatting");
      Add_Routine (T, T_0025'Access,
                     "T_0025, Right decimal formatting");
      Add_Routine (T, T_0026'Access,
                     "T_0026, Format 'First Integer");
      Add_Routine (T, T_0027'Access,
                     "T_0027, Format 'Last Integer");
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
   procedure T_0018 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0019 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0020 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0021 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0022 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0023 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0024 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0025 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0026 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0027 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Integers;
