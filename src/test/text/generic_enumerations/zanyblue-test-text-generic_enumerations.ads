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

pragma License (GPL);

with AUnit.Test_Suites;

package ZanyBlue.Test.Text.Generic_Enumerations is

   use AUnit.Test_Suites;

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Register routines to be run:
   overriding
   procedure Register_Tests
     (T : in out Test_Case);

   --  Provide name identifying the test case:
   overriding
   function Name
     (T : Test_Case) return AUnit.Test_String;

   function Suite return Access_Test_Suite;

end ZanyBlue.Test.Text.Generic_Enumerations;
