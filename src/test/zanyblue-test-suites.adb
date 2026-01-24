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

with ZanyBlue.Test.Compiler.Suites;
with ZanyBlue.Test.OS.Suites;
with ZanyBlue.Test.Text.Suites;
with ZanyBlue.Test.Utils.Suites;

package body ZanyBlue.Test.Suites is

   function Suites return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, ZanyBlue.Test.Compiler.Suites.Suite);
      Add_Test (Result, ZanyBlue.Test.OS.Suites.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Suites.Suite);
      Add_Test (Result, ZanyBlue.Test.Utils.Suites.Suite);
      return Result;
   end Suites;

end ZanyBlue.Test.Suites;
