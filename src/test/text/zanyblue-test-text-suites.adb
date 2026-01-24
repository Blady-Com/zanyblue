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

with ZanyBlue.Test.Text.Arguments;
with ZanyBlue.Test.Text.Booleans;
with ZanyBlue.Test.Text.Buffer;
with ZanyBlue.Test.Text.Catalogs;
with ZanyBlue.Test.Text.Characters;
with ZanyBlue.Test.Text.CLDR;
with ZanyBlue.Test.Text.Durations;
with ZanyBlue.Test.Text.Floats;
with ZanyBlue.Test.Text.Format_Message;
with ZanyBlue.Test.Text.Format_Parser;
with ZanyBlue.Test.Text.Formatting;
with ZanyBlue.Test.Text.Generic_Buffer;
with ZanyBlue.Test.Text.Generic_Enumerations;
with ZanyBlue.Test.Text.Generic_Fixed;
with ZanyBlue.Test.Text.Generic_Floats;
with ZanyBlue.Test.Text.Generic_Integers;
with ZanyBlue.Test.Text.Generic_Modulars;
with ZanyBlue.Test.Text.Integers;
with ZanyBlue.Test.Text.Locales;
with ZanyBlue.Test.Text.Null_Object;
with ZanyBlue.Test.Text.Properties_Parser;
with ZanyBlue.Test.Text.Pseudo;
with ZanyBlue.Test.Text.Strings;
with ZanyBlue.Test.Text.Times;
with ZanyBlue.Test.Text.Utils;
with ZanyBlue.Test.Text.Wide_Characters;
with ZanyBlue.Test.Text.Wide_Strings;

package body ZanyBlue.Test.Text.Suites is

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, ZanyBlue.Test.Text.Arguments.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Booleans.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Buffer.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Catalogs.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Characters.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.CLDR.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Durations.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Floats.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Format_Message.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Format_Parser.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Formatting.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Generic_Buffer.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Generic_Enumerations.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Generic_Fixed.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Generic_Floats.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Generic_Integers.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Generic_Modulars.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Integers.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Locales.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Null_Object.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Properties_Parser.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Pseudo.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Strings.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Times.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Utils.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Wide_Characters.Suite);
      Add_Test (Result, ZanyBlue.Test.Text.Wide_Strings.Suite);
      return Result;
   end Suite;

end ZanyBlue.Test.Text.Suites;
