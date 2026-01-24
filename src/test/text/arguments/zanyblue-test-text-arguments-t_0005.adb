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

with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Strings;

separate (ZanyBlue.Test.Text.Arguments)
procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Strings;

   Locale : constant Locale_Type := Make_Locale ("");
   List : Argument_List;

   procedure Check (List     : Argument_List;
                    Position : Natural);

   procedure Check (List     : Argument_List;
                    Position : Natural) is
   begin
      R.Assert (List.Format (Position, "", Locale, True) = "no valid",
                "Unexpected Format to be raise an exception");
   exception
   when No_Such_Argument_Error =>
      R.Assert (True, "Expected exception No_Such_Argument_Error raised");
   end Check;

begin
   R.Assert (List.Length = 0,
             "Length of emtpy list is not 0");
   Append (List, +String'("String#0"));
   R.Assert (List.Length = 1,
             "Length is not 1");
   R.Assert (List.Format (0, "", Locale, False) = "String#0",
             "Unexpected To_String (#0)");
   R.Assert (List.Format (1, "", Locale, False) = "|1|",
             "Unexpected To_String (#1) to be |1|");
   Check (List, 1);
end T_0005;
