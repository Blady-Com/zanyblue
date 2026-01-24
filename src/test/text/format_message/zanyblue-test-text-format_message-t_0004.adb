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

separate (ZanyBlue.Test.Text.Format_Message)
procedure T_0004 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text;
   use ZanyBlue.Text.Integers;

   Locale    : constant Locale_Type := Make_Locale ("en_US");
   Arguments : Argument_List;

begin
   Arguments.Append (+6);
   Arguments.Append (+55);
   Discard (Format_Message ("{1###}", Arguments, null, Locale, True));
   R.Assert (False, "Expected Invalid_Format_Error not raised");
exception
when Invalid_Format_Error =>
   R.Assert (True, "Expected Invalid_Format_Error raised");
end T_0004;
