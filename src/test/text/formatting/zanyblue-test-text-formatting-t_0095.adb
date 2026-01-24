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

separate (ZanyBlue.Test.Text.Formatting)
procedure T_0095 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Locale : constant Locale_Type := Make_Locale ("");

begin
   Discard (Format ("Arg: {0//////}", +10, Locale => Locale));
   R.Assert (False, "Expected Invalid_Format exception not raised");
exception
when Invalid_Format_Error =>
   R.Assert (True, "Expected Invalid_Format exception raised");
end T_0095;
