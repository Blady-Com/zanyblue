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

with ZanyBlue.Compiler;

separate (ZanyBlue.Test.Text.Format_Parser)
procedure T_0033 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Compiler;

   Locale      : constant Locale_Type := Make_Locale ("en_US");
   Format_Spec : constant Wide_String := "*^#11.8odf";

begin
   declare
      Format_Rep  : constant Wide_String := To_String (Parse (Format_Spec,
                                                              Locale));
   begin
      R.Assert (False,
              To_UTF8 ("No exception raised for " & Format_Rep));
   end;
exception
when Invalid_Format =>
   R.Assert (True, "Expected exception raised");
end T_0033;
