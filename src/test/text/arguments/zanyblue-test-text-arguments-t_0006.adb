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
procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Strings;

   Locale : constant Locale_Type := Make_Locale ("");
   List : Argument_List;

begin
   R.Assert (List.Length = 0, "Length of emtpy list is not 0");
   Append (List, +String'("String#0"));
   R.Assert (List.Length = 1, "Length is not 1");
   Check_Value (R, List.Format (0, "", Locale, False), "String#0",
           "Format valid argument");
   Check_Value (R, List.Format (1, "xyz", Locale, False), "|1,xyz|",
           "Format missing argument");
end T_0006;
