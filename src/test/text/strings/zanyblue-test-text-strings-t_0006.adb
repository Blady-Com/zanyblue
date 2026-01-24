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
with ZanyBlue.Text.Arguments;

separate (ZanyBlue.Test.Text.Strings)
procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   Locale    : constant Locale_Type := Make_Locale ("");
   V1        : constant String := "";
   V2        : constant String := "A";
   V3        : constant String := "James Joyce";
   V4        : constant String := "Ulysses";
   List      : Argument_List;

begin
   Append (List, +V1);
   Append (List, +V2);
   Append (List, +V3);
   Append (List, +V4);
   Check_Value (R, List.Format (0, "", Locale, False), "",
           "List.Format Empty failed");
   Check_Value (R, List.Format (1, "", Locale, False), "A",
           "List.Format A failed");
   Check_Value (R, List.Format (2, "", Locale, False), "James Joyce",
           "List.Format James Joyce failed");
   Check_Value (R, List.Format (3, "", Locale, False), "Ulysses",
           "List.Format Ulysses failed");
end T_0006;
