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

with Ada.Characters.Conversions;

separate (ZanyBlue.Test.Text.Utils)
procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Characters.Conversions;

   procedure OK (Source : Wide_String; Expected : String);

   procedure OK (Source : Wide_String; Expected : String) is
   begin
      R.Assert (Escape_String (Source) = Expected,
              "Escaped to " & Expected & " failed");
   end OK;

begin
   OK ("abc", "abc");
   OK ("This is π", "This is \u03c0");
end T_0003;
