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
procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Characters.Conversions;

   procedure OK (Source : String; Expected : Character);
   procedure OK (Source : String; Expected : Wide_String);

   procedure OK (Source : String; Expected : Character) is
   begin
      OK (Source, "" & To_Wide_Character (Expected));
   end OK;

   procedure OK (Source : String; Expected : Wide_String) is
   begin
      R.Assert (Unescape_String (Source) = Expected,
              "Unescaped """ & Source & """ failed");
   end OK;

begin
   OK ("\b", ASCII.BS);
   OK ("\f", ASCII.FF);
   OK ("\n", ASCII.LF);
   OK ("\r", ASCII.CR);
   OK ("\t", ASCII.HT);
end T_0006;
