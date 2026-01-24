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

separate (ZanyBlue.Test.Text.Utils)
procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text;

   procedure Invalid (Source : String);
   procedure OK (Source : String; Expected : Wide_String);

   procedure Invalid (Source : String) is
   begin
      Discard (Unescape_String (Source));
      R.Assert (False, "Failed to raise exception for " & Source);
   exception
   when Unicode_Format_Error =>
      R.Assert (True, "Expected Unicode_Format_Error raised");
   end Invalid;

   procedure OK (Source : String; Expected : Wide_String) is
   begin
      R.Assert (Unescape_String (Source) = Expected,
              "Unescaped """ & Source & """ failed");
   end OK;

begin
   OK ("This is \u03c0", "This is π");
   OK ("This is \u03C0", "This is π");
   OK ("This is \X03C0", "This is X03C0");
   Invalid ("\u");
   Invalid ("\u0");
   Invalid ("\u03");
   Invalid ("\u03c");
   Invalid ("\u03cx");
end T_0005;
