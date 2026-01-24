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

separate (ZanyBlue.Test.Text.Locales)
procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   type Op_Type is (EQ, NEQ);

   procedure Check_Locales (Left, Right : Wide_String;
                            Op          : Op_Type);

   procedure Check_Locales (Left, Right : Wide_String;
                            Op          : Op_Type) is
      Left_Locale  : constant Locale_Type := Make_Locale (Left);
      Right_Locale : constant Locale_Type := Make_Locale (Right);
   begin
      case Op is
      when EQ =>
         R.Assert (Left_Locale = Right_Locale,
                   To_UTF8 ("'" & Left & "' should = '" & Right & "'"));
      when NEQ =>
         R.Assert (Left_Locale /= Right_Locale,
                   To_UTF8 ("'" & Left & "' should /= '" & Right & "'"));
      end case;
   end Check_Locales;

begin
   Check_Locales ("",      "",      EQ);
   Check_Locales ("fr",    "",      NEQ);
   Check_Locales ("",      "fr",    NEQ);
   Check_Locales ("fr_FR", "fr",    NEQ);
   Check_Locales ("fr",    "fr_FR", NEQ);
   Check_Locales ("fr",    "fr",    EQ);
   Check_Locales ("fr_FR", "fr_FR", EQ);
end T_0005;
