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

separate (ZanyBlue.Test.Text.CLDR)
procedure T_0034 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   fr : constant Locale_Type := Make_Locale ("fr");

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String) is
   begin
      Check_Value (R, Script_Name (Abbreviation, Locale => fr), Value,
                      "Expected: " & Value);
   end Check_Script;

begin
   if not Is_Locale_Defined ("fr", "", "") then
      R.Assert (True, "FR localization not included");
      return;
   end if;
   Check_Script ("Arab", "arabe");
   Check_Script ("Bali", "balinais");
   Check_Script ("Cakm", "chakma");
   Check_Script ("Deva", "dévanâgarî");
   Check_Script ("Egyd", "démotique égyptien");
   Check_Script ("Geok", "géorgien khoutsouri");
   Check_Script ("Hang", "hangûl");
   Check_Script ("Inds", "indus");
   Check_Script ("Java", "javanais");
   Check_Script ("Kana", "katakana");
   Check_Script ("Lana", "lanna");
   Check_Script ("Mand", "mandéen");
   Check_Script ("Nkoo", "n’ko");
   Check_Script ("Ogam", "ogam");
   Check_Script ("Perm", "ancien permien");
   Check_Script ("Rjng", "rejang");
   Check_Script ("Samr", "samaritain");
   Check_Script ("Tagb", "tagbanoua");
   Check_Script ("Ugar", "ougaritique");
   Check_Script ("Vaii", "vaï");
   Check_Script ("Xpeo", "cunéiforme persépolitain");
   Check_Script ("Yiii", "yi");
   Check_Script ("Zmth", "notation mathématique");
end T_0034;
