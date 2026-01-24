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
procedure T_0039 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   fr : constant Locale_Type := Make_Locale ("fr");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (R, Territory_Name (Abbreviation, Locale => fr), Value,
                      "Expected: " & Value);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("fr", "", "") then
      R.Assert (True, "FR localization not included");
      return;
   end if;
   Check_Territory ("001", "Monde");
   Check_Territory ("AC", "Île de l'Ascension");
   Check_Territory ("BB", "Barbade");
   Check_Territory ("CA", "Canada");
   Check_Territory ("DE", "Allemagne");
   Check_Territory ("EA", "Ceuta et Melilla");
   Check_Territory ("FI", "Finlande");
   Check_Territory ("GA", "Gabon");
   Check_Territory ("HK", "R.A.S. chinoise de Hong Kong");
   Check_Territory ("IC", "Îles Canaries");
   Check_Territory ("JE", "Jersey");
   Check_Territory ("KE", "Kenya");
   Check_Territory ("LA", "Laos");
   Check_Territory ("MA", "Maroc");
   Check_Territory ("NA", "Namibie");
   Check_Territory ("OM", "Oman");
   Check_Territory ("PA", "Panama");
   Check_Territory ("QA", "Qatar");
   Check_Territory ("RE", "Réunion");
   Check_Territory ("SA", "Arabie saoudite");
   Check_Territory ("TA", "Tristan da Cunha");
   Check_Territory ("UA", "Ukraine");
   Check_Territory ("VA", "État de la Cité du Vatican");
   Check_Territory ("WF", "Wallis-et-Futuna");
   Check_Territory ("YE", "Yémen");
   Check_Territory ("ZA", "Afrique du Sud");
end T_0039;
