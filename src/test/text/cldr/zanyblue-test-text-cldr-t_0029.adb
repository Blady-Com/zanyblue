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
procedure T_0029 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   fr : constant Locale_Type := Make_Locale ("fr");

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String);

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String) is
   begin
      Check_Value (R, Language_Name (Abbreviation, Locale => fr), Value,
                      "Expected: " & Value);
   end Check_Language;

begin
   if not Is_Locale_Defined ("fr", "", "") then
      R.Assert (True, "FR localization not included");
      return;
   end if;
   Check_Language ("aa", "afar");
   Check_Language ("ba", "bachkir");
   Check_Language ("ca", "catalan");
   Check_Language ("da", "danois");
   Check_Language ("ee", "éwé");
   Check_Language ("fa", "persan");
   Check_Language ("ga", "irlandais");
   Check_Language ("ha", "haoussa");
   Check_Language ("ia", "interlingua");
   Check_Language ("ja", "japonais");
   Check_Language ("ka", "géorgien");
   Check_Language ("ky", "kirghize");
   Check_Language ("mad", "madurais");
   Check_Language ("na", "nauruan");
   Check_Language ("oc", "occitan");
   Check_Language ("pa", "pendjabi");
   Check_Language ("qu", "langue quechua");
   Check_Language ("raj", "rajasthani");
   Check_Language ("sa", "sanskrit");
   Check_Language ("ta", "tamoul");
   Check_Language ("udm", "oudmourte");
   Check_Language ("vai", "vaï");
   Check_Language ("wa", "wallon");
   Check_Language ("xal", "kalmouk");
   Check_Language ("yao", "yao");
   Check_Language ("za", "zhuang");
end T_0029;
