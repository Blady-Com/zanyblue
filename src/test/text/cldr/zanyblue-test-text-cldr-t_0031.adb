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
procedure T_0031 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   he : constant Locale_Type := Make_Locale ("he");

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String);

   procedure Check_Language (Abbreviation : Wide_String;
                             Value        : Wide_String) is
   begin
      Check_Value (R, Language_Name (Abbreviation, Locale => he), Value,
                      "Expected: " & Value);
   end Check_Language;

begin
   if not Is_Locale_Defined ("he", "", "") then
      R.Assert (True, "HE localization not included");
      return;
   end if;
   Check_Language ("aa", "אפארית");
   Check_Language ("ba", "בשקירית");
   Check_Language ("ca", "קטלאנית");
   Check_Language ("da", "דנית");
   Check_Language ("egy", "מצרית עתיקה");
   Check_Language ("fa", "פרסית");
   Check_Language ("ga", "אירית");
   Check_Language ("ha", "האוסה");
   Check_Language ("ia", "‏אינטרלינגואה");
   Check_Language ("ja", "יפנית");
   Check_Language ("ka", "גיאורגית");
   Check_Language ("la", "לטינית");
   Check_Language ("mag", "מאגאהית");
   Check_Language ("na", "נאורית");
   Check_Language ("oc", "אוקסיטנית");
   Check_Language ("pa", "פנג׳אבית");
   Check_Language ("raj", "ראג׳סטן");
   Check_Language ("sa", "סנסקריט");
   Check_Language ("ta", "טמילית");
   Check_Language ("ug", "אויגהור");
   Check_Language ("ve", "וונדה");
   Check_Language ("xh", "קסוסה");
   Check_Language ("yap", "יאפזית");
   Check_Language ("zh", "סינית");
end T_0031;
