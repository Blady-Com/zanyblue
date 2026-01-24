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
procedure T_0041 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   he : constant Locale_Type := Make_Locale ("he");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (R, Territory_Name (Abbreviation, Locale => he), Value,
                      "Expected: " & Value);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("he", "", "") then
      R.Assert (True, "HE localization not included");
      return;
   end if;
   Check_Territory ("001", "העולם");
   Check_Territory ("AC", "האי אסנשן");
   Check_Territory ("BA", "בוסניה והרצגובינה");
   Check_Territory ("CA", "קנדה");
   Check_Territory ("DE", "גרמניה");
   Check_Territory ("EA", "סאוטה ומלייה");
   Check_Territory ("FI", "פינלנד");
   Check_Territory ("GA", "גאבון");
   Check_Territory ("HK", "הונג קונג - מחוז מנהלי מיוחד של סין");
   Check_Territory ("IC", "האיים הקנריים");
   Check_Territory ("JE", "ג׳רסי");
   Check_Territory ("KE", "קניה");
   Check_Territory ("LA", "לאוס");
   Check_Territory ("MA", "מרוקו");
   Check_Territory ("NA", "נמיביה");
   Check_Territory ("OM", "עומאן");
   Check_Territory ("PA", "פנמה");
   Check_Territory ("QA", "קטאר");
   Check_Territory ("RE", "ראוניון");
   Check_Territory ("SA", "ערב הסעודית");
   Check_Territory ("TA", "טריסטן דה קונה");
   Check_Territory ("UA", "אוקראינה");
   Check_Territory ("VA", "הוותיקן");
   Check_Territory ("WF", "איי ווליס ופוטונה");
   Check_Territory ("YE", "תימן");
   Check_Territory ("ZA", "דרום אפריקה");
end T_0041;
