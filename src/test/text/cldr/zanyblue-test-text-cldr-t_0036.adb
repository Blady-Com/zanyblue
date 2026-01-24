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
procedure T_0036 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   he : constant Locale_Type := Make_Locale ("he");

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String) is
   begin
      Check_Value (R, Script_Name (Abbreviation, Locale => he), Value,
                      "Expected: " & Value);
   end Check_Script;

begin
   if not Is_Locale_Defined ("he", "", "") then
      R.Assert (True, "HE localization not included");
      return;
   end if;
   Check_Script ("Arab", "ערבי");
   Check_Script ("Bali", "באלינזי");
   Check_Script ("Cher", "צ׳ירוקי");
   Check_Script ("Deva", "דוואנגרי");
   Check_Script ("Egyp", "כתב חרטומים");
   Check_Script ("Geor", "גאורגי");
   Check_Script ("Hang", "האנגול");
   Check_Script ("Inds", "אינדוס");
   Check_Script ("Jpan", "יפני");
   Check_Script ("Knda", "קאנדה");
   Check_Script ("Laoo", "לאית");
   Check_Script ("Mong", "מונגולי");
   Check_Script ("Orya", "אורייה");
   Check_Script ("Phnx", "פיניקי");
   Check_Script ("Runr", "רוני");
   Check_Script ("Sinh", "סינהלה");
   Check_Script ("Taml", "טמיל");
   Check_Script ("Ugar", "אוגריתי");
   Check_Script ("Xpeo", "פרסי עתיק");
   Check_Script ("Zxxx", "לא כתוב");
end T_0036;
