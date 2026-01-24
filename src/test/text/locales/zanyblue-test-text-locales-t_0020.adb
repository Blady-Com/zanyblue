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
procedure T_0020 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   procedure Check_Parent (Child_Name  : Wide_String;
                           Parent_Name : Wide_String;
                           Root_Name   : Wide_String);

   procedure Check_Parent (Child_Name  : Wide_String;
                           Parent_Name : Wide_String;
                           Root_Name   : Wide_String) is

      Language       : Language_Type;
      Script         : Script_Type;
      Territory      : Territory_Type;
      Root_Language  : Language_Type;
      Root_Script    : Script_Type;
      Root_Territory : Territory_Type;
      Locale         : constant Locale_Type := Make_Locale (Child_Name);
      Root           : constant Locale_Type := Make_Locale (Root_Name);

   begin
      Get_Locale_Codes (Locale, Language, Script, Territory);
      Get_Locale_Codes (Root, Root_Language, Root_Script, Root_Territory);
      Parent_Codes (Language, Script, Territory, Root_Territory);
      Check_Value (R, Locale_Name (Language, Script, Territory), Parent_Name,
                   "Incorrect parent calculated");
   end Check_Parent;

begin
   Check_Parent ("",           "",        "fr_FR");
   Check_Parent ("fr",         "",        "fr_FR");
   Check_Parent ("fr_FR",      "fr",      "fr_FR");
   Check_Parent ("fr_Latn",    "fr_FR",   "fr_FR");
   Check_Parent ("fr_Latn_FR", "fr_Latn", "fr_FR");
end T_0020;
