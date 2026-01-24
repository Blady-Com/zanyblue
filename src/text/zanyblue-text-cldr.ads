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

pragma License (Modified_GPL);

with ZanyBlue.Text.Locales;

package ZanyBlue.Text.CLDR is

   use ZanyBlue.Text.Locales;

   function Language_Name (Code    : Wide_String;
                           Unknown : Wide_String := "";
                           Locale  : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the language name associated with an ISO language code, e.g.,
   --  in an English locale,
   --
   --     Language_Name ("fr") = "French"
   --
   --  For unkown languages, the argument string Unknown is returned, e.g.,
   --
   --     Language_Name ("xx", "UNKNOWN") = "UNKNOWN"
   --

   function Script_Name (Code : Wide_String;
                         Unknown : Wide_String := "";
                         Locale  : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the  name associated with an ISO script code, e.g.,
   --  in an English locale,
   --
   --     Script_Name ("Cyrl") = "Cyrillic"
   --
   --  For unkown scripts, the argument string Unknown is returned, e.g.,
   --
   --     Script_Name ("xx", "UNKNOWN") = "UNKNOWN"
   --

   function Territory_Name (Code : Wide_String;
                            Unknown : Wide_String := "";
                            Locale  : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the territory name associated with an ISO two letter territory
   --  code, e.g., in an English locale,
   --
   --     Territory_Name ("FR") = "France"
   --
   --  For unkown territories, the argument string Unknown is returned, e.g.,
   --
   --     Territory_Name ("xx", "UNKNOWN") = "UNKNOWN"
   --

   function Full_Locale_Name (Value  : Locale_Type;
                              Locale : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the full locale name, language and territory
   --

end ZanyBlue.Text.CLDR;
