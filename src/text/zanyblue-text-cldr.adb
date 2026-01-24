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

with ZanyBlue.Text.CLDR_Data;
with ZanyBlue.Text.Catalogs;

package body ZanyBlue.Text.CLDR is

   use ZanyBlue.Text;
   use ZanyBlue.Text.Catalogs;

   CLDR_Catalog : constant Catalog_Type := Create;

   function Lookup (Facility : Wide_String;
                    Key      : Wide_String;
                    Unknown  : Wide_String := "";
                    Locale   : Locale_Type := Current_Locale)
      return Wide_String;
   --  Use the CLDR catalog to locate a key for a locale.  If the key is not
   --  present, simply return the Unknown value rather than raising an
   --  exception.

   ----------------------
   -- Full_Locale_Name --
   ----------------------

   function Full_Locale_Name (Value  : Locale_Type;
                              Locale : Locale_Type := Current_Locale)
      return Wide_String is
   begin
      if Territory (Value) = "" then
         return Language_Name (Language (Value), Locale => Locale);
      end if;
      return Language_Name (Language (Value), Locale => Locale) & " ("
           & Territory_Name (Territory (Value), Locale => Locale) & ")";
   end Full_Locale_Name;

   -------------------
   -- Language_Name --
   -------------------

   function Language_Name (Code    : Wide_String;
                           Unknown : Wide_String := "";
                           Locale  : Locale_Type := Current_Locale)
      return Wide_String is
   begin
      if Code = "" then
         return Lookup ("l", "root", Unknown, Locale);
      else
         return Lookup ("l", Code, Unknown, Locale);
      end if;
   end Language_Name;

   ------------
   -- Lookup --
   ------------

   function Lookup (Facility : Wide_String;
                    Key      : Wide_String;
                    Unknown  : Wide_String := "";
                    Locale   : Locale_Type := Current_Locale)
      return Wide_String is
   begin
      return Get_Text (CLDR_Catalog, Facility, Key, Locale);
   exception
   when No_Such_Key_Error =>
      return Unknown;
   end Lookup;

   -----------------
   -- Script_Name --
   -----------------

   function Script_Name (Code    : Wide_String;
                         Unknown : Wide_String := "";
                         Locale  : Locale_Type := Current_Locale)
      return Wide_String is
   begin
      return Lookup ("s", Code, Unknown, Locale);
   end Script_Name;

   --------------------
   -- Territory_Name --
   --------------------

   function Territory_Name (Code    : Wide_String;
                            Unknown : Wide_String := "";
                            Locale  : Locale_Type := Current_Locale)
      return Wide_String is
   begin
      return Lookup ("t", Code, Unknown, Locale);
   end Territory_Name;

begin
   ZanyBlue.Text.CLDR_Data.Initialize (CLDR_Catalog);
end ZanyBlue.Text.CLDR;
