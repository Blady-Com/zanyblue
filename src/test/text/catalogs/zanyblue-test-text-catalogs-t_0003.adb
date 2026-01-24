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

separate (ZanyBlue.Test.Text.Catalogs)
procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Wide_Text_IO;

   F          : constant Wide_String := "f";
   K          : constant Wide_String := "k";
   b          : constant Locale_Type := Make_Locale ("");
   en         : constant Locale_Type := Make_Locale ("en");
   en_US      : constant Locale_Type := Make_Locale ("en_US");
   en_Latn    : constant Locale_Type := Make_Locale ("en_Latn");
   en_Latn_US : constant Locale_Type := Make_Locale ("en_Latn_US");
   Catalog    : constant Catalog_Type := Create;

   procedure Check (L            : Locale_Type;
                    V            : Wide_String;
                    b_M          : Wide_String;
                    en_M         : Wide_String;
                    en_US_M      : Wide_String;
                    en_Latn_M    : Wide_String;
                    en_Latn_US_M : Wide_String);
   procedure Check (V : Wide_String;
                    L : Locale_Type);

   procedure Check (V : Wide_String;
                    L : Locale_Type) is
   begin
      Check_Value (R, Get_Text (Catalog, F, K, L), V,
                   "Locale """ & Locale_Name (L) & """");
   end Check;

   procedure Check (L            : Locale_Type;
                    V            : Wide_String;
                    b_M          : Wide_String;
                    en_M         : Wide_String;
                    en_US_M      : Wide_String;
                    en_Latn_M    : Wide_String;
                    en_Latn_US_M : Wide_String) is
   begin
      Add (Catalog, F, K, V, L);
      Check (en_Latn_US_M, en_Latn_US);
      Check (en_Latn_M, en_Latn);
      Check (en_US_M, en_US);
      Check (en_M, en);
      Check (b_M, b);
   end Check;

begin
   Check (b, "B",            "B", "B", "B",  "B",  "B");
   Check (en, "E",           "B", "E", "E",  "E",  "E");
   Check (en_US, "EU",       "B", "E", "EU", "E",  "EU");
   Check (en_Latn, "EL",     "B", "E", "EU", "EL", "EL");
   Check (en_Latn_US, "ELU", "B", "E", "EU", "EL", "ELU");
end T_0003;
