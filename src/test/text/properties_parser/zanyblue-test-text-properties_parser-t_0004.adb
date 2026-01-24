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

with Ada.Exceptions;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Text.Properties_Parser)
procedure T_0004 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Exceptions;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;

   Test_Name : constant Wide_String := "t_0004";
   File_Name : constant Wide_String := Test_In_Name (Test_Area, Test_Name);
   Facility  : constant Wide_String := "myfac";
   Locale    : constant Locale_Type := Make_Locale ("en_US");
   Catalog   : constant Catalog_Type := Create;
   Handler   : Catalog_Handler_Type;

   function Last_N (S : String; N : Positive) return String;

   function Last_N (S : String; N : Positive) return String is
   begin
      if S'Length <= N then
         return S;
      end if;
      return S (S'Last + 1 - N .. S'Last);
   end Last_N;

begin
   Handler.Set_Catalog (Catalog);
   Parse (File_Name, Facility, Locale, Handler);
   R.Assert (False, "Expected exception not raised");
exception
when Error : Unicode_Escape_Error =>
   R.Assert (True, "Expected syntax error raised");
   Check_Value (R, To_Wide_String (Last_N (Exception_Message (Error), 20)),
                   "t_0004.in: 2:\u33x:x",
           "File name and line number not correctly reported");
end T_0004;
