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
with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Text.Formatting)
procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Exceptions;
   use ZanyBlue.Text.Catalogs;

   My_Error : exception;
   Locale    : constant Locale_Type := Make_Locale ("");
   Facility  : constant Wide_String := "fac1";
   Key       : constant Wide_String := "key1";
   Message   : constant Wide_String := "Message: {0}, {1}, {2}, {3}, {4}";
   Catalog   : constant Catalog_Type := Create;

begin
   Add (Catalog, Facility, Key, Message, Locale);
   Raise_Exception (My_Error'Identity,
                    Facility, Key, +1, +2, +3, +4, +5,
                    Locale => Locale, Catalog => Catalog);
   R.Assert (False, "Raise_Exception returned!");
exception
when E : My_Error =>
   Check_Value (R, From_UTF8 (Exception_Message (E)),
                   "Message: 1, 2, 3, 4, 5",
           "My message not raised");
end T_0006;
