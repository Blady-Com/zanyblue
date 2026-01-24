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

with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Text.Formatting)
procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Catalogs;

   Locale    : constant Locale_Type := Make_Locale ("");
   Facility  : constant Wide_String := "fac1";
   Key       : constant Wide_String := "key1";
   Message   : constant Wide_String := "From {0} to {1} is a long way";
   Expect    : constant Wide_String := "From 10 to 20 is a long way";
   Catalog   : constant Catalog_Type := Create;
   Arguments : Argument_List;

begin
   Add (Catalog, Facility, Key, Message, Locale);
   Append (Arguments, +10);
   Append (Arguments, +20);
   Check_Value (R, Format (Facility, Key, Arguments, Locale, Catalog),
                   Expect,
           "Message was incorrectly formatted");
end T_0014;
