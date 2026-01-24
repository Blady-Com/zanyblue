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
procedure T_0049 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Catalogs;

   Locale    : constant Locale_Type := Make_Locale ("");
   Facility  : constant Wide_String := "fac1";
   Key       : constant Wide_String := "key1";
   Message   : constant Wide_String := "Message: #1={0}, #2={1}";
   Expect    : constant Wide_String := "Message: #1=10, #2=|1|";
   Catalog   : constant Catalog_Type := Create;

   procedure With_Exception;
   procedure Without_Exception;

   procedure With_Exception is
   begin
      Enable_Exceptions (Catalog);
      Discard (Format (Facility, Key, +10,
                       Locale => Locale, Catalog => Catalog));
      R.Assert (False, "Expected an exception");
   exception
   when No_Such_Argument_Error =>
      R.Assert (True, "Expected exception raised");
   end With_Exception;

   procedure Without_Exception is
   begin
      Disable_Exceptions (Catalog);
      Check_Value (R, Format (Facility, Key, +10,
                              Locale => Locale, Catalog => Catalog),
                   Expect,
              "Format missing argument failed");
   end Without_Exception;

begin
   Add (Catalog, Facility, Key, Message, Locale);
   With_Exception;
   Without_Exception;
end T_0049;
