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

separate (ZanyBlue.Test.Text.Formatting)
procedure T_0094 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   package T_0094_Types is

      type T_0094_Type is (T_0094_1, T_0094_2, T_0094_3, T_0094_4, T_0094_5);

      type T_0094_Argument is new Argument_Type with private;

      function Create (Value : T_0094_Type) return T_0094_Argument;

      function "+" (Value : T_0094_Type) return T_0094_Argument
         renames Create;

      overriding
      function Format (Value    : T_0094_Argument;
                       Template : Wide_String;
                       Locale   : Locale_Type) return Wide_String;

   private

      type T_0094_Argument is new Argument_Type with record
         Data : T_0094_Type;
      end record;

   end T_0094_Types;

   package body T_0094_Types is
      separate;

   use T_0094_Types;

   Locale : constant Locale_Type := Make_Locale ("");

begin
   Check_Value (R, Format ("Arg: {0}", +T_0094_1,
                           Locale => Locale),
                   "Arg: T_0094_1",
           "Formatting should have generated T_0094_1");
   Check_Value (R, Format ("Arg: {0,2}", +T_0094_2,
                           Locale => Locale),
                   "Arg: T_0094_2",
           "Formatting should have generated T_0094_2");
   Check_Value (R, Format ("Arg: {0:2}", +T_0094_3,
                           Locale => Locale),
                   "Arg: T_0094_3",
           "Formatting should have generated T_0094_3");
   Check_Value (R, Format ("Arg: {0:2//////}", +T_0094_4,
                           Locale => Locale),
                   "Arg: T_0094_4",
           "Formatting should have generated T_0094_4");
end T_0094;
