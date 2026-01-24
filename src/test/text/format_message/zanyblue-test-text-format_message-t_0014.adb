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

with ZanyBlue.Text.Integers;

separate (ZanyBlue.Test.Text.Format_Message)
procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Integers;

   function Format (Text : Wide_String;
                    Arg1 : Argument_Type'Class;
                    Arg2 : Argument_Type'Class) return Wide_String;

   function Format (Text : Wide_String;
                    Arg1 : Argument_Type'Class;
                    Arg2 : Argument_Type'Class) return Wide_String is
      Locale    : constant Locale_Type := Make_Locale ("en_US");
      Arguments : Argument_List;
   begin
      Arguments.Append (Arg1);
      Arguments.Append (Arg2);
      return Format_Message (Text, Arguments, null, Locale, False);
   end Format;

begin
   Check_Value (R, Format ("Ａ＃１： ＜{0}＞； Ａ＃２： ＜{1}＞", +11, +55),
                "Ａ＃１： ＜11＞； Ａ＃２： ＜55＞",
                "Format with wide character content");
end T_0014;
