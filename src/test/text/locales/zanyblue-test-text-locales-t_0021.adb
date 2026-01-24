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
procedure T_0021 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("en_US");

begin
   Check_Value (R, Time_Format (L, Full), "h:mm:ss a zzzz",
                "en_US full time format");
   Check_Value (R, Time_Format (L, Long), "h:mm:ss a z",
                "en_US long time format");
   Check_Value (R, Time_Format (L, Medium), "h:mm:ss a",
                "en_US medium time format");
   Check_Value (R, Time_Format (L, Short), "h:mm a",
                "en_US short time format");
end T_0021;
