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
procedure T_0065 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("fr");

begin
   Check_Value (R, Full_Month_Name (L, Jan), "janvier",
                "fr full month name, Jan");
   Check_Value (R, Full_Month_Name (L, Feb), "février",
                "fr full month name, Feb");
   Check_Value (R, Full_Month_Name (L, Mar), "mars",
                "fr full month name, Mar");
   Check_Value (R, Full_Month_Name (L, Apr), "avril",
                "fr full month name, Apr");
   Check_Value (R, Full_Month_Name (L, May), "mai",
                "fr full month name, May");
   Check_Value (R, Full_Month_Name (L, Jun), "juin",
                "fr full month name, Jun");
   Check_Value (R, Full_Month_Name (L, Jul), "juillet",
                "fr full month name, Jul");
   Check_Value (R, Full_Month_Name (L, Aug), "août",
                "fr full month name, Aug");
   Check_Value (R, Full_Month_Name (L, Sep), "septembre",
                "fr full month name, Sep");
   Check_Value (R, Full_Month_Name (L, Oct), "octobre",
                "fr full month name, Oct");
   Check_Value (R, Full_Month_Name (L, Nov), "novembre",
                "fr full month name, Nov");
   Check_Value (R, Full_Month_Name (L, Dec), "décembre",
                "fr full month name, Dec");
end T_0065;
