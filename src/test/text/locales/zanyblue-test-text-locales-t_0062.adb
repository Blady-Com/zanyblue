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
procedure T_0062 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("ar");

begin
   Check_Value (R, Short_Month_Name (L, Jan), "Jan",
                "ar short month name, Jan");
   Check_Value (R, Short_Month_Name (L, Feb), "Feb",
                "ar short month name, Feb");
   Check_Value (R, Short_Month_Name (L, Mar), "Mar",
                "ar short month name, Mar");
   Check_Value (R, Short_Month_Name (L, Apr), "Apr",
                "ar short month name, Apr");
   Check_Value (R, Short_Month_Name (L, May), "May",
                "ar short month name, May");
   Check_Value (R, Short_Month_Name (L, Jun), "Jun",
                "ar short month name, Jun");
   Check_Value (R, Short_Month_Name (L, Jul), "Jul",
                "ar short month name, Jul");
   Check_Value (R, Short_Month_Name (L, Aug), "Aug",
                "ar short month name, Aug");
   Check_Value (R, Short_Month_Name (L, Sep), "Sep",
                "ar short month name, Sep");
   Check_Value (R, Short_Month_Name (L, Oct), "Oct",
                "ar short month name, Oct");
   Check_Value (R, Short_Month_Name (L, Nov), "Nov",
                "ar short month name, Nov");
   Check_Value (R, Short_Month_Name (L, Dec), "Dec",
                "ar short month name, Dec");
end T_0062;
