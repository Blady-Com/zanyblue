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

pragma License (Modified_GPL);

with Ada.Calendar;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;

package ZanyBlue.Utils is

   use Ada.Calendar;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;

   function Banner (Facility_Name      : Wide_String;
                    Banner_Message     : Wide_String := "00001";
                    Copyright_Message  : Wide_String := "00002";
                    Catalog            : Catalog_Type := Standard_Catalog)
      return Time;
   --  Print a stdandard application banner on startup returning the start
   --  time.

   procedure Banner (Facility_Name      : Wide_String;
                     Start_Time         : out Time;
                     Banner_Message     : Wide_String := "00001";
                     Copyright_Message  : Wide_String := "00002";
                     Catalog            : Catalog_Type := Standard_Catalog);
   --  Print a stdandard application banner on startup with given the start
   --  time.

   procedure Trailer (Facility_Name     : Wide_String;
                      Start_Time        : Time;
                      Trailer_Message   : Wide_String := "00003";
                      Catalog           : Catalog_Type := Standard_Catalog);
   --  Print the standard "goodbye" message prior to exiting an application.

end ZanyBlue.Utils;
