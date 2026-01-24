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

with ZanyBlue.Text.Generic_Enumerations;

package body ZanyBlue.Utils is

   package Version_Status_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (ZanyBlue.Version_Status_Type);

   ------------
   -- Banner --
   ------------

   function Banner (Facility_Name      : Wide_String;
                    Banner_Message     : Wide_String := "00001";
                    Copyright_Message  : Wide_String := "00002";
                    Catalog            : Catalog_Type := Standard_Catalog)
      return Time is
      Start_Time : Time;
   begin
      Banner (Facility_Name, Start_Time, Banner_Message, Copyright_Message,
              Catalog);
      return Start_Time;
   end Banner;

   ------------
   -- Banner --
   ------------

   procedure Banner (Facility_Name      : Wide_String;
                     Start_Time         : out Time;
                     Banner_Message     : Wide_String := "00001";
                     Copyright_Message  : Wide_String := "00002";
                     Catalog            : Catalog_Type := Standard_Catalog) is

      use Version_Status_Arguments;

      Arguments : Argument_List;

   begin
      Start_Time := Clock;
      Append (Arguments, +ZanyBlue.Version_Major);
      Append (Arguments, +ZanyBlue.Version_Minor);
      Append (Arguments, +ZanyBlue.Version_Patch);
      Append (Arguments, +ZanyBlue.Version_Status);
      Append (Arguments, +ZanyBlue.Revision);
      Append (Arguments, +Start_Time);
      Print_Line (Facility_Name, Banner_Message, Arguments,
                  Catalog => Catalog);
      Print_Line (Facility_Name, Copyright_Message, +ZanyBlue.Copyright_Year,
                  Catalog => Catalog);
   end Banner;

   -------------
   -- Trailer --
   -------------

   procedure Trailer (Facility_Name     : Wide_String;
                      Start_Time        : Ada.Calendar.Time;
                      Trailer_Message   : Wide_String := "00003";
                      Catalog           : Catalog_Type := Standard_Catalog) is

      Now     : constant Time := Clock;
      Elapsed : constant Duration := Now - Start_Time;

   begin
      Print_Line (Facility_Name, Trailer_Message, +Now, +Elapsed,
                  Catalog => Catalog);
   end Trailer;

end ZanyBlue.Utils;
