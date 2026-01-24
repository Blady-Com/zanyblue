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
with Ada.Command_Line;
with Curtime_Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Formatting;

procedure X_CurTime is

   use Ada.Calendar;
   use ZanyBlue.Text.Formatting;

   procedure Process_Command_Line;

   Usage_Error : exception;

   procedure Process_Command_Line is
      use Ada.Command_Line;
      use ZanyBlue.Text.Pseudo;
      use ZanyBlue.Text.Locales;
   begin
      for I in 1 .. Argument_Count loop
         declare
            Option : constant String := Argument (I);
         begin
            if Option = "-xh" or Option = "-x" then
               Pseudo_Translate (Halfwidth_Forms_Map);
            elsif Option = "-xe" then
               Pseudo_Translate (Enclosed_Alphanumeric_Map);
            elsif Option = "-xl" then
               Pseudo_Translate (Lowercase_Map);
            elsif Option = "-xu" then
               Pseudo_Translate (Uppercase_Map);
            elsif Option (1 .. 2) = "-l" then
               Set_Locale (Option (3 .. Option'Last));
            else
               raise Usage_Error;
            end if;
         end;
      end loop;
   end Process_Command_Line;

   Time_Format : constant String := "EEE MMM d HH:mm:ss yyyy";

   Now : constant Time := Clock;

begin
   Process_Command_Line;
   Print_Line ("curtime", "0001", +Now);
   Print_Line ("curtime", "0002", +Now);
   Print_Line ("curtime", "0003", +Now);
   Print_Line ("curtime", "0004", +Now);
   Print_Line ("curtime", "0005", +Now);
   Print_Line ("curtime", "0006", +Now);
   Print_Line ("curtime", "0007", +Now);
   Print_Line ("curtime", "0008", +Now);
   Print_Line ("curtime", "0009", +Now);
   Print_Line ("curtime", "0010", +Now, +Time_Format);
end X_CurTime;
