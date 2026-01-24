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

with Ada.Command_Line;
with Ada.Wide_Text_IO;
with Moons_Messages;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Generic_Enumerations;

procedure X_Moons is

   type Planet_Names is (Mercury, Venus, Earth, Mars,
                         Jupiter, Saturn, Uranus, Neptune);

   package Planet_Name_Formatting is
      new ZanyBlue.Text.Generic_Enumerations (Planet_Names);
   package Planet_Name_IO is
      new Ada.Wide_Text_IO.Enumeration_IO (Planet_Names);

   use Ada.Command_Line;
   use Ada.Wide_Text_IO;
   use Planet_Name_IO;
   use ZanyBlue.Text.Locales;
   use Planet_Name_Formatting;
   use ZanyBlue.Text.Formatting;

   Moons  : constant array (Planet_Names) of Natural := (
                  Earth => 1, Mars => 2, Jupiter => 63,
                  Saturn => 62, Uranus => 27, Neptune => 13,
                  others => 0);
   Planet : Planet_Names;

begin
   if Argument_Count = 1 then
      Set_Locale (Argument (1));
   end if;
   loop
      Print ("moons", "0001");
      Get (Planet);
      if Moons (Planet) /= 1 then
         Print_Line ("moons", "0002", +Moons (Planet),
                                      +Planet);
      else
         Print_Line ("moons", "0003", +Planet);
      end if;
   end loop;
exception
when End_Error | Data_Error =>
   New_Line;
   Print_Line ("moons", "0004");
end X_Moons;
