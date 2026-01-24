--  -*- encoding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Command_Line;
with Ada.Wide_Text_IO;
with Messages.Moons_Wide_Prints;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Generic_Enumerations;

procedure X_AMoons is

   type Planet_Names is (Mercury, Venus, Earth, Mars,
                         Jupiter, Saturn, Uranus, Neptune);

   package Planet_Name_Formatting is
      new ZanyBlue.Text.Generic_Enumerations (Planet_Names);
   package Planet_Name_IO is
      new Ada.Wide_Text_IO.Enumeration_IO (Planet_Names);

   use Ada.Command_Line;
   use Ada.Wide_Text_IO;
   use Planet_Name_IO;
   use Messages.Moons_Wide_Prints;
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
      Print_0001 (With_NL => False);
      begin
         Get (Planet);
         if Moons (Planet) /= 1 then
            Print_0002 (+Moons (Planet), +Planet);
         else
            Print_0003 (+Planet);
         end if;
      exception
         when Data_Error =>
            Print_0005;
      end;
   end loop;
exception
when End_Error | Data_Error =>
   New_Line;
   Print_0004;
end X_AMoons;
