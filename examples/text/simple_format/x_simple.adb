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

--
--  Example of the use of the ZanyBlue.Text packages
--

pragma License (Modified_GPL);

with Ada.Command_Line;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Generic_Enumerations;

procedure X_Simple is

   type Release_Status_Type is (Alpha, Beta, Production);

   package Status_Args is
      new ZanyBlue.Text.Generic_Enumerations (Release_Status_Type);

   use ZanyBlue.Text.Formatting;
   use Status_Args;

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

   Version_Major : constant := 1;
   Version_Minor : constant := 0;
   Version_Patch : constant := 0;
   Version_Status : constant Release_Status_Type := Alpha;

begin
   Process_Command_Line;
   Print_Line ("This is TEXT_XMPL V{0}.{1}.{2}, {3}",
               +Version_Major, +Version_Minor,
               +Version_Patch, +Version_Status);
   Print_Line ("Here is an ""{0}"" embedded string", +String'("a value"));
   Print_Line ("{1} base 16 in a field of width {0} is |{1:>{0}x}|",
               +10, +1964);
   Print_Line ("Expect a missing argument exception for the following");
   Print_Line ("Expect an exception for missing argument: ""{0}"", ""{1}""",
               +10);
end X_Simple;
