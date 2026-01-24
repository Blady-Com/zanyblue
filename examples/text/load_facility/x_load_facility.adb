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
with Ada.Strings.Unbounded;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Generic_Enumerations;

procedure X_Load_Facility is

   type Release_Status_Type is (Alpha, Beta, Production);

   package Status_Args is
      new ZanyBlue.Text.Generic_Enumerations (Release_Status_Type);

   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;
   use Status_Args;

   Version_Major : constant := 1;
   Version_Minor : constant := 0;
   Version_Patch : constant := 0;
   Version_Status : constant Release_Status_Type := Alpha;

   Usage_Error : exception;

   procedure Process_Command_Line;

   Message_Directory : Unbounded_String := To_Unbounded_String (".");

   procedure Process_Command_Line is
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
            elsif Option'Length > 1 and Option (1) = '-' then
               raise Usage_Error;
            else
               Message_Directory := To_Unbounded_String (Option);
            end if;
         end;
      end loop;
   end Process_Command_Line;

   N_Locales   : Natural;
   N_Messages  : Natural;

begin
   Process_Command_Line;
   Load_Facility (Standard_Catalog, "textxmpl", N_Locales, N_Messages,
                  To_Wide_String (To_String (Message_Directory)));
   Print_Line ("textxmpl", "banner", +Version_Major, +Version_Minor,
                                     +Version_Patch, +Version_Status);
   Print_Line ("textxmpl", "loaded", +N_Messages, +N_Locales);
exception
when Usage_Error =>
   Print_Line ("textxmpl", "usage");
end X_Load_Facility;
