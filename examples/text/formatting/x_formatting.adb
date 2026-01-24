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
with XFormatting_Messages;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Generic_Integers;
with ZanyBlue.Text.Generic_Enumerations;

procedure X_Formatting is

   type Release_Status_Type is (Alpha, Beta, Production);

   package Status_Args is
      new ZanyBlue.Text.Generic_Enumerations (Release_Status_Type);

   use Ada.Command_Line;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Formatting;
   use Status_Args;

   Usage_Error : exception;

   procedure Process_Command_Line;

   generic
      type Integer_Type is range <>;
   procedure Generic_Display_Value (Value : Integer_Type; Name : String);

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
            else
               raise Usage_Error;
            end if;
         end;
      end loop;
   end Process_Command_Line;

   procedure Generic_Display_Value (Value : Integer_Type; Name : String) is
      package Integer_Handler is
         new ZanyBlue.Text.Generic_Integers (Integer_Type);
      use Integer_Handler;
   begin
      Print_Line ("xformatting", "title", +Value, +Name);
      Print_Line ("xformatting", "00001", +Value);
      Print_Line ("xformatting", "00002", +Value);
      Print_Line ("xformatting", "00003", +Value);
      Print_Line ("xformatting", "00004", +Value);
      Print_Line ("xformatting", "00005", +Value);
      Print_Line ("xformatting", "00006", +Value);
      Print_Line ("xformatting", "00007", +Value);
      Print_Line ("xformatting", "00008", +Value);
      Print_Line ("xformatting", "00009", +Value);
      Print_Line ("xformatting", "00010", +Value);
      Print_Line ("xformatting", "00011", +Value);
      Print_Line ("xformatting", "00012", +Value);
      Print_Line ("xformatting", "00013", +Value);
      Print_Line ("xformatting", "00014", +Value);
      Print_Line ("xformatting", "00015", +Value);
      Print_Line ("xformatting", "00016", +Value);
      Print_Line ("xformatting", "00017", +Value);
      Print_Line ("xformatting", "00018", +Value);
      Print_Line ("xformatting", "00019", +Value);
      Print_Line ("xformatting", "00020", +Value);
   end Generic_Display_Value;

   procedure Display_Value is
      new Generic_Display_Value (Integer);

   procedure Display_Value is
      new Generic_Display_Value (Long_Integer);

   procedure Display_Value is
      new Generic_Display_Value (Long_Long_Integer);

   Version_Major : constant := 1;
   Version_Minor : constant := 0;
   Version_Patch : constant := 0;
   Version_Status : constant Release_Status_Type := Alpha;

begin
   Process_Command_Line;
   Print_Line ("xformatting", "banner", +Integer'(Version_Major),
                                        +Integer'(Version_Minor),
                                        +Integer'(Version_Patch),
                                        +Version_Status);
   Display_Value (Integer'(1964), "Simple positive value");
   Display_Value (Integer'(-1964), "Simple negative value");
   Display_Value (Integer'First, "First integer value");
   Display_Value (Integer'Last, "Last integer value");
   Display_Value (Long_Integer'First, "First long integer first value");
   Display_Value (Long_Integer'Last, "Last long integer last value");
   Display_Value (Long_Long_Integer'First, "First long long integer value");
   Display_Value (Long_Long_Integer'Last, "Last long long integer value");
   Display_Value (Long_Long_Integer'Last - 1964,
                  "Random long long integer value");
exception
when Usage_Error =>
   Print_Line ("xformatting", "usage");
end X_Formatting;
