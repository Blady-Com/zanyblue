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

--
--  Example of the use of the ZanyBlue.Text packages
--

with Ada.Command_Line;
with XFormatting_Messages;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Generic_Floats;
with ZanyBlue.Text.Generic_Integers;
with ZanyBlue.Text.Generic_Enumerations;

procedure X_Formatting is

   type Release_Status_Type is (Alpha, Beta, Production);
   type My_Float is new Float;

   package ZB_Floats is
      new ZanyBlue.Text.Generic_Floats (My_Float);
   package Status_Args is
      new ZanyBlue.Text.Generic_Enumerations (Release_Status_Type);

   use Ada.Command_Line;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Formatting;
   use ZB_Floats;
   use Status_Args;

   Usage_Error : exception;

   procedure Process_Command_Line;

   generic
      type Integer_Type is range <>;
   procedure Generic_Display_Value (Value : Integer_Type; Name : String);

   procedure Display_Value (Value : My_Float);
   procedure Display_Value (Value : My_Float; Precision : Natural);

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

   procedure Display_Value (Value : My_Float) is
   begin
      Print_Line ("xformatting", "00022", +Value);
   end Display_Value;

   procedure Display_Value (Value : My_Float; Precision : Natural) is
   begin
      Print_Line ("xformatting", "00021", +Value, +Precision);
   end Display_Value;

   Version_Major : constant := 1;
   Version_Minor : constant := 0;
   Version_Patch : constant := 0;
   Version_Status : constant Release_Status_Type := Alpha;

   X : constant My_Float := 1.2345678901e10;

begin
   Process_Command_Line;
   Print_Line ("xformatting", "mkr");
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
   Display_Value (X);
   Display_Value (X, 0);
   Display_Value (X, 1);
   Display_Value (X, 2);
   Display_Value (X, 3);
   Display_Value (X, 4);
   Display_Value (X, 5);
   Display_Value (X, 6);
   Display_Value (X, 7);
   Display_Value (X, 8);
   Disable_Exceptions;
   Print_Line ("xformatting", "00023");
exception
when Usage_Error =>
   Print_Line ("xformatting", "usage");
end X_Formatting;
