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

with Ada.Containers;
with Ada.Command_Line;
with DL_Messages;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Generic_Modulars;
with ZanyBlue.Text.Generic_Enumerations;

procedure X_DumpLocale is

   use Ada.Containers;
   use Ada.Command_Line;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;

   package Hash_Type_Arguments is
      new ZanyBlue.Text.Generic_Modulars (Hash_Type);
   package Date_Time_Style_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Date_Time_Style_Type);
   package Day_Period_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Day_Period_Type);
   package Day_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Day_Type);
   package Era_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Era_Type);
   package Month_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Month_Type);
   package Numeric_Style_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Numeric_Style_Type);
   package Numeric_Item_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Numeric_Item_Type);
   package Text_Layout_Arguments is
      new ZanyBlue.Text.Generic_Enumerations (Text_Layout_Type);

   use Day_Arguments;
   use Era_Arguments;
   use Month_Arguments;
   use Hash_Type_Arguments;
   use Day_Period_Arguments;
   use Text_Layout_Arguments;
   use Numeric_Item_Arguments;
   use Numeric_Style_Arguments;
   use Date_Time_Style_Arguments;

   procedure Display (Attribute  : String;
                      Value      : Argument_Type'Class;
                      Quoted     : Boolean := False);

   procedure Dump_Locale (Name : String);

   procedure Dump_Locale (Name : String) is
      Locale : constant Locale_Type := Make_Locale (To_Wide_String (Name));
   begin
      Print_Line ("dl", "0002", +Name);
      Display ("Language",       +Language (Locale), True);
      Display ("Script",         +Script (Locale), True);
      Display ("Territory",      +Territory (Locale), True);
      Display ("Name",           +Locale_Name (Locale), True);
      Display ("Traits Name",    +Traits_Name (Locale), True);
      Display ("Traits Tag",     +Traits_Tag (Locale), True);
      Display ("Locale Level",   +Integer (Locale_Level (Locale)));
      Display ("Is Root Locale", +Is_Root_Locale (Locale));
      Display ("Layout",         +Text_Layout (Locale));
      Display ("Lower Digits",   +Locale_Digits (Locale, True));
      Display ("Upper Digits",   +Locale_Digits (Locale, False));
      Display ("Hash Value",     +Hash (Locale));
      Print_Line ("dl", "0005");
      Print_Line ("dl", "0006");
      for Style in Date_Time_Style_Type loop
         Print_Line ("dl", "0016", +Style,
                                   +Date_Format (Locale, Style),
                                   +Time_Format (Locale, Style),
                                   +Date_Time_Format (Locale, Style));
      end loop;
      Print_Line ("dl", "0007");
      for Period in Day_Period_Type loop
         Print_Line ("dl", "0003", +Period, +Day_Period_Name (Locale, Period));
      end loop;
      Print_Line ("dl", "0008");
      for Era in Era_Type loop
         Print_Line ("dl", "0003", +Era, +Era_Name (Locale, Era));
      end loop;
      Print_Line ("dl", "0009");
      Print_Line ("dl", "0012");
      for Day in Day_Type loop
         Print_Line ("dl", "0011", +Day,
                                   +Short_Day_Name (Locale, Day),
                                   +Full_Day_Name (Locale, Day));
      end loop;
      Print_Line ("dl", "0010");
      Print_Line ("dl", "0012");
      for Month in Month_Type loop
         Print_Line ("dl", "0011", +Month,
                                   +Short_Month_Name (Locale, Month),
                                   +Full_Month_Name (Locale, Month));
      end loop;
      Print_Line ("dl", "0014");
      for Style in Numeric_Style_Type loop
         Print_Line ("dl", "0003", +Style, +Numeric_Format (Locale, Style));
      end loop;
      Print_Line ("dl", "0015");
      for Item in Numeric_Item_Type loop
         Print_Line ("dl", "0003", +Item, +Numeric_Item (Locale, Item));
      end loop;
   end Dump_Locale;

   procedure Display (Attribute  : String;
                      Value      : Argument_Type'Class;
                      Quoted     : Boolean := False) is
   begin
      if Quoted then
         Print_Line ("dl", "0004", +Attribute, Value);
      else
         Print_Line ("dl", "0003", +Attribute, Value);
      end if;
   end Display;

begin
   if Argument_Count = 0 then
      Dump_Locale ("");
   elsif Argument_Count = 1 then
      Dump_Locale (Argument (1));
   else
      Print_Line ("dl", "0001");
   end if;
end X_DumpLocale;
