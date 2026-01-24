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

with ZanyBlue.Text.Buffer;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Utils;

package body ZanyBlue.Text.Times is

   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Utils;

   function Apply_Format (Format_String : Wide_String;
                          Data          : Time;
                          TZ_Offset     : Time_Offset;
                          Locale        : Locale_Type) return Wide_String;
   --  Apply the date/time format string which includes the Unicode.org
   --  value strings, e.g., EEEE for the full day name.

   function To_Month (Number : Month_Number) return Month_Type;
   --  Convert a Month_Number to the Month_Type enumeration.

   ------------------
   -- Apply_Format --
   ------------------

   function Apply_Format (Format_String : Wide_String;
                          Data          : Time;
                          TZ_Offset     : Time_Offset;
                          Locale        : Locale_Type) return Wide_String is

      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      DSeconds : Day_Duration;
      Hours    : Integer range 0 .. 24;
      Minutes  : Integer range 0 .. 60;
      Seconds  : Integer range 0 .. 60;
      Position : Natural := Format_String'First;
      Buffer   : Buffer_Type (64);
      Quoted   : Boolean := False;
      Ch       : Wide_Character;

   begin
      Split (Data, Year, Month, Day, DSeconds);
      Seconds := Integer (DSeconds) rem 60;
      Minutes := (Integer (DSeconds) / 60) rem 60;
      Hours := Integer (DSeconds) / 3600;
      loop
         exit when Position > Format_String'Last;
         Ch := Format_String (Position);
         Position := Position + 1;
         if Quoted then
            if Ch = ''' then
               Quoted := False;
            else
               Add_Left (Buffer, Ch);
            end if;
         else
            case Ch is
            when 'a' =>
               if Integer (DSeconds) < 43200 then
                  Add_Left (Buffer, Day_Period_Name (Locale, AM));
               elsif Integer (DSeconds) > 43200 then
                  Add_Left (Buffer, Day_Period_Name (Locale, PM));
               else
                  Add_Left (Buffer, Day_Period_Name (Locale, Noon));
               end if;
            when 'd' =>      --  dd
               if Starts_With (Format_String, Position, "d") then
                  Accumulate_Left (Buffer, Integer (Day), Locale, Width => 2);
                  Position := Position + 1;
               else
                  Accumulate_Left (Buffer, Integer (Day), Locale);
               end if;
            when 'E' =>      --  EEEE
               if Starts_With (Format_String, Position, "EEE") then
                  Add_Left (Buffer,
                            Full_Day_Name (Locale,
                                           Day_In_Week (Day, Month, Year)));
                  Position := Position + 3;
               elsif Starts_With (Format_String, Position, "EE") then
                  Add_Left (Buffer,
                            Short_Day_Name (Locale,
                                            Day_In_Week (Day, Month, Year)));
                  Position := Position + 2;
               else
                  Add_Left (Buffer, Ch);
               end if;
            when 'G' =>
               --  Only supports CE
               Add_Left (Buffer, Era_Name (Locale, CE));
            when 'h' =>
               if Hours > 12 then
                  Accumulate_Left (Buffer, Hours rem 12, Locale);
               else
                  Accumulate_Left (Buffer, Hours, Locale);
               end if;
            when 'H' =>      --  HH
               if Starts_With (Format_String, Position, "H") then
                  Accumulate_Left (Buffer, Hours, Locale, Width => 2);
                  Position := Position + 1;
               else
                  Accumulate_Left (Buffer, Hours, Locale);
               end if;
            when 'm' =>      --  mm
               if Starts_With (Format_String, Position, "m") then
                  Accumulate_Left (Buffer, Minutes, Locale, Width => 2);
                  Position := Position + 1;
               else
                  Accumulate_Left (Buffer, Minutes, Locale);
               end if;
            when 'M' =>      --  MM MMM MMMM
               if Starts_With (Format_String, Position, "MMM") then
                  Add_Left (Buffer,
                            Full_Month_Name (Locale, To_Month (Month)));
                  Position := Position + 3;
               elsif Starts_With (Format_String, Position, "MM") then
                  Add_Left (Buffer,
                            Short_Month_Name (Locale, To_Month (Month)));
                  Position := Position + 2;
               elsif Starts_With (Format_String, Position, "M") then
                  Accumulate_Left (Buffer, Integer (Month), Locale,
                                   Width => 2);
                  Position := Position + 1;
               else
                  Accumulate_Left (Buffer, Integer (Month), Locale);
               end if;
            when 's' =>      --  ss
               if Starts_With (Format_String, Position, "s") then
                  Accumulate_Left (Buffer, Seconds, Locale, Width => 2);
                  Position := Position + 1;
               else
                  Accumulate_Left (Buffer, Seconds, Locale);
               end if;
            when 'y' =>      --  yy yyyy
               if Starts_With (Format_String, Position, "yyy") then
                  Accumulate_Left (Buffer, Integer (Year), Locale, Width => 4);
                  Position := Position + 3;
               elsif Starts_With (Format_String, Position, "y") then
                  Accumulate_Left (Buffer, Integer (Year) rem 100, Locale,
                                   Width => 2);
                  Position := Position + 1;
               else
                  Accumulate_Left (Buffer, Integer (Year), Locale);
               end if;
            when 'z' =>      --  zzzz
               if Starts_With (Format_String, Position, "zzz") then
                  --  'z' and 'zzzz' are the same here.
                  Position := Position + 3;
               end if;
               if TZ_Offset < 0 then
                  Add_Left (Buffer, '-');
               else
                  Add_Left (Buffer, '+');
               end if;
               Accumulate_Left (Buffer,
                                (abs Integer (TZ_Offset)) / 60,
                                Locale,
                                Width => 2);
               Accumulate_Left (Buffer,
                                (abs Integer (TZ_Offset)) mod 60,
                                Locale,
                                Width => 2);
            when ''' =>
               Quoted := True;
            when others =>
               Add_Left (Buffer, Ch);
            end case;
         end if;
      end loop;
      return Left (Buffer);
   end Apply_Format;

   ------------
   -- Create --
   ------------

   function Create (Time_Value : Time) return Time_Argument is
   begin
      return Create (Time_Value, UTC_Time_Offset (Time_Value));
   exception
   when Unknown_Zone_Error =>
      return Create (Time_Value, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Time_Value : Time;
                    TZ_Offset  : Time_Offset) return Time_Argument is
   begin
      return Time_Argument'(Data => Time_Value,
                            TZ_Offset => TZ_Offset);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Value    : Time_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is

      T : constant Time := Value.Data;
      Z : constant Time_Offset := Value.TZ_Offset;

   begin
      if Template = "time,full" then
         return Apply_Format (Time_Format (Locale, Full), T, Z, Locale);
      elsif Template = "time,long" then
         return Apply_Format (Time_Format (Locale, Long), T, Z, Locale);
      elsif Template = "time,medium" then
         return Apply_Format (Time_Format (Locale, Medium), T, Z, Locale);
      elsif Template = "time,short" or Template = "time" then
         return Apply_Format (Time_Format (Locale, Short), T, Z, Locale);
      elsif Template = "date,full" then
         return Apply_Format (Date_Format (Locale, Full), T, Z, Locale);
      elsif Template = "date,long" then
         return Apply_Format (Date_Format (Locale, Long), T, Z, Locale);
      elsif Template = "date,medium" then
         return Apply_Format (Date_Format (Locale, Medium), T, Z, Locale);
      elsif Template = "date,short" or Template = "date" then
         return Apply_Format (Date_Format (Locale, Short), T, Z, Locale);
      elsif Template = "full" then
         return Format (Date_Time_Format (Locale, Full),
                        +Apply_Format (Date_Format (Locale, Full),
                                       T, Z, Locale),
                        +Apply_Format (Time_Format (Locale, Full),
                                       T, Z, Locale));
      elsif Template = "long" then
         return Format (Date_Time_Format (Locale, Long),
                        +Apply_Format (Date_Format (Locale, Long),
                                       T, Z, Locale),
                        +Apply_Format (Time_Format (Locale, Long),
                                       T, Z, Locale));
      elsif Template = "medium" then
         return Format (Date_Time_Format (Locale, Medium),
                        +Apply_Format (Date_Format (Locale, Medium),
                                       T, Z, Locale),
                        +Apply_Format (Time_Format (Locale, Medium),
                                       T, Z, Locale));
      elsif Template = "short" or Template'Length = 0 then
         return Format (Date_Time_Format (Locale, Short),
                        +Apply_Format (Date_Format (Locale, Short),
                                       T, Z, Locale),
                        +Apply_Format (Time_Format (Locale, Short),
                                       T, Z, Locale));
      else
         return Apply_Format (Template, T, Z, Locale);
      end if;
   end Format;

   --------------
   -- To_Month --
   --------------

   function To_Month (Number : Month_Number) return Month_Type is
   begin
      return Month_Type'Val (Month_Type'Pos (Jan) + Number - 1);
   end To_Month;

end ZanyBlue.Text.Times;
