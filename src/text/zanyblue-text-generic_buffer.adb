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

package body ZanyBlue.Text.Generic_Buffer is

   procedure Accumulate_Left_Base (Buffer     : in out Buffer_Type;
                                   Value      : Integer_Type'Base;
                                   Locale     : Locale_Type;
                                   Width      : Natural);
   --  Add (accumulate) a numeric value to the left side of the buffer with
   --  given base.

   ---------------------
   -- Accumulate_Left --
   ---------------------

   procedure Accumulate_Left (Buffer     : in out Buffer_Type;
                              Value      : Integer_Type;
                              Locale     : Locale_Type;
                              Width      : Natural := 1) is
   begin
      Accumulate_Left_Base (Buffer, Integer_Type'Base (Value), Locale, Width);
   end Accumulate_Left;

   --------------------------
   -- Accumulate_Left_Base --
   --------------------------

   procedure Accumulate_Left_Base (Buffer     : in out Buffer_Type;
                                   Value      : Integer_Type'Base;
                                   Locale     : Locale_Type;
                                   Width      : Natural) is

      Digit_Map : constant Wide_String := Locale_Digits (Locale, True);
      Decrement : constant array (Boolean) of Natural :=
                     (True => 1, False => 0);
      Index     : Positive;

   begin
      if Value > 0 or Width > 0 then
         Accumulate_Left_Base (Buffer, Value / 10, Locale,
                               Width - Decrement (Width > 0));
         Index := Positive (Value rem 10 + 1);
         Add_Left (Buffer, Digit_Map (Index));
      end if;
   end Accumulate_Left_Base;

   ----------------------
   -- Accumulate_Right --
   ----------------------

   procedure Accumulate_Right (Buffer     : in out Buffer_Type;
                               Value      : in out Integer_Type;
                               Locale     : Locale_Type;
                               Width      : Natural := 0;
                               Scale      : Natural := 0;
                               Prefix     : Wide_Character := Null_Character;
                               Base       : Positive := 10;
                               Lowercase  : Boolean := True) is

      Digit_Map       : constant Wide_String := Locale_Digits (Locale,
                                                               Lowercase);
      Amount          : Integer_Type'Base := Value;
      Index           : Positive;
      Width_Remaining : Integer := Width;

   begin
      if Scale /= 0 then
         Amount := Amount rem Integer_Type'Base (Scale);
      end if;
      if Prefix /= Null_Character then
         Add_Right (Buffer, Prefix);
      end if;
      loop
         Index := Positive (abs (Amount rem Integer_Type'Base (Base)) + 1);
         Add_Right (Buffer, Digit_Map (Index));
         Amount := Amount / Integer_Type'Base (Base);
         Width_Remaining := Width_Remaining - 1;
         exit when Amount = 0 and Width_Remaining <= 0;
      end loop;
      if Scale /= 0 then
         Value := Value / Integer_Type'Base (Scale);
      end if;
   end Accumulate_Right;

   --------------
   -- Add_Left --
   --------------

   procedure Add_Left (Buffer : in out Buffer_Type;
                       Data   : Wide_Character) is
   begin
      if Buffer.Left_Index > Buffer.Right_Index then
         raise Overflow;
      end if;
      Buffer.Data (Buffer.Left_Index) := Data;
      Buffer.Left_Index := Buffer.Left_Index + 1;
   end Add_Left;

   --------------
   -- Add_Left --
   --------------

   procedure Add_Left (Buffer : in out Buffer_Type;
                       Data   : Wide_String) is
   begin
      for I in Data'Range loop
         Add_Left (Buffer, Data (I));
      end loop;
   end Add_Left;

   ---------------
   -- Add_Right --
   ---------------

   procedure Add_Right (Buffer : in out Buffer_Type;
                        Data   : Wide_Character) is
   begin
      if Buffer.Right_Index < Buffer.Left_Index then
         raise Overflow;
      end if;
      Buffer.Data (Buffer.Right_Index) := Data;
      Buffer.Right_Index := Buffer.Right_Index - 1;
   end Add_Right;

   ---------------
   -- Add_Right --
   ---------------

   procedure Add_Right (Buffer : in out Buffer_Type;
                        Data   : Wide_String) is
   begin
      for I in reverse Data'Range loop
         Add_Right (Buffer, Data (I));
      end loop;
   end Add_Right;

   ----------
   -- Left --
   ----------

   function Left (Buffer : Buffer_Type) return Wide_String is
   begin
      return Buffer.Data (1 .. Buffer.Left_Index - 1);
   end Left;

   -----------
   -- Right --
   -----------

   function Right (Buffer : Buffer_Type) return Wide_String is
   begin
      return Buffer.Data (Buffer.Right_Index + 1 .. Buffer.Size);
   end Right;

end ZanyBlue.Text.Generic_Buffer;
