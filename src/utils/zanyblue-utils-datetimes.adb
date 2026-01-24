--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2018, Michael Rohan <mrohan@zanyblue.com>
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

package body ZanyBlue.Utils.DateTimes is

   function Determine_Day_In_Week (Self : in out DateTime_Type)
      return Day_Type;
   procedure Make_HMS (Self : in out DateTime_Type);
   procedure Split_Time (Self : in out DateTime_Type);

   ---------
   -- Day --
   ---------

   function Day (Self : in out DateTime_Type) return Day_Number is
   begin
      if not Self.Have_Split then
         Split_Time (Self);
      end if;
      return Self.Day_Value;
   end Day;

   -----------------
   -- Day_In_Week --
   -----------------

   function Day_In_Week (Self : in out DateTime_Type) return Day_Type is
   begin
      if not Self.Have_Day_In_Week then
         Self.Day_In_Week_Value := Determine_Day_In_Week (Self);
         Self.Have_Day_In_Week := True;
      end if;
      return Self.Day_In_Week_Value;
   end Day_In_Week;

   --------------------------
   -- Day_Of_Week_In_Month --
   --------------------------
   --
   --  TODO: Implement!
   --

   function Day_Of_Week_In_Month (Self : in out DateTime_Type)
      return Day_Of_Week_In_Month_Type
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Day_Of_Week_In_Month;

   -----------------
   -- Day_Of_Year --
   -----------------
   --
   --  Calculate the number of days since Jan 1.
   --

   function Day_Of_Year (Self : in out DateTime_Type) return Day_Of_Year_Type
   is
   begin
      if not Self.Have_Day_Of_Year then
         declare
            Jan1_Date : constant Time := Time_Of (Self.Year, 1, 1);
            S_In_Year : constant Float := Float (Self.Time_Value - Jan1_Date);
            Days_Float : constant Float := S_In_Year / (24.0 * 3600.0);
         begin
            Self.Day_Of_Year_Value := Integer (Float'Floor (Days_Float)) + 1;
         end;
      end if;
      return Self.Day_Of_Year_Value;
   end Day_Of_Year;

   ---------------------------
   -- Determine_Day_In_Week --
   ---------------------------
   --
   --  The following algorithm is taken from the Wikipedia entry for
   --  calculating the day of the week:
   --
   --      http://en.wikipedia.org/wiki/Calculating_the_day_of_the_week
   --

   function Determine_Day_In_Week (Self : in out DateTime_Type)
      return Day_Type
   is

      subtype Leap_Year_P is Boolean;
      subtype Day_Number_Type is Natural range 0 .. 6;
      type Month_Map_Type is
         array (Month_Number, Leap_Year_P) of Day_Number_Type;
      type Num_To_Day_Type is array (Day_Number_Type) of Day_Type;
      Month_Map : constant Month_Map_Type :=
                  (1  => (True => 6, False => 0),
                   2  => (True => 2, False => 3),
                   3  => (others => 3),
                   4  => (others => 6),
                   5  => (others => 1),
                   6  => (others => 4),
                   7  => (others => 6),
                   8  => (others => 2),
                   9  => (others => 5),
                   10 => (others => 0),
                   11 => (others => 3),
                   12 => (others => 5));
      Num_To_Day : constant Num_To_Day_Type :=
                   (0 => Sun,
                    1 => Mon,
                    2 => Tue,
                    3 => Wed,
                    4 => Thu,
                    5 => Fri,
                    6 => Sat);
      Is_Leap       : constant Leap_Year_P :=
                         (Self.Year rem 4 = 0 and then Self.Year rem 100 /= 0)
                         or else Self.Year rem 400 = 0;
      CC   : constant Day_Number_Type := 2 * (3 - ((Self.Year / 100) mod 4));
      YY   : constant Natural         := Self.Year mod 100;
      YY_4 : constant Natural         := YY / 4;
      MM   : constant Day_Number_Type := Month_Map (Self.Month, Is_Leap);

   begin
      return Num_To_Day ((CC + YY + YY_4 + MM + Natural (Self.Day)) mod 7);
   end Determine_Day_In_Week;

   ----------------------
   -- Fraction_Seconds --
   ----------------------

   function Fraction_Seconds (Self : in out DateTime_Type) return Float is
   begin
      if not Self.Have_HMS then
         Make_HMS (Self);
      end if;
      return Self.Fraction_Seconds_Value;
   end Fraction_Seconds;

   ----------
   -- Hour --
   ----------

   function Hour (Self : in out DateTime_Type) return Hour_Type is
   begin
      if not Self.Have_HMS then
         Make_HMS (Self);
      end if;
      return Self.Hour_Value;
   end Hour;

   -------------
   -- Is_Noon --
   -------------

   function Is_Noon (Self : in out DateTime_Type) return Boolean is
      Hour   : constant Hour_Type := Self.Hour;
      Minute : constant Minute_Type := Self.Minute;
      Second : constant Second_Type := Self.Second;
   begin
      return Hour = 12 and Minute = 0 and Second = 0;
   end Is_Noon;

   -------------------
   -- Make_DateTime --
   -------------------

   function Make_DateTime (Time_Value : Time) return DateTime_Type is
   begin
      return Result : DateTime_Type do
         Result.Time_Value := Time_Value;
      end return;
   end Make_DateTime;

   --------------
   -- Make_HMS --
   --------------

   procedure Make_HMS (Self : in out DateTime_Type) is
      Seconds_Duration : constant Day_Duration := Seconds (Self);
      Sec_Float : constant Float := Float (Seconds_Duration);
      Sec_Int : constant Integer := Integer (Seconds_Duration);
   begin
      Self.Fraction_Seconds_Value := Sec_Float - Float'Floor (Sec_Float);
      Self.Second_Value := Sec_Int rem 60;
      Self.Minute_Value := (Sec_Int / 60) rem 60;
      Self.Hour_Value := Sec_Int / 3600;
      Self.Have_HMS := True;
   end Make_HMS;

   ------------
   -- Minute --
   ------------

   function Minute (Self : in out DateTime_Type) return Minute_Type is
   begin
      if not Self.Have_HMS then
         Make_HMS (Self);
      end if;
      return Self.Minute_Value;
   end Minute;

   -------------------------
   -- Modified_Julian_Day --
   -------------------------
   --
   --  TODO: Implement!

   function Modified_Julian_Day (Self : in out DateTime_Type) return Natural is
      pragma Unreferenced (Self);
   begin
      return 2451334;
   end Modified_Julian_Day;

   -----------
   -- Month --
   -----------

   function Month (Self : in out DateTime_Type) return Month_Number is
   begin
      if not Self.Have_Split then
         Split_Time (Self);
      end if;
      return Self.Month_Value;
   end Month;

   -------------
   -- Quarter --
   -------------

   function Quarter (Self : in out DateTime_Type) return Quarter_Type is
   begin
      if not Self.Have_Quarter then
         declare
            Month_Value : constant Month_Number := Self.Month;
         begin
            if Month_Value <= 3 then
               Self.Quarter_Value := 1;
            elsif Month_Value <= 6 then
               Self.Quarter_Value := 2;
            elsif Month_Value <= 9 then
               Self.Quarter_Value := 3;
            else
               Self.Quarter_Value := 4;
            end if;
         end;
         Self.Have_Quarter := True;
      end if;
      return Self.Quarter_Value;
   end Quarter;

   ------------
   -- Second --
   ------------

   function Second (Self : in out DateTime_Type) return Second_Type is
   begin
      if not Self.Have_HMS then
         Make_HMS (Self);
      end if;
      return Self.Second_Value;
   end Second;

   -------------
   -- Seconds --
   -------------

   function Seconds (Self : in out DateTime_Type) return Day_Duration is
   begin
      if not Self.Have_Split then
         Split_Time (Self);
      end if;
      return Self.Seconds_Value;
   end Seconds;

   ----------------
   -- Split_Time --
   ----------------

   procedure Split_Time (Self : in out DateTime_Type) is
   begin
      Split (Self.Time_Value, Self.Year_Value, Self.Month_Value,
             Self.Day_Value, Self.Seconds_Value);
      Self.Have_Split := True;
   end Split_Time;

   -------------------
   -- Week_In_Month --
   -------------------
   --
   --  TODO: Calculate the week number!
   --

   function Week_In_Month (Self : in out DateTime_Type)
      return Week_In_Month_Type
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Week_In_Month;

   ------------------
   -- Week_In_Year --
   ------------------
   --
   --  TODO: Calculate the week number!
   --

   function Week_In_Year (Self : in out DateTime_Type)
      return Week_In_Year_Type
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Week_In_Year;

   ----------
   -- Year --
   ----------

   function Year (Self : in out DateTime_Type) return Year_Number is
   begin
      if not Self.Have_Split then
         Split_Time (Self);
      end if;
      return Self.Year_Value;
   end Year;

end ZanyBlue.Utils.DateTimes;
