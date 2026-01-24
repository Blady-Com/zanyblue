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

with Ada.Calendar;

package ZanyBlue.Utils.DateTimes is

   use Ada.Calendar;

   subtype Hour_Type is Natural range 0 .. 23;
   --  Hour numbers for day periods.

   subtype Minute_Type is Natural range 0 .. 59;
   --  Minute numbers for day periods.

   subtype Second_Type is Natural range 0 .. 60;
   --  Second numbers for day periods (60 to account for leap seconds).

   subtype Quarter_Type is Positive range 1 .. 4;

   subtype Week_In_Year_Type is Positive range 1 .. 52;
   --  Week number within the year.

   subtype Week_In_Month_Type is Positive range 1 .. 4;
   --  Week number within the year.

   subtype Day_Of_Year_Type is Positive range 1 .. 366;
   --  Day number within a year.

   subtype Day_Of_Week_In_Month_Type is Positive range 1 .. 5;
   --  Day of Week in Month, e.g., 2nd Wed of July.

   type Day_Type is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   --  Days of the week.

   type Month_Type is (Jan, Feb, Mar, Apr, May, Jun,
                       Jul, Aug, Sep, Oct, Nov, Dec);
   --  Months of the year.

   type DateTime_Type is tagged private;
   --  Wrapper around standard Ada time to give deferred execution for
   --  attributes needed when formatting, e.g., don't always need to determine
   --  the day in the week.

   function Make_DateTime (Time_Value : Time) return DateTime_Type;
   --  Create a DateTime wrapper object.

   function Year (Self : in out DateTime_Type) return Year_Number;
   --  Return the year number associated with a wrapped object.

   function Month (Self : in out DateTime_Type) return Month_Number;
   --  Return the month number associated with a wrapped object.

   function Day (Self : in out DateTime_Type) return Day_Number;
   --  Return the day number associated with a wrapped object.

   function Day_Of_Year (Self : in out DateTime_Type) return Day_Of_Year_Type;
   --  Return the day number within the year associated with a wrapped object.

   function Modified_Julian_Day (Self : in out DateTime_Type) return Natural;
   --  Modified Julian day.

   function Week_In_Month (Self : in out DateTime_Type)
      return Week_In_Month_Type;
   --  Return the week within the month number associated with wrapped object.

   function Week_In_Year (Self : in out DateTime_Type)
      return Week_In_Year_Type;
   --  Return the week number associated with a wrapped object.

   function Hour (Self : in out DateTime_Type) return Hour_Type;
   --  Return the hour (H:M:S) associated with a wrapped object.

   function Minute (Self : in out DateTime_Type) return Minute_Type;
   --  Return the minute (H:M:S) associated with a wrapped object.

   function Second (Self : in out DateTime_Type) return Second_Type;
   --  Return the second (H:M:S) associated with a wrapped object.

   function Is_Noon (Self : in out DateTime_Type) return Boolean;
   --  Is the wrapper object at 12:00?

   function Seconds (Self : in out DateTime_Type) return Day_Duration;
   --  Return the duration within the day (as returned by Split).

   function Fraction_Seconds (Self : in out DateTime_Type) return Float;
   --  Return the fraction of seconds associated with the object.

   function Day_In_Week (Self : in out DateTime_Type) return Day_Type;
   --  Return the day in the week for the wrapper object (Sun, Mon, etc).

   function Day_Of_Week_In_Month (Self : in out DateTime_Type)
      return Day_Of_Week_In_Month_Type;
   --  Day of Week in Month, e.g., 2nd Wed of July.

   function Quarter (Self : in out DateTime_Type) return Quarter_Type;
   --  Return the quarter in the year for the wrapped object.

private

   type DateTime_Type is tagged
      record
         Time_Value             : Time;
         Have_Split             : Boolean := False;
         Year_Value             : Year_Number;
         Month_Value            : Month_Number;
         Day_Value              : Day_Number;
         Seconds_Value          : Day_Duration;
         Have_Day_In_Week       : Boolean := False;
         Day_In_Week_Value      : Day_Type;
         Have_HMS               : Boolean := False;
         Hour_Value             : Hour_Type;
         Minute_Value           : Minute_Type;
         Second_Value           : Second_Type;
         Fraction_Seconds_Value : Float;
         Have_Quarter           : Boolean := False;
         Quarter_Value          : Quarter_Type;
         Have_Day_Of_Year       : Boolean := False;
         Day_Of_Year_Value      : Day_Of_Year_Type;
      end record;

end ZanyBlue.Utils.DateTimes;
