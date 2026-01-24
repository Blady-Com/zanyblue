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

separate (ZanyBlue.Test.Utils.DateTimes.Suites)
procedure T_0015 (T : in out Test_Case'Class) is

   use Ada.Calendar;

   DT_1 : DateTime_Type := Make_DateTime (Time_Of (2018, 1, 1));
   DT_2 : DateTime_Type := Make_DateTime (Time_Of (2018, 1, 31));
   DT_3 : DateTime_Type := Make_DateTime (Time_Of (2018, 2, 1));
   DT_4 : DateTime_Type := Make_DateTime (Time_Of (2018, 12, 31));
   DT_5 : DateTime_Type := Make_DateTime (Time_Of (2018, 3, 1));
   DT_6 : DateTime_Type := Make_DateTime (Time_Of (2016, 3, 1));
   DT_7 : DateTime_Type := Make_DateTime (Time_Of (2016, 12, 31));

begin
   WAssert (T, DT_1.Day_Of_Year = 1, "Jan 1 is first day");
   WAssert (T, DT_2.Day_Of_Year = 31, "Jan 31 is 31st day");
   WAssert (T, DT_3.Day_Of_Year = 32, "Feb 1 is 32st day");
   WAssert (T, DT_4.Day_Of_Year = 365, "Dec 31 is 365th day (non-leap)");
   WAssert (T, DT_5.Day_Of_Year = 60, "Mar 1 is 60th day (non-leap)");
   WAssert (T, DT_6.Day_Of_Year = 61, "Mar 1 is 61st day (leap)");
   WAssert (T, DT_7.Day_Of_Year = 366, "Dec 31 is 366th day (leap)");
end T_0015;
