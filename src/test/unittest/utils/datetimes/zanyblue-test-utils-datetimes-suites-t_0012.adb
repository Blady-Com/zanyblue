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
procedure T_0012 (T : in out Test_Case'Class) is

   use Ada.Calendar;

   DT_1 : DateTime_Type := Make_DateTime (Time_Of (2010, 1, 4, 43200.0));
   DT_2 : DateTime_Type := Make_DateTime (Time_Of (2010, 4, 8, 43810.0));
   DT_3 : DateTime_Type := Make_DateTime (Time_Of (2010, 7, 12, 43825.0));
   DT_4 : DateTime_Type := Make_DateTime (Time_Of (2010, 10, 16, 43855.0));

begin
   WAssert (T, DT_1.Second = 0, "Second = 0 failed");
   WAssert (T, DT_2.Second = 10, "Second = 10 failed");
   WAssert (T, DT_3.Second = 25, "Second = 25 failed");
   WAssert (T, DT_4.Second = 55, "Second = 55 failed");
end T_0012;
