--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

with ZanyBlue.Text.Times;
with ZanyBlue.Test.Text.Times.Lower_A.Suites;
with ZanyBlue.Test.Text.Times.Lower_B.Suites;
with ZanyBlue.Test.Text.Times.Lower_C.Suites;
with ZanyBlue.Test.Text.Times.Lower_E.Suites;
with ZanyBlue.Test.Text.Times.Lower_D.Suites;
with ZanyBlue.Test.Text.Times.Lower_H.Suites;
with ZanyBlue.Test.Text.Times.Lower_L.Suites;
with ZanyBlue.Test.Text.Times.Lower_M.Suites;
with ZanyBlue.Test.Text.Times.Lower_Q.Suites;
with ZanyBlue.Test.Text.Times.Lower_R.Suites;
with ZanyBlue.Test.Text.Times.Lower_S.Suites;
with ZanyBlue.Test.Text.Times.Lower_U.Suites;
with ZanyBlue.Test.Text.Times.Lower_Y.Suites;
with ZanyBlue.Test.Text.Times.Lower_Z.Suites;
with ZanyBlue.Test.Text.Times.Upper_A.Suites;
with ZanyBlue.Test.Text.Times.Upper_B.Suites;
with ZanyBlue.Test.Text.Times.Upper_D.Suites;
with ZanyBlue.Test.Text.Times.Upper_E.Suites;
with ZanyBlue.Test.Text.Times.Upper_G.Suites;
with ZanyBlue.Test.Text.Times.Upper_H.Suites;
with ZanyBlue.Test.Text.Times.Upper_L.Suites;
with ZanyBlue.Test.Text.Times.Upper_M.Suites;
with ZanyBlue.Test.Text.Times.Upper_O.Suites;
with ZanyBlue.Test.Text.Times.Upper_Q.Suites;
with ZanyBlue.Test.Text.Times.Upper_S.Suites;
with ZanyBlue.Test.Text.Times.Upper_Y.Suites;
with ZanyBlue.Test.Text.Times.Upper_Z.Suites;

package body ZanyBlue.Test.Text.Times.Suites is

   use Ahven.Framework;

   procedure T_0001 (T : in out Test_Case'Class);
   procedure T_0002 (T : in out Test_Case'Class);
   procedure T_0003 (T : in out Test_Case'Class);
   procedure T_0004 (T : in out Test_Case'Class);
   procedure T_0005 (T : in out Test_Case'Class);
   procedure T_0006 (T : in out Test_Case'Class);
   procedure T_0007 (T : in out Test_Case'Class);
   procedure T_0008 (T : in out Test_Case'Class);
   procedure T_0009 (T : in out Test_Case'Class);
   procedure T_0010 (T : in out Test_Case'Class);
   procedure T_0011 (T : in out Test_Case'Class);
   procedure T_0012 (T : in out Test_Case'Class);
   procedure T_0013 (T : in out Test_Case'Class);
   procedure T_0014 (T : in out Test_Case'Class);
   procedure T_0015 (T : in out Test_Case'Class);
   procedure T_0016 (T : in out Test_Case'Class);
   procedure T_0017 (T : in out Test_Case'Class);
   procedure T_0018 (T : in out Test_Case'Class);
   procedure T_0019 (T : in out Test_Case'Class);
   procedure T_0020 (T : in out Test_Case'Class);
   procedure T_0021 (T : in out Test_Case'Class);
   procedure T_0022 (T : in out Test_Case'Class);
   procedure T_0023 (T : in out Test_Case'Class);
   procedure T_0024 (T : in out Test_Case'Class);
   procedure T_0025 (T : in out Test_Case'Class);
   procedure T_0026 (T : in out Test_Case'Class);
   procedure T_0027 (T : in out Test_Case'Class);
   procedure T_0028 (T : in out Test_Case'Class);
   procedure T_0029 (T : in out Test_Case'Class);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Times");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Create/Format");
      Add_Test_Routine (T, T_0002'Access, "T_0002, +/Format");
      Add_Test_Routine (T, T_0003'Access, "T_0003, ja_JP: Bloomsday");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Argument_List/Format");
      Add_Test_Routine (T, T_0005'Access, "T_0005, Format - Time");
      Add_Test_Routine (T, T_0006'Access, "T_0006, List.Format - Time");
      Add_Test_Routine (T, T_0007'Access, "T_0007, Format - Date");
      Add_Test_Routine (T, T_0008'Access, "T_0008, List.Format - Date");
      Add_Test_Routine (T, T_0009'Access, "T_0009, zh_CN: general formats");

      Add_Test_Routine (T, T_0010'Access, "T_0010, en_IE: Bloomsday");
      Add_Test_Routine (T, T_0011'Access, "T_0011, fr_FR: Bloomsday");
      Add_Test_Routine (T, T_0012'Access, "T_0012, ru_RU: Bloomsday");
      Add_Test_Routine (T, T_0013'Access, "T_0013, el_GR: Bloomsday");
      Add_Test_Routine (T, T_0014'Access, "T_0014, he_IL: Bloomsday");
      Add_Test_Routine (T, T_0015'Access, "T_0015, ar_SA: Bloomsday");
      Add_Test_Routine (T, T_0016'Access, "T_0016, zh_CN: Bloomsday");
      Add_Test_Routine (T, T_0017'Access, "T_0017, ko_KR: Bloomsday");

      Add_Test_Routine (T, T_0018'Access, "T_0018, en_IE: Bloomsday, time");
      Add_Test_Routine (T, T_0019'Access, "T_0019, fr_FR: Bloomsday, time");
      Add_Test_Routine (T, T_0020'Access, "T_0020, en_IE: Bloomsday, date");
      Add_Test_Routine (T, T_0021'Access, "T_0021, fr_FR: Bloomsday, date");

      Add_Test_Routine (T, T_0022'Access, "T_0022, zh_Hans: Bloomsday");

      Add_Test_Routine (T, T_0023'Access, "T_0023, en: time with day period");
      Add_Test_Routine (T, T_0024'Access, "T_0024, de: time with day period");
      Add_Test_Routine (T, T_0025'Access, "T_0025, fr: time with day period");
      Add_Test_Routine (T, T_0026'Access, "T_0026, ja: time with day period");
      Add_Test_Routine (T, T_0027'Access, "T_0027, zh: time with day period");
      Add_Test_Routine (T, T_0028'Access, "T_0028, ar: time with day period");
      Add_Test_Routine (T, T_0029'Access, "T_0029, ko: time with day period");

   end Initialize;

   function Suite return Test_Suite is
   begin
      return S : Test_Suite do
         Add_Test (S, new Test);
         Add_Static_Test (S, Lower_A.Suites.Suite);
         Add_Static_Test (S, Lower_B.Suites.Suite);
         Add_Static_Test (S, Lower_C.Suites.Suite);
         Add_Static_Test (S, Lower_E.Suites.Suite);
         Add_Static_Test (S, Lower_D.Suites.Suite);
         Add_Static_Test (S, Lower_H.Suites.Suite);
         Add_Static_Test (S, Lower_L.Suites.Suite);
         Add_Static_Test (S, Lower_M.Suites.Suite);
         Add_Static_Test (S, Lower_Q.Suites.Suite);
         Add_Static_Test (S, Lower_R.Suites.Suite);
         Add_Static_Test (S, Lower_S.Suites.Suite);
         Add_Static_Test (S, Lower_U.Suites.Suite);
         Add_Static_Test (S, Lower_Y.Suites.Suite);
         Add_Static_Test (S, Lower_Z.Suites.Suite);
         Add_Static_Test (S, Upper_A.Suites.Suite);
         Add_Static_Test (S, Upper_B.Suites.Suite);
         Add_Static_Test (S, Upper_D.Suites.Suite);
         Add_Static_Test (S, Upper_E.Suites.Suite);
         Add_Static_Test (S, Upper_G.Suites.Suite);
         Add_Static_Test (S, Upper_H.Suites.Suite);
         Add_Static_Test (S, Upper_L.Suites.Suite);
         Add_Static_Test (S, Upper_M.Suites.Suite);
         Add_Static_Test (S, Upper_O.Suites.Suite);
         Add_Static_Test (S, Upper_Q.Suites.Suite);
         Add_Static_Test (S, Upper_S.Suites.Suite);
         Add_Static_Test (S, Upper_Y.Suites.Suite);
         Add_Static_Test (S, Upper_Z.Suites.Suite);
      end return;
   end Suite;

   procedure T_0001 (T : in out Test_Case'Class) is separate;
   procedure T_0002 (T : in out Test_Case'Class) is separate;
   procedure T_0003 (T : in out Test_Case'Class) is separate;
   procedure T_0004 (T : in out Test_Case'Class) is separate;
   procedure T_0005 (T : in out Test_Case'Class) is separate;
   procedure T_0006 (T : in out Test_Case'Class) is separate;
   procedure T_0007 (T : in out Test_Case'Class) is separate;
   procedure T_0008 (T : in out Test_Case'Class) is separate;
   procedure T_0009 (T : in out Test_Case'Class) is separate;
   procedure T_0010 (T : in out Test_Case'Class) is separate;
   procedure T_0011 (T : in out Test_Case'Class) is separate;
   procedure T_0012 (T : in out Test_Case'Class) is separate;
   procedure T_0013 (T : in out Test_Case'Class) is separate;
   procedure T_0014 (T : in out Test_Case'Class) is separate;
   procedure T_0015 (T : in out Test_Case'Class) is separate;
   procedure T_0016 (T : in out Test_Case'Class) is separate;
   procedure T_0017 (T : in out Test_Case'Class) is separate;
   procedure T_0018 (T : in out Test_Case'Class) is separate;
   procedure T_0019 (T : in out Test_Case'Class) is separate;
   procedure T_0020 (T : in out Test_Case'Class) is separate;
   procedure T_0021 (T : in out Test_Case'Class) is separate;
   procedure T_0022 (T : in out Test_Case'Class) is separate;
   procedure T_0023 (T : in out Test_Case'Class) is separate;
   procedure T_0024 (T : in out Test_Case'Class) is separate;
   procedure T_0025 (T : in out Test_Case'Class) is separate;
   procedure T_0026 (T : in out Test_Case'Class) is separate;
   procedure T_0027 (T : in out Test_Case'Class) is separate;
   procedure T_0028 (T : in out Test_Case'Class) is separate;
   procedure T_0029 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Times.Suites;
