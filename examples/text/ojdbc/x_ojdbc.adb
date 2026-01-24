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
--  Example of the use of the ZanyBlue.Text_IO packages based on the
--  Oracle JDBC messages (from ojdbc.jar).
--

pragma License (Modified_GPL);

with Ada.Command_Line;
with Oracle.Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Formatting;

procedure X_OJDBC is

   use ZanyBlue.Text.Formatting;

   Usage_Error : exception;

   procedure Process_Command_Line;

   procedure Process_Command_Line is
      use Ada.Command_Line;
      use ZanyBlue.Text.Pseudo;
      use ZanyBlue.Text.Locales;
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

begin
   Process_Command_Line;
   Oracle.Messages.Initialize;
   Print_Line ("ojdbc", "DiagnosibilityMBeanConstructor()");
   Print_Line ("ojdbc", "DiagnosibilityMBeanDescription");
   Print_Line ("ojdbc", "DiagnosibilityMBeanLoggingEnabledDescription");
   Print_Line ("ojdbc", "DiagnosibilityMBeanStateManageableDescription");
   Print_Line ("ojdbc", "DiagnosibilityMBeanStatisticsProviderDescription");
   Print_Line ("ojdbc", "ORA-17001");
   Print_Line ("ojdbc", "ORA-17002");
   Print_Line ("ojdbc", "ORA-17003");
   Print_Line ("ojdbc", "ORA-17004");
   Print_Line ("ojdbc", "ORA-17005");
   Print_Line ("ojdbc", "ORA-17006");
   Print_Line ("ojdbc", "ORA-17007");
   Print_Line ("ojdbc", "ORA-17008");
   Print_Line ("ojdbc", "ORA-17009");
   Print_Line ("ojdbc", "ORA-17010");
   Print_Line ("ojdbc", "ORA-17011");
   Print_Line ("ojdbc", "ORA-17012");
   Print_Line ("ojdbc", "ORA-17014");
   Print_Line ("ojdbc", "ORA-17015");
   Print_Line ("ojdbc", "ORA-17016");
   Print_Line ("ojdbc", "ORA-17017");
   Print_Line ("ojdbc", "ORA-17018");
   Print_Line ("ojdbc", "ORA-17019");
   Print_Line ("ojdbc", "ORA-17020");
   Print_Line ("ojdbc", "ORA-17021");
   Print_Line ("ojdbc", "ORA-17022");
   Print_Line ("ojdbc", "ORA-17023");
   Print_Line ("ojdbc", "ORA-17024");
   Print_Line ("ojdbc", "ORA-17025");
   Print_Line ("ojdbc", "ORA-17026");
   Print_Line ("ojdbc", "ORA-17027");
   Print_Line ("ojdbc", "ORA-17028");
   Print_Line ("ojdbc", "ORA-17029");
   Print_Line ("ojdbc", "ORA-17030");
   Print_Line ("ojdbc", "ORA-17031");
   Print_Line ("ojdbc", "ORA-17032");
   Print_Line ("ojdbc", "ORA-17033");
   Print_Line ("ojdbc", "ORA-17034");
   Print_Line ("ojdbc", "ORA-17035");
   Print_Line ("ojdbc", "ORA-17036");
   Print_Line ("ojdbc", "ORA-17037");
   Print_Line ("ojdbc", "ORA-17038");
   Print_Line ("ojdbc", "ORA-17039");
   Print_Line ("ojdbc", "ORA-17040");
   Print_Line ("ojdbc", "ORA-17041");
   Print_Line ("ojdbc", "ORA-17042");
   Print_Line ("ojdbc", "ORA-17043");
   Print_Line ("ojdbc", "ORA-17044");
   Print_Line ("ojdbc", "ORA-17045");
   Print_Line ("ojdbc", "ORA-17046");
   Print_Line ("ojdbc", "ORA-17047");
   Print_Line ("ojdbc", "ORA-17048");
   Print_Line ("ojdbc", "ORA-17049");
   Print_Line ("ojdbc", "ORA-17050");
   Print_Line ("ojdbc", "ORA-17051");
   Print_Line ("ojdbc", "ORA-17052");
   Print_Line ("ojdbc", "ORA-17053");
   Print_Line ("ojdbc", "ORA-17054");
   Print_Line ("ojdbc", "ORA-17055");
   Print_Line ("ojdbc", "ORA-17056");
   Print_Line ("ojdbc", "ORA-17057");
   Print_Line ("ojdbc", "ORA-17058");
   Print_Line ("ojdbc", "ORA-17059");
   Print_Line ("ojdbc", "ORA-17060");
   Print_Line ("ojdbc", "ORA-17061");
   Print_Line ("ojdbc", "ORA-17062");
   Print_Line ("ojdbc", "ORA-17063");
   Print_Line ("ojdbc", "ORA-17064");
   Print_Line ("ojdbc", "ORA-17065");
   Print_Line ("ojdbc", "ORA-17066");
   Print_Line ("ojdbc", "ORA-17067");
   Print_Line ("ojdbc", "ORA-17068");
   Print_Line ("ojdbc", "ORA-17069");
   Print_Line ("ojdbc", "ORA-17070");
   Print_Line ("ojdbc", "ORA-17071");
   Print_Line ("ojdbc", "ORA-17072");
   Print_Line ("ojdbc", "ORA-17074");
   Print_Line ("ojdbc", "ORA-17075");
   Print_Line ("ojdbc", "ORA-17076");
   Print_Line ("ojdbc", "ORA-17077");
   Print_Line ("ojdbc", "ORA-17078");
   Print_Line ("ojdbc", "ORA-17079");
   Print_Line ("ojdbc", "ORA-17080");
   Print_Line ("ojdbc", "ORA-17081");
   Print_Line ("ojdbc", "ORA-17082");
   Print_Line ("ojdbc", "ORA-17083");
   Print_Line ("ojdbc", "ORA-17084");
   Print_Line ("ojdbc", "ORA-17085");
   Print_Line ("ojdbc", "ORA-17086");
   Print_Line ("ojdbc", "ORA-17087");
   Print_Line ("ojdbc", "ORA-17088");
   Print_Line ("ojdbc", "ORA-17089");
   Print_Line ("ojdbc", "ORA-17090");
   Print_Line ("ojdbc", "ORA-17091");
   Print_Line ("ojdbc", "ORA-17092");
   Print_Line ("ojdbc", "ORA-17093");
   Print_Line ("ojdbc", "ORA-17094");
   Print_Line ("ojdbc", "ORA-17095");
   Print_Line ("ojdbc", "ORA-17096");
   Print_Line ("ojdbc", "ORA-17097");
   Print_Line ("ojdbc", "ORA-17098");
   Print_Line ("ojdbc", "ORA-17099");
   Print_Line ("ojdbc", "ORA-17100");
   Print_Line ("ojdbc", "ORA-17101");
   Print_Line ("ojdbc", "ORA-17102");
   Print_Line ("ojdbc", "ORA-17103");
   Print_Line ("ojdbc", "ORA-17104");
   Print_Line ("ojdbc", "ORA-17105");
   Print_Line ("ojdbc", "ORA-17106");
   Print_Line ("ojdbc", "ORA-17107");
   Print_Line ("ojdbc", "ORA-17108");
   Print_Line ("ojdbc", "ORA-17109");
   Print_Line ("ojdbc", "ORA-17110");
   Print_Line ("ojdbc", "ORA-17111");
   Print_Line ("ojdbc", "ORA-17112");
   Print_Line ("ojdbc", "ORA-17113");
   Print_Line ("ojdbc", "ORA-17114");
   Print_Line ("ojdbc", "ORA-17115");
   Print_Line ("ojdbc", "ORA-17116");
   Print_Line ("ojdbc", "ORA-17117");
   Print_Line ("ojdbc", "ORA-17118");
   Print_Line ("ojdbc", "ORA-17119");
   Print_Line ("ojdbc", "ORA-17120");
   Print_Line ("ojdbc", "ORA-17121");
   Print_Line ("ojdbc", "ORA-17122");
   Print_Line ("ojdbc", "ORA-17123");
   Print_Line ("ojdbc", "ORA-17124");
   Print_Line ("ojdbc", "ORA-17125");
   Print_Line ("ojdbc", "ORA-17126");
   Print_Line ("ojdbc", "ORA-17127");
   Print_Line ("ojdbc", "ORA-17128");
   Print_Line ("ojdbc", "ORA-17129");
   Print_Line ("ojdbc", "ORA-17132");
   Print_Line ("ojdbc", "ORA-17133");
   Print_Line ("ojdbc", "ORA-17134");
   Print_Line ("ojdbc", "ORA-17135");
   Print_Line ("ojdbc", "ORA-17136");
   Print_Line ("ojdbc", "ORA-17137");
   Print_Line ("ojdbc", "ORA-17138");
   Print_Line ("ojdbc", "ORA-17139");
   Print_Line ("ojdbc", "ORA-17140");
   Print_Line ("ojdbc", "ORA-17141");
   Print_Line ("ojdbc", "ORA-17142");
   Print_Line ("ojdbc", "ORA-17143");
   Print_Line ("ojdbc", "ORA-17144");
   Print_Line ("ojdbc", "ORA-17145");
   Print_Line ("ojdbc", "ORA-17146");
   Print_Line ("ojdbc", "ORA-17147");
   Print_Line ("ojdbc", "ORA-17148");
   Print_Line ("ojdbc", "ORA-17149");
   Print_Line ("ojdbc", "ORA-17150");
   Print_Line ("ojdbc", "ORA-17151");
   Print_Line ("ojdbc", "ORA-17152");
   Print_Line ("ojdbc", "ORA-17153");
   Print_Line ("ojdbc", "ORA-17154");
   Print_Line ("ojdbc", "ORA-17155");
   Print_Line ("ojdbc", "ORA-17156");
   Print_Line ("ojdbc", "ORA-17157");
   Print_Line ("ojdbc", "ORA-17158");
   Print_Line ("ojdbc", "ORA-17159");
   Print_Line ("ojdbc", "ORA-17160");
   Print_Line ("ojdbc", "ORA-17161");
   Print_Line ("ojdbc", "ORA-17162");
   Print_Line ("ojdbc", "ORA-17163");
   Print_Line ("ojdbc", "ORA-17164");
   Print_Line ("ojdbc", "ORA-17165");
   Print_Line ("ojdbc", "ORA-17166");
   Print_Line ("ojdbc", "ORA-17167");
   Print_Line ("ojdbc", "ORA-17168");
   Print_Line ("ojdbc", "ORA-17169");
   Print_Line ("ojdbc", "ORA-17170");
   Print_Line ("ojdbc", "ORA-17171");
   Print_Line ("ojdbc", "ORA-17172");
   Print_Line ("ojdbc", "ORA-17173");
   Print_Line ("ojdbc", "ORA-17174");
   Print_Line ("ojdbc", "ORA-17175");
   Print_Line ("ojdbc", "ORA-17176");
   Print_Line ("ojdbc", "ORA-17177");
   Print_Line ("ojdbc", "ORA-17178");
   Print_Line ("ojdbc", "ORA-17179");
   Print_Line ("ojdbc", "ORA-17180");
   Print_Line ("ojdbc", "ORA-17181");
   Print_Line ("ojdbc", "ORA-17182");
   Print_Line ("ojdbc", "ORA-17183");
   Print_Line ("ojdbc", "ORA-17184");
   Print_Line ("ojdbc", "ORA-17185");
   Print_Line ("ojdbc", "ORA-17186");
   Print_Line ("ojdbc", "ORA-17187");
   Print_Line ("ojdbc", "ORA-17188");
   Print_Line ("ojdbc", "ORA-17189");
   Print_Line ("ojdbc", "ORA-17190");
   Print_Line ("ojdbc", "ORA-17191");
   Print_Line ("ojdbc", "ORA-17192");
   Print_Line ("ojdbc", "ORA-17193");
   Print_Line ("ojdbc", "ORA-17194");
   Print_Line ("ojdbc", "ORA-17195");
   Print_Line ("ojdbc", "ORA-17196");
   Print_Line ("ojdbc", "ORA-17197");
   Print_Line ("ojdbc", "ORA-17198");
   Print_Line ("ojdbc", "ORA-17199");
   Print_Line ("ojdbc", "ORA-17200");
   Print_Line ("ojdbc", "ORA-17201");
   Print_Line ("ojdbc", "ORA-17202");
   Print_Line ("ojdbc", "ORA-17203");
   Print_Line ("ojdbc", "ORA-17204");
   Print_Line ("ojdbc", "ORA-17205");
   Print_Line ("ojdbc", "ORA-17206");
   Print_Line ("ojdbc", "ORA-17207");
   Print_Line ("ojdbc", "ORA-17213");
   Print_Line ("ojdbc", "ORA-17215");
   Print_Line ("ojdbc", "ORA-17216");
   Print_Line ("ojdbc", "ORA-17233");
   Print_Line ("ojdbc", "ORA-17235");
   Print_Line ("ojdbc", "ORA-17236");
   Print_Line ("ojdbc", "ORA-17240");
   Print_Line ("ojdbc", "ORA-17241");
   Print_Line ("ojdbc", "ORA-17242");
   Print_Line ("ojdbc", "ORA-17243");
   Print_Line ("ojdbc", "ORA-17244");
   Print_Line ("ojdbc", "ORA-17245");
   Print_Line ("ojdbc", "ORA-17246");
   Print_Line ("ojdbc", "ORA-17247");
   Print_Line ("ojdbc", "ORA-17248");
   Print_Line ("ojdbc", "ORA-17249");
   Print_Line ("ojdbc", "ORA-17250");
   Print_Line ("ojdbc", "ORA-17251");
   Print_Line ("ojdbc", "ORA-17252");
   Print_Line ("ojdbc", "ORA-17253");
   Print_Line ("ojdbc", "ORA-17254");
   Print_Line ("ojdbc", "ORA-17255");
   Print_Line ("ojdbc", "ORA-17256");
   Print_Line ("ojdbc", "ORA-17300");
   Print_Line ("ojdbc", "ORA-17301");
   Print_Line ("ojdbc", "ORA-17302");
   Print_Line ("ojdbc", "ORA-17303");
   Print_Line ("ojdbc", "ORA-17304");
   Print_Line ("ojdbc", "ORA-17305");
   Print_Line ("ojdbc", "ORA-17306");
   Print_Line ("ojdbc", "ORA-17307");
   Print_Line ("ojdbc", "ORA-17308");
   Print_Line ("ojdbc", "ORA-17309");
   Print_Line ("ojdbc", "ORA-17310");
   Print_Line ("ojdbc", "ORA-17311");
   Print_Line ("ojdbc", "ORA-17312");
   Print_Line ("ojdbc", "ORA-17313");
   Print_Line ("ojdbc", "ORA-17314");
   Print_Line ("ojdbc", "ORA-17315");
   Print_Line ("ojdbc", "ORA-17316");
   Print_Line ("ojdbc", "ORA-17317");
   Print_Line ("ojdbc", "ORA-17318");
   Print_Line ("ojdbc", "ORA-17319");
   Print_Line ("ojdbc", "ORA-17320");
   Print_Line ("ojdbc", "ORA-17321");
   Print_Line ("ojdbc", "ORA-17322");
   Print_Line ("ojdbc", "ORA-17323");
   Print_Line ("ojdbc", "ORA-17324");
   Print_Line ("ojdbc", "ORA-17325");
   Print_Line ("ojdbc", "ORA-17326");
   Print_Line ("ojdbc", "ORA-17327");
   Print_Line ("ojdbc", "ORA-17328");
   Print_Line ("ojdbc", "ORA-17329");
   Print_Line ("ojdbc", "ORA-17330");
   Print_Line ("ojdbc", "ORA-17331");
   Print_Line ("ojdbc", "ORA-17332");
   Print_Line ("ojdbc", "ORA-17333");
   Print_Line ("ojdbc", "ORA-17334");
   Print_Line ("ojdbc", "ORA-17335");
   Print_Line ("ojdbc", "ORA-17336");
   Print_Line ("ojdbc", "ORA-17337");
   Print_Line ("ojdbc", "ORA-17338");
   Print_Line ("ojdbc", "ORA-17339");
   Print_Line ("ojdbc", "ORA-17340");
   Print_Line ("ojdbc", "ORA-17341");
   Print_Line ("ojdbc", "ORA-17342");
   Print_Line ("ojdbc", "ORA-17343");
   Print_Line ("ojdbc", "ORA-17344");
   Print_Line ("ojdbc", "ORA-17345");
   Print_Line ("ojdbc", "ORA-17346");
   Print_Line ("ojdbc", "ORA-17347");
   Print_Line ("ojdbc", "ORA-17348");
   Print_Line ("ojdbc", "ORA-17349");
   Print_Line ("ojdbc", "ORA-17350");
   Print_Line ("ojdbc", "ORA-17351");
   Print_Line ("ojdbc", "ORA-17352");
   Print_Line ("ojdbc", "ORA-17353");
   Print_Line ("ojdbc", "ORA-17354");
   Print_Line ("ojdbc", "ORA-17355");
   Print_Line ("ojdbc", "ORA-17356");
   Print_Line ("ojdbc", "ORA-17357");
   Print_Line ("ojdbc", "ORA-17358");
   Print_Line ("ojdbc", "ORA-17359");
   Print_Line ("ojdbc", "ORA-17401");
   Print_Line ("ojdbc", "ORA-17402");
   Print_Line ("ojdbc", "ORA-17403");
   Print_Line ("ojdbc", "ORA-17404");
   Print_Line ("ojdbc", "ORA-17405");
   Print_Line ("ojdbc", "ORA-17406");
   Print_Line ("ojdbc", "ORA-17407");
   Print_Line ("ojdbc", "ORA-17408");
   Print_Line ("ojdbc", "ORA-17409");
   Print_Line ("ojdbc", "ORA-17410");
   Print_Line ("ojdbc", "ORA-17411");
   Print_Line ("ojdbc", "ORA-17412");
   Print_Line ("ojdbc", "ORA-17413");
   Print_Line ("ojdbc", "ORA-17414");
   Print_Line ("ojdbc", "ORA-17415");
   Print_Line ("ojdbc", "ORA-17416");
   Print_Line ("ojdbc", "ORA-17417");
   Print_Line ("ojdbc", "ORA-17418");
   Print_Line ("ojdbc", "ORA-17419");
   Print_Line ("ojdbc", "ORA-17420");
   Print_Line ("ojdbc", "ORA-17421");
   Print_Line ("ojdbc", "ORA-17422");
   Print_Line ("ojdbc", "ORA-17423");
   Print_Line ("ojdbc", "ORA-17424");
   Print_Line ("ojdbc", "ORA-17425");
   Print_Line ("ojdbc", "ORA-17426");
   Print_Line ("ojdbc", "ORA-17427");
   Print_Line ("ojdbc", "ORA-17428");
   Print_Line ("ojdbc", "ORA-17429");
   Print_Line ("ojdbc", "ORA-17430");
   Print_Line ("ojdbc", "ORA-17431");
   Print_Line ("ojdbc", "ORA-17432");
   Print_Line ("ojdbc", "ORA-17433");
   Print_Line ("ojdbc", "ORA-17434");
   Print_Line ("ojdbc", "ORA-17435");
   Print_Line ("ojdbc", "ORA-17436");
   Print_Line ("ojdbc", "ORA-17437");
   Print_Line ("ojdbc", "ORA-17438");
   Print_Line ("ojdbc", "ORA-17439");
   Print_Line ("ojdbc", "ORA-17440");
   Print_Line ("ojdbc", "ORA-17441");
   Print_Line ("ojdbc", "ORA-17442");
   Print_Line ("ojdbc", "ORA-17444");
   Print_Line ("ojdbc", "ORA-17445");
   Print_Line ("ojdbc", "ORA-17446");
   Print_Line ("ojdbc", "ORA-17447");
   Print_Line ("ojdbc", "ORA-17448");
end X_OJDBC;
