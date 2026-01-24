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
--  Example of the use of the ZanyBlue.Text_IO packages based on the Java
--  Web Start strings.properties file distributed with the 1.6.0 JRE.
--

pragma License (Modified_GPL);

with Ada.Command_Line;
with Sun.ORB_Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Formatting;

procedure X_SunORB is

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
   --  We are not supplying arguments to the messages below, disable the
   --  exceptions, i.e., No_Such_Argument_Error
   Disable_Exceptions;
   Print_Line ("sunorb", "bootstrap.exception");
   Print_Line ("sunorb", "bootstrap.filenotfound");
   Print_Line ("sunorb", "bootstrap.filenotreadable");
   Print_Line ("sunorb", "bootstrap.success");
   Print_Line ("sunorb", "bootstrap.usage");
   Print_Line ("sunorb", "orbd.commfailure");
   Print_Line ("sunorb", "orbd.internalexception");
   Print_Line ("sunorb", "orbd.usage");
   Print_Line ("sunorb", "pnameserv.success");
   Print_Line ("sunorb", "servertool.appname");
   Print_Line ("sunorb", "servertool.args");
   Print_Line ("sunorb", "servertool.baddef");
   Print_Line ("sunorb", "servertool.banner");
   Print_Line ("sunorb", "servertool.classpath");
   Print_Line ("sunorb", "servertool.getserverid");
   Print_Line ("sunorb", "servertool.getserverid1");
   Print_Line ("sunorb", "servertool.getserverid2");
   Print_Line ("sunorb", "servertool.helddown");
   Print_Line ("sunorb", "servertool.help");
   Print_Line ("sunorb", "servertool.help1");
   Print_Line ("sunorb", "servertool.list");
   Print_Line ("sunorb", "servertool.list1");
   Print_Line ("sunorb", "servertool.list2");
   Print_Line ("sunorb", "servertool.listactive");
   Print_Line ("sunorb", "servertool.listactive1");
   Print_Line ("sunorb", "servertool.listappnames");
   Print_Line ("sunorb", "servertool.listappnames1");
   Print_Line ("sunorb", "servertool.listappnames2");
   Print_Line ("sunorb", "servertool.locate");
   Print_Line ("sunorb", "servertool.locate1");
   Print_Line ("sunorb", "servertool.locate2");
   Print_Line ("sunorb", "servertool.locateorb");
   Print_Line ("sunorb", "servertool.locateorb1");
   Print_Line ("sunorb", "servertool.locateorb2");
   Print_Line ("sunorb", "servertool.name");
   Print_Line ("sunorb", "servertool.nosuchorb");
   Print_Line ("sunorb", "servertool.nosuchserver");
   Print_Line ("sunorb", "servertool.orbidmap");
   Print_Line ("sunorb", "servertool.orbidmap1");
   Print_Line ("sunorb", "servertool.orbidmap2");
   Print_Line ("sunorb", "servertool.quit");
   Print_Line ("sunorb", "servertool.quit1");
   Print_Line ("sunorb", "servertool.register");
   Print_Line ("sunorb", "servertool.register1");
   Print_Line ("sunorb", "servertool.register2");
   Print_Line ("sunorb", "servertool.register3");
   Print_Line ("sunorb", "servertool.register4");
   Print_Line ("sunorb", "servertool.serverid");
   Print_Line ("sunorb", "servertool.servernotrunning");
   Print_Line ("sunorb", "servertool.serverup");
   Print_Line ("sunorb", "servertool.shorthelp");
   Print_Line ("sunorb", "servertool.shutdown");
   Print_Line ("sunorb", "servertool.shutdown1");
   Print_Line ("sunorb", "servertool.shutdown2");
   Print_Line ("sunorb", "servertool.startserver");
   Print_Line ("sunorb", "servertool.startserver1");
   Print_Line ("sunorb", "servertool.startserver2");
   Print_Line ("sunorb", "servertool.unregister");
   Print_Line ("sunorb", "servertool.unregister1");
   Print_Line ("sunorb", "servertool.unregister2");
   Print_Line ("sunorb", "servertool.usage");
   Print_Line ("sunorb", "servertool.vmargs");
   Print_Line ("sunorb", "tnameserv.exception");
   Print_Line ("sunorb", "tnameserv.hs1");
   Print_Line ("sunorb", "tnameserv.hs2");
   Print_Line ("sunorb", "tnameserv.hs3");
   Print_Line ("sunorb", "tnameserv.invalidhostoption");
   Print_Line ("sunorb", "tnameserv.orbinitialport0");
   Print_Line ("sunorb", "tnameserv.usage");
end X_SunORB;
