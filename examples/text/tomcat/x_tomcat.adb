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
--  Example of the use of the ZanyBlue.Text.Formatting package based on the
--  Java .properties files in Apache Tomcat (rename from LocalStrings).
--

pragma License (Modified_GPL);

with Ada.Command_Line;
with Apache.Tomcat.Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Formatting;

procedure X_Tomcat is

   use ZanyBlue.Text.Formatting;

   procedure Process_Command_Line;

   Usage_Error : exception;

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

   Path : constant Wide_String := "../example.dat";
   Bean : constant Wide_String := "xmpl";
   Alias : constant Wide_String := "xyz";
   Server : constant Wide_String := "www.example.com";
   User : constant Wide_String := "mrohan";
   WCh : constant Wide_Character := '٤';
   Ex_Name : constant Wide_String := "NullPointerException";
   Th_Name : constant Wide_String := "WorkerThread";

begin
   Process_Command_Line;
   Print_Line ("ajp", "ajpprotocol.endpoint.pauseerror");
   Print_Line ("authenticator", "authenticator.notContext");
   Print_Line ("connector", "coyoteConnector.protocolHandlerResumeFailed");
   Print_Line ("ha-session", "deltaManager.createSession.ise");
   Print_Line ("ha-tcp", "IDataSender.senderModes.Resources");
   Print_Line ("host", "hostManagerServlet.cannotInvoke");
   Print_Line ("http11", "http11protocol.socketfactory.initerror");
   Print_Line ("http", "err.io.negativelength");
   Print_Line ("jasper", "jsp.message.dont.modify.servlets");
   Print_Line ("loader", "standardLoader.notContext");
   Print_Line ("manager", "htmlManagerServlet.deployServer");
   Print_Line ("naming", "contextBindings.noContextBoundToThread");
   Print_Line ("naming-res", "resources.notStarted");
   Print_Line ("security", "SecurityUtil.doAsPrivilege");
   Print_Line ("servlets", "invokerServlet.notNamed");
   Print_Line ("session", "JDBCStore.checkConnectionDBClosed");
   Print_Line ("startup", "contextConfig.applicationClose");
   Print_Line ("transport", "ReplicationValve.send.failure");
   Print_Line ("users", "memoryUserDatabase.readOnly");
   Print_Line ("util-buf", "hexUtil.bad");
   Print_Line ("util-http", "sc.203");
   Print_Line ("util-net", "endpoint.err.close");
   Print_Line ("util", "extensionValidator.web-application-manifest");
   Print_Line ("valves", "semaphoreValve.alreadyStarted");
   Print_Line ("webapps", "requestparams.title");

   Print_Line ("core", "applicationContext.resourcePaths.iae", +Path);
   Print_Line ("ha", "cluster.mbean.register.already", +Bean);
   Print_Line ("jsse", "jsse.alias_no_key_entry", +Alias);
   Print_Line ("mbeans", "jmxRemoteLifecycleListener.createServerFailed",
                         +Server);
   Print_Line ("membership", "cluster.mbean.register.already", +Bean);
   Print_Line ("realm", "jaasRealm.loginContextCreated", +User);
   Print_Line ("servlet", "err.not_iso8859_1", +WCh);
   Print_Line ("tribes", "cluster.mbean.register.already", +Bean);
   Print_Line ("util-threads", "threadpool.thread_error", +Ex_Name, +Th_Name);
end X_Tomcat;
