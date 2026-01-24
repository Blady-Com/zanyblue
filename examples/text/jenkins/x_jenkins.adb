--  -*- encoding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
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

--
--  Example of the use of the ZanyBlue.Text_IO packages based on the
--  Oracle JDBC messages (from ojdbc.jar).
--

with Ada.Command_Line;
with Jenkins.Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.OS.Ld_Run_Path;
with ZanyBlue.Text.Formatting;

procedure X_Jenkins is

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

   Filename : constant String := "xmpl/file.in";
   Path1    : constant String := "/home/user/project";
   Path2    : constant String := "/home/user/other/path";
   Name     : constant String := "Example";
   Num1     : constant Integer := 10;
   Num2     : constant Integer := 100;

begin
   Process_Command_Line;
   Jenkins.Messages.Initialize;
   Print_Line ("Jenkins", "MavenBuild.FailedEarlier");
   Print_Line ("Jenkins", "MavenBuild.KeptBecauseOfParent", +Name);
   Print_Line ("Jenkins", "MavenBuild.Triggering", +Num1);
   Print_Line ("Jenkins", "MavenBuilder.Aborted");
   Print_Line ("Jenkins", "MavenBuilder.AsyncFailed");
   Print_Line ("Jenkins", "MavenBuilder.Failed");
   Print_Line ("Jenkins", "MavenBuilder.Waiting");
   Print_Line ("Jenkins", "MavenModule.Pronoun");
   Print_Line ("Jenkins", "MavenModuleSet.DiplayName");
   Print_Line ("Jenkins", "MavenModuleSet.AlternateSettingsRelativePath");
   Print_Line ("Jenkins", "MavenModuleSetBuild.DiscoveredModule",
                          +Name, +Num1);
   Print_Line ("Jenkins", "MavenModuleSetBuild.DownloadedArtifact",
                          +Num1, +Num2);
   Print_Line ("Jenkins", "MavenModuleSetBuild.FailedToParsePom");
   Print_Line ("Jenkins", "MavenModuleSetBuild.FailedToTransfer", +Filename);
   Print_Line ("Jenkins", "MavenModuleSetBuild.FoundModuleWithoutProject",
                          +Path1);
   Print_Line ("Jenkins", "MavenModuleSetBuild.NoMavenConfigured");
   Print_Line ("Jenkins", "MavenModuleSetBuild.NoSuchPOMFile", +Filename);
   Print_Line ("Jenkins", "MavenModuleSetBuild.NoSuchAlternateSettings",
                          +Filename);
   Print_Line ("Jenkins", "MavenModuleSetBuild.NoMavenInstall");
   Print_Line ("Jenkins",
               "MavenModuleSetBuild.SettinsgXmlAndPrivateRepository",
               +Path1, +Path2);
   Print_Line ("Jenkins", "MavenProbeAction.DisplayName");
   Print_Line ("Jenkins", "MavenProcessFactory.ClassWorldsNotFound",
                          +Path1);
   Print_Line ("Jenkins", "MavenRedeployer.DisplayName");
   Print_Line ("Jenkins", "MavenVersionCallable.MavenHomeDoesntExist",
                          +Path1);
   Print_Line ("Jenkins", "MavenVersionCallable.MavenHomeIsNotDirectory",
                          +Path1);
   Print_Line ("Jenkins", "ProcessCache.Reusing");
   Print_Line ("Jenkins", "RedeployPublisher.getDisplayName");
   Print_Line ("Jenkins", "RedeployPublisher.RepositoryURL.Mandatory");
   Print_Line ("Jenkins", "ReleaseAction.DisplayName");
end X_Jenkins;
