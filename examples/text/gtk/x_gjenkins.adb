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

with Ada.Command_Line;         use Ada.Command_Line;
with Gtk.Window;               use Gtk.Window;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Button;               use Gtk.Button;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Label;                use Gtk.Label;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Separator;            use Gtk.Separator;
with Text_Display;             use Text_Display;
with Locale_Buttons;           use Locale_Buttons;
with ZanyBlue.Text.CLDR;       use ZanyBlue.Text.CLDR;
with ZanyBlue.Text.Pseudo;     use ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;    use ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting; use ZanyBlue.Text.Formatting;
with ZanyBlue.OS.Ld_Run_Path;

with AppMsg;
with Jenkins.Messages;

procedure X_GJenkins is

   use ZanyBlue.Text;

   Usage_Error : exception;

   package Button_Cb is
      new Callback (Locale_Button_Record);

   package Dialog_Cb is
      new Callback (Gtk_Widget_Record);

   package Main_Cb is
      new Return_Callback (Gtk_Widget_Record, Boolean);

   function Main_Window_Delete_Event
      (Object : access Gtk_Widget_Record'Class) return Boolean;
   procedure Display_Strings (B : access Locale_Button_Record'Class);
   procedure Add_Buttons (Container    : Gtk_Box;
                          Locale_Name1 : Wide_String;
                          Locale_Name2 : Wide_String);
   procedure Process_Command_Line;

   procedure Process_Command_Line is
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

   function Main_Window_Delete_Event
      (Object : access Gtk_Widget_Record'Class) return Boolean is
      pragma Unreferenced (Object);
   begin
      Main_Quit;
      return True;
   end Main_Window_Delete_Event;

   procedure Display_Strings (B : access Locale_Button_Record'Class) is

      procedure Add_Line (Dialog : Text_Display.Text_Display;
                          Text : String);
      procedure Add_Message (Dialog : Text_Display.Text_Display;
                             Key    : Wide_String;
                             Locale : Locale_Type);

      procedure Add_Line (Dialog : Text_Display.Text_Display;
                          Text : String) is
         Label  : Gtk_Label;
      begin
         Gtk_New (Label, Text);
         Pack_Start (Dialog.Vbox, Label);
      end Add_Line;

      procedure Add_Message (Dialog : Text_Display.Text_Display;
                             Key    : Wide_String;
                             Locale : Locale_Type) is
      begin
         Add_Line (Dialog, Format ("Jenkins", Key, Locale => Locale));
      end Add_Message;

      Locale : constant Locale_Type := B.Locale;
      Dialog : Text_Display.Text_Display;
      Button : Gtk_Button;
      Sep    : Gtk_Separator;

   begin
      Gtk_New (Dialog);
      Set_Border_Width (Dialog, 10);

      if Territory (Locale) /= "" then
         Add_Line (Dialog, Format ("appmsg", "langname",
                                   +Full_Locale_Name (Locale)));
      else
         Add_Line (Dialog, Format ("appmsg", "langname",
                                   +Language_Name (Language (Locale))));
      end if;
      Gtk_New_Hseparator (Sep);
      Pack_Start (Dialog.Vbox, Sep);

      Add_Message (Dialog,
                   "MavenBuild.FailedEarlier",
                   Locale);
      Add_Message (Dialog,
                   "MavenBuild.KeptBecauseOfParent",
                   Locale);
      Add_Message (Dialog,
                   "MavenBuild.Triggering",
                   Locale);
      Add_Message (Dialog,
                   "MavenBuilder.Aborted",
                   Locale);
      Add_Message (Dialog,
                   "MavenBuilder.AsyncFailed",
                   Locale);
      Add_Message (Dialog,
                   "MavenBuilder.Failed",
                   Locale);
      Add_Message (Dialog,
                   "MavenBuilder.Waiting",
                   Locale);
      Add_Message (Dialog,
                   "MavenModule.Pronoun",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSet.DiplayName",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSet.AlternateSettingsRelativePath",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.DiscoveredModule",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.DownloadedArtifact",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.FailedToParsePom",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.FailedToTransfer",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.FoundModuleWithoutProject",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.NoMavenConfigured",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.NoSuchPOMFile",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.NoSuchAlternateSettings",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.NoMavenInstall",
                   Locale);
      Add_Message (Dialog,
                   "MavenModuleSetBuild.SettinsgXmlAndPrivateRepository",
                   Locale);
      Add_Message (Dialog,
                   "MavenProbeAction.DisplayName",
                   Locale);
      Add_Message (Dialog,
                   "MavenProcessFactory.ClassWorldsNotFound",
                   Locale);
      Add_Message (Dialog,
                   "MavenRedeployer.DisplayName",
                   Locale);
      Add_Message (Dialog,
                   "MavenVersionCallable.MavenHomeDoesntExist",
                   Locale);
      Add_Message (Dialog,
                   "MavenVersionCallable.MavenHomeIsNotDirectory",
                   Locale);
      Add_Message (Dialog,
                   "ProcessCache.Reusing",
                   Locale);
      Add_Message (Dialog,
                   "RedeployPublisher.getDisplayName",
                   Locale);
      Add_Message (Dialog,
                   "RedeployPublisher.RepositoryURL.Mandatory",
                   Locale);
      Add_Message (Dialog,
                   "ReleaseAction.DisplayName",
                   Locale);

      Gtk_New (Button, Format ("appmsg", "close"));
      Pack_Start (Dialog.Action_Area, Button, True, True, 0);
      Dialog_Cb.Object_Connect
        (Button, "clicked",
         Dialog_Cb.To_Marshaller (Destroy_Cb'Access), Dialog);

      Show_All (Dialog);
   end Display_Strings;

   procedure Add_Buttons (Container    : Gtk_Box;
                          Locale_Name1 : Wide_String;
                          Locale_Name2 : Wide_String) is
      Button  : Locale_Button;
      Hbox    : Gtk_Box;
   begin
      Gtk_New_Hbox (Hbox);
      Add (Container, Hbox);
      New_Locale_Button (Button, Make_Locale (Locale_Name1));
      Button_Cb.Connect (Button, "clicked",
                         Button_Cb.To_Marshaller (Display_Strings'Access));
      Pack_Start (Hbox, Button);
      New_Locale_Button (Button, Make_Locale (Locale_Name2));
      Button_Cb.Connect (Button, "clicked",
                         Button_Cb.To_Marshaller (Display_Strings'Access));
      Pack_Start (Hbox, Button);
   end Add_Buttons;

   Main_W           : Gtk_Window;
   VBox             : Gtk_Box;

begin
   ZanyBlue.Text.CLDR.Initialize;
   AppMsg.Initialize;
   Jenkins.Messages.Initialize;
   Disable_Exceptions;
   Process_Command_Line;
   Gtk.Main.Init;

   Gtk_New (Main_W, Window_Toplevel);

   Gtk_New_Vbox (VBox);
   Add (Main_W, VBox);

   Add_Buttons (VBox, "", "da");
   Add_Buttons (VBox, "de", "es");
   Add_Buttons (VBox, "fr", "ja");
   Add_Buttons (VBox, "nl", "pt_BR");
   Add_Buttons (VBox, "ru", "tr");
   Add_Buttons (VBox, "zh_CN", "");

   Main_Cb.Connect (Main_W, "delete_event",
                    Main_Cb.To_Marshaller (Main_Window_Delete_Event'Access));

   Show_All (Main_W);
   Gtk.Main.Main;
exception
when Usage_Error =>
   Print_Line ("appmsg", "usage");
end X_GJenkins;
