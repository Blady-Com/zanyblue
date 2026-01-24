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

pragma License (Modified_GPL);

with Ada.Command_Line;         use Ada.Command_Line;
with GNAT.Encode_UTF8_String;  use GNAT.Encode_UTF8_String;
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
with OJDBC.Messages;

procedure X_GOjdbc is

   use ZanyBlue.Text;

   Usage_Error : exception;

   package Button_Cb is
      new Callback (Locale_Button_Record);

   package Dialog_Cb is
      new Callback (Gtk_Widget_Record);

   package Main_Cb is
      new Return_Callback (Gtk_Widget_Record, Boolean);

   function UTF8_String (Facility : Wide_String;
                         Key      : Wide_String;
                         Locale   : Locale_Type) return String;
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

   function UTF8_String (Facility : Wide_String;
                         Key      : Wide_String;
                         Locale   : Locale_Type) return String is
   begin
      return Encode_Wide_String (Format (Facility, Key, Locale => Locale));
   end UTF8_String;

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
         Add_Line (Dialog, UTF8_String ("ojdbc", Key, Locale));
      end Add_Message;

      Locale : constant Locale_Type := B.Locale;
      Dialog : Text_Display.Text_Display;
      Button : Gtk_Button;
      Sep    : Gtk_Separator;

   begin
      Gtk_New (Dialog);
      Set_Border_Width (Dialog, 10);

      if Territory (Locale) /= "" then
         Add_Line (Dialog,
                To_UTF8 (Format ("appmsg", "langname",
                                 +Full_Locale_Name (Locale))));
      else
         Add_Line (Dialog,
                To_UTF8 (Format ("appmsg", "langname",
                                 +Language_Name (Language (Locale)))));
      end if;
      Gtk_New_Hseparator (Sep);
      Pack_Start (Dialog.Vbox, Sep);

      Add_Message (Dialog, "ORA-17001", Locale);
      Add_Message (Dialog, "ORA-17002", Locale);
      Add_Message (Dialog, "ORA-17003", Locale);
      Add_Message (Dialog, "ORA-17004", Locale);
      Add_Message (Dialog, "ORA-17005", Locale);
      Add_Message (Dialog, "ORA-17006", Locale);
      Add_Message (Dialog, "ORA-17007", Locale);
      Add_Message (Dialog, "ORA-17008", Locale);
      Add_Message (Dialog, "ORA-17009", Locale);
      Add_Message (Dialog, "ORA-17010", Locale);
      Add_Message (Dialog, "ORA-17011", Locale);
      Add_Message (Dialog, "ORA-17012", Locale);
      Add_Message (Dialog, "ORA-17014", Locale);
      Add_Message (Dialog, "ORA-17015", Locale);
      Add_Message (Dialog, "ORA-17016", Locale);
      Add_Message (Dialog, "ORA-17023", Locale);
      Add_Message (Dialog, "ORA-17026", Locale);
      Add_Message (Dialog, "ORA-17351", Locale);
      Add_Message (Dialog, "ORA-17352", Locale);
      Add_Message (Dialog, "ORA-17353", Locale);

      Gtk_New (Button, Encode_Wide_String (Format ("appmsg", "close")));
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
   AppMsg.Initialize;
   OJDBC.Messages.Initialize;
   Process_Command_Line;
   Gtk.Main.Init;

   Gtk_New (Main_W, Window_Toplevel);

   Gtk_New_Vbox (VBox);
   Add (Main_W, VBox);

   Add_Buttons (VBox, "",      "ar");
   Add_Buttons (VBox, "ca",    "cs");
   Add_Buttons (VBox, "da",    "de");
   Add_Buttons (VBox, "el",    "es");
   Add_Buttons (VBox, "fi",    "fr");
   Add_Buttons (VBox, "hu",    "it");
   Add_Buttons (VBox, "he",    "ja");
   Add_Buttons (VBox, "ko",    "nl");
   Add_Buttons (VBox, "no",    "pl");
   Add_Buttons (VBox, "pt_BR", "pt");
   Add_Buttons (VBox, "ro",    "ru");
   Add_Buttons (VBox, "sk",    "sv");
   Add_Buttons (VBox, "th",    "tr");
   Add_Buttons (VBox, "zh_CN", "zh_TW");

   Main_Cb.Connect (Main_W, "delete_event",
                    Main_Cb.To_Marshaller (Main_Window_Delete_Event'Access));

   Show_All (Main_W);
   Gtk.Main.Main;
exception
when Usage_Error =>
   Print_Line ("appmsg", "usage");
end X_GOjdbc;
