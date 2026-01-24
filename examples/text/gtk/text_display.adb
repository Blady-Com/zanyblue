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

with Gtk.Separator;
with Gtk.Enums;

package body Text_Display is

   procedure Gtk_New (Dialog : out Text_Display) is
   begin
      Dialog := new Text_Display_Record;
      Initialize (Dialog);
   end Gtk_New;

   procedure Initialize (Dialog : access Text_Display_Record'Class) is
      Sep : Gtk.Separator.Gtk_Separator;
   begin
      Gtk.Window.Initialize (Dialog, Gtk.Enums.Window_Toplevel);

      Gtk.Box.Gtk_New_Vbox (Dialog.Vbox, False, 0);
      Add (Dialog, Dialog.Vbox);
      Gtk.Box.Show (Dialog.Vbox);

      Gtk.Box.Gtk_New_Hbox (Dialog.Action_Area, True, 5);
      Gtk.Box.Set_Border_Width (Dialog.Action_Area, 10);
      Gtk.Box.Pack_End (Dialog.Vbox, Dialog.Action_Area, False, True, 0);
      Gtk.Box.Show (Dialog.Action_Area);

      Gtk.Separator.Gtk_New_Hseparator (Sep);
      Gtk.Box.Pack_End (Dialog.Vbox, Sep, False, True, 0);
      Gtk.Separator.Show (Sep);
   end Initialize;

end Text_Display;
