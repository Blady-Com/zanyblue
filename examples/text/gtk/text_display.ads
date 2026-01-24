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

with Gtk.Window;
with Gtk.Box;

package Text_Display is

   --  This is intended to reprogram the Gtk_Dialog fully in Ada
   --  A dialog is simply a window with two areas

   type Text_Display_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Vbox        : Gtk.Box.Gtk_Box;
         Action_Area : Gtk.Box.Gtk_Box;
      end record;
   type Text_Display is access all Text_Display_Record'Class;

   procedure Gtk_New (Dialog : out Text_Display);
   procedure Initialize (Dialog : access Text_Display_Record'Class);

end Text_Display;
