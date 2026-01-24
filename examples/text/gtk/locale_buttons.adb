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

with ZanyBlue.Text.CLDR;
with ZanyBlue.Text.Formatting;

package body Locale_Buttons is

   use ZanyBlue.Text;
   use ZanyBlue.Text.CLDR;
   use ZanyBlue.Text.Formatting;

   procedure New_Locale_Button (Button  : in out Locale_Button;
                                Locale  : Locale_Type) is
   begin
      Button := new Locale_Button_Record;
      Initialize (Button, To_UTF8 (Format ("appmsg", "button_text",
                                       +Locale_Name (Locale),
                                       +Full_Locale_Name (Locale))));
      Button.Locale := Locale;
   end New_Locale_Button;

end Locale_Buttons;
