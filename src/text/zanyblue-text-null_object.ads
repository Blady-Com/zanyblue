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

with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;

package ZanyBlue.Text.Null_Object is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   type Null_Argument_Type is new Argument_Type with null record;

   overriding
   function Format (Value    : Null_Argument_Type;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String;
   --  Format a null value, simply the empty string.

   Null_Argument : constant Null_Argument_Type := (null record);
   --  Constant use as the default argument to indicate "not used".

end ZanyBlue.Text.Null_Object;
