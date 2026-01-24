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

separate (ZanyBlue.Test.Text.Formatting.T_0094)
package body T_0094_Types is

   function Create (Value : T_0094_Type) return T_0094_Argument is
   begin
      return T_0094_Argument'(Data => Value);
   end Create;

   function Format (Value    : T_0094_Argument;
                    Template : Wide_String;
                    Locale   : Locale_Type) return Wide_String is
      pragma Unreferenced (Locale);

   begin
      case Value.Data is
      when T_0094_1 =>
         R.Assert (Template = "", "Expected empty template");
      when T_0094_2 =>
         R.Assert (Template = "2", "Expected template value 2");
      when T_0094_3 =>
         R.Assert (Template = "2", "Expected template value 2");
      when T_0094_4 =>
         R.Assert (Template = "2//////", "Expected template value 2//////");
      when T_0094_5 =>
         R.Assert (Template = "", "Expected empty template");
      end case;
      return To_Wide_String (T_0094_Type'Image (Value.Data));
   end Format;

end T_0094_Types;
