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

package body ZanyBlue.Text.Pseudo is

   -----------------
   -- Add_Mapping --
   -----------------

   procedure Add_Mapping (Pseudo_Map : in out Pseudo_Map_Type;
                          Mapping    : Pseudo_Map_Vector) is
      From : Wide_Character_Set := Null_Set;
      To   : Wide_Character_Set := Null_Set;
   begin
      for I in Mapping'Range loop
         From := From or To_Set (Mapping (I).Source);
         To := To or To_Set (Mapping (I).Target);
      end loop;
      Pseudo_Map.Mapping := To_Mapping (To_Sequence (From), To_Sequence (To));
   end Add_Mapping;

   ---------
   -- Map --
   ---------

   function Map (Pseudo_Map : Pseudo_Map_Type;
                 Ch         : Wide_Character) return Wide_Character is
   begin
      return Value (Pseudo_Map.Mapping, Ch);
   end Map;

end ZanyBlue.Text.Pseudo;
