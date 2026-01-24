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
--  Bootstrap ZanyBlue.ZBMCompile_Messages package.  This package provides
--  nothing, i.e., no message strings.  All ZanyBlue.Text requests for
--  message strings simply return the request key value.
--

package ZBMCompile.Messages is

   pragma Warnings (Off, ZBMCompile.Messages);

   Facility_1 : constant Wide_String := "zbmcompile";

   procedure Initialize;

end ZBMCompile.Messages;
