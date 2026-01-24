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
--  Root package for the ZanyBlue library.  This is pure package containing
--  function definitions giving version information.
--

pragma License (Modified_GPL);

package ZanyBlue.OS.Ld_Run_Path is

   pragma Warnings (Off, ZanyBlue.OS.Ld_Run_Path);
   pragma Linker_Options ("-Wl,-R,$ORIGIN/../lib");
   --  Embed the ls.do runtime shared library path "../lib" relative to the
   --  executable.

end ZanyBlue.OS.Ld_Run_Path;
