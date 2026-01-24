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

separate (ZanyBlue.Test.Compiler)
procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class) is

begin
   if Name = "GNAT" then
      R.Assert (Spec_File_Name ("XYZ") = "xyz.ads",
                "Failed to generate simple spec file name");
      R.Assert (Spec_File_Name ("X.Y.Z") = "x-y-z.ads",
                "Failed to generate hierarchical spec file name");
   else
      R.Assert (False,
                To_UTF8 ("No " & Name & " compiler Spec_File_Name tests"));
   end if;
end T_0003;
