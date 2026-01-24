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

with ZanyBlue.Text.Catalogs;

package ZBMCompile.Code_Gen is

   use ZanyBlue.Text.Catalogs;

   function Optimize (Catalog : Catalog_Type;
                      Verbose : Boolean) return Catalog_Type;
   --  Optimize the input catalog by organizing the messages by locale.
   --  For large catalogs, data associated with unreferenced locales might
   --  never be loaded into RAM.

   procedure Generate_Ada (Catalog         : Catalog_Type;
                           Package_Name    : Wide_String;
                           Spec_Name       : Wide_String;
                           Body_Name       : Wide_String;
                           Verbose         : Boolean;
                           Body_Initialize : Boolean;
                           Export_Name     : Wide_String);
   --  Generate an Ada package to define the messages in the argument
   --  Catalog, i.e., generate a compiled in representation of a set of
   --  message properties files.

end ZBMCompile.Code_Gen;
