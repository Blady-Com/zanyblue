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
--  Bootstrap ZanyBlue.ZBMCompile_Messages package.  Rather than the standard
--  message package which has the strings compiled in, this package simply
--  loads the English message file from the mesg directory.  During the
--  build, no localized messages are available.
--

with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;

package body ZBMCompile.Messages is

   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;

   procedure Load (Name : Wide_String);
   --  Load a facility from the local "mesg" directory.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Load ("zbmcompile");
      Load ("zbmcode");
   end Initialize;

   ----------
   -- Load --
   ----------

   procedure Load (Name : Wide_String) is
      Filename : constant Wide_String := "mesg/" & Name & ".properties";
      Locale   : constant Locale_Type := Make_Locale ("");
      Count    : Natural;
   begin
      Load_File (Standard_Catalog, Filename, Name, Locale, Count);
      Print_Line ("zbmcompile", "00011", +Count, +Name);
   end Load;

begin
   Initialize;
end ZBMCompile.Messages;
