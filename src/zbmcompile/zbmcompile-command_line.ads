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

with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;

package ZBMCompile.Command_Line is

   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;

   Usage : exception;

   type Facility_Source_Type is
      record
         Directory : Unbounded_Wide_String;
         Facility  : Unbounded_Wide_String;
      end record;
   --  Representation of a command line defined facilities, list of facilities
   --  to be loaded from specific directories.

   function Body_Initialize return Boolean;
   --  Include the initialization call in the generated body, "-b" command
   --  line option.

   function Body_Name (Package_Name : Wide_String) return Wide_String;
   --  Return the name of the body file: either the name defined on the
   --  command line via the -b option or the name generated from the package
   --  for the compiler.

   function Debug return Boolean;
   --  Running in debug mode?  "-D" command line option.

   function Export_Name return Wide_String;
   --  Name of the export name for the Initialize procedure.

   function Export_Name_P return Boolean;
   --  Has an export name been defined via the command line.

   function Extension return Wide_String;
   --  Extension to use for message files, "-e" command line option.

   function Facility (I : Positive) return Facility_Source_Type;
   --  I'th source file to process from the command line arguments.

   function Force return Boolean;
   --  Force generation of source code in the presence of errors.

   function Is_Selected (Locale : Locale_Type) return Boolean;
   --  Was a particular locale selected by the user?

   function N_Facilities return Natural;
   --  Number of the command line files defined via command line properties
   --  file name (after wild card expansion).

   function Optimize return Boolean;
   --  Generate an optimized source file (organized by locale).

   function Package_Body return Wide_String;
   --  Name of the generated package body file

   function Package_Name return Wide_String;
   --  Name of the generated package, first command line argument.

   function Package_Spec return Wide_String;
   --  Name of the generated package spec file

   procedure Process_Command_Line;
   --  Process the command line arguments into a data structures that higher
   --  level function can represent, e.g, Facility (I).
   --  The arguments are:
   --
   --  * Optional arguments
   --    -v            Enable verbose output
   --    -q            Suppress informational output (quiet)
   --    -i            Include Initialize routine in generated package body.
   --    -B            Compile only the base locale
   --    -L locale     Compile only the named locale
   --    -e extension  Set the extension to use for message files
   --    -s filename   Define the name of the output spec file
   --    -b filename   Define the name of the output body file
   --
   --  * Positional arguments
   --    Package_Name      Define the name of the generated Ada package
   --    Sources           The base name of the messages source files

   function Quiet return Boolean;
   --  Running in quiet mode?  "-q" command line option.

   function Selected_Locales return Locale_Set;
   --  Return the set of locales selected by the user.

   function Spec_Name (Package_Name : Wide_String) return Wide_String;
   --  Return the name of the Spec file: either the name defined on the
   --  command line via the -s option or the name generated from the package
   --  for the compiler.

   function Verbose return Boolean;
   --  Running in verbose mode?  "-v" command line option.

end ZBMCompile.Command_Line;
