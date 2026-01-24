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

package body ZanyBlue.Compiler is

   ----------
   -- Name --
   ----------

   function Name return Wide_String is
      separate;

   -----------------
   -- Wide_Create --
   -----------------

   procedure Wide_Create (File : in out Ada.Wide_Text_IO.File_Type;
                          Name : Wide_String) is
      separate;

   ---------------
   -- Wide_Open --
   ---------------

   procedure Wide_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Mode : Ada.Wide_Text_IO.File_Mode;
                        Name : Wide_String) is
      separate;

   --------------------
   -- Spec_File_Name --
   --------------------

   function Spec_File_Name (Package_Name : Wide_String) return Wide_String is
      separate;

   --------------------
   -- Body_File_Name --
   --------------------

   function Body_File_Name (Package_Name : Wide_String) return Wide_String is
      separate;

   -------------
   -- To_UTF8 --
   -------------

   function To_UTF8 (Value : Wide_String) return String is
      separate;

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8 (Value : String) return Wide_String is
      separate;

end ZanyBlue.Compiler;
