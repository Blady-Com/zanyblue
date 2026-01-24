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

with Ada.Wide_Text_IO;

----------
-- Dump --
----------
separate (ZanyBlue.Text.Catalogs)
procedure Dump (Catalog   : Catalog_Type;
                File_Name : Wide_String := "") is
   use Ada.Wide_Text_IO;

   procedure Dump_To_File (Catalog : Catalog_Type;
                           File    : File_Type);
   --  Dump the catalog to an opened file handle.

   procedure Hdr (File  : File_Type;
                  Title : Wide_String;
                  N     : Natural);
   --  Write the header data about the catalog.

   procedure Dump_Message (File : File_Type;
                           N    : in out Positive;
                           F    : Positive;
                           K    : Positive;
                           L    : Positive);
   --  Dump a message given the message indexes.

   procedure Dump_Message (File     : File_Type;
                           N        : in out Positive;
                           Facility : Wide_String;
                           Key      : Wide_String;
                           Locale   : Locale_Type;
                           Message  : Wide_String);
   --  Dump a message given the text.

   ------------------
   -- Dump_Message --
   ------------------

   procedure Dump_Message (File     : File_Type;
                           N        : in out Positive;
                           Facility : Wide_String;
                           Key      : Wide_String;
                           Locale   : Locale_Type;
                           Message  : Wide_String) is
   begin
      Put (File, To_Wide_String (Positive'Image (N)));
      Put (File, ": """);
      Put (File, Facility);
      Put (File, """, """);
      Put (File, Key);
      Put (File, """, """);
      Put (File, Locale_Name (Locale));
      Put (File, """, """);
      Put (File, Message);
      Put (File, """");
      New_Line (File);
      N := N + 1;
   end Dump_Message;

   ------------------
   -- Dump_Message --
   ------------------

   procedure Dump_Message (File : File_Type;
                           N : in out Positive;
                           F : Positive;
                           K : Positive;
                           L : Positive) is
   begin
      Dump_Message (File, N,
                    Get_Facility (Catalog, F),
                    Get_Key (Catalog, K),
                    Get_Locale (Catalog, L),
                    Get_Text (Catalog, F, K, L));
   exception
   when No_Such_Facility_Error |
        No_Such_Key_Error |
        No_Such_Locale_Error |
        No_Such_Message_Error =>
      null;
   end Dump_Message;

   ------------------
   -- Dump_To_File --
   ------------------

   procedure Dump_To_File (Catalog : Catalog_Type;
                           File    : File_Type) is
      N : Positive := 1;

   begin
      Put_Line (File, "Catalog dump");
      Put_Line (File, "------------");
      Hdr (File, "Number_Of_Facilities", Number_Of_Facilities (Catalog));
      for F in 1 .. Number_Of_Facilities (Catalog) loop
         Put_Line (File, "    """ & Get_Facility (Catalog, F) & """");
      end loop;
      Hdr (File, "Number_Of_Keys", Number_Of_Keys (Catalog));
      for K in 1 .. Number_Of_Keys (Catalog) loop
         Put_Line (File, "    """ & Get_Key (Catalog, K) & """");
      end loop;
      Hdr (File, "Number_Of_Locales", Number_Of_Locales (Catalog));
      for L in 1 .. Number_Of_Locales (Catalog) loop
         Put_Line (File, "    """
                       & Locale_Name (Get_Locale (Catalog, L)) & """");
      end loop;
      Hdr (File, "Number_Of_Messages", Number_Of_Messages (Catalog));
      --  Note: This needs to be reimplemented in terms of "Iterate".
      for L in 1 .. Number_Of_Locales (Catalog) loop
         for F in 1 .. Number_Of_Facilities (Catalog) loop
            for K in 1 .. Number_Of_Keys (Catalog) loop
               Dump_Message (File, N, F, K, L);
            end loop;
         end loop;
      end loop;
   end Dump_To_File;

   ---------
   -- Hdr --
   ---------

   procedure Hdr (File : File_Type; Title : Wide_String; N : Natural) is
   begin
      Put (File, Title);
      Set_Col (File, 25);
      Put (File, ": ");
      Put (File, To_Wide_String (Natural'Image (N)));
      New_Line (File);
   end Hdr;

   File : File_Type;

begin
   if File_Name'Length /= 0 then
      Wide_Create (File, File_Name);
      Dump_To_File (Catalog, File);
      Close (File);
   else
      Dump_To_File (Catalog, Current_Output);
   end if;
end Dump;
