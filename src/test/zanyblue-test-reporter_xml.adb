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

with GNAT.IO;            use GNAT.IO;

package body ZanyBlue.Test.Reporter_XML is

   use AUnit;

   procedure Write_String (Level   : Positive;
                           Prefix  : String;
                           Value   : String;
                           Postfix : String);
   procedure Dump_Result_List (L : Result_Lists.List);
   --  List failed assertions

   procedure Report_Test (Test : Test_Result);
   --  Report a single assertion failure or unexpected exception

   function Elapsed_Time (Test : Test_Result) return String;

   ----------------------
   -- Dump_Result_List --
   ----------------------

   procedure Dump_Result_List (L : Result_Lists.List) is

      use Result_Lists;

      C : Cursor := First (L);

   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Test (Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;

   ------------------
   -- Elapsed_Time --
   ------------------

   function Elapsed_Time (Test : Test_Result) return String is
      pragma Unreferenced (Test);
   begin
      return "0.0";
   end Elapsed_Time;

   ------------
   -- Report --
   ------------

   procedure Report (Engine : XML_Reporter;
                     R      : in out Result'Class)
   is
      pragma Unreferenced (Engine);
   begin
      Put_Line ("<?xml version=""1.0"" encoding=""utf-8""?>");
      Put_Line ("<testsuites>");

      declare
         S : Result_Lists.List;
      begin
         Successes (R, S);
         Dump_Result_List (S);
      end;

      declare
         F : Result_Lists.List;
      begin
         Failures (R, F);
         Dump_Result_List (F);
      end;

      declare
         E : Result_Lists.List;
      begin
         Errors (R, E);
         Dump_Result_List (E);
      end;

      Put_Line ("</testsuites>");
   end Report;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Test (Test : Test_Result) is
   begin
      Write_String (1, "<testsuite name=""", Test.Test_Name.all, """");
      Write_String (2, "package=""", Test.Test_Name.all, """>");

      if Test.Routine_Name /= null then
         Write_String (2, "<testcase name=""", Test.Routine_Name.all, """");
      else
         Write_String (2, "<testcase name=""", "NONE", """");
      end if;
      Write_String (3, "time=""", Elapsed_Time (Test), """>");

      if Test.Failure /= null then
         Write_String (2, "<failure type=""", "assertion", """>");
         Write_String (3, "", Test.Failure.Message.all, "");
         if Test.Failure.Source_Name /= null then
            Write_String (3, "", Test.Failure.Source_Name.all,
                             ":" & Natural'Image (Test.Failure.Line));
         end if;
         Write_String (2, "</failure>", "", "");
      elsif Test.Error /= null then
         Write_String (2, "<failure type=""", "error", """");
         Write_String (3, "message=""", Test.Error.Exception_Name.all, """>");
         if Test.Error.Exception_Message /= null then
            Write_String (3, "", Test.Error.Exception_Message.all, "");
         end if;
         if Test.Error.Traceback /= null then
            Write_String (3, "", Test.Error.Traceback.all, "");
         end if;
         Write_String (2, "</failure>", "", "");
      end if;

      Put_Line ("    </testcase>");
      Put_Line ("  </testsuite>");

   end Report_Test;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (Level   : Positive;
                           Prefix  : String;
                           Value   : String;
                           Postfix : String) is
   begin
      for I in 1 .. Level loop
         Put ("  ");
      end loop;
      Put_Line (Prefix & Value & Postfix);
   end Write_String;

end ZanyBlue.Test.Reporter_XML;
