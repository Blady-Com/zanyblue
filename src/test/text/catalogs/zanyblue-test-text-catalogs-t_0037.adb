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

separate (ZanyBlue.Test.Text.Catalogs)
procedure T_0037 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Facility     : constant Wide_String := "strings";
   Dir_Name     : constant Wide_String := Test_Src_Directory (Test_Area);
   Catalog      : Catalog_Type;
   N_Locales    : Natural;
   N_Messages   : Natural;

   procedure Check (Locale : Wide_String;
                    Value  : Wide_String);

   procedure Check (Locale : Wide_String;
                    Value  : Wide_String) is
      L : constant Locale_Type := Make_Locale (Locale);
   begin
      Check_Value (R, Get_Text (Catalog, Facility, "reboot.title", L), Value);
   end Check;

begin
   Catalog := Create;
   Load_Facility (Catalog, Facility, N_Locales, N_Messages, Dir_Name);
   R.Assert (N_Locales = 10, "Expected 10 locales");
   R.Assert (N_Messages = 212, "Expected 212 messages");
   Check ("", "Restart");
   Check ("de", "Neustart");
   Check ("de_DE", "Neustart");
   Check ("es", "Reiniciar");
   Check ("es_ES", "Reiniciar");
   Check ("fr", "Redémarrer");
   Check ("fr_FR", "Redémarrer");
   Check ("it", "Riavvia");
   Check ("it_IT", "Riavvia");
   Check ("ja", "再起動");
   Check ("ja_JP", "再起動");
   Check ("ko", "다시 시작");
   Check ("ko_KR", "다시 시작");
   Check ("sv", "Starta om");
   Check ("sv_SE", "Starta om");
   Check ("zh", "Restart");
   Check ("zh_CN", "重新启动");
   Check ("zh_TW", "重新啟動");
end T_0037;
