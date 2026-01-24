
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
      Load ("zbmbase");
      Load ("zbmexceptions");
   end Initialize;

   ----------
   -- Load --
   ----------

   procedure Load (Name : Wide_String) is
      Filename : constant Wide_String := "mesg/" & Name & ".properties";
      Locale   : constant Locale_Type := Make_Locale ("");
      Count    : Natural;
   begin
      Count := Load_File (Standard_Catalog, Filename, Name, Locale);
      Print_Line ("zbmcompile", "00011", +Count, +Name);
   end Load;

end ZBMCompile.Messages;
