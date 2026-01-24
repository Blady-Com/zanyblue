with Ada.Strings.Hash;
with ZanyBlue.Compiler;

package body ZanyBlue.Text is

   ---------------
   -- Wide_Hash --
   ---------------

   function Wide_Hash (Key : Wide_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_UTF8 (Key));
   end Wide_Hash;

   -----------------
   -- Wide_Create --
   -----------------

   procedure Wide_Create (File : in out Ada.Wide_Text_IO.File_Type;
                          Name : Wide_String)
      renames ZanyBlue.Compiler.Wide_Create;

   ---------------
   -- Wide_Open --
   ---------------

   procedure Wide_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Mode : Ada.Wide_Text_IO.File_Mode;
                        Name : Wide_String)
      renames ZanyBlue.Compiler.Wide_Open;

   -------------
   -- To_UTF8 --
   -------------

   function To_UTF8 (Value : Wide_String) return String
      renames ZanyBlue.Compiler.To_UTF8;

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8 (Value : String) return Wide_String
      renames ZanyBlue.Compiler.From_UTF8;

end ZanyBlue.Text;
