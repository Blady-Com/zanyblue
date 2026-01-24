
--
--  Bootstrap ZanyBlue.ZBMCompile_Messages package.  This package provides
--  nothing, i.e., no message strings.  All ZanyBlue.Text requests for
--  message strings simply return the request key value.
--

package ZBMCompile.Messages is

   pragma Warnings (Off, ZBMCompile.Messages);

   Facility_1 : constant Wide_String := "zbmcompile";

   procedure Initialize;

end ZBMCompile.Messages;
