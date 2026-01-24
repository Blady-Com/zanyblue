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
--  Root package for the ZanyBlue library.  This is pure package containing
--  function definitions giving version information.
--

pragma License (Modified_GPL);

package ZanyBlue is

   pragma Pure;

   type Version_Status_Type is (Alpha, Beta, Production);
   --  There are three supported releases statuses:
   --    * Alpha for development code
   --    * Beta for beta releases
   --    * Production for production releases

   function Version_Major return Natural;
   --  The major version number associated with the ZanyBlue release.

   function Version_Minor return Natural;
   --  The minor version number associated with the ZanyBlue release.

   function Version_Patch return Natural;
   --  The patch version number associated with the ZanyBlue release.
   --  This should normally be 0 unless a serious issue was encountered
   --  with a release.

   function Version_Status return Version_Status_Type;
   --  The release status, see Version_Status_Type for the possible
   --  enumeration values.

   function Revision return Wide_String;
   --  The Subversion revision number for the build.

   function Copyright_Year return Positive;
   --  The copyright year for the build.

end ZanyBlue;
