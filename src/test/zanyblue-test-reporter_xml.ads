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
------------------------------------------------------------------------------
--                                                                          --
-- Implementation of an AUnit reporter generating XML suitable for use      --
-- with Hudson (www.hudson-ci.org), i.e., Java JUnit style XML.             --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Reporter; use AUnit.Reporter;
with AUnit.Test_Results; use AUnit.Test_Results;

package ZanyBlue.Test.Reporter_XML is

   type XML_Reporter is new Reporter with null record;

   procedure Report (Engine : XML_Reporter;
                     R      : in out Result'Class);

end ZanyBlue.Test.Reporter_XML;
