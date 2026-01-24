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

with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;

function ZanyBlue.Text.Format_Message
            (Message      : Wide_String;
             Arguments    : ZanyBlue.Text.Arguments.Argument_List;
             Mapping      : ZanyBlue.Text.Pseudo.Pseudo_Map_Access;
             Locale       : ZanyBlue.Text.Locales.Locale_Type;
             Raise_Errors : Boolean) return Wide_String;
--  Function to format a message text string containing references to
--  arguments, e.g., "{0}", "{1,10}", with the argument values based on
--  the argument list, Arguments.  The Mapping argument, if not null,
--  defines a mapping of source characters to a pseudo translation of
--  the character (as a simple character).  The Locale argument is not
--  use directly by the Format_Message function but is passed on to
--  the formatting functions for individual argument references.  When
--  a reference to an argument the does not exist in the Argument list
--  the exception No_Such_Argument_Error is raised unless Raise_Errors
--  is False.
