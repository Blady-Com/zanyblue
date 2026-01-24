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
--  The ZanyBlue.Text.Locales implements a type (Locale_Type) used
--  to store the locale information, i.e., language and territory
--  information.  E.g., for  a US English locale, the type could be
--  initialized using
--
--     My_Locale : Locale_Type := Make_Locale ("en_US");
--
--  or, alternatively,
--
--     My_Locale : Locale_Type := Make_Locale ("en", "US");
--
--  The empty or root locale is can be checked using the function
--
--     Is_Root_Locale
--
--  The implementation is a primitive implementation which is sufficient
--  for the use in ZanyBlue software and normally only the current locale
--  for the process is used, e.g.,
--
--     My_Locale : Locale_Type := Current_Locale;
--
--  The locale information associated with a Locale_Type can be queried
--  using the accessor functions
--
--     My_Locale.Language:        Two letter ISO language code
--     My_Locale.Language_Name:   Language name (in English)
--     My_Locale.Territory:       Two letter ISO territory code
--     My_Locale.Territory_Name:  Territory name (in English)
--     My_Locale.Name:            The locale ZB_LANG name, e.g., "en_US"
--
--  Locales are ordered, with the locale "fr" being the parent of the
--  locale "fr_CA".  The function Parent will return the locale
--  object for the parent, e.g.,
--
--     Parent ("fr_FR") => "fr"
--     Parent ("fr") => ""
--     Parent ("") => ""
--
--  The empty, or base locale, is it's own parent.
--

pragma License (Modified_GPL);

with Ada.Containers;

package ZanyBlue.Text.Locales is

   use Ada.Containers;

   Maximum_Level : constant := 4;
   --  Maximum depth for a locale: '' 'lang' 'lang-territory', 'lang-script'
   --  and 'lang-script-territory' (zero based).

   Maximum_Locale_Parents : constant := 5;
   --  Maximum number of times the Parent operator can be applied to a locale
   --  before hitting the base locale, e.g., "fr_Latn_FR" => "fr_Latn" =>
   --  "fr_FR" => "fr" => "".  This is used to put an upper limit on traversals
   --  of locales (prevent infinite loops).

   type Era_Type is (BCE, CE);
   --  Standard year eras.

   type Day_Period_Type is (AM, Noon, PM);
   --  Standard day periods.

   type Day_Type is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   --  Days of the week.

   type Month_Type is (Jan, Feb, Mar, Apr, May, Jun,
                       Jul, Aug, Sep, Oct, Nov, Dec);
   --  Months of the year.

   type Date_Time_Style_Type is (Full, Long, Medium, Short);
   --  Date and time format styles, e.g., medium would use short day names
   --  (Sun, etc), Full/Long would use full name (Sunday, etc).

   type Numeric_Item_Type is (Decimal, Group, List, Zero,
                              Plus, Minus, Exponent, Percent,
                              Permille, Infinity, Nan, Digit_Pattern);
   --  Misc. items associated with number, decimal point, zero character, etc.

   type Numeric_Style_Type is (Decimal, Scientific, Percent, Currency);
   --  Number style formats.

   Max_Language_Length  : constant := 3;
   --  Maximum number of characters is an ISO language code.

   Max_Script_Length    : constant := 4;
   --  Maximum number of characters is an ISO script code.

   Max_Territory_Length : constant := 3;
   --  Maximum number of characters is an ISO territory code.

   subtype Language_Type  is Wide_String (1 .. Max_Language_Length);
   --  An ISO language code, e.g., "en ", "zh ".

   subtype Script_Type    is Wide_String (1 .. Max_Script_Length);
   --  An ISO script code, e.g., "Latn", "Hans".

   subtype Territory_Type is Wide_String (1 .. Max_Territory_Length);
   --  An ISO territory code, e.g., "US ", "001".

   Empty_Language  : constant Language_Type := (others => ' ');
   --  Emtpy language code string

   Empty_Script    : constant Script_Type := (others => ' ');
   --  Empty string code string.

   Empty_Territory : constant Territory_Type := (others => ' ');
   --  Empty Territory code string.

   type Level_Type is range 0 .. Maximum_Level;
   --  Level locale exists in parent tree (internal integer type).

   type Locale_Type is tagged private;
   --  Locale_Type: This a private type used to store information on locales,
   --  the language and territory codes, either of which can be empty.
   --  It is a tagged type to allow the methods defined use to be used using
   --  the simpler dot notation.

   function Current_Locale return Locale_Type;
   --  Return the locale associated with the current process.  This is
   --  simply the locale defined by the environment variable ZB_LANG
   --  or LANG

   function Is_Locale_Defined (Language  : Wide_String;
                               Script    : Wide_String;
                               Territory : Wide_String) return Boolean;
   --  Check if the given locale has localized data associated with it.
   --  Note: This check is for direct locale support, if the library
   --  is built with only the Root and EN locales, then the check for
   --  the locales ("", "", "") and ("en", "", "") would return True.
   --  All other locales, e.g., ("en", "", "US") would return False.

   function Is_Root_Locale (Locale : Locale_Type) return Boolean;
   --  Is the locale the root, i.e., empty, locale.  Normally used
   --  as the limit of while loop when scanning up, via Parent.

   function Date_Format (Locale : Locale_Type;
                         Style  : Date_Time_Style_Type) return Wide_String;
   --  Localized format string used for dates in various style.

   function Date_Time_Format (Locale : Locale_Type;
                              Style  : Date_Time_Style_Type)
      return Wide_String;
   --  Localized format string used to combine date times strings in various
   --  styles.

   function Day_Period_Name (Locale     : Locale_Type;
                             Day_Period : Day_Period_Type) return Wide_String;
   --  Localized name of the day period.

   function Era_Name (Locale : Locale_Type;
                      Era    : Era_Type) return Wide_String;
   --  Localized name of the era.

   function Full_Day_Name (Locale : Locale_Type;
                           Day    : Day_Type) return Wide_String;
   --  Full day name for the given locale, e.g., "Sun" -> "Sunday".

   function Full_Month_Name (Locale : Locale_Type;
                             Month  : Month_Type) return Wide_String;
   --  Full month name for the given locale, e.g., "Jan" -> "January".

   procedure Get_Locale_Codes (Locale    : Locale_Type;
                               Language  : in out Language_Type;
                               Script    : in out Script_Type;
                               Territory : in out Territory_Type);
   --  Return the three naming components associated with a locale, e.g.,
   --  "en_Latn_US" => "en ", "Latn", "US ".

   function Hash (Key : Locale_Type) return Hash_Type;
   --  Calculate a Hask value for a locale to support using Locale_Type's
   --  in Ada.Container hash tables.

   function Language (Locale : Locale_Type) return Wide_String;
   --  Return the Language code associated with a locale

   function Locale_Digits (Locale    : Locale_Type;
                           Lowercase : Boolean) return Wide_String;
   --  Return the localized set of digits used to format numbers.  This
   --  is normally "0123456789abcdef".

   function Locale_Level (Locale : Locale_Type) return Level_Type;
   --  The level associated with the locale.  The root locale is at level
   --  0.

   function Make_Locale (Locale_String  : Wide_String) return Locale_Type;
   --  Construct a locale object based on a string value.  The value
   --  should conform to "ll_TT.extra" where "ll" is a two letter ISO
   --  language abbreviation, "TT" is a two letter ISO territory abbreviation
   --  and the ".extra" is optional extra information that is ignore.
   --  The string must have an underscore as the third character and, if
   --  extra information is present, must have a period as the sixth
   --  character.  The root locale (empty) is returned for invalid strings.
   --  Note, the language and territory strings need not map to known
   --  codes.

   function Make_Locale (Language  : Wide_String;
                         Territory : Wide_String) return Locale_Type;
   --  Construct a locale object based on language and territory strings.
   --  Only the initial characters of each string argument is considered.

   function Make_Locale (Language  : Wide_String;
                         Script    : Wide_String;
                         Territory : Wide_String) return Locale_Type;
   --  Construct a locale object based on language, script territory strings.
   --  Only the initial characters of each string argument is considered.

   function Locale_Name (Locale : Locale_Type) return Wide_String;
   --  Return the name of the locale.  The name is simply the value
   --  the ZB_LANG environment variable would need to create the locale,
   --  e.g., "en_US".


   function Locale_Name (Language       : Language_Type;
                         Script         : Script_Type;
                         Territory      : Territory_Type) return Wide_String;
   --  Locale name based on the individual naming components.

   function Number_Of_Defined_Locales return Positive;
   --  Return the number of locale built into the library.  This is a
   --  Positive as there is always the root locale.

   function Numeric_Format (Locale : Locale_Type;
                            Style  : Numeric_Style_Type) return Wide_String;
   --  Localized formats for numbers in various styles.

   function Numeric_Item (Locale : Locale_Type;
                          Item   : Numeric_Item_Type) return Wide_String;
   --  Localized numeric items, zero character, decimal point, etc.

   procedure Parent_Codes (Language       : in out Language_Type;
                           Script         : in out Script_Type;
                           Territory      : in out Territory_Type;
                           Base_Territory : Territory_Type := Empty_Territory);
   --  Determine the parent of a locale, i.e., the locale with one less piece
   --  of information, Parent ("en_US") = "en", Parent ("en") = "".
   --  The locale is used to allow parent searches to include both the
   --  territory and script, e.g., "fr_Latn_FR" => "fr_Latn" => "fr_FR" =>
   --  "fr" => "".
   --
   --       Locale         Root           Result
   --     1 ll_ssss_tt     n/a            ll_ssss
   --     2 ll_ssss        LL_SSSS_TT     ll_TT
   --     3 ll_ssss        !LL_SSSS_TT    ll
   --     4 ll_tt          n/a            ll
   --     5 ll             n/a            *base locale*
   --     6 *base locale*  n/a            *base locale*

   function Script (Locale : Locale_Type) return Wide_String;
   --  Return the script code associated with a locale

   procedure Set_Locale (Locale : Locale_Type);
   --  Set the locale returned by the Current_Locale function over-riding
   --  the environment defined locale via ZB_LANG/LANG.

   procedure Set_Locale (Name : String);
   --  Set the locale returned by the Current_Locale function by locale
   --  name.

   procedure Set_Locale (Wide_Name : Wide_String);
   --  Set the locale returned by the Current_Locale function by locale
   --  name.

   procedure Set_Traits (Locale : in out Locale_Type;
                         Name   : String);
   --  Set the traits associated with a locale.  E.g., to use UK English
   --  style dates and times for a locale use 'Set_Traits (Locale, "en_GB")'.

   procedure Set_Traits (Locale    : in out Locale_Type;
                         Wide_Name : Wide_String);
   --  Set the traits associated with a locale.  E.g., to use UK English
   --  style dates and times for a locale use 'Set_Traits (Locale, "en_GB")'.

   function Short_Day_Name (Locale : Locale_Type;
                            Day    : Day_Type) return Wide_String;
   --  Abbreviated day name for the given locale, e.g., "Sun" -> "Sun".

   function Short_Month_Name (Locale : Locale_Type;
                              Month  : Month_Type) return Wide_String;
   --  Abbreviated month name for the given locale, e.g., "Jan" -> "Jan".

   function Territory (Locale : Locale_Type) return Wide_String;
   --  Return the territory code associated with a locale

   function Time_Format (Locale : Locale_Type;
                         Style  : Date_Time_Style_Type) return Wide_String;
   --  Localized format string used for times in various style.

   function Traits_Name (Locale : Locale_Type) return Wide_String;
   --  Name associated with the locale traits (which defines the day
   --  names, decimal point character, etc), e.g., "en_IE".  Note, this
   --  name need not match the name associated with the locale.

   function Traits_Tag (Locale : Locale_Type) return Wide_String;
   --  Raw tag associated with the locale traits (which defines the day
   --  names, decimal point character, etc).  This is simply the locale values
   --  formatted as a fixed length string, e.g., "EN LATNUS ".

private

   function "=" (Left, Right : Locale_Type) return Boolean;
   --  Utility routine to check for equality

   type Trait_Index_Type is new Positive;

   type Locale_Type is tagged
      record
         Language_Code  : Language_Type      := Empty_Language;
         Script_Code    : Script_Type        := Empty_Script;
         Territory_Code : Territory_Type     := Empty_Territory;
         Traits_Index   : Trait_Index_Type   := 1;
      end record;

end ZanyBlue.Text.Locales;
