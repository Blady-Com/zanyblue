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

with Interfaces.C;

package body ZanyBlue.OS is

   use type Interfaces.C.unsigned_long;

   subtype LCID is Interfaces.C.unsigned_long;

   function GetUserDefaultLCID return LCID;
   pragma Import (Stdcall, GetUserDefaultLCID, "GetUserDefaultLCID");
   --  Return the Windows LCID value for the current user.

   type String_Access is access constant Wide_String;
   type LCID_Map_Type is
      record
         Value : LCID;
         Name  : String_Access;
      end record;

   --  Afrikaans
   L_af         : aliased constant Wide_String := "af";
   --  Albanian
   L_sq         : aliased constant Wide_String := "sq";
   --  Arabic, UAE
   L_ar_AE      : aliased constant Wide_String := "ar_AE";
   --  Arabic, Bahrain
   L_ar_BH      : aliased constant Wide_String := "ar_BH";
   --  Arabic, Algeria
   L_ar_DZ      : aliased constant Wide_String := "ar_DZ";
   --  Arabic, Egypt
   L_ar_EG      : aliased constant Wide_String := "ar_EG";
   --  Arabic, Iraq
   L_ar_IQ      : aliased constant Wide_String := "ar_IQ";
   --  Arabic, Jordan
   L_ar_JO      : aliased constant Wide_String := "ar_JO";
   --  Arabic, Kuwait
   L_ar_KW      : aliased constant Wide_String := "ar_KW";
   --  Arabic, Lebanon
   L_ar_LB      : aliased constant Wide_String := "ar_LB";
   --  Arabic, Libya
   L_ar_LY      : aliased constant Wide_String := "ar_LY";
   --  Arabic, Morocco
   L_ar_MA      : aliased constant Wide_String := "ar_MA";
   --  Arabic, Oman
   L_ar_OM      : aliased constant Wide_String := "ar_OM";
   --  Arabic, Qatar
   L_ar_QA      : aliased constant Wide_String := "ar_QA";
   --  Arabic, Saudi Arabia
   L_ar_SA      : aliased constant Wide_String := "ar_SA";
   --  Arabic, Syria
   L_ar_SY      : aliased constant Wide_String := "ar_SY";
   --  Arabic, Tunisia
   L_ar_TN      : aliased constant Wide_String := "ar_TN";
   --  Arabic, Yemen
   L_ar_YE      : aliased constant Wide_String := "ar_YE";
   --  Armenian
   L_hy         : aliased constant Wide_String := "hy";
   --  Azeri, Latin
   L_az_Latn_AZ : aliased constant Wide_String := "az_Latn_AZ";
   --  Azeri, Cyrillic
   L_az_Cyrl_AZ : aliased constant Wide_String := "az_Cyrl_AZ";
   --  Basque (Basque)
   L_eu         : aliased constant Wide_String := "eu";
   --  Belarusian
   L_be         : aliased constant Wide_String := "be";
   --  Bulgarian
   L_bg         : aliased constant Wide_String := "bg";
   --  Catalan
   L_ca         : aliased constant Wide_String := "ca";
   --  Chinese, China
   L_zh_CN      : aliased constant Wide_String := "zh_CN";
   --  Chinese, Hong Kong
   L_zh_HK      : aliased constant Wide_String := "zh_HK";
   --  Chinese, Macau
   L_zh_MO      : aliased constant Wide_String := "zh_MO";
   --  Chinese, Singapore
   L_zh_SG      : aliased constant Wide_String := "zh_SG";
   --  Chinese, Taiwan
   L_zh_TW      : aliased constant Wide_String := "zh_TW";
   --  Croatian
   L_hr         : aliased constant Wide_String := "hr";
   --  Czech
   L_cs         : aliased constant Wide_String := "cs";
   --  Danish
   L_da         : aliased constant Wide_String := "da";
   --  Dutch, Netherlands
   L_nl_NL      : aliased constant Wide_String := "nl_NL";
   --  Dutch, Belgium
   L_nl_BE      : aliased constant Wide_String := "nl_BE";
   --  English, Australia
   L_en_AU      : aliased constant Wide_String := "en_AU";
   --  English, Belize
   L_en_BZ      : aliased constant Wide_String := "en_BZ";
   --  English, Canada
   L_en_CA      : aliased constant Wide_String := "en_CA";
   --  English, Caribbean
   L_en_029     : aliased constant Wide_String := "en_029";
   --  English, India
   L_en_IN      : aliased constant Wide_String := "en_IN";
   --  English, Ireland
   L_en_IE      : aliased constant Wide_String := "en_IE";
   --  English, Jamaica
   L_en_JM      : aliased constant Wide_String := "en_JM";
   --  English, Malaysia
   L_en_MY      : aliased constant Wide_String := "en_MY";
   --  English, New Zealand
   L_en_NZ      : aliased constant Wide_String := "en_NZ";
   --  English, Phillippines
   L_en_PH      : aliased constant Wide_String := "en_PH";
   --  English, Singapore
   L_en_SG      : aliased constant Wide_String := "en_SG";
   --  English, South Africa
   L_en_ZA      : aliased constant Wide_String := "en_ZA";
   --  English, Trinidad
   L_en_TT      : aliased constant Wide_String := "en_TT";
   --  English, Great Britain
   L_en_GB      : aliased constant Wide_String := "en_GB";
   --  English, United States
   L_en_US      : aliased constant Wide_String := "en_US";
   --  English, Zimbabwe
   L_en_ZW      : aliased constant Wide_String := "en_ZW";
   --  Estonian
   L_et         : aliased constant Wide_String := "et";
   --  Farsi
   L_fa         : aliased constant Wide_String := "fa";
   --  Finnish
   L_fi         : aliased constant Wide_String := "fi";
   --  Faroese
   L_fo         : aliased constant Wide_String := "fo";
   --  French, France
   L_fr_FR      : aliased constant Wide_String := "fr_FR";
   --  French, Belgium
   L_fr_BE      : aliased constant Wide_String := "fr_BE";
   --  French, Canada
   L_fr_CA      : aliased constant Wide_String := "fr_CA";
   --  French, Luxembourg
   L_fr_LU      : aliased constant Wide_String := "fr_LU";
   --  French, Switzerland
   L_fr_CH      : aliased constant Wide_String := "fr_CH";
   --  Gaelic, Ireland
   L_ga         : aliased constant Wide_String := "ga";
   --  Gaelic, Scotland
   L_gd         : aliased constant Wide_String := "gd";
   --  German, Germany
   L_de_DE      : aliased constant Wide_String := "de_DE";
   --  German, Austria
   L_de_AT      : aliased constant Wide_String := "de_AT";
   --  German, Liechtenstein
   L_de_LI      : aliased constant Wide_String := "de_LI";
   --  German, Luxembourg
   L_de_LU      : aliased constant Wide_String := "de_LU";
   --  German, Switzerland
   L_de_CH      : aliased constant Wide_String := "de_CH";
   --  Greek
   L_el         : aliased constant Wide_String := "el";
   --  Hebrew
   L_he         : aliased constant Wide_String := "he";
   --  Hindi
   L_hi         : aliased constant Wide_String := "hi";
   --  Hungarian
   L_hu         : aliased constant Wide_String := "hu";
   --  Icelandic
   L_is         : aliased constant Wide_String := "is";
   --  Indonesian
   L_id         : aliased constant Wide_String := "id";
   --  Italian, Italy
   L_it_IT      : aliased constant Wide_String := "it_IT";
   --  Italian, Switzerland
   L_it_CH      : aliased constant Wide_String := "it_CH";
   --  Japanese
   L_ja         : aliased constant Wide_String := "ja";
   --  Korean
   L_ko         : aliased constant Wide_String := "ko";
   --  Latvian
   L_lv         : aliased constant Wide_String := "lv";
   --  Lithuanian
   L_lt         : aliased constant Wide_String := "lt";
   --  F.Y.R.O. Macedonia
   L_mk         : aliased constant Wide_String := "mk";
   --  Malay, Malaysia
   L_ms_MY      : aliased constant Wide_String := "ms_MY";
   --  Malay – Brunei
   L_ms_BN      : aliased constant Wide_String := "ms_BN";
   --  Maltese
   L_mt         : aliased constant Wide_String := "mt";
   --  Marathi
   L_mr         : aliased constant Wide_String := "mr";
   --  Norwegian, Bokmål
   L_nb_NO      : aliased constant Wide_String := "nb_NO";
   --  Norwegian, Nynorsk
   L_nn_NO      : aliased constant Wide_String := "nn_NO";
   --  Polish
   L_pl         : aliased constant Wide_String := "pl";
   --  Portuguese, Portugal
   L_pt_PT      : aliased constant Wide_String := "pt_PT";
   --  Portuguese, Brazil
   L_pt_BR      : aliased constant Wide_String := "pt_BR";
   --  Raeto-Romance
   L_rm         : aliased constant Wide_String := "rm";
   --  Romanian, Romania
   L_ro         : aliased constant Wide_String := "ro";
   --  Romanian, Moldova
   L_ro_MO      : aliased constant Wide_String := "ro_MO";
   --  Russian
   L_ru         : aliased constant Wide_String := "ru";
   --  Russian, Moldova
   L_ru_MO      : aliased constant Wide_String := "ru_MO";
   --  Sanskrit
   L_sa         : aliased constant Wide_String := "sa";
   --  Serbian, Cyrillic
   L_sr_Cyrl_SP : aliased constant Wide_String := "sr_Cyrl_SP";
   --  Serbian, Latin
   L_sr_Latn_SP : aliased constant Wide_String := "sr_Latn_SP";
   --  Setsuana
   L_tn         : aliased constant Wide_String := "tn";
   --  Slovenian
   L_sl         : aliased constant Wide_String := "sl";
   --  Slovak
   L_sk         : aliased constant Wide_String := "sk";
   --  Sorbian
   L_sb         : aliased constant Wide_String := "sb";
   --  Spanish, Modern
   L_es_ES      : aliased constant Wide_String := "es_ES";
   --  Spanish, Traditional
   L_es         : aliased constant Wide_String := "es";
   --  Spanish, Argentina
   L_es_AR      : aliased constant Wide_String := "es_AR";
   --  Spanish, Bolivia
   L_es_BO      : aliased constant Wide_String := "es_BO";
   --  Spanish, Chile
   L_es_CL      : aliased constant Wide_String := "es_CL";
   --  Spanish, Colombia
   L_es_CO      : aliased constant Wide_String := "es_CO";
   --  Spanish, Costa Rica
   L_es_CR      : aliased constant Wide_String := "es_CR";
   --  Spanish, Dominican Rep
   L_es_DO      : aliased constant Wide_String := "es_DO";
   --  Spanish, Ecuador
   L_es_EC      : aliased constant Wide_String := "es_EC";
   --  Spanish, Guatemala
   L_es_GT      : aliased constant Wide_String := "es_GT";
   --  Spanish, Honduras
   L_es_HN      : aliased constant Wide_String := "es_HN";
   --  Spanish, Mexico
   L_es_MX      : aliased constant Wide_String := "es_MX";
   --  Spanish, Nicaragua
   L_es_NI      : aliased constant Wide_String := "es_NI";
   --  Spanish, Panama
   L_es_PA      : aliased constant Wide_String := "es_PA";
   --  Spanish, Peru
   L_es_PE      : aliased constant Wide_String := "es_PE";
   --  Spanish, Puerto Rico
   L_es_PR      : aliased constant Wide_String := "es_PR";
   --  Spanish, Paraguay
   L_es_PY      : aliased constant Wide_String := "es_PY";
   --  Spanish, El Salvador
   L_es_SV      : aliased constant Wide_String := "es_SV";
   --  Spanish, Uruguay
   L_es_UY      : aliased constant Wide_String := "es_UY";
   --  Spanish, Venezuela
   L_es_VE      : aliased constant Wide_String := "es_VE";
   --  Southern Sotho
   L_st         : aliased constant Wide_String := "st";
   --  Swahili
   L_sw         : aliased constant Wide_String := "sw";
   --  Swedish, Sweden
   L_sv_SE      : aliased constant Wide_String := "sv_SE";
   --  Swedish, Finland
   L_sv_FI      : aliased constant Wide_String := "sv_FI";
   --  Tamil
   L_ta         : aliased constant Wide_String := "ta";
   --  Tatar
   L_tt         : aliased constant Wide_String := "tt";
   --  Thai
   L_th         : aliased constant Wide_String := "th";
   --  Turkish
   L_tr         : aliased constant Wide_String := "tr";
   --  Tsonga
   L_ts         : aliased constant Wide_String := "ts";
   --  Ukrainian
   L_uk         : aliased constant Wide_String := "uk";
   --  Urdu
   L_ur         : aliased constant Wide_String := "ur";
   --  Uzbek, Cyrillic
   L_uz_Cyrl_UZ : aliased constant Wide_String := "uz_Cyrl_UZ";
   --  Uzbek, Latin
   L_uz_Latn_UZ : aliased constant Wide_String := "uz_Latn_UZ";
   --  Vietnamese
   L_vi         : aliased constant Wide_String := "vi";
   --  Xhosa
   L_xh         : aliased constant Wide_String := "xh";
   --  Yiddish
   L_yi         : aliased constant Wide_String := "yi";
   --  Zulu
   L_zu         : aliased constant Wide_String := "zu";

   --  Table mapping LCID value to name.  This table is sorted by LCID values.
   LCID_Mapping : constant array (Positive range <>) of LCID_Map_Type := (
                     (1025, L_ar_SA'Access),
                     (1026, L_bg'Access),
                     (1027, L_ca'Access),
                     (1028, L_zh_TW'Access),
                     (1029, L_cs'Access),
                     (1030, L_da'Access),
                     (1031, L_de_DE'Access),
                     (1032, L_el'Access),
                     (1033, L_en_US'Access),
                     (1034, L_es'Access),
                     (1035, L_fi'Access),
                     (1036, L_fr_FR'Access),
                     (1037, L_he'Access),
                     (1038, L_hu'Access),
                     (1039, L_is'Access),
                     (1040, L_it_IT'Access),
                     (1041, L_ja'Access),
                     (1042, L_ko'Access),
                     (1043, L_nl_NL'Access),
                     (1044, L_nb_NO'Access),
                     (1045, L_pl'Access),
                     (1046, L_pt_BR'Access),
                     (1047, L_rm'Access),
                     (1048, L_ro'Access),
                     (1049, L_ru'Access),
                     (1050, L_hr'Access),
                     (1051, L_sk'Access),
                     (1052, L_sq'Access),
                     (1053, L_sv_SE'Access),
                     (1054, L_th'Access),
                     (1055, L_tr'Access),
                     (1056, L_ur'Access),
                     (1057, L_id'Access),
                     (1058, L_uk'Access),
                     (1059, L_be'Access),
                     (1060, L_sl'Access),
                     (1061, L_et'Access),
                     (1062, L_lv'Access),
                     (1063, L_lt'Access),
                     (1065, L_fa'Access),
                     (1066, L_vi'Access),
                     (1067, L_hy'Access),
                     (1068, L_az_Latn_AZ'Access),
                     (1069, L_eu'Access),
                     (1070, L_sb'Access),
                     (1071, L_mk'Access),
                     (1072, L_st'Access),
                     (1073, L_ts'Access),
                     (1074, L_tn'Access),
                     (1076, L_xh'Access),
                     (1077, L_zu'Access),
                     (1078, L_af'Access),
                     (1080, L_fo'Access),
                     (1081, L_hi'Access),
                     (1082, L_mt'Access),
                     (1084, L_gd'Access),
                     (1085, L_yi'Access),
                     (1086, L_ms_MY'Access),
                     (1089, L_sw'Access),
                     (1091, L_uz_Latn_UZ'Access),
                     (1092, L_tt'Access),
                     (1097, L_ta'Access),
                     (1102, L_mr'Access),
                     (1103, L_sa'Access),
                     (2049, L_ar_IQ'Access),
                     (2052, L_zh_CN'Access),
                     (2055, L_de_CH'Access),
                     (2057, L_en_GB'Access),
                     (2058, L_es_MX'Access),
                     (2060, L_fr_BE'Access),
                     (2064, L_it_CH'Access),
                     (2067, L_nl_BE'Access),
                     (2068, L_nn_NO'Access),
                     (2070, L_pt_PT'Access),
                     (2072, L_ro_MO'Access),
                     (2073, L_ru_MO'Access),
                     (2074, L_sr_Latn_SP'Access),
                     (2077, L_sv_FI'Access),
                     (2092, L_az_Cyrl_AZ'Access),
                     (2108, L_ga'Access),
                     (2110, L_ms_BN'Access),
                     (2115, L_uz_Cyrl_UZ'Access),
                     (3073, L_ar_EG'Access),
                     (3076, L_zh_HK'Access),
                     (3079, L_de_AT'Access),
                     (3081, L_en_AU'Access),
                     (3082, L_es_ES'Access),
                     (3084, L_fr_CA'Access),
                     (3098, L_sr_Cyrl_SP'Access),
                     (4097, L_ar_LY'Access),
                     (4100, L_zh_SG'Access),
                     (4103, L_de_LU'Access),
                     (4105, L_en_CA'Access),
                     (4106, L_es_GT'Access),
                     (4108, L_fr_CH'Access),
                     (5121, L_ar_DZ'Access),
                     (5124, L_zh_MO'Access),
                     (5127, L_de_LI'Access),
                     (5129, L_en_NZ'Access),
                     (5130, L_es_CR'Access),
                     (5132, L_fr_LU'Access),
                     (6145, L_ar_MA'Access),
                     (6153, L_en_IE'Access),
                     (6154, L_es_PA'Access),
                     (7169, L_ar_TN'Access),
                     (7177, L_en_ZA'Access),
                     (7178, L_es_DO'Access),
                     (8193, L_ar_OM'Access),
                     (8201, L_en_JM'Access),
                     (8202, L_es_VE'Access),
                     (9217, L_ar_YE'Access),
                     (9225, L_en_029'Access),
                     (9226, L_es_CO'Access),
                     (10241, L_ar_SY'Access),
                     (10249, L_en_BZ'Access),
                     (10250, L_es_PE'Access),
                     (11265, L_ar_JO'Access),
                     (11273, L_en_TT'Access),
                     (11274, L_es_AR'Access),
                     (12289, L_ar_LB'Access),
                     (12297, L_en_ZW'Access),
                     (12298, L_es_EC'Access),
                     (13313, L_ar_KW'Access),
                     (13321, L_en_PH'Access),
                     (13322, L_es_CL'Access),
                     (14337, L_ar_AE'Access),
                     (14346, L_es_UY'Access),
                     (15361, L_ar_BH'Access),
                     (15370, L_es_PY'Access),
                     (16385, L_ar_QA'Access),
                     (16393, L_en_IN'Access),
                     (16394, L_es_BO'Access),
                     (17417, L_en_MY'Access),
                     (17418, L_es_SV'Access),
                     (18441, L_en_SG'Access),
                     (18442, L_es_HN'Access),
                     (19466, L_es_NI'Access),
                     (20490, L_es_PR'Access));

   function LCID_To_Locale (Value : LCID) return Wide_String;
   --  Use the LCID_Mapping table to locate the name corresponding to an
   --  LCID value (simple binary search).  If not found, return the empty
   --  string.

   --------------------
   -- LCID_To_Locale --
   --------------------

   function LCID_To_Locale (Value : LCID) return Wide_String is

      Left      : Positive := LCID_Mapping'First;
      Right     : Positive := LCID_Mapping'Last + 1;
      Center    : Positive;
      Candidate : LCID;

   begin
      if Value < LCID_Mapping (Left).Value then
         return "";
      end if;
      loop
         Center := Left + (Right - Left) / 2;
         Candidate := LCID_Mapping (Center).Value;
         if Value = Candidate then
            return LCID_Mapping (Center).Name.all;
         end if;

         if Right - Left <= 1 then
            return "";
         elsif Value < Candidate then
            Right := Center;
         else
            Left := Center;
         end if;
      end loop;
   end LCID_To_Locale;

   --------------------
   -- OS_Locale_Name --
   --------------------

   function OS_Locale_Name return Wide_String is
   begin
      return LCID_To_Locale (GetUserDefaultLCID);
   end OS_Locale_Name;

   -------------
   -- OS_Name --
   -------------

   function OS_Name return Wide_String is
   begin
      return "Windows_NT";
   end OS_Name;

end ZanyBlue.OS;
