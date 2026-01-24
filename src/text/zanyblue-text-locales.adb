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

with Ada.Strings.Wide_Fixed;
with Ada.Environment_Variables;
with ZanyBlue.OS;
with ZanyBlue.Text.Utils;

package body ZanyBlue.Text.Locales is

   use Ada.Strings.Wide_Fixed;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Utils;

   Max_Tag_Length : constant := Max_Language_Length
                              + Max_Script_Length
                              + Max_Territory_Length;
   subtype Tag_Type is Wide_String (1 .. Max_Tag_Length);
   --  Internal locale identification: Lang + Script + Terr padded with
   --  spaces, e.g., "EN LATNUS " for "en_Latn_US".

   type String_Index_Type is new Positive;
   --  Strings are accessed via a simple table giving start and ending indexes
   --  of the string within the "global" string pool.  Each string is stored
   --  within locale structures as a index into this table.

   type String_Address_Type is
      record
         First : Positive;
         Last  : Natural;
      end record;
   --  Start and end of a string with the pool

   type String_Addresses_Type is
       array (String_Index_Type range <>) of String_Address_Type;
   --  The collection of strings defined for all locales accessed by index.

   type Month_Names_Type is array (Month_Type) of String_Index_Type;
   --  List of strings (by index) for month names in a locale.

   type Day_Names_Type is array (Day_Type) of String_Index_Type;
   --  List of strings (by index) for day names in a locale.

   type Day_Period_Names_Type is array (Day_Period_Type) of String_Index_Type;
   --  List of strings (by index) for day period names in a locale.

   type Era_Names_Type is array (Era_Type) of String_Index_Type;
   --  List of strings (by index) for era names in a locale.

   type Date_Time_Styles_Type is
      array (Date_Time_Style_Type) of String_Index_Type;
   --  List of strings (by index) for various time/date formats in a locale.

   type Numeric_Items_Type is array (Numeric_Item_Type) of String_Index_Type;
   --  List of strings (by index) for various numeric items in a locale.

   type Numeric_Format_Type is array (Numeric_Style_Type) of String_Index_Type;
   --  List of strings (by index) for various numeric formats in a locale.

   type Locale_Traits_Type is
      record
         Tag                : Tag_Type;
         Level              : Level_Type;
         Name               : String_Index_Type;
         Short_Month_Names  : Month_Names_Type;
         Full_Month_Names   : Month_Names_Type;
         Short_Day_Names    : Day_Names_Type;
         Full_Day_Names     : Day_Names_Type;
         Day_Period_Names   : Day_Period_Names_Type;
         Era_Names          : Era_Names_Type;
         Date_Formats       : Date_Time_Styles_Type;
         Time_Formats       : Date_Time_Styles_Type;
         Date_Time_Formats  : Date_Time_Styles_Type;
         Numeric_Items      : Numeric_Items_Type;
         Numeric_Formats    : Numeric_Format_Type;
      end record;
   --  Collection of strings used for each locale.

   Current_Locale_Value : Locale_Type;
   --  Current locale initialized by the environment (variable or underlying
   --  OS defintion).

   --------------------------------------------------------------------------
   --  The following data structures are generated from the Unicode.org    --
   --  CLDR data and inserted into this file.  Please do not edit this     --
   --  data.  The markers on the below is used by the edit process and     --
   --  should not be removed.                                              --
   --                                                                      --
   --  BEGIN-CLDR-DATA                                                     --

   Pool : constant Wide_String := ""
             & "SundayMondayTuesdayWednesdayThursdayFridaySaturdaySunMonTueW"
             & "edThuFriSatJanuaryFebruaryMarchAprilMayJuneJulyAugustSeptemb"
             & "erOctoberNovemberDecemberJanFebMarAprJunJulAugSepOctNovDecAM"
             & "noonPMBCECE.,;0+-E%‰∞NaN#EEEE, MMMM d, yMMMM d, yMMM d, yM/d"
             & "/yyh:mm:ss a zzzzh:mm:ss a zh:mm:ss ah:mm a{1} {0}#,##0.####"
             & "E0#,##0%¤#,##0.00;(¤#,##0.00)arأحدإثنينثلاثاءأربعاءخميسجمعةس"
             & "بتالأحدالإثنينالثلاثاءالأربعاءالخميسالجمعةالسبتينايرفبرايرما"
             & "رسأبريلمايويونيويوليوأغسطسسبتمبرأكتوبرنوفمبرديسمبرصمق.م٫٬؛٪ا"
             & "س؉ليس رقم#,##0.###;#,##0.###-¤ #,##0.00;¤ #,##0.00-EEEE، d M"
             & "MMM، yd MMMM، ydd‏/MM‏/yyyyd‏/M‏/yyyyzzzz h:mm:ss az h:mm:ss"
             & " acsnepoútstčtpásonedělepondělíúterýstředačtvrtekpáteksobota"
             & "123456789101112lednaúnorabřeznadubnakvětnačervnačervencesrpn"
             & "azáříříjnalistopaduprosincedop.odp.př.Kr.po Kr. #,##0 %#,##0"
             & ".00 ¤EEEE, d. MMMM yd. MMMM yd.M.yyyyd.M.yyH:mm:ss zzzzH:mm:"
             & "ss zH:mm:ssH:mmdasønmantironstorfrelørsøndagmandagtirsdagons"
             & "dagtorsdagfredaglørdagjan.feb.mar.apr.majjun.jul.aug.sep.okt"
             & ".nov.dec.januarfebruarmartsapriljunijuliaugustseptemberoktob"
             & "ernovemberdecemberf.m.e.m.f.Kr.e.Kr.EEEE 'den' d. MMMM yd. M"
             & "MM ydd/MM/yyyydd/MM/yyHH.mm.ss zzzzHH.mm.ss zHH.mm.ssHH.mmde"
             & "So.Mo.Di.Mi.Do.Fr.Sa.SonntagMontagDienstagMittwochDonnerstag"
             & "FreitagSamstagMärMaiOktDezJanuarFebruarMärzJuniJuliOktoberDe"
             & "zembervorm.Mittagnachm.v. Chr.n. Chr.dd.MM.yyyydd.MM.yyHH:mm"
             & ":ss zzzzHH:mm:ss zHH:mm:ssHH:mmelΚυρΔευΤριΤετΠεμΠαρΣαβΚυριακ"
             & "ήΔευτέραΤρίτηΤετάρτηΠέμπτηΠαρασκευήΣάββατοΙανΦεβΜαρΑπρΜαϊΙου"
             & "νΙουλΑυγΣεπΟκτΝοεΔεκΙανουαρίουΦεβρουαρίουΜαρτίουΑπριλίουΜαΐο"
             & "υΙουνίουΙουλίουΑυγούστουΣεπτεμβρίουΟκτωβρίουΝοεμβρίουΔεκεμβρ"
             & "ίουπ.μ.μ.μ.π.Χ.μ.Χ.eEEEE, d MMMM yd MMMM yd MMM yd/M/yyenBCA"
             & "D0.0K00K000K0.0M00M000M0.0B00B000B0.0T00T000Ten_AU¤#,##0.00d"
             & "/MM/yyen_IEa.m.p.m.EEEE d MMMM yen_GBesdomlunmarmiéjueviesáb"
             & "domingolunesmartesmiércolesjuevesviernessábadoenefebabrmayju"
             & "njulagosepoctnovdicenerofebreromarzoabrilmayojuniojulioagost"
             & "oseptiembreoctubrenoviembrediciembrea.C.d.C.¤ #,##0.00EEEE d"
             & " 'de' MMMM 'de' yd 'de' MMMM 'de' yfisumatiketopelasunnuntai"
             & "namaanantainatiistainakeskiviikkonatorstainaperjantainalauan"
             & "tainatammikuutahelmikuutamaaliskuutahuhtikuutatoukokuutakesä"
             & "kuutaheinäkuutaelokuutasyyskuutalokakuutamarraskuutajoulukuu"
             & "taap.ip.eKr.jKr.epälukucccc d. MMMM yH.mm.ss zzzzH.mm.ss zH."
             & "mm.ssH.mmfrdim.lun.mer.jeu.ven.sam.dimanchelundimardimercred"
             & "ijeudivendredisamedijanv.févr.marsavr.maijuinjuil.aoûtsept.o"
             & "ct.déc.janvierfévrieravriljuilletseptembreoctobrenovembredéc"
             & "embremidiav. J.-C.ap. J.-C.iwheיום א׳יום ב׳יום ג׳יום ד׳יום ה"
             & "׳יום ו׳שבתיום ראשוןיום שנייום שלישייום רביעייום חמישייום שיש"
             & "ייום שבתינופברמרסאפרמאייוניולאוגספטאוקנובדצמינוארפברואראפריל"
             & "יונייוליאוגוסטספטמבראוקטוברנובמברדצמברלפנה״צאחה״צלפנה״סלסה״נ"
             & "EEEE, d בMMMM yd בMMMM yd בMMM yyyyhuVHKSzeCsPSzovasárnaphét"
             & "főkeddszerdacsütörtökpéntekszombatfebr.márc.ápr.máj.jún.júl."
             & "szept.januárfebruármárciusáprilismájusjúniusjúliusaugusztuss"
             & "zeptemberoktóberde.du.i. e.i. sz.y. MMMM d., EEEEy. MMMM d.y"
             & "yyy.MM.dd.itmergiovensabdomenicalunedìmartedìmercoledìgioved"
             & "ìvenerdìsabatogenaprmaggiulugsetottgennaiofebbraioaprilemagg"
             & "iogiugnolugliosettembreottobredicembrem.p.aCdCdd MMMM ydd/MM"
             & "M/yja日月火水木金土日曜日月曜日火曜日水曜日木曜日金曜日土曜日1月2月3月4月5月6月7月8月9月10月11月12月"
             & "午前午後y年M月d日EEEEy年M月d日yyyy/MM/ddyy/MM/ddH時mm分ss秒 zzzz{1}{0}ko일"
             & "월화수목금토일요일월요일화요일수요일목요일금요일토요일1월2월3월4월5월6월7월8월9월10월11월12월오전오후기원"
             & "전서기y년 M월 d일 EEEEy년 M월 d일yyyy. M. d.yy. M. d.a h시 m분 s초 zzzza"
             & " h시 m분 s초 za h:mm:ssa h:mmnlzodiwodovrzazondagmaandagdinsdag"
             & "woensdagdonderdagvrijdagzaterdagmrt.meijanuarifebruarimaarta"
             & "ugustusdd-MM-yynonbplniedz.pon.wt.śr.czw.pt.sob.niedzielapon"
             & "iedziałekwtorekśrodaczwartekpiątekstylutkwiczelipsiewrzpaźli"
             & "sgrustycznialutegomarcakwietniamajaczerwcalipcasierpniawrześ"
             & "niapaździernikalistopadagrudniaw południep.n.e.n.e.{1}, {0}p"
             & "tsegterquaquisexsegunda-feiraterça-feiraquarta-feiraquinta-f"
             & "eirasexta-feirajanfevoutdezjaneirofevereiromarçomaiojunhojul"
             & "hosetembrooutubronovembrodezembromeio-diaEEEE, d 'de' MMMM '"
             & "de' yHH'h'mm'min'ss's' zzzzHH'h'mm'min'ss's' zroDuLuMaMiJoVi"
             & "Sâduminicălunimarțimiercurijoivinerisâmbătăian.iun.iul.ianua"
             & "riefebruariemartieaprilieiunieiulieseptembrieoctombrienoiemb"
             & "riedecembrieî.Hr.d.Hr.ruвспнвтсрчтптсбвоскресеньепонедельник"
             & "вторниксредачетвергпятницасубботаянв.февр.мартаапр.маяиюняию"
             & "ляавг.сент.окт.нояб.дек.январяфевраляапреляавгустасентябряок"
             & "тябряноябрядекабрядо н.э.н.э.EEEE, d MMMM y 'г'.d MMMM y 'г'"
             & ".skutštpinedeľapondelokutorokstredaštvrtokpiatokmájjúnjúlaug"
             & "oktdecjanuárafebruáraaprílamájajúnajúlaaugustaseptembraoktób"
             & "ranovembradecembradopoludniapopoludnípred n.l.n.l.svsönmånti"
             & "storslörsöndagmåndagtisdaglördagaugustifmem−×10^¤¤¤EEEE'en' "
             & "'den' d:'e' MMMM yyyyy-MM-dd'kl'. HH:mm:ss zzzzthอา.จ.อ.พ.พฤ"
             & ".ศ.ส.วันอาทิตย์วันจันทร์วันอังคารวันพุธวันพฤหัสบดีวันศุกร์วั"
             & "นเสาร์ม.ค.ก.พ.มี.ค.เม.ย.พ.ค.มิ.ย.ก.ค.ส.ค.ก.ย.ต.ค.พ.ย.ธ.ค.มกร"
             & "าคมกุมภาพันธ์มีนาคมเมษายนพฤษภาคมมิถุนายนกรกฎาคมสิงหาคมกันยาย"
             & "นตุลาคมพฤศจิกายนธันวาคมก่อนเที่ยงหลังเที่ยงปีก่อน ค.ศ.ค.ศ.¤#"
             & ",##0.00;¤-#,##0.00EEEEที่ d MMMM G yd/M/yyyyH นาฬิกา m นาที "
             & "ss วินาที zzzzH นาฬิกา m นาที ss วินาที ztrPazPztSalÇarPerCu"
             & "mCmtPazarPazartesiSalıÇarşambaPerşembeCumaCumartesiOcaŞubNis"
             & "HazTemAğuEylEkiKasAraOcakŞubatMartNisanMayısHaziranTemmuzAğu"
             & "stosEylülEkimKasımAralıkMÖMS% #,##0dd MMMM y EEEEdd MMM yzh周"
             & "日周一周二周三周四周五周六星期日星期一星期二星期三星期四星期五星期六上午下午公元前公元yyyy-M-dyy-M-dzzz"
             & "zah时mm分ss秒zah时mm分ss秒ah:mm:ssah:mmzh_Hant週日週一週二週三週四週五週六西元前西元y"
             & "yyy/M/dyy/M/dzzzzah時mm分ss秒zah時mm分ss秒søn.man.tir.ons.tor.fre."
             & "lør.des.desemberEEEE d. MMMM y"
             & "";

   String_Addresses : constant String_Addresses_Type := (
                         --  1: ""
                         (1, 0),
                         --  2: "#"
                         (205, 205),
                         --  3: "#,##0%"
                         (303, 308),
                         --  4: "#,##0.###"
                         (291, 299),
                         --  5: "#,##0.###;#,##0.###-"
                         (490, 509),
                         --  6: "#,##0.00 ¤"
                         (776, 785),
                         --  7: "#,##0 %"
                         (769, 775),
                         --  8: "#E0"
                         (300, 302),
                         --  9: "%"
                         (199, 199),
                         --  10: "% #,##0"
                         (5069, 5075),
                         --  11: "'kl'. HH:mm:ss zzzz"
                         (4529, 4547),
                         --  12: "+"
                         (196, 196),
                         --  13: ","
                         (193, 193),
                         --  14: "-"
                         (197, 197),
                         --  15: "."
                         (192, 192),
                         --  16: "0"
                         (195, 195),
                         --  17: "0.0B"
                         (1644, 1647),
                         --  18: "0.0K"
                         (1622, 1625),
                         --  19: "0.0M"
                         (1633, 1636),
                         --  20: "0.0T"
                         (1655, 1658),
                         --  21: "000B"
                         (1651, 1654),
                         --  22: "000K"
                         (1629, 1632),
                         --  23: "000M"
                         (1640, 1643),
                         --  24: "000T"
                         (1662, 1665),
                         --  25: "00B"
                         (1648, 1650),
                         --  26: "00K"
                         (1626, 1628),
                         --  27: "00M"
                         (1637, 1639),
                         --  28: "00T"
                         (1659, 1661),
                         --  29: "1"
                         (661, 661),
                         --  30: "10"
                         (670, 671),
                         --  31: "10月"
                         (3112, 3114),
                         --  32: "10월"
                         (3226, 3228),
                         --  33: "11"
                         (672, 673),
                         --  34: "11月"
                         (3115, 3117),
                         --  35: "11월"
                         (3229, 3231),
                         --  36: "12"
                         (674, 675),
                         --  37: "12月"
                         (3118, 3120),
                         --  38: "12월"
                         (3232, 3234),
                         --  39: "1月"
                         (3094, 3095),
                         --  40: "1월"
                         (3208, 3209),
                         --  41: "2"
                         (662, 662),
                         --  42: "2月"
                         (3096, 3097),
                         --  43: "2월"
                         (3210, 3211),
                         --  44: "3"
                         (663, 663),
                         --  45: "3月"
                         (3098, 3099),
                         --  46: "3월"
                         (3212, 3213),
                         --  47: "4"
                         (664, 664),
                         --  48: "4月"
                         (3100, 3101),
                         --  49: "4월"
                         (3214, 3215),
                         --  50: "5"
                         (665, 665),
                         --  51: "5月"
                         (3102, 3103),
                         --  52: "5월"
                         (3216, 3217),
                         --  53: "6"
                         (666, 666),
                         --  54: "6月"
                         (3104, 3105),
                         --  55: "6월"
                         (3218, 3219),
                         --  56: "7"
                         (667, 667),
                         --  57: "7月"
                         (3106, 3107),
                         --  58: "7월"
                         (3220, 3221),
                         --  59: "8"
                         (668, 668),
                         --  60: "8月"
                         (3108, 3109),
                         --  61: "8월"
                         (3222, 3223),
                         --  62: "9"
                         (669, 669),
                         --  63: "9月"
                         (3110, 3111),
                         --  64: "9월"
                         (3224, 3225),
                         --  65: ";"
                         (194, 194),
                         --  66: "AD"
                         (1620, 1621),
                         --  67: "AM"
                         (179, 180),
                         --  68: "Apr"
                         (155, 157),
                         --  69: "April"
                         (92, 96),
                         --  70: "Ara"
                         (4999, 5001),
                         --  71: "Aralık"
                         (5059, 5064),
                         --  72: "Aug"
                         (164, 166),
                         --  73: "August"
                         (108, 113),
                         --  74: "Ağu"
                         (4987, 4989),
                         --  75: "Ağustos"
                         (5038, 5044),
                         --  76: "BC"
                         (1618, 1619),
                         --  77: "BCE"
                         (187, 189),
                         --  78: "CE"
                         (190, 191),
                         --  79: "Cmt"
                         (4922, 4924),
                         --  80: "Cs"
                         (2684, 2685),
                         --  81: "Cum"
                         (4919, 4921),
                         --  82: "Cuma"
                         (4959, 4962),
                         --  83: "Cumartesi"
                         (4963, 4971),
                         --  84: "Dec"
                         (176, 178),
                         --  85: "December"
                         (138, 145),
                         --  86: "Dez"
                         (1224, 1226),
                         --  87: "Dezember"
                         (1259, 1266),
                         --  88: "Di."
                         (1147, 1149),
                         --  89: "Dienstag"
                         (1175, 1182),
                         --  90: "Do."
                         (1153, 1155),
                         --  91: "Donnerstag"
                         (1191, 1200),
                         --  92: "Du"
                         (3889, 3890),
                         --  93: "E"
                         (198, 198),
                         --  94: "EEEE 'den' d. MMMM y"
                         (1057, 1076),
                         --  95: "EEEE d 'de' MMMM 'de' y"
                         (1915, 1937),
                         --  96: "EEEE d MMMM y"
                         (1700, 1712),
                         --  97: "EEEE d. MMMM y"
                         (5297, 5310),
                         --  98: "EEEE'en' 'den' d:'e' MMMM y"
                         (4492, 4518),
                         --  99: "EEEE, MMMM d, y"
                         (206, 220),
                         --  100: "EEEE, d 'de' MMMM 'de' y"
                         (3822, 3845),
                         --  101: "EEEE, d MMMM y"
                         (1581, 1594),
                         --  102: "EEEE, d MMMM y 'г'."
                         (4230, 4248),
                         --  103: "EEEE, d בMMMM y"
                         (2641, 2655),
                         --  104: "EEEE, d. MMMM y"
                         (786, 800),
                         --  105: "EEEE، d MMMM، y"
                         (532, 546),
                         --  106: "EEEEที่ d MMMM G y"
                         (4819, 4836),
                         --  107: "Eki"
                         (4993, 4995),
                         --  108: "Ekim"
                         (5050, 5053),
                         --  109: "Eyl"
                         (4990, 4992),
                         --  110: "Eylül"
                         (5045, 5049),
                         --  111: "Feb"
                         (149, 151),
                         --  112: "Februar"
                         (1233, 1239),
                         --  113: "February"
                         (79, 86),
                         --  114: "Fr."
                         (1156, 1158),
                         --  115: "Freitag"
                         (1201, 1207),
                         --  116: "Fri"
                         (66, 68),
                         --  117: "Friday"
                         (37, 42),
                         --  118: "H"
                         (2679, 2679),
                         --  119: "H นาฬิกา m นาที ss วินาที z"
                         (4875, 4901),
                         --  120: "H นาฬิกา m นาที ss วินาที zzzz"
                         (4845, 4874),
                         --  121: "H.mm"
                         (2226, 2229),
                         --  122: "H.mm.ss"
                         (2219, 2225),
                         --  123: "H.mm.ss z"
                         (2210, 2218),
                         --  124: "H.mm.ss zzzz"
                         (2198, 2209),
                         --  125: "H:mm"
                         (852, 855),
                         --  126: "H:mm:ss"
                         (845, 851),
                         --  127: "H:mm:ss z"
                         (836, 844),
                         --  128: "H:mm:ss zzzz"
                         (824, 835),
                         --  129: "HH'h'mm'min'ss's' z"
                         (3868, 3886),
                         --  130: "HH'h'mm'min'ss's' zzzz"
                         (3846, 3867),
                         --  131: "HH.mm"
                         (1134, 1138),
                         --  132: "HH.mm.ss"
                         (1126, 1133),
                         --  133: "HH.mm.ss z"
                         (1116, 1125),
                         --  134: "HH.mm.ss zzzz"
                         (1103, 1115),
                         --  135: "HH:mm"
                         (1347, 1351),
                         --  136: "HH:mm:ss"
                         (1339, 1346),
                         --  137: "HH:mm:ss z"
                         (1329, 1338),
                         --  138: "HH:mm:ss zzzz"
                         (1316, 1328),
                         --  139: "Haz"
                         (4981, 4983),
                         --  140: "Haziran"
                         (5025, 5031),
                         --  141: "H時mm分ss秒 zzzz"
                         (3159, 3171),
                         --  142: "Jan"
                         (146, 148),
                         --  143: "Januar"
                         (1227, 1232),
                         --  144: "January"
                         (72, 78),
                         --  145: "Jo"
                         (3897, 3898),
                         --  146: "Jul"
                         (161, 163),
                         --  147: "Juli"
                         (1248, 1251),
                         --  148: "July"
                         (104, 107),
                         --  149: "Jun"
                         (158, 160),
                         --  150: "June"
                         (100, 103),
                         --  151: "Juni"
                         (1244, 1247),
                         --  152: "K"
                         (2680, 2680),
                         --  153: "Kas"
                         (4996, 4998),
                         --  154: "Kasım"
                         (5054, 5058),
                         --  155: "Lu"
                         (3891, 3892),
                         --  156: "M/d/yy"
                         (238, 243),
                         --  157: "MMM d, y"
                         (230, 237),
                         --  158: "MMMM d, y"
                         (221, 229),
                         --  159: "MS"
                         (5067, 5068),
                         --  160: "Ma"
                         (3893, 3894),
                         --  161: "Mai"
                         (1218, 1220),
                         --  162: "Mar"
                         (152, 154),
                         --  163: "March"
                         (87, 91),
                         --  164: "Mart"
                         (5011, 5014),
                         --  165: "May"
                         (97, 99),
                         --  166: "Mayıs"
                         (5020, 5024),
                         --  167: "Mi"
                         (3895, 3896),
                         --  168: "Mi."
                         (1150, 1152),
                         --  169: "Mittag"
                         (1272, 1277),
                         --  170: "Mittwoch"
                         (1183, 1190),
                         --  171: "Mo."
                         (1144, 1146),
                         --  172: "Mon"
                         (54, 56),
                         --  173: "Monday"
                         (7, 12),
                         --  174: "Montag"
                         (1169, 1174),
                         --  175: "MÖ"
                         (5065, 5066),
                         --  176: "Mär"
                         (1215, 1217),
                         --  177: "März"
                         (1240, 1243),
                         --  178: "NaN"
                         (202, 204),
                         --  179: "Nis"
                         (4978, 4980),
                         --  180: "Nisan"
                         (5015, 5019),
                         --  181: "Nov"
                         (173, 175),
                         --  182: "November"
                         (130, 137),
                         --  183: "Oca"
                         (4972, 4974),
                         --  184: "Ocak"
                         (5002, 5005),
                         --  185: "Oct"
                         (170, 172),
                         --  186: "October"
                         (123, 129),
                         --  187: "Okt"
                         (1221, 1223),
                         --  188: "Oktober"
                         (1252, 1258),
                         --  189: "P"
                         (2686, 2686),
                         --  190: "PM"
                         (185, 186),
                         --  191: "Paz"
                         (4904, 4906),
                         --  192: "Pazar"
                         (4925, 4929),
                         --  193: "Pazartesi"
                         (4930, 4938),
                         --  194: "Per"
                         (4916, 4918),
                         --  195: "Perşembe"
                         (4951, 4958),
                         --  196: "Pzt"
                         (4907, 4909),
                         --  197: "Sa."
                         (1159, 1161),
                         --  198: "Sal"
                         (4910, 4912),
                         --  199: "Salı"
                         (4939, 4942),
                         --  200: "Samstag"
                         (1208, 1214),
                         --  201: "Sat"
                         (69, 71),
                         --  202: "Saturday"
                         (43, 50),
                         --  203: "Sep"
                         (167, 169),
                         --  204: "September"
                         (114, 122),
                         --  205: "So."
                         (1141, 1143),
                         --  206: "Sonntag"
                         (1162, 1168),
                         --  207: "Sun"
                         (51, 53),
                         --  208: "Sunday"
                         (1, 6),
                         --  209: "Sze"
                         (2681, 2683),
                         --  210: "Szo"
                         (2687, 2689),
                         --  211: "Sâ"
                         (3901, 3902),
                         --  212: "Tem"
                         (4984, 4986),
                         --  213: "Temmuz"
                         (5032, 5037),
                         --  214: "Thu"
                         (63, 65),
                         --  215: "Thursday"
                         (29, 36),
                         --  216: "Tue"
                         (57, 59),
                         --  217: "Tuesday"
                         (13, 19),
                         --  218: "V"
                         (2678, 2678),
                         --  219: "Vi"
                         (3899, 3900),
                         --  220: "Wed"
                         (60, 62),
                         --  221: "Wednesday"
                         (20, 28),
                         --  222: "a h:mm"
                         (3321, 3326),
                         --  223: "a h:mm:ss"
                         (3312, 3320),
                         --  224: "a h시 m분 s초 z"
                         (3300, 3311),
                         --  225: "a h시 m분 s초 zzzz"
                         (3285, 3299),
                         --  226: "a.C."
                         (1897, 1900),
                         --  227: "a.m."
                         (1692, 1695),
                         --  228: "aC"
                         (3043, 3044),
                         --  229: "abr"
                         (1793, 1795),
                         --  230: "abril"
                         (1837, 1841),
                         --  231: "ago"
                         (1805, 1807),
                         --  232: "agosto"
                         (1856, 1861),
                         --  233: "ah:mm"
                         (5189, 5193),
                         --  234: "ah:mm:ss"
                         (5181, 5188),
                         --  235: "août"
                         (2331, 2334),
                         --  236: "ap."
                         (2163, 2165),
                         --  237: "ap. J.-C."
                         (2419, 2427),
                         --  238: "apr"
                         (2958, 2960),
                         --  239: "apr."
                         (935, 938),
                         --  240: "april"
                         (988, 992),
                         --  241: "aprile"
                         (2991, 2996),
                         --  242: "aprilie"
                         (3979, 3985),
                         --  243: "apríla"
                         (4342, 4347),
                         --  244: "ar"
                         (330, 331),
                         --  245: "aug"
                         (4318, 4320),
                         --  246: "aug."
                         (950, 953),
                         --  247: "august"
                         (1001, 1006),
                         --  248: "augusta"
                         (4360, 4366),
                         --  249: "augusti"
                         (4473, 4479),
                         --  250: "augustus"
                         (3420, 3427),
                         --  251: "augusztus"
                         (2811, 2819),
                         --  252: "av. J.-C."
                         (2410, 2418),
                         --  253: "avr."
                         (2315, 2318),
                         --  254: "avril"
                         (2362, 2366),
                         --  255: "března"
                         (686, 691),
                         --  256: "cccc d. MMMM y"
                         (2184, 2197),
                         --  257: "cs"
                         (603, 604),
                         --  258: "csütörtök"
                         (2713, 2721),
                         --  259: "cze"
                         (3524, 3526),
                         --  260: "czerwca"
                         (3576, 3582),
                         --  261: "czw."
                         (3458, 3461),
                         --  262: "czwartek"
                         (3501, 3508),
                         --  263: "d 'de' MMMM 'de' y"
                         (1938, 1955),
                         --  264: "d MMM y"
                         (1603, 1609),
                         --  265: "d MMMM y"
                         (1595, 1602),
                         --  266: "d MMMM y 'г'."
                         (4249, 4261),
                         --  267: "d MMMM، y"
                         (547, 555),
                         --  268: "d בMMM yyyy"
                         (2665, 2675),
                         --  269: "d בMMMM y"
                         (2656, 2664),
                         --  270: "d. MMM y"
                         (1077, 1084),
                         --  271: "d. MMMM y"
                         (801, 809),
                         --  272: "d.C."
                         (1901, 1904),
                         --  273: "d.Hr."
                         (4038, 4042),
                         --  274: "d.M.yy"
                         (818, 823),
                         --  275: "d.M.yyyy"
                         (810, 817),
                         --  276: "d/M/yy"
                         (1610, 1615),
                         --  277: "d/M/yyyy"
                         (4837, 4844),
                         --  278: "d/MM/yy"
                         (1680, 1686),
                         --  279: "dC"
                         (3045, 3046),
                         --  280: "da"
                         (856, 857),
                         --  281: "dd MMM y"
                         (5090, 5097),
                         --  282: "dd MMMM y"
                         (3047, 3055),
                         --  283: "dd MMMM y EEEE"
                         (5076, 5089),
                         --  284: "dd-MM-yy"
                         (3428, 3435),
                         --  285: "dd.MM.yy"
                         (1308, 1315),
                         --  286: "dd.MM.yyyy"
                         (1298, 1307),
                         --  287: "dd/MM/yy"
                         (1095, 1102),
                         --  288: "dd/MM/yyyy"
                         (1085, 1094),
                         --  289: "dd/MMM/y"
                         (3056, 3063),
                         --  290: "dd‏/MM‏/yyyy"
                         (556, 567),
                         --  291: "de"
                         (1139, 1140),
                         --  292: "de."
                         (2837, 2839),
                         --  293: "dec"
                         (4324, 4326),
                         --  294: "dec."
                         (966, 969),
                         --  295: "december"
                         (1031, 1038),
                         --  296: "decembra"
                         (4391, 4398),
                         --  297: "decembrie"
                         (4024, 4032),
                         --  298: "des."
                         (5285, 5288),
                         --  299: "desember"
                         (5289, 5296),
                         --  300: "dez"
                         (3745, 3747),
                         --  301: "dezembro"
                         (3806, 3813),
                         --  302: "di"
                         (3331, 3332),
                         --  303: "dic"
                         (1817, 1819),
                         --  304: "dicembre"
                         (3031, 3038),
                         --  305: "diciembre"
                         (1888, 1896),
                         --  306: "dim."
                         (2232, 2235),
                         --  307: "dimanche"
                         (2256, 2263),
                         --  308: "dinsdag"
                         (3354, 3360),
                         --  309: "do"
                         (3335, 3336),
                         --  310: "dom"
                         (1720, 1722),
                         --  311: "domenica"
                         (2905, 2912),
                         --  312: "domingo"
                         (1741, 1747),
                         --  313: "donderdag"
                         (3369, 3377),
                         --  314: "dop."
                         (748, 751),
                         --  315: "dopoludnia"
                         (4399, 4408),
                         --  316: "du."
                         (2840, 2842),
                         --  317: "dubna"
                         (692, 696),
                         --  318: "duminică"
                         (3903, 3910),
                         --  319: "déc."
                         (2344, 2347),
                         --  320: "décembre"
                         (2398, 2405),
                         --  321: "d‏/M‏/yyyy"
                         (568, 577),
                         --  322: "e"
                         (1580, 1580),
                         --  323: "e.Kr."
                         (1052, 1056),
                         --  324: "e.m."
                         (1043, 1046),
                         --  325: "eKr."
                         (2169, 2172),
                         --  326: "el"
                         (1352, 1353),
                         --  327: "elokuuta"
                         (2116, 2123),
                         --  328: "em"
                         (4482, 4483),
                         --  329: "en"
                         (1616, 1617),
                         --  330: "en_AU"
                         (1666, 1670),
                         --  331: "en_GB"
                         (1713, 1717),
                         --  332: "en_IE"
                         (1687, 1691),
                         --  333: "ene"
                         (1787, 1789),
                         --  334: "enero"
                         (1820, 1824),
                         --  335: "epäluku"
                         (2177, 2183),
                         --  336: "es"
                         (1718, 1719),
                         --  337: "f.Kr."
                         (1047, 1051),
                         --  338: "f.m."
                         (1039, 1042),
                         --  339: "feb"
                         (1790, 1792),
                         --  340: "feb."
                         (927, 930),
                         --  341: "febbraio"
                         (2983, 2990),
                         --  342: "febr."
                         (2735, 2739),
                         --  343: "febrero"
                         (1825, 1831),
                         --  344: "februar"
                         (976, 982),
                         --  345: "februari"
                         (3407, 3414),
                         --  346: "februarie"
                         (3964, 3972),
                         --  347: "február"
                         (2773, 2779),
                         --  348: "februára"
                         (4334, 4341),
                         --  349: "fev"
                         (3739, 3741),
                         --  350: "fevereiro"
                         (3755, 3763),
                         --  351: "fi"
                         (1956, 1957),
                         --  352: "fm"
                         (4480, 4481),
                         --  353: "fr"
                         (2230, 2231),
                         --  354: "fre"
                         (873, 875),
                         --  355: "fre."
                         (5277, 5280),
                         --  356: "fredag"
                         (911, 916),
                         --  357: "févr."
                         (2306, 2310),
                         --  358: "février"
                         (2355, 2361),
                         --  359: "gen"
                         (2955, 2957),
                         --  360: "gennaio"
                         (2976, 2982),
                         --  361: "gio"
                         (2896, 2898),
                         --  362: "giovedì"
                         (2935, 2941),
                         --  363: "giu"
                         (2964, 2966),
                         --  364: "giugno"
                         (3003, 3008),
                         --  365: "gru"
                         (3542, 3544),
                         --  366: "grudnia"
                         (3625, 3631),
                         --  367: "h:mm a"
                         (278, 283),
                         --  368: "h:mm:ss a"
                         (269, 277),
                         --  369: "h:mm:ss a z"
                         (258, 268),
                         --  370: "h:mm:ss a zzzz"
                         (244, 257),
                         --  371: "he"
                         (2430, 2431),
                         --  372: "heinäkuuta"
                         (2106, 2115),
                         --  373: "helmikuuta"
                         (2056, 2065),
                         --  374: "hu"
                         (2676, 2677),
                         --  375: "huhtikuuta"
                         (2077, 2086),
                         --  376: "hétfő"
                         (2698, 2702),
                         --  377: "i. e."
                         (2843, 2847),
                         --  378: "i. sz."
                         (2848, 2853),
                         --  379: "ian."
                         (3944, 3947),
                         --  380: "ianuarie"
                         (3956, 3963),
                         --  381: "ip."
                         (2166, 2168),
                         --  382: "it"
                         (2891, 2892),
                         --  383: "iul."
                         (3952, 3955),
                         --  384: "iulie"
                         (3991, 3995),
                         --  385: "iun."
                         (3948, 3951),
                         --  386: "iunie"
                         (3986, 3990),
                         --  387: "iw"
                         (2428, 2429),
                         --  388: "jKr."
                         (2173, 2176),
                         --  389: "ja"
                         (3064, 3065),
                         --  390: "jan"
                         (3736, 3738),
                         --  391: "jan."
                         (923, 926),
                         --  392: "janeiro"
                         (3748, 3754),
                         --  393: "januar"
                         (970, 975),
                         --  394: "januari"
                         (3400, 3406),
                         --  395: "január"
                         (2767, 2772),
                         --  396: "januára"
                         (4327, 4333),
                         --  397: "janv."
                         (2301, 2305),
                         --  398: "janvier"
                         (2348, 2354),
                         --  399: "jeu."
                         (2244, 2247),
                         --  400: "jeudi"
                         (2282, 2286),
                         --  401: "joi"
                         (3928, 3930),
                         --  402: "joulukuuta"
                         (2153, 2162),
                         --  403: "jue"
                         (1732, 1734),
                         --  404: "jueves"
                         (1768, 1773),
                         --  405: "juil."
                         (2326, 2330),
                         --  406: "juillet"
                         (2367, 2373),
                         --  407: "juin"
                         (2322, 2325),
                         --  408: "jul"
                         (1802, 1804),
                         --  409: "jul."
                         (946, 949),
                         --  410: "julho"
                         (3778, 3782),
                         --  411: "juli"
                         (997, 1000),
                         --  412: "julio"
                         (1851, 1855),
                         --  413: "jun"
                         (1799, 1801),
                         --  414: "jun."
                         (942, 945),
                         --  415: "junho"
                         (3773, 3777),
                         --  416: "juni"
                         (993, 996),
                         --  417: "junio"
                         (1846, 1850),
                         --  418: "júl"
                         (4315, 4317),
                         --  419: "júl."
                         (2757, 2760),
                         --  420: "júla"
                         (4356, 4359),
                         --  421: "július"
                         (2805, 2810),
                         --  422: "jún"
                         (4312, 4314),
                         --  423: "jún."
                         (2753, 2756),
                         --  424: "júna"
                         (4352, 4355),
                         --  425: "június"
                         (2799, 2804),
                         --  426: "ke"
                         (1964, 1965),
                         --  427: "kedd"
                         (2703, 2706),
                         --  428: "keskiviikkona"
                         (2003, 2015),
                         --  429: "kesäkuuta"
                         (2097, 2105),
                         --  430: "ko"
                         (3178, 3179),
                         --  431: "května"
                         (697, 702),
                         --  432: "kwi"
                         (3521, 3523),
                         --  433: "kwietnia"
                         (3564, 3571),
                         --  434: "la"
                         (1970, 1971),
                         --  435: "lauantaina"
                         (2036, 2045),
                         --  436: "ledna"
                         (676, 680),
                         --  437: "lip"
                         (3527, 3529),
                         --  438: "lipca"
                         (3583, 3587),
                         --  439: "lis"
                         (3539, 3541),
                         --  440: "listopada"
                         (3616, 3624),
                         --  441: "listopadu"
                         (731, 739),
                         --  442: "lokakuuta"
                         (2133, 2141),
                         --  443: "lug"
                         (2967, 2969),
                         --  444: "luglio"
                         (3009, 3014),
                         --  445: "lun"
                         (1723, 1725),
                         --  446: "lun."
                         (2236, 2239),
                         --  447: "lundi"
                         (2264, 2268),
                         --  448: "lunedì"
                         (2913, 2918),
                         --  449: "lunes"
                         (1748, 1752),
                         --  450: "luni"
                         (3911, 3914),
                         --  451: "lut"
                         (3518, 3520),
                         --  452: "lutego"
                         (3553, 3558),
                         --  453: "lör"
                         (4446, 4448),
                         --  454: "lördag"
                         (4467, 4472),
                         --  455: "lør"
                         (876, 878),
                         --  456: "lør."
                         (5281, 5284),
                         --  457: "lørdag"
                         (917, 922),
                         --  458: "m."
                         (3039, 3040),
                         --  459: "ma"
                         (1960, 1961),
                         --  460: "maaliskuuta"
                         (2066, 2076),
                         --  461: "maanantaina"
                         (1983, 1993),
                         --  462: "maandag"
                         (3347, 3353),
                         --  463: "maart"
                         (3415, 3419),
                         --  464: "mag"
                         (2961, 2963),
                         --  465: "maggio"
                         (2997, 3002),
                         --  466: "mai"
                         (2319, 2321),
                         --  467: "maio"
                         (3769, 3772),
                         --  468: "maj"
                         (939, 941),
                         --  469: "maja"
                         (3572, 3575),
                         --  470: "man"
                         (861, 863),
                         --  471: "man."
                         (5261, 5264),
                         --  472: "mandag"
                         (885, 890),
                         --  473: "mar"
                         (1726, 1728),
                         --  474: "mar."
                         (931, 934),
                         --  475: "marca"
                         (3559, 3563),
                         --  476: "mardi"
                         (2269, 2273),
                         --  477: "marraskuuta"
                         (2142, 2152),
                         --  478: "mars"
                         (2311, 2314),
                         --  479: "martedì"
                         (2919, 2925),
                         --  480: "martes"
                         (1753, 1758),
                         --  481: "martie"
                         (3973, 3978),
                         --  482: "marts"
                         (983, 987),
                         --  483: "marzo"
                         (1832, 1836),
                         --  484: "março"
                         (3764, 3768),
                         --  485: "marți"
                         (3915, 3919),
                         --  486: "may"
                         (1796, 1798),
                         --  487: "mayo"
                         (1842, 1845),
                         --  488: "mei"
                         (3397, 3399),
                         --  489: "meio-dia"
                         (3814, 3821),
                         --  490: "mer"
                         (2893, 2895),
                         --  491: "mer."
                         (2240, 2243),
                         --  492: "mercoledì"
                         (2926, 2934),
                         --  493: "mercredi"
                         (2274, 2281),
                         --  494: "midi"
                         (2406, 2409),
                         --  495: "miercuri"
                         (3920, 3927),
                         --  496: "mié"
                         (1729, 1731),
                         --  497: "miércoles"
                         (1759, 1767),
                         --  498: "mrt."
                         (3393, 3396),
                         --  499: "máj"
                         (4309, 4311),
                         --  500: "máj."
                         (2749, 2752),
                         --  501: "mája"
                         (4348, 4351),
                         --  502: "május"
                         (2794, 2798),
                         --  503: "márc."
                         (2740, 2744),
                         --  504: "március"
                         (2780, 2786),
                         --  505: "mån"
                         (4436, 4438),
                         --  506: "måndag"
                         (4455, 4460),
                         --  507: "n. Chr."
                         (1291, 1297),
                         --  508: "n.e."
                         (3648, 3651),
                         --  509: "n.l."
                         (4427, 4430),
                         --  510: "nachm."
                         (1278, 1283),
                         --  511: "nb"
                         (3438, 3439),
                         --  512: "ne"
                         (605, 606),
                         --  513: "nedeľa"
                         (4270, 4275),
                         --  514: "neděle"
                         (619, 624),
                         --  515: "niedz."
                         (3442, 3447),
                         --  516: "niedziela"
                         (3469, 3477),
                         --  517: "nl"
                         (3327, 3328),
                         --  518: "no"
                         (3436, 3437),
                         --  519: "noiembrie"
                         (4015, 4023),
                         --  520: "noon"
                         (181, 184),
                         --  521: "nov"
                         (1814, 1816),
                         --  522: "nov."
                         (962, 965),
                         --  523: "november"
                         (1023, 1030),
                         --  524: "novembra"
                         (4383, 4390),
                         --  525: "novembre"
                         (2390, 2397),
                         --  526: "novembro"
                         (3798, 3805),
                         --  527: "noviembre"
                         (1879, 1887),
                         --  528: "oct"
                         (1811, 1813),
                         --  529: "oct."
                         (2340, 2343),
                         --  530: "octobre"
                         (2383, 2389),
                         --  531: "octombrie"
                         (4006, 4014),
                         --  532: "octubre"
                         (1872, 1878),
                         --  533: "odp."
                         (752, 755),
                         --  534: "okt"
                         (4321, 4323),
                         --  535: "okt."
                         (958, 961),
                         --  536: "oktober"
                         (1016, 1022),
                         --  537: "október"
                         (2830, 2836),
                         --  538: "októbra"
                         (4376, 4382),
                         --  539: "ons"
                         (867, 869),
                         --  540: "ons."
                         (5269, 5272),
                         --  541: "onsdag"
                         (898, 903),
                         --  542: "ott"
                         (2973, 2975),
                         --  543: "ottobre"
                         (3024, 3030),
                         --  544: "out"
                         (3742, 3744),
                         --  545: "outubro"
                         (3791, 3797),
                         --  546: "p."
                         (3041, 3042),
                         --  547: "p.m."
                         (1696, 1699),
                         --  548: "p.n.e."
                         (3642, 3647),
                         --  549: "paź"
                         (3536, 3538),
                         --  550: "października"
                         (3604, 3615),
                         --  551: "pe"
                         (1968, 1969),
                         --  552: "perjantaina"
                         (2025, 2035),
                         --  553: "pi"
                         (4268, 4269),
                         --  554: "piatok"
                         (4303, 4308),
                         --  555: "piątek"
                         (3509, 3514),
                         --  556: "pl"
                         (3440, 3441),
                         --  557: "po"
                         (607, 608),
                         --  558: "po Kr."
                         (762, 767),
                         --  559: "pon."
                         (3448, 3451),
                         --  560: "pondelok"
                         (4276, 4283),
                         --  561: "pondělí"
                         (625, 631),
                         --  562: "poniedziałek"
                         (3478, 3489),
                         --  563: "popoludní"
                         (4409, 4417),
                         --  564: "pred n.l."
                         (4418, 4426),
                         --  565: "prosince"
                         (740, 747),
                         --  566: "pt"
                         (3660, 3661),
                         --  567: "pt."
                         (3462, 3464),
                         --  568: "pá"
                         (615, 616),
                         --  569: "pátek"
                         (650, 654),
                         --  570: "péntek"
                         (2722, 2727),
                         --  571: "př.Kr."
                         (756, 761),
                         --  572: "qua"
                         (3668, 3670),
                         --  573: "quarta-feira"
                         (3701, 3712),
                         --  574: "qui"
                         (3671, 3673),
                         --  575: "quinta-feira"
                         (3713, 3724),
                         --  576: "ro"
                         (3887, 3888),
                         --  577: "ru"
                         (4043, 4044),
                         --  578: "sab"
                         (2902, 2904),
                         --  579: "sabato"
                         (2949, 2954),
                         --  580: "sam."
                         (2252, 2255),
                         --  581: "samedi"
                         (2295, 2300),
                         --  582: "seg"
                         (3662, 3664),
                         --  583: "segunda-feira"
                         (3677, 3689),
                         --  584: "sep"
                         (1808, 1810),
                         --  585: "sep."
                         (954, 957),
                         --  586: "sept."
                         (2335, 2339),
                         --  587: "september"
                         (1007, 1015),
                         --  588: "septembra"
                         (4367, 4375),
                         --  589: "septembre"
                         (2374, 2382),
                         --  590: "septembrie"
                         (3996, 4005),
                         --  591: "septiembre"
                         (1862, 1871),
                         --  592: "set"
                         (2970, 2972),
                         --  593: "setembro"
                         (3783, 3790),
                         --  594: "settembre"
                         (3015, 3023),
                         --  595: "sex"
                         (3674, 3676),
                         --  596: "sexta-feira"
                         (3725, 3735),
                         --  597: "sie"
                         (3530, 3532),
                         --  598: "sierpnia"
                         (3588, 3595),
                         --  599: "sk"
                         (4262, 4263),
                         --  600: "so"
                         (617, 618),
                         --  601: "sob."
                         (3465, 3468),
                         --  602: "sobota"
                         (655, 660),
                         --  603: "srpna"
                         (717, 721),
                         --  604: "st"
                         (611, 612),
                         --  605: "streda"
                         (4290, 4295),
                         --  606: "sty"
                         (3515, 3517),
                         --  607: "stycznia"
                         (3545, 3552),
                         --  608: "středa"
                         (637, 642),
                         --  609: "su"
                         (1958, 1959),
                         --  610: "sunnuntaina"
                         (1972, 1982),
                         --  611: "sv"
                         (4431, 4432),
                         --  612: "syyskuuta"
                         (2124, 2132),
                         --  613: "szept."
                         (2761, 2766),
                         --  614: "szeptember"
                         (2820, 2829),
                         --  615: "szerda"
                         (2707, 2712),
                         --  616: "szombat"
                         (2728, 2734),
                         --  617: "sáb"
                         (1738, 1740),
                         --  618: "sábado"
                         (1781, 1786),
                         --  619: "sâmbătă"
                         (3937, 3943),
                         --  620: "sön"
                         (4433, 4435),
                         --  621: "söndag"
                         (4449, 4454),
                         --  622: "søn"
                         (858, 860),
                         --  623: "søn."
                         (5257, 5260),
                         --  624: "søndag"
                         (879, 884),
                         --  625: "tammikuuta"
                         (2046, 2055),
                         --  626: "ter"
                         (3665, 3667),
                         --  627: "terça-feira"
                         (3690, 3700),
                         --  628: "th"
                         (4548, 4549),
                         --  629: "ti"
                         (1962, 1963),
                         --  630: "tiistaina"
                         (1994, 2002),
                         --  631: "tir"
                         (864, 866),
                         --  632: "tir."
                         (5265, 5268),
                         --  633: "tirsdag"
                         (891, 897),
                         --  634: "tis"
                         (4439, 4441),
                         --  635: "tisdag"
                         (4461, 4466),
                         --  636: "to"
                         (1966, 1967),
                         --  637: "tor"
                         (870, 872),
                         --  638: "tor."
                         (5273, 5276),
                         --  639: "tors"
                         (4442, 4445),
                         --  640: "torsdag"
                         (904, 910),
                         --  641: "torstaina"
                         (2016, 2024),
                         --  642: "toukokuuta"
                         (2087, 2096),
                         --  643: "tr"
                         (4902, 4903),
                         --  644: "ut"
                         (4264, 4265),
                         --  645: "utorok"
                         (4284, 4289),
                         --  646: "v. Chr."
                         (1284, 1290),
                         --  647: "vasárnap"
                         (2690, 2697),
                         --  648: "ven"
                         (2899, 2901),
                         --  649: "ven."
                         (2248, 2251),
                         --  650: "vendredi"
                         (2287, 2294),
                         --  651: "venerdì"
                         (2942, 2948),
                         --  652: "vie"
                         (1735, 1737),
                         --  653: "viernes"
                         (1774, 1780),
                         --  654: "vineri"
                         (3931, 3936),
                         --  655: "vorm."
                         (1267, 1271),
                         --  656: "vr"
                         (3337, 3338),
                         --  657: "vrijdag"
                         (3378, 3384),
                         --  658: "w południe"
                         (3632, 3641),
                         --  659: "wo"
                         (3333, 3334),
                         --  660: "woensdag"
                         (3361, 3368),
                         --  661: "wrz"
                         (3533, 3535),
                         --  662: "września"
                         (3596, 3603),
                         --  663: "wt."
                         (3452, 3454),
                         --  664: "wtorek"
                         (3490, 3495),
                         --  665: "y. MMMM d."
                         (2870, 2879),
                         --  666: "y. MMMM d., EEEE"
                         (2854, 2869),
                         --  667: "yy-M-d"
                         (5152, 5157),
                         --  668: "yy. M. d."
                         (3276, 3284),
                         --  669: "yy/M/d"
                         (5228, 5233),
                         --  670: "yy/MM/dd"
                         (3151, 3158),
                         --  671: "yyyy-M-d"
                         (5144, 5151),
                         --  672: "yyyy-MM-dd"
                         (4519, 4528),
                         --  673: "yyyy. M. d."
                         (3265, 3275),
                         --  674: "yyyy.MM.dd."
                         (2880, 2890),
                         --  675: "yyyy/M/d"
                         (5220, 5227),
                         --  676: "yyyy/MM/dd"
                         (3141, 3150),
                         --  677: "y年M月d日"
                         (3135, 3140),
                         --  678: "y年M月d日EEEE"
                         (3125, 3134),
                         --  679: "y년 M월 d일"
                         (3257, 3264),
                         --  680: "y년 M월 d일 EEEE"
                         (3244, 3256),
                         --  681: "z h:mm:ss a"
                         (592, 602),
                         --  682: "za"
                         (3339, 3340),
                         --  683: "zah时mm分ss秒"
                         (5171, 5180),
                         --  684: "zah時mm分ss秒"
                         (5247, 5256),
                         --  685: "zaterdag"
                         (3385, 3392),
                         --  686: "zh"
                         (5098, 5099),
                         --  687: "zh_Hant"
                         (5194, 5200),
                         --  688: "zo"
                         (3329, 3330),
                         --  689: "zondag"
                         (3341, 3346),
                         --  690: "zzzz h:mm:ss a"
                         (578, 591),
                         --  691: "zzzzah时mm分ss秒"
                         (5158, 5170),
                         --  692: "zzzzah時mm分ss秒"
                         (5234, 5246),
                         --  693: "září"
                         (722, 725),
                         --  694: "{1} {0}"
                         (284, 290),
                         --  695: "{1}, {0}"
                         (3652, 3659),
                         --  696: "{1}{0}"
                         (3172, 3177),
                         --  697: " "
                         (768, 768),
                         --  698: "¤#,##0.00"
                         (1671, 1679),
                         --  699: "¤#,##0.00;(¤#,##0.00)"
                         (309, 329),
                         --  700: "¤#,##0.00;¤-#,##0.00"
                         (4799, 4818),
                         --  701: "¤ #,##0.00"
                         (1905, 1914),
                         --  702: "¤ #,##0.00;¤ #,##0.00-"
                         (510, 531),
                         --  703: "¤¤¤"
                         (4489, 4491),
                         --  704: "Çar"
                         (4913, 4915),
                         --  705: "Çarşamba"
                         (4943, 4950),
                         --  706: "×10^"
                         (4485, 4488),
                         --  707: "ápr."
                         (2745, 2748),
                         --  708: "április"
                         (2787, 2793),
                         --  709: "î.Hr."
                         (4033, 4037),
                         --  710: "února"
                         (681, 685),
                         --  711: "út"
                         (609, 610),
                         --  712: "úterý"
                         (632, 636),
                         --  713: "července"
                         (709, 716),
                         --  714: "června"
                         (703, 708),
                         --  715: "čt"
                         (613, 614),
                         --  716: "čtvrtek"
                         (643, 649),
                         --  717: "října"
                         (726, 730),
                         --  718: "śr."
                         (3455, 3457),
                         --  719: "środa"
                         (3496, 3500),
                         --  720: "Şub"
                         (4975, 4977),
                         --  721: "Şubat"
                         (5006, 5010),
                         --  722: "št"
                         (4266, 4267),
                         --  723: "štvrtok"
                         (4296, 4302),
                         --  724: "Απρ"
                         (1432, 1434),
                         --  725: "Απριλίου"
                         (1489, 1496),
                         --  726: "Αυγ"
                         (1446, 1448),
                         --  727: "Αυγούστου"
                         (1516, 1524),
                         --  728: "Δεκ"
                         (1458, 1460),
                         --  729: "Δεκεμβρίου"
                         (1554, 1563),
                         --  730: "Δευ"
                         (1357, 1359),
                         --  731: "Δευτέρα"
                         (1382, 1388),
                         --  732: "Ιαν"
                         (1423, 1425),
                         --  733: "Ιανουαρίου"
                         (1461, 1470),
                         --  734: "Ιουλ"
                         (1442, 1445),
                         --  735: "Ιουλίου"
                         (1509, 1515),
                         --  736: "Ιουν"
                         (1438, 1441),
                         --  737: "Ιουνίου"
                         (1502, 1508),
                         --  738: "Κυρ"
                         (1354, 1356),
                         --  739: "Κυριακή"
                         (1375, 1381),
                         --  740: "Μαΐου"
                         (1497, 1501),
                         --  741: "Μαρ"
                         (1429, 1431),
                         --  742: "Μαρτίου"
                         (1482, 1488),
                         --  743: "Μαϊ"
                         (1435, 1437),
                         --  744: "Νοε"
                         (1455, 1457),
                         --  745: "Νοεμβρίου"
                         (1545, 1553),
                         --  746: "Οκτ"
                         (1452, 1454),
                         --  747: "Οκτωβρίου"
                         (1536, 1544),
                         --  748: "Πέμπτη"
                         (1401, 1406),
                         --  749: "Παρ"
                         (1369, 1371),
                         --  750: "Παρασκευή"
                         (1407, 1415),
                         --  751: "Πεμ"
                         (1366, 1368),
                         --  752: "Σάββατο"
                         (1416, 1422),
                         --  753: "Σαβ"
                         (1372, 1374),
                         --  754: "Σεπ"
                         (1449, 1451),
                         --  755: "Σεπτεμβρίου"
                         (1525, 1535),
                         --  756: "Τετ"
                         (1363, 1365),
                         --  757: "Τετάρτη"
                         (1394, 1400),
                         --  758: "Τρίτη"
                         (1389, 1393),
                         --  759: "Τρι"
                         (1360, 1362),
                         --  760: "Φεβ"
                         (1426, 1428),
                         --  761: "Φεβρουαρίου"
                         (1471, 1481),
                         --  762: "μ.Χ."
                         (1576, 1579),
                         --  763: "μ.μ."
                         (1568, 1571),
                         --  764: "π.Χ."
                         (1572, 1575),
                         --  765: "π.μ."
                         (1564, 1567),
                         --  766: "авг."
                         (4143, 4146),
                         --  767: "августа"
                         (4184, 4190),
                         --  768: "апр."
                         (4128, 4131),
                         --  769: "апреля"
                         (4178, 4183),
                         --  770: "воскресенье"
                         (4059, 4069),
                         --  771: "вс"
                         (4045, 4046),
                         --  772: "вт"
                         (4049, 4050),
                         --  773: "вторник"
                         (4081, 4087),
                         --  774: "дек."
                         (4161, 4164),
                         --  775: "декабря"
                         (4212, 4218),
                         --  776: "до н.э."
                         (4219, 4225),
                         --  777: "июля"
                         (4139, 4142),
                         --  778: "июня"
                         (4135, 4138),
                         --  779: "марта"
                         (4123, 4127),
                         --  780: "мая"
                         (4132, 4134),
                         --  781: "н.э."
                         (4226, 4229),
                         --  782: "нояб."
                         (4156, 4160),
                         --  783: "ноября"
                         (4206, 4211),
                         --  784: "окт."
                         (4152, 4155),
                         --  785: "октября"
                         (4199, 4205),
                         --  786: "пн"
                         (4047, 4048),
                         --  787: "понедельник"
                         (4070, 4080),
                         --  788: "пт"
                         (4055, 4056),
                         --  789: "пятница"
                         (4100, 4106),
                         --  790: "сб"
                         (4057, 4058),
                         --  791: "сент."
                         (4147, 4151),
                         --  792: "сентября"
                         (4191, 4198),
                         --  793: "ср"
                         (4051, 4052),
                         --  794: "среда"
                         (4088, 4092),
                         --  795: "суббота"
                         (4107, 4113),
                         --  796: "февр."
                         (4118, 4122),
                         --  797: "февраля"
                         (4171, 4177),
                         --  798: "четверг"
                         (4093, 4099),
                         --  799: "чт"
                         (4053, 4054),
                         --  800: "янв."
                         (4114, 4117),
                         --  801: "января"
                         (4165, 4170),
                         --  802: "אוג"
                         (2550, 2552),
                         --  803: "אוגוסט"
                         (2589, 2594),
                         --  804: "אוק"
                         (2556, 2558),
                         --  805: "אוקטובר"
                         (2601, 2607),
                         --  806: "אחה״צ"
                         (2625, 2629),
                         --  807: "אפר"
                         (2538, 2540),
                         --  808: "אפריל"
                         (2576, 2580),
                         --  809: "דצמ"
                         (2562, 2564),
                         --  810: "דצמבר"
                         (2614, 2618),
                         --  811: "יול"
                         (2547, 2549),
                         --  812: "יולי"
                         (2585, 2588),
                         --  813: "יום א׳"
                         (2432, 2437),
                         --  814: "יום ב׳"
                         (2438, 2443),
                         --  815: "יום ג׳"
                         (2444, 2449),
                         --  816: "יום ד׳"
                         (2450, 2455),
                         --  817: "יום ה׳"
                         (2456, 2461),
                         --  818: "יום ו׳"
                         (2462, 2467),
                         --  819: "יום חמישי"
                         (2505, 2513),
                         --  820: "יום ראשון"
                         (2471, 2479),
                         --  821: "יום רביעי"
                         (2496, 2504),
                         --  822: "יום שבת"
                         (2522, 2528),
                         --  823: "יום שישי"
                         (2514, 2521),
                         --  824: "יום שלישי"
                         (2487, 2495),
                         --  825: "יום שני"
                         (2480, 2486),
                         --  826: "יונ"
                         (2544, 2546),
                         --  827: "יוני"
                         (2581, 2584),
                         --  828: "ינו"
                         (2529, 2531),
                         --  829: "ינואר"
                         (2565, 2569),
                         --  830: "לסה״נ"
                         (2636, 2640),
                         --  831: "לפנה״ס"
                         (2630, 2635),
                         --  832: "לפנה״צ"
                         (2619, 2624),
                         --  833: "מאי"
                         (2541, 2543),
                         --  834: "מרס"
                         (2535, 2537),
                         --  835: "נוב"
                         (2559, 2561),
                         --  836: "נובמבר"
                         (2608, 2613),
                         --  837: "ספט"
                         (2553, 2555),
                         --  838: "ספטמבר"
                         (2595, 2600),
                         --  839: "פבר"
                         (2532, 2534),
                         --  840: "פברואר"
                         (2570, 2575),
                         --  841: "שבת"
                         (2468, 2470),
                         --  842: "؉"
                         (482, 482),
                         --  843: "؛"
                         (478, 478),
                         --  844: "أبريل"
                         (423, 427),
                         --  845: "أحد"
                         (332, 334),
                         --  846: "أربعاء"
                         (346, 351),
                         --  847: "أغسطس"
                         (442, 446),
                         --  848: "أكتوبر"
                         (453, 458),
                         --  849: "إثنين"
                         (335, 339),
                         --  850: "اس"
                         (480, 481),
                         --  851: "الأحد"
                         (363, 367),
                         --  852: "الأربعاء"
                         (383, 390),
                         --  853: "الإثنين"
                         (368, 374),
                         --  854: "الثلاثاء"
                         (375, 382),
                         --  855: "الجمعة"
                         (397, 402),
                         --  856: "الخميس"
                         (391, 396),
                         --  857: "السبت"
                         (403, 407),
                         --  858: "ثلاثاء"
                         (340, 345),
                         --  859: "جمعة"
                         (356, 359),
                         --  860: "خميس"
                         (352, 355),
                         --  861: "ديسمبر"
                         (465, 470),
                         --  862: "سبت"
                         (360, 362),
                         --  863: "سبتمبر"
                         (447, 452),
                         --  864: "ص"
                         (471, 471),
                         --  865: "فبراير"
                         (413, 418),
                         --  866: "ق.م"
                         (473, 475),
                         --  867: "ليس رقم"
                         (483, 489),
                         --  868: "م"
                         (472, 472),
                         --  869: "مارس"
                         (419, 422),
                         --  870: "مايو"
                         (428, 431),
                         --  871: "نوفمبر"
                         (459, 464),
                         --  872: "يناير"
                         (408, 412),
                         --  873: "يوليو"
                         (437, 441),
                         --  874: "يونيو"
                         (432, 436),
                         --  875: "٪"
                         (479, 479),
                         --  876: "٫"
                         (476, 476),
                         --  877: "٬"
                         (477, 477),
                         --  878: "ก.ค."
                         (4654, 4657),
                         --  879: "ก.พ."
                         (4631, 4634),
                         --  880: "ก.ย."
                         (4662, 4665),
                         --  881: "กรกฎาคม"
                         (4721, 4727),
                         --  882: "กันยายน"
                         (4735, 4741),
                         --  883: "กุมภาพันธ์"
                         (4684, 4693),
                         --  884: "ก่อนเที่ยง"
                         (4764, 4773),
                         --  885: "ค.ศ."
                         (4795, 4798),
                         --  886: "จ."
                         (4553, 4554),
                         --  887: "ต.ค."
                         (4666, 4669),
                         --  888: "ตุลาคม"
                         (4742, 4747),
                         --  889: "ธ.ค."
                         (4674, 4677),
                         --  890: "ธันวาคม"
                         (4757, 4763),
                         --  891: "ปีก่อน ค.ศ."
                         (4784, 4794),
                         --  892: "พ."
                         (4557, 4558),
                         --  893: "พ.ค."
                         (4645, 4648),
                         --  894: "พ.ย."
                         (4670, 4673),
                         --  895: "พฤ."
                         (4559, 4561),
                         --  896: "พฤศจิกายน"
                         (4748, 4756),
                         --  897: "พฤษภาคม"
                         (4706, 4712),
                         --  898: "ม.ค."
                         (4627, 4630),
                         --  899: "มกราคม"
                         (4678, 4683),
                         --  900: "มิ.ย."
                         (4649, 4653),
                         --  901: "มิถุนายน"
                         (4713, 4720),
                         --  902: "มี.ค."
                         (4635, 4639),
                         --  903: "มีนาคม"
                         (4694, 4699),
                         --  904: "วันจันทร์"
                         (4576, 4584),
                         --  905: "วันพฤหัสบดี"
                         (4600, 4610),
                         --  906: "วันพุธ"
                         (4594, 4599),
                         --  907: "วันศุกร์"
                         (4611, 4618),
                         --  908: "วันอังคาร"
                         (4585, 4593),
                         --  909: "วันอาทิตย์"
                         (4566, 4575),
                         --  910: "วันเสาร์"
                         (4619, 4626),
                         --  911: "ศ."
                         (4562, 4563),
                         --  912: "ส."
                         (4564, 4565),
                         --  913: "ส.ค."
                         (4658, 4661),
                         --  914: "สิงหาคม"
                         (4728, 4734),
                         --  915: "หลังเที่ยง"
                         (4774, 4783),
                         --  916: "อ."
                         (4555, 4556),
                         --  917: "อา."
                         (4550, 4552),
                         --  918: "เม.ย."
                         (4640, 4644),
                         --  919: "เมษายน"
                         (4700, 4705),
                         --  920: "‰"
                         (200, 200),
                         --  921: "−"
                         (4484, 4484),
                         --  922: "∞"
                         (201, 201),
                         --  923: "上午"
                         (5135, 5136),
                         --  924: "下午"
                         (5137, 5138),
                         --  925: "公元"
                         (5142, 5143),
                         --  926: "公元前"
                         (5139, 5141),
                         --  927: "午前"
                         (3121, 3122),
                         --  928: "午後"
                         (3123, 3124),
                         --  929: "周一"
                         (5102, 5103),
                         --  930: "周三"
                         (5106, 5107),
                         --  931: "周二"
                         (5104, 5105),
                         --  932: "周五"
                         (5110, 5111),
                         --  933: "周六"
                         (5112, 5113),
                         --  934: "周四"
                         (5108, 5109),
                         --  935: "周日"
                         (5100, 5101),
                         --  936: "土"
                         (3072, 3072),
                         --  937: "土曜日"
                         (3091, 3093),
                         --  938: "日"
                         (3066, 3066),
                         --  939: "日曜日"
                         (3073, 3075),
                         --  940: "星期一"
                         (5117, 5119),
                         --  941: "星期三"
                         (5123, 5125),
                         --  942: "星期二"
                         (5120, 5122),
                         --  943: "星期五"
                         (5129, 5131),
                         --  944: "星期六"
                         (5132, 5134),
                         --  945: "星期四"
                         (5126, 5128),
                         --  946: "星期日"
                         (5114, 5116),
                         --  947: "月"
                         (3067, 3067),
                         --  948: "月曜日"
                         (3076, 3078),
                         --  949: "木"
                         (3070, 3070),
                         --  950: "木曜日"
                         (3085, 3087),
                         --  951: "水"
                         (3069, 3069),
                         --  952: "水曜日"
                         (3082, 3084),
                         --  953: "火"
                         (3068, 3068),
                         --  954: "火曜日"
                         (3079, 3081),
                         --  955: "西元"
                         (5218, 5219),
                         --  956: "西元前"
                         (5215, 5217),
                         --  957: "週一"
                         (5203, 5204),
                         --  958: "週三"
                         (5207, 5208),
                         --  959: "週二"
                         (5205, 5206),
                         --  960: "週五"
                         (5211, 5212),
                         --  961: "週六"
                         (5213, 5214),
                         --  962: "週四"
                         (5209, 5210),
                         --  963: "週日"
                         (5201, 5202),
                         --  964: "金"
                         (3071, 3071),
                         --  965: "金曜日"
                         (3088, 3090),
                         --  966: "금"
                         (3185, 3185),
                         --  967: "금요일"
                         (3202, 3204),
                         --  968: "기원전"
                         (3239, 3241),
                         --  969: "목"
                         (3184, 3184),
                         --  970: "목요일"
                         (3199, 3201),
                         --  971: "서기"
                         (3242, 3243),
                         --  972: "수"
                         (3183, 3183),
                         --  973: "수요일"
                         (3196, 3198),
                         --  974: "오전"
                         (3235, 3236),
                         --  975: "오후"
                         (3237, 3238),
                         --  976: "월"
                         (3181, 3181),
                         --  977: "월요일"
                         (3190, 3192),
                         --  978: "일"
                         (3180, 3180),
                         --  979: "일요일"
                         (3187, 3189),
                         --  980: "토"
                         (3186, 3186),
                         --  981: "토요일"
                         (3205, 3207),
                         --  982: "화"
                         (3182, 3182),
                         --  983: "화요일"
                         (3193, 3195));

   Locale_Data : constant array (Trait_Index_Type range <>)
                    of Locale_Traits_Type := (

      (Tag => "          ",
       Level => 0,
       Name => 1,
       Short_Day_Names =>
          (Sun => 207, Mon => 172, Tue => 216,
           Wed => 220, Thu => 214, Fri => 116,
           Sat => 201),
       Full_Day_Names =>
          (Sun => 208, Mon => 173, Tue => 217,
           Wed => 221, Thu => 215, Fri => 117,
           Sat => 202),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 144, Feb => 113, Mar => 163,
           Apr => 69, May => 165, Jun => 150,
           Jul => 148, Aug => 73, Sep => 204,
           Oct => 186, Nov => 182, Dec => 85),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 77, CE => 78),
       Date_Formats =>
           (Full => 99, Long => 158, Medium => 157, Short => 156),
       Time_Formats =>
           (Full => 370, Long => 369, Medium => 368, Short => 367),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 699)),

      (Tag => "AR        ",
       Level => 1,
       Name => 244,
       Short_Day_Names =>
          (Sun => 845, Mon => 849, Tue => 858,
           Wed => 846, Thu => 860, Fri => 859,
           Sat => 862),
       Full_Day_Names =>
          (Sun => 851, Mon => 853, Tue => 854,
           Wed => 852, Thu => 856, Fri => 855,
           Sat => 857),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 872, Feb => 865, Mar => 869,
           Apr => 844, May => 870, Jun => 874,
           Jul => 873, Aug => 847, Sep => 863,
           Oct => 848, Nov => 871, Dec => 861),
       Day_Period_Names =>
           (AM => 864, Noon => 520, PM => 868),
       Era_Names =>
           (BCE => 866, CE => 868),
       Date_Formats =>
           (Full => 105, Long => 267, Medium => 290, Short => 321),
       Time_Formats =>
           (Full => 690, Long => 681, Medium => 368, Short => 367),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 876, Group => 877,
            List => 843, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 850, Percent => 875,
            Permille => 842, Infinity => 922,
            Nan => 867, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 5, Scientific => 8,
            Percent => 3, Currency => 702)),

      (Tag => "CS        ",
       Level => 1,
       Name => 257,
       Short_Day_Names =>
          (Sun => 512, Mon => 557, Tue => 711,
           Wed => 604, Thu => 715, Fri => 568,
           Sat => 600),
       Full_Day_Names =>
          (Sun => 514, Mon => 561, Tue => 712,
           Wed => 608, Thu => 716, Fri => 569,
           Sat => 602),
       Short_Month_Names =>
          (Jan => 29, Feb => 41, Mar => 44,
           Apr => 47, May => 50, Jun => 53,
           Jul => 56, Aug => 59, Sep => 62,
           Oct => 30, Nov => 33, Dec => 36),
       Full_Month_Names =>
          (Jan => 436, Feb => 710, Mar => 255,
           Apr => 317, May => 431, Jun => 714,
           Jul => 713, Aug => 603, Sep => 693,
           Oct => 717, Nov => 441, Dec => 565),
       Day_Period_Names =>
           (AM => 314, Noon => 520, PM => 533),
       Era_Names =>
           (BCE => 571, CE => 558),
       Date_Formats =>
           (Full => 104, Long => 271, Medium => 275, Short => 274),
       Time_Formats =>
           (Full => 128, Long => 127, Medium => 126, Short => 125),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 6)),

      (Tag => "DA        ",
       Level => 1,
       Name => 280,
       Short_Day_Names =>
          (Sun => 622, Mon => 470, Tue => 631,
           Wed => 539, Thu => 637, Fri => 354,
           Sat => 455),
       Full_Day_Names =>
          (Sun => 624, Mon => 472, Tue => 633,
           Wed => 541, Thu => 640, Fri => 356,
           Sat => 457),
       Short_Month_Names =>
          (Jan => 391, Feb => 340, Mar => 474,
           Apr => 239, May => 468, Jun => 414,
           Jul => 409, Aug => 246, Sep => 585,
           Oct => 535, Nov => 522, Dec => 294),
       Full_Month_Names =>
          (Jan => 393, Feb => 344, Mar => 482,
           Apr => 240, May => 468, Jun => 416,
           Jul => 411, Aug => 247, Sep => 587,
           Oct => 536, Nov => 523, Dec => 295),
       Day_Period_Names =>
           (AM => 338, Noon => 520, PM => 324),
       Era_Names =>
           (BCE => 337, CE => 323),
       Date_Formats =>
           (Full => 94, Long => 270, Medium => 288, Short => 287),
       Time_Formats =>
           (Full => 134, Long => 133, Medium => 132, Short => 131),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 13, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 6)),

      (Tag => "DE        ",
       Level => 1,
       Name => 291,
       Short_Day_Names =>
          (Sun => 205, Mon => 171, Tue => 88,
           Wed => 168, Thu => 90, Fri => 114,
           Sat => 197),
       Full_Day_Names =>
          (Sun => 206, Mon => 174, Tue => 89,
           Wed => 170, Thu => 91, Fri => 115,
           Sat => 200),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 176,
           Apr => 68, May => 161, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 187, Nov => 181, Dec => 86),
       Full_Month_Names =>
          (Jan => 143, Feb => 112, Mar => 177,
           Apr => 69, May => 161, Jun => 151,
           Jul => 147, Aug => 73, Sep => 204,
           Oct => 188, Nov => 182, Dec => 87),
       Day_Period_Names =>
           (AM => 655, Noon => 169, PM => 510),
       Era_Names =>
           (BCE => 646, CE => 507),
       Date_Formats =>
           (Full => 104, Long => 271, Medium => 286, Short => 285),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 6)),

      (Tag => "EL        ",
       Level => 1,
       Name => 326,
       Short_Day_Names =>
          (Sun => 738, Mon => 730, Tue => 759,
           Wed => 756, Thu => 751, Fri => 749,
           Sat => 753),
       Full_Day_Names =>
          (Sun => 739, Mon => 731, Tue => 758,
           Wed => 757, Thu => 748, Fri => 750,
           Sat => 752),
       Short_Month_Names =>
          (Jan => 732, Feb => 760, Mar => 741,
           Apr => 724, May => 743, Jun => 736,
           Jul => 734, Aug => 726, Sep => 754,
           Oct => 746, Nov => 744, Dec => 728),
       Full_Month_Names =>
          (Jan => 733, Feb => 761, Mar => 742,
           Apr => 725, May => 740, Jun => 737,
           Jul => 735, Aug => 727, Sep => 755,
           Oct => 747, Nov => 745, Dec => 729),
       Day_Period_Names =>
           (AM => 765, Noon => 520, PM => 763),
       Era_Names =>
           (BCE => 764, CE => 762),
       Date_Formats =>
           (Full => 101, Long => 265, Medium => 264, Short => 276),
       Time_Formats =>
           (Full => 370, Long => 369, Medium => 368, Short => 367),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 13, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 322, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 6)),

      (Tag => "EN        ",
       Level => 1,
       Name => 329,
       Short_Day_Names =>
          (Sun => 207, Mon => 172, Tue => 216,
           Wed => 220, Thu => 214, Fri => 116,
           Sat => 201),
       Full_Day_Names =>
          (Sun => 208, Mon => 173, Tue => 217,
           Wed => 221, Thu => 215, Fri => 117,
           Sat => 202),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 144, Feb => 113, Mar => 163,
           Apr => 69, May => 165, Jun => 150,
           Jul => 148, Aug => 73, Sep => 204,
           Oct => 186, Nov => 182, Dec => 85),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 76, CE => 66),
       Date_Formats =>
           (Full => 99, Long => 158, Medium => 157, Short => 156),
       Time_Formats =>
           (Full => 370, Long => 369, Medium => 368, Short => 367),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 24, Scientific => 8,
            Percent => 3, Currency => 699)),

      (Tag => "EN     AU ",
       Level => 2,
       Name => 330,
       Short_Day_Names =>
          (Sun => 207, Mon => 172, Tue => 216,
           Wed => 220, Thu => 214, Fri => 116,
           Sat => 201),
       Full_Day_Names =>
          (Sun => 208, Mon => 173, Tue => 217,
           Wed => 221, Thu => 215, Fri => 117,
           Sat => 202),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 144, Feb => 113, Mar => 163,
           Apr => 69, May => 165, Jun => 150,
           Jul => 148, Aug => 73, Sep => 204,
           Oct => 186, Nov => 182, Dec => 85),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 76, CE => 66),
       Date_Formats =>
           (Full => 101, Long => 265, Medium => 288, Short => 278),
       Time_Formats =>
           (Full => 370, Long => 369, Medium => 368, Short => 367),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 24, Scientific => 8,
            Percent => 3, Currency => 698)),

      (Tag => "EN     GB ",
       Level => 2,
       Name => 331,
       Short_Day_Names =>
          (Sun => 207, Mon => 172, Tue => 216,
           Wed => 220, Thu => 214, Fri => 116,
           Sat => 201),
       Full_Day_Names =>
          (Sun => 208, Mon => 173, Tue => 217,
           Wed => 221, Thu => 215, Fri => 117,
           Sat => 202),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 144, Feb => 113, Mar => 163,
           Apr => 69, May => 165, Jun => 150,
           Jul => 148, Aug => 73, Sep => 204,
           Oct => 186, Nov => 182, Dec => 85),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 76, CE => 66),
       Date_Formats =>
           (Full => 101, Long => 265, Medium => 264, Short => 288),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 24, Scientific => 8,
            Percent => 3, Currency => 698)),

      (Tag => "EN     IE ",
       Level => 2,
       Name => 332,
       Short_Day_Names =>
          (Sun => 207, Mon => 172, Tue => 216,
           Wed => 220, Thu => 214, Fri => 116,
           Sat => 201),
       Full_Day_Names =>
          (Sun => 208, Mon => 173, Tue => 217,
           Wed => 221, Thu => 215, Fri => 117,
           Sat => 202),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 144, Feb => 113, Mar => 163,
           Apr => 69, May => 165, Jun => 150,
           Jul => 148, Aug => 73, Sep => 204,
           Oct => 186, Nov => 182, Dec => 85),
       Day_Period_Names =>
           (AM => 227, Noon => 520, PM => 547),
       Era_Names =>
           (BCE => 76, CE => 66),
       Date_Formats =>
           (Full => 96, Long => 265, Medium => 264, Short => 288),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 24, Scientific => 8,
            Percent => 3, Currency => 698)),

      (Tag => "ES        ",
       Level => 1,
       Name => 336,
       Short_Day_Names =>
          (Sun => 310, Mon => 445, Tue => 473,
           Wed => 496, Thu => 403, Fri => 652,
           Sat => 617),
       Full_Day_Names =>
          (Sun => 312, Mon => 449, Tue => 480,
           Wed => 497, Thu => 404, Fri => 653,
           Sat => 618),
       Short_Month_Names =>
          (Jan => 333, Feb => 339, Mar => 473,
           Apr => 229, May => 486, Jun => 413,
           Jul => 408, Aug => 231, Sep => 584,
           Oct => 528, Nov => 521, Dec => 303),
       Full_Month_Names =>
          (Jan => 334, Feb => 343, Mar => 483,
           Apr => 230, May => 487, Jun => 417,
           Jul => 412, Aug => 232, Sep => 591,
           Oct => 532, Nov => 527, Dec => 305),
       Day_Period_Names =>
           (AM => 227, Noon => 520, PM => 547),
       Era_Names =>
           (BCE => 226, CE => 272),
       Date_Formats =>
           (Full => 95, Long => 263, Medium => 288, Short => 287),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 701)),

      (Tag => "FI        ",
       Level => 1,
       Name => 351,
       Short_Day_Names =>
          (Sun => 609, Mon => 459, Tue => 629,
           Wed => 426, Thu => 636, Fri => 551,
           Sat => 434),
       Full_Day_Names =>
          (Sun => 610, Mon => 461, Tue => 630,
           Wed => 428, Thu => 641, Fri => 552,
           Sat => 435),
       Short_Month_Names =>
          (Jan => 625, Feb => 373, Mar => 460,
           Apr => 375, May => 642, Jun => 429,
           Jul => 372, Aug => 327, Sep => 612,
           Oct => 442, Nov => 477, Dec => 402),
       Full_Month_Names =>
          (Jan => 625, Feb => 373, Mar => 460,
           Apr => 375, May => 642, Jun => 429,
           Jul => 372, Aug => 327, Sep => 612,
           Oct => 442, Nov => 477, Dec => 402),
       Day_Period_Names =>
           (AM => 236, Noon => 520, PM => 381),
       Era_Names =>
           (BCE => 325, CE => 388),
       Date_Formats =>
           (Full => 256, Long => 271, Medium => 275, Short => 275),
       Time_Formats =>
           (Full => 124, Long => 123, Medium => 122, Short => 121),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 335, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 6)),

      (Tag => "FR        ",
       Level => 1,
       Name => 353,
       Short_Day_Names =>
          (Sun => 306, Mon => 446, Tue => 474,
           Wed => 491, Thu => 399, Fri => 649,
           Sat => 580),
       Full_Day_Names =>
          (Sun => 307, Mon => 447, Tue => 476,
           Wed => 493, Thu => 400, Fri => 650,
           Sat => 581),
       Short_Month_Names =>
          (Jan => 397, Feb => 357, Mar => 478,
           Apr => 253, May => 466, Jun => 407,
           Jul => 405, Aug => 235, Sep => 586,
           Oct => 529, Nov => 522, Dec => 319),
       Full_Month_Names =>
          (Jan => 398, Feb => 358, Mar => 478,
           Apr => 254, May => 466, Jun => 407,
           Jul => 406, Aug => 235, Sep => 589,
           Oct => 530, Nov => 525, Dec => 320),
       Day_Period_Names =>
           (AM => 67, Noon => 494, PM => 190),
       Era_Names =>
           (BCE => 252, CE => 237),
       Date_Formats =>
           (Full => 96, Long => 265, Medium => 264, Short => 287),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 6)),

      (Tag => "HE        ",
       Level => 1,
       Name => 371,
       Short_Day_Names =>
          (Sun => 813, Mon => 814, Tue => 815,
           Wed => 816, Thu => 817, Fri => 818,
           Sat => 841),
       Full_Day_Names =>
          (Sun => 820, Mon => 825, Tue => 824,
           Wed => 821, Thu => 819, Fri => 823,
           Sat => 822),
       Short_Month_Names =>
          (Jan => 828, Feb => 839, Mar => 834,
           Apr => 807, May => 833, Jun => 826,
           Jul => 811, Aug => 802, Sep => 837,
           Oct => 804, Nov => 835, Dec => 809),
       Full_Month_Names =>
          (Jan => 829, Feb => 840, Mar => 834,
           Apr => 808, May => 833, Jun => 827,
           Jul => 812, Aug => 803, Sep => 838,
           Oct => 805, Nov => 836, Dec => 810),
       Day_Period_Names =>
           (AM => 832, Noon => 520, PM => 806),
       Era_Names =>
           (BCE => 831, CE => 830),
       Date_Formats =>
           (Full => 103, Long => 269, Medium => 268, Short => 287),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 6)),

      (Tag => "HU        ",
       Level => 1,
       Name => 374,
       Short_Day_Names =>
          (Sun => 218, Mon => 118, Tue => 152,
           Wed => 209, Thu => 80, Fri => 189,
           Sat => 210),
       Full_Day_Names =>
          (Sun => 647, Mon => 376, Tue => 427,
           Wed => 615, Thu => 258, Fri => 570,
           Sat => 616),
       Short_Month_Names =>
          (Jan => 391, Feb => 342, Mar => 503,
           Apr => 707, May => 500, Jun => 423,
           Jul => 419, Aug => 246, Sep => 613,
           Oct => 535, Nov => 522, Dec => 294),
       Full_Month_Names =>
          (Jan => 395, Feb => 347, Mar => 504,
           Apr => 708, May => 502, Jun => 425,
           Jul => 421, Aug => 251, Sep => 614,
           Oct => 537, Nov => 523, Dec => 295),
       Day_Period_Names =>
           (AM => 292, Noon => 520, PM => 316),
       Era_Names =>
           (BCE => 377, CE => 378),
       Date_Formats =>
           (Full => 666, Long => 665, Medium => 674, Short => 674),
       Time_Formats =>
           (Full => 128, Long => 127, Medium => 126, Short => 125),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 6)),

      (Tag => "IT        ",
       Level => 1,
       Name => 382,
       Short_Day_Names =>
          (Sun => 310, Mon => 445, Tue => 473,
           Wed => 490, Thu => 361, Fri => 648,
           Sat => 578),
       Full_Day_Names =>
          (Sun => 311, Mon => 448, Tue => 479,
           Wed => 492, Thu => 362, Fri => 651,
           Sat => 579),
       Short_Month_Names =>
          (Jan => 359, Feb => 339, Mar => 473,
           Apr => 238, May => 464, Jun => 363,
           Jul => 443, Aug => 231, Sep => 592,
           Oct => 542, Nov => 521, Dec => 303),
       Full_Month_Names =>
          (Jan => 360, Feb => 341, Mar => 483,
           Apr => 241, May => 465, Jun => 364,
           Jul => 444, Aug => 232, Sep => 594,
           Oct => 543, Nov => 525, Dec => 304),
       Day_Period_Names =>
           (AM => 458, Noon => 520, PM => 546),
       Era_Names =>
           (BCE => 228, CE => 279),
       Date_Formats =>
           (Full => 96, Long => 282, Medium => 289, Short => 287),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 701)),

      (Tag => "IW        ",
       Level => 1,
       Name => 387,
       Short_Day_Names =>
          (Sun => 813, Mon => 814, Tue => 815,
           Wed => 816, Thu => 817, Fri => 818,
           Sat => 841),
       Full_Day_Names =>
          (Sun => 820, Mon => 825, Tue => 824,
           Wed => 821, Thu => 819, Fri => 823,
           Sat => 822),
       Short_Month_Names =>
          (Jan => 828, Feb => 839, Mar => 834,
           Apr => 807, May => 833, Jun => 826,
           Jul => 811, Aug => 802, Sep => 837,
           Oct => 804, Nov => 835, Dec => 809),
       Full_Month_Names =>
          (Jan => 829, Feb => 840, Mar => 834,
           Apr => 808, May => 833, Jun => 827,
           Jul => 812, Aug => 803, Sep => 838,
           Oct => 805, Nov => 836, Dec => 810),
       Day_Period_Names =>
           (AM => 832, Noon => 520, PM => 806),
       Era_Names =>
           (BCE => 831, CE => 830),
       Date_Formats =>
           (Full => 103, Long => 269, Medium => 268, Short => 287),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 6)),

      (Tag => "JA        ",
       Level => 1,
       Name => 389,
       Short_Day_Names =>
          (Sun => 938, Mon => 947, Tue => 953,
           Wed => 951, Thu => 949, Fri => 964,
           Sat => 936),
       Full_Day_Names =>
          (Sun => 939, Mon => 948, Tue => 954,
           Wed => 952, Thu => 950, Fri => 965,
           Sat => 937),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 39, Feb => 42, Mar => 45,
           Apr => 48, May => 51, Jun => 54,
           Jul => 57, Aug => 60, Sep => 63,
           Oct => 31, Nov => 34, Dec => 37),
       Day_Period_Names =>
           (AM => 927, Noon => 520, PM => 928),
       Era_Names =>
           (BCE => 76, CE => 66),
       Date_Formats =>
           (Full => 678, Long => 677, Medium => 676, Short => 670),
       Time_Formats =>
           (Full => 141, Long => 127, Medium => 126, Short => 125),
       Date_Time_Formats =>
           (Full => 696, Long => 696, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 698)),

      (Tag => "KO        ",
       Level => 1,
       Name => 430,
       Short_Day_Names =>
          (Sun => 978, Mon => 976, Tue => 982,
           Wed => 972, Thu => 969, Fri => 966,
           Sat => 980),
       Full_Day_Names =>
          (Sun => 979, Mon => 977, Tue => 983,
           Wed => 973, Thu => 970, Fri => 967,
           Sat => 981),
       Short_Month_Names =>
          (Jan => 142, Feb => 111, Mar => 162,
           Apr => 68, May => 165, Jun => 149,
           Jul => 146, Aug => 72, Sep => 203,
           Oct => 185, Nov => 181, Dec => 84),
       Full_Month_Names =>
          (Jan => 40, Feb => 43, Mar => 46,
           Apr => 49, May => 52, Jun => 55,
           Jul => 58, Aug => 61, Sep => 64,
           Oct => 32, Nov => 35, Dec => 38),
       Day_Period_Names =>
           (AM => 974, Noon => 520, PM => 975),
       Era_Names =>
           (BCE => 968, CE => 971),
       Date_Formats =>
           (Full => 680, Long => 679, Medium => 673, Short => 668),
       Time_Formats =>
           (Full => 225, Long => 224, Medium => 223, Short => 222),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 698)),

      (Tag => "NB        ",
       Level => 1,
       Name => 511,
       Short_Day_Names =>
          (Sun => 623, Mon => 471, Tue => 632,
           Wed => 540, Thu => 638, Fri => 355,
           Sat => 456),
       Full_Day_Names =>
          (Sun => 624, Mon => 472, Tue => 633,
           Wed => 541, Thu => 640, Fri => 356,
           Sat => 457),
       Short_Month_Names =>
          (Jan => 391, Feb => 340, Mar => 478,
           Apr => 239, May => 466, Jun => 416,
           Jul => 411, Aug => 246, Sep => 585,
           Oct => 535, Nov => 522, Dec => 298),
       Full_Month_Names =>
          (Jan => 393, Feb => 344, Mar => 478,
           Apr => 240, May => 466, Jun => 416,
           Jul => 411, Aug => 247, Sep => 587,
           Oct => 536, Nov => 523, Dec => 299),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 337, CE => 323),
       Date_Formats =>
           (Full => 97, Long => 271, Medium => 270, Short => 285),
       Time_Formats =>
           (Full => 11, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 701)),

      (Tag => "NL        ",
       Level => 1,
       Name => 517,
       Short_Day_Names =>
          (Sun => 688, Mon => 459, Tue => 302,
           Wed => 659, Thu => 309, Fri => 656,
           Sat => 682),
       Full_Day_Names =>
          (Sun => 689, Mon => 462, Tue => 308,
           Wed => 660, Thu => 313, Fri => 657,
           Sat => 685),
       Short_Month_Names =>
          (Jan => 391, Feb => 340, Mar => 498,
           Apr => 239, May => 488, Jun => 414,
           Jul => 409, Aug => 246, Sep => 585,
           Oct => 535, Nov => 522, Dec => 294),
       Full_Month_Names =>
          (Jan => 394, Feb => 345, Mar => 463,
           Apr => 240, May => 488, Jun => 416,
           Jul => 411, Aug => 250, Sep => 587,
           Oct => 536, Nov => 523, Dec => 295),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 646, CE => 507),
       Date_Formats =>
           (Full => 96, Long => 265, Medium => 264, Short => 284),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 702)),

      (Tag => "NO        ",
       Level => 1,
       Name => 518,
       Short_Day_Names =>
          (Sun => 623, Mon => 471, Tue => 632,
           Wed => 540, Thu => 638, Fri => 355,
           Sat => 456),
       Full_Day_Names =>
          (Sun => 624, Mon => 472, Tue => 633,
           Wed => 541, Thu => 640, Fri => 356,
           Sat => 457),
       Short_Month_Names =>
          (Jan => 391, Feb => 340, Mar => 478,
           Apr => 239, May => 466, Jun => 416,
           Jul => 411, Aug => 246, Sep => 585,
           Oct => 535, Nov => 522, Dec => 298),
       Full_Month_Names =>
          (Jan => 393, Feb => 344, Mar => 478,
           Apr => 240, May => 466, Jun => 416,
           Jul => 411, Aug => 247, Sep => 587,
           Oct => 536, Nov => 523, Dec => 299),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 337, CE => 323),
       Date_Formats =>
           (Full => 97, Long => 271, Medium => 270, Short => 285),
       Time_Formats =>
           (Full => 11, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 701)),

      (Tag => "PL        ",
       Level => 1,
       Name => 556,
       Short_Day_Names =>
          (Sun => 515, Mon => 559, Tue => 663,
           Wed => 718, Thu => 261, Fri => 567,
           Sat => 601),
       Full_Day_Names =>
          (Sun => 516, Mon => 562, Tue => 664,
           Wed => 719, Thu => 262, Fri => 555,
           Sat => 602),
       Short_Month_Names =>
          (Jan => 606, Feb => 451, Mar => 473,
           Apr => 432, May => 468, Jun => 259,
           Jul => 437, Aug => 597, Sep => 661,
           Oct => 549, Nov => 439, Dec => 365),
       Full_Month_Names =>
          (Jan => 607, Feb => 452, Mar => 475,
           Apr => 433, May => 469, Jun => 260,
           Jul => 438, Aug => 598, Sep => 662,
           Oct => 550, Nov => 440, Dec => 366),
       Day_Period_Names =>
           (AM => 67, Noon => 658, PM => 190),
       Era_Names =>
           (BCE => 548, CE => 508),
       Date_Formats =>
           (Full => 101, Long => 265, Medium => 264, Short => 286),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 695, Long => 695, Medium => 695, Short => 695),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 6)),

      (Tag => "PT        ",
       Level => 1,
       Name => 566,
       Short_Day_Names =>
          (Sun => 310, Mon => 582, Tue => 626,
           Wed => 572, Thu => 574, Fri => 595,
           Sat => 617),
       Full_Day_Names =>
          (Sun => 312, Mon => 583, Tue => 627,
           Wed => 573, Thu => 575, Fri => 596,
           Sat => 618),
       Short_Month_Names =>
          (Jan => 390, Feb => 349, Mar => 473,
           Apr => 229, May => 466, Jun => 413,
           Jul => 408, Aug => 231, Sep => 592,
           Oct => 544, Nov => 521, Dec => 300),
       Full_Month_Names =>
          (Jan => 392, Feb => 350, Mar => 484,
           Apr => 230, May => 467, Jun => 415,
           Jul => 410, Aug => 232, Sep => 593,
           Oct => 545, Nov => 526, Dec => 301),
       Day_Period_Names =>
           (AM => 67, Noon => 489, PM => 190),
       Era_Names =>
           (BCE => 226, CE => 272),
       Date_Formats =>
           (Full => 100, Long => 263, Medium => 288, Short => 287),
       Time_Formats =>
           (Full => 130, Long => 129, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 699)),

      (Tag => "RO        ",
       Level => 1,
       Name => 576,
       Short_Day_Names =>
          (Sun => 92, Mon => 155, Tue => 160,
           Wed => 167, Thu => 145, Fri => 219,
           Sat => 211),
       Full_Day_Names =>
          (Sun => 318, Mon => 450, Tue => 485,
           Wed => 495, Thu => 401, Fri => 654,
           Sat => 619),
       Short_Month_Names =>
          (Jan => 379, Feb => 340, Mar => 474,
           Apr => 239, May => 466, Jun => 385,
           Jul => 383, Aug => 246, Sep => 586,
           Oct => 529, Nov => 522, Dec => 294),
       Full_Month_Names =>
          (Jan => 380, Feb => 346, Mar => 481,
           Apr => 242, May => 466, Jun => 386,
           Jul => 384, Aug => 247, Sep => 590,
           Oct => 531, Nov => 519, Dec => 297),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 709, CE => 273),
       Date_Formats =>
           (Full => 101, Long => 265, Medium => 286, Short => 286),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 695, Long => 695, Medium => 695, Short => 695),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 6)),

      (Tag => "RU        ",
       Level => 1,
       Name => 577,
       Short_Day_Names =>
          (Sun => 771, Mon => 786, Tue => 772,
           Wed => 793, Thu => 799, Fri => 788,
           Sat => 790),
       Full_Day_Names =>
          (Sun => 770, Mon => 787, Tue => 773,
           Wed => 794, Thu => 798, Fri => 789,
           Sat => 795),
       Short_Month_Names =>
          (Jan => 800, Feb => 796, Mar => 779,
           Apr => 768, May => 780, Jun => 778,
           Jul => 777, Aug => 766, Sep => 791,
           Oct => 784, Nov => 782, Dec => 774),
       Full_Month_Names =>
          (Jan => 801, Feb => 797, Mar => 779,
           Apr => 769, May => 780, Jun => 778,
           Jul => 777, Aug => 767, Sep => 792,
           Oct => 785, Nov => 783, Dec => 775),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 776, CE => 781),
       Date_Formats =>
           (Full => 102, Long => 266, Medium => 286, Short => 285),
       Time_Formats =>
           (Full => 128, Long => 127, Medium => 126, Short => 125),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 6)),

      (Tag => "SK        ",
       Level => 1,
       Name => 599,
       Short_Day_Names =>
          (Sun => 512, Mon => 557, Tue => 644,
           Wed => 604, Thu => 722, Fri => 553,
           Sat => 600),
       Full_Day_Names =>
          (Sun => 513, Mon => 560, Tue => 645,
           Wed => 605, Thu => 723, Fri => 554,
           Sat => 602),
       Short_Month_Names =>
          (Jan => 390, Feb => 339, Mar => 473,
           Apr => 238, May => 499, Jun => 422,
           Jul => 418, Aug => 245, Sep => 584,
           Oct => 534, Nov => 521, Dec => 293),
       Full_Month_Names =>
          (Jan => 396, Feb => 348, Mar => 475,
           Apr => 243, May => 501, Jun => 424,
           Jul => 420, Aug => 248, Sep => 588,
           Oct => 538, Nov => 524, Dec => 296),
       Day_Period_Names =>
           (AM => 315, Noon => 520, PM => 563),
       Era_Names =>
           (BCE => 564, CE => 509),
       Date_Formats =>
           (Full => 104, Long => 271, Medium => 275, Short => 275),
       Time_Formats =>
           (Full => 128, Long => 127, Medium => 126, Short => 125),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 6)),

      (Tag => "SV        ",
       Level => 1,
       Name => 611,
       Short_Day_Names =>
          (Sun => 620, Mon => 505, Tue => 634,
           Wed => 539, Thu => 639, Fri => 354,
           Sat => 453),
       Full_Day_Names =>
          (Sun => 621, Mon => 506, Tue => 635,
           Wed => 541, Thu => 640, Fri => 356,
           Sat => 454),
       Short_Month_Names =>
          (Jan => 390, Feb => 339, Mar => 473,
           Apr => 238, May => 468, Jun => 413,
           Jul => 408, Aug => 245, Sep => 584,
           Oct => 534, Nov => 521, Dec => 293),
       Full_Month_Names =>
          (Jan => 394, Feb => 345, Mar => 478,
           Apr => 240, May => 468, Jun => 416,
           Jul => 411, Aug => 249, Sep => 587,
           Oct => 536, Nov => 523, Dec => 295),
       Day_Period_Names =>
           (AM => 352, Noon => 520, PM => 328),
       Era_Names =>
           (BCE => 337, CE => 323),
       Date_Formats =>
           (Full => 98, Long => 265, Medium => 264, Short => 672),
       Time_Formats =>
           (Full => 11, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 697,
            List => 65, Zero => 16,
            Plus => 12, Minus => 921,
            Exponent => 706, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 703, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 7, Currency => 6)),

      (Tag => "TH        ",
       Level => 1,
       Name => 628,
       Short_Day_Names =>
          (Sun => 917, Mon => 886, Tue => 916,
           Wed => 892, Thu => 895, Fri => 911,
           Sat => 912),
       Full_Day_Names =>
          (Sun => 909, Mon => 904, Tue => 908,
           Wed => 906, Thu => 905, Fri => 907,
           Sat => 910),
       Short_Month_Names =>
          (Jan => 898, Feb => 879, Mar => 902,
           Apr => 918, May => 893, Jun => 900,
           Jul => 878, Aug => 913, Sep => 880,
           Oct => 887, Nov => 894, Dec => 889),
       Full_Month_Names =>
          (Jan => 899, Feb => 883, Mar => 903,
           Apr => 919, May => 897, Jun => 901,
           Jul => 881, Aug => 914, Sep => 882,
           Oct => 888, Nov => 896, Dec => 890),
       Day_Period_Names =>
           (AM => 884, Noon => 520, PM => 915),
       Era_Names =>
           (BCE => 891, CE => 885),
       Date_Formats =>
           (Full => 106, Long => 265, Medium => 264, Short => 277),
       Time_Formats =>
           (Full => 120, Long => 119, Medium => 126, Short => 125),
       Date_Time_Formats =>
           (Full => 695, Long => 695, Medium => 695, Short => 695),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 700)),

      (Tag => "TR        ",
       Level => 1,
       Name => 643,
       Short_Day_Names =>
          (Sun => 191, Mon => 196, Tue => 198,
           Wed => 704, Thu => 194, Fri => 81,
           Sat => 79),
       Full_Day_Names =>
          (Sun => 192, Mon => 193, Tue => 199,
           Wed => 705, Thu => 195, Fri => 82,
           Sat => 83),
       Short_Month_Names =>
          (Jan => 183, Feb => 720, Mar => 162,
           Apr => 179, May => 165, Jun => 139,
           Jul => 212, Aug => 74, Sep => 109,
           Oct => 107, Nov => 153, Dec => 70),
       Full_Month_Names =>
          (Jan => 184, Feb => 721, Mar => 164,
           Apr => 180, May => 166, Jun => 140,
           Jul => 213, Aug => 75, Sep => 110,
           Oct => 108, Nov => 154, Dec => 71),
       Day_Period_Names =>
           (AM => 67, Noon => 520, PM => 190),
       Era_Names =>
           (BCE => 175, CE => 159),
       Date_Formats =>
           (Full => 283, Long => 282, Medium => 281, Short => 286),
       Time_Formats =>
           (Full => 138, Long => 137, Medium => 136, Short => 135),
       Date_Time_Formats =>
           (Full => 694, Long => 694, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 13, Group => 15,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 10, Currency => 6)),

      (Tag => "ZH        ",
       Level => 1,
       Name => 686,
       Short_Day_Names =>
          (Sun => 935, Mon => 929, Tue => 931,
           Wed => 930, Thu => 934, Fri => 932,
           Sat => 933),
       Full_Day_Names =>
          (Sun => 946, Mon => 940, Tue => 942,
           Wed => 941, Thu => 945, Fri => 943,
           Sat => 944),
       Short_Month_Names =>
          (Jan => 39, Feb => 42, Mar => 45,
           Apr => 48, May => 51, Jun => 54,
           Jul => 57, Aug => 60, Sep => 63,
           Oct => 31, Nov => 34, Dec => 37),
       Full_Month_Names =>
          (Jan => 39, Feb => 42, Mar => 45,
           Apr => 48, May => 51, Jun => 54,
           Jul => 57, Aug => 60, Sep => 63,
           Oct => 31, Nov => 34, Dec => 37),
       Day_Period_Names =>
           (AM => 923, Noon => 520, PM => 924),
       Era_Names =>
           (BCE => 926, CE => 925),
       Date_Formats =>
           (Full => 678, Long => 677, Medium => 671, Short => 667),
       Time_Formats =>
           (Full => 691, Long => 683, Medium => 234, Short => 233),
       Date_Time_Formats =>
           (Full => 696, Long => 696, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 698)),

      (Tag => "ZH HANT   ",
       Level => 3,
       Name => 687,
       Short_Day_Names =>
          (Sun => 963, Mon => 957, Tue => 959,
           Wed => 958, Thu => 962, Fri => 960,
           Sat => 961),
       Full_Day_Names =>
          (Sun => 946, Mon => 940, Tue => 942,
           Wed => 941, Thu => 945, Fri => 943,
           Sat => 944),
       Short_Month_Names =>
          (Jan => 39, Feb => 42, Mar => 45,
           Apr => 48, May => 51, Jun => 54,
           Jul => 57, Aug => 60, Sep => 63,
           Oct => 31, Nov => 34, Dec => 37),
       Full_Month_Names =>
          (Jan => 39, Feb => 42, Mar => 45,
           Apr => 48, May => 51, Jun => 54,
           Jul => 57, Aug => 60, Sep => 63,
           Oct => 31, Nov => 34, Dec => 37),
       Day_Period_Names =>
           (AM => 923, Noon => 520, PM => 924),
       Era_Names =>
           (BCE => 956, CE => 955),
       Date_Formats =>
           (Full => 678, Long => 677, Medium => 675, Short => 669),
       Time_Formats =>
           (Full => 692, Long => 684, Medium => 234, Short => 233),
       Date_Time_Formats =>
           (Full => 696, Long => 696, Medium => 694, Short => 694),
       Numeric_Items =>
           (Decimal => 15, Group => 13,
            List => 65, Zero => 16,
            Plus => 12, Minus => 14,
            Exponent => 93, Percent => 9,
            Permille => 920, Infinity => 922,
            Nan => 178, Digit_Pattern => 2),
       Numeric_Formats =>
           (Decimal => 4, Scientific => 8,
            Percent => 3, Currency => 698)));

   --  END-CLDR-DATA                                                       --
   --  End of the generated data inserted into this file from the          --
   --  Unicode.org CDLR data.                                              --
   --------------------------------------------------------------------------

   procedure Environment_Initialize;
   --  Initialize the current locale based on the ZB_LANG/LANG environment
   --  variables.

   procedure Decompose_Name (Name      : Wide_String;
                             Language  : out Language_Type;
                             Script    : out Script_Type;
                             Territory : out Territory_Type);
   --  Decompose a locale name, e.g., "en", "en_Latn_US", etc. into it's
   --  component language, script and territory values.

   function Find_Traits (Language  : Wide_String;
                         Script    : Wide_String;
                         Territory : Wide_String) return Trait_Index_Type;
   --  Locate the traits entry "matching" the given locale data (matching
   --  attempts locale resolution, i.e., "fr_FR" => "fr", etc.

   procedure Lookup_Traits (Language  : Wide_String;
                            Script    : Wide_String;
                            Territory : Wide_String;
                            Index     : out Trait_Index_Type;
                            Found     : out Boolean);
   --  Binary search lookup of a traits by tag value.

   function To_String (Index : String_Index_Type) return Wide_String;
   --  Convert a string index of a pooled string to a string value.

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Locale_Type) return Boolean is
   begin
      return Left.Language_Code = Right.Language_Code
         and Left.Script_Code = Right.Script_Code
         and Left.Territory_Code = Right.Territory_Code;
   end "=";

   --------------------
   -- Current_Locale --
   --------------------

   function Current_Locale return Locale_Type is
   begin
      return Current_Locale_Value;
   end Current_Locale;

   -----------------
   -- Date_Format --
   -----------------

   function Date_Format (Locale : Locale_Type;
                         Style  : Date_Time_Style_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Date_Formats (Style));
   end Date_Format;

   ----------------------
   -- Date_Time_Format --
   ----------------------

   function Date_Time_Format (Locale : Locale_Type;
                              Style  : Date_Time_Style_Type)
      return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Date_Time_Formats (Style));
   end Date_Time_Format;

   ---------------------
   -- Day_Period_Name --
   ---------------------

   function Day_Period_Name (Locale     : Locale_Type;
                             Day_Period : Day_Period_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Day_Period_Names (Day_Period));
   end Day_Period_Name;

   --------------------
   -- Decompose_Name --
   --------------------

   procedure Decompose_Name (Name      : Wide_String;
                             Language  : out Language_Type;
                             Script    : out Script_Type;
                             Territory : out Territory_Type) is

      Separator : Wide_Character := '_';
      First     : Positive := Name'First;
      Last      : Natural := 0;

      procedure Get_Separated_Item (Result : out Wide_String;
                                    From   : in out Positive;
                                    Last   : Natural);

      procedure Get_Separated_Item (Result : out Wide_String;
                                    From   : in out Positive;
                                    Last   : Natural) is

         First    : constant Positive := From;
         Position : Natural := First;

      begin
         loop
            Position := Position + 1;
            exit when Position >= Last or else Name (Position) = Separator;
         end loop;
         From := Position + 1;
         if Position >= Last then
            Position := Last;
         elsif Name (Position) = Separator then
            Position := Position - 1;
         end if;
         Result := Head (Name (First .. Position), Result'Length);
      end Get_Separated_Item;

   begin
      --  If strings contains dashes, assume it's the separator, e.g., "en-us"
      if Index (Name, "-", First) /= 0 then
         Separator := '-';
      end if;
      --  Ignore any encoding info, e.g., "en_US.utf8"
      Last := Index (Name, ".", First);
      if Last = 0 then
         Last := Name'Last;
      else
         Last := Last - 1;
      end if;
      Get_Separated_Item (Language, First, Last);
      Get_Separated_Item (Script, First, Last);
      Get_Separated_Item (Territory, First, Last);
      if Script (Script'Last) = ' ' then
         --  Fix up, the script is really the territory
         Territory := Script (Territory'Range);
         Script := Empty_Script;
      end if;
   end Decompose_Name;

   ----------------------------
   -- Environment_Initialize --
   ----------------------------

   procedure Environment_Initialize is

      use Ada.Environment_Variables;

      ZBLang : constant String := "ZB_LANG";

   begin
      if Exists (ZBLang) then
         Set_Locale (To_Wide_String (Value (ZBLang)));
      else
         Set_Locale (ZanyBlue.OS.OS_Locale_Name);
      end if;
   end Environment_Initialize;

   --------------
   -- Era_Name --
   --------------

   function Era_Name (Locale : Locale_Type;
                      Era    : Era_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Era_Names (Era));
   end Era_Name;

   -----------------
   -- Find_Traits --
   -----------------

   function Find_Traits (Language  : Wide_String;
                         Script    : Wide_String;
                         Territory : Wide_String) return Trait_Index_Type
   is
      Result : Trait_Index_Type := 1;
      Found  : Boolean := False;
   begin
      Lookup_Traits (Language, Script, Territory, Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, Script, "", Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, "", Territory, Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, "", "", Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits ("", "", "", Result, Found);
      return Result;
   end Find_Traits;

   -------------------
   -- Full_Day_Name --
   -------------------

   function Full_Day_Name (Locale : Locale_Type;
                           Day    : Day_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Full_Day_Names (Day));
   end Full_Day_Name;

   ---------------------
   -- Full_Month_Name --
   ---------------------

   function Full_Month_Name (Locale : Locale_Type;
                             Month  : Month_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Full_Month_Names (Month));
   end Full_Month_Name;

   ----------------------
   -- Get_Locale_Codes --
   ----------------------

   procedure Get_Locale_Codes (Locale    : Locale_Type;
                               Language  : in out Language_Type;
                               Script    : in out Script_Type;
                               Territory : in out Territory_Type) is
   begin
      Language := Locale.Language_Code;
      Script := Locale.Script_Code;
      Territory := Locale.Territory_Code;
   end Get_Locale_Codes;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Locale_Type) return Ada.Containers.Hash_Type is
   begin
      return Wide_Hash (Locale_Name (Key));
   end Hash;

   -----------------------
   -- Is_Locale_Defined --
   -----------------------

   function Is_Locale_Defined (Language  : Wide_String;
                               Script    : Wide_String;
                               Territory : Wide_String) return Boolean is

      Index : Trait_Index_Type;
      Found : Boolean;

   begin
      Lookup_Traits (Language, Script, Territory, Index, Found);
      return Found;
   end Is_Locale_Defined;

   --------------------
   -- Is_Root_Locale --
   --------------------

   function Is_Root_Locale (Locale : Locale_Type) return Boolean is
   begin
      return Locale.Language_Code  = Empty_Language
         and Locale.Script_Code    = Empty_Script
         and Locale.Territory_Code = Empty_Territory;
   end Is_Root_Locale;

   --------------
   -- Language --
   --------------

   function Language (Locale : Locale_Type) return Wide_String is
   begin
      return Non_Blank_Prefix (Locale.Language_Code);
   end Language;

   -------------------
   -- Locale_Digits --
   -------------------

   function Locale_Digits (Locale    : Locale_Type;
                           Lowercase : Boolean) return Wide_String is
      Locale_Zero : constant Wide_String := Numeric_Item (Locale, Zero);
      Zero        : Natural;
      Result      : Wide_String (1 .. 16);
   begin
      if Locale_Zero'Length > 0 then
         Zero := Wide_Character'Pos (Locale_Zero (Locale_Zero'First));
      else
         Zero := Wide_Character'Pos ('0');
      end if;
      for I in 0 .. 9 loop
         Result (I + 1) := Wide_Character'Val (Zero + I);
      end loop;
      if Lowercase then
         Result (11 .. 16) := "abcdef";
      else
         Result (11 .. 16) := "ABCDEF";
      end if;
      return Result;
   end Locale_Digits;

   ------------------
   -- Locale_Level --
   ------------------

   function Locale_Level (Locale : Locale_Type) return Level_Type is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Level;
   end Locale_Level;

   -----------------
   -- Locale_Name --
   -----------------

   function Locale_Name (Language       : Language_Type;
                         Script         : Script_Type;
                         Territory      : Territory_Type) return Wide_String is

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : Wide_Character);

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : Wide_String;
                        Include_UC : Boolean := True);

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : Wide_Character) is
      begin
         Position := Position + 1;
         Result (Position) := Value;
      end Append;

      procedure Append (Result     : in out Wide_String;
                        Position   : in out Natural;
                        Value      : Wide_String;
                        Include_UC : Boolean := True) is
      begin
         if Value (Value'First) = ' ' then
            return;
         end if;
         if Include_UC then
            Append (Result, Position, '_');
         end if;
         for I in Value'Range loop
            if Value (I) /= ' ' then
               Append (Result, Position, Value (I));
            end if;
         end loop;
      end Append;

      Result   : Wide_String (1 .. 12);
      Position : Natural := 0;

   begin
      if Language (Language'First) /= ' ' then
         Append (Result, Position, Language, False);
         Append (Result, Position, Script);
         Append (Result, Position, Territory);
      end if;
      return Result (1 .. Position);
   end Locale_Name;

   -----------------
   -- Locale_Name --
   -----------------

   function Locale_Name (Locale : Locale_Type) return Wide_String is
   begin
      return Locale_Name (Locale.Language_Code,
                          Locale.Script_Code,
                          Locale.Territory_Code);
   end Locale_Name;

   -------------------
   -- Lookup_Traits --
   -------------------

   procedure Lookup_Traits (Language  : Wide_String;
                            Script    : Wide_String;
                            Territory : Wide_String;
                            Index     : out Trait_Index_Type;
                            Found     : out Boolean) is


      Key : Tag_Type := Head (Language, Max_Language_Length)
                      & Head (Script, Max_Script_Length)
                      & Head (Territory, Max_Territory_Length);

      Left      : Trait_Index_Type := Locale_Data'First;
      Right     : Trait_Index_Type := Locale_Data'Last + 1;
      Center    : Trait_Index_Type;
      Candidate : Tag_Type;

   begin
      ASCII_Uppercase (Key);
      Found := False;
      if Key < Locale_Data (Left).Tag then
         return;
      end if;
      loop
         Center := Left + (Right - Left) / 2;
         Candidate := Locale_Data (Center).Tag;
         if Key = Candidate then
            Index := Center;
            Found := True;
            return;
         end if;

         if Right - Left <= 1 then
            return;
         elsif Key < Candidate then
            Right := Center;
         else
            Left := Center;
         end if;
      end loop;
   end Lookup_Traits;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale (Locale_String  : Wide_String) return Locale_Type is

      Language  : Language_Type;
      Script    : Script_Type;
      Territory : Territory_Type;

   begin
      Decompose_Name (Locale_String, Language, Script, Territory);
      return Make_Locale (Language, Script, Territory);
   end Make_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale (Language  : Wide_String;
                         Territory : Wide_String) return Locale_Type is
   begin
      return Make_Locale (Language, "", Territory);
   end Make_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale (Language  : Wide_String;
                         Script    : Wide_String;
                         Territory : Wide_String) return Locale_Type is
   begin
      return Result : Locale_Type do
         Result.Language_Code := Head (Language, Max_Language_Length);
         Result.Script_Code := Head (Script, Max_Script_Length);
         Result.Territory_Code := Head (Territory, Max_Territory_Length);
         ASCII_Lowercase (Result.Language_Code);
         ASCII_Capitalize (Result.Script_Code);
         ASCII_Uppercase (Result.Territory_Code);
         Result.Traits_Index := Find_Traits (Result.Language_Code,
                                             Result.Script_Code,
                                             Result.Territory_Code);
      end return;
   end Make_Locale;

   -------------------------------
   -- Number_Of_Defined_Locales --
   -------------------------------

   function Number_Of_Defined_Locales return Positive is
   begin
      return Locale_Data'Length;
   end Number_Of_Defined_Locales;

   --------------------
   -- Numeric_Format --
   --------------------

   function Numeric_Format (Locale : Locale_Type;
                            Style  : Numeric_Style_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Numeric_Formats (Style));
   end Numeric_Format;

   ------------------
   -- Numeric_Item --
   ------------------

   function Numeric_Item (Locale : Locale_Type;
                          Item   : Numeric_Item_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Numeric_Items (Item));
   end Numeric_Item;

   ------------------
   -- Parent_Codes --
   ------------------

   procedure Parent_Codes (Language       : in out Language_Type;
                           Script         : in out Script_Type;
                           Territory      : in out Territory_Type;
                           Base_Territory : Territory_Type := Empty_Territory)
   is

      Language_P     : constant Boolean := Language (1) /= ' ';
      Script_P       : constant Boolean := Script (1) /= ' ';
      Territory_P    : constant Boolean := Territory (1) /= ' ';
      B_Territory_P  : constant Boolean := Base_Territory (1) /= ' ';

   begin
      if Language_P and Script_P and Territory_P then
         Territory := Empty_Territory;
         return;
      end if;

      if Language_P and Script_P and not Territory_P and B_Territory_P then
         Script := Empty_Script;
         Territory := Base_Territory;
         return;
      end if;

      if Language_P and Script_P and not Territory_P and not B_Territory_P then
         Script := Empty_Script;
         Territory := Empty_Territory;
         return;
      end if;

      if Language_P and not Script_P and Territory_P then
         Script := Empty_Script;
         Territory := Empty_Territory;
         return;
      end if;

      Language := Empty_Language;
      Script := Empty_Script;
      Territory := Empty_Territory;
   end Parent_Codes;

   ------------
   -- Script --
   ------------

   function Script (Locale : Locale_Type) return Wide_String is
   begin
      return Non_Blank_Prefix (Locale.Script_Code);
   end Script;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Locale : Locale_Type) is
   begin
      Current_Locale_Value := Locale;
   end Set_Locale;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Name : String) is
   begin
      Set_Locale (To_Wide_String (Name));
   end Set_Locale;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Wide_Name : Wide_String) is
   begin
      Set_Locale (Make_Locale (Wide_Name));
   end Set_Locale;

   ----------------
   -- Set_Traits --
   ----------------

   procedure Set_Traits (Locale : in out Locale_Type;
                         Name   : String) is
   begin
      Set_Traits (Locale, To_Wide_String (Name));
   end Set_Traits;

   ----------------
   -- Set_Traits --
   ----------------

   procedure Set_Traits (Locale    : in out Locale_Type;
                         Wide_Name : Wide_String) is
      Language  : Language_Type;
      Script    : Script_Type;
      Territory : Territory_Type;
   begin
      Decompose_Name (Wide_Name, Language, Script, Territory);
      Locale.Traits_Index := Find_Traits (Language, Script, Territory);
   end Set_Traits;

   --------------------
   -- Short_Day_Name --
   --------------------

   function Short_Day_Name (Locale : Locale_Type;
                            Day    : Day_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Short_Day_Names (Day));
   end Short_Day_Name;

   ----------------------
   -- Short_Month_Name --
   ----------------------

   function Short_Month_Name (Locale : Locale_Type;
                              Month  : Month_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Short_Month_Names (Month));
   end Short_Month_Name;

   ---------------
   -- Territory --
   ---------------

   function Territory (Locale : Locale_Type) return Wide_String is
   begin
      return Non_Blank_Prefix (Locale.Territory_Code);
   end Territory;

   -----------------
   -- Time_Format --
   -----------------

   function Time_Format (Locale : Locale_Type;
                         Style  : Date_Time_Style_Type) return Wide_String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Time_Formats (Style));
   end Time_Format;

   ---------------
   -- To_String --
   ---------------

   function To_String (Index : String_Index_Type) return Wide_String is
      Address : constant String_Address_Type := String_Addresses (Index);
   begin
      return Pool (Address.First .. Address.Last);
   end To_String;

   -----------------
   -- Traits_Name --
   -----------------

   function Traits_Name (Locale : Locale_Type) return Wide_String is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Name);
   end Traits_Name;

   ----------------
   -- Traits_Tag --
   ----------------

   function Traits_Tag (Locale : Locale_Type) return Wide_String is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Tag;
   end Traits_Tag;

begin
   Environment_Initialize;
end ZanyBlue.Text.Locales;
