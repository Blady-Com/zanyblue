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

with Ada.Strings.Wide_Maps;

package ZanyBlue.Text.Pseudo is

   use Ada.Strings.Wide_Maps;

   type Pseudo_Map_Type is tagged private;
   type Pseudo_Map_Access is access Pseudo_Map_Type;

   type Pseudo_Character_Map is
      record
         Source : Wide_Character;
         Target : Wide_Character;
      end record;

   type Pseudo_Map_Vector is array (Positive range <>) of Pseudo_Character_Map;

   procedure Add_Mapping (Pseudo_Map : in out Pseudo_Map_Type;
                          Mapping    : Pseudo_Map_Vector);
   --  Add vector of character mappings to the pseudo map.

   function Map (Pseudo_Map : Pseudo_Map_Type;
                 Ch         : Wide_Character) return Wide_Character;
   --  Return the mapping for a character wrt a pseudo map.

   Pseudo_Start              : constant Wide_Character;
   Pseudo_End                : constant Wide_Character;
   --  Characters used to mark the start and end of a formatted message.

   Format_Start              : constant Wide_Character;
   Format_End                : constant Wide_Character;
   --  Characters used to mark the start and end of a formatted argument.

   Null_Map                  : constant Pseudo_Map_Vector;
   Uppercase_Map             : constant Pseudo_Map_Vector;
   Lowercase_Map             : constant Pseudo_Map_Vector;
   Halfwidth_Forms_Map       : constant Pseudo_Map_Vector;
   Enclosed_Alphanumeric_Map : constant Pseudo_Map_Vector;
   --  Standard pseudo maps.

private

   type Pseudo_Map_Type is tagged
      record
         Mapping : Wide_Character_Mapping;
      end record;

   Full_Block                    : constant Wide_Character
                                          := Wide_Character'Val (16#2588#);
   Diamond_With_Left_Half_Black  : constant Wide_Character
                                          := Wide_Character'Val (16#2B16#);
   Diamond_With_Right_Half_Black : constant Wide_Character
                                          := Wide_Character'Val (16#2B17#);
   Pseudo_Start : constant Wide_Character := Diamond_With_Left_Half_Black;
   Pseudo_End   : constant Wide_Character := Diamond_With_Right_Half_Black;

   Format_Start          : constant Wide_Character := '«';
   Format_End            : constant Wide_Character := '»';

   Null_Map : constant Pseudo_Map_Vector := (('a', 'a'), ('b', 'b'));

   Uppercase_Map : constant Pseudo_Map_Vector := (
      ('a', 'A'),       ('b', 'B'),       ('c', 'C'),       ('d', 'D'),
      ('e', 'E'),       ('f', 'F'),       ('g', 'G'),       ('h', 'H'),
      ('i', 'I'),       ('j', 'J'),       ('k', 'K'),       ('l', 'L'),
      ('m', 'M'),       ('n', 'N'),       ('o', 'O'),       ('p', 'P'),
      ('q', 'Q'),       ('r', 'R'),       ('s', 'S'),       ('t', 'T'),
      ('u', 'U'),       ('v', 'V'),       ('w', 'W'),       ('x', 'X'),
      ('y', 'Y'),       ('z', 'Z'));

   Lowercase_Map : constant Pseudo_Map_Vector := (
      ('A', 'a'),       ('B', 'b'),       ('C', 'c'),       ('D', 'd'),
      ('E', 'e'),       ('F', 'f'),       ('G', 'g'),       ('H', 'h'),
      ('I', 'i'),       ('J', 'j'),       ('K', 'k'),       ('L', 'l'),
      ('M', 'm'),       ('N', 'n'),       ('O', 'o'),       ('P', 'p'),
      ('Q', 'q'),       ('R', 'r'),       ('S', 's'),       ('T', 't'),
      ('U', 'u'),       ('V', 'v'),       ('W', 'w'),       ('X', 'x'),
      ('Y', 'y'),       ('Z', 'z'));

   Halfwidth_Forms_Map : constant Pseudo_Map_Vector := (
      (' ', ' '),       --  U+2001
      ('!', '！'),      --  U+FF01
      ('"', '＂'),      --  U+FF02
      ('#', '＃'),      --  U+FF03
      ('$', '＄'),      --  U+FF04
      ('%', '％'),      --  U+FF05
      ('&', '＆'),      --  U+FF06
      (''', '＇'),      --  U+FF07
      ('(', '（'),      --  U+FF08
      (')', '）'),      --  U+FF09
      ('*', '＊'),      --  U+FF0A
      ('+', '＋'),      --  U+FF0B
      (',', '，'),      --  U+FF0C
      ('-', '－'),      --  U+FF0D
      ('.', '．'),      --  U+FF0E
      ('/', '／'),      --  U+FF0F
      ('0', '０'),      --  U+FF10
      ('1', '１'),      --  U+FF11
      ('2', '２'),      --  U+FF12
      ('3', '３'),      --  U+FF13
      ('4', '４'),      --  U+FF14
      ('5', '５'),      --  U+FF15
      ('6', '６'),      --  U+FF16
      ('7', '７'),      --  U+FF17
      ('8', '８'),      --  U+FF18
      ('9', '９'),      --  U+FF19
      (':', '：'),      --  U+FF1A
      (';', '；'),      --  U+FF1B
      ('<', '＜'),      --  U+FF1C
      ('=', '＝'),      --  U+FF1D
      ('>', '＞'),      --  U+FF1E
      ('?', '？'),      --  U+FF1F
      ('@', '＠'),      --  U+FF20
      ('A', 'Ａ'),      --  U+FF21
      ('B', 'Ｂ'),      --  U+FF22
      ('C', 'Ｃ'),      --  U+FF23
      ('D', 'Ｄ'),      --  U+FF24
      ('E', 'Ｅ'),      --  U+FF25
      ('F', 'Ｆ'),      --  U+FF26
      ('G', 'Ｇ'),      --  U+FF27
      ('H', 'Ｈ'),      --  U+FF28
      ('I', 'Ｉ'),      --  U+FF29
      ('J', 'Ｊ'),      --  U+FF2A
      ('K', 'Ｋ'),      --  U+FF2B
      ('L', 'Ｌ'),      --  U+FF2C
      ('M', 'Ｍ'),      --  U+FF2D
      ('N', 'Ｎ'),      --  U+FF2E
      ('O', 'Ｏ'),      --  U+FF2F
      ('P', 'Ｐ'),      --  U+FF30
      ('Q', 'Ｑ'),      --  U+FF31
      ('R', 'Ｒ'),      --  U+FF32
      ('S', 'Ｓ'),      --  U+FF33
      ('T', 'Ｔ'),      --  U+FF34
      ('U', 'Ｕ'),      --  U+FF35
      ('V', 'Ｖ'),      --  U+FF36
      ('W', 'Ｗ'),      --  U+FF37
      ('X', 'Ｘ'),      --  U+FF38
      ('Y', 'Ｙ'),      --  U+FF39
      ('Z', 'Ｚ'),      --  U+FF3A
      ('[', '［'),      --  U+FF3B
      ('\', '＼'),      --  U+FF3C
      (']', '］'),      --  U+FF3D
      ('^', '＾'),      --  U+FF3E
      ('_', '＿'),      --  U+FF3F
      ('`', '｀'),      --  U+FF40
      ('a', 'ａ'),      --  U+FF41
      ('b', 'ｂ'),      --  U+FF42
      ('c', 'ｃ'),      --  U+FF43
      ('d', 'ｄ'),      --  U+FF44
      ('e', 'ｅ'),      --  U+FF45
      ('f', 'ｆ'),      --  U+FF46
      ('g', 'ｇ'),      --  U+FF47
      ('h', 'ｈ'),      --  U+FF48
      ('i', 'ｉ'),      --  U+FF49
      ('j', 'ｊ'),      --  U+FF4A
      ('k', 'ｋ'),      --  U+FF4B
      ('l', 'ｌ'),      --  U+FF4C
      ('m', 'ｍ'),      --  U+FF4D
      ('n', 'ｎ'),      --  U+FF4E
      ('o', 'ｏ'),      --  U+FF4F
      ('p', 'ｐ'),      --  U+FF50
      ('q', 'ｑ'),      --  U+FF51
      ('r', 'ｒ'),      --  U+FF52
      ('s', 'ｓ'),      --  U+FF53
      ('t', 'ｔ'),      --  U+FF54
      ('u', 'ｕ'),      --  U+FF55
      ('v', 'ｖ'),      --  U+FF56
      ('w', 'ｗ'),      --  U+FF57
      ('x', 'ｘ'),      --  U+FF58
      ('y', 'ｙ'),      --  U+FF59
      ('z', 'ｚ'),      --  U+FF5A
      ('{', '｛'),      --  U+FF5B
      ('|', '｜'),      --  U+FF5C
      ('}', '｝'),      --  U+FF5D
      ('~', '～'));     --  U+FF5E

   Enclosed_Alphanumeric_Map : constant Pseudo_Map_Vector := (
      ('1', '①'),      --  U+2460
      ('2', '②'),      --  U+2461
      ('3', '③'),      --  U+2462
      ('4', '④'),      --  U+2463
      ('5', '⑤'),      --  U+2464
      ('6', '⑥'),      --  U+2465
      ('7', '⑦'),      --  U+2466
      ('8', '⑧'),      --  U+2467
      ('9', '⑨'),      --  U+2468
      ('A', 'Ⓐ'),      --  U+24B6
      ('B', 'Ⓑ'),      --  U+24B7
      ('C', 'Ⓒ'),      --  U+24B8
      ('D', 'Ⓓ'),      --  U+24B9
      ('E', 'Ⓔ'),      --  U+24BA
      ('F', 'Ⓕ'),      --  U+24BB
      ('G', 'Ⓖ'),      --  U+24BC
      ('H', 'Ⓗ'),      --  U+24BD
      ('I', 'Ⓘ'),      --  U+24BE
      ('J', 'Ⓙ'),      --  U+24BF
      ('K', 'Ⓚ'),      --  U+24C0
      ('L', 'Ⓛ'),      --  U+24C1
      ('M', 'Ⓜ'),      --  U+24C2
      ('N', 'Ⓝ'),      --  U+24C3
      ('O', 'Ⓞ'),      --  U+24C4
      ('P', 'Ⓟ'),      --  U+24C5
      ('Q', 'Ⓠ'),      --  U+24C6
      ('R', 'Ⓡ'),      --  U+24C7
      ('S', 'Ⓢ'),      --  U+24C8
      ('T', 'Ⓣ'),      --  U+24C9
      ('U', 'Ⓤ'),      --  U+24CA
      ('V', 'Ⓥ'),      --  U+24CB
      ('W', 'Ⓦ'),      --  U+24CC
      ('X', 'Ⓧ'),      --  U+24CD
      ('Y', 'Ⓨ'),      --  U+24CE
      ('Z', 'Ⓩ'),      --  U+24CF
      ('a', 'ⓐ'),      --  U+24D0
      ('b', 'ⓑ'),      --  U+24D1
      ('c', 'ⓒ'),      --  U+24D2
      ('d', 'ⓓ'),      --  U+24D3
      ('e', 'ⓔ'),      --  U+24D4
      ('f', 'ⓕ'),      --  U+24D5
      ('g', 'ⓖ'),      --  U+24D6
      ('h', 'ⓗ'),      --  U+24D7
      ('i', 'ⓘ'),      --  U+24D8
      ('j', 'ⓙ'),      --  U+24D9
      ('k', 'ⓚ'),      --  U+24DA
      ('l', 'ⓛ'),      --  U+24DB
      ('m', 'ⓜ'),      --  U+24DC
      ('n', 'ⓝ'),      --  U+24DD
      ('o', 'ⓞ'),      --  U+24DE
      ('p', 'ⓟ'),      --  U+24DF
      ('q', 'ⓠ'),      --  U+24E0
      ('r', 'ⓡ'),      --  U+24E1
      ('s', 'ⓢ'),      --  U+24E2
      ('t', 'ⓣ'),      --  U+24E3
      ('u', 'ⓤ'),      --  U+24E4
      ('v', 'ⓥ'),      --  U+24E5
      ('w', 'ⓦ'),      --  U+24E6
      ('x', 'ⓧ'),      --  U+24E7
      ('y', 'ⓨ'),      --  U+24E8
      ('z', 'ⓩ'));     --  U+24E9

end ZanyBlue.Text.Pseudo;
