{
HtmlViewer Version 11.4
Copyright (c) 2010-2012 by Bernd Gabriel

This source module is based on code of CodeChangerDecode.pas written by SchwarzKopf-M (SchwarzKopf-M@yandex.ru)
and the source code library libiconv, which is published under the GNU Library General Public License.

Thanks to SchwarzKopf-M for his extensive work.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

//This is the original copyright notice in CodeChangerDecode.pas

//This is charset Windows-1251:
//ћодуль перекодировки текста €вл€етс€ свободно распростран€емым и распростран€етс€ " ак есть".
//ѕри модификации или частичном использовании исходного кода ссылки
//  на оригинальный модуль об€зательны.
//јвторское право оставл€ю за собой - SchwarzKopf-M (SchwarzKopf-M@yandex.ru).
//¬ данном модуле используетс€ информаци€ найденна€ в »нтернете
//  без нарушени€ исходных авторских прав.
//ƒл€ перекодировки некоторых азиатских €зыков использовалась вольна€
//  интерпретаци€ исходных кодов библиотеки libiconv.
//
//—пециально дл€ HTMLViewer (http://code.google.com/p/thtmlviewer)

// Translated by "http://translate.google.de/#ru|en|" it is:
// Text conversion module is freeware and is distributed "as is".
// If you modify or partial use of source code links
// To the original module is required.
// Copyright reserve the - SchwarzKopf-M (SchwarzKopf-M@yandex.ru).
// This module uses the information found on the Internet
// Without affecting the original copyright holder.
// For the conversion of some Asian languages ??used in freestyle
// Interpretation of the source code library libiconv.
//
// Especially for HTMLViewer (http://code.google.com/p/thtmlviewer)

{$I htmlcons.inc}

unit BuffConv;

interface

uses
  Windows,
  {$ifdef LCL}
    LclIntf, LclType,
  {$endif}
  HtmlGlobals,
  HtmlBuffer;

type
  TBuffConverterClass =  class of TBuffConverter;

  TBuffConvInfo = record
    CodePage:  TBuffCodePage;
    CharSet:   TBuffCharSet;
    Converter: TBuffConverterClass;
    Name: String;
  end;

function FindCodePageInfo(CodePage: TBuffCodePage): Integer;
function GetCodePageInfo(Index: Integer): TBuffConvInfo;

implementation

uses
  BuffConvArrays;

type

  // BG, 06.10.2012: converts UTF7 to unicode.
  TBuffConvUTF7 = class(TBuffConverter)
  private
    State: Byte;
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 26.09.2012: converts UTF8 to unicode.
  TBuffConvUTF8 = class(TBuffConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 02.10.2012: converts several single byte code pages using an AnsiCharHighMap to unicode.
  TBuffConvHighMap = class(TBuffConverter)
  private
    FMap: AnsiCharHighMap;
    procedure UpdateMap;
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    procedure AfterConstruction; override;
    function NextChar: TBuffChar; override;
  end;

  // BG, 02.10.2012: converts the single byte code of ISO 8859 using an AnsiCharIsoMap to unicode.
  TBuffConvIsoMap = class(TBuffConverter)
  private
    FMap: AnsiCharISOMap;
    procedure UpdateMap;
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    procedure AfterConstruction; override;
    function NextChar: TBuffChar; override;
  end;

  TBuffBaseConverter = class(TBuffConverter)
  protected
    function ASCIIDecodeChar(c1: Byte): TBuffChar;
    function CP922EstonianDecodeChar(c1: Byte): TBuffChar;
    function GB2312DecodeChar(c1, c2: Byte): TBuffChar;
    function GBKDecodeChar(c1, c2: Byte): TBuffChar;
    function ISO8859_11DecodeChar(c1: Byte): TBuffChar;
    function ISO8859_15DecodeChar(c1: Byte): TBuffChar;
    function ISO8859_5DecodeChar(c1: Byte): TBuffChar;
    function ISO8859_6DecodeChar(c1: Byte): TBuffChar;
    function ISO8859_7DecodeChar(c1: Byte): TBuffChar;
    function ISO8859_8DecodeChar(c1: Byte): TBuffChar;
    function ISO8859_9DecodeChar(c1: Byte): TBuffChar;
    function JIS_X0201DecodeChar(c1: Byte): TBuffChar;
    function JIS_X0208DecodeChar(c1, c2: Byte): TBuffChar;
    function JIS_X0212DecodeChar(c1, c2: Byte): TBuffChar;
    function KSC5601DecodeChar(c1, c2: Byte): TBuffChar;
    function Latin1EuroDecodeChar(c1: Byte): TBuffChar;
    function MultiByteToWideCharDecodeChar(c1: Byte): TBuffChar;
  end;

  // BG, 02.10.2012: converts several single byte code pages using one of the *DecoderChar functions to unicode.
  TBuffSingleByteDecoder = function(c1: Byte): TBuffChar of object;
  TBuffConvSingleByte = class(TBuffBaseConverter)
  private
    FDecoder: TBuffSingleByteDecoder;
    procedure UpdateDecoder;
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    procedure AfterConstruction; override;
    function NextChar: TBuffChar; override;
  end;

  // BG, 02.10.2012: converts several single byte code pages using one of the *DecoderChar functions to unicode.
  TBuffDoubleByteDecoder = function(c1, c2: Byte): TBuffChar of object;
  TBuffConvDoubleByte = class(TBuffBaseConverter)
  private
    FDecoder: TBuffDoubleByteDecoder;
    procedure UpdateDecoder;
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    procedure AfterConstruction; override;
    function NextChar: TBuffChar; override;
  end;

  // BG, 26.09.2012: converts shift-jis (codepages 932, 943) to unicode.
  TBuffConvShiftJis932 = class(TBuffConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 03.10.2012: converts simplified Chinese (PRC, Singapore) / Chinese simplified (GB2312) to unicode.
  TBuffConvIBM936 = class(TBuffBaseConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 04.10.2012: converts hangeul (codepage 949 to unicode.
  TBuffConvHangeul949 = class(TBuffBaseConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 04.10.2012: converts Traditional Chinese (code page 950) to unicode.
  TBuffConvBig5CP950 = class(TBuffConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  TBuffConvGB18030 = class(TBuffBaseConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  TBuffConvEUC_JP = class(TBuffBaseConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  TBuffConvISO2022CN = class(TBuffBaseConverter)
  private
    CNState1: (CNSTATE_ASCII, CNSTATE_TWOBYTE);
    CNState2: (CNSTATE2_NONE, STATE2_DESIGNATED_GB2312, STATE2_DESIGNATED_CNS11643_1);
    CNState3: (CNSTATE3_NONE, STATE3_DESIGNATED_CNS11643_2);
    function CNS11643_1DecodeChar(c1, c2: Word): TBuffChar;
    function CNS11643_2DecodeChar(c1, c2: Word): TBuffChar;
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 27.09.2012: converts stateful ISO-2022-JP, -1, -2 (codepage 50220) to unicode.
  TBuffConvISO2022JP = class(TBuffBaseConverter)
  private
    JPState: (
      JPS_ASCII,
      JPS_JISX0201ROMAN,
      JPS_JISX0208,
      JPS_JISX0212,
      JPS_GB2312,
      JPS_KSC5601,
      JPS_8859_1,
      JPS_8859_7);
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    function NextChar: TBuffChar; override;
  end;

  TBuffConvISO2022KR = class(TBuffBaseConverter)
  private
    KRState1: (KRSTATE_ASCII, KRSTATE_TWOBYTE);
    KRState2: (KRSTATE2_NONE, STATE2_DESIGNATED_KSC5601);
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    function NextChar: TBuffChar; override;
  end;

// CodePages (most names taken from http://msdn.microsoft.com/en-us/library/windows/desktop/dd317756%28v=vs.85%29.aspx):
const CodePageInfos: array [0..161] of TBuffConvInfo = (
  // first entry must be code page CP_UNKNOWN!!!
  ( CodePage: CP_UNKNOWN; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Unknown'),
  //
  ( CodePage:     CP_ACP; CharSet: DEFAULT_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Default ANSI CP'),
  ( CodePage:   CP_OEMCP; CharSet: OEM_CHARSET;         Converter: TBuffConvSingleByte;  Name: 'Default OEM CP'),
  ( CodePage:   CP_MACCP; CharSet: MAC_CHARSET;         Converter: TBuffConvSingleByte;  Name: 'Default MAC CP'),
  ( CodePage:         37; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC US-Canada'),
  ( CodePage:        437; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM United States'),
  ( CodePage:        500; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC International'),
  ( CodePage:        708; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Arabic (ASMO 708)'),
  ( CodePage:        709; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Arabic (ASMO-449+, BCON V4)'),
  ( CodePage:        710; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Arabic - Transparent Arabic'),
  ( CodePage:        720; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Arabic (Transparent ASMO); Arabic (DOS)'),
  ( CodePage:        737; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Greek (formerly 437G); Greek (DOS)'),
  ( CodePage:        775; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Baltic; Baltic (DOS)'),
  ( CodePage:        850; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Multilingual Latin 1; Western European (DOS)'),
  ( CodePage:        852; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Latin 2; Central European (DOS)'),
  ( CodePage:        855; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Cyrillic (primarily Russian)'),
  ( CodePage:        857; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Turkish; Turkish (DOS)'),
  ( CodePage:        858; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'OEM Multilingual Latin 1 + Euro symbol'),
  ( CodePage:        860; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Portuguese; Portuguese (DOS)'),
  ( CodePage:        861; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Icelandic; Icelandic (DOS)'),
  ( CodePage:        862; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Hebrew; Hebrew (DOS)'),
  ( CodePage:        863; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM French Canadian; French Canadian (DOS)'),
  ( CodePage:        864; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Arabic; Arabic (864)'),
  ( CodePage:        865; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Nordic; Nordic (DOS)'),
  ( CodePage:        866; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Russian; Cyrillic (DOS)'),
  ( CodePage:        869; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'OEM Modern Greek; Greek, Modern (DOS)'),
  ( CodePage:        870; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2'),
  ( CodePage:        874; CharSet: THAI_CHARSET;        Converter: TBuffConvHighMap;     Name: 'ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows)'),
  ( CodePage:        875; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Greek Modern'),
  ( CodePage:        922; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ANSI/OEM Estonian (DOS)'),
  ( CodePage:        932; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvShiftJis932; Name: 'ANSI/OEM Japanese; Japanese (Shift-JIS)'),
  ( CodePage:        936; CharSet: GB2312_CHARSET;      Converter: TBuffConvIBM936;      Name: 'ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)'),
  ( CodePage:        943; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvShiftJis932; Name: 'ANSI/OEM Japanese (Shift-JIS)'),
  ( CodePage:        949; CharSet: HANGEUL_CHARSET;     Converter: TBuffConvHangeul949;  Name: 'ANSI/OEM Korean (Unified Hangul Code)'),
  ( CodePage:        950; CharSet: CHINESEBIG5_CHARSET; Converter: TBuffConvBig5CP950;   Name: 'ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)'),
  ( CodePage:       1026; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Turkish (Latin 5)'),
  ( CodePage:       1047; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Latin 1/Open System'),
  ( CodePage:       1140; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro)'),
  ( CodePage:       1141; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro)'),
  ( CodePage:       1142; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro)'),
  ( CodePage:       1143; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro)'),
  ( CodePage:       1144; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro)'),
  ( CodePage:       1145; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro)'),
  ( CodePage:       1146; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro)'),
  ( CodePage:       1147; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro)'),
  ( CodePage:       1148; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro)'),
  ( CodePage:       1149; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro)'),
  ( CodePage:       1200; CharSet: DEFAULT_CHARSET;     Converter: TBuffConverter;       Name: 'Unicode UTF-16, little endian byte order (BMP of ISO 10646); available only to managed applications'),
  ( CodePage:       1201; CharSet: DEFAULT_CHARSET;     Converter: TBuffConverter;       Name: 'Unicode UTF-16, big endian byte order; available only to managed applications'),
  ( CodePage:       1250; CharSet: EASTEUROPE_CHARSET;  Converter: TBuffConvSingleByte;  Name: 'ANSI Central European; Central European (Windows)'),
  ( CodePage:       1251; CharSet: RUSSIAN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ANSI Cyrillic; Cyrillic (Windows)'),
  ( CodePage:       1252; CharSet: ANSI_CHARSET;        Converter: TBuffConvSingleByte;  Name: 'ANSI Latin 1; Western European (Windows)'),
  ( CodePage:       1253; CharSet: GREEK_CHARSET;       Converter: TBuffConvSingleByte;  Name: 'ANSI Greek; Greek (Windows)'),
  ( CodePage:       1254; CharSet: TURKISH_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ANSI Turkish; Turkish (Windows)'),
  ( CodePage:       1255; CharSet: HEBREW_CHARSET;      Converter: TBuffConvSingleByte;  Name: 'ANSI Hebrew; Hebrew (Windows)'),
  ( CodePage:       1256; CharSet: ARABIC_CHARSET;      Converter: TBuffConvSingleByte;  Name: 'ANSI Arabic; Arabic (Windows)'),
  ( CodePage:       1257; CharSet: BALTIC_CHARSET;      Converter: TBuffConvSingleByte;  Name: 'ANSI Baltic; Baltic (Windows)'),
  ( CodePage:       1258; CharSet: VIETNAMESE_CHARSET;  Converter: TBuffConvSingleByte;  Name: 'ANSI/OEM Vietnamese; Vietnamese (Windows)'),
  ( CodePage:       1361; CharSet: JOHAB_CHARSET;       Converter: TBuffConvSingleByte;  Name: 'Korean (Johab)'),
  ( CodePage:      10000; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'MAC Roman; Western European (Mac)'),
  ( CodePage:      10001; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Japanese (Mac)'),
  ( CodePage:      10002; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'MAC Traditional Chinese (Big5); Chinese Traditional (Mac)'),
  ( CodePage:      10003; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Korean (Mac)'),
  ( CodePage:      10004; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Arabic (Mac)'),
  ( CodePage:      10005; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Hebrew (Mac)'),
  ( CodePage:      10006; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Greek (Mac)'),
  ( CodePage:      10007; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Cyrillic (Mac)'),
  ( CodePage:      10008; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac)'),
  ( CodePage:      10010; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Romanian (Mac)'),
  ( CodePage:      10017; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Ukrainian (Mac)'),
  ( CodePage:      10021; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Thai (Mac)'),
  ( CodePage:      10029; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'MAC Latin 2; Central European (Mac)'),
  ( CodePage:      10079; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Icelandic (Mac)'),
  ( CodePage:      10081; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Turkish (Mac)'),
  ( CodePage:      10082; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvHighMap;     Name: 'Croatian (Mac)'),
  ( CodePage:      12000; CharSet: DEFAULT_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Unicode UTF-32, little endian byte order; available only to managed applications'),
  ( CodePage:      12001; CharSet: DEFAULT_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Unicode UTF-32, big endian byte order; available only to managed applications'),
  ( CodePage:      20000; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'CNS Taiwan; Chinese Traditional (CNS)'),
  ( CodePage:      20001; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'TCA Taiwan'),
  ( CodePage:      20002; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Eten Taiwan; Chinese Traditional (Eten)'),
  ( CodePage:      20003; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM5550 Taiwan'),
  ( CodePage:      20004; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'TeleText Taiwan'),
  ( CodePage:      20005; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Wang Taiwan'),
  ( CodePage:      20105; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5)'),
  ( CodePage:      20106; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IA5 German (7-bit)'),
  ( CodePage:      20107; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IA5 Swedish (7-bit)'),
  ( CodePage:      20108; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IA5 Norwegian (7-bit)'),
  ( CodePage:      20127; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'US-ASCII (7-bit)'),
  ( CodePage:      20261; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'T.61'),
  ( CodePage:      20269; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 6937 Non-Spacing Accent'),
  ( CodePage:      20273; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Germany'),
  ( CodePage:      20277; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Denmark-Norway'),
  ( CodePage:      20278; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Finland-Sweden'),
  ( CodePage:      20280; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Italy'),
  ( CodePage:      20284; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Latin America-Spain'),
  ( CodePage:      20285; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC United Kingdom'),
  ( CodePage:      20290; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Japanese Katakana Extended'),
  ( CodePage:      20297; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC France'),
  ( CodePage:      20420; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Arabic'),
  ( CodePage:      20423; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Greek'),
  ( CodePage:      20424; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Hebrew'),
  ( CodePage:      20833; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Korean Extended'),
  ( CodePage:      20838; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Thai'),
  ( CodePage:      20866; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Russian (KOI8-R); Cyrillic (KOI8-R)'),
  ( CodePage:      20871; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Icelandic'),
  ( CodePage:      20880; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Cyrillic Russian'),
  ( CodePage:      20905; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Turkish'),
  ( CodePage:      20924; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Latin 1/Open System (1047 + Euro symbol)'),
  ( CodePage:      20932; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Japanese (JIS 0208-1990 and 0121-1990)'),
  ( CodePage:      20936; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Simplified Chinese (GB2312); Chinese Simplified (GB2312-80)'),
  ( CodePage:      20949; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Korean Wansung'),
  ( CodePage:      21025; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'IBM EBCDIC Cyrillic Serbian-Bulgarian'),
  ( CodePage:      21027; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: '(deprecated)'),
  ( CodePage:      21866; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Ukrainian (KOI8-U); Cyrillic (KOI8-U)'),
  ( CodePage:      28591; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConverter;       Name: 'ISO 8859-1 Latin 1; Western European (ISO)'),
  ( CodePage:      28592; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvIsoMap;      Name: 'ISO 8859-2 Latin 2; Central European; Central European (ISO)'),
  ( CodePage:      28593; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvIsoMap;      Name: 'ISO 8859-3 Latin 3; Southern European'),
  ( CodePage:      28594; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvIsoMap;      Name: 'ISO 8859-4 Latin 4; Northern European'),
  ( CodePage:      28595; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-5 Cyrillic'),
  ( CodePage:      28596; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-6 Arabic'),
  ( CodePage:      28597; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-7 Greek'),
  ( CodePage:      28598; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-8 Hebrew; Hebrew (ISO-Visual)'),
  ( CodePage:      28599; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-9 Turkish'),
  ( CodePage:      28600; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvIsoMap;      Name: 'ISO 8859-10 Latin 6; Nordic'),
  ( CodePage:      28601; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-11 Thai'),
  ( CodePage:      28603; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvIsoMap;      Name: 'ISO 8859-13 Latin 7; Baltic'),
  ( CodePage:      28604; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvIsoMap;      Name: 'ISO 8859-14 Latin 8; Celtic'),
  ( CodePage:      28605; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-15 Latin 9; Western European'),
  ( CodePage:      28606; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvIsoMap;      Name: 'ISO 8859-16 Latin 10; South Eastern European'),
  ( CodePage:      29001; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Europa 3'),
  ( CodePage:      38598; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-8 Hebrew; Hebrew (ISO-Logical)'),
  ( CodePage:      50220; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvISO2022JP;   Name: 'ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)'),
  ( CodePage:      50221; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvISO2022JP;   Name: 'ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)'),
  ( CodePage:      50222; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvISO2022JP;   Name: 'ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI)'),
  ( CodePage:      50225; CharSet: HANGEUL_CHARSET;     Converter: TBuffConvISO2022KR;   Name: 'ISO 2022 Korean'),
  ( CodePage:      50227; CharSet: GB2312_CHARSET;      Converter: TBuffConvISO2022CN;   Name: 'ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022)'),
  ( CodePage:      50229; CharSet: CHINESEBIG5_CHARSET; Converter: TBuffConvSingleByte;  Name: 'ISO 2022 Traditional Chinese'),
  ( CodePage:      50930; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Japanese (Katakana) Extended'),
  ( CodePage:      50931; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC US-Canada and Japanese'),
  ( CodePage:      50933; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Korean Extended and Korean'),
  ( CodePage:      50935; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Simplified Chinese Extended and Simplified Chinese'),
  ( CodePage:      50936; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Simplified Chinese'),
  ( CodePage:      50937; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC US-Canada and Traditional Chinese'),
  ( CodePage:      50939; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Japanese (Latin) Extended and Japanese'),
  ( CodePage:      51932; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvEUC_JP;      Name: 'EUC Japanese'),
  ( CodePage:      51936; CharSet: GB2312_CHARSET;      Converter: TBuffConvDoubleByte;  Name: 'EUC Simplified Chinese; Chinese Simplified (EUC)'),
  ( CodePage:      51949; CharSet: HANGEUL_CHARSET;     Converter: TBuffConvDoubleByte;  Name: 'EUC Korean'),
  ( CodePage:      51950; CharSet: CHINESEBIG5_CHARSET; Converter: TBuffConvBig5CP950;   Name: 'EUC Traditional Chinese'),
  ( CodePage:      52936; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ)'),
  ( CodePage:      54936; CharSet: GB2312_CHARSET;      Converter: TBuffConvGB18030;     Name: 'Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)'),
  ( CodePage:      57002; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Devanagari'),
  ( CodePage:      57003; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Bengali'),
  ( CodePage:      57004; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Tamil'),
  ( CodePage:      57005; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Telugu'),
  ( CodePage:      57006; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Assamese'),
  ( CodePage:      57007; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Oriya'),
  ( CodePage:      57008; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Kannada'),
  ( CodePage:      57009; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Malayalam'),
  ( CodePage:      57010; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Gujarati'),
  ( CodePage:      57011; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISCII Punjabi'),
  ( CodePage:      65000; CharSet: DEFAULT_CHARSET;     Converter: TBuffConvUTF7;        Name: 'Unicode (UTF-7)'),
  ( CodePage:      65001; CharSet: DEFAULT_CHARSET;     Converter: TBuffConvUTF8;        Name: 'Unicode (UTF-8)')
);

//-- BG ---------------------------------------------------------- 26.09.2012 --
function FindCodePageInfo(CodePage: TBuffCodePage): Integer;
var
  Low, Index: Integer;
begin
  Result := Length(CodePageInfos);
  Low := 0;
  while Low < Result do
  begin
    Index := (Low + Result) shr 1;
    if CodePageInfos[Index].CodePage < CodePage then
      Low := Index + 1
    else
      Result := Index;
  end;
  if CodePageInfos[Result].CodePage <> CodePage then
    Result := -1;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function GetCodePageInfo(Index: Integer): TBuffConvInfo;
begin
  Result := CodePageInfos[Index];
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.MultiByteToWideCharDecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00: Result := TBuffChar(c1);
  else
    MultiByteToWideChar(FCodePage, 0, PAnsiChar(@c1), 1, @Result, 1)
  end;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffBaseConverter.ASCIIDecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00..$7F: Result := TBuffChar(c1);
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffBaseConverter.GB2312DecodeChar(c1, c2: Byte): TBuffChar;
var
  i: Integer;
begin
  case c1 of
    $21..$29, $30..$77:
      case c2 of
        $21..$7E:
        begin
          i := 94 * (c1 - $21) + (c2 - $21);
          case i of
               0.. 830: Result := GB2312_1Map[i];
            1410..8177: Result := GB2312_2Map[i - 1410];
          else
            Result := TBuffChar($FFFD);
          end;
        end;
      else
        Result := TBuffChar($FFFD);
      end;
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 06.10.2012 --
function TBuffBaseConverter.GBKDecodeChar(c1, c2: Byte): TBuffChar;

  function Offset(cmul, c1, c2: Word): Integer;
  begin
    case c2 of
      $40..$7E: Result := cmul * (c1 - $81) + (c2 - $40);
      $80..$FE: Result := cmul * (c1 - $81) + (c2 - $41);
    else
      Result := MaxInt;
    end;
  end;

var
  i: Integer;
begin
  case c1 of
    $81..$A0:
    begin
      Result := GBKE_1Map[Offset(190, c1, c2)];
      Exit;
    end;

    $A1:
      case c2 of
        $A4: Result := TBuffChar($00B7);
        $AA: Result := TBuffChar($2014);
      else
        Result := GB2312DecodeChar(c1 - $80, c2 - $80);
      end;

    $A2:
      case c2 of
        $A1..$AA: Result := TBuffChar($2170 + (c2 - $A1));
      else
        Result := GB2312DecodeChar(c1 - $80, c2 - $80);
      end;
  else
    Result := GB2312DecodeChar(c1 - $80, c2 - $80);
  end;
  if Result <> TBuffChar(#$FFFD) then
    Exit;

  case c1 of
    $A6, $A8:
    begin
      i := Offset(190, c1, c2);
      case i of
        7189..7210: Result := CP936E_1Map[i - 7189];
        7532..7537: Result := CP936E_2Map[i - 7532];
      end;
      if Result <> TBuffChar(#$FFFD) then
        exit;
    end;
  end;

  case c1 of
    $A8..$FE:
    begin
      i := Offset(96, c1, c2);
      case i of
        3744..12015: Result := GBKE_2Map[i - 3744];
      end;
      if Result <> TBuffChar(#$FFFD) then
        exit;
    end;
  end;

  case c1 of
    $A1..$A2:
      case c2 of
        $40..$7E: Result := TBuffChar($E4C6 + 96 * (c1 - $81) + (c2 - $40));
        $80..$A0: Result := TBuffChar($E4C6 + 96 * (c1 - $A1) + (c2 - $41));
      end;

    $AA..$AF, $F8..$FE:
      case c2 of
        $A1..$F7: Result := TBuffChar($E000 + 94 * (c1 - $AA) + (c2 - $A1));
        $F8..$FE: Result := TBuffChar($E000 + 94 * (c1 - $F2) + (c2 - $A1));
      end;
  end;
end;

//-- BG ---------------------------------------------------------- 04.10.2012 --
function TBuffBaseConverter.JIS_X0201DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $5C: Result := #$00A5;
    $7E: Result := #$203E;
    $A1..$DF: Result := TBuffChar(c1 + $FEC0);
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 04.10.2012 --
function TBuffBaseConverter.JIS_X0208DecodeChar(c1, c2: Byte): TBuffChar;
var
  i: Integer;
begin
  case c1 of
    $21..$27, $30..$73:
      case c2 of
        $21..$7E:
        begin
          i := 94 * (c1 - $21) + (c2 - $21);
          case i of
               0.. 689: Result := JIS_X0208_1Map[i];
            1410..7807: Result := JIS_X0208_2Map[i - 1410];
          else
            Result := TBuffChar($FFFD);
          end;
        end;
      else
        Result := TBuffChar($FFFD);
      end;
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 04.10.2012 --
function TBuffBaseConverter.JIS_X0212DecodeChar(c1, c2: Byte): TBuffChar;
var
  i: Integer;
begin
  case c1 of
    $22, $26..$27, $29..$2B, $30..$6D:
      case c2 of
        $21..$7E:
        begin
          i := 94 * (c1 - $21) + (c2 - $21);
          case i of
               0.. 174: Result := JIS_X0212_1Map[i - 94];
             470.. 657: Result := JIS_X0212_2Map[i - 470];
             752..1026: Result := JIS_X0212_3Map[i - 752];
            1410..7210: Result := JIS_X0212_4Map[i - 1410];
          else
            Result := TBuffChar($FFFD);
          end;
        end;
      else
        Result := TBuffChar($FFFD);
      end;
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffBaseConverter.KSC5601DecodeChar(c1, c2: Byte): TBuffChar;
var
  i: Integer;
begin
  case c1 of
    $21..$2C, $30..$48, $4A..$7D:
      case c2 of
        $21..$7E:
        begin
          i := 94 * (c1 - $21) + (c2 - $21);
          case i of
               0..1114: Result := KSC5601_1Map[i];
            1410..3759: Result := KSC5601_2Map[i - 1410];
            3854..8741: Result := KSC5601_3Map[i - 3854];
          else
            Result := TBuffChar($FFFD);
          end;
        end;
      else
        Result := TBuffChar($FFFD);
      end;
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.ISO8859_5DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00..$A0, $AD: Result := TBuffChar(c1);
    $F0          : Result := #$2116;
    $FD          : Result := #$00A7;
  else
    Result := TBuffChar(c1 + $0360);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.ISO8859_6DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00..$A0, $A4, $AD               : Result := TBuffChar(c1);
    $AC, $BB, $BF, $C1..$DA, $E0..$F2: Result := TBuffChar(c1 + $0580);
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.ISO8859_7DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00..$A0, $A6..$A9, $AB..$AD,
    $B0..$B3, $B7, $BB, $BD: Result := TBuffChar(c1);
    $A1     : Result := #$2018;
    $A2     : Result := #$2019;
    $AF     : Result := #$2015;
    $D2, $FF: Result := #$FFFF;
  else
    Result := TBuffChar(c1 + $02D0);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.ISO8859_8DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00..$A0, $A2..$A9, $AB..$AE,
    $B0..$B9, $BB..$BE: Result := TBuffChar(c1);
    $AA     : Result := #$00D7;
    $AF     : Result := #$203E;
    $BA     : Result := #$00F7;
    $DF     : Result := #$2017;
    $E0..$FA: Result := TBuffChar(c1 + $04E0);
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.ISO8859_9DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $D0: Result := #$011E;
    $DD: Result := #$0130;
    $DE: Result := #$015E;
    $F0: Result := #$011F;
    $FD: Result := #$0131;
    $FE: Result := #$015F;
  else
    Result := TBuffChar(c1);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.ISO8859_11DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00..$A0: Result := TBuffChar(c1);
    $A1..$DA,
    $DF..$FB: Result := TBuffChar(c1 + $0D60);
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffBaseConverter.ISO8859_15DecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $A4: Result := #$20AC;
    $A6: Result := #$00A6;
    $A8: Result := #$0161;
    $B4: Result := #$017D;
    $B8: Result := #$017E;
    $BC: Result := #$0152;
    $BD: Result := #$0153;
    $BE: Result := #$0178;
  else
    Result := TBuffChar(c1);
  end;
end;

//-- BG ---------------------------------------------------------- 03.10.2012 --
function TBuffBaseConverter.Latin1EuroDecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $00..$7F: Result := TBuffChar(c1);
    $D5: Result := #$20AC;
  else
    Result := CP850Map[c1];
  end;
end;

//-- BG ---------------------------------------------------------- 03.10.2012 --
function TBuffBaseConverter.CP922EstonianDecodeChar(c1: Byte): TBuffChar;
begin
  case c1 of
    $A0..$AF: Result := CP922_1Map[c1 - $A0];
    $D0..$DF: Result := CP922_2Map[c1 - $D0];
    $F0..$FF: Result := CP922_3Map[c1 - $F0];
  else
    Result := TBuffChar(c1);
  end;
end;

{ TBuffConvSingleByte }

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvSingleByte.AfterConstruction;
begin
  inherited;
  UpdateDecoder;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvSingleByte.Assign(Source: TBuffConverter);
begin
  inherited;
  UpdateDecoder;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffConvSingleByte.NextChar: TBuffChar;
begin
  Result := FDecoder(GetNext);
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvSingleByte.UpdateDecoder;
begin
  case FCodePage of
      858: FDecoder := Latin1EuroDecodeChar;
      922: FDecoder := CP922EstonianDecodeChar;
    28595: FDecoder := ISO8859_5DecodeChar;
    28596: FDecoder := ISO8859_6DecodeChar;
    28597: FDecoder := ISO8859_7DecodeChar;
    28598: FDecoder := ISO8859_8DecodeChar;
    28599: FDecoder := ISO8859_9DecodeChar;
    28601: FDecoder := ISO8859_11DecodeChar;
    28605: FDecoder := ISO8859_15DecodeChar;
  else
    FDecoder := MultiByteToWideCharDecodeChar;
  end;
end;

{ TBuffConvDoubleByte }

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvDoubleByte.AfterConstruction;
begin
  inherited;
  UpdateDecoder;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvDoubleByte.Assign(Source: TBuffConverter);
begin
  inherited;
  UpdateDecoder;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffConvDoubleByte.NextChar: TBuffChar;
var
  c1: Word;
begin
  c1 := GetNext;
  case c1 of
    $00..$80: Result := TBuffChar(c1);
    $A1..$FE: Result := FDecoder(c1 - $80, GetNext - $80);
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvDoubleByte.UpdateDecoder;
begin
  case FCodePage of
    51936: FDecoder := GB2312DecodeChar;
    51949: FDecoder := KSC5601DecodeChar;
  end;
end;

{ TBuffConvUTF8 }

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffConvUTF8.NextChar: TBuffChar;
var
  Buffer: Word;
  Chr: Word;
begin
  Buffer := GetNext;
  if (Buffer and $80) <> 0 then
  begin
    Chr := Buffer and $3F;
    if (Buffer and $20) <> 0 then
    begin
      Buffer := GetNext;
      if (Buffer and $C0) <> $80 then
      begin
        Result := TBuffChar(0);
        exit;
      end;
      Chr := (Chr shl 6) or (Buffer and $3F);
    end;
    Buffer := GetNext;
    if (Buffer and $C0) <> $80 then
    begin
      Result := TBuffChar(0);
      exit;
    end;
    Result := TBuffChar((Chr shl 6) or (Buffer and $3F));
  end
  else
    Result := TBuffChar(Buffer);
end;

{ TBuffConvHighMap }

//-- BG ---------------------------------------------------------- 02.10.2012 --
procedure TBuffConvHighMap.AfterConstruction;
begin
  inherited;
  UpdateMap;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
procedure TBuffConvHighMap.Assign(Source: TBuffConverter);
begin
  inherited;
  UpdateMap;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvHighMap.NextChar: TBuffChar;
begin
  Result := TBuffChar(GetNext);
  case Ord(Result) of
    $80..$FF: Result := FMap[Ord(Result)];
  end;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
procedure TBuffConvHighMap.UpdateMap;
begin
  case CodePage of
    437: FMap := CP437Map;
    708: FMap := Windows708Map;
    737: FMap := Windows737Map;
    775: FMap := Windows775Map;
    850: FMap := CP850Map;
    852: FMap := CP852Map;
    855: FMap := CP855Map;
    857: FMap := CP857Map;
    860: FMap := IBM860Map;
    861: FMap := CP861Map;
    862: FMap := CP862Map;
    863: FMap := CP863Map;
    864: FMap := Windows864Map;
    865: FMap := CP865Map;
    866: FMap := CP866Map;
    869: FMap := CP869Map;
    874: FMap := Windows874Map;
    10000: FMap := MacRomanMap;
    10004: FMap := MacArabicMap;
    10005: FMap := MacHebrewMap;
    10006: FMap := MacGreekMap;
    10007: FMap := MacCyrillicMap;
    10010: FMap := MacRomaniaMap;
    10017: FMap := MacUkraineMap;
    10021: FMap := MacThaiMap;
    10029: FMap := MacCentralEuropeMap;
    10079: FMap := MacIcelandicMap;
    10081: FMap := MacTurkishMap;
    10082: FMap := MacCroatianMap;
  end;
end;

{ TBuffConvIsoMap }

//-- BG ---------------------------------------------------------- 04.10.2012 --
procedure TBuffConvIsoMap.AfterConstruction;
begin
  inherited;
  UpdateMap;
end;

//-- BG ---------------------------------------------------------- 04.10.2012 --
procedure TBuffConvIsoMap.Assign(Source: TBuffConverter);
begin
  inherited;
  UpdateMap;
end;

//-- BG ---------------------------------------------------------- 04.10.2012 --
function TBuffConvIsoMap.NextChar: TBuffChar;
begin
  Result := TBuffChar(GetNext);
  case Ord(Result) of
    $A0..$FF: Result := FMap[Ord(Result)];
  end;
end;

//-- BG ---------------------------------------------------------- 04.10.2012 --
procedure TBuffConvIsoMap.UpdateMap;
begin
  case CodePage of
    28592: FMap := ISO8859_2Map;
    28593: FMap := ISO8859_3Map;
    28594: FMap := ISO8859_4Map;
    28600: FMap := ISO8859_10Map;
    28603: FMap := ISO8859_13Map;
    28604: FMap := ISO8859_14Map;
    28606: FMap := ISO8859_16Map;
  end;
end;

{ TBuffConvShiftJis932 }

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvShiftJis932.NextChar: TBuffChar;
var c1, c2, t1, t2, i, a, b: Word;
    WC: TBuffChar;
begin
  c1 := GetNext;
  Result := TBuffChar(c1);
  case c1 of
    $00..$80: ;
      // done above: Result := TBuffChar(c1);

    $A1..$DF:
      Result := TBuffChar(c1 + $FEC0);

    $81..$86, $88..$9F, $E0..$EA:
    begin
      c2 := GetNext;
      case c2 of
        $40..$7E, $80..$FC:
        begin
          if c1 < $E0 then
            t1 := c1 - $81
          else
            t1 := c1 - $C1;

          if c2 < $80 then
            t2 := c2 - $40
          else
            t2 := c2 - $41;

          if t2 < $5E then
          begin
            c1 := 2 * t1 + 0 + $21;
            c2 := t2 + $21;
          end
          else
          begin
            c1 := 2 * t1 + 1 + $21;
            c2 := t2 - $5E + $21;
          end;

          case c1 of
            $21..$28, $30..$74:
              case c2 of
                $21..$7E:
                begin
                  i := 94 * (c1 - $21) + (c2 - $21);
                  WC := #$FFFD;
                  if i < 1410 then
                  begin
                    if i < 690 then
                      WC := JIS_X0208_1Map[i];
                  end
                  else
                  begin
                    if i < 7808 then
                      WC := JIS_X0208_2Map[i - 1410];
                  end;
                  if WC <> #$FFFD then
                    Result := WC;
                end;
              end;
          end;
        end;
      end;
    end;

    $87, $ED..$EE, $FA..$FC:
    begin
      c2 := GetNext;
      case c2 of
        $40..$7E, $80..$FC:
        begin
          if c1 >= $E0 then
            a := $C1
          else
            a := $81;

          if c2 >= $80 then
            b := $41
          else
            b := $40;

          i := 188 * (c1 - a) + (c2 - b);
          WC := #$FFFD;
          if i < 8272 then
          begin
            if i < 1220 then
              WC := CP932E_1Map[i - 1128];
          end
          else if i < 10716 then
          begin
            if i < 8648 then
              WC := CP932E_2Map[i - 8272]
          end
          else
          begin
            if i < 11104 then
              WC := CP932E_3Map[i - 10716];
          end;
          if WC <> #$FFFD then
            Result := WC;
        end;
      end;
    end;

    $F0..$F9:
    begin
      c2 := GetNext;
      case c2 of
        $40..$7E: Result := TBuffChar($E000 + 188 * (c1 - $F0) + (c2 - $40));
        $80..$FC: Result := TBuffChar($E000 + 188 * (c1 - $F0) + (c2 - $41));
      end;
    end;
  end;
end;

{ TBuffConvIBM936 }

//-- BG ---------------------------------------------------------- 03.10.2012 --
function TBuffConvIBM936.NextChar: TBuffChar;
var
  c1: Word;
begin
  c1 := GetNext;
  case c1 of
    $00..$7F: Result := TBuffChar(c1);
    $80:      Result := TBuffChar($20AC);
  else
    // two byte code
    Result := GBKDecodeChar(c1, GetNext);
  end;
end;

{ TBuffConvHangeul949 }

//-- BG ---------------------------------------------------------- 04.10.2012 --
function TBuffConvHangeul949.NextChar: TBuffChar;

  function UHC1Decode(row, col: Integer): TBuffChar;
  var
    I: Integer;
  begin
    i := 178 * row + col;
    if i < 5696 then
    begin
      if col >= 89 then
        Result := TBuffChar(UHC_1_1Map[2 * row + 1] + UHC_1_2Map[i])
      else
        Result := TBuffChar(UHC_1_1Map[2 * row] + UHC_1_2Map[i])
    end
    else
      Result := #$FFFD;
  end;

  function UHC2Decode(row, col: Integer): TBuffChar;
  var
    I: Integer;
  begin
    i := 84 * row + col;
    if i < 3126 then
    begin
      if col >= 42 then
        Result := TBuffChar(UHC_2_1Map[2 * row + 1] + UHC_2_2Map[i])
      else
        Result := TBuffChar(UHC_2_1Map[2 * row] + UHC_2_2Map[i])
    end
    else
      Result := #$FFFD;
  end;

  function KSC5601Decode(c1, c2: Integer): TBuffChar;
  begin
    Result := KSC5601DecodeChar(c1 - $80, c2 - $80);
  end;

var
  c1, c2: Word;
begin
  c1 := GetNext;
  case c1 of
    $00..$7F: Result := TBuffChar(c1);

    $81..$A0:
    begin
      c2 := GetNext;
      case c2 of
        $40..$5A: Result := UHC1Decode(c1 - $81, c2 - $41);
        $61..$7A: Result := UHC1Decode(c1 - $81, c2 - $47);
        $81..$FE: Result := UHC1Decode(c1 - $81, c2 - $4D);
      else
        Result := TBuffChar($FFFD);
      end;
    end;

    $A1..$C5:
    begin
      c2 := GetNext;
      case c2 of
        $40..$5A: Result := UHC2Decode(c1 - $A1, c2 - $41);
        $61..$7A: Result := UHC2Decode(c1 - $A1, c2 - $47);
        $81..$A0: Result := UHC2Decode(c1 - $A1, c2 - $4D);

        $A1..$E7,
        $E9..$FE: Result := KSC5601Decode(c1, c2);

        $E8:
          if c1 <> $A2 then
            Result := KSC5601Decode(c1, c2)
          else
            Result := TBuffChar($FFFD);

      else
        Result := TBuffChar($FFFD);
      end;
    end;

    $C6..$FD:
    begin
      c2 := GetNext;
      Result := KSC5601Decode(c1, c2);
    end;

    $FE:
    begin
      c2 := GetNext;
      Result := TBuffChar($E000 + (c2 - $A1));
    end;

  else
    Result := TBuffChar($FFFD);
  end;
end;

{ TBuffConvBig5CP950 }

//-- BG ---------------------------------------------------------- 04.10.2012 --
function TBuffConvBig5CP950.NextChar: TBuffChar;

  function CP950Decode1(row, col: Integer): TBuffChar;
  var
    i: Integer;
  begin
    i := 157 * row + col;
    Result := CP950Map[i];
  end;

  function BIG5Decode(row, col: Integer): TBuffChar;
  var
    i: Integer;
  begin
    i := 157 * row + col;
    case i of
         0.. 6120: Result := BIG5_1Map[i];
      6280..13931: Result := BIG5_2Map[i - 6280];
    else
      Result := TBuffChar($FFFD);
    end;
  end;

var
  c1, c2, cc: Word;
begin
  c1 := GetNext;
  case c1 of
    $00..$7F: Result := TBuffChar(c1);    // ANSI/ASCII
    $80, $FF: Result := TBuffChar($FFFD); // undefined
    $81..$A0: Result := TBuffChar($FFFD); // private

    $A1..$A2:
    begin
      c2 := GetNext;
      case c2 of
        $40..$7E: Result := CP950Decode1(c1 - $A1, c2 - $40);
        $A1..$FE: Result := CP950Decode1(c1 - $A1, c2 - $62);
      else
        Result := TBuffChar($FFFD);
      end;
    end;

    $A3..$F9:
    begin
      c2 := GetNext;
      cc := c1 shl 8 + c2;
      case cc of
        $A3E1:        Result := TBuffChar($20AC); // Euro sign
        $C6A1..$C6FE,
        $C800..$C8FE: Result := TBuffChar($FFFD); // private
        $F9D6..$F9FE: Result := CP950EMap[c2 - $D6];
        $F9FF..$FEFE: Result := TBuffChar($FFFD); // private
      else
        case c2 of
          $40..$7E: Result := BIG5Decode(c1 - $A1, c2 - $40);
          $A1..$FE: Result := BIG5Decode(c1 - $A1, c2 - $62);
        else
          Result := TBuffChar($FFFD);
        end;
      end;
    end;
  else
    Result := TBuffChar($FFFD);
  end;
end;

{ TBuffConvEUC_JP }

function TBuffConvEUC_JP.NextChar: TBuffChar;
var
  c1, c2, c3: Word;
begin
  c1 := GetNext;
  case c1 of
    $00..$7F: Result := TBuffChar(c1);    // ANSI/ASCII

    $8E:
    begin
      // two byte code
      c2 := GetNext;
      case c2 of
        $A1..$DF: Result := JIS_X0201DecodeChar(c2);
      else
        Result := TBuffChar($FFFD);
      end;
    end;

    $8F:
    begin
      // three byte code
      c2 := GetNext;
      c3 := GetNext;
      case c2 of
        $A1..$F4:
          case c3 of
            $A1..$FE: Result := JIS_X0212DecodeChar(c2 - $80, c3 - $80);
          else
            Result := TBuffChar($FFFD);
          end;

        $F5..$FE:
          case c3 of
            $A1..$FE: Result := TBuffChar($E3AC + 94 * (c2 - $F5) + (c3 - $A1));
          else
            Result := TBuffChar($FFFD);
          end;
      else
        Result := TBuffChar($FFFD);
      end;
    end;

    $A1..$F4:
    begin
      // two byte code
      c2 := GetNext;
      Result := JIS_X0208DecodeChar(c1 - $80, c2 - $80);
    end;

    $F5..$FE:
    begin
      // two byte code
      c2 := GetNext;
      case c2 of
        $A1..$FE: Result := TBuffChar($E000 + 94 * (c1 - $F5) + (c2 - $A1));
      else
        Result := TBuffChar($FFFD);
      end;
    end;

  else
    // $80..$A0, $FF:
    Result := TBuffChar($FFFD);
  end;
end;

{ TBuffConvISO2022CN }

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvISO2022CN.Assign(Source: TBuffConverter);
var
  Src: TBuffConvISO2022CN absolute Source;
begin
  inherited;
  if Source is TBuffConvISO2022CN then
  begin
    CNState1 := Src.CNState1;
    CNState2 := Src.CNState2;
    CNState3 := Src.CNState3;
  end
  else
  begin
    CNState1 := CNSTATE_ASCII;
    CNState2 := CNSTATE2_NONE;
    CNState3 := CNSTATE3_NONE;
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffConvISO2022CN.CNS11643_1DecodeChar(c1, c2: Word): TBuffChar;
var
  i: Integer;
begin
  case c1 of
    $21..$27, $42, $44..$7D:
    begin
      case c2 of
        $21..$7E:
        begin
          i := 94 * (Ord(c1) - $21) + (Ord(c2) - $21);
          case i of
               0.. 499: Result := CNS11643_1_1Map[i];
             571:       Result := TBuffChar($4EA0);
             578:       Result := TBuffChar($51AB);
             583:       Result := TBuffChar($52F9);
            3102..3134: Result := CNS11643_1_2Map[i - 3102];
            3290..8690: Result := CNS11643_1_3Map[i - 3290];
          else
            Result := TBuffChar(#$FFFD);
          end;
        end;
      else
        Result := TBuffChar(#$FFFD);
      end;
    end;
  else
    Result := TBuffChar(#$FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffConvISO2022CN.CNS11643_2DecodeChar(c1, c2: Word): TBuffChar;
var
  i: Integer;
begin
  case c1 of
    $21..$72:
      case c2 of
        $21..$7E:
        begin
          i := 94 * (c1 - $21) + (c2 - $21);
          if i < 7650 then
            Result := CNS11643_2Map[i]
          else
            Result := TBuffChar(#$FFFD);
        end;
      else
        Result := TBuffChar(#$FFFD);
      end;
  else
    Result := TBuffChar(#$FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffConvISO2022CN.NextChar: TBuffChar;
var
  c1, c2: Word;
  Pos: PAnsiChar;
begin
  repeat
    c1 := GetNext;
    case c1 of
      $1B:
        if FEnd.AnsiChr - FPos.AnsiChr > 2 then
        begin
          Pos := FPos.AnsiChr;
          case TBuffChar(GetNext) of
            '$':
              case TBuffChar(GetNext) of
                ')':
                case TBuffChar(GetNext) of
                  'A':
                  begin
                    CNState2 := STATE2_DESIGNATED_GB2312;
                    Continue;
                  end;

                  'G':
                  begin
                    CNState2 := STATE2_DESIGNATED_CNS11643_1;
                    Continue;
                  end;
                end;

                '*':
                  case TBuffChar(GetNext) of
                    'H':
                    begin
                      CNState3 := STATE3_DESIGNATED_CNS11643_2;
                      Continue;
                    end;
                  end;
              end;

            'N':
              case CNState3 of

                CNSTATE3_NONE:
                  Break;

                STATE3_DESIGNATED_CNS11643_2:
                begin
                  // do not use both GetNexts as parameters. You don't know which parameter is processed first!
                  c2 := GetNext;
                  Result := CNS11643_2DecodeChar(c2, GetNext);
                  Exit;
                end;

              end;
          end;
          FPos.AnsiChr := Pos;
        end;

        $0E:
        begin
          if (CNState2 <> STATE2_DESIGNATED_GB2312) and (CNState2 <> STATE2_DESIGNATED_CNS11643_1) then
            Break;
          CNState1 := CNSTATE_TWOBYTE;
          Continue;
        end;

        $0F:
        begin
          CNState1 := CNSTATE_ASCII;
          Continue;
        end;

    end;
    break;
  until false;

  case CNState1 of
    CNSTATE_ASCII:
    begin
      Result := ASCIIDecodeChar(c1);
      case Result of
        #$000A, #$000D:
        begin
          CNState2 := CNSTATE2_NONE;
          CNState3 := CNSTATE3_NONE;
        end;
      end;
    end;

    CNSTATE_TWOBYTE:
    begin
      c2 := GetNext;
      case CNState2 of
        STATE2_DESIGNATED_GB2312:     Result := GB2312DecodeChar(c1, c2);
        STATE2_DESIGNATED_CNS11643_1: Result := CNS11643_1DecodeChar(c1, c2);
      else
        Result := TBuffChar(#$FFFD);
      end;
    end;
  else
    Result := TBuffChar(#$FFFD);
  end;
end;

{ TBuffConvISO2022KR }

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvISO2022KR.Assign(Source: TBuffConverter);
var
  Src: TBuffConvISO2022KR absolute Source;
begin
  inherited;
  if Source is TBuffConvISO2022KR then
  begin
    KRState1 := Src.KRState1;
    KRState2 := Src.KRState2;
  end
  else
  begin
    KRState1 := KRSTATE_ASCII;
    KRState2 := KRSTATE2_NONE;
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffConvISO2022KR.NextChar: TBuffChar;
var
  c1: Word;
  Pos: PAnsiChar;
begin
  repeat
    c1 := GetNext;
    case c1 of
      $1B:
        if FEnd.AnsiChr - FPos.AnsiChr > 2 then
        begin
          Pos := FPos.AnsiChr;
          case TBuffChar(GetNext) of
            '$':
              case TBuffChar(GetNext) of
                ')':
                  case TBuffChar(GetNext) of
                    'C':
                    begin
                      KRState2 := STATE2_DESIGNATED_KSC5601;
                      Continue;
                    end;
                  end;
              end;
          end;
          FPos.AnsiChr := Pos;
        end;

      $0E:
      begin
        if KRState2 <> STATE2_DESIGNATED_KSC5601 then
          Break;
        KRState1 := KRSTATE_TWOBYTE;
        Continue;
      end;

      $0F:
      begin
        KRState1 := KRSTATE_ASCII;
        Continue;
      end;

    end;
    Break;
  until False;

  case KRState1 of
    KRSTATE_ASCII:
    begin
      Result := ASCIIDecodeChar(c1);
      case Result of
        #$000A, #$000D:
          KRState2 := KRSTATE2_NONE;
      end;
    end;

    KRSTATE_TWOBYTE:
    begin
      if KRState2 <> STATE2_DESIGNATED_KSC5601 then
      begin
        Result := TBuffChar(#$FFFD);
        Exit;
      end;
      Result := KSC5601DecodeChar(c1, GetNext);
    end;
  else
    Result := TBuffChar(#$FFFD);
  end;
end;

{ TBuffConvISO2022JP }

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvISO2022JP.Assign(Source: TBuffConverter);
var
  Src: TBuffConvISO2022JP absolute Source;
begin
  inherited;
  if Source is TBuffConvISO2022JP then
    JPState := Src.JPState;
end;

//-- BG ---------------------------------------------------------- 05.10.2012 --
function TBuffConvISO2022JP.NextChar: TBuffChar;
var
  c1: Word;
  Pos: PAnsiChar;
begin
  repeat
    c1 := GetNext;
    case c1 of
      $1B:
        if FEnd.AnsiChr - FPos.AnsiChr > 2 then
        begin
          Pos := FPos.AnsiChr;
          case TBuffChar(GetNext) of
            '(':
              case TBuffChar(GetNext) of
                'B':
                begin
                  JPState := JPS_ASCII;
                  Continue;
                end;

                'J':
                begin
                  JPState := JPS_JISX0201ROMAN;
                  Continue;
                end;
              end;

            '$':
              case TBuffChar(GetNext) of
                '@', 'B':
                begin
                  JPState := JPS_JISX0208;
                  Continue;
                end;

                'A':
                begin
                  JPState := JPS_GB2312;
                  Continue;
                end;

                '(':
                  case TBuffChar(GetNext) of
                    'C':
                    begin
                      JPState := JPS_KSC5601;
                      Continue;
                    end;

                    'D':
                    begin
                      JPState := JPS_JISX0212;
                      Continue;
                    end;
                  end;
              end;

            '.':
              case TBuffChar(GetNext) of
                'A':
                begin
                  JPState := JPS_8859_1;
                  Continue;
                end;

                'F':
                begin
                  JPState := JPS_8859_7;
                  Continue;
                end;
              end;
            
          end;
          FPos.AnsiChr := Pos;
        end;
    end;
    Break;
  until False;

  case JPState of
    JPS_ASCII:          Result := ASCIIDecodeChar(c1);
    JPS_8859_1:         Result := TBuffChar(c1);
    JPS_8859_7:         Result := ISO8859_7DecodeChar(c1);
    JPS_JISX0201ROMAN:  Result := JIS_X0201DecodeChar(c1);
    JPS_JISX0208:       Result := JIS_X0208DecodeChar(c1, GetNext);
    JPS_JISX0212:       Result := JIS_X0212DecodeChar(c1, GetNext);
    JPS_KSC5601:        Result := KSC5601DecodeChar(c1, GetNext);
    JPS_GB2312:         Result := GB2312DecodeChar(c1, GetNext);
  else
    Result := TBuffChar($FFFD);
  end;
end;

//-- BG ---------------------------------------------------------- 06.10.2012 --
function TBuffConvGB18030.NextChar: TBuffChar;

  function GB18030ExtDecode(row, col: Integer): TBuffChar;
  var
    i: Integer;
  begin
    i := 190 * row + col;
    case i of
      6432:         Result := #$20AC;
      7536:         Result := #$01F9;
      7672..7684:   Result := GB18030Ext_2uni_pagea9[i-7672];
      23750..23844: Result := GB18030Ext_2uni_pagefe[i-23750];
    else
      Result := TBuffChar($FFFD);
    end;
  end;

  function GB18030Uni1DecodeChar(c1, c2, c3, c4: Byte): TBuffChar;
  var
    i: Integer;
    k, k1, k2: Byte;
  begin
    case c4 of
      $30..$39:
      begin
        i := (((c1 - $81) * 10 + (c2 - $30)) * 126 + (c3 - $81)) * 10 + (c4 - $30);
        case i of
          0..39419:
          begin
            k1 := 0;
            k2 := 193;
            while k1 < k2 do
            begin
              k := (k1 + k2) div 2;
              if i <= GB18030Uni_charset2uni_ranges[k shl 1 + 1] then
                k2 := k
              else if i >= GB18030Uni_charset2uni_ranges[k shl 1 + 2] then
                k1 := k + 1
              else
              begin
                Result := TBuffChar($FFFD);
                exit;
              end;
            end;
            Result := TBuffChar(i + gb18030uni_ranges[k1]);
          end;
        else
          Result := TBuffChar($FFFD);
        end;
      end;
    else
      Result := TBuffChar($FFFD);
    end;
  end;

{$ifdef TBuffChar_Has_At_Least_3_Bytes}
  function GB18030Uni2DecodeChar(c1, c2, c3, c4: Byte): TBuffChar;
  var
    i: Integer;
  begin
    case c4 of
      $30..$39:
      begin
        i := (((c1 - $90) * 10 + (c2 - $30)) * 126 + (c3 - $81)) * 10 + (c4 - $30);
        if (i >= 0) and (i < $100000) then
          Result := TBuffChar($10000 + i)
        else
          Result := TBuffChar($FFFD);
      end;
    else
      Result := TBuffChar($FFFD);
    end;
  end;
{$endif TBuffChar_Has_At_Least_3_Bytes}

var
  c1, c2, c3, c4: Word;
begin
  c1 := GetNext;
{$ifdef TBuffChar_Has_At_Least_3_Bytes}
  c2 := 0;
  c3 := 0;
  c4 := 0;
{$endif TBuffChar_Has_At_Least_3_Bytes}
  case c1 of
    $00..$7F: Result := ASCIIDecodeChar(c1);

    $81..$84:
    begin
      // Code set 2 (remainder of Unicode U+0000..U+FFFF)
      c2 := GetNext;
      case c2 of
        $30..$39:
        begin
          c3 := GetNext;
          case c3 of
            $81..$FE:
            begin
              c4 := GetNext;
              Result := GB18030Uni1DecodeChar(c1, c2, c3, c4);
            end;
          else
            Result := TBuffChar($FFFD);
          end;
        end;
      else
        Result := TBuffChar($FFFD);
      end;
    end;

    $A2, $A8..$A9, $FE:
    begin
      // Code set 1 (GBK extended)
      c2 := GetNext;
      case c2 of
        $40..$7E: Result := GB18030ExtDecode(c1 - $81, c2 - $40);
        $80..$FE: Result := GB18030ExtDecode(c1 - $81, c2 - $41);
      else
        Result := TBuffChar($FFFD);
      end;
    end;

  else
    // Code set 1 (GBK extended)
    Result := GBKDecodeChar(c1, GetNext);
  end;

{$ifdef TBuffChar_Has_At_Least_3_Bytes}
  if Result <> TBuffChar($FFFD) then
    exit;

  { Code set 3 (Unicode U+10000..U+10FFFF) }
  case c1 of
    $90..$E3:
    begin
      if c2 = 0 then
        c2 := GetNext;
      case c2 of
        $30..$39:
        begin
          if c3 = 0 then
            c3 := GetNext;
          case c3 of
            $81..$FE:
            begin
              if c4 = 0 then
                c4 := GetNext;
              Result := GB18030Uni2DecodeChar(c1, c2, c3, c4);
            end;
          else
            Result := TBuffChar($FFFD);
          end;
        end;
      end;
    end;
  end;
{$endif TBuffChar_Has_At_Least_3_Bytes}
end;

{ TBuffConvUTF7 }

//-- BG ---------------------------------------------------------- 05.10.2012 --
procedure TBuffConvUTF7.Assign(Source: TBuffConverter);
var
  Src: TBuffConvUTF7 absolute Source;
begin
  inherited;
  if Source is TBuffConvUTF7 then
  begin
    State := Src.State;
  end
  else
  begin
    State := 0;
  end;
end;

//-- BG ---------------------------------------------------------- 06.10.2012 --
function TBuffConvUTF7.NextChar: TBuffChar;
// Specification: RFC 2152 (and old RFC 1641, RFC 1642)
// The original Base64 encoding is defined in RFC 2045.

  // Set of direct characters:
  //   A-Z a-z 0-9 ' ( ) , - . / : ? space tab lf cr
  function IsDirect(ch: Byte): Boolean; {$ifdef UseInline} inline; {$endif}
  const
    Tab: array [0..15] of Byte = (
      $00, $26, $00, $00, $81, $f3, $ff, $87,
      $fe, $ff, $ff, $07, $fe, $ff, $ff, $07);
  begin
    Result := (ch < 128) and (((Tab[ch shr 3] shr (ch and 7)) and 1) <> 0);
  end;

  // Set of direct and optional direct characters:
  //   A-Z a-z 0-9 ' ( ) , - . / : ? space tab lf cr
  //   ! " # $ % & * ; < = > @ [ ] ^ _ ` { | }
  function IsXDirect(ch: Byte): Boolean; {$ifdef UseInline} inline; {$endif}
  const
    Tab: array [0..15] of Byte = (
      $00, $26, $00, $00, $ff, $f7, $ff, $ff,
      $ff, $ff, $ff, $ef, $ff, $ff, $ff, $3f);
  begin
    Result := (ch < 128) and (((Tab[ch shr 3] shr (ch and 7)) and 1) <> 0);
  end;

  // Set of base64 characters, extended:
  //   A-Z a-z 0-9 + / -
  function IsXBase64(ch: Byte): Boolean; {$ifdef UseInline} inline; {$endif}
  const
    Tab: array [0..15] of Byte = (
      $00, $00, $00, $00, $00, $a8, $ff, $03,
      $fe, $ff, $ff, $07, $fe, $ff, $ff, $07);
  begin
    Result := (ch < 128) and (((Tab[ch shr 3] shr (ch and 7)) and 1) <> 0);
  end;

label
  active, inactive, none;
var
  c1, c2: Byte;
  wc: Cardinal;
  base64state: Byte;
  kmax: Cardinal;         // number of payload bytes to read
  k: Cardinal;            // number of payload bytes already read
  base64count: Cardinal;  // number of base64 bytes already read
  i: Cardinal;
{$ifdef TBuffChar_Has_At_Least_3_Bytes}
  wc1, wc2: Cardinal;
{$endif TBuffChar_Has_At_Least_3_Bytes}
begin
  c1 := GetNext;
  if (state and 3) <> 0 then
    goto active
  else
    goto inactive;

inactive:
  // Here (state & 3) == 0
  if c1 = 0 then
    goto none;

  if IsXDirect(c1) then
  begin
    Result := TBuffChar(c1);
    exit;
  end;
  if c1 = Ord('+') then
  begin
    c2 := GetNext;
    if c2 = Ord('-') then
    begin
      Result := TBuffChar(c1);
      exit;
    end;
    c1 := c2;
    state := 1;
    goto active;
  end;
  Result := TBuffChar($FFFD);
exit;

active:
  // base64 encoding active
  wc := 0;
  base64state := State;
  kmax := 2;
  k := 0;
  base64count := 0;
  i := 0; // valium for the compiler
  while c1 <> 0 do
  begin
    case c1 of
      Ord('A')..Ord('Z'): i := c1 - Ord('A');
      Ord('a')..Ord('z'): i := c1 - Ord('a') + 26;
      Ord('0')..Ord('9'): i := c1 - Ord('0') + 52;
      Ord('+'):           i := 62;
      Ord('/'):           i := 63;
    else
      // c terminates base64 encoding
      if (base64state and Cardinal(-4)) <> 0 then
      begin
        Result := TBuffChar($FFFD); // data must be 0, otherwise illegal
        State := 0;
        exit;
      end;
      if base64count <> 0 then
      begin
        Result := TBuffChar($FFFD); // partial UTF-16 characters are invalid
        State := 0;
        exit;
      end;

      if c1 = Ord('-') then
        c1 := GetNext;
      state := 0;
      goto inactive;
    end;
    //s++;
    Inc(base64count);
    //* read 6 bits: 0 <= i < 64 */
    case base64state and 3 of
      1: // inside base64, no pending bits
      begin
        base64state := i shl 2;
      end;

      0: // inside base64, 6 bits remain from 1st byte
      begin
        wc := (wc shl 8) or (base64state and Cardinal(-4)) or (i shr 4);
        Inc(k);
        base64state := ((i and 15) shl 4) or 2;
      end;

      2: // inside base64, 4 bits remain from 2nd byte
      begin
        wc := (wc shl 8) or (base64state and Cardinal(-4)) or (i shr 2);
        Inc(k);
        base64state := ((i and 3) shl 6) or 3;
      end;

      3: // inside base64, 2 bits remain from 3rd byte
      begin
        wc := (wc shl 8) or (base64state and Cardinal(-4)) or i;
        Inc(k);
        base64state := 1;
      end;
    end;
    if k = kmax then
    begin
      // UTF-16: When we see a High Surrogate, we must also decode the following Low Surrogate.
      if (kmax = 2) and (wc >= $D800) and (wc <= $DBFF) then
        kmax := 4
      else
        break;
    end;
    if base64count <= 0 then
      goto none;
    c1 := GetNext;
    if c1 = 0 then
      goto none;
  end;

  // Here k = kmax > 0, hence base64count > 0.
  if (base64state and 3) = 0 then
    Result := TBuffChar($FFFD)
  else if kmax = 4 then
{$ifdef TBuffChar_Has_At_Least_3_Bytes}
  begin
    wc1 := wc shr 16;
    wc2 := wc and 0xffff;
    if not ((wc1 >= $D800) and (wc1 < $DC00)) then
      Result := TBuffChar($FFFD)
    else if not ((wc2 >= $DC00) and (wc2 < $E000)) then
      Result := TBuffChar($FFFD)
    else
      Result := $10000 + ((wc1 - $D800) shl 10) + (wc2 - $DC00);
  end
{$else  TBuffChar_Has_At_Least_3_Bytes}
    Result := TBuffChar($FFFD)
{$endif TBuffChar_Has_At_Least_3_Bytes}
  else
    Result := TBuffChar(wc);
  State := base64state;
exit;

none:
  if c1 = 0 then
    Result := TBuffChar(0)
  else
    Result := TBuffChar($FFFD);
end;

end.
