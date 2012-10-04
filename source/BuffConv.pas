{
HtmlViewer Version 11.4
Copyright (c) 2010-2012 by Bernd Gabriel

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

{.$I htmlcons.inc}

unit BuffConv;

interface

uses
  Windows,
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
  // BG, 26.09.2012: default converter. Uses function MultiByteToWideChar() but cannot convert
  // multibyte code pages as MultiByteToWideChar() does not return the number of consumed source bytes.
  TBuffConvSingleByte = class(TBuffConverter)
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
    FHighMap: AnsiCharHighMap;
    procedure UpdateHighMap;
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    procedure AfterConstruction; override;
    function NextChar: TBuffChar; override;
  end;

  // BG, 02.10.2012: converts code page 858 using an AnsiCharHighMap to unicode.
  TBuffConvLatin1Euro = class(TBuffConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  TBuffConvEstonian922 = class(TBuffConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 26.09.2012: converts shift-jis (codepages 932, 943) to unicode.
  TBuffConvShiftJis932 = class(TBuffConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 27.09.2012: converts stateful ISO-2022-JP (codepage 50220) to unicode.
  TBuffConvISO2022JP = class(TBuffConverter)
  private
    FJis: ( // lead in:
      bjsAscii,       // ESC ( B
      bjsX0201_1976,  // ESC ( J
      bjsX0208_1978,  // ESC $ @
      bjsX0208_1983   // ESC $ B
    );

    function GetAsShiftJis(j, k: Word; out Buffer: TBuffArray4): Integer;
    function GetNextJisAsShiftJis(out Buffer: TBuffArray4): Integer; {$ifdef UseInline} inline; {$endif}
  protected
    procedure Assign(Source: TBuffConverter); override;
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 02.10.2012: base class for several eastern asia codepages to unicode converters
  TBuffConvEastAsia = class(TBuffConverter)
  protected
    function GB2312DecodeChar(c1, c2: Byte): TBuffChar;
    function KSC5601DecodeChar(c1, c2: Byte): TBuffChar;
    function JIS_X0201DecodeChar(c1: Byte): TBuffChar;
    function JIS_X0208DecodeChar(c1, c2: Byte): TBuffChar;
    function JIS_X0212DecodeChar(c1, c2: Byte): TBuffChar;
  end;

  // BG, 03.10.2012: converts simplified Chinese (PRC, Singapore) / Chinese simplified (GB2312) to unicode.
  TBuffConvIBM936 = class(TBuffConvEastAsia)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 04.10.2012: converts hangeul (codepage 949 to unicode.
  TBuffConvHangeul949 = class(TBuffConvEastAsia)
  public
    function NextChar: TBuffChar; override;
  end;

  // BG, 04.10.2012: converts Traditional Chinese (code page 950) to unicode.
  TBuffConvBig5CP950 = class(TBuffConverter)
  public
    function NextChar: TBuffChar; override;
  end;

  TBuffConvEUC = class(TBuffConvEastAsia)
  protected
    function TwoByteDecoder(c1, c2: Byte): TBuffChar; virtual; abstract;
  public
    function NextChar: TBuffChar; override;
  end;

  TBuffConvEUC_CN_GB = class(TBuffConvEUC)
  protected
    function TwoByteDecoder(c1: Byte; c2: Byte): TBuffChar; override;
  end;

  TBuffConvEUC_KR = class(TBuffConvEUC)
  protected
    function TwoByteDecoder(c1: Byte; c2: Byte): TBuffChar; override;
  end;

  TBuffConvEUC_JP = class(TBuffConvEastAsia)
  public
    function NextChar: TBuffChar; override;
  end;

// CodePages (most names taken from http://msdn.microsoft.com/en-us/library/windows/desktop/dd317756%28v=vs.85%29.aspx):
const CodePageInfos: array [0..162] of TBuffConvInfo = (
  // first entry must be code page CP_UNKNOWN!!!
  ( CodePage: CP_UNKNOWN; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Unknown'),
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
  ( CodePage:        858; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvLatin1Euro;  Name: 'OEM Multilingual Latin 1 + Euro symbol'),
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
  ( CodePage:        922; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvEstonian922; Name: 'ANSI/OEM Estonian (DOS)'),
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
  ( CodePage:      28591; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-1 Latin 1; Western European (ISO)'),
  ( CodePage:      28592; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-2 Central European; Central European (ISO)'),
  ( CodePage:      28593; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-3 Latin 3'),
  ( CodePage:      28594; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-4 Baltic'),
  ( CodePage:      28595; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-5 Cyrillic'),
  ( CodePage:      28596; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-6 Arabic'),
  ( CodePage:      28597; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-7 Greek'),
  ( CodePage:      28598; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-8 Hebrew; Hebrew (ISO-Visual)'),
  ( CodePage:      28599; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-9 Turkish'),
  ( CodePage:      28600; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-10 ??? CP 28600'),
  ( CodePage:      28601; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-11 ??? CP 28601'),
  ( CodePage:      28602; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-12 ??? CP 28602'),
  ( CodePage:      28603; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-13 Estonian'),
  ( CodePage:      28604; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-14 ??? CP 28604'),
  ( CodePage:      28605; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-15 Latin 9'),
  ( CodePage:      28606; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-16 ??? CP 28606'),
  ( CodePage:      29001; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Europa 3'),
  ( CodePage:      38598; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 8859-8 Hebrew; Hebrew (ISO-Logical)'),
  ( CodePage:      50220; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvISO2022JP;   Name: 'ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)'),
  ( CodePage:      50221; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvISO2022JP;   Name: 'ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)'),
  ( CodePage:      50222; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvISO2022JP;   Name: 'ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI)'),
  ( CodePage:      50225; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 2022 Korean'),
  ( CodePage:      50227; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022)'),
  ( CodePage:      50229; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'ISO 2022 Traditional Chinese'),
  ( CodePage:      50930; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Japanese (Katakana) Extended'),
  ( CodePage:      50931; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC US-Canada and Japanese'),
  ( CodePage:      50933; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Korean Extended and Korean'),
  ( CodePage:      50935; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Simplified Chinese Extended and Simplified Chinese'),
  ( CodePage:      50936; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Simplified Chinese'),
  ( CodePage:      50937; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC US-Canada and Traditional Chinese'),
  ( CodePage:      50939; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'EBCDIC Japanese (Latin) Extended and Japanese'),
  ( CodePage:      51932; CharSet: SHIFTJIS_CHARSET;    Converter: TBuffConvEUC_JP;      Name: 'EUC Japanese'),
  ( CodePage:      51936; CharSet: GB2312_CHARSET;      Converter: TBuffConvEUC_CN_GB;   Name: 'EUC Simplified Chinese; Chinese Simplified (EUC)'),
  ( CodePage:      51949; CharSet: HANGEUL_CHARSET;     Converter: TBuffConvEUC_KR;      Name: 'EUC Korean'),
  ( CodePage:      51950; CharSet: CHINESEBIG5_CHARSET; Converter: TBuffConvBig5CP950;   Name: 'EUC Traditional Chinese'),
  ( CodePage:      52936; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ)'),
  ( CodePage:      54936; CharSet: UNKNOWN_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)'),
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
  ( CodePage:      65000; CharSet: DEFAULT_CHARSET;     Converter: TBuffConvSingleByte;  Name: 'Unicode (UTF-7)'),
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

{ TBuffConvSingleByte }

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffConvSingleByte.NextChar: TBuffChar;
var
  Buffer: Word;
begin
  Buffer := GetNext;
  if Buffer <> 0 then
    MultiByteToWideChar(FCodePage, 0, PAnsiChar(@Buffer), 1, @Result, 1)
  else
    Result := TBuffChar(0);
end;

{ TBuffConvUTF8 }

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffConvUTF8.NextChar: TBuffChar;
var
  Buffer: Word;
  Chr: Cardinal;
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
  UpdateHighMap;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
procedure TBuffConvHighMap.Assign(Source: TBuffConverter);
begin
  inherited;
  UpdateHighMap;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvHighMap.NextChar: TBuffChar;
begin
  Result := TBuffChar(GetNext);
  case Ord(Result) of
    $80..$FF: Result := FHighMap[Ord(Result)];
  end;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
procedure TBuffConvHighMap.UpdateHighMap;
begin
  case CodePage of
    437: FHighMap := CP437Map;
    708: FHighMap := Windows708Map;
    737: FHighMap := Windows737Map;
    775: FHighMap := Windows775Map;
    850: FHighMap := CP850Map;
    852: FHighMap := CP852Map;
    855: FHighMap := CP855Map;
    857: FHighMap := CP857Map;
    860: FHighMap := IBM860Map;
    861: FHighMap := CP861Map;
    862: FHighMap := CP862Map;
    863: FHighMap := CP863Map;
    864: FHighMap := Windows864Map;
    865: FHighMap := CP865Map;
    866: FHighMap := CP866Map;
    869: FHighMap := CP869Map;
    874: FHighMap := Windows874Map;
    10000: FHighMap := MacRomanMap;
    10004: FHighMap := MacArabicMap;
    10005: FHighMap := MacHebrewMap;
    10006: FHighMap := MacGreekMap;
    10007: FHighMap := MacCyrillicMap;
    10010: FHighMap := MacRomaniaMap;
    10017: FHighMap := MacUkraineMap;
    10021: FHighMap := MacThaiMap;
    10029: FHighMap := MacCentralEuropeMap;
    10079: FHighMap := MacIcelandicMap;
    10081: FHighMap := MacTurkishMap;
    10082: FHighMap := MacCroatianMap;
  end;
end;

{ TBuffConvLatin1Euro }

//-- BG ---------------------------------------------------------- 03.10.2012 --
function TBuffConvLatin1Euro.NextChar: TBuffChar;
begin
  Result := TBuffChar(GetNext);
  case Ord(Result) of
    $00..$7F:;
    $D5: Result := #$20AC;
  else
    Result := CP850Map[Ord(Result)];
  end;
end;

{ TBuffConvEstonian922 }

//-- BG ---------------------------------------------------------- 03.10.2012 --
function TBuffConvEstonian922.NextChar: TBuffChar;
begin
  Result := TBuffChar(GetNext);
  case Ord(Result) of
    $A0..$AF: Result := CP922_1Map[Ord(Result) - $A0];
    $D0..$DF: Result := CP922_2Map[Ord(Result) - $D0];
    $F0..$FF: Result := CP922_3Map[Ord(Result) - $F0];
  end;
end;

{ TBuffConvISO2022JP }

//-- BG ---------------------------------------------------------- 27.09.2012 --
function TBuffConvISO2022JP.GetAsShiftJis(j, k: Word; out Buffer: TBuffArray4): Integer;
// core method to convert a 2 byte EUC-JP resp. X0208 code to ShiftJIS
var
  s, t: Word;
begin
  if (j = 0) or (k = 0) then
  begin
    Buffer[0] := 0;
    Result := 1;
    exit;
  end;

  Result := 2;

  {first byte}
  //j := FPos.BytePtr^ and $7F; {and $7F just for safety}
  if (j in [33..96]) then
    s := (j + 1) div 2 + 112
  else
    s := (j + 1) div 2 + 176;
  Buffer[0] := s;

  {second byte}
  //k := FPos.BytePtr^ and $7F; {and $7F just for safety}
  if odd(j) then
  begin
    t := k + 31;
    if k > 95 then
      inc(t);
  end
  else
    t := k + 126;
  Buffer[1] := t;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
procedure TBuffConvISO2022JP.Assign(Source: TBuffConverter);
var
  Src: TBuffConvISO2022JP absolute Source;
begin
  inherited;
  FJis := Src.FJis;
end;

//-- BG ---------------------------------------------------------- 27.09.2012 --
function TBuffConvISO2022JP.GetNextJisAsShiftJis(out Buffer: TBuffArray4): Integer;
{
  The following table gives the escape sequences and the character sets
  used in "ISO-2022-JP" messages. The reg# is the registration number
  in ISO's registry [ISOREG].

                          94 character sets
  reg#  character set      ESC sequence                designated to
  ------------------------------------------------------------------
  6     ASCII              ESC 2/8 4/2      ESC ( B    G0
  42    JIS X 0208-1978    ESC 2/4 4/0      ESC $ @    G0
  87    JIS X 0208-1983    ESC 2/4 4/2      ESC $ B    G0
  14    JIS X 0201-Roman   ESC 2/8 4/10     ESC ( J    G0

  The following table gives the additional escape sequences and the
  character sets used in "ISO-2022-JP-2" messages.

                          94 character sets
  reg#  character set      ESC sequence                designated to
  ------------------------------------------------------------------
  58    GB2312-1980        ESC 2/4 4/1      ESC $ A    G0
  149   KSC5601-1987       ESC 2/4 2/8 4/3  ESC $ ( C  G0
  159   JIS X 0212-1990    ESC 2/4 2/8 4/4  ESC $ ( D  G0

                          96 character sets
  reg#  character set      ESC sequence                designated to
  ------------------------------------------------------------------
  100   ISO8859-1          ESC 2/14 4/1     ESC . A    G2
  126   ISO8859-7(Greek)   ESC 2/14 4/6     ESC . F    G2

  Source: http://tools.ietf.org/html/rfc1554#ref-ISO2022

  Currently this methods only support ISO-2022-JP.
}
const
  ESC = 27;
var
  Pos: PAnsiChar;
  Chr: Word;
begin
  Result := 0;
  while FPos.AnsiChr < FEnd.AnsiChr do
  begin
    Chr := GetNext;
    case Chr of
      ESC:
        if FEnd.AnsiChr - FPos.AnsiChr > 2 then
        begin
          Pos := FPos.AnsiChr;
          case GetNext of
            Ord('('):
              case GetNext of
                Ord('B'):
                  begin
                    FJis := bjsAscii;
                    continue;
                  end;

                Ord('J'):
                  begin
                    FJis := bjsX0201_1976;
                    continue;
                  end;
              end;

            Ord('$'):
              case GetNext of
                Ord('@'):
                  begin
                    FJis := bjsX0208_1978;
                    continue;
                  end;

                Ord('B'):
                  begin
                    FJis := bjsX0208_1983;
                    continue;
                  end;
              end;

          end;
          FPos.AnsiChr := Pos;
        end;
    end;

    case FJis of
      {one byte codes}
      bjsAscii,
      bjsX0201_1976:
        begin
          Buffer[0] := Chr;
          Result := 1;
          break;
        end;

      {two byte codes / 94 character sets}
      bjsX0208_1978,
      bjsX0208_1983:
      begin
        Result := GetAsShiftJis(Chr, GetNext, Buffer);
        break;
      end;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 27.09.2012 --
function TBuffConvISO2022JP.NextChar: TBuffChar;
var
  Len: Integer;
  Buffer2: TBuffArray4;
begin
  if FPos.AnsiChr < FEnd.AnsiChr then
  begin
    Len := GetNextJisAsShiftJis(Buffer2);
    MultiByteToWideChar(932, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
  end
  else
    Result := TBuffChar(0);

end;

{ TBuffConvShiftJis932 }

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvShiftJis932.NextChar: TBuffChar;
var c1, c2, t1, t2, i, a, b: Cardinal;
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
        $40..$7E:
          Result := TBuffChar($E000 + 188 * (c1 - $F0) + (c2 - $40));

        $80..$FC:
          Result := TBuffChar($E000 + 188 * (c1 - $F0) + (c2 - $41));
      end;
    end;
  end;
end;

{ TBuffConvEastAsia }

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvEastAsia.GB2312DecodeChar(c1, c2: Byte): TBuffChar;
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
            Result := TBuffChar($0);
          end;
        end;
      else
        Result := TBuffChar($0);
      end;
  else
    Result := TBuffChar($0);
  end;
end;

//-- BG ---------------------------------------------------------- 04.10.2012 --
function TBuffConvEastAsia.JIS_X0201DecodeChar(c1: Byte): TBuffChar;
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
function TBuffConvEastAsia.JIS_X0208DecodeChar(c1, c2: Byte): TBuffChar;
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
function TBuffConvEastAsia.JIS_X0212DecodeChar(c1, c2: Byte): TBuffChar;
var
  i: Integer;
begin
  Result := #$0;
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
function TBuffConvEastAsia.KSC5601DecodeChar(c1, c2: Byte): TBuffChar;
var
  i: Integer;
begin
  Result := #$0;
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
          end;
        end;
      end;
  end;
end;

{ TBuffConvEUC }

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvEUC.NextChar: TBuffChar;
var
  c1, c2: Cardinal;
begin
  c1 := Ord(FPos.AnsiChr[0]);
  Result := TBuffChar(c1);
  Inc(FPos.AnsiChr, 1);
  case c1 of
    0:
    begin
      Dec(FPos.AnsiChr, 1);
      Result := #$0;
    end;

    $01..$80: ;
      // done above: Result := TBuffChar(c1);

    $A1..$FE:
    begin
      c2 := Ord(FPos.AnsiChr[0]);
      if c2 = 0 then
        Exit;
      Inc(FPos.AnsiChr, 1);
      Result := TwoByteDecoder(c1 - $80, c2 - $80);
    end;
  end;
end;

{ TBuffConvEUC_CN_GB }

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvEUC_CN_GB.TwoByteDecoder(c1, c2: Byte): TBuffChar;
begin
  Result := GB2312DecodeChar(c1, c2);
end;

{ TBuffConvEUC_KR }

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConvEUC_KR.TwoByteDecoder(c1, c2: Byte): TBuffChar;
begin
  Result := KSC5601DecodeChar(c1, c2);
end;

{ TBuffConvIBM936 }

//-- BG ---------------------------------------------------------- 03.10.2012 --
function TBuffConvIBM936.NextChar: TBuffChar;

  function Offset(cmul, c1, c2: Cardinal): Integer;
  begin
    case c2 of
      $40..$7E: Result := cmul * (c1 - $81) + (c2 - $40);
      $80..$FE: Result := cmul * (c1 - $81) + (c2 - $41);
    else
      Result := MaxInt;
    end;
  end;

var
  c1, c2: Cardinal;
  i: Integer;
begin
  c1 := GetNext;
  case c1 of
    $00..$7F: Result := TBuffChar(c1);
    $80:      Result := TBuffChar($20AC);
  else
    // two byte code
    c2 := GetNext;

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
    if Result <> #0 then
      Exit;

    case c1 of
      $A6, $A8:
      begin
        i := Offset(190, c1, c2);
        case i of
          7189..7210: Result := CP936E_1Map[i - 7189];
          7532..7537: Result := CP936E_2Map[i - 7532];
        end;
        if Result = #$FFFD then
          Result := #0;
        if Result <> #0 then
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
        if Result = #$FFFD then
          Result := #0;
        if Result <> #0 then
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
    if Result = #0 then
      Result := #$FFFD;
  end;

var
  c1, c2: Cardinal;
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

        $A1..$E7, $E9..$FE: Result := KSC5601Decode(c1, c2);

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
  c1, c2, cc: Cardinal;
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
  c1, c2, c3: Cardinal;
begin
  c1 := GetNext;
  case c1 of
    $00..$7F: Result := TBuffChar(c1);    // ANSI/ASCII
    $80, $FF: Result := TBuffChar($FFFD); // undefined
    // $81..$A0: Result := TBuffChar($FFFD);

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
  end;
end;

end.
