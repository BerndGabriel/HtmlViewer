{
HtmlViewer Version 11.10
Copyright (c) 2010-2023 by Bernd Gabriel

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

{$I htmlcons.inc}

unit HtmlBuffer;

interface

{Important: Be sure to list LclType after SysUtils and Classes
   in order to use LclType's THandle declaration (32 or 64 bits)
   rather than THandle in SysUtils and Classes (=System.THandle,
   which apparently is always 32 bits).}
uses
{$ifdef LCL}
  Classes, SysUtils,
  LclIntf, LclType, HtmlMisc,
{$else}
  Windows,
  Classes, SysUtils,
{$endif}
  Graphics, 
{$ifdef UNICODE}
  AnsiStrings,
{$endif}
  //
  HtmlGlobals;

type
  EConversionError = class(Exception);

  TBuffCodePage = LongInt;
  TBuffCharSet = SmallInt;  // slightly more than a TFontCharset.

  TBuffChar = ThtChar;      // the type of a wide char
  PBuffChar = PhtChar;
  TBuffString = ThtString;  // the type of a wide string
  TBuffStringList = ThtStringList;  // the type of a wide string

  TBuffPointer = record
    case Integer of
      0: (BytePtr: PByte;);
      1: (WordPtr: PWord;);
      2: (AnsiChr: PAnsiChar;);
      3: (WideChr: PWideChar;);
    end;

  TBuffConverter = class
  protected
    FPos, FEnd: TBuffPointer;
    FCodePage, FInitalCodePage: TBuffCodePage;
    function GetNext: Word;
    procedure Assign(Source: TBuffConverter); virtual;
  public
    constructor Create(CurrPtr, EndPtr: TBuffPointer; CodePage, InitalCodePage: TBuffCodePage); virtual;
    constructor CreateCopy(Source: TBuffConverter); overload;

    function NextChar: TBuffChar; virtual;
    function PeekChar: TBuffChar;
    function AsString: TBuffString; virtual;
    property CodePage: TBuffCodePage read FCodePage;
  end;
  TBuffConverterClass =  class of TBuffConverter;

  TBuffArray = array of Byte;
  TBuffArray4 = array [0..3] of Byte;

  // BG, 11.11.2011: Issue 93: LoadStrings failure
  TBufferState = set of (
    bsFixedCodePage // ignore writing to property CodePage.
                    // Helps to ignore <META http-equiv="Content-Type" ...>,
                    // if buffer is an already converted TBuffString.
  );

  // BG, 17.12.2010: helps converting any kind of stream to WideChars.
  //
  // Although loading files via ThtStringList.LoadFromFile(FileName) produces WideChars,
  // these WideChars may not be coded correctly, if the file is not a unicode file.
  // For those cases you can apply a CodePage to the TBuffString versions of Convert()/Create().
  // Or you can use the PByte versions with the InitialCodePage parameters and set InitialCodePage to CP_UTF16LE.
  //
  // Avoid passing such raw ThtStringList.Text to HtmlViewer.
  // HtmlViewer assumes correct unicode in ThtString.
  // You MUST convert it before passing it to HtmlViewer: HtmlViewer.LoadFromString(TBuffer.Convert(StringList.Text, CodePage));
  // Or you let HtmlViewer load it from file or a TFileStream.
  //
  TBuffer = class
  private
    FBuffer: TBuffArray;
    FStart: TBuffPointer;
    FEnd: TBuffPointer;
    FName: TBuffString;
    FConverter: TBuffConverter;
    FState: TBufferState;
    FBomLength: Cardinal;

    function GetCodePage: TBuffCodePage;  {$ifdef UseInline} inline; {$endif}
    function GetPosition: Integer; {$ifdef UseInline} inline; {$endif}
    procedure DetectCodePage;
    procedure Reset;
    procedure SetCodePage(Value: TBuffCodePage); {$ifdef UseInline} inline; {$endif}
    procedure SetCodePages(Value, Initial: TBuffCodePage);
    procedure SetPosition(const Value: Integer); {$ifdef UseInline} inline; {$endif}
    procedure SetStream(Stream: TStream); {$ifdef UseInline} inline; {$endif}
  public
    class function Convert(Text: PByte; ByteCount: Integer; CodePage, InitialCodePage: TBuffCodePage): TBuffString; overload; {$ifdef UseInline} inline; {$endif}
    class function Convert(Text: PByte; ByteCount: Integer; CodePage: TBuffCodePage): TBuffString; overload; {$ifdef UseInline} inline; {$endif}
    class function Convert(const Text: TBuffString; CodePage: TBuffCodePage): TBuffString; overload; {$ifdef UseInline} inline; {$endif}
    constructor Create(const Doc: TBuffer); overload;
    constructor Create(Stream: TStream; CodePage: TBuffCodePage; Name: TBuffString = ''); overload;
    constructor Create(Stream: TStream; Name: TBuffString = ''); overload;
    constructor Create(Text: PByte; ByteCount: Integer; CodePage, InitialCodePage: TBuffCodePage; const Name: TBuffString = ''); overload;
    constructor Create(Text: PByte; ByteCount: Integer; CodePage: TBuffCodePage; const Name: TBuffString = ''); overload;
    constructor Create(const Text: TBuffString; CodePage: TBuffCodePage; const Name: TBuffString = ''); overload;
    constructor Create(const Text: TBuffString; const Name: TBuffString = ''; CodePage: TBuffCodePage = CP_UTF16LE); overload;
    destructor Destroy; override;
    function AsString: TBuffString; {$ifdef UseInline} inline; {$endif}
    function GetString(FromIndex, UntilIndex: Integer): TBuffString;
    function NextChar: TBuffChar; {$ifdef UseInline} inline; {$endif}
    function PeekChar: TBuffChar; {$ifdef UseInline} inline; {$endif}
    function Size: Integer; {$ifdef UseInline} inline; {$endif}
    procedure Assign(Source: TBuffer); virtual;
    procedure AssignTo(Destin: TObject); virtual;
    property CodePage: TBuffCodePage read GetCodePage write SetCodePage;
    property Name: TBuffString read FName;
    property Position: Integer read GetPosition write SetPosition;
    property BomLength: Cardinal read FBomLength;
  end;

// BG, 13.10.2012: Code Page Management
//
// You can register your own code page to unicode converters or unregister your own or predefined converters.
// Registering an existing converter again updates it.
//
// As each code page may have any number of aliases there are 2 sets of un-/register methods.
// One to register a code page name to code page number mappings and
// one to register the converter. 

// Add a name of a code page. Code pages can have many names, but each name names one code page only:
procedure RegisterCodePageName(const CodePageName: TBuffString; CodePage: TBuffCodePage);
// Remove a name of a code page:
procedure UnregisterCodePageName(const CodePageName: TBuffString);
// Get code page by name:
function StrToCodePage(const CodePageName: TBuffString): TBuffCodePage;

// Add a code page to unicode converter. Only one converter per code page is allowed,
// but a converter may handle more than one code page:
procedure RegisterCodePageToUnicodeConverter(
  CodePage: TBuffCodePage; Converter: TBuffConverterClass;
  const ConverterName: TBuffString = ''; CharSet: TBuffCharSet = UNKNOWN_CHARSET);
// Remove a code page to unicode converter:
procedure UnregisterCodePageToUnicodeConverter(CodePage: TBuffCodePage);
// Get charset of code page:
function CodePageToCharSet(CodePage: TBuffCodePage): TBuffCharSet;

implementation

uses
  BuffConv;

type

  TBuffConvInfo = class
  private
    FName:      TBuffString;
    FCodePage:  TBuffCodePage;
    FCharSet:   TBuffCharSet;
    FConverter: TBuffConverterClass;
  public
    constructor Create(CodePage: TBuffCodePage; CharSet: TBuffCharSet; Converter: TBuffConverterClass; const Name: TBuffString);
    property CharSet: TBuffCharSet read FCharSet;
    property CodePage: TBuffCodePage read FCodePage;
    property Converter: TBuffConverterClass read FConverter;
    property Name: TBuffString read FName;
  end;

  TBuffConvInfoList = class(TList)
  private
    FSorted: Boolean;
    function GetItem(Index: Integer): TBuffConvInfo;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create;
    procedure Add(Item: TBuffConvInfo);
    function Find(CodePage: TBuffCodePage): Integer;
    procedure Sort;
    property Items[Index: Integer]: TBuffConvInfo read GetItem; default;
    property Sorted: Boolean read FSorted;
  end;

  TBuffCodePageName = class
  private
    FName: TBuffString;
    FCodePage: TBuffCodePage;
  public
    constructor Create(Name: TBuffString; CodePage: TBuffCodePage);
    property CodePage: TBuffCodePage read FCodePage;
    property Name: TBuffString read FName;
  end;

  TBuffCodePageNameList = class(TBuffStringList)
  private
    function GetItem(Index: Integer): TBuffCodePageName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Info: TBuffCodePageName); reintroduce;
    function IndexOf(const S: TBuffString): Integer; override;
    property Items[Index: Integer]: TBuffCodePageName read GetItem; default;
  end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
var
  VFinalizing: Boolean;
  VConvertersByCodePage: TBuffConvInfoList;
  VCodePagesByName: TBuffCodePageNameList;

//-- BG ---------------------------------------------------------- 12.10.2012 --
function GetCodePageNameList: TBuffCodePageNameList;
begin
  if (VCodePagesByName = nil) and not VFinalizing then
    VCodePagesByName := TBuffCodePageNameList.Create;
  Result := VCodePagesByName;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
procedure RegisterCodePageName(const CodePageName: TBuffString; CodePage: TBuffCodePage);
var
  CodePagesByName: TBuffCodePageNameList;
begin
  CodePagesByName := GetCodePageNameList;
  if CodePagesByName <> nil then
    CodePagesByName.Add(TBuffCodePageName.Create(CodePageName, CodePage));
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
procedure UnregisterCodePageName(const CodePageName: TBuffString);
var
  CodePagesByName: TBuffCodePageNameList;
  Index: Integer;
begin
  CodePagesByName := GetCodePageNameList;
  if CodePagesByName <> nil then
  begin
    Index := -1;
    if not CodePagesByName.Find(CodePageName, Index) then
      Index := CodePagesByName.IndexOf(CodePageName);
    if Index >= 0 then
    begin
      CodePagesByName[Index].Free;
      CodePagesByName.Delete(Index);
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
function StrToCodePageInfo(const CodePageName: TBuffString): TBuffCodePageName;
var
  CodePagesByName: TBuffCodePageNameList;
  Index: Integer;
begin
  CodePagesByName := GetCodePageNameList;
  Result := nil;
  if CodePagesByName <> nil then
  begin
    Index := -1;
    if not CodePagesByName.Find(CodePageName, Index) then
      Index := CodePagesByName.IndexOf(CodePageName);
    if Index >= 0 then
      Result := CodePagesByName[Index];
  end;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
function StrToCodePage(const CodePageName: TBuffString): TBuffCodePage;
var
  Info: TBuffCodePageName;
begin
  Info := StrToCodePageInfo(CodePageName);
  if Info <> nil then
    Result := Info.CodePage
  else
    Result := CP_UNKNOWN;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
function GetConvInfoList: TBuffConvInfoList;
begin
  if (VConvertersByCodePage = nil) and not VFinalizing then
    VConvertersByCodePage := TBuffConvInfoList.Create;
  Result := VConvertersByCodePage;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
procedure RegisterCodePageToUnicodeConverter(
  CodePage: TBuffCodePage; Converter: TBuffConverterClass;
  const ConverterName: TBuffString = ''; CharSet: TBuffCharSet = UNKNOWN_CHARSET);
var
  Index: Integer;
  CodePageInfos: TBuffConvInfoList;
  CodePageInfo: TBuffConvInfo;
begin
  CodePageInfos := GetConvInfoList;
  if CodePageInfos <> nil then
  begin
    Index := CodePageInfos.Find(CodePage);
    if Index >= 0 then
    begin
      // update existing code page
      CodePageInfo := CodePageInfos[Index];
      CodePageInfo.FName := ConverterName;
      CodePageInfo.FCharSet := CharSet;
      CodePageInfo.FConverter := Converter;
    end
    else
      // Add new code page
      CodePageInfos.Add(TBuffConvInfo.Create(CodePage, CharSet, Converter, ConverterName));
  end;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
procedure UnregisterCodePageToUnicodeConverter(CodePage: TBuffCodePage);
var
  Index: Integer;
  CodePageInfos: TBuffConvInfoList;
begin
  CodePageInfos := GetConvInfoList;
  if CodePageInfos <> nil then
  begin
    Index := CodePageInfos.Find(CodePage);
    if Index >= 0 then
      CodePageInfos.Delete(Index);
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
function CodePageToCharSet(CodePage: TBuffCodePage): TBuffCharSet;
var
  Index: Integer;
  CodePageInfos: TBuffConvInfoList;
begin
  Result := UNKNOWN_CHARSET;
  CodePageInfos := GetConvInfoList;
  if CodePageInfos <> nil then
  begin
    Index := CodePageInfos.Find(CodePage);
    if Index >= 0 then
      Result := CodePageInfos[Index].CharSet;
  end;

  if Result = UNKNOWN_CHARSET then
    //raise EConversionError.CreateFmt('Don''t know equivalent char set for code page %d.', [CodePage]);
    Result := DEFAULT_CHARSET;
end;

//-- BG ---------------------------------------------------------- 15.07.2012 --
function StrNCmpA(Str1, Str2: PAnsiChar; N: Integer): Integer;
var
  StrEnd: PAnsiChar;
begin
  StrEnd := Str1 + N;
  while Str1 < StrEnd do
  begin
    Result := Ord(Str1^) - Ord(Str2^);
    if (Result <> 0) or (Str1^ = #0) then
      Exit;
    Inc(Str1);
    Inc(Str2);
  end;
  Result := 0;
end;

//-- BG ---------------------------------------------------------- 15.07.2012 --
function StrNPosA(Str, SubStr: PAnsiChar; N: Integer): PAnsiChar;
// returns a pointer to the first occurrence of SubStr in the first N chars of Str.
// Finds SubStr only, if the entire SubStr is within the first N chars.
// Example:
//   StrNPosA('expression', 'press', 7) finds pointer to 'pression' while
//   StrNPosA('expression', 'press', 6) retuns nil, as 'press' is not in the first 6 chars ('expres').
var
  L: Integer;
  StrEnd: PAnsiChar;
begin
  StrEnd := SubStr;
  while StrEnd^ <> #0 do
    Inc(StrEnd);
  L := StrEnd - SubStr;
  if L > 0 then  // Make sure substring not null string and SubStr fits into Str at all.
  begin
    Result := Str;
    StrEnd := Str + N - L;
    while (Result < StrEnd) and (Result^ <> #0) do
    begin
      if StrNCmpA(Result, SubStr, L) = 0 then
        Exit;
      Inc(Result);
    end;
  end;
  Result := nil;
end;

{ TBuffConvInfo }

//-- BG ---------------------------------------------------------- 12.10.2012 --
constructor TBuffConvInfo.Create(CodePage: TBuffCodePage; CharSet: TBuffCharSet; Converter: TBuffConverterClass; const Name: TBuffString);
begin
  inherited Create;
  FName := Name;
  FCodePage := CodePage;
  FCharSet := CharSet;
  FConverter := Converter;
end;

{ TBuffConvInfoList }

function CompareConvInfo(Item1, Item2: Pointer): Integer;
var
  Info1: TBuffConvInfo absolute Item1;
  Info2: TBuffConvInfo absolute Item2;
begin
  Result := Info1.CodePage - Info2.CodePage;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
procedure TBuffConvInfoList.Add(Item: TBuffConvInfo);
begin
  inherited Add(Item);
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
constructor TBuffConvInfoList.Create;
begin
  inherited Create;
  Add(TBuffConvInfo.Create(CP_UNKNOWN, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'Unknown'));
  Add(TBuffConvInfo.Create(    CP_ACP, DEFAULT_CHARSET,     TBuffConvSingleByte,    'Default ANSI CP'));
  Add(TBuffConvInfo.Create(  CP_OEMCP, OEM_CHARSET,         TBuffConvSingleByte,    'Default OEM CP'));
  Add(TBuffConvInfo.Create(  CP_MACCP, MAC_CHARSET,         TBuffConvSingleByte,    'Default MAC CP'));
  Add(TBuffConvInfo.Create(        37, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC US-Canada'));
  Add(TBuffConvInfo.Create(       437, ANSI_CHARSET,        TBuffConvHighMap,       'OEM United States'));
  Add(TBuffConvInfo.Create(       500, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC International'));
  Add(TBuffConvInfo.Create(       708, ARABIC_CHARSET,      TBuffConvHighMap,       'Arabic (ASMO 708)'));
  Add(TBuffConvInfo.Create(       709, ARABIC_CHARSET,      TBuffConvSingleByte,    'Arabic (ASMO-449+, BCON V4)'));
  Add(TBuffConvInfo.Create(       710, ARABIC_CHARSET,      TBuffConvSingleByte,    'Arabic - Transparent Arabic'));
  Add(TBuffConvInfo.Create(       720, ARABIC_CHARSET,      TBuffConvSingleByte,    'Arabic (Transparent ASMO); Arabic (DOS)'));
  Add(TBuffConvInfo.Create(       737, GREEK_CHARSET,       TBuffConvHighMap,       'OEM Greek (formerly 437G); Greek (DOS)'));
  Add(TBuffConvInfo.Create(       775, BALTIC_CHARSET,      TBuffConvHighMap,       'OEM Baltic; Baltic (DOS)'));
  Add(TBuffConvInfo.Create(       850, ANSI_CHARSET,        TBuffConvHighMap,       'OEM Multilingual Latin 1; Western European (DOS)'));
  Add(TBuffConvInfo.Create(       852, EASTEUROPE_CHARSET,  TBuffConvHighMap,       'OEM Latin 2; Central European (DOS)'));
  Add(TBuffConvInfo.Create(       855, RUSSIAN_CHARSET,     TBuffConvHighMap,       'OEM Cyrillic (primarily Russian)'));
  Add(TBuffConvInfo.Create(       857, TURKISH_CHARSET,     TBuffConvHighMap,       'OEM Turkish; Turkish (DOS)'));
  Add(TBuffConvInfo.Create(       858, ANSI_CHARSET,        TBuffConvSingleByte,    'OEM Multilingual Latin 1 + Euro symbol'));
  Add(TBuffConvInfo.Create(       860, ANSI_CHARSET,        TBuffConvHighMap,       'OEM Portuguese; Portuguese (DOS)'));
  Add(TBuffConvInfo.Create(       861, ANSI_CHARSET,        TBuffConvHighMap,       'OEM Icelandic; Icelandic (DOS)'));
  Add(TBuffConvInfo.Create(       862, HEBREW_CHARSET,      TBuffConvHighMap,       'OEM Hebrew; Hebrew (DOS)'));
  Add(TBuffConvInfo.Create(       863, ANSI_CHARSET,        TBuffConvHighMap,       'OEM French Canadian; French Canadian (DOS)'));
  Add(TBuffConvInfo.Create(       864, ARABIC_CHARSET,      TBuffConvHighMap,       'OEM Arabic; Arabic (864)'));
  Add(TBuffConvInfo.Create(       865, ANSI_CHARSET,        TBuffConvHighMap,       'OEM Nordic; Nordic (DOS)'));
  Add(TBuffConvInfo.Create(       866, RUSSIAN_CHARSET,     TBuffConvHighMap,       'OEM Russian; Cyrillic (DOS)'));
  Add(TBuffConvInfo.Create(       869, GREEK_CHARSET,       TBuffConvHighMap,       'OEM Modern Greek; Greek, Modern (DOS)'));
  Add(TBuffConvInfo.Create(       870, EASTEUROPE_CHARSET,  TBuffConvSingleByte,    'IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2'));
  Add(TBuffConvInfo.Create(       874, THAI_CHARSET,        TBuffConvHighMap,       'ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows)'));
  Add(TBuffConvInfo.Create(       875, GREEK_CHARSET,       TBuffConvSingleByte,    'IBM EBCDIC Greek Modern'));
  Add(TBuffConvInfo.Create(       922, BALTIC_CHARSET,      TBuffConvSingleByte,    'ANSI/OEM Estonian (DOS)'));
  Add(TBuffConvInfo.Create(       932, SHIFTJIS_CHARSET,    TBuffConvShiftJis932,   'ANSI/OEM Japanese; Japanese (Shift-JIS)'));
  Add(TBuffConvInfo.Create(       936, GB2312_CHARSET,      TBuffConvIBM936,        'ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)'));
  Add(TBuffConvInfo.Create(       943, SHIFTJIS_CHARSET,    TBuffConvShiftJis932,   'ANSI/OEM Japanese (Shift-JIS)'));
  Add(TBuffConvInfo.Create(       949, HANGEUL_CHARSET,     TBuffConvHangeul949,    'ANSI/OEM Korean (Unified Hangul Code)'));
  Add(TBuffConvInfo.Create(       950, CHINESEBIG5_CHARSET, TBuffConvBig5CP950,     'ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)'));
  Add(TBuffConvInfo.Create(      1026, TURKISH_CHARSET,     TBuffConvSingleByte,    'IBM EBCDIC Turkish (Latin 5)'));
  Add(TBuffConvInfo.Create(      1047, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Latin 1/Open System'));
  Add(TBuffConvInfo.Create(      1140, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro)'));
  Add(TBuffConvInfo.Create(      1141, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro)'));
  Add(TBuffConvInfo.Create(      1142, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro)'));
  Add(TBuffConvInfo.Create(      1143, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro)'));
  Add(TBuffConvInfo.Create(      1144, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro)'));
  Add(TBuffConvInfo.Create(      1145, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro)'));
  Add(TBuffConvInfo.Create(      1146, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro)'));
  Add(TBuffConvInfo.Create(      1147, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro)'));
  Add(TBuffConvInfo.Create(      1148, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro)'));
  Add(TBuffConvInfo.Create(      1149, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro)'));
  Add(TBuffConvInfo.Create(      1200, DEFAULT_CHARSET,     TBuffConverter,         'Unicode UTF-16, little endian byte order (BMP of ISO 10646); available only to managed applications'));
  Add(TBuffConvInfo.Create(      1201, DEFAULT_CHARSET,     TBuffConverter,         'Unicode UTF-16, big endian byte order; available only to managed applications'));
  Add(TBuffConvInfo.Create(      1250, EASTEUROPE_CHARSET,  TBuffConvHighMap,       'ANSI Central European; Central European (Windows)'));
  Add(TBuffConvInfo.Create(      1251, RUSSIAN_CHARSET,     TBuffConvHighMap,       'ANSI Cyrillic; Cyrillic (Windows)'));
  Add(TBuffConvInfo.Create(      1252, ANSI_CHARSET,        TBuffConvHighMap,       'ANSI Latin 1; Western European (Windows)'));
  Add(TBuffConvInfo.Create(      1253, GREEK_CHARSET,       TBuffConvHighMap,       'ANSI Greek; Greek (Windows)'));
  Add(TBuffConvInfo.Create(      1254, TURKISH_CHARSET,     TBuffConvHighMap,       'ANSI Turkish; Turkish (Windows)'));
  Add(TBuffConvInfo.Create(      1255, HEBREW_CHARSET,      TBuffConvHighMap,       'ANSI Hebrew; Hebrew (Windows)'));
  Add(TBuffConvInfo.Create(      1256, ARABIC_CHARSET,      TBuffConvHighMap,       'ANSI Arabic; Arabic (Windows)'));
  Add(TBuffConvInfo.Create(      1257, BALTIC_CHARSET,      TBuffConvHighMap,       'ANSI Baltic; Baltic (Windows)'));
  Add(TBuffConvInfo.Create(      1258, VIETNAMESE_CHARSET,  TBuffConvHighMap,       'ANSI/OEM Vietnamese; Vietnamese (Windows)'));
  Add(TBuffConvInfo.Create(      1361, JOHAB_CHARSET,       TBuffConvSingleByte,    'Korean (Johab)'));
  Add(TBuffConvInfo.Create(     10000, ANSI_CHARSET,        TBuffConvHighMap,       'MAC Roman; Western European (Mac)'));
  Add(TBuffConvInfo.Create(     10001, SHIFTJIS_CHARSET,    TBuffConvSingleByte,    'Japanese (Mac)'));
  Add(TBuffConvInfo.Create(     10002, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'MAC Traditional Chinese (Big5); Chinese Traditional (Mac)'));
  Add(TBuffConvInfo.Create(     10003, HANGEUL_CHARSET,     TBuffConvSingleByte,    'Korean (Mac)'));
  Add(TBuffConvInfo.Create(     10004, ARABIC_CHARSET,      TBuffConvHighMap,       'Arabic (Mac)'));
  Add(TBuffConvInfo.Create(     10005, HEBREW_CHARSET,      TBuffConvHighMap,       'Hebrew (Mac)'));
  Add(TBuffConvInfo.Create(     10006, GREEK_CHARSET,       TBuffConvHighMap,       'Greek (Mac)'));
  Add(TBuffConvInfo.Create(     10007, RUSSIAN_CHARSET,     TBuffConvHighMap,       'Cyrillic (Mac)'));
  Add(TBuffConvInfo.Create(     10008, GB2312_CHARSET,      TBuffConvSingleByte,    'MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac)'));
  Add(TBuffConvInfo.Create(     10010, EASTEUROPE_CHARSET,  TBuffConvHighMap,       'Romanian (Mac)'));
  Add(TBuffConvInfo.Create(     10017, RUSSIAN_CHARSET,     TBuffConvHighMap,       'Ukrainian (Mac)'));
  Add(TBuffConvInfo.Create(     10021, THAI_CHARSET,        TBuffConvHighMap,       'Thai (Mac)'));
  Add(TBuffConvInfo.Create(     10029, EASTEUROPE_CHARSET,  TBuffConvHighMap,       'MAC Latin 2; Central European (Mac)'));
  Add(TBuffConvInfo.Create(     10079, ANSI_CHARSET,        TBuffConvHighMap,       'Icelandic (Mac)'));
  Add(TBuffConvInfo.Create(     10081, TURKISH_CHARSET,     TBuffConvHighMap,       'Turkish (Mac)'));
  Add(TBuffConvInfo.Create(     10082, EASTEUROPE_CHARSET,  TBuffConvHighMap,       'Croatian (Mac)'));
  Add(TBuffConvInfo.Create(     12000, DEFAULT_CHARSET,     TBuffConvSingleByte,    'Unicode UTF-32, little endian byte order; available only to managed applications'));
  Add(TBuffConvInfo.Create(     12001, DEFAULT_CHARSET,     TBuffConvSingleByte,    'Unicode UTF-32, big endian byte order; available only to managed applications'));
  Add(TBuffConvInfo.Create(     20000, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'CNS Taiwan; Chinese Traditional (CNS)'));
  Add(TBuffConvInfo.Create(     20001, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'TCA Taiwan'));
  Add(TBuffConvInfo.Create(     20002, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'Eten Taiwan; Chinese Traditional (Eten)'));
  Add(TBuffConvInfo.Create(     20003, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'IBM5550 Taiwan'));
  Add(TBuffConvInfo.Create(     20004, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'TeleText Taiwan'));
  Add(TBuffConvInfo.Create(     20005, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'Wang Taiwan'));
  Add(TBuffConvInfo.Create(     20105, ANSI_CHARSET,        TBuffConvSingleByte,    'IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5)'));
  Add(TBuffConvInfo.Create(     20106, ANSI_CHARSET,        TBuffConvSingleByte,    'IA5 German (7-bit)'));
  Add(TBuffConvInfo.Create(     20107, ANSI_CHARSET,        TBuffConvSingleByte,    'IA5 Swedish (7-bit)'));
  Add(TBuffConvInfo.Create(     20108, ANSI_CHARSET,        TBuffConvSingleByte,    'IA5 Norwegian (7-bit)'));
  Add(TBuffConvInfo.Create(     20127, ANSI_CHARSET,        TBuffConvSingleByte,    'US-ASCII (7-bit)'));
  Add(TBuffConvInfo.Create(     20261, ANSI_CHARSET,        TBuffConvSingleByte,    'T.61'));
  Add(TBuffConvInfo.Create(     20269, ANSI_CHARSET,        TBuffConvSingleByte,    'ISO 6937 Non-Spacing Accent'));
  Add(TBuffConvInfo.Create(     20273, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Germany'));
  Add(TBuffConvInfo.Create(     20277, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Denmark-Norway'));
  Add(TBuffConvInfo.Create(     20278, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Finland-Sweden'));
  Add(TBuffConvInfo.Create(     20280, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Italy'));
  Add(TBuffConvInfo.Create(     20284, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Latin America-Spain'));
  Add(TBuffConvInfo.Create(     20285, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC United Kingdom'));
  Add(TBuffConvInfo.Create(     20290, SHIFTJIS_CHARSET,    TBuffConvSingleByte,    'IBM EBCDIC Japanese Katakana Extended'));
  Add(TBuffConvInfo.Create(     20297, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC France'));
  Add(TBuffConvInfo.Create(     20420, ARABIC_CHARSET,      TBuffConvSingleByte,    'IBM EBCDIC Arabic'));
  Add(TBuffConvInfo.Create(     20423, GREEK_CHARSET,       TBuffConvSingleByte,    'IBM EBCDIC Greek'));
  Add(TBuffConvInfo.Create(     20424, HEBREW_CHARSET,      TBuffConvSingleByte,    'IBM EBCDIC Hebrew'));
  Add(TBuffConvInfo.Create(     20833, HANGEUL_CHARSET,     TBuffConvSingleByte,    'IBM EBCDIC Korean Extended'));
  Add(TBuffConvInfo.Create(     20838, THAI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Thai'));
  Add(TBuffConvInfo.Create(     20866, RUSSIAN_CHARSET,     TBuffConvHighMap,       'Russian (KOI8-R); Cyrillic (KOI8-R)'));
  Add(TBuffConvInfo.Create(     20871, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Icelandic'));
  Add(TBuffConvInfo.Create(     20880, RUSSIAN_CHARSET,     TBuffConvSingleByte,    'IBM EBCDIC Cyrillic Russian'));
  Add(TBuffConvInfo.Create(     20905, TURKISH_CHARSET,     TBuffConvSingleByte,    'IBM EBCDIC Turkish'));
  Add(TBuffConvInfo.Create(     20924, ANSI_CHARSET,        TBuffConvSingleByte,    'IBM EBCDIC Latin 1/Open System (1047 + Euro symbol)'));
  Add(TBuffConvInfo.Create(     20932, SHIFTJIS_CHARSET,    TBuffConvSingleByte,    'Japanese (JIS 0208-1990 and 0121-1990)'));
  Add(TBuffConvInfo.Create(     20936, GB2312_CHARSET,      TBuffConvSingleByte,    'Simplified Chinese (GB2312); Chinese Simplified (GB2312-80)'));
  Add(TBuffConvInfo.Create(     20949, HANGEUL_CHARSET,     TBuffConvSingleByte,    'Korean Wansung'));
  Add(TBuffConvInfo.Create(     21025, RUSSIAN_CHARSET,     TBuffConvSingleByte,    'IBM EBCDIC Cyrillic Serbian-Bulgarian'));
  Add(TBuffConvInfo.Create(     21027, UNKNOWN_CHARSET,     TBuffConvSingleByte,    '(deprecated)'));
  Add(TBuffConvInfo.Create(     21866, RUSSIAN_CHARSET,     TBuffConvSingleByte,    'Ukrainian (KOI8-U); Cyrillic (KOI8-U)'));
  Add(TBuffConvInfo.Create(     28591, ANSI_CHARSET,        TBuffConverter,         'ISO 8859-1 Latin 1; Western European (ISO)'));
  Add(TBuffConvInfo.Create(     28592, EASTEUROPE_CHARSET,  TBuffConvIsoMap,        'ISO 8859-2 Latin 2; Central European; Central European (ISO)'));
  Add(TBuffConvInfo.Create(     28593, ANSI_CHARSET,        TBuffConvIsoMap,        'ISO 8859-3 Latin 3; Southern European'));
  Add(TBuffConvInfo.Create(     28594, ANSI_CHARSET,        TBuffConvIsoMap,        'ISO 8859-4 Latin 4; Northern European'));
  Add(TBuffConvInfo.Create(     28595, RUSSIAN_CHARSET,     TBuffConvSingleByte,    'ISO 8859-5 Cyrillic'));
  Add(TBuffConvInfo.Create(     28596, ARABIC_CHARSET,      TBuffConvSingleByte,    'ISO 8859-6 Arabic'));
  Add(TBuffConvInfo.Create(     28597, GREEK_CHARSET,       TBuffConvSingleByte,    'ISO 8859-7 Greek'));
  Add(TBuffConvInfo.Create(     28598, HEBREW_CHARSET,      TBuffConvSingleByte,    'ISO 8859-8 Hebrew; Hebrew (ISO-Visual)'));
  Add(TBuffConvInfo.Create(     28599, TURKISH_CHARSET,     TBuffConvSingleByte,    'ISO 8859-9 Turkish'));
  Add(TBuffConvInfo.Create(     28600, ANSI_CHARSET,        TBuffConvIsoMap,        'ISO 8859-10 Latin 6; Nordic'));
  Add(TBuffConvInfo.Create(     28601, THAI_CHARSET,        TBuffConvSingleByte,    'ISO 8859-11 Thai'));
  Add(TBuffConvInfo.Create(     28603, BALTIC_CHARSET,      TBuffConvIsoMap,        'ISO 8859-13 Latin 7; Baltic'));
  Add(TBuffConvInfo.Create(     28604, ANSI_CHARSET,        TBuffConvIsoMap,        'ISO 8859-14 Latin 8; Celtic'));
  Add(TBuffConvInfo.Create(     28605, ANSI_CHARSET,        TBuffConvSingleByte,    'ISO 8859-15 Latin 9; Western European'));
  Add(TBuffConvInfo.Create(     28606, ANSI_CHARSET,        TBuffConvIsoMap,        'ISO 8859-16 Latin 10; South Eastern European'));
  Add(TBuffConvInfo.Create(     29001, ANSI_CHARSET,        TBuffConvSingleByteMap, 'Europa 3'));
  Add(TBuffConvInfo.Create(     38598, HEBREW_CHARSET,      TBuffConvSingleByte,    'ISO 8859-8 Hebrew; Hebrew (ISO-Logical)'));
  Add(TBuffConvInfo.Create(     50220, SHIFTJIS_CHARSET,    TBuffConvISO2022JP,     'ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)'));
  Add(TBuffConvInfo.Create(     50221, SHIFTJIS_CHARSET,    TBuffConvISO2022JP,     'ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)'));
  Add(TBuffConvInfo.Create(     50222, SHIFTJIS_CHARSET,    TBuffConvISO2022JP,     'ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI)'));
  Add(TBuffConvInfo.Create(     50225, HANGEUL_CHARSET,     TBuffConvISO2022KR,     'ISO 2022 Korean'));
  Add(TBuffConvInfo.Create(     50227, GB2312_CHARSET,      TBuffConvISO2022CN,     'ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022)'));
  Add(TBuffConvInfo.Create(     50229, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'ISO 2022 Traditional Chinese'));
  Add(TBuffConvInfo.Create(     50930, SHIFTJIS_CHARSET,    TBuffConvSingleByte,    'EBCDIC Japanese (Katakana) Extended'));
  Add(TBuffConvInfo.Create(     50931, ANSI_CHARSET,        TBuffConvSingleByte,    'EBCDIC US-Canada and Japanese'));
  Add(TBuffConvInfo.Create(     50933, HANGEUL_CHARSET,     TBuffConvSingleByte,    'EBCDIC Korean Extended and Korean'));
  Add(TBuffConvInfo.Create(     50935, GB2312_CHARSET,      TBuffConvSingleByte,    'EBCDIC Simplified Chinese Extended and Simplified Chinese'));
  Add(TBuffConvInfo.Create(     50936, GB2312_CHARSET,      TBuffConvSingleByte,    'EBCDIC Simplified Chinese'));
  Add(TBuffConvInfo.Create(     50937, CHINESEBIG5_CHARSET, TBuffConvSingleByte,    'EBCDIC US-Canada and Traditional Chinese'));
  Add(TBuffConvInfo.Create(     50939, SHIFTJIS_CHARSET,    TBuffConvSingleByte,    'EBCDIC Japanese (Latin) Extended and Japanese'));
  Add(TBuffConvInfo.Create(     51932, SHIFTJIS_CHARSET,    TBuffConvEUC_JP,        'EUC Japanese'));
  Add(TBuffConvInfo.Create(     51936, GB2312_CHARSET,      TBuffConvDoubleByte,    'EUC Simplified Chinese; Chinese Simplified (EUC)'));
  Add(TBuffConvInfo.Create(     51949, HANGEUL_CHARSET,     TBuffConvDoubleByte,    'EUC Korean'));
  Add(TBuffConvInfo.Create(     51950, CHINESEBIG5_CHARSET, TBuffConvBig5CP950,     'EUC Traditional Chinese'));
  Add(TBuffConvInfo.Create(     52936, GB2312_CHARSET,      TBuffConvSingleByte,    'HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ)'));
  Add(TBuffConvInfo.Create(     54936, GB2312_CHARSET,      TBuffConvGB18030,       'Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)'));
  Add(TBuffConvInfo.Create(     57002, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Devanagari'));
  Add(TBuffConvInfo.Create(     57003, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Bengali'));
  Add(TBuffConvInfo.Create(     57004, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Tamil'));
  Add(TBuffConvInfo.Create(     57005, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Telugu'));
  Add(TBuffConvInfo.Create(     57006, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Assamese'));
  Add(TBuffConvInfo.Create(     57007, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Oriya'));
  Add(TBuffConvInfo.Create(     57008, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Kannada'));
  Add(TBuffConvInfo.Create(     57009, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Malayalam'));
  Add(TBuffConvInfo.Create(     57010, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Gujarati'));
  Add(TBuffConvInfo.Create(     57011, UNKNOWN_CHARSET,     TBuffConvSingleByte,    'ISCII Punjabi'));
  Add(TBuffConvInfo.Create(     65000, DEFAULT_CHARSET,     TBuffConvUTF7,          'Unicode (UTF-7)'));
  Add(TBuffConvInfo.Create(     65001, DEFAULT_CHARSET,     TBuffConvUTF8,          'Unicode (UTF-8)'));
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
function TBuffConvInfoList.Find(CodePage: TBuffCodePage): Integer;
var
  Low, Index: Integer;
begin
  Result := Count;
  if not Sorted then
    Sort;
  Low := 0;
  while Low < Result do
  begin
    Index := (Low + Result) shr 1;
    if Items[Index].CodePage < CodePage then
      Low := Index + 1
    else
      Result := Index;
  end;
  if Items[Result].CodePage <> CodePage then
    Result := -1;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
function TBuffConvInfoList.GetItem(Index: Integer): TBuffConvInfo;
begin
  Result := Get(Index);
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
procedure TBuffConvInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  case Action of
    lnAdded:   FSorted := False;
    lnDeleted: TBuffConvInfo(Ptr).Free;
  end;
end;

//-- BG ---------------------------------------------------------- 12.10.2012 --
procedure TBuffConvInfoList.Sort;
begin
  inherited Sort(CompareConvInfo);
  FSorted := True;
end;

{ TBuffCodePageName }

//-- BG ---------------------------------------------------------- 22.12.2010 --
constructor TBuffCodePageName.Create(Name: TBuffString; CodePage: TBuffCodePage);
begin
  inherited Create;
  FName := Name;
  FCodePage := CodePage;
end;

{ TBuffCodePageNameList }

//-- BG ---------------------------------------------------------- 22.12.2010 --
procedure TBuffCodePageNameList.Add(Info: TBuffCodePageName);
begin
  AddObject(Info.FName, Info);
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
constructor TBuffCodePageNameList.Create;
//  The official CharSet definitions:
//    http://www.iana.org/assignments/character-sets
//    http://www.iana.org/assignments/ianacharset-mib
begin
  inherited Create;
  BeginUpdate;
  try
    Add(TBuffCodePageName.Create('708', 708));                       //708
    Add(TBuffCodePageName.Create('asmo-708', 708));                  //708

    Add(TBuffCodePageName.Create('737', 737));                       //737
    Add(TBuffCodePageName.Create('ibm737', 737));                    //737
    Add(TBuffCodePageName.Create('cp437', 737));                     //737
    Add(TBuffCodePageName.Create('cspc8codepage437', 737));          //737

    Add(TBuffCodePageName.Create('775', 775));                       //775
    Add(TBuffCodePageName.Create('ibm775', 775));                    //775
    Add(TBuffCodePageName.Create('cp775', 775));                     //775
    Add(TBuffCodePageName.Create('cspc775baltic', 775));             //775

    Add(TBuffCodePageName.Create('850', 850));                       //850
    Add(TBuffCodePageName.Create('ibm850', 850));                    //850
    Add(TBuffCodePageName.Create('cp850', 850));                     //850
    Add(TBuffCodePageName.Create('cspc850multilingual', 850));       //850

    Add(TBuffCodePageName.Create('852', 852));                       //852
    Add(TBuffCodePageName.Create('ibm852', 852));                    //852
    Add(TBuffCodePageName.Create('cp852', 852));                     //852
    Add(TBuffCodePageName.Create('cspcp852', 852));                  //852

    Add(TBuffCodePageName.Create('855', 855));                       //855
    Add(TBuffCodePageName.Create('ibm855', 855));                    //855
    Add(TBuffCodePageName.Create('cp855', 855));                     //855
    Add(TBuffCodePageName.Create('csibm855', 855));                  //855

    Add(TBuffCodePageName.Create('857', 857));                       //857
    Add(TBuffCodePageName.Create('ibm857', 857));                    //857
    Add(TBuffCodePageName.Create('cp857', 857));                     //857
    Add(TBuffCodePageName.Create('csibm857', 857));                  //857

    Add(TBuffCodePageName.Create('858', 858));                       //858
    Add(TBuffCodePageName.Create('ibm00858', 858));                  //858
    Add(TBuffCodePageName.Create('cp858', 858));                     //858
    Add(TBuffCodePageName.Create('ccsid00858', 858));                //858
    Add(TBuffCodePageName.Create('cp00858', 858));                   //858
    Add(TBuffCodePageName.Create('pc-multilingual-850+euro', 858));  //858

    Add(TBuffCodePageName.Create('860', 860));                       //860
    Add(TBuffCodePageName.Create('ibm860', 860));                    //860
    Add(TBuffCodePageName.Create('cp860', 860));                     //860
    Add(TBuffCodePageName.Create('csibm860', 860));                  //860

    Add(TBuffCodePageName.Create('861', 861));                       //861
    Add(TBuffCodePageName.Create('ibm861', 861));                    //861
    Add(TBuffCodePageName.Create('cp861', 861));                     //861
    Add(TBuffCodePageName.Create('csibm861', 861));                  //861
    Add(TBuffCodePageName.Create('cp-is', 861));                     //861

    Add(TBuffCodePageName.Create('862', 862));                       //862
    Add(TBuffCodePageName.Create('ibm862', 862));                    //862
    Add(TBuffCodePageName.Create('cp862', 862));                     //862
    Add(TBuffCodePageName.Create('cspc862latinhebrew', 862));        //862

    Add(TBuffCodePageName.Create('863', 863));                       //863
    Add(TBuffCodePageName.Create('ibm863', 863));                    //863
    Add(TBuffCodePageName.Create('cp863', 863));                     //863
    Add(TBuffCodePageName.Create('csibm863', 863));                  //863

    Add(TBuffCodePageName.Create('864', 864));                       //864
    Add(TBuffCodePageName.Create('ibm864', 864));                    //864
    Add(TBuffCodePageName.Create('cp864', 864));                     //864
    Add(TBuffCodePageName.Create('csibm864', 864));                  //864

    Add(TBuffCodePageName.Create('865', 865));                       //865
    Add(TBuffCodePageName.Create('ibm865', 865));                    //865
    Add(TBuffCodePageName.Create('cp865', 865));                     //865
    Add(TBuffCodePageName.Create('csibm865', 865));                  //865

    Add(TBuffCodePageName.Create('866', 866));                       //866
    Add(TBuffCodePageName.Create('ibm866', 866));                    //866
    Add(TBuffCodePageName.Create('cp866', 866));                     //866
    Add(TBuffCodePageName.Create('csibm866', 866));                  //866

    Add(TBuffCodePageName.Create('869', 869));                       //869
    Add(TBuffCodePageName.Create('ibm869', 869));                    //869
    Add(TBuffCodePageName.Create('cp869', 869));                     //869
    Add(TBuffCodePageName.Create('cp-gr', 869));                     //869
    Add(TBuffCodePageName.Create('csibm869', 869));                  //869

    Add(TBuffCodePageName.Create('874', 874));                       //874
    Add(TBuffCodePageName.Create('windows-874', 874));               //874
    Add(TBuffCodePageName.Create('dos-874', 874));                   //874
    Add(TBuffCodePageName.Create('cp874', 874));                     //874
    Add(TBuffCodePageName.Create('thai', 874));                      //874
    Add(TBuffCodePageName.Create('tis-620', 874));                   //874
    Add(TBuffCodePageName.Create('tis620', 874));                    //874

    Add(TBuffCodePageName.Create('922', 922));                       //922
    Add(TBuffCodePageName.Create('cp922', 922));                     //922

    Add(TBuffCodePageName.Create('932', 932));                       //932
    Add(TBuffCodePageName.Create('ms_kanji', 932));                  //932
    Add(TBuffCodePageName.Create('csshiftjis', 932));                //932
    Add(TBuffCodePageName.Create('cswindows31j', 932));              //932
    Add(TBuffCodePageName.Create('shift-jis', 932));                 //932
    Add(TBuffCodePageName.Create('shift_jis', 932));                 //932
    Add(TBuffCodePageName.Create('shiftjis', 932));                  //932
    Add(TBuffCodePageName.Create('x-ms-cp932', 932));                //932
    Add(TBuffCodePageName.Create('ms932', 932));                     //932
    Add(TBuffCodePageName.Create('cp932', 932));                     //932
    Add(TBuffCodePageName.Create('x-sjis', 932));                    //932

    Add(TBuffCodePageName.Create('936', 936));                       //936
    Add(TBuffCodePageName.Create('gbk', 936));                       //936
    Add(TBuffCodePageName.Create('cp936', 936));                     //936
    Add(TBuffCodePageName.Create('ms936', 936));                     //936
    Add(TBuffCodePageName.Create('windows-936', 936));               //936
    Add(TBuffCodePageName.Create('chinese', 936));                   //936

    Add(TBuffCodePageName.Create('943', 943));                       //943
    Add(TBuffCodePageName.Create('cp943', 943));                     //943

    Add(TBuffCodePageName.Create('949', 949));                       //949
    Add(TBuffCodePageName.Create('cp949', 949));                     //949
    Add(TBuffCodePageName.Create('unc', 949));                       //949
    Add(TBuffCodePageName.Create('ks_c_5601-1987', 949));            //949
    Add(TBuffCodePageName.Create('iso-ir-149', 949));                //949
    Add(TBuffCodePageName.Create('ks_c_5601-1989', 949));            //949
    Add(TBuffCodePageName.Create('ksc_5601', 949));                  //949
    Add(TBuffCodePageName.Create('ksc5601', 949));                   //949
    Add(TBuffCodePageName.Create('ks_c_5601', 949));                 //949
    Add(TBuffCodePageName.Create('korean', 949));                    //949
    Add(TBuffCodePageName.Create('csksc56011987', 949));             //949
    Add(TBuffCodePageName.Create('5601', 949));                      //949
    Add(TBuffCodePageName.Create('hangeul', 949));                   //949

    Add(TBuffCodePageName.Create('950', 950));                       //950
    Add(TBuffCodePageName.Create('cp950', 950));                     //950
    Add(TBuffCodePageName.Create('big5', 950));                      //950
    Add(TBuffCodePageName.Create('big-5', 950));                     //950
    Add(TBuffCodePageName.Create('csbig5', 950));                    //950
    Add(TBuffCodePageName.Create('cn-big5', 950));                   //950
    Add(TBuffCodePageName.Create('x-x-big5', 950));                  //950
    Add(TBuffCodePageName.Create('ms950', 950));                     //950
    Add(TBuffCodePageName.Create('big5-hkscs', 950));                //950
    Add(TBuffCodePageName.Create('big-five', 950));                  //950
    Add(TBuffCodePageName.Create('bigfive', 950));                   //950

    Add(TBuffCodePageName.Create('utf-16le', 1200));                 //1200
    Add(TBuffCodePageName.Create('utf16le', 1200));                  //1200
    Add(TBuffCodePageName.Create('unicode', 1200));                  //1200
    Add(TBuffCodePageName.Create('utf-16', 1200));                   //1200

    Add(TBuffCodePageName.Create('utf-16be', 1201));                 //1201
    Add(TBuffCodePageName.Create('utf16be', 1201));                  //1201
    Add(TBuffCodePageName.Create('unicodefffe', 1201));              //1201

    Add(TBuffCodePageName.Create('1250', 1250));                     //1250
    Add(TBuffCodePageName.Create('x-cp1250', 1250));                 //1250
    Add(TBuffCodePageName.Create('cp-1250', 1250));                  //1250
    Add(TBuffCodePageName.Create('cp1250', 1250));                   //1250
    Add(TBuffCodePageName.Create('ms-ee', 1250));                    //1250
    Add(TBuffCodePageName.Create('windows-1250', 1250));             //1250
    Add(TBuffCodePageName.Create('easteurope', 1250));               //1250

    Add(TBuffCodePageName.Create('1251', 1251));                     //1251
    Add(TBuffCodePageName.Create('x-cp1251', 1251));                 //1251
    Add(TBuffCodePageName.Create('cp-1251', 1251));                  //1251
    Add(TBuffCodePageName.Create('cp1251', 1251));                   //1251
    Add(TBuffCodePageName.Create('ms-cyr', 1251));                   //1251
    Add(TBuffCodePageName.Create('ms-cyrl', 1251));                  //1251
    Add(TBuffCodePageName.Create('win-1251', 1251));                 //1251
    Add(TBuffCodePageName.Create('win1251', 1251));                  //1251
    Add(TBuffCodePageName.Create('windows-1251', 1251));             //1251
    Add(TBuffCodePageName.Create('russian', 1251));                  //1251

    Add(TBuffCodePageName.Create('1252', 1252));                     //1252
    Add(TBuffCodePageName.Create('windows-1252', 1252));             //1252
    Add(TBuffCodePageName.Create('cp1252', 1252));                   //1252
    Add(TBuffCodePageName.Create('cp-1252', 1252));                  //1252
    Add(TBuffCodePageName.Create('x-ansi', 1252));                   //1252
    Add(TBuffCodePageName.Create('ms-ansi', 1252));                  //1252
    Add(TBuffCodePageName.Create('ansi', 1252));                     //1252

    Add(TBuffCodePageName.Create('1253', 1253));                     //1253
    Add(TBuffCodePageName.Create('windows-1253', 1253));             //1253
    Add(TBuffCodePageName.Create('cp-1253', 1253));                  //1253
    Add(TBuffCodePageName.Create('cp1253', 1253));                   //1253
    Add(TBuffCodePageName.Create('ms-greek', 1253));                 //1253

    Add(TBuffCodePageName.Create('1254', 1254));                     //1254
    Add(TBuffCodePageName.Create('windows-1254', 1254));             //1254
    Add(TBuffCodePageName.Create('cp-1254', 1254));                  //1254
    Add(TBuffCodePageName.Create('cp1254', 1254));                   //1254
    Add(TBuffCodePageName.Create('ms-turk', 1254));                  //1254
    Add(TBuffCodePageName.Create('turkish', 1254));                  //1254

    Add(TBuffCodePageName.Create('1255', 1255));                     //1255
    Add(TBuffCodePageName.Create('windows-1255', 1255));             //1255
    Add(TBuffCodePageName.Create('cp-1255', 1255));                  //1255
    Add(TBuffCodePageName.Create('cp1255', 1255));                   //1255
    Add(TBuffCodePageName.Create('ms-hebr', 1255));                  //1255

    Add(TBuffCodePageName.Create('1256', 1256));                     //1256
    Add(TBuffCodePageName.Create('windows-1256', 1256));             //1256
    Add(TBuffCodePageName.Create('cp1256', 1256));                   //1256
    Add(TBuffCodePageName.Create('cp-1256', 1256));                  //1256
    Add(TBuffCodePageName.Create('ms-arab', 1256));                  //1256

    Add(TBuffCodePageName.Create('1257', 1257));                     //1257
    Add(TBuffCodePageName.Create('windows-1257', 1257));             //1257
    Add(TBuffCodePageName.Create('cp-1257', 1257));                  //1257
    Add(TBuffCodePageName.Create('cp1257', 1257));                   //1257
    Add(TBuffCodePageName.Create('winbaltrim', 1257));               //1257

    Add(TBuffCodePageName.Create('1258', 1258));                     //1258
    Add(TBuffCodePageName.Create('windows-1258', 1258));             //1258
    Add(TBuffCodePageName.Create('cp-1258', 1258));                  //1258
    Add(TBuffCodePageName.Create('cp1258', 1258));                   //1258
    Add(TBuffCodePageName.Create('vietnamese', 1258));               //1258

    Add(TBuffCodePageName.Create('johab', 1361));                    //1361
    Add(TBuffCodePageName.Create('cp1361', 1361));                   //1361

    Add(TBuffCodePageName.Create('macintosh', 10000));               //10000
    Add(TBuffCodePageName.Create('csmacintosh', 10000));             //10000
    Add(TBuffCodePageName.Create('mac', 10000));                     //10000
    Add(TBuffCodePageName.Create('macroman', 10000));                //10000

    Add(TBuffCodePageName.Create('x-mac-arabic', 10004));            //10004

    Add(TBuffCodePageName.Create('x-mac-hebrew', 10005));            //10005

    Add(TBuffCodePageName.Create('x-mac-greek', 10006));             //10006

    Add(TBuffCodePageName.Create('x-mac-cyrillic', 10007));          //10007
    Add(TBuffCodePageName.Create('maccyrillic', 10007));             //10007

    Add(TBuffCodePageName.Create('x-mac-romanian', 10010));          //10010

    Add(TBuffCodePageName.Create('x-mac-ukrainian', 10017));         //10017

    Add(TBuffCodePageName.Create('x-mac-thai', 10021));              //10021

    Add(TBuffCodePageName.Create('x-mac-ce', 10029));                //10029
    Add(TBuffCodePageName.Create('cmac', 10029));                    //10029
    Add(TBuffCodePageName.Create('macce', 10029));                   //10029
    Add(TBuffCodePageName.Create('maccentraleurope', 10029));        //10029

    Add(TBuffCodePageName.Create('x-mac-icelandic', 10079));         //10079

    Add(TBuffCodePageName.Create('x-mac-turkish', 10081));           //10081

    Add(TBuffCodePageName.Create('x-mac-croatian', 10082));          //10082

    Add(TBuffCodePageName.Create('us-ascii', 20127));                //20127
    Add(TBuffCodePageName.Create('ansi_x3.4-1968', 20127));          //20127
    Add(TBuffCodePageName.Create('iso-ir-6', 20127));                //20127
    Add(TBuffCodePageName.Create('ansi_x3.4-1986', 20127));          //20127
    Add(TBuffCodePageName.Create('iso_646.irv:1991', 20127));        //20127
    Add(TBuffCodePageName.Create('ascii', 20127));                   //20127
    Add(TBuffCodePageName.Create('iso646-us', 20127));               //20127
    Add(TBuffCodePageName.Create('us', 20127));                      //20127
    Add(TBuffCodePageName.Create('ibm367', 20127));                  //20127
    Add(TBuffCodePageName.Create('cp367', 20127));                   //20127
    Add(TBuffCodePageName.Create('csascii', 20127));                 //20127
    Add(TBuffCodePageName.Create('iso-ir-6us', 20127));              //20127

    Add(TBuffCodePageName.Create('koi8-r', 20866));                  //20866
    Add(TBuffCodePageName.Create('cskoi8r', 20866));                 //20866
    Add(TBuffCodePageName.Create('koi', 20866));                     //20866
    Add(TBuffCodePageName.Create('koi8', 20866));                    //20866
    Add(TBuffCodePageName.Create('koi8r', 20866));                   //20866

    Add(TBuffCodePageName.Create('koi8-u', 21866));                  //21866
    Add(TBuffCodePageName.Create('cskoi8u', 21866));                 //21866
    Add(TBuffCodePageName.Create('koi8u', 21866));                   //21866
    Add(TBuffCodePageName.Create('koi8-ru', 21866));                 //21866

    Add(TBuffCodePageName.Create('iso-8859-1', 28591));              //28591
    Add(TBuffCodePageName.Create('cp819', 28591));                   //28591
    Add(TBuffCodePageName.Create('ibm819', 28591));                  //28591
    Add(TBuffCodePageName.Create('latin1', 28591));                  //28591
    Add(TBuffCodePageName.Create('latin-1', 28591));                 //28591
    Add(TBuffCodePageName.Create('iso_8859-1:1987', 28591));         //28591
    Add(TBuffCodePageName.Create('iso-ir-100', 28591));              //28591
    Add(TBuffCodePageName.Create('iso_8859-1', 28591));              //28591
    Add(TBuffCodePageName.Create('l1', 28591));                      //28591
    Add(TBuffCodePageName.Create('csisolatin1', 28591));             //28591
    Add(TBuffCodePageName.Create('iso8859-1', 28591));               //28591
    Add(TBuffCodePageName.Create('iso 8859-1', 28591));              //28591

    Add(TBuffCodePageName.Create('iso-8859-2', 28592));              //28592
    Add(TBuffCodePageName.Create('iso_8859-2:1987', 28592));         //28592
    Add(TBuffCodePageName.Create('iso-ir-101', 28592));              //28592
    Add(TBuffCodePageName.Create('iso_8859-2', 28592));              //28592
    Add(TBuffCodePageName.Create('latin2', 28592));                  //28592
    Add(TBuffCodePageName.Create('latin-2', 28592));                 //28592
    Add(TBuffCodePageName.Create('l2', 28592));                      //28592
    Add(TBuffCodePageName.Create('csisolatin2', 28592));             //28592
    Add(TBuffCodePageName.Create('iso8859-2', 28592));               //28592
    Add(TBuffCodePageName.Create('iso 8859-2', 28592));              //28592

    Add(TBuffCodePageName.Create('iso-8859-3', 28593));              //28593
    Add(TBuffCodePageName.Create('iso_8859-3:1988', 28593));         //28593
    Add(TBuffCodePageName.Create('iso-ir-109', 28593));              //28593
    Add(TBuffCodePageName.Create('iso_8859-3', 28593));              //28593
    Add(TBuffCodePageName.Create('latin3', 28593));                  //28593
    Add(TBuffCodePageName.Create('latin-3', 28593));                 //28593
    Add(TBuffCodePageName.Create('l3', 28593));                      //28593
    Add(TBuffCodePageName.Create('csisolatin3', 28593));             //28593
    Add(TBuffCodePageName.Create('iso 8859-3', 28593));              //28593
    Add(TBuffCodePageName.Create('iso8859-3', 28593));               //28593

    Add(TBuffCodePageName.Create('iso-8859-4', 28594));              //28594
    Add(TBuffCodePageName.Create('iso_8859-4:1988', 28594));         //28594
    Add(TBuffCodePageName.Create('iso-ir-110', 28594));              //28594
    Add(TBuffCodePageName.Create('iso_8859-4', 28594));              //28594
    Add(TBuffCodePageName.Create('latin4', 28594));                  //28594
    Add(TBuffCodePageName.Create('latin-4', 28594));                 //28594
    Add(TBuffCodePageName.Create('l4', 28594));                      //28594
    Add(TBuffCodePageName.Create('csisolatin4', 28594));             //28594
    Add(TBuffCodePageName.Create('iso 8859-4', 28594));              //28594
    Add(TBuffCodePageName.Create('iso8859-4', 28594));               //28594

    Add(TBuffCodePageName.Create('iso-8859-5', 28595));              //28595
    Add(TBuffCodePageName.Create('iso_8859-5:1988', 28595));         //28595
    Add(TBuffCodePageName.Create('iso-ir-144', 28595));              //28595
    Add(TBuffCodePageName.Create('iso_8859-5', 28595));              //28595
    Add(TBuffCodePageName.Create('cyrillic', 28595));                //28595
    Add(TBuffCodePageName.Create('csisolatincyrillic', 28595));      //28595
    Add(TBuffCodePageName.Create('l5', 28595));                      //28595
    Add(TBuffCodePageName.Create('iso 8859-5', 28595));              //28595
    Add(TBuffCodePageName.Create('iso8859-5', 28595));               //28595

    Add(TBuffCodePageName.Create('iso-8859-6', 28596));              //28596
    Add(TBuffCodePageName.Create('iso_8859-6:1987', 28596));         //28596
    Add(TBuffCodePageName.Create('arabic', 28596));                  //28596
    Add(TBuffCodePageName.Create('csisolatinarabic', 28596));        //28596
    Add(TBuffCodePageName.Create('ecma-114', 28596));                //28596
    Add(TBuffCodePageName.Create('iso_8859-6', 28596));              //28596
    Add(TBuffCodePageName.Create('iso-ir-127', 28596));              //28596
    Add(TBuffCodePageName.Create('iso 8859-6', 28596));              //28596
    Add(TBuffCodePageName.Create('iso8859-6', 28596));               //28596

    Add(TBuffCodePageName.Create('iso-8859-7', 28597));              //28597
    Add(TBuffCodePageName.Create('iso_8859-7:1987', 28597));         //28597
    Add(TBuffCodePageName.Create('iso-ir-126', 28597));              //28597
    Add(TBuffCodePageName.Create('iso_8859-7', 28597));              //28597
    Add(TBuffCodePageName.Create('elot_928', 28597));                //28597
    Add(TBuffCodePageName.Create('ecma-118', 28597));                //28597
    Add(TBuffCodePageName.Create('greek', 28597));                   //28597
    Add(TBuffCodePageName.Create('greek8', 28597));                  //28597
    Add(TBuffCodePageName.Create('csisolatingreek', 28597));         //28597
    Add(TBuffCodePageName.Create('iso 8859-7', 28597));              //28597
    Add(TBuffCodePageName.Create('iso8859-7', 28597));               //28597

    Add(TBuffCodePageName.Create('iso-8859-8', 28598));              //28598
    Add(TBuffCodePageName.Create('iso_8859-8:1988', 28598));         //28598
    Add(TBuffCodePageName.Create('iso-ir-138', 28598));              //28598
    Add(TBuffCodePageName.Create('iso_8859-8', 28598));              //28598
    Add(TBuffCodePageName.Create('hebrew', 28598));                  //28598
    Add(TBuffCodePageName.Create('csisolatinhebrew', 28598));        //28598
    Add(TBuffCodePageName.Create('visual', 28598));                  //28598
    Add(TBuffCodePageName.Create('iso 8859-8', 28598));              //28598
    Add(TBuffCodePageName.Create('iso8859-8', 28598));               //28598

    Add(TBuffCodePageName.Create('iso-8859-9', 28599));              //28599
    Add(TBuffCodePageName.Create('iso_8859-9:1989', 28599));         //28599
    Add(TBuffCodePageName.Create('iso-ir-148', 28599));              //28599
    Add(TBuffCodePageName.Create('iso_8859-9', 28599));              //28599
    Add(TBuffCodePageName.Create('latin5', 28599));                  //28599
    Add(TBuffCodePageName.Create('latin-5', 28599));                 //28599
    Add(TBuffCodePageName.Create('l5', 28599));                      //28599
    Add(TBuffCodePageName.Create('csisolatin5', 28599));             //28599
    Add(TBuffCodePageName.Create('iso 8859-9', 28599));              //28599
    Add(TBuffCodePageName.Create('iso8859-9', 28599));               //28599

    Add(TBuffCodePageName.Create('iso-8859-10', 28600));             //28600
    Add(TBuffCodePageName.Create('iso-ir-157', 28600));              //28600
    Add(TBuffCodePageName.Create('l6', 28600));                      //28600
    Add(TBuffCodePageName.Create('iso_8859-10:1992', 28600));        //28600
    Add(TBuffCodePageName.Create('csisolatin6', 28600));             //28600
    Add(TBuffCodePageName.Create('iso 8859-10', 28600));             //28600
    Add(TBuffCodePageName.Create('iso8859-10', 28600));              //28600
    Add(TBuffCodePageName.Create('iso_8859-10', 28600));             //28600
    Add(TBuffCodePageName.Create('latin6', 28600));                  //28600
    Add(TBuffCodePageName.Create('latin-6', 28600));                 //28600

    Add(TBuffCodePageName.Create('iso-8859-11', 28601));             //28601
    Add(TBuffCodePageName.Create('iso 8859-11', 28601));             //28601
    Add(TBuffCodePageName.Create('iso8859-11', 28601));              //28601
    Add(TBuffCodePageName.Create('iso_8859-11', 28601));             //28601
    Add(TBuffCodePageName.Create('iso_8859-11:1992', 28601));        //28601
    Add(TBuffCodePageName.Create('tactis', 28601));                  //28601

    Add(TBuffCodePageName.Create('iso-8859-13', 28603));             //28603
    Add(TBuffCodePageName.Create('iso 8859-13', 28603));             //28603
    Add(TBuffCodePageName.Create('iso-ir-179', 28603));              //28603
    Add(TBuffCodePageName.Create('iso8859-13', 28603));              //28603
    Add(TBuffCodePageName.Create('iso_8859-13', 28603));             //28603
    Add(TBuffCodePageName.Create('l7', 28603));                      //28603
    Add(TBuffCodePageName.Create('latin7', 28603));                  //28603
    Add(TBuffCodePageName.Create('latin-7', 28603));                 //28603

    Add(TBuffCodePageName.Create('iso-8859-14', 28604));             //28604
    Add(TBuffCodePageName.Create('iso 8859-14', 28604));             //28604
    Add(TBuffCodePageName.Create('iso-ir-199', 28604));              //28604
    Add(TBuffCodePageName.Create('iso8859-14', 28604));              //28604
    Add(TBuffCodePageName.Create('iso_8859-14', 28604));             //28604
    Add(TBuffCodePageName.Create('iso_8859-14:1998', 28604));        //28604
    Add(TBuffCodePageName.Create('l8', 28604));                      //28604
    Add(TBuffCodePageName.Create('latin8', 28604));                  //28604
    Add(TBuffCodePageName.Create('latin-8', 28604));                 //28604
    Add(TBuffCodePageName.Create('iso-celtic', 28604));              //28604

    Add(TBuffCodePageName.Create('iso-8859-15', 28605));             //28605
    Add(TBuffCodePageName.Create('iso_8859-15', 28605));             //28605
    Add(TBuffCodePageName.Create('latin-9', 28605));                 //28605
    Add(TBuffCodePageName.Create('latin9', 28605));                  //28605
    Add(TBuffCodePageName.Create('l9', 28605));                      //28605
    Add(TBuffCodePageName.Create('iso 8859-15', 28605));             //28605
    Add(TBuffCodePageName.Create('iso-ir-203', 28605));              //28605
    Add(TBuffCodePageName.Create('iso8859-15', 28605));              //28605
    Add(TBuffCodePageName.Create('iso_8859-15:1998', 28605));        //28605
    Add(TBuffCodePageName.Create('latin-0', 28605));                 //28605
    Add(TBuffCodePageName.Create('latin0', 28605));                  //28605

    Add(TBuffCodePageName.Create('iso-8859-16', 28606));             //28606
    Add(TBuffCodePageName.Create('iso 8859-16', 28606));             //28606
    Add(TBuffCodePageName.Create('iso-ir-226', 28606));              //28606
    Add(TBuffCodePageName.Create('iso8859-16', 28606));              //28606
    Add(TBuffCodePageName.Create('iso_8859-16', 28606));             //28606
    Add(TBuffCodePageName.Create('iso_8859-16:2000', 28606));        //28606
    Add(TBuffCodePageName.Create('latin10', 28606));                 //28606

    Add(TBuffCodePageName.Create('iso-2022-jp', 50220));             //50220
    Add(TBuffCodePageName.Create('iso 2022-jp', 50220));             //50220
    Add(TBuffCodePageName.Create('csiso2022jp', 50220));             //50220

    Add(TBuffCodePageName.Create('iso-2022-jp-1', 50222));           //50222
    Add(TBuffCodePageName.Create('_iso-2022-jp$sio', 50222));        //50222

    Add(TBuffCodePageName.Create('iso-2022-kr', 50225));             //50225
    Add(TBuffCodePageName.Create('csiso2022kr', 50225));             //50225

    Add(TBuffCodePageName.Create('iso-2022-cn', 50227));             //50227
    Add(TBuffCodePageName.Create('csiso2022cn', 50227));             //50227
    Add(TBuffCodePageName.Create('x-cp50227', 50227));               //50227

    Add(TBuffCodePageName.Create('euc-jp', 51932));                  //51932
    Add(TBuffCodePageName.Create('eucjp', 51932));                   //51932
    Add(TBuffCodePageName.Create('cseucjp', 51932));                 //51932
    Add(TBuffCodePageName.Create('euc_jp', 51932));                  //51932
    Add(TBuffCodePageName.Create('cseucpkdfmtjapanese', 51932));     //51932
    Add(TBuffCodePageName.Create('x-euc', 51932));                   //51932
    Add(TBuffCodePageName.Create('x-euc-jp', 51932));                //51932

    Add(TBuffCodePageName.Create('cn-gb', 51936));                   //51936
    Add(TBuffCodePageName.Create('euc-cn', 51936));                  //51936
    Add(TBuffCodePageName.Create('euc_cn', 51936));                  //51936
    Add(TBuffCodePageName.Create('euccn', 51936));                   //51936
    Add(TBuffCodePageName.Create('csgb2312', 51936));                //51936
    Add(TBuffCodePageName.Create('gb2312', 51936));                  //51936
    Add(TBuffCodePageName.Create('gb_2312-80', 51936));              //51936
    Add(TBuffCodePageName.Create('x-cp20936', 51936));               //51936
    Add(TBuffCodePageName.Create('csiso58gb231280', 51936));         //51936
    Add(TBuffCodePageName.Create('iso-ir-58', 51936));               //51936

    Add(TBuffCodePageName.Create('euc-kr', 51949));                  //51949
    Add(TBuffCodePageName.Create('euc_kr', 51949));                  //51949
    Add(TBuffCodePageName.Create('cseuckr', 51949));                 //51949
    Add(TBuffCodePageName.Create('euckr', 51949));                   //51949

    Add(TBuffCodePageName.Create('gb18030', 54936));                 //54936
    Add(TBuffCodePageName.Create('gb-18030', 54936));                //54936

    Add(TBuffCodePageName.Create('utf-7', 65000));                   //65000
    Add(TBuffCodePageName.Create('csunicode11utf7', 65000));         //65000
    Add(TBuffCodePageName.Create('unicode-1-1-utf-7', 65000));       //65000
    Add(TBuffCodePageName.Create('x-unicode-2-0-utf-7', 65000));     //65000

    Add(TBuffCodePageName.Create('utf-8', 65001));                   //65001
    Add(TBuffCodePageName.Create('utf8', 65001));                    //65001
    Add(TBuffCodePageName.Create('unicode-1-1-utf-8', 65001));       //65001
    Add(TBuffCodePageName.Create('unicode-2-0-utf-8', 65001));       //65001
    Add(TBuffCodePageName.Create('x-unicode-2-0-utf-8', 65001));     //65001

//    Add(TBuffCodePageName.Create('koi8-t', CP_UNKNOWN));             //-5

    Add(TBuffCodePageName.Create('default', CP_ACP));
// BG, 11.10.2012: too many compiler versions do not declare CP_SYMBOL. Do we actually need it?     
//    Add(TBuffCodePageName.Create('symbol', CP_SYMBOL));
    Add(TBuffCodePageName.Create('oem', CP_OEMCP));

    CaseSensitive := False;
    Sort;
  finally
    EndUpdate;
  end;
end;

//-- BG ---------------------------------------------------------- 08.01.2011 --
destructor TBuffCodePageNameList.Destroy;
var
  I: Integer;
begin
  //BG, 08.01.2011: Issue 48: Memory leaks in HTMLBuffer.pas
  //  free the objects. Unlike Guilleaumes idea to set TStringList.OwnsObjects to True, we
  //  must free the objects on our own, as OwnsObjects is a property added in Delphi 2009.
  BeginUpdate;
  try
    for I := Count - 1 downto 0 do
      Objects[I].Free;
  finally
    EndUpdate;
  end;
  inherited;
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
function TBuffCodePageNameList.GetItem(Index: Integer): TBuffCodePageName;
begin
  Result := TBuffCodePageName(Objects[Index]);
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
function TBuffCodePageNameList.IndexOf(const S: TBuffString): Integer;
var
  LS: TBuffString;
begin
  Result := inherited IndexOf(S);
  if Result = -1 then
  begin
    LS := htLowerCase(S);
    Result := Count - 1;
    while (Result >= 0) and (Pos(Strings[Result], LS) = 0) do
      Dec(Result);
  end;
end;

{ TBuffConverter }

//-- BG ---------------------------------------------------------- 02.10.2012 --
procedure TBuffConverter.Assign(Source: TBuffConverter);
begin
  FPos := Source.FPos;
  FEnd := Source.FEnd;
  FCodePage := Source.FCodePage;
  FInitalCodePage := Source.FInitalCodePage;
end;

//-- BG ---------------------------------------------------------- 02.10.2012 --
function TBuffConverter.AsString: TBuffString;
var
  Bytes, Chars: Integer;
  OldPos: PByte;
begin
  Bytes := FEnd.AnsiChr - FPos.AnsiChr;
  if (FCodePage = CP_UTF16LE) and (FInitalCodePage = CP_UTF16LE) then
  begin
    SetLength(Result, Bytes div 2);
    if Length(Result) > 0 then
      System.Move(FPos.BytePtr^, Result[1], Bytes);
  end
  else
  begin
    Chars := 0;
    if Bytes > 0 then
    begin
      SetLength(Result, Bytes);
      OldPos := FPos.BytePtr;
      try
        repeat
          Inc(Chars);
          Result[Chars] := NextChar;
          if Result[Chars] = #0 then
          begin
            Dec(Chars);
            break;
          end;
        until FPos.AnsiChr >= FEnd.AnsiChr;
      finally
        FPos.BytePtr := OldPos;
      end;
    end;
    SetLength(Result, Chars);
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
constructor TBuffConverter.Create(CurrPtr, EndPtr: TBuffPointer; CodePage, InitalCodePage: TBuffCodePage);
begin
  inherited Create;
  FPos := CurrPtr;
  FEnd := EndPtr;
  FCodePage := CodePage;
  FInitalCodePage := InitalCodePage;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
constructor TBuffConverter.CreateCopy(Source: TBuffConverter);
begin
  inherited Create;
  Assign(Source);
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffConverter.GetNext: Word;

  procedure SwapBytes(var Chr: Word);
  var
    Bytes: array[0..1] of Byte absolute Chr;
    B: Byte;
  begin
    B := Bytes[0];
    Bytes[0]:= Bytes[1];
    Bytes[1] := B;
  end;

begin
  // Issue 50: THTMLViewer.LoadStrings does not seem to work
  // Creating the buffer from wide-/unicodestring may contain incorrectly
  // converted characters of a single byte codepage though. After changing the
  // codepage to the correct single byte codepage, the first or second character
  // read is a #0 (namely the high byte of the previous WideChar).
  //
  // And #0 represents the end of the buffer.
  //
  // Thus always use the initial codepage for reading the appropriate number
  // of bytes of a character in the correct byte order from buffer.
  if FPos.AnsiChr < FEnd.AnsiChr then
    case FInitalCodePage of
      CP_UTF16LE:
        begin
          Result := FPos.WordPtr^;
          Inc(FPos.BytePtr, sizeof(Word));
        end;

      CP_UTF16BE:
        begin
          Result := FPos.WordPtr^;
          Inc(FPos.BytePtr, sizeof(Word));
          SwapBytes(Result);
        end;
    else
      Result := FPos.BytePtr^;
      Inc(FPos.BytePtr, sizeof(Byte));
    end
  else
    Result := 0;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffConverter.NextChar: TBuffChar;
begin
  Result := TBuffChar(GetNext);
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffConverter.PeekChar: TBuffChar;
var
  Clone: TBuffConverter;
  TheClass: TClass;
begin
  TheClass := ClassType;
  Clone := TBuffConverterClass(TheClass).CreateCopy(Self);
  try
    Result := Clone.NextChar;
  finally
    Clone.Free;
  end;
end;

{ TBuffer }

procedure TBuffer.Assign(Source: TBuffer);
begin
  FBuffer := Copy(Source.FBuffer);
  Reset;
  Inc(FStart.AnsiChr, Source.FStart.AnsiChr - PAnsiChar(Source.FBuffer));
  FName := Source.FName;
  FConverter := TBuffConverterClass(Source.FConverter.ClassType).Create(FStart, FEnd, Source.FConverter.FCodePage, Source.FConverter.FInitalCodePage);
  FState := Source.FState;
  FBomLength := Source.FBomLength;
end;

//-- BG ---------------------------------------------------------- 27.12.2010 --
procedure TBuffer.AssignTo(Destin: TObject);
var
  DstMem: TMemoryStream absolute Destin;
begin
  if DstMem is TMemoryStream then
  begin
    DstMem.SetSize(Size);
    System.Move(FBuffer[0], DstMem.Memory^, Size);
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffer.AsString: TBuffString;
begin
  Result := FConverter.AsString;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; Name: TBuffString = '');
begin
  inherited Create;
  SetStream(Stream);
  FName := Name;
  DetectCodePage;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; CodePage: TBuffCodePage; Name: TBuffString = '');
begin
  inherited Create;
  SetStream(Stream);
  FName := Name;
  SetCodePage(CodePage);
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(const Text: TBuffString; const Name: TBuffString = ''; CodePage: TBuffCodePage = CP_UTF16LE);
var
  I: Integer;
begin
  inherited Create;
  I := Length(Text) * sizeof(TBuffChar);
  SetLength(FBuffer, I + sizeof(TBuffChar));
  if I > 0 then
    Move(Text[1], FBuffer[0], I);
  PBuffChar(@FBuffer[I])^ := TBuffChar(0);
  Reset;
  FName := Name;
  SetCodePages(CodePage, CP_UTF16LE);
  Include(FState, bsFixedCodePage);
end;

//-- BG ---------------------------------------------------------- 29.10.2012 --
constructor TBuffer.Create(const Doc: TBuffer);
begin
  inherited Create;
  Assign(Doc);
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(const Text: TBuffString; CodePage: TBuffCodePage; const Name: TBuffString = '');
begin
  Create(Text, Name, CodePage);
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(Text: PByte; ByteCount: Integer; CodePage: TBuffCodePage; const Name: TBuffString = '');
begin
  Create(Text, ByteCount, CodePage, CodePage, Name);
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(Text: PByte; ByteCount: Integer; CodePage, InitialCodePage: TBuffCodePage; const Name: TBuffString = '');
var
  I: Integer;
begin
  inherited Create;
  I := ByteCount;
  SetLength(FBuffer, I + sizeof(TBuffChar));
  Move(Text^, FBuffer[0], I);
  PBuffChar(@FBuffer[I])^ := TBuffChar(0);
  Reset;
  FName := Name;
  SetCodePages(CodePage, InitialCodePage);
end;

//-- BG ---------------------------------------------------------- 06.12.2011 --
class function TBuffer.Convert(const Text: TBuffString; CodePage: TBuffCodePage): TBuffString;
var
  Buffer: TBuffer;
begin
  Buffer := TBuffer.Create(Text, CodePage);
  try
    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 06.12.2011 --
class function TBuffer.Convert(Text: PByte; ByteCount: Integer; CodePage: TBuffCodePage): TBuffString;
var
  Buffer: TBuffer;
begin
  Buffer := TBuffer.Create(Text, ByteCount, CodePage);
  try
    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 06.12.2011 --
class function TBuffer.Convert(Text: PByte; ByteCount: Integer; CodePage, InitialCodePage: TBuffCodePage): TBuffString;
var
  Buffer: TBuffer;
begin
  Buffer := TBuffer.Create(Text, ByteCount, CodePage, InitialCodePage);
  try
    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
destructor TBuffer.Destroy;
begin
  FConverter.Free;
  inherited;
end;

procedure TBuffer.DetectCodePage;
var
  ByteCount: Integer;

  function IsIso2022JP: boolean;
  {look for iso-2022-jp Japanese file}
  const
    EscSequence: array [0..3] of PAnsiChar = (#$1b'$@'#0, #$1b'$B'#0, #$1b'(J'#0, #$1b'(B'#0);
  var
    I, J, K, L: PAnsiChar;
  begin
    Result := False;
    I := StrNPosA(FStart.AnsiChr, EscSequence[0], ByteCount); {look for starting sequence}
    J := StrNPosA(FStart.AnsiChr, EscSequence[1], ByteCount);
    if I < J then
      I := J;
    if I <> nil then
    begin {now look for ending sequence after the start}
      K := StrNPosA(I, EscSequence[2], ByteCount);
      L := StrNPosA(I, EscSequence[3], ByteCount);
      if K < L then
        K := L; {pick a positive value}
      if K <> nil then {start and end sequence found}
        Result := True;
    end;
  end;

begin
  FreeAndNil(FConverter);
  ByteCount := FEnd.AnsiChr - FStart.AnsiChr;
  if ByteCount >= 2 then
  begin
    // a preamble of up to 3 bytes might tell us the file encoding:
    case FStart.WordPtr^ of
      $FEFF:
        begin
          // this is little endian unicode
          Inc(FStart.WordPtr);
          CodePage := CP_UTF16LE;
          Include(FState, bsFixedCodePage);
          // TODO: if followed by $0000, this is UTF-32-LE
          FBomLength := 2;
          Exit;
        end;

      $FFFE:
        begin
          // this is big endian unicode
          // swap the 2 bytes of one char.
          Inc(FStart.WordPtr);
          CodePage := CP_UTF16BE;
          Include(FState, bsFixedCodePage);
          // TODO: if followed by $0000, this is UTF-32-3412
          FBomLength := 2;
          Exit;
        end;

      $BBEF:
        if ByteCount >= 3 then
        begin
          Inc(FStart.WordPtr);
          if FStart.BytePtr^ = $BF then
          begin
            // this is UTF-8
            Inc(FStart.BytePtr);
            CodePage := CP_UTF8;
            Include(FState, bsFixedCodePage);
            FBomLength := 3;
            Exit;
          end;
          Dec(FStart.WordPtr);
        end;

      //TODO:
      {
      $0000:
        if followed by $FFFE, this is UTF-32-BE
        if followed by $FEFF, this is UTF-32-2143
      }
    end;

    if (FStart.WordPtr^ and $00FF) = 0 then
    begin
      if (FStart.WordPtr^ and $FF00) <> 0 then
      begin
        CodePage := CP_UTF16BE;
        Include(FState, bsFixedCodePage);
        Exit;
      end;
    end
    else
    begin
      if (FStart.WordPtr^ and $FF00) = 0 then
      begin
        CodePage := CP_UTF16LE;
        Include(FState, bsFixedCodePage);
        Exit;
      end;
    end;
  end;
  if IsIso2022JP then
  begin
    CodePage := CP_ISO2022JP;
    Include(FState, bsFixedCodePage);
    Exit;
  end;
  // no preamble: this is most probably a 1-byte per character code.
{$ifdef LCL}
  if htExpectsUTF8 then
    CodePage := CP_UTF8
  else
{$endif}
  CodePage := CP_ACP;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffer.GetCodePage: TBuffCodePage;
begin
  if FConverter <> nil then
    Result := FConverter.CodePage
  else
    Result := CP_UNKNOWN;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
function TBuffer.GetPosition: Integer;
begin
  Result := FConverter.FPos.AnsiChr - PAnsiChar(FBuffer);
end;

//-- BG ---------------------------------------------------------- 31.01.2011 --
function TBuffer.GetString(FromIndex, UntilIndex: Integer): TBuffString;
var
  OldPos, I: Integer;
begin
  SetLength(Result, UntilIndex - FromIndex);
  if Length(Result) > 0 then
  begin
    OldPos := Position;
    try
      I := 0;
      Position := FromIndex;
      while Position < UntilIndex do
      begin
        Inc(I);
        Result[I] := NextChar;
      end;
      SetLength(Result, I);
    finally
      Position := OldPos;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffer.NextChar: TBuffChar;
begin
  Result := FConverter.NextChar;
end;

//-- BG ---------------------------------------------------------- 26.09.2012 --
function TBuffer.PeekChar: TBuffChar;
begin
  Result := FConverter.PeekChar;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.Reset;
begin
  FStart.AnsiChr := PAnsiChar(FBuffer);
  FEnd.AnsiChr := FStart.AnsiChr + Size;
end;

//-- BG --------------------------------------------------------- 11.11.2011 --
procedure TBuffer.SetCodePage(Value: TBuffCodePage);
begin
  if bsFixedCodePage in FState then
    exit;

  if FConverter = nil then
    SetCodePages(Value, Value)
  else
    SetCodePages(Value, FConverter.FInitalCodePage);
end;

//-- BG ---------------------------------------------------------- 13.10.2012 --
procedure TBuffer.SetCodePages(Value, Initial: TBuffCodePage);
var
  Index: Integer;
  OldConverter: TBuffConverter;
  CodePageInfos: TBuffConvInfoList;
begin
  CodePageInfos := GetConvInfoList;
  if CodePageInfos <> nil then
  begin
    if FConverter = nil then
    begin
      // setting CodePage for the first time:
      Index := CodePageInfos.Find(Value);
      if Index < 0 then
        Index := CodePageInfos.Find(CP_UNKNOWN);
      FConverter := CodePageInfos[Index].Converter.Create(FStart, FEnd, Value, Initial)
    end
    else
    begin
      // setting CodePage again:
      if FConverter.CodePage = Value then
        // same CodePage again. Nothing to do.
        exit;

      // Change the CodePage. Keep common internal states including InitialCodePage.
      OldConverter := FConverter;
      OldConverter.FCodePage := Value; // set new code page to old converter. CreateCopy will set it to the new converter.
      OldConverter.FInitalCodePage := Initial;
      Index := CodePageInfos.Find(Value);
      if Index < 0 then
        Index := CodePageInfos.Find(CP_UNKNOWN);
      FConverter := CodePageInfos[Index].Converter.CreateCopy(OldConverter);
      OldConverter.Free;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.SetPosition(const Value: Integer);
begin
  FConverter.FPos.AnsiChr := PAnsiChar(FBuffer) + Value;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.SetStream(Stream: TStream);
var
  I: Integer;
begin
  I := Stream.Size - Stream.Position;
  SetLength(FBuffer, I + sizeof(TBuffChar));
  //BG, 25.02.2011: Issue 74: Range check error when creating a buffer for the empty stream
  //  Do not read from empty/exhausted stream:
  if I > 0 then
    Stream.Read(FBuffer[0], I);
  PBuffChar(@FBuffer[I])^ := TBuffChar(0);
  Reset;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
function TBuffer.Size: Integer;
begin
  Result := Length(FBuffer) - sizeof(TBuffChar);
end;

initialization
finalization
// GDG
  VFinalizing := True;
  FreeAndNil(VCodePagesByName);
  FreeAndNil(VConvertersByCodePage);
end.


