{
Version   12
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
  Math,
  //
  HtmlCaches,
  HtmlGlobals;

const
{ Charsets defined in unit Windows:
  ANSI_CHARSET        = 0;
  DEFAULT_CHARSET     = 1;
  SYMBOL_CHARSET      = 2;
  SHIFTJIS_CHARSET    = 128;
  HANGEUL_CHARSET     = 129;
  JOHAB_CHARSET       = 130;
  GB2312_CHARSET      = 134;
  CHINESEBIG5_CHARSET = 136;
  GREEK_CHARSET       = 161;
  TURKISH_CHARSET     = 162;
  VIETNAMESE_CHARSET  = 163;
  HEBREW_CHARSET      = 177;
  ARABIC_CHARSET      = 178;
  RUSSIAN_CHARSET     = 204;
  THAI_CHARSET        = 222;
  EASTEUROPE_CHARSET  = 238;
  OEM_CHARSET         = 255;
}

  // some more charset constants
  UNKNOWN_CHARSET = -1;

  // some more codepage constants
  CP_UNKNOWN = -1;
  CP_UTF16LE = 1200;
  CP_UTF16BE = 1201;
  CP_ISO2022JP = 50220;
//  CP_EUCJP = -6;

type
  EConversionError = class(Exception);

  TBuffCodePage = LongInt;
  TBuffCharSet = SmallInt;  // slightly more than a TFontCharset.

  TBuffChar = ThtChar;      // the type of a wide char
  PBuffChar = PhtChar;
  TBuffString = ThtString;  // the type of a wide string

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

  TBuffArray = array of Byte;
  TBuffArray4 = array [0..3] of Byte;

  // BG, 11.11.2011: Issue 93: LoadStrings failure
  TBufferState = set of (
    bsFixedCodePage // ignore writing to property CodePage.
                    // Helps to ignore <META http-equiv="Content-Type" ...>,
                    // if buffer is an already converted TBuffString.
  );

  // BG, 17.12.2010: helps converting any kind of stream to WideChars.
  TBuffer = class(ThtCachable)
  private
    FBuffer: TBuffArray;
    FStart: TBuffPointer;
    FEnd: TBuffPointer;
    FName: TBuffString;
    FConverter: TBuffConverter;

    FState: TBufferState;

// BG, 26.09.2012: deprecated
//    FCharSet: TBuffCharSet;
//    FJis: TBuffJisState;
//    FCodePage: TBuffCodePage;
//    FInitalCodePage: TBuffCodePage;
//    function GetNext: Word; {$ifdef UseInline} inline; {$endif}
//    function GetNextEucAsShiftJis(out Buffer: TBuffArray4): Integer; {$ifdef UseInline} inline; {$endif}
//    function GetNextJisAsShiftJis(out Buffer: TBuffArray4): Integer; {$ifdef UseInline} inline; {$endif}
//    function GetAsShiftJis(j, k: Word; out Buffer: TBuffArray4): Integer; {$ifdef UseInline} inline; {$endif}
// BG, 26.09.2012: end of deprecated

    function GetCodePage: TBuffCodePage;
    function GetPosition: Integer; {$ifdef UseInline} inline; {$endif}
    procedure DetectCodePage;
    procedure Reset;
    procedure SetStream(Stream: TStream); {$ifdef UseInline} inline; {$endif}
    procedure SetPostion(const Value: Integer); {$ifdef UseInline} inline; {$endif}
    procedure SetCodePage(const Value: TBuffCodePage);
  protected
  public
    class function Convert(Text: TBuffString; CodePage: TBuffCodePage): TBuffString;
    constructor Create(Stream: TStream; Name: TBuffString = ''); overload;
    constructor Create(Stream: TStream; CodePage: TBuffCodePage; Name: TBuffString = ''); overload;
    constructor Create(Text: TBuffString; Name: TBuffString = ''); overload;
    procedure AssignTo(Destin: TObject);
    function AsString: TBuffString;
    function NextChar: TBuffChar;
    function PeekChar: TBuffChar;  {$ifdef UseInline} inline; {$endif}
    function Size: Integer;
    function GetString(FromIndex, UntilIndex: Integer): TBuffString;
    property Name: TBuffString read FName;
    property CodePage: TBuffCodePage read GetCodePage write SetCodePage;
    property Position: Integer read GetPosition write SetPostion;
  end;

  ThtBuffer = TBuffer;

  TGetBufferEvent = function(Sender: TObject; const Url: TBuffString): ThtBuffer of object;

//------------------------------------------------------------------------------
// ThtBufferCache is the buffer cache, that holds the above buffers.
//------------------------------------------------------------------------------

  ThtBufferCache = class(ThtCache)
  {a list of buffer filenames and their ThtBuffers}
  public
    function AddObject(const S: ThtString; AObject: ThtBuffer): Integer; reintroduce; {$ifdef UseInline} inline; {$endif}
    function GetBuffer(I: Integer): ThtBuffer; {$ifdef UseInline} inline; {$endif}
  end;

  TBuffCharSetCodePageInfo = class
  private
    FName: string;
    FCodePage: TBuffCodePage;
  public
    constructor Create(Name: string; CodePage: TBuffCodePage);
    property CodePage: TBuffCodePage read FCodePage;
    property Name: string read FName;
  end;

function GetCharSetCodePageInfo(const Name: string): TBuffCharSetCodePageInfo;
//function CharSetToCodePage(CharSet: TBuffCharSet): TBuffCodePage;
function CodePageToCharSet(CodePage: TBuffCodePage): TBuffCharSet;

implementation

uses
  BuffConv;

type
  TBuffCharSetCodePageInfoList = class(TStringList)
  private
    function GetInfo(Index: Integer): TBuffCharSetCodePageInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInfo(Info: TBuffCharSetCodePageInfo);
    function IndexOf(const S: String): Integer; override;
    property Infos[Index: Integer]: TBuffCharSetCodePageInfo read GetInfo; default;
  end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
var
  VCharSetCodePageByName: TBuffCharSetCodePageInfoList;

//-- BG ---------------------------------------------------------- 22.12.2010 --
function GetCharSetCodePageInfo(const Name: string): TBuffCharSetCodePageInfo;
var
  Index: Integer;
begin
  if VCharSetCodePageByName = nil then
    VCharSetCodePageByName := TBuffCharSetCodePageInfoList.Create;
  if not VCharSetCodePageByName.Find(Name, Index) then
    Index := VCharSetCodePageByName.IndexOf(Name);
  if Index >= 0 then
    Result := VCharSetCodePageByName[Index]
  else
    Result := nil;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
function CodePageToCharSet(CodePage: TBuffCodePage): TBuffCharSet;
var
  Index: Integer;
begin
  Index := Max(0, FindCodePageInfo(CodePage));
  Result := GetCodePageInfo(Index).CharSet;

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

{ TBuffCharSetCodePageInfo }

//-- BG ---------------------------------------------------------- 22.12.2010 --
constructor TBuffCharSetCodePageInfo.Create(Name: string;
  //CharSet: TBuffCharSet;
  CodePage: TBuffCodePage);
begin
  inherited Create;
  FName := Name;
  //FCharSet := CharSet;
  //if CodePage <> CP_UNKNOWN then
    FCodePage := CodePage;
  //else
  //  FCodePage := CharSetToCodePage(FCharSet);
end;

{ TBuffCharSetCodePageInfoList }

//-- BG ---------------------------------------------------------- 22.12.2010 --
procedure TBuffCharSetCodePageInfoList.AddInfo(Info: TBuffCharSetCodePageInfo);
begin
  AddObject(Info.FName, Info);
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
constructor TBuffCharSetCodePageInfoList.Create;
//  The official CharSet definitions:
//    http://www.iana.org/assignments/character-sets
//    http://www.iana.org/assignments/ianacharset-mib
begin
  inherited Create;
  BeginUpdate;
  try
    AddInfo(TBuffCharSetCodePageInfo.Create('708', 708));                       //708
    AddInfo(TBuffCharSetCodePageInfo.Create('asmo-708', 708));                  //708

    AddInfo(TBuffCharSetCodePageInfo.Create('737', 737));                       //737
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm737', 737));                    //737
    AddInfo(TBuffCharSetCodePageInfo.Create('cp437', 737));                     //737
    AddInfo(TBuffCharSetCodePageInfo.Create('cspc8codepage437', 737));          //737

    AddInfo(TBuffCharSetCodePageInfo.Create('775', 775));                       //775
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm775', 775));                    //775
    AddInfo(TBuffCharSetCodePageInfo.Create('cp775', 775));                     //775
    AddInfo(TBuffCharSetCodePageInfo.Create('cspc775baltic', 775));             //775

    AddInfo(TBuffCharSetCodePageInfo.Create('850', 850));                       //850
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm850', 850));                    //850
    AddInfo(TBuffCharSetCodePageInfo.Create('cp850', 850));                     //850
    AddInfo(TBuffCharSetCodePageInfo.Create('cspc850multilingual', 850));       //850

    AddInfo(TBuffCharSetCodePageInfo.Create('852', 852));                       //852
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm852', 852));                    //852
    AddInfo(TBuffCharSetCodePageInfo.Create('cp852', 852));                     //852
    AddInfo(TBuffCharSetCodePageInfo.Create('cspcp852', 852));                  //852

    AddInfo(TBuffCharSetCodePageInfo.Create('855', 855));                       //855
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm855', 855));                    //855
    AddInfo(TBuffCharSetCodePageInfo.Create('cp855', 855));                     //855
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm855', 855));                  //855

    AddInfo(TBuffCharSetCodePageInfo.Create('857', 857));                       //857
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm857', 857));                    //857
    AddInfo(TBuffCharSetCodePageInfo.Create('cp857', 857));                     //857
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm857', 857));                  //857

    AddInfo(TBuffCharSetCodePageInfo.Create('858', 858));                       //858
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm00858', 858));                  //858
    AddInfo(TBuffCharSetCodePageInfo.Create('cp858', 858));                     //858
    AddInfo(TBuffCharSetCodePageInfo.Create('ccsid00858', 858));                //858
    AddInfo(TBuffCharSetCodePageInfo.Create('cp00858', 858));                   //858
    AddInfo(TBuffCharSetCodePageInfo.Create('pc-multilingual-850+euro', 858));  //858

    AddInfo(TBuffCharSetCodePageInfo.Create('860', 860));                       //860
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm860', 860));                    //860
    AddInfo(TBuffCharSetCodePageInfo.Create('cp860', 860));                     //860
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm860', 860));                  //860

    AddInfo(TBuffCharSetCodePageInfo.Create('861', 861));                       //861
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm861', 861));                    //861
    AddInfo(TBuffCharSetCodePageInfo.Create('cp861', 861));                     //861
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm861', 861));                  //861
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-is', 861));                     //861

    AddInfo(TBuffCharSetCodePageInfo.Create('862', 862));                       //862
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm862', 862));                    //862
    AddInfo(TBuffCharSetCodePageInfo.Create('cp862', 862));                     //862
    AddInfo(TBuffCharSetCodePageInfo.Create('cspc862latinhebrew', 862));        //862

    AddInfo(TBuffCharSetCodePageInfo.Create('863', 863));                       //863
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm863', 863));                    //863
    AddInfo(TBuffCharSetCodePageInfo.Create('cp863', 863));                     //863
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm863', 863));                  //863

    AddInfo(TBuffCharSetCodePageInfo.Create('864', 864));                       //864
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm864', 864));                    //864
    AddInfo(TBuffCharSetCodePageInfo.Create('cp864', 864));                     //864
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm864', 864));                  //864

    AddInfo(TBuffCharSetCodePageInfo.Create('865', 865));                       //865
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm865', 865));                    //865
    AddInfo(TBuffCharSetCodePageInfo.Create('cp865', 865));                     //865
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm865', 865));                  //865

    AddInfo(TBuffCharSetCodePageInfo.Create('866', 866));                       //866
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm866', 866));                    //866
    AddInfo(TBuffCharSetCodePageInfo.Create('cp866', 866));                     //866
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm866', 866));                  //866

    AddInfo(TBuffCharSetCodePageInfo.Create('869', 869));                       //869
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm869', 869));                    //869
    AddInfo(TBuffCharSetCodePageInfo.Create('cp869', 869));                     //869
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-gr', 869));                     //869
    AddInfo(TBuffCharSetCodePageInfo.Create('csibm869', 869));                  //869

    AddInfo(TBuffCharSetCodePageInfo.Create('874', 874));                       //874
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-874', 874));               //874
    AddInfo(TBuffCharSetCodePageInfo.Create('dos-874', 874));                   //874
    AddInfo(TBuffCharSetCodePageInfo.Create('cp874', 874));                     //874
    AddInfo(TBuffCharSetCodePageInfo.Create('thai', 874));                      //874
    AddInfo(TBuffCharSetCodePageInfo.Create('tis-620', 874));                   //874
    AddInfo(TBuffCharSetCodePageInfo.Create('tis620', 874));                    //874

    AddInfo(TBuffCharSetCodePageInfo.Create('922', 922));                       //922
    AddInfo(TBuffCharSetCodePageInfo.Create('cp922', 922));                     //922

    AddInfo(TBuffCharSetCodePageInfo.Create('932', 932));                       //932
    AddInfo(TBuffCharSetCodePageInfo.Create('ms_kanji', 932));                  //932
    AddInfo(TBuffCharSetCodePageInfo.Create('csshiftjis', 932));                //932
    AddInfo(TBuffCharSetCodePageInfo.Create('cswindows31j', 932));              //932
    AddInfo(TBuffCharSetCodePageInfo.Create('shift-jis', 932));                 //932
    AddInfo(TBuffCharSetCodePageInfo.Create('shift_jis', 932));                 //932
    AddInfo(TBuffCharSetCodePageInfo.Create('shiftjis', 932));                  //932
    AddInfo(TBuffCharSetCodePageInfo.Create('x-ms-cp932', 932));                //932
    AddInfo(TBuffCharSetCodePageInfo.Create('ms932', 932));                     //932
    AddInfo(TBuffCharSetCodePageInfo.Create('cp932', 932));                     //932
    AddInfo(TBuffCharSetCodePageInfo.Create('x-sjis', 932));                    //932

    AddInfo(TBuffCharSetCodePageInfo.Create('936', 936));                       //936
    AddInfo(TBuffCharSetCodePageInfo.Create('gbk', 936));                       //936
    AddInfo(TBuffCharSetCodePageInfo.Create('cp936', 936));                     //936
    AddInfo(TBuffCharSetCodePageInfo.Create('ms936', 936));                     //936
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-936', 936));               //936
    AddInfo(TBuffCharSetCodePageInfo.Create('chinese', 936));                   //936

    AddInfo(TBuffCharSetCodePageInfo.Create('943', 943));                       //943
    AddInfo(TBuffCharSetCodePageInfo.Create('cp943', 943));                     //943

    AddInfo(TBuffCharSetCodePageInfo.Create('949', 949));                       //949
    AddInfo(TBuffCharSetCodePageInfo.Create('cp949', 949));                     //949
    AddInfo(TBuffCharSetCodePageInfo.Create('unc', 949));                       //949
    AddInfo(TBuffCharSetCodePageInfo.Create('ks_c_5601-1987', 949));            //949
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-149', 949));                //949
    AddInfo(TBuffCharSetCodePageInfo.Create('ks_c_5601-1989', 949));            //949
    AddInfo(TBuffCharSetCodePageInfo.Create('ksc_5601', 949));                  //949
    AddInfo(TBuffCharSetCodePageInfo.Create('ksc5601', 949));                   //949
    AddInfo(TBuffCharSetCodePageInfo.Create('ks_c_5601', 949));                 //949
    AddInfo(TBuffCharSetCodePageInfo.Create('korean', 949));                    //949
    AddInfo(TBuffCharSetCodePageInfo.Create('csksc56011987', 949));             //949
    AddInfo(TBuffCharSetCodePageInfo.Create('5601', 949));                      //949
    AddInfo(TBuffCharSetCodePageInfo.Create('hangeul', 949));                   //949

    AddInfo(TBuffCharSetCodePageInfo.Create('950', 950));                       //950
    AddInfo(TBuffCharSetCodePageInfo.Create('cp950', 950));                     //950
    AddInfo(TBuffCharSetCodePageInfo.Create('big5', 950));                      //950
    AddInfo(TBuffCharSetCodePageInfo.Create('big-5', 950));                     //950
    AddInfo(TBuffCharSetCodePageInfo.Create('csbig5', 950));                    //950
    AddInfo(TBuffCharSetCodePageInfo.Create('cn-big5', 950));                   //950
    AddInfo(TBuffCharSetCodePageInfo.Create('x-x-big5', 950));                  //950
    AddInfo(TBuffCharSetCodePageInfo.Create('ms950', 950));                     //950
    AddInfo(TBuffCharSetCodePageInfo.Create('big5-hkscs', 950));                //950
    AddInfo(TBuffCharSetCodePageInfo.Create('big-five', 950));                  //950
    AddInfo(TBuffCharSetCodePageInfo.Create('bigfive', 950));                   //950

    AddInfo(TBuffCharSetCodePageInfo.Create('utf-16le', 1200));                 //1200
    AddInfo(TBuffCharSetCodePageInfo.Create('utf16le', 1200));                  //1200
    AddInfo(TBuffCharSetCodePageInfo.Create('unicode', 1200));                  //1200
    AddInfo(TBuffCharSetCodePageInfo.Create('utf-16', 1200));                   //1200

    AddInfo(TBuffCharSetCodePageInfo.Create('utf-16be', 1201));                 //1201
    AddInfo(TBuffCharSetCodePageInfo.Create('utf16be', 1201));                  //1201
    AddInfo(TBuffCharSetCodePageInfo.Create('unicodefffe', 1201));              //1201

    AddInfo(TBuffCharSetCodePageInfo.Create('1250', 1250));                     //1250
    AddInfo(TBuffCharSetCodePageInfo.Create('x-cp1250', 1250));                 //1250
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1250', 1250));                  //1250
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1250', 1250));                   //1250
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-ee', 1250));                    //1250
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1250', 1250));             //1250
    AddInfo(TBuffCharSetCodePageInfo.Create('easteurope', 1250));               //1250

    AddInfo(TBuffCharSetCodePageInfo.Create('1251', 1251));                     //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('x-cp1251', 1251));                 //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1251', 1251));                  //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1251', 1251));                   //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-cyr', 1251));                   //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-cyrl', 1251));                  //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('win-1251', 1251));                 //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('win1251', 1251));                  //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1251', 1251));             //1251
    AddInfo(TBuffCharSetCodePageInfo.Create('russian', 1251));                  //1251

    AddInfo(TBuffCharSetCodePageInfo.Create('1252', 1252));                     //1252
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1252', 1252));             //1252
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1252', 1252));                   //1252
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1252', 1252));                  //1252
    AddInfo(TBuffCharSetCodePageInfo.Create('x-ansi', 1252));                   //1252
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-ansi', 1252));                  //1252
    AddInfo(TBuffCharSetCodePageInfo.Create('ansi', 1252));                     //1252

    AddInfo(TBuffCharSetCodePageInfo.Create('1253', 1253));                     //1253
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1253', 1253));             //1253
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1253', 1253));                  //1253
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1253', 1253));                   //1253
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-greek', 1253));                 //1253

    AddInfo(TBuffCharSetCodePageInfo.Create('1254', 1254));                     //1254
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1254', 1254));             //1254
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1254', 1254));                  //1254
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1254', 1254));                   //1254
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-turk', 1254));                  //1254
    AddInfo(TBuffCharSetCodePageInfo.Create('turkish', 1254));                  //1254

    AddInfo(TBuffCharSetCodePageInfo.Create('1255', 1255));                     //1255
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1255', 1255));             //1255
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1255', 1255));                  //1255
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1255', 1255));                   //1255
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-hebr', 1255));                  //1255

    AddInfo(TBuffCharSetCodePageInfo.Create('1256', 1256));                     //1256
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1256', 1256));             //1256
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1256', 1256));                   //1256
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1256', 1256));                  //1256
    AddInfo(TBuffCharSetCodePageInfo.Create('ms-arab', 1256));                  //1256

    AddInfo(TBuffCharSetCodePageInfo.Create('1257', 1257));                     //1257
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1257', 1257));             //1257
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1257', 1257));                  //1257
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1257', 1257));                   //1257
    AddInfo(TBuffCharSetCodePageInfo.Create('winbaltrim', 1257));               //1257

    AddInfo(TBuffCharSetCodePageInfo.Create('1258', 1258));                     //1258
    AddInfo(TBuffCharSetCodePageInfo.Create('windows-1258', 1258));             //1258
    AddInfo(TBuffCharSetCodePageInfo.Create('cp-1258', 1258));                  //1258
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1258', 1258));                   //1258
    AddInfo(TBuffCharSetCodePageInfo.Create('vietnamese', 1258));               //1258

    AddInfo(TBuffCharSetCodePageInfo.Create('johab', 1361));                    //1361
    AddInfo(TBuffCharSetCodePageInfo.Create('cp1361', 1361));                   //1361

    AddInfo(TBuffCharSetCodePageInfo.Create('macintosh', 10000));               //10000
    AddInfo(TBuffCharSetCodePageInfo.Create('csmacintosh', 10000));             //10000
    AddInfo(TBuffCharSetCodePageInfo.Create('mac', 10000));                     //10000
    AddInfo(TBuffCharSetCodePageInfo.Create('macroman', 10000));                //10000

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-arabic', 10004));            //10004

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-hebrew', 10005));            //10005

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-greek', 10006));             //10006

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-cyrillic', 10007));          //10007
    AddInfo(TBuffCharSetCodePageInfo.Create('maccyrillic', 10007));             //10007

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-romanian', 10010));          //10010

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-ukrainian', 10017));         //10017

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-thai', 10021));              //10021

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-ce', 10029));                //10029
    AddInfo(TBuffCharSetCodePageInfo.Create('cmac', 10029));                    //10029
    AddInfo(TBuffCharSetCodePageInfo.Create('macce', 10029));                   //10029
    AddInfo(TBuffCharSetCodePageInfo.Create('maccentraleurope', 10029));        //10029

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-icelandic', 10079));         //10079

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-turkish', 10081));           //10081

    AddInfo(TBuffCharSetCodePageInfo.Create('x-mac-croatian', 10082));          //10082

    AddInfo(TBuffCharSetCodePageInfo.Create('us-ascii', 20127));                //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('ansi_x3.4-1968', 20127));          //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-6', 20127));                //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('ansi_x3.4-1986', 20127));          //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_646.irv:1991', 20127));        //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('ascii', 20127));                   //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('iso646-us', 20127));               //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('us', 20127));                      //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm367', 20127));                  //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('cp367', 20127));                   //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('csascii', 20127));                 //20127
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-6us', 20127));              //20127

    AddInfo(TBuffCharSetCodePageInfo.Create('koi8-r', 20866));                  //20866
    AddInfo(TBuffCharSetCodePageInfo.Create('cskoi8r', 20866));                 //20866
    AddInfo(TBuffCharSetCodePageInfo.Create('koi', 20866));                     //20866
    AddInfo(TBuffCharSetCodePageInfo.Create('koi8', 20866));                    //20866
    AddInfo(TBuffCharSetCodePageInfo.Create('koi8r', 20866));                   //20866

    AddInfo(TBuffCharSetCodePageInfo.Create('koi8-u', 21866));                  //21866
    AddInfo(TBuffCharSetCodePageInfo.Create('cskoi8u', 21866));                 //21866
    AddInfo(TBuffCharSetCodePageInfo.Create('koi8u', 21866));                   //21866
    AddInfo(TBuffCharSetCodePageInfo.Create('koi8-ru', 21866));                 //21866

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-1', 28591));              //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('cp819', 28591));                   //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('ibm819', 28591));                  //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('latin1', 28591));                  //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-1', 28591));                 //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-1:1987', 28591));         //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-100', 28591));              //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-1', 28591));              //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('l1', 28591));                      //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatin1', 28591));             //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-1', 28591));               //28591
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-1', 28591));              //28591

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-2', 28592));              //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-2:1987', 28592));         //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-101', 28592));              //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-2', 28592));              //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('latin2', 28592));                  //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-2', 28592));                 //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('l2', 28592));                      //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatin2', 28592));             //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-2', 28592));               //28592
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-2', 28592));              //28592

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-3', 28593));              //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-3:1988', 28593));         //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-109', 28593));              //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-3', 28593));              //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('latin3', 28593));                  //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-3', 28593));                 //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('l3', 28593));                      //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatin3', 28593));             //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-3', 28593));              //28593
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-3', 28593));               //28593

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-4', 28594));              //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-4:1988', 28594));         //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-110', 28594));              //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-4', 28594));              //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('latin4', 28594));                  //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-4', 28594));                 //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('l4', 28594));                      //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatin4', 28594));             //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-4', 28594));              //28594
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-4', 28594));               //28594

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-5', 28595));              //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-5:1988', 28595));         //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-144', 28595));              //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-5', 28595));              //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('cyrillic', 28595));                //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatincyrillic', 28595));      //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('l5', 28595));                      //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-5', 28595));              //28595
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-5', 28595));               //28595

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-6', 28596));              //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-6:1987', 28596));         //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('arabic', 28596));                  //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatinarabic', 28596));        //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('ecma-114', 28596));                //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-6', 28596));              //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-127', 28596));              //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-6', 28596));              //28596
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-6', 28596));               //28596

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-7', 28597));              //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-7:1987', 28597));         //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-126', 28597));              //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-7', 28597));              //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('elot_928', 28597));                //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('ecma-118', 28597));                //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('greek', 28597));                   //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('greek8', 28597));                  //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatingreek', 28597));         //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-7', 28597));              //28597
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-7', 28597));               //28597

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-8', 28598));              //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-8:1988', 28598));         //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-138', 28598));              //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-8', 28598));              //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('hebrew', 28598));                  //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatinhebrew', 28598));        //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('visual', 28598));                  //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-8', 28598));              //28598
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-8', 28598));               //28598

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-9', 28599));              //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-9:1989', 28599));         //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-148', 28599));              //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-9', 28599));              //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('latin5', 28599));                  //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-5', 28599));                 //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('l5', 28599));                      //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatin5', 28599));             //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-9', 28599));              //28599
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-9', 28599));               //28599

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-10', 28600));             //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-157', 28600));              //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('l6', 28600));                      //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-10:1992', 28600));        //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('csisolatin6', 28600));             //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-10', 28600));             //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-10', 28600));              //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-10', 28600));             //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('latin6', 28600));                  //28600
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-6', 28600));                 //28600

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-11', 28601));             //28601
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-11', 28601));             //28601
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-11', 28601));              //28601
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-11', 28601));             //28601
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-11:1992', 28601));        //28601
    AddInfo(TBuffCharSetCodePageInfo.Create('tactis', 28601));                  //28601

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-13', 28603));             //28603
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-13', 28603));             //28603
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-179', 28603));              //28603
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-13', 28603));              //28603
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-13', 28603));             //28603
    AddInfo(TBuffCharSetCodePageInfo.Create('l7', 28603));                      //28603
    AddInfo(TBuffCharSetCodePageInfo.Create('latin7', 28603));                  //28603
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-7', 28603));                 //28603

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-14', 28604));             //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-14', 28604));             //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-199', 28604));              //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-14', 28604));              //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-14', 28604));             //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-14:1998', 28604));        //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('l8', 28604));                      //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('latin8', 28604));                  //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-8', 28604));                 //28604
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-celtic', 28604));              //28604

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-15', 28605));             //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-15', 28605));             //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-9', 28605));                 //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('latin9', 28605));                  //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('l9', 28605));                      //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-15', 28605));             //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-203', 28605));              //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-15', 28605));              //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-15:1998', 28605));        //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('latin-0', 28605));                 //28605
    AddInfo(TBuffCharSetCodePageInfo.Create('latin0', 28605));                  //28605

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-8859-16', 28606));             //28606
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 8859-16', 28606));             //28606
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-226', 28606));              //28606
    AddInfo(TBuffCharSetCodePageInfo.Create('iso8859-16', 28606));              //28606
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-16', 28606));             //28606
    AddInfo(TBuffCharSetCodePageInfo.Create('iso_8859-16:2000', 28606));        //28606
    AddInfo(TBuffCharSetCodePageInfo.Create('latin10', 28606));                 //28606

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-2022-jp', 50220));             //50220
    AddInfo(TBuffCharSetCodePageInfo.Create('iso 2022-jp', 50220));             //50220
    AddInfo(TBuffCharSetCodePageInfo.Create('csiso2022jp', 50220));             //50220

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-2022-jp-1', 50222));           //50222
    AddInfo(TBuffCharSetCodePageInfo.Create('_iso-2022-jp$sio', 50222));        //50222

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-2022-kr', 50225));             //50225
    AddInfo(TBuffCharSetCodePageInfo.Create('csiso2022kr', 50225));             //50225

    AddInfo(TBuffCharSetCodePageInfo.Create('iso-2022-cn', 50227));             //50227
    AddInfo(TBuffCharSetCodePageInfo.Create('csiso2022cn', 50227));             //50227
    AddInfo(TBuffCharSetCodePageInfo.Create('x-cp50227', 50227));               //50227

    AddInfo(TBuffCharSetCodePageInfo.Create('euc-jp', 51932));                  //51932
    AddInfo(TBuffCharSetCodePageInfo.Create('eucjp', 51932));                   //51932
    AddInfo(TBuffCharSetCodePageInfo.Create('cseucjp', 51932));                 //51932
    AddInfo(TBuffCharSetCodePageInfo.Create('euc_jp', 51932));                  //51932
    AddInfo(TBuffCharSetCodePageInfo.Create('cseucpkdfmtjapanese', 51932));     //51932
    AddInfo(TBuffCharSetCodePageInfo.Create('x-euc', 51932));                   //51932
    AddInfo(TBuffCharSetCodePageInfo.Create('x-euc-jp', 51932));                //51932

    AddInfo(TBuffCharSetCodePageInfo.Create('cn-gb', 51936));                   //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('euc-cn', 51936));                  //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('euc_cn', 51936));                  //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('euccn', 51936));                   //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('csgb2312', 51936));                //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('gb2312', 51936));                  //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('gb_2312-80', 51936));              //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('x-cp20936', 51936));               //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('csiso58gb231280', 51936));         //51936
    AddInfo(TBuffCharSetCodePageInfo.Create('iso-ir-58', 51936));               //51936

    AddInfo(TBuffCharSetCodePageInfo.Create('euc-kr', 51949));                  //51949
    AddInfo(TBuffCharSetCodePageInfo.Create('euc_kr', 51949));                  //51949
    AddInfo(TBuffCharSetCodePageInfo.Create('cseuckr', 51949));                 //51949
    AddInfo(TBuffCharSetCodePageInfo.Create('euckr', 51949));                   //51949

    AddInfo(TBuffCharSetCodePageInfo.Create('gb18030', 54936));                 //54936
    AddInfo(TBuffCharSetCodePageInfo.Create('gb-18030', 54936));                //54936

    AddInfo(TBuffCharSetCodePageInfo.Create('utf-7', 65000));                   //65000
    AddInfo(TBuffCharSetCodePageInfo.Create('csunicode11utf7', 65000));         //65000
    AddInfo(TBuffCharSetCodePageInfo.Create('unicode-1-1-utf-7', 65000));       //65000
    AddInfo(TBuffCharSetCodePageInfo.Create('x-unicode-2-0-utf-7', 65000));     //65000

    AddInfo(TBuffCharSetCodePageInfo.Create('utf-8', 65001));                   //65001
    AddInfo(TBuffCharSetCodePageInfo.Create('utf8', 65001));                    //65001
    AddInfo(TBuffCharSetCodePageInfo.Create('unicode-1-1-utf-8', 65001));       //65001
    AddInfo(TBuffCharSetCodePageInfo.Create('unicode-2-0-utf-8', 65001));       //65001
    AddInfo(TBuffCharSetCodePageInfo.Create('x-unicode-2-0-utf-8', 65001));     //65001

//    AddInfo(TBuffCharSetCodePageInfo.Create('koi8-t', CP_UNKNOWN));             //-5

    AddInfo(TBuffCharSetCodePageInfo.Create('default', CP_ACP));
    AddInfo(TBuffCharSetCodePageInfo.Create('symbol', CP_SYMBOL));
    AddInfo(TBuffCharSetCodePageInfo.Create('oem', CP_OEMCP));

    CaseSensitive := False;
    Sort;
  finally
    EndUpdate;
  end;
end;

//-- BG ---------------------------------------------------------- 08.01.2011 --
destructor TBuffCharSetCodePageInfoList.Destroy;
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
function TBuffCharSetCodePageInfoList.GetInfo(Index: Integer): TBuffCharSetCodePageInfo;
begin
  Result := TBuffCharSetCodePageInfo(Objects[Index]);
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
function TBuffCharSetCodePageInfoList.IndexOf(const S: String): Integer;
var
  LS: string;
begin
  Result := inherited IndexOf(S);
  if Result = -1 then
  begin
    LS := LowerCase(S);
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
  if FCodePage = FInitalCodePage then
  begin
    SetLength(Result, Bytes);
    System.Move(Result[1], FPos.BytePtr^, Bytes);
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
          Result[Chars] := NextChar;
          if (Result[Chars] = #0) or (FPos.AnsiChr >= FEnd.AnsiChr) then
            break;
          Inc(Chars);
        until false;
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

(*
//-- BG ---------------------------------------------------------- 14.12.2010 --
function TBuffer.AsString: TBuffString;

  procedure CharByChar;
  var
    I: Integer;
  begin
    I := 1;
    repeat
      Result[I] := NextChar;
      if Result[I] = #0 then
        break;
      Inc(I);
    until false;
    SetLength(Result, I);
  end;

var
  I, J: Integer;
begin
  I := FEnd.AnsiChr - FPos.AnsiChr;
  SetLength(Result, I);
  if I > 0 then
  begin
    case FCodePage of
      CP_ISO2022JP,
      CP_EUCJP,
      CP_UTF16LE,
      CP_UTF16BE,
      CP_UTF8:
        CharByChar;

      CP_ACP,
      CP_OEMCP,
      CP_MACCP:
        if FCodePage <> FInitalCodePage then
          CharByChar
        else
        begin
          J := MultiByteToWideChar(CP_UTF8, 0, FPos.AnsiChr, I, PWideChar(Result), I);
          //BG, 23.12.2010: Some single byte chars may have been converted accidently as UTF-8.
          // As a real UTF-8 source becomes significantly shorter, if number of chars shrinks
          // less than 10 percent, do not assume it is UTF-8.
          if (J > 0) and (I - J > I div 25) then
          begin
            FCodePage := CP_UTF8;
            FCharSet := DEFAULT_CHARSET;
          end
          else
            J := MultiByteToWideChar(FCodePage, 0, FPos.AnsiChr, I, PWideChar(Result), I);
          SetLength(Result, J);
          Inc(FPos.AnsiChr, I);
        end;
    else
      if FCodePage <> FInitalCodePage then
        CharByChar
      else
      begin
        J := MultiByteToWideChar(FCodePage, 0, FPos.AnsiChr, I, PWideChar(Result), I);
        if J = 0 then
        begin
          //BG, 14.12.2010: maybe an UTF8 without preamble?
          J := MultiByteToWideChar(CP_UTF8, 0, FPos.AnsiChr, I, PWideChar(Result), I);
          if J > 0 then
          begin
            FCodePage := CP_UTF8;
            FCharSet := DEFAULT_CHARSET;
          end;
        end;
        SetLength(Result, J);
        Inc(FPos.AnsiChr, I);
      end;
    end
  end
  else
    Result := '';
end;
*)

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; Name: TBuffString = '');
begin
  inherited Create;
  SetStream(Stream);
  FName := Name;
  DetectCodePage;
end;

////-- BG ---------------------------------------------------------- 14.12.2010 --
//constructor TBuffer.Create(Stream: TStream; CharSet: TBuffCharSet; Name: TBuffString = '');
//begin
//  inherited Create;
//  SetStream(Stream);
//  FName := Name;
//  Self.CharSet := CharSet;
//  SetCodePage(CharSetToCodePage(CharSet));
//end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; CodePage: TBuffCodePage; Name: TBuffString = '');
begin
  inherited Create;
  SetStream(Stream);
  FName := Name;
  SetCodePage(CodePage);
//  FCharSet := CodePageToCharSet(Self.CodePage);
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(Text: TBuffString; Name: TBuffString = '');
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
  SetCodePage(CP_UTF16LE);
  Include(FState, bsFixedCodePage);
//  FCharSet := UNKNOWN_CHARSET;
end;

////-- BG ---------------------------------------------------------- 17.12.2010 --
//constructor TBuffer.Create(Text: AnsiString; CharSet: TBuffCharSet; Name: TBuffString = '');
//var
//  I: Integer;
//begin
//  inherited Create;
//  I := Length(Text);
//  SetLength(FBuffer, I + sizeof(TBuffChar));
//  Move(Text[1], FBuffer[0], I);
//  PBuffChar(@FBuffer[I])^ := TBuffChar(0);
//  Reset;
//  FName := Name;
//  Self.CharSet := CharSet;
//  SetCodePage(CharSetToCodePage(FCharSet));
//end;

//-- BG ---------------------------------------------------------- 06.12.2011 --
class function TBuffer.Convert(Text: TBuffString; CodePage: TBuffCodePage): TBuffString;
var
  Buffer: TBuffer;
begin
  Buffer := TBuffer.Create(Text);
  try
    Buffer.SetCodePage(CodePage);
    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
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
  end;
  if IsIso2022JP then
  begin
    CodePage := CP_ISO2022JP;
    Include(FState, bsFixedCodePage);
    exit;
  end;
  // no preamble: this is most probably a 1-byte per character code.
  CodePage := CP_ACP;
end;

(*
//------------------------------------------------------------------------------
function TBuffer.GetNextEucAsShiftJis(out Buffer: TBuffArray4): Integer;
var
  Chr: Word;
begin
  Chr := GetNext;
  if Chr <= $A0 then
  begin
    Buffer[0] := Chr;
    Result := 1;
  end
  else
    Result := GetAsShiftJis(Chr, GetNext, Buffer);
end;
*)

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

(*
//-- BG ---------------------------------------------------------- 14.12.2010 --
function TBuffer.NextChar: TBuffChar;
var
  Buffer: Word;
  Chr: Cardinal;
  Buffer2: TBuffArray4;
  Len: Integer;
begin
  //Buffer2 := nil; // valium for the compiler
  case FCodePage of
    CP_UTF16LE:
      Result := TBuffChar(GetNext);

    CP_UTF16BE:
      Result := TBuffChar(GetNext);

    CP_UTF8:
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

    CP_ISO2022JP:
      if FPos.AnsiChr < FEnd.AnsiChr then
      begin
        Len := GetNextJisAsShiftJis(Buffer2);
        MultiByteToWideChar(932, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
      end
      else
        Result := TBuffChar(0);

    CP_EUCJP:
      if FPos.AnsiChr < FEnd.AnsiChr then
      begin
        Len := GetNextEucAsShiftJis(Buffer2);
        MultiByteToWideChar(932, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
      end
      else
        Result := TBuffChar(0);

    932: // shift jis
      if FPos.AnsiChr < FEnd.AnsiChr then
      begin
        Len := 1;
        Buffer2[0] := GetNext;
        case Buffer2[0] of
          $81..$9f, $E0..$FC:
            if FPos.AnsiChr < FEnd.AnsiChr then
            begin
              Buffer2[1] := GetNext;
              Len := 2;
            end;
        end;
        MultiByteToWideChar(FCodePage, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
      end
      else
        Result := TBuffChar(0);

    936, // Simplified Chinese GBK
    949: // Korean
      if FPos.AnsiChr < FEnd.AnsiChr then
      begin
        Len := 1;
        Buffer2[0] := GetNext;
        case Buffer2[0] of
          $81..$FE:
            if FPos.AnsiChr < FEnd.AnsiChr then
            begin
              Buffer2[1] := GetNext;
              Len := 2;
            end;
        end;
        MultiByteToWideChar(FCodePage, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
      end
      else
        Result := TBuffChar(0);

    950: // BIG5
      if FPos.AnsiChr < FEnd.AnsiChr then
      begin
        Len := 1;
        Buffer2[0] := GetNext;
        case Buffer2[0] of
          $A1..$F9:
            if FPos.AnsiChr < FEnd.AnsiChr then
            begin
              Buffer2[1] := GetNext;
              Len := 2;
            end;
        end;
        MultiByteToWideChar(FCodePage, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
      end
      else
        Result := TBuffChar(0);

  else
    Buffer := GetNext;
    if Buffer <> 0 then
      MultiByteToWideChar(FCodePage, 0, PAnsiChar(@Buffer), 1, @Result, 1)
    else
      Result := TBuffChar(0);
  end;
end;
*)

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.Reset;
begin
  FStart.AnsiChr := PAnsiChar(FBuffer);
  FEnd.AnsiChr := FStart.AnsiChr + Size;
end;

////-- BG --------------------------------------------------------- 11.11.2011 --
//procedure TBuffer.SetCharSet(const Value: TBuffCharSet);
//begin
//  FCharSet := Value;
//end;

//-- BG --------------------------------------------------------- 11.11.2011 --
procedure TBuffer.SetCodePage(const Value: TBuffCodePage);
var
  Index: Integer;
  OldConverter: TBuffConverter;
begin
  if bsFixedCodePage in FState then
    exit;
  if FConverter = nil then
  begin
    // setting CodePage for the first time:
    Index := Max(0, FindCodePageInfo(Value)); // Index = 0: CP_UNKNOWN
    FConverter := GetCodePageInfo(Index).Converter.Create(FStart, FEnd, Value, Value)
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
    Index := Max(0, FindCodePageInfo(Value)); // Index = 0: CP_UNKNOWN
    FConverter := GetCodePageInfo(Index).Converter.CreateCopy(OldConverter);
    OldConverter.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.SetPostion(const Value: Integer);
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

{ ThtBufferCache }

//-- BG ---------------------------------------------------------- 30.04.2011 --
function ThtBufferCache.AddObject(const S: ThtString; AObject: ThtBuffer): Integer;
begin
  Result := inherited AddObject(S, AObject);
end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
function ThtBufferCache.GetBuffer(I: Integer): ThtBuffer;
begin
  Result := ThtBuffer(GetCachable(I));
end;

// GDG
initialization
finalization
  FreeAndNil(VCharSetCodePageByName);
end.


