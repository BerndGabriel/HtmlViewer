{
HtmlViewer Version 11
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
  HtmlGlobals;

const
  // more char sets
  UNKNOWN_CHARSET = -1;
  EUCJP_CHARSET = 30; {unused number}
  EASTEUROPE2_CHARSET = 31;

  // more code pages
  CP_UNKNOWN = -1;
  CP_UTF16LE = -3;
  CP_UTF16BE = -4;
  CP_ISO2022JP = -5;
  CP_EUCJP = -6;

type
  EConversionError = class(Exception);

  TBuffCodePage = LongInt;
  TBuffCharSet = SmallInt;  // slightly more than a TFontCharset.

  TBuffChar = ThtChar;     // the type of a wide char
  PBuffChar = PhtChar;
  TBuffString = ThtString; // the type of a wide string

  TBuffPointer = record
    case Integer of
      0: (BytePtr: PByte;);
      1: (WordPtr: PWord;);
      2: (AnsiChr: PAnsiChar;);
      3: (WideChr: PWideChar;);
    end;

  TBuffJisState = ( // lead in:
    bjsAscii,       // ESC ( B
    bjsX0201_1976,  // ESC ( J
    bjsX0208_1978,  // ESC $ @
    bjsX0208_1983   // ESC $ B
  );

  TBuffArray = array of Byte;
  TBuffArray4 = array [0..3] of Byte;

  // BG, 11.11.2011: Issue 93: LoadStrings failure
  TBufferState = set of (
    bsFixedCodePage // ignore writing to property CodePage.
                    // Helps to ignore <META http-equiv="Content-Type" ...>,
                    // if buffer is an already converted TBuffString.
  );

  // BG, 17.12.2010: helps converting any kind of stream to WideChars.
  TBuffer = class
  private
    FBuffer: TBuffArray;
    FName: TBuffString;
    FPos: TBuffPointer;
    FEnd: TBuffPointer;
    FJis: TBuffJisState;

    FState: TBufferState;
    FCharSet: TBuffCharSet;
    FCodePage: TBuffCodePage;
    FInitalCodePage: TBuffCodePage;
    function GetNext: Word; {$ifdef UseInline} inline; {$endif}
    function GetNextEucAsShiftJis(out Buffer: TBuffArray4): Integer; {$ifdef UseInline} inline; {$endif}
    function GetNextJisAsShiftJis(out Buffer: TBuffArray4): Integer; {$ifdef UseInline} inline; {$endif}
    function GetAsShiftJis(j, k: Word; out Buffer: TBuffArray4): Integer; {$ifdef UseInline} inline; {$endif}
    function GetPosition: Integer; {$ifdef UseInline} inline; {$endif}
    procedure DetectCodePage;
    procedure Reset;
    procedure SetStream(Stream: TStream); {$ifdef UseInline} inline; {$endif}
    procedure SetPostion(const Value: Integer); {$ifdef UseInline} inline; {$endif}
    procedure SetCharSet(const Value: TBuffCharSet);
    procedure SetCodePage(const Value: TBuffCodePage);
  protected
  public
    class function Convert(Text: TBuffString; CodePage: TBuffCodePage): TBuffString;
    constructor Create(Stream: TStream; Name: TBuffString = ''); overload;
    constructor Create(Stream: TStream; CharSet: TBuffCharSet; Name: TBuffString = ''); overload;
    constructor Create(Stream: TStream; CodePage: TBuffCodePage; Name: TBuffString = ''); overload;
    constructor Create(Text: TBuffString; Name: TBuffString = ''); overload;
    constructor Create(Text: AnsiString; CharSet: TBuffCharSet; Name: TBuffString = ''); overload;
    procedure AssignTo(Destin: TObject);
    function AsString: TBuffString;
    function NextChar: TBuffChar;
    function PeekChar: TBuffChar;  {$ifdef UseInline} inline; {$endif}
    function Size: Integer;
    function GetString(FromIndex, UntilIndex: Integer): TBuffString;
    property Name: TBuffString read FName;
    property CharSet: TBuffCharSet read FCharSet write SetCharSet;
    property CodePage: TBuffCodePage read FCodePage write SetCodePage;
    property Position: Integer read GetPosition write SetPostion;
  end;

  TBuffCharSetCodePageInfo = class
  private
    FName: string;
    FCharSet: TBuffCharSet;
    FCodePage: TBuffCodePage;
  public
    constructor Create(Name: string; CharSet: TBuffCharSet; CodePage: TBuffCodePage);
    property CharSet: TBuffCharSet read FCharSet;
    property CodePage: TBuffCodePage read FCodePage;
    property Name: string read FName;
  end;

function GetCharSetCodePageInfo(Name: string): TBuffCharSetCodePageInfo;
function CharSetToCodePage(CharSet: TBuffCharSet): TBuffCodePage;
function CodePageToCharSet(CodePage: TBuffCodePage): TBuffCharSet;
function AnsiPosX(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;

implementation

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
function GetCharSetCodePageInfo(Name: string): TBuffCharSetCodePageInfo;
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

const
  // always add pairs of CharSet and CodePage.
  // Used by translation methods CharSetToCodePage() and CodePageToCharSet()

  CharSets: array[1..20] of TBuffCharSet = (
    ANSI_CHARSET,     DEFAULT_CHARSET,    SYMBOL_CHARSET,     MAC_CHARSET,          SHIFTJIS_CHARSET,
    HANGEUL_CHARSET,  JOHAB_CHARSET,      GB2312_CHARSET,     CHINESEBIG5_CHARSET,  GREEK_CHARSET,
    TURKISH_CHARSET,  VIETNAMESE_CHARSET, HEBREW_CHARSET,     ARABIC_CHARSET,       BALTIC_CHARSET,
    RUSSIAN_CHARSET,  THAI_CHARSET,       EASTEUROPE_CHARSET, OEM_CHARSET,          EASTEUROPE2_CHARSET);

  CodePages: array[1..20] of integer = (
    1252, CP_ACP, 0,    CP_MACCP, 932,
    949,  1361,   936,  950,      1253,
    1254, 1258,   1255, 1256,     1257,
    1251, 874,    1250, CP_OEMCP, 28592);


//-- BG ---------------------------------------------------------- 14.12.2010 --
function CharSetToCodePage(CharSet: TBuffCharSet): TBuffCodePage;
var
  Index: Integer;
begin
  for Index := low(CharSets) to high(CharSets) do
    if CharSets[Index] = Charset then
    begin
      Result := CodePages[Index];
      exit;
    end;
  raise EConversionError.CreateFmt('Don''t know equivalent code page for char set %d.', [CharSet]);
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
function CodePageToCharSet(CodePage: TBuffCodePage): TBuffCharSet;
var
  Index: Integer;
begin
  for Index := low(CodePages) to high(CodePages) do
    if CodePages[Index] = CodePage then
    begin
      Result := CharSets[Index];
      exit;
    end;
  raise EConversionError.CreateFmt('Don''t know equivalent char set for code page %d.', [CodePage]);
end;

//-- BG ---------------------------------------------------------- 08.01.2011 --
procedure SwapBytes(var Chr: Word);
var
  Bytes: array[0..1] of Byte absolute Chr;
  B: Byte;
begin
  B := Bytes[0];
  Bytes[0]:= Bytes[1];
  Bytes[1] := B;
end;

//------------------------------------------------------------------------------
function AnsiPosX(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
{find substring in S starting at Offset}
var
  S1: AnsiString;
  I: Integer;
begin
  if Offset <= 1 then
    Result := AnsiPos(SubStr, S)
  else
  begin
    S1 := Copy(S, Offset, Length(S) - Offset + 1);
    I := AnsiPos(SubStr, S1);
    if I > 0 then
      Result := I + Offset - 1
    else
      Result := 0;
  end;
end;

{ TBuffCharSetCodePageInfo }

//-- BG ---------------------------------------------------------- 22.12.2010 --
constructor TBuffCharSetCodePageInfo.Create(Name: string;
  CharSet: TBuffCharSet; CodePage: TBuffCodePage);
begin
  inherited Create;
  FName := Name;
  FCharSet := CharSet;
  if CodePage <> CP_UNKNOWN then
    FCodePage := CodePage
  else
    FCodePage := CharSetToCodePage(FCharSet);
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
    AddInfo(TBuffCharSetCodePageInfo.Create('1250', EASTEUROPE_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1251', RUSSIAN_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1252', ANSI_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1253', GREEK_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1254', TURKISH_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1255', HEBREW_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1256', ARABIC_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1257', BALTIC_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('1258', VIETNAMESE_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('5601', HANGEUL_CHARSET, CP_UNKNOWN)); {Korean}
    AddInfo(TBuffCharSetCodePageInfo.Create('866', RUSSIAN_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('874', THAI_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-1', ANSI_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-2', EASTEUROPE2_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-3', TURKISH_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-4', BALTIC_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-5', RUSSIAN_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-7', GREEK_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-9', TURKISH_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('8859-15', ANSI_CHARSET, CP_UNKNOWN)); {almost Ansi, but not quite}
    AddInfo(TBuffCharSetCodePageInfo.Create('932', SHIFTJIS_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('936', GB2312_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('949', HANGEUL_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('950', CHINESEBIG5_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('ansi', ANSI_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('arabic', ARABIC_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('big5', CHINESEBIG5_CHARSET, CP_UNKNOWN)); {traditional Chinese}
    AddInfo(TBuffCharSetCodePageInfo.Create('default', DEFAULT_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('easteurope', EASTEUROPE_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('euc-jp', EUCJP_CHARSET, 932));
    AddInfo(TBuffCharSetCodePageInfo.Create('euc-kr', HANGEUL_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('gb2312', GB2312_CHARSET, CP_UNKNOWN)); {simplified Chinese}
    AddInfo(TBuffCharSetCodePageInfo.Create('greek', GREEK_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('hangeul', HANGEUL_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('hebrew', HEBREW_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('johab', JOHAB_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('koi', RUSSIAN_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('oem', OEM_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('russian', RUSSIAN_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('shift_jis', SHIFTJIS_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('shiftjis', SHIFTJIS_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('symbol', SYMBOL_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('thai', THAI_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('tis-620', THAI_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('turkish', TURKISH_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('utf-16be', DEFAULT_CHARSET, CP_UTF16BE)); //http://tools.ietf.org/html/rfc2781
    AddInfo(TBuffCharSetCodePageInfo.Create('utf-16le', DEFAULT_CHARSET, CP_UTF16LE)); //http://tools.ietf.org/html/rfc2781
    AddInfo(TBuffCharSetCodePageInfo.Create('utf-8', DEFAULT_CHARSET, CP_UTF8));
    AddInfo(TBuffCharSetCodePageInfo.Create('vietnamese', VIETNAMESE_CHARSET, CP_UNKNOWN));
    AddInfo(TBuffCharSetCodePageInfo.Create('x-sjis', SHIFTJIS_CHARSET, CP_UNKNOWN));
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

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; Name: TBuffString = '');
begin
  inherited Create;
  SetStream(Stream);
  FName := Name;
  DetectCodePage;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; CharSet: TBuffCharSet; Name: TBuffString = '');
begin
  inherited Create;
  SetStream(Stream);
  FName := Name;
  FCharSet := CharSet;
  FCodePage := CharSetToCodePage(FCharSet);
  FInitalCodePage := FCodePage;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; CodePage: TBuffCodePage; Name: TBuffString = '');
begin
  inherited Create;
  SetStream(Stream);
  FName := Name;
  FCodePage := CodePage;
  FInitalCodePage := FCodePage;
  FCharSet := CodePageToCharSet(FCodePage);
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(Text: TBuffString; Name: TBuffString = '');
var
  I: Integer;
begin
  inherited Create;
  I := Length(Text) * sizeof(TBuffChar);
  SetLength(FBuffer, I);
  if I > 0 then
    Move(Text[1], FBuffer[0], I);
  Reset;
  FName := Name;
  FCodePage := CP_UTF16LE;
  FInitalCodePage := FCodePage;
  Include(FState, bsFixedCodePage);
  FCharSet := UNKNOWN_CHARSET;
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(Text: AnsiString; CharSet: TBuffCharSet; Name: TBuffString = '');
var
  I: Integer;
begin
  inherited Create;
  I := Length(Text);
  SetLength(FBuffer, I);
  Move(Text[1], FBuffer[0], I);
  Reset;
  FName := Name;
  FCharSet := CharSet;
  FCodePage := CharSetToCodePage(FCharSet);
  FInitalCodePage := FCodePage;
end;

//-- BG ---------------------------------------------------------- 06.12.2011 --
class function TBuffer.Convert(Text: TBuffString; CodePage: TBuffCodePage): TBuffString;
var
  Buffer: TBuffer;
begin
  Buffer := TBuffer.Create(Text);
  try
    Buffer.FCodePage := CodePage;
    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
procedure TBuffer.DetectCodePage;

  function IsIso2022JP: boolean;
  {look for iso-2022-jp Japanese file}
  const
    EscSequence: array [0..3] of AnsiString = (
      #$1b'$@', #$1b'$B', #$1b'(J', #$1b'(B');
  var
    DocS: AnsiString;
    I, J, K, L: integer;
  begin
    Result := False;
    DocS := FPos.AnsiChr;
    I := AnsiPos(EscSequence[0], DocS); {look for starting sequence}
    J := AnsiPos(EscSequence[1], DocS);
    I := Max(I, J); {pick a positive value}
    if I > 0 then
    begin {now look for ending sequence after the start}
      K := AnsiPosX(EscSequence[2], DocS, I);
      L := AnsiPosX(EscSequence[3], DocS, I);
      K := Max(K, L); {pick a positive value}
      if K > 0 then {start and end sequence found}
        Result := True;
    end;
  end;

var
  ByteCount: Integer;
begin
  ByteCount := FEnd.AnsiChr - FPos.AnsiChr;
  if ByteCount >= 2 then
  begin
    // a preamble of up to 3 bytes might tell us the file encoding:
    case FPos.WordPtr^ of
      $FEFF:
        begin
          // this is little endian unicode
          FCodePage := CP_UTF16LE;
          FInitalCodePage := FCodePage;
          Include(FState, bsFixedCodePage);
          FCharSet := UNKNOWN_CHARSET;
          Inc(FPos.WordPtr);
          // TODO: if followed by $0000, this is UTF-32-LE
          Exit;
        end;

      $FFFE:
        begin
          // this is big endian unicode
          // swap the 2 bytes of one char.
          FCodePage := CP_UTF16BE;
          FInitalCodePage := FCodePage;
          Include(FState, bsFixedCodePage);
          FCharSet := UNKNOWN_CHARSET;
          Inc(FPos.WordPtr);
          // TODO: if followed by $0000, this is UTF-32-3412
          Exit;
        end;

      $BBEF:
        if ByteCount >= 3 then
        begin
          Inc(FPos.WordPtr);
          if FPos.BytePtr^ = $BF then
          begin
            // this is UTF-8
            FCodePage := CP_UTF8;
            FInitalCodePage := FCodePage;
            Include(FState, bsFixedCodePage);
            FCharSet := DEFAULT_CHARSET;
            Inc(FPos.BytePtr);
            Exit;
          end;
          Dec(FPos.WordPtr);
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
    FCharSet := SHIFTJIS_CHARSET;
    FCodePage := CP_ISO2022JP;
    FInitalCodePage := FCodePage;
    Include(FState, bsFixedCodePage);
    exit;
  end;
  // no preamble: this is most probably a 1-byte per character code.
  FCharSet := DEFAULT_CHARSET;
  FCodePage := CharSetToCodePage(FCharSet);
  FInitalCodePage := FCodePage;
end;

//-- BG ---------------------------------------------------------- 08.01.2011 --
function TBuffer.GetNext: Word;
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

//------------------------------------------------------------------------------
function TBuffer.GetNextJisAsShiftJis(out Buffer: TBuffArray4): Integer;
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
  Pos: Integer;
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
          Pos := Position;
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
                    Inc(FPos.BytePtr);
                    continue;
                  end;

                Ord('B'):
                  begin
                    FJis := bjsX0208_1983;
                    Inc(FPos.BytePtr);
                    continue;
                  end;
              end;

          end;
          Position := Pos;
        end;
    end;

    case FJis of
      {one byte codes}
      bjsAscii,
      bjsX0201_1976:
        begin
          Buffer[0] := Chr;
          Result := 1;
        end;

      {two byte codes / 94 character sets}
      bjsX0208_1978,
      bjsX0208_1983:
        Result := GetAsShiftJis(Chr, GetNext, Buffer);
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
function TBuffer.GetAsShiftJis(j, k: Word; out Buffer: TBuffArray4): Integer;
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

//-- BG ---------------------------------------------------------- 16.12.2010 --
function TBuffer.GetPosition: Integer;
begin
  Result := FPos.AnsiChr - PAnsiChar(FBuffer);
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
        MultiByteToWideChar(FCodePage, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
      end
      else
        Result := TBuffChar(0);

    CP_EUCJP:
      if FPos.AnsiChr < FEnd.AnsiChr then
      begin
        Len := GetNextEucAsShiftJis(Buffer2);
        MultiByteToWideChar(FCodePage, 0, PAnsiChar(@Buffer2[0]), Len, @Result, 1);
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

//-- BG ---------------------------------------------------------- 16.12.2010 --
function TBuffer.PeekChar: TBuffChar;
var
  Pos: Integer;
  Jis: TBuffJisState;
begin
  Pos := Position;
  Jis := FJis;
  Result := NextChar;
  FJis := Jis;
  Position := Pos;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.Reset;
begin
  FPos.AnsiChr := PAnsiChar(FBuffer);
  FEnd.AnsiChr := FPos.AnsiChr + Length(FBuffer);
end;

//-- BG --------------------------------------------------------- 11.11.2011 --
procedure TBuffer.SetCharSet(const Value: TBuffCharSet);
begin
  FCharSet := Value;
end;

//-- BG --------------------------------------------------------- 11.11.2011 --
procedure TBuffer.SetCodePage(const Value: TBuffCodePage);
begin
  if bsFixedCodePage in FState then
    exit;
  FCodePage := Value;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.SetPostion(const Value: Integer);
begin
  FPos.AnsiChr := PAnsiChar(FBuffer) + Value;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.SetStream(Stream: TStream);
var
  I: Integer;
begin
  I := Stream.Size - Stream.Position;
  SetLength(FBuffer, I);
  //BG, 25.02.2011: Issue 74: Range check error when creating a buffer for the empty stream
  //  Do not read from empty/exhausted stream:
  if I > 0 then
    Stream.Read(FBuffer[0], I);
  Reset;
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
function TBuffer.Size: Integer;
begin
  Result := Length(FBuffer);
end;

// GDG
initialization
finalization
  FreeAndNil(VCharSetCodePageByName);
end.


