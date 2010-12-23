{
Version   10.2
Copyright (c) 2010 by B.Gabriel

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

uses
  Windows, Graphics, {$ifdef UNICODE} AnsiStrings, {$endif} Classes, SysUtils, Math;

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

  TBuffChar = WideChar;     // the type of a wide char
  PBuffChar = PWideChar;
  TBuffString = WideString; // the type of a wide string

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

  TBuffArray = array of byte;

  // BG, 17.12.2010: helps converting any kind of stream to WideChars.
  TBuffer = class
  private
    FBuffer: TBuffArray;
    FPos: TBuffPointer;
    FEnd: TBuffPointer;
    FJis: TBuffJisState;

    FCharSet: TBuffCharSet;
    FCodePage: TBuffCodePage;
    function GetNextEucAsShiftJis: TBuffArray;
    function GetNextJisAsShiftJis: TBuffArray;
    function GetNextTwoAsShiftJis: TBuffArray;
//    function Position: Integer;
//    function Size: Integer;
    procedure DetectCodePage;
    procedure Read(var Buffer; Count: Integer);
    procedure Reset;
    procedure SetStream(Stream: TStream);
  protected
    function GetNextChar: TBuffChar;
    function GetAsString: TBuffString;
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: TStream; CharSet: TBuffCharSet); overload;
    constructor Create(Stream: TStream; CodePage: TBuffCodePage); overload;
    constructor Create(Text: TBuffString); overload;
    constructor Create(Text: AnsiString; CharSet: TBuffCharSet); overload;
    destructor Destroy; override;
    property NextChar: TBuffChar read GetNextChar;
    property AsString: TBuffString read GetAsString;
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
function AnsiPosX(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;

implementation

uses
  HtmlGlobals;

type
  TBuffCharSetCodePageInfoList = class(TStringList)
  private
    function GetInfo(Index: Integer): TBuffCharSetCodePageInfo;
  public
    constructor Create;
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

//-- BG ---------------------------------------------------------- 14.12.2010 --
procedure SwapBytes(var Chr: TBuffChar);
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

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream);
begin
  inherited Create;
  SetStream(Stream);
  DetectCodePage;
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; CharSet: TBuffCharSet);
begin
  inherited Create;
  SetStream(Stream);
  FCharSet := CharSet;
  FCodePage := CharSetToCodePage(FCharSet);
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
constructor TBuffer.Create(Stream: TStream; CodePage: TBuffCodePage);
begin
  inherited Create;
  SetStream(Stream);
  FCodePage := CodePage;
  FCharSet := CodePageToCharSet(FCodePage);
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(Text: TBuffString);
var
  I: Integer;
begin
  I := Length(Text) * sizeof(TBuffChar);
  SetLength(FBuffer, I);
  Move(Text[1], FBuffer[0], I);
  Reset;
  FCodePage := CP_UTF16LE;
  FCharSet := UNKNOWN_CHARSET;
end;

//-- BG ---------------------------------------------------------- 17.12.2010 --
constructor TBuffer.Create(Text: AnsiString; CharSet: TBuffCharSet);
var
  I: Integer;
begin
  I := Length(Text);
  SetLength(FBuffer, I);
  Move(Text[1], FBuffer[0], I);
  Reset;
  FCharSet := CharSet;
  FCodePage := CharSetToCodePage(FCharSet);
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
destructor TBuffer.Destroy;
begin
  inherited;
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
          FCharSet := UNKNOWN_CHARSET;
          Inc(FPos.WordPtr);
          Exit;
        end;

      $FFFE:
        begin
          // this is big endian unicode
          // swap the 2 bytes of one char.
          FCodePage := CP_UTF16BE;
          FCharSet := UNKNOWN_CHARSET;
          Inc(FPos.WordPtr);
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
            FCharSet := DEFAULT_CHARSET;
            Inc(FPos.BytePtr);
            Exit;
          end;
          Dec(FPos.WordPtr);
        end;
    end;
  end;
  if IsIso2022JP then
  begin
    FCharSet := SHIFTJIS_CHARSET;
    FCodePage := CP_ISO2022JP;
    exit;
  end;
  // no preamble: this is most probably a 1-byte per character code.
  FCharSet := DEFAULT_CHARSET;
  FCodePage := CharSetToCodePage(FCharSet);
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
function TBuffer.GetAsString: TBuffString;
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

      CP_ACP,
      CP_OEMCP,
      CP_MACCP:
        begin
          J := MultiByteToWideChar(CP_UTF8, 0, FPos.AnsiChr, I, PWideChar(Result), I);
          //BG, 23.12.2010: Some single byte chars may have been converted accidently as UTF-8.
          // As a real UTF-8 source becomes significantly shorter, if number of chars shrinks
          // less than 10 percent, do not assume it is UTF-8.
          if (J > 0) and (I - J > I div 10) then
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
    end
  end
  else
    Result := '';
end;

//-- BG ---------------------------------------------------------- 14.12.2010 --
function TBuffer.GetNextChar: TBuffChar;
var
  Buffer: Byte;
  Buffer2: TBuffArray;
  Chr: Cardinal;
begin
  Buffer2 := nil; // valium for the compiler
  if FPos.AnsiChr < FEnd.AnsiChr then
    case FCodePage of
      CP_UTF16LE:
        begin
          Read(Result, 2);
        end;

      CP_UTF16BE:
        begin
          Read(Result, 2);
          SwapBytes(Result);
        end;

      CP_UTF8:
        begin
          Read(Buffer, 1);
          if (Buffer and $80) <> 0 then
          begin
            Chr := Buffer and $3F;
            if (Buffer and $20) <> 0 then
            begin
              Read(Buffer, 1);
              if (Buffer and $C0) <> $80 then
              begin
                Result := TBuffChar(0);
                exit;
              end;
              Chr := (Chr shl 6) or (Buffer and $3F);
            end;
            Read(Buffer, 1);
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
        begin
          Buffer2 := GetNextJisAsShiftJis;
          MultiByteToWideChar(FCodePage, 0, @Buffer2[0], Length(Buffer2), @Result, 1);
        end;

      CP_EUCJP:
        begin
          Buffer2 := GetNextEucAsShiftJis;
          MultiByteToWideChar(FCodePage, 0, @Buffer2[0], Length(Buffer2), @Result, 1);
        end;
    else
      Read(Buffer, 1);
      MultiByteToWideChar(FCodePage, 0, @Buffer, 1, @Result, 1);
    end
  else
    Result := TBuffChar(0);
end;

//------------------------------------------------------------------------------
function TBuffer.GetNextEucAsShiftJis: TBuffArray;
begin
  if FPos.AnsiChr < FEnd.AnsiChr then
  begin
    if FPos.BytePtr^ <= $A0 then
    begin
      SetLength(Result, 1);
      Result[0] := FPos.BytePtr^;
      Inc(FPos.BytePtr);
      exit;
    end;

    Result := GetNextTwoAsShiftJis;
  end;
end;

//------------------------------------------------------------------------------
function TBuffer.GetNextJisAsShiftJis: TBuffArray;
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
begin
  while FPos.AnsiChr < FEnd.AnsiChr do
  begin
    case FPos.BytePtr^ of
      ESC:
        if FEnd.AnsiChr - FPos.AnsiChr > 2 then
        begin
          Inc(FPos.BytePtr);
          case FPos.BytePtr^ of

            Ord('('):
              begin
                Inc(FPos.BytePtr);
                case FPos.BytePtr^ of
                  Ord('B'):
                    begin
                      FJis := bjsAscii;
                      Inc(FPos.BytePtr);
                      continue;
                    end;

                  Ord('J'):
                    begin
                      FJis := bjsX0201_1976;
                      Inc(FPos.BytePtr);
                      continue;
                    end;
                end;
                Dec(FPos.BytePtr);
              end;

            Ord('$'):
              begin
                Inc(FPos.BytePtr);
                case FPos.BytePtr^ of
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
                Dec(FPos.BytePtr);
              end;

          end;
          Dec(FPos.BytePtr);
        end;
    end;

    case FJis of
      {one byte codes}
      bjsAscii,
      bjsX0201_1976:
        begin
          SetLength(Result, 1);
          Result[0] := FPos.BytePtr^;
          Inc(FPos.BytePtr);
          exit;
        end;

      {two byte codes / 94 character sets}
      bjsX0208_1978,
      bjsX0208_1983:
        begin
          Result := GetNextTwoAsShiftJis;
          exit;
        end;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 22.12.2010 --
function TBuffer.GetNextTwoAsShiftJis: TBuffArray;
// core method to convert a 2 byte EUC-JP resp. X0208 code to ShiftJIS
var
  j, k, s, t: Word;
begin
  SetLength(Result, 2);

  {first byte}
  j := FPos.BytePtr^ and $7F; {and $7F just for safety}
  if (j in [33..96]) then
    s := (j + 1) div 2 + 112
  else
    s := (j + 1) div 2 + 176;
  Result[0] := s;
  Inc(FPos.BytePtr);

  {second byte}
  k := FPos.BytePtr^ and $7F; {and $7F just for safety}
  if odd(j) then
  begin
    t := k + 31;
    if k > 95 then
      inc(t);
  end
  else
    t := k + 126;
  Result[1] := t;
  Inc(FPos.BytePtr);
end;

////-- BG ---------------------------------------------------------- 16.12.2010 --
//function TBuffer.Position: Integer;
//begin
//  Result := FPos.AnsiChr - PAnsiChar(FBuffer);
//end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.Read(var Buffer; Count: Integer);
begin
  case Count of
    1: PByte(@Buffer)^ := FPos.BytePtr^;
    2: PWord(@Buffer)^ := FPos.WordPtr^;
  else
    Move(FPos.AnsiChr^, Buffer, Count);
  end;
  Inc(FPos.BytePtr, Count);
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.Reset;
begin
  FPos.AnsiChr := PAnsiChar(FBuffer);
  FEnd.AnsiChr := FPos.AnsiChr + Length(FBuffer);
end;

//-- BG ---------------------------------------------------------- 16.12.2010 --
procedure TBuffer.SetStream(Stream: TStream);
var
  I: Integer;
begin
  I := Stream.Size - Stream.Position;
  SetLength(FBuffer, I);
  Stream.Read(FBuffer[0], I);
  Reset;
end;

////-- BG ---------------------------------------------------------- 16.12.2010 --
//function TBuffer.Size: Integer;
//begin
//  Result := Length(FBuffer);
//end;

end.
