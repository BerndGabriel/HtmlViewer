{
Version   11.3
Copyright (c) 2008-2012 by HtmlViewer Team

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

unit HtmlGlobals;

{$I htmlcons.inc}

interface

uses
  {$ifdef UseVCLStyles}
  Vcl.Themes,
  {$endif}
  Classes, SysUtils, Graphics, Controls,
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef LCL}
  LclIntf, LclType,
  StdCtrls, Buttons, Forms, Base64,
  HtmlMisc,
  WideStringsLcl,
  {$ifdef DebugIt}
    {$message 'HtmlViewer uses LCL standard controls.'}
  {$endif}
{$else}
  Consts,
  StrUtils,
  {$ifdef UseTNT}
    {$ifdef DebugIt}
      {$message 'HtmlViewer uses TNT unicode controls.'}
    {$endif}
    TntControls,
    TntStdCtrls,
    {$ifdef Compiler18_Plus}
      WideStrings,
    {$else}
      TntWideStrings,
    {$endif}
    TntClasses,
  {$else UseTNT}
    {$ifdef DebugIt}
      {$message 'HtmlViewer uses VCL standard controls.'}
    {$endif}
    StdCtrls,
    Buttons,
    {$ifdef Compiler18_Plus}
      WideStrings,
    {$else}
      TntWideStrings,
      TntClasses,
    {$endif}
  {$endif UseTNT}
{$endif}
  Math;

type

{$IFNDEF DOTNET}
  {$IFNDEF FPC}
     //needed so that in FreePascal, we can use pointers of different sizes
    {$IFDEF WIN32}
      PtrInt = LongInt;
      PtrUInt = LongWord;
    {$ENDIF}
    {$IFDEF WIN64}
      PtrInt = Int64;
      PtrUInt = Int64;
    {$ENDIF}
    //NOTE:  The code below asumes a 32bit Linux architecture (such as target i386-linux)
    {$IFDEF KYLIX}
      PtrInt = LongInt;
      PtrUInt = LongWord;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$ifdef DebugIt}
  {$ifdef UNICODE}
    {$message 'Compiler uses unicode by default.'}
  {$else}
    {$message 'Compiler uses single byte chars by default.'}
  {$endif}

  {$message 'HtmlViewer uses unicode.'}
{$endif}

{$ifdef FPC}
{$else}
  {$ifdef Compiler18_Plus}
  {$else}
    TWideStringList = class(TTntStringList);
  {$endif}
{$endif}
{$ifdef UNICODE}
  ThtChar = Char;
  ThtString = string;
  ThtStrings = TStrings;
  ThtStringList = TStringList;
  PhtChar = PChar;
{$else}
  UnicodeString = WideString;
  ThtChar = WideChar;
  ThtString = WideString;
  ThtStrings = TWideStrings;
  ThtStringList = TWideStringList;
  PhtChar = PWideChar;
{$endif}
  ThtCharArray = array of ThtChar;
  ThtStringArray = array of ThtString;
  ThtIntegerArray = array of Integer;

  ThtEdit = class({$ifdef UseTNT} TTntEdit {$else} TEdit {$endif})
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

{$ifdef UseTNT}
  ThtButton = TTntButton;
  ThtMemo = TTntMemo;
  ThtCombobox = TTntComboBox;
  ThtListbox = TTntListBox;
  ThtCheckBox = TTntCheckBox;
  ThtRadioButton = TTntRadioButton;
  ThtHintWindow = TTntHintWindow;
{$else}
  ThtButton = TBitBtn; //BG, 25.12.2010: TBitBtn uses correct charset, but TButton does not.
  ThtMemo = TMemo;
  ThtCombobox = TCombobox;
  ThtListbox = TListbox;
  ThtCheckBox = TCheckBox;
  ThtRadioButton = TRadioButton;
  ThtHintWindow = THintWindow;
{$endif}

  //BG, 10.12.2010: don't add virtual methods or fields. It is only used to access protected stuff of TCanvas.
  ThtCanvas = class(TCanvas)
  public
    procedure htTextRect(const Rect: TRect; X, Y: Integer; const Text: ThtString);
  end;

const
  EofChar     = ThtChar(#0);
  TabChar     = ThtChar(#9);
  LfChar      = ThtChar(#10);
  FfChar      = ThtChar(#12);
  CrChar      = ThtChar(#13);
  SpcChar     = ThtChar(' ');
  DotChar     = ThtChar('.');
  LessChar    = ThtChar('<');
  MinusChar   = ThtChar('-');
  GreaterChar = ThtChar('>');
  PercentChar = ThtChar('%');
  AmperChar   = ThtChar('&');
  StarChar    = ThtChar('*');
  CrLf        = ThtString(#13#10);
  CrLfTab     = ThtString(#13#10#9);
  NullRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

{$ifdef LCL}
const
  HWND_MESSAGE = HWND(-3);

  //ANSI_CHARSET            = 0;       // ANSI charset (Windows-1252)
  //DEFAULT_CHARSET         = 1;
  //SYMBOL_CHARSET          = 2;
  MAC_CHARSET             = 77;
  //SHIFTJIS_CHARSET        = 128;     // Shift JIS charset    (Windows-932)
  //HANGEUL_CHARSET         = 129;     // Hangeul charset      (Windows-949)
  JOHAB_CHARSET           = 130;     // Johab charset        (Windows-1361)
  //GB2312_CHARSET          = 134;     // GB2312 charset       (Windows-936)
  //CHINESEBIG5_CHARSET     = 136;     // Chinese Big5 charset (Windows-950)
  //GREEK_CHARSET           = 161;     // Greek charset        (Windows-1253)
  //TURKISH_CHARSET         = 162;     // Turkish charset      (Windows-1254)
  VIETNAMESE_CHARSET      = 163;     // Vietnamese charset   (Windows-1258)
  //HEBREW_CHARSET          = 177;     // Hebrew charset       (Windows-1255)
  //ARABIC_CHARSET          = 178;     // Arabic charset       (Windows-1256)
  //BALTIC_CHARSET          = 186;     // Baltic charset       (Windows-1257)
  //RUSSIAN_CHARSET         = 204;     // Cyrillic charset     (Windows-1251)
  //THAI_CHARSET            = 222;     // Thai charset         (Windows-874)
  //EASTEUROPE_CHARSET      = 238;     // Eastern european charset (Windows-1250)
  //OEM_CHARSET             = 255;

const
  { Global Memory Flags }

  GMEM_FIXED = 0;
  {$EXTERNALSYM GMEM_FIXED}
  GMEM_MOVEABLE = 2;
  {$EXTERNALSYM GMEM_MOVEABLE}
  GMEM_NOCOMPACT = $10;
  {$EXTERNALSYM GMEM_NOCOMPACT}
  GMEM_NODISCARD = $20;
  {$EXTERNALSYM GMEM_NODISCARD}
  GMEM_ZEROINIT = $40;
  {$EXTERNALSYM GMEM_ZEROINIT}
  GMEM_MODIFY = $80;
  {$EXTERNALSYM GMEM_MODIFY}
  GMEM_DISCARDABLE = $100;
  {$EXTERNALSYM GMEM_DISCARDABLE}
  GMEM_NOT_BANKED = $1000;
  {$EXTERNALSYM GMEM_NOT_BANKED}
  GMEM_SHARE = $2000;
  {$EXTERNALSYM GMEM_SHARE}
  GMEM_DDESHARE = $2000;
  {$EXTERNALSYM GMEM_DDESHARE}
  GMEM_NOTIFY = $4000;
  {$EXTERNALSYM GMEM_NOTIFY}
  GMEM_LOWER = GMEM_NOT_BANKED;
  {$EXTERNALSYM GMEM_LOWER}
  GMEM_VALID_FLAGS = 32626;
  {$EXTERNALSYM GMEM_VALID_FLAGS}
  GMEM_INVALID_HANDLE = $8000;
  {$EXTERNALSYM GMEM_INVALID_HANDLE}

  GHND = GMEM_MOVEABLE or GMEM_ZEROINIT;
  {$EXTERNALSYM GHND}
  GPTR = GMEM_FIXED or GMEM_ZEROINIT;
  {$EXTERNALSYM GPTR}

  // device caps
  {$EXTERNALSYM SHADEBLENDCAPS}
  SHADEBLENDCAPS = 45;
  {$EXTERNALSYM SB_CONST_ALPHA}
  SB_CONST_ALPHA = 1;

{
const
  HeapAllocFlags = GMEM_MOVEABLE;

type
  // missing in unit Graphics
  TProgressStage = (
    psStarting,
    psRunning,
    psEnding
  );
}

procedure DecodeStream(Input, Output: TStream);
{$endif}

{$ifndef Compiler17_Plus}
type
  TBytes = array of byte;
{$endif}

var
  IsWin95: Boolean;
  IsWin32Platform: Boolean; {win95, 98, ME}
  ColorBits: Byte;
  ThePalette: HPalette; {the rainbow palette for 256 colors}
  PalRelative: integer;

{$ifdef CopyPaletteMissing}
function CopyPalette(Palette: HPALETTE): HPALETTE;
{$endif}

{$ifdef TransparentStretchBltMissing}
function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
{$endif}

procedure htAppendChr(var Dest: ThtString; C: ThtChar); {$ifdef UseInline} inline; {$endif}
procedure htAppendStr(var Dest: ThtString; const S: ThtString); {$ifdef UseInline} inline; {$endif}
procedure htSetString(var Dest: ThtString; Chr: PhtChar; Len: Integer); {$ifdef UseInline} inline; {$endif}
function htCompareString(S1, S2: ThtString): Integer; {$ifdef UseInline} inline; {$endif}
function htLowerCase(Str: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}
function htTrim(Str: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}
function htUpperCase(Str: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}
// Posx(SubStr, S, Offst): find substring in S starting at Offset:
function PosX(const SubStr, S: ThtString; Offset: Integer = 1): Integer;

function IsAlpha(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
function IsDigit(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
function RemoveQuotes(const S: ThtString): ThtString;

//{$ifdef UnitConstsMissing}
//const
//  SOutOfResources	= 'Out of system resources';
//  SInvalidBitmap	= 'Bitmap image is not valid';
//  SScanLine		= 'Scan line index out of range';
//{$endif}

function PtrSub(P1, P2: Pointer): Integer; {$ifdef UseInline} inline; {$endif}
function PtrAdd(P1: Pointer; Offset: Integer): Pointer; {$ifdef UseInline} inline; {$endif}
procedure PtrInc(var P1; Offset: Integer); {$ifdef UseInline} inline; {$endif}

//code movement from HTMLUn2
function Darker(Color: TColor): TColor; {$ifdef UseInline} inline; {$endif}
function Lighter(Color: TColor): TColor;  {$ifdef UseInline} inline; {$endif}

//code movements from HTMLSubs
function FindSpaces(PStart : PWideChar; const ACount : Integer) : Integer; {$ifdef UseInline} inline; {$endif}
procedure InitFullBg(var FullBG : Graphics.TBitmap; const W, H: Integer; const AIsCopy : Boolean); {$ifdef UseInline} inline; {$endif}
procedure Circle(ACanvas : TCanvas; const X, Y, Rad: Integer); {$ifdef UseInline} inline; {$endif}

//alpha blend determination for Printers only
function CanPrintAlpha(ADC : HDC) : Boolean; {$ifdef UseInline} inline; {$endif}

procedure GetTSize(DC: HDC; P : PWideChar; N : Integer; var VSize : TSize);  {$ifdef UseInline} inline; {$endif}

function ThemedColor(const AColor : TColor): TColor; {$ifdef UseInline} inline; {$endif}

implementation

function ThemedColor(const AColor : TColor): TColor;
begin
  {$ifdef UseVCLStyles}
  Result := StyleServices.GetSystemColor(AColor);
  if Result < 0 then
  begin
    Result := GetSysColor(AColor and $FFFFFF);
    if Result < 0 then
      Result := AColor and $FFFFFF;
  end;
  {$else}
  if AColor < 0 then
    Result := GetSysColor(AColor and $FFFFFF)
  else
    Result := AColor and $FFFFFF;
  {$endif}
end;

procedure GetTSize(DC: HDC; P : PWideChar; N : Integer; var VSize : TSize);
var
    Dummy: Integer;
begin
  if not IsWin32Platform then
    GetTextExtentExPointW(DC, P, N, 0, @Dummy, nil, VSize)
  else
    GetTextExtentPoint32W(DC, P, N, VSize); {win95, 98 ME}
end;

function CanPrintAlpha(ADC : HDC) : Boolean; {$ifdef UseInline} inline; {$endif}
begin
  Result := GetDeviceCaps(ADC,SHADEBLENDCAPS) and SB_CONST_ALPHA > 0;
end;

function Darker(Color: TColor): TColor;
  {find a somewhat darker color for shading purposes}
const
  F = 0.75; // F < 1 makes color darker
var
  Red, Green, Blue: Byte;
begin
  Color := ThemedColor(Color);
  Red := Color and $FF;
  Green := (Color and $FF00) shr 8;
  Blue := (Color and $FF0000) shr 16;
  Result := RGB(Round(F * Red), Round(F * Green), Round(F * Blue));
end;

function Lighter(Color: TColor): TColor;
{find a somewhat lighter color for shading purposes}
const
  F = 1.15; // F > 1 makes color lighter
var
  Red, Green, Blue: Byte;
begin
  Color := ThemedColor(Color);
  if Color = 0 then
    Result := 0
  else
  begin
    Red := Color and $FF;
    Green := (Color and $FF00) shr 8;
    Blue := (Color and $FF0000) shr 16;
    Result := RGB(Min(255, Round(F * Red)), Min(255, Round(F * Green)), Min(255, Round(F * Blue)));
  end;
end;

function FindSpaces(PStart : PWideChar; const ACount : Integer) : Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ACount - 2 do {-2 so as not to count end spaces}
    if ((PStart + I)^ = ' ') or ((PStart + I)^ = #160) then
      Inc(Result);
end;

procedure InitFullBg(var FullBG : Graphics.TBitmap; const W, H: Integer; const AIsCopy : Boolean);
begin
  if not Assigned(FullBG) then
  begin
    FullBG := Graphics.TBitmap.Create;
    if AIsCopy then
    begin
      FullBG.HandleType := bmDIB;
      if ColorBits <= 8 then
        FullBG.Palette := CopyPalette(ThePalette);
    end;
  end;
  FullBG.Width := Max(W,2);
  FullBG.Height := Max(H,2);
end;

procedure Circle(ACanvas : TCanvas; const X, Y, Rad: Integer);
begin
    ACanvas.Ellipse(X, Y - Rad, X + Rad, Y);
end;

//-- BG ------------------------------------------------------------------------
function PtrSub(P1, P2: Pointer): Integer;
begin
{$ifdef FPC}
  Result := P1 - P2;
{$else}
  Result := PAnsiChar(P1) - PAnsiChar(P2);
{$endif}
end;

//-- BG ------------------------------------------------------------------------
function PtrAdd(P1: Pointer; Offset: Integer): Pointer;
begin
{$ifdef FPC}
  Result := P1 + Offset;
{$else}
  Result := PAnsiChar(P1) + Offset;
{$endif}
end;

//-- BG ------------------------------------------------------------------------
procedure PtrInc(var P1; Offset: Integer);
begin
{$ifdef FPC}
  Inc(PAnsiChar(P1), Offset);
{$else}
  Inc(PAnsiChar(P1), Offset);
{$endif}
end;

procedure CalcPalette(DC: HDC);
{calculate a rainbow palette, one with equally spaced colors}
const
  Values: array[0..5] of integer = (55, 115, 165, 205, 235, 255);
var
  LP: ^TLogPalette;
  I, J, K, Sub: integer;
begin
  GetMem(LP, Sizeof(TLogPalette) + 256 * Sizeof(TPaletteEntry));
  try
    with LP^ do
    begin
      palVersion := $300;
      palNumEntries := 256;
      GetSystemPaletteEntries(DC, 0, 256, palPalEntry);
      Sub := 10; {start at entry 10}
      for I := 0 to 5 do
        for J := 0 to 5 do
          for K := 0 to 5 do
            if not ((I = 5) and (J = 5) and (K = 5)) then {skip the white}
              with palPalEntry[Sub] do
              begin
                peBlue := Values[I];
                peGreen := Values[J];
                peRed := Values[K];
                peFlags := 0;
                Inc(Sub);
              end;
      for I := 1 to 24 do
        if not (I in [7, 15, 21]) then {these would be duplicates}
          with palPalEntry[Sub] do
          begin
            peBlue := 130 + 5 * I;
            peGreen := 130 + 5 * I;
            peRed := 130 + 5 * I;
            peFlags := 0;
            Inc(Sub);
          end;
      Sub := 245;
      with palPalEntry[Sub] do
      begin
        peBlue := 254;
        peGreen := 255;
        peRed := 255;
        peFlags := 0;
      end;
      ThePalette := CreatePalette(LP^);
    end;
  finally
    FreeMem(LP, Sizeof(TLogPalette) + 256 * Sizeof(TPaletteEntry));
  end;
end;

{$ifdef CopyPaletteMissing}
// -----------
// CopyPalette
// -----------
// Copies a HPALETTE.
//
// Copied from D3 graphics.pas. This is declared private in some old versions
// of Delphi 2 and is missing in Lazarus Component Library (LCL), so we have
// to implement it here to support those versions.
//
// Parameters:
// Palette	The palette to copy.
//
// Returns:
// The handle to a new palette.
//
function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;
{$endif}

{$ifdef TransparentStretchBltMissing}
const
  SOutOfResources = 'Out of system resources';
var
  SystemPalette16: HPalette; // 16 color palette that maps to the system palette

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  Save: THandle;
  crText, crBack: TColorRef;
  SavePal: HPALETTE;
begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(
        DstDC, DstX, DstY, DstW, DstH,
        SrcDC, SrcX, SrcY,
        MemBmp, MaskX, MaskY,
        MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;
  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(0));
  try
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, SrcW, SrcH));
    Save := SelectObject(MemDC, MemBmp);
    SavePal := SelectPalette(SrcDC, SystemPalette16, False);
    SelectPalette(SrcDC, SavePal, False);
    if SavePal <> 0 then
      SavePal := SelectPalette(MemDC, SavePal, True)
    else
      SavePal := SelectPalette(MemDC, SystemPalette16, True);
    RealizePalette(MemDC);

    StretchBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcCopy);
    StretchBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcErase);
    crText := SetTextColor(DstDC, $0);
    crBack := SetBkColor(DstDC, $FFFFFF);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
    SetTextColor(DstDC, crText);
    SetBkColor(DstDC, crBack);

    if Save <> 0 then SelectObject(MemDC, Save);
    DeleteObject(MemBmp);
  finally
    if SavePal <> 0 then SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;

{$endif TransparentStretchBltMissing}

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure htAppendChr(var Dest: ThtString; C: ThtChar);
begin
  SetLength(Dest, Length(Dest) + 1);
  Dest[Length(Dest)] := C;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure htAppendStr(var Dest: ThtString; const S: ThtString);
var
  L, N: Integer;
begin
  L := Length(S);
  if L > 0 then
  begin
    N := Length(Dest);
    SetLength(Dest, N + L);
    Move(S[1], Dest[N + 1], L * sizeof(ThtChar));
  end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function htCompareString(S1, S2: ThtString): Integer;
begin
{$ifdef UNICODE}
  Result := CompareStr(S1, S2);
{$else}
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, @S1[1], Length(S1), @S2[1], Length(S2)) - 2;
{$endif}
end;

//-- BG ---------------------------------------------------------- 28.01.2011 --
function htLowerCase(Str: ThtString): ThtString;
begin
  {$ifdef UNICODE}
    Result := LowerCase(Str);
  {$else}
    Result := Str;
    CharLowerBuffW(@Result[1], Length(Result));
  {$endif}
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure htSetString(var Dest: ThtString; Chr: PhtChar; Len: Integer);
begin
{$ifdef UNICODE}
  SetString(Dest, Chr, Len);
{$else}
  SetLength(Dest, Len);
  Move(Chr^, Dest[1], Len * sizeof(ThtChar));
{$endif}
end;

//-- BG ---------------------------------------------------------- 09.08.2011 --
function htTrim(Str: ThtString): ThtString;
{$ifdef UNICODE}
begin
  Result := Trim(Str);
{$else}
var
  I, L: Integer;
begin
  L := Length(Str);
  I := 1;
  while (I <= L) and (Str[I] <= ' ') do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while Str[L] <= ' ' do
      Dec(L);
    Result := Copy(Str, I, L - I + 1);
  end;
{$endif}
end;

//-- BG ---------------------------------------------------------- 28.01.2011 --
function htUpperCase(Str: ThtString): ThtString;
begin
  {$ifdef UNICODE}
    Result := UpperCase(Str);
  {$else}
    Result := Str;
    CharUpperBuffW(@Result[1], Length(Result));
  {$endif}
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function IsAlpha(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
begin
  case Ch of
    'a'..'z', 'A'..'Z':
      Result := True;
  else
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function IsDigit(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
begin
  case Ch of
    '0'..'9':
      Result := True;
  else
    Result := False;
  end;
end;

{----------------RemoveQuotes}

function RemoveQuotes(const S: ThtString): ThtString;
{if ThtString is a quoted ThtString, remove the quotes (either ' or ")}
begin
  Result := S;
  if Length(Result) >= 2 then
  begin
    case Result[1] of
      '"', '''':
        if Result[Length(Result)] = Result[1] then
          Result := Copy(Result, 2, Length(Result) - 2);
    end;
  end;
end;

function PosX(const SubStr, S: ThtString; Offset: Integer = 1): Integer;
{find substring in S starting at Offset}
var
  S1: ThtString;
  I: Integer;
begin
  if Offset <= 1 then
    Result := Pos(SubStr, S)
  else
  begin
    S1 := Copy(S, Offset, Length(S) - Offset + 1);
    I := Pos(SubStr, S1);
    if I > 0 then
      Result := I + Offset - 1
    else
      Result := 0;
  end;
end;

{ ThtEdit }

procedure ThtEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and ([ssCtrl] = Shift) then
    SelectAll;
end;

{ ThtCanvas }

procedure ThtCanvas.htTextRect(const Rect: TRect; X, Y: Integer; const Text: ThtString);
{$ifdef LCL}
{$else}
var
  Options: Longint;
{$endif LCL}
begin
{$ifdef LCL}
  inherited TextRect(Rect, X, Y, Utf8Encode(Text));
{$else}
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Options := ETO_CLIPPED or TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((TextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text), Length(Text), nil);
  Changed;
{$endif LCL}
end;

{$ifdef LCL}
procedure DecodeStream(Input, Output: TStream);
const
  BufferSize = 999;
var
  Decoder: TBase64DecodingStream;
  Buffer: array[1..BufferSize] of Byte;
  Count: LongInt;
  I, J: Integer;
begin
  I := 0;
  J := 0;
  Decoder := TBase64DecodingStream.Create(Input, bdmMIME);
  try
    Decoder.Reset;
    repeat
      Count := Decoder.Read(Buffer[1], BufferSize);
      Output.Write(Buffer[1], Count);
      Inc(I);
      Inc(J, Count);
    until Count < BufferSize;
  finally
    Decoder.Free;
  end;
end;
{$endif LCL}

{ initialization }

var
  DC: HDC;

initialization
  DC := GetDC(0);
  try
    ColorBits := GetDeviceCaps(DC, BitsPixel) * GetDeviceCaps(DC, Planes);

    if ColorBits <= 4 then
      ColorBits := 4
    else if ColorBits <= 8 then
      ColorBits := 8
    else
      ColorBits := 24;

    ThePalette := 0;
    if ColorBits = 8 then
      CalcPalette(DC);
    if ColorBits <= 8 then {use Palette Relative bit only when Palettes used}
      PalRelative := $2000000
    else
      PalRelative := 0;
  finally
    ReleaseDC(0, DC);
  end;

{$ifdef TransparentStretchBltMissing}
  // Note: This doesn't return the same palette as the Delphi 3 system palette
  // since the true system palette contains 20 entries and the Delphi 3 system
  // palette only contains 16.
  // For our purpose this doesn't matter since we do not care about the actual
  // colors (or their number) in the palette.
  // Stock objects doesn't have to be deleted.
  SystemPalette16 := GetStockObject(DEFAULT_PALETTE);
{$endif}

{$ifdef FPC}
  IsWin95 := False;           // Use the same always with FPC
  IsWin32Platform := False;
{$else}
  IsWin95 := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (Win32MinorVersion in [0..9]);
  IsWin32Platform := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
{$endif FPC}
finalization
  if ThePalette <> 0 then
    DeleteObject(ThePalette);
end.

