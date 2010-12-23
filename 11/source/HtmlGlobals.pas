{
Version   10.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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
  Windows, Graphics,
{$ifdef FPC}
  RtlConsts,
{$else}
  Consts,
{$endif}
{$ifdef UseTNT}
  {$message 'HtmlViewer uses TNT unicode controls.'}
  TntStdCtrls,
  {$ifdef Compiler17_Plus}
    WideStrings,
  {$else}
    TntWideStrings,
  {$endif}
  TntClasses,
{$else UseTNT}
  {$ifdef UseElPack}
    {$message 'HtmlViewer uses ElPack unicode controls.'}
    ElListBox, ElCombos, ElEdits, ElPopBtn,
  {$else UseElPack}
    {$message 'HtmlViewer uses VCL standard controls.'}
    StdCtrls,
  {$endif UseElPack}
{$endif UseTNT}
  Classes, SysUtils,
  HtmlBuffer;

type
{$ifdef UseUnicode}
  {$message 'HtmlViewer uses unicode.'}
  {$ifdef UNICODE}
    ThtChar = Char;
    ThtString = string;
    ThtStrings = TStrings;
    ThtStringList = TStringList;
    PhtChar = PChar;
  {$else}
    ThtChar = WideChar;
    ThtString = WideString;
    ThtStrings = TWideStrings;
    {$ifdef UseTNT}
      ThtStringList = TTntStringList;
    {$else}
      ThtStringList = TWideStringList;
    {$endif}
    PhtChar = PWideChar;
  {$endif}
{$else}
  {$message 'HtmlViewer uses single byte chars.'}
  {$ifdef UNICODE}
    ThtChar = AnsiChar;
    ThtString = AnsiString;
    ThtStrings = TAnsiStrings;
    ThtStringList = TAnsiStringList;
    PhtChar = PAnsiChar;
  {$else}
    ThtChar = Char;
    ThtString = string;
    ThtStrings = TStrings;
    ThtStringList = TStringList;
    PhtChar = PChar;
  {$endif}
{$endif}

{$ifdef UNICODE}
  {$message 'Compiler uses unicode by default.'}
{$else}
  {$message 'Compiler uses single byte chars by default.'}
  UnicodeString = WideString;
{$endif}

  ThtEdit = class(
    {$ifdef UseTNT}TTntEdit{$else}
    {$ifdef UseElPack}TElEdit{$else}
    TEdit{$endif}{$endif})
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

{$ifdef UseTNT}
  ThtButton = TTntButton;
  ThtMemo = TTntMemo;
  ThtCombobox = TTntCombobox;
  ThtListbox = TTntListbox;
  ThtCheckBox = TTntCheckBox;
  ThtRadioButton = TTntRadioButton;
{$else}
  {$ifdef UseElPack}
  ThtButton = TElPopupButton;
  ThtMemo = TElMemo;
  ThtCombobox = TElCombobox;
  ThtListbox = TElListbox;
  {$else}
  ThtButton = TButton;
  ThtMemo = TMemo;
  ThtCombobox = TCombobox;
  ThtListbox = TListbox;
  {$endif}
  ThtCheckBox = TCheckBox;
  ThtRadioButton = TRadioButton;
{$endif}

  //BG, 10.12.2010: don't add virtual methods or fields. It is only used to access protected stuff of TCanvas.
  ThtCanvas = class(TCanvas)
  public
    procedure htTextRect(const Rect: TRect; X, Y: Integer; const Text: ThtString);
  end;

const
  SpcChar:      ThtChar = ' ';
  DotChar:      ThtChar = '.';
  TabChar:      ThtChar = #9;
  EofChar:      ThtChar = #0;
  CrChar:       ThtChar = #13;
  LfChar:       ThtChar = #10;
  LessChar:     ThtChar = '<';
  MinusChar:    ThtChar = '-';
  GreaterChar:  ThtChar = '>';
  PercentChar:  ThtChar = '%';
  AmperChar:    ThtChar = '&';

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

const
  HeapAllocFlags = GMEM_MOVEABLE;

type
  // missing in unit Graphics
  TProgressStage = (
    psStarting,
    psRunning,
    psEnding
  );
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

// UNICODE dependent loading string methods
function LoadStringFromStreamA(Stream: TStream): AnsiString;
function LoadStringFromStreamW(Stream: TStream): WideString;
function LoadStringFromFileA(const Name: ThtString): AnsiString;
function LoadStringFromFileW(const Name: ThtString): WideString;

function LoadStringFromStream(Stream: TStream): ThtString; {$ifdef UseInline} inline; {$endif}
function LoadStringFromFile(const Name: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}


function htUpCase(Chr: ThtChar): ThtChar; {$ifdef UseInline} inline; {$endif}

//{$ifdef UnitConstsMissing}
//const
//  SOutOfResources	= 'Out of system resources';
//  SInvalidBitmap	= 'Bitmap image is not valid';
//  SScanLine		= 'Scan line index out of range';
//{$endif}

implementation

uses
  HtmlUn2;

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
(*
**  GDI Error handling
**  Adapted from graphics.pas
*)
{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$endif}
{$ifdef D3_BCB3}
function GDICheck(Value: Integer): Integer;
{$else}
function GDICheck(Value: Cardinal): Cardinal;
{$endif}
var
  ErrorCode		: integer;
// 2008.10.19 ->
{$ifdef VER20_PLUS}
  Buf			: array [byte] of WideChar;
{$else}
  Buf			: array [byte] of AnsiChar;
{$endif}
// 2008.10.19 <-

  function ReturnAddr: Pointer;
  // From classes.pas
  asm
    MOV		EAX,[EBP+4] // sysutils.pas says [EBP-4], but this works !
  end;

begin
  if (Value = 0) then
  begin
    ErrorCode := GetLastError;
    if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
      ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
      raise EOutOfResources.Create(Buf) at ReturnAddr
    else
      raise EOutOfResources.Create(SOutOfResources) at ReturnAddr;
  end;
  Result := Value;
end;
{$ifdef R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$endif}

var
  // From Delphi 3 graphics.pas
  SystemPalette16: HPalette; // 16 color palette that maps to the system palette

// Copied from D3 graphics.pas
// Fixed by Brian Lowe of Acro Technology Inc. 30Jan98
function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy		= $00AA0029;
var
  MemDC			,
  OrMaskDC		: HDC;
  MemBmp		,
  OrMaskBmp		: HBITMAP;
  Save			,
  OrMaskSave		: THandle;
  crText, crBack	: TColorRef;
  SavePal		: HPALETTE;

begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;

  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(DstDC));
  try
    { Color bitmap for combining OR mask with source bitmap }
    MemBmp := GDICheck(CreateCompatibleBitmap(DstDC, SrcW, SrcH));
    try
      Save := SelectObject(MemDC, MemBmp);
      try
        { This bitmap needs the size of the source but DC of the dest }
        OrMaskDC := GDICheck(CreateCompatibleDC(DstDC));
        try
          { Need a monochrome bitmap for OR mask!! }
          OrMaskBmp := GDICheck(CreateBitmap(SrcW, SrcH, 1, 1, nil));
          try
            OrMaskSave := SelectObject(OrMaskDC, OrMaskBmp);
            try

              // OrMask := 1
              // Original: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, WHITENESS);
              // Replacement, but not needed: PatBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, WHITENESS);
              // OrMask := OrMask XOR Mask
              // Not needed: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, SrcInvert);
              // OrMask := NOT Mask
              BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, NotSrcCopy);

              // Retrieve source palette (with dummy select)
              SavePal := SelectPalette(SrcDC, SystemPalette16, False);
              // Restore source palette
              SelectPalette(SrcDC, SavePal, False);
              // Select source palette into memory buffer
              if SavePal <> 0 then
                SavePal := SelectPalette(MemDC, SavePal, True)
              else
                SavePal := SelectPalette(MemDC, SystemPalette16, True);
              RealizePalette(MemDC);

              // Mem := OrMask
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, SrcCopy);
              // Mem := Mem AND Src
{$IFNDEF GIF_TESTMASK} // Define GIF_TESTMASK if you want to know what it does...
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcAnd);
{$else}
              StretchBlt(DstDC, DstX, DstY, DstW DIV 2, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              StretchBlt(DstDC, DstX+DstW DIV 2, DstY, DstW DIV 2, DstH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              exit;
{$endif}
            finally
              if (OrMaskSave <> 0) then
                SelectObject(OrMaskDC, OrMaskSave);
            end;
          finally
            DeleteObject(OrMaskBmp);
          end;
        finally
          DeleteDC(OrMaskDC);
        end;

        crText := SetTextColor(DstDC, $00000000);
        crBack := SetBkColor(DstDC, $00FFFFFF);

        { All color rendering is done at 1X (no stretching),
          then final 2 masks are stretched to dest DC }
        // Neat trick!
        // Dst := Dst AND Mask
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, SrcX, SrcY, SrcW, SrcH, SrcAnd);
        // Dst := Dst OR Mem
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcPaint);

        SetTextColor(DstDC, crText);
        SetTextColor(DstDC, crBack);

      finally
        if (Save <> 0) then
          SelectObject(MemDC, Save);
      end;
    finally
      DeleteObject(MemBmp);
    end;
  finally
    if (SavePal <> 0) then
      SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;
{$endif}

function LoadStringFromStreamA(Stream: TStream): AnsiString;
var
  ByteCount: Integer;
{$ifdef UNICODE}
  PreambleSize: Integer;
  Buffer: TBytes;
  Encoding: TEncoding;
{$endif}
begin
  //BG, 07.12.2010: cannot start in the middle of a stream,
  // if I want to recognize encoding by preamble.
  Stream.Position := 0;
{$ifdef UNICODE}
  ByteCount := Stream.Size - Stream.Position;
  if ByteCount = 0 then
  begin
    Result := '';
    exit;
  end;
  SetLength(Buffer, ByteCount);
  Stream.Read(Buffer[0], ByteCount);
  Encoding := nil;
  PreambleSize := TEncoding.GetBufferEncoding(Buffer, Encoding);
  if Encoding = TEncoding.Default then
  begin
    // BG, 04.12.2010: GetBufferEncoding looks for preambles only to detected
    // encoding, but often there is no header/preamble in UTF-8 streams/files.
    Result := TEncoding.UTF8.GetString(Buffer, PreambleSize, Length(Buffer) - PreambleSize);
    if Result <> '' then
      exit;
  end;
  Result := Encoding.GetString(Buffer, PreambleSize, Length(Buffer) - PreambleSize);
{$else}
  ByteCount := Stream.Size - Stream.Position;
  SetString(Result, nil, ByteCount);
  Stream.Read(Result[1], ByteCount);
{$endif}
end;

function LoadStringFromStreamW(Stream: TStream): WideString;
var
  Buffer: TBuffer;
begin
  Buffer := TBuffer.Create(Stream);
  try
    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

function LoadStringFromFileA(const Name: ThtString): AnsiString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadStringFromStreamA(Stream);
  finally
    Stream.Free;
  end;
end;

function LoadStringFromFileW(const Name: ThtString): WideString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadStringFromStreamW(Stream);
  finally
    Stream.Free;
  end;
end;

function LoadStringFromStream(Stream: TStream): ThtString;
begin
{$ifdef UseUnicode}
  Result := LoadStringFromStreamW(Stream);
{$else}
  Result := LoadStringFromStreamA(Stream);
{$endif}
end;

function LoadStringFromFile(const Name: ThtString): ThtString;
begin
{$ifdef UseUnicode}
  Result := LoadStringFromFileW(Name);
{$else}
  Result := LoadStringFromFileA(Name);
{$endif}
end;


//-- BG ---------------------------------------------------------- 11.12.2010 --
function htUpCase(Chr: ThtChar): ThtChar;
begin
{$ifdef UseUnicode}
  {$ifdef UNICODE}
    Result := UpCase(Chr);
  {$else}
    Result := Chr;
    CharUpperBuffW(@Result, 1);
  {$endif}
{$else}
  {$ifdef UNICODE}
    Result := Chr;
    CharUpperBuffA(@Result, 1);
  {$else}
    Result := UpCase(Chr);
  {$endif}
{$endif}
end;

{ ThtEdit }

procedure ThtEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and ([ssCtrl] = Shift) then
    SelectAll;
end;

{ ThtCanvas }

procedure ThtCanvas.htTextRect(const Rect: TRect; X, Y: Integer; const Text: ThtString);
var
  Options: Longint;
begin
{$ifdef LCL}
  inherited TextRect(Rect, X, Y, Text);
{$else}
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Options := ETO_CLIPPED or TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((TextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
{$ifdef UseUnicode}
  Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text), Length(Text), nil);
{$else}
  Windows.ExtTextOutA(Handle, X, Y, Options, @Rect, PChar(Text), Length(Text), nil);
{$endif UseUnicode}
  Changed;
{$endif LCL}
end;

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

  IsWin95 := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (Win32MinorVersion in [0..9]);
  IsWin32Platform := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
finalization
  if ThePalette <> 0 then
    DeleteObject(ThePalette);
end.

