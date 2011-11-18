
{***************************************************************}
{*                  DitherUnit.PAS                             *}
{*                                                             *}
{*   Thanks to Anders Melander, anders@melander.dk, for the    *}
{*   color dithering code in this module.  This code was       *}
{*   extracted from his excellent TGifImage.pas unit.          *}
{*                                                             *}
{*                                                             *}
{*              Bugs introduced by Dave Baldwin                *}
{***************************************************************}


// Copyright	(c) 1997,98 Anders Melander.                                  //
//		All rights reserved.                                          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// This software is copyrighted as noted above.  It may be freely copied,     //
// modified, and redistributed, provided that the copyright notice(s) is      //
// preserved on all copies.                                                   //
//                                                                            //
// TGIFImage is freeware and I would like it to remain so. This means that it //
// may not be bundled with commercial libraries or sold as shareware. You are //
// welcome to use it in commercial and shareware applications providing you   //
// do not charge for the functionality provided by TGIFImage.                 //
// If you are in doubt, please contact me and I will explain this.            //
//                                                                            //
// There is no warranty or other guarantee of fitness for this software, it   //
// is provided solely "as is".  Bug reports or fixes may be sent to the       //
// author, who may or may not act on them as he desires.                      //
//                                                                            //
// If you redistribute this code in binary form (i.e. as a library or linked  //
// into an application), the accompanying documentation should state that     //
// "this software is based, in part, on the work of Anders Melander" or words //
// to that effect.                                                            //
//                                                                            //
// If you modify this software, you should include a notice in the revision   //
// history in the history.txt file giving the date and the name of the person //
// performing the modification and a brief description of the modification.   //
//                                                                            //

unit DitherUnit;

{$I htmlcons.inc}

interface

{$ifNdef LCL}
 {$DEFINE PIXELFORMAT_TOO_SLOW}
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//		Determine Delphi and C++ Builder version
//
////////////////////////////////////////////////////////////////////////////////

// Delphi 2.x
{$IFDEF VER90}
Error: This module not used with Delphi 2
{$ENDIF}

// Delphi 3.x
{$IFDEF VER100}
{$DEFINE VER10x}
{$ENDIF}

// C++ Builder 3.x
{$IFDEF VER110}
{$DEFINE VER10x}
{$DEFINE VER11_PLUS}
{$DEFINE D4_BCB3}
{$ENDIF}

// Delphi 4.x
{$IFDEF VER120}
{$DEFINE VER10x}
{$DEFINE VER11_PLUS}
{$DEFINE D4_BCB3}
{$ENDIF}

{$IFDEF Ver130} {Delphi 5}
{$DEFINE VER10x}
{$DEFINE VER11_PLUS}
{$DEFINE D4_BCB3}
{$ENDIF}

{$IFDEF ver125} {C++Builder 4}
{$DEFINE VER11_PLUS}
{$DEFINE D4_BCB3}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			External dependecies
//
////////////////////////////////////////////////////////////////////////////////
uses
{$ifdef LCL}
  LclIntf, LclType, //LMessages,
{$else}
  Windows,
{$endif}
  SysUtils, Graphics, Classes;

function GetBitmap(Source: TPersistent): TBitmap; {LDB}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//			Implementation
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
{$IFDEF DEBUG}
  dialogs,
{$ENDIF}
//BG, 01.09.2009: ifdef added:
{$IFDEF DEBUG_DITHERPERFORMANCE}
  mmsystem, // timeGetTime()
  messages,
{$ENDIF}
  HtmlGlobals;

////////////////////////////////////////////////////////////////////////////////
//
//                      Error messages
//
////////////////////////////////////////////////////////////////////////////////
resourcestring
  // GIF Error messages
  //BG, 01.09.2009: unused: sOutOfData		= 'Premature end of data';
  sOutOfMemDIB = 'Failed to allocate memory for GIF DIB';
  sDIBCreate = 'Failed to create DIB from Bitmap';
  sNoDIB = 'Image has no DIB';
  sInvalidBitmap = 'Bitmap image is not valid';
  SInvalidPixelFormat = 'Invalid pixel format';
  SScanLine = 'Scan line index out of range';

////////////////////////////////////////////////////////////////////////////////
//
//			Misc constants and support types
//
////////////////////////////////////////////////////////////////////////////////
type
  // TGIFImage mostly throws exceptions of type GIFException
  GIFException = class(EInvalidGraphic);

  // Color reduction methods
  TColorReduction =
    (rmNone, // Do not perform color reduction
    rmWindows20, // Reduce to the Windows 20 color system palette
    rmWindows256, // Reduce to the Windows 256 color halftone palette (Only works in 256 color display mode)
    rmNetscape, // Reduce to the Netscape 216 color palette
    rmMyPalette,
    rmQuantizeWindows // Reduce to optimal 256 color windows palette
    );
  TDitherMode =
    (dmNearest, // Nearest color matching w/o error correction
    dmFloydSteinberg // Floyd Steinberg Error Diffusion dithering
     // dmOrdered,		// Ordered dither
     // dmCustom		// Custom palette
    );

////////////////////////////////////////////////////////////////////////////////
//
//                      Utility routines
//
////////////////////////////////////////////////////////////////////////////////
//  // WebPalette creates a 216 color uniform palette a.k.a. the Netscape Palette
//  function WebPalette: HPalette;
//
//  // ReduceColors
//  // Map colors in a bitmap to their nearest representation in a palette using
//  // the methods specified by the ColorReduction and DitherMode parameters.
//  // The ReductionBits parameter specifies the desired number of colors (bits
//  // per pixel) when the reduction method is rmQuantize.
//  function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
//    DitherMode: TDitherMode): TBitmap;

function WebPalette: HPalette;
type
  TLogWebPalette = packed record
    palVersion: word;
    palNumEntries: word;
    PalEntries: array[0..5, 0..5, 0..5] of TPaletteEntry;
  end;
var
  r, g, b: byte;
  LogWebPalette: TLogWebPalette;
  LogPalette: TLogpalette absolute LogWebPalette; // Stupid typecast
begin
  with LogWebPalette do
  begin
    palVersion := $0300;
    palNumEntries := 216;
    for r := 0 to 5 do
      for g := 0 to 5 do
        for b := 0 to 5 do
        begin
          with PalEntries[r, g, b] do
          begin
            peRed := 51 * r;
            peGreen := 51 * g;
            peBlue := 51 * b;
            peFlags := 0;
          end;
        end;
  end;
  Result := CreatePalette(Logpalette);
end;

(*
**  Raise error condition
*)

procedure Error(msg: string);

  function ReturnAddr: Pointer;
{$ifdef LCL}
  begin
    Result := nil; // ToDo: Fix it. Why is there assy?
{$else}
  asm  // From classes.pas
    MOV		EAX,[EBP+4] // sysutils.pas says [EBP-4] !
{$endif}
  end;

begin
  raise GIFException.Create(msg) at ReturnAddr;
end;

// Round to arbitrary number of bits

function AlignBit(Bits, BitsPerPixel, Alignment: Cardinal): Cardinal;
begin
  Dec(Alignment);
  Result := ((Bits * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result shr 3;
end;

//type
//  TPixelFormats = set of TPixelFormat;

// --------------------------
// InitializeBitmapInfoHeader
// --------------------------
// Fills a TBitmapInfoHeader with the values of a bitmap when converted to a
// DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Info		The TBitmapInfoHeader buffer that will receive the values.
// PixelFormat	The pixel format of the destination DIB.
//
{$IFDEF D4_BCB3}
  // Disable optimization to circumvent D4/BCB3 optimizer bug
{$IFOPT O+}
{$DEFINE O_PLUS}
{$O-}
{$ENDIF}
{$ENDIF}

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var Info: TBitmapInfoHeader;
  PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  DIB: TDIBSection;
  Bytes: Integer;
begin
  FillChar(DIB, sizeof(DIB), 0);
  Bytes := GetObject(Bitmap, SizeOf(DIB), @DIB);
  if (Bytes = 0) then
    Error(sInvalidBitmap);

  if (Bytes >= (sizeof(DIB.dsbm) + sizeof(DIB.dsbmih))) and
    (DIB.dsbmih.biSize >= sizeof(DIB.dsbmih)) then
    Info := DIB.dsbmih
  else
  begin
    FillChar(Info, sizeof(Info), 0);
    with Info, DIB.dsbm do
    begin
      biSize := SizeOf(Info);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case PixelFormat of
    pf1bit: Info.biBitCount := 1;
    pf4bit: Info.biBitCount := 4;
    pf8bit: Info.biBitCount := 8;
    pf24bit: Info.biBitCount := 24;
  else
    Error(sInvalidPixelFormat);
    // Info.biBitCount := DIB.dsbm.bmBitsPixel * DIB.dsbm.bmPlanes;
  end;
  Info.biPlanes := 1;
  Info.biSizeImage := AlignBit(Info.biWidth, Info.biBitCount, 32) * Cardinal(abs(Info.biHeight));
end;
{$IFDEF O_PLUS}
{$O+}
{$UNDEF O_PLUS}
{$ENDIF}

// -------------------
// InternalGetDIBSizes
// -------------------
// Calculates the buffer sizes nescessary for convertion of a bitmap to a DIB
// of a specified PixelFormat.
// See the GetDIBSizes API function for more info.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// InfoHeaderSize
//		The returned size of a buffer that will receive the DIB's
//		TBitmapInfo structure.
// ImageSize	The returned size of a buffer that will receive the DIB's
//		pixel data.
// PixelFormat	The pixel format of the destination DIB.
//

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
  var ImageSize: longInt; PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  Info: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, Info, PixelFormat);
  // Check for palette device format
  if (Info.biBitCount > 8) then
  begin
    // Header but no palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if ((Info.biCompression and BI_BITFIELDS) <> 0) then
      Inc(InfoHeaderSize, 12);
  end
  else
    // Header and palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl Info.biBitCount);
  ImageSize := Info.biSizeImage;
end;

// --------------
// InternalGetDIB
// --------------
// Converts a bitmap to a DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Pal		The handle of the source palette.
// BitmapInfo	The buffer that will receive the DIB's TBitmapInfo structure.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// Bits		The buffer that will receive the DIB's pixel data.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// PixelFormat	The pixel format of the destination DIB.
//
// Returns:
// True on success, False on failure.
//
// Note: The InternalGetDIBSizes function can be used to calculate the
// nescessary sizes of the BitmapInfo and Bits buffers.
//

function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
// From graphics.pas, "optimized" for our use
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), PixelFormat);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if (Palette <> 0) then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := (GetDIBits(DC, Bitmap, 0, abs(TBitmapInfoHeader(BitmapInfo).biHeight),
      @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0);
  finally
    if (OldPal <> 0) then
      SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

// --------------
// GetPixelFormat
// --------------
// Returns the current pixel format of a bitmap.
//
// Replacement for delphi 3 TBitmap.PixelFormat getter.
//
// Parameters:
// Bitmap	The bitmap which pixel format is returned.
//
// Returns:
// The PixelFormat of the bitmap
//

function GetPixelFormat(Bitmap: TBitmap): TPixelFormat;
begin
  Result := Bitmap.PixelFormat;
end;

// --------------
// SetPixelFormat
// --------------
// Changes the pixel format of a TBitmap.
//
// Replacement for delphi 3 TBitmap.PixelFormat setter.
// The returned TBitmap will always be a DIB.
//
// Note: Under Delphi 3.x this function will leak a palette handle each time it
//       converts a TBitmap to pf8bit format!
//       If possible, use SafeSetPixelFormat instead to avoid this.
//
// Parameters:
// Bitmap	The bitmap to modify.
// PixelFormat	The pixel format to convert to.
//

procedure SetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);
begin
  Bitmap.PixelFormat := PixelFormat;
end;

// ------------------
// SafeSetPixelFormat
// ------------------
// Changes the pixel format of a TBitmap but doesn't preserve the contents.
//
// Replacement for delphi 3 TBitmap.PixelFormat setter.
// The returned TBitmap will always be an empty DIB of the same size as the
// original bitmap.
//
// This function is used to avoid the palette handle leak that SetPixelFormat
// and TBitmap.PixelFormat suffers from.
//
// Parameters:
// Bitmap	The bitmap to modify.
// PixelFormat	The pixel format to convert to.

{$IFDEF VER11_PLUS}

procedure SafeSetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);
begin
  Bitmap.PixelFormat := PixelFormat;
end;
{$ELSE}

var
  pf8BitBitmap: TBitmap = nil;

procedure SafeSetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);
var
  Width,
    Height: integer;
begin
  if (PixelFormat = pf8bit) then
  begin
    // Solution to "TBitmap.PixelFormat := pf8bit" leak by Greg Chapman <glc@well.com>
    if (pf8BitBitmap = nil) then
    begin
      // Create a "template" bitmap
      // The bitmap is deleted in the finalization section of the unit.
      pf8BitBitmap := TBitmap.Create;
      // Convert template to pf8bit format
      // This will leak 1 palette handle, but only once
      pf8BitBitmap.PixelFormat := pf8Bit;
    end;
    // Store the size of the original bitmap
    Width := Bitmap.Width;
    Height := Bitmap.Height;
    // Convert to pf8bit format by copying template
    Bitmap.Assign(pf8BitBitmap);
    // Restore the original size
    Bitmap.Width := Width;
    Bitmap.Height := Height;
  end
  else
    // This is safe since only pf8bit leaks
    Bitmap.PixelFormat := PixelFormat;
end;
{$ENDIF}


////////////////////////////////////////////////////////////////////////////////
//
//			TDIB Class
//
//  These classes gives read and write access to TBitmap's pixel data
//  independantly of the Delphi version used.
//
////////////////////////////////////////////////////////////////////////////////
type
  TDIB = class(TObject)
  private
    FBitmap: TBitmap;
    FPixelFormat: TPixelFormat;
    function GetScanline(Row: integer): pointer; virtual; abstract;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat); virtual;
    property Scanline[Row: integer]: pointer read GetScanline;
    property Bitmap: TBitmap read FBitmap;
  end;

  TDIBReader = class(TDIB)
  protected
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat); override;
    destructor Destroy; override;
  end;

  TDIBWriter = class(TDIB)
  private
{$IFDEF PIXELFORMAT_TOO_SLOW}
    FDIBInfo: PBitmapInfo;
    FDIBBits: pointer;
    FDIBInfoSize: integer;
    FDIBBitsSize: longInt;
{$ENDIF}
  protected
    procedure CreateDIB;
    procedure FreeDIB;
    procedure NeedDIB;
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat); override;
    destructor Destroy; override;
    procedure UpdateBitmap;
  end;

////////////////////////////////////////////////////////////////////////////////

constructor TDIB.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
begin
  inherited Create;
  FBitmap := ABitmap;
  FPixelFormat := APixelFormat;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TDIBReader.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
begin
  inherited Create(ABitmap, APixelFormat);
  SetPixelFormat(FBitmap, FPixelFormat);
end;

destructor TDIBReader.Destroy;
begin
  inherited Destroy;
end;

function TDIBReader.GetScanline(Row: integer): pointer;
begin
{$ifndef FPC_TODO}
  Result := FBitmap.ScanLine[Row];
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////

constructor TDIBWriter.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
{$IFNDEF PIXELFORMAT_TOO_SLOW}
var
  SavePalette: HPalette;
{$ENDIF}
begin
  inherited Create(ABitmap, APixelFormat);
{$IFNDEF PIXELFORMAT_TOO_SLOW}
  SavePalette := FBitmap.ReleasePalette;
  try
    SafeSetPixelFormat(FBitmap, FPixelFormat);
  finally
    FBitmap.Palette := SavePalette;
  end;
{$ELSE PIXELFORMAT_TOO_SLOW}
  FDIBInfo := nil;
  FDIBBits := nil;
{$ENDIF PIXELFORMAT_TOO_SLOW}
end;

destructor TDIBWriter.Destroy;
begin
  UpdateBitmap;
  FreeDIB;
  inherited Destroy;
end;

function TDIBWriter.GetScanline(Row: integer): pointer;
begin
{$IFDEF PIXELFORMAT_TOO_SLOW}
  NeedDIB;

  if (FDIBBits = nil) then
    Error(sNoDIB);
  with FDIBInfo^.bmiHeader do
  begin
    if (Row < 0) or (Row >= FBitmap.Height) then
      raise EInvalidGraphicOperation.Create(SScanLine);
    GDIFlush;

    if biHeight > 0 then // bottom-up DIB
      Row := biHeight - Row - 1;
    Result := PByte(Cardinal(FDIBBits) + Cardinal(Row) * AlignBit(biWidth, biBitCount, 32));
  end;
{$ELSE PIXELFORMAT_TOO_SLOW}
{$ifdef LCL}
  Result := nil;  // ToDo: Find replacement of FBitmap.ScanLine for LCL
{$else LCL}
  Result := FBitmap.ScanLine[Row];
{$endif LCL}
{$ENDIF PIXELFORMAT_TOO_SLOW}
end;

procedure TDIBWriter.CreateDIB;
{$IFDEF PIXELFORMAT_TOO_SLOW}
var
  SrcColors,
    DstColors: WORD;

  procedure ByteSwapColors(var Colors; Count: Integer);
  // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
{$IFDEF WIN32}
  // From Delphi 3.02 graphics.pas
  // There is a bug in the ByteSwapColors from Delphi 3.0
  var
    SysInfo: TSystemInfo;
  begin
    GetSystemInfo(SysInfo);
    asm
          MOV   EDX, Colors
          MOV   ECX, Count
          DEC   ECX
          JS    @@END
          LEA   EAX, SysInfo
          CMP   [EAX].TSystemInfo.wProcessorLevel, 3
          JE    @@386
    @@1:  MOV   EAX, [EDX+ECX*4]
          BSWAP EAX
          SHR   EAX,8
          MOV   [EDX+ECX*4],EAX
          DEC   ECX
          JNS   @@1
          JMP   @@END
    @@386:
          PUSH  EBX
    @@2:  XOR   EBX,EBX
          MOV   EAX, [EDX+ECX*4]
          MOV   BH, AL
          MOV   BL, AH
          SHR   EAX,16
          SHL   EBX,8
          MOV   BL, AL
          MOV   [EDX+ECX*4],EBX
          DEC   ECX
          JNS   @@2
          POP   EBX
      @@END:
    end;
{$ENDIF WIN32}
{$IFDEF WIN64}
  // From Delphi XE2 Vcl.Graphics unit
  // Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
  var
    C: PDWORD;
    Color: DWORD;
    I: Integer;
  begin
    C := @Colors;
    I := 0;
    while I < Count do
    begin
      Color := C^;
      C^ := (Byte(Color shr 16) or (Word(Color) shr 8) shl 8) or
        (Byte(Word(Color)) shl 16);
      Inc(I);
      Inc(C);
    end;
{$ENDIF WIN64}
  end;
{$ENDIF PIXELFORMAT_TOO_SLOW}
begin
{$IFDEF PIXELFORMAT_TOO_SLOW}
  if (FBitmap.Handle = 0) then
    Error(sInvalidBitmap);

  FreeDIB;

  // Get header- and pixel data size
  InternalGetDIBSizes(FBitmap.Handle, FDIBInfoSize, FDIBBitsSize, FPixelFormat);

  // Allocate TBitmapInfo structure
  GetMem(FDIBInfo, FDIBInfoSize);
  try
    // Allocate pixel buffer
    FDIBBits := GlobalAllocPtr(GMEM_MOVEABLE, FDIBBitsSize);
    if (FDIBBits = nil) then
      raise EOutOfMemory.Create(sOutOfMemDIB);
    // Get pixel data
    if not (InternalGetDIB(FBitmap.Handle, FBitmap.Palette, FDIBInfo^, FDIBBits^, FPixelFormat)) then
      Error(sDIBCreate);

    if (FPixelFormat <= pf8bit) then
    begin
      // Find number of colors defined by palette
      if (FBitmap.Palette = 0) or
        (GetObject(FBitmap.Palette, sizeof(SrcColors), @SrcColors) = 0) or
        (SrcColors = 0) then
        exit;
      // Determine how many colors there are room for in DIB header
      DstColors := FDIBInfo^.bmiHeader.biClrUsed;
      if (DstColors = 0) then
        DstColors := 1 shl FDIBInfo^.bmiHeader.biBitCount;
      // Don't copy any more colors than there are room for
      if (DstColors <> 0) and (DstColors < SrcColors) then
        SrcColors := DstColors;

      // Copy all colors...
      GetPaletteEntries(FBitmap.Palette, 0, SrcColors, FDIBInfo^.bmiColors[0]);
      // ...and convert BGR to RGB
      ByteSwapColors(FDIBInfo^.bmiColors[0], SrcColors);

      // Finally zero any unused entried
      if (SrcColors < DstColors) then
        FillChar(pointer(LongInt(@FDIBInfo^.bmiColors) + SizeOf(TRGBQuad) * SrcColors)^,
          DstColors - SrcColors, 0);
    end;

  except
    FreeDIB;
    raise;
  end;
{$ENDIF PIXELFORMAT_TOO_SLOW}
end;

procedure TDIBWriter.FreeDIB;
begin
{$IFDEF PIXELFORMAT_TOO_SLOW}
  if (FDIBInfo <> nil) then
    FreeMem(FDIBInfo);
  if (FDIBBits <> nil) then
    GlobalFreePtr(FDIBBits);
  FDIBInfo := nil;
  FDIBBits := nil;
{$ENDIF}
end;

procedure TDIBWriter.NeedDIB;
begin
{$IFDEF PIXELFORMAT_TOO_SLOW}
  if (FDIBBits = nil) then
    CreateDIB;
{$ENDIF}
end;

// Convert the DIB created by CreateDIB back to a TBitmap

procedure TDIBWriter.UpdateBitmap;
{$IFDEF PIXELFORMAT_TOO_SLOW}
var
  Stream: TMemoryStream;
  FileSize: longInt;
  BitmapFileHeader: TBitmapFileHeader;
{$ENDIF}
begin
{$IFDEF PIXELFORMAT_TOO_SLOW}
  if (FDIBInfo = nil) or (FDIBBits = nil) then
    exit;
  Stream := TMemoryStream.Create;
  try
    // Make room in stream for a TBitmapInfo and pixel data
    FileSize := sizeof(TBitmapFileHeader) + FDIBInfoSize + FDIBBitsSize;
    Stream.SetSize(FileSize);
    // Initialize file header
    FillChar(BitmapFileHeader, sizeof(TBitmapFileHeader), 0);
    with BitmapFileHeader do
    begin
      bfType := $4D42; // 'BM' = Windows BMP signature
      bfSize := FileSize; // File size (not needed)
      bfOffBits := sizeof(TBitmapFileHeader) + FDIBInfoSize; // Offset of pixel data
    end;
    // Save file header
    Stream.Write(BitmapFileHeader, sizeof(TBitmapFileHeader));
    // Save TBitmapInfo structure
    Stream.Write(FDIBInfo^, FDIBInfoSize);
    // Save pixel data
    Stream.Write(FDIBBits^, FDIBBitsSize);

    // Rewind and load DIB into bitmap
    Stream.Position := 0;
    FBitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color Mapping
//
////////////////////////////////////////////////////////////////////////////////
type
  TColorLookup = class(TObject)
  private
    FColors: integer;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; virtual; abstract;
  public
    constructor Create(Palette: hPalette); virtual;
    property Colors: integer read FColors;
  end;

  //PRGBQuadArray = ^TRGBQuadArray; // From Delphi 3 graphics.pas
  TRGBQuadArray = array[Byte] of TRGBQuad; // From Delphi 3 graphics.pas

  //BGRArray = array[0..0] of TRGBTriple;
  //PBGRArray = ^BGRArray;

  PalArray = array[byte] of TPaletteEntry;
  PPalArray = ^PalArray;

  // TFastColorLookup implements a simple but reasonably fast generic color
  // mapper. It trades precision for speed by reducing the size of the color
  // space.
  // Using a class instead of inline code results in a speed penalty of
  // approx. 15% but reduces the complexity of the color reduction routines that
  // uses it. If bitmap to GIF conversion speed is really important to you, the
  // implementation can easily be inlined again.
  TInverseLookup = array[0..1 shl 15 - 1] of SmallInt;
  PInverseLookup = ^TInverseLookup;

  TFastColorLookup = class(TColorLookup)
  private
    FPaletteEntries: PPalArray;
    FInverseLookup: PInverseLookup;
  public
    constructor Create(Palette: hPalette); override;
    destructor Destroy; override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

  // TNetscapeColorLookup maps colors to the netscape 6*6*6 color cube.
  TNetscapeColorLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

constructor TColorLookup.Create(Palette: hPalette);
begin
  inherited Create;
end;

constructor TFastColorLookup.Create(Palette: hPalette);
var
  i: integer;
  InverseIndex: integer;
begin
  inherited Create(Palette);

  GetMem(FPaletteEntries, sizeof(TPaletteEntry) * 256);
  FColors := GetPaletteEntries(Palette, 0, 256, FPaletteEntries^);

  New(FInverseLookup);
  for i := low(TInverseLookup) to high(TInverseLookup) do
    FInverseLookup^[i] := -1;

  // Premap palette colors
  if (FColors > 0) then
    for i := 0 to FColors - 1 do
      with FPaletteEntries^[i] do
      begin
        InverseIndex := (peRed shr 3) or ((peGreen and $F8) shl 2) or ((peBlue and $F8) shl 7);
        if (FInverseLookup^[InverseIndex] = -1) then
          FInverseLookup^[InverseIndex] := i;
      end;
end;

destructor TFastColorLookup.Destroy;
begin
  if (FPaletteEntries <> nil) then
    FreeMem(FPaletteEntries);
  if (FInverseLookup <> nil) then
    Dispose(FInverseLookup);

  inherited Destroy;
end;

// Map color to arbitrary palette

function TFastColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
var
  i: integer;
  InverseIndex: integer;
  Delta,
    MinDelta,
    MinColor: integer;
begin
  // Reduce color space with 3 bits in each dimension
  InverseIndex := (Red shr 3) or ((Green and $F8) shl 2) or ((Blue and $F8) shl 7);

  if (FInverseLookup^[InverseIndex] <> -1) then
    Result := Byte(FInverseLookup^[InverseIndex])
  else
  begin
    // Sequential scan for nearest color to minimize euclidian distance
    MinDelta := 3 * (256 * 256);
    MinColor := 0;
    for i := 0 to FColors - 1 do
      with FPaletteEntries[i] do
      begin
        Delta := ABS(peRed - Red) + ABS(peGreen - Green) + ABS(peBlue - Blue);
        if (Delta < MinDelta) then
        begin
          MinDelta := Delta;
          MinColor := i;
        end;
      end;
    Result := Byte(MinColor);
    FInverseLookup^[InverseIndex] := MinColor;
  end;

  with FPaletteEntries^[ord(Result)] do
  begin
    R := peRed;
    G := peGreen;
    B := peBlue;
  end;
end;

constructor TNetscapeColorLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 6 * 6 * 6; // This better be true or something is wrong
end;

// Map color to netscape 6*6*6 color cube

function TNetscapeColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  R := (Red + 3) div 51;
  G := (Green + 3) div 51;
  B := (Blue + 3) div 51;
  Result := Byte(B + 6 * G + 36 * R);
  R := R * 51;
  G := G * 51;
  B := B * 51;
end;


////////////////////////////////////////////////////////////////////////////////
//
//			Dithering engine
//
////////////////////////////////////////////////////////////////////////////////
type
  TDitherEngine = class
  protected
    FDirection: integer;
    FColumn: integer;
    FLookup: TColorLookup;
    Width: integer;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); virtual;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; virtual;
    procedure NextLine; virtual;

    property Direction: integer read FDirection;
    property Column: integer read FColumn;
  end;

  // Note: TErrorTerm does only *need* to be 16 bits wide, but since
  // it is *much* faster to use native machine words (32 bit), we sacrifice
  // some bytes (a lot actually) to improve performance.
  TErrorTerm = Integer;
  TErrors = array[0..0] of TErrorTerm;
  PErrors = ^TErrors;

  TFloydSteinbergEngine = class(TDitherEngine)
  private
    ErrorsR,
      ErrorsG,
      ErrorsB: PErrors;
    ErrorR,
      ErrorG,
      ErrorB: PErrors;
    CurrentErrorR, // Current error or pixel value
      CurrentErrorG,
      CurrentErrorB,
      BelowErrorR, // Error for pixel below current
      BelowErrorG,
      BelowErrorB,
      BelowPrevErrorR, // Error for pixel below previous pixel
      BelowPrevErrorG,
      BelowPrevErrorB: TErrorTerm;

  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
    procedure NextLine; override;
  end;

constructor TDitherEngine.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create;

  FLookup := Lookup;
  Width := AWidth;

  FDirection := 1;
  FColumn := 0;
end;

function TDitherEngine.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  // Map color to palette
  Result := FLookup.Lookup(Red, Green, Blue, R, G, B);
  inc(FColumn, FDirection);
end;

procedure TDitherEngine.NextLine;
begin
  FDirection := -FDirection;
  if (FDirection = 1) then
    FColumn := 0
  else
    FColumn := Width - 1;
end;

constructor TFloydSteinbergEngine.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);

  // The Error arrays has (columns + 2) entries; the extra entry at
  // each end saves us from special-casing the first and last pixels.
  // We can get away with a single array (holding one row's worth of errors)
  // by using it to store the current row's errors at pixel columns not yet
  // processed, but the next row's errors at columns already processed.  We
  // need only a few extra variables to hold the errors immediately around the
  // current column.  (If we are lucky, those variables are in registers, but
  // even if not, they're probably cheaper to access than array elements are.)
  GetMem(ErrorsR, sizeof(TErrorTerm) * (Width + 2));
  GetMem(ErrorsG, sizeof(TErrorTerm) * (Width + 2));
  GetMem(ErrorsB, sizeof(TErrorTerm) * (Width + 2));
  FillChar(ErrorsR^, sizeof(TErrorTerm) * (Width + 2), 0);
  FillChar(ErrorsG^, sizeof(TErrorTerm) * (Width + 2), 0);
  FillChar(ErrorsB^, sizeof(TErrorTerm) * (Width + 2), 0);
  ErrorR := ErrorsR;
  ErrorG := ErrorsG;
  ErrorB := ErrorsB;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorR;
  BelowErrorR := CurrentErrorR;
  BelowErrorG := CurrentErrorR;
  BelowErrorB := CurrentErrorR;
  BelowPrevErrorR := CurrentErrorR;
  BelowPrevErrorG := CurrentErrorR;
  BelowPrevErrorB := CurrentErrorR;
end;

destructor TFloydSteinbergEngine.Destroy;
begin
  FreeMem(ErrorsR);
  FreeMem(ErrorsG);
  FreeMem(ErrorsB);
  inherited Destroy;
end;

{$IFOPT R+}
{$DEFINE R_PLUS}
{$RANGECHECKS OFF}
{$ENDIF}

function TFloydSteinbergEngine.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
var
  BelowNextError: TErrorTerm;
  Delta: TErrorTerm;
begin
  CurrentErrorR := Red + (CurrentErrorR + ErrorR[FDirection] + 8) div 16;
  if (CurrentErrorR < 0) then
    CurrentErrorR := 0
  else if (CurrentErrorR > 255) then
    CurrentErrorR := 255;

  CurrentErrorG := Green + (CurrentErrorG + ErrorG[FDirection] + 8) div 16;
  if (CurrentErrorG < 0) then
    CurrentErrorG := 0
  else if (CurrentErrorG > 255) then
    CurrentErrorG := 255;

  CurrentErrorB := Blue + (CurrentErrorB + ErrorB[FDirection] + 8) div 16;
  if (CurrentErrorB < 0) then
    CurrentErrorB := 0
  else if (CurrentErrorB > 255) then
    CurrentErrorB := 255;

  // Map color to palette
  Result := inherited Dither(CurrentErrorR, CurrentErrorG, CurrentErrorB, R, G, B);

  // Propagate Floyd-Steinberg error terms.
  // Errors are accumulated into the error arrays, at a resolution of
  // 1/16th of a pixel count.  The error at a given pixel is propagated
  // to its not-yet-processed neighbors using the standard F-S fractions,
  //		...	(here)	7/16
  //		3/16	5/16	1/16
  // We work left-to-right on even rows, right-to-left on odd rows.

  // Red component
  CurrentErrorR := CurrentErrorR - R;
  BelowNextError := CurrentErrorR; // Error * 1

  Delta := CurrentErrorR * 2;
  CurrentErrorR := CurrentErrorR + Delta;
  ErrorR[0] := BelowPrevErrorR + CurrentErrorR; // Error * 3

  CurrentErrorR := CurrentErrorR + Delta;
  BelowPrevErrorR := BelowErrorR + CurrentErrorR; // Error * 5

  BelowErrorR := BelowNextError; // Error * 1

  CurrentErrorR := CurrentErrorR + Delta; // Error * 7

  // Green component
  CurrentErrorG := CurrentErrorG - G;
  BelowNextError := CurrentErrorG; // Error * 1

  Delta := CurrentErrorG * 2;
  CurrentErrorG := CurrentErrorG + Delta;
  ErrorG[0] := BelowPrevErrorG + CurrentErrorG; // Error * 3

  CurrentErrorG := CurrentErrorG + Delta;
  BelowPrevErrorG := BelowErrorG + CurrentErrorG; // Error * 5

  BelowErrorG := BelowNextError; // Error * 1

  CurrentErrorG := CurrentErrorG + Delta; // Error * 7

  // Blue component
  CurrentErrorB := CurrentErrorB - B;
  BelowNextError := CurrentErrorB; // Error * 1

  Delta := CurrentErrorB * 2;
  CurrentErrorB := CurrentErrorB + Delta;
  ErrorB[0] := BelowPrevErrorB + CurrentErrorB; // Error * 3

  CurrentErrorB := CurrentErrorB + Delta;
  BelowPrevErrorB := BelowErrorB + CurrentErrorB; // Error * 5

  BelowErrorB := BelowNextError; // Error * 1

  CurrentErrorB := CurrentErrorB + Delta; // Error * 7

  // Move on to next column
  if (FDirection = 1) then
  begin
    inc(ErrorR);
    inc(ErrorG);
    inc(ErrorB);
  end
  else
  begin
    dec(ErrorR);
    dec(ErrorG);
    dec(ErrorB);
  end;
end;
{$IFDEF R_PLUS}
{$RANGECHECKS ON}
{$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
{$DEFINE R_PLUS}
{$RANGECHECKS OFF}
{$ENDIF}

procedure TFloydSteinbergEngine.NextLine;
begin
  ErrorR[0] := BelowPrevErrorR;
  ErrorG[0] := BelowPrevErrorG;
  ErrorB[0] := BelowPrevErrorB;

  // Note: The optimizer produces better code for this construct:
  //   a := 0; b := a; c := a;
  // compared to this construct:
  //   a := 0; b := 0; c := 0;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorG;
  BelowErrorR := CurrentErrorG;
  BelowErrorG := CurrentErrorG;
  BelowErrorB := CurrentErrorG;
  BelowPrevErrorR := CurrentErrorG;
  BelowPrevErrorG := CurrentErrorG;
  BelowPrevErrorB := CurrentErrorG;

  inherited NextLine;

  if (FDirection = 1) then
  begin
    ErrorR := ErrorsR;
    ErrorG := ErrorsG;
    ErrorB := ErrorsB;
  end
  else
  begin
    ErrorR := @ErrorsR[Width + 1];
    ErrorG := @ErrorsG[Width + 1];
    ErrorB := @ErrorsB[Width + 1];
  end;
end;
{$IFDEF R_PLUS}
{$RANGECHECKS ON}
{$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Engine
//
////////////////////////////////////////////////////////////////////////////////
//  Adapted from Earl F. Glynn's ColorQuantizationLibrary, March 1998
////////////////////////////////////////////////////////////////////////////////
type
  TOctreeNode = class; // Forward definition so TReducibleNodes can be declared

  TReducibleNodes = array[0..7] of TOctreeNode;

  TOctreeNode = class(TObject)
  public
    IsLeaf: Boolean;
    PixelCount: integer;
    RedSum: integer;
    GreenSum: integer;
    BlueSum: integer;
    Next: TOctreeNode;
    Child: TReducibleNodes;

    constructor Create(Level: integer; ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);
    destructor Destroy; override;
  end;

  TColorQuantizer = class(TObject)
  private
    FTree: TOctreeNode;
    FLeafCount: integer;
    FReducibleNodes: TReducibleNodes;
    FMaxColors: integer;
    FColorBits: integer;

  protected
    procedure AddColor(var Node: TOctreeNode; r, g, b: byte; ColorBits: integer;
      Level: integer; var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(const Node: TOctreeNode;
      var RGBQuadArray: TRGBQuadArray; var Index: integer);
    procedure ReduceTree(ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);

  public
    constructor Create(MaxColors: integer; ColorBits: integer);
    destructor Destroy; override;

    procedure GetColorTable(var RGBQuadArray: TRGBQuadArray);
    function ProcessImage(const DIB: TDIBReader): boolean;

    property ColorCount: integer read FLeafCount;
  end;

constructor TOctreeNode.Create(Level: integer; ColorBits: integer;
  var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
var
  i: integer;
begin
  PixelCount := 0;
  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  for i := Low(Child) to High(Child) do
    Child[i] := nil;

  IsLeaf := (Level = ColorBits);
  if (IsLeaf) then
  begin
    Next := nil;
    inc(LeafCount);
  end
  else
  begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := self;
  end;
end;

destructor TOctreeNode.Destroy;
var
  i: integer;
begin
  for i := High(Child) downto Low(Child) do
    Child[i].Free;
end;

constructor TColorQuantizer.Create(MaxColors: integer; ColorBits: integer);
var
  i: integer;
begin
  ASSERT(ColorBits <= 8, 'ColorBits must be 8 or less');

  FTree := nil;
  FLeafCount := 0;

  // Initialize all nodes even though only ColorBits+1 of them are needed
  for i := Low(FReducibleNodes) to High(FReducibleNodes) do
    FReducibleNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := ColorBits;
end;

destructor TColorQuantizer.Destroy;
begin
  if (FTree <> nil) then
    DeleteTree(FTree);
end;

procedure TColorQuantizer.GetColorTable(var RGBQuadArray: TRGBQuadArray);
var
  Index: integer;
begin
  Index := 0;
  GetPaletteColors(FTree, RGBQuadArray, Index);
end;

// Handles passed to ProcessImage should refer to DIB sections, not DDBs.
// In certain cases, specifically when it's called upon to process 1, 4, or
// 8-bit per pixel images on systems with palettized display adapters,
// ProcessImage can produce incorrect results if it's passed a handle to a
// DDB.

function TColorQuantizer.ProcessImage(const DIB: TDIBReader): boolean;
var
  i,
    j: integer;
  ScanLine: pointer;
  Pixel: PRGBTriple;
begin
  Result := True;

  for j := 0 to DIB.Bitmap.Height - 1 do
  begin
    Scanline := DIB.Scanline[j];
    Pixel := ScanLine;
    for i := 0 to DIB.Bitmap.Width - 1 do
    begin
      with Pixel^ do
        AddColor(FTree, rgbtRed, rgbtGreen, rgbtBlue,
          FColorBits, 0, FLeafCount, FReducibleNodes);

      while FLeafCount > FMaxColors do
        ReduceTree(FColorbits, FLeafCount, FReducibleNodes);
      inc(Pixel);
    end;
  end;
end;

procedure TColorQuantizer.AddColor(var Node: TOctreeNode; r, g, b: byte;
  ColorBits: integer; Level: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
const
  Mask: array[0..7] of BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);
var
  Index: integer;
  Shift: integer;
begin
  // If the node doesn't exist, create it.
  if (Node = nil) then
    Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  if (Node.IsLeaf) then
  begin
    inc(Node.PixelCount);
    inc(Node.RedSum, r);
    inc(Node.GreenSum, g);
    inc(Node.BlueSum, b);
  end
  else
  begin
    // Recurse a level deeper if the node is not a leaf.
    Shift := 7 - Level;

    Index := (((r and mask[Level]) shr Shift) shl 2) or
      (((g and mask[Level]) shr Shift) shl 1) or
      ((b and mask[Level]) shr Shift);
    AddColor(Node.Child[Index], r, g, b, ColorBits, Level + 1, LeafCount, ReducibleNodes);
  end;
end;

procedure TColorQuantizer.DeleteTree(var Node: TOctreeNode);
var
  i: integer;
begin
  for i := High(TReducibleNodes) downto Low(TReducibleNodes) do
    if (Node.Child[i] <> nil) then
      DeleteTree(Node.Child[i]);

  Node.Free;
  Node := nil;
end;

procedure TColorQuantizer.GetPaletteColors(const Node: TOctreeNode;
  var RGBQuadArray: TRGBQuadArray; var Index: integer);
var
  i: integer;
begin
  if (Node.IsLeaf) then
  begin
    with RGBQuadArray[Index] do
    begin
      if (Node.PixelCount <> 0) then
      begin
        rgbRed := BYTE(Node.RedSum div Node.PixelCount);
        rgbGreen := BYTE(Node.GreenSum div Node.PixelCount);
        rgbBlue := BYTE(Node.BlueSum div Node.PixelCount);
      end
      else
      begin
        rgbRed := 0;
        rgbGreen := 0;
        rgbBlue := 0;
      end;
      rgbReserved := 0;
    end;
    inc(Index);
  end
  else
  begin
    for i := Low(Node.Child) to High(Node.Child) do
      if (Node.Child[i] <> nil) then
        GetPaletteColors(Node.Child[i], RGBQuadArray, Index);
  end;
end;

procedure TColorQuantizer.ReduceTree(ColorBits: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
var
  RedSum,
    GreenSum,
    BlueSum: integer;
  Children: integer;
  i: integer;
  Node: TOctreeNode;
begin
  // Find the deepest level containing at least one reducible node
  i := Colorbits - 1;
  while (i > 0) and (ReducibleNodes[i] = nil) do
    dec(i);

  // Reduce the node most recently added to the list at level i.
  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  Children := 0;

  for i := Low(ReducibleNodes) to High(ReducibleNodes) do
    if (Node.Child[i] <> nil) then
    begin
      inc(RedSum, Node.Child[i].RedSum);
      inc(GreenSum, Node.Child[i].GreenSum);
      inc(BlueSum, Node.Child[i].BlueSum);
      inc(Node.PixelCount, Node.Child[i].PixelCount);
      Node.Child[i].Free;
      Node.Child[i] := nil;
      inc(Children);
    end;

  Node.IsLeaf := TRUE;
  Node.RedSum := RedSum;
  Node.GreenSum := GreenSum;
  Node.BlueSum := BlueSum;
  dec(LeafCount, Children - 1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Wrapper
//
////////////////////////////////////////////////////////////////////////////////
//	Adapted from Earl F. Glynn's PaletteLibrary, March 1998
////////////////////////////////////////////////////////////////////////////////

// Wrapper for internal use - uses TDIBReader for bitmap access

function doCreateOptimizedPaletteForSingleBitmap(const DIB: TDIBReader;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  SystemPalette: HPalette;
  ColorQuantizer: TColorQuantizer;
  i: integer;
  LogicalPalette: TMaxLogPalette;
  RGBQuadArray: TRGBQuadArray;
  Offset: integer;
begin
  LogicalPalette.palVersion := $0300;
  LogicalPalette.palNumEntries := Colors;

  if (Windows) then
  begin
    // Get the windows 20 color system palette
    SystemPalette := GetStockObject(DEFAULT_PALETTE);
    GetPaletteEntries(SystemPalette, 0, 10, LogicalPalette.palPalEntry[0]);
    GetPaletteEntries(SystemPalette, 10, 10, LogicalPalette.palPalEntry[245]);
    Colors := 236;
    Offset := 10;
    LogicalPalette.palNumEntries := 256;
  end
  else
    Offset := 0;

  // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
  // use ColorBits = 8.
  ColorQuantizer := TColorQuantizer.Create(Colors, ColorBits);
  try
    ColorQuantizer.ProcessImage(DIB);
    ColorQuantizer.GetColorTable(RGBQuadArray);
  finally
    ColorQuantizer.Free;
  end;

  for i := 0 to Colors - 1 do
    with LogicalPalette.palPalEntry[i + Offset] do
    begin
      peRed := RGBQuadArray[i].rgbRed;
      peGreen := RGBQuadArray[i].rgbGreen;
      peBlue := RGBQuadArray[i].rgbBlue;
      peFlags := RGBQuadArray[i].rgbReserved;
    end;
  Result := CreatePalette(pLogPalette(@LogicalPalette)^);
end;

function CreateOptimizedPaletteForSingleBitmap(const Bitmap: TBitmap;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  DIB: TDIBReader;
begin
  DIB := TDIBReader.Create(Bitmap, pf24bit);
  try
    Result := doCreateOptimizedPaletteForSingleBitmap(DIB, Colors, ColorBits, Windows);
  finally
    DIB.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color reduction
//
////////////////////////////////////////////////////////////////////////////////
{$IFOPT R+}
{$DEFINE R_PLUS}
{$RANGECHECKS OFF}
{$ENDIF}

function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
  DitherMode: TDitherMode): TBitmap;
var
  Palette: hPalette;
  ColorLookup: TColorLookup;
  Ditherer: TDitherEngine;
  Row: Integer;
  DIBResult: TDIBWriter;
  DIBSource: TDIBReader;
  SrcScanLine,
    Src: PRGBTriple;
  DstScanLine,
    Dst: PByte;
  BGR: TRGBTriple;
{$IFDEF DEBUG_DITHERPERFORMANCE}
  TimeStart,
    TimeStop: DWORD;
{$ENDIF}


begin
{$IFDEF DEBUG_DITHERPERFORMANCE}
  timeBeginPeriod(5);
  TimeStart := timeGetTime;
{$ENDIF}

  Result := TBitmap.Create;
  try

    if (ColorReduction = rmNone) then
    begin
      Result.Assign(Bitmap);
      SetPixelFormat(Result, pf24bit);
      exit;
    end;

    // Set bitmap width and height
    Result.Width := Bitmap.Width;
    Result.Height := Bitmap.Height;

    // Set the bitmap pixel format
    SafeSetPixelFormat(Result, pf8bit);
    Result.Palette := 0;

    ColorLookup := nil;
    Ditherer := nil;
    DIBResult := nil;
    DIBSource := nil;
    Palette := 0;
    try // Protect above resources

      // Dithering and color mapper only supports 24 bit bitmaps,
      // so we have convert the source bitmap to the appropiate format.
      DIBSource := TDIBReader.Create(Bitmap, pf24bit);

      try
        // Create a palette based on current options
        case (ColorReduction) of
          rmQuantizeWindows:
            Palette := CreateOptimizedPaletteForSingleBitmap(Bitmap, 256, 8, True);
          rmNetscape:
            Palette := WebPalette;
          rmMyPalette:
            Palette := CopyPalette(ThePalette);
          rmWindows20:
            Palette := GetStockObject(DEFAULT_PALETTE);
        else
          exit;
        end;

        Result.Palette := Palette;

        case (ColorReduction) of
          // For some strange reason my fast and dirty color lookup
          // is more precise that Windows GetNearestPaletteIndex...
          rmNetscape:
            ColorLookup := TNetscapeColorLookup.Create(Palette);
        else
          ColorLookup := TFastColorLookup.Create(Palette);
        end;

        // Nothing to do if palette doesn't contain any colors
        if (ColorLookup.Colors = 0) then
          exit;

        // Create a ditherer based on current options
        case (DitherMode) of
          dmNearest:
            Ditherer := TDitherEngine.Create(Bitmap.Width, ColorLookup);
          dmFloydSteinberg:
            Ditherer := TFloydSteinbergEngine.Create(Bitmap.Width, ColorLookup);
        else
          exit;
        end;

        // The processed bitmap is returned in pf8bit format
        DIBResult := TDIBWriter.Create(Result, pf8bit);

        // Process the image
        Row := 0;
        while (Row < Bitmap.Height) do
        begin
          SrcScanline := DIBSource.ScanLine[Row];
          DstScanline := DIBResult.ScanLine[Row];
          Src := PtrAdd(SrcScanLine, Ditherer.Column * sizeof(TRGBTriple));
          Dst := PtrAdd(DstScanLine, Ditherer.Column);

          while (Ditherer.Column < Ditherer.Width) and (Ditherer.Column >= 0) do
          begin
            BGR := Src^;
            // Dither and map a single pixel
            Dst^ := Ditherer.Dither(BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue,
              BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue);

            inc(Src, Ditherer.Direction);
            inc(Dst, Ditherer.Direction);
          end;

          Inc(Row);
          Ditherer.NextLine;
        end;
      except
        Result.ReleasePalette;
        if (Palette <> 0) then
          DeleteObject(Palette);
        raise;
      end;
    finally
      if (ColorLookup <> nil) then
        ColorLookup.Free;
      if (Ditherer <> nil) then
        Ditherer.Free;
      if (DIBResult <> nil) then
        DIBResult.Free;
      if (DIBSource <> nil) then
        DIBSource.Free;
    end;
  except
    Result.Free;
    raise;
  end;

{$IFDEF DEBUG_DITHERPERFORMANCE}
  TimeStop := timeGetTime;
  ShowMessage(format('Dithered %d pixels in %d mS, Rate %d pixels/mS (%d pixels/S)',
    [Bitmap.Height * Bitmap.Width, TimeStop - TimeStart,
    MulDiv(Bitmap.Height, Bitmap.Width, TimeStop - TimeStart + 1),
      MulDiv(Bitmap.Height, Bitmap.Width * 1000, TimeStop - TimeStart + 1)]));
  timeEndPeriod(5);
{$ENDIF}
end;
{$IFDEF R_PLUS}
{$RANGECHECKS ON}
{$UNDEF R_PLUS}
{$ENDIF}

function GetBitmap(Source: TPersistent): TBitmap;
var
  PixelFormat: TPixelFormat;
  FBitmap: TBitmap;
  ColorReduction: TColorReduction;
  DitherMode: TDitherMode;

begin
  Result := nil;
  if (Source is TBitmap) then {should always be}
  begin
    if (TBitmap(Source).Empty) then
      exit;
    PixelFormat := GetPixelFormat(TBitmap(Source));
    if (PixelFormat > pfDevice) then
    begin
      if ColorBits >= 8 then
        ColorReduction := rmMyPalette
      else
        ColorReduction := rmWindows20;
      DitherMode := dmFloydSteinberg;
      // Convert image to 8 bits/pixel or less
      FBitmap := ReduceColors(TBitmap(Source), ColorReduction, DitherMode);
    end
    else
    begin
      // Create new bitmap and copy
      FBitmap := TBitmap.Create;
      FBitmap.Assign(TBitmap(Source));
    end;
    Result := FBitmap;
  end;
end;

end.
