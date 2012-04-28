{
Version   11.2
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2012 by Bernd Gabriel

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

unit GDIPL2A;

interface

uses
  Windows, ActiveX, SysUtils, Graphics, Classes,
  HtmlGlobals;

var
  GDIPlusActive: boolean;

type
  TGpImage = class(TObject)
  private
    fHandle: Pointer;
    fWidth, fHeight: integer;
    fFilename: string;
    function GetHeight: integer;
    function GetWidth: integer;
  public
    constructor Create(Filename: ThtString; TmpFile: boolean = False); overload;
    constructor Create(IStr: IStream); overload;
    constructor Create(Stream: TStream); overload;
    destructor Destroy; override;
    function GetBitmap: TBitmap;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
  end;

  TGpGraphics = class;

  TGpBitmap = class(TGpImage)
  public
    constructor Create(W, H: integer); overload;
    constructor Create(IStr: IStream); overload;
    constructor Create(Stream: TStream); overload;
    constructor Create(W, H: integer; Graphics: TGpGraphics); overload;
    function GetPixel(X, Y: integer): DWord;
    procedure SetPixel(X, Y: integer; Color: DWord);
  end;

  TGpGraphics = class(TObject)
  private
    fGraphics: Pointer;
    procedure DrawSmallStretchedImage(Image: TGPImage; X, Y, Width, Height: Integer);
  public
    constructor Create(Handle: HDC); overload;
    constructor Create(Image: TGpImage); overload;
    destructor Destroy; override;
    procedure DrawImage(Image: TGPImage; X, Y: Integer); overload;
    procedure DrawImage(Image: TGPImage; X, Y, Width, Height: Integer); overload;
    procedure DrawImage(Image: TGpImage; x, y, srcx, srcy, srcwidth, srcheight: integer); overload;
    procedure DrawImage(Image: TGpImage; dx, dy, dw, dh, sx, sy, sw, sh: integer); overload;
    procedure Clear(Color: Cardinal);
    procedure ScaleTransform(sx, sy: Single);
  end;

procedure CheckInitGDIPlus;
procedure CheckExitGDIPlus;

implementation

const
  GdiPlusLib = 'GdiPlus.dll';

type
  ARGB = DWORD;
  TARGB = ARGB;
  GpStatus = Integer;
  PixelFormat = Integer;
  NotificationHookProc = function (out token : ULONG) : GpStatus stdcall;
  NotificationUnhookProc = procedure (token : ULONG) stdcall;

  DebugEventLevel = Integer;
  DebugEventProc = procedure (level : DebugEventLevel; message_ : PAnsiChar) stdcall;
  GdiplusStartupInput = packed record
    GdiplusVersion : DWORD;  // Must be 1  (or 2 for the Ex version)
    DebugEventCallback : DebugEventProc; // Ignored on free builds
    SuppressBackgroundThread : BOOL; // FALSE unless you're prepared to call
                                       // the hook/unhook functions properly
    SuppressExternalCodecs : BOOL; // FALSE unless you want GDI+ only to use
                                       // its internal image codecs.
  end;
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;
  GdiplusStartupOutput = record
    NotificationHook : NotificationHookProc;
    NotificationUnhook : NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;
  PGpGraphics = Pointer;
  PGpImage = Pointer;
  PGpBitmap = Pointer;
  GpUnit = Integer; //enumeration
  GpMatrixOrder = Integer;
  InterpolationMode = Integer;
  TInterpolationMode = InterpolationMode;
  PGpImageAttributes = Pointer;
  ImageAbort = function : BOOL stdcall;
  DrawImageAbort = ImageAbort;
  EGDIPlus = class(Exception);

  //TRectF = record
  //  X: Single;
  //  Y: Single;
  //  Width: Single;
  //  Height: Single;
  //end;

  //ImageCodecInfo = packed record
  //  Clsid: TGUID;
  //  FormatID: TGUID;
  //  CodecName: PWCHAR;
  //  DllName: PWCHAR;
  //  FormatDescription: PWCHAR;
  //  FilenameExtension: PWCHAR;
  //  MimeType: PWCHAR;
  //  Flags: DWORD;
  //  Version: DWORD;
  //  SigCount: DWORD;
  //  SigSize: DWORD;
  //  SigPattern: PBYTE;
  //  SigMask: PBYTE;
  //end;
  //TImageCodecInfo = ImageCodecInfo;
  //PImageCodecInfo = ^TImageCodecInfo;

var
{$IFNDEF NoGDIPlus}
  GdiplusStartup: function(out Token: ULONG;
    const Input : PGdiplusStartupInput;
    const Output: PGdiplusStartupOutput): GpStatus stdcall;
  GdiplusShutdown: procedure(Token: ULONG) stdcall;
  GdipDrawImageI: function(Graphics : PGpGraphics; Image : PGpImage; X, Y: Integer): GpStatus stdcall;
  GdipCreateHBITMAPFromBitmap: function(bitmap: PGpBitmap; out hbmReturn: HBITMAP;
    background: TARGB): GpStatus stdcall;
  GdipGetInterpolationMode: function(graphics: PGpGraphics; var interpolationMode: TInterpolationMode): GpStatus stdcall;
{$ENDIF$}
  GdipDeleteGraphics: function(Graphics: PGpGraphics): GpStatus stdcall;
  GdipCreateFromHDC: function(hdc: HDC; out Graphics: PGpGraphics): GpStatus stdcall;
  GdipDrawImageRectI: function(Graphics : PGpGraphics; Image : PGpImage; X, Y, Width, Height: Integer): GpStatus stdcall;
  GdipLoadImageFromFile: function(const FileName: PWideChar; out Image: PGpImage): GpStatus stdcall;
  GdipLoadImageFromStream: function(stream: ISTREAM;
    out image: PGpImage): GpStatus stdcall;
  GdipCreateBitmapFromStream: function(stream: ISTREAM; out bitmap: PGpBitmap): GpStatus stdcall;
  GdipDisposeImage: function(Image: PGpImage): GpStatus stdcall;
  GdipGetImageWidth: function(Image: PGpImage; out Width: Integer): GpStatus stdcall;

  GdipGetImageHeight: function(Image: PGpImage; out Height: Integer): GpStatus stdcall;
  GdipGetImageGraphicsContext: function(Image: PGpImage; out graphics: PGpGraphics): GpStatus stdcall;
  GdipGraphicsClear: function(Graphics: PGpGraphics; Color: TARGB): GpStatus stdcall;
  GdipCreateBitmapFromScan0: function(width: Integer; height: Integer;
    stride: Integer; pixelformat: PixelFormat; scan0: Pointer;
    out bitmap: PGpBitmap): GpStatus stdcall;
  GdipDrawImagePointRect: function(graphics: PGpGraphics; image: PGpImage;
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: GpUnit): GpStatus stdcall;
  GdipScaleWorldTransform: function(graphics: PGpGraphics; sx: Single; sy: Single;
    order: GpMatrixOrder): GpStatus stdcall;
  GdipCreateBitmapFromGraphics: function(width, height: Integer;
    Graphics: PGpGraphics; out Bitmap: PGpBitmap): GpStatus; stdcall;
  GdipBitmapGetPixel: function(bitmap : PGpBitmap; x, y: Integer; out color: TARGB): GpStatus stdcall;
  GdipDrawImageRectRectI: function(graphics : PGpGraphics; image : PGpImage;
    dstx, dsty, dstwidth, dstheight, srcx, srcy, srcwidth, srcheight,
    srcUnit : GpUnit; imageAttributes: PGpImageAttributes;
    callback: Pointer; callbackData: Pointer): GpStatus; stdcall;

  GdipSetInterpolationMode: function(graphics : PGpGraphics; interpolationMode: InterpolationMode): GpStatus stdcall;
  GdipBitmapSetPixel: function(bitmap : PGpBitmap; x, y: Integer; color: TARGB): GpStatus stdcall;

var
  Err: GpStatus;

//-- BG ---------------------------------------------------------- 01.04.2012 --
function IStreamFromStream(Stream: TStream): IStream;
// thanks to Sérgio Alexandre for this method.
var
  Handle: THandle;
begin
  Handle := GlobalAlloc(GPTR, Stream.Size);
  if Handle <> 0 then
  begin
    Stream.Read(Pointer(Handle)^, Stream.Size);
    CreateStreamOnHGlobal(Handle, True, Result);
  end;
end;

{ TGpGraphics }

constructor TGpGraphics.Create(Handle: HDC);
var
  err: GpStatus;
begin
  inherited Create;
  err := GdipCreateFromHDC(Handle, fGraphics);
  if err <> 0 then
    raise EGDIPlus.CreateFmt('Can''t Create Graphics. GDI error %d', [err]);
end;

constructor TGpGraphics.Create(Image: TGpImage);
var
  err: integer;
begin
  inherited Create;
  err := GdipGetImageGraphicsContext(image.fHandle, fgraphics);
  if err <> 0 then
    raise EGDIPlus.CreateFmt('Can''t Create Graphics. GDI error %d', [err]);
end;

destructor TGpGraphics.Destroy;
begin
  if fGraphics <> nil then
    GdipDeleteGraphics(fGraphics);
  inherited;
end;

procedure TGpGraphics.DrawImage(Image: TGPImage; X, Y, Width, Height: Integer);
begin
  if ((Image.Width <= 10) and (Width > Image.Width)) or
    ((Image.Height <= 10) and (Height > Image.Height)) then
    DrawSmallStretchedImage(Image, X, Y, Width, Height)
  else
    GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Width, Height);
end;

procedure TGpGraphics.DrawSmallStretchedImage(Image: TGPImage; X, Y, Width, Height: Integer);
{when a small image is getting enlarged, add a row and column to it copying
 the last row/column to the new row/column.  This gives much better interpolation.}
const
  NearestNeighbor = 5;
var
  g1, g2: TGpGraphics;
  BM1, BM2: TGpBitmap;
  W, H: integer;
begin
  W := Image.Width + 1; {new dimensions}
  H := Image.Height + 1;
  BM1 := TGpBitmap.Create(W, H); {new bitmap with extra row and column}
  try
    g1 := TGpGraphics.Create(BM1);
    try
      {draw the original image to BM1}
      g1.DrawImage(Image, 0, 0);

      {copy the additional column inside BM1}
      g1.DrawImage(BM1, W - 1,     0, 1, H - 1,  W - 2,     0, 1, H - 1);

      {copy the additional row incl. additional pixel inside BM1}
      g1.DrawImage(BM1,     0, H - 1, W,     1,      0, H - 2, W,     1);

      BM2 := TGpBitmap.Create(Width, Height);
      try
        g2 := TGpGraphics.Create(BM2);
        try
          // BG, 25.12.2011: Issue 59: Image scaling error
          // - With InterpolationMode = NearestNeighbor only 50% of the stretch is visible.
          // - Not setting it stretches the small image to the full width/height.

          // GdipSetInterpolationMode(g2.fGraphics, NearestNeighbor);
          {now draw the image stretched where needed}
          g2.DrawImage(BM1, 0, 0, Width, Height, 0, 0, Image.Width, Image.Height);
          DrawImage(BM2, X, Y); {now draw the stretched image}
        finally
          g2.Free;
        end;
      finally
        BM2.Free;
      end;
    finally
      g1.Free;
    end;
  finally
    BM1.Free;
  end;
end;

procedure TGpGraphics.DrawImage(Image: TGPImage; X, Y: Integer);
begin
  GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Image.Width, Image.Height);
end;

procedure TGPGraphics.DrawImage(Image: TGpImage; x, y,
  srcx, srcy, srcwidth, srcheight: integer);
const
  UnitPixel = 2;
begin
  GdipDrawImagePointRect(fGraphics, Image.fHandle, x, y,
    srcx, srcy, srcwidth, srcheight, UnitPixel);
end;

procedure TGPGraphics.DrawImage(Image: TGpImage; dx, dy, dw, dh, sx, sy, sw, sh: integer);
const
  UnitPixel = 2;
begin
  GdipDrawImageRectRectI(fGraphics, Image.fHandle, dx, dy, dw, dh,
    sx, sy, sw, sh, UnitPixel, nil, nil, nil);

end;

procedure TGpGraphics.Clear(Color: Cardinal);
begin
  GdipGraphicsClear(fGraphics, Color);
end;

procedure TGPGraphics.ScaleTransform(sx, sy: Single);
const
  MatrixOrderPrepend = 0;
begin
  GdipScaleWorldTransform(fGraphics, sx, sy, MatrixOrderPrepend);
end;

{ TGpImage }

constructor TGpImage.Create(Filename: ThtString; TmpFile: boolean = False);
var
  err: Integer;
begin
  inherited Create;
  err := GdipLoadImageFromFile(PWideChar(FileName), fHandle);
  if err <> 0 then
    if GetLastError = 2 then
      raise EGDIPlus.CreateFmt('Image file "%s" not found. GDI error %d', [FileName, err])
    else
      raise EGDIPlus.CreateFmt('Can''t load image file "%s". GDI error %d', [FileName, err]);
  if TmpFile then
    fFilename := Filename;
end;

constructor TGpImage.Create(IStr: IStream);
var
  err: Integer;
begin
  inherited Create;
  err := GdipLoadImageFromStream(IStr, fHandle);
  if err <> 0 then
    raise EGDIPlus.CreateFmt('Can''t load image stream. GDI error %d', [err]);
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
constructor TGpImage.Create(Stream: TStream);
begin
  Create(IStreamFromStream(Stream));
end;

destructor TGpImage.Destroy;
begin
  GdipDisposeImage(fHandle);
  if Length(fFilename) > 0 then
  try
    DeleteFile(fFilename);
  except
  end;
  inherited;
end;

function TGpImage.GetWidth: integer;
begin
  if fWidth = 0 then
    GdipGetImageWidth(fHandle, fWidth);
  Result := fWidth;
end;

function TGpImage.GetHeight: integer;
begin
  if fHeight = 0 then
    GdipGetImageHeight(fHandle, fHeight);
  Result := fHeight;
end;

function TGpImage.GetBitmap: TBitmap;
var
  g: TGpGraphics;
begin
  Result := TBitmap.Create;
  Result.Width := GetWidth;
  Result.Height := GetHeight;
  PatBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, Whiteness);
  g := TGpGraphics.Create(Result.Canvas.Handle);
  g.DrawImage(Self, 0, 0, Result.Width, Result.Height);
  g.Free;
end;

constructor TGpBitmap.Create(W, H: integer);
const
  PixelFormatGDI = $00020000; // Is a GDI-supported format
  PixelFormatAlpha = $00040000; // Has an alpha component
  PixelFormatCanonical = $00200000;
  PixelFormat32bppARGB = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI
    or PixelFormatCanonical);
var
  err: integer;
begin
  inherited Create;
  err := GdipCreateBitmapFromScan0(W, H, 0, PixelFormat32bppARGB, nil, fHandle);
  if err <> 0 then
    raise EGDIPlus.CreateFmt('Can''t create bitmap of size %d x %d. GDI error %d', [W, H, err]);
end;

constructor TGpBitmap.Create(IStr: IStream);
var
  err: integer;
begin
  inherited Create;
  err := GdipCreateBitmapFromStream(IStr, fHandle);
  if err <> 0 then
    raise EGDIPlus.CreateFmt('Can''t create bitmap from IStream. GDI error %d', [err]);
end;

constructor TGpBitmap.Create(W, H: integer; Graphics: TGpGraphics);
begin
  inherited Create;
  err := GdipCreateBitmapFromGraphics(W, H, Graphics.fGraphics, fHandle);
  if err <> 0 then
    raise EGDIPlus.CreateFmt('Can''t create bitmap of size %d x %d from TGpGraphics. GDI error %d', [W, H, err]);
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
constructor TGpBitmap.Create(Stream: TStream);
begin
  Create(IStreamFromStream(Stream));
end;

function TGpBitmap.GetPixel(X, Y: integer): DWord;
begin
  GdipBitmapGetPixel(fHandle, X, Y, Result);
end;

procedure TGpBitmap.SetPixel(X, Y: integer; Color: DWord);
begin
  GdipBitmapSetPixel(fHandle, X, Y, Color);
end;

{$IFNDEF NoGDIPlus}
var
  InitToken: DWord;
  Startup: GdiplusStartupInput;
  LibHandle: THandle;
  GDIPlusCount: integer;
{$ENDIF}

procedure CheckInitGDIPlus;
begin
{$IFNDEF NoGDIPlus}
  if GDIPlusCount = 0 then
  begin
    LibHandle := LoadLibrary(GdiPlusLib);
    if LibHandle <> 0 then
    begin
      @GdiplusStartup := GetProcAddress(LibHandle, 'GdiplusStartup');
      @GdiplusShutdown := GetProcAddress(LibHandle, 'GdiplusShutdown');
      @GdipDeleteGraphics := GetProcAddress(LibHandle, 'GdipDeleteGraphics');
      @GdipCreateFromHDC := GetProcAddress(LibHandle, 'GdipCreateFromHDC');
      @GdipDrawImageI := GetProcAddress(LibHandle, 'GdipDrawImageI');
      @GdipDrawImageRectI := GetProcAddress(LibHandle, 'GdipDrawImageRectI');
      @GdipLoadImageFromFile := GetProcAddress(LibHandle, 'GdipLoadImageFromFile');
      @GdipLoadImageFromStream := GetProcAddress(LibHandle, 'GdipLoadImageFromStream');
      @GdipCreateBitmapFromStream := GetProcAddress(LibHandle, 'GdipCreateBitmapFromStream');
      @GdipDisposeImage := GetProcAddress(LibHandle, 'GdipDisposeImage');
      @GdipGetImageWidth := GetProcAddress(LibHandle, 'GdipGetImageWidth');
      @GdipGetImageHeight := GetProcAddress(LibHandle, 'GdipGetImageHeight');
      @GdipGetImageGraphicsContext := GetProcAddress(LibHandle, 'GdipGetImageGraphicsContext');
      @GdipGraphicsClear := GetProcAddress(LibHandle, 'GdipGraphicsClear');
      @GdipCreateBitmapFromScan0 := GetProcAddress(LibHandle, 'GdipCreateBitmapFromScan0');
      @GdipDrawImagePointRect := GetProcAddress(LibHandle, 'GdipDrawImagePointRect');
      @GdipScaleWorldTransform := GetProcAddress(LibHandle, 'GdipScaleWorldTransform');
      @GdipCreateBitmapFromGraphics := GetProcAddress(LibHandle, 'GdipCreateBitmapFromGraphics');
      @GdipBitmapGetPixel := GetProcAddress(LibHandle, 'GdipBitmapGetPixel');
      @GdipDrawImageRectRectI := GetProcAddress(LibHandle, 'GdipDrawImageRectRectI');
      @GdipCreateHBITMAPFromBitmap := GetProcAddress(LibHandle, 'GdipCreateHBITMAPFromBitmap');

      @GdipSetInterpolationMode := GetProcAddress(LibHandle, 'GdipSetInterpolationMode');
      @GdipGetInterpolationMode := GetProcAddress(LibHandle, 'GdipGetInterpolationMode');
      @GdipBitmapSetPixel := GetProcAddress(LibHandle, 'GdipBitmapSetPixel');

      FillChar(Startup, sizeof(Startup), 0);
      Startup.GdiplusVersion := 1;
      Err := GdiPlusStartup(InitToken, @Startup, nil);
      GDIPlusActive := Err = 0;
      if not GDIPlusActive then
        FreeLibrary(LibHandle);
    end;
  end;
  Inc(GDIPlusCount);
{$ENDIF}
end;

procedure CheckExitGDIPlus;
begin
{$IFNDEF NoGDIPlus}
  Dec(GDIPlusCount);
  if GDIPlusCount = 0 then
    if GDIPlusActive then
    begin
      GdiplusShutdown(InitToken);
      FreeLibrary(LibHandle);
      GDIPlusActive := False;
    end;
{$ENDIF}
end;

end.
