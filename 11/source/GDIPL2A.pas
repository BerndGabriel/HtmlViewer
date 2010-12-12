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

{$I htmlcons.inc}

unit GDIPL2A;

interface

uses Windows, SysUtils, ActiveX, Graphics;

var
  GDIPlusActive: boolean;

type
  TGpImage = class(TObject)
  private
    fHandle: integer;
    fWidth, fHeight: integer;
    fFilename: string;
    function GetHeight: integer;
    function GetWidth: integer;
  public
    constructor Create(Filename: WideString; TmpFile: boolean = False); overload;
    constructor Create(IStr: IStream); overload;
    destructor Destroy; override;
    function GetTBitmap: TBitmap;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
  end;

  TGpGraphics = class;

  TGpBitmap = class(TGpImage)
  public
    constructor Create(W, H: integer); overload;
    constructor Create(IStr: IStream); overload;
    constructor Create(W, H: integer; Graphics: TGpGraphics); overload;
    function GetPixel(X, Y: integer): DWord;
    procedure SetPixel(X, Y: integer; Color: DWord);
  end;

  TGpGraphics = class(TObject)
  private
    fGraphics: integer;
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
  EGDIPlus = class(Exception);
  TRectF = record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
  end;

  ImageCodecInfo = packed record
    Clsid: TGUID;
    FormatID: TGUID;
    CodecName: PWCHAR;
    DllName: PWCHAR;
    FormatDescription: PWCHAR;
    FilenameExtension: PWCHAR;
    MimeType: PWCHAR;
    Flags: DWORD;
    Version: DWORD;
    SigCount: DWORD;
    SigSize: DWORD;
    SigPattern: PBYTE;
    SigMask: PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

var
{$IFNDEF NoGDIPlus}
  GdiplusStartup: function(var Token: DWord; const Input, Output: Pointer): Integer; stdcall;
  GdiplusShutdown: procedure(Token: DWord); stdcall;
  GdipDrawImageI: function(Graphics, Image, X, Y: Integer): Integer; stdcall;
  GdipCreateHBITMAPFromBitmap: function(bitmap: integer; out hbmReturn: HBITMAP;
    background: DWord): integer; stdcall;
  GdipGetInterpolationMode: function(graphics: integer; var interpolationMode: integer): integer; stdcall;
{$ENDIF$}
  GdipDeleteGraphics: function(Graphics: Integer): Integer; stdcall;
  GdipCreateFromHDC: function(hdc: HDC; var Graphics: Integer): Integer; stdcall;
  GdipDrawImageRectI: function(Graphics, Image, X, Y, Width, Height: Integer): Integer; stdcall;
  GdipLoadImageFromFile: function(const FileName: PWideChar; var Image: Integer): Integer; stdcall;
  GdipLoadImageFromStream: function(stream: ISTREAM;
    out image: integer): integer; stdcall;
  GdipCreateBitmapFromStream: function(stream: ISTREAM; out bitmap: integer): integer; stdcall;
  GdipDisposeImage: function(Image: Integer): Integer; stdcall;
  GdipGetImageWidth: function(Image: Integer; var Width: Integer): Integer; stdcall;

  GdipGetImageHeight: function(Image: Integer; var Height: Integer): Integer; stdcall;
  GdipGetImageGraphicsContext: function(Image: integer; out graphics: integer): integer; stdcall;
  GdipGraphicsClear: function(Graphics: Integer; Color: Cardinal): Integer; stdcall;
  GdipCreateBitmapFromScan0: function(width: Integer; height: Integer;
    stride: Integer; pixelformat: dword; scan0: Pointer;
    out bitmap: integer): integer; stdcall;
  GdipDrawImagePointRect: function(graphics: integer; image: integer;
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: integer): integer; stdcall;
  GdipScaleWorldTransform: function(graphics: integer; sx: Single; sy: Single;
    order: integer): integer; stdcall;
  GdipCreateBitmapFromGraphics: function(width, height: Integer;
    Graphics: integer; out Bitmap: integer): integer; stdcall;
  GdipBitmapGetPixel: function(bitmap, x, y: Integer; var color: DWord): integer; stdcall;
  GdipDrawImageRectRectI: function(graphics, image,
    dstx, dsty, dstwidth, dstheight, srcx, srcy, srcwidth, srcheight,
    srcUnit, imageAttributes: integer;
    callback: Pointer; callbackData: integer): integer; stdcall;

  GdipSetInterpolationMode: function(graphics, interpolationMode: integer): integer; stdcall;
  GdipBitmapSetPixel: function(bitmap, x, y: Integer; color: DWord): Integer; stdcall;

type
  TGDIStartup = packed record
    Version: Integer; // Must be one
    DebugEventCallback: Pointer; // Only for debug builds
    SuppressBackgroundThread: Bool; // True if replacing GDI+ background processing
    SuppressExternalCodecs: Bool; // True if only using internal codecs
  end;

var
  Err: Integer;

{ TGpGraphics }

constructor TGpGraphics.Create(Handle: HDC);
var
  err: integer;
begin
  inherited Create;
  err := GdipCreateFromHDC(Handle, fGraphics);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t Create Graphics');
end;

constructor TGpGraphics.Create(Image: TGpImage);
var
  err: integer;
begin
  inherited Create;
  err := GdipGetImageGraphicsContext(image.fHandle, fgraphics);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t Create Graphics');
end;

destructor TGpGraphics.Destroy;
begin
  if fGraphics <> 0 then
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
  Pixel: DWord;
begin
  W := Image.Width + 1; {new dimensions}
  H := Image.Height + 1;
  BM1 := TGpBitmap.Create(W, H); {new bitmap with extra row and column}
  try
    g1 := TGpGraphics.Create(BM1);
    try
      g1.DrawImage(Image, 0, 0); {draw the original image}
      g1.DrawImage(Image, W - 1, 0, 1, H, {copy the column, then the row}
        W - 2, 0, 1, H);
      g1.DrawImage(Image, 0, H - 1, W, 1,
        0, H - 2, W, 1);
      Pixel := BM1.GetPixel(W - 2, H - 2); {for some reason also need to set the lower right pixel}
      BM1.SetPixel(W - 1, H - 1, Pixel);
      BM2 := TGpBitmap.Create(Width, Height);
      try
        g2 := TGpGraphics.Create(BM2);
        try
          GdipSetInterpolationMode(g2.fGraphics, NearestNeighbor);
          g2.DrawImage(BM1, 0, 0, Width, Height, {now draw the image stretched where needed}
            0, 0, Image.Width, Image.Height);
          DrawImage(BM2, X, Y); {now draw the image stretched where needed}
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
    sx, sy, sw, sh, UnitPixel, 0, nil, 0);

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

constructor TGpImage.Create(Filename: WideString; TmpFile: boolean = False);
var
  err: Integer;
//    Buffer: array [0..511] of WideChar;
begin
  inherited Create;
  if not FileExists(FileName) then
    raise EGDIPlus.Create(Format('Image file %s not found.', [FileName]));
  err := GdipLoadImageFromFile(PWideChar(FileName), fHandle);
  if err <> 0 then
    raise EGDIPlus.Create(Format('Can''t load image file %s.', [FileName]));
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
    raise EGDIPlus.Create('Can''t load image stream');
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

function TGpImage.GetTBitmap: TBitmap;
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
    raise EGDIPlus.Create('Can''t create bitmap');
end;

constructor TGpBitmap.Create(IStr: IStream);
var
  err: integer;
begin
  inherited Create;
  err := GdipCreateBitmapFromStream(IStr, fHandle);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t create bitmap');
end;

constructor TGpBitmap.Create(W, H: integer; Graphics: TGpGraphics);
begin
  inherited Create;
  err := GdipCreateBitmapFromGraphics(W, H, Graphics.fGraphics, fHandle);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t create bitmap');
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
  Startup: TGDIStartup;
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
      Startup.Version := 1;
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

