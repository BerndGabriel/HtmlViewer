{
Version   11.4
Copyright (c) 1995-2008 by L. David Baldwin
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

{$I htmlcons.inc}

unit GDIPL2A;

interface

uses
   {$ifdef HasGDIPlus}
   GDIPAPI,
   {$endif}
  Windows, ActiveX, SysUtils, Graphics, Classes,
  HtmlGlobals;

var
  GDIPlusActive: boolean;

type
  {$ifndef HasGDIPlus}
  //types for GDIPlus API
  GpGraphics = Pointer;
//  PGpGraphics = ^GpGraphics;
  GpImage = Pointer;
//  PGpImage = ^GpImage;
  GpBitmap = Pointer;
//  PGpBitmap = ^GpBitmap;
  GpStatus = Integer;
  PixelFormat = Integer;
  GpImageAttributes = Pointer;
  {$endif}
  //end section
  THtGpImage = class(TObject)
  protected
    fHandle: GpImage;
    fWidth, fHeight: Cardinal;
    fFilename: string;
    function GetHeight: Cardinal;
    function GetWidth: Cardinal;
  public
    constructor Create(Filename: ThtString; TmpFile: boolean = False); overload;
    constructor Create(IStr: IStream); overload;
    constructor Create(Stream: TStream); overload;
    destructor Destroy; override;
    function GetBitmap: TBitmap;
    property Height: Cardinal read GetHeight;
    property Width: Cardinal read GetWidth;
  end;

  THtGpGraphics = class;

  THtGpBitmap = class(THtGpImage)
  public
    constructor Create(W, H: integer); overload;
    constructor Create(IStr: IStream); overload;
    constructor Create(Stream: TStream); overload;
    constructor Create(W, H: integer; Graphics: THtGpGraphics); overload;
    function GetPixel(X, Y: integer): DWord;
    procedure SetPixel(X, Y: integer; Color: DWord);
  end;

  THtGpGraphics = class(TObject)
  private
    fGraphics: GpGraphics;
    procedure DrawSmallStretchedImage(Image: THtGPImage; X, Y, Width, Height: Integer);
  public
    constructor Create(Handle: HDC); overload;
    constructor Create(Image: THtGpImage); overload;
    destructor Destroy; override;
    procedure DrawImage(Image: THtGPImage; X, Y: Cardinal); overload;
    procedure DrawImage(Image: THtGPImage; X, Y, Width, Height: Cardinal); overload;
    procedure DrawImage(Image: THtGpImage; x, y, srcx, srcy, srcwidth, srcheight: integer); overload;
    procedure DrawImage(Image: THtGpImage; dx, dy, dw, dh, sx, sy, sw, sh: integer); overload;
    procedure Clear(Color: Cardinal);
    procedure ScaleTransform(sx, sy: Single);
  end;

procedure CheckInitGDIPlus;
procedure CheckExitGDIPlus;

implementation
 {$ifndef HasGDIPlus}
const
  GdiPlusLib = 'GdiPlus.dll';
  Ok : GpStatus = 0;

type
  ARGB = DWORD;
  GpColor = ARGB;
  NotificationHookProc = function (out token : ULONG) : GpStatus stdcall;
  NotificationUnhookProc = procedure (token : ULONG) stdcall;

  DebugEventLevel = Integer;
  DebugEventProc = procedure (level : DebugEventLevel; message_ : PChar) stdcall;
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

  GpUnit = Integer; //enumeration
  GpMatrixOrder = Integer;
  InterpolationMode = Integer;
  TInterpolationMode = InterpolationMode;
  PGpImageAttributes = Pointer;
  ImageAbort = function : BOOL stdcall;
  DrawImageAbort = ImageAbort;


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
//  GdipDrawImageI: function(Graphics : PGpGraphics; Image : PGpImage; X, Y: Integer): GpStatus stdcall;
//  GdipCreateHBITMAPFromBitmap: function(bitmap: PGpBitmap; out hbmReturn: HBITMAP; background: TARGB): GpStatus stdcall;
//  GdipGetInterpolationMode: function(graphics: PGpGraphics; var interpolationMode: TInterpolationMode): GpStatus stdcall;
{$ENDIF$}
  GdipDeleteGraphics: function(Graphics: GpGraphics): GpStatus stdcall;
  GdipCreateFromHDC: function(hdc: HDC; out Graphics: GpGraphics): GpStatus stdcall;
  GdipDrawImageRectI: function(graphics: GpGraphics; image: GpImage; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  GdipLoadImageFromFile: function(const FileName: PWideChar; out Image: GpImage): GpStatus stdcall;
  GdipLoadImageFromStream: function(stream: ISTREAM;
    out image: GpImage): GpStatus stdcall;
  GdipCreateBitmapFromStream: function(stream: ISTREAM; out bitmap: GpBitmap): GpStatus stdcall;
  GdipDisposeImage: function(Image: GpImage): GpStatus stdcall;
  GdipGetImageWidth: function(Image: GpImage; var Width: Cardinal): GpStatus stdcall;

  GdipGetImageHeight: function(Image: GpImage; var Height: Cardinal): GpStatus stdcall;
  GdipGetImageGraphicsContext: function(Image: GpImage; out graphics: GpGraphics): GpStatus stdcall;
  GdipGraphicsClear: function(Graphics: GpGraphics; Color: ARGB): GpStatus stdcall;
  GdipCreateBitmapFromScan0: function(width: Integer; height: Integer;
    stride: Integer; pixelformat: PixelFormat; scan0: Pointer;
    out bitmap: GpBitmap): GpStatus stdcall;
  GdipDrawImagePointRect: function(graphics: GpGraphics; image: GpImage;
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: GpUnit): GpStatus; stdcall;
  GdipScaleWorldTransform: function(graphics: GpGraphics; sx: Single; sy: Single;
    order: GpMatrixOrder): GpStatus stdcall;
  GdipCreateBitmapFromGraphics: function(width: Integer; height: Integer;
    target: GpGraphics; out bitmap: GpBitmap): GpStatus; stdcall;
  GdipBitmapGetPixel: function(bitmap: GpBitmap; x: Integer; y: Integer;
    var color: ARGB): GpStatus; stdcall;
  GdipDrawImageRectRectI: function(graphics : GpGraphics; image : GpImage;
    dstx, dsty, dstwidth, dstheight, srcx, srcy, srcwidth, srcheight : Integer;
    srcUnit : GpUnit; imageAttributes: GpImageAttributes;
    callback: DrawImageAbort; callbackData: Pointer): GpStatus; stdcall;

//  GdipSetInterpolationMode: function(graphics : PGpGraphics; interpolationMode: InterpolationMode): GpStatus stdcall;
  GdipBitmapSetPixel: function(bitmap : GpBitmap; x, y: Integer; color: ARGB): GpStatus stdcall;

{$else}
uses GDIPOBJ, GDIPUTIL;

var Err : GpStatus;
{$endif}

type
  EGDIPlus = class(Exception);

procedure GDICheck(const AProc : String; const AErr : GpStatus);
begin
  if AErr <> Ok then begin
   {$ifdef HasGDIPlus}
   raise EGDIPlus.CreateFmt('%s GDI error %s', [AProc,GetStatus(err)]);
   {$else}
    raise EGDIPlus.CreateFmt('%s GDI error %d', [AProc,err]);
    {$endif}
  end;
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
function IStreamFromStream(Stream: TStream): IStream;
// thanks to Sérgio Alexandre for this method.
var
  Handle: HGLOBAL;
  Ptr: Pointer absolute Handle;
begin
  Handle := GlobalAlloc(GPTR, Stream.Size);
  if Handle <> 0 then
  begin
    Stream.Read(Ptr^, Stream.Size);
    CreateStreamOnHGlobal(Handle, True, Result);
  end;
end;

{ THtGpGraphics }

constructor THtGpGraphics.Create(Handle: HDC);
var
  err: GpStatus;
begin
  inherited Create;
  err := GdipCreateFromHDC(Handle, fGraphics);
  if err <> Ok then
   {$ifdef HasGDIPlus}
   raise EGDIPlus.CreateFmt('Can''t Create Graphics. GDI error %s', [GetStatus(err)]);
   {$else}
    raise EGDIPlus.CreateFmt('Can''t Create Graphics. GDI error %d', [err]);
    {$endif}
end;

constructor THtGpGraphics.Create(Image: THtGpImage);
var
  err: GpStatus;
begin
  inherited Create;
  err := GdipGetImageGraphicsContext(image.fHandle, fgraphics);
  if err <> Ok then
   {$ifdef HasGDIPlus}
   raise EGDIPlus.CreateFmt('Can''t Create Graphics. GDI error %s', [GetStatus(err)]);
   {$else}
    raise EGDIPlus.CreateFmt('Can''t Create Graphics. GDI error %d', [err]);
   {$endif}
end;

destructor THtGpGraphics.Destroy;
begin
  if fGraphics <> nil then
    GDICheck('THtGpGraphics.Destroy',GdipDeleteGraphics(fGraphics));
  inherited;
end;

procedure THtGpGraphics.DrawImage(Image: THtGPImage; X, Y, Width, Height: Cardinal);
begin
  if ((Image.Width <= 10) and (Width > Image.Width)) or
    ((Image.Height <= 10) and (Height > Image.Height)) then
    DrawSmallStretchedImage(Image, X, Y, Width, Height)
  else
    GDICheck('THtGpGraphics.DrawImage',GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Width, Height));
end;

procedure THtGpGraphics.DrawSmallStretchedImage(Image: THtGPImage; X, Y, Width, Height: Integer);
{when a small image is getting enlarged, add a row and column to it copying
 the last row/column to the new row/column.  This gives much better interpolation.}
//const
//  NearestNeighbor = 5;
var
  g1, g2: THtGpGraphics;
  BM1, BM2: THtGpBitmap;
  W, H: integer;
begin
  W := Image.Width + 1; {new dimensions}
  H := Image.Height + 1;
  BM1 := THtGpBitmap.Create(W, H); {new bitmap with extra row and column}
  try
    g1 := THtGpGraphics.Create(BM1);
    try
      {draw the original image to BM1}
      g1.DrawImage(Image, 0, 0);

      {copy the additional column inside BM1}
      g1.DrawImage(BM1, W - 1,     0, 1, H - 1,  W - 2,     0, 1, H - 1);

      {copy the additional row incl. additional pixel inside BM1}
      g1.DrawImage(BM1,     0, H - 1, W,     1,      0, H - 2, W,     1);

      BM2 := THtGpBitmap.Create(Width, Height);
      try
        g2 := THtGpGraphics.Create(BM2);
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

procedure THtGpGraphics.DrawImage(Image: THtGPImage; X, Y: Cardinal);
begin
  GDICheck('THtGpGraphics.DrawImage', GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Image.Width, Image.Height));
end;

procedure THtGPGraphics.DrawImage(Image: THtGpImage; x, y,
  srcx, srcy, srcwidth, srcheight: integer);
   {$ifndef HasGDIPlus}
const
  UnitPixel = 2;
   {$endif}

begin
  GDICheck('THtGpGraphics.DrawImage',GdipDrawImagePointRect(fGraphics, Image.fHandle, x, y,
    srcx, srcy, srcwidth, srcheight, UnitPixel));
end;

procedure THtGPGraphics.DrawImage(Image: THtGpImage; dx, dy, dw, dh, sx, sy, sw, sh: integer);
   {$ifndef HasGDIPlus}
const
  UnitPixel = 2;
   {$endif}

begin
 GDICheck('THtGpGraphics.DrawImage', GdipDrawImageRectRectI(fGraphics, Image.fHandle, dx, dy, dw, dh,
    sx, sy, sw, sh, UnitPixel, nil, nil, nil));

end;

procedure THtGpGraphics.Clear(Color: Cardinal);
begin
  GDICheck('THtGpGraphics.Clear', GdipGraphicsClear(fGraphics, Color));
end;

procedure THtGPGraphics.ScaleTransform(sx, sy: Single);
   {$ifndef HasGDIPlus}
const
  MatrixOrderPrepend = 0;
  {$endif}
begin
  GDICheck('THtGpGraphics.DrawImage', GdipScaleWorldTransform(fGraphics, sx, sy, MatrixOrderPrepend));
end;

{ TGpImage }

constructor THtGpImage.Create(Filename: ThtString; TmpFile: boolean = False);
var
  err: GpStatus;
begin
  inherited Create;
  err := GdipLoadImageFromFile(PWideChar(FileName), fHandle);
  if err <> Ok then
    if GetLastError = 2 then
   {$ifdef HasGDIPlus}
      raise EGDIPlus.CreateFmt('Image file "%s" not found. GDI error %s', [FileName, GetStatus(err)])
   {$else}
      raise EGDIPlus.CreateFmt('Image file "%s" not found. GDI error %d', [FileName, err])
   {$endif}
    else
   {$ifdef HasGDIPlus}
      raise EGDIPlus.CreateFmt('Can''t load image file "%s". GDI error %s', [FileName, GetStatus(err)]);
   {$else}
      raise EGDIPlus.CreateFmt('Can''t load image file "%s". GDI error %d', [FileName, err]);
   {$endif}
  if TmpFile then
    fFilename := Filename;
end;

constructor THtGpImage.Create(IStr: IStream);
var
  err: GpStatus;
begin
  inherited Create;
  err := GdipLoadImageFromStream(IStr, fHandle);
  if err <> Ok then
   {$ifdef HasGDIPlus}
      raise EGDIPlus.CreateFmt('Can''t load image stream. GDI error %s', [GetStatus(err)]);
   {$else}
    raise EGDIPlus.CreateFmt('Can''t load image stream. GDI error %d', [err]);
    {$endif}
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
constructor THtGpImage.Create(Stream: TStream);
begin
  Create(IStreamFromStream(Stream));
end;

destructor THtGpImage.Destroy;
begin
  GDICheck('THtGpImage.Destroy', GdipDisposeImage(fHandle));
  if Length(fFilename) > 0 then
  try
    DeleteFile(fFilename);
  except
  end;
  inherited;
end;

function THtGpImage.GetWidth: Cardinal;
begin
  if fWidth = 0 then
     GDICheck('THtGpImage.GetWidth',GdipGetImageWidth(fHandle, fWidth));
  Result := fWidth;
end;

function THtGpImage.GetHeight: Cardinal;
begin
  if fHeight = 0 then
     GDICheck('THtGpImage.GetWidth',GdipGetImageHeight(fHandle, fHeight));
  Result := fHeight;
end;

function THtGpImage.GetBitmap: TBitmap;
var
  g: THtGpGraphics;
begin
  Result := TBitmap.Create;
  Result.Width := GetWidth;
  Result.Height := GetHeight;
  PatBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, Whiteness);
  g := THtGpGraphics.Create(Result.Canvas.Handle);
  g.DrawImage(Self, 0, 0, Result.Width, Result.Height);
  g.Free;
end;

constructor THtGpBitmap.Create(W, H: integer);
const
  PixelFormatGDI = $00020000; // Is a GDI-supported format
  PixelFormatAlpha = $00040000; // Has an alpha component
  PixelFormatCanonical = $00200000;
  PixelFormat32bppARGB = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI
    or PixelFormatCanonical);
var
  err: GpStatus;

begin
  inherited Create;
  err := GdipCreateBitmapFromScan0(W, H, 0, PixelFormat32bppARGB, nil, fHandle);
  if err <> Ok then
   {$ifdef HasGDIPlus}
      raise EGDIPlus.CreateFmt('Can''t create bitmap of size %d x %d. GDI error %s', [W, H,GetStatus(err)]);
   {$else}
    raise EGDIPlus.CreateFmt('Can''t create bitmap of size %d x %d. GDI error %d', [W, H, err]);
   {$endif}
end;

constructor THtGpBitmap.Create(IStr: IStream);
var
  err: GpStatus;
begin
  inherited Create;
  err := GdipCreateBitmapFromStream(IStr, fHandle);
  if err <> Ok then
   {$ifdef HasGDIPlus}
      raise EGDIPlus.CreateFmt('Can''t create bitmap of size %d x %d. GDI error %s', [GetStatus(err)]);
   {$else}
    raise EGDIPlus.CreateFmt('Can''t create bitmap from IStream. GDI error %d', [err]);
    {$endif}
end;

constructor THtGpBitmap.Create(W, H: integer; Graphics: THtGpGraphics);
var err : GpStatus;
begin
  inherited Create;
  err := GdipCreateBitmapFromGraphics(W, H, Graphics.fGraphics, fHandle);
  if err <> Ok then
   {$ifdef HasGDIPlus}
      raise EGDIPlus.CreateFmt('Can''t create bitmap of size %d x %d. GDI error %s', [W, H,GetStatus(err)]);
   {$else}
    raise EGDIPlus.CreateFmt('Can''t create bitmap of size %d x %d from TGpGraphics. GDI error %d', [W, H, err]);
    {$endif}
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
constructor THtGpBitmap.Create(Stream: TStream);
begin
  Create(IStreamFromStream(Stream));
end;

function THtGpBitmap.GetPixel(X, Y: integer): DWord;
begin
  GDICheck('THtGpBitmap.GetPixel',GdipBitmapGetPixel(fHandle, X, Y, Result));
end;

procedure THtGpBitmap.SetPixel(X, Y: integer; Color: DWord);
begin
  GDICheck('THtGpBitmap.SetPixel',GdipBitmapSetPixel(fHandle, X, Y, Color));
end;

{$IFDEF NoGDIPlus}
  {$ifndef HasGDIPlus}
var
  InitToken: DWord;
  Startup: GdiplusStartupInput;
  LibHandle: THandle;
  GDIPlusCount: integer;
    {$endif}
{$ENDIF}

procedure CheckInitGDIPlus;
begin
{$IFNDEF NoGDIPlus}
  {$ifndef HasGDIPlus}
  if GDIPlusCount = 0 then
  begin
    LibHandle := LoadLibrary(GdiPlusLib);
    if LibHandle <> 0 then
    begin
      @GdiplusStartup := GetProcAddress(LibHandle, 'GdiplusStartup');
      @GdiplusShutdown := GetProcAddress(LibHandle, 'GdiplusShutdown');
      @GdipDeleteGraphics := GetProcAddress(LibHandle, 'GdipDeleteGraphics');
      @GdipCreateFromHDC := GetProcAddress(LibHandle, 'GdipCreateFromHDC');
      //@GdipDrawImageI := GetProcAddress(LibHandle, 'GdipDrawImageI');
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
      //@GdipCreateHBITMAPFromBitmap := GetProcAddress(LibHandle, 'GdipCreateHBITMAPFromBitmap');

      //@GdipSetInterpolationMode := GetProcAddress(LibHandle, 'GdipSetInterpolationMode');
      //@GdipGetInterpolationMode := GetProcAddress(LibHandle, 'GdipGetInterpolationMode');
      @GdipBitmapSetPixel := GetProcAddress(LibHandle, 'GdipBitmapSetPixel');

      FillChar(Startup, sizeof(Startup), 0);
      Startup.GdiplusVersion := 1;
      Err := GdiPlusStartup(InitToken, @Startup, nil);
      GDIPlusActive := Err = Ok;
      if not GDIPlusActive then
        FreeLibrary(LibHandle);
    end;
  end;
  Inc(GDIPlusCount);
  {$endif}
  GDIPlusActive := True;
{$ENDIF}
end;

procedure CheckExitGDIPlus;
begin
{$IFNDEF NoGDIPlus}
    {$ifndef HasGDIPlus}
  Dec(GDIPlusCount);
  if GDIPlusCount = 0 then
    if GDIPlusActive then
    begin
      GdiplusShutdown(InitToken);
      FreeLibrary(LibHandle);
      GDIPlusActive := False;
    end;
  {$endif}
    GDIPlusActive := False;
{$ENDIF}
end;

end.
