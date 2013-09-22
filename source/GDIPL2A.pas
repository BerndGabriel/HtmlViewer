{
Version   11.4
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2013 by HtmlViewer Team

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
{
  NOTICE: Conditional symbols control code emitted by this unit:

  NoGDIPlus:  This units produces code only, if conditional symbol NoGDIPlus is not defined.

  HasGDIPlus: This unit uses the compiler's GDI units, if conditional symbol HasGDIPlus is defined
              else the API to GdiPlus.dll is implemented inside this unit.

  Define or undefine these symbols in htmlcons.inc.
}

{$I htmlcons.inc}

unit GDIPL2A;

interface
{$ifndef NoGDIPlus}

uses
{$ifdef HasGDIPlus}
   GDIPAPI,
{$endif}
  Windows, ActiveX, SysUtils, Graphics, Classes,
  HtmlGlobals;

var
  GDIPlusActive: Boolean;

{$ifndef HasGDIPlus}
type
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

type
  ThtGpImage = class(TObject)
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

  ThtGpGraphics = class;

  ThtGpBitmap = class(ThtGpImage)
  public
    constructor Create(W, H: integer); overload;
    constructor Create(IStr: IStream); overload;
    constructor Create(Stream: TStream); overload;
    constructor Create(W, H: integer; Graphics: ThtGpGraphics); overload;
    function GetPixel(X, Y: integer): DWord;
    procedure SetPixel(X, Y: integer; Color: DWord);
  end;

  ThtGpGraphics = class(TObject)
  private
    fGraphics: GpGraphics;
    procedure DrawSmallStretchedImage(Image: THtGPImage; X, Y, Width, Height: Integer);
  public
    constructor Create(Handle: HDC); overload;
    constructor Create(Image: ThtGpImage); overload;
    destructor Destroy; override;
    procedure DrawImage(Image: THtGPImage; X, Y: Cardinal); overload;
    procedure DrawImage(Image: THtGPImage; X, Y, Width, Height: Cardinal); overload;
    procedure DrawImage(Image: ThtGpImage; x, y, srcx, srcy, srcwidth, srcheight: integer); overload;
    procedure DrawImage(Image: ThtGpImage; dx, dy, dw, dh, sx, sy, sw, sh: integer); overload;
    procedure Clear(Color: Cardinal);
    procedure ScaleTransform(sx, sy: Single);
  end;

procedure CheckInitGDIPlus;
procedure CheckExitGDIPlus;

{$endif NoGDIPlus}
implementation
{$ifndef NoGDIPlus}

{$ifdef HasGDIPlus}
  uses
    GDIPOBJ, GDIPUTIL;
{$else HasGDIPlus}
  const
    GdiPlusLib = 'GdiPlus.dll';
    UnitPixel = 2;
    MatrixOrderPrepend = 0;
    Ok : GpStatus = 0;

  type
    ARGB = DWORD;
    GpColor = ARGB;
    NotificationHookProc = function (out token : ULONG) : GpStatus stdcall;
    NotificationUnhookProc = procedure (token : ULONG) stdcall;

    DebugEventLevel = Integer;
    DebugEventProc = procedure (level : DebugEventLevel; message_ : PChar) stdcall;

    GdiplusStartupInput = packed record
      GdiplusVersion : DWORD;               // Must be 1 (or 2 for the Ex version)
      DebugEventCallback : DebugEventProc;  // Ignored on free builds
      SuppressBackgroundThread : BOOL;      // FALSE unless you're prepared to call the hook/unhook functions properly
      SuppressExternalCodecs : BOOL;        // FALSE unless you want GDI+ only to use its internal image codecs.
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
    GdiplusStartup: function(out Token: ULONG; const Input : PGdiplusStartupInput; const Output: PGdiplusStartupOutput): GpStatus stdcall;
    GdiplusShutdown: procedure(Token: ULONG) stdcall;
    // GpGraphics methods
    GdipCreateFromHDC: function(hdc: HDC; out Graphics: GpGraphics): GpStatus stdcall;
    GdipScaleWorldTransform: function(graphics: GpGraphics; sx, sy: Single; order: GpMatrixOrder): GpStatus stdcall;

    //GdipGetInterpolationMode: function(graphics: GpGraphics; var interpolationMode: TInterpolationMode): GpStatus stdcall;
    //GdipSetInterpolationMode: function(graphics: GpGraphics; interpolationMode: TInterpolationMode): GpStatus stdcall;

    GdipGraphicsClear: function(Graphics: GpGraphics; Color: ARGB): GpStatus stdcall;
    GdipDeleteGraphics: function(Graphics: GpGraphics): GpStatus stdcall;

    // GpBitmap methods

    GdipCreateBitmapFromStream: function(stream: ISTREAM; out bitmap: GpBitmap): GpStatus stdcall;
    GdipCreateBitmapFromScan0: function(width, height, stride: Integer; pixelformat: PixelFormat; scan0: Pointer; out bitmap: GpBitmap): GpStatus stdcall;
    GdipCreateBitmapFromGraphics: function(width, height: Integer; target: GpGraphics; out bitmap: GpBitmap): GpStatus; stdcall;
    //GdipCreateHBITMAPFromBitmap: function(bitmap: GpBitmap; out hbmReturn: HBITMAP; background: TARGB): GpStatus stdcall;

    GdipBitmapGetPixel: function(bitmap: GpBitmap; x, y: Integer; var color: ARGB): GpStatus; stdcall;
    GdipBitmapSetPixel: function(bitmap: GpBitmap; x, y: Integer; color: ARGB): GpStatus stdcall;

    // GpImage methods

    GdipLoadImageFromFile: function(const FileName: PWideChar; out Image: GpImage): GpStatus stdcall;
    GdipLoadImageFromStream: function(stream: ISTREAM; out image: GpImage): GpStatus stdcall;

    GdipGetImageWidth: function(Image: GpImage; var Width: Cardinal): GpStatus stdcall;
    GdipGetImageHeight: function(Image: GpImage; var Height: Cardinal): GpStatus stdcall;

    GdipDisposeImage: function(Image: GpImage): GpStatus stdcall;

    // GpGraphics and GpImage methods

    GdipGetImageGraphicsContext: function(Image: GpImage; out graphics: GpGraphics): GpStatus stdcall;

    //GdipDrawImageI: function(Graphics : GpGraphics; Image : PGpImage; X, Y: Integer): GpStatus stdcall;
    GdipDrawImageRectI: function(graphics: GpGraphics; image: GpImage; x, y, width, height: Integer): GPSTATUS; stdcall;
    GdipDrawImageRectRectI: function(graphics: GpGraphics; image: GpImage;
      dstx, dsty, dstwidth, dstheight, srcx, srcy, srcwidth, srcheight : Integer;
      srcUnit : GpUnit; imageAttributes: GpImageAttributes;
      callback: DrawImageAbort; callbackData: Pointer): GpStatus; stdcall;

    GdipDrawImagePointRect: function(graphics: GpGraphics; image: GpImage; x, y, srcX, srcY, srcWidth, srcHeight: Single; srcUnit: GpUnit): GpStatus; stdcall;

  //-- BG ---------------------------------------------------------- 18.08.2013 --
  function GetStatus(AErr: GpStatus): String;
  begin
    Result := Format('%d', [AErr]);
  end;

{$endif HasGDIPlus}

type
  EGDIPlus = class(Exception);

resourcestring
  SGDIError = '%s GDI error %s';
  SCannotCreateGraphics = 'Can''t Create Graphics.';

procedure GDICheck(const AProc : String; const AErr : GpStatus);
begin
  if AErr <> Ok then
    raise EGDIPlus.CreateFmt('%s GDI error %s', [AProc, GetStatus(AErr)]);
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
function IStreamFromStream(Stream: TStream): IStream;
// thanks to S�rgio Alexandre for this method.
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

{ ThtGpGraphics }

constructor ThtGpGraphics.Create(Handle: HDC);
begin
  inherited Create;
  GDICheck(SCannotCreateGraphics, GdipCreateFromHDC(Handle, fGraphics));
end;

constructor ThtGpGraphics.Create(Image: ThtGpImage);
begin
  inherited Create;
  GDICheck(SCannotCreateGraphics, GdipGetImageGraphicsContext(Image.fHandle, fGraphics));
end;

destructor ThtGpGraphics.Destroy;
begin
  if fGraphics <> nil then
    GDICheck('ThtGpGraphics.Destroy', GdipDeleteGraphics(fGraphics));
  inherited;
end;

procedure ThtGpGraphics.DrawImage(Image: THtGPImage; X, Y, Width, Height: Cardinal);
begin
  if ((Image.Width <= 10) and (Width > Image.Width)) or
    ((Image.Height <= 10) and (Height > Image.Height)) then
    DrawSmallStretchedImage(Image, X, Y, Width, Height)
  else
    GDICheck('ThtGpGraphics.DrawImage', GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Width, Height));
end;

procedure ThtGpGraphics.DrawSmallStretchedImage(Image: THtGPImage; X, Y, Width, Height: Integer);
{when a small image is getting enlarged, add a row and column to it copying
 the last row/column to the new row/column.  This gives much better interpolation.}
//const
//  NearestNeighbor = 5;
var
  g1, g2: ThtGpGraphics;
  BM1, BM2: ThtGpBitmap;
  W, H: integer;
begin
  {new bitmap with extra row and column}
  W := Image.Width + 1;
  H := Image.Height + 1;
  BM1 := THtGpBitmap.Create(W, H);
  g1 := THtGpGraphics.Create(BM1);
  try
    {draw the original image to BM1}
    g1.DrawImage(Image, 0, 0);
    {copy the additional column inside BM1}
    g1.DrawImage(BM1, W - 1,     0, 1, H - 1,  W - 2,     0, 1, H - 1);
    {copy the additional row incl. additional pixel inside BM1}
    g1.DrawImage(BM1,     0, H - 1, W,     1,      0, H - 2, W,     1);
    BM2 := ThtGpBitmap.Create(Width, Height);
    g2 := ThtGpGraphics.Create(BM2);
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
      BM2.Free;
    end;
  finally
    g1.Free;
    BM1.Free;
  end;
end;

procedure ThtGpGraphics.DrawImage(Image: THtGPImage; X, Y: Cardinal);
begin
  GDICheck('ThtGpGraphics.DrawImage', GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Image.Width, Image.Height));
end;

procedure THtGPGraphics.DrawImage(Image: ThtGpImage; x, y,
  srcx, srcy, srcwidth, srcheight: integer);
begin
  GDICheck('ThtGpGraphics.DrawImage',GdipDrawImagePointRect(fGraphics, Image.fHandle, x, y,
    srcx, srcy, srcwidth, srcheight, UnitPixel));
end;

procedure THtGPGraphics.DrawImage(Image: ThtGpImage; dx, dy, dw, dh, sx, sy, sw, sh: integer);
begin
 GDICheck('ThtGpGraphics.DrawImage', GdipDrawImageRectRectI(fGraphics, Image.fHandle, dx, dy, dw, dh,
    sx, sy, sw, sh, UnitPixel, nil, nil, nil));
end;

procedure ThtGpGraphics.Clear(Color: Cardinal);
begin
  GDICheck('ThtGpGraphics.Clear', GdipGraphicsClear(fGraphics, Color));
end;

procedure THtGPGraphics.ScaleTransform(sx, sy: Single);
begin
  GDICheck('ThtGpGraphics.DrawImage', GdipScaleWorldTransform(fGraphics, sx, sy, MatrixOrderPrepend));
end;

{ TGpImage }

constructor ThtGpImage.Create(Filename: ThtString; TmpFile: boolean = False);
var
  err: GpStatus;
begin
  inherited Create;
  err := GdipLoadImageFromFile(PWideChar(FileName), fHandle);
  if err <> Ok then
    if GetLastError = 2 then
      raise EGDIPlus.CreateFmt('Image file "%s" not found. GDI error %s', [FileName, GetStatus(err)])
    else
      raise EGDIPlus.CreateFmt('Can''t load image file "%s". GDI error %s', [FileName, GetStatus(err)]);
  if TmpFile then
    fFilename := Filename;
end;

constructor ThtGpImage.Create(IStr: IStream);
begin
  inherited Create;
  GDICheck('Can''t load image stream.', GdipLoadImageFromStream(IStr, fHandle));
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
constructor ThtGpImage.Create(Stream: TStream);
begin
  Create(IStreamFromStream(Stream));
end;

destructor ThtGpImage.Destroy;
begin
  GDICheck('ThtGpImage.Destroy', GdipDisposeImage(fHandle));
  if Length(fFilename) > 0 then
    try
      DeleteFile(fFilename);
    except
    end;
  inherited;
end;

function ThtGpImage.GetWidth: Cardinal;
begin
  if fWidth = 0 then
     GDICheck('ThtGpImage.GetWidth', GdipGetImageWidth(fHandle, fWidth));
  Result := fWidth;
end;

function ThtGpImage.GetHeight: Cardinal;
begin
  if fHeight = 0 then
     GDICheck('ThtGpImage.GetWidth', GdipGetImageHeight(fHandle, fHeight));
  Result := fHeight;
end;

function ThtGpImage.GetBitmap: TBitmap;
var
  g: ThtGpGraphics;
begin
  Result := TBitmap.Create;
  Result.Width := GetWidth;
  Result.Height := GetHeight;
  PatBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, Whiteness);
  g := ThtGpGraphics.Create(Result.Canvas.Handle);
  g.DrawImage(Self, 0, 0, Result.Width, Result.Height);
  g.Free;
end;

constructor ThtGpBitmap.Create(W, H: integer);
const
  PixelFormatGDI = $00020000; // Is a GDI-supported format
  PixelFormatAlpha = $00040000; // Has an alpha component
  PixelFormatCanonical = $00200000;
  PixelFormat32bppARGB = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
begin
  inherited Create;
  GDICheck(Format('Can''t create bitmap of size %d x %d.', [W, H]), GdipCreateBitmapFromScan0(W, H, 0, PixelFormat32bppARGB, nil, fHandle));
end;

constructor ThtGpBitmap.Create(IStr: IStream);
begin
  inherited Create;
  GDICheck('Can''t create bitmap from stream.', GdipCreateBitmapFromStream(IStr, fHandle));
end;

constructor ThtGpBitmap.Create(W, H: integer; Graphics: ThtGpGraphics);
begin
  inherited Create;
  GDICheck(Format('Can''t create bitmap of size %d x %d from graphics.', [W, H]), GdipCreateBitmapFromGraphics(W, H, Graphics.fGraphics, fHandle));
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
constructor ThtGpBitmap.Create(Stream: TStream);
begin
  Create(IStreamFromStream(Stream));
end;

function ThtGpBitmap.GetPixel(X, Y: integer): DWord;
begin
  GDICheck('ThtGpBitmap.GetPixel', GdipBitmapGetPixel(fHandle, X, Y, Result));
end;

procedure ThtGpBitmap.SetPixel(X, Y: integer; Color: DWord);
begin
  GDICheck('ThtGpBitmap.SetPixel', GdipBitmapSetPixel(fHandle, X, Y, Color));
end;

var
  GDIPlusCount: integer;
{$ifndef HasGDIPlus}
  InitToken: DWord;
  Startup: GdiplusStartupInput;
  LibHandle: THandle;
{$endif}

procedure CheckInitGDIPlus;
{$ifndef HasGDIPlus}
var
  Err: GpStatus;
{$endif}
begin
  if GDIPlusCount = 0 then
  begin
{$ifndef HasGDIPlus}
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
      begin
        FreeLibrary(LibHandle);
        Libhandle := 0;
      end;
    end;
{$else}
    GDIPlusActive := True;
{$endif HasGDIPlus}
  end;
  Inc(GDIPlusCount);
end;

procedure CheckExitGDIPlus;
begin
  Dec(GDIPlusCount);
  if GDIPlusCount = 0 then
  begin
{$ifndef HasGDIPlus}
    if GDIPlusActive then
    begin
      GdiplusShutdown(InitToken);
      GDIPlusActive := False;
    end;
    if LibHandle <> 0 then
    begin
      FreeLibrary(LibHandle);
      LibHandle := 0;
    end;
{$else}
    GDIPlusActive := False;
{$endif HasGDIPlus}
  end;
end;

{$endif NoGDIPlus}
end.
