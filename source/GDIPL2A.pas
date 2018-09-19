{
Version   11.9
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2018 by HtmlViewer Team

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
{$ifdef LCL}
  LCLType,
{$endif}
  Windows, ActiveX, SysUtils, Graphics, Classes, ClipBrd,
  HtmlGlobals;

var
  GDIPlusActive: Boolean;

{$ifndef HasGDIPlus}
type
  //types for GDIPlus API
  GpGraphics = Pointer;
  GpImage = Pointer;
  GpBitmap = Pointer;
  GpStatus = Integer;
  PixelFormat = Integer;
  GpImageAttributes = Pointer;

const
  NotImplemented: GpStatus = 6;
{$endif}

type

  { ThtGpImage }

  ThtGpImage = class(ThtGraphic)
  private
    FWidth, FHeight: Cardinal;
    FFilename: string;
    FBitmap: TBitmap;
  protected
    fHandle: GpImage;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
  public
    destructor Destroy; override;

    function GetBitmap: TBitmap;
{$ifdef LCL}
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat); override;
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat); override;
{$else}
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
{$endif}
    procedure LoadFromFile(const Filename: String{; TmpFile: boolean = False}); override;
    procedure LoadFromStream(IStr: IStream); reintroduce; overload;
    procedure LoadFromStream(Stream: TStream); overload; override;
    procedure SaveToFile(const Filename: string); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  ThtGpGraphics = class;

  ThtGpBitmap = class(ThtGpImage)
  public
    constructor Create(); overload; override;
    constructor Create(W, H: integer); reintroduce; overload;
    constructor Create(W, H: integer; Graphics: ThtGpGraphics); reintroduce; overload;
    function GetPixel(X, Y: integer): DWord;
    procedure SetPixel(X, Y: integer; Color: DWord);
  end;

  ThtGpGraphics = class(TObject) // like TCanvas
  private
    fGraphics: GpGraphics;
    procedure DrawSmallStretchedImage(Image: THtGPImage; X, Y, Width, Height: Integer);
  public
    constructor Create(Handle: HDC); overload;
    constructor Create(Image: ThtGpImage); overload;
    destructor Destroy; override;
    procedure DrawImage(Image: ThtGpImage; X, Y: Integer); overload;
    procedure DrawImage(Image: ThtGpImage; X, Y, Width, Height: Integer); overload;
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
//    GpColor = ARGB;
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
//    InterpolationMode = Integer;
//    TInterpolationMode = InterpolationMode;
//    PGpImageAttributes = Pointer;
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

const
  PixelFormatIndexed   = $00010000; // Indexes into a palette
  PixelFormatGDI       = $00020000; // Is a GDI-supported format
  PixelFormatAlpha     = $00040000; // Has an alpha component
  //PixelFormatPAlpha    = $00080000; // Pre-multiplied alpha
  PixelFormatExtended  = $00100000; // Extended color 16 bits/channel
  PixelFormatCanonical = $00200000;

  //PixelFormatUndefined = 0;
  //PixelFormatDontCare  = 0;
  //
  //PixelFormat1bppIndexed = (1 or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  //PixelFormat4bppIndexed = (2 or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  //PixelFormat8bppIndexed = (3 or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  //PixelFormat16bppGrayScale = (4 or (16 shl 8) or PixelFormatExtended);
  //PixelFormat16bppRGB555 = (5 or (16 shl 8) or PixelFormatGDI);
  //PixelFormat16bppRGB565 = (6 or (16 shl 8) or PixelFormatGDI);
  //PixelFormat16bppARGB1555 = (7 or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  //PixelFormat24bppRGB = (8 or (24 shl 8) or PixelFormatGDI);
  //PixelFormat32bppRGB = (9 or (32 shl 8) or PixelFormatGDI);
  PixelFormat32bppARGB   = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  //PixelFormat32bppPARGB  = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  //PixelFormat48bppRGB  = (12 or (48 shl 8) or PixelFormatExtended);
  //PixelFormat64bppARGB = (13 or (64 shl 8) or PixelFormatAlpha or PixelFormatCanonical or PixelFormatExtended);
  //PixelFormat64bppPARGB = (14 or (64 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatExtended);
  //PixelFormat32bppCMYK = (15 or (32 shl 8));
  //PixelFormatMax = 16;

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

    //GdipCreateBitmapFromStream: function(stream: ISTREAM; out bitmap: GpBitmap): GpStatus stdcall;
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

function GetPixelFormatSize(const pixfmt : PixelFormat) : Cardinal; {$ifdef UseInline} inline; {$endif}
begin
  Result := (pixfmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(const pixfmt : PixelFormat) : Boolean; {$ifdef UseInline} inline; {$endif}
begin
  Result := (pixfmt and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(const pixfmt : PixelFormat) : Boolean; {$ifdef UseInline} inline; {$endif}
begin
  Result := (pixfmt and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(const pixfmt : PixelFormat) : Boolean; {$ifdef UseInline} inline; {$endif}
begin
  Result := (pixfmt and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(const  pixfmt : PixelFormat) : Boolean; {$ifdef UseInline} inline; {$endif}
begin
  Result := (pixfmt and PixelFormatCanonical) <> 0;
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
    raise EGDIPlus.CreateFmt(SGDIError, [AProc, GetStatus(AErr)]);
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
    {GDICheck('ThtGpGraphics.Destroy',} GdipDeleteGraphics(fGraphics) {)};
  inherited;
end;

procedure ThtGpGraphics.DrawImage(Image: THtGPImage; X, Y, Width, Height: Integer);
var
  IW, IH: Integer;
begin
  if ((Image.Width <= 10) and (Width > Image.Width)) or ((Image.Height <= 10) and (Height > Image.Height)) then
    DrawSmallStretchedImage(Image, X, Y, Width, Height)
  else
  begin
    //BG, 22.11.2016: calling GdipDrawImageRectI() tents to skip the right and bottom parts of the image.
    //GDICheck('ThtGpGraphics.DrawImage', GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Width, Height));
    IW := Image.Width;
    if Width < IW then
      Inc(IW);
    IH := Image.Height;
    if Height < IH then
      Inc(IH);
    DrawImage(Image, X, Y, Width, Height, 0, 0, IW, IH);
  end;
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

procedure ThtGpGraphics.DrawImage(Image: THtGPImage; X, Y: Integer);
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

{ ThtGpImage }

destructor ThtGpImage.Destroy;
begin
  fBitmap.Free;
  if fHandle <> nil then
    GdipDisposeImage(fHandle);
  if Length(fFilename) > 0 then
    DeleteFile(fFilename);
  inherited;
end;

procedure ThtGpImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  g: ThtGpGraphics;
begin
  g := ThtGpGraphics.Create(ACanvas.Handle);
  try
    g.DrawImage(Self, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, 0, 0, Width, Height);
  finally
    g.Free;
  end;
end;

function ThtGpImage.GetHeight: Integer;
begin
  if fHeight = 0 then
     GDICheck('ThtGpImage.GetHeight', GdipGetImageHeight(fHandle, fHeight));
  Result := fHeight;
end;

function ThtGpImage.GetWidth: Integer;
begin
  if fWidth = 0 then
     GDICheck('ThtGpImage.GetWidth', GdipGetImageWidth(fHandle, fWidth));
  Result := fWidth;
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtGpImage.LoadFromClipboardFormat(
{$ifdef LCL}
  FormatID: TClipboardFormat
{$else}
  AFormat: Word; AData: THandle; APalette: HPALETTE
{$endif}
);
begin
  GDICheck('ThtGpImage.LoadFromClipboardFormat', NotImplemented );
end;

procedure ThtGpImage.LoadFromFile(const Filename: String);
var
  err: GpStatus;
begin
  err := GdipLoadImageFromFile(PWideChar(WideString(FileName)), fHandle);
  if err <> Ok then
    if GetLastError = 2 then
      raise EGDIPlus.CreateFmt('Image file "%s" not found. GDI error %s', [FileName, GetStatus(err)])
    else
      raise EGDIPlus.CreateFmt('Can''t load image file "%s". GDI error %s', [FileName, GetStatus(err)]);
//  if TmpFile then
    fFilename := Filename;
end;

procedure ThtGpImage.LoadFromStream(IStr: IStream);
begin
  GDICheck('Can''t load image stream.', GdipLoadImageFromStream(IStr, fHandle));
end;

//-- BG ---------------------------------------------------------- 01.04.2012 --
procedure ThtGpImage.LoadFromStream(Stream: TStream);
// thanks to Sérgio Alexandre for this method.
var
  Handle: HGLOBAL;
  Ptr: Pointer absolute Handle;
  IStr: IStream;
begin
  Handle := GlobalAlloc(GPTR, Stream.Size);
  if Handle <> 0 then
  begin
    Stream.Read(Ptr^, Stream.Size);
    CreateStreamOnHGlobal(Handle, True, IStr);
    LoadFromStream(IStr);
  end;
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtGpImage.SaveToClipboardFormat(
{$ifdef LCL}
  FormatID: TClipboardFormat
{$else}
  var AFormat: Word; var AData: THandle; var APalette: HPALETTE
{$endif}
);
begin
  GDICheck('ThtGpImage.SaveToClipboardFormat', NotImplemented );
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtGpImage.SaveToFile(const Filename: string);
begin
  GDICheck('Can''t save to file', NotImplemented {GdipSaveImageToFile(fhandle, PWideChar(FileName), , )} );
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtGpImage.SaveToStream(Stream: TStream);
begin
  GDICheck('Can''t save to stream', NotImplemented {GdipSaveImageToStream(fhandle, IStreamOfStream, , )} );
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtGpImage.SetHeight(Value: Integer);
begin
  GDICheck('ThtGpImage.SetHeight', NotImplemented );
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtGpImage.SetWidth(Value: Integer);
begin
  GDICheck('ThtGpImage.SetWidth', NotImplemented );
end;

function ThtGpImage.GetBitmap: TBitmap;
var
  g: ThtGpGraphics;
begin
  if fBitmap = nil then
    fBitmap := TBitmap.Create;
  Result := fBitmap;
  Result.Width := GetWidth;
  Result.Height := GetHeight;
  PatBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, Whiteness);
  g := ThtGpGraphics.Create(Result.Canvas.Handle);
  g.DrawImage(Self, 0, 0, Result.Width, Result.Height);
  g.Free;
end;

function ThtGpImage.GetEmpty: Boolean;
begin
  Result := fHandle = nil;
end;

constructor ThtGpBitmap.Create;
begin
  inherited Create;
end;

constructor ThtGpBitmap.Create(W, H: integer);
begin
  inherited Create;
  GDICheck(Format('Can''t create bitmap of size %d x %d.', [W, H]), GdipCreateBitmapFromScan0(W, H, 0, PixelFormat32bppARGB, nil, fHandle));
end;

constructor ThtGpBitmap.Create(W, H: integer; Graphics: ThtGpGraphics);
begin
  inherited Create;
  GDICheck(Format('Can''t create bitmap of size %d x %d from graphics.', [W, H]), GdipCreateBitmapFromGraphics(W, H, Graphics.fGraphics, fHandle));
end;

//procedure ThtGpBitmap.LoadFromStream(IStr: IStream);
//begin
//  GDICheck('Can''t create bitmap from stream.', GdipCreateBitmapFromStream(IStr, fHandle));
//end;
//
////-- BG ---------------------------------------------------------- 01.04.2012 --
//procedure ThtGpBitmap.LoadFromStream(Stream: TStream);
//begin
//  LoadFromStream(IStreamFromStream(Stream));
//end;

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
  InitToken: DWord;
  Startup: GdiplusStartupInput;
{$ifndef HasGDIPlus}
  LibHandle: THandle;
{$endif}

procedure CheckInitGDIPlus;
var
  Err: GpStatus;
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
      //@GdipCreateBitmapFromStream := GetProcAddress(LibHandle, 'GdipCreateBitmapFromStream');
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
    FillChar(Startup, sizeof(Startup), 0);
    Startup.GdiplusVersion := 1;
    Err := GdiPlusStartup(InitToken, @Startup, nil);
    GDIPlusActive := Err = Ok;
{$endif HasGDIPlus}
  end;
  Inc(GDIPlusCount);
end;

procedure CheckExitGDIPlus;
begin
  Dec(GDIPlusCount);
  if GDIPlusCount = 0 then
  begin
    if GDIPlusActive then
    begin
      GdiplusShutdown(InitToken);
      GDIPlusActive := False;
    end;
{$ifndef HasGDIPlus}
    if LibHandle <> 0 then
    begin
      FreeLibrary(LibHandle);
      LibHandle := 0;
    end;
{$endif HasGDIPlus}
  end;
end;

{$endif NoGDIPlus}
end.
