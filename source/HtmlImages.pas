{
Version   11.6
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2015 by HtmlViewer Team

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

unit HtmlImages;

interface

uses
{$ifdef LCL}
  LclIntf, IntfGraphics, FpImage, LclType, LResources, LMessages, HtmlMisc,
{$else}
  Windows, Jpeg,
{$endif}
  SysUtils, Classes, Graphics, Forms,
  Controls,
  //Messages,
  //Variants,
  Types,
  Math,
{$IFNDEF NoGDIPlus}
  GDIPL2A,
{$ENDIF}
{$ifdef METAFILEMISSING}
  MetaFilePrinter,
{$endif}
  UrlSubs,
  HtmlCaches,
  HtmlGlobals,
  HtmlGif2,
  StyleTypes,
{$IFDEF UNICODE} PngImage, {$ENDIF}
  DitherUnit;

const
  HandCursor = crHandPoint; //10101;
  OldThickIBeamCursor = 2;
  UpDownCursor = 10103;
  UpOnlyCursor = 10104;
  DownOnlyCursor = 10105;
{$ifdef LCL}
{$else}
  DefaultBitmap = 1002;
  ErrBitmap = 1001;
  ErrBitmapMask = 1005;
  Hand_Cursor = 1003;
  ThickIBeam_Cursor = 1006;
{$endif}

type

//------------------------------------------------------------------------------
// ThtImage is an image wrapper.
//------------------------------------------------------------------------------
// It is used in place of any bitmap / image type used in HtmlViewer < 11.
// Currently it supports ThtBitmap, (animated) TGifImage, TGpImage or ThtMetafile
// It replaces the unspecific TgpObject and TBitmapItem of the old image cache.
//------------------------------------------------------------------------------
  TGpObject = TObject;

  ThtImageFormat = (itNone, itBmp, itIco, itCur, itGif, itPng, itJpg, {$IFNDEF NoGDIPlus} itTiff, {$ENDIF NoGDIPlus} itMetafile);
  TTransparency = (NotTransp, LLCorner, TrGif, TrPng);

  //BG, 09.04.2011
  ThtImage = class(ThtCachable)
  protected
    function GetGpObject: TGpObject; virtual; abstract;
    function GetBitmap: TBitmap; virtual; abstract;
    function GetImageHeight: Integer; virtual; abstract;
    function GetImageWidth: Integer; virtual; abstract;
    function GetMask: TBitmap; virtual; abstract;
  public
    Transp: TTransparency; {identifies what the mask is for}
    constructor Create(Tr: TTransparency); overload;
    procedure Draw(Canvas: TCanvas; X, Y, W, H: Integer); virtual; abstract;
    procedure Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor); virtual;
    procedure DrawTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer); virtual;
    procedure PrintTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer; BgColor: TColor); virtual;
    property Bitmap: TBitmap read GetBitmap;
    property Mask: TBitmap read GetMask;
    property Height: Integer read GetImageHeight;
    property Width: Integer read GetImageWidth;
    //property MImage: TGpObject read GetGpObject;
  end;

  TGetImageEvent = function(Sender: TObject; const Url: ThtString): ThtImage of object;

//------------------------------------------------------------------------------
// ThtImageCache is the image cache, that holds the above image wrappers.
//------------------------------------------------------------------------------

  ThtImageCache = class(ThtCache)
  {a list of image filenames and their ThtImages}
  public
    constructor Create;
    destructor Destroy; override;
    function AddObject(const S: ThtString; AObject: ThtImage): Integer; reintroduce; {$ifdef UseInline} inline; {$endif}
    // InsertImage(): Get the proper Index by using Find().
    procedure InsertImage(Index: Integer; const S: ThtString; AObject: ThtImage); {$ifdef UseInline} inline; {$endif}
    function GetImage(I: Integer): ThtImage; {$ifdef UseInline} inline; {$endif}
  end;

//------------------------------------------------------------------------------
// always supported image kinds: bitmap and (animated) gif.
//------------------------------------------------------------------------------
// optionally supported image kinds:
// if NoGDIPlus  is not defined: png, tiff, ico, cur, ...
// if NoMetafile is not defined: metafile,
//------------------------------------------------------------------------------

  //BG, 11.01.2015: remember bitmap etc. until it is inserted:
  ThtBitmapToInsert = class
  public
    Bitmap: TBitmap;
    Color: TColor;
    Transp: TTransparency;
    OwnsBitmap: Boolean;
    constructor Create(AImage: TBitmap; ATransp: TTransparency; AColor: TColor; AOwnsBitmap: Boolean = True); overload;
  end;

  //BG, 09.04.2011
  ThtBitmapImage = class(ThtImage)
  private
    Bitmap, Mask: TBitmap;
    OwnsBitmap, OwnsMask: Boolean;
  protected
    function GetGpObject: TGpObject; override;
    function GetBitmap: TBitmap; override;
    function GetImageHeight: Integer; override;
    function GetImageWidth: Integer; override;
    function GetMask: TBitmap; override;
  public
    constructor Create(AImage, AMask: TBitmap; Tr: TTransparency; AOwnsBitmap: Boolean = True; AOwnsMask: Boolean = True); overload;
    constructor Create(AImage: TBitmap; Tr: TTransparency; Color: TColor; AOwnsBitmap: Boolean = True); overload;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y, W, H: Integer); override;
  end;

  //BG, 09.04.2011
  ThtGifImage = class(ThtImage)
  private
    FGif: TGifImage;
  protected
    function GetGpObject: TGpObject; override;
    function GetBitmap: TBitmap; override;
    function GetImageHeight: Integer; override;
    function GetImageWidth: Integer; override;
    function GetMask: TBitmap; override;
  public
    constructor Create(AImage: TGifImage); overload;
    destructor Destroy; override;
    function Clone: ThtGifImage;
    procedure Draw(Canvas: TCanvas; X, Y, W, H: Integer); override;
    property Gif: TGifImage read FGif;
  end;

{$IFNDEF NoGDIPlus}
  //BG, 09.04.2011
  //BG, 18.05.2014: GDIPlus can handle image kinds: PNG, TIFF, JPEG, ICO, EMF, WMF, EXIF, BMP, GIF (not animated)
  ThtGdipImage = class(ThtImage)
  private
    Gpi: ThtGpImage;
  protected
    function GetGpObject: TGpObject; override;
    function GetBitmap: TBitmap; override;
    function GetImageHeight: Integer; override;
    function GetImageWidth: Integer; override;
    function GetMask: TBitmap; override;
  public
    constructor Create(AImage: ThtGpImage); overload;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y, W, H: Integer); override;
    procedure DrawTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer); override;
    procedure Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor); override;
  end;
{$ENDIF !NoGDIPlus}

{$IFNDEF NoMetafile}
//------------------------------------------------------------------------------
// bitmap meta file
//------------------------------------------------------------------------------

  ThtMetaFile = class(TMetaFile)
  private
    FhtBitmap: ThtBitmapImage;
    FWhiteBGBitmap: TBitmap;
    function GetBitmap: TBitmap;
    function GetMask: TBitmap;
    procedure Construct;
    function GetWhiteBGBitmap: TBitmap;
  public
    destructor Destroy; override;
    property Bitmap: TBitmap read GetBitmap;
    property Mask: TBitmap read GetMask;
    property WhiteBGBitmap: TBitmap read GetWhiteBGBitmap;
  end;

  //BG, 09.04.2011
  ThtMetafileImage = class(ThtImage)
  private
    MetaFile: ThtMetafile;
  protected
    function GetGpObject: TGpObject; override;
    function GetBitmap: TBitmap; override;
    function GetImageHeight: Integer; override;
    function GetImageWidth: Integer; override;
    function GetMask: TBitmap; override;
  public
    constructor Create(AImage: ThtMetaFile); overload;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y, W, H: Integer); override;
    procedure Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor); override;
  end;
{$ENDIF !NoMetafile}

//------------------------------------------------------------------------------
// image methods
//------------------------------------------------------------------------------

function LoadImageFromFile(const FName: ThtString; Transparent: TTransparency{; var AMask: TBitmap}): ThtImage;
function LoadImageFromStream(Stream: TStream; Transparent: TTransparency{; var AMask: TBitmap}): ThtImage;
//function KindOfImage(Stream: TStream): ThtImageFormat;

function GetImageHeight(Image: TGpObject): Integer;
function GetImageWidth(Image: TGpObject): Integer;
function EnlargeImage(Image: TGpObject; W, H: Integer): TBitmap;

function GetImageMask(Image: TBitmap; ColorValid: boolean; AColor: TColor): TBitmap;
procedure FinishTransparentBitmap(ahdc: HDC; InImage, Mask: TBitmap; xStart, yStart, W, H: Integer);
procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: Integer; Bitmap, Mask: TBitmap; YI, HI: Integer);

function GetClipRegion(Canvas: TCanvas): Integer;

procedure DrawBackground(ACanvas: TCanvas; ARect: TRect; XStart, YStart, XLast, YLast: Integer;
  Image: ThtImage; {Mask: TBitmap; AniGif: TGifImage;} BW, BH: Integer; BGColor: TColor);
{draw the background color and any tiled images on it}
{ARect, the cliprect, drawing outside this will not show but images may overhang
 XStart, YStart are first image position already calculated for the cliprect and parameters.
 XLast, YLast   Tiling stops here.
 BW, BH  bitmap dimensions.
}

procedure DoImageStuff(Canvas: TCanvas; IW, IH: Cardinal; BGImage: ThtImage; PRec: PtPositionRec;
  var TiledImage: TGpObject; var TiledMask: TBitmap; var NoMask: boolean);
{Set up for the background image. Allow for tiling, and transparency
  BGImage is the image
  PRec describes the location and tiling
  IW, IH, the width and height of the background
}


{$IFNDEF NoGDIPlus}
procedure DrawGpImage(Handle: THandle; Image: ThtGpImage; DestX, DestY: Integer); overload;
procedure DrawGpImage(Handle: THandle; Image: ThtGpImage; DestX, DestY, SrcX, SrcY, SrcW, SrcH: Integer); overload;
procedure StretchDrawGpImage(Handle: THandle; Image: ThtGpImage; DestX, DestY, DestW, DestH: Integer);
procedure PrintGpImageDirect(Handle: THandle; Image: ThtGpImage; DestX, DestY: Integer; ScaleX, ScaleY: single);
procedure StretchPrintGpImageDirect(Handle: THandle; Image: ThtGpImage; DestX, DestY, DestW, DestH: Integer; ScaleX, ScaleY: Single);
procedure StretchPrintGpImageOnColor(Canvas: TCanvas; Image: ThtGpImage; DestX, DestY, DestW, DestH: Integer; Color: TColor = clWhite);
{$ENDIF NoGDIPlus}

//------------------------------------------------------------------------------

var
  DefBitMap, ErrorBitMap, ErrorBitmapMask: TBitMap;
  DefImage: ThtBitmapImage;
  ErrorImage: ThtBitmapImage;

implementation

//------------------------------------------------------------------------------
// device independent bitmap wrapper
//------------------------------------------------------------------------------

type

  TDib = class(TObject)
  private
    Info: PBitmapInfoHeader;
    InfoSize: Integer;
    Image: Pointer;
    ImageSize: Integer;
    FHandle: THandle;
    procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP);
    procedure GetDIBX(DC: HDC; Bitmap: HBITMAP; Palette: HPALETTE);
    procedure Allocate(Size: Integer);
    procedure DeAllocate;
  public
    constructor CreateDIB(DC: HDC; Bitmap: TBitmap);
    destructor Destroy; override;
    function CreateDIBmp: hBitmap;
    procedure DrawDIB(DC: HDC; X, Y, W, H: Integer; ROP: DWord);
  end;

//------------------------------------------------------------------------------

function KindOfImage(Stream: TStream): ThtImageFormat;
var
  Pos: Int64;
  Magic: DWord;
  WMagic: Word absolute Magic;
begin
  Pos := Stream.Position;
  Stream.Position := 0;
  try
    Stream.Read(Magic, sizeof(Magic));
    if Magic = $38464947 then
    begin
//      Stream.Read(BMagic, sizeof(BMagic));
//      if BMagic = Ord('9') then
//        Result := Gif89
//      else
        Result := itGif;
    end
    else if Magic = $474E5089 then
      Result := itPng
    else if Magic = $00010000 then
      Result := itIco
    else if Magic = $00020000 then
      Result := itCur
    else
      case WMagic of
        $4D42: Result := itBmp;
        $D8FF: Result := itJpg;
{$ifndef NoGDIPlus}
         // .TIFF little endian (II - Intell)
        $4949: if (Magic and $2A0000)=$2A0000 then
                 Result := itTiff
               else
                 Result := itNone;
         // .TIFF big endian (MM - Motorola)
        $4D4D: if (Magic and $2A000000)=$2A000000 then
                 Result := itTiff
               else
                 Result := itNone;
{$endif !NoGDIPlus}
      else
        Result := itNone;
      end;
  finally
    Stream.Position := Pos;
  end;
end;

function ConvertImage(Bitmap: TBitmap): TBitmap;
{convert bitmap into a form for BitBlt later}

  function DIBConvert: TBitmap;
  var
    DC: HDC;
    DIB: TDib;
    OldBmp: HBitmap;
    OldPal: HPalette;
    Hnd: HBitmap;
  begin
    DC := CreateCompatibleDC(0);
    OldBmp := SelectObject(DC, Bitmap.Handle);
    OldPal := SelectPalette(DC, ThePalette, False);
    RealizePalette(DC);
    DIB := TDib.CreateDIB(DC, Bitmap);
    Hnd := DIB.CreateDIBmp;
    DIB.Free;
    SelectPalette(DC, OldPal, False);
    SelectObject(DC, OldBmp);
    DeleteDC(DC);
    Bitmap.Free;
    Result := TBitmap.Create;
    Result.Handle := Hnd;
    if (ColorBits = 8) and (Result.Palette = 0) then
      Result.Palette := CopyPalette(ThePalette);
  end;

begin
  if not Assigned(Bitmap) then
  begin
    Result := nil;
    Exit;
  end;

  if ColorBits > 8 then
  begin
    if Bitmap.PixelFormat <= pf8bit then
      Result := DIBConvert
    else
      Result := Bitmap;
    Exit;
  end;

  if Bitmap.HandleType = bmDIB then
  begin
    Result := GetBitmap(Bitmap);
    Bitmap.Free;
    Exit;
  end;
  Result := DIBConvert;
end;

{----------------GetImageAndMaskFromStream}

function GetImageMask(Image: TBitmap; ColorValid: boolean; AColor: TColor): TBitmap;
begin
  Result := nil;
  try
    if ColorValid then
      Image.TransparentColor := AColor; {color has already been selected}
  {else the transparent color is the lower left pixel of the bitmap}

    Image.Transparent := True;

    Result := TBitmap.Create;
    Result.Handle := Image.ReleaseMaskHandle;
    Image.Transparent := False;
  except
    FreeAndNil(Result);
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2010 --
function LoadImageFromStream(Stream: TStream; Transparent: TTransparency{; var AMask: TBitmap}): ThtImage;
// extracted from ThtDocument.GetTheBitmap(), ThtDocument.InsertImage(), and ThtDocument.ReplaceImage()

  function LoadMetafileImage: ThtImage;
{$ifndef NoMetafile}
  var
    Meta: ThtMetaFile;
  begin
    Meta := ThtMetafile.Create;
    try
      Meta.LoadFromStream(Stream);
    except
      Meta.Free;
      raise;
    end;
    Result := ThtMetafileImage.Create(Meta);
  end;
{$else}
  begin
    Result := nil;
  end;
{$endif NoMetafile}

  function LoadGpImage: ThtImage;
{$ifndef NoGDIPlus}
  var
    Image: ThtGpImage;
  begin
    Result := nil;
    if GDIPlusActive then
    begin
      Image := ThtGpImage.Create(Stream);
      try
        Result := ThtGdipImage.Create(Image);
      except
        Image.Free;
        raise;
      end;
    end;
  end;
{$else}
  begin
    Result := nil;
  end;
{$endif !NoGDIPlus}

var
  Bitmap, Mask: TBitmap;

  function LoadGif: ThtImage;
  var
    Gif: TGifImage;
    NonAnimated: Boolean;
  begin
    Gif := LoadGifFromStream(NonAnimated, Stream);
    if NonAnimated then
    begin {else already have animated GIF}
      try
        Bitmap := TBitmap.Create;
        Bitmap.Assign(Gif.MaskedBitmap);
        if Gif.IsTransparent then
        begin
          Mask := TBitmap.Create;
          Mask.Assign(Gif.Mask);
          Transparent := TrGif;
        end
        else if Transparent = LLCorner then
          Mask := GetImageMask(Bitmap, False, 0);
      finally
        Gif.Free;
      end;
      Result := nil;
    end
    else
      Result := ThtGifImage.Create(Gif);
  end;

  function LoadPng: ThtImage;
{$ifdef LCL}
  var
    pngImage: TPortableNetworkGraphic;
  begin
    pngImage := TPortableNetworkGraphic.Create;
    try
      Transparent := TrPng;
      pngImage.LoadFromStream(Stream);
      if ColorBits <= 8 then
        pngImage.PixelFormat := pf8bit
      else
        pngImage.PixelFormat := pf24bit;
      Bitmap := TBitmap.Create;
      Bitmap.Assign(pngImage);
      pngImage.Mask(clDefault);
      if pngImage.MaskHandleAllocated then
      begin
        Mask := TBitmap.Create;
        Mask.LoadFromBitmapHandles(pngImage.MaskHandle, 0);
      end;
    finally
      pngImage.Free;
    end;
{$else}
  begin
{$endif}
    Result := nil;
  end;

  function LoadJpeg: ThtImage;
  var
    jpImage: TJpegImage;
  begin
    jpImage := TJpegImage.Create;
    try
      jpImage.LoadFromStream(Stream);
      if ColorBits <= 8 then
      begin
        jpImage.PixelFormat := {$ifdef LCL} pf8bit {$else} jf8bit {$endif};
        if not jpImage.GrayScale and (ColorBits = 8) then
          jpImage.Palette := CopyPalette(ThePalette);
      end
      else
        jpImage.PixelFormat := {$ifdef LCL} pf24bit {$else} jf24bit {$endif};
      Bitmap := TBitmap.Create;
      Bitmap.Assign(jpImage);
    finally
      jpImage.Free;
    end;
    Transparent := NotTransp;
    Result := nil;
  end;

  function LoadIco: ThtImage;
  var
    Icon: TIcon;
  begin
    Icon := TIcon.Create;
    try
      Icon.LoadFromStream(Stream);

      Bitmap := TBitmap.Create;
      Bitmap.Assign(Icon);
      Transparent := LLCorner;
    finally
      Icon.Free;
    end;
    Result := nil;
  end;

  function LoadBmp: ThtImage;
  begin
    Bitmap := TBitmap.Create;
    Bitmap.LoadFromStream(Stream);
    Result := nil;
  end;

var
  ImageFormat: ThtImageFormat;
begin
  Result := nil;
  if (Stream = nil) or (Stream.Size < 20) then
    Exit;

  Stream.Position := 0;
  Mask := nil;
  Bitmap := nil;
  try
    ImageFormat := KindOfImage(Stream);

{$ifndef NoGDIPlus}
    if ImageFormat in [itGif] then
    else if (ImageFormat in [itBmp]) {and (Transparent <> NotTransp)} then
    else
      try
        Result := LoadGpImage;
      except
        // just continue without image...
      end;
{$endif !NoGDIPlus}

    if Result = nil then
      case ImageFormat of
        itIco,
        itCur: Result := LoadIco;
        itGif: Result := LoadGif;
        itPng: Result := LoadPng;
        itJpg: Result := LoadJpeg;
        itBmp: Result := LoadBmp;
      end;

    if Result = nil then
      if Bitmap <> nil then
      begin
        //if Transparent = LLCorner then
          Mask := GetImageMask(Bitmap, False, 0);
        Bitmap := ConvertImage(Bitmap);
        Result := ThtBitmapImage.Create(Bitmap, Mask, Transparent);
      end;
  except
    FreeAndNil(Bitmap);
    FreeAndNil(Mask);
    FreeAndNil(Result);
  end;
{$IFNDEF NoMetafile}
  if Result = nil then
    try
      Result := LoadMetafileImage;
    except
      // just continue without image...
    end;
{$ENDIF}
end;

//-- BG ---------------------------------------------------------- 26.09.2010 --
function LoadImageFromFile(const FName: ThtString; Transparent: TTransparency{; var AMask: TBitmap}): ThtImage;
// extracted from ThtDocument.GetTheBitmap() and redesigned.
// Now the image file is loaded once only (was: 2 to 3 times) and GetImageAndMaskFromFile() is obsolete.
var
  Stream: TStream;
begin {look for the image file}
  Result := nil;
  if FileExists(FName) then
  begin
    Stream := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadImageFromStream(Stream, Transparent);
    finally
      Stream.Free;
    end;
  end;
end;

{----------------FinishTransparentBitmap }

procedure FinishTransparentBitmap(ahdc: HDC; InImage, Mask: TBitmap; xStart, yStart, W, H: Integer);
var
  bmAndBack,
    bmSave,
    bmBackOld,
    bmObjectOld: HBitmap;
  hdcInvMask,
    hdcMask,
    hdcImage: HDC;
  DestSize, SrcSize: TPoint;
  OldBack, OldFore: TColor;
  BM: {$ifdef LCL} LclType.Bitmap {$else} Windows.TBitmap {$endif};
  Image: TBitmap;

begin
  Image := TBitmap.Create; {protect original image}
  try
    Image.Assign(InImage);

    hdcImage := CreateCompatibleDC(ahdc);
    SelectObject(hdcImage, Image.Handle); { select the bitmap }

  { convert bitmap dimensions from device to logical points}
    SrcSize.x := Image.Width;
    SrcSize.y := Image.Height;
    DPtoLP(hdcImage, SrcSize, 1);

    DestSize.x := W;
    DestSize.y := H;
    DPtoLP(hdcImage, DestSize, 1);

  { create a bitmap for each DC}
  { monochrome DC}
    bmAndBack := CreateBitmap(SrcSize.x, SrcSize.y, 1, 1, nil);

    bmSave := CreateCompatibleBitmap(ahdc, DestSize.x, DestSize.y);
    GetObject(bmSave, SizeOf(BM), @BM);
    if (BM.bmBitsPixel > 1) or (BM.bmPlanes > 1) then
    begin
    { create some DCs to hold temporary data}
      hdcInvMask := CreateCompatibleDC(ahdc);
      hdcMask := CreateCompatibleDC(ahdc);

    { each DC must select a bitmap object to store pixel data}
      bmBackOld := SelectObject(hdcInvMask, bmAndBack);

    { set proper mapping mode}
      SetMapMode(hdcImage, GetMapMode(ahdc));

      bmObjectOld := SelectObject(hdcMask, Mask.Handle);

    { create the inverse of the object mask}
      BitBlt(hdcInvMask, 0, 0, SrcSize.x, SrcSize.y, hdcMask, 0, 0, NOTSRCCOPY);

    {set the background color of the source DC to the color contained in the
     parts of the bitmap that should be transparent, the foreground to the parts that
     will show}
      OldBack := SetBkColor(ahDC, clWhite);
      OldFore := SetTextColor(ahDC, clBlack);

    { Punch out a black hole in the background where the image will go}
      SetStretchBltMode(ahDC, WhiteOnBlack);
      StretchBlt(ahDC, XStart, YStart, DestSize.x, DestSize.y, hdcMask, 0, 0, SrcSize.x, SrcSize.y, SRCAND);

    { mask out the transparent colored pixels on the bitmap}
      BitBlt(hdcImage, 0, 0, SrcSize.x, SrcSize.y, hdcInvMask, 0, 0, SRCAND);

    { XOR the bitmap with the background on the destination DC}
{$IFDEF HalfToneStretching}
      SetStretchBltMode(ahDC, HALFTONE);
{$ELSE}
      SetStretchBltMode(ahDC, COLORONCOLOR);
{$ENDIF}
      StretchBlt(ahDC, XStart, YStart, W, H, hdcImage, 0, 0, Image.Width, Image.Height, SRCPAINT);

      SetBkColor(ahDC, OldBack);
      SetTextColor(ahDC, OldFore);

    { delete the memory bitmaps}
      DeleteObject(SelectObject(hdcInvMask, bmBackOld));
      SelectObject(hdcMask, bmObjectOld);

    { delete the memory DCs}
      DeleteDC(hdcInvMask);
      DeleteDC(hdcMask);
    end
    else
    begin
      DeleteObject(bmAndBack);
    end;
    DeleteObject(bmSave);
    DeleteDC(hdcImage);
  finally
    Image.Free;
  end;
end;


{ Regions }

//-- BG ---------------------------------------------------------- 13.04.2011 --
function GetClipRegion(Canvas: TCanvas): Integer;
// returns 0, if canvas is unclipped, else handle of region
begin
  Result := CreateRectRgn(0, 0, 1, 1);
  if GetClipRgn(Canvas.Handle, Result) = 0 then
  begin
    {0 = region does not exists}
    DeleteObject(Result);
    Result := 0;
  end;
end;


{----------------BitmapToRegion}

function BitmapToRegion(ABmp: TBitmap; XForm: PXForm; TransparentColor: TColor): HRGN;
{Find a Region corresponding to the non-transparent area of a bitmap.

 Thanks to Felipe Machado.  See http://www.delphi3000.com/
 Minor modifications made.}
const
  AllocUnit = 100;
type
  PRectArray = ^TRectArray;
  TRectArray = array[0..(MaxInt div SizeOf(TRect)) - 1] of TRect;
var
  pr: PRectArray; // used to access the rects array of RgnData by index
  h: HRGN; // Handles to regions
  RgnData: PRgnData; // Pointer to structure RGNDATA used to create regions
  lr, lg, lb: Byte; // values for lowest and hightest trans. colors
  x, y, x0: Integer; // coordinates of current rect of visible pixels
  maxRects: Cardinal; // Number of rects to realloc memory by chunks of AllocUnit
{$ifdef LCL}
  bmp: TLazIntfImage;
  b: TFpColor;
{$else}
  b: PByteArray; // used to easy the task of testing the byte pixels (R,G,B)
  ScanLinePtr: Pointer; // Pointer to current ScanLine being scanned
  ScanLineInc: Integer; // Offset to next bitmap scanline (can be negative)
  bmp: TBitmap;
{$endif}
begin
  Result := 0;
  lr := GetRValue(TransparentColor);
  lg := GetGValue(TransparentColor);
  lb := GetBValue(TransparentColor);
  { ensures that the pixel format is 32-bits per pixel }
{$ifdef LCL}
  bmp := TLazIntfImage.Create(0,0);
  try
    bmp.LoadFromBitmap(ABmp.Handle, 0);
    { alloc initial region data }
    maxRects := AllocUnit;
    GetMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects));
    FillChar(RgnData^, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), 0);
    try
      with RgnData^.rdh do
      begin
        dwSize := SizeOf(TRgnDataHeader);
        iType := RDH_RECTANGLES;
        nCount := 0;
        nRgnSize := 0;
        SetRect(rcBound, MAXLONG, MAXLONG, 0, 0);
      end;
      { scan each bitmap row - the orientation doesn't matter (Bottom-up or not) }
      for y := 0 to bmp.Height - 1 do
      begin
        x := 0;
        while x < bmp.Width do
        begin
          x0 := x;
          while x < bmp.Width do
          begin
            b := bmp[x,y];
            if (b.red = lr) and (b.green = lg) and (b.blue = lb) then
              Break; // pixel is transparent
            Inc(x);
          end;
          { test to see if we have a non-transparent area in the image }
          if x > x0 then
          begin
            { increase RgnData by AllocUnit rects if we exceeds maxRects }
            if RgnData^.rdh.nCount >= maxRects then
            begin
              Inc(maxRects, AllocUnit);
              ReallocMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
              pr := @RgnData^.Buffer;
              FillChar(pr^[maxRects - AllocUnit], AllocUnit * SizeOf(TRect), 0);
            end;
            { Add the rect (x0, y)-(x, y+1) as a new visible area in the region }
            pr := @RgnData^.Buffer; // Buffer is an array of rects
            with RgnData^.rdh do
            begin
              SetRect(pr[nCount], x0, y, x, y + 1);
              { adjust the bound rectangle of the region if we are "out-of-bounds" }
              if x0 < rcBound.Left then
                rcBound.Left := x0;
              if y < rcBound.Top then
                rcBound.Top := y;
              if x > rcBound.Right then
                rcBound.Right := x;
              if y + 1 > rcBound.Bottom then
                rcBound.Bottom := y + 1;
              Inc(nCount);
            end;
          end; // if x > x0
          { Need to create the region by muliple calls to ExtCreateRegion, 'cause }
          { it will fail on Windows 98 if the number of rectangles is too large   }
          if RgnData^.rdh.nCount = 2000 then
          begin
            h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), RgnData^);
            if Result > 0 then
            begin // Expand the current region
              CombineRgn(Result, Result, h, RGN_OR);
              DeleteObject(h);
            end
            else // First region, assign it to Result
              Result := h;
            RgnData^.rdh.nCount := 0;
            SetRect(RgnData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
          end;
          Inc(x);
        end; // scan every sample byte of the image
      end;
      { need to call ExCreateRegion one more time because we could have left    }
      { a RgnData with less than 2000 rects, so it wasn't yet created/combined  }
      if RgnData^.rdh.nCount > 0 then {LDB  0 Count causes exception and abort in Win98}
        h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects), RgnData^)
      else
        h := 0;
      if Result > 0 then
      begin
        CombineRgn(Result, Result, h, RGN_OR);
        DeleteObject(h);
      end
      else
        Result := h;
    finally
      FreeMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
    end;
  finally
    bmp.Free;
  end;
{$else}
  bmp := TBitmap.Create;
  try
    bmp.Assign(ABmp);
    bmp.PixelFormat := pf32bit;
    { alloc initial region data }
    maxRects := AllocUnit;
    GetMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects));
    FillChar(RgnData^, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), 0);
    try
      with RgnData^.rdh do
      begin
        dwSize := SizeOf(TRgnDataHeader);
        iType := RDH_RECTANGLES;
        nCount := 0;
        nRgnSize := 0;
        SetRect(rcBound, MAXLONG, MAXLONG, 0, 0);
      end;
      { scan each bitmap row - the orientation doesn't matter (Bottom-up or not) }
      ScanLinePtr := bmp.ScanLine[0];
      if bmp.Height > 1 then
        ScanLineInc := PtrSub(bmp.ScanLine[1], ScanLinePtr)
      else
        ScanLineInc := 0;
      for y := 0 to bmp.Height - 1 do
      begin
        x := 0;
        while x < bmp.Width do
        begin
          x0 := x;
          while x < bmp.Width do
          begin
            b := @PByteArray(ScanLinePtr)[x * SizeOf(TRGBQuad)];
            // BGR-RGB: Windows 32bpp BMPs are made of BGRa quads (not RGBa)
            if (b[2] = lr) and (b[1] = lg) and (b[0] = lb) then
              Break; // pixel is transparent
            Inc(x);
          end;
          { test to see if we have a non-transparent area in the image }
          if x > x0 then
          begin
            { increase RgnData by AllocUnit rects if we exceeds maxRects }
            if RgnData^.rdh.nCount >= maxRects then
            begin
              Inc(maxRects, AllocUnit);
              ReallocMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
              pr := @RgnData^.Buffer;
              FillChar(pr^[maxRects - AllocUnit], AllocUnit * SizeOf(TRect), 0);
            end;
            { Add the rect (x0, y)-(x, y+1) as a new visible area in the region }
            pr := @RgnData^.Buffer; // Buffer is an array of rects
            with RgnData^.rdh do
            begin
              SetRect(pr[nCount], x0, y, x, y + 1);
              { adjust the bound rectangle of the region if we are "out-of-bounds" }
              if x0 < rcBound.Left then
                rcBound.Left := x0;
              if y < rcBound.Top then
                rcBound.Top := y;
              if x > rcBound.Right then
                rcBound.Right := x;
              if y + 1 > rcBound.Bottom then
                rcBound.Bottom := y + 1;
              Inc(nCount);
            end;
          end; // if x > x0
          { Need to create the region by muliple calls to ExtCreateRegion, 'cause }
          { it will fail on Windows 98 if the number of rectangles is too large   }
          if RgnData^.rdh.nCount = 2000 then
          begin
            h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), RgnData^);
            if Result > 0 then
            begin // Expand the current region
              CombineRgn(Result, Result, h, RGN_OR);
              DeleteObject(h);
            end
            else // First region, assign it to Result
              Result := h;
            RgnData^.rdh.nCount := 0;
            SetRect(RgnData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
          end;
          Inc(x);
        end; // scan every sample byte of the image
        PtrInc(ScanLinePtr, ScanLineInc);
      end;
      { need to call ExCreateRegion one more time because we could have left    }
      { a RgnData with less than 2000 rects, so it wasn't yet created/combined  }
      if RgnData^.rdh.nCount > 0 then {LDB  0 Count causes exception and abort in Win98}
        h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects), RgnData^)
      else
        h := 0;
      if Result > 0 then
      begin
        CombineRgn(Result, Result, h, RGN_OR);
        DeleteObject(h);
      end
      else
        Result := h;
    finally
      FreeMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
    end;
  finally
    bmp.Free;
  end;
{$endif}
end;

{----------------DrawBackground}

procedure DrawBackground(ACanvas: TCanvas; ARect: TRect; XStart, YStart, XLast, YLast: Integer;
  Image: ThtImage; {Mask: TBitmap; AniGif: TGifImage;} BW, BH: Integer; BGColor: TColor);
{draw the background color and any tiled images on it}
{ARect, the cliprect, drawing outside this will not show but images may overhang
 XStart, YStart are first image position already calculated for the cliprect and parameters.
 XLast, YLast   Tiling stops here.
 BW, BH  bitmap dimensions.
}
var
  OldBrush: HBrush;
  OldPal: HPalette;
  DC: HDC;
  OldBack, OldFore: TColor;
//  Bitmap: TBitmap;
//  {$IFNDEF NoGDIPlus}
//  Graphics: TGpGraphics;
//  {$ENDIF NoGDIPlus}
begin
  DC := ACanvas.handle;
  if DC <> 0 then
  begin
    OldPal := SelectPalette(DC, ThePalette, False);
    RealizePalette(DC);
    ACanvas.Brush.Color :=BGColor;// ThemedColor(BGColor {$ifdef has_StyleElements},seClient in AStyleElements{$endif}) or PalRelative;
    OldBrush := SelectObject(DC, ACanvas.Brush.Handle);
    OldBack := SetBkColor(DC, clWhite);
    OldFore := SetTextColor(DC, clBlack);
    try
      ACanvas.FillRect(ARect); {background color}
      if Image <> nil then {tile the animated gif}
        Image.DrawTiled(ACanvas, XStart, YStart, XLast, YLast, BW, BH);
    finally
      SelectObject(DC, OldBrush);
      SelectPalette(DC, OldPal, False);
      RealizePalette(DC);
      SetBkColor(DC, OldBack);
      SetTextColor(DC, OldFore);
    end;
  end;
end;

//BG, 07.04.2011: same as CalcBackgroundLocationAndTiling plus DrawBackground ?
//TODO: let the better version win, but keep it in 2 separate methods for THtmlViewer.
procedure DoImageStuff(Canvas: TCanvas; IW, IH: Cardinal; BGImage: ThtImage; PRec: PtPositionRec;
  var TiledImage: TGpObject; var TiledMask: TBitmap; var NoMask: boolean);
{Set up for the background image. Allow for tiling, and transparency
  BGImage is the image
  PRec describes the location and tiling
  IW, IH, the width and height of the background
}
var
  OW, OH: Cardinal;
  X, XX, Y, X2, Y2: Integer;
  TheMask, NewBitmap, NewMask: TBitmap;
  TheGpObj: TBitmap;
{$IFNDEF NoGDIPlus}
  g: ThtGpGraphics;
{$ENDIF !NoGDIPlus}

//----------------------------------------------------------
//these methods fill the TiledImage and TiledMask.

  procedure Tile(Bitmap, Mask: TBitmap; W, H: Integer);
  begin
    repeat {tile BGImage in the various dc's}
      XX := X;
      repeat
        TBitmap(TiledImage).Canvas.Draw(XX, Y, Bitmap);
        if Assigned(TheMask) then
          TiledMask.Canvas.Draw(XX, Y, Mask)
        else if not NoMask then
          PatBlt(TiledMask.Canvas.Handle, XX, Y, Bitmap.Width, Bitmap.Height, Blackness);
        Inc(XX, Bitmap.Width);
      until XX >= X2;
      Inc(Y, Bitmap.Height);
    until Y >= Y2;
  end;

{$IFNDEF NoGDIPlus}
  procedure TileGpImage(Image: ThtGpImage; W, H: Cardinal);
  var
    ImW, ImH: Integer;
    graphics: ThtGpGraphics;
  begin
    ImW := Image.Width;
    ImH := Image.Height;
    try
      graphics := ThtGpGraphics.Create(ThtGpImage(TiledImage));
      try
        repeat {tile Image in the various dc's}
          XX := X;
          repeat
            graphics.DrawImage(Image, XX, Y, Image.Width, Image.Height);
            Inc(XX, ImW);
          until XX >= X2;
          Inc(Y, ImH);
        until Y >= Y2;
      except
      end;
      graphics.Free;
    except
    end;
  end;
{$ENDIF !NoGDIPlus}

begin
  if (IW = 0) or (IH = 0) then
    exit;

  TheGpObj := BGImage.Bitmap;
  TheMask  := BGImage.Mask;
  NoMask := not Assigned(TheMask) and PRec.X.RepeatD and PRec.Y.RepeatD;

  OW := BGImage.Width;
  OH := BGImage.Height;

{$IFNDEF NoGDIPlus}
  if (BGImage is ThtGdipImage) and not ((OW = 1) or (OH = 1)) then
  begin {TiledImage will be a TGpBitmap unless Image needs to be enlarged}
    if Assigned(TiledImage) then
      with ThtGpBitmap(TiledImage) do
        if (IW <> Width) or (IH <> Height) then
          FreeAndNil(TiledImage);
    if not Assigned(TiledImage) then
      TiledImage := ThtGpBitmap.Create(IW, IH);
    g := ThtGpGraphics.Create(ThtGpBitmap(TiledImage));
    g.Clear(0); {clear to transparent black}
    g.Free;
  end
  else
{$ENDIF !NoGDIPlus}
  begin {TiledImage will be a TBitmap}
    if not Assigned(TiledImage) then
      TiledImage := TBitmap.Create;
    TBitmap(TiledImage).Palette := CopyPalette(ThePalette);
    TBitmap(TiledImage).Height := IH;
    TBitmap(TiledImage).Width := IW;
    PatBlt(TBitmap(TiledImage).Canvas.Handle, 0, 0, IW, IH, Blackness);

    if not NoMask then
    begin
      if not Assigned(TiledMask) then
        TiledMask := TBitmap.Create;
      TiledMask.Monochrome := True;
      TiledMask.Height := IH;
      TiledMask.Width := IW;
      if not Assigned(TheMask) then
        PatBlt(TiledMask.Canvas.Handle, 0, 0, IW, IH, Whiteness);
    end;
  end;

{compute the location and tiling of BGImage in the background}
  with PRec.X do
  begin
    X := GetPositionInRange(PosType, Value, IW - OW);
    AdjustForTiling(RepeatD, 0, IW, OW, X, X2);
  end;
  with PRec.Y do
  begin
    Y := GetPositionInRange(PosType, Value, IH - OH);
    AdjustForTiling(RepeatD, 0, IH, OH, Y, Y2);
  end;

  if (OW = 1) or (OH = 1) then
  begin
    {in case a 1 pixel bitmap is being tiled.  EnlargeImage returns a TBitmap regardless of TheGpObj type.}
    NewBitmap := EnlargeImage(TheGpObj, X2 - X + 1, Y2 - Y + 1);
    try
      if Assigned(TheMask) then
        NewMask := EnlargeImage(TheMask, X2 - X + 1, Y2 - Y + 1)
      else
        NewMask := nil;
      try
        Tile(NewBitmap, NewMask, X2 - X + 1, Y2 - Y + 1);
      finally
        NewMask.Free;
      end;
    finally
      NewBitmap.Free;
    end;
  end
{$IFNDEF NoGDIPlus}
  else if (BGImage is ThtGdipImage) then
    TileGpImage(ThtGpImage(BGImage.GetGpObject), OW, OH)
{$ENDIF !NoGDIPlus}
  else
    Tile(TBitmap(TheGpObj), TheMask, OW, OH)
  ;
end;

{ TgpObject }

function EnlargeImage(Image: TGpObject; W, H: Integer): TBitmap;
{enlarge 1 pixel images for tiling.  Returns a TBitmap regardless of Image type}
var
  TmpBitmap: TBitmap;
{$IFNDEF NoGDIPlus}
  OwnsBitmap: Boolean;
{$ENDIF !NoGDIPlus}
begin
  Result := TBitmap.Create;
  {$IFNDEF NoGDIPlus}
  OwnsBitmap := False;
  {$ENDIF !NoGDIPlus}
  if Image is ThtImage then
  begin
    TmpBitmap := ThtImage(Image).GetBitmap;
{$IFNDEF NoGDIPlus}
    OwnsBitmap := Image is ThtGdipImage;
  end
  else if Image is ThtGpImage then
  begin
    TmpBitmap := ThtGpImage(Image).GetBitmap;
    OwnsBitmap := True;
{$ENDIF !NoGDIPlus}
  end
  else
    TmpBitmap := Image as TBitmap;
  Result.Assign(TmpBitmap);
  if TmpBitmap.Width = 1 then
    Result.Width := Min(100, W)
  else
    Result.Width := TmpBitmap.Width;
  if TmpBitmap.Height = 1 then
    Result.Height := Min(100, H)
  else
    Result.Height := TmpBitmap.Height;
  Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), TmpBitmap);
  {$IFNDEF NoGDIPlus}
  if OwnsBitmap then
    TmpBitmap.Free;
  {$ENDIF NoGDIPlus}
end;

function GetImageHeight(Image: TGpObject): Integer;
begin
  if Image is TBitmap then
    Result := TBitmap(Image).Height
  else {$IFNDEF NoGDIPlus}
  if Image is ThtGpImage then
    Result := ThtGpImage(Image).Height
  else {$ENDIF !NoGDIPlus}
  if Image is TGifImage then
    Result := TGifImage(Image).Height
{$IFNDEF NoMetafile}
  else if Image is ThtMetaFile then
    Result := ThtMetaFile(Image).Height
{$ENDIF}
  else
    raise(EInvalidArgument.Create('Not a TBitmap, TGifImage, TMetafile, or ThtGpImage'));
end;

function GetImageWidth(Image: TGpObject): Integer;
begin
  if Image is TBitmap then
    Result := TBitmap(Image).Width
  else {$IFNDEF NoGDIPlus}
  if Image is ThtGpImage then
    Result := ThtGpImage(Image).Width
  else {$ENDIF !NoGDIPlus}
  if Image is TGifImage then
    Result := TGifImage(Image).Width
{$IFNDEF NoMetafile}
  else if Image is ThtMetaFile then
    Result := ThtMetaFile(Image).Width
{$ENDIF}
  else
    raise(EInvalidArgument.Create('Not a TBitmap, TGifImage, TMetafile, or ThtGpImage'));
end;


{----------------PrintBitmap}

type
  ThtAllocRec = class(TObject)
  public
    Ptr: Pointer;
    ASize: Integer;
    AHandle: THandle;
  end;

procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
{Y relative to top of display here}

  function Allocate(Size: Integer): ThtAllocRec;
  begin
    Result := ThtAllocRec.Create;
    with Result do
    begin
      ASize := Size;
      if Size < $FF00 then
        GetMem(Ptr, Size)
      else
      begin
        AHandle := GlobalAlloc(HeapAllocFlags, Size);
        if AHandle = 0 then
          ABort;
        Ptr := GlobalLock(AHandle);
      end;
    end;
  end;

  procedure DeAllocate(AR: ThtAllocRec);
  begin
    with AR do
      if ASize < $FF00 then
        Freemem(Ptr, ASize)
      else
      begin
        GlobalUnlock(AHandle);
        GlobalFree(AHandle);
      end;
    AR.Free;
  end;

{$ifdef LCL}
{$else}
var
  OldPal: HPalette;
  DC: HDC;
  Info: PBitmapInfo;
  Image: ThtAllocRec;
  ImageSize: DWord;
  InfoSize: DWord;
{$endif}
begin
{$ifdef LCL}
  Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), Bitmap);
{$else}
  if (Bitmap = nil) or (Bitmap.Handle = 0) then
    Exit;
  DC := Canvas.Handle;
  try
    GetDIBSizes(Bitmap.Handle, InfoSize, ImageSize);
    GetMem(Info, InfoSize);
    try
      Image := Allocate(ImageSize);
      OldPal := SelectPalette(DC, ThePalette, False);
      try
        GetDIB(Bitmap.Handle, ThePalette, Info^, Image.Ptr^);
        RealizePalette(DC);
        with Info^.bmiHeader do
          StretchDIBits(DC, X, Y, W, H, 0, 0, biWidth, biHeight, Image.Ptr, Info^, DIB_RGB_COLORS, SRCCOPY);
      finally
        DeAllocate(Image);
        SelectPalette(DC, OldPal, False);
      end;
    finally
      FreeMem(Info, InfoSize);
    end;
  except
  end;
{$endif}
end;

{----------------PrintTransparentBitmap3}

procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: Integer;
  Bitmap, Mask: TBitmap; YI, HI: Integer);
{Y relative to top of display here}
{This routine prints transparently on complex background by printing through a clip region}
{X, Y are point where upper left corner will be printed.
 NewW, NewH are the Width and Height of the output (possibly stretched)
 Vertically only a portion of the Bitmap, Mask may be printed starting at
   Y=YI in the bitmap and a height of HI
}
var
  DC: HDC;
  hRgn, OldRgn: THandle;
  Rslt: Integer;
  XForm: TXForm;
  SizeV, SizeW: TSize;
  HF, VF: double;
  ABitmap, AMask: TBitmap;
  BitmapCopy: boolean;
  Origin: TPoint; //BG, 29.08.2009: window origin for correct mask translation
begin
{the following converts the black masked area in the image to white.  This may look
 better in WPTools which currently doesn't handle the masking}
  if (Bitmap.Handle = 0) or (HI <= 0) or (Bitmap.Width <= 0) then
    Exit;
  BitmapCopy := Bitmap.Height <> HI;
  try
    if BitmapCopy then
    begin
      ABitmap := TBitmap.Create;
      AMask := TBitmap.Create;
    end
    else
    begin
      ABitmap := Bitmap;
      AMask := Mask;
    end;
    try
      if BitmapCopy then
      begin
        Abitmap.Assign(Bitmap);
        ABitmap.Height := HI;
        BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, Bitmap.Canvas.Handle, 0, YI, SrcCopy);
        AMask.Assign(Mask);
        AMask.Height := HI;
        BitBlt(AMask.Canvas.Handle, 0, 0, AMask.Width, HI, Mask.Canvas.Handle, 0, YI, SrcCopy);
      end;

      SetBkColor(ABitmap.Canvas.Handle, clWhite);
      SetTextColor(ABitmap.Canvas.Handle, clBlack);
      BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, AMask.Canvas.Handle, 0, 0, SRCPAINT);

      DC := Canvas.Handle;
    {calculate a transform for the clip region as it may be a different size than
     the mask and needs to be positioned on the canvas.}
      GetViewportExtEx(DC, SizeV);
      GetWindowExtEx(DC, SizeW);
      GetWindowOrgEx(DC, Origin); //BG, 29.08.2009: get origin for correct mask translation

      HF := (SizeV.cx / SizeW.cx); {Horizontal adjustment factor}
      VF := (SizeV.cy / SizeW.cy); {Vertical adjustment factor}

      XForm.eM11 := HF * (NewW / Bitmap.Width);
      XForm.eM12 := 0;
      XForm.eM21 := 0;
      XForm.eM22 := VF * (NewH / HI);
      XForm.edx := HF * (X - Origin.X); //BG, 29.08.2009: subtract origin
      XForm.edy := VF * Y;

    {Find the region for the white area of the Mask}
      hRgn := BitmapToRegion(AMask, @XForm, $FFFFFF);
      if hRgn <> 0 then {else nothing to output--this would be unusual}
      begin
        OldRgn := CreateRectRgn(0, 0, 1, 1); {a valid region is needed for the next call}
        Rslt := GetClipRgn(DC, OldRgn); {save the Old clip region}
        try
          if Rslt = 1 then
            CombineRgn(hRgn, hRgn, OldRgn, RGN_AND);
          SelectClipRgn(DC, hRgn);
          PrintBitmap(Canvas, X, Y, NewW, NewH, ABitmap);
        finally
          if Rslt = 1 then
            SelectClipRgn(DC, OldRgn)
          else
            SelectClipRgn(DC, 0);
          DeleteObject(hRgn);
          DeleteObject(OldRgn);
        end;
      end;
    finally
      if BitmapCopy then
      begin
        ABitmap.Free;
        AMask.Free;
      end;
    end;
  except
  end;
end;

{$IFNDEF NoGDIPlus}

procedure DrawGpImage(Handle: THandle; Image: ThtGpImage; DestX, DestY: Integer);
{Draws the entire image as specified at the point specified}
var
  g: ThtGpGraphics;
begin
  g := ThtGpGraphics.Create(Handle);
  try
    g.DrawImage(Image, DestX, DestY, Image.Width, Image.Height);
  except
  end;
  g.Free;
end;

procedure DrawGpImage(Handle: THandle; Image: ThtGpImage; DestX, DestY,
  SrcX, SrcY, SrcW, SrcH: Integer);
{Draw a portion of the image at DestX, DestY.  No stretching}
var
  g: ThtGpGraphics;
begin
  g := ThtGpGraphics.Create(Handle);
  try
    g.DrawImage(Image, DestX, DestY, SrcX, SrcY, SrcW, SrcH);
  except
  end;
  g.Free;
end;

procedure StretchDrawGpImage(Handle: THandle; Image: ThtGpImage; DestX, DestY,
  DestW, DestH: Integer);
{Draws the entire image in the rectangle specified}
var
  g: ThtGpGraphics;
begin
  g := ThtGpGraphics.Create(Handle);
  try
    g.DrawImage(Image, DestX, DestY, DestW, DestH);
  except
  end;
  g.Free;
end;

procedure StretchPrintGpImageDirect(Handle: THandle; Image: ThtGpImage;
  DestX, DestY, DestW, DestH: Integer;
  ScaleX, ScaleY: single);
{Prints the entire image at the point specified with the height and width specified}
var
  g: ThtGpGraphics;
begin
  g := ThtGpGraphics.Create(Handle);
  try
    g.ScaleTransform(ScaleX, ScaleY);
    g.DrawImage(Image, DestX, DestY, DestW, DestH);
  except
  end;
  g.Free;
end;

procedure StretchPrintGpImageOnColor(Canvas: TCanvas; Image: ThtGpImage;
  DestX, DestY, DestW, DestH: Integer; Color: TColor = clWhite);
var
  g: ThtGpGraphics;
  bg: TBitmap;
begin {Draw image on white background first, then print}
  bg := TBitmap.Create;
  bg.Width := ThtGpImage(Image).Width;
  bg.Height := ThtGpImage(Image).Height;
  bg.Canvas.Brush.Color := Color;
  bg.Canvas.FillRect(Rect(0, 0, bg.Width, bg.Height));
  g := ThtGpGraphics.Create(bg.Canvas.Handle);
  g.DrawImage(ThtGpImage(Image), 0, 0, bg.Width, bg.Height);
  g.Free;
  Canvas.StretchDraw(Rect(DestX, DestY, DestX + DestW, DestY + DestH), bg);
  bg.Free;
end;

procedure PrintGpImageDirect(Handle: THandle; Image: ThtGpImage; DestX, DestY: Integer;
  ScaleX, ScaleY: single);
{Prints the entire image as specified at the point specified}
var
  g: ThtGpGraphics;
begin
  g := ThtGpGraphics.Create(Handle);
  try
    g.ScaleTransform(ScaleX, ScaleY);
    g.DrawImage(Image, DestX, DestY, Image.Width, Image.Height);
  except
  end;
  g.Free;
end;

{$ENDIF NoGDIPlus}

{ TDib }

constructor TDib.CreateDIB(DC: HDC; Bitmap: TBitmap);
{given a TBitmap, construct a device independent bitmap}
var
  ImgSize: DWord;
begin
  InitializeBitmapInfoHeader(Bitmap.Handle);
  ImgSize := Info^.biSizeImage;
  Allocate(ImgSize);
  try
    GetDIBX(DC, Bitmap.Handle, Bitmap.Palette);
  except
    DeAllocate;
    raise;
  end;
end;

destructor TDib.Destroy;
begin
  DeAllocate;
  inherited Destroy;
end;

procedure TDib.Allocate(Size: Integer);
begin
  ImageSize := Size;
  if Size < $FF00 then
    GetMem(Image, Size)
  else
  begin
    FHandle := GlobalAlloc(HeapAllocFlags, Size);
    if FHandle = 0 then
      ABort;
    Image := GlobalLock(FHandle);
  end;
end;

procedure TDib.DeAllocate;
begin
  if ImageSize > 0 then
  begin
    if ImageSize < $FF00 then
      Freemem(Image, ImageSize)
    else
    begin
      GlobalUnlock(FHandle);
      GlobalFree(FHandle);
    end;
    ImageSize := 0;
  end;
  if InfoSize > 0 then
  begin
    FreeMem(Info, InfoSize);
    InfoSize := 0;
  end;
end;

procedure TDib.InitializeBitmapInfoHeader(Bitmap: HBITMAP);
var
  BM: {$ifdef LCL} LclType.Bitmap {$else} Windows.TBitmap {$endif};
  BitCount: Integer;

  function WidthBytes(I: Integer): Integer;
  begin
    Result := ((I + 31) div 32) * 4;
  end;

begin
  GetObject(Bitmap, SizeOf(BM), @BM);
  BitCount := BM.bmBitsPixel * BM.bmPlanes;
  if BitCount > 8 then
    InfoSize := SizeOf(TBitmapInfoHeader)
  else
    InfoSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl BitCount);
  GetMem(Info, InfoSize);

  with Info^ do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := BM.bmWidth;
    biHeight := BM.bmHeight;
    biBitCount := BM.bmBitsPixel * BM.bmPlanes;
    biPlanes := 1;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
    biCompression := BI_RGB;
    if biBitCount in [16, 32] then
      biBitCount := 24;
    biSizeImage := WidthBytes(biWidth * biBitCount) * biHeight;
  end;
end;

procedure TDib.GetDIBX(DC: HDC; Bitmap: HBITMAP; Palette: HPALETTE);
var
  OldPal: HPALETTE;
  Rslt: Integer;
  bmInfo: PBitmapInfo;
begin
  OldPal := 0;
  if Palette <> 0 then
  begin
    OldPal := SelectPalette(DC, Palette, False);
    RealizePalette(DC);
  end;
  bmInfo := PBitmapInfo(Info);
  Rslt := GetDIBits(DC, Bitmap, 0, Info^.biHeight, Image, bmInfo^, DIB_RGB_COLORS);
  if OldPal <> 0 then
    SelectPalette(DC, OldPal, False);
  if Rslt = 0 then
  begin
    OutofMemoryError;
  end;
end;

procedure TDib.DrawDIB(DC: HDC; X, Y, W, H: Integer; ROP: DWord);
var
  bmInfo: PBitmapInfo;
begin
  bmInfo := PBitmapInfo(Info);
  with Info^ do
    StretchDIBits(DC, X, Y, W, H, 0, 0, biWidth, biHeight, Image,
      bmInfo^, DIB_RGB_COLORS, ROP);
end;

function TDib.CreateDIBmp: hBitmap;
var
  bmInfo: PBitmapInfo;
  DC: HDC;
  OldPal: HPalette;
begin
  bmInfo := PBitmapInfo(Info);
  DC := GetDC(0);
  OldPal := SelectPalette(DC, ThePalette, False);
  RealizePalette(DC);
  try
    Result := CreateDIBitmap(DC, bmInfo^.bmiHeader, CBM_INIT, Image,
      bmInfo^, DIB_RGB_COLORS);
  finally
    SelectPalette(DC, OldPal, False);
    ReleaseDC(0, DC);
  end;
end;

{$IFNDEF NoMetafile}

{ ThtMetafile }

procedure ThtMetaFile.Construct;
var
  Tmp: TBitmap;
  pe: TPaletteEntry;
  Color: TColor;
  FBitmap: TBitmap;
begin
  if not Assigned(FhtBitmap) then
  begin
    FBitmap := TBitmap.Create;
    try
      FBitmap.Width := Width;
      FBitmap.Height := Height;
      PatBlt(FBitmap.Canvas.Handle, 0, 0, Width, Height, Blackness);
      FBitmap.Canvas.Draw(0, 0, Self);

      Tmp := TBitmap.Create;
      try
        Tmp.Width := Width;
        Tmp.Height := Height;
        Tmp.PixelFormat := pf8Bit;
      {pick an odd color from the palette to represent the background color,
       one not likely in the metafile}
        GetPaletteEntries(Tmp.Palette, 115, 1, pe);
        Color := pe.peBlue shl 16 or pe.peGreen shl 8 or pe.peRed;
        Tmp.Canvas.Brush.Color := Color; //ThemedColor( Color );
        Tmp.Canvas.FillRect(Rect(0, 0, Width, Height));
        Tmp.Canvas.Draw(0, 0, Self);

        FhtBitmap := ThtBitmapImage.Create(FBitmap, GetImageMask(Tmp, False, Color), LLCorner);
      finally
        Tmp.Free;
      end;
    except
      FreeAndNil(FBitmap);
    end;
  end;
end;

function ThtMetaFile.GetBitmap: TBitmap;
begin
  Construct;
  Result := FhtBitmap.Bitmap;
end;

function ThtMetaFile.GetMask: TBitmap;
begin
  Construct;
  Result := FhtBitmap.Mask;
end;

function ThtMetaFile.GetWhiteBGBitmap: TBitmap;
begin
  if not Assigned(FWhiteBGBitmap) then
  begin
    FWhiteBGBitmap := TBitmap.Create;
    try
      FWhiteBGBitmap.Width := Width;
      FWhiteBGBitmap.Height := Height;
      PatBlt(FWhiteBGBitmap.Canvas.Handle, 0, 0, Width, Height, Whiteness);
      FWhiteBGBitmap.Canvas.Draw(0, 0, Self);
    except
      FreeAndNil(FWhiteBGBitmap);
    end;
  end;
  Result := FWhiteBGBitmap;
end;

destructor ThtMetaFile.Destroy;
begin
  FreeAndNil(FhtBitmap);
  FreeAndNil(FWhiteBGBitmap);
  inherited;
end;
{$ENDIF}

{ ThtImage }

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtImage.Create(Tr: TTransparency);
begin
  inherited Create;
  Transp := Tr;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
procedure ThtImage.DrawTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer);
var
  X, Y: Integer;
begin
  Y := YStart;
  while Y < YEnd do
  begin
    X := XStart;
    while X < XEnd do
    begin
      Draw(Canvas, X, Y, W, H);
      Inc(X, W);
    end;
    Inc(Y, H);
  end;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
procedure ThtImage.Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor);
begin
  if Mask = nil then
    PrintBitmap(Canvas, X, Y, W, H, Bitmap)
  else
    PrintTransparentBitmap3(Canvas, X, Y, W, H, Bitmap, Mask, 0, Bitmap.Height);
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
procedure ThtImage.PrintTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer; BgColor: TColor);
var
  X, Y: Integer;
begin
  Y := YStart;
  while Y < YEnd do
  begin
    X := XStart;
    while X < XEnd do
    begin
      Print(Canvas, X, Y, W, H, BgColor);
      Inc(X, W);
    end;
    Inc(Y, H);
  end;
end;

{ ThtBitmapImage }

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtBitmapImage.Create(AImage, AMask: TBitmap; Tr: TTransparency; AOwnsBitmap, AOwnsMask: Boolean);
begin
  if AImage = nil then
    raise EInvalidImage.Create('ThtBitmapImage requires an image');
  inherited Create(Tr);
  Bitmap := AImage;
  OwnsBitmap := AOwnsBitmap;
  Mask := AMask;
  OwnsMask := AOwnsMask;
end;

//-- BG ---------------------------------------------------------- 10.01.2015 --
constructor ThtBitmapImage.Create(AImage: TBitmap; Tr: TTransparency; Color: TColor; AOwnsBitmap: Boolean);
begin
  if AImage = nil then
    raise EInvalidImage.Create('ThtBitmapImage requires an image');
  inherited Create(Tr);
  Bitmap := AImage;
  OwnsBitmap := AOwnsBitmap;
  case Transp of
    TrGif:    Mask := GetImageMask(Bitmap, True, Color);
    LLCorner: Mask := GetImageMask(Bitmap, False, Color);
  end;
end;

destructor ThtBitmapImage.Destroy;
begin
  if OwnsBitmap then
    Bitmap.Free;
  if OwnsMask then
    Mask.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
procedure ThtBitmapImage.Draw(Canvas: TCanvas; X, Y, W, H: Integer);
begin
  if Bitmap = nil then
    exit;
  if (Mask = nil) or (Transp = NotTransp) then
  begin
{$IFDEF HalfToneStretching}
    SetStretchBltMode(Canvas.Handle, HALFTONE);
{$ELSE}
    SetStretchBltMode(Canvas.Handle, COLORONCOLOR);
{$ENDIF}
    SetBrushOrgEx(Canvas.Handle, 0, 0, nil);
    StretchBlt(Canvas.Handle, X, Y, W, H, Bitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
  end
  else
    FinishTransparentBitmap(Canvas.Handle, Bitmap, Mask, X, Y, W, H);
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtBitmapImage.GetBitmap: TBitmap;
begin
  Result := Bitmap;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
function ThtBitmapImage.GetGpObject: TGpObject;
begin
  Result := Bitmap;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtBitmapImage.GetImageHeight: Integer;
begin
  if Bitmap <> nil then
    Result := Bitmap.Height
  else
    Result := 0;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtBitmapImage.GetImageWidth: Integer;
begin
  if Bitmap <> nil then
    Result := Bitmap.Width
  else
    Result := 0;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtBitmapImage.GetMask: TBitmap;
begin
  Result := Mask;
end;

{ ThtGifImage }

//-- BG ---------------------------------------------------------- 11.04.2011 --
function ThtGifImage.Clone: ThtGifImage;
begin
  Result := ThtGifImage.Create(TGIFImage.CreateCopy(Gif));
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtGifImage.Create(AImage: TGifImage);
begin
  inherited Create(trGif);
  FGif := AImage;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
destructor ThtGifImage.Destroy;
begin
  Gif.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
procedure ThtGifImage.Draw(Canvas: TCanvas; X, Y, W, H: Integer);
begin
  Gif.ShowIt := True; // animate it
  Gif.Draw(Canvas, X, Y, W, H);
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGifImage.GetBitmap: TBitmap;
begin
  Result := Gif.MaskedBitmap; // same as Gif.Bitmap
end;

function ThtGifImage.GetGpObject: TGpObject;
begin
  Result := Gif;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGifImage.GetImageHeight: Integer;
begin
  Result := Gif.Height;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGifImage.GetImageWidth: Integer;
begin
  Result := Gif.Width;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGifImage.GetMask: TBitmap;
begin
  Result := Gif.Mask;
end;

{$IFNDEF NoGDIPlus}

{ ThtGdipImage }

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtGdipImage.Create(AImage: ThtGpImage);
begin
  inherited Create(TrPng);
  Gpi := AImage;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
destructor ThtGdipImage.Destroy;
begin
  Gpi.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
procedure ThtGdipImage.Draw(Canvas: TCanvas; X, Y, W, H: Integer);
var
  Graphics: ThtGpGraphics;
begin
  Graphics := ThtGpGraphics.Create(Canvas.Handle);
  try
    Graphics.DrawImage(Gpi, X, Y, W, H);
  finally
    Graphics.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
procedure ThtGdipImage.DrawTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer);
var
  X, Y: Integer;
  Graphics: ThtGpGraphics;
begin
  Y := YStart;
  Graphics := ThtGpGraphics.Create(Canvas.Handle);
  try
    while Y < YEnd do
    begin
      X := XStart;
      while X < XEnd do
      begin
        Graphics.DrawImage(Gpi, X, Y, W, H);
        Inc(X, W);
      end;
      Inc(Y, H);
    end;
  finally
    Graphics.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGdipImage.GetBitmap: TBitmap;
begin
  Result := Gpi.GetBitmap;
end;

function ThtGdipImage.GetGpObject: TGpObject;
begin
  Result := Gpi;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGdipImage.GetImageHeight: Integer;
begin
  Result := Gpi.Height;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGdipImage.GetImageWidth: Integer;
begin
  Result := Gpi.Width;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGdipImage.GetMask: TBitmap;
begin
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
procedure ThtGdipImage.Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor);
begin
  StretchPrintGpImageOnColor(Canvas, Gpi, X, Y, W, H, BGColor);
end;

{$ENDIF NoGDIPlus}

{$IFNDEF NoMetafile}

{ ThtMetafileImage }

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtMetafileImage.Create(AImage: ThtMetaFile);
begin
  inherited Create(NotTransp);
  MetaFile := AImage;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
destructor ThtMetafileImage.Destroy;
begin
  MetaFile.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
procedure ThtMetafileImage.Draw(Canvas: TCanvas; X, Y, W, H: Integer);
begin
  Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), MetaFile);
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtMetafileImage.GetBitmap: TBitmap;
begin
  Result := MetaFile.GetBitmap;
end;

function ThtMetafileImage.GetGpObject: TGpObject;
begin
  Result := MetaFile;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtMetafileImage.GetImageHeight: Integer;
begin
  Result := MetaFile.Height;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtMetafileImage.GetImageWidth: Integer;
begin
  Result := MetaFile.Width;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtMetafileImage.GetMask: TBitmap;
begin
  Result := MetaFile.Mask;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
procedure ThtMetafileImage.Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor);
begin
  Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), MetaFile);
end;

{$ENDIF !NoMetafile}

{ ThtImageCache }

//------------------------------------------------------------------------------
constructor ThtImageCache.Create;
begin
  inherited Create;
  {$IFNDEF NoGDIPlus}
  CheckInitGDIPlus;
  {$ENDIF NoGDIPlus}
end;

//------------------------------------------------------------------------------
destructor ThtImageCache.Destroy;
begin
  {$IFNDEF NoGDIPlus}
  // must exit GDI after freeing images in clear!
  Clear;
  CheckExitGDIPlus;
  {$ENDIF NoGDIPlus}
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function ThtImageCache.AddObject(const S: ThtString; AObject: ThtImage): Integer;
begin
  Result := inherited AddObject(S, AObject);
end;

//-- BG ---------------------------------------------------------- 11.04.2011 --
function ThtImageCache.GetImage(I: Integer): ThtImage;
begin
  Result := ThtImage(inherited GetCachable(I));
end;

//-- BG ---------------------------------------------------------- 19.08.2011 --
procedure ThtImageCache.InsertImage(Index: Integer; const S: ThtString; AObject: ThtImage);
begin
  InsertItem(Index, S, AObject);
end;

{ ThtBitmapToInsert }

constructor ThtBitmapToInsert.Create(AImage: TBitmap; ATransp: TTransparency; AColor: TColor; AOwnsBitmap: Boolean);
begin
  Bitmap     := AImage;
  Color      := AColor;
  Transp     := ATransp;
  OwnsBitmap := AOwnsBitmap;
end;

initialization
  DefBitMap := TBitmap.Create;
  ErrorBitMap := TBitmap.Create;
  ErrorBitMapMask := TBitmap.Create;
{$ifdef LCL}
  {$I htmlun2.lrs}
  DefBitMap.LoadFromLazarusResource('ErrBitmap');
  ErrorBitMap.LoadFromLazarusResource('DefaultBitmap');
  ErrorBitMapMask.LoadFromLazarusResource('ErrBitmapMask');
  Screen.Cursors[HandCursor] := LoadCursorFromLazarusResource('Hand_Cursor');
  Screen.Cursors[UpDownCursor] := LoadCursorFromLazarusResource('UPDOWNCURSOR');
  Screen.Cursors[UpOnlyCursor] := LoadCursorFromLazarusResource('UPONLYCURSOR');
  Screen.Cursors[DownOnlyCursor] := LoadCursorFromLazarusResource('DOWNONLYCURSOR');
{$else}
  {$R Html32.res}
  DefBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(DefaultBitmap));
  ErrorBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmap));
  ErrorBitMapMask.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmapMask));
  Screen.Cursors[HandCursor] := LoadCursor(HInstance, MakeIntResource(Hand_Cursor));
  Screen.Cursors[UpDownCursor] := LoadCursor(HInstance, 'UPDOWNCURSOR');
  Screen.Cursors[UpOnlyCursor] := LoadCursor(HInstance, 'UPONLYCURSOR');
  Screen.Cursors[DownOnlyCursor] := LoadCursor(HInstance, 'DOWNONLYCURSOR');
{$endif}

  DefImage := ThtBitmapImage.Create(DefBitmap, nil, NotTransp);
  ErrorImage := ThtBitmapImage.Create(ErrorBitmap, ErrorBitmapMask, LLCorner);
finalization
  DefImage.Free;
  ErrorImage.Free;

//  DefBitMap.Free;
//  ErrorBitMap.Free;
//  ErrorBitMapMask.Free;
end.
