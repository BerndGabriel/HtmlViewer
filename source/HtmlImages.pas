{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2022 by HtmlViewer Team

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
  SysUtils, Classes,
{$ifdef LCL}
  LclIntf, IntfGraphics, FpImage, LclType, LResources, LMessages, HtmlMisc,
{$else}
  Windows, Jpeg,
{$endif}
  Contnrs, Graphics, Forms, Controls,
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
  //Messages,
  //Variants,
  Types,
  Math,
{$ifndef NoGDIPlus}
  GDIPL2A,
{$endif}
{$ifdef METAFILEMISSING}
  MetaFilePrinter,
{$endif}
  URLSubs,
  HtmlCaches,
  HtmlGlobals,
  HTMLGif2,
  StyleTypes,
{$ifdef Compiler20_Plus}
  PngImage,
{$endif}
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

  ThtImageTransparency = (itrNone, itrIntrinsic, itrLLCorner);

  //BG, 09.04.2011
  ThtImage = class(ThtCachable)
  protected
    function GetBitmap: TBitmap; virtual; abstract;
    function GetGraphic: TGraphic; virtual;
    function GetImageHeight: Integer; virtual; abstract;
    function GetImageWidth: Integer; virtual; abstract;
    function GetMask: TBitmap; virtual; abstract;
    function GetAnimate: Boolean; virtual;
    procedure SetAnimate(const Value: Boolean); virtual;

    function EnlargeBitmap(Bitmap: TBitmap; W, H: Integer): TBitmap; virtual;
    function EnlargeImage(W, H: Integer): ThtImage; virtual;
  public
    Transp: ThtImageTransparency; {identifies what the mask is for}
    constructor Create(Tr: ThtImageTransparency); overload;
    function Clone: ThtImage; virtual;

    procedure TileImage(PRec: PtPositionRec; DstW, DstH: Integer; var TiledImage: ThtImage; var NoMask: Boolean); virtual;

    procedure Draw(Canvas: TCanvas; X, Y, W, H: Integer); virtual;
    procedure Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor); virtual;
    procedure DrawTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer); virtual;
    procedure PrintTiled(Canvas: TCanvas; XStart, YStart, XEnd, YEnd, W, H: Integer; BgColor: TColor); virtual;
    // used to draw/print background images.
    // BG, 06.09.2015: Currently only ThtBitmapImage and ThtGdipImage implement these methods.
    procedure DrawUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean); virtual;
    procedure PrintUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean); virtual;

    function IsAnimated: Boolean; virtual;
    property Bitmap: TBitmap read GetBitmap;
    property Mask: TBitmap read GetMask;
    property Graphic: TGraphic read GetGraphic;
    property Height: Integer read GetImageHeight;
    property Width: Integer read GetImageWidth;
    property Animate: Boolean read GetAnimate write SetAnimate;
  end;

{$ifdef UseGenerics}
  ThtImageList = class(TObjectList<ThtImage>);
{$else}
  ThtImageList = class(TObjectList)
  private
    function Get(Index: Integer): ThtImage; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: ThtImage read Get; default;
  end;
{$endif}

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
    Transp: ThtImageTransparency;
    OwnsBitmap: Boolean;
    constructor Create(AImage: TBitmap; ATransp: ThtImageTransparency; AColor: TColor; AOwnsBitmap: Boolean = True); overload;
  end;

  //BG, 09.04.2011
  ThtBitmapImage = class(ThtImage)
  private
    Bitmap, Mask: TBitmap;
    OwnsBitmap, OwnsMask: Boolean;
  protected
    function GetBitmap: TBitmap; override;
    function GetImageHeight: Integer; override;
    function GetImageWidth: Integer; override;
    function GetMask: TBitmap; override;
  public
    constructor Create(AImage: ThtBitmap; Tr: ThtImageTransparency; AOwnsBitmap: Boolean = True); overload;
    constructor Create(AImage, AMask: TBitmap; Tr: ThtImageTransparency; AOwnsBitmap: Boolean = True; AOwnsMask: Boolean = True); overload;
    constructor Create(AImage: TBitmap; Tr: ThtImageTransparency; Color: TColor; AOwnsBitmap: Boolean = True); overload;
    destructor Destroy; override;
    procedure Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor); override;
    procedure DrawUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean); override;
    procedure PrintUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean); override;
  end;

  //BG, 09.04.2011
  ThtGifImage = class(ThtImage)
  private
    FGif: TGifImage;
  protected
    function GetBitmap: TBitmap; override;
    function GetImageHeight: Integer; override;
    function GetImageWidth: Integer; override;
    function GetMask: TBitmap; override;
    function GetAnimate: Boolean; override;
    procedure SetAnimate(const Value: Boolean); override;
  public
    constructor Create(AImage: TGifImage); overload;
    destructor Destroy; override;
    function Clone: ThtImage; override;
    function IsAnimated: Boolean; override;
    procedure Draw(Canvas: TCanvas; X, Y, W, H: Integer); override;
    property Gif: TGifImage read FGif;
  end;

{$ifndef NoGDIPlus}
  //BG, 09.04.2011
  //BG, 18.05.2014: GDIPlus can handle image kinds: PNG, TIFF, JPEG, ICO, EMF, WMF, EXIF, BMP, GIF (not animated)
  ThtGdipImage = class(ThtImage)
  private
    Gpi: ThtGpImage;
  protected
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

    procedure DrawUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean); override;
    procedure PrintUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean); override;
  end;
{$endif !NoGDIPlus}

//------------------------------------------------------------------------------
// ThtGraphicImage: support for all TGraphics without own TBitmap, e.g. meta files
//------------------------------------------------------------------------------

  //BG, 09.04.2011
  ThtGraphicImage = class(ThtImage)
  private
    FGraphic: TGraphic;
    FImage: ThtBitmapImage;
    procedure Construct;
  protected
    function GetBitmap: TBitmap; override;
    function GetGraphic: TGraphic; override;
    function GetImageHeight: Integer; override;
    function GetImageWidth: Integer; override;
    function GetMask: TBitmap; override;
  public
    constructor Create(AGraphic: TGraphic); overload;
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------
// ThtImageLoader is the base class for image loaders, that the global function
// LoadImageFromStream() uses to load images of various kinds.
//
// HtmlViewer components use LoadImageFromStream() to load images.
//
// You may derive your own image loader from this class to support further
// image file formats. Your loader must load the images into a derivate of
// the ThtImage declared above.
//------------------------------------------------------------------------------

type
  //BG, 24.08.2015:
  ThtImageLoader = class
  private
    FSupportedKinds: TStringList;
    FSupportedExts: TStringList;
  protected
    // AddSupportedKinds adds one kind with a comma separated list of file extensions
    // e.g.: AddSupportedKinds('Bitmaps', '*.bmp,*.dib')
    procedure AddSupportedKinds(const KindName, Extensions: string); overload;

    // AddSupportedKinds() adds all supported kinds using the above AddSupportedKinds().
    procedure AddSupportedKinds; overload; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LoadGraphicFromStream(Stream: TStream; Transparent: ThtImageTransparency): TGraphic; virtual;
    function LoadImageFromStream(Stream: TStream; Transparent: ThtImageTransparency): ThtImage; virtual;
    // Filter returns a file dialog filter compliant string of supported image types
    function GetFilter(const AllKinds: string): string;
  end;

// SetImageLoader() sets a global image loader.
// The loader will be freed during finalization or by setting another loader.
procedure SetImageLoader(AImageLoader: ThtImageLoader);

// GetImageLoader() gets the global image loader. If no image loader has been
// set yet, a ThtImageLoader is created.
function GetImageLoader(): ThtImageLoader;

// LoadImageFromStream() tries to load an image from stream using the currently
// set global image loader.
//
// The HtmlViewer components use LoadImageFromStream() to load images from streams.
function LoadImageFromStream(Stream: TStream; Transparent: ThtImageTransparency): ThtImage;

//------------------------------------------------------------------------------
// image methods
//------------------------------------------------------------------------------

function EnlargeImage(Image: TBitmap; W, H: Integer): TBitmap;

procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: Integer; Bitmap, Mask: TBitmap; YI, HI: Integer);

function GetClipRegion(Canvas: TCanvas): Integer;

procedure DrawBackground(ACanvas: TCanvas; ARect: TRect; XStart, YStart, XLast, YLast: Integer;
  Image: ThtImage; BW, BH: Integer; BGColor: TColor);
{draw the background color and any tiled images on it}
{ARect, the cliprect, drawing outside this will not show but images may overhang
 XStart, YStart are first image position already calculated for the cliprect and parameters.
 XLast, YLast   Tiling stops here.
 BW, BH  bitmap dimensions.
}

//procedure DoImageStuff(IW, IH: Integer; BGImage: ThtImage;
//  PRec: PtPositionRec; var TiledImage: ThtImage; var NoMask: boolean);
//{Set up for the background image. Allow for tiling, and transparency
//  BGImage is the image
//  PRec describes the location and tiling
//  IW, IH, the width and height of the background
//}


{$IFNDEF NoGDIPlus}
procedure DrawGpImage(Handle: HDC; Image: ThtGpImage; DestX, DestY: Integer); overload;
procedure DrawGpImage(Handle: HDC; Image: ThtGpImage; DestX, DestY, SrcX, SrcY, SrcW, SrcH: Integer); overload;
procedure StretchDrawGpImage(Handle: HDC; Image: ThtGpImage; DestX, DestY, DestW, DestH: Integer);
procedure PrintGpImageDirect(Handle: HDC; Image: ThtGpImage; DestX, DestY: Integer; ScaleX: single = 1.0; ScaleY: single = 1.0);
procedure StretchPrintGpImageDirect(Handle: HDC; Image: ThtGpImage; DestX, DestY, DestW, DestH: Integer; ScaleX, ScaleY: Single);
procedure StretchPrintGpImageOnColor(Canvas: TCanvas; Image: ThtGpImage; DestX, DestY, DestW, DestH: Integer; Color: TColor = clWhite);
{$ENDIF NoGDIPlus}

//------------------------------------------------------------------------------

var
  DefBitMap, ErrorBitMap, ErrorBitmapMask: TBitMap;
  DefImage: ThtBitmapImage;
  ErrorImage: ThtBitmapImage;

implementation

var
  ImageLoader: ThtImageLoader;

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
    FHandle: HGLOBAL;
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

{ Try to recognize image type by reading the image header at Stream position.
  Return image type or itNone if not recognized.

  Do not reset stream position when done; this is left to the caller
  which knows better how to interpret the results. }
type
  ThtImageFormat = (itNone, itBmp, itIco, itCur, itGif, itPng, itJpg, itTiff, itMeta);

function KindOfImage(const Stream: TStream): ThtImageFormat;
type
  THeaderRec = record
    case Integer of
      0: (c: Cardinal);
      1: (w: Word);
      2: (EMF: record
          Type_: Cardinal;
          Size: Cardinal;
          Bounds: array[0..15] of Byte;
          Frame: array[0..15] of Byte;
          RecordSignature: Cardinal;
        end);
      3: (WMF: record
          Type_: Word;
          HeaderSize: Word;
          Version: Word;
        end);
      4: (TIFF: record
          ID: Word;
          Version: Word;
        end);
    end;
var
  Header: THeaderRec;
  n: Integer;
begin
  Assert(Assigned(Stream));

  n := Stream.Read(Header, SizeOf(Header));
  if n >= SizeOf(Header.c) then
  begin

    case Header.c of
      $00000001:
        { Windows Enhanced Metafile Specs & Info:
          * https://msdn.microsoft.com/en-us/library/cc230635.aspx }
        if (n >= SizeOf (Header.Emf)) and (Header.EMF.RecordSignature = $464D4520) then
        begin
          Result := itMeta;
          Exit;
        end;

      $00010000: begin Result := itIco ; Exit; end;
      $00020000: begin Result := itCur ; Exit; end;
      $002A4949: begin Result := itTiff; Exit; end; // .TIFF little endian (II - Intel).
      $2A004D4D: begin Result := itTiff; Exit; end; // .TIFF big endian (MM - Motorola).
      $38464947: begin Result := itGif ; Exit; end;
      $474E5089: begin Result := itPng ; Exit; end;

      { Windows Metafile with WmfPlaceableFileHeader
        * https://msdn.microsoft.com/en-us/library/windows/desktop/ms534075%28v=vs.85%29.aspx }
      $9AC6CDD7: begin Result := itMeta; Exit; end;
    end;

    case Header.w of
      $0001, $0002:
        { Windows Metafile Specs & Info:
          * https://msdn.microsoft.com/en-us/library/cc250418.aspx
          * https://www.symantec.com/avcenter/reference/inside.the.windows.meta.file.format.pdf

          NOTICE: with NoGDIPlus defined Vcl.Graphics.TMetaFile should handle this kind of stream
            but since Delphi 4 it does not recognize WMF metafiles without a WmfPlaceableFileHeader
            which starts with magic number $9AC6CDD7 and is detected in the case above.
        }
        if (n >= SizeOf(Header.WMF)) and
          (Header.Wmf.HeaderSize = 9) then
            case Header.Wmf.Version of
              $0100, $0300:
                begin
                  Result := itMeta;
                  Exit;
                end;
            end;

      $4D42: begin Result := itBmp; Exit; end;
      $D8FF: begin Result := itJpg; Exit; end;
    end;
  end;

  Result := itNone;
end;

function ConvertImage(Bitmap: ThtBitmap): ThtBitmap;
{convert bitmap into a form for BitBlt later}

  function DIBConvert: ThtBitmap;
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
    Result := ThtBitmap.Create(Bitmap.WithTransparentMask);
    Result.BitmapMask := Bitmap.BitmapMask;
    Bitmap.Free;
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

//-- BG ---------------------------------------------------------- 24.08.2015 --
procedure SetImageLoader(AImageLoader: ThtImageLoader);
begin
  if ImageLoader <> AImageLoader then
  begin
    ImageLoader.Free;
    ImageLoader := AImageLoader;
  end;
end;

//-- BG ---------------------------------------------------------- 24.08.2015 --
function GetImageLoader(): ThtImageLoader;
begin
  if ImageLoader = nil then
    ImageLoader := ThtImageLoader.Create;
  Result := ImageLoader;
end;

//-- BG ---------------------------------------------------------- 24.08.2015 --
function LoadImageFromStream(Stream: TStream; Transparent: ThtImageTransparency): ThtImage;
begin
  Result := GetImageLoader.LoadImageFromStream(Stream, Transparent);
end;

//-- BG ---------------------------------------------------------- 24.08.2015 --
procedure ThtImageLoader.AddSupportedKinds(const KindName, Extensions: string);
var
  I: Integer;
  Exts, SupportedExts: TStringList;
begin
  Exts := TStringList.Create;
  try
    Exts.CommaText := Extensions;

    for I := 0 to Exts.Count - 1 do
    begin
      if FSupportedExts.IndexOf(Exts[I]) < 0 then
        FSupportedExts.Add(Exts[I]);
    end;

    I := FSupportedKinds.IndexOf(KindName);
    if I >= 0 then
    begin
      SupportedExts := TStringList(FSupportedKinds.Objects[I]);
      for I := 0 to Exts.Count - 1 do
      begin
        if SupportedExts.IndexOf(Exts[I]) < 0 then
          SupportedExts.Add(Exts[I]);
      end;
    end
    else
    begin
      // new entry
      FSupportedKinds.AddObject(KindName, Exts);
      Exts := nil;
    end;
  finally
    Exts.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 24.08.2015 --
procedure ThtImageLoader.AddSupportedKinds;
begin

  AddSupportedKinds('Bitmaps', 'bmp');
  AddSupportedKinds('Graphics Interchange Format', 'gif');
  AddSupportedKinds('JPEG', 'jpg,jpeg');
{$if defined(LCL) or defined(Compiler20_Plus) }
  AddSupportedKinds('Portable Network Graphics', 'png');
{$ifend}
{$ifndef NoGDIPlus}
  AddSupportedKinds('Portable Network Graphics', 'png');
  AddSupportedKinds('Tagged Image Files', 'tif,tiff');
  AddSupportedKinds('Icons', 'ico');
  AddSupportedKinds('Cursors', 'cur');
  AddSupportedKinds('Windows Metafiles', 'wmf,emf');
{$endif}
{$ifndef NoMetafile}
  AddSupportedKinds('Windows Metafiles', 'wmf,emf');
{$endif}
end;

//-- BG ---------------------------------------------------------- 24.08.2015 --
constructor ThtImageLoader.Create;
begin
  inherited;
  FSupportedKinds := TStringList.Create;
  FSupportedExts := TStringList.Create;
  AddSupportedKinds;
end;

//-- BG ---------------------------------------------------------- 24.08.2015 --
destructor ThtImageLoader.Destroy;
var
  I: Integer;
begin
  FSupportedExts.Free;
  for i := 0 to FSupportedKinds.Count - 1 do
    FSupportedKinds.Objects[I].Free;
  FSupportedKinds.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 24.08.2015 --
function ThtImageLoader.GetFilter(const AllKinds: string): string;
var
  Files: TStringList;
  Filters: TStringList;

  procedure AddFilter(const Name: string; Exts: TStrings);
  var
    I: Integer;
  begin
    Files.Clear;
    for I := 0 to Exts.Count - 1 do
      Files.Add('*.' + Exts[I]);
    Files.Delimiter := ',';
    Filters.Add( Name + ' (' + Files.DelimitedText + ')' );
    Files.Delimiter := ';';
    Filters.Add( Files.DelimitedText );
  end;

var
  I: Integer;

begin
  Filters := TStringList.Create;
  Files := TStringList.Create;
{$ifdef HasStrictDelimiter}
  Files.StrictDelimiter := True;
{$endif}
  try
    if Length(AllKinds) > 0 then
      AddFilter( AllKinds, FSupportedExts );

    for I := 0 to FSupportedKinds.Count - 1 do
      AddFilter( FSupportedKinds[I], TStringList(FSupportedKinds.Objects[I]) );

    Filters.Delimiter := '|';
{$ifdef HasStrictDelimiter}
    Filters.StrictDelimiter := True;
    Result := Filters.DelimitedText;
{$else}
    SetLength(Result, 0);
    for I := 0 to Filters.Count - 1 do
      Result := Result + Filters[I] + '|';
    SetLength(Result, Length(Result) - 1);
{$endif}

  finally
    Files.Free;
    Filters.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 26.08.2015 --
function ThtImageLoader.LoadGraphicFromStream(Stream: TStream; Transparent: ThtImageTransparency): TGraphic;
var
  ImageFormat: ThtImageFormat;
begin
  Result := nil;
  if not Assigned(Stream) then
    Exit;

  try
    Stream.Seek(0, soFromBeginning); // Seek is faster than Position.
    ImageFormat := KindOfImage(Stream);
    if ImageFormat = itNone then
      Exit;

    Stream.Seek(0, soFromBeginning); // Seek is faster than Position.

{$ifndef NoGDIPlus}
    if GDIPlusActive then
      // as GDI+ is available, try it first to load images as it renders images nicer.
      try
        case ImageFormat of
          // GDI+ does not support animated GIFs:
          itGif: ;

          // GDI+ does not support transparency for bitmaps
          itBmp: if Transparent = itrNone then Result := ThtGpImage.Create;

          // GDI+ does not support lower left corner transparency for icons and cursors:
          itCur,
          itIco: if Transparent <> itrLLCorner then Result := ThtGpImage.Create;
        else
          Result := ThtGpImage.Create;
        end;
      except
        // just continue without image...
      end;
{$endif !NoGDIPlus}

    if Result = nil then
      case ImageFormat of
        itCur,
        itIco:  Result := TIcon.Create;
        itBmp:  Result := TBitmap.Create;
        itGif:  Result := TGifImage.Create;
        itJpg:  Result := TJPEGImage.Create;
{$ifndef NoMetafile}
        itMeta: Result := TMetafile.Create;
{$endif}
{$ifdef Compiler20_Plus}
        itPng:  Result := TPngImage.Create;
{$endif}
      end;

    if Result <> nil then
      Result.LoadFromStream(Stream);

  except
    FreeAndNil(Result);
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2010 --
function ThtImageLoader.LoadImageFromStream(Stream: TStream; Transparent: ThtImageTransparency): ThtImage;
// extracted from ThtDocument.GetTheBitmap(), ThtDocument.InsertImage(), and ThtDocument.ReplaceImage()

  procedure LoadMeta;
{$ifndef NoMetafile}
  var
    Meta: TMetaFile;
  begin
    Meta := TMetafile.Create;
    try
      Meta.LoadFromStream(Stream);
    except
      Meta.Free;
      raise;
    end;
    Result := ThtGraphicImage.Create(Meta);
{$else}
  begin
{$endif NoMetafile}
  end;

{$ifndef NoGDIPlus}
  procedure LoadGdipImage;
  var
    Image: ThtGpImage;
  begin
    Image := ThtGpImage.Create;
    try
      Image.LoadFromStream(Stream);
      Result := ThtGdipImage.Create(Image);
    except
      Image.Free;
      raise;
    end;
  end;
{$endif !NoGDIPlus}

  procedure LoadGif;
  var
    Gif: TGifImage;
  begin
    Gif := TGifImage.Create;
    Gif.LoadFromStream(Stream);
    if Gif.IsAnimated then
      Result := ThtGifImage.Create(Gif)
    else
    begin
      if Gif.IsTransparent then
        Transparent := itrIntrinsic;
      Result := ThtBitmapImage.Create(Gif, Transparent);
    end;
  end;

var
  Bitmap: ThtBitmap;

  procedure LoadPng;
{$if defined(LCL)}
  var
    PngImage: TPortableNetworkGraphic;
  begin
    PngImage := TPortableNetworkGraphic.Create;
    try
      PngImage.LoadFromStream(Stream);
      if ColorBits <= 8 then
        PngImage.PixelFormat := pf8bit
      else
        PngImage.PixelFormat := pf24bit;

      Bitmap := ThtBitmap.Create(PngImage.MaskHandleAllocated);
      Bitmap.Assign(PngImage);
      if PngImage.MaskHandleAllocated then
      begin
        Bitmap.BitmapMask.LoadFromBitmapHandles(PngImage.MaskHandle, 0);
        Transparent := itrIntrinsic;
      end
      else
        Transparent := itrNone;
    finally
      PngImage.Free;
    end;
{$elseif defined(Compiler20_Plus)}
  var
    PngImage: TPngImage;
  begin
    PngImage := TPngImage.Create;
    try
      PngImage.LoadFromStream(Stream);
      // As TPngImage cannot paint transparently on MetaFileCanvas in
      // TransparencyMode = ptmBit we convert it to a Bitmap.
      Bitmap := ThtBitmap.Create(PngImage.TransparencyMode = ptmBit);
      Bitmap.Assign(PngImage);
      // avoid ConvertImage:
      Result := ThtGraphicImage.Create(Bitmap);
      Result.Transp := itrIntrinsic;
      Bitmap := nil;
    finally
      PngImage.Free;
    end;
{$else}
  begin
{$ifend}
  end;

  procedure LoadJpeg;
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
      Bitmap := ThtBitmap.Create;
      Bitmap.Assign(jpImage);
      Transparent := itrNone;
    finally
      jpImage.Free;
    end;
  end;

  procedure LoadIco;
  var
    Icon: TIcon;
    IconInfo: TIconInfo;
  begin
    Icon := TIcon.Create;
    try
      Icon.LoadFromStream(Stream);
{$ifdef MSWindows}
      if GetIconInfo(Icon.Handle, {$ifdef LCL}@{$endif} IconInfo) then
      begin
        Bitmap := ThtBitmap.Create(Transparent <> itrLLCorner);
        Bitmap.Handle := IconInfo.hbmColor;
        if Transparent <> itrLLCorner then
        begin
          Bitmap.BitmapMask.Handle := IconInfo.hbmMask;
          Transparent := itrIntrinsic;
        end;
      end;
{$else}
      Bitmap := ThtBitmap.Create(Transparent <> itrLLCorner);
      Bitmap.LoadFromBitmapHandles(Icon.BitmapHandle, 0);
      if Transparent <> itrLLCorner then
      begin
        Bitmap.BitmapMask.LoadFromBitmapHandles(Icon.MaskHandle, 0);
        Transparent := itrIntrinsic;
      end;
{$endif}
    finally
      Icon.Free;
    end;
  end;

  procedure LoadBmp;
  begin
    Bitmap := ThtBitmap.Create;
    Bitmap.LoadFromStream(Stream);
  end;

var
  ImageFormat: ThtImageFormat;
  Mask: TBitmap;
begin
  Result := nil;
  if not Assigned(Stream) then
    Exit;

  Bitmap := nil;
  try
    Stream.Seek(0, soFromBeginning); // Seek is faster than Position.
    ImageFormat := KindOfImage(Stream);
    if ImageFormat = itNone then
      Exit;

    Stream.Seek(0, soFromBeginning); // Seek is faster than Position.

{$ifndef NoGDIPlus}
    if GDIPlusActive then
      // as GDI+ is available, try it first to load images as it renders images nicer.
      try
        case ImageFormat of
          // GDI+ does not support animated GIFs:
          itGif: ;

          // GDI+ does not support transparency for bitmaps
          itBmp: if Transparent = itrNone then LoadGdipImage;

          // GDI+ does not support lower left corner transparency for icons and cursors:
          itCur,
          itIco: if Transparent <> itrLLCorner then LoadGdipImage;
        else
          LoadGdipImage;
        end;
      except
        // just continue without image...
      end;
{$endif !NoGDIPlus}

    if Result = nil then
      case ImageFormat of
        itCur,
        itIco:  LoadIco;
        itBmp:  LoadBmp;
        itGif:  LoadGif;
        itJpg:  LoadJpeg;
        itMeta: LoadMeta;
        itPng:  LoadPng;
      end;

    if Bitmap <> nil then
    begin
      Mask := nil;
      if Bitmap.BitmapMask = nil then
        if Transparent = itrLLCorner then
          Mask := GetImageMask(Bitmap, False, 0);
      Bitmap := ConvertImage(Bitmap);
      if Mask <> nil then
      begin
        Bitmap.WithTransparentMask := True;
        Bitmap.BitmapMask := Mask;
        Mask.Free;
      end;
      Result := ThtBitmapImage.Create(Bitmap, Transparent);
    end;

  except
    Bitmap.Free;
    FreeAndNil(Result);
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
  Image: ThtImage; BW, BH: Integer; BGColor: TColor);
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
  OldBack, OldFore: TColorRef;
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
////TODO: let the better version win, but keep it in 2 separate methods for THtmlViewer.
//procedure DoImageStuff(IW, IH: Integer; BGImage: ThtImage; PRec: PtPositionRec; var TiledImage: ThtImage; var NoMask: boolean);
//{Set up for the background image. Allow for tiling, and transparency
//  BGImage is the image
//  PRec describes the location and tiling
//  IW, IH, the width and height of the background
//}
//var
//  OW, OH: Integer;
//  X, XX, Y, X2, Y2: Integer;
//
////----------------------------------------------------------
////these methods fill the TiledImage and TiledMask.
//
//  procedure Tile(Bitmap, Mask: TBitmap; W, H: Integer);
//  begin
//    repeat {tile BGImage in the various dc's}
//      XX := X;
//      repeat
//        TiledImage.Bitmap.Canvas.Draw(XX, Y, Bitmap);
//        if Mask <> nil then
//          TiledImage.Mask.Canvas.Draw(XX, Y, Mask)
//        else if not NoMask then
//          PatBlt(TiledImage.Mask.Canvas.Handle, XX, Y, Bitmap.Width, Bitmap.Height, Blackness);
//        Inc(XX, Bitmap.Width);
//      until XX >= X2;
//      Inc(Y, Bitmap.Height);
//    until Y >= Y2;
//  end;
//
//  procedure Enlarge;
//  var
//    TheBitmap, TheMask: TBitmap;
//    NewBitmap, NewMask: TBitmap;
//  begin
//    TheBitmap := BGImage.Bitmap;
//    TheMask   := BGImage.Mask;
//
//    NewBitmap := EnlargeImage(TheBitmap, X2 - X + 1, Y2 - Y + 1);
//    try
//      if TheMask <> nil then
//        NewMask := EnlargeImage(TheMask, X2 - X + 1, Y2 - Y + 1)
//      else
//        NewMask := nil;
//
//      try
//        Tile(NewBitmap, NewMask, X2 - X + 1, Y2 - Y + 1);
//      finally
//        NewMask.Free;
//      end;
//    finally
//      NewBitmap.Free;
//    end;
//  end;
//
//{$IFNDEF NoGDIPlus}
//  procedure TileGpImage(Image: ThtGdipImage; W, H: Cardinal);
//  var
//    ImW, ImH: Integer;
//    graphics: ThtGpGraphics;
//  begin
//    if Assigned(TiledImage) then
//      if (IW <> TiledImage.Width) or (IH <> TiledImage.Height) then
//        FreeAndNil(TiledImage);
//    if TiledImage = nil then
//      TiledImage := ThtGdipImage.Create( ThtGpBitmap.Create(IW, IH) );
//
//    ImW := Image.Width;
//    ImH := Image.Height;
//    try
//      graphics := ThtGpGraphics.Create(ThtGpImage(TiledImage));
//      try
//        graphics.Clear(clBlack); {clear to transparent black}
//        repeat {tile Image in the various dc's}
//          XX := X;
//          repeat
//            graphics.DrawImage(Image.Gpi, XX, Y, Image.Width, Image.Height);
//            Inc(XX, ImW);
//          until XX >= X2;
//          Inc(Y, ImH);
//        until Y >= Y2;
//      except
//      end;
//      graphics.Free;
//    except
//    end;
//  end;
//{$ENDIF !NoGDIPlus}
//
//var
//  TiledBitmap, TiledMask: TBitmap;
//begin
//  if (IW = 0) or (IH = 0) then
//    exit;
//
//  OW := BGImage.Width;
//  OH := BGImage.Height;
//
//{compute the location and tiling of BGImage in the background}
//  with PRec.X do
//  begin
//    X := GetPositionInRange(PosType, Value, IW - OW);
//    AdjustForTiling(RepeatD, 0, IW, OW, X, X2);
//  end;
//  with PRec.Y do
//  begin
//    Y := GetPositionInRange(PosType, Value, IH - OH);
//    AdjustForTiling(RepeatD, 0, IH, OH, Y, Y2);
//  end;
//
//  NoMask := not Assigned(BGImage.Mask) and PRec.X.RepeatD and PRec.Y.RepeatD;
//
//  if (OW = 1) or (OH = 1) then
//  begin
//    {in case a 1 pixel bitmap is being tiled.  EnlargeImage returns a TBitmap regardless of BgImage type.}
//    Enlarge;
//  end
//  else
//{$IFNDEF NoGDIPlus}
//  if BGImage is ThtGdipImage then
//  begin
//    {TiledImage becomes a ThtGdipBitmap as well}
//
//    TileGpImage(ThtGdipImage(BGImage), OW, OH);
//  end
//  else
//{$ENDIF !NoGDIPlus}
//  begin
//    {TiledImage becomes a ThtBitmapImage}
//    if TiledImage = nil then
//    begin
//      TiledBitmap := TBitmap.Create;
//      TiledBitmap.Palette := CopyPalette(ThePalette);
//      TiledBitmap.Height := IH;
//      TiledBitmap.Width := IW;
//      PatBlt(TiledBitmap.Canvas.Handle, 0, 0, IW, IH, Blackness);
//
//      if not NoMask then
//      begin
//        TiledMask := TBitmap.Create;
//        TiledMask.Monochrome := True;
//        TiledMask.Height := IH;
//        TiledMask.Width := IW;
//        if BGImage.Mask = nil then
//          PatBlt(TiledMask.Canvas.Handle, 0, 0, IW, IH, Whiteness);
//      end
//      else
//        TiledMask := nil;
//
//      TiledImage := ThtBitmapImage.Create(TiledBitmap, TiledMask, BGImage.Transp);
//    end;
//    Tile(BGImage.Bitmap, BGImage.Mask, OW, OH);
//  end;
//end;

{ TgpObject }

function EnlargeImage(Image: TBitmap; W, H: Integer): TBitmap;
{enlarge 1 pixel images for tiling.  Returns a TBitmap regardless of Image type}
begin
  Result := TBitmap.Create;
  Result.Assign(Image);
  if Image.Width = 1 then
    Result.Width := Min(100, W)
  else
    Result.Width := Image.Width;
  if Image.Height = 1 then
    Result.Height := Min(100, H)
  else
    Result.Height := Image.Height;
  Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), Image);
end;

{----------------PrintBitmap}

{$ifdef LCL}
{$else}
type
  ThtAllocRec = class(TObject)
  public
    Ptr: Pointer;
    ASize: Integer;
    AHandle: HGLOBAL;
  end;
{$endif}

procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
{Y relative to top of display here}

{$ifdef LCL}
begin
  Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), Bitmap);
end;
{$else}

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
          Abort;
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

var
  OldPal: HPalette;
  DC: HDC;
  Info: PBitmapInfo;
  Image: ThtAllocRec;
  ImageSize: DWord;
  InfoSize: DWord;

begin
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
end;
{$endif}

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
  Rgn, OldRgn: HRGN;
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
      Rgn := BitmapToRegion(AMask, @XForm, $FFFFFF);
      if Rgn <> 0 then {else nothing to output--this would be unusual}
      begin
        OldRgn := CreateRectRgn(0, 0, 1, 1); {a valid region is needed for the next call}
        Rslt := GetClipRgn(DC, OldRgn); {save the Old clip region}
        try
          if Rslt = 1 then
            CombineRgn(Rgn, Rgn, OldRgn, RGN_AND);
          SelectClipRgn(DC, Rgn);
          PrintBitmap(Canvas, X, Y, NewW, NewH, ABitmap);
        finally
          if Rslt = 1 then
            SelectClipRgn(DC, OldRgn)
          else
            SelectClipRgn(DC, 0);
          DeleteObject(Rgn);
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

procedure DrawGpImage(Handle: HDC; Image: ThtGpImage; DestX, DestY: Integer);
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

procedure DrawGpImage(Handle: HDC; Image: ThtGpImage; DestX, DestY,
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

procedure StretchDrawGpImage(Handle: HDC; Image: ThtGpImage; DestX, DestY,
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

procedure StretchPrintGpImageDirect(Handle: HDC; Image: ThtGpImage;
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

procedure PrintGpImageDirect(Handle: HDC; Image: ThtGpImage; DestX, DestY: Integer;
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

{ ThtImage }

//-- BG ---------------------------------------------------------- 02.09.2015 --
function ThtImage.Clone: ThtImage;
begin
  // currently only animated images need to be cloneable!
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtImage.Create(Tr: ThtImageTransparency);
begin
  inherited Create;
  Transp := Tr;
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtImage.Draw(Canvas: TCanvas; X, Y, W, H: Integer);
begin
  if Graphic <> nil then
    Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), Graphic);
end;

//-- BG ---------------------------------------------------------- 06.09.2015 --
procedure ThtImage.DrawUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean);
begin
  //TODO -oBG, 06.09.2015: draw any kind of image unstretched
end;

//-- BG ---------------------------------------------------------- 06.09.2015 --
function ThtImage.EnlargeBitmap(Bitmap: TBitmap; W, H: Integer): TBitmap;
{enlarge 1 pixel images for tiling.  Returns a TBitmap regardless of Image type}
begin
  if Bitmap = nil then
    Result := nil
  else
  begin
    Result := ThtBitmap.Create;
    Result.Assign(Bitmap);
    if Result.Width = 1 then
      Result.Width := Min(100, W);
    if Result.Height = 1 then
      Result.Height := Min(100, H);
    Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), Bitmap);
  end;
end;

//-- BG ---------------------------------------------------------- 06.09.2015 --
function ThtImage.EnlargeImage(W, H: Integer): ThtImage;
begin
  Result := ThtBitmapImage.Create(
    EnlargeBitmap(Bitmap, W, H),
    EnlargeBitmap(Mask  , W, H), Transp);
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

//-- BG ---------------------------------------------------------- 02.09.2015 --
function ThtImage.GetAnimate: Boolean;
begin
  Result := False;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
function ThtImage.GetGraphic: TGraphic;
begin
  Result := GetBitmap;
end;

//-- BG ---------------------------------------------------------- 02.09.2015 --
function ThtImage.IsAnimated: Boolean;
begin
  Result := False;
end;

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtImage.Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor);
begin
  if Graphic <> nil then
    Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), Graphic);
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

//-- BG ---------------------------------------------------------- 06.09.2015 --
procedure ThtImage.PrintUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean);
begin
  if Graphic <> nil then
    Canvas.Draw(X, Y, Graphic);
end;

//-- BG ---------------------------------------------------------- 02.09.2015 --
procedure ThtImage.SetAnimate(const Value: Boolean);
begin
  // do nothing as base class is not animated
end;

//-- BG ---------------------------------------------------------- 06.09.2015 --
procedure ThtImage.TileImage(PRec: PtPositionRec; DstW, DstH: Integer; var TiledImage: ThtImage; var NoMask: Boolean);
{EnlargeImage returns a ThtBitmapImage in TiledImage regardless of Self type.  Descendants may return other types}

  procedure Enlarge(W, H: Integer);
  var
    LargerImage: ThtImage;
  begin
    LargerImage := EnlargeImage(W + 1, H + 1);
    try
      LargerImage.TileImage(PRec, DstW, DstH, TiledImage, NoMask);
    finally
      LargerImage.Free;
    end;
  end;

var
  OW, OH, IW, IH: Integer;
  X, XX, Y, X2, Y2: Integer;

  procedure ValidateTiledImage;
  var
    TiledBitmap, TiledMask: TBitmap;
  begin
    {TiledImage becomes a ThtBitmapImage}
    if TiledImage = nil then
    begin
      TiledBitmap := TBitmap.Create;
      TiledBitmap.Palette := CopyPalette(ThePalette);
      TiledBitmap.Height := IH;
      TiledBitmap.Width := IW;
      PatBlt(TiledBitmap.Canvas.Handle, 0, 0, IW, IH, Blackness);

      if not NoMask then
      begin
        TiledMask := TBitmap.Create;
        TiledMask.Monochrome := True;
        TiledMask.Height := IH;
        TiledMask.Width := IW;
        if Mask = nil then
          PatBlt(TiledMask.Canvas.Handle, 0, 0, IW, IH, Whiteness);
      end
      else
        TiledMask := nil;

      TiledImage := ThtBitmapImage.Create(TiledBitmap, TiledMask, Transp);
    end;
  end;

  procedure Tile(W, H: Integer);
  begin

    repeat {tile BGImage in the various dc's}
      XX := X;
      repeat
        TiledImage.Bitmap.Canvas.Draw(XX, Y, Bitmap);
        if Mask <> nil then
          TiledImage.Mask.Canvas.Draw(XX, Y, Mask)
        else if not NoMask then
          PatBlt(TiledImage.Mask.Canvas.Handle, XX, Y, Bitmap.Width, Bitmap.Height, Blackness);
        Inc(XX, Bitmap.Width);
      until XX >= X2;
      Inc(Y, Bitmap.Height);
    until Y >= Y2;
  end;

begin
  IW := DstW;
  IH := DstH;
  OW := Width;
  OH := Height;

  if (IW = 0) or (IH = 0) then
  begin
    FreeAndNil(TiledImage);
    NoMask := True;
  end
  else if (OW = 1) or (OH = 1) then
  begin
    {in case a 1 pixel bitmap is being tiled}
    Enlarge(IW, IH);
  end
  else
  begin
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

    NoMask := not Assigned(Mask) and PRec.X.RepeatD and PRec.Y.RepeatD;

    ValidateTiledImage;
    Tile(OW, OH);
  end;
end;

{ ThtBitmapImage }

//-- BG ---------------------------------------------------------- 29.09.2015 --
constructor ThtBitmapImage.Create(AImage: ThtBitmap; Tr: ThtImageTransparency; AOwnsBitmap: Boolean);
begin
  if AImage = nil then
    raise EInvalidImage.Create('ThtBitmapImage requires an image');
  inherited Create(Tr);
  Bitmap := AImage;
  OwnsBitmap := AOwnsBitmap;
  Mask := AImage.BitmapMask;
  OwnsMask := False;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtBitmapImage.Create(AImage, AMask: TBitmap; Tr: ThtImageTransparency; AOwnsBitmap, AOwnsMask: Boolean);
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
constructor ThtBitmapImage.Create(AImage: TBitmap; Tr: ThtImageTransparency; Color: TColor; AOwnsBitmap: Boolean);
begin
  if AImage = nil then
    raise EInvalidImage.Create('ThtBitmapImage requires an image');
  inherited Create(Tr);
  Bitmap := AImage;
  OwnsBitmap := AOwnsBitmap;
  case Transp of
    itrIntrinsic:  Mask := GetImageMask(Bitmap, True, Color);
    itrLLCorner:   Mask := GetImageMask(Bitmap, False, Color);
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

//-- BG ---------------------------------------------------------- 06.09.2015 --
procedure ThtBitmapImage.DrawUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean);
var
  FullBG: TBitmap;
begin
  if Bitmap = nil then
    Exit;
  if (Mask = nil) or (Transp = itrNone) then
    BitBlt(Canvas.Handle, X, Y, W, H, Bitmap.Canvas.Handle, SrcX, SrcY, SRCCOPY)
  else
  begin
    FullBG := nil;
    InitFullBG(FullBG, W, H, False);
    try
      if FillBackground then
      begin
        FullBG.Canvas.Brush := Canvas.Brush;
        FullBG.Canvas.FillRect( Rect(0, 0, W, H));
      end
      else
        BitBlt(FullBG.Canvas.Handle, 0, 0, W, H,        Canvas.Handle, X,    Y, SRCCOPY   );

      BitBlt(FullBG.Canvas.Handle, 0, 0, W, H, Bitmap.Canvas.Handle, 0, SrcY, SRCINVERT );
      BitBlt(FullBG.Canvas.Handle, 0, 0, W, H,   Mask.Canvas.Handle, 0, SrcY, SRCAND    );
      BitBlt(FullBG.Canvas.Handle, 0, 0, W, H, Bitmap.Canvas.Handle, 0, SrcY, SRCPAINT  );

      BitBlt(       Canvas.Handle, X, Y, W, H, FullBG.Canvas.Handle, 0,    0, SRCCOPY   );
    finally
      FullBG.Free;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtBitmapImage.GetBitmap: TBitmap;
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

//-- BG ---------------------------------------------------------- 31.08.2015 --
procedure ThtBitmapImage.Print(Canvas: TCanvas; X, Y, W, H: Integer; BgColor: TColor);
begin
  if Transp = itrNone then
    inherited
  else if Mask = nil then
    PrintBitmap(Canvas, X, Y, W, H, Bitmap)
  else
    PrintTransparentBitmap3(Canvas, X, Y, W, H, Bitmap, Mask, 0, Bitmap.Height);
end;

//-- BG ---------------------------------------------------------- 06.09.2015 --
procedure ThtBitmapImage.PrintUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean);
var
  FullBG: TBitmap;
begin
  if Bitmap = nil then
    Exit;
  if (Mask = nil) or (Transp = itrNone) then
    PrintBitmap(Canvas, X, Y, W, H, Bitmap)
  else if FillBackground then
  begin
    FullBG := nil;
    InitFullBG(FullBG, W, H, True);
    try
      FullBG.Canvas.Brush := Canvas.Brush;
      FullBG.Canvas.FillRect( Rect(0, 0, W, H));

      BitBlt(FullBG.Canvas.Handle, 0, 0, W, H, Bitmap.Canvas.Handle, 0, SrcY, SRCINVERT );
      BitBlt(FullBG.Canvas.Handle, 0, 0, W, H,   Mask.Canvas.Handle, 0, SrcY, SRCAND    );
      BitBlt(FullBG.Canvas.Handle, 0, 0, W, H, Bitmap.Canvas.Handle, 0, SrcY, SRCPAINT  );

      PrintBitmap(Canvas, X, Y, W, H, FullBG);
    finally
      FullBG.Free;
    end;
  end
  else
    PrintTransparentBitmap3(Canvas, X, Y, W, H, Bitmap, Mask, SrcY, H);
end;

{ ThtGifImage }

//-- BG ---------------------------------------------------------- 11.04.2011 --
function ThtGifImage.Clone: ThtImage;
begin
  Result := ThtGifImage.Create(TGIFImage.CreateCopy(Gif));
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtGifImage.Create(AImage: TGifImage);
begin
  inherited Create(itrIntrinsic);
  FGif := AImage;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
destructor ThtGifImage.Destroy;
begin
  FGif.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
procedure ThtGifImage.Draw(Canvas: TCanvas; X, Y, W, H: Integer);
begin
  Gif.ShowIt := True; // animate it
  //Gif.Draw(Canvas, Rect(X, Y, X + W, Y + H));
  inherited;
end;

//-- BG ---------------------------------------------------------- 02.09.2015 --
function ThtGifImage.GetAnimate: Boolean;
begin
  Result := Gif.Animate;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGifImage.GetBitmap: TBitmap;
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
  Result := Gif.BitmapMask;
end;

//-- BG ---------------------------------------------------------- 02.09.2015 --
function ThtGifImage.IsAnimated: Boolean;
begin
  Result := Gif.IsAnimated;
end;

//-- BG ---------------------------------------------------------- 02.09.2015 --
procedure ThtGifImage.SetAnimate(const Value: Boolean);
begin
  Gif.Animate := Value;
end;

{$IFNDEF NoGDIPlus}

{ ThtGdipImage }

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtGdipImage.Create(AImage: ThtGpImage);
begin
  inherited Create(itrIntrinsic);
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

//-- BG ---------------------------------------------------------- 06.09.2015 --
procedure ThtGdipImage.DrawUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean);
begin
  DrawGpImage(Canvas.Handle, Gpi, X, Y, SrcX, SrcY, W, H);
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGdipImage.GetBitmap: TBitmap;
begin
  Result := Gpi.GetBitmap;
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

//-- BG ---------------------------------------------------------- 06.09.2015 --
procedure ThtGdipImage.PrintUnstretched(Canvas: TCanvas; X, Y, W, H, SrcX, SrcY: Integer; FillBackground: Boolean);
var
  FullBG: TBitmap;
begin
  if FillBackground then
  begin
    FullBG := nil;
    InitFullBG(FullBG, W, H, True);
    try
      FullBG.Canvas.Brush := Canvas.Brush;
      FullBG.Canvas.FillRect(Rect(0, 0, W, H));

      DrawGpImage(FullBg.Canvas.Handle, Gpi, 0, 0);

      PrintBitmap(Canvas, X, Y, W, H, FullBG);
    finally
      FullBG.Free;
    end;
  end
  else
    PrintGpImageDirect(Canvas.Handle, Gpi, X, Y{, Document.ScaleX, Document.ScaleY});

end;

{$ENDIF NoGDIPlus}

{ ThtGraphicImage }

//-- BG ---------------------------------------------------------- 09.04.2011 --
constructor ThtGraphicImage.Create(AGraphic: TGraphic);
begin
  inherited Create(itrNone);
  FGraphic := AGraphic;
end;

//-- BG ---------------------------------------------------------- 10.04.2011 --
destructor ThtGraphicImage.Destroy;
begin
  FImage.Free;
  FGraphic.Free;
  inherited;
end;

procedure ThtGraphicImage.Construct;
var
  Tmp: TBitmap;
  pe: TPaletteEntry;
  Color: TColor;
  FBitmap: TBitmap;
begin
  if not Assigned(FImage) then
  begin
    FBitmap := TBitmap.Create;
    try
      FBitmap.Width := Width;
      FBitmap.Height := Height;
      PatBlt(FBitmap.Canvas.Handle, 0, 0, Width, Height, Blackness);
      FBitmap.Canvas.Draw(0, 0, FGraphic);

      Tmp := TBitmap.Create;
      try
        Tmp.Width := Width;
        Tmp.Height := Height;
        Tmp.PixelFormat := pf8Bit;
      {pick an odd color from the palette to represent the background color,
       one not likely in the metafile}
        GetPaletteEntries(Tmp.Palette, 115, 1, pe);
        Color := pe.peBlue shl 16 or pe.peGreen shl 8 or pe.peRed;
        Tmp.Canvas.Brush.Color := Color;
        Tmp.Canvas.FillRect(Rect(0, 0, Width, Height));
        Tmp.Canvas.Draw(0, 0, FGraphic);

        FImage := ThtBitmapImage.Create(FBitmap, GetImageMask(Tmp, False, Color), itrLLCorner);
      finally
        Tmp.Free;
      end;
    except
      FreeAndNil(FBitmap);
    end;
  end;
end;

////-- BG ---------------------------------------------------------- 09.04.2011 --
//procedure ThtGraphicImage.Draw(Canvas: TCanvas; X, Y, W, H: Integer);
//begin
//  Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), FGraphic);
//end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGraphicImage.GetBitmap: TBitmap;
begin
  Construct;
  Result := FImage.Bitmap;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGraphicImage.GetGraphic: TGraphic;
begin
  Result := FGraphic;
end;

function ThtGraphicImage.GetImageHeight: Integer;
begin
  Result := FGraphic.Height;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGraphicImage.GetImageWidth: Integer;
begin
  Result := FGraphic.Width;
end;

//-- BG ---------------------------------------------------------- 09.04.2011 --
function ThtGraphicImage.GetMask: TBitmap;
begin
  Construct;
  Result := FImage.Mask;
end;

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

constructor ThtBitmapToInsert.Create(AImage: TBitmap; ATransp: ThtImageTransparency; AColor: TColor; AOwnsBitmap: Boolean);
begin
  Bitmap     := AImage;
  Color      := AColor;
  Transp     := ATransp;
  OwnsBitmap := AOwnsBitmap;
end;

{ ThtImageList }

{$ifdef UseGenerics}
{$else}
//-- BG ---------------------------------------------------------- 06.10.2016 --
function ThtImageList.Get(Index: Integer): ThtImage;
begin
  Result := inherited Get(Index);
end;
{$endif}

initialization
  DefBitMap := TBitmap.Create;
  ErrorBitMap := TBitmap.Create;
  ErrorBitMapMask := TBitmap.Create;
{$ifdef LCL}
  {$I htmlun2.lrs}
  DefBitMap.LoadFromLazarusResource('DefaultBitmap');
  ErrorBitMap.LoadFromLazarusResource('ErrBitmap');
  ErrorBitMapMask.LoadFromLazarusResource('ErrBitmapMask');
{$ifdef UseHandCursor}
  Screen.Cursors[HandCursor] := LoadCursorFromLazarusResource('Hand_Cursor');
{$endif}
  Screen.Cursors[UpDownCursor] := LoadCursorFromLazarusResource('UPDOWNCURSOR');
  Screen.Cursors[UpOnlyCursor] := LoadCursorFromLazarusResource('UPONLYCURSOR');
  Screen.Cursors[DownOnlyCursor] := LoadCursorFromLazarusResource('DOWNONLYCURSOR');
{$else}
  {$R Html32.res}
  DefBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(DefaultBitmap));
  ErrorBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmap));
  ErrorBitMapMask.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmapMask));
{$ifdef UseHandCursor}
  Screen.Cursors[HandCursor] := LoadCursor(HInstance, MakeIntResource(Hand_Cursor));
{$endif}
  Screen.Cursors[UpDownCursor] := LoadCursor(HInstance, 'UPDOWNCURSOR');
  Screen.Cursors[UpOnlyCursor] := LoadCursor(HInstance, 'UPONLYCURSOR');
  Screen.Cursors[DownOnlyCursor] := LoadCursor(HInstance, 'DOWNONLYCURSOR');
{$endif}

  DefImage := ThtBitmapImage.Create(DefBitmap, nil, itrNone);
  ErrorImage := ThtBitmapImage.Create(ErrorBitmap, ErrorBitmapMask, itrLLCorner);
//  ImageLoader := nil;
finalization
  ImageLoader.Free;
  ErrorImage.Free;
  DefImage.Free;
end.
