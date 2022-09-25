{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2022 by Bernd Gabriel

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

unit StyleTypes;

interface

uses
{$ifdef MSWindows}
  Windows,
{$else}
  Types,
{$endif}
{$ifdef LCL}
  LclIntf, LclType,
{$endif}
  SysUtils, Math, Forms, Variants, Graphics,
  //
  HtmlGlobals;

const
  varInt = [varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord];
  varFloat = [varSingle, varDouble, varCurrency];
  varNum = varInt + varFloat;

type
  ThtAlignmentStyle = (
    aNone,
    aTop,
    aMiddle,
    aBaseline,
    aBottom,
    aLeft,
    aRight,
    aJustify,
    aSub,
    aSuper);

const
  CAlignmentStyle: array[ThtAlignmentStyle] of ThtString = (
    'none',
    'top',
    'middle',
    'baseline',
    'bottom',
    'left',
    'right',
    'justify',
    'sub',
    'super');

type
  ThtBoxSizing = (
    ContentBox,
    BorderBox);

const
  CBoxSizing: array[ThtBoxSizing] of ThtString = (
    'content-box',
    'border-box');

type
  ThtBackgroundPosition = (
    bpTop,
    bpCenter,
    bpBottom,
    bpLeft,
    bpRight,
    bpPercent,
    bpDim);

  PositionRec = record
    PosType: ThtBackgroundPosition;
    Value: Integer;
    RepeatD: Boolean;
    Fixed: Boolean;
  end;
  PPositionRec = ^PositionRec;
  PtPositionRec = record
    X, Y: PositionRec;
  end;

type
  ThtBoxFloatStyle = (
    flNone,
    flLeft,
    flRight,
    flTop,
    flBottom);
const
  CBoxFloatStyle: array[ThtBoxFloatStyle] of ThtString = (
    'none',
    'left',
    'right',
    'top',
    'bottom');

type
  ThtBoxPositionStyle = (
    posStatic,
    posRelative,
    posAbsolute,
    posFixed);
const
  CBoxPositionStyle: array[ThtBoxPositionStyle] of ThtString = (
    'static',
    'relative',
    'absolute',
    'fixed');

type
  ThtBorderStyle = (
    bssNone,
    bssSolid,
    bssInset,
    bssOutset,
    bssGroove,
    bssRidge,
    bssDashed,
    bssDotted,
    bssDouble);
const
  CBorderStyle: array[ThtBorderStyle] of ThtString = (
    'none',
    'solid',
    'inset',
    'outset',
    'groove',
    'ridge',
    'dashed',
    'dotted',
    'double');

type
  ThtBulletStyle = (
    lbBlank,
    lbCircle,
    lbDecimal,
    lbDisc,
    lbLowerAlpha,
    lbLowerRoman,
    lbNone,
    lbSquare,
    lbUpperAlpha,
    lbUpperRoman);
const
  CBulletStyle: array[ThtBulletStyle] of ThtString = (
    'blank',
    'circle',
    'decimal',
    'disc',
    'lower-alpha',
    'lower-roman',
    'none',
    'square',
    'upper-alpha',
    'upper-roman');

type
  ThtClearStyle = (
    clrNone,
    clrLeft,
    clrRight,
    clrBoth);
const
  CClearStyle: array[ThtClearStyle] of ThtString = (
    'none',
    'left',
    'right',
    'both');

type
  ThtDisplayStyle = (
    pdUnassigned,
    pdInline,
    pdBlock,
    pdInlineBlock,
    pdListItem,
    pdRunIn,
    pdCompact,
    pdTable,
    pdInlineTable,
    pdTableRowGroup,
    pdTableHeaderGroup,
    pdTableFooterGroup,
    pdTableRow,
    pdTableColumnGroup,
    pdTableColumn,
    pdTableCell,
    pdTableCaption,
    pdNone);
const
  CDisplayStyle: array [ThtDisplayStyle] of ThtString = (
    '',
    'inline',
    'block',
    'inline-block',
    'list-item',
    'run-in',
    'compact',
    'table',
    'inline-table',
    'table-row-group',
    'table-header-group',
    'table-footer-group',
    'table-row',
    'table-column-group',
    'table-column',
    'table-cell',
    'table-caption',
    'none');

  CRootDisplayStyle: array[ThtDisplayStyle] of ThtDisplayStyle = (
    pdUnassigned,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdTable,
    pdTable,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdBlock,
    pdNone);

function ToRootDisplayStyle(Display: ThtDisplayStyle): ThtDisplayStyle; {$ifdef UseInline} inline; {$endif}

type
  ThtTextTransformStyle = (
    txNone,
    txUpper,
    txLower,
    txCaps);

type
  ThtVisibilityStyle = (
    viInherit,
    viHidden,
    viVisible);

type
  ThtWhiteSpaceStyle = (
    wsNormal,
    wsNoWrap,
    wsPre,
    wsPreWrap,
    wsPreLine{, wsInherit});

const
  CWhiteSpace: array [ThtWhiteSpaceStyle] of ThtString = (
    'normal',
    'nowrap',
    'pre',
    'pre-wrap',
    'pre-line');

//BG, 16.09.2010: CSS2.2: same sizes like html font size:
type
  ThtFontSizeIncrement = -6..6;
  ThtFontConvBase = array[0..7] of Double;
  ThtFontConv = array[1..7] of Double;
const
  FontConvBase:    ThtFontConvBase = (0.5833, 0.75,   0.8333, 1.0,    1.1667, 1.5,  2.0,    3.0);
  PreFontConvBase: ThtFontConvBase = (0.5,    0.5833, 0.75,   0.8333, 1.0,    1.25, 1.6667, 2.5);

type
  ThtRectEdge     = (reLeft, reTop, reRight, reBottom);
  ThtRectIntegers = packed array[ThtRectEdge] of Integer;
  ThtRectColors   = packed array[ThtRectEdge] of TColor;
  ThtRectStyles   = packed array[ThtRectEdge] of ThtBorderStyle;
const
  NullIntegers: ThtRectIntegers = (0, 0, 0, 0);
  NoneColors: ThtRectColors = (clNone, clNone, clNone, clNone);
  NoneStyles: ThtRectStyles = (bssNone, bssNone, bssNone, bssNone);

function htRectIntegers(Left, Top, Right, Bottom: Integer): ThtRectIntegers;
function htRectColors(Left, Top, Right, Bottom: TColor): ThtRectColors;
function htRectStyles(Left, Top, Right, Bottom: ThtBorderStyle): ThtRectStyles;

//------------------------------------------------------------------------------
// media types
//------------------------------------------------------------------------------

type
  // http://www.w3.org/TR/2010/WD-CSS2-20101207/media.html
  ThtMediaType = (
    mtAll,        // Suitable for all devices.
    mtBraille,    // Intended for braille tactile feedback devices.
    mtEmbossed,   // Intended for paged braille printers.
    mtHandheld,   // Intended for handheld devices (typically small screen, limited bandwidth).
    mtPrint,      // Intended for paged material and for documents viewed on screen in print preview mode. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtProjection, // Intended for projected presentations, for example projectors. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtScreen,     // Intended primarily for color computer screens.
    mtSpeech,     // Intended for speech synthesizers. Note: CSS2 had a similar media type called 'aural' for this purpose. See the appendix on aural style sheets for details.
    mtTty,        // Intended for media using a fixed-pitch character grid (such as teletypes, terminals, or portable devices with limited display capabilities). Authors should not use pixel units with the "tty" media type.
    mtTv          // Intended for television-type devices (low resolution, color, limited-scrollability screens, sound available).
  );

const
  CMediaTypes: array[ThtMediaType] of ThtString = (
    'all',
    'braille',
    'embossed',
    'handheld',
    'print',
    'projection',
    'screen',
    'speech',
    'tty',
    'tv'
  );

type
  ThtMediaOperator = (
    moUnknown,
    moIs,         // Is true
    moEq,         //  =
    moLe,         // <=
    moGe);        // >=

type
  ThtMediaFeature = (
    mfUnknown,
    mfWidth,
    mfHeight,
    mfOrientation,
    mfAspectRatio,
    mfDeviceWidth,
    mfDeviceHeight,
    mfDeviceAspectRatio,
    mfColor,
    mfColorIndex,
    mfMonochrome,
    mfResolution,
    mfScan,
    mfGrid
  );

const
  CMediaFeatures: array[ThtMediaFeature] of record
    Name: ThtString;
  end = (
    (Name:''                    ),
    (Name:'width'               ),
    (Name:'height'              ),
    (Name:'orientation'         ),
    (Name:'aspect-ratio'        ),
    (Name:'device-width'        ),
    (Name:'device-height'       ),
    (Name:'device-aspect-ratio' ),
    (Name:'color'               ),
    (Name:'color-index'         ),
    (Name:'monochrome'          ),
    (Name:'resolution'          ),
    (Name:'scan'                ),
    (Name:'grid'                )
  );

//------------------------------------------------------------------------------
// media queries
//------------------------------------------------------------------------------

type

  ThtMediaExpression = record
    Oper: ThtMediaOperator;
    Feature: ThtMediaFeature;
    Expression: Variant;
  end;

  ThtMediaExpressions = array of ThtMediaExpression;

  ThtMediaQuery = record
    MediaType: ThtMediaType;
    Negated: Boolean;
    Expressions: ThtMediaExpressions;
  end;

  ThtMediaQueries = array of ThtMediaQuery;
  ThtMediaQueryEvent = procedure(Sender: TObject; const MediaQuery: ThtMediaQuery; var MediaMatchesQuery: Boolean) of object;

function MediaQueryToStr(const MediaQuery: ThtMediaQuery): ThtString;
function MediaQueriesToStr(const MediaQueries: ThtMediaQueries): ThtString;
function TryStrToMediaType(const Str: ThtString; out MediaType: ThtMediaType): Boolean;
function TryStrToMediaFeature(const Str: ThtString; out Feature: ThtMediaFeature; out Oper: ThtMediaOperator): Boolean;

function StrToFontName(const Str: ThtString): ThtString;
function StrToFontSize(const Str: ThtString; const FontConvBase: ThtFontConvBase; DefaultFontSize, Base, Default: Double; PixelsPerInch: Integer): Double; overload;
function StrToFontSize(const Str: ThtString; const FontConv: ThtFontConv; Base, Default: Double; PixelsPerInch: Integer): Double; overload;
function StrToLength(const Str: ThtString; Relative: Boolean; Base, EmBase, Default: Double; PixelsPerInch: Integer): Double;

function TryStrToAlignmentStyle(const Str: ThtString; out AlignmentStyle: ThtAlignmentStyle): Boolean;
function TryStrToBoxSizing(const Str: ThtString; out ABoxSizing: ThtBoxSizing): Boolean;
function TryStrToBorderStyle(const Str: ThtString; out BorderStyle: ThtBorderStyle): Boolean;
function TryStrToBoxFloatStyle(const Str: ThtString; out Float: ThtBoxFloatStyle): Boolean;
function TryStrToBoxPositionStyle(const Str: ThtString; out Position: ThtBoxPositionStyle): Boolean;
function TryStrToBulletStyle(const Str: ThtString; out BulletStyle: ThtBulletStyle): Boolean;
function TryStrToClearStyle(const Str: ThtString; out Clear: ThtClearStyle): Boolean;
function TryStrToDisplayStyle(const Str: ThtString; out Display: ThtDisplayStyle): Boolean;
function TryStrToWhiteSpace(const Str: ThtString; out AWhiteSpace: ThtWhiteSpaceStyle): Boolean;
function GetPositionInRange(Which: ThtBackgroundPosition; Where, Range: Integer): Integer;
{
 Returns a positon according to the given settings.
 Which: which position in the range to get. pLeft and pTop return 0, pBottom and pRight return Range.
 Where: percentage or pixels for Which = pPercentage resp. pDim.
 Range: the range in which the result can vary.
 In the usual alignment calculations the range is outer size - inner size.
 If you have to consider an offset to outer's parent, add it to the function result afterwards.
}
procedure AdjustForTiling(Tiled: Boolean; TileAreaMin, TileAreaMax, TileSize: Integer;
  var Pos: Integer; out TiledEnd: Integer);
{
 Returns the start and end value for tiling an object of given size.
 Tiled: if false returns a TiledEnd according to unmodified Pos, that allows to pass the tiling
   process and depending on the visibility of the object in the cliparea the untiled object is
   processes at most once. If true, Pos is moved to a position between ClipMin - ObjectSize and
   ClipMin so that the tiling process will put one of the tiles to original Pos.
 TileAreaMin, TileAreaMax: the area in with the object is to tile.
 TileSize: the size of the tile.
 Pos: on input: the position to consider for tiling. on output the new position shifted by multiples
   of the object size to where the object covers the tile area minimum.
 TiledEnd: a position on and after which no more tiles are processed.
}
procedure CalcBackgroundLocationAndTiling(const PRec: PtPositionRec; ARect: TRect;
  XOff, YOff, IW, IH, BW, BH: Integer; out X, Y, X2, Y2: Integer);
{
 PRec has the CSS information on the background image, it's starting location and
 whether it is tiled in x, y, neither, or both.
 ARect is the cliprect, no point in drawing tiled images outside it.
 XOff, YOff are offsets which allow for the fact that the viewable area may not be at 0,0.
 IW, IH are the total width and height of the document if you could see it all at once.
 BW, BH are bitmap dimensions used to calc tiling.
 X, Y are the position (window coordinates) where the first background iamge will be drawn.
 X2, Y2 are tiling limits.  X2 and Y2 may be such that 0, 1, or many images will
   get drawn.  They're calculated so that only images within ARect are drawn.
}

{Internal stuff we expose for inlining}

const
// CSS 2.1 defines a fixed ratio between pt and px at 96 dpi: 1px = 0.75pt.
// (see http://www.w3.org/TR/2010/WD-CSS2-20101207/syndata.html#value-def-length for details).
//  HTMLViewerPixelsPerInch = 96.0; // fixed assumption in CSS 2.1 as lots of designs rely on it.
  HTMLViewerPointsPerInch = 72.0;
  //BG, 16.06.2015: Issue 399: StyleTypes.pas: Code improvement for C++Builder
  // HTMLViewerPointsPerPixel = HTMLViewerPointsPerInch / HTMLViewerPixelsPerInch;
  // produces idiotic "static const System::Extended HTMLViewerPointsPerPixel = 7.500000E-01;"
  // in StyleTypes.hpp which then produces "unused" warnings.
  // The following declaration produces just another #define:
//  HTMLViewerPointsPerPixel = 0.75; // HTMLViewerPointsPerInch / HTMLViewerPixelsPerInch;

type
  ThtLengthUnitInfo = record
    Name: ThtString;
    Factor: Double; // factor of length unit
    Index: Integer; // index of font size
    IsAbsolute: Boolean;
  end;
  ThtUnit = (
    luNone, luEm, luEx, luPercent, luPt, luPx, luPc, luIn, luCm, luMm,
    fsNone, fsSmaller, fsLarger,
    fsXxSmall, fsXSmall, fsSmall, fsMedium, fsLarge, fsXLarge, fsXxLarge);

  ThtLengthUnit = luNone..luMm;
  ThtFontSize = fsNone..fsXxLarge;

const
  CUnitInfo: array [ThtUnit] of ThtLengthUnitInfo = (
    // length units. Index = 0: constant Factor, Index = 1: Factor * PixelsPerInch.
    (Name: '';          Factor: 1.00       ; Index: 0; IsAbsolute: True),
    (Name: 'em';        Factor: 1.00       ; Index: 0; IsAbsolute: False),
    (Name: 'ex';        Factor: 0.50       ; Index: 0; IsAbsolute: False),
    (Name: '%' ;        Factor: 0.01       ; Index: 0; IsAbsolute: False),
    (Name: 'pt';        Factor: 1.00 / 72.0; Index: 1; IsAbsolute: True),
    (Name: 'px';        Factor: 1.00       ; Index: 0; IsAbsolute: True),
    (Name: 'pc';        Factor: 9.00       ; Index: 0; IsAbsolute: True),
    (Name: 'in';        Factor: 1.00       ; Index: 1; IsAbsolute: True),
    (Name: 'cm';        Factor: 1.00 / 2.54; Index: 1; IsAbsolute: True),
    (Name: 'mm';        Factor: 1.00 / 25.4; Index: 1; IsAbsolute: True),

    // css font sizes
    (Name: '';          Factor: 1.00; Index:  3; IsAbsolute: True),
    (Name: 'smaller';   Factor: 1.00; Index: -1; IsAbsolute: False),
    (Name: 'larger';    Factor: 1.00; Index:  1; IsAbsolute: False),
    (Name: 'xx-small';  Factor: 1.00; Index:  0; IsAbsolute: True),
    (Name: 'x-small';   Factor: 1.00; Index:  1; IsAbsolute: True),
    (Name: 'small';     Factor: 1.00; Index:  2; IsAbsolute: True),
    (Name: 'medium';    Factor: 1.00; Index:  3; IsAbsolute: True),
    (Name: 'large';     Factor: 1.00; Index:  4; IsAbsolute: True),
    (Name: 'x-large';   Factor: 1.00; Index:  5; IsAbsolute: True),
    (Name: 'xx-large';  Factor: 1.00; Index:  6; IsAbsolute: True)
  );

implementation

//-- BG ---------------------------------------------------------- 05.04.2011 --
function htRectIntegers(Left, Top, Right, Bottom: Integer): ThtRectIntegers;
 {$ifdef UseInline} inline; {$endif}
begin
  Result[reTop] := Top;
  Result[reLeft] := Left;
  Result[reRight] := Right;
  Result[reBottom] := Bottom;
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
function htRectColors(Left, Top, Right, Bottom: TColor): ThtRectColors;
 {$ifdef UseInline} inline; {$endif}
begin
  Result[reTop] := Top;
  Result[reLeft] := Left;
  Result[reRight] := Right;
  Result[reBottom] := Bottom;
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
function htRectStyles(Left, Top, Right, Bottom: ThtBorderStyle): ThtRectStyles;
 {$ifdef UseInline} inline; {$endif}
begin
  Result[reTop] := Top;
  Result[reLeft] := Left;
  Result[reRight] := Right;
  Result[reBottom] := Bottom;
end;

//-- BG ---------------------------------------------------------- 22.10.2016 --
function MediaQueryToStr(const MediaQuery: ThtMediaQuery): ThtString;
var
  I: Integer;
  E: ThtString;
begin
  SetLength(Result, 0);
  with MediaQuery do
  begin
    if Negated then
      Result := 'not ';
    htAppendStr(Result, CMediaTypes[MediaType]);
    for I := Low(Expressions) to High(Expressions) do
      with Expressions[I] do
      begin
        E := Expression;
        if Length(E) > 0 then
          E := ':' + E;
        E := CMediaFeatures[Feature].Name + E;
        case Oper of
           moGe: E := 'min-' + E;
           moLe: E := 'max-' + E;
        end;
      end;
  end;
end;

//-- BG ---------------------------------------------------------- 22.10.2016 --
function MediaQueriesToStr(const MediaQueries: ThtMediaQueries): ThtString;
var
  I: Integer;
begin
  SetLength(Result, 0);
  if MediaQueries <> nil then
    for I := Low(MediaQueries) to High(MediaQueries) do
    begin
      if Length(Result) > 0 then
        htAppendStr(Result, ', ');
      htAppendStr(Result, MediaQueryToStr(MediaQueries[I]));
    end;
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
function TryStrToMediaType(const Str: ThtString; out MediaType: ThtMediaType): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtMediaType;
begin
  // case sensitive!
  for I := Low(I) to High(I) do
    if CMediaTypes[I] = Str then
    begin
      Result := True;
      MediaType := I;
      exit;
    end;

  Result := Str = 'aural';
  if Result then
    MediaType := mtSpeech;
end;

function TryStrToMediaFeature(const Str: ThtString; out Feature: ThtMediaFeature; out Oper: ThtMediaOperator): Boolean;
var
  P: Integer;
  O, F: ThtString;
  I: ThtMediaFeature;
begin
  P := Pos('-', Str);
  if P = 0 then
  begin
    Oper := moEq;
    F := Str;
  end
  else
  begin
    O := Copy(Str,     1, P - 1);
    F := Copy(Str, P + 1, MaxInt);
    if O = 'min' then
      Oper := moGe
    else if O = 'max' then
      Oper := moLe
    else
    begin
      Oper := moUnknown;
      F := Str;
    end;
  end;

  for I := Low(I) to High(I) do
    if CMediaFeatures[I].Name = F then
    begin
      Result := True;
      Feature := I;
      exit;
    end;

  Feature := mfUnknown;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
function TryStrToAlignmentStyle(const Str: ThtString; out AlignmentStyle: ThtAlignmentStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtAlignmentStyle;
begin
  for I := low(I) to high(I) do
    if CAlignmentStyle[I] = Str then
    begin
      Result := True;
      AlignmentStyle := I;
      exit;
    end;
  Result := False;
end;

//-- JPM --------------------------------------------------------- 03.02-2012 --

function TryStrToBoxSizing(const Str: ThtString; out ABoxSizing: ThtBoxSizing): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtBoxSizing;
begin
  for I := low(I) to high(I) do
    if CBoxSizing[I] = Str then
    begin
      Result := True;
      ABoxSizing := I;
      exit;
    end;
  Result := False;
end;

//-- JPM --------------------------------------------------------- 03.02-2012 --

function TryStrToWhiteSpace(const Str: ThtString; out AWhiteSpace: ThtWhiteSpaceStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtWhiteSpaceStyle;
begin
  for I := low(I) to high(I) do
    if CWhiteSpace[I] = Str then
    begin
      Result := True;
      AWhiteSpace := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 16.03.2011 --
function TryStrToBorderStyle(const Str: ThtString; out BorderStyle: ThtBorderStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtBorderStyle;
begin
  for I := low(I) to high(I) do
    if CBorderStyle[I] = Str then
    begin
      Result := True;
      BorderStyle := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TryStrToBoxFloatStyle(const Str: ThtString; out Float: ThtBoxFloatStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtBoxFloatStyle;
begin
  for I := low(I) to high(I) do
    if CBoxFloatStyle[I] = Str then
    begin
      Result := True;
      Float := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TryStrToBoxPositionStyle(const Str: ThtString; out Position: ThtBoxPositionStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtBoxPositionStyle;
begin
  for I := low(I) to high(I) do
    if CBoxPositionStyle[I] = Str then
    begin
      Result := True;
      Position := I;
      exit;
    end;
  Result := False;
end;


//-- BG ---------------------------------------------------------- 16.04.2011 --
function TryStrToBulletStyle(const Str: ThtString; out BulletStyle: ThtBulletStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtBulletStyle;
begin
  for I := low(I) to high(I) do
    if CBulletStyle[I] = Str then
    begin
      Result := True;
      BulletStyle := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 16.03.2011 --
function TryStrToDisplayStyle(const Str: ThtString; out Display: ThtDisplayStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtDisplayStyle;
begin
  for I := low(I) to high(I) do
    if CDisplayStyle[I] = Str then
    begin
      Result := True;
      Display := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 16.03.2011 --
function TryStrToClearStyle(const Str: ThtString; out Clear: ThtClearStyle): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtClearStyle;
begin
  for I := low(I) to high(I) do
    if CClearStyle[I] = Str then
    begin
      Result := True;
      Clear := I;
      exit;
    end;

  // clear attribute of <br> uses 'all' as 'both'
  Result := Str = 'all';
  if Result then
    Clear := clrBoth;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function ToRootDisplayStyle(Display: ThtDisplayStyle): ThtDisplayStyle; {$ifdef UseInline} inline; {$endif}
 {$ifdef UseInline} inline; {$endif}
begin
  Result := CRootDisplayStyle[Display];
end;


//-- BG ---------------------------------------------------------- 01.05.2011 --
function StrToFontName(const Str: ThtString): ThtString;

  function GetNextSplitter(const Str: ThtString; var I: Integer; out Splitter: ThtString): Boolean;
  var
    J: Integer;
    Dlm: ThtChar;
  begin
    Result := I < Length(Str);
    if Result then
    begin
      J := htPos(',', Str, I);
      if J = 0 then
        // no more commas, try the rest
        J := Length(Str) + 1;

      // trim left and detect string delimiters " or '
      Dlm := #0;
      while I < J do
      begin
        case Str[I] of
          TabChar,
          LfChar,
          FfChar,
          CrChar,
          SpcChar:
            Inc(I);

          '''',
          '"':
          begin
            Dlm := Str[I];
            Inc(I);
            break;
          end;
        else
          break;
        end;
      end;

      // trim right and detect matching string delimiter
      while I < J do
      begin
        case Str[J - 1] of
          TabChar,
          LfChar,
          FfChar,
          CrChar,
          SpcChar:
            Dec(I);
        else
          if Str[J - 1] = Dlm then
            Dec(J);
          break;
        end;
      end;

      if I < J then
        Splitter := Copy(Str, I, J - I)
      else
        Result := False;
      I := J + 1;
    end;
  end;

  procedure TranslateGenericFontName(var Str: ThtString);
  const
    AMax = 5;
  const
    Generic1: array[1..AMax] of ThtString = ('serif'  , 'monospace', 'sans-serif', 'cursive'  , 'helvetica');
    Generic2: array[1..AMax] of ThtString = (FontSerif, FontMono   , FontSans    , FontCursive, FontHelvet );
  var
    I: Integer;
    F: ThtString;
  begin
    F := htLowerCase(Str);
    for I := 1 to AMax do
      if htCompareStr(F, Generic1[I]) = 0 then
      begin
        Str := Generic2[I];
        break;
      end;
  end;

var
  Pos: Integer;
  FontName: ThtString;
begin
  Pos := 1;
  while GetNextSplitter(Str, Pos, FontName) do
  begin
    TranslateGenericFontName(FontName);
    if Screen.Fonts.IndexOf(htStringToString(FontName)) >= 0 then
    begin
      Result := FontName;
      break;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 14.07.2010 --
function DecodeSize(const Str: ThtString; out V: Double; out U: ThtString): Boolean;
 {$ifdef UseInline} inline; {$endif}
{
 Get a mandatory numerical value and an optional unit string from given Str.
 Returns true, if at least the numerical value has been parsed from Str.
 Str is the string to parse.
 V returns the parsed numerical value.
 U returns the parsed unit string or an empty string;
}
var
  I, J, L: Integer;
begin
  U := '';
  Val(Str, V, I);
  Result := I <> 1;
  if Result then
  begin
    L := Length(Str);
    if I = 0 then
      I := L + 1;
    J := Pos('e', Str); {'e' would be legal for Val but not for us}
    if (J > 0) and (I > J) then
      I := J;
    if I <= L then
    begin
      Val(Copy(Str, 1, I - 1), V, J);
      U := Trim(Copy(Str, I, L - I + 1)); // text after number, maybe a unit
    end;
  end;
end;


//-- BG ---------------------------------------------------------- 01.05.2011 --
function TryStrToLenthUnit(const Str: ThtString; out LengthUnit: ThtLengthUnit): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  L: ThtString;
  I: ThtLengthUnit;
begin
  L := htLowerCase(Str);
  for I := low(I) to high(I) do
    if htCompareStr(CUnitInfo[I].Name, L) = 0 then
    begin
      Result := True;
      LengthUnit := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TryStrToFontSize(const Str: ThtString; out FontSize: ThtFontSize): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  L: ThtString;
  I: ThtFontSize;
begin
  L := htLowerCase(Str);
  for I := low(I) to high(I) do
    if htCompareStr(CUnitInfo[I].Name, L) = 0 then
    begin
      Result := True;
      FontSize := I;
      exit;
    end;
  Result := False;
end;

//------------------------------------------------------------------------------
function StrToLength(const Str: ThtString; Relative: Boolean; Base, EmBase, Default: Double; PixelsPerInch: Integer): Double;
 {$ifdef UseInline} inline; {$endif}
{
 Given a length string, return the appropriate pixel value.
 Base is the base value for a relative value without unit or with percentage.
 EmBase is the base value for units relative to the font.
 Default returned if no match.
}
var
  V: Double;
  U: ThtString;
  LU: ThtLengthUnit;
begin
  Result := Default;
  if DecodeSize(Str, V, U) then
  begin
    {U the units}
    if U = '' then
    begin
      if Relative then
        Result := V * Base;
    end
    else if TryStrToLenthUnit(U, LU) then
      with CUnitInfo[LU] do
      begin
        if Index = 1 then
          V := V * PixelsPerInch;
        if IsAbsolute then
          Result := V * Factor
        else if LU = luPercent then
          Result := V * Factor * Base
        else
          Result := V * Factor * EmBase;
      end
    else
  end;
end;

//------------------------------------------------------------------------------
function StrToFontSize(const Str: ThtString; const FontConv: ThtFontConv; Base, Default: Double; PixelsPerInch: Integer): Double;
{given a font-size ThtString, return the point size}

  function IncFontSize(Increment: ThtFontSizeIncrement): Double;
  var
    OldIndex, NewIndex: Byte;
    D1, D2: Double;
  begin
    // get nearest old font size index
    OldIndex := 4;
    D1 := Base - FontConv[OldIndex];
    repeat
      case Sign(D1) of
        -1:
        begin
          Dec(OldIndex);
          D2 := Base - FontConv[OldIndex];
          if D2 >= 0 then
          begin
            if Abs(D1) < Abs(D2) then
              Inc(OldIndex);
            break;
          end;
          D1 := D2;
        end;

        1:
        begin
          Inc(OldIndex);
          D2 := Base - FontConv[OldIndex];
          if D2 <= 0 then
          begin
            if Abs(D1) > Abs(D2) then
              Dec(OldIndex);
            break;
          end;
          D1 := D2;
        end;

      else
        break;
      end;
    until (OldIndex = 1) or (OldIndex = 7);

    NewIndex := OldIndex + Increment;
    if NewIndex < 1 then
    begin
      Inc(OldIndex, 1 - NewIndex);
      NewIndex := 1;
    end
    else if NewIndex > 7 then
    begin
      Dec(OldIndex, NewIndex - 7);
      NewIndex := 7;
    end;

    if OldIndex = NewIndex then
      Result := Base
    else
      Result := Base * FontConv[NewIndex] / FontConv[OldIndex];
  end;

var
  V: Double;
  U: ThtString;
  LU: ThtLengthUnit;
  FS: ThtFontSize;
begin
  Result := Default;
  if DecodeSize(Str, V, U) then
  begin
    if TryStrToLenthUnit(U, LU) then
      with CUnitInfo[LU] do
      begin
        if Index = 1 then
          V := V * PixelsPerInch;
        if IsAbsolute then
          Result := V * Factor * HTMLViewerPointsPerInch / PixelsPerInch
        else
          Result := V * Factor * Base;
      end
  end
  else
  begin
    if TryStrToFontSize(Str, FS) then
      with CUnitInfo[FS] do
        if IsAbsolute then
          Result := FontConv[Index]
        else
          Result := IncFontSize(Index);
  end;
end;

//------------------------------------------------------------------------------
function StrToFontSize(const Str: ThtString; const FontConvBase: ThtFontConvBase; DefaultFontSize, Base, Default: Double; PixelsPerInch: Integer): Double;
{given a font-size ThtString, return the point size}

  function IncFontSize(Increment: ThtFontSizeIncrement): Double;
  var
    OldIndex, NewIndex: Byte;
    D1, D2: Double;
  begin
    // get nearest old font size index
    OldIndex := 4;
    D1 := Base - FontConvBase[OldIndex] * DefaultFontSize;
    repeat
      case Sign(D1) of
        -1:
        begin
          Dec(OldIndex);
          D2 := Base - FontConvBase[OldIndex] * DefaultFontSize;
          if D2 >= 0 then
          begin
            if Abs(D1) < Abs(D2) then
              Inc(OldIndex);
            break;
          end;
          D1 := D2;
        end;

        1:
        begin
          Inc(OldIndex);
          D2 := Base - FontConvBase[OldIndex] * DefaultFontSize;
          if D2 <= 0 then
          begin
            if Abs(D1) > Abs(D2) then
              Dec(OldIndex);
            break;
          end;
          D1 := D2;
        end;

      else
        break;
      end;
    until (OldIndex = 1) or (OldIndex = 7);

    NewIndex := OldIndex + Increment;
    if NewIndex < 1 then
    begin
      Inc(OldIndex, 1 - NewIndex);
      NewIndex := 1;
    end
    else if NewIndex > 7 then
    begin
      Dec(OldIndex, NewIndex - 7);
      NewIndex := 7;
    end;

    if OldIndex = NewIndex then
      Result := Base
    else
      Result := Base * FontConvBase[NewIndex] / FontConvBase[OldIndex];
  end;

var
  V: Double;
  U: ThtString;
  LU: ThtLengthUnit;
  FS: ThtFontSize;
begin
  Result := Default;
  if DecodeSize(Str, V, U) then
  begin
    if TryStrToLenthUnit(U, LU) then
      with CUnitInfo[LU] do
      begin
        if Index = 1 then
          V := V * PixelsPerInch;
        if IsAbsolute then
          Result := V * Factor * HTMLViewerPointsPerInch / PixelsPerInch
        else
          Result := V * Factor * Base;
      end;
  end
  else
  begin
    if TryStrToFontSize(Str, FS) then
      with CUnitInfo[FS] do
        if IsAbsolute then
          Result := FontConvBase[Index] * DefaultFontSize
        else
          Result := IncFontSize(Index);
  end;
end;

//-- BG ---------------------------------------------------------- 07.04.2011 --
function GetPositionInRange(Which: ThtBackgroundPosition; Where, Range: Integer): Integer;
 {$ifdef UseInline} inline; {$endif}
{
 Returns a positon according to the given settings.
 Which: which position in the range to get. pLeft and pTop return 0, pBottom and pRight return Range.
 Where: percentage or pixels for Which = pPercentage resp. pDim.
 Range: the range in which the result can vary.
 In the usual alignment calculations the range is outer size - inner size.
 If you have to consider an offset to outer's parent, add it to the function result afterwards.
}
begin
  case Which of
    bpTop,
    bpLeft:
      Result := 0;

    bpCenter:
      Result := Range div 2;

    bpBottom,
    bpRight:
      Result := Range;

    bpPercent:
      Result := (Range * Where) div 100;

    bpDim:
      Result := Where;
  else
    Result := 0;
  end;
end;

//-- BG ---------------------------------------------------------- 07.04.2011 --
procedure AdjustForTiling(Tiled: Boolean; TileAreaMin, TileAreaMax, TileSize: Integer;
  var Pos: Integer; out TiledEnd: Integer);
   {$ifdef UseInline} inline; {$endif}
{
 Returns the start and end value for tiling a tile of given size.
 Tiled: if false returns a TiledEnd according to unmodified Pos, that allows to pass the tiling
   process and depending on the visibility of the object in the cliparea the untiled object is
   processes at most once. If true, Pos is moved to a position between ClipMin - ObjectSize and
   ClipMin so that the tiling process will put one of the tiles to original Pos.
 TileAreaMin, TileAreaMax: the area in which the object is to tile.
 TileSize: the size of the tile.
 Pos: on input: the position to consider for tiling. on output the new position shifted by multiples
   of the object size to where the object covers the tile area minimum.
 TiledEnd: a position on and after which no more tiles are processed.
}
var
  TileAreaMinPos: Integer;
begin
  if Tiled then
  begin
    TileAreaMinPos := TileAreaMin - Pos;
    {figure a starting point for tiling.  This will be less that one object size less than the tile area min}
    if TileSize <= TileAreaMinPos then
      Pos := TileAreaMin - TileAreaMinPos mod TileSize
    else if TileAreaMinPos < 0 then
      Pos := TileAreaMin - (TileSize - -TileAreaMinPos mod TileSize);
    TiledEnd := TileAreaMax;
  end
  else
  begin {a single image or row}
    TiledEnd := Pos; {assume it's not in the tile area and won't be output}
    if (TileAreaMin < Pos + TileSize) and (Pos < TileAreaMax) then
      Inc(TiledEnd); {it is in the tile area, show it}
  end;
end;

//-- BG ---------------------------------------------------------- 07.04.2011 --
procedure CalcBackgroundLocationAndTiling(const PRec: PtPositionRec; ARect: TRect;
  XOff, YOff, IW, IH, BW, BH: Integer; out X, Y, X2, Y2: Integer);
 {$ifdef UseInline} inline; {$endif}
{
 PRec has the CSS information on the background image, it's starting location and
 whether it is tiled in x, y, neither, or both.
 ARect is the cliprect, no point in drawing tiled images outside it.
 XOff, YOff are offsets which allow for the fact that the viewable area may not be at 0,0.
 IW, IH are the total width and height of the document if you could see it all at once.
 BW, BH are bitmap dimensions used to calc tiling.
 X, Y are the position (window coordinates) where the first background iamge will be drawn.
 X2, Y2 are tiling limits.  X2 and Y2 may be such that 0, 1, or many images will
   get drawn.  They're calculated so that only images within ARect are drawn.
}
begin
  with PRec.X do
  begin
    X := GetPositionInRange(PosType, Value, IW - BW) - XOff;
    AdjustForTiling(RepeatD, ARect.Left, ARect.Right, BW, X, X2);
  end;
  with PRec.Y do
  begin
    Y := GetPositionInRange(PosType, Value, IH - BH) - YOff;
    AdjustForTiling(RepeatD, ARect.Top, ARect.Bottom, BH, Y, Y2);
  end;
end;

end.
