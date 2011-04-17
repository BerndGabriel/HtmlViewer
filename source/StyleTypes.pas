{
Version   12
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011 by Bernd Gabriel

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

unit StyleTypes;

interface

uses
  Windows, SysUtils,
  //
  HtmlGlobals;

type
  TAlignmentStyle = (
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
  CAlignmentStyle: array[TAlignmentStyle] of ThtString = (
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
  TBackgroundPosition = (
    bpTop,
    bpCenter,
    bpBottom,
    bpLeft,
    bpRight,
    bpPercent,
    bpDim);

  PositionRec = record
    PosType: TBackgroundPosition;
    Value: Integer;
    RepeatD: Boolean;
    Fixed: Boolean;
  end;
  PtPositionRec = array[1..2] of PositionRec;

type
  TBoxPositionStyle = (
    posStatic,
    posRelative,
    posAbsolute,
    posFixed);

type
  TBorderStyle = (
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
  CBorderStyle: array[TBorderStyle] of ThtString = (
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
  TBulletStyle = (
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
  CBulletStyle: array[TBulletStyle] of ThtString = (
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
  TClearStyle = (
    clrNone,
    clLeft,
    clRight,
    clAll);

type
  TDisplayStyle = (
    pdUnassigned,
    pdInline,
    pdBlock,
    pdListItem,
    pdRunIn,
    pdInlineBlock,
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
  CDisplayStyle: array [TDisplayStyle] of ThtString = (
    '',
    'inline',
    'block',
    'list-item',
    'run-in',
    'inline-block',
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

type
  TTextTransformStyle = (
    txNone,
    txUpper,
    txLower,
    txCaps);

type
  TVisibilityStyle = (
    viInherit,
    viHidden,
    viVisible);

type
  TWhiteSpaceStyle = (
    wsNormal,
    wsPre,
    wsNoWrap,
    wsPreWrap,
    wsPreLine{, wsInherit});

//BG, 16.09.2010: CSS2.2: same sizes like html font size:
type
  TFontSizeIncrement = -6..6;
const
  FontConvBase: array[1..7] of double = (8.0, 10.0, 12.0, 14.0, 18.0, 24.0, 36.0);
  PreFontConvBase: array[1..7] of double = (7.0, 8.0, 10.0, 12.0, 15.0, 20.0, 30.0);

//------------------------------------------------------------------------------
// media types
//------------------------------------------------------------------------------

type
  // http://www.w3.org/TR/2010/WD-CSS2-20101207/media.html
  TMediaType = (
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
  TMediaTypes = set of TMediaType;

const
  CMediaTypes: array[TMediaType] of ThtString = (
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
  AllMediaTypes: TMediaTypes = [
    //mtAll,        // Suitable for all devices.
    mtBraille,    // Intended for braille tactile feedback devices.
    mtEmbossed,   // Intended for paged braille printers.
    mtHandheld,   // Intended for handheld devices (typically small screen, limited bandwidth).
    mtPrint,      // Intended for paged material and for documents viewed on screen in print preview mode. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtProjection, // Intended for projected presentations, for example projectors. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtScreen,     // Intended primarily for color computer screens.
    mtSpeech,     // Intended for speech synthesizers. Note: CSS2 had a similar media type called 'aural' for this purpose. See the appendix on aural style sheets for details.
    mtTty,        // Intended for media using a fixed-pitch character grid (such as teletypes, terminals, or portable devices with limited display capabilities). Authors should not use pixel units with the "tty" media type.
    mtTv          // Intended for television-type devices (low resolution, color, limited-scrollability screens, sound available).
  ];

function MediaTypesToStr(const MediaTypes: TMediaTypes): ThtString;
function TranslateMediaTypes(const MediaTypes: TMediaTypes): TMediaTypes;
function TryStrToMediaType(const Str: ThtString; out MediaType: TMediaType): Boolean;
function TryStrToMediaTypes(const Str: ThtString; out MediaTypes: TMediaTypes): Boolean;

function TryStrToAlignmentStyle(const Str: ThtString; out AlignmentStyle: TAlignmentStyle): Boolean;
function TryStrToBorderStyle(const Str: ThtString; out BorderStyle: TBorderStyle): Boolean;
function TryStrToBulletStyle(const Str: ThtString; out BulletStyle: TBulletStyle): Boolean;
function TryStrToDisplayStyle(const Str: ThtString; out Display: TDisplayStyle): Boolean;

function GetPositionInRange(Which: TBackgroundPosition; Where, Range: Integer): Integer;
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
{PRec has the CSS information on the background image, it's starting location and
 whether it is tiled in x, y, neither, or both.
 ARect is the cliprect, no point in drawing tiled images outside it.
 XOff, YOff are offsets which allow for the fact that the viewable area may not be at 0,0.
 IW, IH are the total width and height of the document if you could see it all at once.
 BW, BH are bitmap dimensions used to calc tiling.
 X, Y are the position (window coordinates) where the first background iamge will be drawn.
 X2, Y2 are tiling limits.  X2 and Y2 may be such that 0, 1, or many images will
   get drawn.  They're calculated so that only images within ARect are drawn.
}

implementation

//-- BG ---------------------------------------------------------- 20.03.2011 --
function MediaTypesToStr(const MediaTypes: TMediaTypes): ThtString;
var
  I: TMediaType;
begin
  SetLength(Result, 0);
  for I := low(I) to high(I) do
    if I in MediaTypes then
    begin
      if Length(Result) = 0 then
        Result := CMediaTypes[I]
      else
        Result := Result + ', ' + CMediaTypes[I];
    end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TranslateMediaTypes(const MediaTypes: TMediaTypes): TMediaTypes;
begin
  if mtAll in MediaTypes then
    Result := AllMediaTypes
  else
    Result := MediaTypes;
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
function TryStrToMediaType(const Str: ThtString; out MediaType: TMediaType): Boolean;
var
  I: TMediaType;
begin
  for I := low(I) to high(I) do
    if CMediaTypes[I] = Str then
    begin
      Result := True;
      MediaType := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 17.04.2011 --
function TryStrToMediaTypes(const Str: ThtString; out MediaTypes: TMediaTypes): Boolean;
var
  I, J: Integer;
  MediaType: TMediaType;
begin
  Result := False;
  MediaTypes := [];
  I := 1;
  repeat
    J := PosX(',', Str, I);
    if J = 0 then
      // no more commas, try the rest
      J := Length(Str) + 1;
    if TryStrToMediaType(htLowerCase(Trim(Copy(Str, I, J - I))), MediaType) then
    begin
      Include(MediaTypes, MediaType);
      Result := True;
    end;
    I := J + 1;
  until J > Length(Str);
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
function TryStrToAlignmentStyle(const Str: ThtString; out AlignmentStyle: TAlignmentStyle): Boolean;
var
  I: TAlignmentStyle;
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

//-- BG ---------------------------------------------------------- 16.03.2011 --
function TryStrToBorderStyle(const Str: ThtString; out BorderStyle: TBorderStyle): Boolean;
var
  I: TBorderStyle;
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

//-- BG ---------------------------------------------------------- 16.04.2011 --
function TryStrToBulletStyle(const Str: ThtString; out BulletStyle: TBulletStyle): Boolean;
var
  I: TBulletStyle;
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
function TryStrToDisplayStyle(const Str: ThtString; out Display: TDisplayStyle): Boolean;
var
  I: TDisplayStyle;
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

//-- BG ---------------------------------------------------------- 07.04.2011 --
function GetPositionInRange(Which: TBackgroundPosition; Where, Range: Integer): Integer;
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
{
  Returns the start and end value for tiling a tile of given size.
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

{----------------CalcBackgroundLocationAndTiling}

procedure CalcBackgroundLocationAndTiling(const PRec: PtPositionRec; ARect: TRect;
  XOff, YOff, IW, IH, BW, BH: Integer; out X, Y, X2, Y2: Integer);

{PRec has the CSS information on the background image, it's starting location and
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
  with PRec[1] do
  begin
    X := GetPositionInRange(PosType, Value, IW - BW) - XOff;
    AdjustForTiling(RepeatD, ARect.Left, ARect.Right, BW, X, X2);
  end;
  with PRec[2] do
  begin
    Y := GetPositionInRange(PosType, Value, IH - BH) - YOff;
    AdjustForTiling(RepeatD, ARect.Top, ARect.Bottom, BH, Y, Y2);
  end;
end;

end.
